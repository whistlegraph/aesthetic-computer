#!/usr/bin/env python3
"""Cron-driven watcher on jasellite: emails once when the BTX node finishes IBD,
and once when the miner-rewards wallet first shows a nonzero balance."""
import os, sys, json, subprocess

HOME = os.path.expanduser("~")
CLI = [f"{HOME}/btx/build/bin/btx-cli", f"-datadir={HOME}/.btx"]
NOTIFY = [sys.executable, f"{HOME}/btx-notify.py"]

def cli(*a):
    try:
        return subprocess.run(CLI + list(a), capture_output=True, text=True, timeout=30).stdout
    except Exception:
        return ""

def notify(subj, body):
    try:
        subprocess.run(NOTIFY + [subj, body], timeout=60)
    except Exception:
        pass

info = {}
try:
    info = json.loads(cli("getblockchaininfo") or "{}")
except Exception:
    sys.exit(0)

blocks = info.get("blocks")
headers = info.get("headers")
prog = info.get("verificationprogress", 0)
ibd = info.get("initialblockdownload", True)

# 1) fully-synced notice (once)
synced_flag = f"{HOME}/.btx-synced-notified"
if not ibd and not os.path.exists(synced_flag):
    bal = "?"
    try:
        bal = json.loads(cli("-rpcwallet=miner-rewards", "getbalances") or "{}").get("mine", {})
    except Exception:
        pass
    notify("✅ BTX node fully synced on jasellite",
           f"initialblockdownload is now false.\n"
           f"Height {blocks}/{headers} ({prog*100:.2f}%).\n"
           f"miner-rewards balances: {bal}\n")
    open(synced_flag, "w").write("done\n")

# 2) first nonzero wallet balance (once)
paid_flag = f"{HOME}/.btx-paid-notified"
if not os.path.exists(paid_flag):
    try:
        m = json.loads(cli("-rpcwallet=miner-rewards", "getbalances") or "{}").get("mine", {})
        total = sum(float(m.get(k, 0) or 0) for k in ("trusted", "untrusted_pending", "immature"))
        if total > 0:
            notify("💰 BTX payout landed — nonzero wallet balance!",
                   f"miner-rewards now shows: {m}\nHeight {blocks}/{headers}.\n")
            open(paid_flag, "w").write("done\n")
    except Exception:
        pass
