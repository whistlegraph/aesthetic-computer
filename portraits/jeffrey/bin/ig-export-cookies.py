#!/usr/bin/env python3
"""Export an instaloader session pickle (dict of cookie name→value) into a
JSON cookie list compatible with Playwright's `context.addCookies()`.

Usage:
    bin/ig-export-cookies.py [--account=whistlegraph] [--out=<path>]

Default --out is portraits/jeffrey/sessions/<account>.cookies.json.

Run via the instaloader venv:
    /opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3 \\
        portraits/jeffrey/bin/ig-export-cookies.py --account=whistlegraph
"""
import argparse, json, os, pickle, sys, time
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]

p = argparse.ArgumentParser()
p.add_argument("--account", default="whistlegraph")
p.add_argument("--out", default=None)
args = p.parse_args()

session_path = REPO / "portraits/jeffrey/sessions" / args.account
out_path = Path(args.out) if args.out else REPO / "portraits/jeffrey/sessions" / f"{args.account}.cookies.json"

if not session_path.exists():
    print(f"missing {session_path}", file=sys.stderr)
    sys.exit(1)

with open(session_path, "rb") as f:
    jar = pickle.load(f)

# 1y from now — long enough that we never hit expiry inside a single archive run.
expires = int(time.time()) + 365 * 24 * 3600

cookies = []
for name, value in jar.items():
    cookies.append({
        "name": name,
        "value": str(value),
        "domain": ".instagram.com",
        "path": "/",
        "expires": expires,
        "httpOnly": name in {"sessionid", "ds_user_id"},
        "secure": True,
        "sameSite": "Lax",
    })

out_path.parent.mkdir(parents=True, exist_ok=True)
with open(out_path, "w") as f:
    json.dump(cookies, f, indent=2)

print(f"wrote {out_path} ({len(cookies)} cookies)")
print(f"  sessionid: {jar.get('sessionid','')[:30]}...")
print(f"  ds_user_id: {jar.get('ds_user_id')}")
