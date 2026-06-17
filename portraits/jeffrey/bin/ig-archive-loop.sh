#!/bin/bash
# Auto-resuming wrapper for the sight-gated archiver.
# On a TRANSIENT halt (auto-advance flashed a reel/loading frame) it re-launches
# if we're still on a valid @whistlegraph post/grid. It STOPS for a paused/locked
# connection (needs the Mac password) or if we've drifted off-account.
cd /Users/jas/aesthetic-computer || exit 1
TAP=toolchain/macos/iphone-tap/iphone-tap
LOG=portraits/jeffrey/curated/whistlegraph-archived.jsonl

classify() {
  "$TAP" shot /tmp/wrap.png >/dev/null 2>&1 || { echo nowindow; return; }
  "$TAP" ocr /tmp/wrap.png 2>&1 | python3 -c '
import json, sys, re
d = json.load(sys.stdin); ls = d.get("lines", [])
t = " ".join(l["text"] for l in ls).lower()
if "resume" in t or "connection paused" in t or "connection interrupted" in t: print("paused"); sys.exit()
if "enter the mac login" in t or "is locked" in t: print("locked"); sys.exit()
post = any(l["text"].strip() == "Posts" and abs(l["x"] + l["w"]/2 - 0.5) < 0.12 and l["y"] + l["h"]/2 < 0.17 for l in ls)
grid = ("followers" in t and "following" in t) or (re.search(r"\breels\b", t) and re.search(r"\btagged\b", t))
acct = "whistlegraph" in t
print("ok" if (post or grid) and acct else ("transient" if acct else "offaccount"))
'
}

stuck=0
for run in $(seq 1 40); do
  before=$(grep -c '' "$LOG")
  echo "=== wrapper run $run ==="
  node portraits/jeffrey/bin/ig-archive-mirror.mjs --live --limit=400 --min-delay=0.6 --max-delay=1.3
  sleep 2
  after=$(grep -c '' "$LOG")
  st=$(classify)
  echo "[wrapper] post-run state: $st  (cumulative: $after)"
  # No-progress guard: if two runs in a row archive nothing, we're stuck on one
  # post — stop rather than spin.
  if [ "$after" -le "$before" ]; then stuck=$((stuck+1)); else stuck=0; fi
  if [ "$stuck" -ge 2 ]; then echo "[wrapper] no progress for 2 runs — stuck on a post, stopping."; break; fi
  case "$st" in
    ok|transient) sleep 2 ;;
    paused|locked) echo "[wrapper] NEEDS HUMAN ($st) — stopping."; break ;;
    *) echo "[wrapper] off-account/unknown ($st) — stopping for safety."; break ;;
  esac
done
echo "[wrapper] done. cumulative log: $(grep -c '' "$LOG")"
