#!/bin/bash
# sweep.sh — run a queue of hear.mjs evaluations, two at a time, logging each.
# Meant to be nohup'd on an evaluation host (see offload.sh):
#   ssh poorslice 'cd ~/aesthetic-computer/grants/.../macneopolitan &&
#     nohup bash bin/sweep.sh queue.txt > /tmp/sweep.log 2>&1 & echo $!'
# queue.txt: one run per line — `tag  [flag ...]`, blank lines and # ignored.
# SCORES=<glob> evaluates a different corpus (the held-out trio movements,
# say) instead of the eight dialogs.
set -uo pipefail
Q="${1:?usage: sweep.sh queue.txt [parallel]}"
PAR="${2:-2}"
cd "$(dirname "$0")/.."
export PATH="$HOME/whisper/bin:$HOME/bin:$HOME/node/bin:$PATH"
export SINGRENDER="${SINGRENDER:-$HOME/singbench/.build/release/singrender}"
run_one() {
  local tag="$1"; shift
  local t0=$SECONDS
  # shellcheck disable=SC2086
  node bin/hear.mjs "${SCORES:-scores/dialog-*.mbscore}" --tag "$tag" --no-spoken "$@" > "hear/$tag.log" 2>&1
  local line; line=$(grep -h "^═" "hear/$tag.log" | tail -1)
  printf '%s  [%ds]  %s\n' "$(date +%H:%M:%S)" "$((SECONDS - t0))" "${line:-✗ $tag produced no total (see hear/$tag.log)}"
}
n=0
while read -r tag rest; do
  [ -z "${tag:-}" ] && continue
  case "$tag" in \#*) continue;; esac
  [ -f "hear/$tag.json" ] && { echo "$(date +%H:%M:%S)  = $tag already done — skipping"; continue; }
  # shellcheck disable=SC2086
  run_one "$tag" $rest &
  n=$((n + 1))
  [ $((n % PAR)) -eq 0 ] && wait
done < "$Q"
wait
echo "$(date +%H:%M:%S)  queue done"
