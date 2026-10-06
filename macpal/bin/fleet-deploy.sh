#!/usr/bin/env bash
# fleet-deploy.sh — push THIS checkout's MacPal to every machine in the fleet and
# relaunch it there, in one command from the author's Mac.
#
#   macpal/bin/fleet-deploy.sh                 hosts from $MACPAL_FLEET or ~/.config/macpal/fleet
#   macpal/bin/fleet-deploy.sh panda chicken   explicit ssh hosts (aliases from ~/.ssh/config)
#   macpal/bin/fleet-deploy.sh --dry-run …     show the plan, touch nothing
#   macpal/bin/fleet-deploy.sh --status …      report installed vs source version only
#
# Per host: stream the source over ssh (tar, no git on the minis), build there
# with build.sh, install with ditto (cp -R breaks the ad-hoc signature on
# macOS 26), re-sign in place, then bootstrap/kickstart the launchd agent and
# verify the installed CFBundleShortVersionString matches the source. Hosts run
# in parallel; each prints one line per step prefixed with its name.
# Host lists live outside the repo (config file / env) — nothing fleet-specific
# is baked in here.
set -euo pipefail
cd "$(dirname "$0")/.."
SRC_VERSION=$(sed -n 's:.*<key>CFBundleShortVersionString</key>::p; ' Resources/Info.plist >/dev/null; perl -0ne 'print $1 if /CFBundleShortVersionString<\/key>\s*<string>([^<]+)</' Resources/Info.plist)
LABEL=${MACPAL_LAUNCHD_LABEL:-computer.aesthetic.macpal}
BUILD_DIR=${MACPAL_REMOTE_BUILD_DIR:-'~/Developer/macpal-build'}
DRY=0; STATUS=0; HOSTS=()
for a in "$@"; do case "$a" in --dry-run) DRY=1;; --status) STATUS=1;; *) HOSTS+=("$a");; esac; done
if [[ ${#HOSTS[@]} -eq 0 ]]; then
  if [[ -n "${MACPAL_FLEET:-}" ]]; then read -r -a HOSTS <<<"$MACPAL_FLEET"
  elif [[ -f "$HOME/.config/macpal/fleet" ]]; then mapfile -t HOSTS < <(grep -v '^\s*#' "$HOME/.config/macpal/fleet" | sed '/^\s*$/d')
  else echo "no hosts: pass them, set MACPAL_FLEET, or list one per line in ~/.config/macpal/fleet" >&2; exit 2; fi
fi
echo "macpal $SRC_VERSION → ${HOSTS[*]}"
status_of(){ ssh -o ConnectTimeout=8 "$1" 'v=$(defaults read /Applications/MacPal.app/Contents/Info.plist CFBundleShortVersionString 2>/dev/null || echo none); r=$(pgrep -x MacPal >/dev/null && echo running || echo stopped); echo "$v $r"'; }
deploy_one(){
  local h=$1 t0=$SECONDS
  local before; before=$(status_of "$h" 2>/dev/null || echo "unreachable")
  echo "[$h] installed: $before"
  [[ $STATUS -eq 1 ]] && return 0
  [[ "$before" == "$SRC_VERSION running" && "${FORCE:-0}" != 1 ]] && { echo "[$h] already $SRC_VERSION and running — skip (FORCE=1 to redo)"; return 0; }
  [[ $DRY -eq 1 ]] && { echo "[$h] would: stream source → $BUILD_DIR, build.sh, ditto → /Applications/MacPal.app, codesign -s -, bootstrap+kickstart $LABEL"; return 0; }
  tar -cf - --exclude=build --exclude=dist --exclude=.git --exclude=node_modules . \
    | ssh "$h" "mkdir -p $BUILD_DIR && tar -xf - -C $BUILD_DIR" && echo "[$h] source streamed"
  ssh "$h" "cd $BUILD_DIR && ./build.sh >/tmp/macpal-build.log 2>&1 || { tail -20 /tmp/macpal-build.log; exit 1; }" && echo "[$h] built"
  ssh "$h" "launchctl bootout gui/\$(id -u)/$LABEL 2>/dev/null; pkill -x MacPal 2>/dev/null; sleep 1; \
    rm -rf /Applications/MacPal.app && /usr/bin/ditto $BUILD_DIR/build/MacPal.app /Applications/MacPal.app && \
    codesign --force --deep -s - /Applications/MacPal.app && \
    launchctl enable gui/\$(id -u)/$LABEL 2>/dev/null; launchctl bootstrap gui/\$(id -u) ~/Library/LaunchAgents/$LABEL.plist 2>/dev/null || launchctl kickstart -k gui/\$(id -u)/$LABEL" \
    && echo "[$h] installed + relaunched"
  sleep 3
  local after; after=$(status_of "$h")
  if [[ "$after" == "$SRC_VERSION running" ]]; then echo "[$h] ✓ $after ($((SECONDS-t0))s)"; else echo "[$h] ✗ expected '$SRC_VERSION running', got '$after'" >&2; return 1; fi
}
rc=0; pids=()
for h in "${HOSTS[@]}"; do deploy_one "$h" & pids+=($!); done
for p in "${pids[@]}"; do wait "$p" || rc=1; done
exit $rc
