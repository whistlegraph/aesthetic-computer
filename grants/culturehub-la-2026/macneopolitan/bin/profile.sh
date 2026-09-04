#!/usr/bin/env bash
# profile.sh — one band member's service record, as JSON on stdout.
# The deeper companion to harvest.sh: logins, uptimes, workload, live rocks.
#
#   ./profile.sh              this machine
#   ./profile.sh blueberry    that machine, over ssh
#
# "Uptime stretch" is the time between a boot and the next recorded shutdown
# or boot — sleep counts as up, the honest laptop definition.

set -euo pipefail

if [ -n "${1:-}" ] && [ "$1" != "local" ]; then
  exec ssh "$1" 'bash -s' -- local < "$0"
fi

# ---- login + reboot history (wtmp via `last`) ----
LAST=$(last 2>/dev/null || true)
logins_console=$(echo "$LAST" | grep -c "^[a-z_].*console" || true)
logins_tty=$(echo "$LAST" | grep -c "^[a-z_].*ttys" || true)
reboots=$(echo "$LAST" | grep -c "^reboot" || true)
shutdowns=$(echo "$LAST" | grep -c "^shutdown" || true)
wtmp_begins=$(echo "$LAST" | awk '/^wtmp begins/ {sub(/^wtmp begins /,""); print; exit}')

uptime_json=$(echo "$LAST" | python3 -c '
import sys, json, subprocess, re
from datetime import datetime
YEAR = 2026
events = []  # (datetime, kind) chronological
for line in sys.stdin:
    m = re.match(r"^(reboot|shutdown)\s+time\s+(\w{3}) (\w{3}) +(\d+) (\d+):(\d+)", line)
    if m:
        kind, _, mon, day, hh, mm = m.groups()
        dt = datetime.strptime(f"{mon} {day} {hh}:{mm} {YEAR}", "%b %d %H:%M %Y")
        events.append((dt, kind))
events.reverse()
best = None; best_span = 0.0
for i, (dt, kind) in enumerate(events):
    if kind != "reboot":
        continue
    end = events[i + 1][0] if i + 1 < len(events) else datetime.now()
    span = (end - dt).total_seconds()
    if span > best_span:
        best_span, best = span, dt
try:
    out = subprocess.check_output(["sysctl", "-n", "kern.boottime"], text=True)
    epoch = int(re.search(r"sec = (\d+)", out).group(1))
    cur = (datetime.now() - datetime.fromtimestamp(epoch)).total_seconds()
    if cur > best_span:
        best_span = cur
        best = datetime.fromtimestamp(epoch)
except Exception:
    pass
days = best_span / 86400
print(json.dumps({
    "longest_stretch_days": round(days, 1),
    "longest_stretch_began": best.strftime("%Y-%m-%d") if best else None,
}))
')

booted=$(sysctl -n kern.boottime | sed -E 's/^\{ sec = ([0-9]+).*/\1/')
booted_iso=$(date -r "$booted" '+%Y-%m-%dT%H:%M:%S')

# ---- disk + load ----
disk=$(df -H /System/Volumes/Data 2>/dev/null | awk 'NR==2 {printf "{\"used\":\"%s\",\"free\":\"%s\",\"pct\":\"%s\"}", $3, $4, $5}')
procs=$(ps ax | wc -l | tr -d ' ')
loadavg=$(sysctl -n vm.loadavg | tr -d '{}' | awk '{printf "[%s,%s,%s]", $1, $2, $3}')

# ---- cumulative CPU champions among running processes ----
cpu_json=$(ps axo time=,comm= | awk '
{
  t=$1; cmd=$2; n=split($0, parts, " "); # comm may contain spaces; rebuild
  sub(/^ *[0-9:.]+ +/, "", $0); cmd=$0;
  split(t, a, ":");
  secs=0; for (i=1; i<=length(a); i++) secs = secs*60 + a[i];
  cum[cmd] += secs
}
END { for (c in cum) printf "%d\t%s\n", cum[c], c }' \
  | sort -rn | head -6 \
  | python3 -c '
import sys, json, os
out = []
for line in sys.stdin:
    secs, cmd = line.rstrip("\n").split("\t", 1)
    out.append({"cmd": os.path.basename(cmd), "cpu_hours": round(int(secs)/3600, 1)})
print(json.dumps(out))')

# ---- agent workload: sessions by project + month ----
sessions_json="null"
CLI="$HOME/aesthetic-computer/memory/cli.mjs"
if [ -f "$CLI" ] && command -v node >/dev/null; then
  sessions_json=$(node "$CLI" list --limit 100000 --json 2>/dev/null | node -e '
let raw = ""; process.stdin.on("data", d => raw += d).on("end", () => {
  const s = JSON.parse(raw);
  const arr = Array.isArray(s) ? s : (s.sessions || []);
  const by = {}, mo = {}, prov = {};
  for (const x of arr) {
    by[x.project] = (by[x.project] || 0) + 1;
    prov[x.provider] = (prov[x.provider] || 0) + 1;
    const m = (x.created_at || "").slice(0, 7);
    if (m) mo[m] = (mo[m] || 0) + 1;
  }
  console.log(JSON.stringify({ total: arr.length, by_project: by, by_provider: prov, by_month: mo }));
});' || echo null)
fi

# ---- prox: live rocks on this machine right now ----
rocks_json="[]"
export LEDGER="$HOME/.config/slab/ledger/local.json"
if [ -f "$LEDGER" ] && command -v node >/dev/null; then
  rocks_json=$(node -e '
const j = require(process.env.LEDGER);
const out = (j.entries || []).map(e => ({ name: e.name, agent: e.agentType }));
console.log(JSON.stringify(out));' 2>/dev/null || echo "[]")
fi

# ---- fish personality ----
abbrs=$(fish -c 'abbr --list' 2>/dev/null | wc -l | tr -d ' ')

cat <<JSON
{
  "hostname": "$(hostname -s)",
  "logins": { "console": ${logins_console:-0}, "tty": ${logins_tty:-0}, "recorded_since": "${wtmp_begins:-?}" },
  "boots": { "reboots": ${reboots:-0}, "shutdowns": ${shutdowns:-0}, "current_boot": "$booted_iso" },
  "uptime": $uptime_json,
  "disk": ${disk:-null},
  "load": { "processes": $procs, "loadavg": $loadavg },
  "cpu_champions": $cpu_json,
  "agent_workload": $sessions_json,
  "prox_rocks_now": $rocks_json,
  "fish_abbreviations": ${abbrs:-0},
  "profiled_at": "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
}
JSON
