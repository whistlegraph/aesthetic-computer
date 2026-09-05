#!/usr/bin/env bash
# journey.sh — what a band member has actually written and been through.
# Reads the machine's agent-memory commit ledger (the post-commit hook logs
# every commit made on that machine into ~/.ac-agent-memory) and aggregates
# it into a journey: months, lanes, firsts and lasts. Prints JSON to stdout;
# redirect into members/<name>/journey.json.
# Local: `bin/journey.sh`. Remote: `bin/journey.sh blueberry`.
set -euo pipefail
if [ -n "${1:-}" ] && [ "$1" != "local" ]; then
  exec ssh "$1" 'bash -s' -- local < "$0"
fi

CLI="$HOME/aesthetic-computer/memory/cli.mjs"
RAW=$(mktemp); REFLOG=$(mktemp)
trap 'rm -f "$RAW" "$REFLOG"' EXIT
node "$CLI" search "." --regex --session post-commit:core --limit 20000 --json 2>/dev/null > "$RAW" || echo '{}' > "$RAW"
# Fallback source: the clone's reflog remembers commits made on this machine
# even when the ledger hook was never wired (expires after ~90 days).
git -C "$HOME/aesthetic-computer" log -g \
  --format='%ad%x09%gs' --date=format:'%Y-%m-%dT%H:%M:%S' 2>/dev/null > "$REFLOG" || true
python3 - "$RAW" "$REFLOG" <<'PY'
import json, sys, re
from collections import Counter, defaultdict

d = json.load(open(sys.argv[1]))
hits = d.get("hits", [])

def subject(snippet):
    s = snippet.split(" Claude-Session:")[0].strip()
    s = re.sub(r"\s+", " ", s)
    return s

source = "ledger"
commits = []
for h in hits:
    commits.append({"when": h["when"], "subject": subject(h.get("snippet", ""))})
if not commits:
    source = "reflog"
    for line in open(sys.argv[2]):
        when, _, action = line.rstrip("\n").partition("\t")
        m = re.match(r"^(commit(?: \(amend\)| \(initial\))?|cherry-pick): (.*)$", action)
        if m:
            commits.append({"when": when, "subject": m.group(2)})
commits.sort(key=lambda c: c["when"])

by_month = Counter(c["when"][:7] for c in commits)

# Lanes: repo convention is "lane: description". Keep short, spaceless-ish
# prefixes; versioned lanes like "oskiewar v94" collapse to their stem.
lanes = defaultdict(list)
for c in commits:
    m = re.match(r"^([\w@./-]+(?: [\w./-]+)?):\s", c["subject"])
    if not m:
        continue
    lane = m.group(1).lower()
    lane = re.sub(r"\s+v?\d+(\.\d+)*$", "", lane)   # strip version suffixes
    lanes[lane].append(c)

lane_rows = []
for lane, cs in sorted(lanes.items(), key=lambda kv: -len(kv[1])):
    lane_rows.append({
        "lane": lane, "commits": len(cs),
        "first": cs[0]["when"][:10], "last": cs[-1]["when"][:10],
        "first_subject": cs[0]["subject"][:110],
        "last_subject": cs[-1]["subject"][:110],
    })

out = {
    "source": source,
    "total_commits": len(commits),
    "ledger_begins": commits[0]["when"][:10] if commits else None,
    "ledger_ends": commits[-1]["when"][:10] if commits else None,
    "first_commit": commits[0]["subject"][:160] if commits else None,
    "latest_commit": commits[-1]["subject"][:160] if commits else None,
    "by_month": dict(sorted(by_month.items())),
    "lanes_total": len(lane_rows),
    "lanes": lane_rows[:40],
    "busiest_day": None,
}
by_day = Counter(c["when"][:10] for c in commits)
if by_day:
    day, n = by_day.most_common(1)[0]
    out["busiest_day"] = {"date": day, "commits": n}

print(json.dumps(out, indent=2))
PY
