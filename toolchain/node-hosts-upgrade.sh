#!/usr/bin/env bash
# Upgrade Node on the hosts the stack runs on, restart what runs on it, verify,
# and leave a receipt an agent can read without touching the hosts.
#
#   toolchain/node-hosts-upgrade.sh                 # all hosts to .node-version
#   toolchain/node-hosts-upgrade.sh --check         # read-only: versions + stale processes
#   toolchain/node-hosts-upgrade.sh --only lith,session
#   toolchain/node-hosts-upgrade.sh --version 24.22.1 --dry-run
#
# Receipt: ~/.local/share/aespatchercron/hosts/latest.json (+ history.jsonl, per-run .log).
# Hosts: lith and oven install NodeSource's apt package; the session droplet uses fnm + systemd.
set -u

ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
WANT=$(tr -d '[:space:]' < "$ROOT/.node-version")
ONLY="lith,oven,session"; MODE="upgrade"
while [ $# -gt 0 ]; do
  case "$1" in
    --version) WANT="$2"; shift ;;
    --only) ONLY="$2"; shift ;;
    --check) MODE="check" ;;
    --dry-run) MODE="dry-run" ;;
    -h|--help) sed -n '2,11p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 64 ;;
  esac
  shift
done
WANT="${WANT#v}"
[[ "$WANT" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || { echo "bad version: $WANT" >&2; exit 64; }

HOME_DIR="$HOME/.local/share/aespatchercron/hosts"
mkdir -p "$HOME_DIR" && chmod 700 "$HOME_DIR"
STAMP=$(date -u +%Y%m%dT%H%M%SZ)
LOG="$HOME_DIR/$STAMP.log"
: > "$LOG"; chmod 600 "$LOG"

# host | user@address | identity file | recipe | service
hosts() {
  cat <<EOF
lith|root@lith.aesthetic.computer|$ROOT/aesthetic-computer-vault/home/.ssh/id_rsa|apt|lith
oven|root@137.184.237.166|$ROOT/aesthetic-computer-vault/oven/ssh/oven-deploy-key|apt|oven
session|root@157.245.134.225|$HOME/.ssh/session_server|fnm|session-server
EOF
}

# Runs on an apt (NodeSource) host. Prints one RESULT line; STALE lines list node
# processes still running on the old binary.
apt_recipe() {
  cat <<'EOF'
set -u
WANT="$1"; SERVICE="$2"; MODE="$3"
before=$(node -v 2>/dev/null || echo none)
if [ "$MODE" = upgrade ] && [ "$before" != "v$WANT" ]; then
  major="${WANT%%.*}"
  # A host on another NodeSource line (oven sat on 20.x) has no package for this major until its repo moves.
  if [ "${before%%.*}" != "v$major" ]; then
    curl -fsSL "https://deb.nodesource.com/setup_${major}.x" 2>/dev/null | bash - >/dev/null 2>&1 || echo "DETAIL nodesource setup_${major}.x failed"
  fi
  apt-get update -qq >/dev/null 2>&1
  apt-get install -y -qq "nodejs=${WANT}-1nodesource1" >/dev/null 2>&1 || apt-get install -y -qq nodejs >/dev/null 2>&1 || echo "DETAIL apt install failed"
  echo "DETAIL apt candidate $(apt-cache policy nodejs 2>/dev/null | sed -n 's/^ *Candidate: //p')"
fi
after=$(node -v 2>/dev/null || echo none)
bin=$(readlink -f "$(command -v node)"); bin_mtime=$(stat -c %Y "$bin")
if [ "$MODE" = upgrade ]; then systemctl restart "$SERVICE"; sleep 3; fi
active=$(systemctl is-active "$SERVICE" 2>/dev/null || echo unknown)
stale=0
for pid in $(pgrep -x node); do
  started=$(stat -c %Y "/proc/$pid" 2>/dev/null || echo 0)
  if [ "$started" -lt "$bin_mtime" ]; then stale=$((stale + 1)); echo "STALE pid=$pid $(tr '\0' ' ' < /proc/$pid/cmdline | cut -c1-100)"; fi
done
echo "RESULT host=$(hostname) before=$before after=$after service=$SERVICE active=$active stale=$stale brotli=$(node -p process.versions.brotli)"
EOF
}

# Runs on the fnm host. systemd's session-server unit owns the server (see
# session-server/deploy-remote.sh); any pm2 copy left from the older setup is retired.
fnm_recipe() {
  cat <<'EOF'
set -u
WANT="$1"; UNIT="$2"; MODE="$3"
export PATH="/root/.local/share/fnm:$PATH"; eval "$(fnm env)"
repo=/home/aesthetic-computer
before=$(node -v 2>/dev/null || echo none)
rev=$(git -C $repo rev-parse --short HEAD 2>/dev/null || echo unknown)
if [ "$MODE" = upgrade ]; then
  fnm install "$WANT" >/dev/null 2>&1; fnm default "$WANT" >/dev/null 2>&1
  eval "$(fnm env)"; fnm use "$WANT" >/dev/null 2>&1
  echo "DETAIL origin $(git -C $repo remote get-url origin 2>/dev/null) dirty=$(git -C $repo status --short 2>/dev/null | wc -l | tr -d ' ')"
  if git -C $repo fetch -q origin main 2>/dev/null; then
    git -C $repo merge --ff-only FETCH_HEAD >/dev/null 2>&1 || echo "DETAIL main not fast-forwardable: local $(git -C $repo rev-parse --short HEAD) fetched $(git -C $repo rev-parse --short FETCH_HEAD)"
  else echo "DETAIL git fetch failed"; fi
  rev=$(git -C $repo rev-parse --short HEAD 2>/dev/null || echo unknown)
  (cd $repo/session-server && npm install --omit=dev >/dev/null 2>&1) || echo "DETAIL npm install failed"
  if command -v pm2 >/dev/null 2>&1; then
    for name in session session-server; do pm2 delete "$name" >/dev/null 2>&1 && echo "DETAIL removed pm2 app $name"; done
    pm2 save --force >/dev/null 2>&1; pm2 unstartup systemd >/dev/null 2>&1; pm2 kill >/dev/null 2>&1
  fi
  systemctl reset-failed "$UNIT" >/dev/null 2>&1; systemctl restart "$UNIT" || echo "DETAIL systemctl restart $UNIT failed"
  sleep 4
fi
after=$(node -v 2>/dev/null || echo none)
active=$(systemctl is-active "$UNIT" 2>/dev/null || echo unknown)
main=$(systemctl show "$UNIT" -p MainPID --value 2>/dev/null)
exe=$(readlink -f "/proc/$main/exe" 2>/dev/null || echo none)
running=$(printf '%s' "$exe" | sed -n 's#.*/node-versions/\(v[0-9.]*\)/.*#\1#p'); [ -n "$running" ] || running=unknown
echo "DETAIL $UNIT main pid=$main node=$running restarts=$(systemctl show "$UNIT" -p NRestarts --value 2>/dev/null) exec=$(systemctl show "$UNIT" -p ExecStart --value 2>/dev/null | sed -n 's/.*argv\[\]=\([^;]*\);.*/\1/p' | cut -c1-90)"
[ "$running" = "v$WANT" ] || echo "STALE pid=$main $UNIT main process runs $running"
for pid in $(pgrep -f 'session\.mjs$'); do
  [ "$pid" = "$main" ] && continue
  echo "STALE pid=$pid unit=$(ps -o unit= -p "$pid" | tr -d ' ') parent=$(ps -o args= -p "$(ps -o ppid= -p "$pid" | tr -d ' ')" | cut -c1-50) started=$(ps -o lstart= -p "$pid")"
done
git -C $repo status --short 2>/dev/null | head -10 | sed 's/^/DETAIL dirty /'
git -C $repo diff --stat 2>/dev/null | tail -5 | sed 's/^/DETAIL diffstat /' | cut -c1-140
ss -ltnp 2>/dev/null | awk 'NR>1 {print "DETAIL listen " $4 " " $6}' | grep -i node | cut -c1-120
journalctl -u "$UNIT" -n 12 --no-pager -o cat 2>/dev/null | cut -c1-160 | sed 's/^/DETAIL journal /'
echo "RESULT host=$(hostname) before=$before after=$after service=$UNIT active=$active rev=$rev brotli=$(node -p process.versions.brotli)"
EOF
}

json_escape() { printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'; }

rows=(); failures=0
while IFS='|' read -r name target key kind service; do
  [[ ",$ONLY," == *",$name,"* ]] || continue
  echo "== $name ($target) target v$WANT [$MODE]" | tee -a "$LOG"
  if [ ! -r "$key" ]; then
    echo "   key unreadable: $key" | tee -a "$LOG"; rows+=("{\"host\":\"$name\",\"status\":\"no-key\"}"); failures=$((failures + 1)); continue
  fi
  if [ "$MODE" = dry-run ]; then
    echo "   would ssh -i $key $target and run the $kind recipe for $service" | tee -a "$LOG"
    rows+=("{\"host\":\"$name\",\"status\":\"dry-run\"}"); continue
  fi
  remote_mode=$([ "$MODE" = check ] && echo check || echo upgrade)
  recipe=$([ "$kind" = apt ] && apt_recipe || fnm_recipe)
  out=$(ssh -i "$key" -o BatchMode=yes -o ConnectTimeout=15 -o IdentitiesOnly=yes "$target" "bash -s -- '$WANT' '$service' '$remote_mode'" <<< "$recipe" 2>&1)
  rc=$?
  printf '%s\n' "$out" >> "$LOG"
  result=$(printf '%s\n' "$out" | grep '^RESULT ' | tail -1)
  stale=$(printf '%s\n' "$out" | grep -c '^STALE ')
  after=$(printf '%s' "$result" | sed -n 's/.* after=\([^ ]*\).*/\1/p')
  active=$(printf '%s' "$result" | sed -n 's/.* active=\([^ ]*\).*/\1/p')
  status="ok"
  if [ $rc -ne 0 ] || [ -z "$result" ]; then status="ssh-failed"
  elif [ "$MODE" = upgrade ] && [ "$after" != "v$WANT" ]; then status="version-mismatch"
  elif [ "$MODE" = upgrade ] && [[ "$active" != active* && "$active" != online* ]]; then status="service-not-active"
  elif [ "$stale" -gt 0 ]; then status="stale-processes"
  elif [ "$MODE" = check ] && [ "$after" != "v$WANT" ]; then status="behind"
  fi
  [ "$status" = ok ] || [ "$status" = behind ] || failures=$((failures + 1))
  echo "   $status  ${result:-$(printf '%s' "$out" | tail -1)}" | tee -a "$LOG"
  printf '%s\n' "$out" | grep -E '^(STALE|DETAIL) ' | sed 's/^/   /' | tee -a "$LOG"
  rows+=("{\"host\":\"$name\",\"status\":\"$status\",\"rc\":$rc,\"stale\":$stale,\"result\":\"$(json_escape "$result")\"}")
done < <(hosts)

if [ "$MODE" = upgrade ] && [[ ",$ONLY," == *",lith,"* ]]; then
  code=$(curl -sS -m 15 -o /dev/null -w '%{http_code}' https://aesthetic.computer/.commit-ref 2>/dev/null || echo 000)
  echo "== public check aesthetic.computer/.commit-ref -> $code" | tee -a "$LOG"
  [ "$code" = 200 ] || failures=$((failures + 1))
  rows+=("{\"host\":\"public\",\"status\":\"$([ "$code" = 200 ] && echo ok || echo http-$code)\"}")
fi

receipt="{\"at\":\"$STAMP\",\"want\":\"v$WANT\",\"mode\":\"$MODE\",\"failures\":$failures,\"log\":\"$LOG\",\"hosts\":[$(IFS=,; echo "${rows[*]}")]}"
printf '%s\n' "$receipt" > "$HOME_DIR/latest.json"; chmod 600 "$HOME_DIR/latest.json"
printf '%s\n' "$receipt" >> "$HOME_DIR/history.jsonl"; chmod 600 "$HOME_DIR/history.jsonl"
echo "receipt: $HOME_DIR/latest.json (failures: $failures)"
[ "$failures" -eq 0 ]
