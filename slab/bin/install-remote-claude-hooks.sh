#!/bin/bash
# Installs minimal, Linux-safe slab claude hooks on jasellite so remote claude
# sessions write the marker files the local slab bridge mirrors. Idempotent.
set -eu
SLAB_HOME="$HOME/.local/share/slab"
HOOKS="$SLAB_HOME/hooks"
STATE="$SLAB_HOME/state"
mkdir -p "$HOOKS" "$STATE/active-prompts" "$STATE/awaiting-prompts" "$STATE/running-tools"

# --- UserPromptSubmit: mark session working, tag with launch id ---
cat > "$HOOKS/remote-prompt-log.sh" <<'HOOK'
#!/bin/bash
set -u
SLAB_HOME="${SLAB_HOME:-$HOME/.local/share/slab}"
ACTIVE="$SLAB_HOME/state/active-prompts"; AWAIT="$SLAB_HOME/state/awaiting-prompts"; RUN="$SLAB_HOME/state/running-tools"
input=$(cat); [ -z "$input" ] && exit 0
mkdir -p "$ACTIVE" "$AWAIT" "$RUN"
sid=$(echo "$input" | jq -r '.session_id // empty'); [ -z "$sid" ] && exit 0
cpid=$$
for _ in 1 2 3 4 5 6 7 8; do
  par=$(ps -o ppid= -p "$cpid" 2>/dev/null | tr -d ' '); { [ -z "$par" ] || [ "$par" = 1 ]; } && break
  comm=$(ps -o comm= -p "$par" 2>/dev/null | tr -d ' '); cpid=$par; case "$comm" in *claude*) break;; esac
done
tty=$(ps -o tty= -p "$cpid" 2>/dev/null | tr -d ' ')
ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)
summary=$(echo "$input" | jq -r '.prompt // ""' | tr '\n\r\t' '   ' | awk '{gsub(/^ +| +$/,"");n=(NF>7)?7:NF;out="";for(i=1;i<=n;i++)out=(i==1?$i:out" "$i);if(length(out)>48)out=substr(out,1,45)"…";else if(NF>n)out=out"…";print out}')
echo "$input" | jq -c --arg sid "$sid" --arg tty "$tty" --arg pid "$cpid" --arg ts "$ts" --arg sum "$summary" --arg lid "${SLAB_LAUNCH_ID:-}" \
  '{session_id:$sid,cwd:.cwd,subject:(.prompt|tostring|.[0:140]),summary:$sum,tty:$tty,claude_pid:($pid|tonumber? //0),updated:$ts,state:"working",launch_id:$lid}' > "$ACTIVE/$sid"
rm -f "$AWAIT/$sid" "$RUN/$sid"
exit 0
HOOK

# --- Stop: turn idle (complete) ---
cat > "$HOOKS/remote-stop.sh" <<'HOOK'
#!/bin/bash
set -u
SLAB_HOME="${SLAB_HOME:-$HOME/.local/share/slab}"
ACTIVE="$SLAB_HOME/state/active-prompts"
input=$(cat); sid=$(echo "$input" | jq -r '.session_id // empty'); [ -z "$sid" ] && exit 0
f="$ACTIVE/$sid"; [ -f "$f" ] || exit 0
ts=$(date -u +%Y-%m-%dT%H:%M:%SZ); tmp=$(mktemp)
jq -c --arg ts "$ts" '.state="complete"|.updated=$ts' "$f" > "$tmp" 2>/dev/null && mv "$tmp" "$f" || rm -f "$tmp"
rm -f "$SLAB_HOME/state/running-tools/$sid"
exit 0
HOOK

# --- Notification: awaiting permission / input ---
cat > "$HOOKS/remote-notify.sh" <<'HOOK'
#!/bin/bash
set -u
SLAB_HOME="${SLAB_HOME:-$HOME/.local/share/slab}"
ACTIVE="$SLAB_HOME/state/active-prompts"; AWAIT="$SLAB_HOME/state/awaiting-prompts"
input=$(cat); sid=$(echo "$input" | jq -r '.session_id // empty'); [ -z "$sid" ] && exit 0
mkdir -p "$AWAIT"
echo "$input" | jq -r '.message // "waiting"' | tr -d '\n' > "$AWAIT/$sid"
f="$ACTIVE/$sid"; if [ -f "$f" ]; then tmp=$(mktemp); jq -c '.state="awaiting"' "$f" > "$tmp" 2>/dev/null && mv "$tmp" "$f" || rm -f "$tmp"; fi
exit 0
HOOK

# --- bridge reader: emit all markers as one JSON blob for the local poller ---
cat > "$HOOKS/remote-dump.mjs" <<'DUMP'
import { readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
const S = join(process.env.HOME, ".local/share/slab/state");
const rd = (d) => { try { return readdirSync(join(S, d)); } catch { return []; } };
const out = { active: {}, awaiting: {}, running: {} };
for (const f of rd("active-prompts")) { try { out.active[f] = JSON.parse(readFileSync(join(S, "active-prompts", f), "utf8")); } catch {} }
for (const f of rd("awaiting-prompts")) { try { out.awaiting[f] = readFileSync(join(S, "awaiting-prompts", f), "utf8"); } catch {} }
for (const f of rd("running-tools")) { try { out.running[f] = statSync(join(S, "running-tools", f)).mtimeMs; } catch {} }
process.stdout.write(JSON.stringify(out));
DUMP

chmod +x "$HOOKS"/remote-*.sh

# --- wire claude's user settings to call the hooks (merge if present) ---
mkdir -p "$HOME/.claude"
SETTINGS="$HOME/.claude/settings.json"
HOOKCFG=$(cat <<JSON
{
  "hooks": {
    "UserPromptSubmit": [{"hooks":[{"type":"command","command":"$HOOKS/remote-prompt-log.sh"}]}],
    "Stop": [{"hooks":[{"type":"command","command":"$HOOKS/remote-stop.sh"}]}],
    "Notification": [{"hooks":[{"type":"command","command":"$HOOKS/remote-notify.sh"}]}]
  }
}
JSON
)
if [ -f "$SETTINGS" ]; then
  tmp=$(mktemp)
  if jq -s '.[0] * .[1]' "$SETTINGS" <(echo "$HOOKCFG") > "$tmp" 2>/dev/null; then mv "$tmp" "$SETTINGS"; else rm -f "$tmp"; echo "WARN: could not merge settings.json"; fi
else
  echo "$HOOKCFG" > "$SETTINGS"
fi

echo "INSTALLED hooks:"; ls -1 "$HOOKS"
echo "settings.json hooks:"; jq -r '.hooks | keys[]' "$SETTINGS" 2>/dev/null
echo "node check dump:"; node "$HOOKS/remote-dump.mjs" && echo " (dump OK)"
