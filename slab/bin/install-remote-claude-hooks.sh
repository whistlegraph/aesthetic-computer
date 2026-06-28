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

# --- zellij persistence: helper + layout so a dropped window reattaches ---
BIN="$HOME/.local/bin"; mkdir -p "$BIN"
cat > "$BIN/jasellite-session" <<'HELPER'
#!/bin/bash
# Attach to a named zellij session running claude, or create it. Keeps claude
# alive on the box so a dropped/closed `jasellite` window reattaches to the
# running session instead of starting fresh.
set -u
S="${1:?session name}"; DIR="${2:-$HOME}"; LID="${3:-$S}"
if zellij list-sessions -s 2>/dev/null | grep -qx "$S"; then
  zellij attach "$S"; exit
fi
mkdir -p "$HOME/.cache/jasellite"
# Per-session env the layout's pane sources — bakes dir + launch id so the
# running claude is correct even when zellij's reused server env is stale.
{
  printf 'cd %q\n' "$DIR"
  printf 'export SLAB_LAUNCH_ID=%q\n' "$LID"
  printf 'export COLORTERM=truecolor\n'
  printf 'export CLAUDE_CODE_OAUTH_TOKEN=$(cat ~/.config/claude/oauth-token 2>/dev/null)\n'
} > "$HOME/.cache/jasellite/env-$S"
# --new-session-with-layout (NOT --layout): with --session, plain --layout means
# "add a tab to an existing session" and errors when it doesn't exist yet.
# Dedicated minimal config (no pane frames) + COLORTERM so zellij and claude
# both emit/pass 24-bit color and the window stays chrome-free.
export COLORTERM=truecolor
zellij --config "$HOME/.config/zellij/jasellite-config.kdl" --session "$S" --new-session-with-layout jasellite
HELPER
chmod +x "$BIN/jasellite-session"

ZJ_LAYOUTS="$HOME/.config/zellij/layouts"; mkdir -p "$ZJ_LAYOUTS"
cat > "$ZJ_LAYOUTS/jasellite.kdl" <<'KDL'
layout {
    // A tab template of just `children` drops zellij's default tab + status
    // bars, so the window is only claude — looks like a plain local session.
    default_tab_template {
        children
    }
    pane {
        command "bash"
        args "-lc" "source \"$HOME/.cache/jasellite/env-$ZELLIJ_SESSION_NAME\" 2>/dev/null; exec claude"
    }
}
KDL

# Minimal, theme-inheriting config for jasellite sessions only (passed via
# --config, so it doesn't touch the box's main config.kdl used by other layouts):
# no pane frames, no zellij theme override → claude renders in the user's own
# Terminal palette. Combined with the no-bars layout, zellij is ~invisible.
cat > "$HOME/.config/zellij/jasellite-config.kdl" <<'CFG'
pane_frames false
CFG

echo "INSTALLED hooks:"; ls -1 "$HOOKS"
echo "session helper + layout:"; ls -1 "$BIN/jasellite-session" "$ZJ_LAYOUTS/jasellite.kdl"
echo "settings.json hooks:"; jq -r '.hooks | keys[]' "$SETTINGS" 2>/dev/null
echo "node check dump:"; node "$HOOKS/remote-dump.mjs" && echo " (dump OK)"
