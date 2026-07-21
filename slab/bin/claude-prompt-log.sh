#!/bin/bash
# UserPromptSubmit hook: log prompt, mark session as active, set awake.
# Writes JSON metadata to active-prompts/<session_id> so the menubar can
# render per-session status (subject, cwd, tty, claude pid).
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
PROMPT_LOG=${PROMPT_LOG:-$HOME/.claude/prompts.log.jsonl}
ACTIVE_DIR="$SLAB_HOME/state/active-prompts"
AWAITING_DIR="$SLAB_HOME/state/awaiting-prompts"

input=$(cat)

"$SLAB_BIN/slab-cancel-pending" 2>/dev/null || true

# User responded to a pending Notification (or started fresh work) — clear
# the ambient pause flag so the daemon can re-arm ambient on its next tick.
rm -f /tmp/slab-ambient-paused

# keep the machine awake while this new prompt runs
"$SLAB_BIN/claude-sleep" awake >/dev/null 2>&1 &

if [[ -n "$input" ]]; then
    mkdir -p "$ACTIVE_DIR" "$AWAITING_DIR"
    session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)

    if [[ -n "$session_id" ]]; then
        # Walk up the process tree to find the claude pid so the menubar
        # can verify liveness via pid existence.
        claude_pid=$$
        for _ in 1 2 3 4 5 6 7 8; do
            parent=$(ps -o ppid= -p "$claude_pid" 2>/dev/null | tr -d ' ')
            [[ -z "$parent" || "$parent" == "1" ]] && break
            comm=$(ps -o comm= -p "$parent" 2>/dev/null | tr -d ' ')
            claude_pid=$parent
            [[ "$comm" == *claude* ]] && break
        done
        # The hook subprocess is spawned without a controlling terminal, so
        # `ps -o tty -p $$` returns "??". The claude process found above keeps
        # the Terminal tab's tty — read it from there instead.
        tty=${SLAB_TERMINAL_TTY:-$(ps -o tty= -p "$claude_pid" 2>/dev/null | tr -d ' ')}
        ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)

        # 4–8 word summary used as the live Terminal title and the menubar's
        # short subject. We collapse whitespace, take the first 7 words, and
        # cap at 48 chars so it fits in a window-title bar.
        summary=$(echo "$input" | jq -r '.prompt // ""' \
            | tr '\n\r\t' '   ' \
            | awk '{
                gsub(/^ +| +$/, "");
                n = (NF > 7) ? 7 : NF;
                out = "";
                for (i = 1; i <= n; i++) out = (i == 1 ? $i : out " " $i);
                # Elliptical, always: any truncation (by word count or by
                # width) trails off in an ellipsis so the title reads as a
                # trailing thought, never a mid-sentence chop.
                if (length(out) > 48) out = substr(out, 1, 45) "…";
                else if (NF > n) out = out "…";
                print out;
            }')

        echo "$input" | jq -c \
            --arg sid "$session_id" \
            --arg tty "$tty" \
            --arg pid "$claude_pid" \
            --arg ts "$ts" \
            --arg sum "$summary" \
            --arg nudge "${SLAB_NUDGE_SCREEN:-}" --arg contact "${SLAB_LOOPBOY_CONTACT:-}" \
            '{session_id: $sid, cwd: .cwd, subject: (.prompt | tostring | .[0:140]), summary: $sum, tty: $tty, claude_pid: ($pid | tonumber? // 0), agent_pid: ($pid | tonumber? // 0), agent_type: "claude", updated: $ts, state: "working", nudge_screen:$nudge, loopboy_contact:$contact}' \
            > "$ACTIVE_DIR/$session_id" 2>/dev/null

        # Live terminal title: write OSC 0 ("set window + icon name") direct
        # to the controlling TTY. Terminal.app picks it up unless the
        # menubar's "Theme by status" has set a custom title — that wins,
        # and reads the same `summary` field from active-prompts.
        if [[ -n "$tty" && -e "/dev/$tty" && -n "$summary" ]]; then
            printf '\033]0;%s\007' "$summary" > "/dev/$tty" 2>/dev/null
        fi

        # User responded — clear any awaiting marker for this session, plus a
        # stale running-tool flag so the fresh turn's interrupt-detection
        # starts clean (see claude-tool-heartbeat.sh).
        rm -f "$AWAITING_DIR/$session_id"
        rm -f "$SLAB_HOME/state/running-tools/$session_id"
    fi

    mkdir -p "$(dirname "$PROMPT_LOG")"
    echo "$input" | jq -c --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
        '{ts: $ts, session: .session_id, cwd: .cwd, prompt: .prompt}' \
        >> "$PROMPT_LOG" 2>/dev/null
fi

exit 0
