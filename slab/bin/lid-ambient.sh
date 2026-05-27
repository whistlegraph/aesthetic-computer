#!/bin/bash
# lid-ambient daemon. Long-lived via launchd KeepAlive; polls lid every POLL
# seconds. The reactive listener (Python) owns all audio — ambient bed, soft
# mic-reactive noise, and pluck-arp triggers — and runs only when:
#   lid is closed AND sleep is disabled AND at least one active marker exists
#   in $SLAB_HOME/state/active-prompts/ (UserPromptSubmit → Stop lifecycle)
#   or $SLAB_HOME/state/active-subagents/ (Task tool → SubagentStop lifecycle)
#   AND the ambient pause flag is absent (Notification → next prompt/Stop).
# Lid close always turns off the display (when sleep is disabled).
# Lid open asks the listener to fade out (SIGTERM), then lets the return
# chime play while it ramps to silence and exits.
# Completion + auto-sleep is handled by the Stop hook — see claude-stop.sh.
# Pause-for-feedback ("help me") is handled by the Notification hook — see
# claude-notify.sh.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}

POLL=${POLL:-0.5}
SOUNDS="$SLAB_HOME/sounds"
start_wav="$SOUNDS/lid-start.wav"
return_wav="$SOUNDS/lid-return.wav"
open_ding="$SOUNDS/lid-open-ding.wav"
return_dur=2.5
AMBIENT_FLAG=/tmp/slab-ambient-active
# Set by claude-notify.sh when the agent pauses for user feedback. While
# present, ambient stays off even though an active marker exists. Cleared by
# claude-prompt-log.sh (user responded) or claude-stop.sh (work ended).
AMBIENT_PAUSE_FLAG=/tmp/slab-ambient-paused
# Persistent mute flag toggled by the menubar's "Mute ambient sonification"
# item. When present, ambient never starts and any in-flight pad is faded.
MUTE_FLAG="$SLAB_HOME/state/muted"

ACTIVE_DIR="$SLAB_HOME/state/active-prompts"
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"
mkdir -p "$ACTIVE_DIR" "$SUBAGENT_DIR"

PID_DIR=/tmp
reactive_pid_file="$PID_DIR/lidreactive.pid"
synth_pid_file="$PID_DIR/lidsynth.pid"
monitor_pid_file="$PID_DIR/slab-monitor.pid"
log="$SLAB_HOME/logs/lidalive.log"
mkdir -p "$(dirname "$log")"

reactive_py="$SLAB_HOME/venv/bin/python3"
reactive_script="$SLAB_BIN/lid-reactive.py"
synth_bin="$SLAB_BIN/lid-ambient-synth"

log_msg() { echo "$(date '+%Y-%m-%d %H:%M:%S') $*" >> "$log"; }

# Start the AVAudioEngine-backed ambient synth (Swift binary). It records
# its own output to $SLAB_HOME/sessions/ambient-<timestamp>.wav.
start_synth() {
    if [[ -x "$synth_bin" ]]; then
        nohup "$synth_bin" > /dev/null 2>&1 &
        echo $! > "$synth_pid_file"
        log_msg "started ambient synth pid $!"
    fi
}

start_reactive() {
    start_synth
    if [[ -x "$reactive_py" && -f "$reactive_script" ]]; then
        nohup "$reactive_py" "$reactive_script" > /dev/null 2>&1 &
        echo $! > "$reactive_pid_file"
        : > "$AMBIENT_FLAG"
        log_msg "started reactive listener pid $!"
    fi
}

# Ask the listener AND synth to fade out and exit (SIGTERM → graceful fade).
# Returns immediately; both take ~FADE_DUR seconds to actually exit.
fade_reactive() {
    "$SLAB_BIN/slab-fade-ambient"
}

# Hard-stop (safety net for cleanup paths).
stop_reactive() {
    fade_reactive
    pkill -f lid-reactive.py 2>/dev/null
    pkill -f lid-ambient-synth 2>/dev/null
}

start_monitor() {
    if [[ -x "$SLAB_BIN/slab-monitor.sh" ]]; then
        nohup "$SLAB_BIN/slab-monitor.sh" 15 > /dev/null 2>&1 &
        echo $! > "$monitor_pid_file"
        log_msg "started resource monitor pid $!"
    fi
}

stop_monitor() {
    local pid
    if [[ -f "$monitor_pid_file" ]]; then
        pid=$(cat "$monitor_pid_file" 2>/dev/null)
        [[ -n "$pid" ]] && kill "$pid" 2>/dev/null
        rm -f "$monitor_pid_file"
    fi
    pkill -f slab-monitor.sh 2>/dev/null
}

claude_running() {
    # Three shapes of "claude is running":
    #   1. compiled bundled CLI — process name is literally "claude"
    #   2. legacy node-based CLI — node .../@anthropic-ai/claude-code/cli.js
    #   3. desktop-app embed     — .../claude.app/Contents/MacOS/claude
    # pgrep -x catches (1) cheaply; the regex catches (2) and (3).
    pgrep -x claude >/dev/null 2>&1 \
        || ps -eo command 2>/dev/null | grep -qE 'claude\.app/Contents/MacOS/claude |@anthropic-ai/claude-code/.*cli\.js'
}

active_work_count() {
    shopt -s nullglob
    local p=("$ACTIVE_DIR"/*)
    local s=("$SUBAGENT_DIR"/*)
    shopt -u nullglob
    echo $((${#p[@]} + ${#s[@]}))
}

cleanup() {
    log_msg "daemon exiting, cleaning up"
    stop_reactive
    stop_monitor
    exit 0
}
trap cleanup SIGTERM SIGINT SIGHUP

log_msg "daemon starting (pid $$)"
stop_reactive
stop_monitor
start_monitor

prev_lid=""
ambient_running=0

while true; do
    lid_state=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
    sleep_disabled=$(pmset -g | awk '/SleepDisabled/{print $2; exit}')
    sleep_disabled=${sleep_disabled:-0}
    claude_alive=0
    claude_running && claude_alive=1
    active_count=$(active_work_count)
    # drop stale markers if no Claude process is around at all
    if (( claude_alive == 0 && active_count > 0 )); then
        rm -f "$ACTIVE_DIR"/* "$SUBAGENT_DIR"/* 2>/dev/null
        active_count=0
    fi

    # Lid just closed: darken display (ambient start is gated on Claude activity below).
    if [[ "$lid_state" == "Yes" && "$prev_lid" == "No" ]]; then
        log_msg "lid CLOSED (sleep_disabled=$sleep_disabled active=$active_count)"
        if [[ "$sleep_disabled" == "1" ]]; then
            sudo -n /usr/bin/pmset displaysleepnow 2>/dev/null &
        fi
    fi

    # Lid just opened: if ambient was playing, ding + return stinger AND
    # ask the reactive listener to fade out concurrently. The listener
    # ramps its master gain to zero over ~FADE_DUR seconds so ambient
    # settles softly beneath the return chime rather than cutting hard.
    if [[ "$lid_state" == "No" && "$prev_lid" == "Yes" ]]; then
        log_msg "lid OPENED (ambient_running=$ambient_running)"
        if (( ambient_running == 1 )); then
            fade_reactive
            "$SLAB_BIN/slab-afplay" "$open_ding"  &
            "$SLAB_BIN/slab-afplay" "$return_wav" &
            (sleep "$return_dur"
             cur=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
             if [[ "$cur" == "No" ]]; then
                 # Safety net — listener + synth should have already exited.
                 pkill -f lid-reactive.py 2>/dev/null
                 pkill -f lid-ambient-synth 2>/dev/null
                 log_msg "ambient + reactive finalized after return stinger"
             fi
            ) &
            ambient_running=0
        fi
    fi

    # Ambient gate: lid closed + sleep disabled + active prompt or subagent,
    # and NOT paused-for-feedback by a Notification hook, and NOT muted
    # via the menubar's "Mute ambient sonification" toggle.
    ambient_wanted=0
    if [[ "$lid_state" == "Yes" && "$sleep_disabled" == "1" ]] && (( active_count > 0 )) && [[ ! -f "$AMBIENT_PAUSE_FLAG" ]] && [[ ! -f "$MUTE_FLAG" ]]; then
        ambient_wanted=1
    fi

    if (( ambient_wanted == 1 && ambient_running == 0 )); then
        log_msg "ambient START (active=$active_count)"
        "$SLAB_BIN/slab-afplay" "$start_wav" &
        start_reactive
        ambient_running=1
    elif (( ambient_wanted == 0 && ambient_running == 1 )) && [[ "$lid_state" == "Yes" ]]; then
        # All active work finished or sleep was re-enabled while lid still closed.
        log_msg "ambient STOP (active=$active_count sleep_disabled=$sleep_disabled)"
        fade_reactive
        ambient_running=0
    fi

    prev_lid="$lid_state"
    sleep "$POLL"
done
