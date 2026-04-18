#!/bin/bash
# lid-ambient daemon. Long-lived via launchd KeepAlive; polls lid every POLL
# seconds. Ambient loop + reactive listener run only when:
#   lid is closed AND sleep is disabled AND at least one active marker exists
#   in $SLAB_HOME/state/active-prompts/ (UserPromptSubmit → Stop lifecycle)
#   or $SLAB_HOME/state/active-subagents/ (Task tool → SubagentStop lifecycle).
# Lid close always turns off the display (when sleep is disabled).
# Completion + auto-sleep is handled by the Stop hook — see claude-stop.sh.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}

POLL=${POLL:-0.5}
SOUNDS="$SLAB_HOME/sounds"
wav="$SOUNDS/ambient.wav"
start_wav="$SOUNDS/lid-start.wav"
return_wav="$SOUNDS/lid-return.wav"
open_ding="$SOUNDS/lid-open-ding.wav"
return_dur=2.5

ACTIVE_DIR="$SLAB_HOME/state/active-prompts"
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"
mkdir -p "$ACTIVE_DIR" "$SUBAGENT_DIR"

PID_DIR=/tmp
pid_file="$PID_DIR/lidambient.pid"
reactive_pid_file="$PID_DIR/lidreactive.pid"
monitor_pid_file="$PID_DIR/slab-monitor.pid"
log="$SLAB_HOME/logs/lidalive.log"
mkdir -p "$(dirname "$log")"

reactive_py="$SLAB_HOME/venv/bin/python3"
reactive_script="$SLAB_BIN/lid-reactive.py"

log_msg() { echo "$(date '+%Y-%m-%d %H:%M:%S') $*" >> "$log"; }

stop_player() {
    local pid
    if [[ -f "$pid_file" ]]; then
        pid=$(cat "$pid_file" 2>/dev/null)
        if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
            pkill -P "$pid" 2>/dev/null
            kill "$pid" 2>/dev/null
        fi
        rm -f "$pid_file"
    fi
    pkill -x afplay 2>/dev/null
}

start_player() {
    (while true; do /usr/bin/afplay "$wav"; done) > /dev/null 2>&1 &
    echo $! > "$pid_file"
    log_msg "started ambient loop pid $!"
}

start_reactive() {
    if [[ -x "$reactive_py" && -f "$reactive_script" ]]; then
        nohup "$reactive_py" "$reactive_script" > /dev/null 2>&1 &
        echo $! > "$reactive_pid_file"
        log_msg "started reactive listener pid $!"
    fi
}

stop_reactive() {
    local pid
    if [[ -f "$reactive_pid_file" ]]; then
        pid=$(cat "$reactive_pid_file" 2>/dev/null)
        [[ -n "$pid" ]] && kill "$pid" 2>/dev/null
        rm -f "$reactive_pid_file"
    fi
    pkill -f lid-reactive.py 2>/dev/null
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
    ps -eo command | grep -qE '^/Users/.*/claude\.app/Contents/MacOS/claude '
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
    stop_player
    stop_reactive
    stop_monitor
    exit 0
}
trap cleanup SIGTERM SIGINT SIGHUP

log_msg "daemon starting (pid $$)"
stop_player
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

    # Lid just opened: if ambient was playing, ding + return stinger, then stop.
    # If ambient wasn't running, stay silent.
    if [[ "$lid_state" == "No" && "$prev_lid" == "Yes" ]]; then
        log_msg "lid OPENED (ambient_running=$ambient_running)"
        if (( ambient_running == 1 )); then
            /usr/bin/afplay "$open_ding"  2>/dev/null &
            /usr/bin/afplay "$return_wav" 2>/dev/null &
            (sleep "$return_dur"
             cur=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
             if [[ "$cur" == "No" ]]; then
                 stop_player
                 stop_reactive
                 log_msg "ambient + reactive stopped after return stinger"
             fi
            ) &
            ambient_running=0
        fi
    fi

    # Ambient gate: lid closed + sleep disabled + active prompt or subagent.
    ambient_wanted=0
    if [[ "$lid_state" == "Yes" && "$sleep_disabled" == "1" ]] && (( active_count > 0 )); then
        ambient_wanted=1
    fi

    if (( ambient_wanted == 1 && ambient_running == 0 )); then
        log_msg "ambient START (active=$active_count)"
        /usr/bin/afplay "$start_wav" 2>/dev/null &
        start_player
        start_reactive
        ambient_running=1
    elif (( ambient_wanted == 0 && ambient_running == 1 )) && [[ "$lid_state" == "Yes" ]]; then
        # All active work finished or sleep was re-enabled while lid still closed.
        log_msg "ambient STOP (active=$active_count sleep_disabled=$sleep_disabled)"
        stop_player
        stop_reactive
        ambient_running=0
    fi

    prev_lid="$lid_state"
    sleep "$POLL"
done
