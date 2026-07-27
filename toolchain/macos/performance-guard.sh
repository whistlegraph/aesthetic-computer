#!/usr/bin/env bash
# Aggressive, low-overhead host pressure monitor for the AC prompt machine.
# Arbitrary hot processes are reported, never killed. The sole self-heal is
# duplicate Caddy instances validated by exact command and repo working dir.

set -u

SCRIPT_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
REPO="${AC_REPO:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
SYSTEM_DIR="${REPO}/system"
STATE_DIR="${HOME}/.local/share/slab/performance"
LOG_PATH="${STATE_DIR}/performance-guard.log"
LATEST_PATH="${STATE_DIR}/latest.txt"
BREACH_PATH="${STATE_DIR}/breach-count"
ALERT_PATH="${STATE_DIR}/last-alert-epoch"
SWAP_PATH="${STATE_DIR}/last-swapouts"
PRESSURE_FLAG="${STATE_DIR}/pressure-active"
LOCK_DIR="${TMPDIR:-/tmp}/computer.aesthetic.performance-guard-$(id -u).lock"
PLIST="${HOME}/Library/LaunchAgents/computer.aesthetic.performance-guard.plist"
LABEL="computer.aesthetic.performance-guard"
INTERVAL=30
REPAIR=0

usage() {
    echo "usage: performance-guard.sh [--once [--repair] | --watch [--repair] | --status | --install | --uninstall]"
}

number_or_zero() {
    case "${1:-}" in ''|*[!0-9.]*) echo 0 ;; *) echo "$1" ;; esac
}

validated_caddy_pids() {
    local pid command cwd
    pgrep -x caddy 2>/dev/null | while IFS= read -r pid; do
        command="$(ps -p "$pid" -o command= 2>/dev/null || true)"
        case "$command" in
            *"caddy run --config Caddyfile"*) ;;
            *) continue ;;
        esac
        cwd="$(lsof -a -p "$pid" -d cwd -Fn 2>/dev/null | sed -n 's/^n//p' | head -1)"
        [[ "$cwd" == "$SYSTEM_DIR" ]] && echo "$pid"
    done
}

repair_duplicate_caddy() {
    local pids="$1" keep pid repaired=0
    keep="$(printf '%s\n' "$pids" | awk 'NF' | sort -n | tail -1)"
    [[ -n "$keep" ]] || { echo 0; return; }
    while IFS= read -r pid; do
        [[ -n "$pid" && "$pid" != "$keep" ]] || continue
        kill -TERM "$pid" 2>/dev/null && repaired=$((repaired + 1))
    done <<<"$pids"
    sleep 0.25
    while IFS= read -r pid; do
        [[ -n "$pid" && "$pid" != "$keep" ]] || continue
        kill -0 "$pid" 2>/dev/null && kill -KILL "$pid" 2>/dev/null || true
    done <<<"$pids"
    echo "$repaired"
}

notify_pressure() {
    local message="$1"
    /usr/bin/osascript -e "display notification \"${message//\"/}\" with title \"AC performance guard\"" >/dev/null 2>&1 || true
}

rotate_log() {
    [[ -f "$LOG_PATH" ]] || return
    local bytes
    bytes="$(stat -f %z "$LOG_PATH" 2>/dev/null || echo 0)"
    if (( bytes > 5242880 )); then
        tail -1000 "$LOG_PATH" > "${LOG_PATH}.next"
        mv "${LOG_PATH}.next" "$LOG_PATH"
    fi
}

sample_once() {
    mkdir -p "$STATE_DIR"
    if ! mkdir "$LOCK_DIR" 2>/dev/null; then return 0; fi
    trap 'rmdir "$LOCK_DIR" 2>/dev/null || true' EXIT HUP INT TERM

    local timestamp cores load1 free_pct processes threads codex_count node_count swift_build_count
    local caddy_pids caddy_count repaired terminal_cpu window_cpu slab_cpu menuband_cpu
    local swapouts previous_swap swap_delta severe reason top_cpu top_mem breaches last_alert now
    timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    cores="$(number_or_zero "$(sysctl -n hw.logicalcpu 2>/dev/null)")"
    load1="$(number_or_zero "$(sysctl -n vm.loadavg 2>/dev/null | awk '{print $2}')")"
    free_pct="$(number_or_zero "$(memory_pressure 2>/dev/null | awk '/System-wide memory free percentage/{gsub(/%/,"",$5); print $5; exit}')")"
    processes="$(ps -A -o pid= 2>/dev/null | wc -l | tr -d ' ')"
    threads="$(ps -M -A -o pid= 2>/dev/null | wc -l | tr -d ' ')"
    codex_count="$(pgrep -x codex 2>/dev/null | wc -l | tr -d ' ')"
    node_count="$(pgrep -x node 2>/dev/null | wc -l | tr -d ' ')"
    swift_build_count="$(pgrep -x swift-build 2>/dev/null | wc -l | tr -d ' ')"
    caddy_pids="$(validated_caddy_pids)"
    caddy_count="$(printf '%s\n' "$caddy_pids" | awk 'NF{n++} END{print n+0}')"
    repaired=0
    if (( REPAIR == 1 && caddy_count > 1 )); then
        repaired="$(repair_duplicate_caddy "$caddy_pids")"
        caddy_count=$((caddy_count - repaired))
    fi

    terminal_cpu="$(ps -A -o %cpu=,comm= | awk '$2 ~ /\/Terminal$/ {s+=$1} END{printf "%.1f",s+0}')"
    window_cpu="$(ps -A -o %cpu=,comm= | awk '$2 ~ /\/WindowServer$/ {s+=$1} END{printf "%.1f",s+0}')"
    slab_cpu="$(ps -A -o %cpu=,comm= | awk '$2 ~ /\/slab-menubar$/ {s+=$1} END{printf "%.1f",s+0}')"
    menuband_cpu="$(ps -A -o %cpu=,comm= | awk '$2 ~ /\/MenuBand$/ {s+=$1} END{printf "%.1f",s+0}')"
    swapouts="$(number_or_zero "$(memory_pressure 2>/dev/null | awk '/Swapouts:/{print $2; exit}')")"
    previous_swap="$(number_or_zero "$(cat "$SWAP_PATH" 2>/dev/null || true)")"
    swap_delta=0
    if (( previous_swap > 0 && swapouts >= previous_swap )); then swap_delta=$((swapouts - previous_swap)); fi
    printf '%s\n' "$swapouts" > "$SWAP_PATH"

    severe=0
    reason=""
    awk -v l="$load1" -v c="$cores" 'BEGIN{exit !(l > c*1.5)}' && { severe=1; reason="load"; }
    awk -v f="$free_pct" 'BEGIN{exit !(f < 15)}' && { severe=1; reason="${reason:+$reason+}memory"; }
    awk -v t="$terminal_cpu" -v w="$window_cpu" 'BEGIN{exit !((t+w) > 125)}' && { severe=1; reason="${reason:+$reason+}display"; }
    (( swap_delta > 4096 )) && { severe=1; reason="${reason:+$reason+}swap"; }
    (( codex_count > 8 )) && { severe=1; reason="${reason:+$reason+}sessions"; }
    (( swift_build_count > 1 )) && { severe=1; reason="${reason:+$reason+}builds"; }
    (( caddy_count > 1 || repaired > 0 )) && { severe=1; reason="${reason:+$reason+}caddy"; }

    top_cpu="$(ps -A -o pid=,%cpu=,rss=,comm= | sort -k2 -nr | head -5 | tr '\n' ';')"
    top_mem="$(ps -A -o pid=,%cpu=,rss=,comm= | sort -k3 -nr | head -5 | tr '\n' ';')"
    {
        echo "timestamp=$timestamp"
        echo "load1=$load1 cores=$cores free_pct=$free_pct processes=$processes threads=$threads"
        echo "codex=$codex_count node=$node_count swift_builds=$swift_build_count caddy=$caddy_count caddy_repaired=$repaired swapout_pages_delta=$swap_delta"
        echo "terminal_cpu=$terminal_cpu windowserver_cpu=$window_cpu slab_cpu=$slab_cpu menuband_cpu=$menuband_cpu"
        echo "top_cpu=$top_cpu"
        echo "top_mem=$top_mem"
        echo "pressure=$severe reason=${reason:-none}"
    } > "${LATEST_PATH}.next"
    mv "${LATEST_PATH}.next" "$LATEST_PATH"

    if (( severe == 1 )); then
        : > "$PRESSURE_FLAG"
        rotate_log
        tr '\n' ' ' < "$LATEST_PATH" >> "$LOG_PATH"
        echo >> "$LOG_PATH"
        breaches="$(number_or_zero "$(cat "$BREACH_PATH" 2>/dev/null || true)")"
        breaches=$((breaches + 1))
        echo "$breaches" > "$BREACH_PATH"
        now="$(date +%s)"
        last_alert="$(number_or_zero "$(cat "$ALERT_PATH" 2>/dev/null || true)")"
        if (( repaired > 0 )); then
            notify_pressure "Removed ${repaired} duplicate AC Caddy processes."
            echo "$now" > "$ALERT_PATH"
        elif (( breaches >= 3 && now - last_alert >= 600 )); then
            notify_pressure "Sustained ${reason} pressure: load ${load1}, free memory ${free_pct}%."
            echo "$now" > "$ALERT_PATH"
        fi
    else
        rm -f "$PRESSURE_FLAG"
        echo 0 > "$BREACH_PATH"
    fi

    rmdir "$LOCK_DIR" 2>/dev/null || true
    trap - EXIT HUP INT TERM
}

install_guard() {
    mkdir -p "$HOME/Library/LaunchAgents" "$STATE_DIR"
    cat > "$PLIST" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key><array>
    <string>/bin/bash</string><string>$SCRIPT_PATH</string><string>--once</string><string>--repair</string>
  </array>
  <key>RunAtLoad</key><true/>
  <key>StartInterval</key><integer>$INTERVAL</integer>
  <key>ProcessType</key><string>Background</string>
  <key>LowPriorityIO</key><true/>
  <key>EnvironmentVariables</key><dict>
    <key>AC_REPO</key><string>$REPO</string>
  </dict>
  <key>StandardOutPath</key><string>$STATE_DIR/launchd.out</string>
  <key>StandardErrorPath</key><string>$STATE_DIR/launchd.err</string>
</dict></plist>
EOF
    plutil -lint "$PLIST"
    launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
    launchctl bootstrap "gui/$(id -u)" "$PLIST"
    echo "installed $LABEL (every ${INTERVAL}s)"
}

uninstall_guard() {
    launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
    rm -f "$PLIST"
    echo "uninstalled $LABEL; retained logs in $STATE_DIR"
}

case "${1:---once}" in
    --once)  [[ "${2:-}" == "--repair" ]] && REPAIR=1; sample_once ;;
    --watch) [[ "${2:-}" == "--repair" ]] && REPAIR=1; while true; do sample_once; sleep "$INTERVAL"; done ;;
    --status) if [[ -f "$LATEST_PATH" ]]; then cat "$LATEST_PATH"; else echo "no performance sample yet"; fi ;;
    --install) install_guard ;;
    --uninstall) uninstall_guard ;;
    -h|--help) usage ;;
    *) usage >&2; exit 2 ;;
esac
