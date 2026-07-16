#!/usr/bin/env bash
# Keep BTX mining out of the way of interactive users and build workloads.
set -u

MINER_UNIT="btx-miner.service"
STATE_DIR="/var/lib/btx-miner-guard"
LAST_BUSY="$STATE_DIR/last-busy"
QUIET_SECONDS=600
QEMU_BUSY_PERCENT=1
QEMU_BUSY_IO_BYTES=$((1024 * 1024))

mkdir -p "$STATE_DIR"

stop_miner() {
  local reason="$1"
  date +%s >"$LAST_BUSY"
  if systemctl is-active --quiet "$MINER_UNIT"; then
    logger -t btx-miner-guard "pausing $MINER_UNIT: $reason"
    systemctl stop "$MINER_UNIT"
  fi
  exit 0
}

# Will may arrive through a named local account or a Tailscale identity shown by
# `who` in parentheses. Keep this deliberately narrow so ordinary maintenance
# sessions do not suppress mining.
if who | grep -Eiq '(^will([^[:alnum:]_]|$)|\([^)]*will[^)]*\))'; then
  stop_miner "Will is logged in"
fi

# Native build tools on the host. The bracketed first character keeps pgrep
# from matching its own command line.
if pgrep -af '(^|/)([c]c|[c]make|[g]cc|[g]\+\+|[c]lang|[c]lang\+\+|[r]ustc|[n]inja|[m]ake|[o]sbuild)( |$)|[d]ocker([^ ]* )+build' >/dev/null; then
  stop_miner "a compiler or build tool is running"
fi

# ChromeOS builds run inside the long-lived Fedora QEMU VM. Sample its CPU use
# instead of treating an idle VM as a build.
mapfile -t qemu_pids < <(pgrep -f 'qemu-system.*fedora38-cros-builder\.qcow2' || true)
if ((${#qemu_pids[@]})); then
  clk_tck=$(getconf CLK_TCK)
  declare -A before_cpu before_io
  for pid in "${qemu_pids[@]}"; do
    if [[ -r "/proc/$pid/stat" ]]; then
      read -r utime stime < <(awk '{print $14, $15}' "/proc/$pid/stat")
      before_cpu[$pid]=$((utime + stime))
      before_io[$pid]=$(awk '/^(read_bytes|write_bytes):/ {total += $2} END {print total + 0}' "/proc/$pid/io" 2>/dev/null)
    fi
  done
  sleep 5
  for pid in "${qemu_pids[@]}"; do
    if [[ -n "${before_cpu[$pid]:-}" && -r "/proc/$pid/stat" ]]; then
      read -r utime stime < <(awk '{print $14, $15}' "/proc/$pid/stat")
      cpu_delta=$((utime + stime - before_cpu[$pid]))
      cpu_percent=$((cpu_delta * 100 / (clk_tck * 5)))
      io_now=$(awk '/^(read_bytes|write_bytes):/ {total += $2} END {print total + 0}' "/proc/$pid/io" 2>/dev/null)
      io_delta=$((io_now - before_io[$pid]))
      if ((cpu_percent >= QEMU_BUSY_PERCENT || io_delta >= QEMU_BUSY_IO_BYTES)); then
        stop_miner "ChromeOS build VM is active (${cpu_percent}% CPU, ${io_delta} I/O bytes)"
      fi
    fi
  done
fi

# Do not bounce the GPU miner back on between short compiler phases.
now=$(date +%s)
if [[ -f "$LAST_BUSY" ]]; then
  last_busy=$(cat "$LAST_BUSY" 2>/dev/null || echo "$now")
  if ((now - last_busy < QUIET_SECONDS)); then
    exit 0
  fi
fi

if ! systemctl is-active --quiet "$MINER_UNIT"; then
  logger -t btx-miner-guard "starting $MINER_UNIT after ${QUIET_SECONDS}s quiet"
  systemctl start "$MINER_UNIT"
fi
