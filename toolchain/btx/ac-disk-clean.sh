#!/usr/bin/env bash
# Safe disk inventory/cleanup for jastow.
#
# Default: report only. Nothing is removed without --apply.
set -euo pipefail

APPLY=0
INSTALL=0
PRUNE_VOLUMES=0

usage() {
  cat <<'EOF'
Usage: ac-disk-clean [--apply] [--prune-volumes] [--install]

  (no args)         Report disk use and cleanup candidates; change nothing.
  --apply           Clear only known regenerable caches.
  --prune-volumes   With --apply, also remove unused Docker volumes.
  --install         Install as /usr/local/bin/ac-disk-clean and enable its
                    weekly systemd timer.

Protected on every run: fleet-os-builds, VM images, ChromiumOS source/build
state, repositories, node_modules, Downloads, wallet/key material, and active
application caches.
EOF
}

while (($#)); do
  case "$1" in
    --apply) APPLY=1 ;;
    --install) INSTALL=1 ;;
    --prune-volumes) PRUNE_VOLUMES=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

if ((EUID != 0 && (APPLY || INSTALL))); then
  elevated_args=()
  ((APPLY)) && elevated_args+=(--apply)
  ((INSTALL)) && elevated_args+=(--install)
  ((PRUNE_VOLUMES)) && elevated_args+=(--prune-volumes)
  exec sudo -n "$(readlink -f "$0")" "${elevated_args[@]}"
fi

TARGET_USER="${SUDO_USER:-${USER:-me}}"
if [[ "$TARGET_USER" == root ]]; then
  TARGET_USER=me
fi
TARGET_HOME=$(getent passwd "$TARGET_USER" | cut -d: -f6)
TARGET_HOME=${TARGET_HOME:-/home/me}

as_root() {
  if ((EUID == 0)); then
    "$@"
  else
    sudo -n "$@"
  fi
}

human_size() {
  local path="$1"
  if [[ -e "$path" ]]; then
    as_root du -sh -- "$path" 2>/dev/null | awk '{print $1}' || echo "?"
  else
    echo "0"
  fi
}

disk_used_bytes() {
  df -B1 --output=used / | tail -1 | tr -d ' '
}

disk_percent() {
  df --output=pcent / | tail -1 | tr -dc '0-9'
}

active_build() {
  pgrep -af '(^|/)([c]c|[c]make|[g]cc|[g]\+\+|[c]lang|[c]lang\+\+|[r]ustc|[n]inja|[m]ake|[o]sbuild)( |$)|[d]ocker([^ ]* )+build' >/dev/null ||
    pgrep -f 'qemu-system.*fedora38-cros-builder\.qcow2' >/dev/null
}

report() {
  local build_state="idle"
  active_build && build_state="ACTIVE — protected"

  echo "jastow disk report — $(date -Is)"
  echo
  df -h / | awk 'NR == 1 || NR == 2'
  echo "Build workload: $build_state"
  echo
  printf '%-34s %9s  %s\n' "Path/category" "Size" "Policy"
  printf '%-34s %9s  %s\n' "----------------------------------" "---------" "------"
  printf '%-34s %9s  %s\n' "fleet-os-builds" "not scanned" "PROTECTED; deep scans are intentionally manual"
  printf '%-34s %9s  %s\n' "  ChromiumOS tree" "not scanned" "PROTECTED"
  printf '%-34s %9s  %s\n' "  builder VM image" "$(human_size "$TARGET_HOME/fleet-os-builds/vm-build/fedora38-cros-builder.qcow2")" "PROTECTED"
  printf '%-34s %9s  %s\n' "aesthetic-computer/.git" "$(human_size "$TARGET_HOME/aesthetic-computer/.git")" "PROTECTED"
  printf '%-34s %9s  %s\n' "PackageKit cache" "$(human_size /var/cache/PackageKit)" "safe when package manager idle"
  printf '%-34s %9s  %s\n' "DNF cache" "$(human_size /var/cache/dnf)" "safe when package manager idle"
  printf '%-34s %9s  %s\n' "osbuild caches" "$(human_size /var/cache/osbuild-worker) + $(human_size /var/cache/osbuild-composer)" "report only"
  printf '%-34s %9s  %s\n' "Chrome cache" "$(human_size "$TARGET_HOME/.cache/google-chrome")" "safe when Chrome idle"
  printf '%-34s %9s  %s\n' "Puppeteer cache" "$(human_size "$TARGET_HOME/.cache/puppeteer")" "safe when browser tooling idle"
  printf '%-34s %9s  %s\n' "GNOME Software cache" "$(human_size "$TARGET_HOME/.cache/gnome-software")" "safe when app idle"
  printf '%-34s %9s  %s\n' "npm content cache" "$(human_size "$TARGET_HOME/.npm/_cacache")" "safe when npm idle"
  printf '%-34s %9s  %s\n' "Electron/node-gyp caches" "$(human_size "$TARGET_HOME/.cache/electron") + $(human_size "$TARGET_HOME/.cache/node-gyp")" "safe when build idle"
  printf '%-34s %9s  %s\n' "system journal" "$(as_root journalctl --disk-usage 2>/dev/null | sed -n 's/.*take up \([^ ]*\).*/\1/p')" "capped at 500M"
  printf '%-34s %9s  %s\n' "Trash" "$(human_size "$TARGET_HOME/.local/share/Trash")" "safe"
  echo
  docker system df 2>/dev/null || true
  echo
  if ((APPLY == 0)); then
    echo "Report only. Run 'ac-disk-clean --apply' to clear safe caches."
  fi
}

clean_contents() {
  local path="$1"
  [[ -d "$path" ]] || return 0
  find "$path" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +
}

skip() {
  echo "  skip: $1"
}

apply_cleanup() {
  local before after reclaimed
  before=$(disk_used_bytes)
  echo "Applying safe cleanup on jastow..."

  journalctl --vacuum-size=500M >/dev/null
  systemd-tmpfiles --clean

  if pgrep -x packagekitd >/dev/null || pgrep -x dnf >/dev/null || pgrep -x rpm >/dev/null; then
    skip "package manager is active; PackageKit/DNF caches kept"
  else
    clean_contents /var/cache/PackageKit
    dnf clean all >/dev/null 2>&1 || true
  fi

  if pgrep -f '[g]oogle-chrome|/chrome ' >/dev/null; then
    skip "Chrome is active; Chrome cache kept"
  else
    clean_contents "$TARGET_HOME/.cache/google-chrome"
  fi

  if pgrep -f '[p]uppeteer|chrome-headless' >/dev/null; then
    skip "Puppeteer/browser tooling is active; Puppeteer cache kept"
  else
    clean_contents "$TARGET_HOME/.cache/puppeteer"
  fi

  if pgrep -x gnome-software >/dev/null; then
    skip "GNOME Software is active; its cache kept"
  else
    clean_contents "$TARGET_HOME/.cache/gnome-software"
  fi

  if active_build; then
    skip "build workload is active; npm/Electron/node-gyp caches kept"
  else
    clean_contents "$TARGET_HOME/.npm/_cacache"
    clean_contents "$TARGET_HOME/.cache/electron"
    clean_contents "$TARGET_HOME/.cache/node-gyp"
    clean_contents "$TARGET_HOME/.cache/ffmpeg-static-nodejs"
  fi

  clean_contents "$TARGET_HOME/.cache/Homebrew"
  clean_contents "$TARGET_HOME/.local/share/Trash/files"
  clean_contents "$TARGET_HOME/.local/share/Trash/info"

  docker image prune -af >/dev/null 2>&1 || true
  docker builder prune -af >/dev/null 2>&1 || true
  if ((PRUNE_VOLUMES)); then
    docker volume prune -f >/dev/null 2>&1 || true
  fi

  after=$(disk_used_bytes)
  reclaimed=$((before - after))
  ((reclaimed < 0)) && reclaimed=0
  echo "Cleanup complete: $(numfmt --to=iec-i --suffix=B "$reclaimed") reclaimed."
  df -h / | awk 'NR == 1 || NR == 2'
}

install_utility() {
  local source
  source=$(readlink -f "$0")
  if [[ "$source" != /usr/local/bin/ac-disk-clean ]]; then
    install -m 0755 "$source" /usr/local/bin/ac-disk-clean
  else
    chmod 0755 /usr/local/bin/ac-disk-clean
  fi

  cat >/etc/systemd/system/ac-disk-clean.service <<'EOF'
[Unit]
Description=jastow safe weekly disk cleanup
After=network.target

[Service]
Type=oneshot
ExecStart=/usr/local/bin/ac-disk-clean --apply
EOF

  cat >/etc/systemd/system/ac-disk-clean.timer <<'EOF'
[Unit]
Description=Run jastow safe disk cleanup weekly

[Timer]
OnCalendar=weekly
RandomizedDelaySec=30m
Persistent=true

[Install]
WantedBy=timers.target
EOF

  systemctl daemon-reload
  # Supersede the older unconditional weekly cache pruner. Keep the separate
  # 90%-full emergency disk guard installed by jas-nzxt-disk-guard.sh.
  systemctl disable --now disk-tidy.timer >/dev/null 2>&1 || true
  systemctl enable --now ac-disk-clean.timer
  echo "Installed /usr/local/bin/ac-disk-clean and enabled ac-disk-clean.timer."
}

if ((INSTALL)); then
  install_utility
  exit 0
fi

report
if ((APPLY)); then
  echo
  apply_cleanup
fi
