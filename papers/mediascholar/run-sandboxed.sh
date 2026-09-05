#!/usr/bin/env bash
# Execute the paper worker with a home view containing only public research
# input, the installed runtime, its private state, and its Prox advertisement.
set -euo pipefail

RUNTIME="${MEDIASCHOLAR_RUNTIME:-$HOME/.local/lib/mediascholar}"
REPO="${MEDIASCHOLAR_REPO:-$HOME/aesthetic-computer}"
STATE="${MEDIASCHOLAR_HOME:-$HOME/.local/share/mediascholar}"
ADVERTISE_DIR="$(dirname "${MEDIASCHOLAR_ADVERTISE_PATH:-$HOME/.config/slab/ledger/advertise/mediascholar.json}")"
RUNNER="$RUNTIME/papers/bin/mediascholar.mjs"

if ! command -v bwrap >/dev/null 2>&1; then
  if [[ "${MEDIASCHOLAR_ENABLED:-0}" != "1" ]]; then
    exec /usr/bin/env node "$RUNNER" run
  fi
  echo "Mediascholar refuses an enabled run without bubblewrap" >&2
  exit 75
fi

mkdir -p "$STATE" "$ADVERTISE_DIR"

declare -a root_paths=()
for source_path in /bin /sbin /lib /lib64; do
  if [[ -L "$source_path" ]]; then
    root_paths+=(--symlink "$(readlink "$source_path")" "$source_path")
  elif [[ -e "$source_path" ]]; then
    root_paths+=(--ro-bind "$source_path" "$source_path")
  fi
done

exec bwrap \
  --die-with-parent \
  --new-session \
  --unshare-pid \
  --unshare-ipc \
  --unshare-uts \
  --cap-drop ALL \
  --proc /proc \
  --dev /dev \
  --tmpfs /tmp \
  --ro-bind /usr /usr \
  "${root_paths[@]}" \
  --ro-bind /etc /etc \
  --dir /var \
  --ro-bind /var/lib /var/lib \
  --dir /var/cache \
  --ro-bind-try /var/cache/fontconfig /var/cache/fontconfig \
  --dir /home \
  --dir "$HOME" \
  --dir "$HOME/.local" \
  --dir "$HOME/.local/lib" \
  --dir "$HOME/.local/share" \
  --dir "$HOME/.config" \
  --dir "$HOME/.config/slab" \
  --dir "$HOME/.config/slab/ledger" \
  --ro-bind "$REPO" "$REPO" \
  --ro-bind "$RUNTIME" "$RUNTIME" \
  --bind "$STATE" "$STATE" \
  --bind "$ADVERTISE_DIR" "$ADVERTISE_DIR" \
  --setenv HOME "$HOME" \
  --setenv XDG_RUNTIME_DIR /tmp \
  --chdir "$REPO" \
  /usr/bin/env node "$RUNNER" run
