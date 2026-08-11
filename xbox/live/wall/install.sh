#!/usr/bin/env bash
# Installs the oskiewar wall on the machine it is run on, and stands the previous
# dashboards down.
#
# Run it from the directory this script lives in, with wall.html beside it:
#   scp -r xbox/live/wall xbox/live/wall.html host:/tmp/oskiewar-wall-install/
#   ssh host 'bash /tmp/oskiewar-wall-install/wall/install.sh'
#
# Nothing is deleted. The cockpit autostarts are renamed aside, so putting them
# back is one `mv` each — see the summary this prints at the end.
set -euo pipefail

HERE="$(cd -- "$(dirname "$0")" && pwd)"
ROOT="$HOME/.local/share/oskiewar-wall"
BIN="$HOME/.local/bin"
AUTOSTART="$HOME/.config/autostart"
UNITS="$HOME/.config/systemd/user"

mkdir -p "$ROOT" "$BIN" "$AUTOSTART" "$UNITS"

# wall.html may sit beside this script or one directory up, depending on how it
# was copied over.
for candidate in "$HERE/wall.html" "$HERE/../wall.html"; do
  [ -f "$candidate" ] && cp "$candidate" "$ROOT/wall.html" && break
done
[ -f "$ROOT/wall.html" ] || { echo "install: wall.html not found next to $HERE" >&2; exit 1; }

install -m 755 "$HERE/oskiewar-wall" "$BIN/oskiewar-wall"
install -m 644 "$HERE/oskiewar-wall-serve.service" "$UNITS/oskiewar-wall-serve.service"
# The autostart entry needs an absolute path, so the user's home is baked in here
# rather than left as a %h the desktop spec will not expand in Exec.
sed "s#^Exec=.*#Exec=$BIN/oskiewar-wall#" "$HERE/oskiewar-wall.desktop" \
  > "$AUTOSTART/oskiewar-wall.desktop"

# --- stand the old dashboards down, reversibly ---
restored=()
for entry in fleet-cockpit btx-fleet-cockpit; do
  if [ -f "$AUTOSTART/$entry.desktop" ]; then
    mv "$AUTOSTART/$entry.desktop" "$AUTOSTART/$entry.desktop.disabled"
    restored+=("mv $AUTOSTART/$entry.desktop.disabled $AUTOSTART/$entry.desktop")
  fi
done
# Close what is on the monitors right now. The cockpit is two xterms plus a
# foot window; none of them own any state worth saving.
pkill -f "/opt/fleet-cockpit/fleet_tui.py" 2>/dev/null || true
pkill -f "app-id=btx-cockpit" 2>/dev/null || true
pkill -f "/usr/local/bin/fleet-cockpit" 2>/dev/null || true
pkill -f "python3 /home/$USER/fleet-cockpit.py" 2>/dev/null || true
# The collector is a system timer; stopping it needs root, and it is only writing
# a JSON file, so a machine without passwordless sudo just leaves it running.
if sudo -n true 2>/dev/null; then
  sudo systemctl disable --now fleet-cockpit-collect.timer 2>/dev/null || true
  restored+=("sudo systemctl enable --now fleet-cockpit-collect.timer")
fi

systemctl --user daemon-reload
systemctl --user enable --now oskiewar-wall-serve.service

echo
echo "oskiewar wall installed."
echo "  page      $ROOT/wall.html"
echo "  launcher  $BIN/oskiewar-wall"
echo "  server    systemctl --user status oskiewar-wall-serve"
echo "  autostart $AUTOSTART/oskiewar-wall.desktop"
if [ ! -f "$ROOT/wall.key" ]; then
  echo
  echo "  NOTE: no $ROOT/wall.key — the reel block will read as locked."
  echo "        echo '<key>' > $ROOT/wall.key   then rerun the launcher."
fi
echo
echo "to put the old dashboards back:"
for line in "${restored[@]}"; do echo "  $line"; done
