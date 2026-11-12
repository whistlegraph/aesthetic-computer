#!/usr/bin/env bash
# Push the wallpaper source to the Fuser minis at once, compile on each,
# swap the binary into the installed app and restart it.
#
#   ./push-minis.sh                  # panda + chicken
#   ./push-minis.sh panda            # one machine
#   ./push-minis.sh --feeder         # also ship the vault's fishery feeder
#
# Each machine keeps its previous Sources/ as Sources.prev/ and its previous
# binary as BlueberryWallpaper.prev inside the app, so `--rollback` undoes the
# last push. The minis' build dir is ~/Developer/blueberry-wallpaper-build.
set -uo pipefail
cd "$(dirname "$0")"

FEEDER=0 ROLLBACK=0 HOSTS=()
for a in "$@"; do
  case "$a" in
    --feeder) FEEDER=1 ;;
    --rollback) ROLLBACK=1 ;;
    -*) echo "unknown flag $a" >&2; exit 2 ;;
    *) HOSTS+=("$a") ;;
  esac
done
[ ${#HOSTS[@]} -eq 0 ] && HOSTS=(panda chicken)

VAULT="${AC_VAULT:-$(cd ../../.. && pwd)/aesthetic-computer-vault}"
FEEDER_SRC="$VAULT/fuser/bin/fuser-fishery/fishery.mjs"
if [ $FEEDER -eq 1 ] && [ ! -f "$FEEDER_SRC" ]; then echo "no feeder at $FEEDER_SRC" >&2; exit 2; fi

SSH=(ssh -o ConnectTimeout=30 -o ServerAliveInterval=10 -o BatchMode=yes)
LOGDIR="$(mktemp -d)"

push() {
  local host="$1"
  if [ $ROLLBACK -eq 1 ]; then
    "${SSH[@]}" "$host" bash -s <<'REMOTE'
set -e
B=~/Developer/blueberry-wallpaper-build APP=~/Applications/BlueberryWallpaper.app
[ -d $B/Sources.prev ] && rm -rf $B/Sources && mv $B/Sources.prev $B/Sources
[ -f $APP/Contents/MacOS/BlueberryWallpaper.prev ] && mv $APP/Contents/MacOS/BlueberryWallpaper.prev $APP/Contents/MacOS/BlueberryWallpaper
codesign --force --options runtime --sign - $APP 2>/dev/null
launchctl kickstart -k gui/$(id -u)/computer.aesthetic.blueberry-wallpaper
echo "rolled back"
REMOTE
    return
  fi
  "${SSH[@]}" "$host" 'B=~/Developer/blueberry-wallpaper-build; mkdir -p $B/build/next && rm -rf $B/Sources.prev && cp -R $B/Sources $B/Sources.prev' || return 1
  rsync -a --delete -e "${SSH[*]}" Sources/ "$host:Developer/blueberry-wallpaper-build/Sources/" || return 1
  rsync -a -e "${SSH[*]}" swap.sh "$host:Developer/blueberry-wallpaper-build/swap.sh" || return 1
  # swap.sh replaces only the binary, so runtime files go where the app looks.
  "${SSH[@]}" "$host" 'mkdir -p ~/.config/blueberry-wallpaper'
  rsync -a -e "${SSH[*]}" Resources/unifont.otf "$host:.config/blueberry-wallpaper/unifont.otf" || return 1
  if [ $FEEDER -eq 1 ]; then
    "${SSH[@]}" "$host" 'cp ~/.hermes/bin/fuser-fishery.mjs ~/.hermes/bin/fuser-fishery.mjs.prev 2>/dev/null; true'
    rsync -a -e "${SSH[*]}" "$FEEDER_SRC" "$host:.hermes/bin/fuser-fishery.mjs" || return 1
    "${SSH[@]}" "$host" 'launchctl kickstart -k gui/$(id -u)/studio.fuser.fishery'
  fi
  # The guard refuses a compile while the machine is under pressure.
  "${SSH[@]}" "$host" 'export PATH=$HOME/.local/bin:/opt/homebrew/bin:$PATH
    cd ~/Developer/blueberry-wallpaper-build
    if command -v ac-performance-guard >/dev/null; then ac-performance-guard --admit "wallpaper swap" "$PWD" || exit 1; fi
    APP=~/Applications/BlueberryWallpaper.app
    cp $APP/Contents/MacOS/BlueberryWallpaper $APP/Contents/MacOS/BlueberryWallpaper.prev 2>/dev/null
    ./swap.sh && sleep 4 && pgrep -fl BlueberryWallpaper.app >/dev/null && echo "running · $(grep frames ~/.local/share/blueberry-wallpaper/stdout.log | tail -1)"'
}

for h in "${HOSTS[@]}"; do
  ( push "$h" > "$LOGDIR/$h.log" 2>&1; echo $? > "$LOGDIR/$h.rc" ) &
done
wait

fail=0
for h in "${HOSTS[@]}"; do
  rc=$(cat "$LOGDIR/$h.rc")
  if [ "$rc" = 0 ]; then echo "✓ $h  $(tail -1 "$LOGDIR/$h.log")"
  else fail=1; echo "✗ $h (exit $rc)"; sed 's/^/    /' "$LOGDIR/$h.log" | tail -15; fi
done
rm -rf "$LOGDIR"
exit $fail
