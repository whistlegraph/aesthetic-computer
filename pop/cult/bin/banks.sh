#!/usr/bin/env bash
# banks.sh — rebuild the derived sample banks this record needs and git does
# not carry (pop/**/samples and sung are ignored). Run after a fresh clone,
# before render10.mjs or c/cultremix. Idempotent; ~10 s. The commands are
# the ones the takes were actually made with (MIX-NOTES-2026-09-01.md).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; LANE="$(dirname "$HERE")"; POP="$(dirname "$LANE")"
PY="$POP/.venv/bin/python"; [ -x "$PY" ] || PY=python3

echo "→ the dinner bell (FEM bell engine)"
[ -x "$POP/bell/c/bell" ] || bash "$POP/bell/c/build.sh" >/dev/null
[ -f "$LANE/samples/gong-b6.wav" ] || "$POP/bell/c/bell" --note B6 --material bronze --geometry handbell --dur 26 --vel 0.5 --sr 48000 --out "$LANE/samples/gong-b6.wav" >/dev/null

echo "→ the aesthetivoxed takes (WORLD chain)"
sing() { [ -f "$LANE/sung/$2.wav" ] || "$PY" "$HERE/sing.py" "$LANE/$1" "$LANE/sung/$2.wav" "${@:3}" 2>/dev/null | tail -1; }
sing samples/dotorg.wav          dotorg-long          --notes "B2:1.00,F#2:0.80,B1:1.60" --vibrato-cents 14 --vibrato-onset-ms 700 --formant-db 1.4 --attack-ms 30 --release-ms 400
sing sung/iwanna-a-sung.wav      iwannalong-a         --notes "D4:0.55,E4:1.15" --vibrato-cents 22 --vibrato-onset-ms 380 --attack-ms 40 --release-ms 220
sing sung/iwanna-c-sung.wav      iwannalong-c         --notes "B3:0.55,C#4:1.20" --vibrato-cents 22 --vibrato-onset-ms 380 --attack-ms 40 --release-ms 220
sing sung/runrealfast-hi.wav     runrealfast-fastlong --notes "G4:0.32,F#4:0.30,D4:1.70" --vibrato-cents 20 --vibrato-onset-ms 500 --formant-db 1.2 --attack-ms 12 --release-ms 260

echo "→ the guitars (strum machine)"
[ -x "$POP/guitar/c/strum" ] || bash "$POP/guitar/c/build.sh" >/dev/null
S="$POP/guitar/c/strum"; B="$LANE/samples"
render() { local out="$B/$1.wav"; shift; [ -f "$out" ] || "$S" "$@" --sr 48000 --out "$out" >/dev/null 2>&1 || echo "! failed: $out"; }
for c in "bm:47,54,59,62,66" "d:50,57,62,66" "g:43,47,50,55,59,67" "em:40,47,52,55,59,64" "a:45,52,57,61,64" "fsm:42,49,54,57,61,66"; do n=${c%%:*}; m=${c##*:}
  render gt-folk-$n  --chord $m --pattern "D..d..u.u.D..u.." --bpm 120 --bars 1 --acoustic --rake 22 --force 0.62 --up 4 --human 0.5 --tail 0.9
  render gt-palm-$n  --chord $m --pattern "DdDdDdDdDdDdDdDx" --bpm 120 --bars 1 --electric --mute palm --drive 0.6 --force 0.7 --human 0.4 --tail 0.5
  render gt-rock-$n  --chord $m --pattern "D..d..u.u.D..u.." --bpm 120 --bars 1 --electric --drive 0.7 --rake 16 --force 0.8 --up 4 --human 0.5 --tail 0.9
  render gt-rockx-$n --chord $m --pattern "D..d..u.x.D..u.." --bpm 120 --bars 1 --electric --drive 0.7 --rake 16 --force 0.8 --up 4 --human 0.5 --tail 0.9
  render gt-up-$n    --chord $m --pattern "........u......." --bpm 120 --bars 1 --acoustic --up 3 --force 0.4 --human 0.3 --tail 1.2
done
render gt-stroke-bm --chord 47,54,59,62 --pattern "D..............." --bpm 120 --bars 1 --electric --drive 0.75 --force 0.95 --rake 12 --tail 1.6
render gt-pickup-bm --chord 47,54,59,62,66 --pattern "............u..." --bpm 120 --bars 1 --acoustic --up 3 --force 0.45 --tail 1.0
render gt-flower    --chord "47,54,61,66|43,50,54,59|40,47,54,62|42,49,54,59" --pattern "..D...u.d.U..u.." --bpm 120 --bars 4 --acoustic --rake 24 --force 0.55 --up 4 --human 0.5 --tail 1.0
echo "✓ banks"
