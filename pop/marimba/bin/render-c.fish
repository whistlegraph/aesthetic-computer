#!/usr/bin/env fish
# render-c.fish — the ONE-COMMAND fluttabap360 pipeline (C-canonical).
#
# JS composes + bakes the score; the C engine (pop/marimba/c/fluttabap360.c)
# does ALL the audio. This wraps the three steps into one:
#   1. bake  → out/fluttabap360.score.txt   (events + section map)
#   2. album → out/fluttabap360.mp3 + .distrokid.wav   (C engine)
#   3. loop  → out/fluttabap360-loop.mp3 + .distrokid.wav   (C, seamless)
#
# Usage:
#   pop/marimba/bin/render-c.fish              # bake + album + loop
#   pop/marimba/bin/render-c.fish --no-loop    # skip the seamless loop
#   pop/marimba/bin/render-c.fish --play       # open the album cut when done

set -l here (dirname (status filename))
set -l root (cd $here/../../..; pwd)
set -l mjs  $root/pop/marimba/bin/render-fluttabap360.mjs
set -l runc $root/pop/marimba/c/run-c.mjs
set -l out  $root/pop/marimba/out
set -l score $out/fluttabap360.score.txt

set -l do_loop 1
set -l do_play 0
for a in $argv
    switch $a
        case --no-loop; set do_loop 0
        case --play;    set do_play 1
    end
end

echo "▸ baking score (JS compose) …"
node $mjs --bake $score --no-open; or exit 1

echo "▸ album cut (C engine) …"
node $runc $score --out $out/fluttabap360.mp3 --wav; or exit 1

if test $do_loop -eq 1
    echo "▸ seamless loop (C engine) …"
    node $runc $score --out $out/fluttabap360-loop.mp3 --loop --wav; or exit 1
end

echo ""
echo "✓ fluttabap360 (C-canonical) →"
echo "    $out/fluttabap360.mp3        (+ .distrokid.wav)"
if test $do_loop -eq 1
    echo "    $out/fluttabap360-loop.mp3   (+ .distrokid.wav, seamless)"
end

if test $do_play -eq 1
    open -a "QuickTime Player" $out/fluttabap360.mp3
end
