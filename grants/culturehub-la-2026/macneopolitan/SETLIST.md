# Setlist — the MacNeoPolitan Trio in the round

The show on the full room: three singing Macs, six AC OS laptops with their
own stems, notation and lights, the sub, the wedge. Drafted 2026-09-24 from
the pieces that have played on the system (Good morning, Sophia with echoes;
Femrag++) and the songbook's measured pieces. Lengths from the scores; gaps
are the runner's rests. Every Trio piece goes through the same planner
(voice bounce, harmonies, music box, pad, percussion, notation, wedge).

| # | piece | score | length | why here |
|---|---|---|---|---|
| 1 | Good morning, Sophia | trio-wake | 0:52 | the opener: frisbee's first word is her name; the room wakes with her |
| 2 | Open me | trio-open-me | 1:16 | call and response; "o · pen · me" passed around the ring |
| 3 | Spell Sophia | trio-spell-sophia | 0:52 | the first lesson |
| 4 | A B C | trio-abc | 1:10 | the letters climb the trio, then the ring |
| 5 | What starts with | trio-what-starts-with | 1:18 | seven rounds; ends on Sophia |
| 6 | Sums | trio-sums | 1:23 | question · answer · confirmation |
| 7 | Take away | trio-take-away | 1:27 | the mirror of Sums; counts itself down and stops |
| 8 | Thirteen days | trio-thirteen-days | 1:22 | blueberry's thirteen tones; the sub carries the drone |
| 9 | Vocalise | trio-vocalise | 1:15 | wordless; the room shakes one D chord at bar 20 |
| 10 | Femrag++ in the round | (folder: femrag-spatial) | 2:27 | the instrumental; the laptops dance, the trio rests |
| 11 | The record | trio-the-record | 1:02 | the numbers the machines keep (hocket by word, open question) |
| 12 | Lights out | trio-lights-out | 1:13 | "good night Sophia"; the closer |
| — | Lullaby for frisbee | trio-lullaby | 0:49 | the encore, if the room asks |

Running time ≈ 17:40 of music; with ~8 s rests ≈ 19:30. Stylings (2:46) is
the alternate for 9 if the room wants more texture; Spell frisbee (1:20)
swaps for 3 on a night frisbee is the guest.

## What the queue needs to run this without three-minute holes

Today a piece costs ~2–3 minutes between songs: the singers' phrases render
(30–60 s), stems mix, six seats upload ~50 MB each, the SUB loads. For a
live set, prepare the whole list before doors:

1. **Singers keep every preparation.** Menu Band retains one cache per
   preparation id; only `stop` clears them all. The runner's cleanup must not
   post `stop` to the singers after a run that ended by itself (the sequence
   ends on its own) — then all twelve pieces stay prepared through the show.
2. **Seats hold every piece.** Stem parts are already named by hash and
   persist on the device; store each song's config as
   `/pieces/trio-fleet-config-<hash>.json` and give the native piece a
   `switch` command that loads a config from disk and re-assembles its stem
   from the parts already there (seconds, no upload). Staging the set = one
   upload pass per song before the show.
3. **SUB holds every score.** Its `/api/trio/load` takes one score at a
   time; the queue loads the next song's score in the rest between pieces
   (the receiver re-reads on hash change).
4. **One `venue_enqueue` per row**, `announce` where a spoken bit belongs
   (the score's own `intro` lines can be spoken by the member instead),
   `gap` 8; `venue_autoplay on` at doors.

Between-song words: the scores' `intro`/`outro` lines are spoken by the
member in its cast voice; the wake and lights-out bookends could take a line
from `play.json`'s stage directions. Not decided.
