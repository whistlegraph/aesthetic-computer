# notespatial — the ring, heard from the middle

Two records live here: the binaural print of the whole suite (below), and
**climbalift**, the Climb and the Lift cut out of it and rebuilt as a club
record — see [CLIMBALIFT.md](CLIMBALIFT.md).

**A suite in eleven chapters for five laptops in a ring and one held at the
center, printed binaurally from the listener's seat.** 12:54 · instrumental ·
**headphones**. On speakers the room folds to a stereo sketch; in headphones
the theme walks around your head.

```bash
bash pop/notespatial/bin/render.sh        # print → master → out/notespatial-MASTER.flac
```

## What it is

*Note(s)pat(ial) Native* is the spatial score written for the September 24,
2026 CultureHub LA performance: `fedac/native/scores/notespatial-native.nsscore`,
composed by `fedac/native/tools/compose-notespatial-native.mjs`. Six salvaged
laptops run AC OS; seats 1 to 5 sit in a pentagon around the audience (seat 1
front, numbers clockwise) and the sixth is in the performer's hands at the
center on a small speaker. A Windows sub under seat 1 carries what a laptop
cannot. The arrangement, chapter by chapter, is
`grants/culturehub-la-2026/NOTESPATIAL-ARRANGEMENT.md`; the reading behind
it is `papers/chamber-platter/`.

The release is not a recording of the room. It is the score rendered by the
studio's model of the room — `fedac/native/tools/notespatial-native-render.mjs`
— every seat's feed synthesized with the runtime's own envelopes and the
gm_synth core, run through a mono model of the seat effects, and placed
around a listener at the center with measured KEMAR head-related transfer
functions (`fedac/native/tools/hrir/`). Seat energies are equalized so only the
direction cues survive (delay, spectrum, left–right balance). The sub feed
sits front at the render's default level (0.25, 80 Hz low-pass).

| | Chapter | Starts | Key · meter · tempo | Where it is |
|---|---|---|---|---|
| I | Overture | 0:00 | free, pulse 55→136 | center, then a circle drawn faster |
| II | The Walk | 1:00 | C · 4/4 · 100→104 | front–back axis, an opposing line |
| III | Waltz | 1:56 | A minor · 3/4 · 132→112 | a sway; melody circling under a held line |
| IV | Chase | 3:15 | F · 4/4 · 152→200 | two runners circling |
| V | Sneak | 4:03 | C minor · 4/4 · 84 | one seat at a time, behind you |
| VI | Lullaby | 5:01 | F · 6/8 · 72 | a slow cradle; the room revolves |
| VII | The Climb | 6:15 | C · 4/4 · 112→124 | two directions, then percussion alone |
| VIII | The Lift | 7:21 | D · 4/4 · 120→124→118 | everything, spinning; then one chord |
| IX | Fanfare | 9:14 | G · 4/4 · 132 | pairs across the room |
| X | Return | 9:52 | C · 4/4 · 104→88 | the ring closes to the front, then the center |
| XI | Vanish | 11:53 | free, pulse 60 | a circle drawn backwards, a point, one knock |

## The print and the master

`bin/render.sh` is two steps, both deterministic:

1. **Print.** `notespatial-native-render.mjs --audio-only --sub --float` →
   `out/notespatial-binaural-sub-f32.wav` (32-bit float, 44.1 kHz, peak 0.95).
   The `--float` flag was added for this release; the tool's 16-bit print is
   for review, not mastering.
2. **Master.** `bin/master.sh <print> 3 out/notespatial-MASTER.flac` — the
   house law (`pop/MASTERING.md`): 20 Hz subsonic high-pass, bass mono below
   120 Hz (the side channel high-passed at 120 Hz), **one static +3 dB**, a
   4× oversampled true-peak limiter (6 ms / 90 ms, ASC, ceiling −2.2 dBTP),
   24-bit FLAC at the project rate. Nothing rides the song; the composed
   level arc (0.3 → 0.8 → 0.3) is the record's dynamics.

The gain was chosen by measuring the limiter's work against a limiter-less
pass of the same chain: at +3 dB it reduces more than 2 dB for 2.2 s of the
13 minutes and peaks at 4.5 dB once, on the Lift's tutti D chord (9:08).
At +4 dB that grows to 5.1 s and 5.5 dB; at +2.5 dB it is 1.3 s and 4.0 dB.

Master: **−16.7 LUFS · −2.1 dBTP · LRA 15.7 · PLR 14.6** · 775.9 s. That is
quiet against the pop catalog on purpose. A 13-minute chamber suite whose
chapters sit between −25 and −15 LUFS is not a single to press to −10;
the ambient exception in `MASTERING.md` applies. Mono fold on the loudest
8 s loses 2.1 dB (gate: 1.5) — the binaural placement is the composition,
as with `pop/bracelet/`; phone proxy loses 2.9 dB (gate: 5), fine.

## Cover

`out/notespatial-cover-dark-3000.jpg` — the render tool's own isometric
view of the room at 8:35, inside the Lift's eight-turn spin: the pentagon of
laptops lit by what they are playing, the held laptop at the center, a kick,
and the notes flying in along their radials. Rendered at 1500², cropped to
the ring, scaled ×3 nearest-neighbor. A light-theme sibling sits beside it.
No AI.

## Files

- `bin/render.sh` · `bin/master.sh` — the whole record from the repo
- `out/` (gitignored) — print, candidates, master, preview mp3, covers, audit
- Release ledger: `pop/RELEASES.md`; public record: `pop/releases/notespatial/release.json`;
  DistroKid packet: `~/Documents/Shelf/notespatial-DISTROKID/`
