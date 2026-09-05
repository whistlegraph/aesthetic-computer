# imab — "i'm a butterfly" · the dance track

The club cousin of the flutterbap family (`pop/marimba/`). Where
flutterbap skips through the park, imab goes out at night: a
four-on-the-floor dance record sung around one hook — *i'm a
butterfly* — with the "i-MAB" stamp as its chant.

Built bottom-up from AC instruments (marimba modal synth, perc synth,
the clubber360 floor recipes) — never generated wholesale. Vocals ship
aesthetivoxed, never raw.

## musicality (locked unless we relock it)

- **BPM 124** — the flutterbap family tempo; beat ≈ 0.4839 s, bar ≈ 1.9355 s.
- **Key: A minor verses / C major hook** (relative pair). The hook
  quotes flutterbap's opening cell (E5 G5 C6) exactly.
- **Hook progression:** `Am | Am | G | G | C | C | F | G` (8-bar loop).
  Bass roots A2 A2 G2 G2 C3 C3 F2 G2.

### the hook, syllable by syllable (CAPS = held)

One line per bar; beats in parentheses. Score doc: `imab.np`.

```
1  i'm(E5 @1)  a(G5 @1.5)  BUT(C6 @2, 1.5 beats)  ter(B5 @3.5)  fly(G5 @4)
2  …rest…                              i(A4 @4)  MAB(E5 @4.5)     ← pickup
3  i'm(D5 @1)  a(G5 @1.5)  BUT(B5 @2, 1.5 beats)  ter(A5 @3.5)  fly(G5 @4)
4  …rest…                              i(A4 @4)  MAB(E5 @4.5)
5  i'm(E5 @1)  a(G5 @1.5)  BUT(C6 @2, 1.5 beats)  ter(D6 @3.5)  fly(E6 @4)   ← the lift
6  FLY(E6 @1, 2 beats)  →  (C6 @3, 1.5 beats)
7  no(E6 @1) thing(D6 @1.5) holds(C6 @2) me(G5 @2.5) DOWN(A5 @3, 1.5 beats)
8  i(A4 @1) MAB(E5 @1.5, held)   i(A4 @3) MAB(E5 @3.5, held)     ← the chant
```

Lineage: bars 1/5 = flutterbap bar 1 (E5 G5 C6); bar 3 = flutterbap
bar 3 (D5 G5 B5); bar 7 = "nothing holds me" from lately-when-i-fly
(E6 D6 C6 G5).

## tools

```bash
node pop/imab/bin/hitbaker.mjs         # the CHART arrangement (single-study calibrated):
                                       #   104-bar verse/pre/chorus form, G-pedal break,
                                       #   terminal-lift finale → out/imab-hitbaker-demo1
                                       #   vocal doors: choruses at bars 24 / 56 / 76 (A = 24)
node pop/imab/bin/fetch-real-kit.mjs   # CC0 Freesound drum one-shots → samples/real/
                                       #   (hitbaker prefers these; provenance in
                                       #   kit-real.json + the pop menu sample registry)
node pop/imab/bin/gen-click.mjs        # click + guide → out/ (juke-sync picks mp3s up)
node pop/imab/bin/gen-kit.mjs          # one-shot sample kit → samples/kit/
node pop/imab/bin/lyrictrack.mjs       # canonical lyric timing: drawn boundaries →
                                       #   stem + floor JSONs + grid-fit proof mp4
pop/.venv/bin/python pop/imab/bin/lyricscroll.py
                                       # the scrolling review clip: alignment-video
                                       #   chassis over the real floor mix, lyric
                                       #   ribbon underneath (needs lyrictrack first)
```

- **out/imab-click-124** — 2-bar count-in, then 64 bars. Beat 1
  accented, brighter door tick at every 8-bar phrase start.
- **out/imab-guide-124** — the same click with the hook melody
  (xylophone) and bass roots looping, for practicing and cutting takes.

## recording performances

1. Record at 48 kHz against the click or guide (one ear).
2. Drop files in `performances/` named `imab-take-NN-<desc>.wav`.
3. Log every take in `performances/manifest.json` immediately — a take
   that isn't logged doesn't exist. Fields:
   `{ id, file, date, kind (vocal|whistle|clap|beatbox|other),
      content, clickUsed, keeper, notes }`.
4. Raw takes never ship: vocal comps go through the aesthetivox chain
   (`pop/bin/autotune.py`, A minor) before they touch a mix.

## aesthetivox-alignment-video

`bin/aesthetivox-alignment-video.py` — the named feature: a piano-roll
study mp4 (syllable blocks at sung pitch with their own waveform
envelopes printed inside, numbered bars, per-beat tinted grid, kick
floor, scrolling waveform, fixed playhead) over the click+kick+vocal
study audio. The alignment scrutiny instrument for every aesthetivox
vocal. Display latency is calibrated with loner's synccal.mp4; set
SYNC_MS to the value that locks.

## samples

`samples/kit/` — sample-free one-shots rendered from the AC engines
(perc kit, bass-perc, xylophone hook notes, click ticks), manifest in
`kit.json`. Chops of logged performances go in `samples/perf/` with
their own manifest, named after the take they came from.

## the dance track (build plan)

Carrying the loner/clubber360 learnings:

- everything on ONE grid; stretches only at exact 0.5×/0.25× ratios
- rhythmic narrative as a palindrome — the floor assembles act by act,
  reduces to a seed in the kickless break, returns whole at the drop,
  peels in reverse; kick turn-figures before doors; click-rush doors
- sidechain pump on melodic layers keyed to the kick; phrase rests
  (last half-bar of each 8-bar phrase floors the decorative layers —
  kick, bass and the voice are never gated)
- vocal-keyed wub swelling only in the gaps the voice leaves
- master: cut-wax material chain posture; MEASURE → one static dB →
  true-peak limit; −11.5 juke print, −13.5 release
- build pipeline lives in `bin/` in the repo, work dir in
  `~/.cache/ac/imab` — never in a scratchpad
