# climbalift — the Climb and the Lift, rebuilt as a club record

**Status 2026-09-25: v15 in progress, not released.** `bin/climbalift.mjs` is the
whole score and renderer; `bin/master-v2.sh` the master; `bin/score-video.mjs`
the analysis mp4; `bin/wub-batch.mjs` / `bin/kick-batch.mjs` the audition batches.

```bash
node pop/notespatial/bin/climbalift.mjs --events out/climbalift-score.json   # the binaural print (+ event log)
ARC=0 PAD=3 PAD_FADE=1.5 bash pop/notespatial/bin/master-v2.sh out/climbalift-print.wav out/climbalift-vN.flac -11 -2.0
node pop/notespatial/bin/score-video.mjs --score out/climbalift-score.json --audio out/climbalift-vN.flac --out out/climbalift-vN-score.mp4
```

## The through line

A chord broken into a line, running the ring, getting faster and higher until it
becomes the theme. Climb = the arpeggio accelerating and rising. Lift = the
theme, one key up, hopping the ring. 100 bars, two arches, one peak:

intro 8 · verse 8 · climb 8 · lift 8 · still 8 · verse 8 · climb 8 · **LIFT 16 (+3)** ·
after 8 · climb 8 · **LIFT 8 (+6, 136 BPM)** · outro 4 (home). Bm → Dm → Fm, each
lift prepared in the climb before it (digest 01 R11), a tritone fall home.
Harmony: verse Bm Bm G G Bm Bm C F#; climbs pinned on one chord; lifts
Bm Bm G G Em Em F# F# with A→A# under the theme over the F#. The reviews that
produced this form are summarized in the session ledger; the research is
`papers/chamber-platter/` and `papers/bass-platter/`.

## What plays

- Held voice: FEM bronze handbell (`pop/bell`), church bell in the still passages.
  Verse line: the modal marimba. The sigh: the novelizer friction voice. The still
  drone: the novelizer two-mass creature. Glock climbs: implokick + cracklesnare.
- Guitar: `pop/guitar` strum → a pedal push, two asymmetric stages with sag, tone
  stack, closed 4×12 cabinet model; power chords, double-tracked, take two darker.
- Bass (bass-platter R1–R8, engine audit 2026-09-25): sine sub at the real root on
  the cabinet; wobble mid an octave up, Q 1.3, sweep < 2 octaves, LFO phase-synced;
  skrill growl two octaves up on held passages; a phone layer 250–700 Hz. The
  engines' own subs are never used.
- Kick: club electro (190→48 Hz in 35 ms, gone in 280, chirp click, 150 Hz knock);
  every other layer sidechained 10 dB to it, back by the next beat.
- Bed: cabin air and a sixteenth-note space hat orbiting the ring, under everything,
  exempt from the silences. No tape stops.
- Space: one moving voice outside the lifts; the blast turns the field at lift 2,
  the eight-turn spin at the peak; kick, sub and hats pinned.

## Engine flags landed for this track (opt-in, defaults unchanged)

`pop/dance/synths/wobble.mjs` and `skrill.mjs`: `opts.phaseSync` starts the LFO on
the beat grid; `skrill.mjs`: `opts.trueSub` renders a real octave-below sine (the
default sub path is a half-wave hump with DC, kept for reproducibility).

## Numbers (v15)

−12.3 LUFS · −1.9 dBTP · LRA 8.1 · phone-proxy loss 5.4 dB (gate 5; the club pump
and the real sub are the record — documented exception, as hellsine's sub climax).

## Versions

v1 first form · v3 SALEM layers, glock breaks, wub · v5 the through-line rewrite ·
v6 amp+cab guitar · v7 the bed, no spin-downs · v9 the bass pipeline · v13 electro
kick + full sidechain · v15 current. Masters and previews in `out/` (gitignored).
