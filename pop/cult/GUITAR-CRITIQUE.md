# wannadash — the guitar, critiqued

Read from `bin/render10.mjs` (`guitar`, `guitarChord`, `guitarShred`,
`guitarStrum`, `STRUM`, every call site) and `MIX-NOTES-2026-09-01.md`,
against the new engine in `pop/guitar/` (`strum.c`). Bars are score bars.

## What works

- The **B-pedal chug** under bars 48–63 is a real section idea: one note held
  while the harmony moves under it is what makes THE REPLY feel like a
  transmission resuming. Keep the idea; the execution is the problem.
- The **GCJ voicings** (Bm9 / Gmaj7 / Em9 / F#7sus4) at 65–67 are the only
  chords on the record with a ninth or a seventh in them, and the **FLOWER**
  line landing maj7 → 9 → 3 over the G bed is correctly jazzy. Good ears.
- **`STRUM`'s asymmetry** — up-strokes faster, shorter, quieter, catching
  fewer strings — is the right observation about a hand. It is the one
  place the JS guitar knows it is being played.
- The **finale strum (76–95) is aligned** to `HOME` two bars a chord; so is
  `guitar-wide`. The bar-31 chord under the first "run real fast" is the
  right gesture at the right moment.
- The shreds at 55/63/91 accelerating **into** the downbeat and letting the
  last note ring across it is a real guitarist's phrase shape.

## What is mechanical

**The string.** `guitar()` is a two-point-average Karplus-Strong with a
full-band noise burst: no pick position, no body, and an *integer* period
with the average's half-sample delay uncompensated. Measured against
`hz(midi)`: B2 −3 c, F#4 −10 c, D5/E5/F#5 **−16 to −17 cents**. The shred's
top four notes are a sixth of a semitone flat against sine layers that are
exact; that is the "weird" in the runs, not the notes. The two-point loop
also kills everything above ~3 kHz within ~40 ms at B2, so every stroke is a
thud followed by fundamentals — the "marching band" is partly this. Then
every single string goes through its own tanh drive and 0.16 one-pole, so
there is no intermodulation between strings: six fuzzes summed, never a
chord being driven. B1 (35) and F#5 (78) get the same drive and the same
lowpass, so the secret-act "guitar" at 40/44 (`triad − 24` = B1, 61.7 Hz,
5.2 s) is a fuzz bass with a guitar label. Call it one.

**The rake.** `guitarChord` spaces strings at a fixed 17 ms whatever the
force or direction; `up` reverses the order and nothing else. `guitarStrum`
uses 16 / 9.3 ms, also fixed. Nothing shortens when the hand hits harder,
nothing accelerates through the strings, and the only "dynamics" are the
six `force` constants in the table. No pick contact: each stroke spawns
four *new* strings 1.3 s long on top of the previous stroke's still-ringing
four, so by the third stroke of a bar twelve KS lines are ringing the same
chord. A hand catches the string before it re-plucks it; this stacks.

**The timing.** Every finale strum sits at `+0.12 · BEAT` = **60 ms behind
the beat**, all twenty bars, while the kick is on the grid and the bass at
`+0.5 beat ± 3 ms`. Sixty milliseconds is not laid back, it is late. Odd
bars take `STRUM.slice(3)` — beats 3–4 only — so half the bars have a hole
where beats 1–2 should be. The bar-9 pickup shred is the only guitar that
plays *before* a downbeat; everything else is placed after one.

**The shred.** Every note is a fresh pluck (no hammer / pull / slide), the
pan flips ±0.34 **per note** — a run that ping-pongs across the stereo
field is a machine tell — and the `bow` swell is a sine over the note index,
identical all three times.

**The voicings and the register.** `GC` is four root-position stacks with
the bottom note at E2 / G2 / B2 / D3 and no top string: the G chord is
missing its G4, the Em its E4/B3 — the very strings a strum is *for*. All of
them live under 250 Hz, where `bass()` (root + fifth + sub, 4× the summed
in-band gain of anything else per FLOW-BRIEF) already owns the room. The
`GC` cycle also runs **four** bars a chord while the bed's `ROWS` run two,
in a different row: at 52 the guitar plays D over the bed's G (fine, a
maj9), at 60 Em over G (fine, a 6th), but at **56 it plays G [43,50,55,59]
over the bed's Bm whose pad top is F#3** — G against F# is a minor ninth, and
that is the bar that reads as wrong. The dot-field chords at 72/74 are
`chord − 12` close triads, a keyboard voicing no guitar can finger.

No fret noise, no chuck, no chord change ever *heard*: chords replace each
other at bar lines with nothing in between.

## Where the sampled and the synthesized guitars fight

`guitar-chug` and `guitar-wide` are not samples; they are the same numpy KS
(`gen-instruments.py`) through `tanh(7.5)` / `tanh(5)` **and a flanger** at
0.9 / 0.5 Hz. So the record carries three tunings of every chord at once: the
flanged sample, its `semis: ±0.07` double 7 cents off, and the JS chord dead
on (or 16 cents flat, above). Three tanh stages (sample, double, JS) and
three darkness settings (0.34, 0.46, the 0.16 one-pole) stack on the same
notes. In the finale, `guitar-wide` (0.19), its double (0.11), `guitarStrum`
(0.058), the boings and the accordion b/d/g/e all state the same four-note
chord every two bars; the hand @jeffrey asked for is **10 dB under a static
wall of the same voicing**, so it cannot be heard as a hand. At 64 the chug
(a B pedal in 8ths, accents every two bars) and a `GC` block chord at
`+0.55 beat` were the whole marching band; the pass that shortened the chug
left the two-layer pattern intact.

## What the strum machine changes, bar by bar

The engine renders a bar or a phrase to WAV; `shot()` places it the way the
`accordion-*.wav` bank is placed. Single-note lines (FLOWER, the shreds)
stay in JS for now — the engine has no hammer-ons — but fix their tuning:
`period = SR / hz − 0.5` with an allpass, as `strum.c` does.

- **9** — keep the pickup shred; add one acoustic up-stroke `..u.` (`--up
  3`, force 0.4) on beat 4 so a hand is present before the vocal lands.
- **31** — `--chord 47,54,59,62 --pattern "D..............." --electric`:
  one hard down-stroke with pick contact, on the grid, no `+0.035`.
- **40 / 44** — call it bass in the events, or make it a guitar: `--chord Em
  --pattern "D..............." --force 0.35 --rake 40` acoustic, low E open.
- **48–63** — replace chug + `GC` with the engine following **the bed's
  chord** (`degAt`), one render per bar: `--mute palm --pattern
  "DdDdDdDdDdDdDdDd" --electric --chord "47,54,59"` for the pedal bars, and
  on the bed's own chord (F#m at 50, G at 52, Bm at 56, D 58, G 60, Em 62)
  `--chord <name> --mute palm --pattern "DdDdDdDdDdDdDdDx"`: the `x` chuck
  on the last 16th is the breath the phrase never had, and the fret-hand
  squeak on each change is the chord change becoming audible. Skip 54–55.
- **64–67** — drop the chug entirely; it is the marching band. The
  flowering pass is the demo: `--chord "Bm9|Gmaj7|Em9|F#7sus4" --bars 4
  --pattern "..D...u.d.U..u.." --rake 24 --force 0.55` acoustic
  (`out/flower-acoustic.wav`). The 200 / 400 Hz top modes give the chords
  the warmth `dark: 0.34` was faking, and the ninths sit on the top strings
  instead of at 82 Hz. Keep FLOWER over it, retuned.
- **72–75** — one quiet acoustic up-stroke per bar on the bed's chord,
  `--up 3`, so the dot field has a guitar that is *only* top strings.
- **76–95** — one 20-bar render, `--chord "Bm|Bm|D|D|G|G|Em|Em" --bars 20
  --electric --pattern "D..d..u.u.D..u.."`, on the grid, every bar the full
  pattern, odd bars `D..d..u.x.D..u..` so the chuck replaces the hole. Retire
  `guitar-wide` and its double; if a bed is wanted, the same engine acoustic
  at `--rake 30 --force 0.5 --pattern "D..............."` two bars a chord
  gives one tuning, no flanger, and the bridge coupling makes it sustain as
  one instrument. Bring the hand up to where the wall was (≈0.15), not the
  wall down to the hand.
- **91** — keep the shred, retuned, pan fixed on one side.

The engine's limits are in `pop/guitar/README.md`: no inharmonicity, no
bends, chord changes only on bar lines. It does not replace the lines. It
replaces every place the record pretended a chord was a hand.
