# wattajetta stone club — the arrangement

The composition, human- and agent-readable. `bin/render-wattajetta.mjs
--stone-club` is the extraction of this document into sound; when the two
disagree, decide musically, then update both. Tempo rides 127→138 BPM over
72 bars (~2:10). Everything below is deterministic — same bake every time.

## Form: a tour through the western modes

All on the root E. The track starts in the stone's own pentatonic, walks the
modal spectrum flat-to-bright, strips to bare machine, arrives in the major,
and comes home.

| bars  | section | mode           | wave | what happens |
|-------|---------|----------------|------|--------------|
| 0–8   | intro   | E pentatonic   | 0.72 | percussion/water-floor only; no bells, bowls, ukulele, strings, or harp-like resonances |
| 8–18  | verse 1 | E aeolian      | 0.90 | walking line enters the natural minor; disco bass starts (bar 4+); flyby counter-melody descends (bar 11, with wingmen) |
| 18–26 | chorus 1| E dorian       | 1.15 | THE HOOK (call) / line (response); cliff run bar 24; quat motif bar 22 |
| 26–36 | verse 2 | E phrygian     | 0.90 | darkest color; flyby climbs in answer (bar 29); mini rush bar 27 |
| 36–44 | chorus 2| E mixolydian   | 1.15 | hook brightens (G♯); bar-40 BREAKOUT: 5-turn spin + 12-note rush; cliff bar 42; quat bar 43 |
| 44–49 | bridge  | E lydian       | 0.50 | underwater: line submerged, uke harp-wash + Fibonacci arps own it; submarine flyby (45); distant owl (46); quat 46/48 |
| 49–54 | techno  | (E pent)       | 0.42 | MINIMAL MACHINE: tight kick (2.0 decay→short), dry rimshots, closed hats, 5-polymeter tick, sparse castanets. No bells, uke, disco, bloops, bowls, gallop. |
| 54–66 | chorus 3| E ionian       | 1.20 | two-octave gliss slams the door; hook arrives MAJOR, doubled an octave down; cliffs 56 + 64; Emaj7 chord gate |
| 66–72 | outro   | E pentatonic   | 0.55 | circle closes; last owl (68.5); dry drips gone; church toll bar 63 rings out |

Mode intervals (semitones on E): pent 0-3-5-7-10 · aeolian 0-2-3-5-7-8-10 ·
dorian 0-2-3-5-7-9-10 · phrygian 0-1-3-5-7-8-10 · mixolydian 0-2-4-5-7-9-10 ·
lydian 0-2-4-6-7-9-11 · ionian 0-2-4-5-7-9-11.

The song no longer repeats one four-chord modal loop. Its harmony has a plot:

| section | progression |
|---------|-------------|
| intro | Em(add9) · Cmaj7/E · Am9/E · B7sus–B7 |
| verse 1 | Em9 · D/F♯ · Cmaj7 · Am9 · B7sus–B7(♭9) |
| chorus 1 | Em7 · A(add9)/E · Cmaj7 · B7sus–B7 |
| verse 2 | Em(add9) · Fmaj7/E · Cmaj7/E · Am/E · B7(♭9)–B7 |
| chorus 2 | E7sus · D/A · A(add9) · B7sus–B7 |
| bridge | Emaj7(♯11) · F♯7sus · D♯m7/A♯ · Amaj7/E |
| techno | E5 pedal |
| final chorus | E(add9) · B/D♯ · C♯m7 · Amaj9 · F♯m7 · B7sus–B7 |
| outro | Em(add9) · Cmaj7/E · Esus2 · E5 |

The bass, bowls, uke, disco voice, and later guitar will follow this map after
the harmony-and-melody study is accepted.

## Voices

- **Walking line** (granite FEM, E4–E5 lane of the section mode): one
  through-composed cursor that never resets; asymmetric turns + wiggle leaps
  (sudden 4ths/5ths, 14%). Right hand leads, left answers late/soft. Yields
  to: hook bars, rolls/pickups/rushes, cliff bars, the techno strip.
- **Harmony voice**: parallel organum under right-hand strikes — diatonic
  third below in verses (26%), sixth below in choruses (42%). 7-note modes only.
- **Ornaments** (≤2/bar, one per strike): flams (downbeats), diddles→dead
  strokes (tubular), octave-below double stops, crest fills.
- **Breathing**: two incommensurate waves (13 & 7.3 bars) × section wave.
  Valleys = lay out + single strikes RING 2.5–6.5 s; crests = tight ≤0.7 s,
  swing wider, sprout fills.
- **THE HOOK** is one eight-bar tune, not a two-bar loop. Chorus 1 states it
  in E Dorian; chorus 2 raises the third toward Mixolydian; the final chorus
  raises the third, sixth, and seventh, then adds a four-bar cadential answer.
  Verses expose only fragments. The bridge stretches its pitches into long
  tones, and the outro remembers the opening fragment.
- **Cliff runs** (bars 24, 42, 56, 64): two-gear accelerating climb (16ths →
  32nds) 12 steps up the ladder, ONE fast-attack 9-second glass summit bell
  (the only non-stone body), decelerating slide back to the next lead. Gain
  tapers 0.3 dB/semitone above E5 — sparkle, never tang.
- **Rolls** (4-bar turns, non-sparse): shapes cycle rush → cresc → dyad,
  never repeating; land with an octave-below punctuation.
- **Seam pickups**: ascending sextuplet into 18 · descending turn into 36 ·
  full two-octave gliss into 54. Mini rushes at 27, 40 (the breakout), 45, 62.
- **Quaternion motif** (bars 22, 43, 46, 48, 60): a 4-note cell rotated in
  3-space by successive elements of the binary tetrahedral group (2T),
  requantized to the section ladder.
- **Ukulele** (nylon Karplus-Strong, octave-down, ~no damping): mode-triad
  strums per 2-bar phrase (ring 24 s in valleys), chnk skanks on crest
  offbeats, seam-resolution strums, Fibonacci-word arpeggiations (valleys +
  bridge; intervals walk 1-1-2-3-5, reflecting).
- **Disco bass** (filtered saw + sub octave, Chic vocabulary): syncopated
  16ths, octave-jump flourishes, ghost chucks, slides; ♭7 becomes ♮7 in the
  bright modes; dubby skeleton in the bridge; silent in techno. High-passed
  Schroeder plate reverb tail.
- **Flyby counter-melody** (sine doppler w/ vibrato): descends (11), climbs
  (29), submarine root-fifth (45). Soft entries, 3+ bars each.
- **Sub/fuselage**: progression roots (mode-tuned) + offbeat gallop
  (sidechain pump); gallop rests in techno.
- **Water**: bloops (mode-tuned E3-octave drips, swung offbeats), spray,
  choir "ooh" (third follows the mode: G→G♯), chord gates at 18/36/54/67
  voiced in the section mode (54 = Emaj7).
- **Castanets**: vimib (slab prox ping, ×2.1 rate) + mugot (slab prox beep,
  ×1.4) — 80 ms clicks; dense bars 0–4, then 10% sparse spatial chatter all
  track (16% in techno).
- **Owls**: Freesound (vault-cached, attributed): close hoot bar 1.5,
  distant answer bar 46 (through water), farewell 68.5.
- **Empty Trash**: bar 3 (rate 0.55 — slow), 18 (1.18), 36 (0.95), 54 (0.78).

## Drums (the sacred floor + the math)

- **Kick**: four-on-the-floor, never stops. Club voice 126→43 Hz; techno
  voice tighter (128→45, 7 ms hole, 0.2 decay).
- **Backbeat**: 2 & 4, dragged +10 ms, tone/velocity breathe over a 12-hit
  cycle; techno = fixed dry rimshot.
- **Hats**: offbeat eighths, eager −8 ms; open every 4th bar (closed-only in
  techno).
- **Shakers**: Euclidean necklaces E(k,16), k = 5→7→9→11 per mutation,
  rotated +5/bar (coprime → full 16-phase orbit); accents where a
  counter-rotating E(3,16) coincides.
- **Polymeter**: rim tick every 5 sixteenths (from bar 8) + wood tick every
  3 (from bar 36, not in techno) — {3,4,5} family, rephases every 15 bars.
- **Fibonacci ghosts**: aperiodic word picks off-sixteenth taps, density 1/φ².
- **Skip fills**: 4 rotating patterns, tone ramp flips on odd fills (bars
  7, 17, 25, 35, 43, 53, 61, 69).
- **Quaternion orbit**: during spin windows, hat/shaker pan = a point
  rotated about a tilted axis by 2π·turns·progress — drums orbit the head.

## Space & dynamics

- **Sidechain**: kick→engine −8 dB (8 ms/170 ms) · kick→disco −50% (160 ms)
  · snare→bells up to −58%.
- **Spin windows** (Special Sign rule, mono-safe side-only return, ~60%
  depth): bars 16 (×2), 34 (×−3), 39.5 (×5 — the breakout), 52 (×2), 64 (×1).
- **Macro arc**: slow reveal → mutation valleys → late crest → 15 s retreat.
- **Crunch**: constant 0.85 tanh on engine+kick only; bells stay pure.
- **Master**: gentle 1.8:1 comp, +1.6 dB @54 Hz, limit 0.89, −0.5 dB trim.

## Verification

- Redundancy sweep merges same-note near-unisons (<40 ms); audit fails the
  render if any survive. `--stone-study` must stay byte-identical (score
  diff); cliff bells are the one sanctioned non-stone exception.
- `--score-json` exports ground truth for `bin/score-video.mjs`.
- Stems: `--stems` → kick / water engine / bells+uke / trash FX, true-summing.
- `node pop/wattajetta/bin/render-composition-study.mjs` renders only harmony,
  bass, lead, and a quiet phrase pulse. Production decisions wait on this cut.
