# sailor-song · the take read against the platter

Her take (`~/Desktop/IMG_8699.mov`, 2:59), measured in `take.analysis.json`,
`measures.json` and `vox-notes.json`, read against the papers platter:
[rhythm](../../papers/rhythm-platter/) (necklace geometry, syncopation,
distance, entrainment, beat bins), [chamber](../../papers/chamber-platter/)
(form, envelopes, the pop strategies digest) and
[bass](../../papers/bass-platter/) (sub register, tempo bands, delay in
beats). Every number below is produced by `bin/platter-analysis.mjs`, which
uses `pop/lib/necklace.mjs` for the rhythm math; the full readout is
`platter-analysis.json`. Rules are cited as `digest · Rn`.

```sh
node pop/sailor-song/bin/platter-analysis.mjs
```

The lyric is Gigi Perez's and stays out of this file. Bars are 1-based, as
in `measures.json` and `c/sailor-chart.h`.

## 1 · Her hand is a periodic necklace, not a syncopation

The motif her right hand plays, `X..XX..X` on 8ths, is onsets {0, 3, 4, 7},
inter-onset intervals **(3, 1, 3, 1)**. Read through
[rhythm 01–04](../../papers/rhythm-platter/digest/):

| Property | Value | Meaning |
|---|---|---|
| Euclidean | no | E(4,8) is straight 8ths `x.x.x.x.`; hers is one chronotonic unit away |
| Periodic | **yes** (period 4) | the bar is two identical 2-beat cells: 1 &2 · 3 &4 |
| Evenness | 0.955 | nearly even; balanced (centroid at the origin) |
| Syncopation (LHL) | **0** | every strong pulse she leaves empty (2, 6) is weaker than the note sounding through it |
| Povel–Essens C | **0** | a 2-pulse clock fits with no counterevidence |
| Off-beatness | 2 of 4 (pulses 3, 7) | half her onsets sit on generators of C(8) |
| Interval vector | [0, 2, 0, 2, 2] | no adjacent-pulse pairs; the 3s and 1s alternate |

So the figure is **anticipation without syncopation**: the `&2` and `&4`
strums are pickups into 3 and into the next downbeat, and both targets are
also struck. LHL and the C-score both read zero, off-beatness reads 50%: the
platter's point that a rhythm carries a vector, not a scalar
([04](../../papers/rhythm-platter/digest/04-syncopation.md)). Musically, the
pattern is a two-beat rocking cell, and the half-time drum feel the drafts
use (kick 1, snare 3) is the meter that cell already implies.

**The kit is the same necklace.** `render.mjs` puts kick on 1 and `&2`, snare
on 3, rim on `&4`: onsets {0, 3, 4, 7}. That is her hand, doubled. Two
alternatives the platter hands over:

- **Hocket** ([06](../../papers/rhythm-platter/digest/06-complements-canons.md)):
  her complement `.xx..xx.` (pulses 1, 2, 5, 6) interlocks with her exactly
  and shares her chronotonic curve (distance 0 under rotation). A hat or rim
  on the complement fills every 8th between the two of you without ever
  landing on her stroke. Good for the break (bars 68–71), where the guitar
  is loudest and there is no voice to protect.
- **Morph** ([05](../../papers/rhythm-platter/digest/05-distance-similarity.md)):
  the swap-geodesic from her cell to straight 8ths is two moves,
  `x..xx..x → x.x.x..x → x.x.x.x.` (one onset per phrase). Walking it across
  the last chorus into the break turns the folk rock into a driving 8th feel
  without a cut.

**How faithfully she keeps it.** 37 of 82 four-beat bars are the motif
verbatim; the rest add a stroke, mostly on the `&3` (`X..XXXXX` ×10,
`X..XX.XX` ×9). Mean chronotonic drift from the motif per section: verse 2
0.13, verse 1 0.23, chorus 1 0.41, chorus 2 0.45, bridge 0.50, break 0.56.
She densifies as the song goes; the verses are the cleanest statement.
[Chamber 03 rule 2](../../papers/chamber-platter/digest/03-process-and-envelopes.md)
(introduce a pattern by substitution, one note per phrase) is what her own
hand does between verse and chorus.

## 2 · Beat bins: her groove is in the pickups, not in swing

[Rhythm 07](../../papers/rhythm-platter/digest/07-perception-groove.md)
(Danielsen): a beat is a bin, and where in the bin a stroke lands is a
decision. Measured against her own strum-snapped beats:

| Layer | Placement |
|---|---|
| Her off-beat strums | phase 0.511 of the beat (p10 0.436, p90 0.578): **straight 8ths**, 5 ms late, swing ratio 1.05:1 |
| Her voice, all notes | signed median **+16 ms** behind her 8th grid (abs 41 ms) |
| Voice by section | verse 1 **+38**, chorus 1 +16, verse 2 +13, chorus 2 **+5**, bridge +9 |
| Long notes (> 0.4 s) | +7 ms · short notes (≤ 0.2 s): **+29 ms** |
| Phrase starts | 18 of 29 begin on the `&4` pickup, a median 265 ms after beat 4: **~20–120 ms behind the `&4` strum** |

Three findings:

1. **She does not swing.** The 8ths are straight; the earlier phase histogram
   that suggested a late `&` was measuring the beat tracker's 35 ms lead.
2. **Her voice sits behind her hand, and tightens as the song goes.** Verse 1
   is sung 38 ms back; by chorus 2 she is on the grid. Short syllables lag,
   held notes do not. That is a laid-back verse and a locked chorus, which is
   an arrangement in itself.
3. **The anacrusis is the song's rhythmic signature.** Two thirds of her
   phrases begin on the last stroke of the cell (`&4`) and she lands the
   syllable a little after the string. The `&4` rim in the kit is therefore
   the one hit that competes with her most; softening it (or moving it to the
   complement) leaves the pickup to her.

`lock-vox.mjs` pulled note onsets from 41 ms to 8 ms of the grid. Under
[07](../../papers/rhythm-platter/digest/07-perception-groove.md) that is the
grid-flattening the platter warns about: the necklace should decide *which
pulse*, and an offset table should decide *where in the bin*. The table above
is that layer. For the C engine, carry `voiceOffsetMs` per section (+38, +16,
+13, +5, +9) and a `pickupLagMs` (~50) as parameters, and either lock to
*those* or leave her as sung: locking to 8 ms removes the only microtiming
that distinguishes her verse from her chorus.

## 3 · Entrainment: what is felt as a beat, and what is a phrase

[Rhythm 07](../../papers/rhythm-platter/digest/07-perception-groove.md)
(London): a felt beat lives between ~100 ms and ~2 s, strongest at 500–700 ms.
At her median 120.9 BPM:

| Level | Period | Status |
|---|---|---|
| 8th | 248 ms | subdivision, felt |
| quarter (her strum beat) | **496 ms** | 4 ms under the sweet spot's floor: the beat |
| half (the drafts' felt 60) | 993 ms | inside the window, the tactus the kit chooses |
| bar | 1985 ms | **at the ceiling** |
| 2-bar chord cycle | 3970 ms | **not a rhythm**: heard as harmony, not as a cycle |

Her harmonic rhythm is 8.8 beats (a chord holds 2.2 bars on average; B is
held up to 6). So the chord changes are phrase events, and nothing rhythmic
should be asked of them. Any riser, delay throw or morph step wants to be
scheduled on the bar (2 s) or the half (1 s), where the ear still counts.

**Tempo arch.** Median BPM per section: intro 125 · verse 1 **113.6** ·
chorus 1 119.5 · verse 2 114.8 · chorus 2 123 · break 123 · bridge 121.6 ·
outro **130**. The verses sit 6–7 BPM under the choruses, and she runs the
ending. In follow mode this is free arrangement: the choruses lift by tempo
alone, before any layer is added. Any bed constant in beats (delay, LFO,
sidechain release) has to be recomputed per bar from `measures.json`; a fixed
millisecond value drifts 8% between her verse and her outro.

## 4 · Harmony: vi–I with the IV as an event

84 bars: G#m 35, B 42, Emaj7 **7**. Runs: G#m median 2 bars (max 4), B median
2 (max 6), Emaj7 median 1 (max 2). In B major that is vi–I with IV appearing
seven times, always as a one- or two-bar event: bar 18 (verse 1), 28–29 (the
downbeat of chorus 1), 41 (end of chorus 1), 49 (verse 2), 73 and 80 (the
bridge's first and last bars). The tabs' cycle is IV–vi–I–I; her open low
string turns the IV bar into Emaj7/G# ⊃ G#m most of the time, and the true
Emaj7 surfaces where she strums the shape cleanly, which happens to be at
section edges. **The IV marks the seams.** Chorus 1 opens on it; the bridge
opens and closes on it. The bed should treat Emaj7 as a colour change
(add the major 3rd, E, in the tenor sines) rather than as one chord in three.

## 5 · Melody: a pentatonic-leaning aeolian line that ends on 5

279 tuned notes (bleed before 23 s removed), G#3–G#4 (midi 52 dips to E3 in
the choruses). Time on each scale degree:

| 5 | b3 | b6 | b7 | 4 | 1 | 2 |
|---|---|---|---|---|---|---|
| **34%** | 22% | 14% | 13% | 7.5% | 7.3% | **2.5%** |

- **D# (5) is the home tone, not G#.** A third of the sung time sits on the
  fifth; the tonic gets 7%. 17 of 29 phrases cadence on 5, four on b3, one on
  1. The line hangs open on the dominant and the guitar's vi–I underneath
  supplies closure. A sine bed that doubles the tonic under her will feel
  heavier than the take; doubling 5 and b3 keeps its suspension.
- **The 2nd is avoided** (A#, 2.5%). The working scale is 1 b3 4 5 b6 b7: minor
  pentatonic plus b6, i.e. aeolian minus 2. The harmony stems and beds should
  avoid A# in her register.
- **Motion:** 157 steps, 110 leaps, 8 repeats (step share 0.57). Leapy for a
  ballad; the leaps are the 5→1 and 1→5 drops in the hook.
- **The hook** (most repeated 4-note degree sequence): **b7 b6 5 b3** ×12, then
  5 1 b7 1, 1 b7 1 5, b6 b7 b6 5, 5 b6 5 b3. The descending F#–E–D#–B tetrachord
  is the cell she returns to; it fits every chord in the song (b7 is the 5th
  of B, b6 is the root of Emaj7, 5 and b3 belong to G#m and B).
- **Register per section** (median midi): verse 61 (C#4) · chorus 63 (D#4) ·
  bridge 63. A minor third up from verse to chorus and no further:
  [chamber 01 R8](../../papers/chamber-platter/digest/01-tonal-forms.md)
  (register steps a third per phrase, never with density in the same phrase)
  is what she does; the arrangement's job is to move density where she does
  not move register (chorus 2, bridge).
- **Phrases:** 29 breath phrases (from the dry stem's energy), median 6 beats,
  clustering at 3–4 beats (short answers) and 15–17 beats (the long chorus
  lines). Contours 13 rise, 11 fall, 4 level. Eight in verse 1, six in
  chorus 1, two in verse 2, eight in chorus 2, five in the bridge.
- **Against the chords:** 69% of sung time is on a chord tone; the non-chord
  seconds fall on B (21 s) more than on G#m (15 s), because her b3/b6/b7 cell
  rubs the I. Her own harmony stems, by chord-tone share over her chords:
  a diatonic **3rd below 0.60**, 3rd above 0.55, 6th below 0.55, **5th above
  0.35**, octave 0.69. The `up5` stem is the dissonant one and wants to be
  the rarest; `down3` is the safest constant companion.

## 6 · Form: her climax is the guitar break, and her verses are louder than her choruses

| Section | Bars | Start | Fraction | Vox dB | Guitar dB |
|---|---|---|---|---|---|
| intro | 1–10 (10) | 5.0 s | 0.03 | – | −21.5 |
| verse 1 | 11–27 (17) | 24.3 s | 0.14 | **−24.1** | −20.2 |
| chorus 1 | 28–43 (16) | 61.2 s | 0.34 | −22.1 | −19.9 |
| verse 2 | 44–51 (8) | 93.3 s | 0.52 | **−21.0** | −20.7 |
| chorus 2 | 52–67 (16) | 110.1 s | 0.61 | −24.0 | −20.0 |
| break | 68–71 (4) | 141.5 s | **0.79** | – | **−17.3** |
| bridge | 72–80 (9) | 149.4 s | 0.83 | −22.8 | −19.6 |
| outro | 81–84 (4) | 167.0 s | 0.93 | – | **−17.3** |

Read against [chamber 01](../../papers/chamber-platter/digest/01-tonal-forms.md):

- **R1** (phrase lengths from {8, 12, 16, 24}, never three equal in a row):
  chorus 1, verse 2 and chorus 2 pass; verse 1 is 17 (a held B adds a bar),
  the intro 10, the bridge 9. The odd bars are hers and are where a cadence
  or a pickup breathes; the C engine should keep them, not round them.
- **R5** (climax at 0.71 after an unbroken 90 s crescendo) points at bar 60,
  mid chorus 2. **Her loudness says otherwise:** the voice is flat across the
  song (3 dB range) and actually peaks in the verses (loudest bars 15, 22, 18,
  47), while the guitar's peak is the **break at 0.79**, where she stops
  singing and strums 3–4 dB harder. That is Shaker Loops' 0.80, not the
  chaconne's 0.71. The take's own climax is instrumental, at bar 68, and the
  arrangement should build toward it through chorus 2 rather than peak inside
  chorus 2. Her singing does not get louder in the choruses, so the chorus
  lift is the arrangement's to make.
- **R6** (stillness at 0.36: two voices, one chord, 8 bars, no attacks) lands
  at bar 29, the first bars of chorus 1, which open on the IV. A bed that
  thins to two sines on Emaj7 there, then fills over the chorus, uses the
  rule where the song already has a seam.
- **R12** (ending restates the opening at half density, decay doubling per
  phrase, one release cue): the outro is the intro's figure at 130 BPM and
  the guitar's loudest level. Half density is not what she does; the
  arrangement can supply it by dropping the kit at bar 81 and leaving her
  alone with the sub.

**Build curve** from [chamber 04](../../papers/chamber-platter/digest/04-pop-strategies.md)
(one curve, layers entering at 0.10 / 0.28 / 0.40 / 0.62 / 0.78 of the build)
mapped onto 84 bars: entries at bars **9** (end of intro), **24** (last bars
of verse 1), **34** (mid chorus 1), **52** (chorus 2 downbeat), **66** (two
bars before the break). The last entry two bars before her instrumental
peak is exactly the rule's intention, and chamber 03 rule 3 (remove pulse and
bass one phrase early, then land on one downbeat) says bars 64–67 should
thin before 68 lands.

## 7 · The cut

[Chamber 04](../../papers/chamber-platter/digest/04-pop-strategies.md): pop
tracks run about 1:30, and wannadash went from 3:28 to 1:54 by removing
repeated statements with seams on downbeats and 10 ms fades. Verse 2 and
chorus 2 restate verse 1 and chorus 1 on the same chords.

Keep bars **7–43** and **68–84**, drop 1–6 and 44–67. One seam, bar 43 → 68,
G#m → G#m, both on her strum downbeat. Length **1:50**, wannadash's
neighbourhood. The full take stays the master; the cut is a release edit.
Check the lyric across the seam before committing to it.

## 8 · Bass: the numbers the bed should carry

[Bass 01](../../papers/bass-platter/digest/01-dub-bass.md), in her frame
(+13 ¢):

| Rule | Applied to this take |
|---|---|
| R12 tempo band | 120.9 median puts the lane in dub techno's 110–125, not dubstep's 140 |
| R1 register | sub in the 30–60 Hz octave: **E1 41.5 Hz, G#1 52.3 Hz**; B1 is 62.2 Hz, at the band's top, so the I chord's sub can sit on F#1 or stay on B1 and take the 3rd harmonic layer |
| R3 pattern | kick 1 / snare 3 at ~120 is the dubstep half-time layout already in the drafts; a one-drop variant leaves 1 empty and puts kick plus cross-stick on 3, which would hand the downbeat back to her `&4` pickup |
| R4 late entry | at least one 8-bar phrase of space before the first bass note: the sub waits until bar 11, her first sung bar |
| R5 delay | 0.93 of a beat ≈ **462 ms** at median tempo, 4–5 repeats, high-passed 100 Hz, low-passed 4.5 kHz. In follow mode compute it per bar: 490 ms in verse 1, 428 ms in the outro |
| R8 kick vs guitar | her low G#2 (104 Hz) and B2 (124 Hz) sit in the kick-body band (100–200 Hz); duck the sub 2–3 dB per kick with the fastest attack, and either high-pass the guitar stem at ~110 Hz or keep the kick body under 100 Hz |
| R9 | mono below 100 Hz; the reverb the voice, halo and sines share stays above it |
| R10 | a harmonic copy of the sub high-passed at 150 Hz carrying 250–700 Hz, since MASTERING.md allows at most 5 dB loss through the 180 Hz–8 kHz phone proxy and the sub alone fails that test |

## What this changes in the arrangement

1. Keep a **microtiming layer** (§2) instead of locking her to 8 ms: per-section
   voice offsets and the `&4` pickup lag, as chart parameters.
2. Soften or relocate the **`&4` rim** (§2); it is the one hit on her signature.
3. **Build to bar 68**, not bar 60 (§6): layer entries at 9 / 24 / 34 / 52 / 66,
   thin bars 64–67, land the break as the peak, drop the kit at 81.
4. **Hocket on the complement** in the break; **morph to straight 8ths** across
   chorus 2 if a drive is wanted (§1).
5. Beds double **5 and b3**, avoid **A#**, treat **Emaj7 as a colour** at the
   seams (§4, §5); `down3` is the resident harmony stem, `up5` the rare one.
6. Sub on **E1 / G#1**, delay in beats per bar, the guitar high-passed off the
   kick body (§8).
7. A **1:50 cut** at bar 43 → 68 for release; the 2:59 stays the master (§7).

## v5 → v6: what the reading changed in the dance mix

Measured on the masters (`cut.sh`, both at the −10 LUFS house target):

| | v5 | v6 |
|---|---|---|
| Opening | 16 s of guitar with sines fading in, kick building in from bar 7 (−16 → −12 LUFS) | her first strum at 1.65 s, levelled to −17 dB RMS, guitar and voice alone; nothing synthetic until bar 16 |
| Arrival of the floor | bar 7, in pieces | bar 28, whole, on an explosion; one lane per phrase before it (chamber-03 rule 1) |
| Kit against her hand | the same necklace doubled, rim on her `&4` pickup | her complement `.xx..xx.` (hats &1/&3, claps 2/4), floor on 1 and 3; no rim where she sings; off-beats at her +5 ms bin |
| Clock (58–158 s, re-tracked) | local BPM p10–p90 113.9–123, CV 0.037 | 117.5–123, CV 0.024; her rubato untouched before bar 24 |
| Loudness range | LRA 2.2 LU | LRA 6.6 LU (house 4–8) |
| Harmonies | up3 + down6 in choruses | down3 resident from verse 1 (chord-tone share 0.60), up5 only in the bridge (0.35); arpeggiated stacks per section |
| Space | fixed orbits | every drop displaces the ring and springs it back at 1.1 Hz, damping 1.2; kick and sub never move |

Not applied, deliberately: the 1:50 cut (§7) — the arc from bedroom to
floor needs the length; and the morph to straight 8ths (§1), which would
undo the hocket. Both remain options in the score.

## Debts

The phrase segmentation is an energy gate on the Demucs vocal (10 dB under
the loud frames, 0.35 s gap), swept 6–22 dB and set at the knee; it is not a
transcription of her breaths. Loudness per section is bar-mean RMS of the
separated stems, so it carries Demucs's leakage (her "intro" dB is bleed).
`pop/lib/necklace.mjs` deliberately leaves Keith's measure and Gómez's WNBD
unimplemented (its note-to-beat distance is a plain mean), so the syncopation
vector here is LHL, the C-score, note-to-beat and off-beatness, four of the
digest's five. The cut has not been listened to.
