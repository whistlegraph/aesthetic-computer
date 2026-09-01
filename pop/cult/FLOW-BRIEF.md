# FLOW-BRIEF v2 — bar-by-bar critique of the 18:16 build

For `blueberry:bot`, from `blueberry:disu`, 2026-09-01. **Supersedes v1**, which was
measured against the 17:32 build and got three things wrong (noted below).

Sources: `out/cult-remix-v10.events.json` (3814 events), `out/cult-remix-v10-full.wav`
(227.2 s), `out/wannadash-competitive-master.wav`, `out/wannadash-flow-master.wav`.
120 BPM, bar = 2.0 s, 112 bars. Four analysts worked the frozen 18:16 snapshot:
rhythm/meter, harmony/voice-leading, timbre/orchestration/mix, text-setting/prosody.

---

## 0. Corrections to v1 — do not act on these

1. **"kick sits at 3–5 events/bar"** — wrong, a binning artifact of the ±2.5 ms
   humanisation. Binned to the 16th grid it is **exactly 82 kicks at each of
   positions 0/4/8/12**, every one of the 82 active bars. Four-on-the-floor, zero
   variation.
2. **"1172 hats = 31% of events, therefore over-hatted"** — arithmetic right,
   conclusion wrong. See §3.
3. **"swap `acrossfade` `tri` → `qsin`"** — worth ~1 dB, not the fix. See §5.

---

## 1. The README documents a renderer this build does not use

`grep polyAmt bin/render*.mjs` → **render6: 7 · render7: 6 · render8: 6 · render10: 0.**
`render10.mjs` is `BARS = 112`; the README's polyrhythm section describes "the 120 bars."

Measured signal-bus density (click/beep/bop/tap per bar) against the README's table:

| act | README hits/bar | measured |
| --- | --- | --- |
| I carrier | 0 | 1.25 |
| II three | 1.9 | **7.19** |
| III message | 7.0 | 5.06 |
| IV secret | 0.1 | 0.00 |
| V reply | 13.3 | 3.75 |
| VI spread | 15.0 | **1.33** |
| VII whole | **17.3** | 4.15 |
| VIII recognise | 12.9 | 3.50 |
| IX carrieroff | 0.1 | 3.38 |

Pearson r = **0.11**. The measured peak is act II; the README's peak act measures a
quarter of its claim; act VI — the README's near-maximum — is the second-lowest
non-silent value on the record.

Of the five documented cross-rhythm layers: **L1 (3:4)** has one run in the whole
record (bar 75). **L3 (5:4)** is vestigial, 3-attack bursts at bars 20–23, 63–64,
79–80, 87–88, 95–96. **L4 (7:16)** has **zero runs anywhere**. **L5 skid** has no grid
(IOIs 2.25–5.75 beats) — it is a one-shot gesture voice, not a layer.

**L2 (dotted-eighth) survived the port with its shape and lost its point.** `tap` has
67 IOIs at exactly 0.75 beat, but every instance is the same 3-attack cell at 16th
positions 5, 8, 11, restarting at position 5 the next bar. The README says it is
"counted from t = 0, not from the downbeat… it is why the figure lands somewhere new
every bar." **In this build it lands in the same place every bar.** It runs 14
consecutive bars in act V (48–53, 56–63) where it would actually accumulate. One line
— don't truncate at the barline — makes the README true.

**Action: either restore the precession or delete the claim from the README.**

---

## 2. The arc is not flat, it is inverted — and it dies at bar 29

`librosa.onset_strength`, 4-s means on the full render: 1.07 (0:16) → **1.66 at 0:44
(bar 22)** → 1.10–1.28 for the entire remaining 2:40. Detected onsets/bar: **8–15 in
bars 10–28, then 1–8 for the rest of the record.** Per-bar RMS is flat throughout
(bar 19 −20.0 dB, bar 83 −19.6 dB), so this is transient contrast being filled in,
not level.

The inflection is **bar 29** — where `sub` (29–39), `lead` (29–39) and `revkick`
(28–30) enter. It is also the record's **only hypermetric violation** (bar 29 ≡ 1 mod
4, mid-phrase, at no section boundary). The largest timbral change on the record lands
on hypermetric bar 2 of a 4-group and kills the punch for the following 75 bars.

The metrical profile inverts at the same place. Mean onset-envelope peak per 16th:

```
pos:            0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
bars  8-23   4.37  2.99  5.52  1.60  6.12  4.06  8.51  1.28  6.68  3.62  5.06  1.99  7.12  3.39  9.10  1.74
bars 24-39   3.41  0.69  2.44  0.63  3.93  0.90  2.66  0.91  3.53  0.76  2.81  0.73  4.78  0.70 10.37  0.81
bars 76-95   3.30  0.21  0.77  3.26  2.18  0.25  1.09  3.66  2.86  0.06  0.98  0.36  2.60  3.75  1.14  0.31
```

In bars 8–23 the off-eighths (5.52, 8.51, 5.06, 9.10) are **louder than the beats**
(4.37, 6.12, 6.68, 7.12) and every 16th is present — genuine, audible displacement
dissonance, and the best rhythm on the record. From bar 24 the 16ths collapse to
0.63–0.91. By bars 76–95 the off-eighth layer is gone from the audible profile.
**The record becomes metrically consonant exactly where the narrative says it comes
apart.**

**Action: move the bar-29 entry to bar 28 or 32. Put the arc in the kick** — it is 41%
of the energy and plays four identical notes in all 82 bars.

---

## 3. 58% of the event stream carries 2.9% of the energy

Energy proxy Σ(gain²·dur), share of total:

| voice | % of events | % of energy | mean gain |
| --- | --- | --- | --- |
| kick | 8.6 | **41.3** | 0.891 |
| bass | 9.7 | **22.6** | 0.777 |
| lead | 1.6 | **15.0** | 0.676 |
| hat | **30.7** | **0.36** | 0.087 |
| perc | 11.5 | 1.77 | 0.063 |
| tap | 8.2 | 0.11 | 0.203 |
| stab | 3.4 | 0.04 | 0.073 |

Every voice that gets varied (hat, perc, tap, stab, beep, material, guitar-chord =
58% of events) sums to **2.9% of the energy**. Every voice that carries the record
(kick + bass + lead = 20% of events, **79% of energy**) is invariant.
**The arrangement is written entirely in the inaudible half of the mix.**

So the hat count was the wrong complaint. The real hat problem is that **1153 of 1172
are the same `hatC` sample (98.4%)**, only 19 `hatO`. Don't cut hats — **raise them
6–8 dB and give them a second timbre.**

### And the record has no top

Octave energy budget: **20–160 Hz = 62.3%** · 1.28–2.56 kHz = 2.31% · 2.56–5.12 kHz =
0.42% · **5.12–20 kHz = 0.18%**.

**Low-passing the entire master at 4 kHz costs 0.1 LU. At 1 kHz it costs 1.0 LU.**
(Verified directly: −11.2 → −11.3 → −12.2 LUFS.)

Cause is in the score, not the mix: **only 20 of 3814 events are pitched above midi 76
(740 Hz)** — 15 `material` grains, 3 `guitar-shred`, 2 `stab`. Everything above 1.1 kHz
is noise. The instrument is being played in five octaves at the bottom of an eight-
octave register.

**Action: put something pitched above 1 kHz.** This is the highest-leverage change
available and nothing else in this document competes with it.

---

## 4. Bars 68–70 are still digital silence

Unchanged from the 17:32 build. Bars 68, 69, 70 have **zero events**; bar 71 has two.
The last tail is a hat at t=135.77 (dur 0.34) dying at 136.11.

Audio ground truth: scanning the whole 227.2 s render at −60 dB returns **exactly one
hit — 136.81 → 144.00 s**. Bar 70 measures **−93.4 dBFS**. Nothing else in the record,
including the act IV breakdown, ever drops below −60 dB.

The "approach" into bar 72: bar 71 beat 3.994 = one beep (g 0.166), beat 3.999 = one
kick (g 0.960). At 120 BPM those are **0.5 ms and 3 ms** before the downbeat — they
*are* the downbeat, floating-point-early, not an anacrusis. Then the full ensemble
enters on the downbeat with no pickup.

`render10.mjs` now designs around this rather than filling it:

```js
// Bars 64–67 and 72–75 are adjacent in the shipped edit. Treat them as one
const releaseSpreadBar = (bar) => (bar >= 64 && bar < 68) || dotFieldBar(bar);
```

That is defensible for the 2:21 edit, but it makes `cult-remix-v10-full.wav` a source
tape rather than a master — any future edit, stem export, remix or DJ play hits 7
seconds of silence at 2:16.

**Action: fill bars 68–71 as a real 4-bar approach, or delete them from the render and
make it 108 bars. Do not leave a null in a delivered master.**

---

## 5. The seam fix at 1:15 is backwards

Not the crossfade curve. `tri` → `qsin` recovers ~1 dB and is still worth doing
(`qsin` is the constant-power pair, `tri` is linear), but it is not the problem.

The render's silence ends at 143.93 and the **bar-72 downbeat peaks at −8.0 dB**.
`cut-competitive.sh` starts region `[d]` at 143.88 so the 0.24 s crossfade centres on
the bar line — which places that downbeat at the midpoint of its own fade-in:

```
source, bar 72 attack        max  −8.0 dB
through crossfade (tri)      max −13.1 dB
through crossfade (qsin)     max −13.1 dB   ← identical; the curve is irrelevant
butt cut at 144.0            max  −8.0 dB
```

**The crossfade is eating 5.1 dB off the attack transient of the arrival it was added
to smooth.**

**Action: asymmetric join.** Let `[c]` decay into the hole — it has 0.69 s of free
space at 136.12–136.81 — and start `[d]` hard at 143.93 with no fade-in.

---

## 6. Harmony: there is no dominant, so there is nothing to resolve

The four stated rows are reproduced exactly, `chordBars: 2` holds 48/48 with zero
syncopation, and `cycleBars: 32` holds perfectly for bars 0–63.

**The act VII claim survives but is not unique.** Bars 76–95 are genuinely pinned to
Bm·D·G·Em. But `guitar-chord` already plays the identical home row underneath bars
48–63 at a **4-bar** rate while the pad runs rows 2 and 3 above it at 2 bars.
**Act V is already home, in double augmentation.** That is the better story and it is
already in the file. Say "stops rotating," not "only place."

What actually changes at bar 76 and supports the claim better than the row does:
`guitar-chord` doubles its rate and locks to the pad for the first time; `sub` becomes
a 7.7 s B1 pedal; `cult` enters as a chord voice. **That, not the row, is what "the
harmony comes home" audibly means.**

Structural problems:

- **No dominant anywhere.** Scale degree 5 gets a chord exactly twice (bars 18–19,
  50–51) and is **F# minor** both times. Without a structural V there is no
  interruption and nothing for act VII to resolve.
- **The upper voice never moves.** Pad top is **F#3 at the head of all twelve rows**
  and never descends. A 5̂ head-tone asserted twelve times and never left.
- **44 of 48 pad changes are parallel perfect fifths**, and `stab` is a bit-exact
  `pad + 12` on 120 of 126 events — the two chordal layers are one layer with an
  octave doubling. Only bars **34** (G/B, reached by F#→G, a neo-Riemannian **L**) and
  **64** (Bm/D) are voiced otherwise. They sound different from everything else and
  nothing exploits it.
- **It ends on the wrong chord.** Bars 110–111 are **G major with D4 on top** — VI,
  not i. The tonic exists only as a `sub` B1 pedal under a non-tonic chord.
- **The chord quality is acoustically invisible.** Mean gains: bass 0.777 vs pad 0.085,
  stab 0.073, guitar-chord 0.082 — roughly 20 dB. In act VII's tonic bars (76–77,
  84–85) the mid band reads **F#, A, D, G — B is not in the top four.** The pinned
  tonic sounds like D major with a B underneath.
- **Bass bug: bar 34 downbeat is G0 (midi 19, 24.5 Hz).** Every other bar-1 root is
  23–33. A one-off octave wrap below reproduction, wasting headroom.
- **Low-register collisions:** bars **60–61** are the muddiest on the record (E–F#–G–A–B
  clustered below 125 Hz, with sub F#2 a semitone under bass G2). Also bars 34, and
  **78–79 / 86–87** where the B1 pedal sits a minor third under bass D at 62/73 Hz.

**Actions, in order of payoff:** (1) make a real dominant once — you already have F#
major in the file, in the `material` A#s; move it to bars 74–75 with bass F#1 and bar
76 becomes an arrival. (2) Let the pad top fall 5̂–4̂–3̂–2̂–1̂ across acts VII–IX; it costs
four inversions. (3) Alternate root position / first inversion to kill 20 of the 44
parallel fifths. (4) End on i.

---

## 7. Text: the SOS is never sung, and the recordings that would fix it exist

- **`...---...` does not occur in the vocal layer.** Collapsing every dot/dash attack
  gives a 138-character string: dot runs are 9, 10, 11, 12; dash runs are 1, 2, 4, 5,
  6. **Never three of either.** Act II — which `sosFigure` says carries the sung SOS —
  contains only ··· and never a dash at all. First sung dash: bar 28.
- The real SOS is in the beeps, and it is textbook (dot 102 ms / dash 272 ms), six
  statements at bars **20, 22, 63, 79, 87, 95**. Two of them fire at **0:40 and 0:44,
  16 seconds before the first sung dash**. The answer arrives before the call.
- **41 of 115 files in `sung/` are referenced at all.** These emit **zero events**
  despite four of them being named in `render10.mjs`: `sos-dash-{d4,e4,fs4}` (4
  mentions), `runitfast-sung` (2), `threeofus-{rise,fall}` (1), `dotdotdash-{hi,lo}`
  (1), and `iwannahide-{hi,lo}` (**0 mentions — absent entirely**).
- **Chorus line 2, "I wanna hide away", is never assembled.** All 19 "I wanna" events
  resolve to DASH (13), "run real fast" (5) or "away" (4). Zero to "hide away."
- **Act VII word counts per 4-bar group: 22 / 1 / 18 / 1 / 17.** Bars **80, 81, 82** and
  **88, 89, 90** have zero events on any lexical voice; bars 83 and 91 carry only 13
  `material` grains at 55–100 ms / gain 0.08–0.20 — bar 83's vocal-band energy (155) is
  indistinguishable from silent bar 81's (152). **The grain fill is inaudible as text.**
- **But something does answer, and it is good:** the beeped SOS at bar 79 beat 2 runs
  *through the barline* and completes inside bar 80. Same at 87→88 and 95→96. The
  antiphon is real — it just costs 1.2 s against an 8 s hole.
- **Act V, not act VII, is where the message is most complete**: 4 complete modules in
  16 bars vs 3 in 20. The section named "THE WHOLE MESSAGE" is 25% longer and 25% less
  verbal than the one named "THE REPLY."
- **Bar 55 is the loudest naked vocal moment on the record and nothing documents it** —
  9 word events, 0 drums, 500–3500 Hz : sub-200 Hz ratio of **14.8** against 0.9–1.4 in
  act VII. That is your bridge.
- **Vocal gain is not monotonic.** By act: 0.292 → 0.381 → 0.450 → 0.469 → **0.242 (VI)**
  → 0.554 (VII) → 0.406 → 0.134. The **act VI trough is a −48% pre-chorus collapse
  before the biggest chorus** — exactly right, and currently accidental. Document and
  protect it.
- Of the metadata's 11-point `narrative.learning` timeline, **4 of 11 check out.**

### The hook payoff is mis-set, two ways

`runrealfast-hi.wav` has audio from 0.000 s (peak −0.7 dBFS) but its **first syllable
onset is at 0.372 s**. Scheduled on the downbeat, the three syllables land at beats
**1.75 / 2.47 / 3.31** — a loosely-performed dotted-eighth chain. **The downbeat of
every hook-payoff bar is textually empty.**

Worse, the long layer collides with it. Reconstructing from event times plus measured
internal onsets: **16 cross-syllable collisions under 60 ms, median 4.6 ms**, in bars
**31, 35, 39, 54, 56, 62, 64, 76, 94, 96, 102** — every time the long layer's "run"
lands on the main layer's "fast" at beat ≈3.31, four of them under 1 ms apart. Bar 56
stacks four long layers at beats 3.32/3.36/3.41/3.44. The long layer's own "REAL" and
"fast" land at beats **5.31 and 5.57** — it spills a full bar past its own barline.

**Actions:** (1) re-anchor `runrealfast-hi` so "run" hits the downbeat. (2) Move
`runrealfast-long-*` off beat 1.6 — beat 2.0 kills all 16 collisions. (3) Play
`iwannahide-hi.wav`; it is 1.0 s and it makes chorus line 2 real. (4) Play
`sos-dash-*.wav` in act II so the sung SOS exists before its own beeped answer.
(5) Extend the beeped reply across bars 80–82 and 88–90. (6) Play `threeofus-rise.wav`
once in bar 104.

---

## 8. Mix: the pump doesn't exist, the image is mono, the master is a 2.45:1 AGC

- **The 0.72 tube pump is not in the audio.** Deepest dip anywhere in 150–350 Hz is
  −6.5 dB, exactly what the 0.50 music duck alone predicts. The tube bus is buried by
  `bass` in its own band (dash mean gain 0.386 vs bass 0.860). Also the music duck gets
  *weaker* as the record gets busier (−6.5 dB in act III → −2.2 dB in act V), which is
  backwards.
- **The stereo image is composed and inaudible.** 3096 of 3814 events carry a pan and
  the design is systematic — but S/M runs **−14 to −30 dB**, correlation 0.95–0.999,
  and **mono-fold loss ≤ 0.2 dB in every bar from 8 to 103**. Every panned voice is
  20 dB below the mono core. The widest passage is bars 104–111 — after the drums
  leave. Width is a byproduct of thinning, not a parameter.
- **`bass` is the masker, by 4× over everything else combined.** Summed in-band gain
  competing with the 61 lead events: bass 209.5, taps 41.2, hats 55.9, percs 37.5,
  dots 30.0, dashes 29.8. Hats are noise-floor.
- **The low-octave lead doubles never lift and should be deleted:** `iwanna-c-sung`
  (n=5) median **−1.1 dB**, `away-lo` (n=3) **−1.0**, `hideaway-hi` (n=4) +1.7. Against
  `iwanna-b-sung` +10.3 and `runrealfast-hi` +9.4.
- **Cuts with no perceptual loss:** `stab` in act VII (130 events, both −22 dB under a
  bed 20 dB louder in the same band, duplicating `guitar-chord`); the `sub`/`dash`/
  `bass` triple-stack in 60–200 Hz across 29 bars (keep one); high-pass at 32 Hz (11
  events at 24.5–30.9 Hz, ~1 dB of ceiling back).
- **The master is a 2.45:1 macro-compressor at a 3-second time constant** —
  `master = 0.408 × mix − 3.94`, giving +9.20 dB to the quietest fifth and +4.70 dB to
  the loudest. Not a peak limiter: mean gain is +6.9 dB and the ceiling has **3 dB
  unused**. Raw edit LRA 8.1 → master 4.2, **48% gone**.

### The seven explosions, measured (Δ = 4 s pre → post, K-weighted)

| bar | kind | strength | mix | competitive | flow |
| --- | --- | --- | --- | --- | --- |
| 29 | snap | 0.42 | +1.01 | +1.62 | +1.89 |
| 40 | gravity | 0.62 | **−4.75** | −1.45 | −2.63 |
| 48 | recoil | 0.78 | +2.09 | **−0.94** (sign flips) | +0.28 |
| 64 | unfurl | 0.48 | −1.57 | **cut from the edit** | −1.95 |
| 76 | blast | 1.12 | **+3.21** | +2.41 | +3.67 |
| 92 | shatter | 0.90 | +0.53 | +0.53 | +1.17 |
| 104 | exhale | 0.34 | **−6.72** | −3.19 | −4.76 |

`strength` does not predict outcome. The two largest gestures are **subtractive**, and
the master takes 60–70% of both. The competitive edit **cuts the bar-64 unfurl
entirely** (its 119.95→130.45 splice lands on it) and **inverts bar 48 from a +2.09
arrival into a −0.94 dip**.

Note for the score: the explosions are master-bus modulators at gain 0, so all seven
structural markers **contribute no onset**. Bar 92's is a pure spatial gesture with no
rhythmic change at all.

---

## 9. The two edits

| | competitive 2:21 | flow 2:47 |
| --- | --- | --- |
| Integrated | −11.2 LUFS | −11.7 LUFS |
| LRA | 4.2 LU | **6.3 LU** |
| True peak | −2.9 dBTP | −3.0 dBTP |
| Compression ratio (mix→master) | 2.45:1 | **1.33:1** |
| 4-s block range, 0:04–2:00 | 3.6 dB | **6.1 dB** |
| bar-76 blast | +2.41 LU | **+3.67 LU** |
| bar-48 recoil | −0.94 (inverted) | **+0.28** |
| explosions kept | 6 of 7 | **7 of 7** |
| internal seams | 3 | **1** |

`bin/cut-flow.sh` keeps acts V and VII whole, shortens the bars 68–71 hole by 6 s (not
8) so bar 72 still lands on a bar line, lets the bar-67 tail ring into a one-bar
caesura, and now runs to the end — **including the bar-110 choir, the bop at 221.5 and
the two hang-up clicks at 224.55/224.62.** ("The last sound is a hang-up, after the
music" is the point of the ending; the earlier 216 s trim threw it away — my error,
fixed.)

**Neither edit is the answer while the score has a null in it.** Fix §4 and the
`[b]`/`[c]` split can be deleted entirely: 58 → end plays as one unbroken region.

---

## 10. If you only do five things

1. **Put something pitched above 1 kHz** (§3). Everything above 4 kHz is worth 0.1 LU.
2. **Fill or delete bars 68–71** (§4). Do not ship a master with a −93 dBFS null in it.
3. **Move the arc into the kick** (§2, §3). 41% of the energy, four identical notes,
   82 bars. One dropped downbeat at 29, one 16th displacement across 76–79, one
   half-time stretch through 68–71 outweighs all 1172 hats.
4. **Move `runrealfast-long-*` to beat 2.0** (§7). Kills 16 syllable collisions in the
   hook's own payoff bar, at zero cost.
5. **Take the loudness at the ceiling, not out of the 3-second envelope** (§8). There
   are 3 dB of unused true peak.
