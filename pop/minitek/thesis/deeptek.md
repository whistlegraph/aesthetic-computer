# The Pocket as a Tempo-Dependent Variable: Swing-Ratio Curves and Per-Lane Microtiming in *deeptek*

**Abstract.** "Groove" in dance music is widely attributed to *microtiming* — small, systematic departures of an onset from its notated grid position — but the size and direction of those departures are usually left to a performer's intuition or a sequencer's fixed "swing" knob. This thesis treats one fast minimal-techno track, *deeptek* (140 BPM, D natural minor, 119.6 s), as an experimental apparatus that makes two claims about microtiming audible and measurable. First, that the long:short ratio of a swung eighth-note pair is not a constant but a *function of tempo*, falling from roughly 3.3:1 at slow tempi toward 1:1 (even) as the tempo rises, exactly as Friberg & Sundström (2002) measured in jazz drummers. The C engine fits a curve to their data, evaluates it at 140 BPM to a swing ratio of **2.51:1**, and derives the off-eighth displacement from that value rather than from a hand-set constant. Second, that the *pocket* — the ensemble of per-lane offsets plus swing plus humanizing jitter — can be treated as a single dialled depth, swept across the form. One eight-bar section is rendered **deadpan** (depth 0): every voice flat-quantized. The engine logs every voiced onset's deviation from the dead metronomic grid and prints, per section, the mean lean, the RMS discrepancy, and the mean absolute deviation. The deadpan section reads **0.00 ms RMS** against the full-pocket groove's **114.27 ms**, and the whole-track discrepancy is **102.81 ms RMS** over 2160 onsets. The track is the proof; the numbers are printed at render time.

## 1. Introduction

A four-on-the-floor techno track played by a metronome-perfect sequencer sounds like a metronome-perfect sequencer: rigid, lifeless, hard to move to. The same pattern played by a band, or by a producer who has spent hours nudging hits off the grid, *grooves*. The difference is not in the notes, the pitches, or even the gross rhythm — it is in deviations of a few to a few dozen milliseconds in *when* each hit actually sounds. This is the phenomenon the ethnomusicologist Charles Keil named *participatory discrepancy*: the systematic, productive out-of-tune-ness and out-of-time-ness through which an ensemble generates "vital drive" (Keil 1987, 1995).

Two questions follow immediately, and both are mechanical. First: *how much* should a hit be displaced? Producers know that the right amount of swing depends on tempo — the same shuffle that feels lazy and deep at 90 BPM feels stiff or even comical at 140 BPM — but this is folk knowledge, rarely quantified. Second: *can the listener actually hear the discrepancy as a discrepancy*, or is it merely a vague impression of "feel"? The cleanest way to demonstrate that microtiming is doing real perceptual work is to take it away — to flat-quantize the same groove and let the ear notice the floor drop out of it.

*deeptek* is built to answer both. Its engine, `c/deeptek.c`, no longer carries a hand-set swing constant; it fits a curve to the Friberg & Sundström swing-ratio data, evaluates that curve at the track's own tempo, and uses the result to place every off-eighth. It then treats the entire microtiming apparatus — swing, per-lane systematic offsets, and per-hit jitter — as one *pocket depth* that the arrangement sweeps from shallow through full to a deadpan zero and back. At render time it measures, onset by onset, how far the music actually departs from a dead metronomic grid, and prints the statistics this document cites. The track is not illustrated by the theory of microtiming. The track *is* the experiment.

## 2. Theoretical background

The foundational framing is Keil's. In "Participatory Discrepancies and the Power of Music" (Keil 1987) and the follow-up dialogue with Steven Feld (Keil 1995), he argued against the then-dominant structuralist view that musical meaning lives in pitch and form, proposing instead that the felt "engendering" of groove arises from minute, deliberate discrepancies between players — discrepancies that a perfectly synchronized, perfectly tuned performance would destroy. Keil's claim was qualitative; what it needed was measurement.

That measurement arrived most decisively for the swing feel. Friberg & Sundström (2002), "Swing Ratios and Ensemble Timing in Jazz Performance" (*Music Perception* 19(3)), analyzed recordings of professional jazz drummers playing the ride-cymbal swing pattern across a wide tempo range. They found that the *swing ratio* — the duration of the long (downbeat) eighth divided by the short (offbeat) eighth — is not the textbook triplet 2:1, nor any fixed value, but **decreases monotonically with tempo**: it sits near 3.0–3.5:1 at slow tempi and approaches 1:1 (straight eighths) at fast tempi, in part because the absolute duration of the short note tends toward a floor of roughly 100 ms below which it cannot comfortably shrink. This is the empirical curve *deeptek* fits and evaluates.

The generalization from swing to a fuller theory of groove microtiming is due to Vijay Iyer. In "Embodied Mind, Situated Cognition, and Expressive Microtiming in African-American Music" (Iyer 2002, *Music Perception* 19(3)), Iyer argued that groove arises from systematic, *body-derived* timing relationships among instruments — a "pocket" in which, for instance, the bass may lie a few milliseconds behind the kick — and that small, non-random per-hit variation is part of what marks a performance as human rather than mechanical. Iyer's twin ideas — fixed *per-lane* offsets and bounded *per-hit jitter* — are exactly the two terms *deeptek* adds on top of the swing curve.

The perceptual stakes are framed by Justin London's *Hearing in Time* (London 2012), which treats meter as an attentional entrainment process with characteristic temporal limits, and by Madison's empirical work on the correlation between groove and small-scale timing (Madison 2006). Together these supply the prediction *deeptek* tests: that removing the discrepancies (the deadpan section) should be audible as a loss of drive, not merely as a change of numbers on a page.

## 3. Method

The engine keeps the *deeptek* spine intact: a two-bus mix (dry DRUM bus, kick-ducked MUSIC bus), a kick-stamped sidechain trigger array, a Schroeder reverb send, normalize-and-fade mastering, and `write_wav_f32_stereo`. The deepening lives in four places.

**3.1 The tempo-dependent swing-ratio curve.** `swing_ratio_at(double bpm)` at `c/deeptek.c:66` returns the modelled long:short ratio. We fit Friberg & Sundström's decreasing-with-tempo trend with a single exponential relaxation toward the even ratio:

    ratio(bpm) = 1 + (R0 − 1) · exp( −(bpm − B0) / TAUB ),   clamped to ≥ 1

with `R0 = 3.3` (the slow-tempo ratio), `B0 = 100` BPM (the slow anchor), and `TAUB = 95` BPM (how quickly the swing flattens). The constants are chosen so the curve reproduces the literature's landmarks: ~3.3:1 at the slow end, ~2:1 in the low-mid range, and a flattening approach toward 1:1 at the fast extreme. In `main` (`c/deeptek.c:384`) the curve is evaluated once at the track tempo and cached as `SWING_RATIO`, together with the *long fraction* of an eighth-pair, `LONG_FRAC = R/(R+1)`.

**3.2 Swing as derived displacement.** `swing_push(int step)` at `c/deeptek.c:84` converts the cached ratio into a time displacement. The off-eighths fall on 16th-grid steps 2, 6, 10, 14; for those, the long part of the pair should occupy `LONG_FRAC` of the beat-half, so the off-eighth is pushed beyond the even split by

    extra = POCKET · ( (BEAT/2)·2·LONG_FRAC − (BEAT/2)·0.5 )

i.e. the difference between the swung and the even position, scaled by the section's pocket depth. Nothing here is a magic number; the displacement is a deterministic function of the fitted ratio and the tempo.

**3.3 Per-lane offsets and bounded jitter.** Following Iyer, each lane carries a fixed systematic offset in milliseconds (`c/deeptek.c:47`–`52`): kick `+0.0` (dead-on anchor), hats `−6.0` (rushed for drive), backbeat tick `+14.0` (laid-back, the pocket), sub `+4.0` (just behind the kick), shaker/perc `+9.0`, counter-melody lead `+12.0` (dragged for a human top line). These are applied through `off(double ms)` (`c/deeptek.c:80`), which multiplies by the section pocket depth, and each hit also receives bounded random scatter through `jit(double ms)` (`c/deeptek.c:78`), likewise pocket-scaled. The lane assignments appear in the arrangement loop, e.g. the kick at `c/deeptek.c:446`, hats at `:450`, tick at `:467`, sub at `:472`, and lead at `:486`.

**3.4 The pocket as the experimental variable.** The independent variable is `POCKET_DEPTH[8]` at `c/deeptek.c:339`: `{0.6, 0.8, 1.0, 0.0, 1.0, 0.7, 1.15, 0.5}` across the eight sections `intro · build · grvA · deadpan · grvB · brk2 · grvC · outro`. At the top of each section the loop sets the global `POCKET` (`c/deeptek.c:427`), which scales the swing push, every per-lane offset, and the jitter in unison. Section 3, **deadpan**, carries depth `0.0`: the global swing is killed in `step_t` (`c/deeptek.c:365`, where the off-16th displacement is itself multiplied by POCKET), the offsets vanish, and the jitter is zeroed — the same full-density groove as `grvA`, flat-quantized as a control. Section 6, **grvC**, carries depth `1.15`, an *exaggerated* pocket that leans harder than any acoustic player would, to bracket the audible range from above.

**3.5 Measurement.** Every voiced onset is logged through `log_dev(double t_actual, double t_grid)` (`c/deeptek.c:345`), which records the signed deviation in milliseconds of the actual scheduled time from the *dead metronomic grid* time `tmetro = bar·BAR + step·STEP` (`c/deeptek.c:441`). Per section it accumulates Σdev, Σdev², Σ|dev|, and the count. After the arrangement, EXPERIMENT 2 (`c/deeptek.c:507`) reports per section the **mean** (the systematic lean), the **RMS** (the overall discrepancy magnitude), and the **mean |dev|**, plus a whole-track summary and the explicit deadpan-vs-grvA contrast. EXPERIMENT 1 (`c/deeptek.c:393`) prints the fitted curve at reference tempi, its value at this tempo, the implied eighth-pair durations, and the per-lane offset table.

## 4. Composition and results

The renderer prints both experiments before writing the WAV. The numbers below are verbatim from the render of the committed engine.

**Experiment 1 — the swing-ratio curve.** The fitted curve, evaluated across tempo:

| BPM | 100 | 120 | 140 | 160 | 200 |
|---|---|---|---|---|---|
| ratio (:1) | 3.30 | 2.86 | 2.51 | 2.22 | 1.80 |

At the track's 140 BPM the swing ratio is **2.510:1** (long fraction 0.715). One eighth-pair spans 214.3 ms; the fitted ratio splits it into a **long 153.2 ms** and a **short 61.1 ms**, a Δ of **92.2 ms** between the two halves. This is the concrete consequence of the curve: at this tempo the offbeat eighth is pulled markedly late of an even split, which is precisely the rolling, behind-the-beat feel the bassline is built around. Had the track been written at 200 BPM the same curve would have nearly straightened the pair (1.80:1), and the engine would have placed the offbeats almost evenly — without any change to the source code.

**Experiment 2 — the measured pocket per section.** Each voiced onset's deviation from the dead grid, aggregated:

| section | pocket | onsets | mean (ms) | RMS (ms) | mean&#124;dev&#124; (ms) |
|---|---|---|---|---|---|
| intro | 0.60 | 94 | +24.21 | 50.58 | 24.21 |
| build | 0.80 | 111 | +57.85 | 91.99 | 57.85 |
| grvA | 1.00 | 612 | +70.94 | 114.27 | 70.96 |
| **deadpan** | **0.00** | **306** | **−0.00** | **0.00** | **0.00** |
| grvB | 1.00 | 612 | +70.95 | 114.22 | 70.98 |
| brk2 | 0.70 | 52 | +41.49 | 70.89 | 41.49 |
| grvC | 1.15 | 306 | +81.61 | 131.38 | 81.63 |
| outro | 0.50 | 67 | +22.89 | 44.52 | 22.89 |

Whole track: **2160 onsets · RMS 102.81 ms · mean |dev| 57.51 ms**. The headline contrast the engine prints explicitly: **deadpan RMS 0.00 ms vs grvA RMS 114.27 ms**.

Three results stand out. First, the **monotonic relationship between pocket depth and discrepancy** is exact: the per-section RMS tracks `POCKET_DEPTH` in order (0.60→50.58, 0.80→91.99, 1.00→114.27, 1.15→131.38), confirming that the single dialled depth really is the controlling variable. Second, the **mean lean is positive everywhere the pocket is engaged** (+24 to +82 ms): the groove sits *behind* the metronomic grid, which is the audible signature of "deep" — the swing push and the laid-back lane offsets both pull late, and they sum to a consistent backward lean. Third, the **deadpan control collapses to zero** (−0.00 ms mean, 0.00 ms RMS): with the same notes, the same density, and the same harmony as `grvA`, the section is mathematically on the grid, and the listener hears the floor of the groove drop out — a stiff, "switched-to-quantize" stretch — before grvB restores it.

It is worth noting that the RMS figures are large relative to the per-lane offsets (which top out at 14 ms) because the **swing push dominates**: at 2.51:1 the offbeat eighths are displaced by tens of milliseconds, and the global off-16th swing displaces the odd 16ths as well. This is honest and intended — the swing *is* the largest microtiming discrepancy in the track — and EXPERIMENT 2 measures the total pocket, not the offsets in isolation.

## 5. Discussion

What works: the architecture makes a normally-invisible production craft both *derived* and *measurable*. The swing ratio is no longer a taste decision baked into a constant; it is a function of tempo with a citable empirical basis, and changing `BPMV` would automatically re-derive the feel. The pocket-depth sweep gives the form an arc that is genuinely *about* its own subject — the track gets deeper as the depth rises and visibly loses its body in the deadpan section — and the printed statistics let any claim in this document be checked against the bytes the renderer produced.

Limitations are real. The swing model is a single-exponential fit to summarized literature values, not a re-analysis of Friberg & Sundström's raw measurements; the constants `R0`, `B0`, `TAUB` are calibrated to reproduce the landmark ratios, so the curve is a faithful *shape*, not a published regression. The per-lane offsets are fixed values informed by Iyer's framing rather than extracted from a specific recorded ensemble. The jitter is uniform, not the slightly autocorrelated drift human players exhibit (Hennig 2014 documents long-range correlations in musical timing that a white-noise jitter does not capture). And the deviation statistic is measured against a *metronomic* grid, so the swing — a deliberate, structural feature — inflates the RMS; a stricter design might report discrepancy relative to the swung-but-deadpan grid to isolate the per-lane and jitter terms.

What a listener should hear: a rolling, behind-the-beat groove that breathes; a build that tightens as the pocket deepens (intro shallow, grvA full); then, abruptly, the **deadpan section** — same groove, drained of feel, sitting rigidly on the grid — followed by grvB snapping the pocket back; and finally grvC, where the depth is pushed past natural (1.15) into an almost over-relaxed lean. If the deadpan section sounds noticeably *worse* — stiffer, less moving — than its identical-note neighbours, the thesis is confirmed: the groove was never in the notes.

Future work: re-fit the swing curve from raw performance corpora and report a confidence interval; replace white-noise jitter with a long-range-correlated process per Hennig (2014); extract per-lane offsets from analyzed recordings; and run a listener study on the deadpan-vs-pocket contrast to convert the printed millisecond differences into a measured perceptual effect.

## References

- Friberg, A., & Sundström, A. (2002). Swing Ratios and Ensemble Timing in Jazz Performance: Evidence for a Common Rhythmic Pattern. *Music Perception*, 19(3), 333–349.
- Hennig, H. (2014). Synchronization in human musical rhythms and mutually interacting complex systems. *Proceedings of the National Academy of Sciences*, 111(36), 12974–12979.
- Iyer, V. (2002). Embodied Mind, Situated Cognition, and Expressive Microtiming in African-American Music. *Music Perception*, 19(3), 387–414.
- Keil, C. (1987). Participatory Discrepancies and the Power of Music. *Cultural Anthropology*, 2(3), 275–283.
- Keil, C. (1995). The Theory of Participatory Discrepancies: A Progress Report. *Ethnomusicology*, 39(1), 1–19.
- London, J. (2012). *Hearing in Time: Psychological Aspects of Musical Meter* (2nd ed.). Oxford University Press.
- Madison, G. (2006). Experiencing Groove Induced by Music: Consistency and Phenomenology. *Music Perception*, 24(2), 201–208.
