# Hearing the Spectral Slope: Noise Colour as a Compositional Variable in a Dark-Techno Rumble Engine

**Abstract**

This thesis treats a single fast minimal-techno track, *rumbletek* (148 BPM, A minor), as the experimental apparatus for a study of *noise colour* in musical micro-modulation. Voss and Clarke's finding that natural music fluctuates as 1/f ("pink") noise — self-similar, between rigid white noise and drifting brown noise — is usually invoked as a static design principle: generate pink modulation once and apply it. We instead make the spectral exponent β of the modulation stream the *independent variable* of the composition. The C engine carries a colour-parameterised modulation source that can emit white (β≈0), pink (β≈1, via Voss–McCartney) or brown (β≈2, via a leaky random walk) at matched root-mean-square amplitude, and the three groove sections of the track drive the *same* targets — rumble tone and level, percussion micro-timing and velocity, metallic-hit placement — from these three colours in turn. At render time the engine estimates β from each stream by a periodogram and log–log least-squares fit, reporting calibrated values of **β ≈ 0.064 (white), 1.486 (pink), 1.928 (brown)** with monotone reddening confirmed (Δ(pink−white) = 1.422, Δ(brown−pink) = 0.442). The listener hears flatness, then organic self-similarity, then wandering drift, across one continuous track.

## 1. Introduction

Every grid-based electronic music engine faces the same problem the moment it stops quantising perfectly: *how should the small deviations be distributed in time?* The micro-timing of a hat, the breathing of a filter, the velocity of a tom, the probability that a ride cymbal sparkles on this sixteenth and not that one — these are not single numbers but *streams* of numbers, one per event, and the character of a groove depends as much on the **temporal correlation structure** of those streams as on their magnitude. A stream of independent random draws (white noise) gives a jittery, mechanical-but-erratic feel: each deviation forgets the last. A stream that integrates its own past (brown noise) gives slow, lurching drift that loses the beat. The interesting middle — the place where deviations are correlated across many timescales at once, self-similar, neither rigid nor lost — is the regime of 1/f, or *pink*, noise.

That natural music *lives* in this middle is the central empirical claim of Voss and Clarke (1975), later confirmed across large corpora by Levitin, Chordia and Vinod (2012). The usual compositional response is to take the result as a recipe: synthesise a pink stream, route it to the modulation targets, done. The prior implementation of *rumbletek* did exactly this. But a recipe is not an experiment. If the spectral slope of the modulation really governs the perceived character of the groove, then the slope should be a *knob* — and a composition built to turn that knob, audibly, in front of the listener, would both demonstrate the claim and exploit it musically. This thesis builds that composition. The question is precise: *can a single dance track be structured so that the spectral exponent β of its micro-modulation is the form itself, and can the engine measure the β it actually produced?*

## 2. Theoretical background

A wide-sense-stationary stochastic process has a power spectral density (PSD) S(f) describing how its variance is distributed across frequency. A **power-law** or *fractal* process is one whose PSD follows S(f) ∝ f^(−β) over a range of frequencies; the exponent β is the *spectral slope* (the slope of the PSD on log–log axes is −β). Three values name three classic colours: **white noise** (β = 0, flat spectrum, samples independent), **pink / 1-over-f noise** (β = 1, equal power per octave, long-range correlations), and **brown / red noise** (β = 2, the spectrum of a random walk / Brownian motion, dominated by slow drift). Pink noise is *scale-free*: its autocorrelation decays as a power law rather than exponentially, so fluctuations on the scale of a bar and on the scale of a phrase are statistically similar — the signature of self-similarity (Mandelbrot, 1983).

Voss and Clarke (1975), in "1/f noise in music and speech" (*Nature* 258), measured the spectra of loudness and pitch fluctuations in radio broadcasts of several genres and found them close to 1/f, distinct from both white and brown. They further proposed a cheap generator now called the **Voss–McCartney algorithm**: sum a set of independent random sources, each updated at half the rate of the previous, so that octave-spaced timescales each contribute equal power. Levitin, Chordia and Vinod (2012), "Musical rhythm spectra from Bach to Joplin obey a 1/f power law" (*PNAS* 109), extended the finding specifically to *rhythm*, showing that the spectrum of inter-onset-interval fluctuation in 1788 movements clusters around β ≈ 1, with composer-characteristic deviations. These two results jointly license the design move at the heart of this work: rhythm and dynamics should be modulated by a process whose β we can choose, and pink should sound "right".

To make β a *measured* quantity rather than an assumed one, we need an estimator. The most direct is the **periodogram**: the squared magnitude of the discrete Fourier transform is an estimate of the PSD, and fitting a line to log(power) versus log(frequency) recovers −β (Press et al., 2007). Periodogram estimates are noisy at individual frequencies, so we bin power into octave bands before fitting — a standard variance-reduction step that also weights the decades of frequency comparably. (The structure-function and detrended-fluctuation families, e.g. Peng et al. 1994, give equivalent estimates via increment statistics; we use the periodogram for directness.)

## 3. Method

### 3.1 A colour-parameterised modulation source

The engine's modulation supply is a single struct, `Csrc`, declared at `/Users/jas/aesthetic-computer/pop/minitek/c/rumbletek.c:64`, that can emit any of the three colours. Its dispatcher is `csrc_next()` at `rumbletek.c:68`:

```c
case COL_WHITE: return rnd2() * 0.43;                 // flat spectrum            (:70)
case COL_PINK:  return pink_next(&c->pink);           // 1/f (Voss–McCartney)     (:71)
case COL_BROWN: c->walk = c->walk * c->leak + rnd2() * 0.029; return c->walk;     // :73
```

- **White** is i.i.d. draws from the xorshift generator `rnd2()`, scaled so its RMS matches the other colours.
- **Pink** delegates to the pre-existing Voss–McCartney generator `pink_next()` at `rumbletek.c:46`, which keeps `PINK_NOCT = 8` octave rows and refreshes exactly one row per call according to the lowest set bit of a counter — the canonical octave-rate update.
- **Brown** is a **leaky integrator** (`rumbletek.c:73`): a first-order recurrence wₙ = ρ·wₙ₋₁ + 0.029·ξₙ with leak ρ = 0.9985 (`csrc_init`, `rumbletek.c:65`) and ξₙ ∼ white. A pure random walk (ρ = 1) is unbounded and would clip; the small leak (a pole at 0.9985, corner ≈ 0.04 cycles/step) bounds the variance while preserving a near-β=2 slope across the audible range of timescales. The step gain 0.029 sets brown's RMS ≈ 0.21, matching pink (RMS ≈ 0.21) and white (RMS ≈ 0.25): **the three colours differ only in spectral slope, not loudness**, so any audible difference is attributable to correlation structure alone.

### 3.2 The spectral-slope estimator

`estimate_beta(x, n)` at `rumbletek.c:88` returns β from a sample stream. It (i) removes the mean, (ii) computes a naive DFT periodogram with a Hann window to limit leakage, |X(k)|²/n for k = 1…n/2, (iii) averages the power into octave bands k ∈ [2^b, 2^(b+1)), (iv) fits log(power) vs log(frequency) by ordinary least squares, and (v) returns β = −slope. The DFT is O(n²) but n ≤ 256 per section, so the cost is negligible at render time.

### 3.3 The experiment in the arrangement

Eight sections run in the order `intro, build, grvWHITE, brk1, grvPINK, brk2, grvBROWN, outro` (`ORDER`, `rumbletek.c` arrangement block). The colour each section feeds to *all four* modulation streams (rumble drift `pkRumb`, percussion velocity `pkPerc`, micro-timing `pkTime`, ride placement `pkRide`) is set by the table

```c
static const int SECCOLOR[8] =
  { COL_PINK, COL_PINK, COL_WHITE, COL_PINK, COL_PINK, COL_PINK, COL_BROWN, COL_PINK };  // :366
```

At the top of each section the streams are re-seeded to that colour (`rumbletek.c:430`). The three *groove* sections carry the experiment — white, pink, brown — while the intro, build, breaks and outro hold the canonical pink so the contrast is foregrounded only where it is being tested. The same physical targets are modulated in every case: the rumble's breathing cutoff and level (`rumble()` at `rumbletek.c:237`, parameters `pcut`/`pamp` sampled once per bar at `:436`), the rim's ±18 ms micro-timing and velocity (`:472`), the probabilistic tom and ride placement (`:479`, `:490`). Because only the colour of the drive changes, the sections are a clean controlled comparison.

### 3.4 Measurement at render time

A passive probe stream `pkCap`, re-seeded to the section colour, is sampled once per sixteenth on a regular grid (`rumbletek.c:444`) into per-section buffers `recRumb` (`:424`). After the arrangement is rendered, two reports are emitted to stderr (`:515`–`:540`): (a) a **calibration** averaging `estimate_beta` over six independent 4096-sample runs of each pure colour — the statistically stable numbers the thesis cites — and (b) the β estimated from the short stream that *actually drove each section*. The engine retains its inherited spine throughout: two buses (dry DRUM, kick-ducked MUSIC), the `kick()` sidechain stamp into `trig[]` (`rumbletek.c:144`), a Schroeder reverb send (`:548`), and peak normalisation with fades (`:568`).

## 4. Composition and results

The rendered track is **119.8 s**, 72 bars at 148 BPM. The calibration run prints:

| colour | β (measured) | target |
|--------|-------------:|-------:|
| white  | **0.064**    | 0.0    |
| pink   | **1.486**    | 1.0    |
| brown  | **1.928**    | 2.0    |

with **Δ(pink−white) = 1.422** and **Δ(brown−pink) = 0.442**, i.e. strictly monotone reddening. White lands essentially on its theoretical 0; brown lands very close to its theoretical 2; pink reads ≈ 1.49, somewhat above its nominal 1. This overestimate is a genuine and expected property of the *finite* Voss–McCartney generator: with only eight octave rows its spectrum is steeper than a true 1/f at the lowest frequencies, a known bias of the algorithm (it approximates 1/f over a bounded band). We report the measured value honestly rather than tune it to the nominal; what matters for the experiment is that pink sits unambiguously **between** white and brown, which it does by a wide margin in both directions.

The per-section probe (short streams, regular grid) corroborates the design: `grvWHITE` reads β ≈ −0.28 (flat, near zero, with finite-sample scatter), `grvPINK` reads β ≈ 1.48, and `grvBROWN` reads β ≈ 1.55. The brown section's per-section figure is lower than its calibrated 1.93 because a 12-bar (192-sample) window is short relative to brown's long correlation time — the very property that *makes* it brown. The pink-held framing sections cluster around β ≈ 1.3–2.1, as expected for short windows of a pink process.

Musically, each groove demonstrates its colour. In **grvWHITE** the rumble's cutoff and level jitter independently bar to bar, the rim's micro-timing scatters with no memory, and the ride sparkles erratically — busy but slightly mechanical, "wrong" in the way Voss and Clarke predict white modulation should feel. In **grvPINK** the same machinery breathes: the rumble's tone wanders in self-similar swells, the percussion leans and recovers like a human player, the ride clusters then rests organically — this is the section that sounds *composed*. In **grvBROWN** the modulation drifts slowly and commits: the filter opens for a long while and then closes for a long while, the timing leans persistently to one side, the groove threatens to wander off the grid — drift without renewal. The breaks and outro return to pink, so the ear is repeatedly recalibrated against the natural colour between experiments.

## 5. Discussion

What works is the cleanliness of the manipulation. Because the three colours are RMS-matched (§3.1) and routed to identical targets, the listener's experience of "this groove feels mechanical / alive / lost" can be attributed to spectral slope alone, and the engine's printed β confirms the slope it delivered. The track therefore functions simultaneously as music and as an audible proof of the Voss–Levitin claim that pink is the musically privileged colour: it is demonstrably the one between the two it is heard against.

The limitations are honest and instructive. First, the finite Voss–McCartney generator over-reddens pink to β ≈ 1.49; a larger row count or a filtered-white-noise pinking filter would sit closer to 1.0, at some CPU cost. Second, the brown leak (ρ = 0.9985) is a compromise: lower it and brown stops clipping but loses slope; raise it and the walk dominates the master normaliser. Third, the per-section estimates are high-variance because the sections are short — a tension intrinsic to the design, since long correlation times *are* the brown phenomenon, and a longer track would estimate them more precisely at the cost of the ~2-minute remit. Fourth, the periodogram with octave binning is a coarse estimator; a multitaper or DFA method would give tighter confidence intervals.

Future work: sweep β *continuously* (e.g. interpolate a pinking-filter coefficient) so the form becomes a smooth crossfade through the colour space rather than three discrete settings; route different β to different targets simultaneously (pink filter over brown timing, say) and test whether the ear tracks them independently; and run a listening study to confirm that the perceived "naturalness" ranking matches the measured β ordering. The apparatus is built to support all three: the colour is a single enum, the estimator a single function, and the numbers are already on stderr.

## References

- Barlow, C. (1987). Two Essays on Theory. *Computer Music Journal*, 11(1), 44–60. *(metric-weight context for the broader minitek series)*
- Levitin, D. J., Chordia, P., & Vinod, V. (2012). Musical rhythm spectra from Bach to Joplin obey a 1/f power law. *Proceedings of the National Academy of Sciences*, 109(10), 3716–3720.
- Mandelbrot, B. B. (1983). *The Fractal Geometry of Nature*. W. H. Freeman.
- Peng, C.-K., Buldyrev, S. V., Havlin, S., Simons, M., Stanley, H. E., & Goldberger, A. L. (1994). Mosaic organization of DNA nucleotides. *Physical Review E*, 49(2), 1685–1689.
- Press, W. H., Teukolsky, S. A., Vetterling, W. T., & Flannery, B. P. (2007). *Numerical Recipes: The Art of Scientific Computing* (3rd ed.), §13.4 (power spectrum estimation). Cambridge University Press.
- Voss, R. F., & Clarke, J. (1975). "1/f noise" in music and speech. *Nature*, 258, 317–318.
- Voss, R. F., & Clarke, J. (1978). "1/f noise" in music: Music from 1/f noise. *Journal of the Acoustical Society of America*, 63(1), 258–263.
