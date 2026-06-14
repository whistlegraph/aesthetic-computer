# Revealing the Metric Tree: Barlow Indispensability as a Compositional Variable in a TB-303 Acid-Techno Engine

**Abstract**

This thesis treats a single fast minimal-techno track, *acidtek* (146 BPM, E minor), as the experimental apparatus for a study of metric indispensability. Where prior work assigns accents to a drum machine or acid line by hand or by static pattern, we replace the hand-tuned hierarchy with Clarence Barlow's true recursive indispensability function (Barlow, 1987) computed at render time over the stratified meter 2·2·2·2. The C engine derives the full 16-pulse indispensability vector from the prime factorization, dense-ranks it, and verifies that its descending order reproduces Barlow's canonical ordering for a sixteen-pulse bar: `0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15`. The composition is then structured as a controlled experiment in which the *independent variable* is the depth of the revealed metric tree: across eight sections, a progressive schedule permits the TB-303 line to accent an increasing top-*k* of the indispensability hierarchy, from the bare downbeat (*k*=1) to the entire profile (*k*=16) and back. The engine prints a per-section accent census; the resulting accent fraction rises from 33.3% to 100% at the peak, and the mean indispensability of accented pulses falls from 0.900 to 0.544, demonstrating that the listener hears the meter assemble and dissolve as a function of a single ranked quantity.

## 1. Introduction

Four-on-the-floor dance music is, on the surface, the least syncopated music imaginable: a kick on every quarter, a hat on every offbeat eighth. Yet the genre's expressive interest lives almost entirely *between* those load-bearing pulses, in the placement of secondary accents — the snap of a clap, the bite of an acid note — onto weaker positions of the bar. The art of the acid line, in particular, is the art of *which sixteenths to lean on*. A TB-303 programmed with accents on only the strong pulses sounds stiff and obvious; one that accents indiscriminately sounds chaotic and loses the groove. The interesting region is the controlled middle, where accents are distributed according to the *metric weight* of each pulse.

This raises a precise question: is there a principled, computable ordering of the sixteen pulses of a 4/4 bar from most to least "essential" to the meter, and can a composition be built so that the *progressive admission of weaker pulses into the accent set* becomes the form itself? This thesis answers yes on both counts. The ordering is Barlow's indispensability function; the composition is *acidtek*, whose eight sections are eight settings of a single experimental parameter — how deep into the metric tree the acid line is allowed to reach.

The contribution is twofold. First, the engine implements Barlow's *recurrence* rather than a tabulated approximation, generalizing over the prime factorization and self-verifying against the published ordering. Second, the arrangement is designed as an audible parameter sweep: the metric hierarchy is *revealed* one stratum at a time, and the engine measures and reports the consequences.

## 2. Theoretical background

The notion that the pulses of a meter form a strict hierarchy of strength is old; what Barlow contributed is a *quantitative* function over that hierarchy. In "Two Essays on Theory" (Barlow, 1987), developed for his composition *Çoğluotobüsişletmesi* and later automated in the *Autobusk* algorithmic system, Barlow defines the **indispensability** of a pulse within a metric stratification as a product of primes. A meter is decomposed into nested strata, each a prime *p*; for example, common time at the sixteenth-note level is the four-fold halving 2·2·2·2. Each stratum carries a *within-stratum* indispensability — a permutation of {0, …, *p*−1} fixing which subdivision of that prime is metrically strongest. Barlow's published within-prime tables are ξ₂ = {1,0}, ξ₃ = {2,0,1}, ξ₅ = {4,0,3,1,2}, ξ₇ = {6,0,4,2,5,1,3}.

For a compound meter with strata pf[0] (outermost/slowest) … pf[*z*−1] (the pulse), the indispensability of pulse *n* is the weighted sum of the within-stratum values of its mixed-radix digits, each weighted by the product of the *coarser* strata it subdivides:

> ind(*n*) = Σ_{j=0}^{z−1} ξ_{pf[j]}( d_j(*n*) ) · Π_{k<j} pf[k]

where d_j(*n*) is the *j*-th mixed-radix digit of *n* (most-significant digit = outermost stratum). The weighting ensures the downbeat, where every stratum simultaneously selects its strongest subdivision, dominates; successive halvings then interleave to produce the metric tree. Crucially, the result for 2·2·2·2 is *not* a simple bit reversal: the early sixteenths (steps 1 and 9) outrank several other odd steps, a subtlety that distinguishes Barlow's measure from naive binary depth.

Barlow's function is the canonical formalization, but it sits within a broader literature on rhythmic weight: Lerdahl and Jackendoff's *metrical grids* (1983) give a generative account of the same hierarchy without numeric ranks, and Toussaint's *off-beatness* and related combinatorial measures (Toussaint, 2005) quantify syncopation as the count of onsets on metrically weak pulses. We borrow Toussaint's framing for a single static statistic — the fraction of pulses below the median rank — and Barlow's full ranking for the compositional engine.

## 3. Method

### 3.1 Computing the indispensability vector

The recurrence is implemented in `build_indispensability()` at `/Users/jas/aesthetic-computer/pop/minitek/c/acidtek.c:290`. The meter is declared as the prime list `pf = {2,2,2,2}`; the within-stratum table is the function `xi_prime()` at `acidtek.c:278`, holding Barlow's ξ₂, ξ₃, ξ₅, ξ₇. For each pulse *n* ∈ [0, 16) the engine extracts the MSB-first mixed-radix digit and accumulates the weighted within-prime value:

```c
for (int j = 0; j < z; j++) {
    int outer = 1; for (int k = 0; k < j; k++) outer *= pf[k];   // Π_{k<j} pf[k]
    int digit = (n / (Np / (outer * pf[j]))) % pf[j];            // MSB-first digit
    v += (long)xi_prime(pf[j], digit) * outer;                   // acidtek.c:301
}
```

The raw integer scores are dense-ranked into 0…15 (15 = strongest, ties broken by earlier step) at `acidtek.c:310`, normalized to `IND16[]` ∈ [0,1] for the synthesis voices, and sorted strongest-to-weakest into `ORDER_BY_RANK[]` at `acidtek.c:315`. The engine then **verifies** the result against the published canonical ordering for 2·2·2·2 (`acidtek.c:396`) and prints the rank vector and the verdict at render time.

### 3.2 The metric-reveal experiment

The independent variable is encoded in `REVEAL[8]` at `acidtek.c:347`: REVEAL[*s*] is the number of top-ranked strata the TB-303 may accent in section *s*. The conversion to a rank threshold is `reveal_threshold()` at `acidtek.c:358`, returning 16 − REVEAL[*s*]; a pulse is in the section's accent set iff its Barlow rank meets that threshold. In the acid-line scheduler (`acidtek.c:467`):

```c
int revealed = (INDRANK[st] >= rthresh);   // top-REVEAL[s] strata only
int accent   = revealed;                   // the meter, not the pattern, decides
```

This is the load-bearing design decision: the pattern character `'A'` no longer forces an accent. Accenting is now *entirely* a function of the pulse's indispensability rank and the section's reveal depth. The synthesis voice `blip()` (`acidtek.c:114`) is a saw through a high-resonance state-variable filter; on an accent it boosts gain and resonance and holds the cutoff envelope open longer. Velocity (`acidtek.c:475`), resonance (`acidtek.c:476`), and the cutoff ceiling (`acidtek.c:478`) all scale continuously with `IND16[st]`, so even unaccented notes lean toward the meter — the discrete accent gate sits atop a continuous indispensability shading.

### 3.3 Measured quantities

Two telemetry blocks emit real numbers. The static apparatus report (`acidtek.c:391`–`acidtek.c:400`) prints the 16-element rank vector, the strongest→weakest ordering, the canonical-match verdict, and the Toussaint-style count of below-median pulses. The dynamic result report (`acidtek.c:510`) accumulates, per section, the number of 303 notes, the number that accented, the accent percentage, and the *mean indispensability of the accented pulses* — the statistic that reveals how the accent set migrates down the hierarchy.

### 3.4 Engine spine (unchanged)

The two-bus architecture is preserved: drums are dry (`addD`), music is ducked (`addM`). The kick (`acidtek.c:59`) stamps the sidechain trigger array `trig[]`; the deep kick-triggered duck on the music bus is applied at `acidtek.c:533` (depth 0.78, ~130 ms release). The acid line feeds a dotted-eighth ping-pong delay and a four-comb Schroeder reverb send (`acidtek.c:537`). The master normalizes to 0.89 peak with short fades before `write_wav_f32_stereo`.

## 4. Composition and results

The form is eight sections totaling 72 bars / 121.4 s, each a setting of REVEAL. The measured accent census printed at render is reproduced below (only the six sections carrying the 303):

| section | REVEAL *k* | 303 notes | accents | accent % | mean ind (accents) |
|---------|-----------:|----------:|--------:|---------:|-------------------:|
| grvA    | 4          | 192       | 64      | 33.3     | 0.900 |
| brk1    | 8          | 96        | 64      | 66.7     | 0.767 |
| grvB    | 10         | 144       | 96      | 66.7     | 0.767 |
| acidpk  | 16         | 48        | 48      | 100.0    | 0.544 |
| grvC    | 12         | 144       | 96      | 66.7     | 0.767 |
| outro   | 3          | 96        | 24      | 25.0     | 0.933 |
| **total** | —        | **720**   | **392** | **54.4** | — |

The numbers track the hypothesis precisely. In **grvA** (*k*=4) only the downbeat, half-bar, and two quarters may accent; one third of the dense 303 pattern accents, and those accents sit at mean indispensability **0.900** — the line leans hard on the metric pillars. As the reveal deepens through **brk1** and **grvB**, the accent fraction rises to 66.7% and the mean indispensability of the accent set *falls* to **0.767**, because the newly admitted pulses are weaker by construction. At **acidpk** (*k*=16) every pulse is revealed: 100% of notes accent, and the mean indispensability collapses to **0.544** — the acid line screams across the whole metric tree, maximally syncopated. **outro** (*k*=3) snaps the window shut to the three strongest strata, 25% accents at mean **0.933**, returning the meter to its skeleton as the track fades.

The static report confirms the apparatus is correct: the computed ordering is `0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15`, which the engine flags as **MATCHES canonical Barlow ordering**. The Toussaint-style statistic reports **8/16 = 0.500** of pulses below the median rank — exactly half the grid is syncopation-capable, the symmetric property of an all-2 stratification.

What the listener hears, concretely: the intro and build present an almost rigid pulse (only the strongest one or two positions can bite); grvA introduces the recognizable four-on-the-floor acid bounce locked to the quarters; the breakdown and grvB let the line wander onto the offbeat eighths, loosening the groove; the peak is the moment of maximum metric saturation where the 303 accents everywhere; and the outro audibly *de-resolves* the meter back to its root pulses.

## 5. Discussion

The experiment succeeds in making an abstract music-theoretic ranking *audible as form*. Because accenting is gated purely by the Barlow rank and the reveal depth, the relationship between the independent variable (REVEAL) and the dependent observables (accent %, mean accent indispensability) is mechanical and reproducible — re-rendering prints the same census. The continuous shading of velocity, resonance, and cutoff by `IND16[]` means the gate does not produce an on/off binary; even within a single reveal setting the strongest revealed pulses are the loudest and brightest, so the metric tree is legible at two scales at once.

**Limitations.** First, Barlow's function is defined over a *fixed* stratification; *acidtek* hard-codes 2·2·2·2, so the engine cannot currently express triplet or 7/8 meters even though `xi_prime()` already holds the ξ₃, ξ₅, ξ₇ tables. The recurrence is written generically over `pf[]`, so a single edit to the prime list would extend it — an obvious next experiment. Second, the reveal schedule is a hand-authored eight-step curve; a more rigorous design would parameterize REVEAL as a continuous function of time and report a regression of accent statistics against it. Third, swing (the micro-timing on odd sixteenths) interacts with indispensability but is not folded into the measure; Barlow's framework is purely about position, not timing, so a perceptual study would be needed to claim the two are independent.

**Future work.** The natural extension is to drive *harmony* as well as accent from indispensability — selecting chord tones on strong pulses and passing tones on weak ones — so that the same ranking governs both the rhythmic and pitch surfaces. A second direction is to compute a true per-section Toussaint syncopation score from the realized onset set (not just the meter's static property) and correlate it with the reveal depth, closing the loop between the meter's intrinsic structure and the music actually written onto it.

In sum, the track demonstrates that a single, computable, self-verifying number per pulse — Barlow's indispensability — is rich enough to organize an entire arrangement when treated as the experimental variable rather than a fixed lookup table.

## References

Barlow, C. (1987). Two Essays on Theory. *Computer Music Journal*, 11(1), 44–60.

Barlow, C., & Lohner, H. (1987). Çoğluotobüsişletmesi. *Computer Music Journal*, 11(1) (accompanying score and commentary).

Lerdahl, F., & Jackendoff, R. (1983). *A Generative Theory of Tonal Music*. Cambridge, MA: MIT Press.

Toussaint, G. T. (2005). The geometry of musical rhythm. In *Proceedings of the Japan Conference on Discrete and Computational Geometry (JCDCG 2004)*, Lecture Notes in Computer Science 3742, 198–212. Springer.

Schroeder, M. R. (1962). Natural sounding artificial reverberation. *Journal of the Audio Engineering Society*, 10(3), 219–223.
