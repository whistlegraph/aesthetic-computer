# The Seam That Is Never Heard: Three Coprime Cycles, Stacked Hemiola, and the Realignment Period of Polymeter in *brokentek*

**Abstract.** Polymeter — the simultaneous sounding of voices whose cycle lengths share no common factor with the bar — is usually demonstrated with a single drifting ostinato, heard as "two against three." This thesis treats one fast broken-techno track, *brokentek* (144 BPM, G natural minor, 116.3 s), as an apparatus that makes audible the *arithmetic* of polymeter rather than a single instance of it. The C engine runs three perc ostinato lanes on cycle lengths coprime to the 16-step bar (3, 5, 7) off a single never-reset global step counter, and computes at render time the full lattice of realignment periods: the three lane×bar least-common-multiples (48, 80, 112 steps), the three-lane mutual coincidence period LCM(3,5,7) = 105 steps, and the full realignment LCM(3,5,7,16) = 1680 steps = 105 bars = 175.0 s — *longer than the track*, so the polymeter's seam is, by construction, never heard. A crash marks the half-realignment coincidence at step 840 (bar 52). Two stacked hemiola voices (3:2 over two beats, 3:4 over the bar) let notated 4/4 and several perceived meters coexist at the climax. The engine also scores the broken kick patterns for metric-weight syncopation (mean 3.0 against a four-on-the-floor control of 0). The track is the proof; the numbers are printed at render time.

## 1. Introduction

A four-on-the-floor kick is a clock. It tells the ear, four times a bar, exactly where it is. Broken techno removes that clock: the kick syncopates, leaves gaps, pushes hits off the beat, and the listener loses the easy downbeat. The danger is that without a clock the music becomes merely arrhythmic — busy but directionless. The compositional question behind *brokentek* is how to keep a strong, danceable sense of forward motion while withholding the metric anchor.

The answer this track proposes is polymeter: layer several short cyclic patterns whose lengths are *coprime* to the bar, so that none of them lines up with the kick or with each other for a very long time. The composite of those layers never repeats exactly, which is what produces the continuous, jacking precession of a good broken groove — the ear hears that something is turning but cannot find the loop point. Crucially, that loop point — the *realignment period* — is not a vague feeling. It is a least-common-multiple, an integer, and it can be computed. This thesis makes the engine compute it, print it, and then structures the form around it.

The deepening over the prior pass is threefold. First, the three coprime lanes are no longer asserted; their realignment lattice is calculated and printed (`c/brokentek.c:386`–`406`). Second, the form is timed so a marked structural event — a crash — lands on the *half*-realignment coincidence, and the *full* realignment is placed deliberately beyond the end of the track. Third, two hemiola voices are stacked at the climax so that the difference between *notated* meter and *perceived* meter becomes the explicit subject of the final section. The track is not illustrated by the theory of polymeter. The track *is* the experiment.

## 2. Theoretical background

The distinction this work depends on is between *polyrhythm* and *polymeter*. A polyrhythm superimposes two subdivisions of the same span (three evenly-spaced notes against two within one beat — a hemiola). A polymeter superimposes two *meters* of different cycle length that share a common pulse but not a common downbeat (a five-pulse pattern looping against a four-pulse bar). London (2012, *Hearing in Time*, 2nd ed.) formalizes meter as a learned, entrained framework of expectation rather than a property of the signal, and notes that the ear maintains at most a small number of nested periodicities at once; polymeter exploits exactly this limit by presenting more periodicities than the ear can lock to, forcing it to choose a perceived meter that the notation does not privilege.

The arithmetic of when superimposed cycles realign is elementary number theory: two cycles of length *a* and *b* return to their joint starting phase after LCM(*a*, *b*) pulses, and LCM(*a*, *b*) = *ab* exactly when gcd(*a*, *b*) = 1, i.e. when *a* and *b* are coprime. This is why coprimality with the bar is the design lever: a lane of length 3 against a 16-step bar realigns only after 3×16 = 48 steps, whereas a lane of length 4 realigns every 16. The use of coprime cycle lengths to maximize the non-repeating span is the structural principle behind the "wheels within wheels" of West African bell patterns and their analysis in Toussaint (2013, *The Geometry of Musical Rhythm*), and behind the systematic process music of Reich (whose *Piano Phase*, 1967, lets two identical figures drift until they realign) and the polymetric études of Ligeti.

The hemiola — three against two — is the smallest and most consonant cross-rhythm, discussed at least since the Baroque (its name is Greek for "one and a half," the 3:2 ratio). Stacking a 3:2 (three over two beats) with a 3:4 (three over the whole four-beat bar) produces a *compound* hemiola in which the bar is simultaneously felt as two dotted pulses and as three slow pulses. Pressing (1997, "Cognitive Complexity and the Structure of Musical Patterns") and the metric hierarchy of Longuet-Higgins & Lee (1984, "The Rhythmic Interpretation of Monophonic Music") give the tool used here to *quantify* how far a pattern departs from its meter: weight each metric position by its depth in the binary subdivision tree, and score an onset as syncopated when it displaces a silent, metrically stronger position that follows it. The broken kick can then be assigned a number, and compared to the four-on-the-floor it abandons.

## 3. Method

The engine keeps the *brokentek* spine intact: a two-bus mix (dry DRUM bus, kick-ducked MUSIC bus), a kick-stamped sidechain trigger array (`kick`, `c/brokentek.c:75`, writing `trig[]`), a tempo-synced delay into a Schroeder reverb send (`c/brokentek.c:504` delay; the comb-filter reverb block at `c/brokentek.c:513`), and normalize-and-fade mastering through `write_wav_f32_stereo`. The deepening lives in four places.

**3.1 The global step counter.** All cross-rhythm depends on a single continuous index that never resets at the bar line: `long gstep = (long)bar * 16 + st;` (`c/brokentek.c:432`). Every coprime lane is keyed off `gstep` modulo its cycle length, so phase accumulates across the entire track rather than restarting each bar.

**3.2 The three coprime lanes.** Inside the `L_POLY` block (`c/brokentek.c:433`–`440`):

- a 3-step **rim** ostinato fires when `gstep % 3 == 0`, indexing a G-minor cell off `gstep/3` (`c/brokentek.c:435`);
- a 5-step **shaker** fires when `gstep % 5 == 0` (`c/brokentek.c:437`);
- a 7-step **ride** bell fires when `gstep % 7 == 0`, gated to the fuller sections (`c/brokentek.c:439`).

Because 3, 5, and 7 are each coprime to 16 (and to one another), the three lanes precess against the kick and against each other continuously.

**3.3 Realignment arithmetic, computed and printed.** `igcd` and `ilcm` (`c/brokentek.c:331`–`332`) implement the Euclidean algorithm and LCM(*a*,*b*) = *a*/gcd(*a*,*b*)·*b*. The EXPERIMENT 1 block (`c/brokentek.c:386`–`406`) computes the full lattice:

    Lp_i = LCM(C_i, 16)              for C_i ∈ {3,5,7}     → 48, 80, 112 steps
    L3   = LCM(3, 5, 7)             (lanes coincide)       → 105 steps
    LFULL= LCM(L3, 16) = LCM(3,5,7,16) (lanes + bar grid)  → 1680 steps = 105 bars
    landGstep = LFULL / 2                                  → 840 steps  (half-realignment)

The half-realignment step is converted to a bar/step landmark (`landBar`, `landStep`, `c/brokentek.c:393`) and a crash (`crash`, `c/brokentek.c:173`) is struck there: `if (gstep == landGstep) crash(t, 0.85);` (`c/brokentek.c:467`). Because the form (8+4+16+4+16+4+12+4 = 68 bars, `SECBARS`, `c/brokentek.c:302`) is shorter than the 105-bar full realignment, the seam is unreachable within the track — a fact the engine prints explicitly.

**3.4 Stacked hemiola.** The 3:2 layer (`L_HEMI`, `c/brokentek.c:441`–`452`) places three hits across each two-beat (8-step) window at the nearest 16ths to the ideal positions 0, 8/3, 16/3 — i.e. 0, 3, 5 — as toms and a clap, producing a perceived dotted-quarter pulse over the beat. The 3:4 layer (`L_HEMI4`, `c/brokentek.c:460`–`466`), new in this pass, places three plucks across the *whole* 16-step bar at the nearest 16ths to 0, 16/3, 32/3 — i.e. 0, 5, 11 — a slower triplet that fights the bar itself. The 3:4 is masked into grvC only (`c/brokentek.c:310`), so the cross-rhythm stack thickens at the climax.

**3.5 Syncopation scoring.** `metric_weight(step)` (`c/brokentek.c:341`) assigns the Longuet-Higgins/Lee tree weights (downbeat 4, beat 3, 8th-offbeat 1, odd-16th 0). `syncopation_score(p)` (`c/brokentek.c:348`) sums, over each onset, the positive weight difference to a metrically stronger *silent* next position — the cognitive cost of the displacement. EXPERIMENT 2 (`c/brokentek.c:408`–`412`) scores the two broken kick patterns against a four-on-the-floor control.

## 4. Composition and results

The renderer prints both experiments before writing the WAV. The numbers below are verbatim from the render of the committed engine (144 BPM, 68 bars, 116.3 s).

**Experiment 1 — polymeter realignment.**

| quantity | value | musical reading |
|---|---|---|
| coprime lane lengths vs 16-grid | 3, 5, 7 (gcd with grid = 1 each) | none divides the bar |
| lane 3 × bar LCM | 48 steps = 3 bars | rim realigns to downbeat every 3 bars |
| lane 5 × bar LCM | 80 steps = 5 bars | shaker every 5 bars |
| lane 7 × bar LCM | 112 steps = 7 bars | ride every 7 bars |
| three-lane mutual coincidence LCM(3,5,7) | 105 steps = 6.5625 bars | all three perc lanes fire together |
| **full realignment** LCM(3,5,7,16) | **1680 steps = 105 bars = 175.0 s** | **the seam — beyond the track** |
| half-realignment landmark | step 840 = bar 52, step 8 | crash; grvC downbeat is bar 52 |

The decisive result is the last two rows. The full realignment at 175.0 s exceeds the track's 116.3 s, so within the listening frame the groove genuinely never repeats its global phase — the polymeter is effectively infinite. The half-realignment at step 840 falls inside grvC (which opens at bar 52), and the crash there is the one moment where the three precessing lanes are guaranteed to coincide; it functions as the structural keystone the rest of the form drifts toward and away from.

**Experiment 2 — metric-weight syncopation.**

| pattern | string | score |
|---|---|---|
| four-on-the-floor control | `x...x...x...x...` | 0 |
| P_KICK | `x.....x..x.x....` | 4 |
| P_KICK2 | `x...x....x...x.x.` | 2 |
| mean broken | — | 3.0 |

The control scores exactly 0 — every kick lands on a beat, displacing nothing — confirming the metric of the measure. The broken patterns score 4 and 2 (mean 3.0): the pushes onto the "and" of 2 and into 3 each displace a stronger silent position, which is precisely the cognitive cost that the broken feel trades on.

**Form as experiment.** The arrangement adds one cross-rhythm stratum at a time (`MASK`, `c/brokentek.c:304`–`312`). intro/build expose the coprime perc lanes over the broken kick alone, so the precession is heard naked. grvA introduces the 3:2 hemiola; brk1 strips the kick so the polymeter floats over pads. grvB restates the full groove. grvC, opening on bar 52, adds the 3:4 hemiola atop the 3:2 and contains the half-realignment crash — the densest cross-rhythm point of the track. outro removes the hemiola and lets the bare coprime lanes drift out, returning the ear to the un-anchored state it started in.

## 5. Discussion

What works is the central claim made audible: the groove never settles into an obvious loop, yet it never feels random, because the precession is governed by fixed integer cycles rather than by noise. A listener should be able to hear, in the intro, the rim (fast), shaker (medium), and ride (slow) lanes sliding past one another; in grvC, the toms and plucks pulling the bar into a triple feel against the kick; and at the bar-52 crash, a momentary "click" of coincidence before the lanes diverge again. The metric-weight numbers also let the broken kick be tuned objectively: a score near 3 is broken enough to lose the floor without dissolving the pulse entirely.

The honest limitations are three. First, the perceptual claim that the seam is "never heard" is supported by arithmetic (175 s > 116 s) and by entrainment theory (London 2012) but not by a listening study; the realignment is unreachable, but whether a trained listener nonetheless extracts a shorter perceived period is untested. Second, the hemiola layers are *quantized* to the nearest 16th (3:2 at 0,3,5; 3:4 at 0,5,11), so they are approximations of the true 8/3 and 16/3 spacings — audible as hemiola but not metronomically exact triplets; a true-triplet grid would render them cleaner at the cost of the swing that gives the track its jack. Third, the metric-weight score uses only the immediate next position, a simplification of the full Longuet-Higgins/Lee recursion.

Future work: sweep the coprime triple itself (e.g. 3,5,7 → 5,7,11) across sections and print how the full LCM grows, making the realignment period a directly heard parameter; add a true-triplet hemiola grid as an A/B against the quantized one; and run a tap-along study to measure the *perceived* period against the computed 175 s. The arithmetic is exact; the next question is what the ear does with it.

## References

- Clough, J. & Douthett, J. (1991). "Maximally Even Sets." *Journal of Music Theory*, 35(1/2), 93–173.
- Ligeti, G. (1985–2001). *Études pour piano* (Books I–III). Schott.
- London, J. (2012). *Hearing in Time: Psychological Aspects of Musical Meter* (2nd ed.). Oxford University Press.
- Longuet-Higgins, H. C. & Lee, C. S. (1984). "The Rhythmic Interpretation of Monophonic Music." *Music Perception*, 1(4), 424–441.
- Pressing, J. (1997). "Cognitive Complexity and the Structure of Musical Patterns." In *Proceedings of the 4th Conference of the Australasian Cognitive Science Society*.
- Reich, S. (1967). *Piano Phase*. Universal Edition.
- Toussaint, G. T. (2013). *The Geometry of Musical Rhythm: What Makes a "Good" Rhythm Good?* Chapman & Hall/CRC.
