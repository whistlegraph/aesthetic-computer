# Precessing Necklaces: Maximal Evenness and the Drift of Composite Euclidean Rhythm in *hypnotek*

**Abstract.** Euclidean rhythms — the maximally-even distributions of *k* onsets across *n* metric positions produced by Bjorklund's algorithm — are usually presented as a static catalogue: E(5,16) is the bossa clave, E(3,8) is the tresillo, and so on. This thesis treats one fast minimal-techno track, *hypnotek* (142 BPM, A minor, 124.7 s), as an experimental apparatus that makes audible two claims about that catalogue. First, that Bjorklund maximizes evenness in a measurable sense: the C engine computes, for each of eight rhythmic lanes, the inter-onset-interval variance, a normalized evenness score E ∈ [0,1], and Toussaint's geometric vertex-distance D, and compares each against a clustered control of identical density. The eight Euclidean lanes average E = 0.888 and sit 3.0× closer to the ideal regular *k*-gon than the controls. Second, that rotating each lane's start offset turns a fixed necklace into a *precessing* one, and that the precession rate of the composite — not its pattern — is the dominant perceptual variable. The form is structured as a sweep of that rate (still → slow → fast → blur → still) with measured per-phrase drift speeds of 0, 10, 30, and 50 step-rotations per four-bar phrase, all sharing a 64-bar realignment period. The track is the proof; the numbers are printed at render time.

## 1. Introduction

Minimal techno survives on a paradox. It is built from a handful of looping one-bar patterns, almost nothing changes, and yet a good track does not feel static — it feels like it is slowly turning, like a mobile drifting in a draft. The question I wanted to answer with *hypnotek* is mechanical: where does that turning come from, if the loops never change?

The answer this track proposes is that the loops *do* change, just not in pitch or pattern — in phase. Each rhythmic layer is a maximally-even Euclidean rhythm, a "necklace" of beads spaced as evenly as the grid allows. If you rotate one necklace by a step every few bars while leaving the others fixed, the *composite* — the sound of all the layers struck together — never repeats the same way twice until every necklace has come back into alignment. That slow re-alignment is the turning. It is hypnotic precisely because the ear can hear that something is moving but cannot find the seam.

This is not a metaphor I imposed in the mix. It is computed. The engine at `c/hypnotek.c` builds every lane with Bjorklund's algorithm, rotates each lane by a per-section rate, and — new in this pass — measures the evenness of each necklace and the drift speed of the composite, printing both to stderr so the claims in this document are grounded in numbers the renderer actually produced. The track is not illustrated by the theory. The track *is* the experiment.

## 2. Theoretical background

The foundational result is Toussaint (2005), "The Euclidean Algorithm Generates Traditional Musical Rhythms," which observed that Bjorklund's algorithm for spacing timing pulses in neutron-detector electronics (Bjorklund 2003) produces exactly the same onset strings as a large family of world rhythms when asked to distribute *k* onsets over *n* steps. The output E(k,n) is *maximally even*: of all the ways to place *k* onsets on *n* positions, it is the one whose onsets sit as close as possible to the vertices of a regular *k*-gon inscribed in the *n*-step cycle.

"As even as possible" has a precise meaning. A maximally-even rhythm uses at most two distinct inter-onset intervals, and they differ by exactly one step — the floor and ceiling of *n/k* (Clough & Douthett 1991, who formalized maximal evenness for pitch-class set theory; the rhythmic case is the dual). E(5,16), for instance, has gaps of 3,3,3,3,4 — four short, one long, never anything more lopsided. This two-gap property is what makes Euclidean rhythms sound *grounded* rather than mechanical: they are the most balanced syncopation a grid can hold.

Toussaint's second contribution is geometric. Represent the cycle as *n* points on a circle and the onsets as a subset of *k* of them. The "evenness" of the subset is how close it lies to a perfectly inscribed *k*-gon. Maximal evenness minimizes the total distance from each onset to its nearest ideal vertex. This gives a real-valued, comparable measure — not just "is it Euclidean" but "*how* even is it" — and it is one of the three metrics I compute.

The rotation idea comes from the necklace formalization. Two rhythms that differ only by a cyclic shift are the same *necklace* (Toussaint 2013, *The Geometry of Musical Rhythm*, ch. on rhythmic necklaces and bracelets). Rotating a Euclidean pattern preserves its evenness perfectly — every rotation of E(k,n) is still maximally even — which is exactly what makes rotation a safe compositional lever: I can precess a lane indefinitely and it never becomes "less even," it only changes its phase relationship to its neighbours. The perceptual literature on this drift is thinner, but the relevant anchors are London (2012, *Hearing in Time*) on metric entrainment and the limits of how fast a listener will re-parse a meter, and the broad polyrhythm-as-phasing tradition (Reich's *Drumming*, Ligeti's late piano études) where two fixed patterns at slightly different rates generate an emergent moving pattern.

## 3. Method

The engine keeps the *hypnotek* spine intact: a two-bus mix (dry DRUM bus, kick-ducked MUSIC bus), a kick-stamped sidechain trigger array, a Schroeder reverb send, normalize-and-fade mastering, and `write_wav_f32_stereo`. The deepening lives in three places.

**3.1 Bjorklund.** `bjorklund(int k, int n, int rot, int *out)` at `c/hypnotek.c:44` builds the maximally-even string by the iterative bucket form — grow *k* "[1]" groups and *(n−k)* "[0]" groups, repeatedly fold the smaller pile onto the larger until one pile has ≤1 group, then read off the concatenation — and finally applies a cyclic rotation by `rot` (`c/hypnotek.c:78`). The rotation is the precession lever: same necklace, different phase.

**3.2 Evenness measurement.** `measure_evenness(const int *p, int n)` at `c/hypnotek.c:108` returns three numbers for any 0/1 pattern. Let the onsets be at indices o₀ < o₁ < … < o_{k−1} and let the cyclic inter-onset intervals be

    gᵢ = o_{i+1} − oᵢ   (and g_{k−1} = o₀ + n − o_{k−1})

with ideal gap ī = n/k. Then:

- **IOI variance** = (1/k) Σ (gᵢ − ḡ)², the spread of the gaps. Maximal evenness forces only floor/ceil(n/k) gaps, so this is small.
- **Normalized evenness** E = 1 − meanAbsDev / worstDev, where meanAbsDev = (1/k) Σ |gᵢ − ī| and worstDev is that same quantity for the maximally-clustered pattern (k−1 gaps of 1, one gap of n−k+1). E = 1 is perfectly even, E = 0 is maximally clustered.
- **Vertex distance** D — Toussaint's geometric measure (`c/hypnotek.c:146`): for each onset, the distance (in step units, wrapped) to the nearest vertex of the ideal *k*-gon, summed over onsets and minimized over a sub-step phase sweep of the *k*-gon. Lower D = closer to the perfect polygon.

A control, `clustered(int k, int n, int *out)` at `c/hypnotek.c:152`, packs all *k* onsets into the first *k* steps — the same density, maximally *un*-even — so each lane can be scored against its own worst case.

**3.3 Precession as the experimental variable.** The form is six sections (`ORDER`, `c/hypnotek.c:400`) each carrying a precession rate `PREC[s]` (`c/hypnotek.c:404`): `{0, 1, 3, 0, 5, 0}`. Per bar (`c/hypnotek.c` arrangement loop) each lane is rebuilt with rotation `phr4 * PREC[s] * w`, where `phr4` is the four-bar phrase index and *w* is a small per-lane weight (CHAT 1, OHAT 1, BLIP 2, RIM −1, SHAK 3, CBASS 2). The kick lane E(4,16) never precesses — it is the fixed floor against which everything else drifts (`c/hypnotek.c`). So `PREC` is exactly the independent variable: when it is 0 the necklaces are frozen and the groove is a locked loop; when it is large the composite phase advances quickly and the interlock blurs.

Two quantities characterize each section. The **per-phrase drift speed** Σrot = Σ_lanes (PREC·w mod 16) is how many total step-rotations the composite advances each phrase. The **realignment period** is the LCM over lanes of 16/gcd(16, PREC·w), the number of phrases until every lane has returned to its starting offset simultaneously — the length of the longest non-repeating stretch. Both are computed in the EXPERIMENT 2 block (`c/hypnotek.c`).

The blip and counter-bass are themselves *melodic* Euclidean lines: both ride E(5,16) lanes and index an A-minor pentatonic cell off the onset position (`c/hypnotek.c`), so the pitch contour is Euclidean too, not just the rhythm.

## 4. Composition and results

The renderer prints both experiments before writing the WAV. The numbers below are verbatim from the render of the committed engine.

**Experiment 1 — necklace evenness.** Eight lanes, each scored against its clustered control:

| lane | pattern | IOIvar | E | D | clustered E | clustered D |
|---|---|---|---|---|---|---|
| E_KICK E(4,16) | `x...x...x...x...` | 0.000 | 1.000 | 0.00 | 0.000 | 4.00 |
| E_CHAT E(7,16) | `x..x.x.x..x.x.x.` | 0.204 | 0.815 | 1.71 | 0.000 | 3.86 |
| E_OHAT E(3,8) | `x..x..x.` | 0.222 | 0.800 | 0.67 | 0.000 | 1.67 |
| E_BLIP E(5,16) | `x..x..x..x..x...` | 0.160 | 0.909 | 1.20 | 0.000 | 3.20 |
| E_RIM E(3,16) | `x....x....x.....` | 0.222 | 0.923 | 0.67 | 0.000 | 2.00 |
| E_SHAK E(9,16) | `x.xx.x.x.xx.x.x.` | 0.173 | 0.750 | 2.22 | 0.000 | 3.89 |
| E_RIDE E(2,16) | `x.......x.......` | 0.000 | 1.000 | 0.00 | 0.000 | 1.00 |
| E_CBASS E(5,16) | `x..x..x..x..x...` | 0.160 | 0.909 | 1.20 | 0.000 | 3.20 |

Mean evenness **E = 0.888** for the Bjorklund lanes versus **0.000** for the controls. Total vertex distance **D = 7.67** versus **22.81** — the Euclidean lanes sit **3.0× closer** to their ideal *k*-gons. The two lanes that divide the cycle exactly (E(4,16), E(2,16)) score a perfect E = 1.000, D = 0.00, as they must: 16/4 and 16/2 are integers, so the regular polygon lands on grid points. The lowest scorer, E(9,16) at E = 0.750, is the *densest* lane — at nine onsets in sixteen steps the grid simply cannot hold a regular nonagon, and the necklace is forced into a 1-2-1-2 gap weave. That is audible: the shaker is the busiest, least "square" layer, which is exactly why it is doing the air-filling grain work rather than holding the floor.

**Experiment 2 — drift speed and realignment.** With `PREC = {0,1,3,0,5,0}`:

| section | prec | Σrot/phrase | realign (phrases) | realign (bars) | realign (s) |
|---|---|---|---|---|---|
| still | 0 | 0 | frozen (∞) | — | — |
| slow | 1 | 10 | 16 | 64 | 108.2 |
| fast | 3 | 30 | 16 | 64 | 108.2 |
| breath | 0 | 0 | frozen (∞) | — | — |
| blur | 5 | 50 | 16 | 64 | 108.2 |
| lock | 0 | 0 | frozen (∞) | — | — |

This is the headline result, and it is more interesting than I expected. The realignment *period* is invariant — 64 bars, 108.2 s, longer than the whole track — for every moving section, because the rotation weights {1,1,2,1,3,2} all share gcd structure with 16 that lands the LCM on 16 phrases regardless of rate. What changes is the **drift speed**: the composite advances 10, then 30, then 50 step-rotations per phrase. The ear does not hear the period (it never completes inside the track); it hears the *rate of phase change*. So the perceptual arc is governed entirely by Σrot, and the three moving sections give a clean 1:3:5 ladder of how fast the necklaces slide past each other.

The form puts this on display deliberately. **still** (bars 1–8, prec 0) establishes a frozen interlock — kick, sub, rim, drone, a locked loop with zero drift, so the listener has a stationary reference. **slow** (9–24, prec 1) introduces the acid blip and the hats and lets them precess gently; the groove starts to turn. **fast** (25–40, prec 3) is the full kit at triple drift speed — the interlock is now visibly sliding, claps and rides and counter-bass weaving past the fixed kick. **breath** (41–48, prec 0) kills the kick and freezes the survivors over a pad, a held still frame mid-flight. **blur** (49–64, prec 5) is the climax: maximum drift, the composite changing fastest, the point where repetition fully dissolves into haze. **lock** (65–72, prec 0) freezes everything one last time for the outro. Still → slow → fast → still → blur → still: the experiment is the song.

Render verification: 124.7 s, A minor, 142 BPM, peak-normalized to 0.89 with a 0.4 s fade-in and 1.8 s fade-out. The kick stays on a dry bus and stamps the sidechain; the music bus ducks 0.72 deep per kick; only the music feeds the reverb and the eighth-note ping-pong delay, so the floor stays clean while the acid smears.

## 5. Discussion

What works: the precession reads. With the kick held fixed as a metric anchor, a listener locks to the four-on-the-floor and then perceives everything else as drifting *relative to it* — which is the whole trick, and it only works because E(4,16) is left un-rotated. The 1:3:5 drift ladder is audible as escalating instability without any change in instrumentation or pattern, which was the thing I most wanted to prove: that you can compose a build out of phase alone.

What I learned the hard way: the realignment period does not vary with the rate, so I cannot honestly claim the *fast* section "repeats sooner." It does not — it drifts faster within the same never-completing cycle. The earlier draft of this engine implied otherwise; the EXPERIMENT 2 print exists partly to keep me honest about it. The right framing is rate, not period, and the numbers force that framing.

Limitations. The evenness metrics are computed on the *unrotated* canonical lanes; rotation preserves evenness exactly, so this is correct, but it means the printed E and D do not vary across the track — they characterize the necklaces, not the moment-to-moment composite. A genuine "composite evenness over time" measure — the evenness of the *union* of all currently-sounding onsets as it precesses — would be a better single number for the perceptual claim, and it is the obvious next experiment. The vertex-distance phase sweep is also coarse (integer step offsets with a sub-step nudge); a continuous optimizer would sharpen D slightly but would not move the comparative result. And the realignment LCM is computed over rotation weights only; the *melodic* index advance (`gphr/3`, `gphr/4`) adds a second, slower modulation the LCM ignores, so the true non-repetition horizon is longer than 64 bars — a happy under-claim.

Future work: vary the precession rate *continuously* rather than in section-quantized steps, so the build is a smooth accelerando of drift; compute composite-onset evenness per phrase and use it to drive the filter cutoff, closing the loop between the measured quantity and the timbre. The instrument is already printing the number — the next pass should listen to it.

The music is real. It is two minutes long, it loops eight necklaces that never change pattern, and it turns the whole time because their phases slide at a rate I can read off the console. That is the thesis.

## References

- Bjorklund, E. (2003). *The Theory of Rep-Rate Pattern Generation in the SNS Timing System*. SNS-NOTE-CNTRL-99, Los Alamos National Laboratory.
- Clough, J., & Douthett, J. (1991). Maximally Even Sets. *Journal of Music Theory*, 35(1/2), 93–173.
- London, J. (2012). *Hearing in Time: Psychological Aspects of Musical Meter* (2nd ed.). Oxford University Press.
- Toussaint, G. T. (2005). The Euclidean Algorithm Generates Traditional Musical Rhythms. In *Proceedings of BRIDGES: Mathematical Connections in Art, Music and Science*, 47–56.
- Toussaint, G. T. (2013). *The Geometry of Musical Rhythm: What Makes a "Good" Rhythm Good?* CRC Press / Chapman & Hall.
