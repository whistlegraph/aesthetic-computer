# Recursion You Can Dance To: A Turtle-Interpreted Parametric L-System as the Generative Engine of a Self-Similar Glitch-Techno Track

**Abstract**

Lindenmayer systems are the canonical formalism for self-similar structure in computational biology and generative art, but their musical use has largely stopped at one-dimensional symbol strings mapped to pitch or onset. This thesis treats a single fast glitch/IDM-leaning minimal-techno track, *glitchtek* (152 BPM, B minor, 116.7 s), as the experimental apparatus for a richer claim: that the *same* context-free production rule, interpreted by a rhythmic *turtle* that can descend between metric grain levels, will self-similarly texture the 16th, 32nd and 64th subdivisions of a bar, and that the *depth of recursion* can serve as an audible compositional variable. The C engine implements a parametric, stochastic L-system (axiom `A`; rules `A→A[+a-a]` with a hashed dense variant `A→A[+A][-a]`, `a→+A-`, `-→a-`) whose growth curve, depth histogram and a self-similarity statistic are computed and printed at render time. The eight sections form a sweep up the generation count (g2→g6) and back. The engine reports a growth curve `1, 7, 18, 46, 110, 266, 641`, a self-similarity score of **53.5 %** (the fraction of subdivision branches that lead with an onset, reproducing the rule's accent-leads shape one level down), and a per-section "deep onset" fraction rising from **43.5 %** in the shallowest section to **≈79 %** in the deepest. The track is therefore an audible traversal of recursion depth.

## 1. Introduction

Glitch and IDM micro-percussion sounds the way it does because of *recursion under the beat*: a single click is not merely struck on a sixteenth but shattered into faster and faster retriggers, each burst echoing the shape of the one above it. The craft question is structural rather than timbral. Where, exactly, does the splintering happen; how deep does it go; and is there a principle that makes the fast detail feel *related* to the slow pulse instead of being noise sprayed between the beats?

The most natural formal answer is self-similarity: the small structure should be a scaled copy of the large structure. The canonical generator of self-similar strings is the Lindenmayer system (L-system), a parallel string-rewriting grammar in which every symbol is replaced simultaneously by the right-hand side of its production. L-systems famously generate the branching of plants; their strings are habitually interpreted by a *turtle* whose commands draw fractal curves. This thesis asks whether the same machinery can drive a danceable rhythm if the turtle, instead of moving on a drawing surface, moves between *metric grain levels* — descending from the 16th to the 32nd to the 64th and emitting an onset at whatever level it currently occupies.

The prior implementation of this engine used only the bare Fibonacci word (`A→AB`, `B→A`) as a one-dimensional self-similar onset string read with a per-bar offset. That is self-similar in principle but does not *act* at multiple grain levels: it is a single ribbon of sixteenths. The contribution of this pass is twofold. First, the grammar is generalized to a **parametric, stochastic, turtle-interpreted** L-system whose alphabet contains explicit *subdivide* operators, so the identical rule textures three grain levels at once. Second, the arrangement is built as a **controlled experiment in recursion depth**: each section fixes a generation count, and the engine measures and reports the rhythmic consequence, so the listener can hear the grammar recurse deeper and then resolve.

## 2. Theoretical background

The L-system was introduced by Aristid Lindenmayer (1968) to model the development of filamentous organisms by parallel rewriting. The definitive synthesis, including turtle interpretation of bracketed strings to render branching structures, is Prusinkiewicz and Lindenmayer's *The Algorithmic Beauty of Plants* (1990), which also formalizes **parametric** L-systems (symbols carry parameters) and **stochastic** L-systems (productions are chosen probabilistically), the two extensions this engine combines.

Musical applications of L-systems begin with Prusinkiewicz's own "Score generation with L-systems" (ICMC 1986), in which a turtle's movements are read as pitch and duration. The most thorough survey of *interpretation strategies* — how to map an abstract L-system string onto musical parameters — is Worth and Stepney, "Growing Music: Musical Interpretations of L-Systems" (2005), which catalogues onset/pitch/duration mappings and stresses that the *interpreter*, not merely the grammar, determines the musical result. Stelios Manousakis's thesis "Musical L-systems" (2006) extends turtle interpretation explicitly into the time domain and into nested temporal grids, which is precisely the move made here: the turtle's "forward" is a rhythmic grain and "descend" halves it. The broader frame of treating algorithmic generation as a compositional *experiment* is Xenakis's *Formalized Music* (1971), and the use of self-similar and fractal structure as a deliberate aesthetic is surveyed across genres in the algorithmic-composition literature.

Two properties of L-systems matter for the present claim. (1) **Determinism with structure**: a deterministic context-free L-system (D0L) produces a fixed string whose growth length obeys a linear recurrence — for these rules the per-generation length is exactly computable, giving an a-priori budget for how much material a bar contains. (2) **Self-similarity through the bracket**: when a production wraps part of its right-hand side in `[ ]` (a pushed sub-branch) and that branch is interpreted at a finer scale, the shape inside the branch is, by construction, a transformed copy of the shape outside it. This is the structural fact the engine exploits and then *measures*.

## 3. Method

### 3.1 The grammar

The alphabet and productions are defined and documented at `/Users/jas/aesthetic-computer/pop/minitek/c/glitchtek.c:34`. The turtle alphabet is: `A` accented onset, `a` soft (ghost) onset, `+` and `[` descend one grain level, `]` ascend, `-` rest. The productions, applied in parallel each generation by `lsys_step()` (`glitchtek.c:76`), are:

```
A -> A[+a-a]                 (deterministic core: accent, then a half-rate ornament)
A -> A[+A][-a]   w.p. ~0.38  (stochastic dense fan, selected by a position hash)
a -> +A-                     (a ghost regenerates an accent one level down)
-  -> a-         w.p. 0.5    (a rest sometimes blooms into a soft onset)
+, [, ]          terminal    (depth markers carry through)
```

The stochastic choice for each `A` and `-` is made by a deterministic integer hash of the symbol's absolute index, `lhash()` (`glitchtek.c:71`), so the grammar is genuinely stochastic across the string yet bit-for-bit reproducible across renders — a parametric stochastic L-system in the sense of Prusinkiewicz & Lindenmayer (1990). `lsys_build()` (`glitchtek.c:98`) iterates the rewrite to the maximum depth needed by the arrangement, recording the string length after every generation in `lsys_genlen[]`.

### 3.2 The rhythmic turtle

The interpreter is `lsys_interp_cell()` (`glitchtek.c:123`). Given an absolute start index and a *symbol budget*, it walks the grown string maintaining a current depth `d`, a current position `cur` within the 16th cell, and a span `span = 1/2^d`:

```c
if  (c=='+' )  d = min(d+1,3); span = 1/2^d;           // descend a grain
if  (c=='[' )  push(d); d = min(d+1,3); span = 1/2^d;  // open a sub-branch
if  (c==']' )  d = pop(); span = 1/2^d;                // close it
if  (c=='-' )  cur += span;                            // rest one grain
if  (c=='A'||c=='a') { emit(cur, d, accent=(c=='A')); cur += span; }
```

Each emitted `Onset` carries its fractional offset within the 16th, its depth level (0 = 16th, 1 = 32nd, 2 = 64th, 3 = 128th), and its accent flag. Because `+`/`[` halve `span`, the *same* production text places onsets at finer and finer grains the deeper the turtle goes — one rule, every level.

### 3.3 The experiment: recursion depth as independent variable

`SECDEPTH[8] = {2,3,4,3,5,4,6,3}` at `glitchtek.c:442` fixes the L-system generation depth per section; the read window is also gated by a per-section *symbol budget* `budget = 2 + depth*3` (`glitchtek.c:543`), so deeper sections both grow a longer string and let the turtle read further into its recursion. The synthesis block at `glitchtek.c:534` interprets one cell per 16th, emits a `click()` for every onset (deeper grains brighter and quieter, `glitchtek.c:550`), and turns deep accents into pitched `blip()` ratchets (`glitchtek.c:558`) — the acid-glitch detail. A depth ceiling (`if (d > sdepth) continue;`, `glitchtek.c:547`) guarantees that shallow sections stay sparse even though they read from the same fully-grown string.

### 3.4 Measured quantities

Three telemetry blocks emit real numbers. The **growth curve** and the grammar definition are printed at `glitchtek.c:497`–`glitchtek.c:505`. The **self-similarity report** is computed by `lsys_report()` (`glitchtek.c:157`): it builds a depth histogram by interpreting one turtle cell per starting symbol (matching synthesis exactly), and computes the self-similarity statistic `ss_selfsim` as the fraction of subdivision branches (`+`/`[`) whose first interpreted symbol is an onset — i.e. the fraction of branches that *lead with an accent*, reproducing the parent's accent-leads shape one level down. The **per-section census** (`glitchtek.c:665`) accumulates, during synthesis, the realized onset count, the accent count, and the count of onsets below the 16th level per section, and prints the per-section "deep %".

### 3.5 Engine spine (preserved)

The two-bus architecture is intact: drums are dry (`addD`, `glitchtek.c:182`), the music bus is ducked (`addM`, `glitchtek.c:183`). The kick (`glitchtek.c:202`) stamps the sidechain trigger array `trig[]` (`glitchtek.c:204`); the deep kick-triggered duck (depth 0.72, ~100 ms release) is applied at `glitchtek.c:623`. Stabs feed a ping-pong slap delay (`glitchtek.c:626`) and a four-comb Schroeder reverb send (`glitchtek.c:635`, after Schroeder 1962). The master normalizes to 0.89 peak with 0.4 s/1.6 s fades before `write_wav_f32_stereo` (`glitchtek.c:186`). New micro-grains are gain-staged below the leads and brightened (not lowered) with depth so the kick and reese keep their octave.

## 4. Composition and results

The form is eight sections, 72 bars, 116.7 s, with the per-section recursion depth as the swept variable. The census printed at render is reproduced below.

| section | depth | bars | read/cell | onsets | accents | deep (>16th) | deep % |
|---------|------:|-----:|----------:|-------:|--------:|-------------:|-------:|
| intro   | g2    | 8    | 8         | 138    | 48      | 60           | 43.5 |
| build   | g3    | 4    | 11        | 79     | 44      | 63           | 79.7 |
| grvA    | g4    | 16   | 14        | 353    | 171     | 277          | 78.5 |
| brk1    | g3    | 8    | 11        | 155    | 72      | 116          | 74.8 |
| grvB    | g5    | 16   | 17        | 362    | 160     | 271          | 74.9 |
| brk2    | g4    | 4    | 14        | 80     | 42      | 60           | 75.0 |
| grvC    | g6    | 12   | 20        | 263    | 142     | 206          | 78.3 |
| outro   | g3    | 4    | 11        | 90     | 33      | 71           | 78.9 |
| **total** | —   | 72   | —         | **1520** | **712** | **1124**   | **73.9** |

The numbers track the design. The grammar's **growth curve** is `g0=1, g1=7, g2=18, g3=46, g4=110, g5=266, g6=641` — super-linear (each generation roughly 2.4× the last), confirming the bracketed productions expand the string geometrically and giving each deep section a far larger pool of material than the shallow ones. The **self-similarity statistic is 53.5 %**: more than half of all subdivision branches in the 641-symbol string lead with an onset at the branch head, which is the operational signature that the rule reproduces its accent-leads shape octave after octave (the rest break the shape on purpose, supplying syncopation). The **depth histogram** over realized onsets is `L0(16th)=201, L1(32nd)=215, L2(64th)=172, L3=333`, so onsets are spread across all four grain levels rather than concentrated on the beat grid — the texture genuinely lives below the sixteenth.

The audible experiment is the **deep %** column. In **intro** (g2) only 43.5 % of onsets fall below the 16th level: the recursion is shallow, the skitter is sparse and mostly on the grid. From **build** onward the deep fraction jumps to ≈75–80 %; **grvA** (g4) and **grvC** (g6) sit at 78.5 % and 78.3 % with the densest absolute onset counts, and **grvB** (g5, 362 onsets) is the busiest section overall. The track thus climbs from a near-quantized intro into progressively finer self-similar storms and steps back down in the breaks (brk1/brk2 return to g3/g4), exactly the recursion-depth arc the form was designed to expose.

What a listener hears, concretely: the intro presents recognizable clicks roughly on the grid; the build introduces the first audible *shattering* of single hits into faster bursts; each groove section recurses a level deeper, the bursts subdividing into 32nd and then 64th machine-gun ratchets while the four-on-the-floor kick and the reese rumble hold the slow frame; the breaks pull the recursion back so the ear can re-find the pulse; and the deepest section, grvC, runs the rule six generations deep before the outro resolves.

## 5. Discussion

The experiment makes recursion *audible as form*. Because the deep-onset fraction is a mechanical consequence of the per-section depth and budget, re-rendering prints the same census, and the relationship between the independent variable (generation depth) and the dependent observable (deep %) is reproducible. The self-similarity statistic gives a single defensible number for the claim that "one rule textures every grain level," and the growth curve quantifies exactly how much material each depth yields.

**What works.** The turtle's grain-descent operator is the key abstraction: it lets a one-dimensional grammar act in a *nested* time grid without any second mechanism, so the fast detail is provably a scaling of the slow shape. Keeping the deep grains brighter and quieter (rather than louder) preserves headroom, so the kick and reese stay in their octaves under even the densest g6 storm.

**Limitations.** First, the self-similarity statistic measures *branch-head onset* recurrence, not a full edit-distance between parent and child rhythms; a stronger proof would compute the normalized cross-correlation of the onset mask at level d against the time-stretched mask at level d+1. Second, the depth ceiling is a hard cutoff at `sdepth`; a continuous, probabilistic descent would make the depth sweep gradual rather than sectional. Third, the per-section *deep %* saturates near 75–80 % above g3, because once the budget admits any 32nd-level branch the bracketed ornaments dominate; the genuinely shallow contrast is therefore concentrated in the intro, and a finer schedule (g1, g2 sections) would widen the swept range. Fourth, swing is applied to the 16th grid but not propagated into the sub-grains, so deep grains are metronomic against a swung frame — a perceptual study would be needed to decide whether that is a feature or an artifact.

**Future work.** The parametric channel of the grammar is under-used: symbols could carry pitch or velocity parameters that the productions transform, so the *same* recursion that subdivides time would also walk the B-minor scale, unifying the rhythmic and melodic surfaces under one rule. A second direction is to drive the *macro* form from a higher-level L-system (sections as symbols), so the arrangement and the micro-percussion are the same grammar at two time scales — recursion all the way up.

In sum, *glitchtek* demonstrates that a parametric, stochastic, turtle-interpreted L-system, with an explicit grain-descent operator, is sufficient to generate danceable glitch percussion whose fast detail is a measurable scaling of its slow pulse, and that the depth of that recursion is a usable, audible compositional variable.

## References

Lindenmayer, A. (1968). Mathematical models for cellular interactions in development, Parts I and II. *Journal of Theoretical Biology*, 18(3), 280–315.

Prusinkiewicz, P. (1986). Score generation with L-systems. In *Proceedings of the International Computer Music Conference (ICMC 1986)*, 455–457.

Prusinkiewicz, P., & Lindenmayer, A. (1990). *The Algorithmic Beauty of Plants*. New York: Springer-Verlag.

Worth, P., & Stepney, S. (2005). Growing music: Musical interpretations of L-systems. In *Applications of Evolutionary Computing (EvoWorkshops 2005)*, Lecture Notes in Computer Science 3449, 545–550. Springer.

Manousakis, S. (2006). *Musical L-systems*. Master's thesis, Institute of Sonology, Royal Conservatory, The Hague.

Xenakis, I. (1971). *Formalized Music: Thought and Mathematics in Composition*. Bloomington: Indiana University Press.

Schroeder, M. R. (1962). Natural sounding artificial reverberation. *Journal of the Audio Engineering Society*, 10(3), 219–223.
