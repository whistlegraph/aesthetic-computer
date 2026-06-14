# Timelines Under Substitution: Rhythmic Oddity and Maximal Evenness as Compositional Variables in a Dub-Techno Engine

**Abstract**

The clave is usually treated by producers as a fixed signature — a stamp applied once and forgotten. This thesis treats it instead as an *experimental variable*. We take a fast (138 BPM) dub-techno track, *dubtek*, and rebuild its C synthesis engine so that the asymmetric timeline organising the chord stab and woodblock is swapped section by section among three near-isochronous five-onset patterns on the sixteen-pulse cycle — the 3-2 son clave, the 3-2 rumba clave, and the maximally even Euclidean bell E(5,16) — over an unchanging C-minor harmony and an unchanging four-on-the-floor kick. The engine computes, at render time, three formal properties of every candidate timeline: Arom's rhythmic-oddity property, Toussaint's geometric evenness, and the off-beatness count, and prints them to standard error so the composition can be discussed in terms of its own measurements. We find that four of five candidate timelines satisfy rhythmic oddity (the bembé E(7,12) bell, on the twelve-pulse grid, is the sole failure), that evenness orders the five-onset claves as Euclid (0.996) > son (0.993) > rumba (0.989), and that this ordering is audible: the rumba section is the most syncopated, the Euclid section the most "settled." The track becomes its own listening test for the geometry of the timeline.

## 1. Introduction

In Afro-Cuban music and in the global dance idioms that inherited from it — including the Berlin/Jamaica lineage of dub techno that *dubtek* inhabits — a short, asymmetric, repeating pattern called a *timeline* (clave, bell, woodblock) governs the placement of every other part. The timeline is a temporal key signature: it tells the ear where "one" is and which displacements count as in-the-pocket rather than wrong. Producers typically choose one such timeline and freeze it.

This is a missed opportunity. The clave family is not a set of arbitrary folk patterns; it is a small, tightly constrained region of rhythm space, and the members of that family differ from one another by *measurable* amounts along well-defined axes. If those differences are measurable, they are composable. The central claim of this thesis is that the choice of timeline can be made an explicit, audible parameter of a piece — that one can hold harmony, tempo, kick, and instrumentation constant and *substitute the timeline* so that a listener hears, across a single track, what the literature describes only in tables.

The phenomenon under study is therefore not a single groove but the *deformation between grooves*: how the same chords land differently when their organising clave is the son, the rumba, or the maximally even Euclidean ideal. The engine is built so that the deformation is the composition.

## 2. Theoretical background

Three results from the mathematical theory of rhythm underwrite the design.

**Rhythmic oddity (Arom).** Simha Arom, in his study of Central African Aka polyphony (Arom, *African Polyphony and Polyrhythm*, 1991), isolated a property of certain asymmetric cycles of even length: no two onsets divide the cycle into two halves of equal duration. A cycle of period $2k$ has the *rhythmic oddity property* iff for every onset $i$ there is no onset at $(i + k) \bmod 2k$. Such rhythms cannot be folded in half; they resist the binary symmetry that the four-on-the-floor kick imposes, and this resistance is the source of their forward lean.

**Maximal evenness and Euclidean rhythms (Clough & Douthett; Toussaint).** Clough and Douthett ("Maximally Even Sets," *Journal of Music Theory*, 1991) formalised the idea of distributing $k$ onsets among $n$ pulses as evenly as the integer grid allows. Toussaint ("The Euclidean Algorithm Generates Traditional Musical Rhythms," 2005; and *The Geometry of Musical Rhythm*, 2013) showed that these maximally even sets are exactly the patterns produced by applying Euclid's GCD algorithm to $(k, n)$, and that an astonishing number of world timelines *are* Euclidean rhythms: the bembé bell is E(7,12); the maximally even five-in-sixteen pattern E(5,16) is the canonical 2-3 son rotation. Geometrically, evenness is captured by inscribing the onsets as vertices on a regular $n$-gon and summing the chord lengths between every pair of onsets; the regular $k$-gon — the perfectly even set — provably maximises this sum.

**Off-beatness (Toussaint).** Toussaint also proposed a syncopation proxy: count the onsets that fall on pulses which can never be a vertex of any inscribed regular polygon dividing the cycle, i.e. pulses $p$ with $\gcd(p, n) = 1$. Such pulses are "unreachable" by any even subdivision and so feel maximally off the beat.

The son and rumba claves are the canonical rhythmic-oddity timelines (Toussaint, 2013, ch. 10); the rumba is the son with its third stroke displaced by one pulse, a one-pulse move that, as we measure below, costs evenness and buys syncopation. Dub techno's off-beat stab is itself a clave gesture — the chord falling between the kicks — so embedding a literal clave is not a graft but a recovery of the form's own logic.

## 3. Method

The engine `c/dubtek.c` realises the theory in three layers: timeline data, a render-time analysis module, and a section-driven substitution mechanism.

**Timeline data.** Each candidate is a binary array over its grid. The five-onset claves live on the sixteen-pulse cycle (`CLAVE_32` at `c/dubtek.c:261`, the true five-stroke 2-3 rotation `CLAVE_23` at `:262`, `RUMBA_32` at `:266`, the maximally even `EUCLID_5_16 = E(5,16)` at `:275`); the bembé bell `BEMBE_12 = E(7,12)` lives on the twelve-pulse cycle at `:272`.

**Analysis module.** Four pure functions, computed at startup and printed to stderr, give the thesis its numbers.

*Onset extraction* (`tl_onsets`, `c/dubtek.c:297`) lists the pulse indices that carry a stroke.

*Rhythmic oddity* (`tl_oddity`, `:302`) implements Arom's test directly:

```
oddity(g, per):
  if per is odd: return true          # vacuous for odd cycles
  half = per / 2
  for p in 0..per-1:
    if g[p] and g[(p + half) mod per]: return false
  return true
```

*Geometric evenness* (`tl_evenness`, `:323`) places the $n$ pulses on the unit circle and returns the ratio of the timeline's pairwise chord-length sum to that of the regular $k$-gon:

$$E = \frac{\sum_{a<b} 2\,\bigl|\sin\!\frac{\pi(o_a - o_b)}{n}\bigr|}{\sum_{a<b} 2\,\bigl|\sin\!\frac{\pi(a-b)}{k}\bigr|}\,,\qquad E \in (0,1],\ E = 1 \text{ iff maximally even.}$$

The chord sum (not the arc/geodesic sum) is essential: the arc-length measure is degenerate for near-even five-onset sets — son, rumba and Euclid all tie at 1.000 under arcs — whereas the chord sum, which the regular polygon provably maximises, separates them. This choice is documented in the code comment at `:316`.

*Off-beatness* (`tl_offbeatness`, `:312`) counts onsets on pulses coprime to the period, via a standard `gcd_i` (`:310`).

The reporter `tl_report` (`:340`) prints, for each timeline, its onset list, $(k/n)$, oddity, evenness and off-beatness; the call block at `c/dubtek.c:407–424` also prints the son↔rumba and son↔euclid Hamming distances and a tally of how many candidates satisfy oddity.

**Substitution mechanism.** The eight sections each nominate a master timeline via `SEC_TL` (`c/dubtek.c:357`): the form runs son → son → son → **rumba** → **rumba** → **euclid** → **euclid** → son. Inside the bar loop the active sixteen-pulse timeline is selected by a `switch (SEC_TL[s])` (`:442`): the son section alternates the 3-2 and 2-3 rotations every two bars (so the clave breathes), while the rumba and Euclid sections state their pattern whole so the difference reads cleanly. The chord stab (`stab`, `:135`) and the audible woodblock (`clave`) fire only where the active timeline `CLAVE[st]` is set (`c/dubtek.c:470` and `:478`); a counter-bass answers in the gaps of the timeline's two-side (`L_CBASS`, `:484`), enacting the call/response of the clave's two halves. The bembé E(7,12) bell rides over everything on its own triplet grid (`:490`).

The remainder of the engine spine is preserved: the deep kick stamps the sidechain trigger array (`kick`, `:60`); the kick-ducked music bus breathes under the dry drum bus (sidechain at `c/dubtek.c:541`); the chord stabs feed a tempo-synced dotted-eighth feedback delay (`:519`) and a six-comb dark Schroeder reverb send (`:545`); the master normalises and applies short fades before `write_wav_f32_stereo`.

## 4. Composition and results

The render is 121.3 s (within the 110–150 s target), 68 bars at 138 BPM. The engine printed the following timeline analysis for this build:

| timeline | onsets | $k/n$ | oddity | evenness | off-beat |
|---|---|---|---|---|---|
| son 3-2 | 0,3,6,10,12 | 5/16 | yes | 0.993 | 1 |
| son 2-3 | 0,2,6,9,12 | 5/16 | yes | 0.993 | 1 |
| rumba 3-2 | 0,3,7,10,12 | 5/16 | yes | 0.989 | 2 |
| euclid E(5,16) | 0,3,6,10,13 | 5/16 | yes | 0.996 | 2 |
| bembé E(7,12) | 0,2,4,5,7,9,11 | 7/12 | **no** | 0.994 | 3 |

with **son↔rumba Hamming = 2 pulses**, **son↔euclid Hamming = 2 pulses**, and **rhythmic oddity: 4/5 candidate timelines pass**.

These numbers structure the form as a controlled experiment.

*Sections 1–3 (intro, build, grvA — son).* The son clave is established as the control. It satisfies oddity, sits at evenness 0.993, and has off-beatness 1 — a single stroke (step 3) on a pulse coprime to 16. The stab/woodblock alternate the two son rotations so the listener internalises the reference before any substitution.

*Sections 4–5 (dub1, grvB — rumba).* The first substitution. The rumba differs from the son by exactly two pulses (Hamming = 2): its third stroke slides from pulse 6 to pulse 7. That one-pulse slide is measurable in both directions — evenness drops to 0.989 (the lowest of the five-onset family: the 7→10 gap and the 10→12 gap now bracket a tighter cluster) and off-beatness rises to 2. The track makes this audible: over identical chords and kick, the rumba section is the most restless, the stab pulling later and harder against the floor.

*Sections 6–7 (dub2, grvC — Euclid E(5,16)).* The second substitution, to the *maximally* even five-onset timeline. Euclid posts the highest evenness in the family (0.996) while retaining off-beatness 2 and oddity. Heard after the rumba, the Euclid section is the resolution: the stabs distribute as smoothly as the integer grid permits, the groove "settles" without becoming square. That E(5,16) is also a literal son rotation (it differs from the 3-2 son by Hamming 2) closes the loop theoretically — the experiment's two endpoints are siblings.

*Section 8 (outro — son).* Return to the control, recontextualised: the listener now hears the son not as default but as one point on a measured continuum between rumba's tension and Euclid's repose.

The bembé bell is the experiment's negative control. On its twelve-pulse triplet grid it is the only candidate that **fails** rhythmic oddity (4/5 pass): its strokes at 0,2,4,5,7,9,11 include the antipodal pair (5, 11) that bisects the cycle. The thesis prediction — that a non-oddity timeline folds more readily against the kick — is borne out: the bembé rides *with* the four-on-the-floor as a shimmering triplet wash rather than fighting it, which is exactly why it functions as colour rather than as the groove's spine.

## 5. Discussion

What works: the three measured axes turn out to track three audible qualities. Evenness predicts "settledness" (Euclid > son > rumba); off-beatness predicts restlessness in the same order; rhythmic oddity predicts whether a timeline contends with the kick (the five-onset claves) or rides it (the bembé). Holding everything but the timeline fixed lets a non-specialist listener hear differences that are otherwise buried under harmonic and timbral change. The chord-sum evenness measure was the load-bearing methodological choice: the more obvious arc-length measure could not separate the claves at all, and switching to chords (Toussaint's actual geometric formulation) recovered the 0.989 / 0.993 / 0.996 ordering.

Limitations. The evenness values are bunched in a narrow band (0.989–0.996) because all five-onset claves are *near* maximally even by construction — the experiment lives in a small neighbourhood of rhythm space, and a listener cannot map a heard groove to a third-decimal-place number. The substitution is also discrete and section-aligned; the displacement between son and rumba is exactly one pulse, so the "continuum" is sampled at only three points. The bembé is analysed on a different grid (12 vs 16), so its evenness is not directly comparable to the claves' — it is a within-grid measure only.

Future work. The natural extension is a *continuous* timeline morph: interpolate stroke positions in real time from son toward rumba and on toward a deliberately non-even, low-evenness pattern, sweeping the evenness scalar smoothly while printing it per bar, so the listener hears the metric itself move. One could also drive the sidechain depth or the reverb decay from the live off-beatness count, coupling timbre to timeline geometry. Finally, a perceptual study — does the measured 0.989/0.993/0.996 ordering predict listener ratings of "tension"? — would test whether the geometry is merely descriptive or genuinely heard.

The track stands as a small instrument for a large idea: that the clave is not a stamp but a dial, and that the dial is calibrated by theorems.

## References

Arom, S. (1991). *African Polyphony and Polyrhythm: Musical Structure and Methodology*. Cambridge University Press.

Clough, J., & Douthett, J. (1991). Maximally Even Sets. *Journal of Music Theory*, 35(1/2), 93–173.

Toussaint, G. T. (2005). The Euclidean Algorithm Generates Traditional Musical Rhythms. In *Proceedings of BRIDGES: Mathematical Connections in Art, Music, and Science*, 47–56.

Toussaint, G. T. (2013). *The Geometry of Musical Rhythm: What Makes a "Good" Rhythm Good?* Chapman and Hall/CRC.

Demaine, E. D., Gomez-Martin, F., Meijer, H., Rappaport, D., Taslakian, P., Toussaint, G. T., Winograd, T., & Wood, D. R. (2009). The Distance Geometry of Music. *Computational Geometry: Theory and Applications*, 42(5), 429–454.
