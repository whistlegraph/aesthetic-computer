# Climbing and Descending the Groove Curve: A Controlled Syncopation Sweep in `peaktek.c`

**Abstract**

Empirical groove research has converged on an inverted-U relation between rhythmic syncopation and the pleasurable urge to move: the wanting-to-move response is weak when a pattern is metrically obvious, rises with moderate syncopation, and falls again once syncopation becomes so dense that metric entrainment fails. Most accounts of this curve sample it across listeners and stimuli. This thesis instead realises the curve *within a single piece of music*. `peaktek.c` is a 118.2-second, 150-BPM minimal-techno track whose four groove-carrying voices (sub bass, resonant stab, counter-bass, rim) carry a different rhythmic pattern in each major section, each dialled to a rising rung of syncopation: LOW (≈0), MEDIUM, HIGH (a deliberate over-shoot), and a settled SWEET pocket. Syncopation is quantified with the Longuet-Higgins & Lee metric-weight model as operationalised by Witek and colleagues, and the engine computes a syncopation score for every active pattern in every section, printing the full trajectory at render time. The measured arc rises from a section total of 0, through 35, peaks at 60 in the over-shoot section, then resolves to a 9–17 pocket. The track is thus an audible traversal of the groove curve.

## 1. Introduction

The word *groove* names a property that listeners report readily and reliably: the sensation that a piece of music compels bodily movement. A robust empirical finding is that this sensation does not increase monotonically with rhythmic complexity. Rhythms that coincide exactly with the metric grid are experienced as stiff or mechanical and elicit little urge to move; rhythms that are extremely dense and contradict the meter are experienced as confusing and likewise elicit little urge to move; the strongest movement response appears at an intermediate degree of syncopation. The relation is an *inverted U*.

Syncopation is the rhythmic device that drives this curve. Informally, a syncopation occurs when a sound event is placed on a metrically weak position and is *not* followed by an event on the next, metrically stronger position — the expected strong-beat event is withheld, and the body, having been primed by the meter, "fills in" the gap with motion. Counting and weighting such events yields a single scalar per pattern, and the inverted-U can then be plotted against that scalar.

The contribution of this work is methodological and compositional rather than perceptual: rather than treating the groove curve as something measured *over* a corpus of stimuli, the track `peaktek` is constructed *to walk along the curve*, section by section, so that a single listener hears the same material rendered at four points on the syncopation axis. The track is the experimental apparatus; the C program that renders it both *enacts* the sweep and *measures* it, printing each section's syncopation so the composition's claims are auditable against numbers. The remainder of this thesis states the theory (Section 2), shows how the engine realises and quantifies the sweep with reference to specific code (Section 3), reports the measured trajectory of the rendered track (Section 4), and discusses what a listener should hear and where the limits lie (Section 5).

## 2. Theoretical background

The metric-weight model underlying this work originates with Longuet-Higgins and Lee (1984), who proposed that a metric hierarchy assigns each grid position a weight reflecting its salience, and that syncopation can be detected as a *note-rest pair* in which an onset on a weak position is followed by a rest on a stronger position. Their formalism makes syncopation computable from a pattern and a metric template alone, without recourse to performance nuance.

Witek, Clarke, Pearce, Wallentin and Vuust (2014), in "Syncopation, Body-Movement and Pleasure in Groove Music" (*PLoS ONE* 9(4): e94446), operationalised exactly this model to study groove. Across drum-break stimuli spanning low to high syncopation they found that both rated pleasure and rated wanting-to-move followed an inverted-U over a syncopation index derived from the metric-weight scheme, peaking at a medium degree of syncopation. This is the curve `peaktek` traverses. The broader claim — that groove arises from the interaction of predictive metric expectation and its controlled violation — is consonant with predictive-coding accounts of rhythm (Vuust & Witek, 2014, "Rhythmic complexity and predictive coding," *Frontiers in Psychology* 5: 1111).

The metric template itself follows the standard subdivision hierarchy for 4/4 at the sixteenth-note grid: the downbeat is strongest, the half-bar next, then the remaining quarter-note beats, then the eighth-note offbeats, then the sixteenth subdivisions weakest. Implementations differ in sign convention; here the strongest position is 0 and weaker positions are increasingly negative, so that a *larger* weight means *more* metrically salient, and a syncopation is an onset whose immediately following grid position is a rest with a *larger* weight (London, 2012, *Hearing in Time*, 2nd ed., Oxford University Press, provides the metric-hierarchy framing adopted here).

## 3. Method

### 3.1 The syncopation metric in code

The metric template is the array `LHL_W[16]` (`c/peaktek.c:51`), with the downbeat at weight 0 and progressively weaker positions at −1, −2, −3, −4. The scoring function `sync_score()` (`c/peaktek.c:58`) implements the Longuet-Higgins/Witek note-rest rule directly. For a 16-character pattern string `p` (`'x'` = onset, `'.'` = rest):

```
score(p) = Σ_{i : p[i]='x'}  [ p[(i+1) mod 16] = '.'  ∧  W[(i+1) mod 16] > W[i] ]
                              · ( W[(i+1) mod 16] − W[i] )
```

That is, for each onset at step *i*, look at the next grid step *j = (i+1) mod 16*; if *j* is a rest whose metric weight exceeds the onset's, accumulate the weight difference. The wrap-around `mod 16` makes the score bar-cyclic, matching how the loop repeats. A companion `onsets()` (`c/peaktek.c:68`) counts hits so that density can be reported alongside syncopation and the two are not conflated.

A consequence of this rule, visible in the variant design below, is that the maximum-syncopation pattern on this grid places onsets on every odd (weak) step `"​.x.x.x.x.x.x.x.x"`: each odd onset is immediately followed by an even rest of strictly greater weight, yielding the ceiling score of 15. This is precisely the kind of "too dense to entrain" pattern that the inverted-U predicts will *reduce* groove, and it is used deliberately as the over-shoot.

### 3.2 The four-rung sweep

The experimental manipulation is a per-section *rung* of syncopation applied to the four groove-carrying voices. Each voice has a four-element table of patterns indexed by rung — `SUB_V`, `STAB_V`, `CBASS_V`, `RIM_V` (`c/peaktek.c:362`–`365`) — and the rungs are named by the enum at `c/peaktek.c:356`:

| rung | design | example (sub) |
|------|--------|---------------|
| LOW  | on-grid, downbeat-locked | `x...x...x...x...` |
| MED  | interlocking offbeat groove | `x.xx.x.x.x.x.x.x` |
| HIGH | onsets crowded on weak steps (ceiling) | `.x.x.x.x.x.x.x.x` |
| SWEET| refined lower-medium pocket | `x.xx.x..x.xx.x..` |

The section-to-rung map `SEC_RUNG[8]` (`c/peaktek.c:369`) assigns the rung per section of the arrangement: `intro` and `build` are LOW; `grvA` and `brk1` are MED; `grvB` is HIGH; `drop`, `grvC` and `outro` are SWEET. Inside the main sequencer loop, the active section's rung selects the working patterns in one place (`c/peaktek.c:408`–`410`):

```c
int rung = SEC_RUNG[s];
const char *P_SUB = SUB_V[rung], *P_STAB = STAB_V[rung];
const char *P_CBASS = CBASS_V[rung], *P_RIM = RIM_V[rung];
```

so that every later reference to `P_SUB[st]`, `P_STAB[st]`, etc. automatically uses the section's rung. No other change to the voice-triggering logic was required — the same `sub()`, `stab()`, `cbass()` and `rim()` synth functions (`c/peaktek.c:201`, `:239`, `:219`, `:128`) are driven, only the *placement* changes. One ornament, a bright off-grid octave stab on step 11, is gated to the SWEET rung only (`c/peaktek.c:455`) so that it enriches the settled pocket without contaminating the audible contrast between the LOW/MED/HIGH stages.

### 3.3 Instrumentation of the measurement

Before any audio is rendered, the engine prints the entire sweep. The reporting block (`c/peaktek.c:378`–`403`) iterates the eight sections, and for each computes the syncopation of every *active* groove lane (gated by the section's lane mask `MASK[s]`), the section total Σ, and the section onset count. It also tracks the running maximum to identify the over-shoot section (`peak_sync`, `peak_sec`, updated at `c/peaktek.c:392`). The closing line (`c/peaktek.c:396`) reports the peak and the full SWEET-rung pocket. Because the numbers in Section 4 are emitted by `sync_score()` — the same function that defines the metric — the composition's structural claims are not asserted but computed.

### 3.4 Engine spine (unchanged)

The deepening preserves the track's two-bus architecture: a dry DRUM bus and a MUSIC bus ducked by a kick-triggered sidechain. The kick stamps the sidechain trigger array `trig[]` on each onset (`c/peaktek.c:88`); a release-envelope follower then ducks the music bus hard on every kick (`c/peaktek.c:485`). Stabs, pads and percussion feed a Schroeder reverb send (`c/peaktek.c:490`) with a 3/16 ping-pong slap delay ahead of it (`c/peaktek.c:476`), and the master normalises to 0.89 peak with short fades (`c/peaktek.c:510`). Gain staging keeps the sub low (root + 12 of `ROOTS` at `c/peaktek.c:319`) distinct from the counter-bass an octave above and the stabs in the F3–C4 register (`STAB_ROOT`, `c/peaktek.c:321`), so that climbing the syncopation rungs never collapses the low-end clarity.

## 4. Composition and results

The arrangement is an eight-section form summing to 72 bars at 150 BPM (1.6 s/bar), giving 118.2 s of audio. The form, with its measured syncopation, is exactly what the engine prints at render time:

```
# section  rung   sub stab cbas rim |  Σ   onsets
# intro   LOW      0    0    0   0 |   0     2
# build   LOW      0    0    0   0 |   0     6
# grvA    MED     10    8    9   8 |  35    25
# brk1    MED     10    8    9   8 |  35    25
# grvB    HIGH    15   15   15  15 |  60    32
# drop    SWEET    6    0    5   0 |  11    11
# grvC    SWEET    6    6    5   0 |  17    15
# outro   SWEET    6    0    0   3 |   9    11
# peak Σ-syncopation = 60 at section 'grvB' (the over-shoot); groove
# sweet-spot for this material sits at the SWEET rung
# (sub=6 stab=6 cbas=5 rim=3, Σ=20).
```

The trajectory is the inverted-U made temporal. Each section "demonstrates" one point on the curve:

- **intro / build (LOW, Σ = 0).** The sub and a few percussion hits sit on the grid. Syncopation is zero by construction; nothing pulls against the meter. This is the *stiff* tail of the curve — present so the listener has a baseline of metric obviousness to depart from.
- **grvA / brk1 (MED, Σ = 35).** All four lanes engage their interlocking offbeat patterns: sub = 10, stab = 8, counter-bass = 9, rim = 8. This is the canonical medium groove. The per-lane scores sit squarely in the medium band that Witek et al. associate with peak wanting-to-move, and the section is the most straightforwardly *danceable*.
- **grvB (HIGH, Σ = 60).** Every groove lane jumps to the ceiling pattern (15 each). This is the deliberate over-shoot: the engine confirms it as the global maximum (peak Σ = 60 at `grvB`). Density rises only modestly (32 onsets vs. 25 at MED) — the change is in *placement*, not quantity — yet the meter becomes hard to lock onto. The track here is meant to feel *exciting but slippery*, the far descending side of the inverted-U.
- **drop / grvC / outro (SWEET, Σ = 11, 17, 9).** The grooves resolve into a refined lower-medium pocket (per-lane sub = 6, stab = 6, counter-bass = 5, rim = 3; full pocket Σ = 20 when all four are active). This is the track's claimed *sweet spot for this material*: not the maximum, and pulled slightly below the MED stage, so that the resolution reads as a settling rather than a mere return.

Two methodological points are worth noting from the numbers. First, syncopation and onset density are decorrelated by design: MED and HIGH differ by 25 in Σ but only 7 in onset count, demonstrating that the manipulation is rhythmic placement rather than busyness. Second, the SWEET total varies by section (11/17/9) because the lane mask differs — `drop` and `outro` strip lanes for arrangement contrast — so the printed Σ honestly reflects what is *sounding*, while the summary line reports the canonical full-pocket value (20).

## 5. Discussion

What works: the engine makes a perceptual hypothesis structurally legible. Because the rung is a single integer per section and the patterns are tabulated by rung, the relationship between the theory (an inverted-U over a metric-weight syncopation index) and the artefact (a 118-second techno track) is direct and inspectable, and the printed trajectory lets any rebuild verify that the composition still climbs and descends the curve. The over-shoot section is genuinely the global maximum and the resolution genuinely sits below the medium plateau, so the arc is not merely asserted.

A listener should hear: a stiff, on-grid opening that feels like it is "waiting"; a locking-in at `grvA` where the parts begin to pull at each other and the head starts to nod; a giddy, slightly disorienting intensification at `grvB` where the pulse is harder to hold even as energy peaks; and a satisfying settle into the closing grooves where the pocket feels deepest precisely because it is *less* syncopated than the peak just heard. The juxtaposition is the point: the SWEET pocket is defined by contrast with the over-shoot that precedes it.

Limitations: the metric model scores only adjacent note-rest pairs on a single 16-step template, so it is blind to longer-range syncopations, to cross-rhythm against the kick, and to the contribution of the sidechain pump, swing (the engine swings odd sixteenths at 0.535, `step_t` at `c/peaktek.c:312`), and timbre — all of which materially affect felt groove but are not in the scalar. The HIGH rung's ceiling pattern is the *metric* maximum but may be perceived as a new, faster pulse rather than as syncopation per se, which would relocate it on a listener's curve. And the inverted-U is here *engineered* into the structure; this is a demonstration, not a perceptual measurement — establishing where listeners actually place this material's peak would require a rating study.

Future work: replace the single-template scorer with a weighted multi-resolution metric (summing across metric levels), incorporate the kick and sidechain into a combined syncopation-against-pulse measure, and run a small listener study sweeping the rung continuously to locate the empirical peak for this timbral palette — turning the engineered curve into a measured one.

## References

- Longuet-Higgins, H. C., & Lee, C. S. (1984). The rhythmic interpretation of monophonic music. *Music Perception*, 1(4), 424–441.
- London, J. (2012). *Hearing in Time: Psychological Aspects of Musical Meter* (2nd ed.). Oxford University Press.
- Vuust, P., & Witek, M. A. G. (2014). Rhythmic complexity and predictive coding: a novel approach to modeling rhythm and meter perception in music. *Frontiers in Psychology*, 5, 1111.
- Witek, M. A. G., Clarke, E. F., Pearce, M., Wallentin, M., & Vuust, P. (2014). Syncopation, body-movement and pleasure in groove music. *PLoS ONE*, 9(4), e94446.
