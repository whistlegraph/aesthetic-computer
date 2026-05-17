# study · how to make a good trance track

**Last updated**: 2026-05-14
**Author**: @jeffrey

study-first. no bed yet. the goal of this file is to pin down what makes a trance track *work* before any mp3 lands in `pop/dance/out/`. once these rules are written down, the bed (`recap/bin/trance.mjs`) and the score format follow naturally.

## why trance (and why first for the `dance/` lane)

- four-on-the-floor + off-beat bass + supersaw lead is the **most legible** dance idiom — every part of the machinery is audible to a first-time listener
- the genre lives or dies on a *single repeated theme*. that maps directly onto AC's posture: one vision per track, compressed to a hook that survives looping
- the tension/release engine (breakdown → build → drop) is a structural argument, not a vibe. it can be specified, scored, and tested — which is what we want from a research-lane track
- jeffrey-pvc's "calm + descriptive" register fits trance better than trap. trance vocals are often spoken or one-shot phrases, not 16-bar rap cadence. lower risk to land
- canonical trance lives at ~138 BPM. that's close enough to the trap bed's 140 BPM that the existing `percussion.mjs` kit and `recap/bin/` tooling carry over with minor tweaks

## the anatomy of a good trance track

the body of this document. each piece below is non-negotiable for the idiom — drop any one of them and the track stops sounding like trance.

### 1. kick

- four-on-the-floor — kick on every beat, no swing, no ghost notes
- fundamental at ~55–65 Hz with a tight transient click around 1.5–2 kHz
- short tail (~150–200ms). the kick has to *clear* before the off-beat bass hits
- AC primitive: `percussion.mjs` letter `c` (TR-808-leaning) — already correct. mix it hot

### 2. off-beat bass — the engine

- hits on the "and" of every beat: positions 3, 7, 11, 15 of a 16-step grid
- short, plucked, single sub-octave note. usually root of the chord with occasional 5th
- **sidechain-ducked by the kick** — this is what creates the pumping "breathing" feel
- without the off-beat bass, four-on-the-floor sounds like house, not trance

### 3. supersaw lead — the song

- 5–7 detuned sawtooth voices, unison, slight chorus/ensemble
- carries the **melodic theme** — usually a 4-bar or 8-bar repeating phrase
- the theme is the song. if a listener can hum it after one breakdown, the track worked
- *AC gap*: no supersaw voice exists yet. `sinebells` is too clean, the piano bank is wrong tonally. **first new voice to build for this lane** (`pop/dance/synths/supersaw.mjs`)

### 4. pad

- long sustained chord pad, sidechain-ducked to the kick
- warm analog character — string ensemble, choir, soft saw stack
- carries the harmonic motion underneath the lead theme
- can substitute for the supersaw during the breakdown — same chord, different texture
- *AC primitive*: `sinebells` with long release approximates it; a proper "saw pad" voice would be better

### 5. hi-hats

- closed hat on the off-beats (between kicks), 8th-note resolution at minimum
- 16th-note hats in the build sections only — saving the density for impact
- open hat hits on the "and of 4" or "and of 2-and-4" to mark phrase ends
- AC primitives: `g` (closed hat), `a` (open hat)

### 6. white-noise riser & snare roll — the connective tissue

- white-noise riser sweeps from low to high frequency over 8–16 bars during builds
- snare roll *doubles in subdivision* each bar: 8ths → 16ths → 32nds → silence
- kick drops out for the last 1–2 bars of the build for maximum anticipation
- the drop on bar 1 of the next section is the **payoff** — full mix, no filter, kick back

### 7. sidechain compression

- not negotiable. pad + bass + sometimes lead duck under every kick
- creates the "engine" feel — the track *breathes* with the kick
- mechanically: every kick triggers a ~150ms volume duck on the pad/bass bus

## the arrangement law

a good trance track is an *argument*: introduce the theme stripped, then prove it in the drop. the radio edit and the club edit follow the same law at different scales.

```
club edit (6–8 minutes)              radio edit (~1:30 — our format)
─────────────────────                ───────────────────────────────
intro          32 bars               intro          8 bars
break 1        32 bars               break 1        16 bars
build 1        16 bars               build 1        8 bars
drop 1         32 bars               drop 1         16 bars
break 2        32 bars               break 2        8 bars
build 2        16 bars               build 2        4 bars
drop 2         32 bars               drop 2         16 bars
outro          32 bars               outro          4 bars
```

at 138 BPM, our 80-bar radio edit ≈ **1:23**, which is inside the `pop/` ~1:30 spec.

the breakdown is where the *theme* lives — exposed, no drums, pad + supersaw alone. the drop is where the *engine* lives — kick, off-beat bass, supersaw, pad, all sidechained, full mix. the listener should be able to hear the breakdown alone and the drop alone and recognize they're the same song.

## key, scale, and emotional center

- **minor key** — A minor, E minor, F# minor most common. trance is melancholy-resolving-to-euphoria, not happy-major-key
- **simple chord progression** — i / VI / III / VII (Am / F / C / G in A minor) or i / VII / VI / VII. four chords, looped. the simplicity is the point
- the supersaw theme can use the natural minor or melodic minor — both work
- avoid: cheerful major-key resolutions, complex jazz chords, modal mixture. trance is not about chord substitution

## vocal posture for jeffrey-pvc

trance vocal is rarely rap. options for jeffrey-pvc, in order of difficulty:

1. **one-shot hook phrases** (easiest, recommended first) — 4–8 word phrases dropped into the breakdown and the drop. e.g. *"the music is real"* over the supersaw theme. heavy reverb tail, low in the mix, possibly pitch-shifted up an octave
2. **spoken breakdown** — short prose section over the pad during break 1, before the drums return. jeffrey-pvc's calm/descriptive register works here without modification
3. **sung melodic line** (hardest) — jeffrey-pvc tries to *sing* the supersaw theme. likely needs heavy autotune + per-syllable pitch correction (the existing `vocal-post.mjs` infrastructure). save for track 2 or 3
4. **instrumental** — no vocal at all. valid for trance. fastest path to a finished track, but loses jeffrey's voice as the AC signature

start with option 1 + 2 combined. that's the standard trance arrangement: spoken/sung verse in the breakdown, hook phrase repeated in the drop.

## the theme — the actual song

the lead theme is what people remember. canonical examples to study (third-party, references only, not committed):

- **Robert Miles — "Children"** (1995): the piano phrase. 4 bars, ascending, minor key. the entire track exists to set up that line
- **Sasha & Emerson — "Scorchio"** (2000): supersaw lead theme, 8 bars, breakdown exposes it
- **Above & Beyond — "Sun & Moon"** (2011): vocal-led trance, but the supersaw theme between vocal phrases is the load-bearing element
- **Eric Prydz — "Opus"** (2015): 9-minute build, theme is the entire track
- **Anjunabeats catalog** generally: progressive trance with strong melodic themes

the theme for an AC trance track should compress *one AC vision* into a 4-bar singable phrase, same as the big-pictures hook compresses one paper into one rapped line. the lyric (if any) sits on top of the theme. the theme is the actual song.

## what to compose vs. what already exists

| component | status | notes |
|-----------|--------|-------|
| kick (TR-808) | ✅ exists | `percussion.mjs` letter `c` |
| closed hat | ✅ exists | `percussion.mjs` letter `g` |
| open hat | ✅ exists | `percussion.mjs` letter `a` |
| snare (for roll) | ✅ exists | `percussion.mjs` letter `d` |
| sidechain bus | 🛠 to build | duck pad/bass on every kick — ffmpeg sidechaincompress or buffer-level ducking in node |
| supersaw lead | ❌ to build | 5–7 detuned saws, unison, ~10–25 cents detune spread. new voice |
| saw pad | ❌ to build | saw stack with long attack/release, low-pass filtered. could share code with supersaw |
| off-beat sub bass | ⚠ partial | sinebells can fake it; a proper saw bass voice would be better |
| white-noise riser | 🛠 to build | filtered noise with rising cutoff over N bars |
| filter automation | 🛠 to build | low-pass cutoff sweep during breakdown → build |

**the one new instrument worth writing**: the supersaw. everything else is either already in `percussion.mjs` or assemblable from existing voices + ffmpeg filtering.

## ~1:30 compression — what we sacrifice

the radio edit above gives us 80 bars at 138 BPM = ~1:23. compared to a 6-minute club edit we lose:

- the long intro that lets DJs blend tracks (we don't need it)
- the second breakdown's depth (we keep it but cut from 32 → 8 bars — still a breath)
- repetition. a club track loops the drop 32 bars to fill the dance floor. we only need 16 bars per drop to make the point

what we **keep**: the breakdown → build → drop arc, the theme, the engine, both drops. losing any one of those breaks the genre.

## next steps

1. land this study + the lane README (`pop/dance/README.md`) — done in this commit
2. pick the first AC vision to compress into a trance theme. candidate: same as plork, but expressed as a 4-bar minor-key supersaw phrase instead of a rap hook. lets us A/B the two beds on the same source
3. write the supersaw voice (`pop/dance/synths/supersaw.mjs` or extend `recap/bin/`). 5-saw unison, parameterized detune cents, parameterized voice count
4. write the trance bed builder (`recap/bin/trance.mjs`). same shape as `trap.mjs`: read a score, mix the bed, output bed-only mp3
5. write the first track's score: `pop/dance/<slug>.np` with the theme + a score of the arrangement (intro/break/build/drop block markers)
6. vocal pass — jeffrey-pvc one-shot hook + breakdown spoken word. WhisperX align. sidechain duck under the kick
7. mix → `pop/dance/out/<slug>.mp3`

## open questions

- supersaw via 7 detuned sawtooth oscillators sums to a fairly aliased signal at higher pitches. do we need band-limited synthesis (PolyBLEP), or is the aliasing part of the character? (probably keep it — vintage hardware aliases too)
- sidechain in node: ffmpeg `sidechaincompress` works at file level, but in-the-mix sidechain (per-kick ducking on the pad bus) is cleaner done at buffer-mixing time. `trap.mjs` already mixes buffers in node — extend that path
- key choice for track 1: A minor is the most-canonical, F# minor is more emotional. pick after the theme is sketched, not before
- do we want a "psy-trance" sub-sub-genre at 142 BPM with the rolling bassline? probably not for track 1. revisit after the standard trance template is proven
- vocal-as-instrument: should the supersaw theme *be* a pitch-shifted jeffrey-pvc vowel formant stack? interesting research thread — punts to `pop/voice/`

---

*maintained by @jeffrey — update when the trance lane proves or pivots*
