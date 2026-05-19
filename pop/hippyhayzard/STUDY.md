# study · how to make a good hippyhayzard track

**Last updated**: 2026-05-19
**Author**: @jeffrey

study-first, same as [`../dance/STUDY.md`](../dance/STUDY.md). pin down what makes the blend *work* before chasing the next mp3. the first bed (`bin/render.mjs`) exists, but it is a sketch against this document, not the other way round.

## why this blend (and why a lane of its own)

happy hardcore and nightcore keep getting filed together — fast, major, pitched-up, hyper-melodic. the nightcore originators literally said they were going for *"'happy hardcore' as they say"*. but one is a *genre with its own canon* and the other is a *speed/pitch treatment of someone else's song*. averaging them is mush. the lane exists to do the opposite: hold both in tension and **switch** between them as the structural argument — the way the dance lane's argument is breakdown → drop.

- happy hardcore gives a real engine that can be specified and scored: 4-on-floor, reverse bass, hoover, M1 stabs, ecstatic breakdown. (history: rave sped 135→155 BPM by mid-'93; the *Mentasm* hoover came off a Roland Alpha Juno-2; classic comps run ~160–175 BPM over chunky kicks and Amen breaks — see sources below.)
- nightcore gives a treatment, not a substrate: +20–35% speed, +~3 semitones, melody/vocals pushed bright and high. applied as a *layer* (octave glock doubling, sheen), never as the song — keeping the bottom-up posture intact.
- the **hazard** is what makes it ours: a half-time, relative-minor switch where the `skrill` talking-bass becomes a rave siren. euphoria that can turn. that switch is the genre.

## the anatomy of a good hippyhayzard track

non-negotiable for the idiom. drop any one and it stops being this thing.

### 1. the kick — chunky, not clicky
4-on-the-floor in hippy; **half-time (1 & 3) in hazard**. fundamental ~46–60 Hz with a hard transient. it has to clear before the reverse bass swells. inline-synthesized (sine pitch-drop 132→46 Hz + a 4 ms click).

### 2. reverse bass — the rave engine
not an off-beat pluck (that's trance). a sub note that **swells in** then is hard-cut by the next kick. sits on the off-beat. without it the four-on-floor reads as house or hardstyle, not happy hardcore.

### 3. the hoover — the hook
the load-bearing voice. detuned saw+pulse stack, PWM, resonant honk, and the **whoop** (pitch bends down into the note and springs back). if a listener can't hum the hoover hook after one drop, the track failed. `synths/hoover.mjs`, preset `stab` for 16th-note riffs, `whoop` for sustained calls, `hazard` for the dark switch.

### 4. the glock octave-double — the nightcore sugar
bright bell/M1 layer doubling the hook **+12**. present in hippy, **gone in hazard**. this single on/off is most of the perceived genre-switch. `sinepower` `stab` preset, short decay.

### 5. swung 16th hats — the "swingy switchez"
hats on a 16th grid with every off-16th delayed ~16%. in hazard the swing widens and thins to sparse 8ths. swing is structural: it is *how* the feel switches, not ear-candy.

### 6. the hazard switch — the argument
8-bar block: half-time kick, relative minor, glock out, `skrill` siren in (slow descending minor talking-bass), hoover on `hazard`. ~3–4 dB hotter than the drop for impact — a *deliberate* lift, never a mix accident (an early render had it +7 dB; that's the failure mode to watch). resolve with a riser back into hippy.

### 7. sidechain pump
every kick ducks the melodic bus ~0.42 for ~160 ms. the track breathes. same law as trance, faster.

## the blend law

```
build      8 bars   glock hook only + riser, no kick           (nightcore, exposed)
hippy 1   16 bars   full engine, glock-doubled                  (happy hardcore)
HAZARD     8 bars   half-time, minor, skrill siren, glock out   (the switch)
hippy 2   16 bars   full engine                                 (return, brighter)
build 2    4 bars   short re-lift
HAZARD 2   8 bars   the switch, varied
hippy 3   16 bars   final drop
outro      4 bars
```

at 174 BPM this 80-bar radio edit ≈ **1:30**, inside the `pop/` spec. the listener must be able to hear the hippy drop alone and the hazard alone and recognize the *same theme* — one bright, one turned. that recognition is the whole track.

## key, scale, emotional center

- **major** for hippy — A major default. happy hardcore is not melancholy-resolving (that's trance); it is already euphoric and the lyric/theme rides that
- **relative or parallel minor** for hazard — F# minor (relative) reads as "same world, gone wrong"; A minor (parallel) reads as a harder slam. default relative; parallel for the heavier track
- simple looped progression, I–V–vi–IV in major flipping to i–VI–III–VII in the hazard. four chords. the simplicity is the point
- avoid: jazz chords, modal noodling, a *tempo* change at the switch (it's a *feel* change — keep 174)

## vocal posture for jeffrey-pvc

instrumental is valid and the fastest finish. when present, in order of difficulty:

1. **pitched-up one-shot hook** (recommended first) — a 3–6 word phrase in the hippy drop, pitch-shifted up ~3 semitones (the literal nightcore move), heavy reverb. e.g. *"the computer is bright"*
2. **low hazard spoken line** — one slow line in the hazard, jeffrey-pvc's calm register, *not* pitched up. the contrast with (1) is the point
3. **sung hook** (hardest) — jeffrey-pvc sings the hoover hook, autotuned, per-syllable corrected (existing `vocal-post.mjs`). save for track 2+

start with 1 + 2: a bright chipmunk hook in hippy, a dark spoken line in hazard. same voice, switched — mirrors the music's own argument.

## what to compose vs. what already exists

| component | status | notes |
|---|---|---|
| hoover | ✅ built | `synths/hoover.mjs`, this lane's gating voice |
| skrill siren | ✅ built | `../dance/synths/skrill.mjs`, the hazard siren |
| glock bells | ✅ exists | `../dance/synths/sinepower.mjs` `stab` +12 |
| kick / snare / hat / reverse-bass / riser | ✅ inline | synthesized in `bin/render.mjs` |
| sidechain pump | ✅ built | per-kick duck on the melodic bus in `render.mjs` |
| `.np` → render wiring | 🛠 to build | renderer currently hardcodes the arrangement; should read `hippyhayzard.np` markers |
| true-peak limiter | 🛠 to build | demo decode-peaks ~1.09; needs a TP-safe limiter before release |
| jeffrey-pvc vocal | ⬜ not started | pitched-up hippy hook + low hazard line |

## next steps

1. land this study + the lane README — done in this commit
2. wire `bin/render.mjs` to actually parse `hippyhayzard.np` section markers instead of the hardcoded `SECTIONS` array
3. extend the arrangement to the full 80-bar radio edit (the demo is a 20-bar proof)
4. true-peak limiter on the master (the one real release blocker)
5. pick the first AC vision to compress into a hippyhayzard hook (candidate: the same vision the dance lane takes, to A/B euphoric-switch vs trance-arc on one source)
6. vocal pass — pitched-up one-shot hook + low hazard line, WhisperX align, duck under the kick

## open questions

- does the hazard want the *relative* (F# minor) or *parallel* (A minor) minor by default? relative is more musical, parallel is more brutal. demo uses relative — revisit per track
- glock as `sinepower` is clean but not very "bell". a short FM ratio-2 bell voice might read more M1. worth a `synths/glock.mjs` or a `sinepower` bell preset?
- the hoover whoop is a fixed down-up shape. real Alpha Juno hoovers vary the bend per player. parameterize whoop curve per `.np` note?
- nightcore is *defined* by being a remix. is an original "nightcore layer" honest, or is the octave-glock-doubling the only defensible nightcore gesture for a bottom-up lane? (current answer: the doubling + brightness is the gesture; speeding up someone else's song is not available to us and that's fine)

## sources

genre research, third-party, references only (not committed):

- [History of happy hardcore and UK hardcore — toucanmusic](https://www.toucanmusic.com/articles/ukhardcore/)
- [Breakbeat hardcore — Wikipedia](https://en.wikipedia.org/wiki/Breakbeat_hardcore)
- [9 tips & production tricks for rave revivalists — MusicRadar](https://www.musicradar.com/tuition/tech/9-tips-and-production-tricks-for-rave-revivalists-636301)
- [Happy Hardcore — Melodigging](https://www.melodigging.com/genre/happy-hardcore)
- [Nightcore — Wikipedia](https://en.wikipedia.org/wiki/Nightcore)
- [What is Nightcore Music — Pond5](https://blog.pond5.com/80716-what-is-nightcore-music/)
- [Exploring Nightcore: sound, speed, appeal — SoundCy](https://soundcy.com/article/what-does-nightcore-sound-like)

---

*maintained by @jeffrey — update when the lane proves or pivots*
