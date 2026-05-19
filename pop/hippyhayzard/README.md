# hippyhayzard

audio-only happy hardcore × nightcore, built bottom-up from AC instruments. the lane and its first track share a name — `hippyhayzard` — because the name *is* the form: **hippy** (bright major euphoria) colliding with **hazard** (a minor rave-siren switch).

opens on the **hippyhayzard** sub-format. future sub-formats (UK hardcore, freeform, makina) land alongside if and when they earn a finished track.

## the blend (decided @jeffrey, 2026-05-19)

happy hardcore and nightcore are cousins — fast, major-key, pitched-up, euphoric — but they are not the same thing and the lane refuses to average them into mush. it switches between them on purpose:

- **happy hardcore** is the *engine*: ~174 BPM, chunky 4-on-the-floor, reverse bass, the hoover, Korg-M1-style piano/bell stabs, ecstatic breakdowns. its own canon, its own comps.
- **nightcore** is the *sugar*: +octave glock/bell doubling, brightness, the sped-up "chipmunk" sheen. nightcore is a *treatment*, not a song — so here it is applied as a layer, never as the substrate (that would be top-down; see [SCORE.md](../SCORE.md) posture).
- **the hazard** is the *switch*: the track drops to half-time + relative minor, kills the glock, and brings the `skrill` talking-bass in as a rave siren. same theme, dark. then it springs back to hippy. the switch is the genre — without it this is just fast happy hardcore.

"glocky bpm, swingy switchez": bright glockenspiel/bell melodic doubling + a swung 16th groove that *switches feel* (bouncy ↔ half-time). swing is structural, not decoration.

## format spec — hippyhayzard

- **length**: ~1:30 (radio edit). compress without losing the build → hippy drop → hazard switch → hippy return arc
- **tempo**: 174 BPM, 4/4 (range 168–178). the hazard switch is felt half-time (≈87 BPM feel), not a tempo change
- **key**: bright **major** for hippy (A major default) ↔ its **relative/parallel minor** for hazard (F# minor). euphoria that can turn
- **structure**: intro/build (8) → hippy drop 1 (16) → hazard (8) → hippy drop 2 (16) → build 2 (4) → hazard 2 (8) → hippy drop 3 (16) → outro (4) bars
- **bed**: chunky kick (4-on-floor; half-time in hazard) + reverse bass (off-beat swell) + swung 16th hats + hoover hook + glock octave-double (hippy only) + skrill siren (hazard only)
- **vocal**: optional. jeffrey-pvc as a pitched-up one-shot hook in the hippy drop (the nightcore "chipmunk") + a low spoken line in the hazard. instrumental is valid
- **output**: single mp3 in `out/hippyhayzard.mp3`. no video. ID3 album `pixsies`

## source → track

one AC vision per track, same as the other lanes — compressed to a *euphoric hook that can turn dark*, not a rap line or a trance theme.

```
papers/arxiv-<slug>/<slug>.tex
  → pop/hippyhayzard/<slug>.txt        (hook phrase / hazard spoken line, if any)
  → pop/hippyhayzard/<slug>.np         (notepat score: hippy theme + hazard variant + markers)
  → pop/hippyhayzard/out/<slug>.mp3    (mix)
```

the `.np` carries the **hoover hook** as primary content (syllable-free pitch events). the hazard section is the same theme in the relative minor — scored as its own marked block, not a separate melody.

## arrangement notation

section markers as comments, so `bin/render.mjs` knows when to switch engine + feel:

```
# build 8 [glock-only, riser, no-kick]
69:- 76:- 73:- 69:- ...

# hippy 16 [4-floor, reverse-bass, hoover, glock-oct, swing16]
69 76 73 69 71 74 76 ...

# hazard 8 [half-time, minor, skrill-siren, glock-out, swing8]
54:- 50:- 45:- 49:- ...

# hippy 16 [full]
...
```

bracket flags tell the renderer which voices/feel to wire per section. unflagged sections inherit their block-type default.

## the one new voice

this lane needed a **hoover** — the Roland Alpha Juno-2 "What The..." patch (Second Phase *Mentasm*, 1991): a detuned saw+pulse stack with PWM, a resonant honk, and the signature pitch **whoop**. it can't be a fan-out of plain `sound.synth()` voices (the whoop + PWM aren't in the AC voice contract), so it hand-rolls per-sample DSP like `skrill` does.

three new bottom-up voices, all pure-float per-sample DSP (C-portable for an eventual ac-native port):

- `synths/hoover.mjs` — presets `whoop` / `stab` / `hazard` / `pad`
- `synths/zitar.mjs` — synthesized **sitar**: Karplus-Strong + jawari bridge buzz + sympathetic-string (taraf) bank. presets `sitar` / `lead` / `drone` / `dry`. (C-port path: extend `fedac/native/src/audio.c` `generate_harp_sample`.)
- reused: `../dance/synths/skrill.mjs` (FM + swept-formant talking bass — the hazard siren *and* the harmonized counter-line + growl bass), `../dance/synths/sinepower.mjs` (clean sine stack → glock bells / soft pad).

## pipeline

`bin/render.mjs` — node buffer-mix, no Web Audio, every sample synthesized here. modes: `arc` (origin demo), `harmony` (Bach demo), **`song`** (the 1:28 ballad → `out/hippyhayzard.mp3`). inline drum synthesis + per-kick sidechain duck.

`bin/sing.mjs` — the ballad-tuned jeffrey-pvc vocal pipeline (say → whisper align → WORLD pitch to `hippyhayzard.np` → rubberband stretch → intimate vocal-forward mix onto the bed → `out/hippyhayzard-song.mp3`). lyric in `hippyhayzard.txt`, score in `hippyhayzard.np` (word order/count must match).

```
node pop/hippyhayzard/bin/render.mjs --mode song   # → out/hippyhayzard.mp3 (bed)
node pop/hippyhayzard/bin/sing.mjs                  # → out/hippyhayzard-song.mp3 (sung)
```

media (beds, finals, the billable vocal stem) are backed to the assets system, not git — see [../ASSETS.md](../ASSETS.md).

## tracks

- **hippyhayzard** — the eponymous track. earthbound/mother sorrow-ballad, 152 BPM half-time, A major / F♯ minor, 16-bar Bach chorale, 1:28 with a break. status: first sung cut rendered 2026-05-19 (jeffrey-pvc on the Bach bed, `loudnorm`-mastered — true-peak resolved). open refinement: bar-locking the sung stem to the bed's section grid (the break/refrain alignment). see [STUDY.md](STUDY.md) for the genre study + the blend law.

---

*maintained by @jeffrey*
