# Score for Pop

## Mill Mission

`pop` is the research home for music that comes out of Aesthetic Computer — songs, instrumentals, and the writing about them. The papers platter pushes AC's thinking out as text. `pop` does the same job in the form of tracks: short, finished pieces of music that can leave the building and be heard.

The mill is not a label. It is a research lane. Every track here exists because it was the most honest way to compress an idea — a feature, a vision, a paper, a moment in the project. If a thread can survive being written as a song, it was real. If it can survive being compressed to ninety seconds, it was essential.

## What This Is

Pop tracks are one output of the AC research platter. They share source material with `papers/` — the same threads, the same readings, the same code — but render as audio. The platter feeds both. Some threads become papers, some become tracks, some become both.

## Posture

**bottom-up + compositional.** tracks here are composed from AC's own instruments — the notepat sample bank, sinebells, chord, beat — the same primitives the recap waltz bed already uses. no suno-style end-to-end song generation; that's product-in, top-down, and not compositional. AI vocal (ElevenLabs) is the one exception, since vocal is performance on top of the composition, not the compositional substrate.

## Process

```
platter (raw material: notes, code, conversations, papers)
  → thread (a vision worth singing)
    → draft lyrics (in jeffrey-pvc voice + per-genre voice)
      → vocal + beat (per-lane pipeline)
        → mix (~1:30 mp3, audio-only)
```

Audio-only by default. No video, no chrome. If a track later becomes a video lane, that's a recap-side concern, not a `pop` concern.

Shipped + in-flight singles (DistroKid status, masters, covers, videos) are tracked in **[RELEASES.md](RELEASES.md)**.

Media files (final mixes, beds, raw billable vocal stems) are **not in git** — code is the source of truth, media is backed up to the assets system per-track. See **[ASSETS.md](ASSETS.md)**.

## Swimlanes

### 1. big pictures (`big-pictures/`)

Hip hop / trap dance versions of jeffrey's AC visions. Roughly **1:30 per track**. Lyrics rapped over a 4/4 trap bed (808s, triplet hats), one track per "big picture" — a single AC vision pulled from the papers platter (laptop orchestras, kidlisp, native OS, identity, latency, etc).

Voice posture: emo-rap honesty. Conviction quiet but absolute. No flexing, no industry posture. Internal rhyme over end rhyme. The vision is the hook.

See [`big-pictures/README.md`](big-pictures/README.md) for the format spec.

### 2. voice (`voice/`)

The jeffrey harness for **Pink Trombone** — a tiny, runnable, anatomically grounded jeffrey voice fitted to jeffrey-pvc on a minimal phoneme corpus, with PT parameter bounds derived from the [jeffrey-platter](../papers/jeffrey-platter/) photographs. Research lane, not a track lane: the deliverable is a C/WASM-runnable synth + a paper, not an mp3.

See [`voice/README.md`](voice/README.md) for the pipeline. Status: scaffolded 2026-05-04.

### 3. dance (`dance/`)

Audio-only dance tracks built bottom-up from AC instruments. One AC vision per track, compressed to a 4-bar **melodic theme** over a four-on-the-floor bed — not a rap hook. Roughly **1:30 per track**.

Opens on **trance** as the first sub-format (138 BPM, minor key, supersaw lead, sidechained pad + bass). The lane lives or dies on the breakdown → build → drop arc, not vibe.

Voice posture: jeffrey-pvc as **one-shot hook phrase** in the drop + spoken word in the breakdown. Not rapped, not fully sung. Instrumental tracks are also valid.

Prerequisite: a **supersaw** voice (5–7 detuned saws in unison) doesn't exist in AC yet and is the gating instrument for this lane.

See [`dance/README.md`](dance/README.md) for the format spec and [`dance/STUDY.md`](dance/STUDY.md) for the genre study + arrangement law.

### 4. chillwave (`chillwave/`)

Audio-only chillwave / ambient tracks built bottom-up from AC instruments. One AC vision (or mood) per track, slow + spacious, compressed to a melody-over-noise-bed rather than a hook or theme. Roughly **1:30–3:00 per track** — longer than the other lanes because the form lives on slow swell.

Voice posture: instrumental by default. jeffrey-pvc as whispered fragments only when present — never sung, never rapped. Reverb-soaked spoken word.

Bed: pink-noise ocean (LFO LP, ~8s wave period) + slow white-noise filter sweeps (~22s cycle, opens fully on the swell) + sparse descending sine-blip bubbles + sinepower pad chord + sinebells melody from the score.

See [`chillwave/README.md`](chillwave/README.md) for the format spec.

### 5. jungle (`jungle/`)

Audio-only sunlit jungle built bottom-up from AC instruments — chopped
breakbeats, deep dub sub, reggae skank, dub sirens. ~165 BPM, felt
half-time. These are **beats for fía to rap over**: the lane ships the
instrumental, fía is the live MC toasting in spanish on top. Her voice
is a real human performance — not jeffrey-pvc, not AI — so it sits
fully inside the bottom-up posture with no exception needed.

Opens on **sunbreak** (golden / daytime, not dark-club) with three
variants: `jungleton` (jungletón — dembow tresillo woven in), `raggasol`
(classic ragga jungle), `rodando` (liquid rollers).

**The hybrid break (decided @jeffrey, 2026-05-16).** Jungle is
classically *built on a sample* — the Amen break. That collides with the
no-top-down-sampling posture. The lane's resolution is a **hybrid**: the
groove is the AC percussion kit (`percussion.mjs`) chopped into
Amen-style syncopation, plus a *synthesized* bitcrush/flange snare-roll
break-stab at phrase ends as the genre nod. No Winstons sample, no loop.
Not an exception to the posture — its honest answer. Whether AC-played
jungle can carry the form is the research question this lane exists to
test.

See [`jungle/README.md`](jungle/README.md) for the format spec.

### 6. hippyhayzard (`hippyhayzard/`)

Audio-only **happy hardcore × nightcore** that the form lets turn into an **earthbound/mother sorrow-ballad** — the name *is* the spec: **hippy** (bright major euphoria) colliding with **hazard** (a minor rave-siren switch). ~152 BPM, half-time ballad feel, a 16-bar hand-voiced Bach chorale (borrowed iv, deceptive cadence), 1:28 with a drop-out **break**.

Three new bottom-up voices were built for it (same module shape as the dance synths, pure-float for an eventual ac-native C port): `skrill` (Skrillex FM + swept-formant talking bass — `dance/synths/`), `hoover` (Alpha-Juno "Mentasm" with the pitch *whoop*), `zitar` (sitar = Karplus-Strong + jawari buzz + sympathetic-string bank).

Voice posture: jeffrey-pvc as a **sung lead** (not rapped, not one-shot) — the lyric *is* the vision, tender and plain. `bin/sing.mjs` is the ballad-tuned vocal pipeline (say → align → WORLD pitch → stretch → intimate mix onto the bed).

See [`hippyhayzard/README.md`](hippyhayzard/README.md) for the format spec and [`hippyhayzard/STUDY.md`](hippyhayzard/STUDY.md) for the genre study + the blend law. Status: scaffolded 2026-05-19; first sung 1:28 cut rendered, vocal↔section bar-lock is the open refinement.

### 7. hellsine (`hellsine/`)

A **concept track** under one strict constraint: *every voice is a sine
wave — no noise, no saw, no square, no samples.* The thesis: a distorted
sine **is** the gabber kick, so "all-sine + hardcore" is hardcore's
literal DNA. The track carries a full **John Williams melodic structure**
(a hand-written heroic leitmotif, stated → developed → transposed →
triumphantly restated) and is built to work as **epic study music**:
continuous, immersive, no dead air under a film arc. ~1:45, 182 BPM, D
minor with a climax key-lift. The strictest possible reading of the
bottom-up posture — one waveform, a whole genre, a whole compositional
tradition.

Voice posture: instrumental. The "voices" are additive-sine brass /
strings, sine-FM stabs, a saturated-sine hoover, and the saturated-sine
kick. No vocal.

See [`hellsine/README.md`](hellsine/README.md) for the all-sine law +
the voice table + the form. Status: scaffolded 2026-05-19.

### 8. (open)

More lanes will land here as they prove themselves. Candidates: kidlisp-as-instrument tracks, AC-native ensemble cuts, voice-memo-grade demo lane. None of them have earned a swimlane yet — they need a real track first.

## Sample sources (commercial-safe)

Tracks ship to DistroKid/Spotify, so any sourced audio MUST be CC0 /
public-domain or project-owned. Approved sources:

- **Freesound API** — approved source; filter `license:"Creative Commons 0"`
  (CC0) only. Credentials are **NOT** committed — they live in the
  private vault: `aesthetic-computer-vault/freesound/credentials.json`
  (Aesthetic Computer account; `api_key` = search/preview token,
  `client_id`/secret = OAuth2 for full-quality downloads).
- **archive.org** — CC0 / `publicdomain/zero` items only (verify each
  item's `licenseurl`; "license: none" ≠ public domain).
- **Project-owned** — the AC zoo bank (`fedac/native/samples/zoo/`) and
  any AC field recordings.

Per-track sourced SFX are logged with provenance + license in that
track's gitignored `out/` dir (e.g. `pop/dance/out/.sfx-credits.txt` —
`trancepenta` uses CC0 archive.org horse gallop + neigh).

## References

Third-party lyrics (emo rap reference corpus, etc) live in the vault. They are not committed to this repo. See [`references/README.md`](references/README.md).

---

*maintained by @jeffrey*
