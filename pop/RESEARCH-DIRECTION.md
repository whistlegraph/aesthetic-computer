# Research Direction · AC Pop

**Last updated**: 2026-05-03
**Author**: @jeffrey

---

## Posture

**bottom-up + compositional.** suno / udio are product-in, top-down, overused, and not compositional — you give them a prompt and get a finished song you didn't compose. that is not the AC posture. tracks here are built from AC's own instruments (the same notepat / chord / sinebells / beat primitives the recap waltz bed already uses), bar by bar. the composition is the artifact.

## Vocal Architecture (decided 2026-05-03)

big-pictures vocal = **jeffrey-pvc via ElevenLabs**. same `provider: "jeffrey", voice: "neutral:0"` already used by the 24h recap pipeline (`/api/say` proxy in `system/netlify/functions/`, called from `recap/bin/tts.mjs`). hooks and verses both. it's literally jeffrey's cloned voice — the most authentic option and already wired up.

**AC-native vocal was attempted and dropped on 2026-05-03.** built a 3-formant synth (`recap/bin/vocal.mjs`) and a `.np` score reader, rendered the plork hook over the trap bed. result: it sounded like tones, not voice. real vocoder/talkbox character would need glottal pulse, F4/F5, pitch jitter, breath, consonant articulation — a full research lane. not blocking track 1 on it. `vocal.mjs` stays in the repo as experimental research; may find use as a *melodic instrument* layer (formant-shaped lead) rather than a vocal.

if/when AC-native vocal resumes, the right starting point is **Pink Trombone** (Neil Thapen, 2017) — a browser-based physical model of the vocal tract that produces articulate speech-like sounds in pure JS. open-source, copy-pasteable. ports cleanly to a node renderer that consumes the `.np` score directly. see `reference_pink_trombone.md` in memory.

after the ElevenLabs stem returns, we run **WhisperX forced alignment** on it (the same dependency the recap pipeline already uses for subtitle timing — see `feedback_recap_subtitle_timing_drift.md`) to get per-word timestamps. those word boundaries:

1. **confirm** every word lands on or near a 16th-note grid line of the trap bed
2. drive the **bar-snap** pass that nudges drift to the nearest beat (per `feedback_recap_musical_snapping.md`, ~200ms tolerance)
3. become the score for any visual layer (subtitle overlays, notepat-style scrolling lyric track, AC piece tied to the track)
4. become the **edit unit** for vocal post-production (next section)

the **`.np` score** for each track stays useful: it pairs the lyrics with a pitch contour, which is the right input to a notepat-style scrolling lyric visual layer and to any future kidlisp-driven music piece.

## Vocal Post-Production (per-word)

once a vocal track has word boundaries (WhisperX for ElevenLabs verses, or directly from the `.np` score for `vocal.mjs` hooks), each word is an editable segment. the post-prod stage applies per-word edits driven by a recipe file (`pop/big-pictures/<slug>.edit.json`):

- **pitch** — shift up/down semitones, autotune to the scale of the bed (C minor pentatonic for trap)
- **elongate** — time-stretch to fit a target duration (rubberband / atempo, formant-preserving)
- **effect** — reverb / delay / distortion / vocoder / formant-shift / saturation, per-word
- **harmonize** — duplicate stem, pitch-shift each copy (+3rd, +5th, octave), mix back
- **realign** — move the word's start to match a target beat / 16th-note slot

the **aggression** is a per-edit (or per-track) knob: `gentle` (≤50ms nudge), `firm` (≤200ms), `aggressive` (snap to nearest beat regardless of distance), `off` (leave timestamps as-is). aggression applies independently to each operation — you can pitch-correct gently and realign aggressively, or vice versa.

post-prod is **generic across vocal sources** — works the same on ElevenLabs verse stems and `vocal.mjs` hook output. the `.edit.json` recipe is paired with the lyrics/score.

implementation hooks: ffmpeg filtergraph (`asetrate`, `atempo`, `aecho`, `aphaser`, `chorus`), rubberband for pitch+time independence, sox for finer effects.

## Current Goals

1. **Land the first big-pictures track** — one AC vision, ninety seconds, mixed and listenable end-to-end. proves the pipeline before scaling
2. **Route lyrics through jeffrey-pvc TTS** — feed plork lyrics into `/api/say` with the existing voice config (`provider: "jeffrey", voice: "neutral:0"`); cache the stem; mix over the trap bed
3. **Build a curated reference corpus** — ~10–20 emo rap tracks' lyrics in the vault, used as in-context style examples for the lyric generator (not training data)
4. **Write the lyric generator** — paper / vision → 16-bar verse + 4-line hook in jeffrey-pvc voice with emo-rap overlay
5. **WhisperX align + bar-snap** — run forced alignment on the jeffrey-pvc stem; emit per-word timestamps; nudge drift to bar grid
6. **Build the per-word post-prod stage** — `pop/bin/vocal-post.mjs`: takes a vocal track + word timestamps + `.edit.json` recipe → applies pitch / elongate / effect / harmonize / realign, with per-edit aggression
7. **Tune the AC-native trap bed** — extend `recap/bin/trap.mjs` (now landed) with: dedicated 808 sub voice, swing toggle, fill on bar 16, optional pan stage

## Open Questions

- copyright posture for reference lyrics: in-context style examples only, vault-only, never committed. enough?
- elevenlabs cadence control: how tightly can we pin the vocal to the bar grid? rap needs the stem to land on the beat, not float. WhisperX + snap should fix most drift; test before committing the lane
- jeffrey-pvc rap performance: the 24h-recap voice is calm + descriptive. does it deliver rap cadence at all, or do we need a separate voice variant ("jeffrey-pvc:rap")? test on the plork hook first
- "big pictures" the show vs. the format: is each track its own episode, or are they collected into albums? defer until track 3

## First Track Plan — `plork`

source: `papers/arxiv-plork/plork.tex`. core hook line already in the paper: *the music is real, the logistics just got cheaper.*

```
1. lyrics      → pop/big-pictures/plork.txt    [done — v1 draft]
2. score       → pop/big-pictures/plork.np     [done — hook + verse 1 + outro; visual layer / future]
3. trap bed    → recap/out/trap.mp3            [done — 16 bars, 140 BPM]
4. vocal stem  → /api/say with jeffrey-pvc     [next: feed plork.txt as narration body]
5. align       → WhisperX → per-word timestamps
6. snap        → nudge words to nearest bar-grid 16th-note
7. post-prod   → vocal-post.mjs --edit plork.edit.json
                 (pitch / elongate / effect / harmonize / realign, aggression-tunable)
8. mix         → pop/big-pictures/out/plork.mp3 (~1:30)
```

If the jeffrey-pvc TTS doesn't carry rap cadence — try a different ElevenLabs voice variant or work the lyric line-breaks / punctuation to coax better delivery before resorting to a different provider.
