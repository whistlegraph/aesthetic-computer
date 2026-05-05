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

## Swimlanes

### 1. big pictures (`big-pictures/`)

Hip hop / trap dance versions of jeffrey's AC visions. Roughly **1:30 per track**. Lyrics rapped over a 4/4 trap bed (808s, triplet hats), one track per "big picture" — a single AC vision pulled from the papers platter (laptop orchestras, kidlisp, native OS, identity, latency, etc).

Voice posture: emo-rap honesty. Conviction quiet but absolute. No flexing, no industry posture. Internal rhyme over end rhyme. The vision is the hook.

See [`big-pictures/README.md`](big-pictures/README.md) for the format spec.

### 2. voice (`voice/`)

The jeffrey harness for **Pink Trombone** — a tiny, runnable, anatomically grounded jeffrey voice fitted to jeffrey-pvc on a minimal phoneme corpus, with PT parameter bounds derived from the [jeffrey-platter](../papers/jeffrey-platter/) photographs. Research lane, not a track lane: the deliverable is a C/WASM-runnable synth + a paper, not an mp3.

See [`voice/README.md`](voice/README.md) for the pipeline. Status: scaffolded 2026-05-04.

### 3. (open)

More lanes will land here as they prove themselves. Candidates: kidlisp-as-instrument tracks, AC-native ensemble cuts, voice-memo-grade demo lane. None of them have earned a swimlane yet — they need a real track first.

## References

Third-party lyrics (emo rap reference corpus, etc) live in the vault. They are not committed to this repo. See [`references/README.md`](references/README.md).

---

*maintained by @jeffrey*
