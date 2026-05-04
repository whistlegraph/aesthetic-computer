# big pictures

audio-only hip hop / trap versions of jeffrey's AC visions. one vision per track. ~1:30 each.

## format spec

- **length**: 90 seconds, ±10s
- **structure**: hook (4 bars) → verse (16 bars) → hook → verse (16 bars) → hook → outro
- **tempo**: ~140 BPM, 4/4
- **bed**: trap — 808 sub, triplet hats, sparse snare on 3, room for the vocal
- **vocal**: rapped, not sung. emo-rap honesty (see `../VOICE.md`)
- **output**: single mp3 per track in `out/<slug>.mp3`. no video.

## source → track

each track corresponds to one paper or one vision from the platter. the lyric is the compression of that paper into the form a song can carry. the hook is the vision in one line.

```
papers/arxiv-<slug>/<slug>.tex
  → pop/big-pictures/<slug>.txt           (plain lyrics)
  → pop/big-pictures/<slug>.np            (notepat score: NOTE:syllable per syllable)
  → pop/big-pictures/out/<slug>.mp3       (mix)
```

the `.np` (notepat) file is the score in the same notation as the folk-songs paper (`papers/arxiv-folk-songs/folk-songs.tex` §3). every syllable carries a pitch — making the lyric playable on notepat in song mode and renderable through `recap/bin/vocal.mjs` (formant synth) or any other pitch-driven voice. the file is its own URL when fed to `notepat.com?song=...`.

## lyric file format

plain text. no metadata header. blocks separated by blank lines, labeled in lowercase:

```
hook
<4 lines>

verse 1
<16 lines>

hook

verse 2
<16 lines>

hook

outro
<2-4 lines>
```

## pipeline (planned)

`recap/bin/big-pictures.mjs` — mirrors the recap cli pattern. cached per step so reruns cost nothing.

```
read paper
  → draft lyrics (jeffrey-pvc voice + emo-rap overlay)
    → write notepat score (.np)            — every syllable carries pitch (visual / kidlisp future)
      → AC-native trap bed (recap/bin/trap.mjs)
        → vocal stem: /api/say with jeffrey-pvc (provider:"jeffrey", voice:"neutral:0")
          → WhisperX forced alignment      — per-word timestamps
            → snap drift to bar grid       — ±200ms tolerance, 16th-note quantization
              → vocal-post per-word edits  — pitch / elongate / effect / harmonize, aggression-tunable
                → mix (bed + vocal)
                  → mp3
```

## vocal source

big-pictures uses **jeffrey-pvc via ElevenLabs** (the same voice the 24h recap pipeline already uses). hooks and verses both. it's literally jeffrey's cloned voice and is already wired up through `/api/say`.

an AC-native formant-synth vocal was attempted and dropped on 2026-05-03 — it produced melodic tones but not voice; getting real vocoder/talkbox character would need glottal pulse + F4/F5 + pitch jitter + consonants, a full research lane. `recap/bin/vocal.mjs` remains in the repo as experimental research; may resurface as a *melodic instrument* layer (formant-shaped lead in the bed) rather than a vocal.

**bottom-up posture preserved at the composition layer.** the bed is composed bar-by-bar from AC instruments (`trap.mjs` over `percussion.mjs`); the score is hand-written in `.np` notation. ElevenLabs is the *performance* on top of that composition.

## tracks

none yet. first candidate: `plork` (laptop orchestras, planetary scale).
