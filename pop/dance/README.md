# dance

audio-only dance tracks built bottom-up from AC instruments. one AC vision per track, compressed to a 4-bar melodic theme over a four-on-the-floor bed.

opens on **trance** as the first sub-format. future sub-formats (house, techno, dnb, hyperpop) land alongside if and when they earn a finished track.

## format spec — trance

- **length**: ~1:30 (radio edit). canonical trance club edits are 6–8 minutes; we compress without sacrificing the breakdown → build → drop arc
- **tempo**: 138 BPM, 4/4 (range 132–142)
- **key**: minor — A minor / E minor / F# minor by default
- **structure**: intro (8) → break 1 (16) → build 1 (8) → drop 1 (16) → break 2 (8) → build 2 (4) → drop 2 (16) → outro (4) bars
- **bed**: kick (4-on-the-floor) + off-beat sub bass + closed hat + supersaw lead + saw pad, sidechained
- **vocal**: jeffrey-pvc — one-shot hook phrase in the drop + spoken word in break 1. not rapped, not fully sung
- **output**: single mp3 per track in `out/<slug>.mp3`. no video

## source → track

each track corresponds to one AC vision from the platter, same as the big-pictures lane — but compressed to a *melodic theme* rather than a rap hook.

```
papers/arxiv-<slug>/<slug>.tex
  → pop/dance/<slug>.txt           (hook phrase + breakdown spoken word, if any)
  → pop/dance/<slug>.np            (notepat score: theme + arrangement markers)
  → pop/dance/out/<slug>.mp3       (mix)
```

the `.np` file carries the **supersaw theme** as the primary content — every note in the theme is a syllable-free pitch event. when vocal lines exist, they get their own lines in the file.

## arrangement notation

the `.np` file gets section markers as comments, so the bed builder knows when to drop the kick, open the filter, drop the snare roll:

```
# intro 8
g g g g g g g g

# break 1 16
A4:- E5:- D5:- C5:- ...

# build 1 8 [riser, snare-roll, kick-out-last-bar]
...

# drop 1 16 [full-mix]
A4 E5 D5 C5 ...

# break 2 8
...
```

bracket flags tell `recap/bin/trance.mjs` which engine pieces to wire in for each section. unflagged sections inherit the default for their block type.

## pipeline (planned)

`recap/bin/trance.mjs` — mirrors the `trap.mjs` cli pattern. cached per step.

```
read paper
  → distill one AC vision → 4-bar theme + (optional) hook phrase
    → write .np score with arrangement markers
      → render trance bed (recap/bin/trance.mjs)
        → vocal stem (if any): /api/say with jeffrey-pvc
          → WhisperX align (only if vocal exists)
            → sidechain compress pad/bass under the kick
              → mix (bed + vocal)
                → mp3
```

## the one new voice

trance needs a **supersaw** — 5–7 detuned sawtooth voices in unison. it doesn't exist in AC yet. landing this voice is the prerequisite for any track in this lane. likely path: extend `recap/bin/`'s synth helpers (or add `pop/dance/synths/supersaw.mjs`) with a parameterized `{ voices, detuneCents, mix }` saw stack.

## vocal source

trance vocal isn't rap. jeffrey-pvc's calm/descriptive register fits the breakdown spoken word and the one-shot hook drop. WhisperX alignment is only used when there *is* a vocal — instrumental trance tracks skip the alignment step entirely.

## tracks

none yet. see [`STUDY.md`](STUDY.md) for the genre study + arrangement law. first candidate paper TBD.
