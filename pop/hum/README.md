# hum

A tiny singable melody — the testbed for the **RFA** (request-for-audio)
voice-take workflow. Sixteen notes, one octave (D3–D4), D natural minor.

```
deep in the hum / a note grows slow / hold to the tone / now let it go
```

## The loop

```
voice-takes/manifest.json   the melody + words (the score of record)
        │
   rfa.mjs ──▶ wizard: per note, plays the pitch + shows the word,
        │       records you singing it, keep / redo / skip
        ▼
voice-takes/<id>.wav        one take per sung note (id = "<bar>-<beat>")
        │
  render.mjs ──▶ synth lead + chord pad, with every recorded take
        │         mixed in on top (lead ducks under your voice)
        ▼
out/hum.wav · out/hum.mp3   always playable, "real" one note at a time
```

## The club cut

`bin/club.mjs` evolves the same melody into a ~2:20 club mix at 128 BPM
(intro → break → build → drop ×2 → outro). All pitched voices are
harmonized sine stacks (no saws), every event gets a raised-cosine
envelope (no clicks), the master is peak-normalized with no clipping
stage, and the mix is spatialized Special-Sign style: per-voice
equal-power pan with drifting azimuth, a slow whole-room rotation, and
a mono-safe L=−R wet return. Voice takes land in the breakdowns and the
second drop, same as the sound test. Output: `out/hum-club.{wav,mp3}`.

## Commands

```bash
node pop/hum/bin/render.mjs --play          # hear the tune (synth lead)
node pop/hum/bin/club.mjs --play            # the club cut (--bpm to retempo)
node pop/bin/rfa.mjs --track hum            # sing it — the wizard walks you
node pop/bin/rfa.mjs --track hum --status   # punch-list: notes sung / missing
node pop/bin/rfa.mjs --track hum --only 6-0 # re-sing one note
```

After the wizard finishes it offers to recompile + play the track back.
A Slab menubar item triggers the same wizard hands-free.
