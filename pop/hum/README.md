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

## Commands

```bash
node pop/hum/bin/render.mjs --play          # hear the tune (synth lead)
node pop/bin/rfa.mjs --track hum            # sing it — the wizard walks you
node pop/bin/rfa.mjs --track hum --status   # punch-list: notes sung / missing
node pop/bin/rfa.mjs --track hum --only 6-0 # re-sing one note
```

After the wizard finishes it offers to recompile + play the track back.
A Slab menubar item triggers the same wizard hands-free.
