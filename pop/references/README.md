# references

third-party reference material — emo rap lyrics, trap song breakdowns, beat references — does **not live in this repo**.

it lives in the vault (jas.life, closed-source). this folder exists only as a pointer so the path is real and pipeline code can resolve a vault-side mount at runtime if needed.

## why not committed

- copyright. the corpus is third-party lyrics used as in-context style examples for lyric generation. that's a fair-use-ish posture for a private notebook, not for a public github repo
- voice hygiene. the AC repo should reflect AC's own writing. reference material is scaffolding, not output

## how it's used

the lyric generator pulls a small curated set (~10–20 tracks) from the vault as in-context examples — cadence, internal rhyme, imagery — and never includes them in any committed artifact.

## vault path

mounted at runtime. exact path TBD when the lyric generator lands.
