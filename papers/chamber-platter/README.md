# Chamber — platter

A reading platter for **arranging tonal chamber music in a room of separate
speakers**: how ten-to-fifteen-minute continuous forms are timed, how
antiphonal and spatial ensembles phrase placement, how process music builds
and releases, and which sine-partial stacks read as instruments under a
linear attack-and-decay runtime. A sub-platter within the
[papers platter](../SCORE.md), parallel to [rhythm-platter](../rhythm-platter/).

> The platter exists so that a `pop/` or `fedac/native` score generator
> resolves "where does the climax go", "how long may a rotation take before
> it blurs", or "what makes a sine sound plucked" to a cited fact and a
> numbered rule, instead of to taste on the day.

It has a specific consumer. *Note(s)pat(ial) Native*
(`fedac/native/tools/compose-notespatial-native.mjs`) is the first score
written against it: five laptops in a ring and one held at the center,
CultureHub LA, 2026-09-24. `pop/` compositions that place voices in space
(`pop/nullabye/c/spatial-sineabye.c`, `pop/bracelet/`, `pop/lib/necklace-space.mjs`)
are the next.

## Posture

**Index and original restatement only.** No source text is reproduced here.
Digest entries are written in AC's own words and exist to specify code. Every
fact carries a locator into `sources-NN.json`. Program notes and publisher
pages are cited as such; where a duration or proportion comes from one
recording, the digest says so under a Debts heading.

## Digest

| Entry | Question it answers |
| --- | --- |
| [01 · tonal forms](digest/01-tonal-forms.md) | How 10–16 minute continuous tonal works are proportioned, cadenced and joined; where the climax and the stillness fall |
| [02 · spatial antiphony](digest/02-spatial-antiphony.md) | How placement is phrased: cori spezzati, Brant, Stockhausen, Xenakis, PLOrk; rotation rates that read as motion rather than blur |
| [03 · process and envelopes](digest/03-process-and-envelopes.md) | Phrase hierarchies in process music; sine-stack recipes (Risset bell, Karplus-Strong law, marimba ratios) for plucked, struck, pad and tap voices |
| [04 · pop strategies](digest/04-pop-strategies.md) | What AC's own `pop/` tracks already do: forms, builds, the warbling spatial techniques and their parameters |

Sources: `sources-01.json` … `sources-04.json`, flat `{id, title, author, year, url, kind}`
arrays, one per digest.

The long-form studies the digests were distilled from, with their
recommendations for the CultureHub piece, are in
`grants/culturehub-la-2026/chamber-studies/`.
