# Bass — platter

A reading platter for **the low end of a dance track**: how the Jamaican dub
bass line was played, recorded and mixed and what the drop-out and return do
to it; how dub techno and UK dubstep carried a filtered sub into 4/4 and
half-time 140; how the wobble was built, why it dates, and what replaced it
(FM growls, formant and wavetable movement, resampled neuro passes, riddim's
gated squares); and the sub/mid law, distortion staging, LFO rates in beat
divisions, sidechain constants and phone translation that a bass *generator*
has to get right. It also covers the screwed 808 of witch house and cloud
rap, because the track it feeds is a witch-house / big-room hybrid. A
sub-platter within the [papers platter](../SCORE.md), parallel to
[chamber-platter](../chamber-platter/) and [rhythm-platter](../rhythm-platter/).

> The platter exists so that `pop/dance/synths/wobble.mjs`,
> `pop/dance/synths/skrill.mjs` and the `climbalift` bed in
> `pop/notespatial/` resolve "how low is the sub", "what rate does the wub
> run at 140", "where does the drive go" and "why does it vanish on a phone"
> to a cited fact and a numbered rule, instead of to taste on the day.

## Posture

**Index and original restatement only.** No source text is reproduced here.
Digest entries are written in AC's own words and exist to specify code. Every
fact carries a key into `sources.json`. Magazine tutorials are cited as
tutorials, not as the artist's method; forum threads are cited as consensus
and flagged; where a figure is a rule of thumb with no source, the digest says
so in the rule itself and again under Debts. No number is invented.

## Shelf

| Shelf | What is on it |
| --- | --- |
| dub | Tubby, Perry, Scientist, Pablo; the MCI desk, the Altec high-pass, spring and tape; the one drop; the drop-out |
| sub | sine subs, 30/40 Hz cuts, mono-below-100, the vinyl and PA reasons, the missing fundamental |
| dub-techno | Basic Channel, Rhythm & Sound, Deepchord; band-passed chords, dotted-eighth echo, the desk as instrument |
| dubstep | FWD>>, DMZ, Mala, Skream, Loefah; 140 half-time; the sine sub |
| wub | Reese, wobble, growl, formant, wavetable, notch/comb, neuro resampling, OTT, riddim |
| 808 | TR-808 as bass, trap 808 tuning and glide, chopped and screwed, SALEM, cloud rap and Drain Gang |
| translation | sidechain constants, distortion staging, mono/stereo split, the 250–700 Hz phone band |

## Digest

| Entry | Question it answers |
| --- | --- |
| [01 · dub bass](digest/01-dub-bass.md) | How the dub bass line was recorded and mixed, how it sits against the one drop, how dub techno and dubstep carried it into electronic music, and where the mono-sine-with-harmonics rule comes from; twelve rules and a tools block for a sub/bass-layer generator |
| [02 · wub bass](digest/02-wub-bass.md) | How wobble, growl, neuro and riddim mid layers are made, why the resonant sweep dates, LFO rates at 128–140, sub/mid separation, drive order, sidechain, phone translation, and the screwed 808; twelve rules and a tools block for a mid/808 generator |

## Use

Read the Unit paragraph first: it converts the digest's tempos and note
divisions to ms and Hz so a rule can be applied without arithmetic. Facts are
for arguing with; Rules are for implementing. A generator should take its
constants (register per layer, split frequency, LFO divisions, gate shape,
sidechain attack/release, drive order) from the Rules and cite the rule number
in a comment. When a rule says "rule of thumb", the constant is tunable and
should be exposed as an option, not hard-coded. Debts list what is
reconstructed rather than sourced; a Debt is a place to go looking, not a
license to guess.

Sources: `sources.json`, an object with a `sources` array of
`{key, title, author, year, url, kind, note, checked}`; an unchecked entry
may not be cited in a paper.
