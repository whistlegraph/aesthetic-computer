# Sampled instrument candidates

Open-licensed acoustic libraries that could join the `` `1 `` AC grand piano
(Salamander V3, `piano/`) as instruments in AC OS and Menu Band. Sourced
2026-10-13 with Exa; every license was read at the source, because shipping
samples inside an app is redistribution.

Audition them: `python3 fedac/native/scripts/audition/audition-sample-libraries.py --out <dir>`
renders each set the way `audio.c` plays the piano (nearest anchor, step
`2^(st/12)`, linear interpolation, 70 ms damper), then writes `<dir>/index.html`.
The pipeline holds the exact file picks per instrument.

## Shippable

| Library | License | What it has | Link |
|---|---|---|---|
| VCSL (Versilian Community Sample Library) | CC0 | Steinway B + Kawai grands, Yamaha + Knight uprights, 5 harpsichords, pipe organ, vibraphone, marimba, xylophone, glockenspiel, tubular bells, harps, kalimbas/mbiras, balafon, wine glasses, recorders, ocarina, harmonica, tenor sax, timpani, hand percussion. 4,231 WAVs, mostly 2–3 velocity layers | github.com/sgossner/VCSL |
| VSCO 2 CE | CC0 | Solo + section strings, flute, oboe, clarinet, bassoon, horn, trumpet, trombone, tuba | github.com/sgossner/VSCO-2-CE |
| A Sampled Celesta | CC0 samples, MIT tools | Mustel celesta C3–F8, stereo, 48 kHz, release + pedal noises. Ships as SF2 | gitlab.com/echoparallax/a-sampled-celesta |
| FreePats upright-piano-KW | CC0 | Kawai upright in a living room, 2 layers, A0–C8 | github.com/freepats/upright-piano-KW |
| FreePats Spanish classical guitar | CC0 | The only CC0 nylon guitar found. Noisy 2008 recording. SF2 | freepats.zenvoid.org/Guitar/acoustic-guitar.html |
| Karoryfer (all free sets now CC0) | CC0 | Meatbass (1958 double bass, arco + pizz), Vengeful Cello/Violin, War Tuba, Weresax | github.com/sfzinstruments |
| Univ. of Iowa MIS | "Freely available for any projects, without restrictions." No formal license | Full orchestra, pp/mf/ff, anechoic (dry), post-2012 at 24/96 stereo | theremin.music.uiowa.edu/MIS.html |

Iowa has no named license. Email them before shipping it.

## Ruled out

| Library | Why |
|---|---|
| Philharmonia | The samples can't be sold "as is" or as a sampler instrument |
| Pianobook | Personal license; redistribution forbidden |
| jRhodes3 | Samples are CC-BY-NC |
| Midnight Wurli | GPL-3 |
| Musical Artifacts Wurlitzer | Unknown source and license |
| Virtual Playing Orchestra, Terkelsen Marimba | No license stated; VCSL/VSCO cover the same ground |

Electric piano is the gap. There's no clean Rhodes or Wurlitzer yet.

## Findings from the audition

- **Octave naming.** Much of VCSL and VSCO 2 CE names middle C "C3", so the
  files sound an octave above their names. Affected: Kawai grand, Yamaha upright,
  harpsichord, mallets, kalimba, balafon, VSCO flute + clarinet, tenor sax,
  ocarina, wine glasses. Correct as named: Steinway B, harp, Meatbass, VSCO solo
  violin, Iowa. Checked with YIN and spectral peaks; recorded as `shift`.
- **Pipe organ is ambiguous.** One anchor measures at its name and another an
  octave up (a strong 4′ rank?). Settle it by ear.
- **Iowa files carry room tone before each attack.** Trim below −50 dB at prep
  time, or the notes land late.
- **Engine fit.** `audio.c` and `MenuBandACPianoVoice` play each sample through
  to its end. Struck and plucked sets (pianos, harpsichord, mallets, celesta, harp,
  kalimba, guitar, pizz bass) work today. Bowed and blown sets (strings, winds,
  sax, organ, glasses) need loop points to hold a note.
- **Prep.** `prep-piano-samples.sh` hardcodes Salamander's names. A generic
  `prep-bank` should take a pick pattern + `shift` like the audition pipeline,
  trim leading silence, and write `<midi>.raw` per anchor.
