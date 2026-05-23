# marimba platter

Whistlegraph-coded pop tracks built bottom-up from the modal-additive
marimba synth at `synths/marimba.mjs`. All share the same DNA: a
syllabic chant the mallet voice hums but never speaks, layered voices
named per section, a pop-mastered mp3 + a colored-pencil + gouache
cover brief.

Score doc is the `.np`. Cover prompt is the `.illy.txt`. Audio is in
`out/<name>.mp3`. Render any track:

```bash
node pop/marimba/bin/render-<name>.mjs            # default path
node pop/marimba/bin/render-<name>.mjs --out ~/x.mp3
```

## the platter

Sorted by mood arc — daytime → playful → energy → night.

| # | track | key | meter | BPM | mp3 | chant |
|---|-------|-----|-------|-----|-----|-------|
| 01 | **marimbaba**  | F maj  | 3/4  | 56  | 1:24 | hush / twin-kle / wow / ba-ba-bap / sleep-now |
| 02 | **flutterbap** | C maj  | 4/4  | 124 | 1:02 | butterfly-cosplayer / mommy-wow / slinky-dog / lately-when-i-fly |
| 03 | **kookabap**   | A maj  | 4/4  | 138 | 0:42 | kook-a-bur-ra / ha-HA-ha / lift-the-dawn |
| 04 | **dingaba**    | E♭ maj | 4/4  | 110 | 0:53 | ding-DONG ding / hel-LO / it's-your-FRIEND |
| 05 | **lollybap**   | C maj  | 6/8  | 124 | 0:32 | lol-LI-pop / sweet-SWEET / one-for-YOU |
| 06 | **dewabap**    | G maj  | 4/4  | 78  | 1:02 | drop-DROP / shine-SHINE / wake-the-mor-ning |
| 07 | **buzaboo**    | A min  | 4/4  | 132 | 0:45 | buzz-BUZZ / flow-er-FLOW-er / land-on-it |
| 08 | **wibblybop**  | F maj  | 4/4  | 116 | 0:51 | wib-bly WOB-bly / jig-gly JIG-gly / shake-the-DISH |
| 09 | **mommyboo**   | F maj  | 3/4  | 72  | 1:03 | mom-MY / where-IS-she / hi-mom-MY / wow / I-SEE-her |
| 10 | **alleyhop**   | A maj  | 4/4  | 128 | 0:45 | al-LEY OOP / one-TWO-THREE / SCORE / yes-yes-YES |
| 11 | **slinkybap**  | G maj  | 4/4  | 96  | 1:00 | slin-ky DOG / down-the-STAIRS / climb-back-UP / boi-oi-OING |
| 12 | **piggabap**   | F min  | 4/4  | 144 | 0:40 | oink-OINK / snort-SNORT / mud-MUD-mud / chase-the-CAT |
| 13 | **goosabap**   | G maj  | 4/4  | 92  | 1:14 | honk-HONK / fly-a-WAY / V-for-VIC-tory / south-WE-go |
| 14 | **trumpaba**   | B♭ maj | 4/4  | 120 | 0:48 | ta-DA / ta-DA-DA / make-WAY / hear-YE |
| 15 | **kikbouba**   | A min  | 5/4  | 84  | 1:12 | ki-KI / bou-BA / sharp-or-ROUND / which-ARE-YOU |
| 16 | **bonggabap**  | C min  | 4/4  | 88  | 1:01 | bong-BONG / gong-GONG / hour-OF / mid-NIGHT |
| 17 | **mooncalf**   | E min  | 3/4  | 60  | 1:13 | moon-CALF / look-UP / where-DO-you-go / fol-low-the-LIGHT |
| 18 | **gloomboo**   | D min  | 3/4  | 64  | 1:07 | where-DID-it-GO / when-WILL-it-come-BACK / wait-some-MORE |

Plus the disco remix and the three unreleased marimbaba variants:

| - | track | meter | mp3 | notes |
|---|-------|-------|-----|-------|
| - | **marimbabapp** | 6/8 disco shuffle, 115 BPM dotted | 6:07 | marimbaba grows up and goes to the club; F→G key change at bar 264 |
| - | **doziba**      | F maj 3/4 56 | 3:55 | marimbaba's deep-sleep variant (kalimba focus) |
| - | **bronzaba**    | F maj 3/4 56 | 3:24 | marimbaba's bronze-gamelan variant |
| - | **tinkaboo**    | F maj 3/4 56 | 3:00 | marimbaba's wind-up music-box variant |

## what's shared across all tracks

- single synth: `synths/marimba.mjs` (`mixEventMarimba`)
- preset palette: rosewood, kelon, bass, staccato, roll, xylophone,
  vibraphone, vibraphone_off, glockenspiel, gamelan, woodblock, kalimba
- output: 48 kHz f32le → ffmpeg → libmp3lame VBR Q2
- mp3 path: `pop/marimba/out/<name>.mp3`
- struct map: `pop/marimba/out/<name>.struct.json` (sections by name +
  startSec/endSec, for any future visualizer)
- cover brief style: colored-pencil + gouache on warm cream paper, no
  text/wordmarks, peer composition (no centered-hero), chartreuse
  MacBook Neo lid-back with whistlegraph-butterfly paper scrap,
  diegetic light only.

## next moves

- Cover gens: `bin/gen-illy.mjs` per track (one-shot, $-budgeted).
- Storyline visualizer (insta-story 9:16): see `bin/preview-score.mjs`
  + `bin/gen-sections.mjs` — works on any track with a `.struct.json`.
- DistroKid: follow the marimbaba path documented in
  [../RELEASES.md](../RELEASES.md). Per-track master chain is already
  embedded in each `render-<name>.mjs`.
