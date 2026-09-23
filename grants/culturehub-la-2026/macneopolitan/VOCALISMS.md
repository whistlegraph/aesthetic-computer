# Vocalisms

What the trio can actually articulate. Menu Band's singer locks one pitch per
note, so every ornament has to be spelled out as notes: a trill is two notes
traded fast, a slide is a staircase. This file is the measurement of where
that stops working — the shortest note that still comes out as a pitch, and
which syllables the speech engine says instead of spelling.

The kit that uses these numbers is `bin/vocalisms.mjs`; the piece that proves
them is `bin/compose-vocalise.mjs` → `scores/trio-vocalise.mbscore`.

## How

Everything below is rendered offline through Menu Band's own singer
(`bin/hear.mjs` → `slab/menuband/.build/release/singrender`) at 90 bpm, into
`~/Shelf/macneopolitan-songs/vocalisms/`. Pitch is read back with
`pyworld.harvest` at 5 ms and compared note by note against the score, over
the middle 60 % of each note so the transitions do not count. "Heard" is
whisper `small.en` on the same WAV.

Two knobs — `singGapMs`, `singLegatoMs` — are **not** forwarded by
`bin/hear.mjs` into the singer's payload, and the singer takes no `SINGER_*`
environment variables except `SINGER_SPEECH_CACHE`. Those two rows were
rendered by calling `singrender` directly with the keys in `--kv`.

## Ornaments

At 90 bpm, one beat is 667 ms. "Minimum" is the shortest note in the figure
at which the measured pitch still lands within a quarter tone.

| ornament | minimum beats/note | measured | verdict |
|---|---|---|---|
| `trill` whole tone | 0.25 (167 ms) | swing 1.41–1.85 of 2.00 st; 12/16 notes hold a pitch | reads as two notes, syllables slur |
| `trill` whole tone | 0.5 (333 ms) | err 0.02 st, swing 2.00/2.00, 8/8 | clean — use this |
| `trill` whole tone | 0.125 (83 ms) | swing 1.56–1.85, only 24/32 notes voiced, heard as "*weird noises*" | a texture, not a trill |
| `trill` half step | 0.5 | swing 0.76 of 1.00 st | loses a quarter of an already small interval — prefer whole tone |
| `mordent` | 0.125 bite | err 0.11 st, max 0.28 | reads: one short note between long ones is not a chain |
| `turn` | 1 beat total (0.125 a step) | err 0.11 st | the most vocal shape here; reads down to one beat |
| `shake` (wide, slow) | 0.5 | err 0.02–0.08 st, swing 2.00/2.00 | survives anything; the safe trill |
| `roll` (arpeggio) | 0.33 | err 0.05 st | fine |
| `gliss` (staircase) | 0.25 a step | err 0.04 st over 8 steps | fine — each tread is a new pitch, not an alternation |
| `pulse` (Reich) | 0.25 | err 0.08 st | fine |
| any stop-framed token (`bip`) | 0.5 | at 0.25 one bip read 49.0 where 57 was written — 8 st out | the consonants eat the vowel; give stops half a beat |

The rule underneath: a **chain** of very short notes smears, a **single**
very short note does not. A 83 ms neighbour inside a mordent lands; 32 of
them in a row do not.

## Syllables

Each token was sung three times alone and spoken three times. The
discriminator is how long the engine takes to SAY one repeat: anything over
about 0.35 s is the engine reading out letter names, which puts two syllables
on one note and ruins the figure.

| token | renders as | say/repeat | verdict |
|---|---|---|---|
| `tss` | "T-S-S" | 0.73 s | spelled — do not use |
| `boh` | "B-O-H" | 0.64 s | spelled — do not use |
| `kh` | "K-H" | 0.61 s | spelled — do not use |
| `ch` | "C-H", sung back as "ch" | 0.54 s | marginal — avoid |
| `tk` | "T-K" | 0.51 s | spelled — do not use |
| `ts` | "T-S" | 0.50 s | spelled — do not use |
| `mm` | "M-M" | 0.49 s | spelled — use `hmm` |
| `bm` | "B-M" | 0.46 s | spelled — do not use |
| `pf` | "P-F" | 0.43 s | spelled — do not use |
| `dm` | one dark click, heard back as "DM" | 0.41 s | **the only mouth percussion that works** |
| `doom` | "doom" | 0.31 s | good (bass) |
| `bom` `dum` | "bum" / "dum" | 0.28 s | good (bass) |
| `nee` | "nee" | 0.28 s | good |
| `lee` | "li" | 0.27 s | good |
| `tee` | "tee", heard as the letter T | 0.27 s | good; one sound, ambiguous transcript |
| `dee` | "dee" | 0.26 s | good |
| `la` | "la" | 0.25 s | good |
| `doo` | "doo" | 0.24 s | good |
| `hah` | "ha" | 0.22 s | good (breath) |
| `poo` `bah` `bip` | "poo" / "baa" / "bip" | 0.21–0.22 s | good; stops need 0.5 beats |
| `huh` | "ha" | 0.21 s | good (breath) |
| `loo` | "lu" | 0.21 s | good |
| `dah` | "da" | 0.20 s | good |
| `ah` | "ah" | 0.18 s | good |
| `ooh` | "oo" | 0.17 s | good |
| `hmm` | a hum; no transcript | 0.14 s | good |
| `uh` | "uh" | 0.12 s | good (glottal) |

The mouth-percussion idea mostly fails: eight candidates, one survivor. `dm`
is the tick in the piece. Breath tokens all work, which is the cheaper way to
get a consonant that is not a pitch.

## Vibrato is not a trill

A held A3 (4 beats), `singVibratoHz` × `singVibCents`:

| setting | measured rate | measured depth | verdict |
|---|---|---|---|
| 5 Hz / 8 cents | 4.9 Hz | ±11.5 c | shimmer; no audible pitch movement (the house default) |
| 5 Hz / 30 cents | 4.9 Hz | ±29.3 c | a real singer's vibrato |
| 8 Hz / 60 cents | 7.9 Hz | ±58.5 c | a bleat |
| 8 Hz / 100 cents | 7.9 Hz | ±104 c | 2 st wide, and the note loses its identity |

The knobs track the request exactly. But the shape is a sine centred on the
written note: it never sits on either edge, so even at 100 cents — the same
span as a whole-tone trill — there is no second pitch, only width. A written
trill at 0.5 beats has plateaus and is half the speed; that is what makes it
read as two notes. Use vibrato as colour at 30 cents or less; write trills.

## Legato and gap

A stepwise run, eight notes at 0.5 beats:

| setting | silence in the line | voiced | pitch error | note-to-note transition |
|---|---|---|---|---|
| default (0/0) | 0.6 % | 99.8 % | 0.033 st | 28 ms |
| `singGapMs:20, singLegatoMs:15` | 0.6 % | 99.8 % | 0.028 st | 32 ms |
| `singLegatoMs:60` | 0.6 % | 99.8 % | 0.028 st | **74 ms** |
| `singGapMs:80` | **8.6 %** | 88.7 % | 0.019 st | 30 ms |

The default is already fully legato. The doo-wop setting (20/15) is inside
the noise — it changes nothing on 333 ms notes. `singLegatoMs: 60` is a real
portamento; on a 0.25-beat trill it cut the measured direction changes from
75 to 49 and pulled the bottom note up 1 st, so it rounds a trill off. A gap
of 80 ms detaches the run and slightly *improves* pitch accuracy, because
there is no transition left to smear. For ornaments: gap up to 60, legato
under 20.

## Registers

`Zoe (Premium)` had never been measured. 62 spoken words through
`pyworld.harvest`: median **55.6 MIDI** (203 Hz), p10 52.5, p90 59.4, mean
55.8. So frisbee speaks a fourth BELOW neo's Noelle (59.6) — in the vocalise
"frisbee on top" is tessitura and speed, not absolute pitch. Pushing her up
to sit over Noelle would break the spinging rule that cost blueberry its
voice on Sept 23.

`BANDS` in `bin/vocalisms.mjs` carries all three, and `checkBand()` asserts
that a line's mean is within 2 semitones of its member's speaking pitch with
every note inside the band.

## Open

- Whether a Premium voice tolerates being pushed UP better than down. The
  croak rule was measured going down a fifth; the ceiling is untested.
- `ch` sits in the spelled band by duration but sings back as a ch sound. One
  more token worth an A/B if more percussion is wanted.
- `bin/hear.mjs` does not pass `singGapMs`/`singLegatoMs`/`singSustainDb`/
  `singShimmerFrames` to the singer, so those knobs in a score are inert
  under the offline loop, and the `--env SINGER_GAP_MS=…` form in DOOWOP.md
  and PHONEME-STUDY.md is a no-op. Not my file to fix.
