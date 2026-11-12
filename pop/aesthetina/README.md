# aesthetina

an ocarina made of a keyboard. dance / techno, Aesthetic Dot Computer.

## the law of this lane

every sound is one of two materials, and the two always arrive together:

- **switch**: a mechanical keyswitch. A tactile click, the bottom-out thock
  of the cap on the plate, a faint spring ping, and the softer upstroke on
  release. Each letter has its own case modes (hashed from the letter), so
  no two keys sound alike. The spacebar is the big stabilised key: lower
  thock, stabiliser rattle.
- **breath**: a vessel flute. A near-pure sine (Helmholtz resonator) with a
  trace of 2nd/3rd harmonic, breath noise banded around the pitch, a chiff
  at the onset, a small scoop up into the note and delayed vibrato.

**the melody is typed.** Lead notes come from notepat's QWERTY map
(`system/public/aesthetic.computer/disks/notepat.mjs` → `NOTE_TO_KEYBOARD_KEY`),
untransposed: type the word into notepat and you hear the same pitches. Every
lead note is a keystroke, with its click.

| part      | made of |
|-----------|---------|
| kick      | spacebar thock + a hard huff into a big vessel (sine 140→45 Hz) |
| hats      | clicky switch downstrokes on 16ths; upstrokes + spring ring for opens |
| clap      | four or five keys mashed inside 12 ms |
| fills     | typing rolls |
| bass      | the bass vessel, a low ocarina on the offbeat |
| pad       | two held ocarinas, A3 + E4 |
| hook      | "aesthetina" typed: A4 E4 D♯4 C♯5 C5 E4 C♯5 D5 B5 A4 |
| counter   | "aesthetic computer" typed, spacebar between the words |

Pedal on A; the words supply the colour (D♯, C, C♯ all in one word).

## render

One engine, many arrangements. `arrangements/<name>.{mjs,json}` places the parts
(meta: bars, bpm, swing, room); `voices/<name>.py` is jeffrey's score for it.

```
pop/.venv/bin/python pop/aesthetina/bin/voice.py --arr=radio   # → out/vocal-radio{,-lyric}.wav
node pop/aesthetina/bin/render.mjs --arr=radio                 # → out/aesthetina-radio.{wav,mp3} + out/stems-radio/
node pop/aesthetina/bin/master.mjs --arr=radio                 # → out/aesthetina-radio-master.{flac,mp3}
```

| arrangement | what it is |
|---|---|
| `radio` | **the single**: 64 bars, 2:03, bedroom room; typo intro, typed tunes, sung lyrics |
| `country` | the 80-bar square-dance version it grew from |
| `phasing` · `palindrome` · `callresponse` · `generative` | composition studies |

Deterministic, with fixed seeds. The DTMF hits need `samples/c_robo_dtmf/SOURCE.md`'s fetch.

## voice

Everything jeffrey sings comes from his ElevenLabs PVC (eleven_v4) through WORLD:

- **syllables**: the permutative phoneme library (`pop/bin/phoneme-library.mjs`; vault
  `voice/jeffrey-phonemes/`), toots, choirs, arpeggios, inflection banks (`--style`), lyric
  syllables (`--words`).
- **phrases**: `bin/phrases.mjs` speaks whole lyric lines in three cadences with character
  timestamps; `sing_phrase` stretches them onto the hook's notes (unvoiced runs keep their
  spoken length, voiced runs share one ratio, every span fills its slot, one WORLD pass,
  f0 replaced). The lyrics are their own stem, dry and centred in the master.

`bin/master.mjs` places each stem through the measured KEMAR head, glues with acdsp's
`danceMasterBus`, and finishes with `pop/notespatial/bin/master.sh` (house target in
`pop/MASTERING.md`).

**Prutti** sings in the radio cut (his Pruttivox IVC, `--voice prutti`). Per the Klokkentales
release gate, that cut is a draft until Prutti approves his clone and the performance.
