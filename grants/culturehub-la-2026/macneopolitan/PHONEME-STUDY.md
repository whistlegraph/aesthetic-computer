# Phoneme study

A separate 40-second composition at 96 BPM, followed by a one-second tail.
The previous lyrical chorus is preserved. Only vocal syllables are scored:
la, loo, doo, dum, bum, ooh, ah, and hmm. There are no instruments or spoken
lines. Apple’s speech engine interprets these spellings; this is not an IPA
or articulatory synthesizer.

Neo uses Noelle (Enhanced), Blueberry uses Tom (Enhanced) for bass, and Frisbee
uses Zoe (Premium). Their green, violet, and pink accents are unchanged.

- 0–10 s: moving la/loo motif over bass pulses and a held upper voice.
- 10–20 s: short upper-voice exchanges with space between them.
- 20–30 s: open-vowel chords, with the upper voices exchanging notes.
- 30–40 s: shared la figures converge into a D-major hum.

Each machine has one vocal part at a time. The rendering keeps the previous
vowel shimmer, short pitch transitions, gentle compression, and short room
return. The video displays the sounding syllable under each machine.

From this directory:

```sh
node bin/compose-phonemes.mjs
node bin/hear.mjs scores/trio-chorus-phonemes.mbscore --tag phoneme-study --keep /Users/jas/Shelf/macneopolitan-phonemes/sung --no-spoken --env SINGER_HOLD_MS=0,SINGER_GAP_MS=20,SINGER_SUSTAIN_DB=5,SINGER_SHIMMER_FRAMES=2,SINGER_LEGATO_MS=15
/Users/jas/aesthetic-computer/pop/.venv/bin/python bin/preview.py scores/trio-chorus-phonemes.mbscore --sung /Users/jas/Shelf/macneopolitan-phonemes/sung --manifest hear/phoneme-study.json --quiet --audit --out /Users/jas/Shelf/macneopolitan-phonemes/phonemes-dry.mp4
/Users/jas/aesthetic-computer/pop/.venv/bin/python bin/finish-chorus.py /Users/jas/Shelf/macneopolitan-phonemes/phonemes-dry --name phonemes --focus-label 'Interlocking vocal syllables'
```

The audit checks the rendered audio, including simultaneous vocal frequencies.
Word-recognition accuracy is not meaningful for this composition.
