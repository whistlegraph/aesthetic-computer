# Chorus vocal arrangement

The chorus starts singing immediately. Each machine has one vocal part at a
time: lead, bass, humming, or a rhythmic answer. Neo is green, Blueberry violet,
and Frisbee pink, taken from their machine profiles.

[Bob Stoloff’s Berklee lesson](https://college.berklee.edu/bt/213/lesson.html)
introduces consonant attacks, contrasting vowels, triplets, and conversational
scat exchanges. [Antonio García’s teaching guide](https://www.garciamusic.com/educator/articles/pedagogical.scat.html)
distinguishes flowing “doo,” accented “bah/dah,” and clipped endings. Here those
ideas become short, original swung figures with two-to-one subdivisions:
Neo’s “doo la,” Frisbee’s repeated “la la la,” and Blueberry’s
lower “dum bum dum.” These are composed responses, not improvised bebop solos.

[Lydia Salnikova’s backing-vocal guide](https://blog.airgigs.com/2017/02/the-anatomy-of-a-background-vocal-arrangement-part-2-bgvs/)
describes sustained vowel beds and contrasting textures around a lead. This
arrangement alternates quiet hums with moving answers, swaps backing roles when
the lead changes, and gives Frisbee’s entrance four beats alone. “I run warm”
keeps its original lead melody, now with moving thirds and a bass harmony.
Three more solo phrases have synchronized la-la/dum parts. The family lyric
retains three-part harmony, followed by a scat tag and a shared D-major hum.

The preview uses nine quiet sine notes at three structural points. The exported
`chorus-acappella.wav` removes those instruments completely.
Backing gains and the sine timbre are preview settings; live playback has not
been validated against this mix.

The chorus cast is explicitly pinned to Noelle (Enhanced), Tom (Enhanced), and
Zoe (Premium). Blueberry is the bass, mostly in the second octave, with one
A1. These voices were verified through AVSpeechSynthesisVoice on the render
host; a missing requested voice now fails instead of falling back to a default.
The older machine-profile registers still describe the earlier auditions.

Wannadash references: `pop/cult/bin/sing.py` for protected consonants,
formant-preserving vowel holds, slow spectral shimmer, and brief log-frequency
transitions; `pop/cult/bin/cut-release.sh` and `pop/MASTERING.md` for gentle
RMS compression, measured static gain, and oversampled limiting. This chorus
adds 0.8 Hz spectral movement (two source frames), 15 ms pitch smoothing, a
short dark room return, modest stereo placement, and a native-rate 24-bit
master aimed at −12 LUFS. There is no second dynamic loudness-normalization pass.

From this directory:

```sh
node bin/compose.mjs --only=chorus
node bin/check-chorus-arrangement.mjs
node bin/hear.mjs scores/trio-iii-chorus.mbscore --tag chorus-smooth --keep /Users/jas/Shelf/macneopolitan-chorus-smooth/sung --no-spoken --env SINGER_HOLD_MS=0,SINGER_GAP_MS=20,SINGER_SUSTAIN_DB=5,SINGER_SHIMMER_FRAMES=2,SINGER_LEGATO_MS=15
/Users/jas/aesthetic-computer/pop/.venv/bin/python bin/preview.py scores/trio-iii-chorus.mbscore --sung /Users/jas/Shelf/macneopolitan-chorus-smooth/sung --manifest hear/chorus-smooth.json --quiet --audit --out /Users/jas/Shelf/macneopolitan-chorus-smooth/chorus-dry.mp4
/Users/jas/aesthetic-computer/pop/.venv/bin/python bin/finish-chorus.py /Users/jas/Shelf/macneopolitan-chorus-smooth/chorus-dry
```

`measured-frequency.json` reports actual processed-stem F0 and simultaneous
interval errors at 10 ms intervals; its plot overlays those measurements on
the actual final master spectrum. Stems are measured before the shared master
limiter, not inferred by running a monophonic pitch detector on the mixed trio.
The report records the master’s SHA256 and measured loudness. Pitch checks do
not establish perfect diction or perceived smoothness; speech recognition still
mishears some words, including “warm” as “home.”
