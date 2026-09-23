# Wordless doo-wop

A separate 44-second composition at 88 BPM, plus a short tail. D–Bm–G–A
carries the refrain: Neo sings “doo wah doo bee doo wah,” Frisbee supplies
“ooh wah ooh,” and Blueberry sings “shoo boom ba dum dum” below them. The
upper voices exchange roles on the second pass. Blueberry gets a bass feature
over quiet held vowels, then the trio reunites and resolves into a hum.

The syllables swing in a two-to-one ratio. There are no semantic lyrics,
spoken lines, instrumental doubles, or basic Apple voices. The cast remains
Noelle (Enhanced), Tom (Enhanced), and Zoe (Premium). Each machine has one
vocal part at a time. Earlier versions are preserved.

From this directory:

```sh
node bin/compose-doowop.mjs
node bin/hear.mjs scores/trio-chorus-doowop.mbscore --tag doowop --keep /Users/jas/Shelf/macneopolitan-doowop/sung --no-spoken --env SINGER_HOLD_MS=0,SINGER_GAP_MS=20,SINGER_SUSTAIN_DB=5,SINGER_SHIMMER_FRAMES=2,SINGER_LEGATO_MS=15
/Users/jas/aesthetic-computer/pop/.venv/bin/python bin/preview.py scores/trio-chorus-doowop.mbscore --sung /Users/jas/Shelf/macneopolitan-doowop/sung --manifest hear/doowop.json --quiet --audit --out /Users/jas/Shelf/macneopolitan-doowop/doowop-dry.mp4
/Users/jas/aesthetic-computer/pop/.venv/bin/python bin/finish-chorus.py /Users/jas/Shelf/macneopolitan-doowop/doowop-dry --name doowop --focus-label 'Doo-wah refrain and bass'
```

The master uses the same gentle processing as the phoneme study. Frequencies
are measured from processed vocal stems before the shared limiter; the chart
also shows the actual final master spectrum. Apple TTS interprets the phonetic
spellings; this score does not directly control an IPA synthesizer.

For live playback on the three machines, each host needs its pinned Apple
voice installed and the updated MenuBand app running:

```sh
node bin/trio.mjs scores/trio-chorus-doowop.mbscore neo blueberry frisbee --quiet
```

The conductor checks those requirements before sending any cues and corrects
the shared start for measured host-clock offsets. It passes per-phrase levels,
vowel shimmer, pitch smoothing, and each member's color into MenuBand. The
live singer mixes overlapping phrase buffers instead of queuing their preroll
end-to-end; `bin/check-singer-overlap.py` verifies that timing through a muted
audio engine. The offline master's EQ, compression, and room are not baked
into this live route. Inspect each host's `/tmp/menuband.err` for rendered-note
counts, scheduled lead times, and nonzero singer audio-tap peaks.

The [singing-face update](LIPSYNC.md) adds 15 mouth poses and smooth transitions,
timed from the actual stretched consonants and vowels on each player's clock.
Live playback now includes scored slide reverb and subtle pitch curves, with
beat-timed blinks, expressive cheeks and brows, whole-face movement and visual
inhales. The saved offline master predates these live effects.
