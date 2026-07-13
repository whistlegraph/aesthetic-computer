# live

Real-time speech-to-singing for DJing. A talking head becomes a vocalist:
their own words, your melody, locked to your beat — with a faucet you turn on
and leave running.

## What it does

Point it at a recording of someone talking. It transcribes them to word-level
timestamps, decomposes their voice with WORLD, and then — live, on a beat —
replaces their pitch with your key and stretches their vowels onto your grid.
Consonants stay crisp because they come from the untouched original recording.
The result sings in their voice, in your track.

    node bin/ingest.mjs <youtube-url> <slug>   # once, per source
    make && ./singer <slug>/vocal.wav <slug>/words.json 4.0 <f0-floor>

    start · stop
    morph 0..1     spoken → sung (drives pitch AND rhythm together)
    snap  0..1     how hard her contour is pulled onto the scale
    mode  snap|melody
    depth 0..1     in-your-face → buried in the track
    level · bpm · floor

Bounce a stem instead of playing live:

    ./singer <slug>/vocal.wav <slug>/words.json 4.0 115 --out track.wav 48

## The two ideas that make it work

**Absolute f0 replacement, not pitch shifting.** WORLD decomposes the voice
into `(f0, spectral envelope, aperiodicity)`. We throw the f0 away and write
our own. The envelope is untouched, so it is unmistakably still their voice —
it just sings now, in whatever key you are playing. Shifting pitch instead of
replacing it leaves their spoken prosody riding on top, fighting the melody.

**Vowel-aware time warping, inside WORLD.** We stretch the `(f0, sp, ap)` frame
arrays *before* synthesis, not the audio after it — so formants survive by
construction, with no phase-vocoder smear. Only the vowel nucleus stretches;
consonants stay at natural rate. The vowel onset lands on the grid and the
consonant leads in *ahead* of the beat, which is what singers actually do.
Anchoring consonants to the beat is what makes chopped vocals sound stiff.

## Ask → sound: 564ms

Analysis is **chunked**. We analyze a ~4s front chunk (~400ms), start playing,
and stream the rest of the corpus in behind the playhead. Analyzing the whole
clip up front took 34 seconds; this takes half a second.

The speed is *not* from being written in C. libworld is the same C either way —
the win is scheduling. C buys a realtime-safe audio thread (no GC, no GIL) and
lets Menu Band and `fedac/native` link the core directly.

## Layout

- `singer.h` / `singer.c` — **the shared core.** Pure DSP. No audio device, no
  file I/O. Menu Band (AVAudioEngine) and `fedac/native` (ALSA) link this and
  supply their own device — the same pattern as `gm_synth.c`, which fedac owns
  and Menu Band symlinks. `make libsinger.a` builds it standalone.
- `main.c` — the standalone driver: miniaudio device + stdin CLI. **This is the
  only file a host replaces.**
- `world/` — vendored WORLD (Morise, BSD). C-linkable.
- `bin/ingest.mjs` — url → yt-dlp → whisper word timestamps → librosa onset
  refinement → `words.json`.
- `bin/floor.py` — measures a speaker's f0 distribution. **Run it per source.**

## The f0 floor matters more than you'd think

Frames below the floor are read as unvoiced and never sing; a floor set too
*low* widens CheapTrick's window and smears formants into a ring. It has to
track the speaker. Measured so far: Puett 117 Hz, Bourgeois 176, Nesbit 204 —
each wanted a different floor. Run `bin/floor.py` on a new source before
assuming a default.

## Corpora are not committed

`live/<slug>/` holds audio and transcripts of other people's talks. Those stay
out of the repo — reproduce them from a URL with `bin/ingest.mjs`. Anything you
perform or publish with this is a rights question worth answering before the
gig, not after.

## Known rough edges

- Vowel detection is a heuristic (voiced + energetic), not a phoneme aligner.
  A forced aligner would slice on true phoneme edges.
- The rhythm patterns in `singer.c` are a small hardcoded table.
- Beat tracking is not wired in yet: BPM is set, not detected. The tempo/phase
  feed from `ac-m4l/` (`daw:tempo`, `daw:phase`) is the obvious source.
