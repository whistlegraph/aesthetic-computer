# tapes

pictures and programs carried inside an audio stream, decoded live on the way
back out. built to answer one question with numbers instead of guesses: what
actually survives a compact cassette, and how much.

two codecs share one tone grid.

- **`lib/spectro.mjs`** — the picture mode. an image painted into the magnitude
  spectrum, read back with an fft. no phase, no error correction, nothing a
  lossy codec or a tape head can take away. degrades by blurring, never by
  cliff.
- **`lib/data.mjs`** — the program mode. the same grid with one bit per cell.
  this is what a *tape of an AC piece* wants: source plus an input timeline is
  kilobytes, and kilobytes survive channels that pixels do not.

## running it

```bash
node tapes/bin/roundtrip.mjs            # picture → audio → picture, clean
node tapes/bin/degrade.mjs              # every route, + tapes/out/degrade.png
node tapes/bin/sweep.mjs                # fft geometry vs. fidelity
node tapes/bin/datatest.mjs             # bytes through a cassette
```

`roundtrip` and `degrade` take an optional png argument; otherwise they use a
built-in test card (ramps for banding, wedges for detail, flats for noise,
checks for ringing).

## what the geometry costs

the channel has a fixed number of independent time-frequency cells per second
and no arrangement beats it. at 44.1k that ceiling is about 11k magnitude cells
per second, and the first version of this overspent it three times over.

measured, on the test card:

| overlap | alternating rows | alternating columns |
| --- | --- | --- |
| hop = n/4 | 33.6 dB | 7.1 dB |
| hop = n/2 | 45.4 dB | 10.7 dB |
| **hop = n** | **exact** | **exact** |

vertical detail was never the problem. with overlap, an analysis window spans
more than one column, so every vertical edge smears into its neighbour. at
`hop = n` each column owns its own stretch of audio and the round trip is
bit-exact. the whole fix was giving up overlap.

## what survives

picture mode, 256×128, `n=2048`, `spacing=4`, **worst of 3 hiss draws** — see
`tapes/out/degrade.png`:

| route | psnr |
| --- | --- |
| untouched | exact |
| mp3 320 | 53.3 dB |
| clipped +6dB | 40.6 dB |
| mp3 192 · aac 256 | 38.9 dB |
| **cassette, good deck** | **31.4 dB** |
| opus 128 | 29.6 dB |
| mp3 128 | 28.4 dB |
| aac 128 · 44.1→48→44.1 | 24.6 dB |
| opus 64 | 23.2 dB |
| **cassette, walkman** | **20.6 dB** |
| cassette → mp3 192 | 20.5 dB |
| **cassette, worn tape** | **13.5 dB** |

every one of these is still legible as the test card. noise lands in the dark
regions first, because black sits at the bottom of the dB range and that is
where the channel floor is.

## what a cassette carries

program mode, 72 KB payload (a kidlisp piece plus eight minutes of pointer
input at 30 Hz). tone spacing is the dial — tight packs more bits, wide
survives more wobble.

| route | dense 1701 B/s | medium 425 B/s | rugged 213 B/s |
| --- | --- | --- | --- |
| untouched | perfect | perfect | perfect |
| mp3 192 | perfect | perfect | perfect |
| mp3 128 | 2.5e-3 | perfect | perfect |
| opus 64 | lost | 3.9e-2 | 4.1e-4 |
| cassette, good deck | lost | perfect | perfect |
| cassette, walkman | lost | lost | **perfect** |
| cassette → mp3 192 | lost | lost | 1 bad bit in 577,232 |
| cassette, worn tape | lost | lost | lost |

the rugged grade moves 213 B/s through a walkman with zero errors, so one C90
side is ~570 KB, or about an hour of recorded piece. a worn tape (0.7% wow)
breaks every grade and is what error correction is for.

the shape of it: **a tape does not want pixels, it wants the program.** eight
minutes of AC performance is 72 KB, which fits in 5.7 minutes of cassette —
less tape than the performance lasts. the same eight minutes as pictures, at
this codec's ~21 columns/sec, would be a slideshow.

## moving pictures

```bash
node tapes/bin/videotest.mjs [source.mp4]   # → ~/Desktop/tapes
```

the picture codec spends one audio column per image column, which caps you
under 1 fps. `lib/video.mjs` instead treats the tone grid as a pipe: flatten
every frame to a stream of greys and pour it through, so all 632 bins carry
picture instead of the handful one frame row would need.

the ceiling is the gabor limit — about `rate/2` independent cells per second.
here that is **632 tones × 21.5 columns/sec ≈ 13,600 greys/sec**, and

```
fps = 13,600 / (width × height)
```

which means every grade below is the same budget spent differently. it also
means that when `w × h × fps` equals the budget, **the audio runs exactly as
long as the video** — the tape is realtime.

measured on `imab.mp4`, the canonical whistlegraph (900×1080, 19 s):

| grade | frame | fps | route | error | audio |
| --- | --- | --- | --- | --- | --- |
| detail | 56×68 | 3.6 | clean | bit-exact | 1.05× |
| detail | 56×68 | 3.6 | mp3 192 | 35.3 dB | 1.05× |
| motion | 34×40 | 10.0 | clean | bit-exact | 1.03× |
| motion | 34×40 | 10.0 | mp3 192 | 37.5 dB | 1.03× |
| tape | 24×28 | 2.5 | cassette, walkman | 27.4 dB | 1.03× |

the cassette grade costs an order of magnitude: 79 tones instead of 632, so
1,701 greys/sec instead of 13,600. that is the whole tape tax in one number.

a whistlegraph is close to the best case — line art on white is high contrast
and needs few grey levels, which is exactly what this codec is good at. the
drawing survives at 56 px wide; the sung lyric text underneath does not.

live action was expected to be much worse and is not. `yc-2018-final.mp4` at
56×100 keeps a recognisable face with readable expression, and still does at
24×42 through a walkman. faces are smooth and low-frequency, which is what a
magnitude codec preserves. **it is fine text that dies, not faces** — the
subtitles smear away on both clips.

## error correction, and where it does not belong

`lib/rs.mjs` is reed-solomon over GF(256) with interleaving. verified: 300/300
blocks recovered at up to 16 symbol errors, 200/200 declared failure when over
capacity with **zero silent corruption**, and bursts up to 200 consecutive
bytes absorbed by the interleaver. `data.encodeProtected` / `decodeProtected`
wrap it, with a nine-times-repeated majority-voted header, since the header is
what tells you how to decode the blocks and so cannot itself be protected.

two findings worth keeping.

**it does not belong in the picture path.** at walkman grade the channel gives
1,701 cells/sec. spent as analog greys that is 24×28 at 2.5 fps, degrading into
grain. spent as 4-bit pixels plus parity it is about 0.47 fps — five times
worse — and it fails to a blank screen instead of a noisy one. this is the same
trade the historical record shows between analog and digital SSTV.

**it does not rescue a worn tape.** at 0.7% wow the raw error rate is 0.49 — a
coin flip, which is not an error rate but total loss of frequency lock, and no
amount of parity decodes a signal the demodulator never acquired. widening the
pilot search needs a wider guard band; widening the guard pushes the top pilot
to 14.4 kHz, which the tape's own 14 kHz rolloff then deletes. measured: guard
20 loses the header on every route. the band cannot hold both.

so RS currently buys **margin, not capability** — the walkman grade was already
clean without it and is still clean with 1.21× overhead. the worn tape wants a
different mechanism: resample the whole signal to correct speed before
demodulating, rather than tracking per block. not built.

## two things that took a while

**sync must live where the picture lives.** the preamble chirp originally swept
to 15 kHz. a cassette rolls off at 14, so on the dub-then-upload route the
picture arrived fine and sync did not — the decoder landed 125,683 samples off
and returned noise. dropping the sweep to 11 kHz fixed it. a sync tone outside
the surviving band is worse than no sync at all, because it fails silently.

**one run of a noisy test proves nothing.** the first fix for the sync problem
above looked like it worked, and got written up as working. it had been
measured once. the cassette hiss is random per run, and re-measured across
twelve draws that "fix" failed **5 times out of 12** — a coin flip reported as
a solution. the real fix was to whiten only the bands the chirp occupies
(full-band PHAT amplifies noise in bands the reference never used, which is
exactly where a tape and a codec both leave nothing) and to lengthen the sweep
to 0.4 s for the processing gain. that holds 0/12 on every route.

hiss is now seeded (`channel.seed`), and `channel.trials(n, fn)` runs across
draws so a marginal result reads as marginal. `degrade.mjs` reports the worst
of three and flags any route that loses sync.

**a continuous pilot cannot time anything.** the pilots run unbroken through
every column, so their level is identical at every offset and a search for
"where the block starts" is a search through noise. their *frequency* is the
useful part: wherever the pilots landed says how fast the tape is running, and
block length follows from that. speed gives timing; timing does not need
searching.

## known gaps

- the decoder is told the image dimensions. a real one wants an in-band header.
- no error correction anywhere yet. the data mode reports raw BER on purpose —
  FEC is the next layer, not a thing to hide the raw number behind.
- cassette here is a simulation (band limit, wow/flutter, hiss, saturation).
  the number that settles it is a real deck, and that measurement hasn't been
  made.
- realtime decode inside an AC piece needs a raw-sample tap on the mic
  worklet — `lib/microphone.mjs` currently exposes only every 8th sample,
  renormalized per poll, which is not enough to decode from.
