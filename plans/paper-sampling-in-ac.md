# Paper: Sampling in Aesthetic Computer

## Working Title
**"Every Sound is a Painting: Sampling as Visual-Auditory Practice in Aesthetic Computer"**

## Abstract Sketch

Aesthetic Computer treats audio samples as first-class visual objects by encoding
them as RGB pixel data in shareable "paintings." This paper traces the evolution
of sampling in AC — from the initial microphone capture in `notepat.mjs` through
`stample.mjs`'s pixel-sample encoding to a cross-platform architecture where bare-
metal devices and web browsers share samples through the same painting infrastructure.
We argue that collapsing the boundary between visual and auditory media enables new
creative workflows: a recorded sound becomes an image you can see, share as a short
code, and play back on any AC device. The system extends Attali's concept of
"composition" (where everyone creates) to the sample itself — every user's sound
becomes a painting in a shared library.

---

## Paper Structure

### 1. Introduction: The Sample Problem

- Digital audio workstations treat samples as opaque binary blobs (WAV, AIFF)
- Sharing samples requires file transfer, format negotiation, storage accounts
- Disconnect between visual creative tools and audio creative tools
- AC's premise: what if a sample was just another painting?

### 2. Background and Related Work

- **Attali's "Noise"** (already cited in PLORK paper): recording → repetition →
  composition. AC pushes into composition where everyone records and shares.
- **Laptop orchestra tradition** (PLORK paper context): PLOrk, SLOrk, CLOrk.
  Sample sharing in ensemble contexts.
- **Visual-auditory encoding precedents**: spectrograms as visual representation,
  Jerobeam Fenderson's oscilloscope art, bytebeat/pixel-to-audio experiments
- **Painting as data format**: the tradition of steganography, data art,
  Manfred Mohr's algorithmic marks containing their own instructions

### 3. Sampling in AC: Current Architecture

#### 3.1 notepat.mjs — The Instrument

- 7 waveform types including "sample" mode
- Home key: record from microphone
- End key: arm per-key sample recording (different sample per note)
- Sample bank: `sampleBank[key]` with individual float32 buffers
- Auto-save to `/mnt/ac-sample.raw` on native boot media
- Auto-load on next boot — the instrument remembers its voice

Reference: `fedac/native/pieces/notepat.mjs` lines 16-28, 385-430, 590-600

#### 3.2 stample.mjs — The Sampler as Painting

- `encodeSampleToBitmap(data, width)`: float32 audio → 8-bit RGB pixels
  - 3 samples per pixel (R, G, B channels)
  - -1.0..+1.0 mapped to 0..255
  - 256px wide, height varies with duration
  - 5 seconds @ 48kHz = 240,000 samples = 80,000 pixels = 256×313 PNG ~100KB
- `decodeBitmapToSample(bitmap, meta)`: reverse process
- `loadPaintingAsAudio(source, opts)`: load ANY painting as audio
  - Painting code (`#k3d`) → resolve via API → download PNG → decode → play
  - KidLisp source (`$roz`) → render to bitmap → decode → play
  - System painting → current canvas → decode → play

Reference: `system/public/aesthetic.computer/lib/pixel-sample.mjs`

#### 3.3 The Painting Infrastructure

- Upload: `track-media.mjs` → presigned URL → DO Spaces CDN → MongoDB record
- Short codes: every painting gets a code like `#k3d`
- URL addressable: `aesthetic.computer/painting/#k3d`
- Owned by @handle: `@jeffrey/painting/slug`
- Social: shareable, embeddable, browsable
- Already handles images of any size and format

### 4. The Bridge: Samples as Paintings Across Platforms

#### 4.1 The Problem of Two Worlds

- Native (ac-native): QuickJS, ALSA, direct hardware, no DOM, no Canvas
- Web (browser): Web Audio API, Canvas, full network stack
- Same pieces need to work on both platforms
- Samples recorded on bare metal should be playable in browsers and vice versa

#### 4.2 The Solution: Paint the Sound

- Phase 1: Add pixel-sample encoding to native (pure JS, no DOM needed)
- Phase 2: Native uploads encoded bitmaps as paintings via existing API
- Phase 3: Native downloads paintings by code and decodes to audio
- Phase 4: Unified `samples.mjs` piece on both platforms
- Phase 5: notepat gains cloud sample support on both platforms

#### 4.3 Data Flow

```
Record sound → float32 array → encode as RGB pixels → upload as painting
              → get short code (#abc) → share
                              ↓
Download painting → decode RGB pixels → float32 array → play as audio
```

### 5. Design Philosophy

#### 5.1 Why Paintings, Not WAV Files

1. **Infrastructure reuse**: no new storage backend, API, or auth flow
2. **Visibility**: you can SEE your sample as an image
3. **Compactness**: 8-bit RGB is 4× smaller than float32
4. **Universality**: PNG renders everywhere, WAV needs specialized tools
5. **Social integration**: paintings are already AC's shareable unit
6. **Artistic potential**: the visual appearance of a sample IS its visual identity

#### 5.2 The Encoding as Aesthetic Choice

The RGB encoding is lossy (32-bit float → 8-bit per channel) but this is
a feature, not a bug. The quantization introduces a subtle lo-fi character
that is consistent and reversible-within-tolerance. Like vinyl's warmth or
cassette's hiss, the encoding IS the medium.

The visual appearance of an encoded sample reveals its structure:
- Sine waves produce smooth color gradients
- Percussion creates sharp color boundaries
- Noise produces visual static
- Speech shows rhythmic color patterns

A user browsing a gallery of sample-paintings can develop visual intuition
for how sounds look — and vice versa.

#### 5.3 Every User's Sound Becomes a Painting

Following Attali's trajectory: in the "composition" era, the distinction
between producer and consumer dissolves. In AC, every user who records a
sample creates a painting. That painting enters the shared library with a
short code. Other users can load it, play it, modify it, re-record it.

The sample is not a file in a folder. It is a painting in a gallery.

### 6. Implementation Status and Future Work

#### 6.1 What Exists Today
- stample.mjs (web): full pixel-sample pipeline
- notepat.mjs (native): microphone recording, per-key sample bank
- samples.mjs (native): local sample library browser
- pixel-sample.mjs: encode/decode library
- Painting upload/download/code infrastructure

#### 6.2 What's In Progress
- Native pixel-sample encoding (pure JS)
- Native painting upload from device
- Cross-platform samples.mjs
- Cloud sample sync (local-first, cloud optional)

#### 6.3 Future Directions
- **Collaborative sampling**: multiple devices record simultaneously, merge into one painting
- **Live sample streaming**: one device records, others play in real time via UDP
- **Generative samples**: KidLisp programs that generate sample-paintings algorithmically
- **Sample DNA**: track lineage when samples are re-recorded/modified (like git for sound)
- **Physical prints**: print sample-paintings on paper, scan them back as audio

### 7. Conclusion

By treating samples as paintings, AC removes an artificial boundary between
visual and auditory creative practice. A sound has a color. A painting has
a voice. The infrastructure is the same. The creative act is unified.

---

## References to Include

- Attali, Jacques. "Noise: The Political Economy of Music." 1985.
- Blackwell et al. "Live Coding: A User's Manual." 2022.
- Turchet, Luca. "Elk Audio OS." 2021.
- Existing AC papers: arxiv-ac, arxiv-plork, arxiv-notepat
- Collins, Nick. "Introduction to Computer Music." (on laptop orchestras)
- Roads, Curtis. "Microsound." (on granular/sample-based synthesis)
- Wishart, Trevor. "On Sonic Art." (on the materiality of sound)

## Placement

This paper fits in the platter as a companion to the notepat paper,
expanding from "the instrument" to "the sample as creative object."
It connects to the PLORK paper's discussion of distributed music-making
and to the main AC paper's architecture sections.

Directory: `papers/arxiv-sampling/`
