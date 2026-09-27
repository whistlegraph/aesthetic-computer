# Singing faces

MenuBand drives a 15-pose cartoon mouth chart from each phrase's actual
audio-player clock. The singing core exports the consonant onset, vowel
sustain, and release boundaries it used when stretching the voice. The face
uses those boundaries instead of treating each scored note as one mouth pose.

Live faces now use a Metal triangle pipeline adapted from Oskiewar Native's
`apple/oskiewar/Sources/MetalSceneView.swift` approach. The face rig builds
one triangle batch, with clipped mouth details, alpha-blended cheeks and 4×
MSAA. Three GPU vertex buffers prevent overwriting a frame still in flight.
Metal presents directly on the display callback instead of waiting for an
AppKit dirty-region redraw. The native display link requests the screen's
maximum refresh rate; older Macs use a bounded CVDisplayLink queue.

The outline's 10 Hz random shape changes now interpolate continuously. Motion
and mouth timing remain driven by elapsed time and the audio clock. The log
reports presentation callbacks per second, CPU/GPU frame time, callback-gap
p95 and skipped submissions; callback timing is not a physical scanout test.
AppKit remains the offline drawing reference and the fallback without Metal.

Profiling the live trio found two additional main-thread costs: repeated
CoreAudio device-latency queries from the mouth/breath clocks, and synchronous
redraws of the covered menu-bar keyboard. Latency is now captured once at each
phrase start; the player's sample clock still advances every frame. Keyboard
state continues updating, while its covered icon redraw waits until the
full-screen face closes. The muted overlap/timing check passes with this cache.
The hidden keyboard image is released during the performance. Caption letters
also use a bounded bitmap cache, like Oskiewar's cached glyphs: CoreGraphics
rasterizes each letter/style once, and Core Animation moves the resulting
GPU image during sway and syllable accents.

The chart distinguishes rest, M/B/P closure, closed humming, AH, EH, EE, OH,
OO/W, F/V, L, TH, S/Z, SH/CH/J, R, and T/D/N. Jaw, width, rounding, lip seal,
teeth, tongue, and lip pressure interpolate separately at display refresh.
Consonant poses get a short readable exposure; shapes anticipate audio by
40 ms, vowels hold, and releases ease into rest. A per-phrase envelope sampled
at 120 Hz adds jaw expression while allowing quiet backing singers to enunciate.
Humming stays closed regardless of its volume.

This is a procedural articulation model, not a neural network. Sound identity
comes from the known wordless syllables; arbitrary prose still uses spelling
approximations. Timing comes from the measured audio warp. This does not
claim phoneme recognition or facial motion capture.

References and model research:

- [Animation Mentor: animating dialogue](https://www.animationmentor.com/blog/some-tips-for-animating-dialogue/)
  informs anticipation, separate facial controls, and phrase-level performance.
- [Toon Boom's mouth chart](https://learn.toonboom.com/modules/lip-sync-animation/topic/activity-2-mouth-shapes-overview)
  describes the conventional sound-to-mouth drawing approach.
- [Rhubarb](https://github.com/DanielSWolf/rhubarb-lip-sync) provides phonetic
  recognition and timed 2D mouth cues. Its v1.14.0 macOS binary could not run
  on this seat (`bad CPU type`); no recognition-quality comparison was made.
- [Audio2Face-3D](https://github.com/NVIDIA/Audio2Face-3D-SDK) and the
  [Swift/MLX port](https://github.com/soniqo/speech-swift) offer learned facial
  coefficients. Neither is integrated or benchmarked here. The current rig
  uses the already-known utterance and synthesis timing without another model
  download or inference stage on the live rendering path.

Validation:

```sh
python3 bin/check-lipsync.py
python3 bin/check-singer-overlap.py
python3 bin/check-metal-face.py
python3 bin/check-caption-cache.py
```

The first checks distinct poses, anticipation, B/M closures, humming, tongue
exposure, held vowels, and silence, then renders the actual AppKit mouth chart
for each machine. The second runs the production audio engine with muted
output, checks phrase overlap, reads actual player clocks, checks slot reuse,
and verifies stop clears the mouth's playback state.

The Metal check renders all 15 poses through the production GPU pipeline to
`~/Shelf/macneopolitan-doowop/metal/`, then opens a twelve-second native window
to measure presentation. Neo's 60 Hz display sustained approximately 60 fps
with less than 1 ms CPU and approximately 1.5 ms GPU work per frame in the
standalone check. Fleet performance during synthesis is measured separately.

`singrender` now exports `mouthCues` alongside each WAV. A seven-syllable
audit (`doo wah bee la fa boom hmm`) and its 60 fps AppKit preview are in
`~/Shelf/macneopolitan-doowop/lipsync/`. `bin/preview-lipsync.py` rebuilds that
preview from the audit's exact WAV and exposure sheet.

The doo-wop score also drives small whole-face translations, rotation and zoom,
faster eye tracking, beat-timed blinks, lifted eyebrows and deeper cheek color
with vocal effort. Before each phrase, a short visual inhale lifts the face
and opens the resting mouth; no recorded breath is added to the audio.

MenuBand's existing slide now bends sung voices as well as speech. Its leftward
axis adds the existing hall reverb, while vertical movement changes pitch at
unchanged playback speed. The singer feeds the pitch processor early by its
reported latency and subtracts that delay from the mouth clock.

From this directory, control the running trio without restarting its voices:

```sh
node bin/slide.mjs neo blueberry frisbee --space .28 --pitch .5
node bin/slide.mjs neo blueberry frisbee --space 0 --pitch 0
```

`space` ranges from 0 to 1; `pitch` is semitones, limited to ±24. A code command
or physical slide takes over from score automation. The doo-wop voice entries
contain `performance: {expression, keys: [{beat, space, pitch}]}` curves with
smooth interpolation, a drier bass, small shared pitch scoops, and a neutral
return after the ending. These curves run on each physical machine.

`python3 bin/check-slide.py` measures the production pitch path with speaker
output muted: 440.08 Hz before the slide, 879.83 Hz after a +12-semitone slide,
and preserved phrase length. The overlap check also passes with the added
pitch processor and its latency compensation.

Final live Metal check (2026-09-22): all 51 phrases played. Median full-screen presentation rates were Neo 56.2 fps, Blueberry 52.0 fps, Frisbee 59.8 fps; all displays reported a 60 Hz target. Neo and Blueberry retain frame-time variation. Detailed measurements and screenshots are in `~/Shelf/macneopolitan-doowop/metal/`.
