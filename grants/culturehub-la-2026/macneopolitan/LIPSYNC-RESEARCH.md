# Lip sync follows the sung buffer

Research for nubaz, September 20, 2026, Los Angeles. Based on the working
tree during the audio-driven jaw change; implementation may have advanced.

The largest remaining mismatch is timing: the DSP puts a consonant **before**
the note so the vowel lands on the beat, but the face changes shape **on** the
note. Export the DSP's mouth timeline with its rendered audio, then advance
both from the player's clock. Keep the new amplitude-driven jaw as expression.

## Evidence in this implementation

- `live/singer.c`, `synth_units()`: the onset length is `vs - a`, capped at
  60 WORLD frames (300 ms). The output starts at
  `round(grid - c_on * morph)`. At the host's `morph = 1`, the vowel begins
  on the note and the consonant precedes it. The current 70 ms m/b/p closure
  in `SingerFaceView.onset()` therefore closes the lips when the vowel should
  be opening. The actual lead varies by unit; 300 ms is a cap, not a measured
  delay for every word.
- The same function delays a stretched vowel's diphthong transition with
  `srcf = vs + pow(u, gamma) * (vlen - 1)`, where `gamma = 2.2` above 2×
  stretch. A face that merely holds one spelling-derived shape cannot follow
  that transition. The existing `o_src`, `o_c`, and `o_u` arrays already record
  source position, consonant status, and unit ownership for output frames.
- `MenuBandSinger.renderNow()` returns audio and `spanOffset`, but no mouth
  events. `AppDelegate` independently schedules captions and mouth onsets from
  the score. Its comment explicitly allows captions to continue when singing
  runs late. That is useful evidence of separate clocks, not proof of the
  amount of visible lag.
- `SingerFaceView.mouthClass()` uses letters. Its `w` rule makes “we” remain
  puckered through its vowel; “house” selects a small round mouth for all of
  /aʊ/; “I” is spread for all of /aɪ/. These need transitions. Zero-crossing
  rate can supply a rough noise cue, but does not identify these vowels.

## Next change to try

1. **Expose render timing.** Return events alongside `SungRender.buffer`:
   `unitID`, output start, vowel start, vowel end, output end, and optional
   mouth-shape keyframes. Generate them inside `synth_units()` after the same
   caps, rounding, clipping, and overlap decisions used for audio. Express
   them in buffer-relative seconds or explicitly identified sample-rate units.
   Carry `spanOffset` once; do not add it in both the event and the player.
   Unit timing alone fixes placement, but does not identify phonemes.
2. **Use actual playback position.** Associate each buffer with its start on
   the player timeline, including any queued predecessor. Derive animation
   progress from `lastRenderTime` and `playerTime(forNodeTime:)`. Handle nil,
   stop/reset, late starts, and cancellation generations explicitly. Retain
   the working audio scheduling fix while adding this readback; a graphics
   improvement need not replace the audio scheduler. Apple's player timeline
   resets to zero on stop. [Apple player timing][player]
3. **Account for audible output.** Render position precedes sound leaving the
   device. Apple exposes `outputPresentationLatency` as the maximum downstream
   render-pipeline latency. Use it as an input to calibration, not a promise
   of exact acoustic timing; measure the actual built-in/Scarlett route.
   [Apple presentation latency][latency]
4. **Use the rendered buffer for the jaw envelope.** A small RMS envelope
   computed off the audio thread can be sampled at the same playback position.
   This avoids making tap delivery the animation clock. Keep the tap for
   diagnostics. A requested 1,024-frame tap is not a guaranteed 23 ms cadence:
   Apple explicitly permits another size. Log actual frame lengths and
   timestamp deltas before relying on its rate. [Apple tap API][tap]

These are implementation recommendations, not changes made by this research.

## Mouth shapes and phoneme sources

Rhubarb's established 2D vocabulary is a good fit for this drawn face:
pressed lips (P/B/M), teeth/narrow opening, medium open, wide open, rounded,
puckered, plus optional F/V, tongue-L, and relaxed rest. Its examples also
show anticipation and intermediate shapes. Keep amplitude subordinate to a
closed-lip event: a voiced M can have energy while the lips remain shut.
[Rhubarb mouth shapes][rhubarb]

For this week's fixed lyrics, a small reviewed phoneme/shape table is a
practical first source. An automatic source can follow:

- Apple's `write(_:toBufferCallback:toMarkerCallback:)` exposes synthesis
  metadata, and `AVSpeechSynthesisMarker` represents phonemes and byte sample
  offsets. Probe Noelle and Allison before depending on this: the API's
  existence does not establish that these installed voices emit phoneme
  markers. Convert offsets using the actual PCM layout, then remap through
  the singer's time warp. [Apple marker callback][markers],
  [Apple marker fields][marker-fields]
- Rhubarb is an offline audio-file analyzer with timed mouth-cue JSON and a
  dialogue-text hint. Try it on an isolated voice, not the mixed band. It is
  a candidate to evaluate, not a demonstrated solution for these sustained
  sung vowels. Analyzing original speech still requires remapping its cues
  through `o_src`; analyzing final singing needs accuracy and runtime checks.
  [Rhubarb CLI and output][rhubarb]

## Rehearsal checks

Use short, isolated sung phrases before the six-minute solo:

- “born,” “past,” “mid-night”: closure belongs before vowel onset.
- “we,” “house,” “night”: sustained first vowel, then the diphthong transition.
- “six minutes”: consonant detail without making every noisy frame a closure.
- A long rest, deliberately late render, Escape, and immediate restart:
  silent/stopped audio must not leave a mouth moving or replay old events.

Capture audio and the screen together. Compare a visible cue with a known
audio onset, then inspect the real singing at normal speed. Suggested project
target: timing error within one 30 fps frame after route calibration; this is
an engineering target, not a cited perceptual threshold. Check both machines
and the venue output separately. Inspect at 10% scale so the mouth shapes
remain readable from the audience.

This pass inspected source and primary documentation. It did not measure
audio/display latency, test marker availability, install a recognizer, or
change the running performance.

[player]: https://developer.apple.com/documentation/avfaudio/avaudioplayernode
[latency]: https://developer.apple.com/documentation/avfaudio/avaudionode/outputpresentationlatency
[tap]: https://developer.apple.com/documentation/avfaudio/avaudionode/installtap(onbus:buffersize:format:block:)
[rhubarb]: https://github.com/DanielSWolf/rhubarb-lip-sync#mouth-shapes
[markers]: https://developer.apple.com/documentation/avfaudio/avspeechsynthesizer/write(_:tobuffercallback:tomarkercallback:)
[marker-fields]: https://developer.apple.com/documentation/avfaudio/avspeechsynthesismarker
