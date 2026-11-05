# Notepat latency comparison

Notepat's musical keyboard events and synthesized note-on/off commands bypass
the graphics frame queue when audio is ready. Pending shortcuts preserve event
order; other pieces retain their frame input path. Touch pads and MIDI notes
also use immediate synthesized note commands. GM/sample playback uses its own
host path and is not measured by the synth benchmark.

Run unit/regression coverage:

```sh
node --test system/tests/notepat-latency.test.mjs system/tests/notepat-midi.test.mjs
```

In an owned, visible Notepat test tab, select **sine**, disable slide/room/glitch,
and play and release a note to activate audio. Open the browser console (or
use the resident Puppet page evaluator) and run each mode separately:

```js
const { benchmarkNotepat } = await import('/aesthetic.computer/lib/notepat-latency.mjs');
const batched = await benchmarkNotepat({ mode: 'batched', count: 80 });
const fast = await benchmarkNotepat({ mode: 'fast', count: 80 });
```

Alternate mode order on repeat runs. Export each returned object as JSON, then:

```sh
node system/scripts/compare-notepat-latency.mjs fast.json batched.json \
  --menuband-log /tmp/menuband.err
```

Add `stallMs: 12` for a controlled main-thread load scenario. The suite generates
short notes, so it should run in a dedicated tab, without concurrent playing or
metronome. It restores its previous probe after completion. Missing samples
fail the comparison command; check activation, sound selection and tab focus
before interpreting timings. The report contains local timing and environment
metadata only; it does not upload or retain played note content.

Stages are browser input handler, piece handler, browser audio bridge, and
estimated output-device time for the first nonzero source sample. The last
stage maps the AudioWorklet sample frame through `getOutputTimestamp()`; it is
an estimate, not microphone/loopback evidence. Probe replies and automation
round trips are not treated as audio onset. Composite voices count once.

For real-key timing, install a bounded probe and play isolated notes:

```js
const { createNotepatLatency } = await import('/aesthetic.computer/lib/notepat-latency.mjs');
window.acNotepatLatency = createNotepatLatency();
// Play, then collect:
window.acNotepatLatency.report();
// Stop capture:
delete window.acNotepatLatency;
```

Menu Band's existing startup log reports IO buffer frames and output-node
presentation time. The comparison tool shows these separately; neither is
keypress-to-sound latency, and they must not be added to browser timings as if
they measured the same stages. Match output route, sample rate and instrument.
Native parity remains unverified until both applications are measured using
the same physical key trigger and audio loopback. The software suite excludes
keyboard scan/debounce, OS delivery before the handler, and acoustic travel.

Timing reference: [Web Audio output timestamps](https://www.w3.org/TR/webaudio-1.1/#dom-audiocontext-getoutputtimestamp)
and [Apple output presentation latency](https://developer.apple.com/documentation/avfaudio/avaudionode/outputpresentationlatency).

## Measured 2026-11-05

Chrome 156 on macOS, sine, 48 kHz AudioContext, warm audio, visible 700×760
window. Each cell summarizes 40 generated notes; all 160 notes produced onset
reports. Quiet runs used `d8865a0be5`; load runs used `fc43be78fe` with the
same audio path. These are one-machine observations, not universal budgets.

| Mode | Input → bridge median / p95 | Input → estimated output median / p95 |
| --- | --- | --- |
| Frame-batched | 14.7 / 22.8 ms | 41.8 / 49.3 ms |
| Immediate | 0.3 / 4.5 ms | 27.5 / 33.7 ms |
| Frame-batched, 12 ms main-thread stall | 17.1 / 22.8 ms | 44.0 / 52.6 ms |
| Immediate, 12 ms main-thread stall | 12.2 / 13.8 ms | 38.9 / 42.1 ms |

The controlled stall still delays the main-thread audio bridge. Removing the
frame waits does not remove browser scheduling or device buffering. The browser
reported 24 ms output latency. Menu Band's earlier startup log on this host
reported 256 frames at 44.1 kHz (5.80 ms) and 1.45 ms presentation time; the
sample rates differ and this is not a matched keypress comparison.
