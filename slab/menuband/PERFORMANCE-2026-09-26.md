# Menu Band responsiveness — September 26, 2026

Read `prox:tesud` and inspected the local Focusrite monitoring, reset,
recording, and drag/export paths. These changes build on that session's
uncommitted work, including its integer/Double ring-addressing fix.

## Evidence

- The running app was using 16-frame callbacks at 44.1 kHz: a 0.36 ms
  deadline. The log reported 95 underflows in one five-second interval
  after launch/reset, followed by clean intervals. Low average CPU does
  not establish that every audio callback met its deadline.
- The last three minutes before installation were also clean: 496,135
  captures and pulls, zero reported underflows/drops/overflows. A clean
  post-install idle run alone cannot establish improved sound quality.
- A five-second `sample` capture showed the main thread mostly waiting
  between interactions. `ps` readings were around 3% CPU. This was idle
  monitoring, not a controlled benchmark of playing or recording.
- The reader handled starvation by advancing `writeIndex` without writing
  those frames. Ring memory could therefore replay an earlier wrap.
- Reset requests independently launched rate flips and delayed “ready”
  notifications. Focus, latency changes, and startup could overlap.
- Background MP3 export first synchronously dispatched WAV conversion,
  stems, and artwork to main. A cold drag additionally waited for ffmpeg.
  The encoder and completion also read the *current* take's mic setting
  and stems, which could belong to the next recording.

## Changes

| Work | Ownership and behavior now |
|---|---|
| Mic recovery | Consume only published frames; wait for fresh capture to restore the cushion. Silence advances the effects timeline too. |
| Monitor diagnostics | Snapshot/reset counters under the ring lock; skip the disabled gate's envelope calculations. |
| Interface reset | One operation per device through settling; repeated requests share completion, different devices queue, and busy stays true until all finish. Delays release the worker. |
| Latency selection | Debounce rapid picks; cancel superseded reset timers; restore only the latest request's buffer setting. |
| Finished recording | Retain an immutable take, including mic routing and mix metadata. The next recording gets separate storage. No bulk copy at export handoff. |
| Export | One utility queue for WAV, stems, artwork, and MP3. Repeated consumers join the pending export. ffmpeg uses one thread and utility QoS. |
| Cassette drag | File promises start immediately and fulfill after export; the provider retains its delegate and captured take. |
| Desktop delivery | Copy the captured take and stems in the background; preserve cached originals for later drags. Collision suffixes also apply to the stems directory. |
| Tape capacity | Queue auto-stop once when the master reaches capacity, including synth-only takes. Check take identity before stopping so an old callback cannot stop a new take. |

## Validation

The app's debug build passed. The standard Swift test build is blocked on
this CLT-only Mac by the missing `XCTest` module. XCTest cases are included
for hosts with Xcode; the standalone runner compiles the actual production
sources and works here:

```sh
bash slab/menuband/bin/check-audio-smoothness.sh
```

The runner passed underrun/recovery, exact addresses beyond `2^24` and
`2^30`, 100,000 jittered callback cycles, callback-size changes, overlapping
and failed resets, frozen audio/mix metadata during the next recording,
stale auto-stop rejection, and asynchronous file-promise lifetime/content.

A synthetic 90-second take measured **0.002 ms** for snapshot handoff and
**0.458 s** for background WAV/stem export. During export a 10 ms main-thread
timer ran 45 times, with a maximum observed gap of **12.3 ms**. This is one
local run, excludes MP3 encoding, and measures moving export work off main,
not a general CPU-speedup ratio or hardware round-trip latency.

The ARM release was installed locally with the existing `MenuBand Self-Signed`
identity. Deep/strict signature validation passed, and a sample of the running
process matched the newly built executable's UUID. The universal installer
could not link the Intel launcher because this CLT toolchain lacks its
`swiftCompatibility56` symbol; the local installation remains ARM-only, as
it was before. No other fleet machine was updated.

After startup/reset settling, six consecutive five-second intervals reported
82,688 captures and pulls with zero underflows, drops, overflows, invalid
samples, or wet underflows at the existing 16-frame setting. Recovery retained
a 32-frame cushion after each pull; the log measured 48 frames before the
16-frame pull (1.09 ms of ring lead). This is an idle monitor check, not an
audition or proof against the spontaneous output-silence problem.

## Remaining constraints

- The monitor still uses `os_unfair_lock` across capture and rendering,
  including pitch-shift DSP. Allocation during ring replacement also holds
  that lock. A preallocated single-producer/single-consumer transport and
  coherent parameter snapshots are the next substantial realtime change.
- Graph mutation is split between main, audio-control, and device-listener
  queues. Serializing reset requests reduces one source of churn; it does
  not establish a single owner for the whole audio graph.
- Input and output on independently clocked devices still need clock-drift
  compensation. Dropping backlog is recovery, not sample-rate conversion.
- The two-tap pitch shifter still has its own audible limitations. These
  changes do not replace its DSP or demonstrate that all perceived aliasing
  is gone.
- Frozen buffers use additional memory while another take is recorded.
  The existing 90-second recording cap remains.
- `tesud` reported a plain output-only tone failing with Menu Band stopped.
  That is evidence for a device/USB/driver-path problem outside Menu Band,
  not proof that the physical interface is faulty. Software changes here
  cannot certify that those silent output failures are cured.

Apple's [audio-source render guidance](https://developer.apple.com/documentation/avfaudio/avaudiosourcenode/init(renderblock:))
requires avoiding blocking render work. Its
[file-promise API](https://developer.apple.com/documentation/appkit/nsfilepromiseproviderdelegate/filepromiseprovider(_:writepromiseto:completionhandler:))
supports asynchronous fulfillment on a supplied operation queue.
