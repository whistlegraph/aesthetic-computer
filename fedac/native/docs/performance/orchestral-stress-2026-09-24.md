# Orchestral stress test — 2026-09-24

Full original score: 774.4119 seconds, 10,739 events, 32 lanes, 128 General MIDI programs. Six native laptops rendered their existing performance graphics while Windows rendered the SUB web app. TTS announced the test before the shared cue. Run `1790279516794-3acdc80066a52-prepare` completed naturally on all six laptops; Windows returned READY and both DMX transports were verified off.

Performance master was 25% on all seven clients. Laptop hardware mixers stayed at 100%; Windows device routing remained VBMatrix In2, with both web-audio channels enabled. The Windows OS mixer was not changed. Battery overlays remained visible. Backlight brightness was not normalized or measured in this baseline; the later request for 100% brightness was deferred until it ended. No reboot or USB boot-image change was made.

| Machine | Samples | Median FPS | 5th-percentile FPS | Lowest interval FPS | Longest frame gap (ms) |
|---|---:|---:|---:|---:|---:|
| ac5 · left front | 656 | 52.15 | 45.70 | 5.04 | 2378.3 |
| ac8 · right front | 659 | 52.19 | 45.25 | 33.05 | 354.4 |
| ac9 · right rear | 658 | 51.98 | 43.76 | 34.65 | 348.3 |
| ac4 · left rear | 658 | 52.14 | 45.03 | 37.89 | 371.0 |
| ac6 · center rear | 659 | 55.97 | 50.20 | 33.41 | 530.2 |
| ac0 · held center | 653 | 58.04 | 52.99 | 9.73 | 4278.0 |
| Windows · SUB | 658 | 60.00 | 59.99 | 58.99 | 33.4 |

Each native sample measures approximately one second of `paint` callback arrival intervals using `performance.now()`. Desktop FPS measures `requestAnimationFrame` callback intervals, not GPU presentation. Repeated native timestamps and desktop heartbeat timestamps are deduplicated; the first 1.5 seconds are excluded. Polling is about once per second, so unobserved intervals and brief voice peaks can be missed. These are sampled baseline measurements, not proof of an optimization.

`paintMeanMs` in the CSV is elapsed wall time around the performance-and-battery paint callback. It excludes following configuration/status reads, DMX sends, telemetry writes, and native rendering after the callback. It is not OS CPU time. The harness reads local configuration each frame and writes telemetry each second; that observer overhead is included in frame gaps. OS CPU utilization, GPU load, audio underruns, and acoustic output latency were not measured.

All five lights ran: room d001/d011/d031/d021 through the Windows COM3 bridge via Neo, and held-center d041 through the native USB bridge. Four-second RGB/amber changes followed the score clock; room envelopes used 150 ms attack and 300 ms release. This exercises the complete light transport, but is not a musically authored light score or a calibrated light/audio synchronization measurement.

Room conductor sent 768 fixture commands; the last cue was at score 764.115 seconds. It then received an incomplete HTTP response from ac5, exited, and executed its final blackout. The scheduled 768- and 772-second groups were not sent, so the room-light tail was incomplete even though the audio completed. Final room state: `off`; final bridge result: `{'result': 'ok', 'fadeFrames': 7, 'id': 'b823f266'}`.

| Movement | Ring median FPS range | Held-center median FPS | Windows median FPS |
|---|---:|---:|---:|
| I · Overture | 54.10–56.12 | 60.00 | 60.00 |
| II · The Walk | 50.99–55.13 | 59.00 | 60.00 |
| III · Waltz | 52.10–55.68 | 58.58 | 60.00 |
| IV · Chase | 50.18–55.12 | 57.44 | 60.00 |
| V · Sneak | 53.11–57.02 | 59.31 | 60.00 |
| VI · Lullaby | 51.23–56.03 | 59.01 | 60.00 |
| VII · The Climb | 50.21–55.09 | 57.99 | 60.00 |
| VIII · The Lift | 50.50–55.42 | 57.98 | 60.00 |
| IX · Fanfare | 52.18–56.89 | 59.00 | 60.00 |
| X · Return | 52.00–55.99 | 58.00 | 60.00 |
| XI · Vanish | 55.10–57.43 | 58.04 | 60.00 |

Highest sampled SUB oscillator count: 7. Native oscillator counts are not exposed by this harness.

The user reported that SUB felt late. Source review identified a plausible timing mechanism: native status is cached for about 250 ms, while the SUB server extrapolates from HTTP receipt plus half RTT without accounting for that cached sample age. This can add variable source-age delay before the unmeasured device/output path. It is not a measurement of acoustic delay. AudioContext base/output latency and the receiver offset setting were not included in this baseline heartbeat. A clock-mapping correction is being prepared separately; this baseline was not changed.

The collector also recorded one incomplete ac6 status response and one ac0 timeout interval; it continued and captured final finished states for all six.

Held-center ac0 had a 4.278-second paint-arrival gap around score 4:45–4:49 and then recovered; native status separately reported a 4.285-second maximum simulation gap. It also had a 1.997-second gap around score 8:34. Left-front ac5 had a 2.378-second gap around score 5:22. The current native DMX implementation performs a blocking serial write on the JS thread, making it a profiling candidate rather than an established cause. A follow-up should time `dmxSend` separately.

The observed ring stalls warrant a controlled repeat that caches the wrapper configuration and reduces telemetry polling, followed by profiling native scheduling/rendering if stalls persist. Keep the same score, gains, graphics, and light cadence for that comparison. No performance change was applied during this run.

Connections and recovery: [fleet contract](../../../../grants/culturehub-la-2026/macneopolitan/fleet/CONNECTIONS.md). The accepted 36-second test remains intact in `.tmp/notespatial-2026-09-24/sub-check/`; this run used separate `orchestral-stress/` assets. At baseline completion the wrappers remained on the full score at 25%; subsequent brightness/timing/candlelight work changes that live state and is outside this measurement.

Reproduction assets and full source-status JSONL: `.tmp/notespatial-2026-09-24/orchestral-stress/` (`prepare.py`, `collect.py`, `summarize.py`, `room-dmx-stress.py`, `samples.jsonl`). Reduced measured samples: [CSV gzip](orchestral-stress-2026-09-24.csv.gz).

Score SHA-256: `d3ae52f7f427aa753734977c7079ec81c66167100107d88e2960e35cf5066d18`.
