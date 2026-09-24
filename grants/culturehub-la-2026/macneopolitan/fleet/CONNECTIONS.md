# CultureHub audio and light connection contract

Verified on 2026-09-24 on Blueberry. Jeffrey approved the announced 36-second,
six-laptop + subwoofer + five-light replay: “yeah that was great.”
The prox session on **Frisbee is reading/waiting for these connections to run a
more synchronized singer test with the lights** (user report; handle unspecified).
This file is the handoff; writing it does not cue another performance.

## Seats and outputs

Preserve this order in every conductor command. Seat numbers on screen are
one-based; score indices are zero-based. These are venue LAN addresses; recheck
reachability after network changes.

| Score index | Seat | Machine | LAN IP | Position | Light start address |
| --- | --- | --- | --- | --- | --- |
| 0 | 1 | ac5 | 192.168.1.237 | Left front | d001, Windows USB |
| 1 | 2 | ac8 | 192.168.1.242 | Right front | d011, Windows USB |
| 2 | 3 | ac9 | 192.168.1.238 | Right rear | d031, Windows USB |
| 3 | 4 | ac4 | 192.168.1.236 | Left rear | d021, Windows USB |
| 4 | 5 | ac6 | 192.168.1.241 | Center rear | No mapped room fixture |
| 5 | 6 | ac0 | 192.168.1.239 | Held center | d041, ac0 USB |

| Host | LAN IP | Responsibility |
| --- | --- | --- |
| Blueberry | 192.168.1.234 | Native conductor; working short-test SUB web server on 8791 |
| Neo | 192.168.1.235 | Room DMX HTTP queue on 8790 |
| Windows / DESKTOP-VIDV882 | 192.168.1.67 | SUB browser/audio output; COM3 room DMX worker |

## Audio contract

The accepted replay used **15% performance master** on all six laptops and the
SUB browser. A subsequently requested orchestral stress test uses **25%**;
its results are separate from the accepted replay. Set and verify the intended
master explicitly when changing pieces or receiver origins.

On laptops, hardware mixers remain at 100%; set `sound.volume.setMix(0.15)`
(or `0.25` for the stress test). Do not use `system.volumeAdjust` to implement
this mix level: it attenuated multiple hardware stages plus software and made
the fleet effectively silent. Master readback settles near the target.
Windows OS output was last observed at 47%; browser master is the performance
control. Percentages do not assert equal acoustic loudness across devices.

Preserve the working Windows **VBMatrix In2** output routing. The user reset a
Windows driver to restore SUB audio. The working test page is
`http://192.168.1.234:8791/`, armed, Web Audio `running`, route `both`, fullscreen.
The separate Windows-local `127.0.0.1:8788` page is an older full-score receiver;
arming it does not arm the Blueberry page. New browser origins may require
Enable Audio and explicit level selection again.

The SUB filters bass/kick events, transposes down an octave, and uses a 25 Hz
high-pass, two 80 Hz low-pass stages, compression and a browser master. Native
and SUB score duration/hash must match before cueing. The 36-second receiver
cannot follow the 12:54 orchestration unchanged.

## DMX contract: two independent USB outputs

**Room:** Windows COM3, Enttec Pro framing, persistent PowerShell worker polling
`http://192.168.1.235:8790/next?wait=1`. Keep that worker running. Neo's service
is `/Users/jas/.ac-os/culturehub/light-monitor/server.py`.

Room fixtures are six-channel units: R, G, B, amber, white, UV. The verified
replay uses RGB only at d001/d011/d021/d031. The seat mapping above supersedes
earlier notes about shared address groups or camera near/far positions.

Read `GET /state` on 8790; require recent `bridgeSeen` (under 3 seconds) and
an empty queue before starting. Submit `POST /command` **from Neo localhost**:

```json
{"address":1,"color":"rgb","rgb":[48,0,0],"level":48,"duration":2,"envelope":{"attack":0.08,"decay":0.5}}
```

The current test uses peak RGB 48/255 and two-second envelopes. Enveloped
voices mix HTP in the worker at approximately 40 updates/second. Local
`POST /cancel` with `{}` clears the queue and requests blackout. Verify its
acknowledgment and `color: "off"`. An acknowledgment confirms worker processing,
not measured optical output.

**Held center:** Chauvet Wedge Tri, three-channel RGB at d041, physically
verified red/green/blue, plugged directly into ac0. Call native
`system.dmxSend(slots)` with a 64-element array: indices **40, 41, 42** are R/G/B;
all other slots zero. The test refreshes at approximately 25 Hz. Send zeros
outside the cue, on stop and on piece leave. `/pieces/center-dmx-live.json`
reports transition telemetry (`ok`, `active`, `scoreTime`, `rgb`, `address`).
Its timestamp is a transition time, not a continuous heartbeat.

## Accepted 36-second replay

The held-center laptop speaks each position/color one second before its cue;
it also announces the test at piece boot and completion at second 34.

| Cue time (seconds) | Audio position | Light |
| --- | --- | --- |
| 1 | Left front + sub | d001 red |
| 5 | Right front + sub | d011 green |
| 9 | Right rear + sub | d031 blue |
| 13 | Left rear + sub | d021 amber via RGB |
| 17 | Center rear + sub | Sound only |
| 21 | Held center + sub | d041 amber via RGB |
| 25 / 28 / 31 | All six + sub | All five red / green / blue |

SUB paired tones: 55, 65.405, 73.415, 82.405, 49, 61.735 Hz; final pulses
55, 49, 65.405 Hz. The native test mutes its bass GM lane so the sub owns it.

Verified score SHA-256:
`8386795478723d47a70fbd91710bb93d312717acbfaa68e5c73cec9cbd44f0a6`.
The completed replay sent 16 room commands and received a successful final
blackout acknowledgment; center reported inactive RGB zero. SUB heartbeat
confirmed 15%, both channels, fullscreen, armed/running and READY afterward.
The user supplied the audible/visual acceptance.

## Reconnect, inspect, cue and stop

Laptops serve HTTP on port 80: `GET /status`, `GET/PUT /pieces/<filename>`,
`PUT /jump/<piece>`. A jump reloads the piece and may interrupt/reset a run.
Inspect `/pieces/spatial-rehearsal-status.json` for `phase`, `runId`, `audioTime`,
`origin`, `scoreTime`, `scoreDuration`. Do not cue while another conductor owns
the rig. Microphones stay closed.

The working short-test bundle currently lives on Blueberry at
`.tmp/notespatial-2026-09-24/sub-check/` (local, ignored by Git): `server.mjs`,
`config.json`, `app.mjs`, `core.mjs`, `score.nsscore`,
`sub-check-performance.mjs`, `sub-check-laptop.mjs`,
`sub-check-announced.mjs`, and `room-dmx-test.py`.
These are recovery paths, not assets included by this documentation commit.
The first five laptops used `sub-check-laptop`; ac0 used
`sub-check-announced`; both load `/pieces/sub-check.nsscore` through the test
performance module. Later stress testing may replace the running pieces.

If the short-test server is absent, start it from the repository on Blueberry:

```sh
node .tmp/notespatial-2026-09-24/sub-check/server.mjs
```

Inspect `http://127.0.0.1:8791/api/receivers` for `online`, `armed`,
`audioState`, `level`, `route`, `fullscreen`, `duration`, `scoreHash`.
`/api/state`, `/api/score` and `/api/world` expose transport and score state.

For the short replay, first start the room follower on Neo in a separate shell:

```sh
ssh neo python3 /Users/jas/.ac-os/culturehub/light-monitor/combined-test-2026-09-24.py
```

It waits for a 36-second native run and blackouts in `finally`. Then cue on
Blueberry using the known deployed controller snapshot:

```sh
node /tmp/ac-spatial-update/fedac/native/tools/spatial-rehearsal.mjs cue \
  192.168.1.237 192.168.1.242 192.168.1.238 \
  192.168.1.236 192.168.1.241 192.168.1.239
```

To stop audio, run the same command with `stop` in place of `cue`. Also cancel
room lights independently:

```sh
ssh neo 'curl -fsS -X POST http://127.0.0.1:8790/cancel -H "Content-Type: application/json" -d "{}"'
```

Verify native playback stopped, SUB left PLAYING, room blackout acknowledged,
and center RGB zero. Do not assume stopping one USB controller stops the other.
Room receipts are saved on Neo at
`/Users/jas/.ac-os/culturehub/light-monitor/combined-test-2026-09-24.json`.

## Frisbee singer synchronization handoff

Use the same seat/address map and output routing. Before taking control, arrange
one conductor owner; the newly requested orchestral stress test must finish or
be explicitly stopped before the singer test starts.

Define one run ID, score duration and future start time for audio and light
schedules. Translate each device's audio clock using measured clock probes;
never equate wall-clock time with `sound.time`. The existing native cue command
probes each laptop seven times and schedules a future start. SUB follows native
transport; it must have the matching score loaded and audio enabled.

The accepted room follower polls native status every 25 ms and posts HTTP cues;
it skips cues more than 400 ms late. This is a demonstrated functional baseline,
**not sample-accurate synchronization or a measured latency guarantee**.
Its schedule is hardcoded to 36 seconds. A singer test needs its own event
schedule and duration handling. For tighter timing, stage events before their
deadlines, establish the receiver's clock relationship, and measure actual
light/audio onset. Current room commands do not establish a future-time
scheduling API; add and verify such support before relying on it.

Log run ID, intended event time, dispatch/receipt time, clock RTT and skew,
late/skipped cues, actual available frame metrics, and final blackout results.
Label missing metrics rather than substituting HTTP polling rate for FPS.
TTS must finish before the musical start; spoken cue lead time is part of the
score when announcements continue during the test.

## Persistence and next reboot

### Next lighting treatment: candlelight

Jeffrey requested subtle, quick changes with a candlelight feel, explicitly
“not strobey.” The staged profile is
[`candlelightRgb`](../../../../fedac/native/lib/candlelight.mjs): warm amber,
small irregular brightness changes smoothly interpolated over 170 ms, slower
1.7-second drift, and slight warmth variation. During an active cue it never
drops to black. Seed each fixture separately while evaluating the shared score
clock; use a smooth entrance/exit fade outside the profile.

This profile is prepared, not deployed or visually accepted. The orchestral
measurement run retains its original four-second color cycle. Render the
candle motion locally at each USB worker's regular frame cadence; do not flood
the room's HTTP queue with per-frame commands. The current Windows worker
needs an explicit profile implementation before it can render this function.

### Boot state

Keep battery percentage visible. The live battery watcher flashes a bell and
sounds sine tones at 10% or lower, every `max(1, percent * 3)` seconds: 30 seconds
at 10%, 15 at 5%, 3 at 1%. These live wrappers have not all been persisted to USB.

Both USB boot partitions on all six were updated and reboot-verified with the
full arrangement from Neo revision `263da999a4`, boot piece
`notespatial-live-263da9`, and the personalized Jeffrey greeting suppressed.
The separate Wi-Fi “connected to CULTUREHUB LA” TTS remains: replacing it with
a short sine refrain is requested and deferred. Battery/volume/test/DMX wrapper
changes require explicit persistence work before they survive reboot.
No reboot is part of this handoff.

The original full score remains `/pieces/spatial-rehearsal.nsscore`, duration
774.4119 seconds, SHA-256
`d3ae52f7f427aa753734977c7079ec81c66167100107d88e2960e35cf5066d18`.
Its sustained bass enters around 60 seconds; stronger/earlier sub arrangement
work was discussed but was not part of the accepted short replay.
