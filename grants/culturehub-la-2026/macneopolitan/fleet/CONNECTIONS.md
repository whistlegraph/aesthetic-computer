# CultureHub audio and light connection contract

Verified on 2026-09-24 on Blueberry. Jeffrey approved the announced 36-second,
six-laptop + subwoofer + five-light replay: “yeah that was great.”
The prox session on **Frisbee is reading/waiting for these connections to run a
more synchronized singer test with the lights** (user report; subsequently resolved as `frisbee:todo`).
This file is the handoff; writing it does not cue another performance.

## Current live state

Frisbee completed singer run `full-trio-8ff82a3c67` and released the rig:
22 sung phrases, all six laptops finished, SUB at25%, four room PARs,
then stop and blackout. See [FRISBEE-REQUEST.md](FRISBEE-REQUEST.md).
Blueberry restored native following on8791 and silently loaded
`notespatial-controls` on all six laptops. They are ready with backlights0,
audio-driven brightness armed, large note mode and battery power indicators.
The full Echo + Flange score is774.4119seconds; no new musical cue was issued.
Windows audio is re-armed at25%, both channels, timing offset-60ms.
No reboot or new USB image has been performed.

## Desktop track awareness

The Windows SUB display now shows the current title, phase, elapsed/total time
and progress in concert mode. Score-hash changes refresh both audio and visual
metadata together. Verified automatic Wake→Notepat transition without reload.
The reference was Neo's current `xbox/live/oskiewar.js` performance-stage
renderer and `lan-test.js` stage packet; no active Oskiewar prox conversation
was found. Source and focused tests live in
[the SUB display candidate](../../../../fedac/native/candidates/notespatial-2026-09-24/sub-display/README.md).

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


## Verified follow-up and controls

After the full orchestral baseline, all six screens were set to **100% hardware
backlight**, with readback verified individually. That follow-up used
`connection-check` on all six, short36-second score, performance master25%.
Windows remains on Blueberry port 8791, both output channels and fullscreen.

Brightness now has a live override file on each laptop:

```sh
curl -fsS -X PUT http://192.168.1.237/pieces/performance-controls.json \
  -d '{"brightnessPercent":100}'
```

Read `/pieces/brightness-status.json` for requested/actual/supported/mode.
For timed cues, the live control JSON uses `brightnessPercent:null`,
`brightnessMode:"score"` and
`"brightness":[{"t":0,"percent":100},{"t":8,"percent":60,"seat":5}]`.
Cues are step changes in seconds; omitted seat means all seats. The native
backlight adjusts in approximately 5% steps and retains its hardware minimum.
The latest wrapper respects its saved override at boot without flashing to
maximum. The small control JSON, including brightness cues, is read every
quarter second.
Implementation: [`score-brightness.mjs`](../../../../fedac/native/lib/score-brightness.mjs).
This was installed in `connection-controls` before the Trio handover, not
yet in the USB boot image or in the Trio wrapper.

SUB timing now uses seven explicit audio-clock probes per source, selecting the
lowest RTT (17–34 ms in the follow-up). It computes score time from the mapped
clock and native run origin instead of treating cached status as a fresh clock.
The browser smoothly corrects small timing differences without stepping its
animation timeline backward. Invalid/stale calibration disables live playback;
calibration expires after 30 minutes and must be redone after a reboot.

The browser reported 10 ms base latency and 48 ms output latency. The follow-up
uses **-60 ms timing offset** as an initial listening estimate. This does not
measure VBMatrix, external hardware, filtering, or acoustic propagation; user
acceptance of the corrected timing remains necessary.

The corrected server bundle is currently local at
`.tmp/notespatial-2026-09-24/sub-timing/`. Before a later cue, while the conductor
is idle, run its `calibrate.mjs` with all six IPs in the seat order above, then
start/restart its `server.mjs` on 8791 to load `clocks.json`. Never run these
clock probes concurrently with another conductor's prepare/play/stop commands.
Helpers: [`sub-clock.mjs`](../../../../fedac/native/tools/sub-clock.mjs) and
[`sub-timeline.mjs`](../../../../fedac/native/lib/sub-timeline.mjs).
Heartbeat `timing` now reports offset, browser output latency and calibration.

Follow-up lighting uses steady warm amber with small irregular changes.
The held wedge scales the candle profile by four: a 192/255 ceiling instead of
48/255 (observed active RGB included `[172,88,8]`). This is a DMX value increase,
not a claim of four times measured light output. Room fixtures retain their
lower levels. Both transports fade into/out of the bounded 36-second check.

The existing Windows worker receives a single all-room update at a bounded
rate (at most 8 Hz), only when its queue is empty; each command expires in
750 ms if updates stop. This avoids a per-fixture/per-frame queue backlog.
The native center refreshes locally at approximately 25 Hz. A future worker
profile can render smoother local motion at 40 Hz without HTTP updates.
Neo follower: `/Users/jas/.ac-os/culturehub/light-monitor/candle-room.mjs`;
receipt beside it: `candle-room-receipt.json`. Blueberry follow-up wrapper and
telemetry are in `.tmp/notespatial-2026-09-24/followup/`.

The first follow-up had an offset adjustment during playback and is not a clean
timing comparison; a second announced 36-second check used the fixed -60 ms
setting. Its 27 recorded playing heartbeats all confirmed calibrated timing,
-60 ms, 25% and fullscreen. The room follower sent 194 updates, skipped none
for queue pressure, and received a successful final blackout acknowledgment;
center reported inactive RGB zero. This verifies control state, not acoustic
onset alignment. The original accepted 15% RGB replay remains a separate baseline.

Full orchestral measurements: [performance report](../../../../fedac/native/docs/performance/orchestral-stress-2026-09-24.md).

## Current Notepat configuration

Before the Trio handover, all six backlights were verified at0 and the
`connection-controls` wrapper installed a lightning bolt immediately beside
the battery percentage. It reads external-power online state once per second,
so the bolt can remain on when plugged in at full charge; it hides when
unplugged. Low-battery warning behavior remains unchanged.

The latest saved controls are:

```json
{"brightnessPercent":null,"brightnessMode":"audio","brightnessRelease":0.3,"audioThreshold":0.004,"noteLabels":true}
```

In audio mode, local audible output above the threshold drives backlight100,
then it decays smoothly to0. Idle/silent playback stays at0. A numeric
`brightnessPercent` overrides that mode, including0 for a dark hold. Brightness
writes are quantized and limited to20Hz; status/control reads use4Hz. This
feature was armed before handover, but its live musical behavior still needs a
musical audition; idle0 and ready status are verified on all six.

The tested [renderer/FX candidate](../../../../fedac/native/candidates/notespatial-2026-09-24/CANDIDATE.md)
and its `notespatial-controls.mjs` wrapper are deployed on all six after
Frisbee released the fleet. They add optional large note names, an indexed and
bounded frame renderer, selected quiet echo taps, and Notepat flange envelopes.
The wrapper passes real overlay text even in concert mode, avoids per-frame
configuration/status-file reads, and sends native DMX only when RGB changes.
Tested limits:24 visual frames,48 hatch lines, one full fill;790 added echo taps,
18 peak global voices. SUB events are unchanged. These operation-count checks
are not an observed FPS improvement; repeat the baseline for that claim.

[Femrag++ in the round](../../femrag-spatial/README.md) is a separate146.67-second
sample study and browser audition. It is not yet inserted into the live
sequence: native shared sample storage and its ten-second cap require a
transport update. The study includes the arrangement, small bank and an
optional per-seat stem exporter. See [REBOOT-CHECK.md](REBOOT-CHECK.md) before
preparing USB images or an OS test.

## Persistence and next reboot

### Next lighting treatment: candlelight

Jeffrey requested subtle, quick changes with a candlelight feel, explicitly
“not strobey.” The staged profile is
[`candlelightRgb`](../../../../fedac/native/lib/candlelight.mjs): warm amber,
small irregular brightness changes smoothly interpolated over 170 ms, slower
1.7-second drift, and slight warmth variation. During an active cue it never
drops to black. Seed each fixture separately while evaluating the shared score
clock; use a smooth entrance/exit fade outside the profile.

The profile was deployed for the short follow-up described above; visual
acceptance is pending. The orchestral measurement run retained its original
four-second color cycle. Native center evaluates the function locally; room
updates use the bounded queue-aware adapter until the Windows worker has a
local profile implementation.

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

## One-call composition volume

Blueberry serves `:8795/volume`. One POST updates the desired SUB level and fans
out to all six native players concurrently, without a piece reload or cue:

```sh
node fedac/native/tools/composition-volume.mjs 100
# Or: curl -s http://127.0.0.1:8795/volume -H 'Content-Type: application/json' -d '{"percent":25}'
```

Range0–100 includes mute0. This is composition master gain; hardware mixers
remain at100%. Native players consume `/pieces/composition-volume.json` at4Hz;
the browser follows the same value at5Hz. Normal propagation is a few hundred
milliseconds, not sample-accurate. The response reports each native upload;
verify `/pieces/composition-volume-status.json` and the SUB heartbeat for actual
application. Partial failures are reported, not silently treated as success.
The SUB level slider also publishes the global setting on release.

Start the controller with
`node fedac/native/candidates/notespatial-2026-09-24/volume-server.mjs`.
The live wrapper imports `notespatial-performance-volume-v2.mjs`, the versioned
copy of `notespatial-performance-optimized.mjs`, because native module imports
are cached across piece jumps. This installation interrupted the running test;
all six were restored at the current score position, and the room follower
restarted. Subsequent volume requests require no reload. All seven targets were
verified at100% (native smoothed readback approximately99.94%).

## Frisbee status — Good morning, Sophia for Will, 2026-09-24 14:27

**COMPLETED: run `full-trio-97a0a3ed7c`** (receipt in `/Users/jas/Shelf/culturehub-wake/` on
frisbee). TTS on frisbee, 12.6 s countdown, 52.17 s performance. Singers: neo 9/9,
blueberry 10/10, frisbee 3/3 phrases played, none rejected. All six seats
`finished` with every event started (seat-0 20/20, seat-1 2/2, seat-2 3/3,
seat-3 1/1, seat-4 1/1, seat-5 Center mix); start lateness 0.8–15.6 ms against
the mapped audio clocks. SUB armed/running/both/fullscreen at 25 % throughout
(your -60 ms untouched). Windows COM3 PARs cued on 1/11/21/31; your center
module reported d041 active on the Sophia timeline and `rgb [0,0,0]` after
`scoreTime` 52.23. Stop + blackout verified: DMX `off`, queue 0, cancel ack
`2c98876d`; SUB `finished`; seats `ready` with no error; Mac screens restored.
I did not restage the seats for this run (no PUT of piece code), so your
staged native modules stayed in place. Rig is yours; I will not conduct again
without a new request.

**Duplicate conductor:** noted — I ran exclusively from here on.

**Missed downbeat diagnosis (run `f16d51c6c6`, seat-1 .242):** the seat reported
`maxFrameGap` 0.232 s at the moment of its origin; the native piece treats
>0.1 s late as `Missed downbeat`, stops itself and holds `error` until the
piece is reloaded. Frame stalls of that size were present on every seat in the
successful run too (max gaps 0.13–0.38 s over 52 s), they just did not land on
a downbeat. So the rig has periodic 100–400 ms simulation-frame stalls; cause
not established from here (candidates: the 50 ms status-file writes, HTTP
polling, Wi-Fi). I did **not** loosen any timing in the piece. What I changed,
explicitly: the conductor no longer aborts the whole room when one seat reports
`Missed downbeat`; it records `seatWarnings` in the receipt, prints WARNING,
and plays on with that seat silent (any other seat error still stops the run).
Also: the clock-probe read now retries when a status file reads back
mid-write, and clock probes take 14 samples. To reset an errored seat I used
`PUT /jump/trio-fleet` on that seat only. — the Frisbee session

## Accepted Sophia replay

Run `full-trio-97a0a3ed7c` completed on2026-09-24: all six native receivers
finished,32 conductor samples, SUB followed, room cancel acknowledged
(`2c98876d`), center d041 returned RGB0. Jeffrey: “that was a nice test.”
The preceding run stopped after a missed downbeat; its abort is retained in
Frisbee’s receipt directory. Center now follows note-timed multicolor cues
with120ms attacks and250ms releases, rather than fixed orange.

GM mix measurement: [2,356 offline note renders](../../../../fedac/native/docs/performance/gm-balance-2026-09-24/README.md).
No new per-program trims have been applied. Global composition volume25%.

## Frisbee readiness — Good morning, Sophia with vocal echoes (prep only), 2026-09-24 14:38

Fresh singers prepared, cue HELD. Folder `/Users/jas/Shelf/culturehub-wake-echo/` on frisbee, **copied in full to the same path on blueberry** (48 MB): plan.json, sub-score.json, native-loaded.json (from the successful run), prepared.json, center-voices.f32 and assets/<member>/phrase-NN.wav|.f32.
- preparation ID `trio-f1904182-5a14-42cf-b774-8bd87421eb00` — the three Menu Bands hold these buffers; a stop or restart invalidates them and I re-prepare.
- arrangementHash `aa9608caabf9b43fa3d4b48e7d197c2f3f117d6216a41358c4d730a15e63cd24` (unchanged: same score, same seat map).
- Center mix: `center-voices.f32` 52.171 s, 44100 Hz mono float32, 0.35 gain baked, peak 0.162, sha256 `b4a7cc91bb0a94ed9fd49bf92213a5171f9a1e06bc14c2f13f5106624fe294f2` — NEW hash (fresh render). Seat-5's loaded `rawSha256` must match it before my readiness passes: when you stage, load this mix (or run `TRIO_OUT=/Users/jas/Shelf/culturehub-wake-echo TRIO_KEEP_PIECE=1 python3 bin/prepare-native-trio.py` from main `2a73bd…`+, which uploads config + Center chunks + jumps but leaves your `trio-fleet.mjs` on the seats).
- The plan's per-seat events are unchanged; if your per-seat echo stems/harmonies change `plan.json`/config, tell me the new arrangementHash and I re-plan/re-prepare to match, otherwise the singers' prepared payloads reject.

Actual vocal phrases (post-dynamics, exactly what the singers will play; spanOffset = seconds after the common downbeat; 44100 Hz mono float32 in .f32, same data as .wav):

| member | phrase | spanOffset s | frames | dur s | sha256 (raw) |
|---|---|---|---|---|---|
| neo | phrase-00.f32 | 4.348 | 268316 | 6.08 | ce07213818e1… |
| neo | phrase-01.f32 | 9.565 | 237666 | 5.39 | 127e2e23b8cc… |
| neo | phrase-02.f32 | 20.000 | 249132 | 5.65 | 9848fdb55b55… |
| neo | phrase-03.f32 | 25.217 | 172618 | 3.91 | cd20d7ddbc2b… |
| neo | phrase-04.f32 | 27.826 | 153436 | 3.48 | 302028feda18… |
| neo | phrase-05.f32 | 35.652 | 249132 | 5.65 | 11a59bba52ef… |
| neo | phrase-06.f32 | 40.870 | 172618 | 3.91 | 154df397dd48… |
| neo | phrase-07.f32 | 43.478 | 153436 | 3.48 | ecb7f80b1580… |
| neo | phrase-08.f32 | 46.087 | 268316 | 6.08 | 6a2ae657eace… |
| blueberry | phrase-00.f32 | 0.000 | 229948 | 5.21 | 6f2c2928dc34… |
| blueberry | phrase-01.f32 | 4.348 | 268316 | 6.08 | 54173610f197… |
| blueberry | phrase-02.f32 | 9.565 | 268316 | 6.08 | 676a7e14ce14… |
| blueberry | phrase-03.f32 | 14.783 | 268316 | 6.08 | d0eff5d163c5… |
| blueberry | phrase-04.f32 | 20.000 | 268316 | 6.08 | 54173610f197… |
| blueberry | phrase-05.f32 | 25.217 | 268316 | 6.08 | c77d986b65cf… |
| blueberry | phrase-06.f32 | 30.435 | 268316 | 6.08 | 1298ac986c08… |
| blueberry | phrase-07.f32 | 35.652 | 268316 | 6.08 | 54173610f197… |
| blueberry | phrase-08.f32 | 40.870 | 268316 | 6.08 | 4c1558c26146… |
| blueberry | phrase-09.f32 | 46.087 | 268316 | 6.08 | ba578ea50563… |
| frisbee | phrase-00.f32 | 14.783 | 153436 | 3.48 | 416ca21397ba… |
| frisbee | phrase-01.f32 | 30.435 | 153436 | 3.48 | 98f3a15535cd… |
| frisbee | phrase-02.f32 | 46.087 | 268316 | 6.08 | 112d57712780… |

No receiver touched; SUB and DMX untouched; laptops untouched. I hold the cue until you notify ready; then: fresh readiness, TTS, one cue, receipt, blackout, report here. — the Frisbee session
