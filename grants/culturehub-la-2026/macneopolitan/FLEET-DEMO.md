# Trio + CultureHub fleet demo

Requested September 22, 2026, across prox:pen (Trio) and prox:dub (fleet).

The next requested demo must include the full surround system and all three singers together, with actual voices mirrored through the fleet. A partial center/sub test does not satisfy this request. Four ring nodes were still offline at the latest check. The three Menu Band singers remain the performance source. Reinforce their actual rendered voices through Center 6, add a composed bass layer through the Windows sub, and add restrained ornamental notes and sine beds through ACOS nodes. All parts must share the Trio conductor’s downbeat, stop, and phrase timing. Keep Concert mode fullscreen throughout playback.

## Verified installation

- Center Rear: node 1 / seat 0, http://192.168.1.237:80. Ready, Concert mode, mono-left. Seat saved to USB. Port changed from 8080 to 80 in fleet.json.
- Held Center: node 6 / seat 5, http://192.168.1.239:80. Ready, Concert mode, vertically flipped display, mono-left.
- Windows SUB: 192.168.1.67, localhost:8788 receiver, route both, level 100%, armed. Control through existing RustDesk session only.
- DMX: d001 left front, d011 right front, d021 left rear, d031 right rear. Six channels each. Existing Windows bridge v4 accepts note attack/decay envelopes. Mac cue endpoint localhost:8790/command.
- Current fleet list: /Users/jas/.ac-os/culturehub/fleet.json. Presence publisher: live-presence.py in that folder.
- Existing shared center/sub/light test: /Users/jas/.ac-os/culturehub/light-monitor/test-concert-sync.py. This is a separate 19.2-second test, not a Trio integration.

## Integration points found in current source

- bin/trio.mjs computes one downbeat epoch and corrects remote Mac clock skew before posting Menu Band play payloads.
- MenuBandSinger.swift returns SungRender(buffer, spanOffset, duration). AppDelegate.swift schedules each rendered line at downbeatEpoch + spanOffset after applying the line gain. Relay this exact audio, not newly synthesized imitations of another member’s voice.
- Rendering remains on each member’s own machine. A receiver may reproduce that member’s audio for reinforcement.
- ACOS sound.sample.loadData exists, but sample.play starts immediately; it has no timestamp argument. It must not be described as sample-accurate network playback. A timestamped receiver path and buffering need implementation/measurement.
- The Windows SUB server currently loads the Note(s)pat(ial) score at startup and rejects a different score duration. It needs an explicit arrangement-load/arm path before it can follow Trio bass. Do not fake the old score duration to run a different piece.
- Native worktree: /Users/jas/ac-worktrees/culturehub-audio-master. Menu Band / Trio source: /Users/jas/aesthetic-computer.

## Next demo acceptance

1. Transfer actual rendered singing phrases ahead of their scheduled onset to Center 6, with independently adjustable reinforcement gain. Measure alignment against the originating Mac.
2. Derive a bass part from the Trio harmony, separate from the vocal reinforcement; load it into the SUB before arming.
3. Add a quiet sine bed on Center Rear and sparse ornaments on available ring nodes, with independent layer levels.
4. Drive lights from those same note events and envelopes; retain square-only Concert displays, hidden pointer, and fullscreen Windows output.
5. One stop command cancels queued audio on every participating device and blacks out lights. Missing/unready devices must be reported before starting.
6. Verify the combined performance in the room. Current tests establish the individual fleet paths, not completed Trio integration or calibrated acoustic synchronization.

The Trio Metal renderer is being changed in prox:pen. Its recent session reported an ongoing rebuild for cached audio latency and reduced hidden icon redraw. Avoid replacing its current AppDelegate.swift or installing an older Menu Band build.

## Trio bridge handoff — prox:pen, September 22

Playback is held. No singers-only or combined demo will run during integration.
Current Trio baseline is the intimate-dynamics Metal build, SHA-256
`215cba4e0a52de2f15adde8af93bbc51e8e18c22cdc0b2a4e58944dd11fad63a`, installed on all three.
The user reports repeated permission-settings prompts on Frisbee despite existing
permission toggles; pen is tracing this before the next install.

Trio owns: silent preparation inside each member's MenuBand, exact post-dynamics
PCM export and reuse for local playback, phrase hashes/offsets and viseme metadata,
and a fail-closed conductor readiness gate. Proposed fleet payload: an arrangement
ID + SHA-256, bpm/duration, three member manifests (float32 mono WAV phrases,
spanOffset seconds relative to the common downbeat, frame count/sample rate/hash),
plus timestamped bass/sine/ornament/DMX events derived from the same notes. Center
must acknowledge every audio asset and timestamped queue capability before arming.

Coordination needed from dub in this file: exact receiver load/ready/clock/arm/stop
schema (including Windows SUB via the approved RustDesk/local bridge), ownership
of timestamped native sample buffering, and readiness of all six surround seats.
Pen will not treat an HTTP 200 alone as a readiness acknowledgment, will not use
the old SUB duration, and will leave d511/d041 inactive pending confirmation.

## Live checkpoint 2026-09-23 01:06 UTC

All SIX ACOS nodes are online and runtime-verified gilded-salmon-reef-mono, mono=true, monoOutput=left, mastering enabled, microphones closed. Canonical current ports are in /Users/jas/.ac-os/culturehub/fleet.json (most now 8080; Center 6 stays 80). /Users/jas/.ac-os/culturehub/audio-audit-all-six.json contains actual API evidence. USB persistence is continuing in fix-audio-fleet.py; do not interrupt its writes. New center DMX is Chauvet Wedge Tri at confirmed d041, mode still unconfirmed; requested 3-CH. White PAR-channel test did not light it. Four original PARs remain confirmed.

## Trio implementation update — preparation only

- The repeated Frisbee prompt was traced to `StickiesBridge.start()` requesting
  Accessibility on every app launch. It now checks/polls silently. No TCC reset
  or permission-toggle changes. This fix is building with the preparation API.
- `bin/fleet-trio.mjs plan|prepare|check --out=/Users/jas/Shelf/culturehub-trio`
  never starts playback. `plan.json` is available now: 204 timestamped bass,
  sine-bed, ornament and DMX events, 43.636… seconds, all six seats required.
- MenuBand `fleetPrepare` silently renders its own pinned voice, applies the same
  dynamics function as ordinary singing, retains those exact buffers, and exports
  float32 WAV **and raw little-endian float32** for each phrase. `fleetReady`
  reports the live app instance/cache; `preparedId` on a later play payload reuses
  the retained buffers and rejects mismatched payloads. Stop invalidates the cache.
- After silent preparation, `prepared.json` will list all phrase hashes/offsets
  and `center-voices.f32` will contain a sample-aligned sum of the actual three
  vocal parts at independently set reinforcement gain (currently 0.35). No new
  TTS is performed on Center. Raw phrase files remain available for separate banks.
- Proposed receipt contract is implemented/tested in `bin/trio-fleet-plan.mjs`:
  `schema: trio-fleet-ready-v1`, receiverId (singer-neo, singer-blueberry,
  singer-frisbee, seat-0…seat-5, sub, dmx), matching arrangementHash, ready:true,
  phase:ready, instance, observedAt (epoch seconds, <=15 seconds old),
  clockUncertaintyMs <=20, capabilities containing timestamped-start and
  cancel-queued. Seat-5 additionally needs actual-vocal-pcm and all 51 raw phrase
  hashes in assetHashes; SUB must acknowledge exact duration; DMX activeAddresses
  must include 1/11/21/31. These are contract checks, not receiver acknowledgments.
- `trio-chorus-doowop.mbscore` now has requiresFleet:true. The old standalone
  `trio.mjs` play path refuses this arrangement; dry/show remain available.

Dub: please reply here with the native/Windows receiver command schema and
ownership of global stop/clock/arm handling. Asset transfer, receiver arming,
global cancellation, acoustic alignment and combined playback are **not yet
verified**. No playback has been issued during this integration.

Pen saw the 01:06 UTC all-six checkpoint and will reread fleet.json at preparation
(the earlier plan used the then-current port 80 list). No USB writes or node
configuration changes from this side. d041 remains inactive in the Trio plan
until its channel mode is confirmed. Silent preparation will stage assets under
`/Users/jas/Shelf/culturehub-trio/`; this path is the handoff to the fleet receiver.

DMX plan events now include a `command` body matching the installed v4 HTTP
bridge (`color:rgb`, integer RGB/level <=128, duration, attack/decay envelope).
They are data only. The current `/command` handler is localhost-only and queues
immediate commands; a blackout command alone does not prove cancellation of
queued note events. A queue-clearing stop acknowledgment remains required.

Receipt checks also require Concert/fullscreen/hidden-pointer state on every
ACOS seat, fullscreen on SUB, and `centerMixHash` matching the staged combined
vocal PCM. This prevents accepting source files while a stale mix remains loaded.

## Trio assets ready — preparation only

Installed on all three: `da90ffb391cb19dfe64437217f32a26ae368bcd0eefcd1354904b14758a2b285`. Frisbee startup and silent preparation
showed no permission/settings window; no TCC toggles were changed.

`/Users/jas/Shelf/culturehub-trio/prepared.json` now exists. Preparation ID:
`trio-043ca7fa-9e58-49ae-a19e-60756bd2d3f7`. All 51 phrases rendered on their own Mac; WAV data equals raw PCM
byte-for-byte. Center mix SHA-256: `200dcf0525ceace9f277d7d0b50f555596d60f6671efc84419be3fbdcfced520`.
Its 0.35 reinforcement gain is **already baked in** (`bakedGain`); default
`playbackGain` is 1. Raw source phrases retain their original post-dynamics level.
471 independent mixed-sample checks passed; peak 0.4236.

Logs prove zero scheduled/played phrases on every Mac. No receiver assets have
been transferred or armed from this side, no lights commanded, no demo started.
Receiver protocol acknowledgment remains pending from dub; full-system readiness
is still false. Do not reuse a readiness receipt after an app restart or stop:
those invalidate the in-memory preparation.

## Fia arrival hold — Trio acknowledgment

Pen acknowledges: all six nodes are physically online, and dub owns the live
engine correction and USB persistence. No cue, node restart, or USB write from
Trio while that update is running. Preserve installed MenuBand
`da90ffb391cb19dfe64437217f32a26ae368bcd0eefcd1354904b14758a2b285`.

Actual vocal PCM is prepared, but **mirrored full-system playback is not ready**
and cannot yet be promised for Fia's arrival. Center still needs a loaded PCM
receipt and timestamped sample start; SUB needs Trio score loading; shared
arm/stop and DMX queue cancellation remain unimplemented. No singers-only
fallback will be presented as this demo. The latest engine-fix log ended with
held Center .239 unreachable; only dub's fresh audit can release the update hold.

Clock handoff: select one future controller UNIX epoch E only after every required
receiver is ready and the update hold is released. Capture a paired controller
wall/monotonic anchor (W0, M0), so the downbeat monotonic time is
M = M0 + (E - W0). Probe each native audioTime against controller monotonic
midpoints; its scheduled downbeat is M + measured offset. Each Mac uses
E + measured remote wall-clock skew, with preparedId
`trio-043ca7fa-9e58-49ae-a19e-60756bd2d3f7` and the exact prepared payload.
SUB must map the same E into its running WebAudio clock, with a fresh receipt.
Every score event uses that common downbeat plus its existing relative t; the
Center mix starts at t=0. Require fresh clock uncertainty <=20 ms and cancel-queued
on all 11 receivers. There is no live downbeat assigned yet.

Jeffrey also requested synchronized Mac display dimming before the entrance and
brightening as the faces enter. Trio is preparing this without cueing audio or
changing the fleet nodes during dub's update.

## Arrival preparation and screen entrance

All three running MenuBand instances freshly acknowledged their original cached
17 phrases each. Center PCM hash rechecked. `arrival-probes.json` records the
evidence in `/Users/jas/Shelf/culturehub-trio/`. No app restart or playback.

`clock-handoff.json` in that folder now contains exact cached singer payload
templates, preparation ID, Center mix metadata/hash, required receivers, clock
transforms and the brightness cue arguments. `startEpoch` is deliberately null;
full-system readiness is false. Replace that field with each receiver's mapped
future downbeat only after the hold and readiness requirements above are met.

Hardware brightness read/write probes passed on Neo (91.3%), Blueberry (100%),
and Frisbee (50%), writing each current value back without visible dimming.
`bin/trio-brightness.py` is available locally and installed at
`/tmp/trio-brightness.py` on Blueberry and Frisbee. It uses DisplayServices,
without Accessibility prompts or MenuBand installation changes. Staged cue:
fade to 28% from E−0.9 to E−0.3 seconds; brighten to 85% from E to E+0.55;
restore the original level at completion, SIGTERM, or its cancellation file.
The helper requires a future local-clock epoch and unique status/cancel paths.
The full-system conductor still needs to launch/cancel these helpers using the
same skew-corrected downbeat as singing. No brightness cue has run; this is a
verified hardware capability and staged entrance, not a combined demo readiness
claim. API reference: https://github.com/nriley/brightness/blob/master/brightness.c

Jeffrey explicitly approved a singers-only Fia demo after being told surround/SUB were not ready. Run fia-b8bf9fc898 cues only Neo, Blueberry, Frisbee and their brightness helpers. Fleet/update hold remains in effect; no mirrored voices claimed.

Jeffrey explicitly requested the 30-second Toxic wordless sketch on the three Macs for Fia. Run fia-toxic-3930912ad3 cues only Neo, Blueberry, Frisbee and their brightness helpers. Fleet/update hold remains in effect; no mirrored voices claimed.

Jeffrey requested more MenuBand echo/FX after the Toxic sketch. Trio is adding
optional scored echo (positive slide X), retaining existing space/pitch curves.
The 30-second sketch now has phrase-end echo throws and stronger room backing,
with restrained bass sends. A MenuBand install is underway on the three Macs;
it preserves Metal/dynamics and the silent permission check, but will invalidate
all old in-memory prepare IDs. Do not arm using the previous preparation receipts.
Trio will silently prepare the updated sketch in `/Users/jas/Shelf/culturehub-toxic-fx`.
No further playback is requested or being cued in this effects update.

Echo update installed on all three; matching binary SHA256
`3dda7ec807e62c1f4b53dc4750443ce81c62648c0ba7765f28bab5f16923a348`.
All 54 updated sketch phrases silently prepared under `trio-2498b0cd-8515-41a8-8efb-5d1e95788b77`
in `/Users/jas/Shelf/culturehub-toxic-fx`. Echo is applied live after prepared
PCM; the staged Center mix remains pre-FX. No playback during this update.

Jeffrey explicitly requested the 30-second Row, Row, Row Your Boat lyrical trio on the three Macs for Fia. Run fia-rowboat-1423ec469d cues only Neo, Blueberry, Frisbee and their brightness helpers. Fleet/update hold remains in effect; no mirrored voices claimed.

## One Big Voice combined test request

Jeffrey powered the system on and explicitly requested the next original Trio
song with the full backing and DMX. Fresh read-only probes find all six saved
ACOS seats including Center on port 80, gilded-salmon-reef-mono, ready on the old
774.4119-second score, microphones closed. fleet.json ports corrected from
those live /status responses. No fix-audio process remains active.
Windows SUB .67 is online but Audio Off; user has been asked to enable it.
DMX bridge is polling and four PAR addresses remain confirmed. No prox:dub
resolves in the live prompt registry; coordination remains in this file.

Trio is preparing One Big Voice: 60 seconds at 128 BPM, original historical
rap verses, octave-unison refrains, piano stabs and techno percussion.
New Metal character shapes have rendered successfully. Preparing/deploying
those faces invalidates older in-memory preparation IDs. No new song cued.

## One Big Voice — full-system cue, 2026-09-23 02:29 UTC

All 48 phrases are prepared with the installed character/echo build ec23209b9686b37ace7a6843cfdb52eefd91756d9700ce6d293f9a75553b55d8. The new runtime-only native receiver is loaded on all six saved seats. Center decoded the SHA-verified actual vocal mix (pre-FX, 0.35 baked gain), 59.880476 seconds; its prior assembly/checksum race was corrected before cueing. No USB writes. SUB now explicitly loads this arrangement, with a three-second conductor watchdog. Windows receiver .67 is armed, both channels, fullscreen Concert mode, at 25% level. DMX queue cancellation received a Windows bridge acknowledgment; active note routes remain 1/11/21/31 only.

The silent readiness gate passed for all 11 logical receivers. bin/run-full-trio.py now owns the common epoch, Mac clock correction, native audio-clock mapping, SUB arm/play, brightness entrance/restoration, note-timed DMX, watchdogs, and global stop. Native scheduling is simulation-frame quantized; no acoustic alignment has been measured. Jeffrey explicitly said to start; the combined run is being cued. Run receipts and live observations are saved under /Users/jas/Shelf/culturehub-one-big-voice/full-trio-*.json. This section supersedes older unimplemented-bridge and update-hold notes.

Combined run full-trio-1b086cf240 completed: all 48 singer phrases scheduled and played, no prepared-play rejections. All six native receivers remained online; Center reported actual deck playback, SUB reported PLAYING, and DMX cues acknowledged. Native start lateness measured 2.76–14.23 ms relative to mapped audio clocks. All three brightness helpers restored their original level. Global stop verified on all native receivers; SUB stopped and DMX blackout/cancel acknowledged. Acoustic sync remains uncalibrated: Center deck-position telemetry appears to advance faster than the conductor and needs investigation before claiming sustained alignment. MenuBand stop invalidated the singer caches; silently prepare again before the next run.

## Shutdown and next session

Jeffrey requested shutdown and commit/push after the successful full demo. All six ACOS nodes acknowledged runtime system.poweroff requests and subsequently stopped responding. MenuBand quit on Neo, Blueberry, and Frisbee. SUB and DMX controller services stopped after acknowledged silence/blackout. The three Macs remain powered on. Neo locked before Windows could be shut down through the authorized RustDesk UI; Windows audio is stopped, but its power-off was not performed.

Resume tomorrow: power on the six ACOS nodes; launch the installed MenuBand build on each Mac; restart the SUB server and the installed light-monitor server; use RustDesk to enable the correct LAN SUB page, both channels, fullscreen Concert. Regenerate/prepare `trio-one-big-voice.mbscore` through `bin/fleet-trio.mjs` into `/Users/jas/Shelf/culturehub-one-big-voice`, stage native receivers with `bin/prepare-native-trio.py`, then load `sub-score.json` through `/api/trio/load`. Run `bin/run-full-trio.py --check` before the next authorized cue. Old prepare IDs are invalid. Runtime pieces were not persisted to USB. `fleet/dmx-server.py` and `fleet/dmx-bridge.ps1` preserve the working bridge source; deploy beside the existing light-monitor state/assets.

First technical follow-up: investigate Center deck position versus conductor time; startup acknowledgment is not proof of sustained acoustic alignment. The room test was completed, but that calibration remains open. The combined-run receipt is `full-trio-1b086cf240.json` in the Shelf folder.

## Good morning, Sophia — full-system wiring, 2026-09-24 (frisbee session, for prox:ropotu)

Jeffrey asked for `trio-wake.mbscore` (Good morning, Sophia; 52.17 s, 69 bpm,
3/4) on the full room: three singers, six ACOS seats, Windows SUB and DMX.
The One Big Voice path is now reusable per song:

- `bin/fleet-trio.mjs plan|prepare|check --score=scores/<song>.mbscore --out=DIR`
  writes `plan.json` **and `sub-score.json`** (the bass layer in the
  sub-receiver's event shape, `hash` = arrangementHash, `dur` = plan duration,
  ready for `POST /api/trio/load`). Bars follow the score's
  `arrangement.meter`; a score's `fleet: {bass, bed, ornament, beatsPerBar}`
  names which member feeds each backing layer. Wake sets bass and bed on
  blueberry's cradle and ornaments on neo's line (frisbee has three phrases).
- `TRIO_OUT=DIR bin/prepare-native-trio.py` and `TRIO_OUT=DIR bin/run-full-trio.py [--check]`
  take the song folder from the environment (default is still the One Big
  Voice folder). `TRIO_ALLOW_PIECES=a,b` lets a seat be loaded from a piece
  other than culturehub-rehearsal/concert/trio-fleet.
- All three Macs run the same clean Menu Band build (installed 12:38 from
  `/tmp/mb-corner` on neo, origin/main `f9e3562577` + the corner ghost); the
  fleetPrepare/fleetReady hooks are in it. Note: a Menu Band silently ignores
  fleetPrepare while `sungSequenceActive` is set — post `stop` to all three
  before preparing after any play.

Staged, silently, in `/Users/jas/Shelf/culturehub-wake/` on neo:
`plan.json` (110 events: 38 sub, 20 bed, 7 ornament, 45 dmx; arrangement
`f85af6a95611…`), `sub-score.json`, `prepared.json` with preparation ID
`trio-ab3a6466-f8ca-407e-9a91-0281bc4bdbb6` (neo 9, blueberry 10, frisbee 3
phrases, all hashes verified), `center-voices.f32` (52.17 s, 0.35 baked gain,
peak 0.162). No receiver has been touched; nothing has played.

Room state read at 12:50 from neo (192.168.1.235): seats .237/.241/.242/.238
answer on port 80 and are showing `sub-check-laptop`; Center `.239` and ac4
`.236` did not answer on 80 or 8080; the DMX bridge on neo:8790 is up
(`all off`); the SUB server (neo:8788) is **not running**; Windows `.67`
(desktop-vidv882) not checked.

Needed from the fleet side before `run-full-trio.py --check` can pass:
1. All six seats reachable in `fleet.json` order and back on
   `culturehub-rehearsal` (or tell me the piece to allow), Concert mode,
   mono-left, microphones closed.
2. SUB server started on neo (`fedac/native/tools/sub-receiver/server.mjs`,
   PORT 8788), Windows receiver `.67` armed, both channels, fullscreen; then
   `POST /api/trio/load` with `/Users/jas/Shelf/culturehub-wake/sub-score.json`.
3. Confirm d041's channel mode or leave it inactive (the plan uses 1/11/21/31).
Then, from neo: `TRIO_OUT=/Users/jas/Shelf/culturehub-wake python3 bin/prepare-native-trio.py`,
`TRIO_OUT=… python3 bin/run-full-trio.py --check`, and the cue only on
Jeffrey's word. Re-prepare if any Menu Band restarts or stops.

Update 12:56: all six seats answer on port 80 (`orchestral-stress` on five,
`orchestral-stress-announced` on Center .239); DMX bridge live; SUB server
still down. Wake's `prepare-native-trio.py` will need
`TRIO_ALLOW_PIECES=orchestral-stress,orchestral-stress-announced` or the seats
back on `culturehub-rehearsal` first.

## Good morning, Sophia — full-system run, 2026-09-24 13:48

Conducted from frisbee (192.168.1.243) with ssh port-forwards to blueberry's
8791 SUB server and neo's 8790 DMX queue; seats on ropotu's verified map
(`fleet/CONNECTIONS.md`). Run `full-trio-8ff82a3c67`: all 22 phrases played,
six seats finished clean, SUB 25 % throughout, lights on 1/11/21/31, stop and
blackout verified, screens restored. Two aborts before it, both pre-cue: a
seat clock probe over the 40 ms limit (probes now 14), and a stray
`/tmp/inspect.py` on neo shadowing the stdlib for the brightness helper.
Acoustic alignment still uncalibrated, as before.

## Good morning, Sophia — second full-system run, for Will, 2026-09-24 14:27

Run `full-trio-97a0a3ed7c` from frisbee: all phrases and all seat events
played, start lateness under 16 ms, blackout verified. ropotu's staged center
module drove the held-center wedge d041 on the same timeline. One earlier
attempt (`f16d51c6c6`) was aborted by a 232 ms frame stall on seat-1 at its
downbeat; every seat shows 130–380 ms stalls during playback, so the conductor
now plays on with a missed seat silent (recorded as `seatWarnings`) instead of
stopping the room, and retries a status file that reads back mid-write. The
native piece's 100 ms downbeat tolerance is unchanged; the stalls are the open
question.

## Good morning, Sophia with vocal echoes — 2026-09-24 15:10

ropotu built six per-seat vocal echo stems from the 22 prepared phrases
(primary phrase cycling seats 0,1,2,4,3,5 with half- and full-beat echoes on
the next seats at 40 %/20 %) plus 116 quiet octave/fifth sines, arrangement
`d74cfbeb…`. Frisbee re-prepared the singers only (`bin/reprepare-singers.py`,
opaque faces, captions 110) and conducted `full-trio-7c86a0a43b`: all phrases
played, 138/141 seat events, center stem full length, blackout verified.
Jeffrey: "nice, that was great" — next: notation on the six laptops, brighter
screens, instruments and percussion for them.

## Good morning, Sophia v3 — the whole room, 2026-09-24 15:55

Run `full-trio-63b022dc49` from frisbee: the voice bounce mixed by the repo
pipeline, harmonies, music box, pad, percussion, notation and wedge on the six
laptops, all phrases played, 195/196 seat events, every seat at brightness
100, blackout verified. Open: seat-3 (.236) stalls close to a second every
run; the wedge telemetry file kept its last colour after stop (fixed in the
piece, not yet restaged).

## Femrag++ in the round — 2026-09-24 16:40, and the ac-venue MCP

ropotu staged six decoded deck stems, a sample-based SUB stem and 1080 light
cues under `/Users/jas/Shelf/femrag-spatial/` with its own conductor
(`run-fleet.py`); frisbee cued it: run `femrag-c8fe7ad368`, 146.67 s, all
seats to the end, no light skips, blackout verified. The room is now driven
through `toolchain/mcp/ac-venue-mcp.mjs` (`venue` in .mcp.json):
venue_prepare / venue_check / venue_cue / venue_result / venue_stop /
venue_claim; a song folder that carries its own run-fleet.py is cued with it.
