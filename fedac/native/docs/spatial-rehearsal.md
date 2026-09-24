# Spatial rehearsal

Output-only CultureHub rehearsal for ACOS. Each laptop synthesizes the shared score against its own audio clock. The controller estimates offsets over Wi-Fi and starts all acknowledged seats together. Output latency and drift remain uncalibrated; microphones stay closed.

From the repository root:

```sh
node fedac/native/tools/compose-sine-line.mjs 1,2,3,4,5,6
node fedac/native/tools/spatial-rehearsal.mjs deploy --score sine-line HOST1 HOST2 HOST3 HOST4 HOST5 HOST6
node fedac/native/tools/spatial-rehearsal.mjs cue HOST1 HOST2 HOST3 HOST4 HOST5 HOST6
node fedac/native/tools/spatial-rehearsal.mjs stop HOST1 HOST2 HOST3 HOST4 HOST5 HOST6
```

Replace HOSTs with LAN IPs in the score's physical left-to-right order. Deployment preserves the score's seat numbers, backs up replaced files locally, verifies readback, and loads silently. Assignments are supplied by deployment; no device identity is baked into ACOS. The next OS build includes the piece, routing library, and three scores. Publishing this code does not release an OTA.

`cue --allow-missing` explicitly permits an incomplete host list: omitted seats remain silent in their original positions. Ordinary `cue` requires the complete ensemble. `beeps` runs a 20-second coordinated pulse test; `identify` sounds each seat in turn. `--score sine-ring` and `--score melodic-orbit` select the other scores at deployment.

Run the browser bridge and presence publisher in separate terminals, listing hosts in numeric seat order, including offline seats:

```sh
node fedac/native/tools/spatial-data.mjs --bind 0.0.0.0 HOST1 HOST2 HOST3 HOST4 HOST5 HOST6
node fedac/native/tools/spatial-presence.mjs HOST1 HOST2 HOST3 HOST4 HOST5 HOST6
```

The read-only bridge serves JSON at port 8787 `/api/seats` and SSE `/events`. The publisher writes connectivity to screens; presence expires after five seconds without updates. See [Will's handoff](will-spatial-audio.md) for browser integration.

Screens show shared placement, local identity/IP, battery and connectivity. Brightness requires audible digital output and at least 90% of a source's routed power. Equal-power partial mixes continue to sound without lighting adjacent screens.

Validation: `node --test fedac/native/tools/spatial-rehearsal.test.mjs`.

## September 18, 2026

- Connected the fleet, added output-only network clock scheduling and live telemetry.
- Added Monosine playback, Melodic Orbit, variable-speed Sine Ring and Sine Line.
- Added shared physical maps, battery, speaker-driven brightness and expired/offline connectivity.
- Expanded to six seats, corrected line order to 1 → 2 → 3 → 4 → 5 → 6, added LAN identity and focused brightness. Seat 5 subsequently lost connectivity at 2% battery; kept its place and continued with the remaining five.
- Included rehearsal assets in the next ACOS build and published the code checkpoint. No OTA release triggered.

## Two-laptop rehearsal on CULTUREHUB LA

The current line uses seat 1 at 192.168.1.236 on the left and seat 2 at 192.168.1.237 on the right. The bridge is now http://192.168.1.235:8787 and follows these two hosts. Generate a separate score without overwriting the six-seat version:

```sh
node fedac/native/tools/compose-sine-line.mjs 1,2 sine-duet
node fedac/native/tools/spatial-rehearsal.mjs deploy --score sine-duet 192.168.1.236 192.168.1.237
node fedac/native/tools/spatial-rehearsal.mjs cue 192.168.1.236 192.168.1.237
```

This version lasts 40.32 seconds with 48 notes and 24 left-to-right passes. Both devices confirmed speaker output with microphones closed.

## Rhythm tests

`compose-rhythm-bounce.mjs` writes a 112 BPM melody-and-percussion score that alternates on the beat. `compose-polyrhythm.mjs` writes `polyrhythm-bounce.nsscore`: three melody attacks against two drum pulses, independently bouncing across the line for 68.57 seconds. Deploy either with `--score rhythm-bounce` or `--score polyrhythm-bounce`, then cue both hosts. Per-lane `linePosition` arrays override the shared path. Focused brightness now requires an active note as well as 90% of that source's routed power.

## Soft Swing Echo

`compose-soft-swing.mjs` writes `soft-swing-echo.nsscore`: the same 3:2 phrase count with 60:40 swing, quiet sine taps, 55 ms melody attacks and 430 ms decays. Three diminishing delayed copies alternate across the speakers; three quiet reflections give a diffuse tail. These are synthesized score echoes, not a microphone or convolution reverb. The score includes 2.4 seconds for tails and lasts 70.97 seconds. Deploy with `--score soft-swing-echo` and cue both laptops.

## Octave Climb

`node fedac/native/tools/compose-soft-swing.mjs --climb` writes `octave-climb.nsscore`. Four 17-second phrases rise by an octave each time, spanning three octave lifts. High-register gain tapers gently. A sparse seventh source adds two-note answering phrases and faint delayed returns above the melody. The soft 3:2 swing, taps, spatial echoes and 70.97-second duration remain. Deploy with `--score octave-climb`, then cue the two laptops.

## Note(s)pat(ial) Native

`compose-notespatial-native.mjs` writes `notespatial-native.nsscore`: the 13:08 performance score for six seats in a ring, plus one file per section rebased to zero. `notespatial-native-check.mjs` prints the routed power per laptop in 20-second windows, the seat of each early hop, and dry-runs the piece against the full score. Form, cue points and risks: `grants/culturehub-la-2026/NOTESPATIAL-ARRANGEMENT.md`. Deploy with `--score notespatial-native` and hosts in seat order 1–6 clockwise from the front. `notespatial-native-render.mjs` renders any ring score as an mp4 heard from the center (KEMAR HRTF via ffmpeg afir, or `--fast` parametric), whole, by `--section n`, or by `--from/--to`.

## Voicings and effects (September 23)

`compose-notespatial-native.mjs --voicing mallets | native | gm` rebuilds the piece with each instrument family made of modal mallet stacks, the engine's whistle, harp and piano, or GM programs (`--set family=recipe` swaps one family; `--fx studio` adds room, drive, wobble and glitch ribbons per chapter). Scores carry the tag, so `--score notespatial-native-mallets-8-the-lift` deploys one chapter in one orchestra. The playback piece now passes an event's `gm` as `gmProgram` and applies a score's `fxRoom`, `fxDrive`, `fxWobble` and `fxGlitch` ribbons (with `seatFx` overrides) ten times a second, zeroing them on stop. Menu and listening notes: `grants/culturehub-la-2026/VOICINGS.md`.
