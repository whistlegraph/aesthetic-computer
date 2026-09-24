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

## Pending boot update — September 24, 2026

Replace the spoken “connected to CULTUREHUB LA” announcement with a short,
quiet sine-wave refrain when a laptop first connects to that network after
boot. Play it once per boot; changing pieces or reconnecting must not repeat
it. Keep the personalized Jeffrey startup greeting disabled, preserve the
saved seat and arrangement, and persist the update on both USB boot copies.

The connection announcement currently comes from the native runtime's Wi-Fi
connection transition in `src/ac-native.c`, separately from the personalized
startup greeting. Suppress that speech as part of the change; adding a melody
in the performance wrapper alone would leave the speech audible.

**Deferred at Jeffrey's request:** log this now and do other work before the
next reboot. No Wi-Fi melody change has been installed and no further reboot
is scheduled. The six laptops have already rebooted successfully into the
updated arrangement and performance. Deployment receipts and rollback notes:
`.tmp/notespatial-2026-09-24/`.

### Live battery display and warning

`lib/battery-watch.mjs` now keeps the percentage visible above the concert
view and draws a flashing bell at 10% or below. The sine ding repeats every
`max(1, percent * 3)` seconds: 30 seconds at 10%, 15 at 5%, 3 at 1%, and
1 at 0%. It stops above 10%; a low reading still warns while charging.

Loaded live on all six as `notespatial-battery-live`, with the score and
performance hashes unchanged. Timing/threshold tests pass via
`node --test fedac/native/tools/battery-watch.test.mjs`. Include this wrapper
and helper in the next persistent USB update alongside the deferred Wi-Fi
refrain; the current USB boot archive still contains the earlier wrapper.
No reboot was performed for this live battery change.
