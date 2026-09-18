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
