# CultureHub spatial audio — Will’s Codex handoff

Build a read-only web UI for the current two-machine ACOS rehearsal on **CULTUREHUB LA** Wi-Fi. **Seat 1 is left; seat 2 is right.** Both are online and synthesize their own speaker output from the shared spatial score. Microphones must stay closed; do not request microphone access, record audio, or issue device commands.

The rehearsal host provides a read-only bridge at **`http://192.168.1.235:8787`**. Start with `GET /api/seats` or subscribe to `/events`; no source checkout or credentials are needed to consume the running bridge. Addresses are local to this Wi-Fi.

## Connect

Current rehearsal addresses, September 18, 2026:

| Screen label | JSON `seat` | Name | HTTP base |
| --- | --- | --- | --- |
| Seat 1 — left | 0 | `ac-device` | `http://192.168.1.236` |
| Seat 2 — right | 1 | `ac5` | `http://192.168.1.237` |

Both currently run build `dusted-mantella-tideline`. Only these two machines are in the active bridge and presence feed.

Use explicit IPs. Read each snapshot’s `machineName` and `ip`, also shown on its screen. DHCP addresses can change; verify the runtime identity and seat assignment whenever connecting. Preserve seat numbers when a device goes offline; do not renumber the remaining laptops.

```sh
curl --fail --max-time 2 http://192.168.1.237/status
curl --fail --max-time 2 http://192.168.1.237/pieces/spatial-rehearsal-status.json
```

| Read endpoint | Meaning |
| --- | --- |
| `/status` | Runtime `{name, build, piece, ip, uptime, port}`. Check that `piece` is `spatial-rehearsal`; `uptime` is LAN service uptime in seconds. |
| `/pieces/spatial-rehearsal-status.json` | Latest rehearsal snapshot, rewritten about every 100 ms while the piece runs. |
| `/pieces/spatial-rehearsal-config.json` | `{seat, seats, maxSeconds}`. Use the live snapshot’s `duration` for the current run length. |
| `/pieces/spatial-rehearsal.nsscore` | Shared score JSON: duration, lanes/events, and spatial control ribbons. Fetch once per score change. |
| `/pieces/spatial-rehearsal.mjs` | Deployed piece source, including bundled deterministic world math. Read as text for implementation reference; do not execute fetched source automatically. |

These reads need no credentials. Keep this integration to GET requests.

## Read the snapshot

| Field | Interpretation |
| --- | --- |
| `machineName`, `ip` | Device name and current IP, also displayed on its screen. |
| `seat`, `seats` | Original zero-based seat index and registered ensemble size. Currently two seats, indices 0–1. |
| `geometry`, `seatOrder` | Runtime layout and ordered zero-based seat indices. Currently `"line"` and `[0,1]`: seat 1 left, seat 2 right. Build the layout from these fields. |
| `connectivity` | `{stale, seats: [{seat, state}]}` from a separate controller presence publisher. States are `online`, `offline`, `unstable`, `unknown`, or `error`. Honor `stale`; this is separate from the bridge’s polling health. |
| `phase` | `ready`, `prepared`, `countdown`, `playing`, `finished`, or `error`. |
| `mode` | `beeps` or `score`. Beep mode gives each laptop the same scheduled pulse. |
| `runId` | Preparation identifier for the run; `null` before preparation. Compare score times only within the same run. |
| `duration`, `scoreDuration`, `scoreName` | Current run length, full score length (both seconds), and score name. Read these dynamically; beep mode lasts 20 seconds. |
| `error` | Empty string normally; display nonempty errors. |
| `command` | Last processed command identifier. Useful context; not a sample sequence number. |
| `audioTime` | This device’s audio clock, in seconds. Origins differ between devices: never subtract one seat’s `audioTime` from another’s. |
| `origin` | Scheduled score start in that same device’s audio clock; `null` before scheduling. |
| `scoreTime` | Seconds relative to `origin`; `null` when unarmed, negative before the scheduled start. Compare between seats during the same playing run, allowing for unequal sample arrival times. A value alone does not prove playback: always inspect `phase`. |
| `output.left`, `output.right` | Current digital speaker amplitudes. Use these for live meters and screen brightness; these are not calibrated sound-pressure measurements. |
| `glow` | Screen brightness, 0–1. Lights only when a routed source has at least 90% power on this seat (`seatGain² ≥ 0.9`) and output amplitude exceeds 0.002. Use it to mirror the laptop’s focus display. |
| `outputPeak` | Largest observed output amplitude since preparation. Historical peak, not a live meter. |
| `sources` | Live source positions and this seat’s gains; see Spatial view below. |
| `beepCount` | Pulses emitted in the current beep run; resets on preparation. |
| `maxFrameGap` | Largest simulation step in audio-clock seconds since preparation. Shows scheduling stalls, not measured acoustic skew. |
| `networkHalfRttMs` | Controller’s half-round-trip timing estimate, in milliseconds; may be `null` before preparation. |
| `timing` | Human-readable timing limitation. Current scheduling estimates audio clocks over Wi-Fi; output latency and drift are uncalibrated. |
| `microphone.hot`, `microphone.recording` | Both must be `false`. Display any unexpected true value; do not try to calibrate using microphones. |
| `battery` | `{percent, charging, status, minutesLeft}`. Negative percentage or minutes means unavailable. `minutesLeft` is an estimate when discharging. |
| `screen` | `{width, height}` in runtime pixels. |

Treat missing or additional fields defensively. This is a rehearsal interface without a versioned schema. The telemetry contains numeric synthesis state, not a microphone recording or an audio stream.

## Browser bridge

| Bridge endpoint | Response |
| --- | --- |
| `http://192.168.1.235:8787/api/seats` | Latest `{updatedAt, seats: [...]}` snapshot. |
| `http://192.168.1.235:8787/events` | Server-Sent Events named `seats`, carrying the same JSON snapshot. |
| `http://192.168.1.235:8787/health` | Bridge health. Check seat freshness separately. |

Each item in `seats` is `{host, connected, stale, ageMs, receivedAt, data, error}`. `data` is the native snapshot described above; it is `null` before the first successful read. `updatedAt` and `receivedAt` are host-side ISO timestamps; `receivedAt` is the last valid fetch time. `ageMs` measures time since the last change in that seat’s `audioTime`, or is `null` before any sample. `connected` describes the latest poll; `error` is `null` on success. Failed reads retain the last valid `data`.

The bridge polls each seat at 5 Hz with at most one request in flight per seat; it marks data stale on a failed read or when the audio clock stops advancing for two seconds. HTTP success alone does not establish that the piece is running. Preserve host associations and show connection failures and stale data separately. Exclude stale data when choosing the current score or layout.

The bridge sends `Access-Control-Allow-Origin: *`. Save this as `index.html`, serve its directory with `python3 -m http.server 8000 --bind 127.0.0.1`, and open `http://127.0.0.1:8000` for a minimal live reader:

```html
<!doctype html>
<meta charset="utf-8">
<title>Spatial audio data</title>
<p id="connection" role="status"></p>
<pre id="seats"></pre>
<script type="module">
const connection = document.querySelector('#connection');
const seats = document.querySelector('#seats');
function renderFleet(data) {
  connection.textContent = '';
  seats.textContent = JSON.stringify(data, null, 2);
}
function showBridgeDisconnected() {
  connection.textContent = 'Bridge disconnected; reconnecting. Last data below.';
}
const base = 'http://192.168.1.235:8787';
async function loadSnapshot() {
  const response = await fetch(base + '/api/seats', {
    cache: 'no-store', signal: AbortSignal.timeout(2000),
  });
  if (!response.ok) throw new Error(`Bridge HTTP ${response.status}`);
  return response.json();
}

loadSnapshot().then(renderFleet).catch(showBridgeDisconnected);
const feed = new EventSource(base + '/events');
feed.addEventListener('seats', event => renderFleet(JSON.parse(event.data)));
feed.onerror = showBridgeDisconnected; // EventSource reconnects automatically.
// Call feed.close() when the view is disposed.
</script>
```

An HTTPS page may block local HTTP through mixed-content or local-network restrictions. If browser access fails, run a Node proxy in your own UI server and consume it from the same origin. Browser permission to access the local network is distinct from microphone access; this UI never needs a microphone.

Direct browser reads from the laptops have an additional limitation: `/status` sends a CORS header, but `/pieces/*` currently does not. JSON files also arrive with `Content-Type: text/javascript`; a server-side client can read the body and parse JSON.

If running your own bridge from a source checkout:

```sh
node fedac/native/tools/spatial-data.mjs --bind 127.0.0.1 --port 8787 \
  192.168.1.236 192.168.1.237
```

For a custom polling implementation, fetch with `cache: 'no-store'`, bounded timeouts, and no overlapping requests per host. Files can be read during a write: keep the last valid JSON and retry on the next tick. Track host receive time and when `audioTime` last advanced; a stale file can still return HTTP 200. Verify `/status` and seat assignments on connection and periodically.

Show registered seats with live output, battery, phase, score time, data age, and microphone state; retain offline seats visibly. Use `glow` for focused-seat brightness and `output` for meters. Keep stale values visibly stale. Do not label differences between asynchronously received snapshots as measured speaker synchronization.

## Spatial view

The current composition is **Soft Swing Echo** (`soft-swing-echo.nsscore`): 112 BPM, 70.97 seconds including tails, six sources, and 64 cycles. Three melodic attacks occupy the same time as two soft tap pulses, with 60:40 swing. Melody and taps bounce independently; three diminishing echo sources alternate across the laptops and a quiet reflection source adds a diffuse tail. These are synthesized repeats, not microphone input or convolution reverb. Read each source's live position independently. The deployed score remains readable at `/pieces/spatial-rehearsal.nsscore`; derive duration, sources and layout from fresh data. Earlier Sine Line, Rhythm Bounce and 3-against-2 scores remain available.

Use `data.sources` directly to draw the world. Each source contains `{lane, name, color, position, seatGain, active}`. `lane` is its zero-based score index and `color` is an RGB array. In line geometry, `position.line` runs from 0 (leftmost seat) to 1 (rightmost); `x` spans −1.6 to 1.6 in virtual units, and `y`/`z` are zero. `angle` is a zero placeholder here: do not use it to draw the line. In ring geometry, `position.angle` is in radians. Coordinates describe the score, not measured room positions.

`seatGain` is the source’s 0–1 equal-power panning gain for the reporting seat. Line geometry hands off between neighbors in `seatOrder` without wrapping the last seat to the first. `active` means that source has a currently scheduled synthesized event on the seat during score playback; its gain may still be zero. Check both fields for participation, and use `output.left/right` for actual digital output level.

Playback is currently output-only: a controller estimates each local audio clock over Wi-Fi, prepares a future local start on every seat, then starts the ensemble. The web UI observes this system. Its network feed must not become the audio scheduling clock.

## Rehearsal changes — September 18, 2026

- Established output-only synthesis, coordinated starts, battery displays, and a read-only HTTP/SSE feed; microphones stay closed.
- Explored Monosine and Melodic Orbit, then moved to Sine Line with four active seats while seat 1 was offline.
- Restored seat 1 on a newer build and added seat 6 at `.87`; expanded the line to 1 → 2 → 3 → 4 → 5 → 6 and Sine Line to 97.92 seconds. Added device names/IPs and focused-seat brightness.
- Seat 5 later went offline after reporting 2% battery during the six-seat rehearsal.
- Moved to CULTUREHUB LA Wi-Fi for a two-machine rehearsal: seat 1 `ac-device` at `192.168.1.236`, seat 2 `ac5` at `192.168.1.237`, both on `dusted-mantella-tideline`. The controller is `192.168.1.235`; Sine Line now uses the 40.32-second duet, with only these two seats in the bridge and presence feed.
- Current work deploys local pieces and tracks source changes in Git. No OTA release was requested.

- Added Rhythm Bounce and 3-against-2 polyrhythm tests. Per-source line paths allow independent movement; idle sources no longer trigger focused brightness.

- Softened the rhythm test and added Soft Swing Echo with longer attacks/decays, swing and spatial echo tails.
