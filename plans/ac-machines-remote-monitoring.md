# AC Machines — Remote Monitoring & Dashboard

**Goal:** Let a user see all their running ac-native devices from `aesthetic.computer/machines`, with live status, logs, crash reports, and OS version — eliminating the need to physically plug/unplug USBs to debug.

**Owner:** @jeffrey

---

## 1. What We Already Have

| Layer | Existing | Notes |
|-------|----------|-------|
| **ac-native networking** | WiFi, WebSocket (`ws-client.c`), UDP, HTTP fetch (curl) | Already connects to `wss://chat-system.aesthetic.computer` for chat |
| **SockLogs** | Session server `/socklogs` WebSocket route | Device→viewer log relay, but no persistence, no auth, no user binding |
| **OTA version tracking** | `releases.json` on S3, `system.version` in JS | ac-native knows its own build name + git hash + build timestamp |
| **Local logging** | `ac_log()` → `/mnt/ac.log`, perf ring buffer `/mnt/perf/*.csv` | Crash-resilient 30s chunks, frame-level metrics |
| **Session server** | WebSocket hub with heartbeat, Redis pub/sub, client tracking | Profile streams, status dashboard, presence — all patterns we can reuse |
| **MongoDB** | `ff1Devices` collection for device pairing, `boots` for telemetry | Upsert + timestamp pattern is the template |
| **machines.json** | `aesthetic-computer-vault/machines.json` via `ac-host` fish function | Static SSH host list — this becomes the seed data concept |

---

## 2. Architecture Overview

```
┌─────────────────┐         ┌──────────────────────────┐
│  ac-native      │         │  session-server           │
│  (bare metal)   │────WSS──│  /machines WebSocket      │
│                 │         │                            │
│  heartbeat +    │         │  • auth (user sub)         │
│  log stream +   │         │  • heartbeat tracking      │
│  crash reports  │         │  • log relay to viewers    │
└─────────────────┘         │  • Redis presence pub/sub  │
                            └────────┬───────────────────┘
                                     │
                              ┌──────┴──────┐
                              │             │
                         ┌────▼────┐   ┌────▼────┐
                         │ MongoDB │   │  Redis   │
                         │         │   │          │
                         │ ac-     │   │ live     │
                         │ machines│   │ presence │
                         │ ac-     │   │ + log    │
                         │ machine │   │ buffer   │
                         │ -logs   │   │          │
                         └─────────┘   └──────────┘
                              │
                    ┌─────────┴──────────┐
                    │  Netlify function   │
                    │  /api/machines      │
                    │  (REST for piece)   │
                    └─────────┬──────────┘
                              │
                    ┌─────────▼──────────┐
                    │  machines.mjs      │
                    │  (AC piece)         │
                    │  aesthetic.computer │
                    │  /machines          │
                    └────────────────────┘
```

---

## 3. Data Model

### MongoDB: `ac-machines` collection

```javascript
{
  _id: ObjectId,
  user: "auth0|...",              // Owner (from token)
  machineId: "notepat-a1b2c3",   // Unique device ID (generated on first boot, persisted to USB)

  // Identity
  label: "X1 Nano",              // User-assignable friendly name
  hostname: "ac-native",         // From system

  // Version
  version: "notepat fb98b847-2026-03-13T10:30",  // system.version string
  buildName: "notepat",
  gitHash: "fb98b847",
  buildTs: "2026-03-13T10:30",

  // Hardware
  hw: {
    display: "2160x1350",
    sampleRate: 192000,
    arch: "x86_64",
    keyboard: "NuPhy Air60 HE"   // If detected
  },

  // Network
  ip: "192.168.1.75",
  wifiSSID: "HomeNetwork",

  // Runtime
  currentPiece: "notepat",        // Which piece is currently running

  // Status
  status: "online" | "offline" | "crashed",
  linked: true,                   // Has valid device token (false = unlinked/fresh device)
  lastSeen: Date,                 // Updated by heartbeat
  lastBoot: Date,                 // When device last started
  uptime: 3600,                   // Seconds since boot
  bootCount: 42,                  // Lifetime boots
  tokenGeneration: 1,             // Bumped to revoke all device tokens

  // Timestamps
  createdAt: Date,
  updatedAt: Date
}
```

**Indexes:**
```javascript
{ user: 1, machineId: 1 }  // unique compound
{ user: 1, lastSeen: -1 }  // dashboard query
{ lastSeen: 1 }            // TTL or stale cleanup
```

### MongoDB: `ac-machine-logs` collection

```javascript
{
  _id: ObjectId,
  machineId: "notepat-a1b2c3",
  user: "auth0|...",

  type: "log" | "crash" | "perf" | "boot" | "update",
  level: "info" | "warn" | "error" | "fatal",

  message: "WiFi connected to HomeNetwork",
  data: { ... },             // Structured payload (perf metrics, stack trace, etc.)

  // For crash reports
  crashInfo: {
    signal: "SIGSEGV",
    lastLogLines: ["...", "..."],   // Last N lines from ac.log
    perfSnapshot: { ... },          // Last perf frame
    jsHeapMb: 12.4,
    activeVoices: 28
  },

  when: Date,                 // Device-side timestamp
  receivedAt: Date            // Server-side timestamp
}
```

**Indexes:**
```javascript
{ machineId: 1, when: -1 }            // Log history per device
{ user: 1, type: 1, when: -1 }        // User's crash reports
{ receivedAt: 1, expireAfterSeconds: 604800 }  // TTL: 7 days for logs, longer for crashes
```

---

## 4. Implementation Steps

### Phase 1: Device Identity & Heartbeat

#### 1a. Generate persistent machine ID on ac-native

**File:** `fedac/native/src/ac-native.c`

On first boot, if `/mnt/.machine-id` doesn't exist, generate a UUID and write it. Read it on every boot and expose as `system.machineId` in JS bindings.

```c
// In main(), after mounting /mnt:
char machine_id[64];
FILE *f = fopen("/mnt/.machine-id", "r");
if (!f) {
    // Generate: "notepat-" + 8 random hex chars
    snprintf(machine_id, sizeof(machine_id), "notepat-%08x", arc4random());
    f = fopen("/mnt/.machine-id", "w");
    fprintf(f, "%s", machine_id);
    fclose(f);
} else {
    fgets(machine_id, sizeof(machine_id), f);
    fclose(f);
}
```

**File:** `fedac/native/src/js-bindings.c`

Expose `system.machineId` alongside existing `system.version`.

#### 1b. Session server `/machines` WebSocket endpoint

**File:** `session-server/session.mjs`

Add a new WebSocket route (like SockLogs but with auth + persistence):

```
wss://session-server.aesthetic.computer/machines?role=device&machineId=X&token=Y
wss://session-server.aesthetic.computer/machines?role=viewer&token=Y
```

**Device flow:**
1. Connect with machineId + auth token (or a device-specific API key)
2. Send `register` message with version, hw info, IP, WiFi SSID
3. Send `heartbeat` every 30 seconds with uptime, perf summary
4. Send `log` messages for important events
5. Send `crash` on signal handler (best-effort before exit)

**Viewer flow:**
1. Connect with user's auth token
2. Receive initial state: all machines for this user + their status
3. Receive real-time updates: heartbeats, logs, status changes
4. Can send commands: `request-logs`, `request-perf`, `reboot` (future)

**Server logic:**
- On device `register`: upsert to `ac-machines` MongoDB collection
- On device `heartbeat`: update `lastSeen`, `uptime`, `status` in MongoDB; relay to viewers
- On device `log`: insert to `ac-machine-logs`; relay to viewers in real-time
- On device disconnect: set `status: "offline"` after 60s grace period (Redis timer)
- Use Redis pub/sub for cross-instance coordination (same pattern as existing profile streams)

#### 1c. ac-native connects on boot

**File:** `fedac/native/pieces/notepat.mjs`

In notepat's `boot()`, after WiFi connects, establish the machines WebSocket:

```javascript
// After WiFi is connected:
const machinesWs = system.ws.connect(
  `wss://session-server.aesthetic.computer/machines?role=device&machineId=${system.machineId}`
);

// Send registration
machinesWs.send(JSON.stringify({
  type: "register",
  version: system.version,
  hw: system.hw,
  buildName: system.hw.buildName
}));

// Heartbeat every 30s (in sim loop, throttled)
let lastHeartbeat = 0;
// ... in sim():
if (now - lastHeartbeat > 30000) {
  machinesWs.send(JSON.stringify({ type: "heartbeat", uptime: system.uptime }));
  lastHeartbeat = now;
}
```

**Note:** ac-native currently has a single WebSocket slot (`ws-client.c`). We need to either:
- **Option A:** Add a second WebSocket slot for machines (preferred — independent lifecycle)
- **Option B:** Multiplex over the existing chat WebSocket with message type routing

**Recommendation: Option A** — add `ws2` (machines WebSocket) in C. The ws-client code is self-contained; duplicating it with a second set of state variables is straightforward (~200 lines). Expose as `system.ws2.connect()` / `system.ws2.send()` in JS bindings.

### Phase 2: REST API for the Piece

#### 2a. Netlify function: `/api/machines`

**New file:** `system/netlify/functions/machines.mjs`

```javascript
// GET /api/machines — list user's machines with status
// GET /api/machines?machineId=X — single machine detail
// GET /api/machines?machineId=X&logs=true&limit=50 — machine + recent logs
// POST /api/machines — update machine label
// DELETE /api/machines?machineId=X — remove machine from dashboard
```

Pattern follows `ff1-devices.mjs`:
- Authorize via Bearer token
- Query `ac-machines` collection filtered by `user.sub`
- For online/offline status: check `lastSeen` vs now (>60s = offline)
- Join with `ac-machine-logs` for recent crash count

#### 2b. Netlify function: `/api/machine-logs`

**New file:** `system/netlify/functions/machine-logs.mjs`

```javascript
// GET /api/machine-logs?machineId=X&type=crash&limit=20
// GET /api/machine-logs?machineId=X&since=2026-03-13T00:00:00Z
```

Paginated log retrieval for the piece's detail view.

### Phase 3: `machines.mjs` AC Piece

**New file:** `system/public/aesthetic.computer/disks/machines.mjs`

#### UI Layout

```
┌─────────────────────────────────────────┐
│  MACHINES                    @jeffrey   │
├─────────────────────────────────────────┤
│                                         │
│  ● X1 Nano             notepat fb98b8   │
│    192.168.1.75    up 2h 34m    online  │
│                                         │
│  ○ ThinkPad X13         notepat 5c1d44  │
│    —                          offline   │
│                                         │
│  ● Living Room          notepat fb98b8  │
│    192.168.1.102   up 45m      online   │
│                                         │
├─────────────────────────────────────────┤
│  Last crash: none                       │
│  2 online · 1 offline                   │
└─────────────────────────────────────────┘
```

**Tapping a machine** → detail view:

```
┌─────────────────────────────────────────┐
│  ← X1 Nano                     online  │
├─────────────────────────────────────────┤
│  Version:  notepat fb98b847-2026-03-13  │
│  IP:       192.168.1.75                 │
│  WiFi:     HomeNetwork                  │
│  Uptime:   2h 34m                       │
│  Display:  2160x1350                    │
│  Audio:    192kHz                       │
│  Keyboard: NuPhy Air60 HE              │
├─────────────────────────────────────────┤
│  LOGS (live)                            │
│  10:30:01  WiFi connected               │
│  10:30:02  WebSocket connected           │
│  10:30:05  notepat boot complete         │
│  10:32:14  OTA check: up to date         │
│  ...                                     │
└─────────────────────────────────────────┘
```

#### Piece Structure

```javascript
let machines = [];
let selectedMachine = null;
let logs = [];
let liveWs = null;  // WebSocket for real-time updates
let scroll = 0;

async function boot({ net, params, user, handle, authorize, ui, screen, store }) {
  if (!user) { /* redirect to login */ }

  // Fetch machine list
  const token = await authorize();
  const res = await fetch("/api/machines", {
    headers: { Authorization: `Bearer ${token}` }
  });
  machines = (await res.json()).devices;

  // Connect viewer WebSocket for live updates
  liveWs = new WebSocket(
    `wss://session-server.aesthetic.computer/machines?role=viewer&token=${token}`
  );
  liveWs.onmessage = (e) => {
    const msg = JSON.parse(e.data);
    if (msg.type === "heartbeat") updateMachineStatus(msg);
    if (msg.type === "log") appendLog(msg);
    if (msg.type === "status") updateMachineStatus(msg);
  };
}

function paint({ wipe, ink, write, box, screen, help }) {
  wipe(20, 18, 28);  // Dark background

  if (selectedMachine) {
    paintDetail(arguments[0]);
  } else {
    paintList(arguments[0]);
  }
}

function act({ event: e, needsPaint, jump }) {
  // Handle tap on machine → selectedMachine = machine
  // Handle back button → selectedMachine = null
  // Handle scroll
  // Handle label edit
}

function leave({ store }) {
  if (liveWs) liveWs.close();
  store["machines:scroll"] = scroll;
  store.persist("machines:scroll");
}

export { boot, paint, act, leave };
export const noBios = true; // Full-screen piece, no BIOS chrome
```

### Phase 4: Crash Reporting

#### 4a. C-level crash handler

**File:** `fedac/native/src/ac-native.c`

Enhance `signal_handler()` to capture crash state before exit:

```c
void crash_handler(int sig) {
    // 1. Capture last 20 lines of ac.log into buffer
    // 2. Capture last perf frame
    // 3. Format as JSON crash report
    // 4. Best-effort HTTP POST via curl (5s timeout)
    //    POST to /api/machine-crash with machineId + crash data
    // 5. Also write crash report to /mnt/crash.json for next-boot upload
    running = 0;
}
```

#### 4b. Next-boot crash upload

In `notepat.mjs` boot, check for `/mnt/crash.json`. If present, upload it via `system.fetchPost()` and delete the file.

### Phase 5: Device Authentication (OTA Token)

Devices authenticate automatically via a token delivered during the OTA update flow — no manual key copying needed.

#### How it works

1. **`ac-os upload`** already authenticates the user (Bearer token → Auth0 userinfo) and writes to `releases.json` with the user's `sub` and `handle`.

2. **New: OTA delivers a device token.** When ac-native checks for updates (existing `system.fetch()` call to `releases.json`), the server also returns a **device token** — a signed JWT or HMAC-signed blob containing `{ user_sub, handle, issued_at }`.

3. **Token delivery endpoint** — new Netlify function:
   ```
   GET /api/machine-token?version=CURRENT_VERSION
   Authorization: none (authenticated by matching a known release uploader)
   ```
   Actually simpler: the token is baked into the OTA response. When `ac-os upload` finalizes, it generates a device token signed with `MACHINE_TOKEN_SECRET` (env var on Netlify + session server) and stores it alongside the release in `releases.json`:
   ```json
   {
     "latest": "notepat fb98b847-2026-03-13T10:30",
     "device_token": "eyJ...",  // HMAC-signed { sub, handle, iat }
     "releases": [...]
   }
   ```

4. **ac-native stores the token.** On OTA check (or first boot after flash), notepat.mjs reads `device_token` from the update response and writes it to `/mnt/.device-token` via `system.writeFile()`.

5. **Device uses token for machines WebSocket:**
   ```
   wss://session-server.aesthetic.computer/machines?role=device&machineId=X&token=DEVICE_TOKEN
   ```

6. **Session server validates** by verifying the HMAC signature with the same `MACHINE_TOKEN_SECRET`. No database lookup needed — the user sub and handle are embedded in the token.

#### Token lifecycle
- **Issued:** on every `ac-os upload` (new token per release)
- **Rotated:** automatically when device fetches a new OTA release
- **Revoked:** user can invalidate all tokens from the machines piece (bumps a `tokenGeneration` counter in MongoDB; session server rejects tokens with stale generation)
- **Fallback:** if no token exists yet (fresh device, never checked OTA), device operates in "unlinked" mode — still sends heartbeats but shows as "unlinked" in the dashboard until first OTA check

#### First-time pairing flow
For a brand new device that has never done an OTA check:
1. Device boots, gets WiFi, connects to machines WebSocket **without a token**
2. Session server accepts but marks as `unlinked` — no user association
3. Device checks for OTA updates → receives `device_token` → stores to `/mnt/.device-token`
4. Device reconnects machines WebSocket with token → now linked to user
5. Dashboard shows the machine instantly

### Phase 6: Remote Commands

The machines WebSocket is bidirectional — the server can push commands to connected devices.

#### Command flow

```
machines.mjs piece                session-server              ac-native device
     │                                  │                           │
     │  WS: { type: "command",          │                           │
     │        machineId: "X",           │                           │
     │        cmd: "jump",              │                           │
     │        args: { piece: "wand" }}  │                           │
     │ ─────────────────────────────►   │                           │
     │                                  │  WS: { type: "command",   │
     │                                  │        cmd: "jump",       │
     │                                  │        args: {...} }      │
     │                                  │  ────────────────────────►│
     │                                  │                           │
     │                                  │  WS: { type: "command-ack",
     │                                  │        cmd: "jump",       │
     │                                  │        status: "ok" }     │
     │                                  │  ◄────────────────────────│
     │  WS: { type: "command-ack",      │                           │
     │        machineId: "X",           │                           │
     │        status: "ok" }            │                           │
     │ ◄─────────────────────────────   │                           │
```

#### Supported commands

| Command | Args | What it does on device | Notes |
|---------|------|----------------------|-------|
| `jump` | `{ piece: "wand" }` | Load a different piece | Uses existing piece-loading infrastructure in notepat or a new multi-piece launcher |
| `reboot` | `{}` | `system("reboot")` | Writes `/mnt/.reboot-requested`, then reboots |
| `update` | `{}` | Trigger OTA check + apply | Same as existing OTA flow but triggered remotely |
| `request-logs` | `{ lines: 50 }` | Read last N lines of `/mnt/ac.log` and send back | For on-demand log fetch without live streaming |
| `request-perf` | `{}` | Send current perf snapshot (voices, heap, fps) | One-shot perf report |
| `set-label` | `{ label: "Living Room" }` | Update device's display name | Stored in `/mnt/.machine-label` + MongoDB |
| `echo` | `{ volume: 0.5 }` | Set echo/reverb wet mix | Direct parameter control |
| `wifi-scan` | `{}` | Return visible networks | For remote WiFi management |

#### ac-native command handler (notepat.mjs)

```javascript
// In the machines WebSocket message handler:
function handleCommand(cmd, args) {
  switch (cmd) {
    case "jump":
      // Future: multi-piece support
      // For now: could reload notepat with different params
      // or: system.exec("jump", args.piece) — needs new C binding
      sendCommandAck(cmd, "ok");
      break;
    case "reboot":
      sendCommandAck(cmd, "ok");
      system.writeFile("/mnt/.reboot-requested", "1");
      system.reboot(); // New C binding: system("reboot")
      break;
    case "update":
      sendCommandAck(cmd, "starting");
      triggerOTACheck(); // Existing OTA flow
      break;
    case "request-logs":
      const logs = system.readFile("/mnt/ac.log", args.lines || 50);
      machinesWs.send(JSON.stringify({
        type: "command-response", cmd, data: { logs }
      }));
      break;
    case "request-perf":
      machinesWs.send(JSON.stringify({
        type: "command-response", cmd, data: system.perfSnapshot()
      }));
      break;
  }
}
```

#### New C bindings needed for commands

| Binding | Implementation |
|---------|---------------|
| `system.reboot()` | `system("reboot")` — already have `system()` calls in wifi.c |
| `system.readFile(path, lastN)` | Read last N lines from a file (tail equivalent) |
| `system.perfSnapshot()` | Return current perf ring buffer entry as JS object |

#### machines.mjs piece — command UI

In the detail view for a selected machine, add command buttons:

```
┌─────────────────────────────────────────┐
│  ← X1 Nano                     online  │
├─────────────────────────────────────────┤
│  Version:  notepat fb98b847-2026-03-13  │
│  ...                                     │
├─────────────────────────────────────────┤
│  COMMANDS                                │
│  [Jump to piece...]  [Update]  [Reboot] │
│                                          │
│  LOGS (live)                             │
│  ...                                     │
└─────────────────────────────────────────┘
```

- **"Jump to piece..."** — opens a text input, type a piece name, sends `jump` command
- **"Update"** — sends `update` command, shows progress as device reports back
- **"Reboot"** — confirmation dialog ("Reboot X1 Nano?"), then sends `reboot`
- Command buttons disable while waiting for ack, show result briefly

---

## 5. New C Bindings Summary

These new JS API bindings are needed in `js-bindings.c`:

| Binding | Purpose | Implementation |
|---------|---------|---------------|
| `system.machineId` | Persistent device identity | Read from `/mnt/.machine-id` |
| `system.ws2.connect(url)` | Second WebSocket (machines) | Duplicate ws-client slot |
| `system.ws2.send(text)` | Send on machines WS | Same pattern as ws1 |
| `system.ws2.close()` | Close machines WS | Same pattern as ws1 |
| `system.ws2.onmessage` | Receive on machines WS | Poll-based like ws1 |
| `system.reboot()` | Remote reboot command | `system("reboot")` |
| `system.readFile(path, lines)` | Read last N lines of a file | For log fetch commands |
| `system.writeFile(path, data)` | Write small files | For token + label storage |
| `system.perfSnapshot()` | Current frame perf data | Read from perf ring buffer |

---

## 6. File Changes Summary

| File | Action | Description |
|------|--------|-------------|
| `fedac/native/src/ac-native.c` | Edit | Machine ID generation, crash handler, reboot binding |
| `fedac/native/src/js-bindings.c` | Edit | `system.machineId`, `system.ws2.*`, `system.reboot()`, `system.readFile()`, `system.writeFile()`, `system.perfSnapshot()` |
| `fedac/native/src/ws-client.c` | Edit | Add second WebSocket slot (ws2) |
| `fedac/native/src/ws-client.h` | Edit | Second slot declarations |
| `fedac/native/pieces/notepat.mjs` | Edit | Machines WS connection, heartbeat, command handler, crash upload |
| `session-server/session.mjs` | Edit | `/machines` WebSocket route (device + viewer + command relay) |
| `system/netlify/edge-functions/os-release-upload.js` | Edit | Generate + embed device token in releases.json |
| `system/netlify/functions/machines.mjs` | **New** | REST API for machine list + detail + label update |
| `system/netlify/functions/machine-logs.mjs` | **New** | REST API for log retrieval |
| `system/public/aesthetic.computer/disks/machines.mjs` | **New** | Dashboard piece with command buttons |

---

## 7. Implementation Order

1. **Machine ID** (ac-native C) — smallest change, unlocks everything
2. **ws2 in C** — second WebSocket slot for machines connection
3. **OTA token delivery** — extend `ac-os upload` / `os-release-upload.js` to sign + embed device token
4. **Session server `/machines` route** — extend existing SockLogs pattern with auth + persistence
5. **MongoDB collection + REST API** (Netlify function) — `ac-machines` + `ac-machine-logs`
6. **ac-native heartbeat** (notepat.mjs) — devices start reporting home
7. **`machines.mjs` piece** — dashboard with live status
8. **Remote commands** — bidirectional command flow (jump, reboot, update)
9. **Crash reporting** (C signal handler + next-boot upload) — reliability layer
10. **New C bindings** for commands (`system.reboot()`, `system.readFile()`, etc.)

---

## 8. Decisions (Resolved)

| # | Question | Decision |
|---|----------|----------|
| 1 | **Device auth method** | **OTA token** — signed token delivered via releases.json, stored on device, zero manual setup |
| 2 | **Log retention** | 7 days for info logs (TTL index), crashes kept forever |
| 3 | **Second WebSocket in C** | **Yes, `ws2`** — independent slot, clean lifecycle separation from chat WS |
| 4 | **Remote commands** | **Yes** — reboot, update, jump to any piece, request-logs, request-perf |
| 5 | **Naming** | **`machines`** — matches `ac-host` terminology, piece at `aesthetic.computer/machines` |
| 6 | **Multi-piece support** | **Yes** — machine reports `currentPiece` field in heartbeat; `jump` command changes it. Initially only notepat, but the schema supports any piece name from day one |

---

## 9. Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| WebSocket reconnection storms | Exponential backoff in notepat.mjs (already have WiFi reconnect pattern) |
| MongoDB write volume from heartbeats | Heartbeat every 30s × N devices is low; could increase interval to 60s |
| Crash handler can't POST (network down) | Write to `/mnt/crash.json`, upload on next boot |
| OTA token not yet delivered (fresh device) | Device operates in "unlinked" mode, pairs automatically on first OTA check |
| Remote reboot abuse | Commands require authenticated viewer WS; rate-limit reboot to 1 per 60s per device |
| Jump to nonexistent piece | Device validates piece name locally; sends `command-ack` with `status: "error"` if piece not found |
| Session server restart loses live state | Redis presence key with TTL; devices reconnect automatically |
| Token secret rotation | Old tokens still valid until next OTA cycle; `tokenGeneration` counter for forced revocation |
