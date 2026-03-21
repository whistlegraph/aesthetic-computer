# Remote Device Control for Artery TUI

## Overview

Enable artery-tui to list, address, and control externally connected local development devices (phones, tablets, other browsers) via the session server. This allows developers to "jump" specific devices to pieces, sync state, and debug across multiple clients.

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Device list API (`/devices`) | ✅ Done | Returns connected clients with metadata |
| Targeted jump (`/jump/:target`) | ✅ Done | By ID, IP, or handle |
| Targeted reload (`/reload/:target`) | ✅ Done | Same targeting as jump |
| Device naming (`/device/:id/name`) | ✅ Done | PUT to set device name |
| `dev:identity` message | ✅ Done | Sent on WebSocket connect with letter (A-Z) |
| `devIdentity` in disk.mjs | ✅ Done | Tracks host name, connection info, device letter |
| LAN mode badge (HUD) | ✅ Done | Pixel-art letter (A-Z) in top-right corner |
| LAN mode on prompt curtain | ✅ Done | Shows host name on starfield |
| Device letters (A-Z) | ✅ Done | Assigned by session server based on connection order |
| Artery TUI devices mode | ✅ Done | 'V' key for device list view |
| Artery TUI QR code | ✅ Done | Scannable QR in menu (uses HOST_IP env var) |
| Remote log forwarding | ✅ Done | `dev:log` from connected devices to session server |
| Products carousel | ⏸️ Disabled | Disabled for faster dev loading |

## Current Architecture

### Full Message Flow (Session WebSocket → disk.mjs)

```
┌─────────────────┐    POST /jump     ┌─────────────────────┐
│  artery-tui     │ ───────────────►  │  session-server     │
│  (Node.js CLI)  │                   │  (session.mjs)      │
└─────────────────┘                   └──────────┬──────────┘
                                                 │
                                     everyone(pack("jump", {piece}))
                                                 │ WebSocket broadcast
                                                 ▼
┌─────────────────┐                   ┌─────────────────────┐
│  disk.mjs       │ ◄──── receive ────│  socket.mjs         │
│  (AC runtime)   │                   │  (WebSocket client) │
└────────┬────────┘                   └─────────────────────┘
         │
         │ $commonApi.jump(content.piece)
         ▼
┌─────────────────┐
│  Browser nav    │
│  (piece loads)  │
└─────────────────┘
```

### 1. Session Server ([session-server/session.mjs](../session-server/session.mjs))

Tracks connected devices with rich metadata:

```javascript
// Unified client tracking (line ~172)
const clients = {}; // Map of connection ID to:
// { handle, user, location, ip, geo, websocket: true/false, udp: true/false }
```

Each WebSocket connection includes:
- **Connection ID**: Numeric identifier (0, 1, 2, ...)
- **IP Address**: e.g., `192.168.1.100` (phone), `172.17.0.1` (local)
- **User Agent**: Identifies device type (Android/iOS/Desktop)
- **Handle**: Username if logged in
- **Geolocation**: Country, region, city (if available)
- **Current location (piece)**: Where the client is navigated (via `location:broadcast`)

### 2. Socket Client ([system/.../lib/socket.mjs](../system/public/aesthetic.computer/lib/socket.mjs))

Browser-side WebSocket class that connects to session server:

```javascript
// socket.mjs - Client-side WebSocket wrapper
export class Socket {
  id;              // Connection ID assigned by server
  connected = false;
  
  connect(host, receive, reload, protocol, connectCallback, disconnectCallback) {
    this.#ws = new WebSocket(`${protocol}://${host}`);
    ws.onmessage = (e) => {
      const msg = JSON.parse(e.data);
      this.#preReceive(msg, receive, reload, sendToBIOS);
    };
  }
  
  send(type, content) {
    this.#ws.send(JSON.stringify({ type, content }));
  }
}
```

### 3. disk.mjs Message Handling ([system/.../lib/disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs))

Connects socket and handles incoming messages including `jump`:

```javascript
// disk.mjs line ~6890 - Socket connection with message receiver
socket?.connect(
  url.host + url.pathname,
  (id, type, content) => {
    // 🎯 Jump to a specific piece!
    if (type === "jump") {
      $commonApi.jump(content.piece);
    }
    receiver?.(id, type, content); // Pass to piece
  },
  $commonApi.reload,  // For live reload
  "wss",
  () => { /* connected callback */ },
  () => { /* disconnected callback */ }
);

// Also handles in main receive function (~line 8493)
if (type === "jump") {
  $commonApi.jump(content.piece, ahistorical, alias);
  return;
}

// And piece-reload (~line 8444)
if (type === "piece-reload") {
  $commonApi.reload({ source: content.source, ... });
  return;
}
```

### Current Jump Implementation (Broadcasts to ALL)

The `/jump` HTTP endpoint broadcasts to ALL connected clients:

```javascript
// Line 295-310 in session.mjs
fastify.post("/jump", async (req) => {
  const { piece } = req.body;
  
  // Broadcast to all browser clients (no targeting!)
  everyone(pack("jump", { piece }, "pieces"));
  
  return { msg: "Jump request sent!", piece };
});
```

### Session Pane Output (from Emacs 📋-session buffer)

Shows connected devices like:
```
🔌 WebSocket connection received: {
  "host": "192.168.1.88:8889",
  "origin": "https://192.168.1.88:8888",
  "userAgent": "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36...",
  "remoteAddress": "192.168.1.100"
}
🧏 Someone joined: 3:192.168.1.100 Online: 2  🫂
```

### Existing Dev Message Types

The session server already supports these dev-time message types:

| Type | Direction | Purpose |
|------|-----------|---------|
| `reload` | Server→Client | Hot reload piece code |
| `jump` | Server→Client | Navigate to piece |
| `code` | Server→Client | Live code update (codeChannel) |
| `piece-reload` | Server→Client | Reload with new KidLisp source |
| `location:broadcast` | Client→Server | Report current piece |
| `code-channel:sub` | Client→Server | Subscribe to code updates |

## Proposed Features

### Phase 1: Device List & Status

Add a "Devices" mode to artery-tui that shows connected clients.

**TUI Menu Addition:**
```javascript
{ key: 'D', label: 'Devices', desc: 'List connected clients', action: () => this.enterDevicesMode() }
```

**Display Format:**
```
╔══════════════════ CONNECTED DEVICES ═══════════════════╗
║  # │ IP              │ Type    │ Piece    │ Handle    ║
╠═══════════════════════════════════════════════════════╣
║  0 │ 172.17.0.1      │ VS Code │ prompt   │ -         ║
║  1 │ 192.168.1.100   │ Android │ prompt   │ @jeffrey  ║
║  2 │ 192.168.1.205   │ iPhone  │ bleep    │ -         ║
╚═══════════════════════════════════════════════════════╝
 [j] Jump selected  [J] Jump all  [r] Refresh  [q] Back
```

**API Endpoint (new):**
```javascript
// GET /devices - Returns connected client list
fastify.get("/devices", async (req) => {
  return getClientStatus(); // Already exists!
});
```

### Phase 2: Targeted Jump

Add ability to jump specific devices by connection ID or IP.

**New API Endpoint:**
```javascript
// POST /jump/:target - Jump specific device(s)
fastify.post("/jump/:target", async (req) => {
  const { target } = req.params;  // Connection ID, IP, or handle
  const { piece, ahistorical, alias } = req.body;
  
  // Find connection(s) matching target
  const matches = targetClients(target);
  
  if (matches.length === 0) {
    return { error: "No matching device", target };
  }
  
  // Send jump only to matching connections
  matches.forEach(ws => {
    ws.send(pack("jump", { piece, ahistorical, alias }, "pieces"));
  });
  
  return { 
    msg: "Targeted jump sent", 
    piece, 
    targets: matches.length 
  };
});

// Helper: Find connections by ID, IP, or handle
function targetClients(target) {
  const results = [];
  for (const [id, ws] of Object.entries(connections)) {
    const client = clients[id];
    if (
      String(id) === String(target) ||
      client?.ip === target ||
      client?.ip?.replace('::ffff:', '') === target ||
      client?.handle === `@${target}` ||
      client?.handle === target
    ) {
      if (ws?.readyState === WebSocket.OPEN) {
        results.push(ws);
      }
    }
  }
  return results;
}
```

**Also add targeted reload:**
```javascript
// POST /reload/:target - Reload specific device(s)
fastify.post("/reload/:target", async (req) => {
  const { target } = req.params;
  const matches = targetClients(target);
  
  matches.forEach(ws => {
    ws.send(pack("reload", req.body, "pieces"));
  });
  
  return { msg: "Targeted reload sent", targets: matches.length };
});
```

**TUI Usage:**
- Select device with arrow keys
- Press `j` to jump selected device
- Press `J` to jump all devices
- Type piece name or use picker

### Phase 3: Device Naming & Groups

Allow assigning friendly names to devices for easier management.

**Device Registry (Redis or file-based):**
```javascript
// Store in Redis or local file
deviceNames = {
  "192.168.1.100": { name: "Jeffrey's Pixel", group: "phones" },
  "192.168.1.205": { name: "Test iPhone", group: "phones" }
}
```

**TUI Display with names:**
```
║  1 │ Jeffrey's Pixel │ Android │ prompt   │ @jeffrey  ║
```

**Group Commands:**
- `jump phones prompt` — Jump all devices in "phones" group
- `jump all prompt` — Jump every connected device

### Phase 4: Sync & Debug Features

**Location Sync:**
- Option to auto-sync piece navigation across all devices
- Useful for demos and synchronized testing

**Device-Specific Console:**
- Stream console logs from specific device to TUI
- Filter logs by connection ID

**Screenshot/Record:**
- Trigger screenshot on specific device
- Record session from device for debugging

## Implementation Plan

### 1. Session Server Changes

**File:** [session-server/session.mjs](../session-server/session.mjs)

```javascript
// Add after line 310 (after existing /jump endpoint)

// GET /devices - List all connected clients with metadata
fastify.get("/devices", async () => {
  return {
    devices: getClientStatus(),
    timestamp: Date.now()
  };
});

// POST /jump/:target - Targeted jump (by ID, IP, or handle)
if (dev) {
  fastify.post("/jump/:target", async (req) => {
    const { target } = req.params;
    const { piece, ahistorical, alias } = req.body;
    
    const targeted = targetClients(target);
    if (targeted.length === 0) {
      return { error: "No matching device", target };
    }
    
    targeted.forEach(ws => {
      ws.send(pack("jump", { piece, ahistorical, alias }, "pieces"));
    });
    
    return { msg: "Targeted jump sent", piece, count: targeted.length };
  });

  // POST /reload/:target - Targeted reload
  fastify.post("/reload/:target", async (req) => {
    const { target } = req.params;
    const targeted = targetClients(target);
    
    targeted.forEach(ws => {
      ws.send(pack("reload", req.body, "pieces"));
    });
    
    return { msg: "Targeted reload sent", count: targeted.length };
  });
  
  // POST /piece-reload/:target - Targeted KidLisp reload
  fastify.post("/piece-reload/:target", async (req) => {
    const { target } = req.params;
    const { source, createCode, authToken } = req.body;
    const targeted = targetClients(target);
    
    targeted.forEach(ws => {
      ws.send(pack("piece-reload", { source, createCode, authToken }, "kidlisp"));
    });
    
    return { msg: "Targeted piece-reload sent", count: targeted.length };
  });
}

// Helper: Find connections by ID, IP, or handle
function targetClients(target) {
  if (target === 'all') {
    return Object.values(connections).filter(ws => ws?.readyState === WebSocket.OPEN);
  }
  
  const results = [];
  for (const [id, ws] of Object.entries(connections)) {
    const client = clients[id];
    const cleanTarget = target.replace('@', '');
    const cleanIp = client?.ip?.replace('::ffff:', '');
    
    if (
      String(id) === String(target) ||
      cleanIp === target ||
      client?.handle === `@${cleanTarget}` ||
      client?.handle === cleanTarget
    ) {
      if (ws?.readyState === WebSocket.OPEN) {
        results.push(ws);
      }
    }
  }
  return results;
}
```

### 2. Artery TUI Changes

**File:** [artery/artery-tui.mjs](../artery/artery-tui.mjs)

Add after line ~465 in menuItems:
```javascript
{ key: 'D', label: 'Devices', desc: 'List & control connected clients', action: () => this.enterDevicesMode() },
```

New mode implementation:
```javascript
async enterDevicesMode() {
  this.mode = 'devices';
  this.devicesLoading = true;
  this.devices = [];
  this.deviceIndex = 0;
  await this.refreshDevices();
  this.devicesLoading = false;
  this.render();
}

async refreshDevices() {
  try {
    const https = await import('https');
    const agent = new https.Agent({ rejectUnauthorized: false });
    const res = await fetch('https://localhost:8889/devices', { agent });
    const data = await res.json();
    this.devices = data.devices || [];
  } catch (e) {
    this.devices = [];
    this.setStatus(`Failed to fetch devices: ${e.message}`);
  }
}

async jumpDevice(target, piece) {
  try {
    const https = await import('https');
    const agent = new https.Agent({ rejectUnauthorized: false });
    const res = await fetch(`https://localhost:8889/jump/${target}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ piece }),
      agent
    });
    const data = await res.json();
    if (data.error) {
      this.setStatus(`Jump failed: ${data.error}`);
    } else {
      this.setStatus(`Jumped ${data.count} device(s) to ${piece}`);
    }
  } catch (e) {
    this.setStatus(`Jump failed: ${e.message}`);
  }
}

async reloadDevice(target, options = {}) {
  try {
    const https = await import('https');
    const agent = new https.Agent({ rejectUnauthorized: false });
    const res = await fetch(`https://localhost:8889/reload/${target}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(options),
      agent
    });
    const data = await res.json();
    this.setStatus(`Reloaded ${data.count} device(s)`);
  } catch (e) {
    this.setStatus(`Reload failed: ${e.message}`);
  }
}

renderDevicesMode() {
  // Render device list table with selection highlight
  // Columns: #, IP, Type, Piece, Handle
  // Show key bindings: [j]ump [r]eload [R]efresh [q]uit
}
```

### 3. Artery Client Helper (Optional - for CLI usage)

**File:** [artery/artery.mjs](../artery/artery.mjs)

Add convenience methods for device control via HTTP (not CDP):

```javascript
// Device control via session server HTTP API
async getDevices() {
  const https = await import('https');
  const agent = new https.Agent({ rejectUnauthorized: false });
  const res = await fetch('https://localhost:8889/devices', { agent });
  return await res.json();
}

async jumpDevice(target, piece, options = {}) {
  const https = await import('https');
  const agent = new https.Agent({ rejectUnauthorized: false });
  const res = await fetch(`https://localhost:8889/jump/${target}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ piece, ...options }),
    agent
  });
  return await res.json();
}

async jumpAll(piece, options = {}) {
  return this.jumpDevice('all', piece, options);
}

async reloadDevice(target, options = {}) {
  const https = await import('https');
  const agent = new https.Agent({ rejectUnauthorized: false });
  const res = await fetch(`https://localhost:8889/reload/${target}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(options),
    agent
  });
  return await res.json();
}
```

## Device Detection Heuristics

Parse User-Agent for device type:
```javascript
function getDeviceType(userAgent) {
  if (!userAgent) return 'Unknown';
  if (userAgent.includes('Android')) return 'Android';
  if (userAgent.includes('iPhone') || userAgent.includes('iPad')) return 'iOS';
  if (userAgent.includes('Code/')) return 'VS Code';
  if (userAgent.includes('Chrome')) return 'Chrome';
  if (userAgent.includes('Firefox')) return 'Firefox';
  if (userAgent.includes('Safari')) return 'Safari';
  return 'Browser';
}
```

## CLI Quick Commands (Future)

For fast iteration without TUI:
```bash
# List devices
artery devices

# Jump specific device
artery jump 192.168.1.100 prompt
artery jump @jeffrey bleep

# Jump all
artery jump --all line
```

## Priority Order

1. **Device List API** — `GET /devices` endpoint ✓ (already exists via `getClientStatus`)
2. **Targeted Jump API** — `POST /jump/:target` endpoint
3. **Targeted Reload API** — `POST /reload/:target` endpoint  
4. **TUI Devices Mode** — List display with selection
5. **TUI Jump/Reload from Devices** — Select device, enter piece, send command
6. **Device Type Detection** — Parse User-Agent for icons
7. **Device Naming** — Store custom names (Redis or file)
8. **CLI Commands** — Quick command-line access

## Testing Strategy

1. **Unit test** `targetClients()` helper function
2. **Integration test** `/devices` API returns correct data
3. **Integration test** `/jump/:target` sends to correct WebSocket
4. **Manual test** with phone connected over local network:
   - Connect phone to `https://192.168.x.x:8888`
   - See phone appear in device list
   - Jump phone to specific piece
   - Verify phone navigates correctly
   - Test reload sends to correct device

## Related Files

- [session-server/session.mjs](../session-server/session.mjs) — WebSocket server, client tracking, message broadcasting
- [system/.../lib/socket.mjs](../system/public/aesthetic.computer/lib/socket.mjs) — Browser WebSocket client
- [system/.../lib/disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs) — AC runtime, handles `jump`/`reload`/`piece-reload` messages
- [artery/artery-tui.mjs](../artery/artery-tui.mjs) — TUI interface
- [artery/artery.mjs](../artery/artery.mjs) — Client helper library (CDP-based, but can add HTTP helpers)
- [artery/test-jump.mjs](../artery/test-jump.mjs) — Existing jump test (CDP-based, local browser only)

## Message Type Reference

| Message Type | Handler Location | Description |
|--------------|------------------|-------------|
| `jump` | disk.mjs:8493, disk.mjs:6895 | Navigate to piece |
| `reload` | socket.mjs (preReceive) | Hot reload piece code |
| `piece-reload` | disk.mjs:8444 | Reload with KidLisp source |
| `code` | socket.mjs (preReceive) | Live code channel update |
| `connected` | socket.mjs (preReceive) | Connection established |
| `location:broadcast` | session.mjs | Client reports current piece |

## Notes

- The session server already tracks clients with IPs and metadata
- The `/status` endpoint provides full client info (used by dashboard)
- CDP-based jump (in artery.mjs) only works for VS Code Simple Browser
- **WebSocket-based jump (proposed) works for ANY connected client**
- Phone connections show up with LAN IPs like `192.168.1.xxx`
- All dev endpoints should be gated with `if (dev)` check
- disk.mjs receives messages via socket.mjs's `receive` callback
