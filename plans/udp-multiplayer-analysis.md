# Aesthetic Computer Realtime Multiplayer Stack Analysis

## Executive Summary

Aesthetic Computer has a **dual-protocol networking architecture** using both WebSockets (reliable, ordered) and WebRTC DataChannels via geckos.io (UDP-like, low-latency). Both protocols connect to a single `session.mjs` monolith. The current `1v1.mjs` game piece uses **only WebSocket** for position updates, leaving the UDP infrastructure underutilized for real-time game data.

---

## 1. Session Server (`session.mjs`)

### Overview
The session server is a **Fastify-based monolith** that handles:
- WebSocket connections for game clients, chat, and status dashboards
- UDP connections via geckos.io (WebRTC DataChannels)
- Redis pub/sub for cross-instance messaging
- Firebase Cloud Messaging for notifications

### WebSocket Connection Handling

```javascript
// Connection tracking
let connectionId = 0; // Incremental ID for WebSocket clients
const connections = {}; // Map of WebSocket connections by ID
const clients = {}; // Unified client tracking: { handle, user, location, websocket: true/false, udp: true/false }

// On WebSocket connection:
connections[connectionId] = ws;
const id = connectionId;
clients[id].websocket = true;

// Message handler - relays messages to all clients or specific groups
ws.on("message", (data) => {
  const msg = JSON.parse(data.toString());
  msg.id = id; // Attach client identifier
  
  // Handle different message types (1v1:join, 1v1:move, world:*, etc.)
  // Then relay to others
  everyone(JSON.stringify(msg));
});
```

### UDP Support (via geckos.io)

**Yes, UDP support exists!** The server uses [geckos.io](https://github.com/geckosio/geckos.io) for WebRTC DataChannel connections:

```javascript
import geckos from "@geckos.io/server";

const io = geckos();
io.addServer(server); // Hook up to the HTTP Server

// Track UDP channels separately
const udpChannels = {};

io.onConnection((channel) => {
  udpChannels[channel.id] = {
    connectedAt: Date.now(),
    state: channel.webrtcConnection.state,
    user: null,
    handle: null
  };
  
  // Initialize client record
  if (!clients[channel.id]) clients[channel.id] = { udp: true };
  
  // Handle identity for linking WebSocket and UDP
  channel.on("udp:identity", (data) => {
    const identity = JSON.parse(data);
    clients[channel.id].user = identity.user?.sub;
    clients[channel.id].handle = identity.handle;
  });
  
  // Current UDP channels (hardcoded):
  channel.on("tv", (data) => { channel.room.emit("tv", data); });
  channel.on("fairy:point", (data) => { channel.broadcast.emit("fairy:point", data); });
});
```

### Player/Client Identification

Clients are tracked in a unified `clients` object with identity from multiple sources:

| Property | Source | Description |
|----------|--------|-------------|
| `handle` | Login, message content | User's display name (e.g., `@jas`) |
| `user` | Auth token (`sub`) | Unique user identifier |
| `websocket` | Connection type | `true` if connected via WebSocket |
| `udp` | Connection type | `true` if connected via UDP |
| `ip` | Socket address | Client IP for geolocation |
| `geo` | geoip-lite | Country, region, city, timezone |
| `slug` | `client-slug` | Current piece/slug the user is viewing |

**Key insight:** WebSocket and UDP connections have **separate IDs** (connectionId for WS, channel.id for UDP). They're linked via the `clients` map using the same `user` or `handle`.

---

## 2. UDP Client Library (`lib/udp.mjs`)

The client-side UDP library wraps geckos.io:

```javascript
import Geckos from "../dep/geckos.io-client.2.3.2.min.js";

function connect(port = 8889, url = undefined, send) {
  channel = Geckos({ url, port });
  
  channel.onConnect((error) => {
    if (error) { reconnect(); return; }
    
    send({ type: "udp:connected" });
    connected = true;
    
    // Send identity for server-side linking
    if (window.acUSER || window.acHANDLE) {
      channel.emit("udp:identity", JSON.stringify({
        user: window.acUSER,
        handle: window.acHANDLE
      }));
    }
    
    // Listen for incoming messages
    channel.on("tv", (content) => respond("tv", content));
    channel.on("fairy:point", (content) => respond("fairy:point", content));
  });
}

export const UDP = {
  connect,
  disconnect,
  send: ({ type, content }) => {
    if (connected) channel.emit(type, JSON.stringify(content));
  },
};
```

---

## 3. Client-Side Networking in `disk.mjs`

### Socket API Exposed to Pieces

Pieces can access networking through `$commonApi`:

```javascript
$commonApi.net.socket = function (receive) {
  receiver = receive; // Store the piece's message handler
  if (!socket) {
    startSocket();
    clearTimeout(socketStartDelay);
  } else {
    // Already connected
    if (socket?.id) receiver(socket.id, "connected:already");
  }
  return socket;
};
```

### UDP API Exposed to Pieces

```javascript
// In disk.mjs
let udp = {
  send: (type, content) => {
    send({ type: "udp:send", content: { type, content } });
  },
  receive: (type, content) => {
    // Dispatch to piece's receive handler
    udpReceive?.(type, content);
  },
  connected: false,
  kill: (outageSeconds) => {
    udp.connected = false;
    send({ type: "udp:disconnect", content: { outageSeconds } });
  },
};

// Pieces can access via:
$commonApi.net.udp = (receive) => {
  udpReceive = receive;
  return udp;
};
```

### Session Establishment Flow

```javascript
// 1. Client requests session from Netlify function
const sesh = await session(slug, forceProd, monolith);
// Returns: { url: "https://session-server.aesthetic.computer", udp: "https://udp.aesthetic.computer" }

// 2. Connect to both protocols
// WebSocket:
socket.connect(url.host + url.pathname, receiver, reload, "wss");

// UDP (via bios.mjs):
send({
  type: "udp:connect",
  content: {
    url: `https://${udpUrl.hostname}`,
    port: udpUrl.port,
  },
});
```

---

## 4. `1v1.mjs` - Current FPS Game Piece

### Current Networking Implementation

The game currently uses **WebSocket only** for multiplayer:

```javascript
// In boot():
server = socket((id, type, content) => {
  if (type === "left") {
    delete others[id];
    delete playerBoxes[id];
  }
  
  if (type.startsWith("connected")) {
    self.id = id;
    gameState = "lobby";
    server.send("1v1:join", { handle, pos, rot, health });
  }
  
  if (type === "1v1:join") {
    others[id] = { handle, pos, rot, health };
    // Create camera frustum visualization
  }
  
  if (type === "1v1:move") {
    if (others[id]) {
      others[id].pos = content.pos;
      others[id].rot = content.rot;
    }
  }
});

// In sim() - called every frame:
if (server && gameState === "playing" && graphInstance) {
  server.send("1v1:move", {
    pos: { x: graphInstance.x, y: graphInstance.y, z: graphInstance.z },
    rot: { x: graphInstance.rotX, y: graphInstance.rotY, z: graphInstance.rotZ },
  });
}
```

### Problems with Current Approach

1. **High latency**: WebSocket messages go through TCP with head-of-line blocking
2. **Bandwidth waste**: Position updates every frame (~60/sec) over reliable transport
3. **No packet loss tolerance**: Old position data is delivered even when stale

---

## 5. Existing UDP Infrastructure

### Current UDP Channel Names (Hardcoded in session.mjs)

| Channel | Purpose |
|---------|---------|
| `tv` | Broadcast to all in room |
| `fairy:point` | Broadcast to all except sender |
| `udp:identity` | Client identification |

### WebRTC Setup

The session server uses geckos.io which handles WebRTC signaling automatically:
- ICE candidates exchanged over HTTP
- STUN/TURN servers configured (currently default)
- DataChannels with `ordered: false, maxRetransmits: 0` for UDP-like behavior

---

## 6. Recommendations for UDP Position Updates

### Option A: Add New UDP Channel (Minimal Changes)

**Server-side (`session.mjs`):**
```javascript
channel.on("1v1:move", (data) => {
  if (channel.webrtcConnection.state === "open") {
    try {
      // Broadcast to all in the same room except sender
      channel.broadcast.emit("1v1:move", data);
    } catch (err) {
      console.warn("UDP broadcast error:", err);
    }
  }
});
```

**Client-side (`1v1.mjs`):**
```javascript
// In boot():
const udpChannel = net.udp((type, content) => {
  if (type === "1v1:move" && content.id !== self.id) {
    if (others[content.id]) {
      others[content.id].pos = content.pos;
      others[content.id].rot = content.rot;
    }
  }
});

// In sim():
if (udpChannel.connected && gameState === "playing") {
  udpChannel.send("1v1:move", {
    id: self.id,  // Include sender ID
    pos: self.pos,
    rot: self.rot,
    seq: frameCount++,  // For ordering/staleness detection
  });
}
```

### Option B: Hybrid Approach (Best Practice)

Use WebSocket for:
- Connection/join/leave events
- Combat events (hits, deaths, respawns)
- Chat messages
- Game state changes

Use UDP for:
- Position/rotation updates
- Velocity updates
- Interpolation hints

**Linking WebSocket and UDP sessions:**
```javascript
// Client sends same handle/user over both protocols
// Server matches them in the unified `clients` map

// In 1v1.mjs boot():
server.send("1v1:join", { handle: self.handle, pos, rot });
udpChannel.send("1v1:hello", { handle: self.handle }); // Link UDP to same identity
```

### Option C: Full Room System (More Work)

Implement geckos.io rooms for piece-based isolation:

```javascript
// Server-side
channel.on("1v1:join-room", (data) => {
  const { room } = JSON.parse(data);
  channel.join(room);
});

// Client-side
udpChannel.send("1v1:join-room", { room: "1v1-match-123" });
```

---

## 7. Changes Required for UDP Position Updates

### Minimal Implementation Checklist

1. **Session Server (`session.mjs`)**:
   - Add `1v1:move` channel handler
   - Optionally add room system for match isolation

2. **Client UDP Library (`udp.mjs`)**:
   - Add dynamic channel subscription or...
   - Hardcode `1v1:move` channel (quick fix)

3. **`1v1.mjs`**:
   - Call `net.udp()` to get UDP handle
   - Send position via UDP instead of WebSocket
   - Keep WebSocket for join/leave/combat events

4. **Session API (`session.js`)**:
   - Already returns both URLs ✓

### Code Changes Summary

| File | Change |
|------|--------|
| `session.mjs` | Add `channel.on("1v1:move", ...)` handler |
| `udp.mjs` | Add `channel.on("1v1:move", ...)` listener |
| `1v1.mjs` | Use `net.udp()` for position, keep `net.socket()` for events |

---

## 8. Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                         CLIENT (Browser)                         │
├─────────────────────────────────────────────────────────────────┤
│  1v1.mjs (Game Piece)                                            │
│    ├── net.socket() → WebSocket (reliable events)               │
│    └── net.udp() → geckos.io DataChannel (fast position)        │
├─────────────────────────────────────────────────────────────────┤
│  disk.mjs                                                        │
│    ├── Socket class (WebSocket wrapper)                         │
│    └── udp object (sends to bios.mjs)                           │
├─────────────────────────────────────────────────────────────────┤
│  bios.mjs                                                        │
│    └── UDP module (geckos.io client)                            │
└─────────────────────────────────────────────────────────────────┘
                              │                  │
                              ▼                  ▼
               ┌──────────────────┐   ┌──────────────────┐
               │ WebSocket (TCP)  │   │ WebRTC DataChan  │
               │ wss://session... │   │ https://udp....  │
               └────────┬─────────┘   └────────┬─────────┘
                        │                      │
                        ▼                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    SESSION SERVER (Monolith)                     │
├─────────────────────────────────────────────────────────────────┤
│  Fastify HTTP Server                                             │
│    ├── WebSocketServer (ws)                                     │
│    │     ├── connections[] - active WS connections              │
│    │     └── Message routing (everyone, others, subscribers)    │
│    │                                                             │
│    └── geckos.io Server (WebRTC)                                │
│          ├── udpChannels{} - active UDP connections             │
│          └── Channel events (tv, fairy:point, etc.)             │
├─────────────────────────────────────────────────────────────────┤
│  Unified Client Tracking                                         │
│    clients{} = { handle, user, websocket, udp, ip, geo, ... }   │
├─────────────────────────────────────────────────────────────────┤
│  Redis (pub/sub for multi-instance)                              │
└─────────────────────────────────────────────────────────────────┘
```

---

## 9. Key Findings

1. **UDP infrastructure exists** but is underutilized (only `tv` and `fairy:point` channels)
2. **WebSocket and UDP share identity** via `user` and `handle` in the unified `clients` map
3. **1v1.mjs uses WebSocket for position** - easy to switch to UDP
4. **Channel names are hardcoded** - needs server-side change for new channels
5. **No room system** - all UDP messages broadcast to everyone (fine for small player counts)

---

## 10. Quick Start: Adding UDP to 1v1

### Step 1: Server (`session.mjs`)
```javascript
// Add after fairy:point handler
channel.on("1v1:move", (data) => {
  if (channel.webrtcConnection.state === "open") {
    channel.broadcast.emit("1v1:move", data);
  }
});
```

### Step 2: Client UDP lib (`udp.mjs`)
```javascript
// Add in onConnect callback
channel.on("1v1:move", (content) => {
  respond("1v1:move", content);
});
```

### Step 3: Game piece (`1v1.mjs`)
```javascript
// In boot():
const { udp } = net;
udp((type, content) => {
  if (type === "1v1:move") {
    const parsed = typeof content === 'string' ? JSON.parse(content) : content;
    if (parsed.id !== self.id && others[parsed.id]) {
      others[parsed.id].pos = parsed.pos;
      others[parsed.id].rot = parsed.rot;
    }
  }
});

// In sim():
if (udp.connected && gameState === "playing") {
  udp.send("1v1:move", { id: self.id, pos: self.pos, rot: self.rot });
}
```

---

## 11. The WebSocket-UDP Linking Problem

### Current State
- WebSocket connection gets `connectionId` (incremental integer)
- UDP connection gets `channel.id` (geckos.io generated)
- Both send `handle` and `user` for identity linking
- **But:** They're stored with different IDs in `clients{}`

### Challenge for 1v1
When a UDP `1v1:move` comes in, the server doesn't know which WebSocket `connectionId` it maps to unless:
1. The client includes their WebSocket ID in UDP messages (requires exposing `socket.id` to pieces)
2. Server looks up by handle/user (requires O(n) search or secondary index)
3. We create a room system where both protocols join the same room

### Recommended Solution: Include Handle in UDP Messages

```javascript
// Client side (1v1.mjs)
udp.send("1v1:move", { 
  handle: self.handle,  // Use handle as the universal identifier
  pos: self.pos, 
  rot: self.rot 
});

// Server side (session.mjs)
channel.on("1v1:move", (data) => {
  // Just broadcast - let clients filter by handle
  channel.broadcast.emit("1v1:move", data);
});

// Receiving client (1v1.mjs)
if (type === "1v1:move" && content.handle !== self.handle) {
  // Find player by handle
  const playerId = Object.keys(others).find(id => others[id].handle === content.handle);
  if (playerId && others[playerId]) {
    others[playerId].pos = content.pos;
    others[playerId].rot = content.rot;
  }
}
```

This works because:
- Handle is set during WebSocket `1v1:join`
- Handle is included in UDP position updates
- Clients match by handle, not connection ID
