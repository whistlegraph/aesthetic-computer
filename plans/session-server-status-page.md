# Session Server Status Page Plan

## Overview
Create an HTML dashboard at `session-server.aesthetic.computer` root path that displays real-time information about all connected clients, including both WebSocket and UDP connections.

## Current Stack Analysis

### Server Infrastructure
- **Framework**: Fastify (with HTTPS in dev, HTTP in production)
- **WebSocket**: `ws` library (`WebSocketServer`)
- **UDP**: `@geckos.io/server` (WebRTC-based UDP)
- **Port**: 8889 (dev) / PORT env var (production)
- **File**: `/workspaces/aesthetic-computer/session-server/session.mjs`

### Existing Connection Tracking

#### WebSocket Connections
**Data Structures**:
```javascript
const connections = {};           // All active WebSocket connections by ID
const worldClients = {};          // Clients in virtual spaces (field, horizon, etc.)
const codeChannels = {};          // Clients subscribed to code channels
const users = {};                 // Map of connection IDs to user subs
let connectionId = 0;             // Auto-incrementing connection ID
```

**Available Data Per Connection**:
- `id`: Unique connection identifier
- `ip`: Client IP address (from `req.socket.remoteAddress`)
- `ws`: WebSocket instance with `.isAlive` ping/pong tracking
- `codeChannel`: Optional code subscription channel
- User identity (if logged in): `users[id]` → user sub ID

**worldClients Structure** (for pieces like "field", "horizon"):
```javascript
worldClients[piece][id] = {
  handle: "@username",
  showing: paintingSlug,  // Current painting/piece being shown
  ghost: boolean,         // Ghosted/disconnected state
  ghosted: boolean        // Alternative ghost flag
}
```

#### UDP Connections
**Framework**: `@geckos.io/server`
**Data Structure**: Managed by geckos.io library
- `io.onConnection((channel) => {...})`
- `channel.id`: Unique channel ID
- `channel.webrtcConnection.state`: Connection state
- Channels organized by rooms (broadcast groups)

**Available Data**:
- Connection count via geckos internal state
- Individual channel IDs
- Connection state ("open", etc.)
- No built-in IP tracking (WebRTC abstraction)

### Comparison: Oven Server Pattern

The `oven/server.mjs` provides a good reference implementation:
- Real-time dashboard with WebSocket updates
- Express route serving HTML at root (`/`)
- WebSocket server on `/ws` path
- Dashboard updates via `subscribeToUpdates()` pattern
- Clean separation: HTTP endpoints + WebSocket broadcast

## Proposed Architecture

### 1. HTML Dashboard Route

Add Fastify route for root path:
```javascript
// session-server/session.mjs

fastify.get("/", async (request, reply) => {
  reply.type("text/html");
  return generateStatusHTML();
});
```

### 2. Status Data API Endpoint

Create JSON endpoint for programmatic access:
```javascript
fastify.get("/status", async (request, reply) => {
  return {
    timestamp: Date.now(),
    server: {
      uptime: process.uptime(),
      environment: dev ? "development" : "production",
      port: info.port,
    },
    websocket: {
      total: wss.clients.size,
      connections: getWebSocketStatus(),
    },
    udp: {
      total: io.connectionsCount || 0,
      channels: getUDPStatus(),
    },
  };
});
```

### 3. Real-time Updates via Existing WebSocket

**Option A**: Extend existing WebSocket protocol
- Add new message type: `server:status`
- Broadcast status updates every N seconds
- Clients subscribe via special message type

**Option B**: Separate status WebSocket path
- New WebSocketServer on `/status-stream` path
- Dedicated for dashboard updates only
- Cleaner separation of concerns

**Recommendation**: Option B - cleaner and won't interfere with game/piece traffic

### 4. Data Collection Functions

```javascript
function getWebSocketStatus() {
  const connections = [];
  
  wss.clients.forEach((ws) => {
    // Find the connection ID for this ws
    const id = Object.keys(connections).find(key => connections[key] === ws);
    
    const connection = {
      id: id || 'unknown',
      alive: ws.isAlive,
      readyState: ws.readyState,
      user: users[id] || null,
      codeChannel: findCodeChannel(id),
      worlds: getWorldMemberships(id),
    };
    
    connections.push(connection);
  });
  
  return connections;
}

function getUDPStatus() {
  // geckos.io doesn't expose internal channel list easily
  // May need to track manually in io.onConnection
  const channels = [];
  
  // Option: Maintain separate tracking map
  // const udpChannels = {}; // Add at top level
  // io.onConnection((channel) => {
  //   udpChannels[channel.id] = { 
  //     id: channel.id, 
  //     connectedAt: Date.now() 
  //   };
  //   channel.onDisconnect(() => delete udpChannels[channel.id]);
  // });
  
  return channels;
}

function getWorldMemberships(connectionId) {
  const worlds = [];
  Object.keys(worldClients).forEach(piece => {
    if (worldClients[piece][connectionId]) {
      worlds.push({
        piece,
        handle: worldClients[piece][connectionId].handle,
        showing: worldClients[piece][connectionId].showing,
        ghost: worldClients[piece][connectionId].ghost || false,
      });
    }
  });
  return worlds;
}

function findCodeChannel(connectionId) {
  for (const [channel, subscribers] of Object.entries(codeChannels)) {
    if (subscribers.has(connectionId)) return channel;
  }
  return null;
}
```

### 5. HTML Dashboard Design

**Inspiration**: Oven server dashboard style (minimalist monospace aesthetic)

**Sections**:
1. **Server Info** (top corner)
   - Uptime
   - Environment (dev/production)
   - Last updated timestamp

2. **WebSocket Connections** (left column)
   - Total count
   - Table/list of connections:
     - ID
     - User identity (if logged in)
     - World/piece location
     - Current piece/command
     - Connection state (alive/dead)
     - Code channel subscription

3. **UDP Connections** (right column)
   - Total count
   - Channel IDs
   - Connection states
   - Active rooms/broadcasts

4. **Tab Tracking Enhancement** (future)
   - Currently no direct tab tracking
   - Could add via client heartbeat messages
   - Track multiple tabs per user identity

**Color Coding**:
- 🟢 Active connections (isAlive: true)
- 🔴 Stale connections (isAlive: false, awaiting timeout)
- 👻 Ghosted users (temporary disconnect, may rejoin)
- 🎨 Users showing paintings
- 💻 Code channel subscribers

### 6. WebSocket Status Stream

```javascript
// New WebSocketServer for status updates
const statusWSS = new WebSocketServer({ 
  server, 
  path: '/status-stream' 
});

const statusClients = new Set();

statusWSS.on('connection', (ws) => {
  console.log('📊 Status dashboard connected');
  statusClients.add(ws);
  
  // Send initial state
  ws.send(JSON.stringify({
    type: 'status',
    data: getFullStatus(),
  }));
  
  ws.on('close', () => {
    statusClients.delete(ws);
  });
});

// Broadcast status updates every 2 seconds
setInterval(() => {
  const status = getFullStatus();
  statusClients.forEach(client => {
    if (client.readyState === WebSocket.OPEN) {
      client.send(JSON.stringify({ type: 'status', data: status }));
    }
  });
}, 2000);
```

## Implementation Plan

### Phase 1: Data Collection
1. Add tracking map for UDP channels (manual tracking since geckos.io doesn't expose)
2. Create `getWebSocketStatus()` function
3. Create `getUDPStatus()` function
4. Create `getFullStatus()` aggregator function
5. Test with `/status` JSON endpoint

### Phase 2: HTML Dashboard
1. Create `generateStatusHTML()` function
2. Add Fastify route for `/` (root)
3. Style based on oven dashboard pattern (black background, monospace, minimalist)
4. Static version first (no real-time updates)
5. Test rendering with current connection data

### Phase 3: Real-time Updates
1. Create separate WebSocketServer on `/status-stream` path
2. Implement broadcast logic (every 2 seconds)
3. Add WebSocket client code in HTML dashboard
4. Update DOM on incoming status messages
5. Add connection status indicator (🟢 connected / 🔴 disconnected)

### Phase 4: Enhanced Features
1. Add tab counting (requires client-side tracking via heartbeat)
2. Add current command/piece tracking (parse from worldClients)
3. Add user authentication status display
4. Add ghost/reconnection status visualization
5. Add filtering/search for specific users or connection IDs

## User Identity Tracking

**Current State**:
- `users[id]` maps connection ID → user sub (from login message)
- `worldClients[piece][id].handle` has "@username" for world participants
- No direct "current piece" tracking (only world memberships)

**Enhancement Options**:
1. Track last message type per connection
2. Add "current piece" field to connection metadata
3. Parse Redis pub/sub `slug:@username` subscriptions to track navigation
4. Client sends periodic "heartbeat" with current location

## Technical Considerations

### Performance
- Status updates every 2s (configurable)
- JSON payload size scales with connection count
- Use efficient data structures (Maps/Sets)
- Consider pagination if >1000 connections

### Security
- Add authentication for status page (optional)
- Filter sensitive data (IPs, user subs)
- Rate limit status endpoint
- CORS configuration for dashboard

### Compatibility
- Works with existing session-server deployment
- No breaking changes to current WebSocket protocol
- Separate path for status stream (no interference)

### Monitoring
- Log dashboard connections separately
- Track status page access
- Alert on abnormal connection patterns

## Files to Modify

1. **session-server/session.mjs**
   - Add root route handler
   - Add `/status` JSON endpoint
   - Add status WebSocketServer
   - Add UDP channel tracking
   - Add data collection functions

## Files to Create

1. **session-server/status.html** (optional, if extracted from inline)
   - Dashboard HTML template
   - Could be inlined in `session.mjs` like oven example

## Testing Strategy

1. **Local Development**
   - Start session server with `ac-session`
   - Visit `https://aesthetic.local:8889`
   - Open multiple tabs/pieces
   - Verify connections appear in dashboard
   - Test WebSocket reconnection
   - Test UDP channel display

2. **Production Deployment**
   - Deploy to `session-server.aesthetic.computer`
   - Visit `https://session-server.aesthetic.computer`
   - Monitor real production connections
   - Verify performance with multiple users

3. **Load Testing**
   - Simulate 100+ connections
   - Check dashboard responsiveness
   - Monitor memory usage
   - Verify WebSocket broadcast performance

## Example Dashboard Mockup

```
┌─────────────────────────────────────────────────────────────┐
│ 🧩 session-server                    🟢 Connected  ⏱ 3h 42m │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  WebSocket Connections (47)          UDP Channels (12)       │
│  ────────────────────────────────    ─────────────────       │
│  🟢 #1234 @whistlegraph             🟢 ch_a8f3d2            │
│     World: field                      State: open            │
│     Showing: pond                     Room: default          │
│     Code: wand                                                │
│                                      🟢 ch_9b2e1f            │
│  🟢 #1235 (guest)                     State: open            │
│     World: horizon                    Room: default          │
│     Code: prompt                                              │
│                                      🟢 ch_4c7a3d            │
│  👻 #1232 @ida                        State: connecting       │
│     World: field (ghost)              Room: tv                │
│     Last: drawing                                             │
│                                      ...                      │
│  🟢 #1236 @jeffrey                                            │
│     Channel: code:wand                                        │
│     User: auth_xyz123                                         │
│                                                               │
│  ...                                                          │
│                                                               │
│  Last updated: 2025-11-10 14:32:18                           │
└─────────────────────────────────────────────────────────────┘
```

## Next Steps

1. Review this plan
2. Implement Phase 1 (data collection)
3. Test `/status` endpoint with real connections
4. Proceed to Phase 2 (HTML dashboard)
5. Deploy to production

## References

- Existing pattern: `oven/server.mjs` dashboard
- WebSocket tracking: `session.mjs` lines 254-730
- UDP server: `session.mjs` lines 751-800
- worldClients structure: `session.mjs` lines 509-577
