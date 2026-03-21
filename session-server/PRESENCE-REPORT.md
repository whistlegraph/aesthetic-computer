# Presence System Report

## Current Architecture Overview

There are **TWO separate WebSocket connection systems** that track users:

### 1. Main Session Server WebSocket (port 8889)
- **File**: `session.mjs`
- **Connects**: On initial page load, stays connected across ALL pieces
- **Tracks in `clients[]`**:
  - `handle` - user's @handle
  - `user` - auth0 sub ID
  - `location` - current piece slug (via `location:broadcast` messages)
  - `ip`, `geo`, etc.
- **Also tracks `worldClients[piece][id]`**:
  - Only for pieces with "world" features (like `field`)
  - Uses `world:*:join`, `world:*:leave`, `world:*:move` messages
  - Handles ghosting/unghosting when users navigate away and back

### 2. Chat WebSocket (separate connections per instance)
- **File**: `chat-manager.mjs`
- **Instances**: `chat-system`, `chat-sotce`, `chat-clock`
- **Connects**: From `lib/chat.mjs` when a piece calls `chat.connect("system")`
- **Tracks per instance**:
  - `instance.connections[id]` - WebSocket connections (ALL connected sockets)
  - `instance.authorizedConnections[id]` - Only authenticated users who have sent a message
  - `instance.authorizedConnections[id].handle` - Their @handle

---

## Why the Online Counter is Inaccurate

### Problem 1: Chat connects globally, not per-piece
The chat connection is established from `lib/chat.mjs` and **persists across piece navigation**. When you're on `aesthetic.computer/prompt` or any other piece, you're still connected to chat-system. The `chatterCount` in chat shows ALL connected WebSockets, not just users actually viewing the `chat` piece.

### Problem 2: `onlineHandles` only shows AUTHORIZED users
The `getOnlineHandles()` function in chat-manager.mjs:
```javascript
getOnlineHandles(instance) {
  const handles = [];
  for (const [id, auth] of Object.entries(instance.authorizedConnections)) {
    if (auth.handle && instance.connections[id]) {
      handles.push(auth.handle);
    }
  }
  return [...new Set(handles)];
}
```
This only includes users who:
1. Have an open WebSocket connection
2. Have successfully authorized (sent a chat message with valid token)
3. Have a handle stored

**Anonymous viewers or logged-in users who haven't chatted** are NOT in `onlineHandles`.

### Problem 3: Connection count includes duplicates
`Object.keys(instance.connections).length` counts WebSocket connections, not unique users. One user with multiple tabs = multiple connections.

---

## What We Know About User Location

The **main session server** (`session.mjs`) DOES track which piece users are on:

```javascript
// From location:broadcast handler (line 1708-1750)
if (msg.content.slug !== "*keep-alive*") {
  clients[id].location = msg.content.slug;
  log(`📍 Location updated for ${clients[id].handle || id}: "${msg.content.slug}"`);
}
```

This is published to Redis:
```javascript
pub.publish("slug:" + msg.content.handle, msg.content.slug)
```

**Key insight**: The session server knows exactly which piece each user is on!

---

## Proposed Solution: "Here" Counter for Chat Piece

### Option A: Cross-reference session server location data

The chat-manager could query the session server's `clients[]` to see which authenticated chat users have `location === "chat"`.

**Implementation**:
1. Export a function from session.mjs: `getClientsOnPiece(piece)`
2. In chat-manager, after broadcasting `online-handles`, also send `here-handles`
3. `here-handles` = intersection of `onlineHandles` AND users where `clients[id].location === "chat"`

### Option B: Chat piece explicitly notifies "in-view"

When the `chat` piece mounts, it could send a message like `chat:enter`. When unmounting/leaving, `chat:leave`.

**Pros**: Explicit, accurate
**Cons**: Requires piece-level code changes, needs cleanup on disconnect

### Option C: Track "last seen piece" per chat connection

Map chat connection IDs to session server connection IDs (via handle or token), then look up their `location`.

---

## Data Flow Diagram

```
┌──────────────────────────────────────────────────────────────┐
│                        BROWSER                                │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                      BIOS/Disk                          │ │
│  │  - Connects to session-server:8889 on load             │ │
│  │  - Sends location:broadcast when piece changes         │ │
│  │  - Stays connected across ALL pieces                   │ │
│  └─────────────────────────────────────────────────────────┘ │
│                              │                                │
│  ┌───────────────────────────┼──────────────────────────────┐│
│  │      lib/chat.mjs         │                              ││
│  │  - Called by chat piece   │                              ││
│  │  - Connects to chat-*.aesthetic.computer                 ││
│  │  - Also persists across piece navigation!                ││
│  └───────────────────────────┼──────────────────────────────┘│
└──────────────────────────────┼───────────────────────────────┘
                               │
         ┌─────────────────────┴─────────────────────┐
         ▼                                           ▼
┌─────────────────────┐                   ┌─────────────────────┐
│   session.mjs:8889  │                   │  chat-manager.mjs   │
│                     │                   │  (chat-*.aesthetic) │
├─────────────────────┤                   ├─────────────────────┤
│ clients[id] = {     │                   │ connections[id] =   │
│   handle,           │                   │   WebSocket         │
│   user,             │  ← NO LINK →      │                     │
│   location: "chat"  │                   │ authorizedConns =   │
│ }                   │                   │   { token, handle } │
├─────────────────────┤                   ├─────────────────────┤
│ KNOWS: who's on     │                   │ KNOWS: who's        │
│        "chat" piece │                   │        connected    │
│                     │                   │        to chat WS   │
│ DOESN'T KNOW:       │                   │                     │
│   who's auth'd for  │                   │ DOESN'T KNOW:       │
│   chat specifically │                   │   actual piece      │
└─────────────────────┘                   └─────────────────────┘
```

---

## Recommended Implementation

### Phase 1: Bridge the data (quick win)

In `session.mjs`, add a function:
```javascript
function getHandlesOnPiece(pieceName) {
  return Object.values(clients)
    .filter(c => c.location === pieceName && c.handle)
    .map(c => c.handle);
}
```

Export this so chat-manager can call it, or expose via internal API.

### Phase 2: Broadcast "here" in chat

Modify `broadcastOnlineHandles()` in chat-manager:
```javascript
broadcastOnlineHandles(instance) {
  const handles = this.getOnlineHandles(instance);
  const hereHandles = this.getHereHandles(instance); // Users actually on chat piece
  this.broadcast(instance, this.pack("presence", { 
    online: handles,      // All auth'd chat connections
    here: hereHandles     // Only those on "chat" piece right now
  }));
}
```

### Phase 3: UI in chat.mjs

```javascript
// Currently cycles through onlineHandles
// Add a "Here" section that shows users actually viewing chat
const hereHandles = client?.hereHandles || [];
const onlineHandles = client?.onlineHandles || [];
```

---

## Questions to Resolve

1. **Should chat disconnect when leaving the chat piece?**
   - Currently it stays connected (enables notifications anywhere)
   - Could add explicit `chat:enter`/`chat:leave` events instead

2. **How to link session ID to chat ID?**
   - Both have `handle` - match on that?
   - Or pass session ID in chat auth?

3. **Should "here" count anonymous viewers?**
   - Session server tracks location for everyone
   - Chat only tracks authorized users

---

## Files to Modify

1. `session-server/session.mjs` - Export helper function or add internal API
2. `session-server/chat-manager.mjs` - Add "here" calculation and broadcast
3. `system/public/aesthetic.computer/lib/chat.mjs` - Handle new `presence` message type
4. `system/public/aesthetic.computer/disks/chat.mjs` - Display "here" users

---

*Report generated: 2026-01-17*
