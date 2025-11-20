# KidLisp.com Live Reload Implementation Plan

## Context Analysis

The aesthetic computer stack has three main interop methods for live communication:

### 1. **Code Channel System** (Recommended ✅)
- **How it works**: WebSocket-based pub/sub through session-server
- **Location**: `system/public/aesthetic.computer/lib/disk.mjs` (line 2377)
- **Flow**: 
  - Editor subscribes to a unique channel via `code.channel(channelId)`
  - Server broadcasts code updates to all subscribers on that channel
  - Clients receive `type: "code"` messages with `{piece, source, codeChannel}`
  - Socket handler in `lib/socket.mjs` triggers reload automatically
- **Used by**: VSCode extension for live piece editing
- **Pros**:
  - Already built into the system
  - Handles multiple concurrent editors/viewers
  - Server-side filtering and routing
  - Persistent connections with auto-reconnect
- **Cons**:
  - Requires WebSocket connection (already used by preview iframe)
  - Need to coordinate channel IDs

### 2. **Window postMessage** (Simple but Limited - ACTUALLY VIABLE!)
- **How it works**: Direct iframe ↔ parent communication
- **Flow**:
  - Parent: `iframe.contentWindow.postMessage({type, data}, '*')`
  - Iframe: `window.addEventListener('message', handler)`
- **Pros**:
  - ✅ **No server/networking required** - completely client-side!
  - ✅ Synchronous, immediate (no network latency)
  - ✅ Simple implementation
  - ✅ Works offline
  - ✅ No WebSocket connection overhead
- **Cons**:
  - Only works for single parent-child relationships
  - No pub/sub or multi-viewer support
  - Security concerns with '*' origin (mitigated by origin checking)
  
**UPDATE**: This is actually the better choice for kidlisp.com because:
1. We have direct parent→child iframe relationship
2. No need for multi-viewer sync (single user editing)
3. Zero network dependency = faster, more reliable
4. Simpler architecture

### 3. **Session Server Reload Endpoint** (Development Only)
- **How it works**: HTTP POST to `/reload` endpoint
- **Location**: `session-server/session.mjs` (line 197)
- **Flow**:
  - POST to `https://localhost:8889/reload` with `{piece, source}`
  - Server broadcasts to all connected clients
  - Used by chokidar file watchers
- **Pros**:
  - Works without maintaining WebSocket in editor
  - Simple HTTP request
- **Cons**:
  - Development mode only (`if (dev)`)
  - Broadcasts to ALL connected clients (no targeting)
  - Requires CORS configuration
  - Not suitable for production

## Recommended Solution: postMessage (Revised!)

**Why postMessage is better for kidlisp.com:**
- ✅ No networking = instant updates
- ✅ Works offline
- ✅ Simpler implementation (no WebSocket management)
- ✅ No server dependency
- ✅ Lower latency
- ✅ We only need single-editor-to-single-preview communication

**When to use Code Channel instead:**
- Multiple viewers need to sync (like collaborative editing)
- Remote/distributed scenarios
- Need persistence across page reloads
- Building a "watch party" feature

For kidlisp.com's use case (single user, local editing), postMessage is the clear winner.

## Alternative: Code Channel System (for reference)

### Architecture

```
┌─────────────────────────┐
│  kidlisp.com Editor     │
│  (Monaco)               │
│                         │
│  WebSocket Connection   │
│  wss://aesthetic.computer
└───────────┬─────────────┘
            │
            │ 1. Subscribe to channel
            │    code-channel:sub
            │
            ▼
┌─────────────────────────┐
│  Session Server         │
│  (WebSocket Hub)        │
│                         │
│  Manages subscriptions  │
└───────────┬─────────────┘
            │
            │ 2. Broadcast code updates
            │    type: "code"
            │
            ▼
┌─────────────────────────┐
│  Preview Iframe         │
│  aesthetic.computer     │
│                         │
│  Receives & auto-reloads│
└─────────────────────────┘
```

### Implementation Steps

#### Phase 1: Setup WebSocket Connection in Editor
1. Create WebSocket connection to `wss://aesthetic.computer` (or localhost:8889 for dev)
2. Generate unique channel ID: `kidlisp-editor-${Date.now()}-${Math.random()}`
3. Subscribe to channel: Send `{type: "code-channel:sub", content: channelId}`
4. Store channel ID in preview iframe URL as query param

#### Phase 2: Modify Preview Iframe
1. Add channel ID to iframe URL: `?channel=${channelId}`
2. Iframe auto-subscribes to same channel on load
3. Iframe receives code updates via existing socket infrastructure
4. Code updates trigger piece reload without full page refresh

#### Phase 3: Send Code Updates from Editor
1. On "run" button or Cmd+Enter, send code via WebSocket:
   ```js
   ws.send(JSON.stringify({
     type: "code",
     content: JSON.stringify({
       piece: encodedCode,
       source: editorCode,
       codeChannel: channelId
     })
   }))
   ```
2. Session server routes to all channel subscribers
3. Preview iframe receives and executes new code

#### Phase 4: Handle Edge Cases
1. Reconnection logic if WebSocket drops
2. Channel cleanup on page unload
3. Error handling for malformed code
4. Visual feedback in editor (connection status indicator)

### Code Changes Required

#### `kidlisp.com/index.html`
- Add WebSocket connection setup
- Generate and store channel ID
- Send code updates through WebSocket
- Add connection status indicator

#### Preview Iframe URL
- Add `?channel=${channelId}` parameter
- Iframe will auto-subscribe via existing `code.channel()` system

#### Session Server (Already Works!)
- ✅ Code channel subscription handling exists
- ✅ Message routing exists
- ✅ Broadcast to channel subscribers exists
- No changes needed!

### Alternative: Hybrid Approach

If code-channel feels too heavy, could use **postMessage for immediate feedback** + **code-channel for multi-viewer sync**:

1. Editor sends code via postMessage for instant local preview
2. Also broadcasts via code-channel for other viewers
3. Best of both worlds: speed + flexibility

### Actually, postMessage DOES Work Here!

**Correction**: The aesthetic.computer piece structure is:
```
kidlisp.com (parent)
  └─ iframe src="aesthetic.computer/[code]?nogap=true" (direct child)
       └─ piece renders directly in this iframe
```

It's only ONE iframe layer! We have direct access:
```js
// In kidlisp.com:
const iframe = document.getElementById('preview-iframe');
iframe.contentWindow.postMessage({
  type: 'reload-kidlisp',
  code: editorCode
}, 'https://aesthetic.computer');
```

The iframe can listen and reload without full page refresh!

### Why NOT HTTP Polling?

- Inefficient (repeated requests)
- Higher latency
- More server load
- Worse UX than WebSocket push

## Conclusion

**Use postMessage** because:
- ✅ Zero network overhead = instant feedback
- ✅ Simpler implementation (< 20 lines of code)
- ✅ No WebSocket management/reconnection logic
- ✅ Works offline
- ✅ Direct parent-child communication (no layers to traverse)
- ✅ Perfect for single-user editing scenario

### Implementation (Super Simple!)

**In kidlisp.com editor:**
```js
const iframe = document.getElementById('preview-iframe');

function updatePreview() {
  const code = editor.getValue();
  iframe.contentWindow.postMessage({
    type: 'kidlisp-update',
    code: code
  }, 'https://aesthetic.computer');
}
```

**In aesthetic.computer (would need to add listener):**
```js
window.addEventListener('message', (event) => {
  if (event.origin !== 'https://kidlisp.com') return; // Security
  if (event.data.type === 'kidlisp-update') {
    // Reload piece with new code without full page refresh
    reloadPiece(event.data.code);
  }
});
```

**Estimated implementation time**: 30 minutes including testing.

**Future enhancement**: Could add code-channel later for multi-viewer "watch party" feature.
