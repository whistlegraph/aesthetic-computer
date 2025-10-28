# Session Server Architecture Plan

## Current State (As of 2025-10-28)

### Overview
Aesthetic Computer has transitioned away from Jamsocket (paid service discontinued) to a self-hosted session server architecture. This document outlines the current setup and what needs to be documented/updated.

### Current Production Architecture

#### Session Server
- **Production URL**: `https://session-server.aesthetic.computer`
- **Purpose**: Handles both WebSocket connections AND UDP connections
- **Location**: Same server that previously only handled UDP (157.245.134.225)
- **Previous UDP-only URL**: `https://udp.aesthetic.computer` (now deprecated/redundant)

#### Connection Flow
1. Client requests session info from Netlify function `/session/{slug}`
2. Function returns URLs for WebSocket and UDP connections
3. Both URLs now point to the same unified session server

### Code Locations

#### Frontend (disk.mjs)
**File**: `system/public/aesthetic.computer/lib/disk.mjs`

**Key Functions**:
- `session(slug, forceProduction, service)` (line ~3855)
  - Fetches session connection info from `/session/` endpoint
  - Parses WebSocket and UDP URLs
  - Currently still references Jamsocket EventSource for backend status (NEEDS UPDATE)

- `startSocket()` (line ~6320)
  - Establishes WebSocket connection via `socket.connect()`
  - Sets up UDP connection via `send({ type: "udp:connect", ... })`

#### Backend Session Function
**File**: `system/netlify/functions/session.js`

**Current Behavior**:
- **Development mode** (`dev && !forceProd`):
  - Returns `http://localhost:8889` for both WebSocket and UDP
- **Production monolith mode** (`service === "monolith"`):
  - Returns `https://session-server.aesthetic.computer` for WebSocket
  - Returns `https://udp.aesthetic.computer` for UDP (REDUNDANT NOW)
- **Jamsocket mode** (old, still in code):
  - Attempts to spawn/check Jamsocket backends (NO LONGER FUNCTIONAL)

### Issues to Fix

#### 1. ac-session Fish Function
**Problem**: The Emacs detection in `ac-session` may be interfering with proper session server startup.

**Current Code** (`.devcontainer/config.fish` line ~1117):
```fish
if test -n "$INSIDE_EMACS"
    echo "📋 Starting session server (non-blocking mode for Emacs)..."
    # ... background startup logic
    return
end
```

**Analysis**: 
- The Emacs mode starts nodemon in background and immediately returns
- This may prevent proper monitoring/interaction with the session server
- The session server might not be getting proper stdio/logging visibility

**Proposed Fix**:
- Remove or simplify Emacs special handling
- Let nodemon run normally in all environments
- Use standard terminal output for monitoring

#### 2. Jamsocket References (Obsolete)
**Problem**: Code still references Jamsocket API and EventSource streams that no longer work.

**Locations**:
1. `disk.mjs` line ~3891: EventSource connection to `api.jamsocket.com`
2. `disk.mjs` line ~3913-3927: State handling for Jamsocket backend states (Loading, Starting, Ready)
3. `session.js` line ~52-130: Jamsocket backend spawning/checking logic

**Proposed Fix**:
- Remove EventSource polling for backend status (session server is always ready in production)
- Simplify `session()` function to just return URLs without state checking
- Keep monolith mode as primary production path
- Remove all Jamsocket API calls and unused Redis backend tracking code

#### 3. Redundant UDP URL
**Problem**: The UDP connection URL still points to `https://udp.aesthetic.computer` but the session server now handles UDP on the same host.

**Current** (`session.js` line ~23):
```javascript
const udpUrl = `https://udp.aesthetic.computer`;
```

**Proposed Fix**:
- In monolith mode, return the same base URL for both WebSocket and UDP
- Update to: `udp: "https://session-server.aesthetic.computer"`
- Or better: just return a single URL and have client use same host for both protocols

### Proposed Changes

#### Priority 1: Fix ac-session Fish Function
```fish
function ac-session
    echo "🎮 Starting session server..."
    ac
    cd session-server
    
    echo "🔍 Cleaning up any stuck processes..."
    pkill -f "nodemon.*session.mjs" 2>/dev/null
    sleep 1
    npx kill-port 8889 2>/dev/null
    
    echo "🚀 Starting session server on port 8889..."
    PORT=8889 NODE_ENV=development npx nodemon -I --watch session.mjs session.mjs
end
```

#### Priority 2: Simplify session.js Netlify Function
```javascript
async function fun(event, context) {
  const forceProd = parseInt(event.queryStringParameters.forceProduction) === 1;

  if (dev && !forceProd) {
    // Local development
    let host = event.headers.host.split(":")[0];
    if (host === "local.aesthetic.computer") {
      return {
        statusCode: 200,
        body: JSON.stringify({ 
          url: `https://session.${host}`, 
          udp: `https://session.${host}`,
          state: "Ready"
        }),
        headers: { "Access-Control-Allow-Origin": "*" },
      };
    } else {
      return {
        statusCode: 200,
        body: JSON.stringify({ 
          url: `http://${host}:8889`, 
          udp: `http://${host}:8889`,
          state: "Ready"
        }),
        headers: { "Access-Control-Allow-Origin": "*" },
      };
    }
  } else {
    // Production - unified session server
    return {
      statusCode: 200,
      body: JSON.stringify({
        url: `https://session-server.aesthetic.computer`,
        udp: `https://session-server.aesthetic.computer`,
        state: "Ready"
      }),
      headers: { "Access-Control-Allow-Origin": "*" },
    };
  }
}
```

#### Priority 3: Simplify disk.mjs session() Function
Remove EventSource logic and Jamsocket state polling since session server is always ready:

```javascript
async function session(slug, forceProduction = false, service) {
  let endPoint = "/session/" + slug;
  const params = { service };
  if (forceProduction) params.forceProduction = 1;
  endPoint += "?" + new URLSearchParams(params);

  const req = await fetch(endPoint);
  
  if (req.status === 200 || req.status === 304) {
    const session = await req.json();
    return session; // Already includes url, udp, and state: "Ready"
  } else {
    const error = await req.text();
    console.error("Session fetch error:", error);
    return error;
  }
}
```

### Testing Plan

1. **Test ac-session locally**:
   - Run `ac-session` in terminal
   - Verify nodemon starts properly
   - Check logs appear in terminal
   - Test hot reload by editing `session.mjs`
   - Try Ctrl+C to stop

2. **Test development WebSocket/UDP**:
   - Start local session server with `ac-session`
   - Load aesthetic.computer locally
   - Verify WebSocket connects to localhost:8889
   - Verify UDP connects to localhost:8889
   - Test piece with `socket()` API

3. **Test production connection**:
   - Deploy simplified `session.js` function
   - Load aesthetic.computer in production
   - Verify WebSocket connects to `session-server.aesthetic.computer`
   - Verify UDP connects to same host
   - Test multiplayer piece functionality

### Migration Notes

**Breaking Changes**: None - this is a cleanup/documentation update

**Rollback Plan**: 
- Keep old session.js code commented out
- Can revert to Jamsocket mode if needed (though service is discontinued)

**Post-Migration**:
- Remove all Jamsocket-related code
- Remove Redis backend tracking code (unused)
- Update documentation to reflect unified session server architecture

### Open Questions

1. Should we unify the WebSocket and UDP URLs into a single connection string?
2. Do we need per-slug session isolation in production? (Current code doesn't use slug parameter)
3. Should `session-server.aesthetic.computer` handle multiple pieces on different paths?

---

**Status**: Draft - Ready for review and implementation
**Created**: 2025-10-28
**Last Updated**: 2025-10-28
