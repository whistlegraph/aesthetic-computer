# WebSocket Module Loader Status Report
**Date:** January 12, 2026

## Goal
Bypass flaky Netlify HTTP proxy on localhost that causes `ERR_CONTENT_LENGTH_MISMATCH` errors by loading ES modules via WebSocket instead.

## What's Working ✅

1. **WebSocket connects successfully** - `📦 WebSocket connected to wss://localhost:8889`
2. **Bundles are received** - `📦 Received bundle for: bios.mjs (89 modules)`
3. **Topological sort works** - No more false circular dependency detection
4. **Blob URLs are created** - `📦 Bundle loaded: lib/disk.mjs (56 modules via WS)`
5. **Main modules load via blob URLs** - `bios.mjs`, `parse.mjs`, `disk.mjs` all load

## Remaining Issues ❌

### 1. Worker Loading Fails
```
📦 Worker load failed (attempt 1/4), retrying in 200ms...
📦 Worker load failed (attempt 2/4), retrying in 400ms...
```
The web worker (`system/public/aesthetic.computer/lib/worker.mjs`) is loaded via `new Worker(url)`. 
- Workers can't use blob URLs directly from parent context
- Worker needs its own module loading mechanism or inline bundling

### 2. HTTP Fallback Still Hits ERR_CONTENT_LENGTH_MISMATCH
```
GET https://localhost:8888/bios.mjs net::ERR_CONTENT_LENGTH_MISMATCH 200 (OK)
```
Some code paths still try HTTP:
- `boot.mjs:579` - `fetchAndShowSource()` fetches source for display/debugging
- Worker loading attempts

### 3. Prefetch Timeout
```
📦 Prefetch failed for bios.mjs: WebSocket timeout for bios.mjs
```
Background prefetch requests time out - likely because the module was already loaded via the first request and the server doesn't send a duplicate response.

## Files Modified

### `/system/public/aesthetic.computer/module-loader.mjs`
- Fixed `getSessionWsUrl()` to fetch `/session/prompt` endpoint (like disk.mjs does)
- Changed dependency graph to only use **static imports** (not dynamic imports)
- Fixed static import regex to require `from` keyword (was incorrectly matching dynamic imports)
- Dynamic imports without blob URLs now convert to **absolute HTTP URLs** (blob URLs can't resolve relative paths)
- `getBlobUrl()` now has `isDynamic` parameter - missing dynamic import blob URLs don't trigger `hadFallback`

### Key Regex Changes
**Dependency graph** (line ~178):
```javascript
// OLD (matched dynamic imports too):
/(?:import|export)\s+(?:[\s\S]*?\s+from\s+)?["'](\.{1,2}\/[^"'\s]+\.m?js)["']/g

// NEW (requires "from" keyword):
/(?:import|export)\s+(?:[^;]*?\s+)?from\s*["'](\.{1,2}\/[^"'\s]+\.m?js)["']/g
```

**Rewrite static imports** (line ~348):
```javascript
// OLD:
/^(?!\s*\/\/).*?((?:import|export)\s+(?:[\s\S]*?\s+from\s+)?["'])(\.\.?\/[^"']+)(["'])/gm

// NEW:
/^(?!\s*\/\/).*?((?:import|export)\s+(?:[^;]*?\s+)?from\s*["'])(\.\.?\/[^"']+)(["'])/gm
```

## Architecture Understanding

### Circular Dependency Pattern
- `headers.mjs` line 2: `import { tokenize } from "./kidlisp.mjs";` (STATIC)
- `kidlisp.mjs` line 3154: `await import('./headers.mjs');` (DYNAMIC - runtime only)

The dynamic import is **intentional** to break the load-time cycle. The fix was to exclude dynamic imports from the dependency graph used for topological sorting.

### Why Blob URLs Can't Resolve Relative Paths
When a module is loaded from `blob:https://localhost/abc123`, any `import("./foo.mjs")` inside it fails because:
- Blob URLs don't have a directory context
- `./foo.mjs` relative to `blob:...` is meaningless

Solution: Convert dynamic imports without blob URLs to absolute HTTP URLs like `https://localhost:8888/aesthetic.computer/lib/foo.mjs`

## Next Steps

1. **Fix Worker Loading**
   - Option A: Bundle worker code into a single blob (no imports)
   - Option B: Create inline worker with `importScripts` or similar
   - Option C: Use service worker to intercept worker module requests

2. **Eliminate Remaining HTTP Fetches**
   - `fetchAndShowSource()` in boot.mjs could use the WebSocket bundle data
   - Or catch and suppress the error since it's just for debugging

3. **Clean Up Debug Logging**
   - Remove or reduce the `📦 DEBUG:` console logs once stable

## Server-Side Reference
Bundle gathering in `/session-server/session.mjs` lines 1341-1400:
- `gatherDeps()` recursively collects static AND dynamic imports
- Uses `path.normalize(path.join(...))` for resolution
- Both static and dynamic imports are included in the bundle sent to client
