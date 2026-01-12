# WebSocket Module Loader

## Problem

On localhost, the HTTP proxy chain (Caddy → Netlify Dev → browser) causes:
- `ERR_CONTENT_LENGTH_MISMATCH` errors
- `ERR_INCOMPLETE_CHUNKED_ENCODING` errors
- Slow module loading due to proxy overhead
- Unreliable hot reloading

Current workarounds in `Caddyfile`:
- Disabled compression (`encode zstd gzip` commented out)
- Strip `Accept-Encoding` header
- Force `Connection: close`

These help but don't fully solve the issue.

## Solution: WebSocket Module Streaming

Establish an early WebSocket connection to session-server and use it to:
1. Stream JS module text directly to the client
2. Cache modules locally (IndexedDB/Cache API)
3. Serve cached modules on subsequent loads
4. Prefetch modules in parallel with execution

## Architecture

```
┌─────────────┐                    ┌──────────────────┐
│  boot.mjs   │───WebSocket───────▶│  session-server  │
│  (early)    │                    │                  │
└─────────────┘                    │  /ws/modules     │
       │                           │  - disk.mjs      │
       ▼                           │  - bios.mjs      │
┌─────────────┐                    │  - graph.mjs     │
│ IndexedDB   │                    │  - ...           │
│ Module Cache│                    └──────────────────┘
└─────────────┘
       │
       ▼
┌─────────────┐
│ Blob URL    │  URL.createObjectURL(new Blob([moduleText]))
│ Import      │  import(blobUrl)
└─────────────┘
```

## Implementation Plan

### Phase 1: Session Server Module Endpoint

**File**: `session-server/session.mjs`

Add a module streaming protocol:

```javascript
// New message types
ws.on('message', (data) => {
  const msg = JSON.parse(data);
  
  if (msg.type === 'module:request') {
    // Request: { type: 'module:request', path: 'lib/disk.mjs' }
    const modulePath = path.join(PUBLIC_DIR, 'aesthetic.computer', msg.path);
    const content = fs.readFileSync(modulePath, 'utf8');
    const hash = crypto.createHash('sha256').update(content).digest('hex').slice(0, 16);
    
    ws.send(JSON.stringify({
      type: 'module:response',
      path: msg.path,
      hash,
      content
    }));
  }
  
  if (msg.type === 'module:check') {
    // Check if module changed: { type: 'module:check', path: 'lib/disk.mjs', hash: '...' }
    const modulePath = path.join(PUBLIC_DIR, 'aesthetic.computer', msg.path);
    const content = fs.readFileSync(modulePath, 'utf8');
    const currentHash = crypto.createHash('sha256').update(content).digest('hex').slice(0, 16);
    
    ws.send(JSON.stringify({
      type: 'module:status',
      path: msg.path,
      changed: currentHash !== msg.hash,
      hash: currentHash
    }));
  }
});
```

### Phase 2: Boot.mjs Early Connection

**File**: `system/public/aesthetic.computer/boot.mjs`

```javascript
// Very early - before any other imports
const MODULE_CACHE_NAME = 'ac-modules-v1';
const SESSION_WS_URL = location.hostname === 'localhost' 
  ? 'ws://localhost:8889'
  : 'wss://session-server.aesthetic.computer';

class ModuleLoader {
  constructor() {
    this.ws = null;
    this.cache = null;
    this.pending = new Map(); // path -> Promise resolvers
    this.modules = new Map(); // path -> { hash, blobUrl }
  }
  
  async init() {
    // Open IndexedDB cache
    this.cache = await this.openCache();
    
    // Connect WebSocket
    this.ws = new WebSocket(SESSION_WS_URL);
    
    return new Promise((resolve, reject) => {
      this.ws.onopen = () => {
        this.ws.onmessage = (e) => this.handleMessage(JSON.parse(e.data));
        resolve();
      };
      this.ws.onerror = reject;
      setTimeout(() => reject(new Error('WS timeout')), 5000);
    });
  }
  
  async openCache() {
    return new Promise((resolve, reject) => {
      const req = indexedDB.open('ac-module-cache', 1);
      req.onupgradeneeded = (e) => {
        const db = e.target.result;
        db.createObjectStore('modules', { keyPath: 'path' });
      };
      req.onsuccess = () => resolve(req.result);
      req.onerror = reject;
    });
  }
  
  handleMessage(msg) {
    if (msg.type === 'module:response') {
      const resolver = this.pending.get(msg.path);
      if (resolver) {
        // Create blob URL
        const blob = new Blob([msg.content], { type: 'application/javascript' });
        const blobUrl = URL.createObjectURL(blob);
        
        // Cache it
        this.modules.set(msg.path, { hash: msg.hash, blobUrl });
        this.cacheModule(msg.path, msg.hash, msg.content);
        
        resolver.resolve(blobUrl);
        this.pending.delete(msg.path);
      }
    }
    
    if (msg.type === 'module:status') {
      const resolver = this.pending.get(`check:${msg.path}`);
      if (resolver) {
        resolver.resolve(msg);
        this.pending.delete(`check:${msg.path}`);
      }
    }
  }
  
  async load(path) {
    // Check local cache first
    const cached = await this.getCached(path);
    if (cached) {
      // Verify hash in background
      this.checkHash(path, cached.hash);
      return cached.blobUrl;
    }
    
    // Request from server
    return new Promise((resolve, reject) => {
      this.pending.set(path, { resolve, reject });
      this.ws.send(JSON.stringify({ type: 'module:request', path }));
      setTimeout(() => reject(new Error(`Module timeout: ${path}`)), 10000);
    });
  }
  
  async getCached(path) {
    return new Promise((resolve) => {
      const tx = this.cache.transaction('modules', 'readonly');
      const req = tx.objectStore('modules').get(path);
      req.onsuccess = () => {
        if (req.result) {
          const blob = new Blob([req.result.content], { type: 'application/javascript' });
          resolve({ hash: req.result.hash, blobUrl: URL.createObjectURL(blob) });
        } else {
          resolve(null);
        }
      };
      req.onerror = () => resolve(null);
    });
  }
  
  cacheModule(path, hash, content) {
    const tx = this.cache.transaction('modules', 'readwrite');
    tx.objectStore('modules').put({ path, hash, content });
  }
  
  async checkHash(path, cachedHash) {
    // Check if server version changed
    return new Promise((resolve) => {
      this.pending.set(`check:${path}`, { resolve });
      this.ws.send(JSON.stringify({ type: 'module:check', path, hash: cachedHash }));
    });
  }
  
  // Prefetch modules we know we'll need
  prefetch(paths) {
    for (const path of paths) {
      if (!this.modules.has(path) && !this.pending.has(path)) {
        this.load(path).catch(() => {}); // Fire and forget
      }
    }
  }
}

// Global instance
window.acModuleLoader = new ModuleLoader();

// Export for use in other modules
export { ModuleLoader };
export const moduleLoader = window.acModuleLoader;
```

### Phase 3: Integration with Boot Sequence

**File**: `system/public/aesthetic.computer/boot.mjs` (updated)

```javascript
// At the very top of boot.mjs
import { moduleLoader } from './module-loader.mjs';

async function boot() {
  // 1. Initialize module loader (WebSocket + IndexedDB)
  try {
    await moduleLoader.init();
    console.log('🔌 Module loader connected');
    
    // 2. Prefetch critical modules immediately
    moduleLoader.prefetch([
      'lib/disk.mjs',
      'lib/graph.mjs',
      'lib/num.mjs',
      'lib/geo.mjs',
      'lib/parse.mjs',
      'lib/help.mjs',
      'bios.mjs'
    ]);
  } catch (e) {
    console.warn('⚠️ Module loader failed, falling back to HTTP:', e);
    // Fall back to normal HTTP imports
  }
  
  // 3. Load disk.mjs (will use cache if available)
  const diskUrl = await moduleLoader.load('lib/disk.mjs').catch(() => './lib/disk.mjs');
  const { boot: diskBoot } = await import(diskUrl);
  
  // ... rest of boot sequence
}
```

### Phase 4: Hot Reload via WebSocket

Session server can push module updates:

```javascript
// session-server: Watch for file changes
const watcher = fs.watch(PUBLIC_DIR, { recursive: true }, (eventType, filename) => {
  if (filename.endsWith('.mjs') || filename.endsWith('.js')) {
    const relativePath = filename.replace(/\\/g, '/');
    const content = fs.readFileSync(path.join(PUBLIC_DIR, filename), 'utf8');
    const hash = crypto.createHash('sha256').update(content).digest('hex').slice(0, 16);
    
    // Broadcast to all connected clients
    broadcast({
      type: 'module:updated',
      path: relativePath,
      hash
    });
  }
});

// Client side: Listen for updates
moduleLoader.ws.onmessage = (e) => {
  const msg = JSON.parse(e.data);
  if (msg.type === 'module:updated') {
    // Invalidate cache
    moduleLoader.modules.delete(msg.path);
    // Optionally trigger hot reload
    if (window.acHotReload) {
      window.acHotReload(msg.path);
    }
  }
};
```

## Benefits

### Development
1. **Bypasses proxy chain** - Direct WebSocket to session-server, no Caddy/Netlify issues
2. **No more `ERR_CONTENT_LENGTH_MISMATCH`** - WebSocket is a clean binary channel
3. **Hot reload** - Push module updates instantly via existing connection
4. **Faster iteration** - Module changes arrive in milliseconds

### Production
1. **Persistent connection** - No TCP/TLS handshake per module (already connected for real-time)
2. **Parallel prefetching** - Download next modules while current ones execute
3. **Local caching** - Instant loads after first visit (IndexedDB survives refresh)
4. **Hash validation** - Know when cache is stale, only re-download changed modules
5. **Single connection** - Reuse for module loading + real-time features + UDP setup

### Both
1. **Graceful fallback** - If WebSocket is slow/offline, HTTP works exactly as before
2. **Progressive enhancement** - Zero breakage, only speed improvements
3. **Transparent** - Code doesn't need to know where modules came from

## Fallback Strategy

The loader races WebSocket against a timeout:

```javascript
async load(path) {
  // Race: WebSocket vs timeout
  const wsPromise = this.loadViaWebSocket(path);
  const timeoutMs = 500; // Half second max wait
  
  try {
    return await Promise.race([
      wsPromise,
      new Promise((_, reject) => 
        setTimeout(() => reject(new Error('WS slow')), timeoutMs)
      )
    ]);
  } catch {
    // WebSocket slow or failed - use normal HTTP
    console.log(`⚡ Falling back to HTTP for ${path}`);
    return `./${path}`; // Normal relative import path
  }
}
```

This means:
- **WebSocket fast (< 500ms)**: Use cached/streamed module ✅
- **WebSocket slow (> 500ms)**: Fall back to HTTP, no delay ✅
- **WebSocket offline**: Immediate fallback to HTTP ✅
- **Cached locally**: Instant, no network at all ✅

## Migration Path

1. **Phase 1**: Add module endpoint to session-server (no client changes)
2. **Phase 2**: Add ModuleLoader class to boot.mjs with HTTP fallback
3. **Phase 3**: Gradually migrate critical modules to use loader
4. **Phase 4**: Add hot reload support
5. **Phase 5**: Consider for production (with CDN cache headers)

## Considerations

- **CORS**: WebSocket doesn't have CORS issues
- **Binary transfer**: Could use binary WebSocket frames for larger modules
- **Compression**: WebSocket can use per-message deflate
- **Fallback**: Always fall back to HTTP if WebSocket fails
- **Production**: Could still be useful for faster initial load + hot reload

## File Changes

| File | Change |
|------|--------|
| `session-server/session.mjs` | Add module streaming handlers |
| `system/public/aesthetic.computer/module-loader.mjs` | New file - ModuleLoader class |
| `system/public/aesthetic.computer/boot.mjs` | Integrate ModuleLoader early |
| `system/public/aesthetic.computer/lib/disk.mjs` | Use moduleLoader for dynamic imports |

## Status

- [ ] Phase 1: Session server module endpoint
- [ ] Phase 2: ModuleLoader class
- [ ] Phase 3: Boot.mjs integration
- [ ] Phase 4: Hot reload
- [ ] Phase 5: Production evaluation
