// Module Loader - WebSocket-based module streaming with IndexedDB caching
// Provides faster module loading by bypassing HTTP proxy chain

const DB_NAME = "ac-module-cache";
const DB_VERSION = 2; // Increment to force schema update
const STORE_NAME = "modules";

// Get WebSocket URL from session endpoint (same approach as disk.mjs/socket.mjs)
async function getSessionWsUrl() {
  const hostname = location.hostname;
  const isLocalhost = hostname === "localhost" || hostname === "127.0.0.1";
  const isLAN = hostname.match(/^192\.168\.|^10\.|^172\.(1[6-9]|2[0-9]|3[01])\./);
  
  // In localhost/LAN dev mode, fetch session endpoint to get the actual URL
  // This works better with VS Code webview proxy
  if (isLocalhost || isLAN) {
    try {
      const res = await fetch('/session/prompt?service=monolith');
      if (res.ok) {
        const session = await res.json();
        // Convert https to wss for WebSocket
        const url = new URL(session.url);
        return `wss://${url.host}`;
      }
    } catch (e) {
      console.warn('📦 Could not fetch session endpoint:', e.message);
    }
    // Fallback to direct URL if fetch fails
    return isLocalhost ? "wss://localhost:8889" : `wss://${hostname}:8889`;
  }
  
  // Production
  return "wss://session-server.aesthetic.computer";
}

class ModuleLoader {
  constructor() {
    this.ws = null;
    this.db = null;
    this.pending = new Map(); // path -> { resolve, reject }
    this.modules = new Map(); // path -> { hash, blobUrl }
    this.blobUrls = new Map(); // path -> blobUrl (for import rewriting)
    this.bundleContents = new Map(); // path -> content (for source display)
    this.loadingModules = new Map(); // path -> Promise (for deduplicating concurrent requests)
    this.connected = false;
    this.connecting = null; // Promise for connection attempt
    this.wsUrl = null; // Set during init
  }

  // Initialize the loader - connect WebSocket and open IndexedDB
  // 🚀 Default timeout reduced to 800ms for faster HTTP fallback
  async init(timeoutMs = 800) {
    if (this.connecting) return this.connecting;
    
    this.connecting = (async () => {
      // 🚀 Check for early WebSocket connection started in HTML <script>
      const earlyWS = window.acEarlyWS;
      if (earlyWS) {
        // Use the early connection if it's already connected or wait for it
        const earlyConnected = earlyWS.isConnected() || await earlyWS.connected;
        if (earlyConnected && earlyWS.ws.readyState === 1) {
          // Reuse the early WebSocket connection
          this.ws = earlyWS.ws;
          this.connected = true;
          this.wsUrl = earlyWS.ws.url;
          
          // Set up message handler
          this.ws.onmessage = (event) => {
            this.handleMessage(JSON.parse(event.data));
          };
          this.ws.onclose = () => {
            this.connected = false;
            for (const [path, { reject }] of this.pending) {
              reject(new Error("WebSocket closed"));
            }
            this.pending.clear();
          };
          
          // Open IndexedDB in parallel
          await this.openDatabase();
          console.log("📦 ModuleLoader ready (early WebSocket ⚡)");
          return true;
        }
      }
      
      // Fall back to normal connection flow
      // Get WebSocket URL (may involve fetch to session endpoint)
      this.wsUrl = await getSessionWsUrl();
      
      // 🚀 Run DB and WS connection in parallel, but don't let WS failure block DB
      const [dbResult] = await Promise.all([
        this.openDatabase(),
        this.connectWebSocket(timeoutMs).catch(() => false) // WS failure is OK
      ]);
      
      if (this.connected) {
        console.log("📦 ModuleLoader ready (WebSocket)");
      } else {
        console.log("📦 ModuleLoader ready (HTTP fallback)");
      }
      return this.connected;
    })().catch(err => {
      console.warn("📦 ModuleLoader init failed:", err.message);
      return false;
    });
    
    return this.connecting;
  }

  // Open IndexedDB for caching
  async openDatabase() {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(DB_NAME, DB_VERSION);
      
      request.onerror = () => {
        console.warn("📦 IndexedDB not available");
        resolve(null); // Don't fail - just disable caching
      };
      
      request.onupgradeneeded = (event) => {
        const db = event.target.result;
        if (!db.objectStoreNames.contains(STORE_NAME)) {
          db.createObjectStore(STORE_NAME, { keyPath: "path" });
        }
      };
      
      request.onsuccess = () => {
        this.db = request.result;
        resolve(this.db);
      };
    });
  }

  // Connect WebSocket to session server
  async connectWebSocket(timeoutMs) {
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        if (this.ws) {
          this.ws.close();
          this.ws = null;
        }
        reject(new Error("WebSocket connection timeout"));
      }, timeoutMs);

      try {
        this.ws = new WebSocket(this.wsUrl);
        
        this.ws.onopen = () => {
          clearTimeout(timeout);
          this.connected = true;
          resolve();
        };
        
        this.ws.onerror = (err) => {
          clearTimeout(timeout);
          this.connected = false;
          reject(new Error("WebSocket connection error"));
        };
        
        this.ws.onclose = () => {
          this.connected = false;
          // Reject any pending requests
          for (const [path, { reject }] of this.pending) {
            reject(new Error("WebSocket closed"));
          }
          this.pending.clear();
        };
        
        this.ws.onmessage = (event) => {
          this.handleMessage(JSON.parse(event.data));
        };
      } catch (err) {
        clearTimeout(timeout);
        reject(err);
      }
    });
  }

  // Handle incoming WebSocket messages
  async handleMessage(msg) {
    if (msg.type === "module:bundle") {
      // Bundle response - all dependencies included (some may be marked as cached)
      const resolver = this.pending.get(msg.entry);
      if (resolver) {
        // Load cached module CONTENT from IndexedDB (don't create blob URLs yet - need to rewrite imports)
        const cachedPaths = msg.cached || [];
        const cachedModules = new Map(); // path -> { hash, content }
        for (const cachedPath of cachedPaths) {
          if (!this.blobUrls.has(cachedPath)) {
            // Try to load from IndexedDB
            const dbCached = await this.getCachedModule(cachedPath);
            if (dbCached) {
              cachedModules.set(cachedPath, { hash: dbCached.hash, content: dbCached.content });
              this.bundleContents.set(cachedPath, dbCached.content);
            }
          }
        }
        
        // SOURCE REWRITING STRATEGY:
        // Blob URLs can't resolve relative imports like `import "./foo.mjs"`.
        // We must rewrite ALL imports to absolute blob URLs.
        //
        // Process in topological order (dependencies first) so when we rewrite
        // module A's imports, its dependencies already have blob URLs.
        //
        // For circular deps: use HTTP fallback URL (original path via normal HTTP).
        // This is acceptable since the whole point is to get MOST modules via WS.
        
        // Combine received modules + cached modules for processing
        const allModules = new Map();
        for (const [path, data] of Object.entries(msg.modules)) {
          allModules.set(path, data);
        }
        for (const [path, data] of cachedModules) {
          allModules.set(path, data);
        }
        
        const moduleEntries = Array.from(allModules.entries());
        
        // Build dependency graph (ONLY static imports, not dynamic)
        // Dynamic imports (import("./path")) are runtime dependencies used to break circular deps
        // They don't need to be in the load-time dependency graph
        const deps = new Map();
        for (const [modPath, modData] of moduleEntries) {
          const moduleDeps = new Set();
          const dir = modPath.includes('/') ? modPath.substring(0, modPath.lastIndexOf('/')) : '';
          
          // Static imports only: import/export ... from "./path"
          // Must have "from" keyword to distinguish from dynamic import("./path")
          const staticRegex = /(?:import|export)\s+(?:[^;]*?\s+)?from\s*["'](\.{1,2}\/[^"'\s]+\.m?js)["']/g;
          let match;
          while ((match = staticRegex.exec(modData.content)) !== null) {
            const resolved = this.resolvePath(dir, match[1]);
            if (allModules.has(resolved) && resolved !== modPath) moduleDeps.add(resolved);
          }
          
          // NOTE: Dynamic imports (import("./path")) are intentionally NOT included
          // They're used at runtime, often specifically to break circular dependencies
          
          deps.set(modPath, moduleDeps);
        }
        
        // Topological sort (leaves first) - dependencies get pushed before dependents
        const processed = new Set();
        const order = [];
        const visit = (modPath) => {
          if (processed.has(modPath)) return;
          processed.add(modPath);
          const moduleDeps = deps.get(modPath) || [];
          for (const dep of moduleDeps) visit(dep);
          order.push(modPath);
        };
        for (const [modPath] of moduleEntries) visit(modPath);
        
        // Track if any module needs HTTP fallback (circular deps or missing deps)
        let hasHttpFallback = false;
        const newBlobUrls = []; // Track blob URLs created in this bundle
        
        // Process modules in topological order
        for (const modPath of order) {
          if (this.blobUrls.has(modPath)) continue;
          
          const modData = allModules.get(modPath);
          if (!modData) continue;
          
          // Store original content for source display (fetchAndShowSource)
          this.bundleContents.set(modPath, modData.content);
          
          // Cache in IndexedDB (async, don't wait) - cache ORIGINAL content, not rewritten
          this.cacheModule(modPath, modData.hash, modData.content);
          
          // Rewrite imports to blob URLs, track if any HTTP fallbacks needed
          const { content: rewritten, hadFallback } = this.rewriteImportsToBlobs(modData.content, modPath, this.blobUrls);
          if (hadFallback) {
            hasHttpFallback = true;
          }
          
          const blob = new Blob([rewritten], { type: "application/javascript" });
          const url = URL.createObjectURL(blob);
          
          this.blobUrls.set(modPath, url);
          this.modules.set(modPath, { hash: modData.hash, blobUrl: url });
          newBlobUrls.push(modPath); // Track what we created
        }
        
        // If any HTTP fallbacks were needed, the blob approach won't work reliably
        // Clean up the broken blob URLs we created - they have un-rewritten imports
        if (hasHttpFallback) {
          console.warn(`📦 Bundle ${msg.entry} has circular deps, cleaning up ${newBlobUrls.length} blob URLs`);
          for (const modPath of newBlobUrls) {
            const url = this.blobUrls.get(modPath);
            if (url) URL.revokeObjectURL(url);
            this.blobUrls.delete(modPath);
            this.modules.delete(modPath);
          }
          resolver.resolve(null); // null signals to use HTTP fallback
        } else {
          // Log cache stats
          const cachedCount = cachedModules.size;
          const sentCount = Object.keys(msg.modules).length;
          if (cachedCount > 0) {
            console.log(`📦 Bundle ${msg.entry}: ${sentCount} received, ${cachedCount} from cache`);
          }
          resolver.resolve(this.blobUrls.get(msg.entry));
        }
        this.pending.delete(msg.entry);
      }
    } else if (msg.type === "module:response") {
      const resolver = this.pending.get(msg.path);
      if (resolver) {
        // Single module - create blob URL directly (no deps)
        const blob = new Blob([msg.content], { type: "application/javascript" });
        const blobUrl = URL.createObjectURL(blob);
        
        // Cache in memory
        this.modules.set(msg.path, { hash: msg.hash, blobUrl });
        this.blobUrls.set(msg.path, blobUrl);
        
        // Cache in IndexedDB (async, don't wait)
        this.cacheModule(msg.path, msg.hash, msg.content);
        
        resolver.resolve(blobUrl);
        this.pending.delete(msg.path);
      }
    } else if (msg.type === "module:error") {
      const resolver = this.pending.get(msg.path);
      if (resolver) {
        resolver.reject(new Error(msg.error || "Module load failed"));
        this.pending.delete(msg.path);
      }
    } else if (msg.type === "module:status") {
      const resolver = this.pending.get(`check:${msg.path}`);
      if (resolver) {
        resolver.resolve(msg);
        this.pending.delete(`check:${msg.path}`);
      }
    } else if (msg.type === "module:updated") {
      // Server pushed a module update notification
      console.log(`📦 Module updated: ${msg.path}`);
      this.modules.delete(msg.path);
      this.blobUrls.delete(msg.path);
      // Could trigger hot reload here
      if (window.acHotReload) {
        window.acHotReload(msg.path);
      }
    }
  }
  
  // Rewrite imports in module content to use blob URLs
  // Returns { content, hadFallback } - hadFallback true if any STATIC import needed HTTP fallback
  rewriteImportsToBlobs(content, modulePath, blobUrlMap) {
    const dir = modulePath.includes('/') ? modulePath.substring(0, modulePath.lastIndexOf('/')) : '';
    const self = this;
    
    // Base URL for absolute paths (for dynamic imports that don't have blob URLs)
    const baseUrl = `${location.origin}/aesthetic.computer/`;
    let hadFallback = false;
    
    // Helper to get blob URL (returns null if not available)
    // isDynamic=true means this is a dynamic import() - missing blobs are OK (used to break circular deps)
    const getBlobUrl = (importPath, isDynamic = false) => {
      if (importPath.includes('...') || importPath.length > 200) return null;
      const resolved = self.resolvePath(dir, importPath);
      if (resolved === modulePath) return null; // Skip self-imports
      const blobUrl = blobUrlMap.get(resolved);
      if (blobUrl) {
        return { url: blobUrl, resolved };
      }
      // No blob URL available
      if (isDynamic) {
        // Dynamic imports are OK to fall back to HTTP - they're often used to break circular deps
        // Don't set hadFallback - this is expected behavior
        return null;
      }
      // Static import missing - this is a problem (missing dep in bundle)
      console.warn(`📦 Missing blob URL: ${modulePath} imports "${importPath}" -> resolved: "${resolved}"`);
      hadFallback = true;
      return null; // Don't rewrite - leave original path
    };
    
    // Rewrite static imports: import/export ... from "./path"
    // Must have "from" keyword to distinguish from dynamic import("./path")
    // Skip commented lines (lines starting with //)
    let result = content.replace(
      /^(?!\s*\/\/).*?((?:import|export)\s+(?:[^;]*?\s+)?from\s*["'])(\.\.?\/[^"']+)(["'])/gm,
      (match, prefix, importPath, suffix) => {
        const info = getBlobUrl(importPath, false); // static import
        if (info) {
          return prefix + info.url + suffix;
        }
        return match; // Keep original if no blob URL
      }
    );
    
    // Rewrite dynamic imports: import("./path")
    // Missing blob URLs are OK here - convert to absolute HTTP URLs
    // (blob URLs don't support relative paths, so we must use absolute URLs)
    result = result.replace(
      /(import\s*\(\s*["'`])(\.{1,2}\/[^"'`\s]+)(["'`]\s*\))/g,
      (match, prefix, importPath, suffix) => {
        const info = getBlobUrl(importPath, true); // dynamic import - OK if missing
        if (info) {
          return prefix + info.url + suffix;
        }
        // No blob URL - convert to absolute HTTP URL (blob URLs can't resolve relative paths)
        const resolved = self.resolvePath(dir, importPath);
        return prefix + baseUrl + resolved + suffix;
      }
    );
    
    return { content: result, hadFallback };
  }
  
  // Resolve relative path
  resolvePath(dir, importPath) {
    const parts = (dir ? dir + '/' + importPath : importPath).split('/');
    const resolved = [];
    for (const part of parts) {
      if (part === '.' || part === '') continue;
      if (part === '..') resolved.pop();
      else resolved.push(part);
    }
    return resolved.join('/');
  }

  // Get original source content for a module (for source display/debugging)
  // Returns the original un-rewritten content if available
  getSourceContent(modulePath) {
    return this.bundleContents.get(modulePath) || null;
  }

  // Load a module - tries cache first, then WebSocket (no HTTP fallback on localhost)
  async load(modulePath, timeoutMs = 5000, withDeps = false) {
    // Check memory cache first
    const cached = this.modules.get(modulePath);
    if (cached) {
      // Background: verify hash is still current
      this.checkHashInBackground(modulePath, cached.hash);
      return cached.blobUrl;
    }
    
    // Check if we're already loading this module (dedup concurrent requests)
    const existingLoad = this.loadingModules.get(modulePath);
    if (existingLoad) {
      return existingLoad;
    }
    
    // Skip IndexedDB cache if loading with deps (bundle response overwrites anyway)
    if (!withDeps) {
      // Check IndexedDB cache
      const dbCached = await this.getCachedModule(modulePath);
      if (dbCached) {
        // Create blob URL and store in memory
        const blob = new Blob([dbCached.content], { type: "application/javascript" });
        const blobUrl = URL.createObjectURL(blob);
        this.modules.set(modulePath, { hash: dbCached.hash, blobUrl });
        this.blobUrls.set(modulePath, blobUrl);
        // Store content for source display
        this.bundleContents.set(modulePath, dbCached.content);
        
        // Background: verify hash is still current
        this.checkHashInBackground(modulePath, dbCached.hash);
        return blobUrl;
      }
    }
    
    // No cache - try WebSocket with timeout
    if (!this.connected) {
      // Fall back to HTTP path
      return `./${modulePath}`;
    }
    
    // Track this load to deduplicate concurrent requests
    const loadPromise = this.loadViaWebSocketWithTimeout(modulePath, timeoutMs, withDeps)
      .finally(() => this.loadingModules.delete(modulePath));
    this.loadingModules.set(modulePath, loadPromise);
    return loadPromise;
  }
  
  // Load a module with all its dependencies via WebSocket
  async loadWithDeps(modulePath, timeoutMs = 3000) {
    return this.load(modulePath, timeoutMs, true);
  }

  // Gather known hashes from memory + IndexedDB cache for delta requests
  async getKnownHashes() {
    const hashes = {};
    // Add memory cache hashes
    for (const [path, data] of this.modules) {
      hashes[path] = data.hash;
    }
    // Add IndexedDB hashes (if not already in memory)
    if (this.db) {
      try {
        const tx = this.db.transaction(STORE_NAME, "readonly");
        const store = tx.objectStore(STORE_NAME);
        const allRequest = store.getAll();
        await new Promise((resolve) => {
          allRequest.onsuccess = () => {
            for (const item of allRequest.result || []) {
              if (!hashes[item.path]) {
                hashes[item.path] = item.hash;
              }
            }
            resolve();
          };
          allRequest.onerror = resolve;
        });
      } catch (err) { /* ignore */ }
    }
    return hashes;
  }

  // Load via WebSocket with timeout (throws on failure - no HTTP fallback)
  async loadViaWebSocketWithTimeout(modulePath, timeoutMs, withDeps = false) {
    // For bundle requests, send known hashes so server can skip unchanged modules
    let knownHashes = null;
    if (withDeps) {
      knownHashes = await this.getKnownHashes();
    }
    
    const wsPromise = new Promise((resolve, reject) => {
      this.pending.set(modulePath, { resolve, reject });
      this.ws.send(JSON.stringify({ 
        type: "module:request", 
        path: modulePath, 
        withDeps,
        knownHashes // Server will skip modules with matching hashes
      }));
    });
    
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error(`WebSocket timeout for ${modulePath}`)), timeoutMs);
    });
    
    try {
      return await Promise.race([wsPromise, timeoutPromise]);
    } catch (err) {
      // Clean up pending request
      this.pending.delete(modulePath);
      throw err; // Let caller handle - no HTTP fallback
    }
  }

  // Get module from IndexedDB cache
  async getCachedModule(modulePath) {
    if (!this.db) return null;
    
    return new Promise((resolve) => {
      try {
        const tx = this.db.transaction(STORE_NAME, "readonly");
        const store = tx.objectStore(STORE_NAME);
        const request = store.get(modulePath);
        
        request.onsuccess = () => resolve(request.result || null);
        request.onerror = () => resolve(null);
      } catch (err) {
        resolve(null);
      }
    });
  }

  // Cache module in IndexedDB
  async cacheModule(modulePath, hash, content) {
    if (!this.db) return;
    
    try {
      const tx = this.db.transaction(STORE_NAME, "readwrite");
      const store = tx.objectStore(STORE_NAME);
      store.put({ path: modulePath, hash, content, cachedAt: Date.now() });
    } catch (err) {
      console.warn("📦 Failed to cache module:", err.message);
    }
  }

  // Check if cached hash matches server (background, don't block)
  async checkHashInBackground(modulePath, cachedHash) {
    if (!this.connected) return;
    
    try {
      const checkPromise = new Promise((resolve) => {
        this.pending.set(`check:${modulePath}`, { resolve, reject: resolve });
        this.ws.send(JSON.stringify({ 
          type: "module:check", 
          path: modulePath, 
          hash: cachedHash 
        }));
        
        // Timeout after 2 seconds
        setTimeout(() => {
          this.pending.delete(`check:${modulePath}`);
          resolve(null);
        }, 2000);
      });
      
      const result = await checkPromise;
      if (result?.changed) {
        console.log(`📦 Module changed on server: ${modulePath}`);
        // Invalidate caches
        this.modules.delete(modulePath);
        if (this.db) {
          try {
            const tx = this.db.transaction(STORE_NAME, "readwrite");
            tx.objectStore(STORE_NAME).delete(modulePath);
          } catch (err) { /* ignore */ }
        }
      }
    } catch (err) {
      // Ignore background check errors
    }
  }

  // Prefetch modules we'll need later via WebSocket bundles
  prefetch(modulePaths) {
    if (!this.connected) return;
    
    for (const modulePath of modulePaths) {
      // Skip if already loaded or currently loading
      if (!this.modules.has(modulePath) && !this.pending.has(modulePath) && !this.loadingModules.has(modulePath)) {
        // Use withDeps=true to get full bundles, 5s timeout
        this.load(modulePath, 5000, true).catch((err) => {
          // Only log non-timeout errors (timeouts for already-loaded modules are expected)
          if (!err.message?.includes('timeout')) {
            console.log(`📦 Prefetch failed for ${modulePath}:`, err.message);
          }
        });
      }
    }
  }

  // Create a bundled worker blob URL with all dependencies inlined
  // This solves the problem of workers not being able to use parent context blob URLs
  async createWorkerBundle(workerPath, timeoutMs = 10000) {
    if (!this.connected) {
      return null; // Let caller fall back to HTTP
    }
    
    try {
      // Request bundle for the worker
      const wsPromise = new Promise((resolve, reject) => {
        const key = `worker:${workerPath}`;
        this.pending.set(key, { 
          resolve: (bundle) => resolve(bundle),
          reject 
        });
        this.ws.send(JSON.stringify({ 
          type: "module:request", 
          path: workerPath, 
          withDeps: true,
          forWorker: true // Signal to server this is for a worker
        }));
      });
      
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => reject(new Error(`Worker bundle timeout for ${workerPath}`)), timeoutMs);
      });
      
      const blobUrl = await Promise.race([wsPromise, timeoutPromise]);
      
      // The bundle message handler will create the blob URL
      // If successful, return it; otherwise return null for HTTP fallback
      if (blobUrl && blobUrl.startsWith('blob:')) {
        console.log(`📦 Worker bundle created: ${workerPath}`);
        return blobUrl;
      }
      return null;
    } catch (err) {
      this.pending.delete(`worker:${workerPath}`);
      console.warn(`📦 Worker bundle failed for ${workerPath}:`, err.message);
      return null; // Let caller fall back to HTTP
    }
  }

  // Clear all caches
  async clearCache() {
    this.modules.clear();
    
    if (this.db) {
      try {
        const tx = this.db.transaction(STORE_NAME, "readwrite");
        tx.objectStore(STORE_NAME).clear();
      } catch (err) {
        console.warn("📦 Failed to clear cache:", err.message);
      }
    }
  }

  // Close connections
  close() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.connected = false;
    this.connecting = null;
  }
}

// Create singleton instance
const moduleLoader = new ModuleLoader();

// Expose globally for debugging
if (typeof window !== "undefined") {
  window.acModuleLoader = moduleLoader;
}

export { ModuleLoader, moduleLoader };
