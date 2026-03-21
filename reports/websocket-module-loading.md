# WebSocket Module Loading System Report

**Date:** January 12, 2026  
**Status:** 🔴 Not Working - Circular Dependency Issue

## Overview

The WebSocket module loading system was designed to bypass flaky HTTP proxy issues (`ERR_CONTENT_LENGTH_MISMATCH`) on localhost by loading ES modules via WebSocket bundles instead of direct HTTP fetches.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Browser (Client)                         │
├─────────────────────────────────────────────────────────────────┤
│  boot.mjs                                                        │
│    ├── module-loader.mjs (WebSocket client)                     │
│    │     ├── Connect to wss://localhost:8889                    │
│    │     ├── Request bundles with dependencies                  │
│    │     ├── Create blob URLs for modules                       │
│    │     └── Rewrite imports to use blob URLs                   │
│    │                                                             │
│    └── importWithRetry()                                        │
│          ├── Try WebSocket bundle first                         │
│          └── Fall back to HTTP on failure                       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ WebSocket
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Session Server (Node.js)                      │
├─────────────────────────────────────────────────────────────────┤
│  session.mjs                                                     │
│    ├── Handle module:request messages                           │
│    ├── gatherDeps() - recursively find all dependencies         │
│    │     ├── Parse static imports (import/export from)          │
│    │     └── Parse dynamic imports (import())                   │
│    └── Send module:bundle response with all modules             │
└─────────────────────────────────────────────────────────────────┘
```

## Loading Flow

### 1. Initialization (boot.mjs)
```javascript
// 1. Import module-loader.mjs via HTTP (single file)
const { moduleLoader } = await import("./module-loader.mjs");

// 2. Connect to WebSocket
await moduleLoader.init(1500); // 1.5s timeout

// 3. Prefetch critical bundles
moduleLoader.prefetch(["lib/disk.mjs", "bios.mjs"]);

// 4. Load entry points with bundles
const blobUrl = await moduleLoader.loadWithDeps("bios.mjs");
const module = await import(blobUrl);
```

### 2. Bundle Request (Client → Server)
```javascript
// Client sends:
{ type: "module:request", path: "bios.mjs", withDeps: true }

// Server responds:
{
  type: "module:bundle",
  entry: "bios.mjs",
  modules: {
    "bios.mjs": { hash: "abc123", content: "..." },
    "lib/loop.mjs": { hash: "def456", content: "..." },
    // ... 89 modules total
  }
}
```

### 3. Bundle Processing (Client)
```javascript
// 1. Build dependency graph from module contents
// 2. Topological sort (process leaves first)
// 3. For each module in order:
//    a. Rewrite imports to blob URLs of already-processed deps
//    b. Create blob URL with rewritten content
//    c. Store in blobUrls map
```

## Current Bottleneck: Circular Dependencies

### The Problem

The system fails when modules have circular imports:

```
lib/help.mjs ──imports──▶ lib/num.mjs
     ▲                         │
     └────────imports──────────┘
```

### What Happens

1. **Topological sort** tries to order: `['num.mjs', 'help.mjs']`
2. **Process num.mjs first:**
   - Tries to rewrite `import { anyKey } from "./help.mjs"`
   - `help.mjs` has no blob URL yet → import stays as relative path
3. **Process help.mjs second:**
   - Rewrites `import * as num from "./num.mjs"` → blob URL ✓
4. **When browser loads:**
   - `help.mjs` blob imports `num.mjs` blob → ✓ works
   - `num.mjs` blob imports `"./help.mjs"` relative → ❌ FAILS
   - Error: "Invalid relative url or base scheme isn't hierarchical"

### Affected Circular Dependencies

| Module A | Module B | In Bundle |
|----------|----------|-----------|
| `lib/help.mjs` | `lib/num.mjs` | lib/disk.mjs |
| `lib/headers.mjs` | `lib/kidlisp.mjs` | lib/disk.mjs |
| `lib/autocomplete.mjs` | itself (in comment) | lib/disk.mjs |

## Other Missing Modules

These modules are referenced but not included in bundles:

| Missing Module | Referenced From | Reason |
|----------------|-----------------|--------|
| `lib/glaze.mjs` | `lib/disk.mjs` | Import is commented out |
| `lib/2d.mjs` | `bios.mjs` | Import is commented out |
| `dep/three/OBJExporter.js` | `lib/3d.mjs` | External dep, not in bundle |

## Strategies Attempted

### Strategy 1: Single-Pass Processing ❌
Process modules in topological order, rewriting as we go.
- **Failed:** Circular deps have one direction that can't be rewritten.

### Strategy 2: Two-Pass with Placeholders ❌
1. Create placeholder blob URLs for all modules
2. Rewrite all modules using placeholder URLs
3. Replace with final blob URLs
- **Failed:** Revoking placeholder URLs broke references from other modules.

### Strategy 3: Two-Pass without Revocation ❌
Same as above but don't revoke placeholders.
- **Failed:** Modules referenced different URLs (placeholder vs final).

### Strategy 4: Snapshot URL Map ❌
Use a snapshot of URLs for all rewrites to ensure consistency.
- **Failed:** Rewritten modules still point to placeholder URLs with relative imports.

## Potential Solutions

### Solution A: Detect and Skip Circular Deps
```javascript
// If module has circular dep, don't create blob URL
// Let it fall back to HTTP loading
if (hasCircularDep(modPath)) {
  continue; // Skip, will load via HTTP
}
```
**Pros:** Simple, works with existing HTTP fallback  
**Cons:** Defeats purpose for circular dep modules

### Solution B: HTTP URL Fallback in Rewrites
```javascript
// Instead of leaving relative path, rewrite to HTTP URL
if (!blobUrl) {
  const httpUrl = `https://localhost:8888/aesthetic.computer/${resolved}`;
  return prefix + httpUrl + suffix;
}
```
**Pros:** Module can still load  
**Cons:** Still hits HTTP proxy (may get ERR_CONTENT_LENGTH_MISMATCH)

### Solution C: Service Worker Interception
Use a Service Worker to intercept relative imports from blob URLs and resolve them.
**Pros:** Transparent to modules  
**Cons:** Complex, SW already exists for caching

### Solution D: Import Map Injection
Dynamically create an import map that maps relative paths to blob URLs.
```html
<script type="importmap">
{
  "imports": {
    "./lib/num.mjs": "blob:...",
    "./lib/help.mjs": "blob:..."
  }
}
</script>
```
**Pros:** Browser handles resolution  
**Cons:** Import maps are static, can't update after page load

### Solution E: Module Concatenation (Bundling)
Concatenate all modules into a single file, eliminating imports.
**Pros:** No import resolution needed  
**Cons:** Loses ES module semantics, complex to implement

## Current Code Location

| File | Purpose |
|------|---------|
| `system/public/aesthetic.computer/module-loader.mjs` | Client-side WebSocket loader |
| `system/public/aesthetic.computer/boot.mjs` | Bootstrap, uses module loader |
| `session-server/session.mjs` | Server-side bundle generation |

## Bundle Statistics

| Bundle | Modules | Size (approx) |
|--------|---------|---------------|
| `lib/disk.mjs` | 56 | ~400KB |
| `bios.mjs` | 89 | ~600KB |
| `lib/parse.mjs` | 12 | ~80KB |

## Next Steps

1. **Implement Solution B** (HTTP fallback for unresolved imports) as quick fix
2. **Investigate Solution D** (import maps) for proper solution
3. **Consider refactoring** circular dependencies in source code

## Error Log Reference

```
📦 Missing blob URL for lib/num.mjs (static import from lib/help.mjs)
📦 Missing blob URL for lib/kidlisp.mjs (static import from lib/headers.mjs)
📦 WebSocket bundle failed for ./bios.mjs, falling back to HTTP: 
    Failed to resolve module specifier "./num.mjs". 
    Invalid relative url or base scheme isn't hierarchical.
```

---

*Report generated during debugging session. Will be updated as solutions are implemented.*
