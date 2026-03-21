# JavaScript/MJS Mode for kidlisp.com - Feasibility Report

## Executive Summary

Building a JavaScript/MJS editing mode for kidlisp.com is **highly feasible** and can reuse 70-80% of existing code. The kidlisp.com editor already uses Monaco (the VS Code editor), which has excellent built-in JavaScript/TypeScript support. The main work involves:

1. Language mode switching (KidLisp ↔ JavaScript)
2. Piece lifecycle integration (`boot`, `sim`, `paint`, `act`)
3. File save/load via aesthetic.computer APIs
4. Module resolution for imports

---

## Current kidlisp.com Stack Analysis

### Monaco Editor (v0.52.0)
- **Already loaded**: CDN from jsDelivr
- **KidLisp language**: Custom registration with syntax highlighting, bracket matching
- **Built-in JS/TS support**: Monaco has first-class JavaScript support including:
  - Full IntelliSense and autocompletion
  - Error checking (via TS compiler in web worker)
  - Hover documentation
  - Go to definition (within same file)
  - Refactoring tools

### Reusable Components (~70-80%)

| Component | Status | Reuse Level |
|-----------|--------|-------------|
| Monaco setup | ✅ | 90% - just add language toggle |
| Theme system (light/dark) | ✅ | 100% - works for any language |
| Keyboard shortcuts | ✅ | 95% - extend for JS-specific |
| Preview iframe | ✅ | 80% - needs piece loader |
| Play/Stop/Pause controls | ✅ | 90% - different execution model |
| Tab system (Learn/List) | ✅ | 70% - need JS docs |
| History/Recent | ✅ | 60% - different file format |
| Console output | ✅ | 100% - already logs JS errors |
| Auth (Auth0) | ✅ | 100% - same authentication |
| Mobile UI/responsive | ✅ | 100% - layout unchanged |
| URL routing | ✅ | 80% - different path scheme |
| Context menu | ✅ | 50% - need JS docs |

### New Components Needed

| Component | Complexity | Notes |
|-----------|------------|-------|
| Language switcher UI | Low | Tab or dropdown toggle |
| JS documentation | Medium | Can use TypeScript inference |
| Piece loader/runner | Medium | Load .mjs in iframe context |
| File save/load API | Medium | Save to user's disk storage |
| Module bundler | High | For imports like `"../lib/..."` |
| LSP integration (optional) | High | Full IDE experience |

---

## AC Piece Architecture Analysis

### Piece Lifecycle Functions

Every AC piece exports these optional functions:

```javascript
// 📦 Boot - Called once when piece loads
export function boot(api) {
  // api contains: wipe, resolution, store, params, colon, net, ...
}

// 🎨 Paint - Called every frame for rendering
export function paint(api) {
  // api contains: wipe, ink, line, box, circle, screen, ...
}

// 🧮 Sim - Called every frame for logic (before paint)
export function sim(api) {
  // api contains: num, store, sound, ...
}

// 🎬 Act - Called on user input events
export function act(api) {
  // api.event contains: { is, pointer, key, ... }
}
```

### The API Object

The `api` object provides 200+ functions. Key categories:
- **Drawing**: `wipe`, `ink`, `line`, `box`, `circle`, `plot`, `paste`
- **Screen**: `resolution`, `screen.width`, `screen.height`
- **Sound**: `sound.synth`, `sound.play`, `sound.sample`
- **Input**: `pen`, `keyboard`, `gamepad`
- **State**: `store`, `params`, `colon`
- **Network**: `net.socket`, `net.udp`
- **System**: `jump`, `alias`, `download`

### Module Imports

Pieces can import from the AC library:

```javascript
import { getNoteColorWithOctave } from "../lib/note-colors.mjs";
import { drawMiniControllerDiagram } from "../lib/gamepad-diagram.mjs";
import * as graph from "./lib/graph.mjs";
```

This is the **main complexity** - we need import resolution.

---

## Implementation Approaches

### Option A: In-iframe Editing (Simpler)

Use the aesthetic.computer iframe as both editor and runtime:

```
kidlisp.com
├── Monaco Editor (left)
├── Preview iframe: aesthetic.computer/{piece} (right)
    └── Live reload on code change
```

**Pros:**
- Full AC runtime available
- All imports work naturally
- Real piece execution

**Cons:**
- Slower feedback loop (full page reload)
- Harder to show errors inline
- Can't use AC's worker architecture

### Option B: Sandboxed Execution (More Complex, Better UX)

Run piece code in a controlled environment:

```
kidlisp.com
├── Monaco Editor (left)  
├── Canvas preview (right)
    ├── Minimal AC runtime
    ├── Import map for modules
    └── HMR-style updates
```

**Pros:**
- Instant preview updates
- Better error handling
- Could work offline

**Cons:**
- Need to bundle/mock AC runtime
- Import resolution complexity
- Subset of full functionality

### Option C: Hybrid (Recommended)

Start with Option A for full compatibility, add Option B features incrementally:

1. **Phase 1**: Monaco + iframe reload (1-2 weeks)
2. **Phase 2**: Live preview for simple pieces (2-3 weeks)
3. **Phase 3**: Full import resolution via importmap (3-4 weeks)

---

## Shared Module Architecture

To make this reusable, extract a shared editor module:

```
system/public/aesthetic.computer/
├── lib/
│   └── ac-editor/
│       ├── editor-core.mjs      # Monaco setup, themes
│       ├── language-kidlisp.mjs # KidLisp syntax
│       ├── language-js.mjs      # JS/piece syntax extras
│       ├── preview-runner.mjs   # Piece execution
│       ├── file-ops.mjs         # Save/load/history
│       └── documentation.mjs    # Docs for both languages
```

Usage:

```javascript
// kidlisp.com
import { createEditor } from "/aesthetic.computer/lib/ac-editor/editor-core.mjs";
createEditor({ language: "kidlisp" });

// code.aesthetic.computer (future)
import { createEditor } from "/aesthetic.computer/lib/ac-editor/editor-core.mjs";
createEditor({ language: "javascript", pieceMode: true });
```

---

## Technical Challenges

### 1. Module Resolution
**Problem**: Pieces use relative imports like `"../lib/note-colors.mjs"`

**Solutions**:
- **Import maps**: Native browser feature for module aliasing
- **Service worker**: Intercept module requests, rewrite URLs
- **Bundler**: Use esbuild-wasm to bundle on-the-fly

### 2. Piece API Injection
**Problem**: Pieces expect `api` parameter with 200+ functions

**Solutions**:
- Build minimal runtime that provides drawing API
- Proxy to full AC runtime in iframe
- Use web workers for non-DOM operations

### 3. File Storage
**Problem**: Where to save .mjs files?

**Solutions**:
- User's handle storage: `/api/user/{handle}/pieces/{name}.mjs`
- Local storage with cloud sync
- GitHub integration (oauth)

### 4. Error Display
**Problem**: JS errors happen at runtime, not parse time

**Solutions**:
- Monaco's TypeScript checker catches many errors
- Window.onerror capture in preview iframe
- postMessage error events back to editor

---

## Development Estimate

| Phase | Work | Time |
|-------|------|------|
| **Phase 1**: Language toggle + basic editing | 1 week |
| **Phase 2**: Preview iframe integration | 1 week |
| **Phase 3**: File save/load API | 1 week |
| **Phase 4**: JS documentation in Learn tab | 1 week |
| **Phase 5**: Import resolution (importmaps) | 2 weeks |
| **Phase 6**: Polish + testing | 1 week |
| **Total** | | **8 weeks** |

For an MVP (edit + preview + save):
- **3-4 weeks** to functional editor
- **+4 weeks** for full IDE experience

---

## Recommendations

1. **Start with language toggle**: Add "KidLisp | JavaScript" tabs, let Monaco handle JS syntax natively.

2. **Reuse preview iframe**: Point to `aesthetic.computer/preview` with posted source code. This leverages existing piece loader.

3. **Build import map generator**: Analyze imports, generate browser importmap for AC libraries.

4. **Consider code.aesthetic.computer**: A dedicated subdomain for the full IDE, separate from kidlisp.com's simpler focus.

5. **API documentation**: Auto-generate from JSDoc in AC source files, similar to KIDLISP_DOCS.

---

## Conclusion

The kidlisp.com architecture is well-suited for JavaScript mode extension. Monaco already supports JS, the preview infrastructure exists, and the UI patterns are proven. The main complexity is module resolution for imports - recommend using browser import maps as the cleanest solution.

A working prototype could be built in 2-3 weeks, with full IDE features in 8 weeks.
