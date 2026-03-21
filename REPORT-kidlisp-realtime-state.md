# KidLisp.com Real-Time Update: State Preservation Report

## Problem

When editing code on kidlisp.com, **any change** — even tweaking a single number in an expression — triggers a **full reset** of the interpreter state: the graphic buffer (`layer0`) is cleared, all user-defined variables (`globalDef`) are wiped, `frameCount` resets to 0, `(once ...)` blocks re-fire, melodies restart, and timers reset. This creates a jarring experience where accumulated visual state vanishes on every keystroke.

## Current Architecture

### The Two Update Paths

KidLisp already has **two distinct update mechanisms** — the infrastructure for state-preserving updates exists but isn't used for normal editing:

| Path | Message Type | Interpreter Method | State Preserved? |
|------|-------------|-------------------|-----------------|
| **Full Reload** (current default) | `kidlisp-reload` | `module(source)` → `reset()` | No — everything wiped |
| **Slide Update** (drag-number only) | `kidlisp-slide` | `slideUpdate(source)` | Yes — variables, timers, `once` flags kept |

### Full Reload Flow (what happens now on every edit)

```
Monaco editor change
  → 1s debounce
  → updatePreview()
  → postMessage({ type: 'kidlisp-reload', code })
  → boot.mjs forwards as { type: 'piece-reload' }
  → disk.mjs calls $commonApi.reload() → $commonApi.load() → lisp.module(source)
```

**`module(source)`** at [kidlisp.mjs:3466](system/public/aesthetic.computer/lib/kidlisp.mjs#L3466) calls **`reset()`** at [kidlisp.mjs:1999](system/public/aesthetic.computer/lib/kidlisp.mjs#L1999), which destroys:

- `this.ast` — cleared
- `this.globalDef` — reset to `{}` (all user `(def ...)` variables gone)
- `this.frameCount` — back to 0
- `this.onceExecuted` — cleared (all `(once ...)` blocks re-execute)
- `this.localEnvStore` — reset
- `this.melodies` / `this.melodyTimers` — cleared
- `this.lastSecondExecutions` / `this.instantTriggersExecuted` — cleared
- `this.layer0` — set to `null` when source changed
- `this.bakes` — cleared when source changed
- Performance caches (`functionCache`, `mathCache`, etc.) — cleared

### Slide Update Flow (what already works for number dragging)

```
Slide mode drag
  → postMessage({ type: 'kidlisp-slide', code })
  → boot.mjs forwards as { type: 'piece-slide' }
  → disk.mjs calls lisp.slideUpdate(source)
```

**`slideUpdate(source)`** at [kidlisp.mjs:1895](system/public/aesthetic.computer/lib/kidlisp.mjs#L1895) is much lighter:

- Re-parses source into fresh AST
- Updates `currentSource` tracking
- Clears `layer0.pixels` (fills with 0 for fresh render)
- Clears bake layers
- **Preserves**: `globalDef`, `onceExecuted`, `frameCount`, timers, melodies, event handlers

## Proposed Solution: Smart Reload

### Core Idea

Instead of always sending `kidlisp-reload`, the editor should **detect the nature of a change** and choose the appropriate update path. A "minor edit" (changing a value within an existing expression) should use the slide-like path; a "structural edit" (adding/removing expressions, changing function names) should use the full reload.

### Implementation Plan

#### 1. New Message Type: `kidlisp-smart-reload`

Add a third message type that carries metadata about the change:

```javascript
// In kidlisp.com index.html — updatePreview()
sendToIframe({
  type: 'kidlisp-smart-reload',
  code: code,
  changeType: detectChangeType(lastSentCodeSource, code), // 'value' | 'structural'
  codeId: codeId,
  createCode: needsNewCode,
  authToken: acToken,
  enableTrace: true
});
```

#### 2. Change Detection in the Editor

Add an AST-level diff function to the editor. Parse both old and new source (KidLisp's parser is fast) and compare structure:

```javascript
function detectChangeType(oldSource, newSource) {
  if (!oldSource || !newSource) return 'structural';

  try {
    // Use KidLisp's parser (or a lightweight copy) to get ASTs
    const oldAST = parseKidLisp(oldSource);
    const newAST = parseKidLisp(newSource);

    // If expression count changed → structural
    if (oldAST.length !== newAST.length) return 'structural';

    // Walk both ASTs comparing structure (function names, nesting)
    // but ignoring literal values (numbers, strings, colors)
    if (structureMatches(oldAST, newAST)) return 'value';

    return 'structural';
  } catch (e) {
    return 'structural'; // Parse error → full reload to show error
  }
}

function structureMatches(a, b) {
  if (typeof a !== typeof b) return false;
  if (typeof a === 'number' || typeof a === 'string') {
    // Both are atoms — if they're function names, compare; if numeric values, skip
    if (typeof a === 'number' && typeof b === 'number') return true; // values can differ
    // For strings: color literals (#fff), number-like strings are "values"
    if (isValueLiteral(a) && isValueLiteral(b)) return true;
    return a === b; // function names must match
  }
  if (!Array.isArray(a) || !Array.isArray(b)) return false;
  if (a.length !== b.length) return false;
  return a.every((el, i) => structureMatches(el, i === 0 ? b[0] : b[i]));
}
```

#### 3. Interpreter: New `softReload(source)` Method

Add a method between `module()` and `slideUpdate()` that:

- Re-parses the source (like `slideUpdate`)
- Preserves `globalDef` user variables (like `slideUpdate`)
- Preserves `onceExecuted` (like `slideUpdate`)
- Preserves `frameCount` (like `slideUpdate`)
- Preserves melodies and timers
- Clears `layer0.pixels` for fresh visual render (like `slideUpdate`)
- **Also** runs precompile on the new AST (unlike current `slideUpdate`)
- Updates performance caches that depend on AST structure

```javascript
// New method in kidlisp.mjs KidLisp class
softReload(source) {
  if (!source) return;

  try {
    const parsed = this.parse(source);

    // Deep copy + precompile (like module() does)
    this.ast = JSON.parse(JSON.stringify(parsed));
    this.precompile(this.ast);
    this.currentSource = source;

    // Clear visual state for fresh render
    if (this.layer0 && this.layer0.pixels) {
      this.layer0.pixels.fill(0);
    }
    if (this.bakes) {
      this.bakes = [];
      this.currentBakeIndex = -1;
    }

    // Clear caches that depend on AST shape
    this.functionCache.clear();
    this.globalEnvCache = null;

    // Preserve: globalDef, onceExecuted, frameCount,
    //   melodies, timers, localEnvStore, tapper, drawer

  } catch (e) {
    console.warn('⚡ Soft reload parse error:', e.message);
  }
}
```

#### 4. Wire It Up in boot.mjs and disk.mjs

**boot.mjs** — handle the new message:

```javascript
} else if (event.data?.type === "kidlisp-smart-reload") {
  const { code, changeType, codeId, createCode, authToken, enableTrace } = event.data;
  window.__acCurrentKidlispCode = code;

  if (changeType === 'value') {
    // Value-only change → soft reload (preserve state)
    window.acSEND({
      type: "piece-soft-reload",
      content: { source: code }
    });
  } else {
    // Structural change → full reload
    window.acSEND({
      type: "piece-reload",
      content: { source: code, codeId, createCode, authToken, enableTrace }
    });
  }
}
```

**disk.mjs** — handle the new internal message:

```javascript
if (type === "piece-soft-reload") {
  if (content.source) {
    lisp.softReload(content.source);
  }
  return;
}
```

### Key Files to Modify

| File | Change |
|------|--------|
| [kidlisp.mjs](system/public/aesthetic.computer/lib/kidlisp.mjs) | Add `softReload()` method (~30 lines) |
| [boot.mjs](system/public/aesthetic.computer/boot.mjs) | Handle `kidlisp-smart-reload` message (~15 lines) |
| [disk.mjs](system/public/aesthetic.computer/lib/disk.mjs) | Handle `piece-soft-reload` message (~5 lines) |
| [index.html](system/public/kidlisp.com/index.html) | Add `detectChangeType()` + modify `updatePreview()` (~60 lines) |

### What Gets Preserved (Value Changes)

| State | Preserved? | Notes |
|-------|-----------|-------|
| `globalDef` (user variables) | Yes | `(def x 10)` keeps its value |
| `frameCount` | Yes | Animations continue from current frame |
| `onceExecuted` | Yes | `(once ...)` blocks don't re-fire |
| `melodies` / `melodyTimers` | Yes | Music keeps playing |
| `tapper` / `drawer` | Yes | Input handlers persist |
| `layer0` pixels | No — cleared | Fresh visual render with new values |
| `bakes` | No — cleared | Baked composites regenerate |
| AST + precompiled data | No — rebuilt | New code takes effect |
| Performance caches | No — cleared | Rebuild from new AST |

### What Triggers Full Reload (Structural Changes)

- Adding or removing a top-level expression
- Changing a function name (e.g., `circle` → `box`)
- Changing expression nesting depth
- Adding/removing arguments to a function call
- Parse errors (to display the error state)

## Quick Win: Simpler Alternative

If the full smart-reload approach feels too complex initially, there's a simpler first step:

**Just use `slideUpdate` for all edits when the expression count hasn't changed.**

In the `onDidChangeModelContent` handler at [index.html:16804](system/public/kidlisp.com/index.html#L16804), instead of always calling `updatePreview()` (which sends `kidlisp-reload`), add a lightweight top-level expression count check:

```javascript
// In the debounced handler
const oldParens = countTopLevelExpressions(lastCode);
const newParens = countTopLevelExpressions(currentCode);

if (oldParens === newParens && oldParens > 0) {
  // Same structure → slide update (preserve state)
  sendToIframe({ type: 'kidlisp-slide', code: currentCode });
} else {
  // Different structure → full reload
  updatePreview();
}
lastCode = currentCode;

function countTopLevelExpressions(code) {
  if (!code) return 0;
  let count = 0, depth = 0;
  for (const ch of code) {
    if (ch === '(') { if (depth === 0) count++; depth++; }
    else if (ch === ')') depth--;
  }
  return count;
}
```

This doesn't require any interpreter changes — it reuses the existing `kidlisp-slide` → `slideUpdate()` path. It's less precise (won't detect when you change a function name without changing structure) but covers the most common case: tweaking a number or color value.

## Edge Cases to Handle

1. **`(def ...)` value changes**: If you change `(def speed 5)` to `(def speed 10)`, the soft reload preserves the *old* `globalDef.speed = 5`. The new `(def speed 10)` would re-execute and overwrite it — so this actually works correctly since `def` is evaluated each frame.

2. **`(once ...)` with changed body**: If you edit code inside a `(once ...)` block, the soft reload skips it because `onceExecuted` is preserved. This is the desired behavior for value tweaks (you don't want initialization to re-run), but might surprise users who change structural code inside `once`. The full AST diff approach handles this by detecting structural changes inside `once` blocks.

3. **Error recovery**: If a soft reload's parse fails, fall back to full reload to show the error state properly.

4. **`$code` substitution**: The `module()` path handles `$code` fetching. The soft reload path should skip this (it's a live-editing context, not a code-loading context).

5. **Resolution changes**: `(half)`, `(third)`, `(fourth)` affect canvas size. If these are added/removed, a full reload is needed. The expression-count heuristic catches this naturally.

## Recommendation

Start with the **Quick Win** (expression-count heuristic using existing `kidlisp-slide`). It's ~20 lines of editor-side code, zero interpreter changes, and covers the primary use case: tweaking values while preserving accumulated graphics and animation state. Graduate to the full `softReload()` approach later if finer-grained control is needed.
