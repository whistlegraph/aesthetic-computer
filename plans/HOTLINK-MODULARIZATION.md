# Hotlink Modularization Plan

**Goal:** Extract and modularize the link detection/rendering code from `chat.mjs` so it can be reused in:
- `chat.mjs` (current usage)
- `moods.mjs` (mood display)
- `prompt.mjs` (MOTD - message of the day)
- Future: `write` command / text write command
- Future: any AC piece needing interactive text

---

## Current State

### 1. Existing Infrastructure ✅

**[lib/chat-highlighting.mjs](../system/public/aesthetic.computer/lib/chat-highlighting.mjs)** already provides:

```javascript
// Parses text and returns array of {type, text, start, end, color?}
export function parseMessageElements(message);

// Applies \color\ codes to text based on parsed elements
export function applyColorCodes(message, elements, colorMap, defaultColor);

// Default color theme
export const defaultColorTheme = {
  handle: "pink",
  url: "cyan",
  prompt: "lime",
  promptcontent: "cyan",
  painting: "orange",
  kidlisp: "magenta",
  r8dio: [255, 0, 255],
};
```

**Detected element types:**
| Type | Pattern | Example |
|------|---------|---------|
| `handle` | `@username` | `@jeffrey` |
| `url` | `https://...` or `www.` | `https://aesthetic.computer` |
| `prompt` | `'...'` (quotes) | `'hello'` |
| `prompt-content` | Content inside quotes | `hello` |
| `kidlisp-token` | Tokens in `'(code)'` | `(ink "red")` |
| `painting` | `#abc` | `#k3d` |
| `kidlisp` | `$name` | `$mycode` |
| `r8dio` | `r8dio` | `r8dio` |

### 2. What `chat.mjs` Has That Others Need

**Beyond parsing (already modular):**

1. **Element position calculation** — maps parsed elements to pixel bounds for hit detection
2. **Click/tap handling** — triggers actions when elements are clicked
3. **Hover state tracking** — changes cursor and colors on hover
4. **Confirmation modal** — "open link?" dialog before navigation

---

## What's Missing in moods.mjs and prompt.mjs

| Feature | moods.mjs | prompt.mjs |
|---------|-----------|------------|
| Parse elements | ❌ None | ❌ None |
| Color highlighting | ❌ Plain text | ❌ Rainbow animation only |
| Click handling | ❌ None | ❌ None |
| Hover states | ❌ None | ❌ None |

---

## Implementation Plan

### Phase 1: Extend `lib/chat-highlighting.mjs` (Core Library)

Add these new exports to make it the central hotlink utility:

```javascript
// NEW: Calculate pixel positions for hit detection
export function calculateElementPositions(elements, fullText, textLines, textApi, rowHeight, typefaceName);

// NEW: Check if a point is inside any element
export function hitTestElement(x, y, elementPositions);

// NEW: Get action for element type (what happens on click)
export function getElementAction(element, jumpFn) {
  switch(element.type) {
    case "handle": return () => jumpFn(element.text);
    case "url": return () => jumpFn("out:" + element.text);
    case "prompt": return () => jumpFn("prompt " + innerPrompt);
    case "painting": return () => jumpFn("painting" + element.text);
    case "kidlisp-token": return () => jumpFn(fullKidlispCode);
    case "r8dio": return () => jumpFn("r8dio");
  }
}

// NEW: Generate display label for confirmation modal
export function getElementLabel(element);
```

### Phase 2: Create `lib/hotlink.mjs` (High-Level API)

A convenience wrapper for pieces that want full hotlink support with minimal code:

```javascript
import { parseMessageElements, applyColorCodes, defaultColorTheme } from "./chat-highlighting.mjs";

export class HotlinkText {
  constructor(text, options = {}) {
    this.text = text;
    this.elements = parseMessageElements(text);
    this.theme = options.theme || defaultColorTheme;
    this.hoveredElements = new Set();
    this.positions = null; // Calculated on paint
  }

  // Get color-coded text for rendering
  getColoredText(defaultColor) {
    return applyColorCodes(this.text, this.elements, this.theme, defaultColor);
  }

  // Update hover state, returns true if changed
  updateHover(x, y, textApi, rowHeight, typefaceName) { ... }

  // Handle click, returns action or null
  handleClick(x, y, textApi, rowHeight, typefaceName) { ... }

  // Check if text has any interactive elements
  hasInteractiveElements() {
    return this.elements.some(e => 
      ["handle", "url", "prompt", "painting", "kidlisp", "r8dio"].includes(e.type)
    );
  }
}
```

### Phase 3: Update `moods.mjs`

**Changes needed:**

```javascript
// Import
import { HotlinkText } from "../lib/hotlink.mjs";

// In renderMoodItem():
function renderMoodItem(item, y, isFocal, ctx) {
  const hotlink = new HotlinkText(item.mood, { theme: moodTheme });
  
  // Render with color codes
  const coloredMood = hotlink.getColoredText(currentScheme.moodTextDefault);
  ctx.ink(coloredMood).write(...);
  
  // Store for click handling
  item.hotlink = hotlink;
}

// In act() for clicks:
if (item.hotlink) {
  const action = item.hotlink.handleClick(e.x - itemX, e.y - itemY, text, rowHeight, font);
  if (action) {
    // Show confirmation modal or execute directly
    showLinkConfirmation(action);
  }
}
```

### Phase 4: Update `prompt.mjs` MOTD

**Changes needed:**

```javascript
// Import
import { parseMessageElements, applyColorCodes } from "../lib/chat-highlighting.mjs";

// In MOTD rendering (around line 5447):
if (motd && screen.height >= 180) {
  // Parse for highlighting
  const elements = parseMessageElements(motd);
  
  // Apply both rainbow animation AND syntax highlighting
  let coloredText = "";
  const parsedText = applyColorCodes(motd, elements, defaultColorTheme, "white");
  
  // Then apply rainbow effect on top (or replace rainbow with hotlink colors)
  // ...existing rainbow logic but respecting parsed highlights...
  
  ink(pal.handleColor).write(coloredText, writePos, ...);
}
```

**Option A - Hotlink colors replace rainbow:**
- URLs in cyan, handles in pink, etc.
- Simpler, more consistent with rest of AC

**Option B - Rainbow with hotlink underlays:**
- Rainbow animation continues
- Hotlinked text gets special treatment (underline, different base color)
- More complex but keeps current aesthetic

### Phase 5: Refactor `chat.mjs` to Use Shared Code

Replace inline implementations with imports from `lib/chat-highlighting.mjs` and `lib/hotlink.mjs`:

```javascript
// Already partially done:
import { parseMessageElements as parseMessageElementsShared } from "../lib/chat-highlighting.mjs";
const parseMessageElements = parseMessageElementsShared;

// TODO: Move these to lib:
// - calculateElementPosition()
// - isClickInsideElement()
// - generateDynamicColorMessage() → use applyColorCodes instead
```

---

## File Changes Summary

| File | Action |
|------|--------|
| [lib/chat-highlighting.mjs](../system/public/aesthetic.computer/lib/chat-highlighting.mjs) | Extend with position/hit utilities |
| **NEW** `lib/hotlink.mjs` | Create high-level HotlinkText class |
| [disks/chat.mjs](../system/public/aesthetic.computer/disks/chat.mjs) | Refactor to use shared libs |
| [disks/moods.mjs](../system/public/aesthetic.computer/disks/moods.mjs) | Add hotlink support to mood text |
| [disks/prompt.mjs](../system/public/aesthetic.computer/disks/prompt.mjs) | Add hotlink support to MOTD |

---

## API Usage Examples

### Simple: Just Color Highlighting

```javascript
import { parseMessageElements, applyColorCodes, defaultColorTheme } from "../lib/chat-highlighting.mjs";

const text = "Check @jeffrey's art at https://aesthetic.computer and run 'wand'";
const elements = parseMessageElements(text);
const colored = applyColorCodes(text, elements, defaultColorTheme, "white");
ink().write(colored, { x: 10, y: 10 });
```

### Full: Interactive Hotlinks

```javascript
import { HotlinkText } from "../lib/hotlink.mjs";

// Setup
const hotlink = new HotlinkText(text);

// Paint
ink().write(hotlink.getColoredText("white"), { x: 10, y: 10 });

// Act (on move)
if (hotlink.updateHover(e.x, e.y, text, rowHeight, font)) needsPaint();

// Act (on click)
const action = hotlink.handleClick(e.x, e.y, text, rowHeight, font);
if (action) action.execute(); // or show confirmation first
```

---

## Future Extensions

1. **Write command** — use `HotlinkText` for interactive text editing
2. **Notification toasts** — hotlink URLs/handles in notices
3. **Help text** — make documentation interactive
4. **KidLisp REPL** — syntax highlight and hotlink code references

---

## Timeline Estimate

| Phase | Effort | Priority |
|-------|--------|----------|
| Phase 1: Extend chat-highlighting.mjs | 1-2 hours | High |
| Phase 2: Create hotlink.mjs | 2-3 hours | High |
| Phase 3: Update moods.mjs | 1-2 hours | Medium |
| Phase 4: Update prompt.mjs | 1 hour | Medium |
| Phase 5: Refactor chat.mjs | 1-2 hours | Low (works now) |

**Total: ~6-10 hours**
