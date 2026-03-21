# List.mjs Redesign Plan

## Current State
A simple scrollable list of all pieces and prompts with basic text buttons.

## Goal
A comprehensive, categorized reference with multiple typefaces, parameter documentation, and rich visual hierarchy—similar to `colors.mjs`, `moods.mjs`, and `chat.mjs`.

---

## Data Structure (from `/docs.json`)

The docs endpoint returns:
```js
{
  api: {
    structure: { boot, paint, act, sim, beat, leave, meta, preview, ... },
    interaction: { pen, pens, pen3d, event },
    graphics: { line, point, box, wipe, ink, circle, ... },
    sound: { "sound.time", "sound.bpm", ... },
    network: { "net.signup", "net.login", ... },
    state: { store, ... },
    math: { num, geo, ... },
    util: { ... },
  },
  prompts: { // Commands typed directly
    tezos: { sig, desc, params[], done },
    keep: { sig, desc, params[], done },
    tape: { sig, desc, params[], done },
    mood: { sig, desc, params[], done },
    ...
  },
  pieces: { // Loadable pieces
    clock: { sig, desc, colon[], params[], examples[], done },
    colors: { sig, desc, done },
    chat: { sig, desc, done },
    ...
  }
}
```

---

## New Architecture

### 1. **View Modes** (Tab/Section Navigation)
- **All** — Flat alphabetical list (current behavior)
- **Pieces** — Just loadable pieces, categorized
- **Commands** — Prompt commands only
- **API** — Developer reference (structure, graphics, sound, network, etc.)

### 2. **Categories for Pieces**
Group pieces by theme (can be auto-detected or manual):
- 🎨 **Creative Tools** — `colors`, `line`, `box`, `fill`, `brush`, `camera`, `crop`
- 🎵 **Audio/Music** — `clock`, `tone`, `bleep`, `chord`, `synth`, `bubble`
- 🎮 **Games** — `brick-breaker`, `pong`, `snake`, `balls`
- 💬 **Social** — `chat`, `mood`, `moods`, `field`, `scream`
- 🤖 **AI/LLM** — `sotce`, `bf`, `gf`, `bro`, `sis`, `dad`, `angel`
- 📹 **Media** — `tape`, `video`, `camera`, `selfie`
- 🎰 **Random/Fun** — `spin`, `flower`, `wiggle`, `wand`
- 💼 **System** — `prompt`, `handle`, `wallet`, `tezos`, `keep`
- 🔧 **Dev Tools** — `debug`, `api`, `list`, `docs`
- 🌍 **World/3D** — `field`, `fly`, `world`
- 🙈 **Hidden** — Items with `hidden: true`

### 3. **Categories for API**
Use existing structure:
- 🏛️ **Structure** — `boot`, `paint`, `act`, `sim`, `beat`, `leave`, `meta`
- 🖱️ **Interaction** — `pen`, `pens`, `event`
- 🖌️ **Graphics** — `line`, `box`, `wipe`, `ink`, `circle`, `write`
- 🔊 **Sound** — `sound.time`, `sound.bpm`, `sound.play`
- 🌐 **Network** — `net.signup`, `net.login`, `net.userRequest`
- 💾 **State** — `store`, `params`, `colon`
- 🔢 **Math/Geometry** — `num`, `geo`, `help`
- 🎛️ **UI** — `ui.Button`, `ui.TextInput`

---

## UI Design

### Layout
```
┌─────────────────────────────────────────┐
│ 📚 LIST                     [search: _] │  ← Header with search
├─────────────────────────────────────────┤
│ [All] [Pieces] [Commands] [API]         │  ← Tab buttons
├─────────────────────────────────────────┤
│ ▼ 🎨 Creative Tools (12)                │  ← Collapsible category
│   box          Draw rectangles          │
│   colors       An index of usable...    │
│   fill         Fill with solid color    │
│                                         │
│ ▼ 🎵 Audio/Music (8)                    │
│   clock        Musical clock with...    │
│   tone         Play a tone              │
│                                         │
│ ▸ 🎮 Games (4)                          │  ← Collapsed category
│ ▸ 💬 Social (6)                         │
└─────────────────────────────────────────┘
```

### Detail Panel (on selection)
```
┌─────────────────────────────────────────┐
│ clock                    MatrixChunky8  │
│ ─────────────────────────────────────── │
│ Musical clock with melody, waveforms,   │
│ Hz shifts, and parallel tracks.         │
│                                         │
│ Usage: clock[:divisor] [melody] [sync]  │  ← signature
│                                         │
│ Colon Parameters:                       │
│   divisor   number   Time divisor       │
│                      (0.5=faster)       │
│                                         │
│ Parameters:                             │
│   melody    string   Notes like cdefg   │
│   sync      "sync"   UTC sync mode      │
│                                         │
│ Examples:                               │
│   clock cdefg                           │
│   clock:0.5 {square}cdefgab             │
│   clock (ceg) (dfa)                     │
│                                         │
│           [ Try It ]                    │  ← Jump to piece
└─────────────────────────────────────────┘
```

---

## Visual Features

### Typography Hierarchy
- **Category headers**: `MatrixChunky8` or custom large font, colored
- **Item names**: Default typeface, color-coded by type
- **Descriptions**: Smaller/dimmer text
- **Signatures**: Monospace style
- **Parameters**: Indented, typed

### Color Scheme (Dark/Light)
```js
const scheme = {
  dark: {
    background: [16, 16, 24],
    categoryHeader: [180, 200, 255],
    pieceName: [100, 255, 150],
    commandName: [255, 200, 100],
    apiName: [200, 150, 255],
    description: [120, 120, 140],
    signature: [80, 180, 180],
    parameter: [180, 180, 200],
    searchBox: [40, 40, 50],
    tabActive: [100, 200, 255],
    tabInactive: [80, 80, 100],
  },
  light: {
    // Lighter variants...
  }
};
```

### Interactive Features
- **Search filter**: Type to filter items in real-time
- **Collapsible categories**: Click header to expand/collapse
- **Keyboard navigation**: Arrow keys, Enter to select
- **Touch-friendly**: Large tap targets
- **Scroll memory**: Remember position per tab

---

## Implementation Steps

### Phase 1: Data & State
1. Fetch docs via `net.requestDocs()`
2. Parse into categorized structure
3. State management for:
   - Current tab (all/pieces/commands/api)
   - Expanded categories
   - Search filter
   - Selected item
   - Scroll positions per tab

### Phase 2: Core UI
1. Tab bar with mode switching
2. Category headers (collapsible)
3. Item rows with name + description
4. Scroll handling per category

### Phase 3: Detail Panel
1. Show on item selection
2. Display signature, description
3. Render colon params, params, examples
4. "Try It" button → `jump()`

### Phase 4: Search & Polish
1. Search input field
2. Keyboard navigation
3. Theme support (dark/light)
4. Animations/transitions
5. Save state on leave

---

## Code Structure

```js
// list.mjs - Comprehensive API & Piece Reference

// Data
let docs = null;
let categories = {};
let currentTab = "all"; // all | pieces | commands | api
let expandedCategories = new Set();
let searchFilter = "";
let selectedItem = null;
let scrollPositions = { all: 0, pieces: 0, commands: 0, api: 0 };

// Categories
const PIECE_CATEGORIES = { /* ... */ };
const API_CATEGORIES = { /* from docs.api structure */ };

// Color schemes
const scheme = { dark: { /* ... */ }, light: { /* ... */ } };

// UI elements
let tabs = [];
let categoryHeaders = [];
let itemButtons = [];
let searchInput = null;

async function boot({ ui, net, store, typeface }) {
  // Load docs
  docs = await net.requestDocs();
  
  // Build categories
  buildCategories();
  
  // Restore state
  currentTab = await store.retrieve("list:tab") || "all";
  scrollPositions = await store.retrieve("list:scrolls") || { /* defaults */ };
  expandedCategories = new Set(await store.retrieve("list:expanded") || []);
  
  // Build UI elements
  buildTabs(ui);
  buildItems(ui, typeface);
}

function paint({ wipe, ink, screen, dark }) {
  const pal = dark ? scheme.dark : scheme.light;
  wipe(pal.background);
  
  // Header
  ink(pal.categoryHeader).write("📚 LIST", { x: 6, y: 6 }, "MatrixChunky8");
  
  // Tabs
  renderTabs(ink, pal);
  
  // Content based on tab
  switch (currentTab) {
    case "pieces": renderPieces(ink, pal); break;
    case "commands": renderCommands(ink, pal); break;
    case "api": renderAPI(ink, pal); break;
    default: renderAll(ink, pal);
  }
  
  // Detail panel if item selected
  if (selectedItem) renderDetail(ink, pal);
}

function act({ event: e, jump, needsPaint }) {
  // Tab switching
  // Category expand/collapse
  // Item selection
  // Search input
  // Scroll handling
  // Keyboard navigation
}

function leave({ store }) {
  store["list:tab"] = currentTab;
  store["list:scrolls"] = scrollPositions;
  store["list:expanded"] = Array.from(expandedCategories);
  store.persist("list:tab", "list:scrolls", "list:expanded");
}
```

---

## Responsive Design

### Screen Size Breakpoints
```js
const TINY = 128;   // ~128x128 (minimal AC resolution)
const SMALL = 192;  // ~192x192  
const MEDIUM = 320; // ~320x240
const LARGE = 480;  // Full detail mode
```

### Tiny Mode (width < 128)
- **No tabs** — Single "All" view only
- **No categories** — Flat alphabetical list
- **Compact rows**: Name only, no description
- **No detail panel** — Direct jump on tap
- **Single column**, ~10px row height
- **No search** — Just scroll

```
┌──────────┐
│ LIST     │
├──────────┤
│ box      │
│ chat     │
│ clock    │
│ colors   │
│ fill     │
│ line     │
│ mood     │
└──────────┘
```

### Small Mode (128 ≤ width < 192)
- **Tabs as icons** — 📦 🎯 📡 (no text)
- **Categories collapsed** by default
- **Short descriptions** — Truncated to ~15 chars
- **No detail panel** — Inline expand or direct jump

```
┌─────────────────┐
│ LIST   [📦🎯📡]│
├─────────────────┤
│ ▸ 🎨 Creative   │
│ ▼ 🎵 Audio      │
│   clock   Mus.. │
│   tone    Play. │
│ ▸ 💬 Social     │
└─────────────────┘
```

### Medium Mode (192 ≤ width < 320)
- **Full tabs** with text
- **Categories visible**
- **Descriptions** — Truncated to ~25 chars
- **Detail panel** — Overlay/modal on selection

```
┌────────────────────────────┐
│ 📚 LIST        [search: _] │
├────────────────────────────┤
│ [All] [Pieces] [Cmds] [API]│
├────────────────────────────┤
│ ▼ 🎨 Creative Tools (12)   │
│   box       Draw rectangles│
│   colors    An index of... │
└────────────────────────────┘
```

### Large Mode (width ≥ 320)
- **Full UI** as designed
- **Side panel** for details (or overlay)
- **Full descriptions**
- **Keyboard shortcuts visible**

### Adaptive Functions
```js
function getLayoutMode(screen) {
  if (screen.width < 128) return "tiny";
  if (screen.width < 192) return "small";
  if (screen.width < 320) return "medium";
  return "large";
}

function getRowHeight(mode) {
  return { tiny: 10, small: 12, medium: 14, large: 16 }[mode];
}

function getMaxDescLength(mode) {
  return { tiny: 0, small: 15, medium: 25, large: 50 }[mode];
}

function showTabs(mode) {
  return mode !== "tiny";
}

function showCategories(mode) {
  return mode !== "tiny";
}

function showSearch(mode) {
  return mode === "medium" || mode === "large";
}
```

### Dynamic Recalculation
- Recalculate layout on `screen` resize
- Store scroll position per mode (tiny scrolls faster)
- Button hitboxes scale with row height

---

## Open Questions

1. **Detail panel placement**: Overlay? Side panel? New screen?
2. **Search scope**: Just names or also descriptions?
3. **API view depth**: Show all params inline or click-to-expand?
4. **Favorites/Bookmarks**: Allow starring items?

---

## References
- `colors.mjs` — Scrollable categorized list with color swatches
- `moods.mjs` — Virtual viewport, day-grouped items, theme support
- `chat.mjs` — Complex UI with multiple panels
- `prompt.mjs` — Autocomplete, command parsing

---

## Timeline Estimate
- Phase 1 (Data): 1-2 hours
- Phase 2 (Core UI): 3-4 hours
- Phase 3 (Detail): 2-3 hours  
- Phase 4 (Polish): 2-3 hours

**Total: ~8-12 hours**
