# KidLisp Editor Architecture Analysis

## Current State: The Problem

**File:** `system/public/kidlisp.com/index.html`  
**Size:** 6,682 lines (HTML + CSS + JS all inline)  
**Functions:** 69  
**Event Listeners:** 48  
**Global Variables:** ~100+  

### Issues

1. **Monolithic Blob** - Everything in one file with no separation of concerns
2. **No Module System** - Just inline `<script type="module">` with global variable soup
3. **Event Listener Chaos** - Multiple `document.addEventListener('click')` fighting each other
4. **State Management** - Scattered `let` declarations across 4000+ lines
5. **Initialization Order** - Variables like `acToken` declared in wrong order, causing bugs
6. **UI Components Mixed** - Platform selector, language dropdown, auth, keeps, editor all intertwined
7. **CSS in HTML** - ~1700 lines of inline styles

---

## Current Code Structure (Approximate Line Ranges)

```
Lines     Section
------    -------
1-170     HTML head, meta, fonts
170-2540  CSS (inline <style>)
2540-2558 External scripts (Split.js, Monaco)
2558-2685 i18n translations
2685-2770 Language/translation functions
2770-3010 Split.js panel layout
3010-3500 Mobile panel collapse/drag system  
3500-3630 Center square drag positioning
3630-3800 Console logging + QR code
3800-3900 URL/database code loading
3900-4600 Monaco Editor setup + syntax highlighting
4600-4720 Language dropdown logic
4720-4860 Theme toggle
4860-4950 Reference panel wizard
4950-5040 Playback state (play/pause/stop)
5040-5200 iframe postMessage communication
5200-5550 Test runner integration
5550-5700 Function word grid
5700-5790 Tab switching
5790-6000 Auth0 (AC login)
6000-6100 initACLogin + event binding
6100-6200 Platform selector (NEW)
6200-6400 Keeps (Tezos wallet/minting)
6400-6682 More keeps + misc
```

---

## Identified Component Groups

### 1. **Core Editor** (should be its own module)
- Monaco Editor setup
- Syntax highlighting / decorations
- Line numbers + padding
- Clear button
- Code loading from URL/localStorage

### 2. **Playback Controller** (should be its own module)
- `isPlaying`, `isPaused`, `lastCode` state
- `updatePreview()`, `stopPlayback()`
- iframe postMessage communication
- Play/Pause/Stop button handling

### 3. **Layout Manager** (should be its own module)
- Split.js initialization
- Mobile panel system
- Panel collapse/expand
- Center square positioning

### 4. **UI Components** (each should be separate)
- Platform selector dropdown
- Language dropdown
- Theme toggle
- QR code modal
- Console panel
- Reference panel / wizard

### 5. **Auth System** (should be its own module)
- Auth0 SDK loading
- Login/logout flow
- Token management (`acToken`)
- UI state updates

### 6. **Keeps/Blockchain** (should be its own module)
- Tezos SDK loading
- Wallet connection
- Minting flow
- Transaction state

---

## Proposed New Architecture

```
kidlisp.com/
├── index.html           (slim: just HTML structure + boot)
├── styles/
│   ├── main.css         (core layout)
│   ├── editor.css       (monaco overrides)
│   ├── components.css   (dropdowns, buttons, panels)
│   └── themes.css       (light/dark)
├── js/
│   ├── app.js           (main entry, orchestrator)
│   ├── state.js         (centralized state management)
│   ├── editor/
│   │   ├── monaco.js    (editor setup)
│   │   ├── syntax.js    (highlighting + decorations)
│   │   └── actions.js   (clear, format, etc)
│   ├── playback/
│   │   ├── controller.js (play/pause/stop logic)
│   │   └── iframe.js     (postMessage bridge)
│   ├── layout/
│   │   ├── splits.js     (Split.js setup)
│   │   ├── mobile.js     (mobile collapse)
│   │   └── panels.js     (panel management)
│   ├── components/
│   │   ├── platform-selector.js
│   │   ├── language-dropdown.js
│   │   ├── theme-toggle.js
│   │   ├── qr-modal.js
│   │   └── console.js
│   ├── auth/
│   │   ├── auth0.js
│   │   └── ui.js
│   └── keeps/
│       ├── tezos.js
│       └── minting.js
└── lib/
    └── (vendored deps if needed)
```

---

## Centralized State Object

```javascript
// state.js
export const state = {
  // Editor
  editor: null,
  code: '',
  
  // Playback
  isPlaying: false,
  isPaused: false,
  lastCode: '',
  
  // Platform
  currentPlatform: 'aesthetic-computer',
  
  // Auth
  auth0Client: null,
  user: null,
  token: null,
  handle: null,
  
  // Keeps
  wallet: null,
  walletAddress: null,
  isMinting: false,
  
  // UI
  theme: 'auto',
  language: 'en',
  
  // Layout
  panelStates: {},
  isMobile: false
};

// Event emitter for state changes
export const events = new EventTarget();

export function setState(key, value) {
  const old = state[key];
  state[key] = value;
  events.dispatchEvent(new CustomEvent('stateChange', {
    detail: { key, old, value }
  }));
}
```

---

## Event System (Replace scattered listeners)

```javascript
// Instead of multiple document.addEventListener('click')
// Use a single delegated handler:

document.addEventListener('click', (e) => {
  // Close all dropdowns
  if (!e.target.closest('.dropdown-trigger')) {
    closeAllDropdowns();
  }
  
  // Route to specific handlers
  const handler = e.target.closest('[data-action]');
  if (handler) {
    const action = handler.dataset.action;
    actions[action]?.(e, handler);
  }
});
```

---

## Migration Path

### Phase 1: Extract CSS (Low risk)
1. Move all `<style>` content to external `.css` files
2. Keep JS untouched
3. Test thoroughly

### Phase 2: Extract State (Medium risk)
1. Create `state.js` with all global variables
2. Import and use throughout
3. Add event emitter for reactivity

### Phase 3: Extract Modules (Higher risk)
1. Start with isolated features (QR modal, theme toggle)
2. Move to larger systems (auth, keeps)
3. Finally extract editor + playback

### Phase 4: Event Consolidation
1. Replace multiple `addEventListener` calls
2. Use event delegation
3. Data attributes for actions

---

## Quick Wins (Can do now)

1. **Move `acToken` declaration to top** - Already done
2. **Create `stopPlayback()` function** - Already done
3. **Consolidate dropdown close logic** - Single handler for all dropdowns
4. **Use CSS classes instead of inline styles** - For dropdowns open/close

---

## Decision Point

**Option A: Incremental Refactor**  
- Lower risk, slower progress
- Keep adding features on shaky foundation
- Tech debt compounds

**Option B: Clean Room Rewrite**  
- Create new `kidlisp-v2/` alongside current
- Build with proper architecture from start
- Swap when ready

**Option C: Hybrid**  
- Extract CSS first (quick win)
- Create state.js module
- Progressively migrate JS to modules
- Keep HTML as shell

---

## Recommendation

Go with **Option C (Hybrid)** starting with:

1. **Extract CSS to files** - 1 hour of work, huge clarity gain
2. **Create `state.js`** - Centralize all those scattered `let` variables
3. **Create `events.js`** - Single event delegation system
4. **Migrate one component at a time** - Start with platform-selector

Would you like me to start with any of these?
