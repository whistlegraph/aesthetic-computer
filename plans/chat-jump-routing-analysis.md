# Chat Message Link Routing Analysis

## Problem Statement

When entering `'shop'` in chat and clicking on it, the `jump("shop")` call goes directly to the `shop` piece rather than routing through `shop.aesthetic.computer` like typing `shop` in `prompt.mjs` does.

Similarly, one-time commands like `'flip'`, `'flop'`, `'right'`, `'left'` don't work via chat links because they're prompt-specific commands that operate on `system.painting` and don't exist as standalone pieces.

## Solution Implemented

### Link Confirmation Modal

Instead of jumping directly when a link is clicked, chat.mjs now shows a **confirmation modal** that:

1. **Displays the action type** - "Run command?", "Open URL?", "Go to profile?", etc.
2. **Shows the link/command text** (truncated if needed)
3. **Provides Yes/No buttons** for user confirmation
4. **Blocks all other interactions** while the modal is open

### How It Works

```javascript
// When a link is clicked, instead of:
jump(innerPrompt);

// We now set up a modal:
linkConfirmModal = {
  type: "prompt",           // Type determines the action label
  text: jumpTarget,         // The actual jump target
  displayText: innerPrompt, // What to show the user
  action: () => jump(jumpTarget)  // Closure that executes on "Yes"
};
```

### Link Types Handled

| Type | Modal Label | Example |
|------|-------------|---------|
| `prompt` | "Run command?" | `'shop'`, `'flip'` |
| `url` | "Open URL?" | `https://example.com` |
| `handle` | "Go to profile?" | `@jeffrey` |
| `painting` | "Open painting?" | `#k3d` |
| `kidlisp` | "Run code?" | `$ceo`, `(def x 1)` |

### Routing Through Prompt

Commands are routed through prompt.mjs to leverage its full command handling:

```javascript
// 'shop' becomes "prompt shop" which then:
// - prompt.mjs handles the "shop" command
// - Opens shop.aesthetic.computer (not the shop piece)

// 'flip' becomes "prompt flip" which then:
// - prompt.mjs handles the "flip" command  
// - Transforms system.painting (vertical flip)
```

## UI/UX Details

- **Semi-transparent backdrop** (alpha 220) dims the chat
- **Centered modal box** with dark background and subtle border
- **Green "Yes" button** - confirms and executes action
- **Red "No" button** - cancels and closes modal
- **Click outside** or **Escape key** also closes modal
- **Hover states** on buttons for visual feedback
- **Command descriptions** - shows what the command will do (if known)

## Command Registry

Created `/lib/prompt-commands.mjs` with:

### `commandDescriptions` Object
Maps command names to human-readable descriptions:
```javascript
export const commandDescriptions = {
  shop: "Open the Aesthetic Computer shop",
  flip: "Flip your painting vertically",
  flop: "Flip your painting horizontally",
  // ... etc
};
```

### `getCommandDescription(command)` Function
Looks up a command's description with smart fallbacks:
- Direct match: `"flip"` → `"Flip your painting vertically"`
- Base command: `"shop/item"` → uses `"shop"` description
- Colon variants: `"tape:add"` → uses `"tape"` description

### `isPromptOnlyCommand(command)` Function
Returns true for commands that execute immediately without loading a piece:
- `flip`, `flop`, `right`, `left`, `wipe`
- `cut`, `stop`, `done`, etc.

## Files Modified

- `system/public/aesthetic.computer/disks/chat.mjs` - Modal system
- `system/public/aesthetic.computer/lib/prompt-commands.mjs` - Command registry (new)
- `plans/chat-jump-routing-analysis.md` - This document

---

## r8Dio Radio Player (Added 2024.12)

### Streaming Audio API

Added to `bios.mjs` a new streaming audio system for live radio/audio streams:

#### Message Types

| Message | Direction | Purpose |
|---------|-----------|---------|
| `stream:play` | disk→bios | Start playing a URL stream |
| `stream:pause` | disk→bios | Pause the stream |
| `stream:resume` | disk→bios | Resume paused stream |
| `stream:stop` | disk→bios | Stop and cleanup stream |
| `stream:volume` | disk→bios | Set stream volume (0-1) |
| `stream:frequencies` | disk→bios | Request frequency data |
| `stream:playing` | bios→disk | Stream started successfully |
| `stream:paused` | bios→disk | Stream paused |
| `stream:stopped` | bios→disk | Stream stopped |
| `stream:error` | bios→disk | Error occurred |
| `stream:frequencies-data` | bios→disk | Frequency data for visualizer |

#### Implementation Details

- Uses HTML5 `Audio` element for streaming
- Connects to Web Audio API `AnalyserNode` for frequency data
- Stored in `streamAudio[id]` object with `{ audio, analyser, source }`
- Supports multiple concurrent streams via unique IDs

### r8dio.mjs Piece

New piece for playing R8dio.dk Danish talk radio:

- **Stream URL**: `https://s3.radio.co/s7cd1ffe2f/listen`
- **32-bar visualizer** with purple/pink gradient colors
- **Play/Pause button** with hover states
- **Volume control** via arrow keys
- **Keyboard shortcuts**: Space (play/pause), ↑↓ (volume), Esc (exit)

### HUD Styling

In `disk.mjs`, special handling for r8dio piece label:
- Displays as "r8Dio" (mixed case)
- "8D" rendered in **magenta** (`\255,0,255\`)
- "r" and "io" in pink (`\255,200,220\`)

### Files Added/Modified

- `bios.mjs` - Added `streamAudio` object and stream message handlers
- `disks/r8dio.mjs` - New radio player piece
- `disks/prompt.mjs` - Added `r8dio:web` and `radio:web` redirects
- `lib/disk.mjs` - Special HUD label for r8dio piece
