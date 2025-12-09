# Prompt Tooltip System Plan

## Overview

Create an intelligent tooltip/overlay system that displays command documentation, parameter hints, and validation feedback while typing in the AC prompt. Uses MatrixChunky8 font for a compact, pixel-perfect aesthetic.

---

## Implementation Status: ✅ PHASE 1 COMPLETE

### What's Working
- **Tooltip state tracking** in `prompt.mjs` (lines 237-256)
- **Dual param system support**: `colonParams[]` and `spaceParams[]`
- **`updateTooltipState()`**: Parses text, finds matching doc, determines cursor context
- **`paintTooltip()`**: Renders signature, description, and suggestions
- **Docs fetching**: `tooltipDocs` populated from `net.requestDocs()`
- **Several commands documented** with colon/space params:
  - `notepat[:wave][:octave] [melody...]`
  - `tone[:wave] [frequency]`
  - `tezos <action> [network]`
  - `line[:thickness]`
  - `camera[:mode]`
  - `clock[:divisor]`

### AC's Dual Param System
```
command:colon1:colon2 space1 space2
        └───────┘     └──────────┘
        colon[0,1]    params[0,1]
```
- **Colon params**: Parsed from `tokens[0].split(":")` → `colon[]` array
- **Space params**: Parsed from `tokens.slice(1)` → `params[]` array
- Example: `notepat:square:5 twinkle` → colon=["square","5"], params=["twinkle"]

---

## Current State Analysis

### Existing Infrastructure

1. **`docs.js`** (Netlify function)
   - Contains comprehensive API documentation (`docs.api`)
   - Contains prompt command documentation (`docs.prompts`)
   - Structure: `{ sig, desc, colon?, params?, examples?, done, hidden? }`

2. **`prompt.mjs`** Tooltip System
   - `tooltipState` object tracks current command, params, cursor context
   - `updateTooltipState(text, cursorPos)` parses and updates state
   - `paintTooltip($, inputText)` renders the tooltip overlay
   - Called every paint frame when input is active

3. **Command Parsing (in parse.mjs)**
   ```javascript
   const colonSplit = tokens[0].split(":");
   tokens[0] = colonSplit[0];           // Command name
   colonParam = colonSplit.slice(1);    // Colon params array
   params = tokens.slice(1);            // Space params array
   ```

4. **Cursor Position Access**
   ```javascript
   const cursorPos = prompt.textPos();  // Character index in text
   const screenPos = prompt.pos(undefined, true);  // { x, y, w, h }
   ```

---

## Phase 2: TODO

### Enhancements Needed
- [ ] Use MatrixChunky8 font for compact tooltip rendering
- [ ] Better positioning (follow cursor horizontally)
- [ ] Highlight current param in signature
- [ ] Parameter validation and error display
- [ ] More commands documented with colon/params schema

### Commands Needing Documentation
- `$` (the painting piece) - has colon scale param
- `replay[:show]`
- `moods[:scale]`
- `oldline[:thickness]`

---

## Original Architecture (Reference)
    desc: "Record your screen.",
    params: {
      duration: {
        type: "number",
        required: false,
        default: 5,
        desc: "Recording duration in seconds (suffix 'f' for frames)",
        examples: ["5", "10", "30f"]
      },
      options: {
        type: "flags",
        values: ["mic", "nomic", "baktok"],
        desc: "Recording options"
      }
    }
  },
  keep: {
    sig: "keep <$code>",
    desc: "Mint KidLisp code as NFT.",
    params: {
      code: {
        type: "string",
        required: true,
        prefix: "$",
        desc: "KidLisp piece code to mint"
      }
    }
  },
  // ... etc
}
```

### 2. Tooltip State Machine

```javascript
// In prompt.mjs
let tooltipState = {
  visible: false,
  command: null,        // Current command being typed
  params: [],           // Parsed params so far
  currentParamIndex: 0, // Which param cursor is in
  suggestions: [],      // Valid values for current param
  error: null,          // Validation error message
  position: { x: 0, y: 0 }
};
```

### 3. Tooltip Parser

```javascript
function parseForTooltip(text, cursorPosition) {
  const tokens = text.split(" ");
  const command = tokens[0].split(":")[0]; // Strip colon params
  const colonParams = tokens[0].split(":").slice(1);
  const params = tokens.slice(1);
  
  // Find which token the cursor is in
  let charCount = 0;
  let currentTokenIndex = 0;
  for (let i = 0; i < tokens.length; i++) {
    if (cursorPosition <= charCount + tokens[i].length) {
      currentTokenIndex = i;
      break;
    }
    charCount += tokens[i].length + 1; // +1 for space
  }
  
  return {
    command,
    colonParams,
    params,
    currentTokenIndex,
    isInCommand: currentTokenIndex === 0,
    currentParamIndex: currentTokenIndex - 1
  };
}
```

### 4. Tooltip Renderer

```javascript
function paintTooltip($, tooltipState, cursorPos) {
  if (!tooltipState.visible) return;
  
  const FONT = "MatrixChunky8";
  const CHAR_W = 4;
  const CHAR_H = 8;
  const PADDING = 2;
  const MARGIN_Y = 4;
  
  // Position below cursor
  const tooltipY = cursorPos.y + cursorPos.h + MARGIN_Y;
  const tooltipX = cursorPos.x;
  
  // Build tooltip content
  let lines = [];
  
  // Line 1: Command signature
  if (tooltipState.command?.sig) {
    lines.push({ text: tooltipState.command.sig, color: [180, 180, 180] });
  }
  
  // Line 2: Current parameter hint
  if (tooltipState.currentParam) {
    const param = tooltipState.currentParam;
    let hint = `${param.name}`;
    if (param.type === "enum") {
      hint += `: ${param.values.join(" | ")}`;
    } else if (param.type === "number") {
      hint += `: <number>`;
    }
    if (!param.required) hint = `[${hint}]`;
    lines.push({ text: hint, color: [100, 200, 255] });
  }
  
  // Line 3: Description or error
  if (tooltipState.error) {
    lines.push({ text: `⚠ ${tooltipState.error}`, color: [255, 100, 100] });
  } else if (tooltipState.currentParam?.desc) {
    lines.push({ text: tooltipState.currentParam.desc, color: [150, 150, 150] });
  }
  
  // Calculate dimensions
  const maxWidth = Math.max(...lines.map(l => l.text.length)) * CHAR_W;
  const totalHeight = lines.length * CHAR_H + (lines.length - 1) * 2;
  
  // Clamp to screen bounds
  const finalX = Math.min(tooltipX, $.screen.width - maxWidth - PADDING * 2);
  const finalY = Math.min(tooltipY, $.screen.height - totalHeight - PADDING * 2);
  
  // Draw background
  $.ink(0, 0, 0, 200).box(
    finalX - PADDING, 
    finalY - PADDING, 
    maxWidth + PADDING * 2, 
    totalHeight + PADDING * 2
  );
  
  // Draw border
  $.ink(60, 60, 60).box(
    finalX - PADDING, 
    finalY - PADDING, 
    maxWidth + PADDING * 2, 
    totalHeight + PADDING * 2,
    "outline"
  );
  
  // Draw lines
  lines.forEach((line, i) => {
    $.ink(...line.color).write(
      line.text,
      { x: finalX, y: finalY + i * (CHAR_H + 2) },
      undefined, undefined, false, FONT
    );
  });
}
```

---

## Implementation Steps

### Phase 1: Documentation Updates (docs.js)

1. Add `params` schema to all existing prompts
2. Document new commands:
   - `tezos connect/disconnect/status`
   - `keep $code`
   - `mood`
   - `channel`
   - All other undocumented prompts

### Phase 2: Tooltip State Management (prompt.mjs)

1. Add tooltip state variables
2. Create `updateTooltipState(text, cursorPos)` function
3. Call on every `prompt:text:replace` event
4. Parse current input and match against docs

### Phase 3: Tooltip Rendering (prompt.mjs paint)

1. Add `paintTooltip()` function
2. Render after prompt text, before overlays
3. Handle screen edge clamping
4. Animate fade-in/out

### Phase 4: Validation System

1. Type checking (enum, number, string)
2. Required param validation
3. Show errors inline with red color
4. Prevent invalid submissions with feedback

### Phase 5: Enhanced Features

1. **Tab completion for params** - Tab through enum values
2. **Inline suggestions** - Ghost text for likely completions
3. **Command history hints** - Show recent uses of command
4. **Keyboard navigation** - Arrow keys to select suggestions

---

## Visual Design

```
┌─────────────────────────────────────┐
│ > tezos connect█                    │  ← Cursor here
│                                     │
│ ┌─────────────────────────────────┐ │
│ │ tezos <subcommand> [network]    │ │  ← Signature (gray)
│ │ network: ghostnet | mainnet     │ │  ← Current param (cyan)
│ │ Connect wallet to network       │ │  ← Description (dim)
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

Error state:
```
┌─────────────────────────────────────┐
│ > tezos blah█                       │
│                                     │
│ ┌─────────────────────────────────┐ │
│ │ tezos <subcommand>              │ │
│ │ subcommand: connect|disconnect  │ │
│ │ ⚠ Invalid: "blah"               │ │  ← Error (red)
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

---

## Commands Needing Documentation

Priority additions to `docs.prompts`:

```javascript
// Wallet/Blockchain
tezos: { subcommands: connect, disconnect, status }
keep: { params: $code }

// Recording
tape: { params: duration, mic, nomic, baktok, cut }
cut: {}

// Social
mood: { params: emoji }
scream: { params: message }
me: {}

// Painting
done: {}
yes: {}
print: {}
mint: {}
p: {} // alias for pain
pain: {}
dl: { params: scale }
download: { params: scale }

// Coding
code: { params: name }
edit: { params: piece }
run: { params: code }
channel: { params: channel_name }

// Settings
nonotifs: {}
notifs: {}
selfie: {}
cam: {}

// Navigation
docs: {}
email: { params: address }
handle: { params: new_handle }
```

---

## Files to Modify

1. **`system/netlify/functions/docs.js`**
   - Add `params` schema to all prompts
   - Document all missing commands

2. **`system/public/aesthetic.computer/disks/prompt.mjs`**
   - Add tooltip state management
   - Add `parseForTooltip()` function
   - Add `updateTooltipState()` function  
   - Add `paintTooltip()` in paint function
   - Modify `prompt:text:replace` handler

3. **`system/public/aesthetic.computer/lib/type.mjs`** (optional)
   - Expose additional cursor position info if needed

---

## Future Enhancements

1. **Fuzzy matching** - "tez" suggests "tezos"
2. **Command aliases** - Map "tz" to "tezos"
3. **Context-aware hints** - Different suggestions based on app state
4. **Learning system** - Prioritize frequently used commands
5. **Multilingual tooltips** - Match prompt translations
6. **Voice hints** - TTS for accessibility

---

## Dependencies

- MatrixChunky8 font (already loaded)
- docs.js endpoint (already exists)
- Cursor position API (already exists)

---

## Timeline Estimate

- Phase 1 (Docs): 2-3 hours
- Phase 2 (State): 1-2 hours  
- Phase 3 (Render): 2-3 hours
- Phase 4 (Validation): 2-3 hours
- Phase 5 (Enhancements): 4-6 hours

**Total: ~12-17 hours**
