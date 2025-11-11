# SpiderLily GameBoy Auto-Loader (slgb) - Implementation Plan

## Overview
Create a custom AC piece `slgb.mjs` that automatically loads and plays the SpiderLily.gbc ROM from the local assets directory without requiring drag-and-drop interaction.

## Current Stack Analysis

### GameBoy System Flow
1. **File Drop** → User drags `.gb` or `.gbc` file into browser
2. **bios.mjs** → Handles file drop event (line ~15489-15505)
   - Reads file as ArrayBuffer
   - Creates romData object with: name, originalName, romData, isGameBoyColor
   - Calls `loadGameboyROM(romData)` (line ~2188-2230)
3. **loadGameboyROM** → Initializes WasmBoy emulator
   - Imports `./dep/wasmboy/wasmboy.ts.esm.js`
   - Configures hidden canvas (160x144) for rendering
   - Sets up `updateGraphicsCallback` to send frame data via `acDISK_SEND`
   - Loads ROM bytes via `gameboyEmulator.loadROM()`
   - Starts playback with `gameboyEmulator.play()`
   - Jumps to `gameboy` piece via send({ type: "jump", content: { piece: "gameboy" }})
4. **gameboy.mjs** → Display piece that:
   - Receives frame data via `sound.gameboy.frame` (92160 RGBA pixels)
   - Renders GameBoy screen using `paste()` with integer scaling
   - Handles input (buttons, keyboard) and sends to bios via `gameboy:input` messages
   - Displays ROM metadata (title, color support) in HUD

### Asset Loading Patterns
Looking at existing pieces (digitpain1.mjs, sno.mjs, prutti.mjs):
- Use `net.preload()` API in boot function
- Paths can be relative: `aesthetic.computer/disks/digitpain/1/0.webp`
- Or absolute: `https://assets.aesthetic.computer/sno/...`
- Local dev uses: `/assets/[piece]/...`
- Production uses: `https://assets.aesthetic.computer/[piece]/...`

### SpiderLily ROM Location
- File: `/workspaces/aesthetic-computer/system/public/assets/false.work/SpiderLily.gbc`
- Should be accessible via: `/assets/false.work/SpiderLily.gbc` (dev)
- Or: `https://assets.aesthetic.computer/false.work/SpiderLily.gbc` (production)

## Implementation Plan

### 1. Create `slgb.mjs` Piece
File: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/slgb.mjs`

**Structure:**
```javascript
// slgb (SpiderLily GameBoy) - Auto-loads SpiderLily.gbc ROM
// false.work - 2025.10.31

let romLoaded = false;
let loadError = null;

export async function boot({ net, debug, send }) {
  // Construct path based on environment
  const basePath = debug 
    ? "/assets/false.work" 
    : "https://assets.aesthetic.computer/false.work";
  
  const romUrl = `${basePath}/SpiderLily.gbc`;
  
  try {
    console.log("🕷️ Loading SpiderLily ROM from:", romUrl);
    
    // Fetch the ROM file
    const response = await fetch(romUrl);
    if (!response.ok) {
      throw new Error(`Failed to load ROM: ${response.status}`);
    }
    
    const romData = await response.arrayBuffer();
    console.log("🕷️ ROM loaded:", romData.byteLength, "bytes");
    
    // Create romData object matching bios.mjs format
    const romDataObj = {
      name: "SpiderLily",
      originalName: "SpiderLily.gbc",
      romData: romData,
      isGameBoyColor: true
    };
    
    // Send load command to bios
    send({
      type: "gameboy:load-rom",
      content: romDataObj
    });
    
    romLoaded = true;
    console.log("🕷️ ROM load command sent to BIOS");
    
  } catch (error) {
    console.error("🕷️ Failed to load SpiderLily ROM:", error);
    loadError = error.message;
  }
}

export function paint({ wipe, ink, write, screen }) {
  wipe("black");
  
  if (loadError) {
    ink("red").write(
      `Error loading SpiderLily ROM:\n${loadError}`,
      { center: "xy", size: 2 }
    );
  } else if (!romLoaded) {
    ink("white").write(
      "Loading SpiderLily...",
      { center: "xy", size: 2 }
    );
  }
  // Once ROM loads, bios will jump to gameboy piece automatically
}

export const meta = {
  title: "SpiderLily GameBoy",
  desc: "Auto-loads the SpiderLily.gbc ROM from false.work"
};
```

### 2. Add Message Handler in bios.mjs
Need to add a handler for `gameboy:load-rom` message that calls the existing `loadGameboyROM()` function.

**Location:** In the disk message handler section where other messages like `gameboy:input` are processed.

**Code to add:**
```javascript
// Handle ROM loading request from disk
if (type === "gameboy:load-rom") {
  console.log("🎮 BIOS received ROM load request:", content);
  loadGameboyROM(content);
  return;
}
```

### 3. Verify File Access
Confirm that `/assets/false.work/SpiderLily.gbc` is accessible via HTTP in dev mode.

### 4. Test Flow
1. Navigate to `aesthetic.computer/slgb`
2. Should see "Loading SpiderLily..." briefly
3. ROM loads from assets
4. BIOS initializes WasmBoy
5. Auto-jumps to `gameboy` piece
6. SpiderLily game starts playing

## Benefits
- No drag-and-drop required
- Direct link: `aesthetic.computer/slgb`
- Can be bookmarked, shared
- ROM loads from curated assets directory
- Reusable pattern for other pre-loaded ROMs

## Future Enhancements
- Could create more auto-loader pieces for different ROMs
- Could add a loading progress bar
- Could add custom branding/intro screen before jumping to gameboy
- Could pass custom metadata to gameboy piece (e.g., custom colors, labels)

## Files to Create/Modify
1. **CREATE:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/slgb.mjs`
2. **MODIFY:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`
   - Add `gameboy:load-rom` message handler

## Dependencies
- SpiderLily.gbc ROM already exists in assets
- WasmBoy emulator already integrated
- GameBoy display piece already working
- Asset loading patterns already established

## Risk Assessment
- **Low Risk:** Following established patterns
- ROM file must be accessible via HTTP (should work if in public/assets)
- Message passing to bios is well-established pattern
- Reusing existing loadGameboyROM function (no new emulator code)
