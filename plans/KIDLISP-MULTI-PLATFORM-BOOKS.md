# KidLisp Multi-Platform Learning Books

## Overview

Transform kidlisp.com into a multi-platform learning environment where users can learn KidLisp through different "runtime targets" — each with its own API surface and interactive book.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  kidlisp.com                                                    │
├─────────────────────────────────────────────────────────────────┤
│  Header: Logo | [Aesthetic.Computer ▼] | @user | Keeps | Book   │
│                        │                                        │
│                        ├─► Aesthetic.Computer (default)         │
│                        ├─► Teenage Engineering Playdate         │
│                        └─► FF1 Art Computer                     │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────────────┐  ┌──────────────────────────────────────┐ │
│  │                  │  │                                      │ │
│  │   Code Editor    │  │   Preview / Iframe                   │ │
│  │   (Monaco)       │  │   (platform-specific)                │ │
│  │                  │  │                                      │ │
│  └──────────────────┘  └──────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Tab Renaming

| Old Name | New Name | Purpose |
|----------|----------|---------|
| Code | **Book** | Platform-specific learning content (paginated, Little Schemer style) |

## Platform Configurations

### 1. Aesthetic.Computer (Default)

**Runtime:** Browser-based, WebGL canvas, real-time  
**Preview:** `https://aesthetic.computer/$code?noauth=true` iframe  
**Book:** "KidLisp on Aesthetic.Computer"  
**API Surface:**
- `wipe`, `ink`, `line`, `box`, `oval`, `plot`
- `width`, `height`, `now`, `frame`
- `tap`, `pan`, `draw`, `lift`
- `repeat`, `later`, `def`
- Audio: `synth`, `sample`
- Networking: realtime multiplayer

**Code State:** Editable, saves to AC backend, can be "kept" as NFT

### 2. Teenage Engineering Playdate

**Runtime:** Lua-based, 1-bit 400×240 display, crank input  
**Preview:** Playdate Simulator view (or mock)  
**Book:** "KidLisp for Playdate"  
**API Surface (KidLisp → Playdate mapping):**
```lisp
; KidLisp           → Playdate Lua
(wipe)              → gfx.clear()
(ink "white")       → gfx.setColor(gfx.kColorWhite)
(box x y w h)       → gfx.fillRect(x, y, w, h)
(line x1 y1 x2 y2)  → gfx.drawLine(x1, y1, x2, y2)
(crank)             → playdate.getCrankPosition()
(a-button)          → playdate.buttonIsPressed(playdate.kButtonA)
(d-pad)             → playdate.buttonIsPressed(...)
```

**Constraints:**
- 1-bit color only (black/white)
- 400×240 fixed resolution
- 30 FPS default (max 50)
- Crank as unique input

**Code State:** Editable, exports to `.pdx` bundle

### 3. FF1 Art Computer (Feral File)

**Runtime:** FF OS, Linux-based, WebGL/WebAssembly, DP-1 protocol  
**Preview:** Mock FF1 display frame or actual kept piece (read-only from IPFS)  
**Book:** "KidLisp for FF1"  
**API Surface:** Same as Aesthetic.Computer (both WebGL) + DP-1 specific:
```lisp
; DP-1 playlist integration
(playlist-info)     → current playlist metadata
(artwork-info)      → current artwork metadata  
(display-info)      → screen resolution, orientation
```

**Kept Pieces Integration:**
- When a piece is "kept" on AC, it gets IPFS URL
- FF1 can play any kept piece via DP-1 protocol
- In FF1 mode, can browse your kept collection
- Code becomes **read-only** (viewing the immutable IPFS version)

**Code State:** Read-only when viewing kept pieces from IPFS

### 4. Ableton Live (Max for Live)

**Runtime:** Max for Live devices in Ableton Live  
**Preview:** Device parameter mock / Live connection  
**Book:** "KidLisp for Ableton"  
**Existing Work:** `ac-m4l/` — AC Metronome, AC Notepat, AC Prompt devices

**API Surface (KidLisp → Max/MSP mapping):**
```lisp
; MIDI output
(note pitch velocity duration)  → noteout
(cc controller value)           → ctlout
(bend value)                    → bendout

; Timing / Transport
(beat)              → current beat position
(tempo)             → live.tempo
(bar)               → bar number
(playing?)          → transport state

; Audio (via buffer~)
(sample name)       → buffer~ playback
(synth freq amp)    → cycle~ / saw~ etc.

; Live parameters
(param name value)  → live.remote~
(track-info)        → live.path / live.object
```

**Constraints:**
- Real-time audio thread (no blocking!)
- MIDI timing critical
- Works within Ableton's timeline/session view
- Device UI limitations (Max for Live patcher)

**Code State:** Editable, exports to `.amxd` device

### 5. Game Boy (GBDK) — 🔒 Coming Soon

**Runtime:** GBDK-2020 C compiler → Game Boy ROM  
**Preview:** Emulator (BGB/mGBA)  
**Book:** "KidLisp for Game Boy"  
**Existing Work:** `kidlisp-gameboy/` — Compiler in progress

**API Surface:**
```lisp
; 160×144, 4 colors (2-bit)
(wipe)              → cls()
(ink 0-3)           → set drawing color
(sprite id x y)     → move_sprite()
(tile x y id)       → set_bkg_tile()
(joypad)            → joypad() buttons
(sound ch freq)     → NR registers
```

**Constraints:**
- 160×144 resolution, 4 shades of green
- 8KB VRAM, limited sprites (40 max, 10 per line)
- No floating point!
- Sound: 4 channels (2 pulse, 1 wave, 1 noise)

**Code State:** Editable, exports to `.gb` ROM

### 6. Nintendo 64 — 🔒 Experimental

**Runtime:** libdragon / N64 homebrew  
**Preview:** Emulator (Ares/simple64)  
**Book:** "KidLisp for N64"  
**Existing Work:** `kidlisp-n64/` — Early experiments

**API Surface:**
```lisp
; 320×240 or 640×480
(wipe color)        → rdp_fill_rectangle
(box x y w h)       → rdp primitives
(sprite id x y)     → rdp_draw_sprite
(3d-tri ...)        → RDP triangle commands
(controller port)   → controller_scan()
```

**Constraints:**
- RDP (Reality Display Processor) rendering
- 4MB RAM (8MB with expansion pak)
- 4 controller ports
- N64 controller: analog stick, C-buttons, Z-trigger

**Code State:** Editable, exports to `.z64` ROM

## Book Structure (per platform)

Each platform's book follows the "Little Schemer" style:

```
┌─────────────────────────────────────────┐
│  KidLisp on [Platform]                  │
│                                         │
│  Chapter 1: Drawing                     │
│  ─────────────────────────────          │
│                                         │
│  What does this do?                     │
│                                         │
│  (wipe "blue")                          │
│  (ink "yellow")                         │
│  (box 10 10 50 50)                      │
│                                         │
│  [Try it →]                             │
│                                         │
│                         ◄ 1/24 ►        │
└─────────────────────────────────────────┘
```

### Book Content Format (JSON/Markdown)

```json
{
  "platform": "aesthetic-computer",
  "title": "KidLisp on Aesthetic.Computer",
  "chapters": [
    {
      "title": "Drawing",
      "pages": [
        {
          "question": "What does this do?",
          "code": "(wipe \"blue\")\n(ink \"yellow\")\n(box 10 10 50 50)",
          "answer": "It clears the screen blue, then draws a yellow box.",
          "tryIt": true
        }
      ]
    }
  ]
}
```

### Book Storage Location

```
system/public/kidlisp.com/books/
├── aesthetic-computer.json
├── ableton.json
├── playdate.json
├── ff1.json
├── gameboy.json
└── n64.json
```

## Implementation Plan

### Phase 1: UI Restructuring
- [ ] Rename "Code" tab to "Book"
- [ ] Add platform dropdown to preview header (next to "Aesthetic.Computer")
- [ ] Store selected platform in localStorage
- [ ] Platform change triggers:
  - Book content swap
  - Preview iframe URL change (future)
  - API reference update (future)

### Phase 2: Book Viewer
- [ ] Create paginated book component
- [ ] Load book JSON based on platform
- [ ] "Try it" button loads code into editor
- [ ] Page navigation (prev/next, chapter jump)
- [ ] Progress tracking (localStorage)

### Phase 3: Aesthetic.Computer Book
- [ ] Write initial chapters:
  1. Drawing (wipe, ink, box, line, oval)
  2. Animation (frame, now, sim)
  3. Interaction (tap, draw, pan)
  4. Variables (def)
  5. Loops (repeat)
  6. Time (later, 0.5s)
  7. Sound (synth, sample)
  8. Making a Game

### Phase 4: FF1 Integration
- [ ] "Kept Pieces" browser in FF1 mode
- [ ] Load piece from IPFS URL
- [ ] Read-only code editor mode
- [ ] Display IPFS/NFT metadata

### Phase 5: Playdate Integration
- [ ] KidLisp → Playdate Lua transpiler (see `kidlisp-gameboy/compiler/`)
- [ ] Playdate simulator mock (or WebAssembly sim)
- [ ] Playdate-specific book content
- [ ] Export to `.pdx` functionality

### Phase 6: Ableton Integration
- [ ] KidLisp → Max/MSP JavaScript transpiler
- [ ] Live device preview / parameter mock
- [ ] Ableton-specific book content (MIDI, timing, audio)
- [ ] Export to `.amxd` device

### Phase 7: Retro Consoles (Game Boy, N64)
- [ ] KidLisp → C transpiler for GBDK/libdragon
- [ ] Emulator embeds for preview
- [ ] Platform-specific books
- [ ] Export to ROM files

## Technical Notes

### Platform Detection State

```javascript
const platforms = {
  'aesthetic-computer': {
    name: 'Aesthetic.Computer',
    icon: '🟪',
    previewUrl: (code) => `https://aesthetic.computer/${code}?noauth=true`,
    bookPath: '/books/aesthetic-computer.json',
    editable: true,
    runtime: 'browser',
    status: 'active'
  },
  'ff1': {
    name: 'FF1 Art Computer',
    icon: '🖼️',
    previewUrl: (ipfsUrl) => ipfsUrl, // From kept piece
    bookPath: '/books/ff1.json',
    editable: false, // Read-only for kept pieces
    runtime: 'browser',
    status: 'active'
  },
  'ableton': {
    name: 'Ableton Live',
    icon: '🎹',
    previewUrl: null, // Device mock
    bookPath: '/books/ableton.json',
    editable: true,
    runtime: 'max-js-transpile',
    status: 'active'
  },
  'playdate': {
    name: 'Playdate',
    icon: '🎮',
    previewUrl: null, // Custom renderer
    bookPath: '/books/playdate.json',
    editable: true,
    runtime: 'lua-transpile',
    status: 'coming-soon'
  },
  'gameboy': {
    name: 'Game Boy',
    icon: '👾',
    previewUrl: null, // Emulator embed
    bookPath: '/books/gameboy.json',
    editable: true,
    runtime: 'gbdk-c-transpile',
    status: 'coming-soon'
  },
  'n64': {
    name: 'Nintendo 64',
    icon: '🕹️',
    previewUrl: null, // Emulator embed
    bookPath: '/books/n64.json',
    editable: true,
    runtime: 'libdragon-c-transpile',
    status: 'experimental'
  }
};
```

### FF1 Kept Piece Loading

```javascript
async function loadKeptPiece(codeId) {
  // Fetch piece metadata from AC API
  const meta = await fetch(`/api/piece/${codeId}`);
  const { ipfsUrl, title, artist } = await meta.json();
  
  // Load code from IPFS (read-only)
  const code = await fetch(ipfsUrl).then(r => r.text());
  
  // Set editor to read-only mode
  editor.updateOptions({ readOnly: true });
  editor.setValue(code);
  
  // Update preview with IPFS-hosted version
  updatePreview(ipfsUrl);
}
```

## Open Questions

1. **Playdate Simulation:** Should we build a web-based Playdate simulator, or just show static preview + export?

2. **Cross-Platform Pieces:** Can a piece be written once and run on multiple platforms with graceful degradation?

3. **Book Editing:** Should there be an admin interface to edit book content, or just JSON files in repo?

4. **FF1 Live Connection:** Could kidlisp.com connect directly to a user's FF1 via DP-1 for live preview?

## References

- [Playdate SDK Documentation](https://sdk.play.date/)
- [FF1 Art Computer](https://feralfile.com/install)
- [DP-1 Protocol](https://github.com/display-protocol/dp1)
- [The Little Schemer](https://mitpress.mit.edu/9780262560993/the-little-schemer/) (book format inspiration)
- [KidLisp Interpreter](/workspaces/aesthetic-computer/kidlisp/)
