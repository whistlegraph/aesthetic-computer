# 🩸 Artery TUI: Emacs eat Terminal Compatibility Plan

## Problem Statement

The `/artery-tui` rendering gets messed up when running inside Emacs `eat` (Emacs Application Terminal) pane:
- Resizing Emacs windows causes visual corruption
- The animated ASCII title ("blood flow" animation) glitches on resize
- Closing and reopening the eat pane causes janky rendering
- Partial screen updates create artifacts

## Root Cause Analysis

### Current Rendering Approach (Problems)

1. **Differential Updates**: The TUI uses `renderHeaderOnly()` for animation frames, only updating specific lines. This assumes:
   - The terminal maintains character positions across resizes
   - Previous content persists correctly
   - The cursor can be positioned absolutely with `moveTo()`

2. **Animation Loop**: 80ms interval (~12fps) blood flow animation constantly re-renders partial screen regions

3. **Full-Screen Redraws**: On resize, `render()` does a "full" redraw but:
   - Writes line-by-line with `writeLine()` 
   - Uses `CURSOR_HOME` + individual line writes instead of true full-screen clearing
   - Background fill happens separately from content

4. **eat Terminal Quirks**:
   - `eat` is an Emacs terminal emulator that may handle ANSI sequences differently
   - Window resize events may not cleanly clear the buffer
   - The `SIGWINCH` signal handling may race with eat's own redraw
   - eat may have different double-buffering behavior than traditional terminals

### Why Games/TUIs Handle This Differently

Games and robust TUIs typically use one of these patterns:

1. **True Double Buffering**: Build entire frame in memory, swap atomically
2. **Immediate Mode + Full Clear**: Clear screen every frame, redraw everything
3. **ncurses/blessed**: Library handles all the complexity
4. **sixel/framebuffer**: Bypass character grid entirely

## Proposed Solutions (Pick One or Hybrid)

### Option 1: Switch to `blessed` Library (Already Installed!)

The project has `blessed` in `devDependencies`. This is a proper ncurses-style library that handles:
- Screen buffering correctly
- Resize events properly  
- ANSI escape sequence quirks
- Double buffering

**Pros**:
- Already installed
- Battle-tested for terminal UIs
- Handles edge cases automatically
- Rich widget library (boxes, lists, forms)

**Cons**:
- Complete rewrite of rendering code
- Different API paradigm
- May be heavier than needed

**Effort**: ~1-2 days for full rewrite

### Option 2: Switch to `terminal-kit` (Already Installed!)

Also in `devDependencies`. Modern terminal manipulation library.

**Pros**:
- Already installed
- Game-oriented features (sprites, animation)
- Built-in screen buffering
- Proper resize handling

**Cons**:
- Still a significant rewrite
- Different mental model

**Effort**: ~1-2 days

### Option 3: Fix Current Implementation (Minimal Changes)

Add proper double-buffering and full-frame rendering:

```javascript
class ArteryTUI {
  constructor() {
    // Add screen buffer
    this.screenBuffer = [];
    this.lastRenderedBuffer = [];
    // ...
  }
  
  // Build entire frame in buffer first
  buildFrame() {
    this.screenBuffer = [];
    // Build all lines into buffer...
  }
  
  // Only output what changed (or everything on resize)
  flushFrame(forceFullRedraw = false) {
    if (forceFullRedraw) {
      // Clear and redraw everything
      this.write(CLEAR_SCREEN);
      this.write(CURSOR_HOME);
      for (let i = 0; i < this.screenBuffer.length; i++) {
        this.write(this.screenBuffer[i]);
        this.write('\n');
      }
    } else {
      // Only update changed lines
      for (let i = 0; i < this.screenBuffer.length; i++) {
        if (this.screenBuffer[i] !== this.lastRenderedBuffer[i]) {
          this.write(moveTo(i + 1, 1));
          this.write(this.screenBuffer[i]);
        }
      }
    }
    this.lastRenderedBuffer = [...this.screenBuffer];
  }
  
  // Handle resize with full redraw
  onResize() {
    this.width = process.stdout.columns || 80;
    this.height = process.stdout.rows || 24;
    this.buildFrame();
    this.flushFrame(true); // Force full redraw on resize
  }
}
```

**Pros**:
- Minimal code changes
- Keep current look/feel
- Better understanding of what's happening

**Cons**:
- May still have edge cases
- Reinventing the wheel
- Animation still runs at 12fps

**Effort**: ~4-6 hours

### Option 4: Emacs-Specific Fixes (vterm instead of eat)

Use Emacs `vterm` instead of `eat`:
- `vterm` uses libvterm and is more compatible with complex TUIs
- Better ANSI escape sequence handling
- True terminal emulation

**Implementation**:
```elisp
;; In aesthetic.el, use vterm for artery
(defun artery-start ()
  (interactive)
  (let ((buf (get-buffer-create "*artery*")))
    (with-current-buffer buf
      (vterm-mode)
      (vterm-send-string "artery\n"))
    (switch-to-buffer buf)))
```

**Pros**:
- No changes to artery-tui.mjs
- vterm handles edge cases better
- Better performance

**Cons**:
- Requires vterm installation (libvterm C library)
- Doesn't fix root cause
- Only works in Emacs

**Effort**: ~1-2 hours

### Option 5: Hybrid - Game Loop + Full Redraw

Treat it like a game: always do full frame updates, never partial:

```javascript
// Remove animation interval, use requestAnimationFrame-style loop
let lastFrame = 0;
const FPS = 10; // Lower fps = less flickering
const frameInterval = 1000 / FPS;

function gameLoop(timestamp) {
  if (timestamp - lastFrame >= frameInterval) {
    lastFrame = timestamp;
    
    // Always build and flush full frame
    this.buildFrame();
    this.flushFrame(false); // Use diff, but frame is complete
  }
  
  // Use setImmediate instead of setTimeout for better timing
  setImmediate(() => gameLoop(Date.now()));
}
```

**Pros**:
- Simple mental model
- Consistent behavior
- No animation artifacts

**Cons**:
- More CPU usage
- Potential flicker if not buffered properly

**Effort**: ~2-3 hours

## Recommended Approach: Option 3 + Option 5 Hybrid

1. **Add proper screen buffering** to current implementation
2. **Force full redraws on resize** with debouncing
3. **Lower animation framerate** to reduce update frequency (8fps instead of 12)
4. **Add alternate rendering mode** for eat terminal detection

### Detection of eat terminal:
```javascript
// Detect if running in Emacs eat
const isEatTerminal = process.env.TERM_PROGRAM === 'eat' || 
                      process.env.INSIDE_EMACS?.includes('eat');

// Use simplified rendering in eat
if (isEatTerminal) {
  // Disable animation or use slower rate
  this.animationEnabled = false;
  // Force full redraws more aggressively
  this.forceFullRedraw = true;
}
```

### Additional eat-specific fixes:

1. **Disable alternate screen buffer** - eat may handle it poorly:
```javascript
// Don't use these in eat:
// '\x1b[?1049h' (enable alternate screen)
// '\x1b[?1049l' (disable alternate screen)
```

2. **Use simpler box drawing** - eat may not render all unicode correctly:
```javascript
const BOX_EAT = {
  topLeft: '+',
  topRight: '+',
  horizontal: '-',
  vertical: '|',
  // ...
};
```

3. **Debounce resize events**:
```javascript
let resizeTimeout;
process.stdout.on('resize', () => {
  clearTimeout(resizeTimeout);
  resizeTimeout = setTimeout(() => {
    this.onResize();
  }, 100); // Wait for resize to settle
});
```

## Implementation Checklist

- [ ] Add screen buffer array for frame building
- [ ] Implement `buildFrame()` method that writes to buffer
- [ ] Implement `flushFrame(forceFullRedraw)` method
- [ ] Add eat terminal detection
- [ ] Debounce resize events (100ms delay)
- [ ] Lower animation FPS from 12 to 8
- [ ] Add `--no-animation` CLI flag for troubleshooting
- [ ] Test in eat, vterm, regular terminal (iTerm, Terminal.app, etc.)
- [ ] Consider falling back to blessed if problems persist

## Alternative: Create Separate Simple Mode

For Emacs use, could create a simplified TUI mode:

```bash
artery --simple   # No animation, basic rendering
artery --full     # Full TUI with animation (default)
```

Simple mode would:
- Skip the animated title completely
- Use basic line-by-line output  
- Work reliably in any terminal

## Future Consideration: Emacs Native UI

Long-term, could bypass the terminal entirely and use Emacs's native UI:
- `completing-read` for menu selection
- Emacs buffers for log display
- Custom major mode with keybindings

This would be the most "Emacs-native" solution but requires elisp development.

## Resources

- [blessed documentation](https://github.com/chjj/blessed)
- [terminal-kit documentation](https://github.com/cronvel/terminal-kit)  
- [eat manual](https://codeberg.org/akib/emacs-eat)
- [ANSI escape codes reference](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [Node.js TTY documentation](https://nodejs.org/api/tty.html)

---

**Author**: GitHub Copilot  
**Date**: December 3, 2025  
**Status**: ✅ Implemented (Phase 1)

## Implementation Notes (Dec 3, 2025)

### Changes Made:

1. **Debounced resize handling** (50ms) - prevents jank during font size changes
2. **Force full redraw on resize** - clears `lastRenderedBuffer` and sets `forceFullRedraw` flag
3. **eat terminal detection** - checks `TERM_PROGRAM=eat` and `INSIDE_EMACS`
4. **Slower animation in eat** - 150ms interval (~7fps) vs 80ms (~12fps) in normal terminals
5. **Full render in eat** - uses `render()` instead of `renderHeaderOnly()` to avoid partial update artifacts
6. **Skip animation during resize** - `renderHeaderOnly()` and animation loop check `resizeTimeout`
7. **CLI flags added**:
   - `--no-animation, -n` - disables blood flow animation
   - `--simple, -s` - simple mode with no animation
   - `--help, -h` - shows help

### Testing:

```bash
# Normal mode
node artery/artery-tui.mjs

# No animation (for troubleshooting)
node artery/artery-tui.mjs --no-animation

# Inside Emacs eat - should auto-detect and use slower animation
```
