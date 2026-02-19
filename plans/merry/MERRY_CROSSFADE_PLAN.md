# Merry Crossfade Implementation Plan

**Date:** 2026-02-18

## Overview
Add a `:fade` option to merry/merryo/mo that crossfades between KidLisp `$code` pieces
using the existing `paintApi.kidlisp()` infrastructure. Instead of `jump()`ing between
pieces (which destroys the old piece), a host piece stays loaded and renders both the
outgoing and incoming `$code` via `kidlisp()` calls with alpha blending.

## Syntax
```
mo1:fade $xom $bop         # 1s each, default 300ms crossfade
mo1:fade.5 $xom $bop       # 1s each, 500ms crossfade
merryo:fade 3-$xom 5-$bop  # custom durations, default crossfade
```

## Files to modify

### 1. `system/public/aesthetic.computer/disks/prompt.mjs`
- **Parse `:fade` from colon params** in the merry command section (~line 1486).
  Before building `processedParams`, check if any colon segment matches `/^fade(\.\d+)?$/`.
  Extract fade duration (default 0.3s). Remove it from the params list.
- **Pass `fade` option to `activateMerry()`**: Add `fadeDuration` to the options.
- **In `activateMerry()`**: Store `fadeDuration` on `system.merry`.
- **In `startMerryPiece()`** (~line 1298): When `system.merry.fadeDuration` is set
  and pieces are `$code`, do NOT call `jump(piece)`. Instead, update
  `system.merry.fadeState` with `{ outIndex, inIndex, startTime, duration }`.
  The host piece reads this state each frame.
- **Initial jump**: On first `activateMerry()` call with fade mode, `jump("merry-fade")`
  once to load the host piece. Never jump again — the host stays loaded.

### 2. `system/public/aesthetic.computer/disks/merry-fade.mjs` (NEW — small host piece)
A thin piece (~60 lines) that:
- `boot()`: reads `system.merry` to get the pipeline
- `paint()`: each frame:
  - Gets current time from `system.merry` state
  - Determines which piece is "current" and whether a crossfade is active
  - Calls `kidlisp(0, 0, w, h, "$code", { noPaste: true })` for each active piece
  - If no crossfade: pastes the current piece buffer directly
  - If crossfade active: pastes incoming piece first, then alpha-blends outgoing on top
    using manual pixel loop (outgoing alpha decreases from 255→0)
  - Calls `needsPaint()` to keep rendering
- `export const nohud = true;`

### 3. `system/public/aesthetic.computer/lib/disk.mjs`
- **Expose `pasteWithAlpha` on `$paintApi`** (~line 4838 in the `$paintApi` object):
  Add a `pasteWithAlpha(source, x, y, alpha)` function that does per-pixel alpha
  blending from a source buffer onto the current screen. This is a straightforward
  pixel loop (same math as kidlisp.mjs:13261 `fallbackPasteWithAlpha`).

### 4. `system/public/aesthetic.computer/disks/mo.mjs`
- Pass colon params through so `:fade` reaches prompt.mjs. Currently `mo.mjs` builds
  args from colon + params but doesn't distinguish options from piece names. Need to
  detect and forward `:fade` as part of the jump target.

## Implementation order
1. Add `pasteWithAlpha` to `$paintApi` in disk.mjs
2. Create `merry-fade.mjs` host piece
3. Modify prompt.mjs to parse `:fade`, pass it through, and jump to host
4. Modify mo.mjs to forward `:fade` colon param
5. Test with `mo1:fade $xom $bop`

## What this does NOT change
- Non-fade merry works exactly as before (jump-based transitions)
- Non-$code pieces (regular .mjs pieces) still use jump-based transitions
- The progress bar overlay still renders normally
- UTC sync still works (the host piece reads system.merry timing)

## Technical study
See `/reports/merry-crossfade-kidlisp-compositor-study.md` for the full investigation
of the KidLisp embed/compositor system and why this approach was chosen.
