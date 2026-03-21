# OBJKT Mode → PACK Mode Rename

**Date**: October 8, 2025  
**Status**: In Progress

## Objective

Rename OBJKT mode to PACK mode to make the packaging system platform-agnostic. This removes platform-specific terminology (TEIA, OBJKT) and focuses on the "pack" concept.

## Changes Required

### 1. Directory Structure
- [ ] Keep `objkt/` directory name (for compatibility with existing workflows)
- [ ] OR rename `objkt/` → `pack/` (more semantic but breaks existing paths)
- **Decision**: Keep `objkt/` for now, only change mode names

### 2. Variable Renames
- [ ] `acOBJKT_MODE` → `acPACK_MODE`
- [ ] `window.acOBJKT_MODE` → `window.acPACK_MODE`
- [ ] `globalThis.acOBJKT_MODE` → `globalThis.acPACK_MODE`
- [ ] `window.acOBJKT_VIEWER` → `window.acPACK_VIEWER`
- [ ] `window.acOBJKT_CREATOR` → `window.acPACK_CREATOR`
- [ ] `window.acOBJKT_COLOPHON` → `window.acPACK_COLOPHON`
- [ ] `globalThis.acOBJKT_COLOPHON` → `globalThis.acPACK_COLOPHON`
- [ ] `window.acOBJKT_MATRIX_CHUNKY_GLYPHS` → `window.acPACK_MATRIX_CHUNKY_GLYPHS`

### 3. Module Renames
- [ ] `lib/objkt-mode.mjs` → `lib/pack-mode.mjs`
- [ ] `getObjktMode()` function → `getPackMode()`
- [ ] Update all imports of `objkt-mode.mjs` → `pack-mode.mjs`

### 4. String Replacements
- [ ] "OBJKT mode" → "PACK mode"
- [ ] "objkt mode" → "pack mode"
- [ ] "Objkt mode" → "Pack mode"
- [ ] "OBJKT" → "PACK" (in mode context, not platform URLs)
- [ ] "packed for TEIA" → "packed on"
- [ ] "packed for OBJKT" → "packed on"

### 5. File Renames
- [ ] `tokens/pack-for-teia.fish` → `tokens/pack.fish` or `tokens/pack-piece.fish`
- [ ] Update `.gitignore` references if needed

### 6. Comments and Documentation
- [ ] Update comments referencing "OBJKT mode" → "PACK mode"
- [ ] Update log messages to be platform-agnostic
- [ ] Update documentation in README files

### 7. Log Message Updates
Pattern: "packed for [PLATFORM]" → "packed on [DATE]"
Example: 
- Before: "This copy was packed for OBJKT on October 8th, 2025"
- After: "This copy was packed on October 8th, 2025"

## Files to Update

### Core System Files
- `system/public/aesthetic.computer/bios.mjs`
- `system/public/aesthetic.computer/lib/objkt-mode.mjs` (rename to pack-mode.mjs)
- `system/public/aesthetic.computer/lib/disk.mjs`
- `system/public/aesthetic.computer/lib/type.mjs`
- `system/public/aesthetic.computer/lib/kidlisp.mjs`
- `system/public/aesthetic.computer/lib/headers.mjs`

### Objkt Directory Files
- `objkt/ac-pack.mjs`
- `objkt/ac-unpack.mjs`
- `objkt/ac-ship.mjs`
- `objkt/kidlisp-extractor.mjs`
- `objkt/dependency-analyzer.mjs`
- `objkt/agents.md`
- `objkt/AC-PACK-TAPE-GUIDE.md`

### Token/Script Files
- `tokens/pack-for-teia.fish` → `tokens/pack.fish`

### Documentation Files
- `plans/*.md` (update references)
- `.gitignore` (update if needed)

## Testing Checklist

After changes:
- [ ] Pack a test piece and verify no errors
- [ ] Check that pack mode is detected correctly
- [ ] Verify cursor hiding works
- [ ] Check console logs show "PACK mode" not "OBJKT mode"
- [ ] Verify colophon shows correct wording
- [ ] Test unpacking a generated package

## Notes

- Platform-specific URLs (teia.art, objkt.com) should remain unchanged
- Focus is on making the **mode name** platform-agnostic
- The directory name `objkt/` can stay for compatibility
