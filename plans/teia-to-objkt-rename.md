# OBJKT → OBJKT Rename Plan

**Date**: October 8, 2025  
**Status**: In Progress  
**Goal**: Rename all OBJKT references to OBJKT throughout the codebase

## Scope

This rename reflects that the packaging system is for OBJKT deployment on teia.art platform.

### What to Change
- ✅ Directory: `teia/` → `objkt/`
- ✅ Variables: `acOBJKT_MODE` → `acOBJKT_MODE`
- ✅ Variables: `objktKidlispCodes` → `objktKidlispCodes`
- ✅ Variables: `acOBJKT_VIEWER` → `acOBJKT_VIEWER`
- ✅ Variables: `acOBJKT_CREATOR` → `acOBJKT_CREATOR`
- ✅ Variables: `acOBJKT_COLOPHON` → `acOBJKT_COLOPHON`
- ✅ Variables: `acOBJKT_MATRIX_CHUNKY_GLYPHS` → `acOBJKT_MATRIX_CHUNKY_GLYPHS`
- ✅ Comments: "OBJKT mode" → "OBJKT mode"
- ✅ Comments: "OBJKT mode" → "OBJKT mode"
- ✅ Comments: "objkt mode" → "objkt mode"
- ✅ File references: imports, paths to objkt/ directory
- ✅ Function names: `patchForObjkt` → `patchForObjkt`
- ✅ Module name: `objkt-mode.mjs` → `objkt-mode.mjs`

### What to Preserve
- ✅ "teia.art" - actual platform URL
- ✅ "Teia Interactive OBJKTs" - when referring to the platform's interactive NFTs
- ✅ Historical context mentions of Teia platform
- ✅ "for Teia" when referring to deploying TO the platform

## Files to Update

### 1. Directory Rename
- [ ] `teia/` → `objkt/`
- [ ] `.gitignore` references to teia/

### 2. Core Scripts (objkt/ directory)
- [ ] `objkt/ac-pack.mjs` - main packer
- [ ] `objkt/ac-unpack.mjs` - unpacker
- [ ] `objkt/ac-ship.mjs` - shipper
- [ ] `objkt/kidlisp-extractor.mjs` - KidLisp cache generator
- [ ] `objkt/dependency-analyzer.mjs` - dependency analyzer
- [ ] `objkt/agents.md` - documentation
- [ ] `objkt/AC-PACK-TAPE-GUIDE.md` - tape guide

### 3. System Libraries
- [ ] `system/public/aesthetic.computer/lib/objkt-mode.mjs` → `objkt-mode.mjs`
- [ ] `system/public/aesthetic.computer/lib/kidlisp.mjs` - imports objkt-mode
- [ ] `system/public/aesthetic.computer/lib/type.mjs` - OBJKT mode checks
- [ ] `system/public/aesthetic.computer/bios.mjs` - acOBJKT_MODE usage

### 4. Documentation & Plans
- [ ] `plans/pack-$39i.md`
- [ ] `plans/bdf-matrix-glyph-rendering-teia-export-bug.md` → `bdf-matrix-glyph-rendering-objkt-export-bug.md`
- [ ] `TODO.txt`
- [ ] `.gitignore`

### 5. Reference Files
- [ ] `reference/tools/recording/gif-renderer.mjs`
- [ ] `reference/tools/recording/orchestrator.mjs`

### 6. Token Scripts
- [ ] `tokens/pack-for-objkt.fish` → `pack-for-objkt.fish` (check .gitignore)

## Implementation Order

1. **Create this plan document** ✅
2. **Rename directory**: `teia/` → `objkt/`
3. **Rename module**: `lib/objkt-mode.mjs` → `lib/objkt-mode.mjs`
4. **Update all file imports** to use new paths
5. **Replace all variable names** (acOBJKT_MODE, etc.)
6. **Update all comments and strings**
7. **Update documentation**
8. **Test**: Build a pack to ensure everything works
9. **Commit**: Single atomic commit with all changes

## Search Patterns

### Variables to Replace
- `acOBJKT_MODE` → `acOBJKT_MODE`
- `acOBJKT_VIEWER` → `acOBJKT_VIEWER`
- `acOBJKT_CREATOR` → `acOBJKT_CREATOR`
- `acOBJKT_COLOPHON` → `acOBJKT_COLOPHON`
- `acOBJKT_MATRIX_CHUNKY_GLYPHS` → `acOBJKT_MATRIX_CHUNKY_GLYPHS`
- `objktKidlispCodes` → `objktKidlispCodes`
- `objktContext` → `objktContext`
- `isObjktMode` → `isObjktMode`

### Comments/Strings to Replace
- "OBJKT mode" → "OBJKT mode"
- "OBJKT mode" → "OBJKT mode"
- "objkt mode" → "objkt mode"
- "TEIA" (standalone) → "OBJKT"
- "Teia" (when not referring to platform) → "OBJKT"
- "patchForObjkt" → "patchForObjkt"
- "objkt-mode.mjs" → "objkt-mode.mjs"

### Paths to Update
- `teia/` → `objkt/`
- `/teia/` → `/objkt/`
- `'../../../teia/` → `'../../../objkt/`

### Exceptions (DO NOT CHANGE)
- `teia.art` - platform URL
- `"for Teia"` or `"to Teia"` - deploying to the platform
- `"Teia Interactive OBJKTs"` - platform feature name
- `"OBJKT package"` or `"OBJKT packages"` - can be "OBJKT package"

## Verification

After changes:
- [ ] No remaining `acTEIA_` variables (except in git history)
- [ ] No remaining `teiaKidlisp` variables
- [ ] Directory is `objkt/` not `teia/`
- [ ] Module is `objkt-mode.mjs` not `objkt-mode.mjs`
- [ ] Build succeeds: `node objkt/ac-pack.mjs '$4bb' --tape ...`
- [ ] Cursor hiding still works
- [ ] Console shows "OBJKT mode" messages

## Notes

- This is a large refactor touching 100+ locations
- Single atomic commit to keep git history clean
- Platform references to "teia.art" remain unchanged
- Focus is on renaming the internal mode/system, not the deployment target
