# KEEP Bundle Progress

## ✅ Working Solution

We now have a fully functional single-script pipeline for creating self-contained KidLisp bundles for Tezos minting.

### Active Scripts

- **`bundle-minimal-keep.mjs`** - Complete bundler: creates minimal HTML with VFS, resolves dependencies, AND compresses to final output

### Removed Scripts (obsolete)

- ~~`compress-ceo.mjs`~~ - Now integrated into bundle-minimal-keep.mjs
- ~~`compress-cow.mjs`~~ - Now integrated into bundle-minimal-keep.mjs
- ~~`compress-roz.mjs`~~ - Now integrated into bundle-minimal-keep.mjs
- ~~`bundle-kidlisp-keep.mjs`~~ - Superseded by bundle-minimal-keep.mjs
- ~~`bundle-piece.mjs`~~ - Old approach without proper VFS
- ~~`create-self-contained.mjs`~~ - Hard-coded for roz, replaced by compress-*.mjs
- ~~`analyze-bundle.mjs`~~ - Debug tool, no longer needed
- ~~`analyze-dependencies.mjs`~~ - Debug tool, no longer needed

### Usage

```bash
# Create a complete bundle for any KidLisp piece (one command!)
node bundle-minimal-keep.mjs <piece-name>

# Examples:
node bundle-minimal-keep.mjs cow
node bundle-minimal-keep.mjs ceo
node bundle-minimal-keep.mjs roz

# Output:
# - keep-bundles/<piece>-minimal-nft.html (uncompressed ~1MB)
# - keep-bundles/<piece>-self-contained.html (compressed ~380-390KB)
```

### Features Implemented

#### 1. ✅ Recursive KidLisp Dependency Resolution
- Automatically fetches referenced `$` pieces
- Walks the dependency tree (e.g., `$cow` → `$39i`, `$r2f`)
- Fetches from `https://aesthetic.computer/api/store-kidlisp?code=<name>`

#### 2. ✅ Multi-Level KidLisp Caching
- **Level 1**: Prefills `globalCodeCache` via `window.acPREFILL_CODE_CACHE`
- **Level 2**: Embeds in `window.objktKidlispCodes` for offline mode
- **Level 3**: Creates `.lisp` VFS files as fallback
- **NO** `.mjs` files created (prevents blocking KidLisp loading)

#### 3. ✅ VFS Path Normalization
- Handles relative paths: `../disks/piece.mjs` → `disks/piece.mjs`
- Strips prefixes: `./`, `/`, `aesthetic.computer/`
- Works with both fetch and XHR interceptors

#### 4. ✅ CSS Link Blocking
- Prevents webfont 404 errors
- Intercepts `appendChild` and `append` calls
- Blocks `.css` file requests

#### 5. ✅ Early objktKidlispCodes Check
- Modified `lib/disk.mjs` to check embedded codes BEFORE file fetch
- Eliminates "VFS miss" and 404 errors for bundled pieces
- Loads directly from cache without network requests

### Bundle Stats

#### Current Size
- **Uncompressed**: ~995 KB (87 files in VFS)
- **Compressed**: ~381 KB (gzip level 9 + base64)
- **Target**: 256 KB
- **Overage**: ~131 KB (need to reduce)

#### Files Included (87 total)
- **Core**: boot.mjs, bios.mjs
- **Libs**: 57 files from lib/ (2d, 3d, kidlisp, graph, etc.)
- **Systems**: nopaint.mjs, prompt-system.mjs, world.mjs
- **Common**: 7 files from disks/common/
- **Dependencies**: 17 files from dep/ (gl-matrix, nanoid, etc.)
- **KidLisp pieces**: 3 .lisp files (cow, 39i, r2f)

### Known Issues

#### Size Constraint
- Bundle is 131 KB over 256 KB limit
- Need to strip more dependencies to fit Tezos constraints
- Candidates for removal:
  - 3D libraries (lib/3d.mjs, dep/three/*)
  - Unused systems
  - Additional lib/ modules not needed for simple KidLisp

#### Incomplete KidLisp Source
- `$39i` source is missing closing parenthesis in original
- This is intentional/acceptable in the source piece
- Validation error is expected

### Technical Details

#### VFS Structure
```javascript
window.VFS = {
  'boot.mjs': { content: '...', binary: false, type: 'mjs' },
  'lib/kidlisp.mjs': { content: '...', binary: false, type: 'js' },
  'disks/cow.lisp': { content: '($39i 0 0 w h 128)...', binary: false, type: 'lisp' },
  // ... 87 total files
}
```

#### Cache Prefill
```javascript
window.acPREFILL_CODE_CACHE = {
  'cow': '($39i 0 0 w h 128)\n($r2f 0 0 w h 128)\n(contrast 1.5)',
  '39i': 'black\n(0.1s (ink (? black white) 32)...',
  'r2f': 'salmon\nink fade:palegreen-purple...'
}
```

#### Import Map
All modules mapped to blob URLs:
```javascript
imports: {
  'boot.mjs': 'blob:...',
  'lib/kidlisp.mjs': 'blob:...',
  // ... all 84 JS modules
}
```

### Next Steps

1. **Reduce bundle size** to fit 256 KB limit
   - Remove 3D support (lib/3d.mjs, dep/three/*)
   - Remove unused lib/ modules
   - Minimize dep/ files

2. **Create compression scripts** for other pieces
   - Template: `compress-<piece>.mjs`
   - Automate from piece name

3. **Test on Tezos**
   - Verify < 256 KB bundles work
   - Test minting process
   - Confirm offline functionality

### Changelog

**2025-11-22**: Unified single-script pipeline ✅
- **MAJOR**: Combined bundling + compression into single script
- Removed compress-ceo.mjs, compress-cow.mjs, compress-roz.mjs
- Now just: `node bundle-minimal-keep.mjs <piece>` for complete output
- Outputs both uncompressed and compressed versions automatically
- Successfully tested with $roz, $cow, $ceo

**2025-11-22**: Successfully tested $roz bundle
- Built $roz bundle: 387 KB (137 KB over limit)
- Confirmed recursive dependency system works (roz has no $ refs)
- Created compress-roz.mjs (later removed)

**2025-11-22**: Initial working version
- Implemented recursive KidLisp dependency resolution
- Added multi-level caching (prefill + objktKidlispCodes + VFS)
- Fixed VFS path normalization for relative imports
- Modified disk.mjs to check objktKidlispCodes before file fetch
- Removed synthetic .mjs files (kept only .lisp)
- Successfully tested with $cow (including $39i and $r2f dependencies)
