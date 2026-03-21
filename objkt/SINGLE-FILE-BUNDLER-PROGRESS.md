# Single-File HTML Bundler Progress

**Date**: November 15, 2025  
**Goal**: Create completely self-contained HTML file for IPFS upload and objkt.com NFT minting

## What We Built

### ac-pack-single-file.mjs (510 lines)
A Node.js build script that generates a single 7.75 MB HTML file containing:
- ✅ 502 embedded files (461 from ac/, 41 from type/)
- ✅ 318 JavaScript modules as blob URLs
- ✅ All KidLisp code and dependencies
- ✅ Font drawings (113+ JSON files with URL-encoded names)
- ✅ Virtual FileSystem (VFS) with fetch/XHR interception
- ✅ Import map for ES module resolution
- ✅ No external network requests (except expected CSS fallbacks)

## Current Status

### ✅ Working Features
1. **XHR Interception** - All 113+ font drawings loading from VFS
2. **Fetch Interception** - painting.mjs and other modules served from VFS
3. **URL Decoding** - Handles %20, special characters (í, ø), etc.
4. **Path Normalization** - Strips aesthetic.computer/, adds ac/ prefix
5. **Blob URL Creation** - 318 JavaScript modules converted to blob URLs
6. **Import Rewriting** - Relative paths (../, ./) rewritten to absolute
7. **Module Path Array** - Pre-computed list of all .js/.mjs files
8. **Import Map Generation** - Dual-path mapping for legacy/normalized imports

### ❌ Known Issues (Need Fixes)
1. **sfx.mjs 404** - Import map missing `/aesthetic.computer/` prefix paths
2. **CSS files 404** - Font CSS files not included in VFS (ywft-processing-*.css, berkeley-mono-variable.css)
3. **Cursor SVG 404** - precise.svg not included in VFS (in cursors/ directory)
4. **painting.lisp 404** - Expected (not included in bundle, external resource)

### 🔧 Fixes Needed
1. **Import Map**: Add entries with leading slash: `/aesthetic.computer/disks/common/sfx.mjs`
2. **VFS**: Include `type/webfonts/*.css` files
3. **VFS**: Include `aesthetic.computer/cursors/*.svg` files
4. **Fetch Intercept**: Handle CSS file requests

### 🔧 Last Fix Applied
**Script Block Scope Bug** (lines 203-287):
- **Problem**: `const modulePaths` defined in first `<script>` block
- **Problem**: Second `<script>` block couldn't access it (block-scoped)
- **Solution**: Changed to `window.modulePaths` (global)
- **Status**: Fix applied to source, bundle rebuilt, server restarted

## Architecture

### Build Time (Node.js)
1. Scan `system/public/aesthetic.computer` and `system/public/type`
2. Inline all files as text/base64 in VFS JSON object
3. Generate array of all JavaScript module paths
4. Embed VFS + module paths in HTML template
5. Write single-file output (7.75 MB)

### Runtime (Browser)
**Script Block 1** (lines 30-45):
- Create `window.VFS` object with 502 files
- Create `window.VFS_BLOB_URLS` object (empty)
- Create `window.modulePaths` array (318 modules)

**Script Block 2** (lines 203-217):
- For each module in `window.modulePaths`:
  - Rewrite imports in module code
  - Create blob URL for rewritten code
  - Store in `window.VFS_BLOB_URLS`

**Script Block 3** (lines 266-287):
- For each module in `window.modulePaths`:
  - Add normalized path → blob URL to import map
  - Add legacy path → blob URL to import map (if ac/disks/)
- Create `<script type="importmap">` element
- Inject into DOM

**Script Block 4** (lines 155-214):
- Intercept `window.fetch()` to serve from VFS
- Handle path normalization and URL decoding

**Script Block 5** (lines 218-259):
- Intercept `XMLHttpRequest` to serve from VFS
- Handle font drawings with spaces in names

**Script Block 6** (lines 290-310):
- Load style.css from VFS
- Load boot.mjs using blob URL from import map

## File Locations

```
/workspaces/aesthetic-computer/objkt/
├── ac-pack-single-file.mjs   # Build script (510 lines)
└── output/
    └── eel-single-file.html   # Generated bundle (7.75 MB)
```

## Testing

### Server
```bash
cd /workspaces/aesthetic-computer/objkt/output
python3 -m http.server 8889
```

### Expected Console Output (After Fix)
```javascript
"Aesthetic.Computer"
"$eel is a piece by @jeffrey"
"🔧 Creating blob URLs for all modules..."
"✅ Created 318 blob URLs"
"✅ Import map activated with 636 entries"
"📝 Sample entries: ['ac/disks/common/sfx.mjs', 'aesthetic.computer/disks/common/sfx.mjs', ...]"
"🔍 VFS fetch: ../disks/painting.mjs → ac/disks/painting.mjs"
"✓ VFS served: ac/disks/painting.mjs (text)"
[... 113+ XHR logs for font drawings ...]
```

### Expected Network Requests
- ✅ 0 requests to aesthetic.computer
- ✅ 0 requests for .mjs files
- ✅ 0 requests for font drawings
- ✅ 0 requests for cursors (pending fix)
- ⚠️ 4 expected CSS 404s (fallback fonts, harmless)
- ⚠️ 1 painting.lisp 404 (expected, not in bundle)

## Next Steps

1. **Debug Why Scripts Not Executing**
   - Check browser console for ReferenceError
   - Verify `window.modulePaths` is accessible
   - Check if blob URL creation loop runs
   - Check if import map injection runs

2. **Fix Cursor SVG 404**
   - Investigate how cursors are loaded (CSS vs fetch)
   - May need CSS rewriting or additional intercept

3. **Validation Testing**
   - Test on objkt.com upload
   - Test on IPFS gateway
   - Verify zero network requests
   - Confirm all features work offline

4. **Upload to IPFS**
   - Use Pinata with wrapWithDirectory
   - Generate directory CID for index.html
   - Mint NFT with keep contract

## Technical Notes

### Import Map Must Be Static
- Import maps CANNOT be created dynamically after modules start loading
- Must be in DOM BEFORE any `import()` calls execute
- We generate at runtime but inject BEFORE boot.mjs loads

### Script Block Scope
- `const`/`let` declarations are block-scoped to their `<script>` tag
- Use `window.property` for cross-block communication
- Function declarations are hoisted within their block

### Blob URLs for Modules
- Native browser module loading bypasses fetch/XHR intercepts
- Must rewrite import paths BEFORE creating blob URL
- Import map maps bare specifiers to blob URLs

### Dual-Path Import Map
- Maps both `ac/disks/common/sfx.mjs` AND `aesthetic.computer/disks/common/sfx.mjs`
- Both paths point to SAME blob URL
- Supports legacy code using old aesthetic.computer/ prefix

## Lessons Learned

1. **ES Modules Cannot Be Intercepted** - Must use import maps + blob URLs
2. **Template Literal Escaping Is Hard** - String concatenation safer for code generation
3. **Function Hoisting Works Within Blocks** - Can call before definition in same `<script>`
4. **Import Map Polyfills Don't Work** - Native browser feature, no JavaScript workaround
5. **Script Block Scope Is Critical** - Global window properties essential for multi-block HTML

## Git Version
- **Hash**: 9ba4a4b9 (dirty)
- **Date**: November 15, 2025 at 7:07:46 AM
- **Build**: 502 files, 318 modules, 7.75 MB
