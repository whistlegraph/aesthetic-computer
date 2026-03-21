# KidLisp Bundle System

## Current Status (November 2025)

**Bundle Script**: `bundle-keep-html.mjs`  
**Technology**: SWC minification + gzip compression  
**Current Size**: ~450 KB  
**Test Pieces**: `$bop`, `$roz`, `$wwi` - KidLisp pieces

## Quick Start

```bash
# Using the fish shell command (recommended):
ac-keep bop          # Create bundle for $bop
ac-keep-test bop     # Create bundle AND start test server

# Or run the script directly:
node tezos/bundle-keep-html.mjs bop
```

## Fish Shell Commands

### `ac-keep <piece>`
Creates a self-contained HTML bundle for a KidLisp piece.

```bash
ac-keep bop      # Creates bundle in tezos/keep-bundles/
ac-keep '$bop'   # $ prefix is optional
```

**Output:**
- `$piece-@author-timestamp.lisp.html` - Gzip compressed, drag-and-drop ready

Example filename:
- `$bop-@jeffrey-2025.11.26.2.19.20.646.lisp.html`

### `ac-keep-test <piece>`
Creates the bundle AND starts a local test server.

```bash
ac-keep-test bop   # Build + serve at http://localhost:8082/
                   # Browse to the .lisp.html file
```

## Drag-and-Drop Support

`.lisp.html` bundles can be dragged onto aesthetic.computer to run them:
- Bundle decompresses and extracts embedded KidLisp source
- Runs the piece with the embedded source code
- Works in any browser that supports gzip decompression

## Console Output (Colophon)

When a bundle runs, it displays rich metadata in the console:

```
Aesthetic.Computer           (rainbow colored title)
$bop is a piece by @jeffrey
Its KidLisp source:
purple, ink, line, blur 5
This copy was packed on 11/25/2025
Using aesthetic-computer git version 847bdc27 (dirty)
View this piece at https://aesthetic.computer
Learn KidLisp at https://kidlisp.com
Contribute on GitHub at https://github.com/whistlegraph/aesthetic-computer
```

## System Architecture

### Pipeline Overview
1. **Dependency Resolution**: Starts from piece, traces all imports
2. **Virtual File System**: Embeds all dependencies into single HTML
3. **Import Rewriting**: Converts ES module imports to VFS lookups
4. **Minification**: SWC (TypeScript/JavaScript) with toplevel mangling
5. **Compression**: Gzip level 9 → Base64 encoding
6. **HTML Generation**: Self-extracting bundle with inline decompressor

### Key Technical Details

**Critical**: Import rewriting must happen BEFORE SWC minification
- SWC requires `module: true` configuration
- Imports rewritten to VFS paths before minification pass
- This differs from Terser which can handle post-rewrite

**Dependencies Included**: 68 files (all required for execution)
- All imported modules traced and bundled
- Missing any file causes black screen errors

## Size Breakdown

```
Uncompressed: ~1,140 KB (all ES modules bundled)
↓ SWC Minification
Minified: ~900 KB (toplevel mangling, dead code elimination)
↓ Gzip Level 9
Compressed: ~320 KB 
↓ Base64 Encoding
Final: ~450 KB (gzip self-extracting HTML)
```

## Configuration

### SWC Settings (in bundle-keep-html.mjs)
```javascript
const USE_SWC = true; // Use SWC for minification

// SWC configuration
jsc: {
  minify: {
    compress: { dead_code: true },
    mangle: { toplevel: true }
  }
}
```

## Known Issues

### Black Screen Errors
**Symptom**: Bundle loads but shows black screen, no execution  
**Cause**: Missing module dependencies (import resolution failures)  
**Solution**: Include all traced dependencies, don't skip files

### Module Specifier Errors
**Symptom**: Console errors like "Failed to resolve module specifier './lib/help.mjs'"  
**Cause**: Import rewriting happened after minification (wrong order for SWC)  
**Solution**: Rewrite imports BEFORE SWC minification pass

## Files

- **bundle-keep-html.mjs**: Main bundler script
- **BUNDLE-SYSTEM.md**: This documentation
- **archive/**: Deprecated scripts (bundle-minimal-keep.mjs, compression-research.mjs, etc.)

## Development Notes

**Testing workflow**:
```bash
# Build bundle
ac-keep bop

# Start local server  
ac-keep-test bop  # or: python3 -m http.server 8082

# Open the .lisp.html file in browser
```

**Success criteria**:
- ✅ No console errors
- ✅ Visual output renders
- ✅ KidLisp code executes
- ✅ Colophon displays in console
- ✅ File size reasonable (even if over target)
