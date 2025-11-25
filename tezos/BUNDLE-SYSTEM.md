# Tezos KEEPS Bundle System

## Current Status (November 2025)

**Bundle System**: `bundle-keep-html.mjs`  
**Technology**: SWC minification + Brotli compression  
**Current Size**: 361 KB (working, ~105 KB over target)  
**Target Size**: 256 KB (Tezos storage limit)  
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
ac-keep bop      # Creates bundles in tezos/keep-bundles/
ac-keep '$bop'   # $ prefix is optional
```

**Output files (named like bios.mjs downloads):**
- `$piece-@author-timestamp.html` - Uncompressed, readable
- `$piece-@author-timestamp.brotli.html` - Brotli compressed for Tezos
- `$piece-@author-timestamp.gzip.html` - Gzip version for browser testing

Example filenames:
- `$bop-@jeffrey-2025.11.25.23.18.17.065.html`
- `$bop-@jeffrey-2025.11.25.23.18.17.065.brotli.html`
- `$bop-@jeffrey-2025.11.25.23.18.17.065.gzip.html`

### `ac-keep-test <piece>`
Creates the bundle AND starts a local test server.

```bash
ac-keep-test bop   # Build + serve at http://localhost:8082/
                   # Browse to the .gzip.html file
```

## Console Output (Colophon)

When a KEEP bundle runs, it displays rich metadata in the console:

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
5. **Compression**: Brotli level 11 → Base64 encoding
6. **HTML Generation**: Self-extracting bundle with inline decompressor

### Key Technical Details

**Critical**: Import rewriting must happen BEFORE SWC minification
- SWC requires `module: true` configuration
- Imports rewritten to VFS paths before minification pass
- This differs from Terser which can handle post-rewrite

**Dependencies Included**: 68 files (all required for execution)
- No SKIP_FILES filter (previous attempts broke functionality)
- All imported modules traced and bundled
- Missing any file causes black screen errors

## Size Breakdown

```
Uncompressed: 1,138 KB (all ES modules bundled)
↓ SWC Minification
Minified: ~900 KB (toplevel mangling, dead code elimination)
↓ Brotli Level 11
Compressed: 276 KB (69% reduction)
↓ Base64 Encoding
Final: 360 KB (+30% for Base64 overhead)
```

**Comparison with Gzip**:
- Brotli: 276 KB → 368 KB base64
- Gzip level 9: 334 KB → 446 KB base64  
- **Brotli wins by 58 KB** (14% better)

## Configuration

### SWC Settings (in bundle-keep-html.mjs)
```javascript
const USE_SWC = true; // Toggle between SWC and Terser

// SWC configuration
jsc: {
  minify: {
    compress: { dead_code: true },
    mangle: { toplevel: true }
  }
}
```

### File Skip List
```javascript
const SKIP_FILES = []; // Currently empty - all files needed
```

**Warning**: Skipping files to reduce size causes runtime errors. The system currently includes all dependencies to ensure functionality. Size optimization requires removing unused features from source code, not skipping bundled files.

## Optimization Roadmap

To reach 256 KB target (need to reduce by 104 KB):

### Potential Strategies
1. **Tree-shaking at source**: Remove unused system features before bundling
2. **Font optimization**: Only include glyphs actually used by piece
3. **Library minimization**: Replace large dependencies with minimal equivalents
4. **Code splitting**: Lazy-load non-critical features (though breaks single-file goal)

### Past Optimization Wins (from git history)
- November 24: Achieved 285 KB with Terser (actually 262 KB measured)
- SKIP_FILES approach: Broke functionality, not viable
- SWC integration: Same size as Terser but cleaner pipeline

## Historical Context

### Previous Iterations
- `bundle-minimal-keep.mjs`: Early version with gzip compression
- `bundle-ultra-minimal-keep.mjs`: Renamed to `bundle-keep-html.mjs`
- Multiple compression experiments: cow, ceo, roz variations
- All consolidated into single script approach

### Key Commits
- `835b2758`: "feat(tezos): optimize KidLisp NFT bundle to 285 KB"
  - Actually used Terser (commit message claimed SWC incorrectly)
  - Measured at 262 KB, working version
- `9a745ea7`: Current HEAD with wallet vault README
- Latest: SWC integration with proper import rewriting

## Known Issues

### Black Screen Errors
**Symptom**: Bundle loads but shows black screen, no execution  
**Cause**: Missing module dependencies (import resolution failures)  
**Solution**: Include all traced dependencies, don't skip files

### Module Specifier Errors
**Symptom**: Console errors like "Failed to resolve module specifier './lib/help.mjs'"  
**Cause**: Import rewriting happened after minification (wrong order for SWC)  
**Solution**: Rewrite imports BEFORE SWC minification pass

### Size vs Functionality Trade-off
**Current state**: Chose working 360 KB over broken 256 KB  
**Next step**: Optimize source code, not bundling process  
**Philosophy**: Better to have working bundle over target than broken bundle under target

## Files

- **bundle-keep-html.mjs**: Main bundler script (current)
- **BUNDLE-SYSTEM.md**: This file (current documentation)
- **archive/bundle-docs/**: Old documentation files
  - BUNDLE-OPTIMIZATION-RESEARCH.md
  - KEEP-BUNDLE-PROGRESS.md
  - OPTIMIZATION-RESULTS.md

## Development Notes

**When modifying**:
1. Test with simple piece first (e.g., `wwi`)
2. Verify bundle loads in browser (check console for errors)
3. Confirm visual output matches expected behavior
4. Check compressed size with `ls -lh`

**Testing workflow**:
```bash
# Build bundle
node bundle-keep-html.mjs wwi

# Start local server
python3 -m http.server 8082

# Open in browser
# Check: http://localhost:8082/wwi-ultra-gzip.html
```

**Success criteria**:
- ✅ No console errors
- ✅ Visual output renders
- ✅ KidLisp code executes
- ✅ File size reasonable (even if over target)
