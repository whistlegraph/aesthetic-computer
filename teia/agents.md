# Teia Package Management - Agent Instructions

This file contains instructions for AI agents working with Teia package creation and testing in the aesthetic.computer project.

## Overview

The Teia packaging system creates offline-ready packages of aesthetic.computer pieces for deployment to Teia (an NFT platform). The packages bundle all necessary assets and configure the system for offline operation.

## Key Scripts

### 1. Pack Script (`ac-pack.mjs`)

**Purpose**: Creates a complete offline package for a piece, bundling all assets and configuring TEIA mode.

**Location**: `/workspaces/aesthetic-computer/teia/ac-pack.mjs`

**Usage**:
```bash
cd /workspaces/aesthetic-computer/teia
node ac-pack.mjs '<piece-name>'
```

**Examples**:
```bash
# Pack a KidLisp piece (note the $ prefix)
node ac-pack.mjs '$roz'

# Pack a regular piece
node ac-pack.mjs 'drawing'

# Unpack latest package automatically
node ac-unpack.mjs

# Unpack specific package
node ac-unpack.mjs 'output/$roz-2025.09.22.04.24.38.942.zip'
```

**What it does**:
- Fetches the piece code (KidLisp or JavaScript)
- Bundles all system libraries and dependencies
- Includes font files (font_1 glyphs, webfonts)
- Sets TEIA mode flags for offline operation
- Creates a zip file ready for Teia deployment
- Output goes to: `/workspaces/aesthetic-computer/teia/output/`

**Key Features**:
- Automatically detects KidLisp vs JavaScript pieces
- Bundles dependencies and handles imports
- Configures offline font loading
- Sets TEIA mode to prevent API calls
- Creates timestamped zip files

### 2. Unpack Script (`ac-unpack.mjs`)

**Purpose**: Extracts and serves packaged Teia files for testing.

**Location**: `/workspaces/aesthetic-computer/teia/ac-unpack.mjs`

**Usage**:
```bash
cd /workspaces/aesthetic-computer/teia

# Unpack and serve the latest zip file (auto-detected)
node ac-unpack.mjs

# Unpack a specific zip file
node ac-unpack.mjs '<zip-file-path>'

# Unpack with custom port
node ac-unpack.mjs '<zip-file-path>' 8003
```

**Examples**:
```bash
# Unpack and serve the latest $roz package
node ac-unpack.mjs 'output/$roz-2025-09-22T04-16-44-018Z.zip'

# Unpack without auto-serving (just extract)
node ac-unpack.mjs 'output/$roz-2025-09-22T04-16-44-018Z.zip' --no-serve
```

**What it does**:
- Automatically finds the latest zip file if no path provided
- Extracts the zip file to `output/test-extract/`
- Analyzes package contents
- Starts a local HTTP server on port 8002 (or custom port)
- Opens browser for testing
- Monitors server logs for debugging

**Testing Checklist**:
1. Check browser console for errors
2. Verify no API calls to `/api/bdf-glyph` or `/api/store-kidlisp`
3. Test font rendering (corner labels, QR codes)
4. Verify offline functionality

## Common Workflows

### Creating and Testing a Package

1. **Pack the piece**:
   ```bash
   cd /workspaces/aesthetic-computer/teia
   node ac-pack.mjs '$roz'
   ```

2. **Test the latest package automatically**:
   ```bash
   # Auto-detects and serves the most recent zip file
   node ac-unpack.mjs
   ```

3. **Monitor testing**:
   - Check browser console at http://localhost:8002
   - Look for TEIA mode debug messages
   - Verify no API call errors
   - Test font rendering

### Debugging Common Issues

#### Font Issues
- **Symptom**: Missing corner labels or broken QR codes
- **Check**: Look for font_1 and MatrixChunky8 in the package
- **Debug**: Check console for font loading messages

#### API Call Issues  
- **Symptom**: Infinite loops, 404s to /api/* endpoints
- **Check**: Look for TEIA mode detection messages in console
- **Debug**: Verify `teia-mode.mjs` is loaded and TEIA mode is true

#### Circular Dependencies
- **Symptom**: "Cannot access before initialization" errors
- **Solution**: Use the shared `teia-mode.mjs` module for TEIA detection

## File Structure

```
teia/
├── ac-pack.mjs          # Main packing script
├── ac-unpack.mjs        # Testing/unpacking script  
├── output/              # Generated packages
│   ├── $roz-*.zip       # Timestamped zip files
│   └── test-extract/    # Unpacked test files
└── agents.md           # This file
```

## Key Technical Details

### Timestamping System
- Uses `num.timestamp()` function for consistent timestamps across the platform
- Format: `YYYY.MM.DD.HH.MM.SS.mmm` (e.g., `2025.09.22.04.24.38.942`)
- Same timestamp format used for GIF generation and other AC features
- Provides precise ordering and avoids filename conflicts

### Auto-Detection Features
- `ac-unpack.mjs` automatically finds the most recent zip file when no argument provided
- Sorts zip files by modification time to find the latest package
- Eliminates need to copy/paste long timestamp filenames for testing

### TEIA Mode Detection
- Uses shared module: `system/public/aesthetic.computer/lib/teia-mode.mjs`
- Prevents API calls in offline packages
- Enables local font loading
- Set during package initialization

### Font System
- `font_1`: Bitmap font for UI elements (bundled as individual glyph files)
- `MatrixChunky8`: BDF font for QR codes (requires special handling)
- Webfonts: Typography fonts (bundled as woff/woff2 files)

### Bundle Structure
Packages include:
- `index.html` - Entry point
- `aesthetic.computer/` - Core system files
- `assets/` - Font and media files  
- `kidlisp-cache.js` - Cached KidLisp code
- `type/webfonts/` - Typography assets

## Troubleshooting

### Common Error Messages

1. **"Uncaught ReferenceError: TEIA_MODE is not defined"**
   - Missing import of `getTeiaMode()` from `teia-mode.mjs`
   - Update code to use the shared TEIA mode module

2. **"Cannot access 'TextInput' before initialization"**
   - Circular dependency issue
   - Use shared modules instead of cross-imports

3. **"📱 TEIA mode: skipping API call"**
   - Good! This means TEIA mode detection is working

4. **Font rendering issues**
   - Check if font files are bundled in the package
   - Verify TEIA mode detection for offline font loading

### Useful Console Messages

- `🔤 Font glyph lookup` - Font loading debug info
- `📱 TEIA mode: skipping` - API call prevention
- `🔍 TEIA check in` - TEIA mode detection status

## Best Practices

1. **Use auto-detection for testing** - Just run `node ac-unpack.mjs` without arguments
2. **Check console logs** for errors and API calls
3. **Verify font rendering** in corner labels and QR codes
4. **Timestamp consistency** - All packages use the same `num.timestamp()` format
5. **Monitor server logs** during testing for unexpected requests

## Agent Notes

When working with Teia packages:
- Run `node ac-unpack.mjs` without arguments to test the latest package automatically
- The pack script uses `num.timestamp()` for consistent timestamp formatting
- Timestamp format matches GIF generation and other AC platform features
- Auto-detection saves time by eliminating manual zip file selection
- All commands should be run from the `/workspaces/aesthetic-computer/teia/` directory