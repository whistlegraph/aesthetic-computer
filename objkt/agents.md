# Teia Package Management - Agent Instructions

This file contains instructions for AI agents working with OBJKT package creation and testing in the aesthetic.computer project.

## Overview

The Teia packaging system creates offline-ready packages of aesthetic.computer pieces for deployment to Teia (an NFT platform). The packages bundle all necessary assets and configure the system for offline operation.

## Key Scripts

### 1. Pack Script (`ac-pack.mjs`)

**Purpose**: Creates a complete offline package for a piece, bundling all assets and configuring PACK mode.

**Location**: `/workspaces/aesthetic-computer/objkt/ac-pack.mjs`

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
- Sets PACK mode flags for offline operation
- Creates a zip file ready for OBJKT deployment
- Output goes to: `/workspaces/aesthetic-computer/teia/output/`

**Key Features**:
- Automatically detects KidLisp vs JavaScript pieces
- Bundles dependencies and handles imports
- Configures offline font loading
- Sets PACK mode to prevent API calls
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

### 3. Ship Script (`ac-ship.mjs`)

**Purpose**: Converts ac-pack zip files into native Electron desktop applications for Mac, Windows, and Linux.

**Location**: `/workspaces/aesthetic-computer/teia/ac-ship.mjs`

**Basic Usage**:
```bash
# Ship the latest zip file to all platforms
node ac-ship.mjs

# Ship a specific zip file
node ac-ship.mjs 'output/@jeffrey-$bop-2025.09.22.04.24.38.942.zip'

# Ship to specific platforms only
node ac-ship.mjs --platforms mac,windows
node ac-ship.mjs 'my-piece.zip' --platforms linux
```

**Examples**:
```bash
# Ship the latest $roz package to all platforms
node ac-ship.mjs

# Ship only macOS and Windows apps
node ac-ship.mjs 'output/$roz-2025-09-22T04-16-44-018Z.zip' --platforms mac,windows

# Ship to Linux only for testing
node ac-ship.mjs --platforms linux
```

**What it does**:
- Automatically finds the latest zip file if no path provided
- Extracts the zip file to a temporary directory
- Creates an Electron project structure around the extracted contents
- Installs Electron dependencies (electron, electron-builder)
- Builds native apps for the specified platforms:
  - **Mac**: `.dmg` installer (supports Intel x64 and Apple Silicon arm64)
  - **Windows**: `.exe` NSIS installer (x64)
  - **Linux**: `.AppImage` portable app (x64)
- Cleans up temporary files
- Outputs apps to `output/{piece-name}-electron/dist/`

**Output Structure**:
```
output/
├── piece-name-electron/
│   ├── dist/                    # Built applications
│   │   ├── piece-name-1.0.0.dmg       # macOS app
│   │   ├── piece-name Setup 1.0.0.exe  # Windows installer  
│   │   └── piece-name-1.0.0.AppImage   # Linux portable app
│   ├── package.json             # Electron project config
│   ├── main.js                  # Electron main process
│   └── app/                     # Extracted ac-pack contents
│       ├── index.html
│       └── aesthetic.computer/
└── piece-name-temp/             # (cleaned up automatically)
```

**Platform Requirements**:
- All platforms can be built from Linux (current dev container)
- Apps are typically 20-50MB each
- No code signing included (apps may show security warnings)

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
   - Look for PACK mode debug messages
   - Verify no API call errors
   - Test font rendering

### Creating Desktop Applications

1. **Ship Electron apps from existing package**:
   ```bash
   # Ship the latest zip file to all platforms
   node ac-ship.mjs
   
   # Or ship specific platforms
   node ac-ship.mjs --platforms mac,windows
   ```

2. **Complete workflow from piece to desktop apps**:
   ```bash
   # Pack, test, and ship in sequence
   node ac-pack.mjs '$bop'
   node ac-unpack.mjs  # Test in browser
   node ac-ship.mjs    # Create desktop apps
   ```

3. **Distribute the apps**:
   - Find built apps in `output/{piece-name}-electron/dist/`
   - Upload to GitHub releases or your preferred distribution method
   - Consider code signing for production distribution

### Debugging Common Issues

#### Font Issues
- **Symptom**: Missing corner labels or broken QR codes
- **Check**: Look for font_1 and MatrixChunky8 in the package
- **Debug**: Check console for font loading messages

#### API Call Issues  
- **Symptom**: Infinite loops, 404s to /api/* endpoints
- **Check**: Look for PACK mode detection messages in console
- **Debug**: Verify `objkt-mode.mjs` is loaded and PACK mode is true

#### Circular Dependencies
- **Symptom**: "Cannot access before initialization" errors
- **Solution**: Use the shared `objkt-mode.mjs` module for OBJKT detection

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

### OBJKT Mode Detection
- Uses shared module: `system/public/aesthetic.computer/lib/objkt-mode.mjs`
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
   - Missing import of `getPackMode()` from `objkt-mode.mjs`
   - Update code to use the shared PACK mode module

2. **"Cannot access 'TextInput' before initialization"**
   - Circular dependency issue
   - Use shared modules instead of cross-imports

3. **"📱 PACK mode: skipping API call"**
   - Good! This means PACK mode detection is working

4. **Font rendering issues**
   - Check if font files are bundled in the package
   - Verify PACK mode detection for offline font loading

### Useful Console Messages

- `🔤 Font glyph lookup` - Font loading debug info
- `📱 PACK mode: skipping` - API call prevention
- `🔍 OBJKT check in` - PACK mode detection status

## Best Practices

1. **Use auto-detection for testing** - Just run `node ac-unpack.mjs` without arguments
2. **Check console logs** for errors and API calls
3. **Verify font rendering** in corner labels and QR codes
4. **Timestamp consistency** - All packages use the same `num.timestamp()` format
5. **Monitor server logs** during testing for unexpected requests

## Agent Notes

When working with OBJKT packages:
- Run `node ac-unpack.mjs` without arguments to test the latest package automatically
- The pack script uses `num.timestamp()` for consistent timestamp formatting
- Timestamp format matches GIF generation and other AC platform features
- Auto-detection saves time by eliminating manual zip file selection
- All commands should be run from the `/workspaces/aesthetic-computer/teia/` directory