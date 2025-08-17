# 🎨 Aesthetic Computer → Teia Packaging

This directory contains packaged aesthetic.computer pieces ready for upload to [Teia](https://teia.art) as Interactive OBJKTs.

## Quick Start

```fish
# From root directory
./pack-for-teia.fish starfield

# Or from tokens directory  
cd tokens && ./pack-for-teia.fish starfield
```

## About

The `ac-pack` utility creates static, self-contained packages of aesthetic.computer pieces that can run on Teia's platform. Each package includes all necessary dependencies and is optimized for the sandboxed iframe environment that Teia provides.

## Usage

### Prerequisites

1. Install dependencies:
   ```bash
   cd utilities
   npm install
   ```

### Basic Usage

```bash
# Pack a piece (creates tokens/piece-name/ and tokens/piece-name.zip)
node utilities/ac-pack.mjs <piece-name>

# Examples
node utilities/ac-pack.mjs paint
node utilities/ac-pack.mjs brush
node utilities/ac-pack.mjs sparkle
```

### Advanced Options

```bash
# Custom title and description
node utilities/ac-pack.mjs paint \
  --title "AI Paint Studio" \
  --description "Interactive AI-powered painting tool" \
  --author "Your Name"

# Custom cover image
node utilities/ac-pack.mjs brush \
  --cover ./path/to/cover.png \
  --title "Custom Brush Tool"
```

## Teia Compatibility

The packager automatically handles:

- ✅ **Static HTML Structure**: Creates `index.html` as the main entry point
- ✅ **Relative Paths**: All resources use relative paths for proper loading
- ✅ **Self-contained**: Bundles all necessary AC system files
- ✅ **Cover Image**: Generates or includes required `og:image` meta tag
- ✅ **Iframe Compatibility**: Handles sandboxed localStorage/sessionStorage limitations
- ✅ **URL Parameters**: Extracts `viewer` and `creator` addresses from Teia
- ✅ **Offline Operation**: Works without server-side processing
- ✅ **Sandbox Mode**: Completely bypasses auth0, authentication, and server features
- ✅ **Network Isolation**: Blocks external network requests except for bundled resources
- ✅ **Mock Services**: Provides stub implementations for missing dependencies

## Package Structure

Each generated package contains:

```
tokens/piece-name/
├── index.html              # Main entry point
├── cover.svg              # Cover image (auto-generated or custom)
└── aesthetic.computer/    # Bundled AC system
    ├── boot.mjs          # Modified bootstrap for Teia
    ├── style.css         # AC styles
    ├── bios.mjs          # System BIOS
    ├── favicon.png       # Simple favicon
    ├── lib/              # Essential library files
    │   ├── helpers.mjs
    │   ├── num.mjs
    │   └── parse.mjs
    └── systems/          # System files
        └── *.mjs
```

## How It Works

1. **Piece Loading**: The packager reads the piece source code (`.mjs` or `.lisp`) from `system/public/aesthetic.computer/disks/`

2. **Static Generation**: Instead of using Netlify functions, it inlines the piece code and creates a self-contained HTML file

3. **System Bundling**: Copies essential AC system files and modifies `boot.mjs` to work in Teia's environment

4. **Teia Integration**: Handles URL parameters (`?viewer=...&creator=...`) and iframe constraints

5. **Compression**: Creates a `.zip` file ready for upload to Teia

## Uploading to Teia

1. Run the packager: `node utilities/ac-pack.mjs your-piece-name`
2. Navigate to [teia.art/mint](https://teia.art/mint)
3. Upload the generated `tokens/your-piece-name.zip` file
4. Preview and test your interactive OBJKT
5. Mint when ready!

## Supported Piece Types

- ✅ **JavaScript pieces** (`.mjs` files)
- ✅ **Lisp pieces** (`.lisp` files) 
- ✅ **Self-contained pieces** (no external dependencies)
- ⚠️ **Limited support** for pieces requiring server-side features

## Limitations

Due to Teia's sandboxed environment:

- No server-side processing
- Limited external network access (see [Teia docs](https://github.com/teia-community/teia-docs/wiki/Interactive-OBJKTs) for allowed domains)
- No direct localStorage access (handled gracefully)
- No real-time multiplayer features

## Testing

You can test packages locally by:

1. **Local HTTP Server**: Serve the `tokens/piece-name/` directory with a static server
2. **Direct Testing**: Open `index.html` in a browser
3. **Teia Simulation**: Test with URL parameters: `?viewer=tz1...&creator=tz1...`
4. **Test Tool**: Use `tokens/test-local.html` to load and test zip files locally

### Using the Local Test Tool

1. Open `tokens/test-local.html` in your browser
2. Select your generated `.zip` file
3. Click "Load Zip" to extract and preview
4. Click "Test with Teia Parameters" to simulate the Teia environment
5. Check the console log for any errors or issues

This helps you verify everything works before uploading to Teia!

## Examples

See the generated packages in this directory for examples of working Teia-compatible aesthetic.computer pieces.
