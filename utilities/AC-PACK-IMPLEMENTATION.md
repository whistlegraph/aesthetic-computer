# 🎨 AC-Pack: Aesthetic Computer → Teia Packager

## Overview

After analyzing the [Teia Interactive OBJKTs documentation](https://github.com/teia-community/teia-docs/wiki/Interactive-OBJKTs) and the aesthetic.computer codebase, I've created a comprehensive packaging solution that allows you to export aesthetic.computer pieces as static, self-contained zip files ready for upload to Teia.

## Key Requirements Addressed

### Teia Platform Constraints
- ✅ **Static HTML Structure**: Main file must be `index.html`
- ✅ **Relative Paths**: All resources use relative paths
- ✅ **Self-contained**: All dependencies bundled (no external resources except allowed domains)
- ✅ **Cover Image**: Required `og:image` meta tag with proper cover image
- ✅ **Iframe Compatibility**: Handles sandboxed localStorage/sessionStorage limitations
- ✅ **URL Parameters**: Extracts `viewer` and `creator` addresses from Teia
- ✅ **Zip Format**: Compressed for direct upload to teia.art/mint

### Aesthetic Computer Integration
- ✅ **Piece Loading**: Supports both `.mjs` (JavaScript) and `.lisp` files
- ✅ **System Dependencies**: Bundles all AC system files (43 lib files + 3 system files)
- ✅ **Bootstrap Modification**: Adapts `boot.mjs` for Teia environment
- ✅ **Metadata Extraction**: Pulls title and description from piece source code
- ✅ **Offline Operation**: Works without server-side Netlify functions

## Usage

### Basic Commands
```bash
# Navigate to utilities and install dependencies
cd utilities && npm install

# Pack a piece
node ac-pack.mjs <piece-name>

# With custom options
node ac-pack.mjs <piece-name> --title "Custom Title" --description "Custom description" --author "Your Name"
```

### Examples
```bash
# Pack the starfield piece
node ac-pack.mjs starfield --title "Aesthetic Starfield" --description "Interactive starfield animation"

# Pack with custom cover image
node ac-pack.mjs paint --cover ./my-cover.png --title "AI Paint Studio"

# Pack a Lisp piece
node ac-pack.mjs brush --title "Brush Tool" --author "Artist Name"
```

## How It Works

### 1. Piece Analysis
- Loads the piece source code from `system/public/aesthetic.computer/disks/`
- Supports both JavaScript (`.mjs`) and Lisp (`.lisp`) pieces
- Extracts metadata from comments and exports

### 2. Static HTML Generation
- Creates `index.html` as the main entry point
- Inlines the piece source code directly in the HTML
- Sets up Teia-compatible initialization with URL parameter handling
- Includes all necessary meta tags for proper display on Teia

### 3. Dependency Bundling
- **Core Files**: `boot.mjs`, `style.css`, `bios.mjs`, `favicon.png`
- **Library Files**: All 43 `.mjs` files from `aesthetic.computer/lib/`
- **System Files**: All `.mjs` files from `aesthetic.computer/systems/`
- **Path Resolution**: Modifies import paths to work with relative file structure

### 4. Teia Compatibility Layer
- Modifies `boot.mjs` to handle iframe constraints
- Provides graceful fallbacks for localStorage/sessionStorage limitations
- Extracts `viewer` and `creator` addresses from URL parameters
- Handles relative path resolution for all bundled modules

### 5. Asset Management
- Generates or copies cover images (supports PNG, SVG)
- Creates minimal favicon
- Maintains proper relative path structure for all resources

## Generated Package Structure

```
tokens/piece-name/
├── index.html                    # Main entry point with Teia compatibility
├── cover.svg                     # Auto-generated or custom cover image
└── aesthetic.computer/           # Complete AC system bundle
    ├── boot.mjs                  # Modified bootstrap for Teia
    ├── style.css                 # AC styles
    ├── bios.mjs                  # System BIOS
    ├── favicon.png               # Simple favicon
    ├── lib/                      # All 43 library files
    │   ├── ui.mjs
    │   ├── geo.mjs
    │   ├── store.mjs
    │   ├── gamepad.mjs
    │   ├── keyboard.mjs
    │   ├── sound/
    │   └── ... (40 more files)
    └── systems/                  # System files
        ├── nopaint.mjs
        ├── prompt-system.mjs
        └── world.mjs
```

## Teia Upload Process

1. **Pack**: `node utilities/ac-pack.mjs your-piece-name`
2. **Upload**: Go to [teia.art/mint](https://teia.art/mint)
3. **Test**: Upload the generated `tokens/your-piece-name.zip`
4. **Preview**: Test the interactive OBJKT in Teia's preview
5. **Mint**: Complete the minting process

## Supported Features

### ✅ Working
- JavaScript and Lisp pieces
- Basic graphics and animation
- User interaction (mouse, keyboard)
- Teia viewer/creator address access
- Self-contained execution
- Responsive design

### ⚠️ Limited Support
- Audio (requires user gesture)
- Network requests (only to allowed domains)
- Local storage (fallback handling)

### ❌ Not Supported
- Server-side processing
- Real-time multiplayer
- Netlify function features
- External API calls (except allowed domains)

## Architecture Benefits

1. **Complete Offline Capability**: No dependency on aesthetic.computer servers
2. **Future-Proof**: Self-contained packages work independently 
3. **Platform Agnostic**: Can run on any static file server
4. **Teia Optimized**: Specifically designed for Teia's constraints
5. **Preservation**: Creates permanent, archival versions of pieces

## Next Steps

The `ac-pack` utility successfully bridges aesthetic.computer's dynamic, server-dependent architecture with Teia's static, self-contained requirements. This enables the aesthetic.computer ecosystem to participate in the NFT/blockchain art space while maintaining the creative coding workflow developers are familiar with.

Future enhancements could include:
- Audio optimization for Teia environment
- Custom shader/GL support
- Asset optimization and compression
- Batch processing for multiple pieces
- Integration with aesthetic.computer's piece management system
