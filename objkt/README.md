# 🎭 Teia Integration for Aesthetic Computer

> **⚠️ For LLMs/AI Agents:** Do NOT edit files in the `/output` directory. These are auto-generated build artifacts that get overwritten. Always modify source files and rebuild using the pack script.

This directory contains tools for packaging aesthetic.computer pieces as Teia Interactive OBJKTs for minting on the Tezos blockchain.

## Quick Start

### Building a Piece for Teia

1. **Navigate to the teia directory:**
   ```bash
   cd /workspaces/aesthetic-computer/teia
   ```

2. **Pack any AC piece for OBJKT deployment:**
   ```bash
   ./pack-for-teia.sh PIECE_NAME
   ```
   
   Example:
   ```bash
   ./pack-for-teia.sh starfield
   ```

3. **Your package will be created at:**
   - **Directory:** `output/PIECE_NAME/`
   - **Zip file:** `output/PIECE_NAME.zip` 

4. **Deploy to Teia:**
   - Go to [teia.art/mint](https://teia.art/mint)
   - Upload the `.zip` file
   - Preview and test your interactive OBJKT
   - Set metadata and mint!

## How the Packing Process Works

The `pack-for-teia.sh` script creates a fully self-contained package:

1. **Loads the piece** from `system/public/aesthetic.computer/disks/PIECE_NAME.mjs`
2. **Bundles AC system** (boot.mjs, bios.mjs, all required libraries)
3. **Applies OBJKT compatibility patches**:
   - Enables PACK mode for sandboxed iframe environment
   - Disables authentication and session features
   - Patches URL resolution for bundled assets
   - Removes verbose console logging
4. **Packages dependencies** (fonts, external libraries, etc.)
5. **Creates deployment-ready ZIP** with HTML entry point

## Package Structure

Each built package contains:
```
PIECE_NAME/
├── index.html              # Entry point with OBJKT configuration
├── cover.svg              # Auto-generated cover image
└── aesthetic.computer/     # Complete AC system bundle
    ├── boot.mjs           # TEIA-patched boot loader
    ├── bios.mjs           # TEIA-patched system core
    ├── style.css          # Enhanced styling for nogap mode
    ├── disks/             # Piece files and dependencies
    ├── lib/               # All AC libraries (disk.mjs, etc.)
    ├── dep/               # External dependencies
    └── type/              # Font assets and glyph files
```

## OBJKT Environment Features

### Automatic Configuration
- **TEIA Mode:** Automatically detected and enabled
- **Sandboxed Environment:** Safe execution with limited network access
- **Clean Console:** Minimal logging for professional presentation
- **Responsive Layout:** Adapts to different iframe sizes

### Available Parameters
Teia provides these URL parameters to your piece:
- `viewer` - Tezos address of current viewer
- `creator` - Tezos address of OBJKT creator  
- `objkt` - Whether this is an OBJKT context

### Access Parameters in Your Piece
```javascript
// These are automatically available as globals
const viewer = window.acOBJKT_VIEWER;    // tz1abc...
const creator = window.acOBJKT_CREATOR;  // tz1def...
```

## Development Tips

### Console Output
The build process creates clean console output showing only:
```
Aesthetic Computer
TEIA
```

### Local Testing
After building, test locally:
```bash
# Server automatically starts on port 8001 after build
# Open http://localhost:8001 with OBJKT simulation parameters
```

### File Organization
- **Source files:** Always edit files in `system/public/aesthetic.computer/`
- **Build script:** Modify `ac-pack.mjs` for packing logic changes
- **Generated files:** Never edit `output/` directory contents directly

## Troubleshooting

### Common Issues
- **403/404 errors:** Usually URL resolution problems - check path construction in patches
- **Piece not loading:** Verify piece exists in `disks/` directory
- **Console spam:** Check that logging patches are applied correctly

### Debugging
- Review generated HTML in `output/PIECE_NAME/index.html`
- Check browser console for error messages
- Test with OBJKT URL parameters: `?viewer=tz1abc&creator=tz1def`

---

**Ready to mint your interactive art on Tezos!** 🎨⛓️
    ├── bios.mjs          # Core system
    ├── style.css         # Styles
    ├── lib/              # Library modules
    └── systems/          # System modules
```

## Supported Pieces

Most aesthetic.computer pieces should work in Teia with minimal to no modifications. However, some limitations apply:

### ✅ Fully Supported
- Drawing and painting tools (paint, brush, spray)
- Generative art (starfield, sparkle, noise)
- Interactive games and experiences
- Audio/visual pieces

### ⚠️ Limited Support
- Network-dependent pieces (may need fallbacks)
- Authentication-required pieces
- WebRTC/real-time multiplayer features

### ❌ Not Supported
- Server-side functionality
- External API dependencies
- File system access

## Development Workflow

1. **Create/Test Piece**: Develop in aesthetic.computer environment
2. **Pack for Teia**: Use packing tools to create Teia-compatible version
3. **Local Testing**: Test the packaged version locally
4. **Upload to Teia**: Deploy as Interactive OBJKT
5. **Monitor Performance**: Check piece behavior in Teia environment

## Testing

Test packaged pieces locally:
```bash
# After packing
cd tokens/piece-name
python -m http.server 8000
# Visit http://localhost:8000
```

Test with Teia parameters:
```
http://localhost:8000?viewer=tz1abc...&creator=tz1def...
```

## Examples

See the `/examples` directory for sample pieces that have been successfully deployed to Teia.

## Troubleshooting

### Common Issues

1. **Piece doesn't load**: Check browser console for JavaScript errors
2. **Missing assets**: Ensure all dependencies are bundled
3. **Performance issues**: Large pieces may need optimization for Teia
4. **Audio problems**: Web Audio API restrictions in iframes

### Debug Mode

Enable debug logging:
```javascript
window.acTEIA_DEBUG = true;
```

## Resources

- [Teia Documentation](https://github.com/teia-community/teia-docs)
- [Interactive OBJKTs Guide](https://github.com/teia-community/teia-docs/wiki/Interactive-OBJKTs)
- [Aesthetic Computer Documentation](../README.md)

## Contributing

To improve Teia integration:

1. Test pieces in Teia environment
2. Document compatibility issues
3. Submit fixes for sandboxing problems
4. Add examples of successful deployments

## License

Same as aesthetic.computer project.
