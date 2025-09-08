# Starfield Example for Teia

## Overview

Starfield is a classic 3D starfield effect that creates the illusion of traveling through space. This piece demonstrates how generative visual effects can be packaged for Teia Interactive OBJKTs.

## Original Piece

**Location**: `system/public/aesthetic.computer/disks/starfield.mjs`  
**Type**: JavaScript (.mjs)  
**Created**: 2021  
**Description**: A classic starfield effect with configurable parameters

## Features

- **3D Perspective**: Stars move toward the viewer creating depth illusion
- **Configurable**: Number of stars, speed, and spread can be adjusted
- **Interactive**: Responds to piece options and parameters
- **Performance**: Optimized for smooth animation

## Teia Compatibility

### ✅ Works Great
- Pure JavaScript with no external dependencies
- Self-contained rendering logic
- No network requests required
- Lightweight and fast

### Technical Details
- **Bundle Size**: ~497KB (includes full AC system)
- **Performance**: 60fps on most devices
- **Memory Usage**: Low (minimal state)
- **Browser Support**: All modern browsers

## Packing Process

```bash
./teia/pack-for-teia.sh starfield
```

Generated files:
- `teia/output/starfield.zip` (ready for Teia upload)
- `teia/output/starfield/` (unpacked version for testing)

## Parameters

The starfield piece accepts options:
- `stars`: Number of stars (default: 128)
- `color`: Star color array `[r, g, b]`
- `alpha`: Star transparency (0-1)

Example URL with parameters:
```
?piece=starfield&stars=256&color=[255,255,0]&alpha=0.8
```

## Testing

Local testing:
```bash
cd teia/output/starfield
python -m http.server 8000
# Visit http://localhost:8000
```

Test with Teia parameters:
```
http://localhost:8000?viewer=tz1abc...&creator=tz1def...
```

## Performance Notes

- Scales well with star count (tested up to 1000 stars)
- CPU-based rendering (no WebGL dependency)
- Minimal memory allocation during animation
- Suitable for mobile devices

## Deployment Status

- [ ] Tested locally ✅
- [ ] Uploaded to Teia
- [ ] Live OBJKT URL: _pending_
- [ ] Community feedback: _pending_

## Lessons Learned

1. **Simplicity Works**: Simple generative pieces often work best in Teia
2. **Self-Contained**: No external dependencies = no compatibility issues
3. **Performance**: Smooth animation is crucial for user experience
4. **Packaging**: AC's modular system bundles cleanly

## Next Steps

- Upload to Teia and test in production environment
- Document any iframe-specific behavior differences
- Consider variations (color cycling, speed controls, etc.)
- Add to Teia community examples
