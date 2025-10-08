# Starfield for Teia

This is an example of packaging the `starfield` piece from aesthetic.computer for deployment as a Teia Interactive OBJKT.

## About Starfield

The starfield piece creates a classic 3D starfield effect with stars moving toward the viewer, creating a hypnotic space travel visualization.

**Features:**
- 128 animated stars by default (configurable)
- 3D projection with perspective 
- Infinite loop with star recycling
- Configurable speed and spread
- Optional color and alpha parameters

## Teia Package Details

**Package Size:** ~520KB  
**Files Bundled:** 62  
**Dependencies:** Full aesthetic.computer system + WebGL libraries  

### Package Contents
```
starfield/
├── index.html              # Entry point with Teia integration
├── cover.svg              # Generated placeholder cover
└── aesthetic.computer/    # AC system bundle
    ├── boot.mjs          # Modified for Teia sandbox
    ├── bios.mjs          # Core system
    ├── style.css         # Styles
    ├── lib/              # 45+ library modules
    ├── systems/          # 3 system modules  
    └── dep/              # 6 dependency files
```

## Testing Results

✅ **Loads successfully** in Teia iframe environment  
✅ **All dependencies** resolved properly  
✅ **Animation runs** smoothly at 60fps  
✅ **No console errors** after fixes  
✅ **Responsive** to window resizing  

## Known Issues

- ⚠️ Some unmapped source file warnings (cosmetic only)
- ⚠️ WebRTC features disabled in sandbox mode (not used by starfield)

## Performance

- **Initial load:** ~0.5s
- **Memory usage:** ~15MB
- **CPU usage:** Low (efficient WebGL rendering)
- **Mobile compatibility:** Good

## Teia Integration Notes

The piece automatically detects Teia environment via:
```javascript
window.acOBJKT_MODE = true;
window.acPIECE_NAME = "starfield";
```

Teia URL parameters are accessible:
```javascript
window.acOBJKT_VIEWER  // Viewer's Tezos address
window.acOBJKT_CREATOR // Creator's Tezos address  
```

## Deployment Steps

1. Package: `./teia/pack-for-teia.sh starfield`
2. Test locally: `cd teia/output/starfield && python3 -m http.server`
3. Upload `starfield.zip` to [teia.art/mint](https://teia.art/mint)
4. Preview and mint!

## Source Code

Original piece: [`/system/public/aesthetic.computer/disks/starfield.mjs`](../../system/public/aesthetic.computer/disks/starfield.mjs)

## Live Example

🚀 **Ready for OBJKT deployment!** This piece demonstrates that aesthetic.computer works beautifully in the Teia Interactive OBJKT environment.
