# Glaze System - Technical Architecture Report

**Date:** 2026-02-16
**Author:** Technical documentation for the Aesthetic Computer glaze rendering system

---

## Executive Summary

The **Glaze** system is a WebGL2-based post-processing pipeline that creates volumetric lighting effects over Aesthetic Computer's software rasterizer. It provides atmospheric depth and visual polish to pieces, particularly the `prompt` interface where it creates a characteristic moody, ethereal glow around text and UI elements.

## System Overview

### Purpose

Glaze transforms the flat 2D output from AC's software rasterizer into a volumetric 3D scene using:
- Physically-based volumetric ray marching
- Henyey-Greenstein phase functions for light scattering
- Multi-pass rendering pipeline (frag → compute → display)

### Key Components

1. **Glaze Core** ([glaze.mjs](../system/public/aesthetic.computer/lib/glaze.mjs))
2. **Uniforms Configuration** ([uniforms.js](../system/public/aesthetic.computer/lib/glazes/uniforms.js))
3. **GLSL Shaders** (per-glaze-type):
   - `prompt-frag.glsl` - Volumetric ray marching
   - `prompt-compute.glsl` - Intermediate processing
   - `prompt-display.glsl` - Final compositing

## Architecture

### Data Flow

```
[Software Rasterizer Output]
         ↓
    [Texture Upload] ← glaze.update()
         ↓
  [Frag Shader Pass] ← Custom volumetric effects
         ↓ (texFbSurfA)
 [Compute Shader Pass] ← Post-processing
         ↓ (texFbSurfB)
 [Display Shader Pass] ← Final composite & upscale
         ↓
  [Canvas Output (WebGL2)]
```

### Three-Stage Rendering Pipeline

#### Stage 1: Frag (Volumetric Rendering)
- **Input:** Original texture (`iTexture`), previous frame (`iTexturePost`)
- **Process:** Ray marching through volumetric fog
- **Output:** Rendered to `texFbSurfA`
- **Resolution:** Matches software rasterizer (e.g., 320×240)

**Key Technique:** Uses physically-based volumetric rendering:
```glsl
// Ray marching loop (simplified)
for (int i = 0; i < fogIterations; i++) {
    pos = marchAlongRay();
    colorValue = getColor(pos);
    stepAbs = calculateAbsorption(colorValue);
    stepCol = calculateScattering(stepAbs);
    volCol += stepCol * volAbs * directLight(pos, dir);
    volAbs *= stepAbs;
}
```

#### Stage 2: Compute
- **Input:** Original texture + Frag output
- **Process:** Intermediate processing (currently minimal)
- **Output:** Rendered to `texFbSurfB`

#### Stage 3: Display
- **Input:** Original texture + Compute output
- **Process:** Final compositing and upscaling to native resolution
- **Output:** Directly to canvas
- **Resolution:** Native display resolution (e.g., 1920×1080 @ devicePixelRatio)

### Uniforms System

Uniforms control the visual appearance of each glaze type. They're defined per-type in `uniforms.js`:

```javascript
uniforms.prompt = {
  "1i:fogIterations": 24,        // Ray marching samples (quality)
  "1i:shadowIterations": 6,      // Shadow samples (quality)
  "1f:lightPower": 10,           // Light intensity
  "1f:innerDensity": 24,         // Text/content glow strength
  "3f:bgColor": [0.084, 0.0533, 0.078],  // Background tint
  // ... more uniforms
};
```

**Naming Convention:** `<type>:<name>`
- `1i` = int
- `1f` = float
- `3f` = vec3

### Dynamic Uniforms

Pieces can override uniforms at runtime:

```javascript
// In disk.mjs
glaze: function(content) {
  if (glazeEnabled === content.on) return;
  glazeEnabled = content.on;
  if (content.on) {
    send({ type: "glaze", content });
  }
}
```

## Integration Points

### 1. Piece API (disk.mjs:2397)

Pieces enable/disable glaze:

```javascript
function boot({ glaze, dark }) {
  if (dark) glaze({ on: true });
}
```

### 2. BIOS Integration (bios.mjs)

The BIOS manages glaze lifecycle:
- Receives `{ type: "glaze", content }` messages from disk
- Calls `glaze.on()` / `glaze.off()`
- Handles resolution changes via `glaze.frame()`
- Uploads textures each frame via `glaze.update()`

### 3. Rendering Loop

Each frame:
1. Software rasterizer draws to ImageData
2. `glaze.update(imageData)` uploads texture
3. `glaze.render(time, mouse)` executes 3-stage pipeline
4. WebGL canvas overlays software canvas

## Performance Characteristics

### Configurable Quality Tradeoffs

| Uniform | Impact | Low | Medium | High |
|---------|--------|-----|--------|------|
| `fogIterations` | Quality/Performance | 16 | 24 | 32+ |
| `shadowIterations` | Shadow smoothness | 4 | 6 | 8+ |
| `innerDensity` | Glow intensity | 16 | 24 | 32+ |
| `lightPower` | Brightness | 6 | 10 | 16+ |

**Current Settings (Darker aesthetic):**
- `fogIterations: 24` - Balanced quality
- `shadowIterations: 6` - Smooth but efficient
- `innerDensity: 24` - Subtle glow
- `lightPower: 10` - Moody, darker feel

**Previous Settings (Brighter - commit bc84aab):**
- `fogIterations: 32` (+33% quality, +33% GPU cost)
- `innerDensity: 32` (+33% vibrancy)
- `lightPower: 16` (+60% brightness)

### GPU Requirements

- **Minimum:** WebGL2 support
- **Contexts:** Uses hardware-accelerated context with:
  - `desynchronized: true` (reduced latency)
  - `antialias: false` (performance)
  - `alpha: false` (opaque output)

## Key Technical Details

### Volumetric Rendering Algorithm

The prompt glaze uses **volumetric path tracing** to simulate light scattering through a foggy medium:

1. **Ray Setup:** Cast rays from camera through each pixel
2. **Volume Definition:** 2D texture extruded into thin 3D volume
3. **Marching:** Sample volume at regular intervals (`fogIterations`)
4. **Absorption:** Calculate light absorbed by particles: `exp(-density * distance * color)`
5. **Scattering:** Use Henyey-Greenstein phase function for directional scattering
6. **Shadows:** March toward light source to calculate occlusion
7. **Accumulation:** Composite samples front-to-back

### Henyey-Greenstein Phase Function

Controls how light scatters:

```glsl
float hgPhase(vec3 dirIn, vec3 dirOut) {
  float g = anisotropy; // -0.123 = slight back-scattering
  return (PI/4) * (1 - g*g) / pow(1 + g*(g - 2*dot(dirIn, dirOut)), 1.5);
}
```

- `anisotropy = 0`: Isotropic (equal scattering)
- `anisotropy < 0`: Back-scattering (current: -0.123)
- `anisotropy > 0`: Forward-scattering

### Radial Blur

Creates depth-of-field effect:

```glsl
vec3 refractionDir = mix(vec3(0, 0, 1), rd, radialBlurAmount);
```

- `radialBlurAmount = 0`: Pure forward blur
- `radialBlurAmount = 1`: Sharp perspective (current: 0.9)

## Historical Context

### Evolution

**2022-02:** Initial implementation
**2024-02-29:** Added dark mode toggle (commit 67e9e5c / cb3c8f7)
**2026-02-13:** Increased brightness/quality (commit bc84aab)
**2026-02-16:** Reverted to darker aesthetic (this change)

### Design Philosophy

The glaze creates AC's signature **"digital fog"** aesthetic - a liminal space between 2D and 3D, pixel art and ray tracing. The darker tuning emphasizes mystery and intimacy over clarity and brightness.

## Usage Examples

### Enabling in a Piece

```javascript
// prompt.mjs
function boot({ glaze, dark }) {
  if (dark) glaze({ on: true });
}
```

### Creating a New Glaze Type

1. Add uniforms in `uniforms.js`:
```javascript
uniforms.myGlaze = {
  "1f:someParam": 1.0,
  "3f:someColor": [1, 0, 0],
};
```

2. Create shaders:
```
lib/glazes/myGlaze/
  ├── myGlaze-frag.glsl
  ├── myGlaze-compute.glsl
  └── myGlaze-display.glsl
```

3. Use in piece:
```javascript
glaze({ on: true, type: "myGlaze" });
```

## Debugging

### Common Issues

**Black screen:**
- Check WebGL2 support: `gl !== null`
- Verify shaders compiled: `programsCompiled === true`

**Performance issues:**
- Reduce `fogIterations` / `shadowIterations`
- Check `devicePixelRatio` (high DPI = 4x pixels)

**Visual artifacts:**
- XY offset when `radialBlurAmount = 0` (known issue, see uniforms.js:15)
- Verify texture upload via `glaze.update()` each frame

### Useful Console Commands

```javascript
// Check if glaze is active
window.glazeEnabled

// Access glaze instance (in bios.mjs scope)
// Not directly exposed - check via canvas element:
document.querySelector('canvas[data-type="glaze"]')
```

## Future Improvements

### Potential Enhancements

1. **Dynamic quality scaling** based on GPU performance
2. **Multi-glaze compositing** (layer multiple effects)
3. **Temporal accumulation** for noise reduction
4. **Glaze presets** (moody, bright, retro, etc.)
5. **User-configurable uniforms** via UI

### Known TODOs

- Fix XY offset when `radialBlurAmount = 0` (uniforms.js:15)
- Rename pipeline stages from frag→compute→display to more intuitive names
- Rename `setCustomUniforms` → `setCustomFragUniforms`

## References

### Source Files

- [glaze.mjs](../system/public/aesthetic.computer/lib/glaze.mjs) - Core implementation
- [uniforms.js](../system/public/aesthetic.computer/lib/glazes/uniforms.js) - Configuration
- [prompt-frag.glsl](../system/public/aesthetic.computer/lib/glazes/prompt/prompt-frag.glsl) - Ray marching shader
- [disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs) - API surface (line 2397)
- [prompt.mjs](../system/public/aesthetic.computer/disks/prompt.mjs) - Primary usage (line 763)

### Related Systems

- **Software Rasterizer** - Generates input texture
- **BIOS** - Orchestrates rendering pipeline
- **Module Loader** - Hot reloads shaders during development

---

*This report documents the glaze system as of 2026-02-16. For questions or improvements, see the source code or contact @jeffrey.*
