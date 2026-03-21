# OSR GPU Native Addon

Native Node.js addon for handling Electron's Offscreen Rendering (OSR) GPU shared textures.

## Overview

When Electron's offscreen rendering is configured with `useSharedTexture: true`, the `paint` event provides a `texture` object containing a native GPU texture handle (IOSurface on macOS, D3D11 texture on Windows). This addon allows you to:

1. Import the texture into OpenGL (macOS)
2. Copy the texture data to a CPU buffer for use in WebGL

## Building

```bash
cd native/osr-gpu
npm install
npm run build
```

## API

### `initContext()`
Initialize the native graphics context (OpenGL on macOS, D3D11 on Windows).

### `copyTextureToBuffer(textureInfo)`
Copy the GPU texture data to a CPU buffer. This is the recommended approach for WebGL compatibility.

**Parameters:**
- `textureInfo` - The texture info object from Electron's paint event `e.texture.textureInfo`
  - `sharedTextureHandle` - Buffer containing the native handle

**Returns:**
- `{ data: Uint8Array, width: number, height: number }` - RGBA pixel data

### `importTexture(textureInfo)` (macOS only)
Import the IOSurface directly as an OpenGL texture.

**Returns:**
- `{ textureId: number, width: number, height: number, target: string }`

### `releaseTexture(textureId)`
Release an imported texture.

### `destroyContext()`
Clean up the native graphics context.

## Usage in Electron

```javascript
const osrGpu = require('./native/osr-gpu');

// Initialize once
osrGpu.initContext();

// In your offscreen window
win.webContents.on('paint', async (e, dirty, image) => {
  if (e.texture) {
    // Option 1: Copy to buffer (recommended for WebGL)
    const { data, width, height } = osrGpu.copyTextureToBuffer(e.texture.textureInfo);
    // Use data as RGBA Uint8Array...
    
    // IMPORTANT: Always release the texture when done!
    e.texture.release();
  }
});

// Cleanup on exit
osrGpu.destroyContext();
```

## Platform Support

- **macOS**: Full support via IOSurface + OpenGL
- **Windows**: Full support via D3D11 shared textures
- **Linux**: Not yet implemented (TODO)

## References

- [Electron OSR Documentation](https://github.com/electron/electron/blob/main/shell/browser/osr/README.md)
- [CEF OSR Implementation](https://github.com/chromiumembedded/cef/blob/main/tests/cefclient/browser/osr_renderer.cc)
