// 🎨 Blend2D Backend
// High-performance 2D vector graphics via WebAssembly

/* #region 📚 Notes
  Blend2D (https://blend2d.com) is a high-performance 2D vector graphics engine.
  - JIT-compiled rendering pipelines
  - Very fast anti-aliased primitives
  - Rich feature set (gradients, patterns, text, composition)
  - Used in production graphics software
  
  WASM Build:
  - No official WASM build, needs to be compiled
  - Build instructions: https://blend2d.com/doc/build-instructions.html
  - Emscripten can be used to compile to WASM
  
  API Overview:
  - BLContext: Main rendering context
  - BLImage: Image buffer
  - BLPath: Vector paths
  - BLGradient: Linear/radial gradients
  
  To use:
  1. Compile blend2d to WASM with Emscripten
  2. Place blend2d.wasm and blend2d.js in lib/gpu/wasm/
  3. The backend will load them on init
#endregion */

import { createBaseRenderer } from "./renderer-interface.mjs";
import { log } from "../logs.mjs";

// Blend2D WASM module (loaded dynamically)
let Blend2D = null;
let blContext = null;
let blImage = null;

/**
 * Load the Blend2D WASM module
 * @returns {Promise<boolean>}
 */
async function loadBlend2D() {
  if (Blend2D) return true;

  try {
    // Try to load Blend2D WASM module
    const wasmPath = new URL("./wasm/blend2d.js", import.meta.url).href;

    // Dynamic import with error handling
    const module = await import(wasmPath).catch(() => null);
    if (!module) {
      log.gpu.warn?.("Blend2D WASM module not found at", wasmPath);
      return false;
    }

    // Initialize Blend2D
    Blend2D = await module.default?.();
    if (!Blend2D) {
      log.gpu.warn?.("Failed to initialize Blend2D module");
      return false;
    }

    log.gpu.success?.("Blend2D WASM loaded");
    return true;
  } catch (err) {
    log.gpu.error?.("Failed to load Blend2D:", err);
    return false;
  }
}

/**
 * Create a Blend2D renderer backend
 * @returns {Object} Renderer instance
 */
export function createBlend2DBackend() {
  const base = createBaseRenderer();
  let ctx = null;
  let canvas = null;
  let imageData = null;
  let clearColor = [0, 0, 0, 255];
  let currentColor = [255, 255, 255, 255];
  let isReady = false;

  // Command queue for batching
  const commandQueue = [];

  return {
    ...base,

    async init(targetCanvas) {
      canvas = targetCanvas;
      ctx = canvas.getContext("2d");

      if (!ctx) {
        log.gpu.warn?.("Blend2D: Failed to get 2D context");
        return false;
      }

      // Try to load Blend2D WASM
      const loaded = await loadBlend2D();
      if (!loaded) {
        log.gpu.warn?.("Blend2D: WASM not available, backend disabled");
        return false;
      }

      try {
        // Create Blend2D image (render target)
        blImage = new Blend2D.Image(canvas.width, canvas.height, Blend2D.Format.PRGB32);

        // Create Blend2D context
        blContext = new Blend2D.Context(blImage);

        // Create image data buffer for blitting
        imageData = ctx.createImageData(canvas.width, canvas.height);

        isReady = true;
        log.gpu.success?.("Blend2D backend initialized");
        return true;
      } catch (err) {
        log.gpu.error?.("Blend2D initialization failed:", err);
        return false;
      }
    },

    destroy() {
      if (blContext) {
        blContext.end();
        blContext = null;
      }
      if (blImage) {
        blImage = null;
      }
      Blend2D = null;
      isReady = false;
      base.destroy();
    },

    isReady() {
      return isReady;
    },

    getName() {
      return "blend2d";
    },

    handleCommand(command) {
      if (!isReady) {
        console.warn("Blend2D not ready");
        return;
      }

      switch (command.type) {
        case "clear":
          clearColor = command.color || [0, 0, 0, 255];
          break;
        case "line":
          commandQueue.push({
            type: "line",
            x1: command.x1,
            y1: command.y1,
            x2: command.x2,
            y2: command.y2,
            color: command.color || currentColor,
          });
          break;
        case "frame-end":
          this.endFrame();
          break;
        case "perf-overlay":
          // TODO: Implement perf overlay
          break;
        default:
          base.handleCommand(command);
      }
    },

    clear(r, g, b, a = 255) {
      clearColor = [r, g, b, a];
    },

    line(x1, y1, x2, y2, color) {
      commandQueue.push({ type: "line", x1, y1, x2, y2, color });
      base.incrementDrawCalls();
    },

    box(x, y, w, h, color) {
      commandQueue.push({ type: "box", x, y, w, h, color });
      base.incrementDrawCalls();
    },

    circle(cx, cy, r, color) {
      commandQueue.push({ type: "circle", cx, cy, r, color });
      base.incrementDrawCalls();
    },

    endFrame() {
      if (!isReady || !Blend2D || !blContext) {
        commandQueue.length = 0;
        return;
      }

      try {
        // Begin rendering
        blContext.begin(blImage);

        // Clear with background color
        blContext.setFillStyle(
          Blend2D.Rgba32(clearColor[0], clearColor[1], clearColor[2], clearColor[3])
        );
        blContext.fillAll();

        // Draw all queued commands
        for (const cmd of commandQueue) {
          if (cmd.type === "line") {
            blContext.setStrokeStyle(
              Blend2D.Rgba32(cmd.color[0], cmd.color[1], cmd.color[2], cmd.color[3] ?? 255)
            );
            blContext.setStrokeWidth(1);
            blContext.strokeLine(cmd.x1, cmd.y1, cmd.x2, cmd.y2);
          } else if (cmd.type === "box") {
            blContext.setFillStyle(
              Blend2D.Rgba32(cmd.color[0], cmd.color[1], cmd.color[2], cmd.color[3] ?? 255)
            );
            blContext.fillRect(cmd.x, cmd.y, cmd.w, cmd.h);
          } else if (cmd.type === "circle") {
            blContext.setFillStyle(
              Blend2D.Rgba32(cmd.color[0], cmd.color[1], cmd.color[2], cmd.color[3] ?? 255)
            );
            blContext.fillCircle(cmd.cx, cmd.cy, cmd.r);
          }
        }

        // End rendering
        blContext.end();

        // Get pixel data and blit to canvas
        const pixels = blImage.getData();
        if (pixels && imageData) {
          // Blend2D uses PRGB32 (premultiplied), may need conversion
          imageData.data.set(new Uint8ClampedArray(pixels.buffer));
          ctx.putImageData(imageData, 0, 0);
        }
      } catch (err) {
        log.gpu.error?.("Blend2D render error:", err);
      }

      // Clear queue
      commandQueue.length = 0;
      base.endFrame();
    },

    resize(width, height) {
      if (!isReady) return;

      canvas.width = width;
      canvas.height = height;

      // Recreate Blend2D image with new size
      if (Blend2D) {
        blImage = new Blend2D.Image(width, height, Blend2D.Format.PRGB32);
        blContext = new Blend2D.Context(blImage);
      }

      imageData = ctx?.createImageData(width, height);
    },
  };
}

export default { createBlend2DBackend };
