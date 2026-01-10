// 🎨 ThorVG Backend
// High-performance vector graphics via WebAssembly

/* #region 📚 Notes
  ThorVG (https://thorvg.org) is a platform-independent vector graphics library.
  - Used by Lottie, Samsung Tizen, and others
  - Small WASM binary (~150KB)
  - Excellent anti-aliased rendering
  - Supports SVG, paths, gradients, effects
  
  WASM Build:
  - Official builds: https://github.com/nicholasflamy/nicholasflamy.github.io (thorvg.wasm)
  - Or build from source: https://github.com/thorvg/thorvg
  
  To use:
  1. Place thorvg.wasm and thorvg.js in lib/gpu/wasm/
  2. The backend will load them on init
#endregion */

import { createBaseRenderer } from "./renderer-interface.mjs";
import { log } from "../logs.mjs";

// ThorVG WASM module (loaded dynamically)
let ThorVG = null;
let tvgCanvas = null;
let tvgSwCanvas = null;

/**
 * Load the ThorVG WASM module
 * @returns {Promise<boolean>}
 */
async function loadThorVG() {
  if (ThorVG) return true;

  try {
    // Try to load ThorVG WASM module
    // The module should export a factory function
    const wasmPath = new URL("./wasm/thorvg.js", import.meta.url).href;
    
    // Dynamic import with error handling
    const module = await import(wasmPath).catch(() => null);
    if (!module) {
      log.gpu.warn?.("ThorVG WASM module not found at", wasmPath);
      return false;
    }

    // Initialize ThorVG
    ThorVG = await module.default?.();
    if (!ThorVG) {
      log.gpu.warn?.("Failed to initialize ThorVG module");
      return false;
    }

    log.gpu.success?.("ThorVG WASM loaded");
    return true;
  } catch (err) {
    log.gpu.error?.("Failed to load ThorVG:", err);
    return false;
  }
}

/**
 * Create a ThorVG renderer backend
 * @returns {Object} Renderer instance
 */
export function createThorVGBackend() {
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
        log.gpu.warn?.("ThorVG: Failed to get 2D context");
        return false;
      }

      // Try to load ThorVG WASM
      const loaded = await loadThorVG();
      if (!loaded) {
        log.gpu.warn?.("ThorVG: WASM not available, backend disabled");
        // Return false to trigger fallback to another backend
        return false;
      }

      try {
        // Create ThorVG canvas (software renderer)
        tvgSwCanvas = new ThorVG.SwCanvas();
        tvgSwCanvas.target(
          canvas.width,
          canvas.height,
          ThorVG.SwCanvas.Colorspace.ABGR8888
        );

        // Create image data buffer for blitting
        imageData = ctx.createImageData(canvas.width, canvas.height);

        isReady = true;
        log.gpu.success?.("ThorVG backend initialized");
        return true;
      } catch (err) {
        log.gpu.error?.("ThorVG initialization failed:", err);
        return false;
      }
    },

    destroy() {
      if (tvgSwCanvas) {
        tvgSwCanvas.delete();
        tvgSwCanvas = null;
      }
      tvgCanvas = null;
      ThorVG = null;
      isReady = false;
      base.destroy();
    },

    isReady() {
      return isReady;
    },

    getName() {
      return "thorvg";
    },

    handleCommand(command) {
      if (!isReady) {
        console.warn("ThorVG not ready");
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
      if (!isReady || !ThorVG) {
        commandQueue.length = 0;
        return;
      }

      try {
        // Clear canvas
        tvgSwCanvas.clear();

        // Set background
        const bgShape = new ThorVG.Shape();
        bgShape.appendRect(0, 0, canvas.width, canvas.height, 0, 0);
        bgShape.fill(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
        tvgSwCanvas.push(bgShape);

        // Draw all queued commands
        for (const cmd of commandQueue) {
          const shape = new ThorVG.Shape();

          if (cmd.type === "line") {
            shape.moveTo(cmd.x1, cmd.y1);
            shape.lineTo(cmd.x2, cmd.y2);
            shape.stroke(cmd.color[0], cmd.color[1], cmd.color[2], cmd.color[3] ?? 255);
            shape.strokeWidth(1);
          } else if (cmd.type === "box") {
            shape.appendRect(cmd.x, cmd.y, cmd.w, cmd.h, 0, 0);
            shape.fill(cmd.color[0], cmd.color[1], cmd.color[2], cmd.color[3] ?? 255);
          } else if (cmd.type === "circle") {
            shape.appendCircle(cmd.cx, cmd.cy, cmd.r, cmd.r);
            shape.fill(cmd.color[0], cmd.color[1], cmd.color[2], cmd.color[3] ?? 255);
          }

          tvgSwCanvas.push(shape);
        }

        // Render to buffer
        tvgSwCanvas.draw();
        tvgSwCanvas.sync();

        // Get pixel data and blit to canvas
        const pixels = tvgSwCanvas.buffer();
        if (pixels && imageData) {
          imageData.data.set(new Uint8ClampedArray(pixels.buffer));
          ctx.putImageData(imageData, 0, 0);
        }
      } catch (err) {
        log.gpu.error?.("ThorVG render error:", err);
      }

      // Clear queue
      commandQueue.length = 0;
      base.endFrame();
    },

    resize(width, height) {
      if (!isReady) return;

      canvas.width = width;
      canvas.height = height;

      if (tvgSwCanvas && ThorVG) {
        tvgSwCanvas.target(width, height, ThorVG.SwCanvas.Colorspace.ABGR8888);
      }

      imageData = ctx?.createImageData(width, height);
    },
  };
}

export default { createThorVGBackend };
