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
  let perfOverlayEnabled = false;

  // Perf tracking
  let frameTimestamps = [];
  let currentFps = 0;
  let lastFrameTime = 0;
  let frameDelta = 0;
  let drawCallCount = 0;

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

      // Try to load ThorVG WASM (optional — falls back to Canvas2D)
      const loaded = await loadThorVG();
      if (loaded) {
        try {
          tvgSwCanvas = new ThorVG.SwCanvas();
          tvgSwCanvas.target(
            canvas.width,
            canvas.height,
            ThorVG.SwCanvas.Colorspace.ABGR8888
          );
          imageData = ctx.createImageData(canvas.width, canvas.height);
          log.gpu.success?.("ThorVG backend initialized (WASM)");
        } catch (err) {
          log.gpu.warn?.("ThorVG WASM init failed, using Canvas2D fallback:", err);
          ThorVG = null;
          tvgSwCanvas = null;
        }
      } else {
        log.gpu.info?.("ThorVG: WASM not available, using Canvas2D fallback");
      }

      isReady = true;
      return true;
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
            x1: command.x1, y1: command.y1,
            x2: command.x2, y2: command.y2,
            color: command.color || currentColor,
          });
          break;
        case "box":
          commandQueue.push({
            type: "box",
            x: command.x, y: command.y,
            w: command.w, h: command.h,
            color: command.color || currentColor,
          });
          break;
        case "circle":
          commandQueue.push({
            type: "circle",
            cx: command.cx, cy: command.cy, r: command.r,
            color: command.color || currentColor,
          });
          break;
        case "frame-end":
          this.endFrame();
          break;
        case "perf-overlay":
          perfOverlayEnabled = command.enabled ?? !perfOverlayEnabled;
          break;
        case "disable":
          this.disable();
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
      if (!isReady || !ctx) {
        commandQueue.length = 0;
        return;
      }

      // Track frame timing
      const now = performance.now();
      frameDelta = lastFrameTime ? now - lastFrameTime : 16.67;
      lastFrameTime = now;
      frameTimestamps.push(now);
      while (frameTimestamps.length > 0 && frameTimestamps[0] < now - 1000) {
        frameTimestamps.shift();
      }
      currentFps = frameTimestamps.length;
      drawCallCount = commandQueue.length;

      try {
        if (ThorVG && tvgSwCanvas) {
          // ThorVG WASM path
          tvgSwCanvas.clear();

          const bgShape = new ThorVG.Shape();
          bgShape.appendRect(0, 0, canvas.width, canvas.height, 0, 0);
          bgShape.fill(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
          tvgSwCanvas.push(bgShape);

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

          tvgSwCanvas.draw();
          tvgSwCanvas.sync();

          const pixels = tvgSwCanvas.buffer();
          if (pixels && imageData) {
            imageData.data.set(new Uint8ClampedArray(pixels.buffer));
            ctx.putImageData(imageData, 0, 0);
          }
        } else {
          // Canvas2D fallback path
          const [r, g, b, a] = clearColor;
          ctx.fillStyle = `rgba(${r}, ${g}, ${b}, ${a / 255})`;
          ctx.fillRect(0, 0, canvas.width, canvas.height);

          for (const cmd of commandQueue) {
            const [cr, cg, cb, ca = 255] = cmd.color || [255, 255, 255, 255];
            const color = `rgba(${cr}, ${cg}, ${cb}, ${ca / 255})`;

            if (cmd.type === "line") {
              ctx.strokeStyle = color;
              ctx.lineWidth = 1;
              ctx.beginPath();
              ctx.moveTo(cmd.x1, cmd.y1);
              ctx.lineTo(cmd.x2, cmd.y2);
              ctx.stroke();
            } else if (cmd.type === "box") {
              ctx.fillStyle = color;
              ctx.fillRect(cmd.x, cmd.y, cmd.w, cmd.h);
            } else if (cmd.type === "circle") {
              ctx.fillStyle = color;
              ctx.beginPath();
              ctx.arc(cmd.cx, cmd.cy, cmd.r, 0, Math.PI * 2);
              ctx.fill();
            }
          }
        }

        if (perfOverlayEnabled) {
          ctx.fillStyle = "rgba(0, 0, 0, 0.7)";
          ctx.fillRect(5, 5, 170, 70);
          ctx.fillStyle = "#0f0";
          ctx.font = "12px monospace";
          ctx.fillText(`ThorVG${ThorVG ? " (WASM)" : " (fallback)"}`, 10, 20);
          ctx.fillText(`FPS: ${currentFps}`, 10, 35);
          ctx.fillText(`Frame: ${frameDelta.toFixed(2)}ms`, 10, 50);
          ctx.fillText(`Draw calls: ${drawCallCount}`, 10, 65);
        }
      } catch (err) {
        log.gpu.error?.("ThorVG render error:", err);
      }

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

    disable() {
      // 🛑 Hide canvas and clear state for clean return to CPU rendering
      if (canvas) {
        canvas.style.display = "none";
      }
      console.log("🛑 ThorVG backend disabled");
    },
  };
}

export default { createThorVGBackend };
