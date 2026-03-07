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
        log.gpu.warn?.("Blend2D: Failed to get 2D context");
        return false;
      }

      // Try to load Blend2D WASM (optional — falls back to Canvas2D)
      const loaded = await loadBlend2D();
      if (loaded) {
        try {
          blImage = new Blend2D.Image(canvas.width, canvas.height, Blend2D.Format.PRGB32);
          blContext = new Blend2D.Context(blImage);
          imageData = ctx.createImageData(canvas.width, canvas.height);
          log.gpu.success?.("Blend2D backend initialized (WASM)");
        } catch (err) {
          log.gpu.warn?.("Blend2D WASM init failed, using Canvas2D fallback:", err);
          Blend2D = null;
          blContext = null;
          blImage = null;
        }
      } else {
        log.gpu.info?.("Blend2D: WASM not available, using Canvas2D fallback");
      }

      isReady = true;
      return true;
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
        if (Blend2D && blContext && blImage) {
          // Blend2D WASM path
          blContext.begin(blImage);
          blContext.setFillStyle(
            Blend2D.Rgba32(clearColor[0], clearColor[1], clearColor[2], clearColor[3])
          );
          blContext.fillAll();

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

          blContext.end();

          const pixels = blImage.getData();
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
          ctx.fillText(`Blend2D${Blend2D ? " (WASM)" : " (fallback)"}`, 10, 20);
          ctx.fillText(`FPS: ${currentFps}`, 10, 35);
          ctx.fillText(`Frame: ${frameDelta.toFixed(2)}ms`, 10, 50);
          ctx.fillText(`Draw calls: ${drawCallCount}`, 10, 65);
        }
      } catch (err) {
        log.gpu.error?.("Blend2D render error:", err);
      }

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

    disable() {
      // 🛑 Hide canvas and clear state for clean return to CPU rendering
      if (canvas) {
        canvas.style.display = "none";
      }
      console.log("🛑 Blend2D backend disabled");
    },
  };
}

export default { createBlend2DBackend };
