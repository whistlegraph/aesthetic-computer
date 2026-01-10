// 🎨 Vello Backend
// GPU compute-based 2D vector graphics via Rust/WASM

/* #region 📚 Notes
  Vello (https://vello.dev) is a GPU compute-centric 2D renderer by Linebender.
  - Uses WebGPU compute shaders for parallel rendering
  - Same team behind Xilem, Druid
  - Very fast: 177fps for 30k paths on M1 Max
  - PostScript-inspired API (fill, stroke, paths)
  
  REQUIREMENTS:
  - WebGPU support (Chrome, experimental in Firefox/Safari)
  - Custom WASM bindings (must build from Rust source)
  
  Build Instructions:
  1. Clone: https://github.com/nicholasflamy/nicholasflamy.github.io/nicholasflamy.github.io
  2. Create wasm-bindgen wrapper (see example in this file)
  3. Build: wasm-pack build --target web
  4. Copy pkg/vello_bindings.js and vello_bindings_bg.wasm to lib/gpu/wasm/
  
  Example Rust wrapper (save as src/lib.rs):
  ```rust
  use wasm_bindgen::prelude::*;
  use vello::{Scene, Renderer, RendererOptions, RenderParams};
  use vello::kurbo::{Line, Circle, Rect, Affine};
  use vello::peniko::{Color, Fill, Stroke};
  
  #[wasm_bindgen]
  pub struct VelloCanvas {
      scene: Scene,
      clear_color: Color,
  }
  
  #[wasm_bindgen]
  impl VelloCanvas {
      #[wasm_bindgen(constructor)]
      pub fn new() -> Self {
          Self {
              scene: Scene::new(),
              clear_color: Color::BLACK,
          }
      }
      
      pub fn clear(&mut self, r: u8, g: u8, b: u8, a: u8) {
          self.scene.reset();
          self.clear_color = Color::rgba8(r, g, b, a);
      }
      
      pub fn line(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, r: u8, g: u8, b: u8, a: u8) {
          self.scene.stroke(
              &Stroke::new(1.0),
              Affine::IDENTITY,
              Color::rgba8(r, g, b, a),
              None,
              &Line::new((x1, y1), (x2, y2)),
          );
      }
      
      pub fn circle(&mut self, cx: f64, cy: f64, radius: f64, r: u8, g: u8, b: u8, a: u8) {
          self.scene.fill(
              Fill::NonZero,
              Affine::IDENTITY,
              Color::rgba8(r, g, b, a),
              None,
              &Circle::new((cx, cy), radius),
          );
      }
      
      pub fn rect(&mut self, x: f64, y: f64, w: f64, h: f64, r: u8, g: u8, b: u8, a: u8) {
          self.scene.fill(
              Fill::NonZero,
              Affine::IDENTITY,
              Color::rgba8(r, g, b, a),
              None,
              &Rect::new(x, y, x + w, y + h),
          );
      }
      
      pub fn get_scene(&self) -> &Scene { &self.scene }
      pub fn get_clear_color(&self) -> Color { self.clear_color }
  }
  ```
#endregion */

import { createBaseRenderer } from "./renderer-interface.mjs";
import { log } from "../logs.mjs";

// Vello WASM module (loaded dynamically)
let Vello = null;

/**
 * Load the Vello WASM module
 * @returns {Promise<boolean>}
 */
async function loadVello() {
  if (Vello) return true;

  try {
    // Try to load Vello WASM bindings (built with wasm-pack)
    const wasmJsPath = new URL("./wasm/vello_wasm.js", import.meta.url).href;
    const wasmBinaryPath = new URL("./wasm/vello_wasm_bg.wasm", import.meta.url).href;

    // Dynamic import with error handling
    const module = await import(wasmJsPath).catch((err) => {
      log.gpu.warn?.("Vello WASM JS import failed:", err);
      return null;
    });
    if (!module) {
      log.gpu.warn?.("Vello WASM module not found at", wasmJsPath);
      return false;
    }

    // Initialize WASM - wasm-pack generates an init function
    if (module.default) {
      await module.default(wasmBinaryPath);
    }
    Vello = module;

    log.gpu.success?.("Vello WASM loaded successfully");
    return true;
  } catch (err) {
    log.gpu.error?.("Failed to load Vello:", err);
    return false;
  }
}

/**
 * Create a Vello renderer backend
 * 
 * NOTE: Current implementation uses VelloScene for scene building
 * but renders via Canvas2D fallback until full WebGPU compute
 * shader integration is implemented. This allows testing the
 * Vello scene API while maintaining visual output.
 * 
 * @returns {Object} Renderer instance
 */
export function createVelloBackend() {
  const base = createBaseRenderer();
  let ctx = null; // Canvas2D context for fallback rendering
  let canvas = null;
  let velloScene = null; // VelloScene from WASM
  let clearColor = [0, 0, 0, 255];
  let isReady = false;
  let perfOverlayEnabled = false;
  let wasmLoaded = false;

  // Command queue for batching
  const commandQueue = [];
  let drawCallCount = 0;

  // Perf tracking
  let frameTimestamps = [];
  let currentFps = 0;
  let lastFrameTime = 0;
  let frameDelta = 0;

  return {
    ...base,

    async init(targetCanvas) {
      canvas = targetCanvas;

      // Get Canvas2D context for fallback rendering
      ctx = canvas.getContext("2d", { 
        alpha: false,
        desynchronized: true 
      });
      
      if (!ctx) {
        log.gpu.error?.("Vello: Failed to get Canvas2D context for fallback");
        return false;
      }

      // Try to load Vello WASM (optional - enhances scene building)
      wasmLoaded = await loadVello();
      if (wasmLoaded) {
        try {
          // Create VelloScene with canvas dimensions
          velloScene = new Vello.VelloScene(canvas.width, canvas.height);
          log.gpu.success?.("Vello: Scene builder initialized from WASM");
        } catch (err) {
          log.gpu.warn?.("Vello: WASM scene creation failed:", err);
          wasmLoaded = false;
        }
      } else {
        log.gpu.info?.("Vello: Running in fallback mode (no WASM)");
      }

      isReady = true;
      log.gpu.success?.("Vello backend initialized" + (wasmLoaded ? " (with WASM)" : " (Canvas2D fallback)"));
      return true;
    },

    destroy() {
      if (velloScene) {
        velloScene.free?.();
        velloScene = null;
      }
      ctx = null;
      Vello = null;
      wasmLoaded = false;
      isReady = false;
      base.destroy();
    },

    isReady() {
      return isReady;
    },

    getName() {
      return "vello";
    },

    handleCommand(command) {
      if (!isReady) {
        console.warn("Vello not ready");
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
            color: command.color,
          });
          break;
        case "box":
          commandQueue.push({
            type: "box",
            x: command.x,
            y: command.y,
            w: command.w,
            h: command.h,
            color: command.color,
          });
          break;
        case "circle":
          commandQueue.push({
            type: "circle",
            cx: command.cx,
            cy: command.cy,
            r: command.r,
            color: command.color,
          });
          break;
        case "frame-end":
          this.endFrame();
          break;
        case "perf-overlay":
          perfOverlayEnabled = command.enabled ?? !perfOverlayEnabled;
          log.gpu.debug?.(`Vello perf overlay: ${perfOverlayEnabled ? "ON" : "OFF"}`);
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
    },

    box(x, y, w, h, color) {
      commandQueue.push({ type: "box", x, y, w, h, color });
    },

    circle(cx, cy, r, color) {
      commandQueue.push({ type: "circle", cx, cy, r, color });
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

      // Rolling FPS calculation
      frameTimestamps.push(now);
      while (frameTimestamps.length > 0 && frameTimestamps[0] < now - 1000) {
        frameTimestamps.shift();
      }
      currentFps = frameTimestamps.length;

      // Track draw calls
      drawCallCount = commandQueue.length;

      try {
        // Clear canvas with background color
        const [r, g, b, a] = clearColor;
        ctx.fillStyle = `rgba(${r}, ${g}, ${b}, ${a / 255})`;
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        // If WASM is loaded, also update VelloScene (for future use)
        if (wasmLoaded && velloScene) {
          velloScene.clear(r, g, b, a);
        }

        // Draw all queued commands via Canvas2D
        for (const cmd of commandQueue) {
          const [cr, cg, cb, ca = 255] = cmd.color || [255, 255, 255, 255];
          const color = `rgba(${cr}, ${cg}, ${cb}, ${ca / 255})`;

          if (cmd.type === "line") {
            // Canvas2D line
            ctx.strokeStyle = color;
            ctx.lineWidth = cmd.strokeWidth || 1;
            ctx.beginPath();
            ctx.moveTo(cmd.x1, cmd.y1);
            ctx.lineTo(cmd.x2, cmd.y2);
            ctx.stroke();
            
            // Mirror to VelloScene
            if (wasmLoaded && velloScene) {
              velloScene.line(cmd.x1, cmd.y1, cmd.x2, cmd.y2, cr, cg, cb, ca, cmd.strokeWidth || 1);
            }
          } else if (cmd.type === "box") {
            // Canvas2D rectangle
            ctx.fillStyle = color;
            ctx.fillRect(cmd.x, cmd.y, cmd.w, cmd.h);
            
            // Mirror to VelloScene
            if (wasmLoaded && velloScene) {
              velloScene.rect(cmd.x, cmd.y, cmd.w, cmd.h, cr, cg, cb, ca);
            }
          } else if (cmd.type === "circle") {
            // Canvas2D circle
            ctx.fillStyle = color;
            ctx.beginPath();
            ctx.arc(cmd.cx, cmd.cy, cmd.r, 0, Math.PI * 2);
            ctx.fill();
            
            // Mirror to VelloScene
            if (wasmLoaded && velloScene) {
              velloScene.circle(cmd.cx, cmd.cy, cmd.r, cr, cg, cb, ca);
            }
          }
        }

        // Draw perf overlay if enabled
        if (perfOverlayEnabled) {
          this.drawPerfOverlay();
        }
      } catch (err) {
        log.gpu.error?.("Vello render error:", err);
      }

      // Clear queue
      commandQueue.length = 0;
      base.endFrame();
    },

    drawPerfOverlay() {
      const stats = this.getStats();
      ctx.fillStyle = "rgba(0, 0, 0, 0.7)";
      ctx.fillRect(5, 5, 160, 70);
      ctx.fillStyle = "#0f0";
      ctx.font = "12px monospace";
      ctx.fillText(`Vello${wasmLoaded ? " (WASM)" : " (fallback)"}`, 10, 20);
      ctx.fillText(`FPS: ${stats.fps}`, 10, 35);
      ctx.fillText(`Frame: ${stats.frameDelta.toFixed(2)}ms`, 10, 50);
      ctx.fillText(`Draw calls: ${stats.drawCalls}`, 10, 65);
    },

    resize(width, height) {
      if (!isReady) return;

      canvas.width = width;
      canvas.height = height;

      // Resize VelloScene if WASM is loaded
      if (wasmLoaded && velloScene) {
        velloScene.resize(width, height);
      }
    },

    getStats() {
      return {
        fps: currentFps,
        frameDelta,
        drawCalls: drawCallCount,
        backend: "vello",
        wasmLoaded,
      };
    },

    setPerfOverlay(enabled) {
      perfOverlayEnabled = enabled;
    },
  };
}

export default { createVelloBackend };
