// 🎨 Canvas2D Backend
// Universal fallback using native Canvas 2D API

/* #region 📚 Notes
  Canvas2D is available in all browsers and provides a reliable fallback
  when GPU-accelerated backends (WebGPU, WebGL2) aren't available.
  
  Pros:
  - Universal support
  - Simple API
  - No initialization needed
  - Hardware accelerated on most browsers (via GPU compositing)
  
  Cons:
  - Slower for very many primitives
  - No custom shaders
  - Less control over rendering pipeline
#endregion */

import { createBaseRenderer } from "./renderer-interface.mjs";
import { log } from "../logs.mjs";

/**
 * Create a Canvas2D renderer backend
 * @returns {Object} Renderer instance
 */
export function createCanvas2DBackend() {
  const base = createBaseRenderer();
  let ctx = null;
  let canvas = null;
  let clearColor = [0, 0, 0, 255];
  let isReady = false;
  let perfOverlayEnabled = false;

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
      ctx = canvas.getContext("2d", {
        alpha: false, // Opaque for better performance
        desynchronized: true, // Lower latency if supported
      });

      if (!ctx) {
        log.gpu.warn?.("Canvas2D: Failed to get 2D context");
        return false;
      }

      isReady = true;
      log.gpu.success?.("Canvas2D backend initialized");
      return true;
    },

    destroy() {
      ctx = null;
      canvas = null;
      isReady = false;
      base.destroy();
    },

    isReady() {
      return isReady;
    },

    getName() {
      return "canvas2d";
    },

    handleCommand(command) {
      if (!isReady) {
        console.warn("Canvas2D not ready");
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
          log.gpu.debug?.(`Canvas2D perf overlay: ${perfOverlayEnabled ? "ON" : "OFF"}`);
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

      // Clear with background color
      const [r, g, b, a] = clearColor;
      ctx.fillStyle = `rgba(${r},${g},${b},${a / 255})`;
      ctx.fillRect(0, 0, canvas.width, canvas.height);

      // Draw all queued commands
      for (const cmd of commandQueue) {
        if (cmd.type === "line") {
          const [cr, cg, cb, ca = 255] = cmd.color;
          ctx.strokeStyle = `rgba(${cr},${cg},${cb},${ca / 255})`;
          ctx.lineWidth = 1;
          ctx.beginPath();
          ctx.moveTo(cmd.x1, cmd.y1);
          ctx.lineTo(cmd.x2, cmd.y2);
          ctx.stroke();
        } else if (cmd.type === "box") {
          const [cr, cg, cb, ca = 255] = cmd.color;
          ctx.fillStyle = `rgba(${cr},${cg},${cb},${ca / 255})`;
          ctx.fillRect(cmd.x, cmd.y, cmd.w, cmd.h);
        } else if (cmd.type === "circle") {
          const [cr, cg, cb, ca = 255] = cmd.color;
          ctx.fillStyle = `rgba(${cr},${cg},${cb},${ca / 255})`;
          ctx.beginPath();
          ctx.arc(cmd.cx, cmd.cy, cmd.r, 0, Math.PI * 2);
          ctx.fill();
        }
      }

      // Draw perf overlay if enabled
      if (perfOverlayEnabled) {
        this.drawPerfOverlay();
      }

      // Clear queue
      commandQueue.length = 0;
      base.endFrame();
    },

    drawPerfOverlay() {
      const padding = 4;
      const lineHeight = 12;
      let y = padding + lineHeight;

      // Background
      ctx.fillStyle = "rgba(0, 0, 0, 0.75)";
      ctx.fillRect(0, 0, 70, 50);

      // Text style
      ctx.font = "10px monospace";

      // FPS
      ctx.fillStyle = "rgb(128, 220, 128)";
      ctx.fillText("FPS", padding, y);
      ctx.fillStyle = "white";
      ctx.fillText(String(Math.round(currentFps)), padding + 26, y);

      y += lineHeight;

      // Frame time
      ctx.fillStyle = "rgb(128, 220, 128)";
      ctx.fillText("MS", padding, y);
      ctx.fillStyle = "white";
      ctx.fillText(frameDelta.toFixed(1), padding + 20, y);

      y += lineHeight;

      // Draw calls
      ctx.fillStyle = "rgb(128, 220, 128)";
      ctx.fillText("DC", padding, y);
      ctx.fillStyle = "white";
      ctx.fillText(String(drawCallCount), padding + 20, y);
    },

    resize(width, height) {
      if (!isReady) return;
      canvas.width = width;
      canvas.height = height;
    },

    getStats() {
      return {
        fps: currentFps,
        frameDelta,
        drawCalls: drawCallCount,
        backend: "canvas2d",
      };
    },

    setPerfOverlay(enabled) {
      perfOverlayEnabled = enabled;
    },
  };
}

export default { createCanvas2DBackend };
