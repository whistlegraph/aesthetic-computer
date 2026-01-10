// 🎨 GPU Renderer Interface
// Standard interface that all renderer backends must implement

/* #region 📚 Notes
  All renderers (WebGPU, WebGL2, ThorVG, Blend2D, Canvas2D) implement this interface.
  The bios.mjs dispatches commands to whichever renderer is active.
#endregion */

/**
 * @typedef {Object} RendererStats
 * @property {number} fps - Frames per second
 * @property {number} frameDelta - Time since last frame (ms)
 * @property {number} drawCalls - Draw calls this frame
 * @property {string} backend - Backend name
 */

/**
 * @typedef {Object} Renderer
 * @property {function(HTMLCanvasElement): Promise<boolean>} init - Initialize renderer
 * @property {function(): void} destroy - Cleanup resources
 * @property {function(Object): void} handleCommand - Process a render command
 * @property {function(number, number, number, number): void} clear - Clear canvas
 * @property {function(number, number, number, number, number[]): void} line - Draw line
 * @property {function(number, number, number, number, number[]): void} box - Draw rectangle
 * @property {function(number, number, number, number[]): void} circle - Draw circle
 * @property {function(): void} beginFrame - Start frame (optional)
 * @property {function(): void} endFrame - Flush and present
 * @property {function(number, number): void} resize - Handle resize
 * @property {function(): string} getName - Get backend name
 * @property {function(): RendererStats} getStats - Get performance stats
 * @property {function(): boolean} isReady - Check if initialized
 */

/**
 * Creates a base renderer with default implementations.
 * Backends can extend this with their specific implementations.
 * @returns {Renderer}
 */
export function createBaseRenderer() {
  let ready = false;
  let canvas = null;
  let frameCount = 0;
  let lastFrameTime = 0;
  let frameDelta = 0;
  let currentFps = 0;
  let drawCallsThisFrame = 0;
  let frameTimestamps = [];

  return {
    // Lifecycle
    async init(targetCanvas) {
      canvas = targetCanvas;
      ready = true;
      return true;
    },

    destroy() {
      ready = false;
      canvas = null;
    },

    isReady() {
      return ready;
    },

    // Command dispatcher
    handleCommand(command) {
      switch (command.type) {
        case "clear":
          this.clear(...(command.color || [0, 0, 0, 255]));
          break;
        case "line":
          this.line(command.x1, command.y1, command.x2, command.y2, command.color);
          break;
        case "box":
          this.box(command.x, command.y, command.w, command.h, command.color);
          break;
        case "circle":
          this.circle(command.cx, command.cy, command.r, command.color);
          break;
        case "frame-end":
          this.endFrame();
          break;
        case "perf-overlay":
          this.setPerfOverlay?.(command.enabled);
          break;
        default:
          console.warn(`Unknown render command: ${command.type}`);
      }
    },

    // Drawing operations (to be overridden)
    clear(r, g, b, a) {
      console.warn("clear() not implemented");
    },

    line(x1, y1, x2, y2, color) {
      console.warn("line() not implemented");
    },

    box(x, y, w, h, color) {
      console.warn("box() not implemented");
    },

    circle(cx, cy, r, color) {
      console.warn("circle() not implemented");
    },

    // Frame control
    beginFrame() {
      drawCallsThisFrame = 0;
    },

    endFrame() {
      // Update timing stats
      const now = performance.now();
      frameDelta = lastFrameTime ? now - lastFrameTime : 16.67;
      lastFrameTime = now;
      frameCount++;

      // Rolling FPS calculation
      frameTimestamps.push(now);
      while (frameTimestamps.length > 0 && frameTimestamps[0] < now - 1000) {
        frameTimestamps.shift();
      }
      currentFps = frameTimestamps.length;
    },

    resize(width, height) {
      if (canvas) {
        canvas.width = width;
        canvas.height = height;
      }
    },

    // Info
    getName() {
      return "base";
    },

    getStats() {
      return {
        fps: currentFps,
        frameDelta,
        frameCount,
        drawCalls: drawCallsThisFrame,
        backend: this.getName(),
      };
    },

    // Helpers
    getCanvas() {
      return canvas;
    },

    incrementDrawCalls() {
      drawCallsThisFrame++;
    },
  };
}

export default { createBaseRenderer };
