// 🎨 WebGL2 Backend
// GPU-accelerated 2D rendering via WebGL2

/* #region 📚 Notes
  WebGL2 provides GPU acceleration with wide browser support (98%+).
  Good middle ground between Canvas2D (simple but slow) and WebGPU (fast but limited support).
  
  Pros:
  - Wide browser support
  - GPU accelerated
  - Shader support (GLSL)
  - Good for batching many primitives
  
  Cons:
  - More complex than Canvas2D
  - State machine API
  - Less modern than WebGPU
#endregion */

import { createBaseRenderer } from "./renderer-interface.mjs";
import { log } from "../logs.mjs";

/**
 * Create a WebGL2 renderer backend
 * @returns {Object} Renderer instance
 */
export function createWebGL2Backend() {
  const base = createBaseRenderer();
  let gl = null;
  let canvas = null;
  let lineProgram = null;
  let lineVAO = null;
  let lineVBO = null;
  let clearColor = [0, 0, 0, 1];
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

  // Shader sources
  const lineVertexShader = `#version 300 es
    in vec2 a_position;
    uniform vec2 u_resolution;
    
    void main() {
      // Convert from pixels to clip space
      vec2 clipSpace = (a_position / u_resolution) * 2.0 - 1.0;
      gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
    }
  `;

  const lineFragmentShader = `#version 300 es
    precision highp float;
    uniform vec4 u_color;
    out vec4 fragColor;
    
    void main() {
      fragColor = u_color;
    }
  `;

  function compileShader(source, type) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);

    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      log.gpu.error?.("Shader compile error:", gl.getShaderInfoLog(shader));
      gl.deleteShader(shader);
      return null;
    }
    return shader;
  }

  function createProgram(vertexSource, fragmentSource) {
    const vertexShader = compileShader(vertexSource, gl.VERTEX_SHADER);
    const fragmentShader = compileShader(fragmentSource, gl.FRAGMENT_SHADER);

    if (!vertexShader || !fragmentShader) return null;

    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      log.gpu.error?.("Program link error:", gl.getProgramInfoLog(program));
      gl.deleteProgram(program);
      return null;
    }

    // Clean up shaders (they're linked to program now)
    gl.deleteShader(vertexShader);
    gl.deleteShader(fragmentShader);

    return program;
  }

  return {
    ...base,

    async init(targetCanvas) {
      canvas = targetCanvas;
      gl = canvas.getContext("webgl2", {
        alpha: false,
        antialias: true,
        depth: false,
        stencil: false,
        premultipliedAlpha: false,
        preserveDrawingBuffer: false,
        powerPreference: "high-performance",
      });

      if (!gl) {
        log.gpu.warn?.("WebGL2: Not supported");
        return false;
      }

      try {
        // Create line shader program
        lineProgram = createProgram(lineVertexShader, lineFragmentShader);
        if (!lineProgram) {
          log.gpu.error?.("WebGL2: Failed to create line program");
          return false;
        }

        // Create VAO and VBO for lines
        lineVAO = gl.createVertexArray();
        gl.bindVertexArray(lineVAO);

        lineVBO = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, lineVBO);

        // Setup vertex attribute
        const positionLoc = gl.getAttribLocation(lineProgram, "a_position");
        gl.enableVertexAttribArray(positionLoc);
        gl.vertexAttribPointer(positionLoc, 2, gl.FLOAT, false, 0, 0);

        gl.bindVertexArray(null);

        // Enable blending
        gl.enable(gl.BLEND);
        gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

        isReady = true;
        log.gpu.success?.("WebGL2 backend initialized");
        return true;
      } catch (err) {
        log.gpu.error?.("WebGL2 initialization failed:", err);
        return false;
      }
    },

    destroy() {
      if (gl) {
        if (lineProgram) gl.deleteProgram(lineProgram);
        if (lineVBO) gl.deleteBuffer(lineVBO);
        if (lineVAO) gl.deleteVertexArray(lineVAO);
      }
      gl = null;
      canvas = null;
      isReady = false;
      base.destroy();
    },

    isReady() {
      return isReady;
    },

    getName() {
      return "webgl2";
    },

    handleCommand(command) {
      if (!isReady) {
        console.warn("WebGL2 not ready");
        return;
      }

      switch (command.type) {
        case "clear":
          const [r, g, b, a = 255] = command.color || [0, 0, 0, 255];
          clearColor = [r / 255, g / 255, b / 255, a / 255];
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
        case "frame-end":
          this.endFrame();
          break;
        case "perf-overlay":
          perfOverlayEnabled = command.enabled ?? !perfOverlayEnabled;
          log.gpu.debug?.(`WebGL2 perf overlay: ${perfOverlayEnabled ? "ON" : "OFF"}`);
          break;
        default:
          base.handleCommand(command);
      }
    },

    clear(r, g, b, a = 255) {
      clearColor = [r / 255, g / 255, b / 255, a / 255];
    },

    line(x1, y1, x2, y2, color) {
      commandQueue.push({ type: "line", x1, y1, x2, y2, color });
    },

    endFrame() {
      if (!isReady || !gl) {
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

      // Set viewport
      gl.viewport(0, 0, canvas.width, canvas.height);

      // Clear
      gl.clearColor(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
      gl.clear(gl.COLOR_BUFFER_BIT);

      // Draw all lines in a batch if possible
      if (commandQueue.length > 0) {
        gl.useProgram(lineProgram);
        gl.bindVertexArray(lineVAO);

        // Set resolution uniform
        const resolutionLoc = gl.getUniformLocation(lineProgram, "u_resolution");
        gl.uniform2f(resolutionLoc, canvas.width, canvas.height);

        const colorLoc = gl.getUniformLocation(lineProgram, "u_color");

        // For now, draw each line separately (could batch by color)
        for (const cmd of commandQueue) {
          if (cmd.type === "line") {
            // Upload vertices
            const vertices = new Float32Array([cmd.x1, cmd.y1, cmd.x2, cmd.y2]);
            gl.bindBuffer(gl.ARRAY_BUFFER, lineVBO);
            gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.DYNAMIC_DRAW);

            // Set color
            const [r, g, b, a = 255] = cmd.color;
            gl.uniform4f(colorLoc, r / 255, g / 255, b / 255, a / 255);

            // Draw
            gl.drawArrays(gl.LINES, 0, 2);
          }
        }

        gl.bindVertexArray(null);
      }

      // Perf overlay (using Canvas2D overlay for simplicity)
      // In a full implementation, we'd render this with WebGL too
      if (perfOverlayEnabled) {
        // Could use a separate 2D canvas overlay for text
      }

      // Clear queue
      commandQueue.length = 0;
      base.endFrame();
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
        backend: "webgl2",
      };
    },

    setPerfOverlay(enabled) {
      perfOverlayEnabled = enabled;
    },
  };
}

export default { createWebGL2Backend };
