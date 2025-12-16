// 🎨 WebGPU Renderer
// A 2D GPU-accelerated renderer for aesthetic.computer

/* #region 📚 Notes
  - Message-based architecture matching existing disk → bios pattern
  - Handles basic 2D primitives: clear, line, circle, rect
  - Uses WebGPU for hardware acceleration
  - Falls back gracefully if WebGPU unavailable
  - Commands are batched per frame and executed together
#endregion */

import { log } from "./logs.mjs";

let device = null;
let context = null;
let canvasFormat = null;
let renderPipeline = null;
let linePipeline = null;
let isInitialized = false;
let canvas = null;

// Track configured canvas size to avoid unnecessary reconfiguration
let configuredWidth = 0;
let configuredHeight = 0;

// Frame state - track current frame's texture and clear color
let currentFrameTexture = null;
let currentFrameTextureView = null;
let frameClearColor = null; // If set, clear with this color at start of frame

// Command queue for batching
const commandQueue = [];

// Current drawing color
let currentColor = [255, 255, 255, 255];

// 📊 Performance overlay state
let perfOverlayEnabled = false;
let frameCount = 0;
let lastFpsUpdate = 0;
let currentFps = 0;
let frameTimestamps = []; // Rolling window for FPS calculation
let drawCallsThisFrame = 0;
let lastFrameTime = 0;
let frameDelta = 0;

// Helper to ensure context is configured for current canvas size
function ensureContextConfigured() {
  if (canvas.width !== configuredWidth || canvas.height !== configuredHeight) {
    if (canvas.width > 0 && canvas.height > 0) {
      try {
        context.configure({
          device,
          format: canvasFormat,
          alphaMode: "opaque",
        });
        configuredWidth = canvas.width;
        configuredHeight = canvas.height;
        log.gpu.debug(`Context configured for ${configuredWidth}x${configuredHeight}`);
      } catch (err) {
        log.gpu.error("Context configure failed:", err);
      }
    } else {
      log.gpu.verbose(`Canvas size is ${canvas.width}x${canvas.height}, skipping configure`);
    }
  }
}

// 🔧 Initialize WebGPU
async function init(targetCanvas) {
  if (isInitialized) return true;
  
  log.gpu.log("Initializing WebGPU renderer...");
  
  // Check for WebGPU support
  if (!navigator.gpu) {
    log.gpu.warn("WebGPU not supported in this browser");
    return false;
  }
  
  try {
    // Request adapter
    const adapter = await navigator.gpu.requestAdapter();
    if (!adapter) {
      log.gpu.warn("No WebGPU adapter found");
      return false;
    }
    
    // Request device
    device = await adapter.requestDevice();
    
    // Store canvas reference
    canvas = targetCanvas;
    
    // Configure canvas context
    context = canvas.getContext("webgpu");
    if (!context) {
      log.gpu.warn("Failed to get WebGPU context from canvas");
      return false;
    }
    
    canvasFormat = navigator.gpu.getPreferredCanvasFormat();
    
    // Initial configuration (may be 0x0, will reconfigure on first draw)
    if (canvas.width > 0 && canvas.height > 0) {
      context.configure({
        device,
        format: canvasFormat,
        alphaMode: "opaque", // Use opaque to fully replace what's behind
      });
      configuredWidth = canvas.width;
      configuredHeight = canvas.height;
    }
    
    // Create basic render pipeline for clear operations
    await createClearPipeline();
    
    // Create line rendering pipeline
    await createLinePipeline();
    
    isInitialized = true;
    log.gpu.success("WebGPU renderer initialized");
    return true;
  } catch (error) {
    log.gpu.error("Failed to initialize WebGPU:", error);
    return false;
  }
}

// 🎨 Create pipeline for clear operations
async function createClearPipeline() {
  // Simple shader that fills the screen with a color
  const shaderModule = device.createShaderModule({
    label: "Clear shader",
    code: `
      struct VertexOutput {
        @builtin(position) position: vec4f,
      }
      
      @vertex
      fn vertexMain(@builtin(vertex_index) vertexIndex: u32) -> VertexOutput {
        var output: VertexOutput;
        
        // Full-screen triangle
        var pos = array<vec2f, 3>(
          vec2f(-1.0, -1.0),
          vec2f(3.0, -1.0),
          vec2f(-1.0, 3.0)
        );
        
        output.position = vec4f(pos[vertexIndex], 0.0, 1.0);
        return output;
      }
      
      @group(0) @binding(0) var<uniform> color: vec4f;
      
      @fragment
      fn fragmentMain() -> @location(0) vec4f {
        return color;
      }
    `,
  });
  
  renderPipeline = device.createRenderPipeline({
    label: "Clear pipeline",
    layout: "auto",
    vertex: {
      module: shaderModule,
      entryPoint: "vertexMain",
    },
    fragment: {
      module: shaderModule,
      entryPoint: "fragmentMain",
      targets: [{
        format: canvasFormat,
        blend: {
          color: {
            srcFactor: "src-alpha",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
          alpha: {
            srcFactor: "one",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
        },
      }],
    },
  });
}

// 🎨 Create pipeline for line rendering
async function createLinePipeline() {
  const shaderModule = device.createShaderModule({
    label: "Line shader",
    code: `
      struct Uniforms {
        color: vec4f,
        resolution: vec2f,
        _padding: vec2f,  // Padding to meet 32-byte minimum
      }
      
      struct VertexInput {
        @location(0) position: vec2f,
      }
      
      struct VertexOutput {
        @builtin(position) position: vec4f,
      }
      
      @group(0) @binding(0) var<uniform> uniforms: Uniforms;
      
      @vertex
      fn vertexMain(input: VertexInput) -> VertexOutput {
        var output: VertexOutput;
        
        // Convert from pixel coordinates to clip space (-1 to 1)
        let clipX = (input.position.x / uniforms.resolution.x) * 2.0 - 1.0;
        let clipY = 1.0 - (input.position.y / uniforms.resolution.y) * 2.0;
        
        output.position = vec4f(clipX, clipY, 0.0, 1.0);
        return output;
      }
      
      @fragment
      fn fragmentMain() -> @location(0) vec4f {
        return uniforms.color;
      }
    `,
  });
  
  linePipeline = device.createRenderPipeline({
    label: "Line pipeline",
    layout: "auto",
    vertex: {
      module: shaderModule,
      entryPoint: "vertexMain",
      buffers: [{
        arrayStride: 8, // 2 floats * 4 bytes
        attributes: [{
          shaderLocation: 0,
          offset: 0,
          format: "float32x2",
        }],
      }],
    },
    fragment: {
      module: shaderModule,
      entryPoint: "fragmentMain",
      targets: [{
        format: canvasFormat,
        blend: {
          color: {
            srcFactor: "src-alpha",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
          alpha: {
            srcFactor: "one",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
        },
      }],
    },
    primitive: {
      topology: "line-list",
      stripIndexFormat: undefined,
    },
  });
}

// 🎨 Handle incoming commands from disk
function handleCommand(command) {
  if (!isInitialized) {
    console.warn("⚠️ WebGPU not initialized, ignoring command:", command);
    return;
  }
  
  switch (command.type) {
    case "clear":
      queueClear(command.color);
      break;
    case "line":
      queueLine(command.x1, command.y1, command.x2, command.y2, command.color);
      break;
    case "frame-end":
      flushFrame();
      break;
    case "perf-overlay":
      perfOverlayEnabled = command.enabled ?? !perfOverlayEnabled;
      console.log(`📊 WebGPU perf overlay: ${perfOverlayEnabled ? "ON" : "OFF"}`);
      break;
    default:
      console.warn("⚠️ Unknown WebGPU command:", command.type);
  }
}

// 🎨 Queue a clear command (will be executed at frame start)
function queueClear(color) {
  // Make canvas visible
  if (canvas.style.display === "none") {
    canvas.style.display = "block";
    console.log("🎨 WebGPU canvas now visible");
  }
  
  // Store clear color for this frame
  frameClearColor = [color[0] / 255, color[1] / 255, color[2] / 255, (color[3] ?? 255) / 255];
}

// 🎨 Queue a line command
function queueLine(x1, y1, x2, y2, color) {
  // Make canvas visible
  if (canvas.style.display === "none") {
    canvas.style.display = "block";
  }
  
  commandQueue.push({
    type: "line",
    x1, y1, x2, y2,
    color: [color[0] / 255, color[1] / 255, color[2] / 255, (color[3] ?? 255) / 255]
  });
}

// 🎨 Flush all queued commands for this frame
function flushFrame() {
  ensureContextConfigured();
  
  // Skip if canvas has no size
  if (canvas.width === 0 || canvas.height === 0) {
    commandQueue.length = 0;
    frameClearColor = null;
    return;
  }
  
  // 📊 Track frame timing
  const now = performance.now();
  frameDelta = lastFrameTime ? now - lastFrameTime : 16.67;
  lastFrameTime = now;
  frameCount++;
  
  // Update FPS calculation using rolling window
  frameTimestamps.push(now);
  // Keep only last second of timestamps
  while (frameTimestamps.length > 0 && frameTimestamps[0] < now - 1000) {
    frameTimestamps.shift();
  }
  currentFps = frameTimestamps.length;
  
  // Track draw calls for this frame
  drawCallsThisFrame = commandQueue.length;
  
  // Get texture for this frame (used by all operations)
  const texture = context.getCurrentTexture();
  const textureView = texture.createView();
  
  // Create command encoder for all frame operations
  const encoder = device.createCommandEncoder();
  
  // Determine load operation - clear if we have a clear color, otherwise load
  const loadOp = frameClearColor ? "clear" : "load";
  const clearValue = frameClearColor 
    ? { r: frameClearColor[0], g: frameClearColor[1], b: frameClearColor[2], a: frameClearColor[3] }
    : { r: 0, g: 0, b: 0, a: 1 };
  
  // Start render pass
  const pass = encoder.beginRenderPass({
    colorAttachments: [{
      view: textureView,
      loadOp,
      storeOp: "store",
      clearValue,
    }],
  });
  
  // Execute all queued draw commands
  for (const cmd of commandQueue) {
    if (cmd.type === "line") {
      drawLineInPass(pass, cmd.x1, cmd.y1, cmd.x2, cmd.y2, cmd.color);
    }
  }
  
  // 📊 Render performance overlay if enabled
  if (perfOverlayEnabled) {
    drawPerfOverlay(pass);
  }
  
  pass.end();
  
  // Submit all commands
  device.queue.submit([encoder.finish()]);
  
  // Clear queue for next frame
  commandQueue.length = 0;
  frameClearColor = null;
}

// 🎨 Draw a line within an existing render pass
function drawLineInPass(pass, x1, y1, x2, y2, color) {
  // Create vertex buffer with line endpoints
  const vertices = new Float32Array([x1, y1, x2, y2]);
  
  const vertexBuffer = device.createBuffer({
    size: vertices.byteLength,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(vertexBuffer, 0, vertices);
  
  // Create uniform buffer for color and resolution
  const uniformData = new Float32Array([
    color[0], color[1], color[2], color[3],  // color (16 bytes)
    canvas.width, canvas.height,              // resolution (8 bytes)
    0, 0,                                     // padding (8 bytes)
  ]);
  
  const uniformBuffer = device.createBuffer({
    size: 32,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(uniformBuffer, 0, uniformData);
  
  // Create bind group
  const bindGroup = device.createBindGroup({
    layout: linePipeline.getBindGroupLayout(0),
    entries: [{
      binding: 0,
      resource: { buffer: uniformBuffer },
    }],
  });
  
  // Draw
  pass.setPipeline(linePipeline);
  pass.setVertexBuffer(0, vertexBuffer);
  pass.setBindGroup(0, bindGroup);
  pass.draw(2);
  
  // Note: buffers will be garbage collected, but for production we should pool them
}

// 📊 Draw performance overlay using simple line-based digits
function drawPerfOverlay(pass) {
  const padding = 4;
  const charWidth = 6;
  const charHeight = 10;
  const lineSpacing = 2;
  
  // Position in top-left corner
  let x = padding;
  let y = padding;
  
  // Background box (using lines to draw filled rect)
  const bgColor = [0, 0, 0, 0.75];
  const boxWidth = 62;
  const boxHeight = 36;
  
  // Draw background as horizontal lines (simple fill)
  for (let i = 0; i < boxHeight; i++) {
    drawLineInPass(pass, x - 2, y + i - 2, x + boxWidth, y + i - 2, bgColor);
  }
  
  // Text color (bright green for classic perf overlay look)
  const textColor = [0.3, 1.0, 0.3, 1.0];
  const labelColor = [0.5, 0.85, 0.5, 1.0];
  const valueColor = [1.0, 1.0, 1.0, 1.0];
  
  // Draw "FPS" label and value
  drawBitmapText(pass, "FPS", x, y, labelColor);
  const fpsStr = String(Math.round(currentFps));
  drawBitmapText(pass, fpsStr, x + 24, y, valueColor);
  
  // Move to next line
  y += charHeight + lineSpacing;
  
  // Draw "MS" and frame time
  drawBitmapText(pass, "MS", x, y, labelColor);
  const msStr = frameDelta.toFixed(1);
  drawBitmapText(pass, msStr, x + 18, y, valueColor);
  
  // Move to next line
  y += charHeight + lineSpacing;
  
  // Draw "DC" (draw calls)
  drawBitmapText(pass, "DC", x, y, labelColor);
  const dcStr = String(drawCallsThisFrame);
  drawBitmapText(pass, dcStr, x + 18, y, valueColor);
}

// 📊 Embedded bitmap font data (5x7 pixel glyphs, stored as line segments)
// Each glyph is an array of [x1,y1,x2,y2] line segments
const BITMAP_FONT = {
  "0": [[1,0,3,0],[0,1,0,6],[4,1,4,6],[1,7,3,7],[0,1,1,0],[3,0,4,1],[0,6,1,7],[3,7,4,6]],
  "1": [[2,0,2,7],[1,1,2,0],[1,7,3,7]],
  "2": [[0,0,3,0],[3,0,4,1],[4,1,4,3],[4,3,3,4],[3,4,1,4],[1,4,0,5],[0,5,0,7],[0,7,4,7]],
  "3": [[0,0,3,0],[3,0,4,1],[4,1,4,3],[3,3,4,3],[4,3,4,6],[3,7,4,6],[0,7,3,7],[1,3,3,3]],
  "4": [[0,0,0,3],[0,3,4,3],[4,0,4,7]],
  "5": [[0,0,4,0],[0,0,0,3],[0,3,3,3],[3,3,4,4],[4,4,4,6],[3,7,4,6],[0,7,3,7]],
  "6": [[1,0,3,0],[0,1,0,6],[1,7,3,7],[4,4,4,6],[3,7,4,6],[0,3,3,3],[3,3,4,4],[0,1,1,0]],
  "7": [[0,0,4,0],[4,0,4,2],[4,2,2,7]],
  "8": [[1,0,3,0],[0,1,0,3],[4,1,4,3],[1,3,3,3],[0,4,0,6],[4,4,4,6],[1,7,3,7],[0,1,1,0],[3,0,4,1],[0,3,1,3],[3,3,4,4],[0,6,1,7],[3,7,4,6]],
  "9": [[1,0,3,0],[0,1,0,3],[4,1,4,6],[1,3,3,3],[1,7,3,7],[0,1,1,0],[3,0,4,1],[0,3,1,3],[3,7,4,6]],
  "F": [[0,0,4,0],[0,0,0,7],[0,3,3,3]],
  "P": [[0,0,3,0],[3,0,4,1],[4,1,4,3],[3,4,4,3],[0,4,3,4],[0,0,0,7]],
  "S": [[1,0,4,0],[0,1,1,0],[0,1,0,3],[0,3,3,3],[3,3,4,4],[4,4,4,6],[3,7,4,6],[0,7,3,7]],
  "M": [[0,0,0,7],[0,0,2,3],[2,3,4,0],[4,0,4,7]],
  "D": [[0,0,2,0],[2,0,4,2],[4,2,4,5],[2,7,4,5],[0,7,2,7],[0,0,0,7]],
  "C": [[1,0,4,0],[0,1,1,0],[0,1,0,6],[0,6,1,7],[1,7,4,7]],
  ".": [[2,6,2,7]],
};

// 📊 Draw text using embedded bitmap font
function drawBitmapText(pass, text, startX, startY, color) {
  let x = startX;
  const scale = 1;
  const spacing = 6 * scale;
  
  for (const char of text) {
    const glyph = BITMAP_FONT[char];
    if (glyph) {
      for (const [x1, y1, x2, y2] of glyph) {
        drawLineInPass(pass, 
          x + x1 * scale, startY + y1 * scale,
          x + x2 * scale, startY + y2 * scale,
          color
        );
      }
    }
    x += spacing;
  }
}

// 🎨 Resize canvas to match display dimensions
function resize(width, height) {
  if (!canvas || !device || !context) return;
  
  if (canvas.width !== width || canvas.height !== height) {
    canvas.width = width;
    canvas.height = height;
    
    // Reconfigure context after resize (required for WebGPU)
    context.configure({
      device,
      format: canvasFormat,
      alphaMode: "opaque", // Use opaque to fully replace what's behind
    });
    configuredWidth = width;
    configuredHeight = height;
    
    console.log(`🎨 WebGPU canvas resized to ${width}x${height}`);
  }
}

// 🎨 Cleanup
function destroy() {
  if (device) {
    device.destroy();
    device = null;
  }
  commandQueue.length = 0;
  frameClearColor = null;
  frameTimestamps = [];
  isInitialized = false;
  log.gpu.debug("WebGPU renderer destroyed");
}

// 📊 Toggle performance overlay
function togglePerfOverlay(enabled) {
  if (enabled !== undefined) {
    perfOverlayEnabled = enabled;
  } else {
    perfOverlayEnabled = !perfOverlayEnabled;
  }
  log.gpu.debug(`Perf overlay: ${perfOverlayEnabled ? "ON" : "OFF"}`);
  return perfOverlayEnabled;
}

// 📊 Get current performance stats
function getPerfStats() {
  return {
    fps: currentFps,
    frameDelta,
    frameCount,
    drawCalls: drawCallsThisFrame,
    overlayEnabled: perfOverlayEnabled,
  };
}

export { init, handleCommand, resize, destroy, togglePerfOverlay, getPerfStats };
