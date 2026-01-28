// gpu-primitives.mjs - WebGL2 GPU-accelerated 2D primitive rendering
// Batches lines, circles, boxes into efficient GPU draw calls
// Works with OffscreenCanvas in Web Workers

let gl = null;
let lineProgram = null;
let positionBuffer = null;
let instanceBuffer = null;
let colorBuffer = null;
let canvas = null;
let initialized = false;
let lastWidth = 0;
let lastHeight = 0;

// Batch storage for primitives
let lineBatch = [];
const MAX_LINES_PER_BATCH = 10000;

// Line vertex shader - uses instancing for efficient batch rendering
const LINE_VERTEX_SHADER = `#version 300 es
precision highp float;

// Per-vertex attributes (line segment template: 0 or 1)
in float a_vertex;

// Per-instance attributes
in vec4 a_lineEndpoints;  // x0, y0, x1, y1
in vec4 a_color;          // r, g, b, a (0-255 range)

uniform vec2 u_resolution;

out vec4 v_color;

void main() {
  // Interpolate between start and end points based on vertex (0 or 1)
  vec2 pos = mix(a_lineEndpoints.xy, a_lineEndpoints.zw, a_vertex);
  
  // Convert to clip space (-1 to 1)
  vec2 clipPos = (pos / u_resolution) * 2.0 - 1.0;
  clipPos.y = -clipPos.y;  // Flip Y for CPU coordinate system
  
  gl_Position = vec4(clipPos, 0.0, 1.0);
  v_color = a_color / 255.0;
}`;

const LINE_FRAGMENT_SHADER = `#version 300 es
precision highp float;

in vec4 v_color;
out vec4 fragColor;

void main() {
  fragColor = v_color;
}`;

/**
 * Initialize WebGL2 context and shaders for primitive rendering
 */
function initWebGL2(width, height) {
  if (initialized && lastWidth === width && lastHeight === height) {
    return true;
  }
  
  try {
    // Create or resize canvas
    if (!canvas) {
      canvas = new OffscreenCanvas(width, height);
      gl = canvas.getContext('webgl2', {
        alpha: true,
        antialias: false,
        depth: false,
        stencil: false,
        premultipliedAlpha: false,
        preserveDrawingBuffer: true
      });
      
      if (!gl) {
        console.warn('🎨 GPU Primitives: WebGL2 not available');
        return false;
      }
      
      // Compile line shader program
      const vertShader = gl.createShader(gl.VERTEX_SHADER);
      gl.shaderSource(vertShader, LINE_VERTEX_SHADER);
      gl.compileShader(vertShader);
      if (!gl.getShaderParameter(vertShader, gl.COMPILE_STATUS)) {
        console.error('Line vertex shader error:', gl.getShaderInfoLog(vertShader));
        return false;
      }
      
      const fragShader = gl.createShader(gl.FRAGMENT_SHADER);
      gl.shaderSource(fragShader, LINE_FRAGMENT_SHADER);
      gl.compileShader(fragShader);
      if (!gl.getShaderParameter(fragShader, gl.COMPILE_STATUS)) {
        console.error('Line fragment shader error:', gl.getShaderInfoLog(fragShader));
        return false;
      }
      
      lineProgram = gl.createProgram();
      gl.attachShader(lineProgram, vertShader);
      gl.attachShader(lineProgram, fragShader);
      gl.linkProgram(lineProgram);
      if (!gl.getProgramParameter(lineProgram, gl.LINK_STATUS)) {
        console.error('Line program link error:', gl.getProgramInfoLog(lineProgram));
        return false;
      }
      
      // Create buffers
      // Vertex buffer: just 0 and 1 for line start/end
      positionBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([0, 1]), gl.STATIC_DRAW);
      
      // Instance buffers for line data
      instanceBuffer = gl.createBuffer();
      colorBuffer = gl.createBuffer();
      
      console.log('🎨 GPU Primitives: Initialized', width, 'x', height);
    } else {
      canvas.width = width;
      canvas.height = height;
    }
    
    lastWidth = width;
    lastHeight = height;
    initialized = true;
    return true;
    
  } catch (e) {
    console.error('🎨 GPU Primitives: Init failed:', e);
    return false;
  }
}

/**
 * Add a line to the current batch
 * @param {number} x0 - Start X
 * @param {number} y0 - Start Y
 * @param {number} x1 - End X
 * @param {number} y1 - End Y
 * @param {number} r - Red (0-255)
 * @param {number} g - Green (0-255)
 * @param {number} b - Blue (0-255)
 * @param {number} a - Alpha (0-255)
 */
export function batchLine(x0, y0, x1, y1, r, g, b, a = 255) {
  if (lineBatch.length >= MAX_LINES_PER_BATCH) {
    console.warn('🎨 GPU Primitives: Line batch full, flushing');
    return false;
  }
  
  lineBatch.push({
    x0, y0, x1, y1,
    r, g, b, a
  });
  
  return true;
}

/**
 * Render all batched lines to the pixel buffer
 * Uses GL_LINES with instancing for efficient rendering
 * @param {Uint8ClampedArray} pixels - Destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @returns {boolean} Success
 */
export function flushLines(pixels, width, height) {
  if (lineBatch.length === 0) return true;
  
  if (!initWebGL2(width, height)) {
    // Fall back to CPU rendering
    lineBatch.length = 0;
    return false;
  }
  
  try {
    const numLines = lineBatch.length;
    
    // Prepare instance data
    const lineData = new Float32Array(numLines * 4);  // x0, y0, x1, y1
    const colorData = new Float32Array(numLines * 4); // r, g, b, a
    
    for (let i = 0; i < numLines; i++) {
      const line = lineBatch[i];
      const baseIdx = i * 4;
      lineData[baseIdx] = line.x0;
      lineData[baseIdx + 1] = line.y0;
      lineData[baseIdx + 2] = line.x1;
      lineData[baseIdx + 3] = line.y1;
      colorData[baseIdx] = line.r;
      colorData[baseIdx + 1] = line.g;
      colorData[baseIdx + 2] = line.b;
      colorData[baseIdx + 3] = line.a;
    }
    
    // Upload instance data
    gl.bindBuffer(gl.ARRAY_BUFFER, instanceBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, lineData, gl.DYNAMIC_DRAW);
    
    gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, colorData, gl.DYNAMIC_DRAW);
    
    // Set up rendering
    gl.viewport(0, 0, width, height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    
    // Enable blending for alpha
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    
    gl.useProgram(lineProgram);
    
    // Set uniforms
    gl.uniform2f(gl.getUniformLocation(lineProgram, 'u_resolution'), width, height);
    
    // Set up vertex attribute (0 or 1)
    const aVertex = gl.getAttribLocation(lineProgram, 'a_vertex');
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.enableVertexAttribArray(aVertex);
    gl.vertexAttribPointer(aVertex, 1, gl.FLOAT, false, 0, 0);
    
    // Set up instance attributes
    const aLineEndpoints = gl.getAttribLocation(lineProgram, 'a_lineEndpoints');
    gl.bindBuffer(gl.ARRAY_BUFFER, instanceBuffer);
    gl.enableVertexAttribArray(aLineEndpoints);
    gl.vertexAttribPointer(aLineEndpoints, 4, gl.FLOAT, false, 0, 0);
    gl.vertexAttribDivisor(aLineEndpoints, 1);  // Per-instance
    
    const aColor = gl.getAttribLocation(lineProgram, 'a_color');
    gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
    gl.enableVertexAttribArray(aColor);
    gl.vertexAttribPointer(aColor, 4, gl.FLOAT, false, 0, 0);
    gl.vertexAttribDivisor(aColor, 1);  // Per-instance
    
    // Draw all lines with instancing
    gl.drawArraysInstanced(gl.LINES, 0, 2, numLines);
    
    // Read back pixels
    const readBuffer = new Uint8Array(width * height * 4);
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, readBuffer);
    
    // Composite onto existing pixels (with alpha blending)
    // WebGL readPixels returns Y-flipped data
    const rowSize = width * 4;
    for (let y = 0; y < height; y++) {
      const srcRow = (height - 1 - y) * rowSize;
      const dstRow = y * rowSize;
      
      for (let x = 0; x < width; x++) {
        const srcIdx = srcRow + x * 4;
        const dstIdx = dstRow + x * 4;
        
        const srcA = readBuffer[srcIdx + 3];
        if (srcA > 0) {
          // Simple alpha composite
          const srcR = readBuffer[srcIdx];
          const srcG = readBuffer[srcIdx + 1];
          const srcB = readBuffer[srcIdx + 2];
          
          if (srcA === 255) {
            // Opaque - just copy
            pixels[dstIdx] = srcR;
            pixels[dstIdx + 1] = srcG;
            pixels[dstIdx + 2] = srcB;
            pixels[dstIdx + 3] = 255;
          } else {
            // Alpha blend
            const dstA = pixels[dstIdx + 3];
            const outA = srcA + dstA * (255 - srcA) / 255;
            if (outA > 0) {
              pixels[dstIdx] = (srcR * srcA + pixels[dstIdx] * dstA * (255 - srcA) / 255) / outA;
              pixels[dstIdx + 1] = (srcG * srcA + pixels[dstIdx + 1] * dstA * (255 - srcA) / 255) / outA;
              pixels[dstIdx + 2] = (srcB * srcA + pixels[dstIdx + 2] * dstA * (255 - srcA) / 255) / outA;
              pixels[dstIdx + 3] = outA;
            }
          }
        }
      }
    }
    
    // Clear the batch
    lineBatch.length = 0;
    
    return true;
  } catch (e) {
    console.error('🎨 GPU Primitives: Flush failed:', e);
    lineBatch.length = 0;
    return false;
  }
}

/**
 * Get current batch size
 */
export function getLineBatchSize() {
  return lineBatch.length;
}

/**
 * Clear the line batch without rendering
 */
export function clearLineBatch() {
  lineBatch.length = 0;
}

/**
 * Check if GPU primitives are available
 */
export function isGpuPrimitivesAvailable() {
  try {
    const testCanvas = new OffscreenCanvas(1, 1);
    const testGl = testCanvas.getContext('webgl2');
    return !!testGl;
  } catch {
    return false;
  }
}

/**
 * Cleanup GPU resources
 */
export function cleanupGpuPrimitives() {
  if (gl) {
    if (lineProgram) gl.deleteProgram(lineProgram);
    if (positionBuffer) gl.deleteBuffer(positionBuffer);
    if (instanceBuffer) gl.deleteBuffer(instanceBuffer);
    if (colorBuffer) gl.deleteBuffer(colorBuffer);
    gl = null;
  }
  canvas = null;
  initialized = false;
  lineBatch.length = 0;
}

export default {
  batchLine,
  flushLines,
  getLineBatchSize,
  clearLineBatch,
  isGpuPrimitivesAvailable,
  cleanupGpuPrimitives
};
