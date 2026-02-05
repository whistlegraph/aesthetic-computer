// gpu-effects.mjs - WebGL2 GPU-accelerated effects using OffscreenCanvas
// Combines spin, zoom, scroll, contrast into a single efficient render pass
// Can run directly in a Web Worker without main thread involvement

let gl = null;
let spinProgram = null;
let compositeProgram = null;
let positionBuffer = null;
let texCoordBuffer = null;
let texture = null;
let framebuffer = null;
let outputTexture = null;
let readbackBuffer = null;
let canvas = null;
let initialized = false;
let lastWidth = 0;
let lastHeight = 0;

// VAO for efficient attribute setup
let vao = null;

// Cached uniform locations to avoid getUniformLocation calls every frame
let spinUniforms = null;
let compositeUniforms = null;
let blurHUniforms = null;
let blurVUniforms = null;
let sharpenUniforms = null;

// Accumulated values (matching CPU behavior)
let spinAccumulator = 0;
let scrollAccumulatorX = 0;
let scrollAccumulatorY = 0;
let zoomAccumulator = 0;

const VERTEX_SHADER = `#version 300 es
in vec2 a_position;
in vec2 a_texCoord;
out vec2 v_texCoord;
void main() {
  gl_Position = vec4(a_position, 0.0, 1.0);
  v_texCoord = a_texCoord;
}`;

// =========================================================================
// SPIN SHADER - Exact match to CPU algorithm
// Uses gl_FragCoord and texelFetch for pixel-perfect addressing
// =========================================================================
const SPIN_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec2 u_center;
uniform float u_steps;
uniform vec4 u_bounds;

in vec2 v_texCoord;
out vec4 fragColor;

const float PI = 3.14159265359;
const float TWO_PI = 6.28318530718;

void main() {
  int destX = int(gl_FragCoord.x);
  int destY = int(gl_FragCoord.y);
  
  int minX = int(u_bounds.x);
  int minY = int(u_bounds.y);
  int maxX = int(u_bounds.z);
  int maxY = int(u_bounds.w);
  
  if (destX < minX || destX >= maxX || destY < minY || destY >= maxY) {
    fragColor = texelFetch(u_texture, ivec2(destX, destY), 0);
    return;
  }
  
  float dx = float(destX) - u_center.x;
  float dy = float(destY) - u_center.y;
  float distanceSquared = dx * dx + dy * dy;
  
  if (distanceSquared < 1.0) {
    fragColor = texelFetch(u_texture, ivec2(destX, destY), 0);
    return;
  }
  
  if (distanceSquared > 16000000.0) {
    fragColor = vec4(0.0);
    return;
  }
  
  float distance = sqrt(distanceSquared);
  float angle = atan(dy, dx);
  float totalAngleChange = u_steps / distance;
  float sourceAngle = angle - totalAngleChange;
  
  sourceAngle = sourceAngle - TWO_PI * floor(sourceAngle / TWO_PI);
  if (sourceAngle < 0.0) sourceAngle += TWO_PI;
  
  float srcXf = u_center.x + distance * cos(sourceAngle);
  float srcYf = u_center.y + distance * sin(sourceAngle);
  
  int boundsWidth = maxX - minX;
  int boundsHeight = maxY - minY;
  float fBoundsWidth = float(boundsWidth);
  float fBoundsHeight = float(boundsHeight);
  float fMinX = float(minX);
  float fMinY = float(minY);
  float fMaxX = float(maxX);
  float fMaxY = float(maxY);
  
  // Wrap as floats FIRST (matching CPU behavior), then round
  float wrappedSrcX = srcXf;
  float wrappedSrcY = srcYf;
  
  // Wrap X (same float math as CPU)
  if (srcXf < fMinX || srcXf >= fMaxX) {
    float normalizedX = srcXf - fMinX;
    wrappedSrcX = fMinX + (normalizedX - fBoundsWidth * floor(normalizedX / fBoundsWidth));
    if (wrappedSrcX < fMinX) wrappedSrcX += fBoundsWidth;
  }
  
  // Wrap Y (same float math as CPU)
  if (srcYf < fMinY || srcYf >= fMaxY) {
    float normalizedY = srcYf - fMinY;
    wrappedSrcY = fMinY + (normalizedY - fBoundsHeight * floor(normalizedY / fBoundsHeight));
    if (wrappedSrcY < fMinY) wrappedSrcY += fBoundsHeight;
  }
  
  // NOW round to nearest integer (matching CPU's Math.round)
  int srcX = int(round(wrappedSrcX));
  int srcY = int(round(wrappedSrcY));
  
  // Final clamp (matching CPU's Math.max/min)
  srcX = clamp(srcX, minX, maxX - 1);
  srcY = clamp(srcY, minY, maxY - 1);
  
  fragColor = texelFetch(u_texture, ivec2(srcX, srcY), 0);
}`;

// =========================================================================
// COMPOSITE SHADER - Zoom OR Scroll OR Contrast (not combined)
// Uses gl_FragCoord and texelFetch for pixel-perfect addressing
// =========================================================================
const COMPOSITE_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec4 u_bounds;  // minX, minY, maxX, maxY

uniform float u_zoomScale;
uniform vec2 u_zoomAnchor;  // Anchor in pixel coords
uniform vec2 u_scrollOffset;  // dx, dy
uniform float u_contrast;
uniform float u_brightness;

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  int destX = int(gl_FragCoord.x);
  int destY = int(gl_FragCoord.y);
  
  int minX = int(u_bounds.x);
  int minY = int(u_bounds.y);
  int maxX = int(u_bounds.z);
  int maxY = int(u_bounds.w);
  int boundsWidth = maxX - minX;
  int boundsHeight = maxY - minY;
  
  // Check if outside working area
  if (destX < minX || destX >= maxX || destY < minY || destY >= maxY) {
    fragColor = texelFetch(u_texture, ivec2(destX, destY), 0);
    return;
  }
  
  int srcX, srcY;
  
  // Check which operation to perform (they're mutually exclusive in practice)
  bool hasZoom = u_zoomScale != 1.0;
  bool hasScroll = u_scrollOffset.x != 0.0 || u_scrollOffset.y != 0.0;
  
  if (hasZoom && !hasScroll) {
    // ZOOM ONLY - find source pixel by inverse transform from anchor
    // CPU uses: srcX = (destX - anchorPixelX) * invScale + anchorPixelX
    float anchorLocalX = u_zoomAnchor.x - float(minX);
    float anchorLocalY = u_zoomAnchor.y - float(minY);
    float localX = float(destX - minX);
    float localY = float(destY - minY);
    
    float invScale = 1.0 / u_zoomScale;
    float srcXf = (localX - anchorLocalX) * invScale + anchorLocalX;
    float srcYf = (localY - anchorLocalY) * invScale + anchorLocalY;
    
    // Match CPU: wrap floats FIRST using normalize-then-denormalize approach
    // CPU: normalizedX = (srcX - minX) / workingWidth
    //      wrappedNormX = normalizedX - floor(normalizedX)
    //      wrappedSrcX = minX + wrappedNormX * workingWidth
    float invWidth = 1.0 / float(boundsWidth);
    float invHeight = 1.0 / float(boundsHeight);
    
    float normalizedX = srcXf * invWidth;
    float normalizedY = srcYf * invHeight;
    float wrappedNormX = normalizedX - floor(normalizedX);
    float wrappedNormY = normalizedY - floor(normalizedY);
    float wrappedSrcXf = wrappedNormX * float(boundsWidth);
    float wrappedSrcYf = wrappedNormY * float(boundsHeight);
    
    // CPU uses round() for nearest-neighbor, then final modulo clamp
    int nearestX = int(round(wrappedSrcXf));
    int nearestY = int(round(wrappedSrcYf));
    
    // Final modulo wrap and clamp (match CPU's finalSrcX calculation)
    srcX = minX + ((nearestX % boundsWidth) + boundsWidth) % boundsWidth;
    srcY = minY + ((nearestY % boundsHeight) + boundsHeight) % boundsHeight;
    
  } else if (hasScroll) {
    // SCROLL ONLY - wrap around bounds
    int localX = destX - minX;
    int localY = destY - minY;
    
    int dx = int(u_scrollOffset.x);
    int dy = int(u_scrollOffset.y);
    
    int srcLocalX = (localX + dx) % boundsWidth;
    int srcLocalY = (localY + boundsHeight - dy) % boundsHeight;
    
    if (srcLocalX < 0) srcLocalX += boundsWidth;
    if (srcLocalY < 0) srcLocalY += boundsHeight;
    
    srcX = minX + srcLocalX;
    srcY = minY + srcLocalY;
    
  } else {
    // NO TRANSFORM - just pass through
    srcX = destX;
    srcY = destY;
  }
  
  vec4 color = texelFetch(u_texture, ivec2(srcX, srcY), 0);
  
  // Apply contrast
  if (u_contrast != 1.0) {
    color.rgb = clamp((color.rgb - 0.5) * u_contrast + 0.5, 0.0, 1.0);
  }
  
  // Apply brightness
  if (u_brightness != 0.0) {
    color.rgb = clamp(color.rgb + u_brightness / 255.0, 0.0, 1.0);
  }
  
  fragColor = color;
}`;

// =========================================================================
// BLUR SHADER - Separable Gaussian blur (horizontal pass)
// Simple version - no Y flipping, handled in readback
// Uses fixed loop bound (15) for WebGL ES compatibility - some GPUs don't support dynamic loop bounds
// =========================================================================
const BLUR_H_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec4 u_bounds;  // minX, minY, maxX, maxY in CPU coords
uniform float u_radius;
uniform float u_weights[16];
uniform int u_kernelSize;

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  vec4 sum = vec4(0.0);
  float texelX = 1.0 / u_resolution.x;
  int radius = u_kernelSize / 2;
  
  // Fixed loop bound for WebGL ES compatibility (max kernel size is 15)
  for (int i = 0; i < 15; i++) {
    if (i >= u_kernelSize) break;
    float offset = float(i - radius);
    vec2 sampleUV = v_texCoord + vec2(offset * texelX, 0.0);
    sampleUV.x = clamp(sampleUV.x, 0.0, 1.0);
    sum += texture(u_texture, sampleUV) * u_weights[i];
  }
  
  fragColor = sum;
}`;

// =========================================================================
// BLUR SHADER - Separable Gaussian blur (vertical pass)
// Simple version - no Y flipping, handled in readback
// Uses fixed loop bound (15) for WebGL ES compatibility - some GPUs don't support dynamic loop bounds
// =========================================================================
const BLUR_V_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec4 u_bounds;
uniform float u_radius;
uniform float u_weights[16];
uniform int u_kernelSize;

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  vec4 sum = vec4(0.0);
  float texelY = 1.0 / u_resolution.y;
  int radius = u_kernelSize / 2;
  
  // Fixed loop bound for WebGL ES compatibility (max kernel size is 15)
  for (int i = 0; i < 15; i++) {
    if (i >= u_kernelSize) break;
    float offset = float(i - radius);
    vec2 sampleUV = v_texCoord + vec2(0.0, offset * texelY);
    sampleUV.y = clamp(sampleUV.y, 0.0, 1.0);
    sum += texture(u_texture, sampleUV) * u_weights[i];
  }
  
  fragColor = sum;
}`;

// =========================================================================
// SHARPEN SHADER - Unsharp mask convolution
// =========================================================================
const SHARPEN_FRAGMENT_SHADER = `#version 300 es
// Use mediump for better compatibility with low-end mobile GPUs (e.g. Unihertz Jelly)
// Sharpen doesn't need highp - it's just texture sampling and simple math
precision mediump float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec4 u_bounds;
uniform float u_strength;

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  vec2 pixelCoord = v_texCoord * u_resolution;
  
  // Check if outside working area (with 1px border for kernel)
  if (pixelCoord.x < u_bounds.x + 1.0 || pixelCoord.x >= u_bounds.z - 1.0 ||
      pixelCoord.y < u_bounds.y + 1.0 || pixelCoord.y >= u_bounds.w - 1.0) {
    fragColor = texture(u_texture, v_texCoord);
    return;
  }
  
  vec2 texel = 1.0 / u_resolution;
  
  // Sample center and neighbors
  vec4 center = texture(u_texture, v_texCoord);
  vec4 top    = texture(u_texture, v_texCoord + vec2(0.0, -texel.y));
  vec4 bottom = texture(u_texture, v_texCoord + vec2(0.0,  texel.y));
  vec4 left   = texture(u_texture, v_texCoord + vec2(-texel.x, 0.0));
  vec4 right  = texture(u_texture, v_texCoord + vec2( texel.x, 0.0));
  
  // Skip transparent pixels
  if (center.a == 0.0) {
    fragColor = center;
    return;
  }
  
  // Unsharp mask: center * (1 + 4*strength) - neighbors * strength
  float centerWeight = 1.0 + 4.0 * u_strength;
  float edgeWeight = -u_strength;
  
  vec3 sharpened = center.rgb * centerWeight + 
                   (top.rgb + bottom.rgb + left.rgb + right.rgb) * edgeWeight;
  
  fragColor = vec4(clamp(sharpened, 0.0, 1.0), center.a);
}`;

let blurHProgram = null;
let blurVProgram = null;
let sharpenProgram = null;
let pingPongTexture = null;  // For multi-pass effects
let pingPongFramebuffer = null;

// Helper to resize textures without recreating the entire context
function resizeTextures(width, height) {
  // Resize input texture
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  
  // Resize output texture
  gl.bindTexture(gl.TEXTURE_2D, outputTexture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  
  // Resize ping-pong texture
  gl.bindTexture(gl.TEXTURE_2D, pingPongTexture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  
  // Resize canvas
  canvas.width = width;
  canvas.height = height;
  
  // Reallocate readback buffer
  readbackBuffer = new Uint8Array(width * height * 4);
  
  lastWidth = width;
  lastHeight = height;
}

function initWebGL2(width, height) {
  // If already initialized, just resize textures if needed
  if (initialized && gl) {
    if (width !== lastWidth || height !== lastHeight) {
      resizeTextures(width, height);
    }
    return true;
  }
  
  try {
    // Create OffscreenCanvas (works in workers!)
    canvas = new OffscreenCanvas(width, height);
    gl = canvas.getContext('webgl2', {
      antialias: false,
      depth: false,
      stencil: false,
      preserveDrawingBuffer: true,
      powerPreference: 'high-performance'
    });
    
    if (!gl) {
      console.warn('🎮 GPU Effects: WebGL2 not available');
      return false;
    }
    
    // Compile spin program
    const spinVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const spinFrag = compileShader(gl, gl.FRAGMENT_SHADER, SPIN_FRAGMENT_SHADER);
    if (!spinVert || !spinFrag) return false;
    
    spinProgram = gl.createProgram();
    gl.attachShader(spinProgram, spinVert);
    gl.attachShader(spinProgram, spinFrag);
    gl.linkProgram(spinProgram);
    if (!gl.getProgramParameter(spinProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Spin program link failed:', gl.getProgramInfoLog(spinProgram));
      return false;
    }
    
    // Compile composite program
    const compVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const compFrag = compileShader(gl, gl.FRAGMENT_SHADER, COMPOSITE_FRAGMENT_SHADER);
    if (!compVert || !compFrag) return false;
    
    compositeProgram = gl.createProgram();
    gl.attachShader(compositeProgram, compVert);
    gl.attachShader(compositeProgram, compFrag);
    gl.linkProgram(compositeProgram);
    if (!gl.getProgramParameter(compositeProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Composite program link failed:', gl.getProgramInfoLog(compositeProgram));
      return false;
    }
    
    // Compile blur horizontal program
    const blurHVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const blurHFrag = compileShader(gl, gl.FRAGMENT_SHADER, BLUR_H_FRAGMENT_SHADER);
    if (!blurHVert || !blurHFrag) return false;
    
    blurHProgram = gl.createProgram();
    gl.attachShader(blurHProgram, blurHVert);
    gl.attachShader(blurHProgram, blurHFrag);
    gl.linkProgram(blurHProgram);
    if (!gl.getProgramParameter(blurHProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Blur H program link failed:', gl.getProgramInfoLog(blurHProgram));
      return false;
    }
    
    // Compile blur vertical program
    const blurVVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const blurVFrag = compileShader(gl, gl.FRAGMENT_SHADER, BLUR_V_FRAGMENT_SHADER);
    if (!blurVVert || !blurVFrag) return false;
    
    blurVProgram = gl.createProgram();
    gl.attachShader(blurVProgram, blurVVert);
    gl.attachShader(blurVProgram, blurVFrag);
    gl.linkProgram(blurVProgram);
    if (!gl.getProgramParameter(blurVProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Blur V program link failed:', gl.getProgramInfoLog(blurVProgram));
      return false;
    }
    
    // Compile sharpen program
    const sharpenVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const sharpenFrag = compileShader(gl, gl.FRAGMENT_SHADER, SHARPEN_FRAGMENT_SHADER);
    if (!sharpenVert || !sharpenFrag) return false;
    
    sharpenProgram = gl.createProgram();
    gl.attachShader(sharpenProgram, sharpenVert);
    gl.attachShader(sharpenProgram, sharpenFrag);
    gl.linkProgram(sharpenProgram);
    if (!gl.getProgramParameter(sharpenProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Sharpen program link failed:', gl.getProgramInfoLog(sharpenProgram));
      return false;
    }
    
    // Cache all uniform locations to avoid getUniformLocation calls every frame
    spinUniforms = {
      u_resolution: gl.getUniformLocation(spinProgram, 'u_resolution'),
      u_center: gl.getUniformLocation(spinProgram, 'u_center'),
      u_steps: gl.getUniformLocation(spinProgram, 'u_steps'),
      u_bounds: gl.getUniformLocation(spinProgram, 'u_bounds'),
      u_texture: gl.getUniformLocation(spinProgram, 'u_texture'),
    };
    
    compositeUniforms = {
      u_resolution: gl.getUniformLocation(compositeProgram, 'u_resolution'),
      u_bounds: gl.getUniformLocation(compositeProgram, 'u_bounds'),
      u_zoomScale: gl.getUniformLocation(compositeProgram, 'u_zoomScale'),
      u_zoomAnchor: gl.getUniformLocation(compositeProgram, 'u_zoomAnchor'),
      u_scrollOffset: gl.getUniformLocation(compositeProgram, 'u_scrollOffset'),
      u_contrast: gl.getUniformLocation(compositeProgram, 'u_contrast'),
      u_brightness: gl.getUniformLocation(compositeProgram, 'u_brightness'),
      u_texture: gl.getUniformLocation(compositeProgram, 'u_texture'),
    };
    
    blurHUniforms = {
      u_resolution: gl.getUniformLocation(blurHProgram, 'u_resolution'),
      u_bounds: gl.getUniformLocation(blurHProgram, 'u_bounds'),
      u_radius: gl.getUniformLocation(blurHProgram, 'u_radius'),
      u_weights: gl.getUniformLocation(blurHProgram, 'u_weights'),
      u_kernelSize: gl.getUniformLocation(blurHProgram, 'u_kernelSize'),
      u_texture: gl.getUniformLocation(blurHProgram, 'u_texture'),
    };
    
    blurVUniforms = {
      u_resolution: gl.getUniformLocation(blurVProgram, 'u_resolution'),
      u_bounds: gl.getUniformLocation(blurVProgram, 'u_bounds'),
      u_radius: gl.getUniformLocation(blurVProgram, 'u_radius'),
      u_weights: gl.getUniformLocation(blurVProgram, 'u_weights'),
      u_kernelSize: gl.getUniformLocation(blurVProgram, 'u_kernelSize'),
      u_texture: gl.getUniformLocation(blurVProgram, 'u_texture'),
    };
    
    sharpenUniforms = {
      u_resolution: gl.getUniformLocation(sharpenProgram, 'u_resolution'),
      u_bounds: gl.getUniformLocation(sharpenProgram, 'u_bounds'),
      u_strength: gl.getUniformLocation(sharpenProgram, 'u_strength'),
      u_texture: gl.getUniformLocation(sharpenProgram, 'u_texture'),
    };
    
    // Create VAO for efficient attribute setup
    vao = gl.createVertexArray();
    gl.bindVertexArray(vao);
    
    // Create geometry (full-screen quad)
    positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
      -1, -1,  1, -1,  -1, 1,
      -1,  1,  1, -1,   1, 1
    ]), gl.STATIC_DRAW);
    
    // Set up position attribute in VAO
    const posLoc = gl.getAttribLocation(spinProgram, 'a_position');
    gl.enableVertexAttribArray(posLoc);
    gl.vertexAttribPointer(posLoc, 2, gl.FLOAT, false, 0, 0);
    
    texCoordBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
      0, 0,  1, 0,  0, 1,
      0, 1,  1, 0,  1, 1
    ]), gl.STATIC_DRAW);
    
    // Set up texCoord attribute in VAO
    const texLoc = gl.getAttribLocation(spinProgram, 'a_texCoord');
    gl.enableVertexAttribArray(texLoc);
    gl.vertexAttribPointer(texLoc, 2, gl.FLOAT, false, 0, 0);
    
    gl.bindVertexArray(null);
    
    // Create input texture (NEAREST for pixel-perfect sampling)
    texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    
    // Create framebuffer for output
    framebuffer = gl.createFramebuffer();
    outputTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, outputTexture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, outputTexture, 0);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    
    // Create ping-pong texture and framebuffer for multi-pass effects (blur)
    pingPongTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, pingPongTexture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);  // LINEAR for blur
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    
    pingPongFramebuffer = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, pingPongFramebuffer);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, pingPongTexture, 0);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    
    // Pre-allocate readback buffer
    readbackBuffer = new Uint8Array(width * height * 4);
    
    lastWidth = width;
    lastHeight = height;
    initialized = true;
    
    console.log(`🎮 GPU Effects: Initialized ${width}x${height}`);
    return true;
  } catch (e) {
    console.error('🎮 GPU Effects: Init failed:', e);
    return false;
  }
}

function compileShader(gl, type, source) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    console.error('🎮 GPU Effects: Shader compile failed:', gl.getShaderInfoLog(shader));
    gl.deleteShader(shader);
    return null;
  }
  return shader;
}

// Note: setupAttributes is no longer used - we use VAOs instead for better performance
// The VAO is set up once during initialization and bound before each draw call

/**
 * GPU-accelerated spin operation - EXACT MATCH to CPU algorithm
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height  
 * @param {number} steps - Rotation steps (accumulates fractionally like CPU)
 * @param {number|null} anchorX - Anchor X (null = center)
 * @param {number|null} anchorY - Anchor Y (null = center)
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean} - True if GPU spin was used, false if fallback needed
 */
export function gpuSpin(pixels, width, height, steps, anchorX = null, anchorY = null, mask = null) {
  // Match CPU early exit
  if (Math.abs(steps) < 0.5) {
    return true;
  }
  
  // Accumulate fractional steps (same as CPU)
  spinAccumulator += steps;
  const integerSteps = Math.floor(spinAccumulator);
  spinAccumulator -= integerSteps;
  
  if (integerSteps === 0) {
    return true;
  }
  
  // Initialize WebGL2 if needed
  if (!initWebGL2(width, height)) {
    return false;
  }
  
  try {
    // Determine bounds
    const minX = mask?.x ?? 0;
    const minY = mask?.y ?? 0;
    const maxX = mask ? mask.x + mask.width : width;
    const maxY = mask ? mask.y + mask.height : height;
    
    const workingWidth = maxX - minX;
    const workingHeight = maxY - minY;
    const centerX = anchorX ?? (minX + Math.floor(workingWidth / 2));
    const centerY = anchorY ?? (minY + Math.floor(workingHeight / 2));
    
    // Upload pixels directly (no flip - shader handles Y flip when sampling)
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    
    // Render to framebuffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.viewport(0, 0, width, height);
    
    gl.useProgram(spinProgram);
    
    // Set uniforms (using cached locations for performance)
    gl.uniform2f(spinUniforms.u_resolution, width, height);
    gl.uniform2f(spinUniforms.u_center, centerX, centerY);
    gl.uniform1f(spinUniforms.u_steps, integerSteps);
    gl.uniform4f(spinUniforms.u_bounds, minX, minY, maxX, maxY);
    gl.uniform1i(spinUniforms.u_texture, 0);
    
    // Use VAO for efficient attribute binding
    gl.bindVertexArray(vao);
    
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back pixels directly - shader output is already in CPU orientation
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    
    return true;
  } catch (e) {
    console.error('🎮 GPU Spin: Render failed:', e);
    return false;
  }
}

/**
 * GPU-accelerated composite effects (zoom + scroll + contrast in one pass)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {Object} options - { zoom, zoomAnchorX, zoomAnchorY, scrollX, scrollY, contrast, brightness, mask }
 * @returns {boolean}
 */
export function gpuComposite(pixels, width, height, options = {}) {
  const {
    zoom = 1.0,
    zoomAnchorX = 0.5,
    zoomAnchorY = 0.5,
    scrollX = 0,
    scrollY = 0,
    contrast = 1.0,
    brightness = 0,
    mask = null
  } = options;
  
  // Early exit if no effects
  if (zoom === 1.0 && scrollX === 0 && scrollY === 0 && contrast === 1.0 && brightness === 0) {
    return true;
  }
  
  if (!initWebGL2(width, height)) {
    return false;
  }
  
  try {
    const minX = mask?.x ?? 0;
    const minY = mask?.y ?? 0;
    const maxX = mask ? mask.x + mask.width : width;
    const maxY = mask ? mask.y + mask.height : height;
    
    const workingWidth = maxX - minX;
    const workingHeight = maxY - minY;
    
    // Anchor in pixel coordinates
    const anchorPixelX = minX + workingWidth * zoomAnchorX;
    const anchorPixelY = minY + workingHeight * zoomAnchorY;
    
    // Upload pixels directly (no flip - shader handles Y flip when sampling)
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.viewport(0, 0, width, height);
    
    gl.useProgram(compositeProgram);
    
    // Set uniforms (using cached locations for performance)
    gl.uniform2f(compositeUniforms.u_resolution, width, height);
    gl.uniform4f(compositeUniforms.u_bounds, minX, minY, maxX, maxY);
    gl.uniform1f(compositeUniforms.u_zoomScale, zoom);
    gl.uniform2f(compositeUniforms.u_zoomAnchor, anchorPixelX, anchorPixelY);
    gl.uniform2f(compositeUniforms.u_scrollOffset, scrollX, scrollY);
    gl.uniform1f(compositeUniforms.u_contrast, contrast);
    gl.uniform1f(compositeUniforms.u_brightness, brightness);
    gl.uniform1i(compositeUniforms.u_texture, 0);
    
    // Use VAO for efficient attribute binding
    gl.bindVertexArray(vao);
    
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back pixels directly - shader output is already in CPU orientation
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    
    return true;
  } catch (e) {
    console.error('🎮 GPU Composite: Render failed:', e);
    return false;
  }
}

/**
 * GPU-accelerated zoom (uses composite shader with zoom only)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} scale - Zoom scale (1.0 = no change, >1 = zoom in, <1 = zoom out)
 * @param {number} anchorX - Anchor X (0-1, default 0.5 = center)
 * @param {number} anchorY - Anchor Y (0-1, default 0.5 = center)
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuZoom(pixels, width, height, scale = 1.0, anchorX = 0.5, anchorY = 0.5, mask = null) {
  if (scale === 1.0) return true;
  
  // Use composite shader with only zoom enabled
  return gpuComposite(pixels, width, height, {
    zoom: scale,
    zoomAnchorX: anchorX,
    zoomAnchorY: anchorY,
    scrollX: 0,
    scrollY: 0,
    contrast: 1.0,
    brightness: 0,
    mask
  });
}

/**
 * GPU-accelerated scroll (pixel wrapping)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} dx - Horizontal scroll amount (integer pixels)
 * @param {number} dy - Vertical scroll amount (integer pixels)
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuScroll(pixels, width, height, dx = 0, dy = 0, mask = null) {
  if (dx === 0 && dy === 0) return true;
  
  // Use composite shader with only scroll enabled
  return gpuComposite(pixels, width, height, {
    zoom: 1.0,
    zoomAnchorX: 0.5,
    zoomAnchorY: 0.5,
    scrollX: dx,
    scrollY: dy,
    contrast: 1.0,
    brightness: 0,
    mask
  });
}

/**
 * Check if GPU effects are available
 */
export function isGpuEffectsAvailable() {
  try {
    const testCanvas = new OffscreenCanvas(1, 1);
    const testGl = testCanvas.getContext('webgl2');
    return !!testGl;
  } catch {
    return false;
  }
}

/**
 * Generate Gaussian weights for blur kernel
 */
function generateGaussianWeights(kernelSize) {
  const weights = new Float32Array(16);  // Max 16 weights
  const radius = Math.floor(kernelSize / 2);
  const sigma = radius / 3.0;
  
  let sum = 0;
  for (let i = 0; i < kernelSize; i++) {
    const x = i - radius;
    const weight = Math.exp(-(x * x) / (2 * sigma * sigma));
    weights[i] = weight;
    sum += weight;
  }
  
  // Normalize
  for (let i = 0; i < kernelSize; i++) {
    weights[i] /= sum;
  }
  
  return weights;
}

/**
 * GPU-accelerated Gaussian blur (separable, 2-pass)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} strength - Blur strength (radius)
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuBlur(pixels, width, height, strength = 1, mask = null) {
  if (strength <= 0.1) return true;
  
  if (!initWebGL2(width, height)) {
    return false;
  }
  
  try {
    const minX = mask?.x ?? 0;
    const minY = mask?.y ?? 0;
    const maxX = mask ? mask.x + mask.width : width;
    const maxY = mask ? mask.y + mask.height : height;
    
    // Calculate kernel size (capped at 15 for performance)
    const blurRadius = Math.max(1, Math.floor(strength));
    const kernelSize = Math.min(blurRadius * 2 + 1, 15);
    const weights = generateGaussianWeights(kernelSize);
    
    // Upload pixels to texture with Y-flip
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);
    
    // PASS 1: Horizontal blur (texture -> pingPong)
    gl.bindFramebuffer(gl.FRAMEBUFFER, pingPongFramebuffer);
    gl.viewport(0, 0, width, height);
    gl.useProgram(blurHProgram);
    
    // Set uniforms (using cached locations for performance)
    gl.uniform2f(blurHUniforms.u_resolution, width, height);
    gl.uniform4f(blurHUniforms.u_bounds, minX, minY, maxX, maxY);
    gl.uniform1f(blurHUniforms.u_radius, blurRadius);
    gl.uniform1fv(blurHUniforms.u_weights, weights);
    gl.uniform1i(blurHUniforms.u_kernelSize, kernelSize);
    gl.uniform1i(blurHUniforms.u_texture, 0);
    
    gl.bindVertexArray(vao);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // PASS 2: Vertical blur (pingPong -> output)
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.useProgram(blurVProgram);
    
    // Set uniforms (using cached locations for performance)
    gl.uniform2f(blurVUniforms.u_resolution, width, height);
    gl.uniform4f(blurVUniforms.u_bounds, minX, minY, maxX, maxY);
    gl.uniform1f(blurVUniforms.u_radius, blurRadius);
    gl.uniform1fv(blurVUniforms.u_weights, weights);
    gl.uniform1i(blurVUniforms.u_kernelSize, kernelSize);
    gl.uniform1i(blurVUniforms.u_texture, 0);
    
    // VAO already bound
    gl.bindTexture(gl.TEXTURE_2D, pingPongTexture);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back pixels (flip Y)
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, readbackBuffer);
    const rowSize = width * 4;
    for (let y = 0; y < height; y++) {
      const srcRow = (height - 1 - y) * rowSize;
      const dstRow = y * rowSize;
      pixels.set(readbackBuffer.subarray(srcRow, srcRow + rowSize), dstRow);
    }
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    return true;
  } catch (e) {
    console.error('🎮 GPU Blur: Render failed:', e);
    return false;
  }
}

/**
 * GPU-accelerated sharpen (unsharp mask)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} strength - Sharpen strength
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuSharpen(pixels, width, height, strength = 1, mask = null) {
  if (strength <= 0) return true;
  
  if (!initWebGL2(width, height)) {
    return false;
  }
  
  try {
    const minX = mask?.x ?? 0;
    const minY = mask?.y ?? 0;
    const maxX = mask ? mask.x + mask.width : width;
    const maxY = mask ? mask.y + mask.height : height;
    
    // Upload pixels to texture with Y-flip
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);
    
    // Render sharpen to framebuffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.viewport(0, 0, width, height);
    gl.useProgram(sharpenProgram);
    
    // Set uniforms (using cached locations for performance)
    gl.uniform2f(sharpenUniforms.u_resolution, width, height);
    gl.uniform4f(sharpenUniforms.u_bounds, minX, minY, maxX, maxY);
    gl.uniform1f(sharpenUniforms.u_strength, strength);
    gl.uniform1i(sharpenUniforms.u_texture, 0);
    
    gl.bindVertexArray(vao);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back pixels (flip Y)
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, readbackBuffer);
    
    // Sanity check: if readback is all zeros but input wasn't, GPU likely failed
    // Check a sample of pixels to detect blank/corrupted output (for low-end GPUs like Unihertz Jelly)
    let hasNonZero = false;
    const sampleStep = Math.max(1, Math.floor(readbackBuffer.length / 100)); // Check ~100 samples
    for (let i = 0; i < readbackBuffer.length && !hasNonZero; i += sampleStep) {
      if (readbackBuffer[i] !== 0) hasNonZero = true;
    }
    
    // Check if input had non-zero pixels (sample the same way)
    let inputHadData = false;
    for (let i = 0; i < pixels.length && !inputHadData; i += sampleStep) {
      if (pixels[i] !== 0) inputHadData = true;
    }
    
    if (inputHadData && !hasNonZero) {
      console.warn('🎮 GPU Sharpen: Output appears blank - falling back to CPU');
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);
      return false;
    }
    
    const rowSize = width * 4;
    for (let y = 0; y < height; y++) {
      const srcRow = (height - 1 - y) * rowSize;
      const dstRow = y * rowSize;
      pixels.set(readbackBuffer.subarray(srcRow, srcRow + rowSize), dstRow);
    }
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    return true;
  } catch (e) {
    console.error('🎮 GPU Sharpen: Render failed:', e);
    return false;
  }
}

/**
 * Reset accumulators (call when context changes)
 */
export function resetAccumulators() {
  spinAccumulator = 0;
  scrollAccumulatorX = 0;
  scrollAccumulatorY = 0;
  zoomAccumulator = 0;
}

/**
 * Clean up GPU resources
 */
export function cleanupGpuEffects() {
  if (gl) {
    if (texture) gl.deleteTexture(texture);
    if (outputTexture) gl.deleteTexture(outputTexture);
    if (pingPongTexture) gl.deleteTexture(pingPongTexture);
    if (framebuffer) gl.deleteFramebuffer(framebuffer);
    if (pingPongFramebuffer) gl.deleteFramebuffer(pingPongFramebuffer);
    if (positionBuffer) gl.deleteBuffer(positionBuffer);
    if (texCoordBuffer) gl.deleteBuffer(texCoordBuffer);
    if (spinProgram) gl.deleteProgram(spinProgram);
    if (compositeProgram) gl.deleteProgram(compositeProgram);
    if (blurHProgram) gl.deleteProgram(blurHProgram);
    if (blurVProgram) gl.deleteProgram(blurVProgram);
    if (sharpenProgram) gl.deleteProgram(sharpenProgram);
    gl = null;
  }
  canvas = null;
  initialized = false;
  resetAccumulators();
}

export default { 
  gpuSpin, 
  gpuComposite,
  gpuZoom,
  gpuScroll,
  gpuBlur,
  gpuSharpen,
  isGpuEffectsAvailable, 
  resetAccumulators,
  cleanupGpuEffects 
};
