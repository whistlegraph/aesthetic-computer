// gpu-effects.mjs - WebGL2 GPU-accelerated effects using OffscreenCanvas
// Combines spin, zoom, scroll, contrast into a single efficient render pass
// Can run directly in a Web Worker without main thread involvement

let gl = null;
let spinProgram = null;
let compositeProgram = null;
let invertProgram = null;  // Invert shader program
let floodSeedProgram = null;  // Flood fill seed initialization
let floodJFAProgram = null;   // Flood fill Jump Flooding Algorithm pass
let floodFillProgram = null;  // Flood fill final color application
let shearProgram = null;  // KidPix-style shear
let layerCompositeProgram = null;  // Multi-layer alpha compositing
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
let invertUniforms = null;  // Invert shader uniforms
let blurHUniforms = null;
let blurVUniforms = null;
let sharpenUniforms = null;
let floodSeedUniforms = null;
let floodJFAUniforms = null;
let floodFillUniforms = null;
let shearUniforms = null;
let layerCompositeUniforms = null;

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
// INVERT SHADER - Simple RGB inversion (255 - value) while preserving alpha
// Uses gl_FragCoord and texelFetch for pixel-perfect addressing
// =========================================================================
const INVERT_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec4 u_bounds;  // minX, minY, maxX, maxY

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  int destX = int(gl_FragCoord.x);
  int destY = int(gl_FragCoord.y);
  
  int minX = int(u_bounds.x);
  int minY = int(u_bounds.y);
  int maxX = int(u_bounds.z);
  int maxY = int(u_bounds.w);
  
  // Check if outside working area
  if (destX < minX || destX >= maxX || destY < minY || destY >= maxY) {
    fragColor = texelFetch(u_texture, ivec2(destX, destY), 0);
    return;
  }
  
  vec4 color = texelFetch(u_texture, ivec2(destX, destY), 0);
  
  // Skip transparent pixels
  if (color.a == 0.0) {
    fragColor = color;
    return;
  }
  
  // Invert RGB channels (1.0 - value), preserve alpha
  fragColor = vec4(1.0 - color.r, 1.0 - color.g, 1.0 - color.b, color.a);
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

// =========================================================================
// SHEAR SHADER - KidPix-style row/column shifting
// Each row shifts horizontally based on distance from vertical center,
// each column shifts vertically based on distance from horizontal center.
// Wraps around within the working area bounds.
// =========================================================================
const SHEAR_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform vec4 u_bounds;  // minX, minY, maxX, maxY
uniform float u_shearX; // horizontal shear factor
uniform float u_shearY; // vertical shear factor

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

  // Outside working area — pass through
  if (destX < minX || destX >= maxX || destY < minY || destY >= maxY) {
    fragColor = texelFetch(u_texture, ivec2(destX, destY), 0);
    return;
  }

  int localX = destX - minX;
  int localY = destY - minY;

  // Apply horizontal shear (rows shift based on distance from vertical center)
  int srcLocalX = localX;
  if (u_shearX != 0.0) {
    float distFromCenter = float(localY) - float(boundsHeight) / 2.0;
    int rowShift = int(round(u_shearX * distFromCenter));
    srcLocalX = localX - rowShift;
    srcLocalX = ((srcLocalX % boundsWidth) + boundsWidth) % boundsWidth;
  }

  // Apply vertical shear (columns shift based on distance from horizontal center)
  int srcLocalY = localY;
  if (u_shearY != 0.0) {
    float distFromCenter = float(srcLocalX) - float(boundsWidth) / 2.0;
    int colShift = int(round(u_shearY * distFromCenter));
    srcLocalY = localY - colShift;
    srcLocalY = ((srcLocalY % boundsHeight) + boundsHeight) % boundsHeight;
  }

  int srcX = minX + srcLocalX;
  int srcY = minY + srcLocalY;

  fragColor = texelFetch(u_texture, ivec2(srcX, srcY), 0);
}`;

// =========================================================================
// FLOOD FILL SHADERS - Jump Flooding Algorithm (JFA)
// Three-pass algorithm:
// 1. Seed pass: Initialize distance field from seed point
// 2. JFA passes: Propagate nearest seed using halving step sizes (O(log n))
// 3. Fill pass: Apply fill color where distance < infinity and color matches
// =========================================================================

// Pass 1: Initialize seed - mark seed point with distance 0, others with infinity
const FLOOD_SEED_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;     // Original image
uniform vec2 u_resolution;
uniform vec2 u_seedPoint;        // Seed coordinate (x, y)
uniform vec4 u_targetColor;      // Color to match (RGBA normalized)
uniform float u_colorTolerance;  // Color matching tolerance

in vec2 v_texCoord;
out vec4 fragColor;

// Output encoding:
// RGB = nearest seed position (normalized 0-1)
// A = 1.0 if reachable (same color as target), 0.0 if not

bool colorsMatch(vec4 c1, vec4 c2, float tolerance) {
  return abs(c1.r - c2.r) <= tolerance &&
         abs(c1.g - c2.g) <= tolerance &&
         abs(c1.b - c2.b) <= tolerance &&
         abs(c1.a - c2.a) <= tolerance;
}

void main() {
  ivec2 coord = ivec2(gl_FragCoord.xy);
  vec4 sourceColor = texelFetch(u_texture, coord, 0);
  
  // Check if this pixel matches the target color
  bool matchesTarget = colorsMatch(sourceColor, u_targetColor, u_colorTolerance);
  
  // Check if this is the seed point
  bool isSeed = (coord.x == int(u_seedPoint.x) && coord.y == int(u_seedPoint.y));
  
  if (isSeed && matchesTarget) {
    // Seed point: store own position, mark as reachable
    fragColor = vec4(gl_FragCoord.xy / u_resolution, 0.0, 1.0);
  } else if (matchesTarget) {
    // Same color region: unknown seed, mark as reachable but infinite distance
    // Use -1,-1 to indicate "no seed found yet"
    fragColor = vec4(-1.0, -1.0, 1.0, 1.0);  // High distance placeholder
  } else {
    // Different color: boundary, not reachable
    fragColor = vec4(0.0, 0.0, 0.0, 0.0);
  }
}`;

// Pass 2: JFA propagation - find nearest seed using jump flooding
const FLOOD_JFA_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_seedMap;     // Current seed map from previous pass
uniform vec2 u_resolution;
uniform int u_stepSize;          // Current jump distance (halves each pass)

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  ivec2 coord = ivec2(gl_FragCoord.xy);
  vec4 current = texelFetch(u_seedMap, coord, 0);
  
  // Not reachable (different color) - pass through
  if (current.a == 0.0) {
    fragColor = current;
    return;
  }
  
  vec2 bestSeed = current.xy;
  float bestDist = 999999.0;
  
  // If we have a valid seed position, calculate its distance
  if (current.x >= 0.0) {
    vec2 seedPos = current.xy * u_resolution;
    bestDist = distance(vec2(coord), seedPos);
  }
  
  // Check 8 neighbors at current step size (+ self)
  for (int dy = -1; dy <= 1; dy++) {
    for (int dx = -1; dx <= 1; dx++) {
      ivec2 neighbor = coord + ivec2(dx, dy) * u_stepSize;
      
      // Bounds check
      if (neighbor.x < 0 || neighbor.y < 0 || 
          neighbor.x >= int(u_resolution.x) || neighbor.y >= int(u_resolution.y)) {
        continue;
      }
      
      vec4 neighborData = texelFetch(u_seedMap, neighbor, 0);
      
      // Skip if neighbor is not reachable or has no seed yet
      if (neighborData.a == 0.0 || neighborData.x < 0.0) continue;
      
      // Calculate distance from this pixel to neighbor's seed
      vec2 neighborSeed = neighborData.xy * u_resolution;
      float dist = distance(vec2(coord), neighborSeed);
      
      if (dist < bestDist) {
        bestDist = dist;
        bestSeed = neighborData.xy;
      }
    }
  }
  
  fragColor = vec4(bestSeed, 0.0, current.a);
}`;

// Pass 3: Apply fill color to all reachable pixels
const FLOOD_FILL_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_texture;     // Original image
uniform sampler2D u_seedMap;     // Final seed map from JFA
uniform vec2 u_resolution;
uniform vec4 u_fillColor;        // Color to fill with (RGBA normalized)

in vec2 v_texCoord;
out vec4 fragColor;

void main() {
  ivec2 coord = ivec2(gl_FragCoord.xy);
  vec4 original = texelFetch(u_texture, coord, 0);
  vec4 seedData = texelFetch(u_seedMap, coord, 0);
  
  // If reachable (alpha = 1) and has valid seed, fill with new color
  if (seedData.a > 0.5 && seedData.x >= 0.0) {
    fragColor = u_fillColor;
  } else {
    // Keep original color
    fragColor = original;
  }
}`;

// =========================================================================
// LAYER COMPOSITE SHADER - GPU-accelerated alpha blending for multiple layers
// Composites up to 8 layers in a single draw call
// =========================================================================
const LAYER_COMPOSITE_FRAGMENT_SHADER = `#version 300 es
precision highp float;

uniform sampler2D u_background;  // Background/destination buffer
uniform sampler2D u_layer0;      // Layer textures (up to 8)
uniform sampler2D u_layer1;
uniform sampler2D u_layer2;
uniform sampler2D u_layer3;
uniform sampler2D u_layer4;
uniform sampler2D u_layer5;
uniform sampler2D u_layer6;
uniform sampler2D u_layer7;

// Layer configuration arrays
uniform vec4 u_layerBounds[8];   // x, y, width, height for each layer
uniform float u_layerAlpha[8];   // Alpha multiplier (0-255) for each layer
uniform int u_layerCount;        // Number of active layers (1-8)
uniform vec2 u_resolution;       // Output resolution

in vec2 v_texCoord;
out vec4 fragColor;

// Alpha blend source over destination
vec4 blend(vec4 dst, vec4 src, float alphaMultiplier) {
  // Apply alpha multiplier to source alpha
  float srcAlpha = src.a * (alphaMultiplier / 255.0);
  
  // Skip fully transparent
  if (srcAlpha < 0.004) return dst;  // ~1/255
  
  // Standard alpha blending: result = src * srcA + dst * (1 - srcA)
  vec3 blended = src.rgb * srcAlpha + dst.rgb * (1.0 - srcAlpha);
  float outAlpha = srcAlpha + dst.a * (1.0 - srcAlpha);
  
  return vec4(blended, outAlpha);
}

// Sample from layer texture based on layer index
vec4 sampleLayer(int idx, ivec2 layerCoord) {
  if (idx == 0) return texelFetch(u_layer0, layerCoord, 0);
  if (idx == 1) return texelFetch(u_layer1, layerCoord, 0);
  if (idx == 2) return texelFetch(u_layer2, layerCoord, 0);
  if (idx == 3) return texelFetch(u_layer3, layerCoord, 0);
  if (idx == 4) return texelFetch(u_layer4, layerCoord, 0);
  if (idx == 5) return texelFetch(u_layer5, layerCoord, 0);
  if (idx == 6) return texelFetch(u_layer6, layerCoord, 0);
  if (idx == 7) return texelFetch(u_layer7, layerCoord, 0);
  return vec4(0.0);
}

void main() {
  ivec2 coord = ivec2(gl_FragCoord.xy);
  
  // Start with background color
  vec4 color = texelFetch(u_background, coord, 0);
  
  // Composite each layer in order (painter's algorithm)
  for (int i = 0; i < 8; i++) {
    if (i >= u_layerCount) break;
    
    vec4 bounds = u_layerBounds[i];
    float lx = bounds.x;
    float ly = bounds.y;
    float lw = bounds.z;
    float lh = bounds.w;
    
    // Check if this pixel is within the layer's bounds
    float px = float(coord.x);
    float py = float(coord.y);
    
    if (px >= lx && px < lx + lw && py >= ly && py < ly + lh) {
      // Calculate layer-local coordinates
      ivec2 layerCoord = ivec2(px - lx, py - ly);
      
      // Sample layer and blend
      vec4 layerColor = sampleLayer(i, layerCoord);
      color = blend(color, layerColor, u_layerAlpha[i]);
    }
  }
  
  fragColor = color;
}`;

let blurHProgram = null;
let blurVProgram = null;
let sharpenProgram = null;
let pingPongTexture = null;  // For multi-pass effects
let pingPongFramebuffer = null;

// Additional textures for flood fill JFA
let floodTexture1 = null;  // JFA ping texture
let floodTexture2 = null;  // JFA pong texture
let floodFramebuffer1 = null;
let floodFramebuffer2 = null;

// Layer composite textures (up to 8 layers + background)
let layerTextures = null;  // Array of textures [layer0, layer1, ..., layer7]
const MAX_LAYERS = 8;

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
  
  // Resize flood fill JFA textures (RGBA32F for position data)
  if (floodTexture1) {
    gl.bindTexture(gl.TEXTURE_2D, floodTexture1);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, height, 0, gl.RGBA, gl.FLOAT, null);
  }
  if (floodTexture2) {
    gl.bindTexture(gl.TEXTURE_2D, floodTexture2);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, height, 0, gl.RGBA, gl.FLOAT, null);
  }
  
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
    
    // Compile invert program
    const invertVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const invertFrag = compileShader(gl, gl.FRAGMENT_SHADER, INVERT_FRAGMENT_SHADER);
    if (!invertVert || !invertFrag) return false;
    
    invertProgram = gl.createProgram();
    gl.attachShader(invertProgram, invertVert);
    gl.attachShader(invertProgram, invertFrag);
    gl.linkProgram(invertProgram);
    if (!gl.getProgramParameter(invertProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Invert program link failed:', gl.getProgramInfoLog(invertProgram));
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

    // Compile shear program
    const shearVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const shearFrag = compileShader(gl, gl.FRAGMENT_SHADER, SHEAR_FRAGMENT_SHADER);
    if (!shearVert || !shearFrag) return false;

    shearProgram = gl.createProgram();
    gl.attachShader(shearProgram, shearVert);
    gl.attachShader(shearProgram, shearFrag);
    gl.linkProgram(shearProgram);
    if (!gl.getProgramParameter(shearProgram, gl.LINK_STATUS)) {
      console.error('🎮 GPU Effects: Shear program link failed:', gl.getProgramInfoLog(shearProgram));
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
    
    invertUniforms = {
      u_resolution: gl.getUniformLocation(invertProgram, 'u_resolution'),
      u_bounds: gl.getUniformLocation(invertProgram, 'u_bounds'),
      u_texture: gl.getUniformLocation(invertProgram, 'u_texture'),
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

    shearUniforms = {
      u_resolution: gl.getUniformLocation(shearProgram, 'u_resolution'),
      u_bounds: gl.getUniformLocation(shearProgram, 'u_bounds'),
      u_shearX: gl.getUniformLocation(shearProgram, 'u_shearX'),
      u_shearY: gl.getUniformLocation(shearProgram, 'u_shearY'),
      u_texture: gl.getUniformLocation(shearProgram, 'u_texture'),
    };

    // Compile flood fill programs (3 passes: seed, JFA, fill)
    const floodSeedVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const floodSeedFrag = compileShader(gl, gl.FRAGMENT_SHADER, FLOOD_SEED_FRAGMENT_SHADER);
    if (floodSeedVert && floodSeedFrag) {
      floodSeedProgram = gl.createProgram();
      gl.attachShader(floodSeedProgram, floodSeedVert);
      gl.attachShader(floodSeedProgram, floodSeedFrag);
      gl.linkProgram(floodSeedProgram);
      if (!gl.getProgramParameter(floodSeedProgram, gl.LINK_STATUS)) {
        console.warn('🎮 GPU Effects: Flood seed program link failed:', gl.getProgramInfoLog(floodSeedProgram));
        floodSeedProgram = null;
      }
    }
    
    const floodJFAVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const floodJFAFrag = compileShader(gl, gl.FRAGMENT_SHADER, FLOOD_JFA_FRAGMENT_SHADER);
    if (floodJFAVert && floodJFAFrag) {
      floodJFAProgram = gl.createProgram();
      gl.attachShader(floodJFAProgram, floodJFAVert);
      gl.attachShader(floodJFAProgram, floodJFAFrag);
      gl.linkProgram(floodJFAProgram);
      if (!gl.getProgramParameter(floodJFAProgram, gl.LINK_STATUS)) {
        console.warn('🎮 GPU Effects: Flood JFA program link failed:', gl.getProgramInfoLog(floodJFAProgram));
        floodJFAProgram = null;
      }
    }
    
    const floodFillVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const floodFillFrag = compileShader(gl, gl.FRAGMENT_SHADER, FLOOD_FILL_FRAGMENT_SHADER);
    if (floodFillVert && floodFillFrag) {
      floodFillProgram = gl.createProgram();
      gl.attachShader(floodFillProgram, floodFillVert);
      gl.attachShader(floodFillProgram, floodFillFrag);
      gl.linkProgram(floodFillProgram);
      if (!gl.getProgramParameter(floodFillProgram, gl.LINK_STATUS)) {
        console.warn('🎮 GPU Effects: Flood fill program link failed:', gl.getProgramInfoLog(floodFillProgram));
        floodFillProgram = null;
      }
    }
    
    // Cache flood fill uniform locations if programs compiled
    if (floodSeedProgram) {
      floodSeedUniforms = {
        u_texture: gl.getUniformLocation(floodSeedProgram, 'u_texture'),
        u_resolution: gl.getUniformLocation(floodSeedProgram, 'u_resolution'),
        u_seedPoint: gl.getUniformLocation(floodSeedProgram, 'u_seedPoint'),
        u_targetColor: gl.getUniformLocation(floodSeedProgram, 'u_targetColor'),
        u_colorTolerance: gl.getUniformLocation(floodSeedProgram, 'u_colorTolerance'),
      };
    }
    if (floodJFAProgram) {
      floodJFAUniforms = {
        u_seedMap: gl.getUniformLocation(floodJFAProgram, 'u_seedMap'),
        u_resolution: gl.getUniformLocation(floodJFAProgram, 'u_resolution'),
        u_stepSize: gl.getUniformLocation(floodJFAProgram, 'u_stepSize'),
      };
    }
    if (floodFillProgram) {
      floodFillUniforms = {
        u_texture: gl.getUniformLocation(floodFillProgram, 'u_texture'),
        u_seedMap: gl.getUniformLocation(floodFillProgram, 'u_seedMap'),
        u_resolution: gl.getUniformLocation(floodFillProgram, 'u_resolution'),
        u_fillColor: gl.getUniformLocation(floodFillProgram, 'u_fillColor'),
      };
    }
    
    // Compile layer composite shader
    const layerCompositeVert = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    const layerCompositeFrag = compileShader(gl, gl.FRAGMENT_SHADER, LAYER_COMPOSITE_FRAGMENT_SHADER);
    if (layerCompositeVert && layerCompositeFrag) {
      layerCompositeProgram = gl.createProgram();
      gl.attachShader(layerCompositeProgram, layerCompositeVert);
      gl.attachShader(layerCompositeProgram, layerCompositeFrag);
      gl.linkProgram(layerCompositeProgram);
      if (!gl.getProgramParameter(layerCompositeProgram, gl.LINK_STATUS)) {
        console.warn('🎮 GPU Effects: Layer composite program link failed:', gl.getProgramInfoLog(layerCompositeProgram));
        layerCompositeProgram = null;
      }
    }
    
    // Cache layer composite uniform locations
    if (layerCompositeProgram) {
      layerCompositeUniforms = {
        u_background: gl.getUniformLocation(layerCompositeProgram, 'u_background'),
        u_layer0: gl.getUniformLocation(layerCompositeProgram, 'u_layer0'),
        u_layer1: gl.getUniformLocation(layerCompositeProgram, 'u_layer1'),
        u_layer2: gl.getUniformLocation(layerCompositeProgram, 'u_layer2'),
        u_layer3: gl.getUniformLocation(layerCompositeProgram, 'u_layer3'),
        u_layer4: gl.getUniformLocation(layerCompositeProgram, 'u_layer4'),
        u_layer5: gl.getUniformLocation(layerCompositeProgram, 'u_layer5'),
        u_layer6: gl.getUniformLocation(layerCompositeProgram, 'u_layer6'),
        u_layer7: gl.getUniformLocation(layerCompositeProgram, 'u_layer7'),
        u_layerBounds: gl.getUniformLocation(layerCompositeProgram, 'u_layerBounds'),
        u_layerAlpha: gl.getUniformLocation(layerCompositeProgram, 'u_layerAlpha'),
        u_layerCount: gl.getUniformLocation(layerCompositeProgram, 'u_layerCount'),
        u_resolution: gl.getUniformLocation(layerCompositeProgram, 'u_resolution'),
      };
      
      // Create layer textures array
      layerTextures = [];
      for (let i = 0; i < MAX_LAYERS; i++) {
        const tex = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, tex);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        layerTextures.push(tex);
      }
      // console.log('🎮 GPU Layer Composite: Available (up to 8 layers)');
    }
    
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
    
    // Create flood fill JFA textures and framebuffers (RGBA32F for position data)
    // Only create if flood programs compiled successfully
    if (floodSeedProgram && floodJFAProgram && floodFillProgram) {
      // Check for float texture support
      const floatExt = gl.getExtension('EXT_color_buffer_float');
      if (floatExt) {
        floodTexture1 = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, floodTexture1);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, height, 0, gl.RGBA, gl.FLOAT, null);
        
        floodFramebuffer1 = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, floodFramebuffer1);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, floodTexture1, 0);
        
        floodTexture2 = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, floodTexture2);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, height, 0, gl.RGBA, gl.FLOAT, null);
        
        floodFramebuffer2 = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, floodFramebuffer2);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, floodTexture2, 0);
        
        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        // console.log('🎮 GPU Effects: Flood fill JFA initialized');
      } else {
        console.warn('🎮 GPU Effects: Float textures not supported, flood fill GPU disabled');
        floodSeedProgram = null;
        floodJFAProgram = null;
        floodFillProgram = null;
      }
    }
    
    // Pre-allocate readback buffer
    readbackBuffer = new Uint8Array(width * height * 4);
    
    lastWidth = width;
    lastHeight = height;
    initialized = true;
    
    // console.log(`🎮 GPU Effects: Initialized ${width}x${height}`);
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
 * GPU-accelerated contrast adjustment (uses composite shader with contrast only)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} level - Contrast level (1.0 = no change, >1 = more contrast, <1 = less)
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuContrast(pixels, width, height, level = 1.0, mask = null) {
  if (level === 1.0) return true;
  
  // Use composite shader with only contrast enabled
  return gpuComposite(pixels, width, height, {
    zoom: 1.0,
    zoomAnchorX: 0.5,
    zoomAnchorY: 0.5,
    scrollX: 0,
    scrollY: 0,
    contrast: level,
    brightness: 0,
    mask
  });
}

/**
 * GPU-accelerated brightness adjustment (uses composite shader with brightness only)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} adjustment - Brightness adjustment (-255 to +255, 0 = no change)
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuBrightness(pixels, width, height, adjustment = 0, mask = null) {
  if (adjustment === 0) return true;
  
  // Use composite shader with only brightness enabled
  return gpuComposite(pixels, width, height, {
    zoom: 1.0,
    zoomAnchorX: 0.5,
    zoomAnchorY: 0.5,
    scrollX: 0,
    scrollY: 0,
    contrast: 1.0,
    brightness: adjustment,
    mask
  });
}

/**
 * GPU-accelerated invert (255 - RGB value, preserve alpha)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuInvert(pixels, width, height, mask = null) {
  if (!initialized || !gl || !invertProgram) return false;
  
  try {
    ensureResources(width, height);
    
    // Upload texture (Y-flip happens here)
    uploadPixels(pixels, width, height);
    
    // Set up render target
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.viewport(0, 0, width, height);
    
    // Use invert program
    gl.useProgram(invertProgram);
    
    // Set uniforms
    gl.uniform2f(invertUniforms.u_resolution, width, height);
    
    // Set bounds (mask or full screen)
    const bounds = mask ? [mask.x, mask.y, mask.x + mask.width, mask.y + mask.height] : [0, 0, width, height];
    gl.uniform4f(invertUniforms.u_bounds, bounds[0], bounds[1], bounds[2], bounds[3]);
    
    // Bind texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.uniform1i(invertUniforms.u_texture, 0);
    
    // Draw
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back pixels (Y-flip happens here too)
    renderAndReadback(pixels, width, height);
    
    return true;
  } catch (e) {
    console.error('🎮 GPU Invert: Render failed:', e);
    return false;
  }
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
 * GPU-accelerated shear (KidPix-style row/column shifting)
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} shearX - Horizontal shear factor
 * @param {number} shearY - Vertical shear factor
 * @param {Object|null} mask - Optional mask bounds {x, y, width, height}
 * @returns {boolean}
 */
export function gpuShear(pixels, width, height, shearX = 0, shearY = 0, mask = null) {
  if (shearX === 0 && shearY === 0) return true;

  if (!initWebGL2(width, height)) {
    return false;
  }

  try {
    const minX = mask?.x ?? 0;
    const minY = mask?.y ?? 0;
    const maxX = mask ? mask.x + mask.width : width;
    const maxY = mask ? mask.y + mask.height : height;

    // Upload pixels
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);

    // Render to framebuffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.viewport(0, 0, width, height);

    gl.useProgram(shearProgram);

    // Set uniforms
    gl.uniform2f(shearUniforms.u_resolution, width, height);
    gl.uniform4f(shearUniforms.u_bounds, minX, minY, maxX, maxY);
    gl.uniform1f(shearUniforms.u_shearX, shearX);
    gl.uniform1f(shearUniforms.u_shearY, shearY);
    gl.uniform1i(shearUniforms.u_texture, 0);

    // Use VAO for efficient attribute binding
    gl.bindVertexArray(vao);

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);

    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // Read back pixels
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);

    gl.bindFramebuffer(gl.FRAMEBUFFER, null);

    return true;
  } catch (e) {
    console.error('🎮 GPU Shear: Render failed:', e);
    return false;
  }
}

/**
 * GPU-accelerated flood fill using Jump Flooding Algorithm (JFA)
 * O(log n) complexity vs O(n) for CPU scanline algorithm
 * @param {Uint8ClampedArray} pixels - Source/destination pixel buffer
 * @param {number} width - Buffer width
 * @param {number} height - Buffer height
 * @param {number} x - Seed point X coordinate
 * @param {number} y - Seed point Y coordinate
 * @param {Array} fillColor - Fill color [R, G, B, A] (0-255)
 * @returns {{success: boolean, area: number}} - Result with filled area count
 */
export function gpuFlood(pixels, width, height, x, y, fillColor) {
  // Check if flood fill programs are available
  if (!floodSeedProgram || !floodJFAProgram || !floodFillProgram || !floodTexture1 || !floodTexture2) {
    return { success: false, area: 0 };
  }
  
  // Bounds check
  if (x < 0 || y < 0 || x >= width || y >= height) {
    return { success: true, area: 0 };
  }
  
  // Initialize WebGL2 if needed
  if (!initWebGL2(width, height)) {
    return { success: false, area: 0 };
  }
  
  try {
    // Get target color at seed point (RGBA 0-255)
    const seedIdx = (y * width + x) * 4;
    const targetColor = [
      pixels[seedIdx] / 255,
      pixels[seedIdx + 1] / 255,
      pixels[seedIdx + 2] / 255,
      pixels[seedIdx + 3] / 255
    ];
    
    // Normalize fill color to 0-1
    const fillColorNorm = [
      fillColor[0] / 255,
      fillColor[1] / 255,
      fillColor[2] / 255,
      fillColor[3] / 255
    ];
    
    // If target is transparent, nothing to fill
    if (targetColor[3] === 0) {
      return { success: true, area: 0 };
    }
    
    // Upload source pixels to main texture (flip Y for WebGL)
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);
    
    // Flip Y coordinate for WebGL
    const seedY = height - 1 - y;
    
    // === PASS 1: Seed initialization ===
    gl.bindFramebuffer(gl.FRAMEBUFFER, floodFramebuffer1);
    gl.viewport(0, 0, width, height);
    gl.useProgram(floodSeedProgram);
    
    gl.uniform1i(floodSeedUniforms.u_texture, 0);
    gl.uniform2f(floodSeedUniforms.u_resolution, width, height);
    gl.uniform2f(floodSeedUniforms.u_seedPoint, x, seedY);
    gl.uniform4f(floodSeedUniforms.u_targetColor, targetColor[0], targetColor[1], targetColor[2], targetColor[3]);
    gl.uniform1f(floodSeedUniforms.u_colorTolerance, 0.004); // ~1/255 tolerance
    
    gl.bindVertexArray(vao);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // === PASS 2: JFA propagation passes ===
    // Number of passes = ceil(log2(max(width, height)))
    const maxDim = Math.max(width, height);
    const numPasses = Math.ceil(Math.log2(maxDim));
    
    let readTex = floodTexture1;
    let writeFB = floodFramebuffer2;
    let writeTex = floodTexture2;
    
    gl.useProgram(floodJFAProgram);
    gl.uniform2f(floodJFAUniforms.u_resolution, width, height);
    
    for (let pass = 0; pass < numPasses; pass++) {
      const stepSize = Math.pow(2, numPasses - 1 - pass);
      
      gl.bindFramebuffer(gl.FRAMEBUFFER, writeFB);
      gl.uniform1i(floodJFAUniforms.u_stepSize, stepSize);
      gl.uniform1i(floodJFAUniforms.u_seedMap, 0);
      
      gl.activeTexture(gl.TEXTURE0);
      gl.bindTexture(gl.TEXTURE_2D, readTex);
      gl.drawArrays(gl.TRIANGLES, 0, 6);
      
      // Swap buffers for next pass
      if (writeFB === floodFramebuffer2) {
        readTex = floodTexture2;
        writeFB = floodFramebuffer1;
        writeTex = floodTexture1;
      } else {
        readTex = floodTexture1;
        writeFB = floodFramebuffer2;
        writeTex = floodTexture2;
      }
    }
    
    // readTex now contains the final JFA result
    
    // === PASS 3: Apply fill color ===
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.useProgram(floodFillProgram);
    
    gl.uniform2f(floodFillUniforms.u_resolution, width, height);
    gl.uniform4f(floodFillUniforms.u_fillColor, fillColorNorm[0], fillColorNorm[1], fillColorNorm[2], fillColorNorm[3]);
    
    // Bind original texture to unit 0, seed map to unit 1
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.uniform1i(floodFillUniforms.u_texture, 0);
    
    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D, readTex);
    gl.uniform1i(floodFillUniforms.u_seedMap, 1);
    
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back pixels (flip Y)
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, readbackBuffer);
    
    // Count filled pixels and copy back with Y-flip
    let filledCount = 0;
    const rowSize = width * 4;
    for (let row = 0; row < height; row++) {
      const srcRow = (height - 1 - row) * rowSize;
      const dstRow = row * rowSize;
      
      for (let col = 0; col < width; col++) {
        const srcIdx = srcRow + col * 4;
        const dstIdx = dstRow + col * 4;
        
        // Check if pixel was filled (different from original)
        const wasFilled = (
          readbackBuffer[srcIdx] !== pixels[dstIdx] ||
          readbackBuffer[srcIdx + 1] !== pixels[dstIdx + 1] ||
          readbackBuffer[srcIdx + 2] !== pixels[dstIdx + 2]
        );
        if (wasFilled) filledCount++;
        
        pixels[dstIdx] = readbackBuffer[srcIdx];
        pixels[dstIdx + 1] = readbackBuffer[srcIdx + 1];
        pixels[dstIdx + 2] = readbackBuffer[srcIdx + 2];
        pixels[dstIdx + 3] = readbackBuffer[srcIdx + 3];
      }
    }
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.activeTexture(gl.TEXTURE0);
    
    return { success: true, area: filledCount };
  } catch (e) {
    console.error('🎮 GPU Flood: Render failed:', e);
    return { success: false, area: 0 };
  }
}

/**
 * Check if GPU flood fill is available
 */
export function isGpuFloodAvailable() {
  return !!(floodSeedProgram && floodJFAProgram && floodFillProgram && floodTexture1 && floodTexture2);
}

/**
 * Check if GPU layer compositing is available
 */
export function isGpuLayerCompositeAvailable() {
  return !!(layerCompositeProgram && layerTextures);
}

/**
 * GPU-accelerated multi-layer compositing
 * Composites up to 8 layers onto a background in a single GPU pass
 * 
 * @param {Uint8ClampedArray} backgroundPixels - Destination/background buffer
 * @param {number} width - Output width
 * @param {number} height - Output height  
 * @param {Array<{pixels: Uint8ClampedArray, x: number, y: number, width: number, height: number, alpha: number}>} layers - Array of layer objects
 * @returns {{success: boolean}} - Result
 */
export function gpuCompositeLayers(backgroundPixels, width, height, layers) {
  if (!layerCompositeProgram || !layerTextures || !layers || layers.length === 0) {
    return { success: false };
  }
  
  if (layers.length > MAX_LAYERS) {
    console.warn(`🎮 GPU Layer Composite: Too many layers (${layers.length}), max is ${MAX_LAYERS}`);
    return { success: false };
  }
  
  try {
    // Ensure canvas/resources match output size
    if (lastWidth !== width || lastHeight !== height) {
      resizeTextures(width, height);
      lastWidth = width;
      lastHeight = height;
    }
    
    gl.viewport(0, 0, width, height);
    gl.bindVertexArray(vao);
    
    // Upload background to main texture (with Y-flip for WebGL)
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    
    // Y-flip background pixels for WebGL
    const flippedBackground = new Uint8Array(backgroundPixels.length);
    const rowSize = width * 4;
    for (let row = 0; row < height; row++) {
      const srcRow = row * rowSize;
      const dstRow = (height - 1 - row) * rowSize;
      flippedBackground.set(backgroundPixels.subarray(srcRow, srcRow + rowSize), dstRow);
    }
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, flippedBackground);
    
    // Upload each layer texture (with Y-flip)
    const layerBounds = new Float32Array(MAX_LAYERS * 4);  // x, y, w, h for each
    const layerAlphas = new Float32Array(MAX_LAYERS);
    
    for (let i = 0; i < layers.length; i++) {
      const layer = layers[i];
      const tex = layerTextures[i];
      
      gl.activeTexture(gl.TEXTURE1 + i);
      gl.bindTexture(gl.TEXTURE_2D, tex);
      
      // Y-flip layer pixels
      const layerW = layer.width;
      const layerH = layer.height;
      const layerRowSize = layerW * 4;
      const flippedLayer = new Uint8Array(layer.pixels.length);
      for (let row = 0; row < layerH; row++) {
        const srcRow = row * layerRowSize;
        const dstRow = (layerH - 1 - row) * layerRowSize;
        flippedLayer.set(layer.pixels.subarray(srcRow, srcRow + layerRowSize), dstRow);
      }
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, layerW, layerH, 0, gl.RGBA, gl.UNSIGNED_BYTE, flippedLayer);
      
      // Store bounds (Y-flip the position too)
      const flippedY = height - layer.y - layerH;
      layerBounds[i * 4] = layer.x;
      layerBounds[i * 4 + 1] = flippedY;
      layerBounds[i * 4 + 2] = layerW;
      layerBounds[i * 4 + 3] = layerH;
      
      layerAlphas[i] = layer.alpha !== undefined ? layer.alpha : 255;
    }
    
    // Render to framebuffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.useProgram(layerCompositeProgram);
    
    // Set uniforms
    gl.uniform2f(layerCompositeUniforms.u_resolution, width, height);
    gl.uniform1i(layerCompositeUniforms.u_layerCount, layers.length);
    
    // Bind background texture
    gl.uniform1i(layerCompositeUniforms.u_background, 0);
    
    // Bind layer textures
    gl.uniform1i(layerCompositeUniforms.u_layer0, 1);
    gl.uniform1i(layerCompositeUniforms.u_layer1, 2);
    gl.uniform1i(layerCompositeUniforms.u_layer2, 3);
    gl.uniform1i(layerCompositeUniforms.u_layer3, 4);
    gl.uniform1i(layerCompositeUniforms.u_layer4, 5);
    gl.uniform1i(layerCompositeUniforms.u_layer5, 6);
    gl.uniform1i(layerCompositeUniforms.u_layer6, 7);
    gl.uniform1i(layerCompositeUniforms.u_layer7, 8);
    
    // Set layer bounds and alpha arrays
    gl.uniform4fv(layerCompositeUniforms.u_layerBounds, layerBounds);
    gl.uniform1fv(layerCompositeUniforms.u_layerAlpha, layerAlphas);
    
    // Draw
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    
    // Read back result (with Y-flip back to CPU coordinates)
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, readbackBuffer);
    
    for (let row = 0; row < height; row++) {
      const srcRow = (height - 1 - row) * rowSize;
      const dstRow = row * rowSize;
      for (let col = 0; col < rowSize; col++) {
        backgroundPixels[dstRow + col] = readbackBuffer[srcRow + col];
      }
    }
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.activeTexture(gl.TEXTURE0);
    
    return { success: true };
  } catch (e) {
    console.error('🎮 GPU Layer Composite: Render failed:', e);
    return { success: false };
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
    if (invertProgram) gl.deleteProgram(invertProgram);
    if (blurHProgram) gl.deleteProgram(blurHProgram);
    if (blurVProgram) gl.deleteProgram(blurVProgram);
    if (sharpenProgram) gl.deleteProgram(sharpenProgram);
    if (shearProgram) gl.deleteProgram(shearProgram);
    // Flood fill resources
    if (floodSeedProgram) gl.deleteProgram(floodSeedProgram);
    if (floodJFAProgram) gl.deleteProgram(floodJFAProgram);
    if (floodFillProgram) gl.deleteProgram(floodFillProgram);
    if (floodTexture1) gl.deleteTexture(floodTexture1);
    if (floodTexture2) gl.deleteTexture(floodTexture2);
    if (floodFramebuffer1) gl.deleteFramebuffer(floodFramebuffer1);
    if (floodFramebuffer2) gl.deleteFramebuffer(floodFramebuffer2);
    // Layer composite resources
    if (layerCompositeProgram) gl.deleteProgram(layerCompositeProgram);
    if (layerTextures) {
      layerTextures.forEach(tex => gl.deleteTexture(tex));
      layerTextures = null;
    }
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
  gpuContrast,
  gpuBrightness,
  gpuInvert,
  gpuBlur,
  gpuSharpen,
  gpuShear,
  gpuFlood,
  gpuCompositeLayers,
  isGpuEffectsAvailable,
  isGpuFloodAvailable,
  isGpuLayerCompositeAvailable,
  resetAccumulators,
  cleanupGpuEffects 
};
