// Metaballs, 2025.6.05.19.30.22.187
// A basic metaballs sample.

/* 📝 Engineering Notes
  The `paint` function runs every animation frame.
  `screen.pixels` is a Uint8ClampedArray with direct access.
  `screen.width` and `screen.height` is also available for aspect ratio / limits.
  Special note: `Use screen.pixels / direct pixel array access for any automated drawing.`
*/

// Metaballs configuration
let balls = [];
let time = 0;
let noiseTexture = null;
let wipeProgress = 0; // For doom-style screen wipe effect
let wipeDirection = 1; // 1 for left-to-right, -1 for right-to-left
let screenBuffer = null; // Buffer to store captured screen for wipe effect
let wipeColumns = []; // Pre-calculated wipe pattern for horizontal
let wipeRows = []; // Pre-calculated wipe pattern for vertical
let tempBuffer = null; // Temporary buffer for two-pass wipe effect
let rowSpeeds = []; // Speed multipliers for each row
let colSpeeds = []; // Speed multipliers for each column

function boot() {
  // Initialize 13 metaballs with varied properties
  balls = [
    // Original triangle metaballs
    { x: 150, y: 100, radius: 30, speedX: 1.5, speedY: 1.0, color: [255, 100, 100] },
    { x: 100, y: 200, radius: 25, speedX: -1.2, speedY: -1.3, color: [100, 255, 100] },
    { x: 250, y: 180, radius: 28, speedX: -1.0, speedY: 1.2, color: [100, 100, 255] },
    
    // Additional 10 metaballs with varied colors and properties
    { x: 320, y: 120, radius: 22, speedX: 0.8, speedY: -1.5, color: [255, 255, 100] },
    { x: 80, y: 80, radius: 35, speedX: -0.9, speedY: 1.1, color: [255, 100, 255] },
    { x: 280, y: 60, radius: 18, speedX: 1.8, speedY: 0.7, color: [100, 255, 255] },
    { x: 200, y: 300, radius: 26, speedX: -1.4, speedY: -0.8, color: [255, 150, 50] },
    { x: 50, y: 250, radius: 32, speedX: 1.3, speedY: -1.0, color: [150, 255, 150] },
    { x: 350, y: 250, radius: 20, speedX: -1.6, speedY: 1.4, color: [255, 50, 150] },
    { x: 180, y: 50, radius: 24, speedX: 0.6, speedY: 1.7, color: [50, 150, 255] },
    { x: 300, y: 320, radius: 29, speedX: -0.7, speedY: -1.2, color: [200, 100, 200] },
    { x: 120, y: 320, radius: 21, speedX: 1.1, speedY: -1.6, color: [100, 200, 100] },
    { x: 380, y: 180, radius: 27, speedX: -1.9, speedY: 0.9, color: [255, 200, 100] }
  ];
    // Generate 16x16 checkerboard texture
  generateNoiseTexture();
  
  // Initialize screen buffer and wipe pattern
  screenBuffer = null; // Will be initialized when we know screen size
}

function paint({ screen }) {
  const { width, height, pixels } = screen;
  time += 0.02;
  
  // Update ball positions
  for (let ball of balls) {
    ball.x += ball.speedX;
    ball.y += ball.speedY;
    
    // Bounce off edges
    if (ball.x - ball.radius < 0 || ball.x + ball.radius > width) {
      ball.speedX *= -1;
    }
    if (ball.y - ball.radius < 0 || ball.y + ball.radius > height) {
      ball.speedY *= -1;
    }
    
    // Keep balls within bounds
    ball.x = Math.max(ball.radius, Math.min(width - ball.radius, ball.x));
    ball.y = Math.max(ball.radius, Math.min(height - ball.radius, ball.y));  }
    // Render every pixel for smoother results
  const step = 1; // Full resolution to avoid banding artifacts
  for (let y = 0; y < height; y += step) {
    for (let x = 0; x < width; x += step) {
      let totalWeight = 0;
      let r = 0, g = 0, b = 0;
        // Calculate inverse distance weights with noise and gritty effects
        for (let ball of balls) {
          const dx = x - ball.x;
          const dy = y - ball.y;
          // Use squared distance to avoid expensive sqrt
          const distanceSquared = dx * dx + dy * dy;          // Improved pseudo-random noise to reduce banding patterns
          const seed1 = Math.sin(x * 0.01234 + y * 0.05678 + time * 12.34) * 0.5 + 0.5;
          const seed2 = Math.sin(x * 0.03456 + y * 0.01789 + time * 23.45) * 0.5 + 0.5;
          
          // Convert to -1 to 1 range and make it choppy
          const noiseX = (seed1 - 0.5) * 4;
          const noiseY = (seed2 - 0.5) * 4;
          
          const noisyDx = dx + noiseX;
          const noisyDy = dy + noiseY;
          const noisyDistanceSquared = noisyDx * noisyDx + noisyDy * noisyDy;
          
          // Prevent division by zero
          const safeDistanceSquared = Math.max(noisyDistanceSquared, 1);
          
          // Simple turbulence without patterns
          const turbulence = 1 + (seed1 - 0.5) * 0.3;
          
          // Simplified inverse distance weighting with noise modulation
          let weight = (1.0 / safeDistanceSquared) * turbulence;
            // Add organic flickering for grittiness without regular patterns
          const flicker = 0.9 + Math.sin(x * 0.0789 + y * 0.0456 + time * 7.89) * 0.1;
          weight *= flicker;
            // Boost core areas with irregular boundaries
          const coreRadius = ball.radius * (1 + Math.sin(time * 3 + ball.x * 0.01) * 0.2);
          if (noisyDistanceSquared < coreRadius * coreRadius) {
            weight *= 4.0; // Simple 4x boost for cores
          }
          
          totalWeight += weight;
          r += ball.color[0] * weight;
          g += ball.color[1] * weight;
          b += ball.color[2] * weight;
        }
        
        // Normalize colors and add some overall noise
        r = Math.min(255, r / totalWeight);
        g = Math.min(255, g / totalWeight);
        b = Math.min(255, b / totalWeight);
          // Add subtle organic color variation without banding
        const colorNoise = Math.sin(x * 0.0234 + y * 0.0345 + time * 8.91) * 10;
        r = Math.max(0, Math.min(255, r + colorNoise));
        g = Math.max(0, Math.min(255, g + colorNoise * 0.7));
        b = Math.max(0, Math.min(255, b + colorNoise * 1.1));
      }
        // Fill the current pixel (no block filling needed with step=1)
      const pixelIndex = (y * width + x) * 4;
      pixels[pixelIndex] = r;     // Red
      pixels[pixelIndex + 1] = g; // Green
      pixels[pixelIndex + 2] = b; // Blue
      pixels[pixelIndex + 3] = 255;    // Alpha
    }  }  
  // Apply doom-style screen wipe effect
  applyDoomWipeEffect(pixels, width, height);
}

// Helper function to check if a point is inside a triangle using barycentric coordinates
function pointInTriangle(px, py, a, b, c) {
  // Calculate vectors
  const v0x = c.x - a.x;
  const v0y = c.y - a.y;
  const v1x = b.x - a.x;
  const v1y = b.y - a.y;
  const v2x = px - a.x;
  const v2y = py - a.y;
  
  // Calculate dot products
  const dot00 = v0x * v0x + v0y * v0y;
  const dot01 = v0x * v1x + v0y * v1y;
  const dot02 = v0x * v2x + v0y * v2y;
  const dot11 = v1x * v1x + v1y * v1y;
  const dot12 = v1x * v2x + v1y * v2y;
  
  // Calculate barycentric coordinates
  const invDenom = 1 / (dot00 * dot11 - dot01 * dot01);
  const u = (dot11 * dot02 - dot01 * dot12) * invDenom;
  const v = (dot00 * dot12 - dot01 * dot02) * invDenom;
  
  // Check if point is in triangle
  return (u >= 0) && (v >= 0) && (u + v <= 1);
}

// Helper function to get barycentric coordinates for texture mapping
function getBarycentricCoords(px, py, a, b, c) {
  const denom = (b.y - c.y) * (a.x - c.x) + (c.x - b.x) * (a.y - c.y);
  const u = ((b.y - c.y) * (px - c.x) + (c.x - b.x) * (py - c.y)) / denom;
  const v = ((c.y - a.y) * (px - c.x) + (a.x - c.x) * (py - c.y)) / denom;
  const w = 1 - u - v;
  return [u, v, w];
}

// Helper function to draw lines between metaball centers
function drawConnectionLines(pixels, width, height) {
  // Draw lines connecting each ball to every other ball
  for (let i = 0; i < balls.length; i++) {
    for (let j = i + 1; j < balls.length; j++) {
      const ball1 = balls[i];
      const ball2 = balls[j];
      
      // Draw line from ball1 to ball2
      drawLine(pixels, width, height, 
        Math.round(ball1.x), Math.round(ball1.y),
        Math.round(ball2.x), Math.round(ball2.y),
        [255, 255, 255, 128] // White with some transparency
      );
    }
  }
}

// Bresenham's line algorithm for drawing lines
function drawLine(pixels, width, height, x0, y0, x1, y1, color) {
  const dx = Math.abs(x1 - x0);
  const dy = Math.abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;
  
  let x = x0;
  let y = y0;
  
  while (true) {
    // Check bounds
    if (x >= 0 && x < width && y >= 0 && y < height) {
      const pixelIndex = (y * width + x) * 4;
      
      // Blend the line color with existing pixel
      const alpha = color[3] / 255;
      const invAlpha = 1 - alpha;
      
      pixels[pixelIndex] = pixels[pixelIndex] * invAlpha + color[0] * alpha;
      pixels[pixelIndex + 1] = pixels[pixelIndex + 1] * invAlpha + color[1] * alpha;
      pixels[pixelIndex + 2] = pixels[pixelIndex + 2] * invAlpha + color[2] * alpha;
      // Alpha channel stays 255
    }
    
    if (x === x1 && y === y1) break;
    
    const e2 = 2 * err;
    if (e2 > -dy) {
      err -= dy;
      x += sx;
    }
    if (e2 < dx) {
      err += dx;
      y += sy;
    }
  }
}

// Set pixel color with bounds checking
function setPixelColor(pixels, width, height, x, y, color) {
  // Check bounds
  if (x < 0 || x >= width || y < 0 || y >= height) {
    return;
  }
  
  const pixelIndex = (y * width + x) * 4;
  
  // Set color values
  pixels[pixelIndex] = color[0];     // Red
  pixels[pixelIndex + 1] = color[1]; // Green
  pixels[pixelIndex + 2] = color[2]; // Blue
  pixels[pixelIndex + 3] = 255;      // Alpha
}

// Generate 16x16 checkerboard texture for triangle mapping
function generateNoiseTexture() {
  const size = 16;
  noiseTexture = new Array(size * size);
  
  // Create a simple checkerboard pattern
  for (let y = 0; y < size; y++) {
    for (let x = 0; x < size; x++) {
      // Checkerboard pattern: alternating 0 and 1 based on sum of coordinates
      const isWhite = (x + y) % 2 === 0;
      noiseTexture[y * size + x] = isWhite ? 1.0 : 0.0;
    }
  }
}

// Doom-style screen wipe effect with both horizontal and vertical passes
function applyDoomWipeEffect(pixels, width, height) {
  // Initialize screen buffer and wipe patterns if needed
  if (!screenBuffer || screenBuffer.length !== pixels.length) {
    screenBuffer = new Uint8ClampedArray(pixels.length);
    tempBuffer = new Uint8ClampedArray(pixels.length);
    
    // Copy current pixels to buffer immediately for seamless start
    for (let i = 0; i < pixels.length; i++) {
      screenBuffer[i] = pixels[i];
    }
    
    // Initialize horizontal wipe pattern (for rows)
    wipeColumns = [];
    rowSpeeds = [];
    for (let y = 0; y < height; y++) {
      const seed = Math.sin(y * 0.1) * 1000;
      const randomWidth = Math.abs(Math.sin(seed) * width * 0.3);
      wipeColumns[y] = Math.floor(randomWidth);      // Each row gets its own speed multiplier (0.95x to 1.05x base speed - ultra subtle variation)
      const speedSeed = Math.sin(y * 0.073 + 123.456) * 1000;
      rowSpeeds[y] = 0.95 + Math.abs(Math.sin(speedSeed)) * 0.1;
    }
    
    // Initialize vertical wipe pattern (for columns)
    wipeRows = [];
    colSpeeds = [];
    for (let x = 0; x < width; x++) {
      const seed = Math.sin(x * 0.1) * 1000;
      const randomHeight = Math.abs(Math.sin(seed) * height * 0.3);
      wipeRows[x] = Math.floor(randomHeight);      // Each column gets its own speed multiplier (0.97x to 1.03x base speed - ultra subtle variation)
      const speedSeed = Math.sin(x * 0.089 + 789.123) * 1000;
      colSpeeds[x] = 0.97 + Math.abs(Math.sin(speedSeed)) * 0.06;
    }
  }  // Update wipe progress continuously (glacial crawl speed)
  wipeProgress += 0.0003; // Ultra slow base speed for barely perceptible movement
  
  // Always capture current frame to buffer (continuous capture)
  for (let i = 0; i < pixels.length; i++) {
    screenBuffer[i] = pixels[i];
  }
    // PASS 1: Horizontal sliding wipe effect with per-row speed variation
  for (let y = 0; y < height; y++) {    // Fully linear wipe effect from top to bottom
    const rowWipePos = (wipeProgress * width) % (width * 2);
    const normalizedY = y / height;
    let slideProgress = (rowWipePos / width) - normalizedY * 0.3; // Linear delay based on row position
    
    slideProgress = slideProgress % 1;
    if (slideProgress < 0) slideProgress += 1;
      const baseSlide = slideProgress * width;
    const slideDistance = Math.floor(baseSlide); // Removed per-row randomness for pure linear effect
    
    // Copy row from buffer to temp buffer with horizontal wrapping and pixel skipping
    for (let x = 0; x < width; x++) {
      const sourceX = x;
      let destX = (x + slideDistance) % width;
      if (destX < 0) destX += width;
      
      const sourceIndex = (y * width + sourceX) * 4;
      const destIndex = (y * width + destX) * 4;        // Reduced pixel skipping for more visible morphing (skip 2 out of 3 pixels)
      const skipPattern = (x + y * 2) % 3;
      if (skipPattern === 0) {
        // Only apply wipe effect to 1 in 3 pixels for more visible morphing
        tempBuffer[destIndex] = screenBuffer[sourceIndex];
        tempBuffer[destIndex + 1] = screenBuffer[sourceIndex + 1];
        tempBuffer[destIndex + 2] = screenBuffer[sourceIndex + 2];
        tempBuffer[destIndex + 3] = screenBuffer[sourceIndex + 3];
      } else {
        // Keep original pixel (no wipe effect)
        tempBuffer[destIndex] = screenBuffer[sourceIndex];
        tempBuffer[destIndex + 1] = screenBuffer[sourceIndex + 1];
        tempBuffer[destIndex + 2] = screenBuffer[sourceIndex + 2];
        tempBuffer[destIndex + 3] = screenBuffer[sourceIndex + 3];
      }
    }  }
  
  // Copy temp buffer to final pixels (horizontal pass only)
  for (let i = 0; i < pixels.length; i++) {
    pixels[i] = tempBuffer[i];
  }
  
  /* PASS 2: Vertical sliding wipe effect with per-column speed variation - TEMPORARILY DISABLED
  for (let x = 0; x < width; x++) {    // Each column moves at its own speed
    const colWipePos = (wipeProgress * colSpeeds[x] * height * 0.3) % (height * 2); // Ultra slow vertical
    const normalizedX = x / width;
    let slideProgress = (colWipePos / height) - normalizedX * 0.25;
    
    slideProgress = slideProgress % 1;
    if (slideProgress < 0) slideProgress += 1;
    
    const baseSlide = slideProgress * height;
    const slideDistance = Math.floor(baseSlide + wipeRows[x]);
    
    // Copy column from temp buffer to final pixels with vertical wrapping and pixel skipping
    for (let y = 0; y < height; y++) {
      const sourceY = y;
      let destY = (y + slideDistance) % height;
      if (destY < 0) destY += height;
      
      const sourceIndex = (sourceY * width + x) * 4;
      const destIndex = (destY * width + x) * 4;
        // Skip more pixels to make vertical pass glacially subtle (skip 5 out of 6 pixels)
      const skipPattern = (x * 2 + y) % 6;
      if (skipPattern === 0) {
        // Keep the horizontally-wiped pixel (from temp buffer) without vertical wipe
        pixels[destIndex] = tempBuffer[sourceIndex];
        pixels[destIndex + 1] = tempBuffer[sourceIndex + 1];
        pixels[destIndex + 2] = tempBuffer[sourceIndex + 2];
        pixels[destIndex + 3] = 255;
      } else {
        // Apply vertical wipe effect
        pixels[destIndex] = tempBuffer[sourceIndex];
        pixels[destIndex + 1] = tempBuffer[sourceIndex + 1];
        pixels[destIndex + 2] = tempBuffer[sourceIndex + 2];
        pixels[destIndex + 3] = 255; // Alpha
      }
    }
  }
  */
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function act({ event: e }) {
//  // Respond to user input here.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
