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
        b = Math.min(255, b / totalWeight);        // Add subtle organic color variation with enhanced effects
        const colorNoise = Math.sin(x * 0.0234 + y * 0.0345 + time * 8.91) * 10;
        r = Math.max(0, Math.min(255, r + colorNoise));
        g = Math.max(0, Math.min(255, g + colorNoise * 0.7));
        b = Math.max(0, Math.min(255, b + colorNoise * 1.1));
        
        // Add pixel-level effects
        const pixelEffect = Math.sin(x * 0.05 + y * 0.07 + time * 15) * 0.5 + 0.5;
        
        // Random pixel brightness variation
        if (pixelEffect > 0.9) {
          const brightness = 1.1 + Math.sin(time * 20 + x + y) * 0.1;
          r = Math.min(255, r * brightness);
          g = Math.min(255, g * brightness);
          b = Math.min(255, b * brightness);
        }
        
        // Add dithering effect for retro look
        const ditherPattern = ((x + y) % 2) * 0.05;
        r = Math.max(0, Math.min(255, r - ditherPattern * 20));
        g = Math.max(0, Math.min(255, g - ditherPattern * 15));
        b = Math.max(0, Math.min(255, b - ditherPattern * 25));
      
        // Fill the current pixel (no block filling needed with step=1)
      const pixelIndex = (y * width + x) * 4;pixels[pixelIndex] = r;     // Red
      pixels[pixelIndex + 1] = g; // Green
      pixels[pixelIndex + 2] = b; // Blue
      pixels[pixelIndex + 3] = 255;    // Alpha
    }
  }  
  // Apply doom-style screen wipe effect
  applyDoomWipeEffect(pixels, width, height);
  
  // Add occasional glitch effect
  applyGlitchEffect(pixels, width, height);
  
  // Draw connection lines between metaball centers
  drawConnectionLines(pixels, width, height);
  
  // Draw triangles using the first 3 metaballs as vertices
  if (balls.length >= 3) {
    drawTriangle(pixels, width, height, balls[0], balls[1], balls[2]);
  }
}

// Draw a triangle connecting three metaballs with enhanced effects
function drawTriangle(pixels, width, height, ball1, ball2, ball3) {
  // Animated color for triangle edges
  const timeColor = (Math.sin(time * 2) * 0.5 + 0.5) * 255;
  const color = [255, timeColor, 255 - timeColor, 255]; 
  
  // Draw lines between each pair of vertices with rounded coordinates
  drawLine(pixels, width, height, 
    Math.round(ball1.x), Math.round(ball1.y), 
    Math.round(ball2.x), Math.round(ball2.y), color);
  drawLine(pixels, width, height, 
    Math.round(ball2.x), Math.round(ball2.y), 
    Math.round(ball3.x), Math.round(ball3.y), color);
  drawLine(pixels, width, height, 
    Math.round(ball3.x), Math.round(ball3.y), 
    Math.round(ball1.x), Math.round(ball1.y), color);
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

// Helper function to draw lines between metaball centers with effects
function drawConnectionLines(pixels, width, height) {
  // Only draw lines between the first 3 balls to avoid performance issues
  const maxBalls = Math.min(3, balls.length);
  for (let i = 0; i < maxBalls; i++) {
    for (let j = i + 1; j < maxBalls; j++) {
      const ball1 = balls[i];
      const ball2 = balls[j];
      
      // Animate line opacity based on distance
      const dx = ball2.x - ball1.x;
      const dy = ball2.y - ball1.y;
      const distance = Math.sqrt(dx * dx + dy * dy);
      const maxDistance = 200;
      const opacity = Math.max(50, 255 - (distance / maxDistance) * 200);
      
      // Pulse effect
      const pulse = Math.sin(time * 3 + i + j) * 0.3 + 0.7;
      const finalOpacity = Math.min(255, opacity * pulse);
      
      // Draw line from ball1 to ball2 with animated color
      const lineColor = [
        Math.round(255 * (0.7 + Math.sin(time * 2 + i) * 0.3)),
        Math.round(255 * (0.7 + Math.sin(time * 2.5 + j) * 0.3)),
        255,
        finalOpacity
      ];
      
      drawLine(pixels, width, height, 
        Math.round(ball1.x), Math.round(ball1.y),
        Math.round(ball2.x), Math.round(ball2.y),
        lineColor
      );
    }
  }
}

// Bresenham's line algorithm for drawing lines with safety checks
function drawLine(pixels, width, height, x0, y0, x1, y1, color) {
  // Ensure coordinates are integers and within bounds
  x0 = Math.max(0, Math.min(width - 1, Math.round(x0)));
  y0 = Math.max(0, Math.min(height - 1, Math.round(y0)));
  x1 = Math.max(0, Math.min(width - 1, Math.round(x1)));
  y1 = Math.max(0, Math.min(height - 1, Math.round(y1)));
  
  const dx = Math.abs(x1 - x0);
  const dy = Math.abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;
  
  let x = x0;
  let y = y0;
  
  while (true) {
    // Double check bounds before writing pixel
    if (x >= 0 && x < width && y >= 0 && y < height) {
      const pixelIndex = (y * width + x) * 4;
      
      // Simplified color blending to avoid potential issues
      if (color.length >= 4) {
        const alpha = color[3] / 255;
        const invAlpha = 1 - alpha;
        
        pixels[pixelIndex] = Math.round(pixels[pixelIndex] * invAlpha + color[0] * alpha);
        pixels[pixelIndex + 1] = Math.round(pixels[pixelIndex + 1] * invAlpha + color[1] * alpha);
        pixels[pixelIndex + 2] = Math.round(pixels[pixelIndex + 2] * invAlpha + color[2] * alpha);
      } else {
        // Fallback: solid color
        pixels[pixelIndex] = color[0] || 255;
        pixels[pixelIndex + 1] = color[1] || 255;
        pixels[pixelIndex + 2] = color[2] || 255;
      }
      pixels[pixelIndex + 3] = 255; // Always set alpha to fully opaque
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

// Random glitch effect for added visual interest
function applyGlitchEffect(pixels, width, height) {
  const currentTime = Date.now() * 0.001;
  const glitchIntensity = Math.sin(currentTime * 0.5) * 0.5 + 0.5;
  
  // Only apply glitch occasionally
  if (glitchIntensity > 0.8) {
    const glitchRows = Math.floor(height * 0.1); // Affect 10% of rows
    
    for (let i = 0; i < glitchRows; i++) {
      const randomY = Math.floor(Math.random() * height);
      const shift = Math.floor((Math.random() - 0.5) * 20); // Random horizontal shift
      
      // Shift pixels in this row
      for (let x = 0; x < width; x++) {
        const sourceX = Math.max(0, Math.min(width - 1, x + shift));
        const sourceIndex = (randomY * width + sourceX) * 4;
        const destIndex = (randomY * width + x) * 4;
        
        if (sourceIndex < pixels.length - 3 && destIndex < pixels.length - 3) {
          // Apply color channel separation for RGB glitch effect
          pixels[destIndex] = pixels[sourceIndex + ((i % 3) * 4)]; // Offset red
          pixels[destIndex + 1] = pixels[sourceIndex + 1]; // Keep green
          pixels[destIndex + 2] = pixels[sourceIndex + 2 + ((i % 2) * 4)]; // Offset blue
          pixels[destIndex + 3] = 255;
        }
      }
    }
  }
}

// Enhanced doom-style screen wipe effect with pixel effects
function applyDoomWipeEffect(pixels, width, height) {
  const currentTime = Date.now() * 0.001;
  
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const index = (y * width + x) * 4;
      
      // Create gentle wave patterns focused on speed rather than distortion
      const waveX = Math.sin((x + currentTime * 60) * 0.008) * 3;  
      const waveY = Math.sin((y + currentTime * 45) * 0.012) * 2; 
      
      // Combine waves for gentle effect
      const totalWave = waveX + waveY;
      const wipePosition = (y + totalWave) % height;
      
      // Create a moving wipe effect with different speeds in different regions
      const speedVariation = 1 + Math.sin(x * 0.005) * 0.2; 
      const wipeThreshold = (currentTime * 25 * speedVariation) % height;
      
      // Add pixel effects based on position and time
      const pixelSeed = x * 0.1 + y * 0.13 + currentTime * 5;
      const pixelNoise = Math.sin(pixelSeed) * 0.5 + 0.5;
      
      if (wipePosition < wipeThreshold) {
        // Apply enhanced color effects to wiped areas
        const enhancement = 1 + pixelNoise * 0.1;
        pixels[index] = Math.min(255, pixels[index] * (1.03 * enhancement));     
        pixels[index + 1] = Math.min(255, pixels[index + 1] * (1.01 * enhancement)); 
        pixels[index + 2] = Math.min(255, pixels[index + 2] * (1.05 * enhancement)); 
        
        // Add scanline effect
        if (y % 2 === 0 && pixelNoise > 0.7) {
          pixels[index] = Math.min(255, pixels[index] * 1.2);
          pixels[index + 1] = Math.min(255, pixels[index + 1] * 0.9);
          pixels[index + 2] = Math.min(255, pixels[index + 2] * 1.1);
        }
        
        // Add random pixel corruption effect
        if (pixelNoise > 0.95) {
          const corruption = Math.sin(currentTime * 20 + x + y) * 50;
          pixels[index] = Math.max(0, Math.min(255, pixels[index] + corruption));
          pixels[index + 1] = Math.max(0, Math.min(255, pixels[index + 1] + corruption * 0.8));
          pixels[index + 2] = Math.max(0, Math.min(255, pixels[index + 2] + corruption * 1.2));
        }
      } else {
        // Add subtle flickering to non-wiped areas
        if (pixelNoise > 0.8) {
          const flicker = 0.95 + Math.sin(currentTime * 15 + x * 0.1 + y * 0.1) * 0.05;
          pixels[index] = Math.round(pixels[index] * flicker);
          pixels[index + 1] = Math.round(pixels[index + 1] * flicker);
          pixels[index + 2] = Math.round(pixels[index + 2] * flicker);
        }
      }
      
      // Add occasional pixel shift effect
      if (pixelNoise > 0.98 && x > 2 && x < width - 2) {
        const shiftDirection = Math.sin(currentTime * 10 + x + y) > 0 ? 1 : -1;
        const sourceIndex = (y * width + (x + shiftDirection * 2)) * 4;
        if (sourceIndex >= 0 && sourceIndex < pixels.length - 3) {
          pixels[index] = pixels[sourceIndex];
          pixels[index + 1] = pixels[sourceIndex + 1];
          pixels[index + 2] = pixels[sourceIndex + 2];
        }
      }
    }
  }
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }
