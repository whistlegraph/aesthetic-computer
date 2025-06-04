// Frac, 2025.6.04.01.26.48.500
// Make fractals.

/* 📝 Engineering Notes
  The `paint` function runs every animation frame.
  `screen.pixels` is a Uint8ClampedArray with direct access.
  `screen.width` and `screen.height` is also available for aspect ratio / limits.
   Special note: `Use screen.pixels / direct pixel array access for any automated drawing.`
   For reference: See other disks in `system/public/aesthetic.computer/disks
*/

function paint({ wipe, screen, write }) {
  // Wiggle control flag - set to true for subtle living effects
  const enableWiggling = true;
  // Animation parameters
  const time = performance.now() * 0.001;

  // Multiple interesting zoom points - skip the boring main body center
  const zoomPoints = [
    { x: -0.77568377, y: 0.13646737 }, // Deep zoom point with infinite detail
    { x: -0.7269, y: 0.1889 }, // Misiurewicz point
    { x: -0.163176, y: 1.041198 }, // Spiral formation
    { x: -1.25066, y: 0.02012 }, // Mini mandelbrot
  ];  // Cycle through zoom points every 120 seconds (60 in, 60 out)
  const pointCycleTime = 120; // Doubled to allow for yoyo motion
  const currentPointIndex =
    Math.floor(time / pointCycleTime) % zoomPoints.length;
  const cycleProgress = (time % pointCycleTime) / pointCycleTime;
  
  // Yoyo progress: 0->1->0 over the cycle using triangle wave
  const pointProgress = cycleProgress <= 0.5 
    ? cycleProgress * 2  // First half: 0 to 1
    : 2 - (cycleProgress * 2); // Second half: 1 to 0

  const deepZoomPoint = zoomPoints[currentPointIndex]; 
  // Yoyo zoom progression - zooms in then back out smoothly
  const maxZoomDepth = 8.0; // Increased from 3.0 to 8.0 for deeper zoom
  const zoomLevel = pointProgress * maxZoomDepth; // Triangle wave zoom progression

  // Debug: Print zoom level
  write(
    `Zoom: ${zoomLevel.toFixed(2)} | Point: ${currentPointIndex} | Progress: ${(pointProgress * 100).toFixed(1)}%`,
    6,
    18,
  );  // Simplified iteration count for faster rendering
  const baseIterations = 150; // Further reduced for speed
  const maxIterations = Math.min(
    300, // Much lower ceiling for speed
    baseIterations + Math.floor(zoomLevel * 6), // Reduced scaling
  );
  // Screen parameters with cute 3D depth! 🌀
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const zoom = Math.pow(2, zoomLevel);
  const pixelSize = 4.0 / (zoom * Math.min(screen.width, screen.height));
  
  // Adorable 3D depth animation - breathing z-axis motion ✨
  const zDepth = Math.sin(time * 0.3) * 0.5; // Oscillating z-depth
  const zSliceSpeed = time * 0.1; // Slow drift through z-space
  const baseZ = zSliceSpeed + zDepth; // Combined z-axis movement// Cute organic wiggling - bringing back the life! 🌊
  const baseWiggleAmplitude = 0.0008; // A bit more lively
  const wiggleAmplitude = baseWiggleAmplitude / Math.pow(zoom, 0.7); 

  // Cute dual-layer wiggling with personality
  const wigglePhase = time * 0.8;
  const primaryWiggle = Math.sin(wigglePhase * 0.3) + 0.3 * Math.cos(wigglePhase * 0.7);
  const cuteWiggle = 0.2 * Math.sin(wigglePhase * 1.2) + 0.15 * Math.cos(wigglePhase * 0.5);
  
  // Adorable breathing motion
  const breathingAmplitude = 1 + 0.05 * Math.sin(time * 0.4);
  const wiggleX = wiggleAmplitude * breathingAmplitude * (primaryWiggle + cuteWiggle);
  const wiggleY = wiggleAmplitude * breathingAmplitude * (Math.cos(wigglePhase * 0.35) + 0.2 * cuteWiggle);  // Apply optional wiggle to the zoom point for dynamic center shifting
  const wiggledZoomPoint = {
    x: deepZoomPoint.x + (enableWiggling ? wiggleX : 0),
    y: deepZoomPoint.y + (enableWiggling ? wiggleY : 0),
  };

  // Cache expensive calculations outside the pixel loop
  const rotationAngle = time * 0.02;
  const cosTheta = Math.cos(rotationAngle);
  const sinTheta = Math.sin(rotationAngle);

  // Cute 3D complexity sampling with depth! 🎭
  function calculateComplexity(x, y) {
    // Quick 3D complexity sample with z-axis
    const sampleReal = (x - centerX) * pixelSize;
    const sampleImag = (y - centerY) * pixelSize;
    const sampleZ = baseZ * 0.3; // Use current z-depth for sampling
    
    const rotatedReal = sampleReal * cosTheta - sampleImag * sinTheta;
    const rotatedImag = sampleReal * sinTheta + sampleImag * cosTheta;
    
    const cReal = wiggledZoomPoint.x + rotatedReal;
    const cImag = wiggledZoomPoint.y + rotatedImag;
    const cZ = sampleZ; // Third dimension
    
    // 3D Mandelbrot-style iteration (simplified Mandelbulb approach)
    let zReal = 0, zImag = 0, zZ = 0;
    let iterations = 0;
    const quickMaxIter = 15; // Reduced for performance
    
    while (iterations < quickMaxIter) {
      const r = Math.sqrt(zReal * zReal + zImag * zImag + zZ * zZ);
      if (r > 2) break;
        // Simplified 3D transformation (power-2 approximation)
      const theta = Math.atan2(zImag, zReal) * 2;
      const phi = r > 0 ? Math.asin(Math.max(-1, Math.min(1, zZ / r))) * 2 : 0;
      const r2 = r * r;
      
      zReal = r2 * Math.cos(theta) * Math.cos(phi) + cReal;
      zImag = r2 * Math.sin(theta) * Math.cos(phi) + cImag;
      zZ = r2 * Math.sin(phi) + cZ;
      
      iterations++;
    }    
    return iterations / quickMaxIter; // Return complexity ratio 0-1
  }

  // Adaptive block sizing with complexity sampling
  for (let blockY = 0; blockY < screen.height; blockY += 1) {
    for (let blockX = 0; blockX < screen.width; blockX += 1) {
      // Sample complexity at this position to determine block size
      const complexity = calculateComplexity(blockX, blockY);
        // Cute adaptive block size with personality! 🎭
      // Larger blocks for complex areas (for speed), but with cute variations
      let adaptiveBlockSize;
      if (zoomLevel > 5.0) {
        adaptiveBlockSize = 1; // Always crisp at very high zoom
      } else if (complexity > 0.85) {
        // Very complex areas get cute variation between 3-4
        adaptiveBlockSize = 3 + Math.floor(Math.sin(blockX * 0.1 + blockY * 0.08 + time) * 0.5 + 0.5);
      } else if (complexity > 0.6) {
        // Medium complexity with cute 2-3 variation  
        adaptiveBlockSize = 2 + Math.floor(Math.sin(blockX * 0.15 + time * 0.5) * 0.5 + 0.5);
      } else if (complexity > 0.3) {
        // Some detail areas with occasional 2x2
        adaptiveBlockSize = 1 + Math.floor(Math.sin(blockX * 0.2 + blockY * 0.12) * 0.3 + 0.7);      } else {
        adaptiveBlockSize = 1; // Keep crisp edges for simple areas
      }
      
      // Skip if we're not at a block boundary
      if (blockX % adaptiveBlockSize !== 0 || blockY % adaptiveBlockSize !== 0) {
        continue;
      }
      
      // Calculate once per block at center position
      const centerX_block = blockX + Math.floor(adaptiveBlockSize / 2);
      const centerY_block = blockY + Math.floor(adaptiveBlockSize / 2);      // Calculate delta from reference point with cached rotation (using block center)
      const baseReal = (centerX_block - centerX) * pixelSize;
      const baseImag = (centerY_block - centerY) * pixelSize;
      
      // Use pre-calculated rotation values
      const rotatedReal = baseReal * cosTheta - baseImag * sinTheta;
      const rotatedImag = baseReal * sinTheta + baseImag * cosTheta;      // Cute per-pixel wiggling with 3D spatial personality 🎨
      let pixelWiggleX = 0, pixelWiggleY = 0, pixelWiggleZ = 0;
      if (enableWiggling) {
        const spatialPhase = centerX_block * 0.008 + centerY_block * 0.006 + baseZ * 0.5 + time * 0.3;
        const pixelPersonality = Math.sin(centerX_block * 0.02) * Math.cos(centerY_block * 0.015) * Math.sin(baseZ * 2);
        
        pixelWiggleX = wiggleAmplitude * 0.4 * (
          Math.sin(spatialPhase) + 
          0.3 * Math.cos(spatialPhase * 1.7) +
          0.1 * pixelPersonality
        );
        pixelWiggleY = wiggleAmplitude * 0.4 * (
          Math.cos(spatialPhase * 1.1) + 
          0.25 * Math.sin(spatialPhase * 0.8) +
          0.12 * pixelPersonality
        );
        pixelWiggleZ = wiggleAmplitude * 0.2 * Math.sin(spatialPhase * 0.6 + baseZ); // Z-wiggle!
      }

      // Final 3D coordinates for this pixel! 🌀
      const cReal = wiggledZoomPoint.x + rotatedReal + pixelWiggleX;
      const cImag = wiggledZoomPoint.y + rotatedImag + pixelWiggleY;
      const cZ = baseZ + pixelWiggleZ; // Third dimension with wiggle      // Cute 3D Mandelbrot calculation (simplified Mandelbulb) ✨
      // Temporarily using 2D to debug the black screen issue
      let zReal = 0.0;
      let zImag = 0.0;
      let iterations = 0;
      let magnitude = 0.0;
      let escaped = false;

      // Standard 2D Mandelbrot for debugging
      while (iterations < maxIterations && !escaped) {
        magnitude = zReal * zReal + zImag * zImag;
        
        if (magnitude > 4.0) {
          escaped = true;
          break;
        }
        
        const newReal = zReal * zReal - zImag * zImag + cReal;
        const newImag = 2 * zReal * zImag + cImag;
          zReal = newReal;
        zImag = newImag;
        iterations++;
      }

      // Debug: Print some values to see what's happening
      if (blockX === 100 && blockY === 100) {
        write(`Complex: ${complexity.toFixed(3)} | Block: ${adaptiveBlockSize}`, 6, 36);
        write(`Iter: ${iterations} | Escaped: ${escaped} | Mag: ${magnitude.toFixed(3)}`, 6, 54);
      }

      // Simplified escape test with faster coloring
      if (!escaped || iterations >= maxIterations - 1) {        // Cute interior coloring with depth and warmth 🌺
        const proximityValue = Math.min(Math.sqrt(magnitude) * 0.4, 1.0);
        const depthFactor = Math.min(iterations / (maxIterations * 0.7), 1.0);
        
        // Adorable color personality based on position and time
        const colorPersonality = Math.sin(centerX_block * 0.03 + time * 0.1) * 
                                Math.cos(centerY_block * 0.025 + time * 0.08);
        
        // Cute hue shifts with breathing motion
        const baseHue = 200 + Math.sin(time * 0.05) * 40; // Gentle breathing hue
        const proximityHue = proximityValue * 50 + colorPersonality * 20;
        const depthHue = depthFactor * 30;
        
        const hue = (baseHue + proximityHue + depthHue + zoomLevel * 8) % 360;
        const saturation = 0.65 + proximityValue * 0.25 + Math.abs(colorPersonality) * 0.1;
        const lightness = 0.25 + proximityValue * 0.35 + depthFactor * 0.2 + Math.sin(time * 0.12) * 0.05;
        // Fast HSL to RGB conversion
        const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
        const x_hsl = c * (1 - Math.abs(((hue / 60) % 2) - 1));
        const m = lightness - c / 2;
        
        let r, g, b;
        const hueSegment = Math.floor(hue / 60) % 6;
        switch (hueSegment) {
          case 0: r = c; g = x_hsl; b = 0; break;
          case 1: r = x_hsl; g = c; b = 0; break;
          case 2: r = 0; g = c; b = x_hsl; break;
          case 3: r = 0; g = x_hsl; b = c; break;
          case 4: r = x_hsl; g = 0; b = c; break;
          default: r = c; g = 0; b = x_hsl; break;
        }
        
        // Calculate final color values once
        const red = Math.floor(255 * Math.max(0, Math.min(1, r + m)));
        const green = Math.floor(255 * Math.max(0, Math.min(1, g + m)));
        const blue = Math.floor(255 * Math.max(0, Math.min(1, b + m)));
        
        // Fill the entire adaptive block with this color
        for (let fillY = blockY; fillY < Math.min(blockY + adaptiveBlockSize, screen.height); fillY++) {
          for (let fillX = blockX; fillX < Math.min(blockX + adaptiveBlockSize, screen.width); fillX++) {
            const fillIndex = (fillY * screen.width + fillX) * 4;
            screen.pixels[fillIndex] = red;
            screen.pixels[fillIndex + 1] = green;
            screen.pixels[fillIndex + 2] = blue;
            screen.pixels[fillIndex + 3] = 255; // Alpha
          }
        }      } else {        // Cute exterior coloring with sparkly personality ✨
        const smoothIter = iterations + 1 - Math.log2(Math.log2(magnitude));
        const t = smoothIter / maxIterations;
        
        // Adorable sparkly color shifts
        const sparkle = Math.sin(smoothIter * 0.5 + time * 2) * 0.3;
        const personalityHue = Math.sin(centerX_block * 0.02) * Math.cos(centerY_block * 0.018);
        
        // Cute rainbow cycling with personality
        const baseHue = (t * 240 + time * 8 + zoomLevel * 12) % 360;
        const sparklyHue = sparkle * 60 + personalityHue * 40;
        const hue = (baseHue + sparklyHue) % 360;
        
        // Lively saturation and lightness
        const saturation = 0.75 + t * 0.2 + Math.abs(sparkle) * 0.15;
        const lightness = 0.35 + t * 0.4 + sparkle * 0.1 + Math.sin(time * 0.15) * 0.05;
        
        // Fast HSL to RGB conversion
        const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
        const x_hsl = c * (1 - Math.abs(((hue / 60) % 2) - 1));
        const m = lightness - c / 2;
        
        let r, g, b;
        const hueSegment = Math.floor(hue / 60) % 6;
        switch (hueSegment) {
          case 0: r = c; g = x_hsl; b = 0; break;
          case 1: r = x_hsl; g = c; b = 0; break;
          case 2: r = 0; g = c; b = x_hsl; break;
          case 3: r = 0; g = x_hsl; b = c; break;
          case 4: r = x_hsl; g = 0; b = c; break;
          default: r = c; g = 0; b = x_hsl; break;
        }
        
        // Calculate final color values once
        const red = Math.floor(255 * Math.max(0, Math.min(1, r + m)));
        const green = Math.floor(255 * Math.max(0, Math.min(1, g + m)));
        const blue = Math.floor(255 * Math.max(0, Math.min(1, b + m)));
        
        // Fill the entire adaptive block with this color
        for (let fillY = blockY; fillY < Math.min(blockY + adaptiveBlockSize, screen.height); fillY++) {
          for (let fillX = blockX; fillX < Math.min(blockX + adaptiveBlockSize, screen.width); fillX++) {
            const fillIndex = (fillY * screen.width + fillX) * 4;
            screen.pixels[fillIndex] = red;
            screen.pixels[fillIndex + 1] = green;
            screen.pixels[fillIndex + 2] = blue;
            screen.pixels[fillIndex + 3] = 255; // Alpha
          }
        }
      }
    }
  }
}

// 📚 Library

function boot({ resolution }) {
  // resolution(256);
  // Runs once at the start.
}

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
