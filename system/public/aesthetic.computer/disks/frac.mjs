// Frac Optimized, 2025.6.04.01.26.48.500
// High-performance audio-reactive fractal visualizations.

let sfx, sfxData, progress, playingSfx;

export async function boot({ net: { preload }, sound, params }) {
  // Load audio track based on parameters
  if (params[0] === "zzzZWAP") {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/zzzZWAP.ogg");
  } else if (params[0] === "WHoOSH") {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/WHoOSH.ogg");
  } else if (params[0] === "BLURP") {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/BLURP.ogg");
  } else {
    sfx = await preload("startup");
  }

  sound.getSampleData(sfx).then((data) => {
    sfxData = data;
    console.log("🎵 Audio data loaded for fractal:", sfxData?.length, "samples");
  });
}

export function paint({ wipe, screen, write, sound }) {
  // Wiggle control flag - set to true for subtle living effects
  const enableWiggling = true;
  
  // Animation parameters
  const time = performance.now() * 0.001;
  
  // 🎵 Audio-reactive parameters with enhanced sensitivity
  const audioAmplitude = sound.speaker?.amplitudes?.left || 0;
  const audioWaveform = sound.speaker?.waveforms?.left || [];
  
  // Enhanced frequency analysis with subtle sensitivity
  const bassLevel = audioWaveform.slice(0, 8).reduce((a, b) => a + Math.abs(b), 0) / 8;
  const midLevel = audioWaveform.slice(8, 24).reduce((a, b) => a + Math.abs(b), 0) / 16;
  const trebleLevel = audioWaveform.slice(24).reduce((a, b) => a + Math.abs(b), 0) / (audioWaveform.length - 24);
  
  // Subtle sensitivity for gentle visual effects
  const bass = Math.min(bassLevel * 2, 1);           // Much more gentle
  const mid = Math.min(midLevel * 2.5, 1);           // Reduced sensitivity  
  const treble = Math.min(trebleLevel * 3, 1);       // Less aggressive
  const overallAmplitude = Math.min(audioAmplitude * 2, 1); // Much more subtle
  
  // Audio-driven dynamic effects - gentle and smooth
  const audioPulse = Math.pow(overallAmplitude, 0.8); // Gentler pulse response
  const bassBoost = Math.pow(bass, 0.6) * 0.5;        // Much more subtle bass
  const trebleSparkle = treble * 0.3;                 // Linear, gentle treble

  // Multiple interesting zoom points organized by audio characteristics
  const zoomPoints = [
    { x: -0.77568377, y: 0.13646737 }, // Deep zoom point with infinite detail
    { x: -0.7269, y: 0.1889 }, // Misiurewicz point
    { x: -0.163176, y: 1.041198 }, // Spiral formation
    { x: -1.25066, y: 0.02012 }, // Mini mandelbrot
    { x: -0.8, y: 0.156 }, // Another interesting region
    { x: -0.74529, y: 0.11307 }, // Seahorse valley
    { x: -1.78, y: 0.0 }, // Filament region
    { x: -0.16, y: 1.03 }, // Spiral zoom
  ];
  
  // Audio-reactive view switching with thresholds
  const audioThresholds = {
    high: 0.7,    // Switch rapidly on high amplitude
    medium: 0.4,  // Medium speed switching 
    low: 0.2      // Slow switching
  };
  
  // Determine switching speed based on audio levels
  let switchingSpeed = 1.0; // Base speed multiplier
  let targetPointIndex = 0;
  
  if (overallAmplitude > audioThresholds.high) {
    // High energy - rapid switching every 10 seconds
    switchingSpeed = 12.0;
    // Bass-driven point selection for high energy
    targetPointIndex = Math.floor(bass * zoomPoints.length);
  } else if (overallAmplitude > audioThresholds.medium) {
    // Medium energy - switch every 20 seconds  
    switchingSpeed = 6.0;
    // Mid-driven point selection
    targetPointIndex = Math.floor(mid * zoomPoints.length);
  } else if (overallAmplitude > audioThresholds.low) {
    // Low-medium energy - switch every 40 seconds
    switchingSpeed = 3.0;
    // Treble-driven point selection
    targetPointIndex = Math.floor(treble * zoomPoints.length);
  } else {
    // Very low energy - use normal slow cycle (120 seconds)
    switchingSpeed = 1.0;
    targetPointIndex = Math.floor(time / 120) % zoomPoints.length;
  }
  
  // Audio-reactive cycle time with dramatic speed changes
  const dynamicCycleTime = 120 / switchingSpeed;
  const audioReactiveIndex = Math.floor(time / dynamicCycleTime) % zoomPoints.length;
  
  // Blend between audio-reactive selection and time-based cycling
  const finalPointIndex = overallAmplitude > audioThresholds.low 
    ? Math.max(0, Math.min(zoomPoints.length - 1, targetPointIndex))
    : audioReactiveIndex;
    
  const cycleProgress = (time % dynamicCycleTime) / dynamicCycleTime;
  
  // Yoyo progress: 0->1->0 over the cycle using triangle wave
  const pointProgress = cycleProgress <= 0.5 
    ? cycleProgress * 2  // First half: 0 to 1
    : 2 - (cycleProgress * 2); // Second half: 1 to 0

  const deepZoomPoint = zoomPoints[finalPointIndex]; 
  
  // Audio-reactive zoom depth - faster zooms on high energy
  const baseMaxZoomDepth = 8.0;
  const audioZoomBoost = Math.min(overallAmplitude * 4, 3.0); // Up to 3x deeper zoom
  const maxZoomDepth = baseMaxZoomDepth + audioZoomBoost;
  const zoomLevel = pointProgress * maxZoomDepth;
  
  // Debug display with audio-reactive info
  write(`Zoom: ${zoomLevel.toFixed(2)} | Point: ${finalPointIndex} | Cycle: ${(dynamicCycleTime).toFixed(1)}s | Speed: ${switchingSpeed.toFixed(1)}x`, 6, 18);
  write(`🎵 Bass: ${(bass * 100).toFixed(0)}% | Mid: ${(mid * 100).toFixed(0)}% | Treble: ${(treble * 100).toFixed(0)}% | Amp: ${(overallAmplitude * 100).toFixed(0)}%`, 6, 30);
  write(`🔊 Pulse: ${(audioPulse * 100).toFixed(0)}% | BassBoost: ${(bassBoost * 100).toFixed(0)}% | Sparkle: ${(trebleSparkle * 100).toFixed(0)}%`, 6, 42);
  
  // Audio status
  const audioStatus = playingSfx ? "🔊 Playing" : (sfx ? "⏸️ Touch to play" : "⏳ Loading...");
  write(audioStatus, 6, 54);

  // Gentle iteration count for faster rendering - subtly audio reactive
  const baseIterations = 150 + Math.floor(bassBoost * 50);  // Much less variation
  const maxIterations = Math.min(
    250 + Math.floor(mid * 100),                            // Reduced range
    baseIterations + Math.floor(zoomLevel * 6) + Math.floor(trebleSparkle * 30),
  );
  
  // Screen parameters with cute 3D depth! 🌀
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const zoom = Math.pow(2, zoomLevel);
  const pixelSize = 4.0 / (zoom * Math.min(screen.width, screen.height));
  
  // Adorable 3D depth animation - breathing z-axis motion ✨ + DRAMATICALLY geometric reactive
  const zDepth = Math.sin(time * 0.3) * 0.5 + bassBoost * 2.0;        // Much stronger bass geometric response
  const zSliceSpeed = time * 0.1 + mid * 0.4;                         // Increased mid geometric response
  const baseZ = zSliceSpeed + zDepth;

  // Cute organic wiggling - bringing back the life! 🌊 + DRAMATICALLY geometric reactive
  const baseWiggleAmplitude = 0.0008 + trebleSparkle * 0.012;          // More dramatic treble wiggling
  const wiggleAmplitude = baseWiggleAmplitude / Math.pow(zoom, 0.7);
  
  // Cute dual-layer wiggling with personality + dramatic geometric reactive
  const wigglePhase = time * 0.8 + audioPulse * 12;                    // Much stronger pulse geometric impact
  const primaryWiggle = Math.sin(wigglePhase * 0.3) + 0.3 * Math.cos(wigglePhase * 0.7);
  const cuteWiggle = 0.2 * Math.sin(wigglePhase * 1.2) + 0.15 * Math.cos(wigglePhase * 0.5);
  
  // Adorable breathing motion + DRAMATICALLY bass geometric reactive
  const breathingAmplitude = 1 + 0.05 * Math.sin(time * 0.4) + bassBoost * 0.8;  // Much stronger bass breathing
  const wiggleX = wiggleAmplitude * breathingAmplitude * (primaryWiggle + cuteWiggle);
  const wiggleY = wiggleAmplitude * breathingAmplitude * (Math.cos(wigglePhase * 0.35) + 0.2 * cuteWiggle);

  // Apply optional wiggle to the zoom point for dynamic center shifting
  const wiggledZoomPoint = {
    x: deepZoomPoint.x + (enableWiggling ? wiggleX : 0),
    y: deepZoomPoint.y + (enableWiggling ? wiggleY : 0),
  };

  // Cache expensive calculations outside the pixel loop with DRAMATIC audio-reactive rotation
  const rotationAngle = time * 0.02 + bassBoost * 0.5 + audioPulse * 0.3;  // Much more dramatic audio rotation
  const cosTheta = Math.cos(rotationAngle);
  const sinTheta = Math.sin(rotationAngle);

  // 🚀 HIGH-PERFORMANCE HIERARCHICAL BLOCK RENDERING 🚀
  renderFractalOptimized();

  function renderFractalOptimized() {
    // High-performance rendering with hierarchical subdivision
    const maxBlockSize = Math.max(4, Math.floor(16 + bass * 48 + overallAmplitude * 24));
    const minBlockSize = Math.max(1, Math.floor(1 + trebleSparkle * 3));
    
    // Fast point sampling with caching
    const sampleCache = new Map();
    
    function samplePoint(pixelX, pixelY) {
      const key = `${pixelX},${pixelY}`;
      if (sampleCache.has(key)) return sampleCache.get(key);
      
      // Convert to complex plane coordinates
      const baseReal = (pixelX - centerX) * pixelSize;
      const baseImag = (pixelY - centerY) * pixelSize;
      
      // DRAMATIC audio-reactive coordinate transformations
      const scaleFactorX = 1.0 + bassBoost * 0.6;           // Bass stretches X axis
      const scaleFactorY = 1.0 + mid * 0.4;                 // Mid stretches Y axis
      const skewFactor = trebleSparkle * 0.3;               // Treble adds skew
      
      // Apply audio-reactive scaling and skewing
      const scaledReal = baseReal * scaleFactorX + baseImag * skewFactor;
      const scaledImag = baseImag * scaleFactorY;
      
      // Apply rotation
      const rotatedReal = scaledReal * cosTheta - scaledImag * sinTheta;
      const rotatedImag = scaledReal * sinTheta + scaledImag * cosTheta;
      
      // Apply per-pixel wiggling
      let pixelWiggleX = 0, pixelWiggleY = 0;
      if (enableWiggling) {
        const spatialPhase = pixelX * 0.008 + pixelY * 0.006 + baseZ * 0.5 + time * 0.3;
        const pixelPersonality = Math.sin(pixelX * 0.02) * Math.cos(pixelY * 0.015);
        
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
      }

      // Final coordinates
      const cReal = wiggledZoomPoint.x + rotatedReal + pixelWiggleX;
      const cImag = wiggledZoomPoint.y + rotatedImag + pixelWiggleY;
      
      // Fast Mandelbrot calculation
      let zReal = 0.0, zImag = 0.0;
      let iterations = 0;
      let magnitude = 0.0;
      
      // Audio-reactive iteration boost - gentler now
      const audioIterationBoost = Math.floor(bass * 75 + mid * 75 + trebleSparkle * 25);  // Much reduced
      const dynamicMaxIterations = Math.max(50, maxIterations + audioIterationBoost);
      
      while (iterations < dynamicMaxIterations) {
        magnitude = zReal * zReal + zImag * zImag;
        if (magnitude > 4.0) break;
        
        const newReal = zReal * zReal - zImag * zImag + cReal;
        const newImag = 2 * zReal * zImag + cImag;
        zReal = newReal;
        zImag = newImag;
        iterations++;
      }
      
      const result = { 
        iterations, 
        magnitude, 
        escaped: magnitude > 4.0,
        maxIterations: dynamicMaxIterations 
      };
      
      sampleCache.set(key, result);
      return result;
    }
    
    // Fast color calculation
    function calculateColor(sample, pixelX, pixelY) {
      const { iterations, magnitude, escaped, maxIterations: dynMaxIter } = sample;
      
      if (!escaped || iterations >= dynMaxIter - 1) {
        // Interior coloring
        const proximityValue = Math.min(Math.sqrt(magnitude) * 0.4, 1.0);
        const depthFactor = Math.min(iterations / (dynMaxIter * 0.7), 1.0);
        const colorPersonality = Math.sin(pixelX * 0.03 + time * 0.1) * Math.cos(pixelY * 0.025 + time * 0.08);
        
        const baseHue = 220 + Math.sin(time * 0.05) * 20 + bassBoost * 10;  // Blue-focused base hue
        const proximityHue = proximityValue * 25 + colorPersonality * 10 + audioPulse * 8;  // Reduced hue variation
        const depthHue = depthFactor * 15 + trebleSparkle * 5;  // Minimal hue shifts
        
        const hue = Math.max(190, Math.min(270, baseHue + proximityHue + depthHue + zoomLevel * 4)); // Constrain to blue range
        const saturation = Math.min(0.65 + proximityValue * 0.25 + Math.abs(colorPersonality) * 0.1 + audioPulse * 0.05, 1.0);  // Minimal saturation changes
        const lightness = Math.min(0.25 + proximityValue * 0.35 + depthFactor * 0.2 + Math.sin(time * 0.12) * 0.05 + bassBoost * 0.05, 0.9);  // Minimal lightness changes
        
        return hslToRgb(hue, saturation, lightness);
      } else {
        // Exterior coloring
        const smoothIter = iterations + 1 - Math.log2(Math.log2(magnitude));
        const t = smoothIter / dynMaxIter;
        
        const sparkle = Math.sin(smoothIter * 0.5 + time * 2) * 0.3 + trebleSparkle * 0.1;  // Much subtler treble sparkle
        const personalityHue = Math.sin(pixelX * 0.02) * Math.cos(pixelY * 0.018) + bassBoost * 0.1;  // Reduced bass personality
        
        const baseHue = 210 + time * 4 + zoomLevel * 6 + audioPulse * 15; // Blue-focused exterior
        const sparklyHue = sparkle * 30 + personalityHue * 20 + mid * 10;  // Reduced variation
        const hue = Math.max(180, Math.min(280, baseHue + sparklyHue)); // Constrain to blue-cyan range
        
        const saturation = Math.min(0.75 + t * 0.2 + Math.abs(sparkle) * 0.15 + audioPulse * 0.05, 1.0);  // Minimal audio saturation
        const lightness = Math.min(0.35 + t * 0.4 + sparkle * 0.1 + Math.sin(time * 0.15) * 0.05 + bassBoost * 0.03, 0.95);  // Minimal bass lightness
        
        return hslToRgb(hue, saturation, lightness);
      }
    }
    
    // Fast HSL to RGB conversion
    function hslToRgb(hue, saturation, lightness) {
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
      
      return {
        r: Math.floor(255 * Math.max(0, Math.min(1, r + m))),
        g: Math.floor(255 * Math.max(0, Math.min(1, g + m))),
        b: Math.floor(255 * Math.max(0, Math.min(1, b + m)))
      };
    }
    
    // Optimized block filling with typed array access
    function fillBlock(x, y, size, color) {
      const endX = Math.min(x + size, screen.width);
      const endY = Math.min(y + size, screen.height);
      
      for (let fillY = y; fillY < endY; fillY++) {
        const rowStart = fillY * screen.width * 4;
        for (let fillX = x; fillX < endX; fillX++) {
          const fillIndex = rowStart + fillX * 4;
          screen.pixels[fillIndex] = color.r;
          screen.pixels[fillIndex + 1] = color.g;
          screen.pixels[fillIndex + 2] = color.b;
          screen.pixels[fillIndex + 3] = 255;
        }
      }
    }
    
    // Hierarchical block subdivision with queue
    const renderQueue = [];
    
    // Start with large blocks
    for (let blockY = 0; blockY < screen.height; blockY += maxBlockSize) {
      for (let blockX = 0; blockX < screen.width; blockX += maxBlockSize) {
        const blockSize = Math.min(maxBlockSize, 
          Math.min(screen.width - blockX, screen.height - blockY));
        renderQueue.push({ x: blockX, y: blockY, size: blockSize });
      }
    }
    
    // Process render queue with subdivision
    while (renderQueue.length > 0) {
      const block = renderQueue.shift();
      const { x, y, size } = block;
      
      // Skip tiny blocks or blocks outside screen
      if (size < minBlockSize || x >= screen.width || y >= screen.height) continue;
      
      // Sample corners to determine if subdivision is needed
      const samples = [
        samplePoint(x, y),
        samplePoint(x + size - 1, y),
        samplePoint(x, y + size - 1),
        samplePoint(x + size - 1, y + size - 1)
      ];
      
      // Check variation between samples to decide subdivision
      const maxDiff = Math.max(
        Math.abs(samples[0].iterations - samples[1].iterations),
        Math.abs(samples[0].iterations - samples[2].iterations),
        Math.abs(samples[0].iterations - samples[3].iterations),
        Math.abs(samples[1].iterations - samples[2].iterations),
        Math.abs(samples[1].iterations - samples[3].iterations),
        Math.abs(samples[2].iterations - samples[3].iterations)
      );
      
      // Audio increases detail sensitivity
      const subdivisionThreshold = 3 + trebleSparkle * 8 + audioPulse * 5;
      
      // Subdivide if variation is high and block is large enough
      if (maxDiff > subdivisionThreshold && size > minBlockSize * 2) {
        const halfSize = Math.floor(size / 2);
        if (halfSize >= minBlockSize) {
          renderQueue.push({ x: x, y: y, size: halfSize });
          renderQueue.push({ x: x + halfSize, y: y, size: halfSize });
          renderQueue.push({ x: x, y: y + halfSize, size: halfSize });
          renderQueue.push({ x: x + halfSize, y: y + halfSize, size: halfSize });
          continue;
        }
      }
      
      // Render uniform block using center sample
      const centerX = x + Math.floor(size / 2);
      const centerY = y + Math.floor(size / 2);
      const sample = samplePoint(centerX, centerY);
      const color = calculateColor(sample, centerX, centerY);
      
      // Fast block fill
      fillBlock(x, y, size, color);
    }
  }
}

export function sim({ sound }) {
  sound.speaker?.poll();
  playingSfx?.progress().then((p) => {
    progress = p.progress;
  }); // Get progress data.
}

export function act({ event: e, sound }) {
  // Respond to user input here.
  if (e.is("touch")) playingSfx = sound.play(sfx, { speed: 1 });
}
