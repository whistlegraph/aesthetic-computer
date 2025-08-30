// wipppps, 2025.6.04.01.26.48.500
// High-performance audio-reactive fractal visualizations.

// TODO: Fix wippppps.world noise static not loading.

let sfx,
  progress,
  playingSfx,
  pausedAt,
  playStartTime,
  totalDuration,
  isPlaying,
  shouldAutoPlay,
  actualDuration,
  isBuffering;

export const nohud = true;

export async function boot({ net: { preload }, sound, params, dom: { html } }) {
  // Initialize playback state
  progress = 0;
  pausedAt = 0;
  playStartTime = 0;
  actualDuration = null; // Will be set when sample data loads
  isPlaying = false;
  shouldAutoPlay = false;
  isBuffering = false;

  // Load audio immediately for instant playback
  const audioUrl = "https://assets.aesthetic.computer/wipppps/zzzZWAP.ogg";
  sfx = await preload(audioUrl);

  // Load accurate sample data AFTER audio starts playing to avoid blocking
  // This way the audio is already decoded when we request sample data

  // Add logo to top left corner
  html`
    <img
      id="wipppps-logo"
      src="https://assets.aesthetic.computer/wipppps/wipppps.webp"
      crossorigin="anonymous"
      class="logo-visible"
    />
    <style>
      #wipppps-logo {
        position: absolute;
        top: 0;
        left: 8px;
        width: 25vmin;
        height: 25vmin;
        z-index: 1000;
        object-fit: contain;
        pointer-events: none;
        transition: opacity 1s ease-in-out;
        filter: contrast(1.2) brightness(1.1) saturate(1.1)
          drop-shadow(0px 0px 1px rgba(255, 0, 255, 0.1))
          drop-shadow(1px 0px 2px rgba(0, 255, 255, 0.2));
        image-rendering: -webkit-optimize-contrast;
        image-rendering: crisp-edges;
      }

      #wipppps-logo.logo-hidden {
        opacity: 0;
      }

      #wipppps-logo.logo-visible {
        opacity: 0.85;
      }
    </style>
  `;

  /*
  if (params[0] === "zzzZWAP") {
    sfx = await preload(
      "https://assets.aesthetic.computer/wipppps/zzzZWAP.ogg",
    );
  } else if (params[0] === "WHoOSH") {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/WHoOSH.ogg");
  } else if (params[0] === "BLURP") {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/BLURP.ogg");
  } else {
    sfx = await preload("startup");
  }
  */
}

export function paint({ wipe, screen, write, sound, ink, box, shape }) {
  // Animation parameters - declare time first
  const time = performance.now() * 0.001;

  // Check if we should end buffering - wait for actual waveform data
  if (isBuffering && isPlaying) {
    const audioWaveform = sound.speaker?.waveforms?.left || [];

    // End buffering only when we have substantial waveform data
    if (audioWaveform.length > 0) {
      // Check for non-zero waveform values (actual audio signal)
      const hasRealSignal = audioWaveform.some(
        (sample) => Math.abs(sample) > 0.01,
      );
      if (hasRealSignal) {
        isBuffering = false;
      }
    }
  }

  // Show buffering screen - vibrant animated pattern
  if (isBuffering) {
    wipe(0, 0, 0); // Black screen base

    // Animated vibrant stripe pattern
    const stripeWidth = 12 + Math.sin(time * 2) * 4; // Variable stripe width
    const stripeSpeed = time * 60; // Stripe movement speed
    const stripeAngle = Math.sin(time * 0.8) * 0.3; // Subtle angle variation

    // Draw multiple layers of vibrant stripes
    for (let layer = 0; layer < 4; layer++) {
      const layerOffset = layer * 40;
      const layerSpeed = stripeSpeed + layerOffset;
      const layerOpacity = 0.4 + layer * 0.15;

      // Create diagonal stripes across the screen
      for (
        let y = -stripeWidth;
        y < screen.height + stripeWidth;
        y += stripeWidth * 2
      ) {
        for (
          let x = -stripeWidth;
          x < screen.width + stripeWidth;
          x += stripeWidth * 2
        ) {
          // Calculate stripe position with animation
          const stripeX = x + Math.sin(stripeAngle) * y + layerSpeed;
          const stripeY = y + Math.cos(stripeAngle) * x;

          // Vibrant, saturated colors - full spectrum cycling
          const hueBase = (time * 60 + layer * 90 + x * 0.1 + y * 0.08) % 360;
          const saturation = 0.8 + Math.sin(time * 1.5 + layer) * 0.2;
          const lightness = 0.5 + Math.sin(time * 2 + layer * 0.5) * 0.3;

          // Convert HSL to RGB for vibrant colors
          const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
          const x_hsl = c * (1 - Math.abs(((hueBase / 60) % 2) - 1));
          const m = lightness - c / 2;

          let r, g, b;
          const hueSegment = Math.floor(hueBase / 60) % 6;
          switch (hueSegment) {
            case 0:
              r = c;
              g = x_hsl;
              b = 0;
              break;
            case 1:
              r = x_hsl;
              g = c;
              b = 0;
              break;
            case 2:
              r = 0;
              g = c;
              b = x_hsl;
              break;
            case 3:
              r = 0;
              g = x_hsl;
              b = c;
              break;
            case 4:
              r = x_hsl;
              g = 0;
              b = c;
              break;
            default:
              r = c;
              g = 0;
              b = x_hsl;
              break;
          }

          const vibrantR = Math.floor(255 * Math.max(0, Math.min(1, r + m)));
          const vibrantG = Math.floor(255 * Math.max(0, Math.min(1, g + m)));
          const vibrantB = Math.floor(255 * Math.max(0, Math.min(1, b + m)));

          ink(vibrantR, vibrantG, vibrantB, Math.floor(layerOpacity * 255));

          // Draw rotated stripe rectangle
          const stripeLength = stripeWidth * 1.5;
          box(
            stripeX % screen.width,
            stripeY % screen.height,
            stripeLength,
            stripeWidth,
          );
        }
      }
    }

    return; // Skip fractal rendering while buffering
  }

  // Wiggle control flag - set to true for subtle living effects
  const enableWiggling = true;

  // 🎵 Audio-reactive parameters with enhanced sensitivity
  const audioAmplitude = sound.speaker?.amplitudes?.left || 0;
  const audioWaveform = sound.speaker?.waveforms?.left || [];

  // Enhanced frequency analysis with subtle sensitivity
  const bassLevel =
    audioWaveform.slice(0, 8).reduce((a, b) => a + Math.abs(b), 0) / 8;
  const midLevel =
    audioWaveform.slice(8, 24).reduce((a, b) => a + Math.abs(b), 0) / 16;
  const trebleLevel =
    audioWaveform.slice(24).reduce((a, b) => a + Math.abs(b), 0) /
    (audioWaveform.length - 24);

  // Subtle sensitivity for gentle visual effects
  const bass = Math.min(bassLevel * 2, 1); // Much more gentle
  const mid = Math.min(midLevel * 2.5, 1); // Reduced sensitivity
  const treble = Math.min(trebleLevel * 3, 1); // Less aggressive
  const overallAmplitude = Math.min(audioAmplitude * 2, 1); // Much more subtle

  // Audio-driven dynamic effects - gentle and smooth
  const audioPulse = Math.pow(overallAmplitude, 0.8); // Gentler pulse response
  const bassBoost = Math.pow(bass, 0.6) * 0.5; // Much more subtle bass
  const trebleSparkle = treble * 0.3; // Linear, gentle treble

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
    high: 0.7, // Switch rapidly on high amplitude
    medium: 0.4, // Medium speed switching
    low: 0.2, // Slow switching
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
  const audioReactiveIndex =
    Math.floor(time / dynamicCycleTime) % zoomPoints.length;

  // Blend between audio-reactive selection and time-based cycling
  const finalPointIndex =
    overallAmplitude > audioThresholds.low
      ? Math.max(0, Math.min(zoomPoints.length - 1, targetPointIndex))
      : audioReactiveIndex;

  const cycleProgress = (time % dynamicCycleTime) / dynamicCycleTime;

  // Yoyo progress: 0->1->0 over the cycle using triangle wave
  const pointProgress =
    cycleProgress <= 0.5
      ? cycleProgress * 2 // First half: 0 to 1
      : 2 - cycleProgress * 2; // Second half: 1 to 0

  const deepZoomPoint = zoomPoints[finalPointIndex];

  // Audio-reactive zoom depth - faster zooms on high energy
  const baseMaxZoomDepth = 8.0;
  const audioZoomBoost = Math.min(overallAmplitude * 4, 3.0); // Up to 3x deeper zoom
  const maxZoomDepth = baseMaxZoomDepth + audioZoomBoost;
  const zoomLevel = pointProgress * maxZoomDepth;

  // Gentle iteration count for faster rendering - subtly audio reactive
  const baseIterations = 150 + Math.floor(bassBoost * 50); // Much less variation
  const maxIterations = Math.min(
    250 + Math.floor(mid * 100), // Reduced range
    baseIterations + Math.floor(zoomLevel * 6) + Math.floor(trebleSparkle * 30),
  );

  // Screen parameters with cute 3D depth! 🌀
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const zoom = Math.pow(2, zoomLevel);
  const pixelSize = 4.0 / (zoom * Math.min(screen.width, screen.height));

  // Adorable 3D depth animation - breathing z-axis motion ✨ + DRAMATICALLY geometric reactive
  const zDepth = Math.sin(time * 0.3) * 0.5 + bassBoost * 2.0; // Much stronger bass geometric response
  const zSliceSpeed = time * 0.1 + mid * 0.4; // Increased mid geometric response
  const baseZ = zSliceSpeed + zDepth;

  // Cute organic wiggling - bringing back the life! 🌊 + DRAMATICALLY geometric reactive
  const baseWiggleAmplitude = 0.0008 + trebleSparkle * 0.012; // More dramatic treble wiggling
  const wiggleAmplitude = baseWiggleAmplitude / Math.pow(zoom, 0.7);

  // Cute dual-layer wiggling with personality + dramatic geometric reactive
  const wigglePhase = time * 0.8 + audioPulse * 12; // Much stronger pulse geometric impact
  const primaryWiggle =
    Math.sin(wigglePhase * 0.3) + 0.3 * Math.cos(wigglePhase * 0.7);
  const cuteWiggle =
    0.2 * Math.sin(wigglePhase * 1.2) + 0.15 * Math.cos(wigglePhase * 0.5);

  // Adorable breathing motion + DRAMATICALLY bass geometric reactive
  const breathingAmplitude = 1 + 0.05 * Math.sin(time * 0.4) + bassBoost * 0.8; // Much stronger bass breathing
  const wiggleX =
    wiggleAmplitude * breathingAmplitude * (primaryWiggle + cuteWiggle);
  const wiggleY =
    wiggleAmplitude *
    breathingAmplitude *
    (Math.cos(wigglePhase * 0.35) + 0.2 * cuteWiggle);

  // Apply optional wiggle to the zoom point for dynamic center shifting
  const wiggledZoomPoint = {
    x: deepZoomPoint.x + (enableWiggling ? wiggleX : 0),
    y: deepZoomPoint.y + (enableWiggling ? wiggleY : 0),
  };

  // Cache expensive calculations outside the pixel loop with DRAMATIC audio-reactive rotation
  const rotationAngle = time * 0.02 + bassBoost * 0.5 + audioPulse * 0.3; // Much more dramatic audio rotation
  const cosTheta = Math.cos(rotationAngle);
  const sinTheta = Math.sin(rotationAngle);

  // 🚀 HIGH-PERFORMANCE HIERARCHICAL BLOCK RENDERING 🚀
  renderFractalOptimized();

  // Update logo visibility based on buffering state
  const logoClass = isBuffering ? "logo-hidden" : "logo-visible";
  // Note: We can't directly manipulate DOM here, the CSS transition handles the fade

  // Pause overlay - only show when paused (not during loading)
  if (!isPlaying && (playingSfx !== null || pausedAt > 0)) {
    // Darker semi-transparent overlay covering the full screen
    ink(0, 180).box(0, 0, screen.width, screen.height);

    // White play triangle in center
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    const triangleSize = 14; // Smaller and cuter size

    // Draw black shadow for play triangle (offset by 1px)
    ink(0, 0, 0, 255);
    shape([
      [centerX - triangleSize / 2 + 1, centerY - triangleSize / 2 + 1],
      [centerX + triangleSize / 2 + 1, centerY + 1],
      [centerX - triangleSize / 2 + 1, centerY + triangleSize / 2 + 1],
    ]);

    // Draw white outline for play triangle (slightly larger)
    ink(255, 255, 255, 255);
    shape([
      [centerX - triangleSize / 2 - 1, centerY - triangleSize / 2 - 1],
      [centerX + triangleSize / 2 + 1, centerY],
      [centerX - triangleSize / 2 - 1, centerY + triangleSize / 2 + 1],
    ]);

    // Draw rainbow play triangle
    ink("rainbow");
    shape([
      [centerX - triangleSize / 2, centerY - triangleSize / 2],
      [centerX + triangleSize / 2, centerY],
      [centerX - triangleSize / 2, centerY + triangleSize / 2],
    ]);
  }

  // Progress bar at the bottom - drawn AFTER pause overlay to appear on top
  if (progress !== undefined) {
    const barHeight = 4; // 4px tall progress bar
    const barY = screen.height - barHeight;

    // Background bar (dark) - full width using ink().box()
    ink(0, 0, 0, 255);
    box(0, barY, screen.width, barHeight);

    // Progress bar - rainbow when playing, white when paused
    const progressWidth = progress * screen.width;
    if (progressWidth > 0) {
      if (isPlaying) {
        ink("rainbow");
      } else {
        ink(255, 255, 255, 255);
      }
      box(0, barY, progressWidth, barHeight);
    }
  }

  // Draw track timer in bottom right corner - only when we have valid duration data
  if (actualDuration && progress !== undefined) {
    const currentTime = progress * actualDuration;

    // Format time as MM:SS
    const formatTime = (seconds) => {
      const mins = Math.floor(seconds / 60);
      const secs = Math.floor(seconds % 60);
      return `${mins}:${secs.toString().padStart(2, "0")}`;
    };

    const timeText = `${formatTime(currentTime)} / ${formatTime(actualDuration)}`;

    // Position in bottom right, accounting for text width
    const textX = screen.width - timeText.length * 6 - 6; // Approximate character width
    const textY = screen.height - 16; // Higher up to be above progress bar

    // Draw shadow (offset by 1px down and right) using ink() for proper shadow color
    ink(32, 32, 32); // Set dark gray ink for shadow
    write(timeText, textX + 1, textY + 1);

    // Draw main text in white using ink()
    ink(255, 255, 255); // Set white ink for main text
    write(timeText, textX, textY);
  }

  function renderFractalOptimized() {
    // Shared functions for both rendering methods
    // Fast point sampling with caching
    const sampleCache = new Map();
    
    // Choose rendering method based on audio activity and zoom level
    const useBlockRendering = zoomLevel > 2.0 && overallAmplitude > 0.1;
    
    if (useBlockRendering) {
      renderFractalBlocks(sampleCache);
    } else {
      renderFractalScanlines(sampleCache);
    }
  }

  function samplePoint(pixelX, pixelY, sampleCache) {
    const key = `${pixelX},${pixelY}`;
    if (sampleCache.has(key)) return sampleCache.get(key);

    // Convert to complex plane coordinates
    const baseReal = (pixelX - centerX) * pixelSize;
    const baseImag = (pixelY - centerY) * pixelSize;

    // DRAMATIC audio-reactive coordinate transformations
    const scaleFactorX = 1.0 + bassBoost * 0.6; // Bass stretches X axis
    const scaleFactorY = 1.0 + mid * 0.4; // Mid stretches Y axis
    const skewFactor = trebleSparkle * 0.3; // Treble adds skew

    // Apply audio-reactive scaling and skewing
    const scaledReal = baseReal * scaleFactorX + baseImag * skewFactor;
    const scaledImag = baseImag * scaleFactorY;

    // Apply rotation
    const rotatedReal = scaledReal * cosTheta - scaledImag * sinTheta;
    const rotatedImag = scaledReal * sinTheta + scaledImag * cosTheta;

    // Apply per-pixel wiggling
    let pixelWiggleX = 0,
      pixelWiggleY = 0;
    if (enableWiggling) {
      const spatialPhase =
        pixelX * 0.008 + pixelY * 0.006 + baseZ * 0.5 + time * 0.3;
      const pixelPersonality =
        Math.sin(pixelX * 0.02) * Math.cos(pixelY * 0.015);

      pixelWiggleX =
        wiggleAmplitude *
        0.4 *
        (Math.sin(spatialPhase) +
          0.3 * Math.cos(spatialPhase * 1.7) +
          0.1 * pixelPersonality);
      pixelWiggleY =
        wiggleAmplitude *
        0.4 *
        (Math.cos(spatialPhase * 1.1) +
          0.25 * Math.sin(spatialPhase * 0.8) +
          0.12 * pixelPersonality);
    }

    // Final coordinates
    const cReal = wiggledZoomPoint.x + rotatedReal + pixelWiggleX;
    const cImag = wiggledZoomPoint.y + rotatedImag + pixelWiggleY;

    // Fast Mandelbrot calculation
    let zReal = 0.0,
      zImag = 0.0;
    let iterations = 0;
    let magnitude = 0.0;

    // Audio-reactive iteration boost - gentler now
    const audioIterationBoost = Math.floor(
      bass * 75 + mid * 75 + trebleSparkle * 25,
    ); // Much reduced
    const dynamicMaxIterations = Math.max(
      50,
      maxIterations + audioIterationBoost,
    );

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
      maxIterations: dynamicMaxIterations,
    };

    sampleCache.set(key, result);
    return result;
  }

  // Fast color calculation
  function calculateColor(sample, pixelX, pixelY) {
    const {
      iterations,
      magnitude,
      escaped,
      maxIterations: dynMaxIter,
    } = sample;

    if (!escaped || iterations >= dynMaxIter - 1) {
      // Interior coloring
      const proximityValue = Math.min(Math.sqrt(magnitude) * 0.4, 1.0);
      const depthFactor = Math.min(iterations / (dynMaxIter * 0.7), 1.0);
      const colorPersonality =
        Math.sin(pixelX * 0.03 + time * 0.1) *
        Math.cos(pixelY * 0.025 + time * 0.08);

      const baseHue = 180 + Math.sin(time * 0.08) * 120 + bassBoost * 60; // Full spectrum base hue
      const proximityHue =
        proximityValue * 80 + colorPersonality * 40 + audioPulse * 50; // Dramatic hue variation
      const depthHue = depthFactor * 60 + trebleSparkle * 40; // Major hue shifts

      const hue = (baseHue + proximityHue + depthHue + zoomLevel * 15) % 360; // Full spectrum cycling
      const saturation = Math.min(
        0.8 +
          proximityValue * 0.2 +
          Math.abs(colorPersonality) * 0.15 +
          audioPulse * 0.2,
        1.0,
      ); // High saturation for dayglo effect
      const lightness = Math.min(
        0.3 +
          proximityValue * 0.4 +
          depthFactor * 0.3 +
          Math.sin(time * 0.15) * 0.1 +
          bassBoost * 0.15,
        0.9,
      ); // More dynamic lightness

      return hslToRgb(hue, saturation, lightness);
    } else {
      // Exterior coloring
      const smoothIter = iterations + 1 - Math.log2(Math.log2(magnitude));
      const t = smoothIter / dynMaxIter;

      const sparkle =
        Math.sin(smoothIter * 0.8 + time * 3) * 0.5 + trebleSparkle * 0.3; // More dramatic treble sparkle
      const personalityHue =
        Math.sin(pixelX * 0.03) * Math.cos(pixelY * 0.025) + bassBoost * 0.4; // Increased bass personality

      const baseHue = 90 + time * 12 + zoomLevel * 20 + audioPulse * 60; // Dynamic full-spectrum exterior
      const sparklyHue = sparkle * 120 + personalityHue * 80 + mid * 40; // Dramatic variation
      const hue = (baseHue + sparklyHue) % 360; // Full spectrum cycling

      const saturation = Math.min(
        0.9 + t * 0.1 + Math.abs(sparkle) * 0.2 + audioPulse * 0.15,
        1.0,
      ); // High saturation for dayglo
      const lightness = Math.min(
        0.4 +
          t * 0.35 +
          sparkle * 0.2 +
          Math.sin(time * 0.2) * 0.1 +
          bassBoost * 0.1,
        0.95,
      ); // More dynamic lightness

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
      case 0:
        r = c;
        g = x_hsl;
        b = 0;
        break;
      case 1:
        r = x_hsl;
        g = c;
        b = 0;
        break;
      case 2:
        r = 0;
        g = c;
        b = x_hsl;
        break;
      case 3:
        r = 0;
        g = x_hsl;
        b = c;
        break;
      case 4:
        r = x_hsl;
        g = 0;
        b = c;
        break;
      default:
        r = c;
        g = 0;
        b = x_hsl;
        break;
    }

    return {
      r: Math.floor(255 * Math.max(0, Math.min(1, r + m))),
      g: Math.floor(255 * Math.max(0, Math.min(1, g + m))),
      b: Math.floor(255 * Math.max(0, Math.min(1, b + m))),
    };
  }

  // Optimized block filling with typed array access and bounds checking
  function fillBlockSafe(x, y, width, height, color) {
    // Ensure we don't exceed screen boundaries
    const endX = Math.min(x + width, screen.width);
    const endY = Math.min(y + height, screen.height);
    const startX = Math.max(x, 0);
    const startY = Math.max(y, 0);

    for (let fillY = startY; fillY < endY; fillY++) {
      const rowStart = fillY * screen.width * 4;
      for (let fillX = startX; fillX < endX; fillX++) {
        const fillIndex = rowStart + fillX * 4;
        screen.pixels[fillIndex] = color.r;
        screen.pixels[fillIndex + 1] = color.g;
        screen.pixels[fillIndex + 2] = color.b;
        screen.pixels[fillIndex + 3] = 255;
      }
    }
  }

  function renderFractalBlocks(sampleCache) {
    // High-performance rendering with hierarchical subdivision
    const maxBlockSize = Math.max(
      4,
      Math.floor(16 + bass * 48 + overallAmplitude * 24),
    );
    const minBlockSize = Math.max(1, Math.floor(1 + trebleSparkle * 3));

    // Hierarchical block subdivision with queue
    const renderQueue = [];

    // Start with large blocks - ensure perfect coverage of the entire screen
    for (let blockY = 0; blockY < screen.height; blockY += maxBlockSize) {
      for (let blockX = 0; blockX < screen.width; blockX += maxBlockSize) {
        const blockWidth = Math.min(maxBlockSize, screen.width - blockX);
        const blockHeight = Math.min(maxBlockSize, screen.height - blockY);
        renderQueue.push({ 
          x: blockX, 
          y: blockY, 
          size: Math.max(blockWidth, blockHeight),
          width: blockWidth,
          height: blockHeight
        });
      }
    }

    // Process render queue with subdivision
    while (renderQueue.length > 0) {
      const block = renderQueue.shift();
      const { x, y, size, width, height } = block;

      // Skip blocks completely outside screen bounds
      if (x >= screen.width || y >= screen.height) continue;

      // Use explicit width/height if provided, otherwise use size
      const actualWidth = width !== undefined ? width : Math.min(size, screen.width - x);
      const actualHeight = height !== undefined ? height : Math.min(size, screen.height - y);

      // Skip if no valid area to render
      if (actualWidth <= 0 || actualHeight <= 0) continue;

      // Sample corners within valid bounds for subdivision decision
      const sampleX1 = Math.min(x, screen.width - 1);
      const sampleY1 = Math.min(y, screen.height - 1);
      const sampleX2 = Math.min(x + actualWidth - 1, screen.width - 1);
      const sampleY2 = Math.min(y + actualHeight - 1, screen.height - 1);

      const samples = [
        samplePoint(sampleX1, sampleY1, sampleCache),
        samplePoint(sampleX2, sampleY1, sampleCache),
        samplePoint(sampleX1, sampleY2, sampleCache),
        samplePoint(sampleX2, sampleY2, sampleCache),
      ];

      // Check variation between samples to decide subdivision
      const maxDiff = Math.max(
        Math.abs(samples[0].iterations - samples[1].iterations),
        Math.abs(samples[0].iterations - samples[2].iterations),
        Math.abs(samples[0].iterations - samples[3].iterations),
        Math.abs(samples[1].iterations - samples[2].iterations),
        Math.abs(samples[1].iterations - samples[3].iterations),
        Math.abs(samples[2].iterations - samples[3].iterations),
      );

      // Audio increases detail sensitivity
      const subdivisionThreshold = 3 + trebleSparkle * 8 + audioPulse * 5;

      // Subdivide if variation is high and block is large enough
      if (
        maxDiff > subdivisionThreshold &&
        Math.min(actualWidth, actualHeight) > minBlockSize * 2
      ) {
        const halfWidth = Math.floor(actualWidth / 2);
        const halfHeight = Math.floor(actualHeight / 2);
        
        // Ensure we don't lose pixels due to rounding - calculate remainders
        const remainderWidth = actualWidth - halfWidth;
        const remainderHeight = actualHeight - halfHeight;

        if (halfWidth >= minBlockSize && halfHeight >= minBlockSize) {
          // Create four sub-blocks with exact coverage
          renderQueue.push({
            x: x,
            y: y,
            size: Math.max(halfWidth, halfHeight),
            width: halfWidth,
            height: halfHeight,
          });
          
          if (remainderWidth > 0) {
            renderQueue.push({
              x: x + halfWidth,
              y: y,
              size: Math.max(remainderWidth, halfHeight),
              width: remainderWidth,
              height: halfHeight,
            });
          }
          
          if (remainderHeight > 0) {
            renderQueue.push({
              x: x,
              y: y + halfHeight,
              size: Math.max(halfWidth, remainderHeight),
              width: halfWidth,
              height: remainderHeight,
            });
          }
          
          if (remainderWidth > 0 && remainderHeight > 0) {
            renderQueue.push({
              x: x + halfWidth,
              y: y + halfHeight,
              size: Math.max(remainderWidth, remainderHeight),
              width: remainderWidth,
              height: remainderHeight,
            });
          }
          continue;
        }
      }

      // Render uniform block using center sample
      const centerX = Math.min(
        x + Math.floor(actualWidth / 2),
        screen.width - 1,
      );
      const centerY = Math.min(
        y + Math.floor(actualHeight / 2),
        screen.height - 1,
      );
      const sample = samplePoint(centerX, centerY, sampleCache);
      const color = calculateColor(sample, centerX, centerY);

      // Fast block fill with proper bounds checking
      fillBlockSafe(x, y, actualWidth, actualHeight, color);
    }
  }

  function renderFractalScanlines(sampleCache) {
    // Simple scanline rendering - guarantees complete coverage
    const stepSize = Math.max(1, Math.floor(1 + overallAmplitude * 3));
    
    for (let y = 0; y < screen.height; y += stepSize) {
      for (let x = 0; x < screen.width; x += stepSize) {
        const sample = samplePoint(x, y, sampleCache);
        const color = calculateColor(sample, x, y);
        
        // Fill a small block for performance
        const blockWidth = Math.min(stepSize, screen.width - x);
        const blockHeight = Math.min(stepSize, screen.height - y);
        fillBlockSafe(x, y, blockWidth, blockHeight, color);
      }
    }
  }
}

export function sim({ sound }) {
  sound.speaker?.poll();

  // Update progress using manual timing when playing
  if (isPlaying && playingSfx && !playingSfx.killed) {
    const currentTime = performance.now();
    const elapsedSeconds = (currentTime - playStartTime) / 1000;

    // Use actual duration if available for accurate progress calculation
    const trackDuration = actualDuration || totalDuration;
    const totalElapsed = pausedAt * trackDuration + elapsedSeconds;
    progress = Math.min(totalElapsed / trackDuration, 1.0);

    // Stop if we've reached the end
    if (progress >= 1.0) {
      playingSfx.kill(0.1);
      playingSfx = null;
      isPlaying = false;
      progress = 0;
      pausedAt = 0;
    }
  }
}

export function act({ event: e, sound }) {
  // Handle play/pause/resume functionality - audio is always ready
  if (e.is("touch")) {
    // Don't allow interaction while buffering
    if (isBuffering) return;

    if (isPlaying && playingSfx && !playingSfx.killed) {
      // Currently playing - pause the track
      const currentTime = performance.now();
      const elapsedSeconds = (currentTime - playStartTime) / 1000;

      // Use actual duration if available for accurate pause position
      const trackDuration = actualDuration || totalDuration;
      pausedAt = Math.min(
        (pausedAt * trackDuration + elapsedSeconds) / trackDuration,
        1.0,
      );

      playingSfx.kill(0.1); // Fade out quickly
      playingSfx = null;
      isPlaying = false;
    } else {
      // Not playing - start buffering, then play
      isBuffering = true;

      // Start audio immediately but keep buffering until we have waveform data
      playingSfx = sound.play(sfx, { speed: 1, from: pausedAt });
      playStartTime = performance.now();
      isPlaying = true;

      // Get duration AFTER playback starts (audio already decoded)
      if (!actualDuration) {
        sound
          .getDuration(sfx)
          .then((duration) => {
            actualDuration = duration;
          })
          .catch((error) => {
            console.warn("🎵 Duration load failed:", error);
          });
      }
    }
  }
}
