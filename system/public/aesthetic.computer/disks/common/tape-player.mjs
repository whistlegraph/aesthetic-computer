// Tape Player - Common utilities for video.mjs and replay.mjs
// Shared code for tape playback UI (progress bars, state management, etc.)

const { floor, sin, max, min } = Math;

const clamp01 = (value) => max(0, min(1, Number.isFinite(value) ? value : 0));

/**
 * Format bytes to megabytes with appropriate precision
 */
export function formatMegabytes(bytes) {
  if (!Number.isFinite(bytes)) return null;
  const value = bytes / (1024 * 1024);
  const precision = value >= 100 ? 1 : 2;
  return `${value.toFixed(precision)} MB`;
}

/**
 * Derive progress state for tape operations (download, upload, frames, etc.)
 * Returns structured progress info for rendering progress bars
 */
export function deriveProgressState({
  phase = "download",
  downloadProgress = 0,
  downloadProgressKnown = false,
  downloadBytes = 0,
  downloadTotalBytes = 0,
  loadProgress = 0,
  loadedFramesCount = 0,
  totalFramesCount = 0,
  timeSeconds = 0,
  error = null,
}) {
  const stage = phase || "download";
  const DOWNLOAD_PORTION = 0.6;
  const UNPACK_PORTION = 0.05;
  const FRAMES_PORTION = 0.35;

  const downloadTotalKnown = Number.isFinite(downloadTotalBytes) && downloadTotalBytes > 0;

  let stageProgress = 0;
  let overallProgress = 0;
  let statusText = "CONNECTING TO BIOS";
  let detailText = "";
  let isIndeterminate = false;
  let indeterminateCycle = 0;
  let color = [120, 220, 255];

  if (stage === "download") {
    color = [0, 180, 255];
    if (downloadProgressKnown && downloadProgress > 0) {
      stageProgress = clamp01(downloadProgress);
      overallProgress = stageProgress * DOWNLOAD_PORTION;
      const overallPct = Math.floor(overallProgress * 100);
      const stagePct = Math.floor(stageProgress * 100);
      const parts = [];
      parts.push(`${overallPct}% total`);
      parts.push(`${stagePct}% download`);
      if (downloadTotalKnown) {
        const receivedText = formatMegabytes(downloadBytes);
        const totalText = formatMegabytes(downloadTotalBytes);
        if (receivedText && totalText) {
          parts.push(`${receivedText} / ${totalText}`);
        }
      } else if (Number.isFinite(downloadBytes) && downloadBytes > 0) {
        const receivedText = formatMegabytes(downloadBytes);
        if (receivedText) {
          parts.push(`${receivedText} transferred`);
        }
      }
      statusText = "Downloading tape";
      detailText = parts.join(" • ");
    } else {
      stageProgress = 0;
      overallProgress = 0;
      isIndeterminate = true;
      indeterminateCycle = ((timeSeconds ?? 0) * 0.6) % 1;
      statusText = "Downloading tape";
      if (Number.isFinite(downloadBytes) && downloadBytes > 0) {
        const receivedText = formatMegabytes(downloadBytes);
        detailText = receivedText ? `${receivedText} transferred` : "Requesting ZIP...";
      } else {
        detailText = "Requesting ZIP...";
      }
    }
  } else if (stage === "unpacking") {
    color = [0, 180, 255];
    stageProgress = downloadProgressKnown ? 0.9 : 0.1;
    overallProgress = DOWNLOAD_PORTION + stageProgress * UNPACK_PORTION;
    const overallPct = Math.floor(clamp01(overallProgress) * 100);
    statusText = "Unpacking ZIP";
    detailText = `${overallPct}% total • Indexing frames`;
  } else if (stage === "frames") {
    color = [255, 160, 0];
    stageProgress = clamp01(loadProgress || 0);
    overallProgress = DOWNLOAD_PORTION + UNPACK_PORTION + stageProgress * FRAMES_PORTION;
    const overallPct = Math.floor(clamp01(overallProgress) * 100);
    const stagePct = Math.floor(stageProgress * 100);
    const parts = [];
    parts.push(`${overallPct}% total`);
    parts.push(`${stagePct}% frames`);
    if (totalFramesCount > 0) {
      parts.push(`${loadedFramesCount}/${totalFramesCount}`);
    }
    statusText = "Loading frames";
    detailText = parts.join(" • ");
  } else if (stage === "presenting") {
    color = [120, 220, 255];
    stageProgress = 1;
    overallProgress = 1;
    statusText = "Preparing playback";
    detailText = "100% total";
  } else if (stage === "error") {
    color = [255, 80, 80];
    stageProgress = 0;
    overallProgress = 0;
    statusText = "Error loading tape";
    detailText = typeof error === "string" ? error : String(error ?? "");
  } else {
    stageProgress = clamp01(downloadProgress || loadProgress || 0);
    overallProgress = stageProgress;
    statusText = "Loading tape";
    const overallPct = Math.floor(clamp01(overallProgress) * 100);
    detailText = `${overallPct}% total`;
  }

  return {
    stage,
    stageProgress,
    overallProgress,
    statusText,
    detailText,
    isIndeterminate,
    indeterminateCycle,
    color,
  };
}

/**
 * Render a progress bar in the center of the screen (loading style)
 * Used when initially loading a tape
 */
export function renderLoadingProgressBar({ 
  ink, 
  write, 
  screen, 
  num,
  help,
  ellipsisTicker,
  progressState, 
  header = "Loading",
  centerY = null,
}) {
  const { floor, max } = Math;
  const centerX = floor(screen.width / 2);
  const cy = centerY ?? floor(screen.height / 2);

  // Render header with ellipsis
  const ellipsis = ellipsisTicker?.text(help?.repeat, { pad: false }) ?? "";
  const displayHeader = ellipsis ? `${header}${ellipsis}` : header;

  ink(...num.rainbow());
  write(displayHeader, {
    x: centerX,
    y: cy - 18,
    size: 2.5,
    center: "x",
  });

  // Render progress bar
  const barWidth = max(100, floor(screen.width * 0.5));
  const barLeft = centerX - floor(barWidth / 2);
  const barTop = cy + 8;
  const barHeight = 4;
  const fillWidth = floor(barWidth * clamp01(progressState?.overallProgress ?? 0));

  ink(0, 24, 48).box(barLeft, barTop, barWidth, barHeight);

  if (progressState) {
    if (!progressState.isIndeterminate && fillWidth > 0) {
      ink(...progressState.color).box(barLeft, barTop, fillWidth, barHeight);
    } else if (progressState.isIndeterminate) {
      const sweepWidth = floor(barWidth * 0.25);
      const sweepCenter = barLeft + floor(barWidth / 2);
      const sweepOffset = floor((barWidth / 2 - sweepWidth / 2) * sin((progressState.indeterminateCycle ?? 0) * Math.PI * 2));
      const sweepStart = sweepCenter - floor(sweepWidth / 2) + sweepOffset;
      ink(...progressState.color).box(sweepStart, barTop, sweepWidth, barHeight);
    }

    ink(180, 220, 255).write(progressState.statusText, {
      x: centerX,
      y: barTop + barHeight + 8,
      size: 1,
      center: "x",
    });

    if (progressState.detailText) {
      ink(160, 200, 255).write(progressState.detailText, {
        x: centerX,
        y: barTop + barHeight + 18,
        size: 1,
        center: "x",
      });
    }
  }
}

/**
 * Render a streaming frames badge (bottom-left overlay during playback)
 * Used when tape is playing but still caching frames
 */
export function renderStreamingBadge({
  ink,
  write,
  screen,
  loadedFramesCount = 0,
  totalFramesCount = 0,
}) {
  if (totalFramesCount <= 0 || loadedFramesCount >= totalFramesCount) return;

  const { floor, max, min } = Math;
  const margin = 16;
  const badgeHeight = 44;
  const availableWidth = max(120, screen.width - margin * 2);
  const badgeWidth = min(260, availableWidth);
  const badgeX = margin;
  const badgeY = screen.height - badgeHeight - margin;
  const ratio = clamp01(totalFramesCount ? loadedFramesCount / totalFramesCount : 0);
  const percent = Math.floor(ratio * 100);

  ink(0, 20, 40).box(badgeX, badgeY, badgeWidth, badgeHeight);
  ink(0, 32, 64).box(badgeX, badgeY, badgeWidth, 2);
  ink(0, 180, 255).box(
    badgeX + 8,
    badgeY + badgeHeight - 10,
    floor((badgeWidth - 16) * ratio),
    4,
  );

  const headline = totalFramesCount - loadedFramesCount > 32 ? "Caching frames…" : "Final frames…";
  ink(200, 230, 255).write(headline, {
    x: badgeX + 12,
    y: badgeY + 14,
    size: 1,
  });

  const detail = `${loadedFramesCount}/${totalFramesCount} frames (${percent}%)`;
  ink(130, 190, 240).write(detail, {
    x: badgeX + 12,
    y: badgeY + 26,
    size: 0.9,
  });
}

/**
 * Calculate VHS effects for a given position and animation frame
 * Returns multipliers for color intensity (0-1 range)
 * 
 * @param {number} x - Horizontal position
 * @param {number} animFrame - Animation frame counter
 * @returns {object} - Object with scanLine, analogGlow, tracking, secondaryGlow multipliers
 */
export function calculateVHSEffects(x, animFrame) {
  const scanLine = sin(animFrame * 0.5 + x * 0.6) * 0.15 + 0.85;
  const glowPhase = (animFrame * 0.15 + x * 0.12) % (Math.PI * 2);
  const analogGlow = sin(glowPhase) * 0.2 + 0.8;
  const tracking = sin(animFrame * 0.08 + x * 0.03) * 0.1 + 0.9;
  const secondaryGlow = sin(animFrame * 0.25 + x * 0.2) * 0.1 + 0.9;
  
  return { scanLine, analogGlow, tracking, secondaryGlow };
}

/**
 * Calculate leader pixel color based on frame index
 * Cycles through yellow, lime, white, magenta
 * 
 * @param {number} frameIndex - Current frame/animation index
 * @returns {object} - RGB color object {r, g, b}
 */
export function getLeaderPixelColor(frameIndex) {
  const colorCycle = floor(frameIndex * 0.3) % 4;
  switch (colorCycle) {
    case 0:
      return { r: 255, g: 255, b: 0 }; // Yellow
    case 1:
      return { r: 0, g: 255, b: 0 }; // Lime
    case 2:
      return { r: 255, g: 255, b: 255 }; // White
    case 3:
      return { r: 255, g: 0, b: 255 }; // Magenta
    default:
      return { r: 255, g: 255, b: 255 }; // White fallback
  }
}

/**
 * Blend a sampled color with VHS red color
 * 
 * @param {object} sampledColor - RGB color object {r, g, b}
 * @param {number} vhsR - VHS red channel value
 * @param {number} vhsG - VHS green channel value
 * @param {number} vhsB - VHS blue channel value
 * @param {number} blendFactor - How much of sampled color to use (0-1)
 * @returns {object} - Blended RGB color object {r, g, b}
 */
export function blendColorWithVHS(sampledColor, vhsR, vhsG, vhsB, blendFactor = 0.55) {
  return {
    r: floor(sampledColor.r * blendFactor + vhsR * (1 - blendFactor)),
    g: floor(sampledColor.g * blendFactor + vhsG * (1 - blendFactor)),
    b: floor(sampledColor.b * blendFactor + vhsB * (1 - blendFactor))
  };
}

/**
 * Render VHS-style progress bar into a canvas context
 * Used for both live recording overlay and GIF export
 * 
 * @param {CanvasRenderingContext2D} ctx - Canvas context to draw into
 * @param {number} width - Width of the canvas
 * @param {number} height - Height of the canvas (bar is 2px at bottom)
 * @param {Uint8ClampedArray} imageData - Original image data for color sampling
 * @param {number} progress - Progress value 0-1
 * @param {number} animFrame - Animation frame counter for VHS effects
 * @returns {void}
 */
export function renderVHSProgressBar({
  ctx,
  width,
  height,
  imageData,
  progress,
  animFrame = 0,
}) {
  const progressWidth = floor(width * progress);
  
  // Helper: Sample color from random positions all over the image
  const sampleColorFromImage = (baseX) => {
    // Use baseX with some randomness to pick Y position from anywhere in the image
    const seed = baseX + animFrame * 0.1;
    const randomY = floor((Math.sin(seed * 0.1) * 0.5 + 0.5) * (height - 3)); // Random Y from 0 to height-3
    const randomX = floor((Math.cos(seed * 0.07) * 0.5 + 0.5) * width); // Add some X variation too
    
    const idx = (randomY * width + randomX) * 4;
    if (idx >= 0 && idx < imageData.length - 3) {
      return {
        r: imageData[idx],
        g: imageData[idx + 1],
        b: imageData[idx + 2]
      };
    }
    return { r: 0, g: 0, b: 0 };
  };
  
  // Draw VHS-style progress bar pixel by pixel (2 pixels tall)
  for (let x = 0; x < width; x++) {
    if (x < progressWidth) {
      // Check if this is the leading edge pixel (playback tip)
      const isLeaderPixel = x === progressWidth - 1 && progressWidth > 0;
      
      let finalR, finalG, finalB;
      
      if (isLeaderPixel) {
        // Use shared leader pixel color cycling
        const color = getLeaderPixelColor(animFrame);
        finalR = color.r;
        finalG = color.g;
        finalB = color.b;
      } else {
        // Sample colors from all over the image for mixing
        const sampledColor = sampleColorFromImage(x);
        const sampledColor2 = sampleColorFromImage(x * 1.3 + 7);
        const sampledColor3 = sampleColorFromImage(x * 0.7 + 13);
        
        // Mix multiple sampled colors
        const mixedR = floor((sampledColor.r + sampledColor2.r + sampledColor3.r) / 3);
        const mixedG = floor((sampledColor.g + sampledColor2.g + sampledColor3.g) / 3);
        const mixedB = floor((sampledColor.b + sampledColor2.b + sampledColor3.b) / 3);
        const mixedColor = { r: mixedR, g: mixedG, b: mixedB };
        
        // Use shared VHS effects calculation
        const effects = calculateVHSEffects(x, animFrame);
        
        // Create VHS red color with effects
        let redIntensity = floor(200 * effects.scanLine * effects.analogGlow * effects.tracking * effects.secondaryGlow);
        const vhsR = max(180, min(255, redIntensity));
        const vhsG = floor(vhsR * 0.05);
        const vhsB = floor(vhsR * 0.02);
        
        // Use shared blending function
        const blended = blendColorWithVHS(mixedColor, vhsR, vhsG, vhsB, 0.55);
        finalR = blended.r;
        finalG = blended.g;
        finalB = blended.b;
      }
      
      // Draw both pixels with the final color (2px tall bar at bottom)
      ctx.fillStyle = `rgba(${finalR}, ${finalG}, ${finalB}, 0.9)`;
      ctx.fillRect(x, height - 2, 1, 2);
    }
  }
}
