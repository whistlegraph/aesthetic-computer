// Build Instructions:
// cd /workspaces/aesthetic-computer/reference/tools/recording && node tape.mjs elcid-flyer.mjs 240 --mp4 --resolution 2048
// TODO:
// [-] Finish copy editing / typography
// [ ] Add flair

// Track frame count for animation - will be injected by frame renderer
let frameCount = 0;
let marqueeOffset = 0; // For scrolling text

// Debug mode toggle
const DEBUG_MODE = false;

// Helper function for VHS-style rainbow overdraw text with SIZE-SCALED thickness and fast opacity blinking
function writeVHS(text, options, api) {
  const { write, ink } = api;
  const { x, y, center, size = 12 } = options; // Default size if not specified
  
  // Glitchy blink effect - MUCH FASTER blinking!
  const blinkSeed = text.length + (options.x || 0) + (options.y || 0);
  const blinkChance = Math.sin(frameCount * 0.15 + blinkSeed * 0.03) * 0.5 + 0.5; // 3x faster
  
  // NEVER completely disappear text - use opacity blinking instead
  const opacityBlink = Math.sin(frameCount * 0.25 + blinkSeed * 0.05) * 0.4 + 0.6; // 0.2-1.0 range - fast flicker
  const baseOpacity = 120 + (blinkChance * 80); // 120-200 base opacity
  
  // Glitch intensity affects how crazy the effect gets - FASTER!
  const glitchIntensity = Math.sin(frameCount * 0.08 + blinkSeed * 0.05) * 0.5 + 0.5; // Much faster
  
  // Assign distinct neon base color per text element
  const textHash = text.split('').reduce((a, b) => a + b.charCodeAt(0), 0);
  const neonColors = [
    [255, 0, 100],     // Hot Pink
    [0, 255, 150],     // Neon Green
    [100, 0, 255],     // Electric Purple
    [255, 150, 0],     // Neon Orange
    [0, 200, 255],     // Electric Blue
    [255, 255, 0],     // Electric Yellow
    [255, 0, 255],     // Magenta
    [0, 255, 255],     // Cyan
    [255, 100, 200],   // Pink
    [150, 255, 0],     // Lime
    [255, 0, 0],       // Red
    [0, 100, 255],     // Blue
  ];
  const baseNeonColor = neonColors[textHash % neonColors.length];
  
  // ENHANCED SINE WAVE WIGGLE - seed-based randomness for each text block
  const wiggleSeedX = (textHash * 17) % 1000;
  const wiggleSeedY = (textHash * 23) % 1000;
  const wiggleSpeedX = 0.05 + (wiggleSeedX / 1000) * 0.04; // 0.05-0.09 speed - faster
  const wiggleSpeedY = 0.05 + (wiggleSeedY / 1000) * 0.04; // 0.05-0.09 speed - faster and matched to X
  const wiggleAmplitudeX = 3 + (wiggleSeedX / 1000) * 4; // 3-7 pixel amplitude - increased
  const wiggleAmplitudeY = 3 + (wiggleSeedY / 1000) * 4; // 3-7 pixel amplitude - increased and matched to X
  
  const wiggleX = Math.sin(frameCount * wiggleSpeedX + wiggleSeedX * 0.01) * wiggleAmplitudeX;
  const wiggleY = Math.sin(frameCount * wiggleSpeedY + wiggleSeedY * 0.01) * wiggleAmplitudeY;
  
  // Much more varied colors for maximum psychedelic effect
  const colors = [
    [255, 50, 50],     // Hot red
    [255, 100, 0],     // Bright orange  
    [255, 200, 0],     // Gold
    [200, 255, 50],    // Lime green
    [0, 255, 100],     // Spring green
    [0, 200, 255],     // Sky blue
    [100, 150, 255],   // Blue
    [150, 50, 255],    // Purple
    [255, 50, 200],    // Hot pink
    [255, 100, 255],   // Magenta
    [255, 255, 100],   // Electric yellow
    [100, 255, 255],   // Cyan
    [255, 150, 200],   // Pink
    [200, 255, 150],   // Light green
    [150, 200, 255],   // Light blue
    [255, 200, 150],   // Peach
    [255, 50, 100],    // Deep pink
    [100, 255, 50],    // Neon green
    [50, 100, 255],    // Electric blue
    [255, 255, 50],    // Bright yellow
    [255, 50, 255],    // Electric magenta
    [50, 255, 255],    // Bright cyan
    [255, 100, 100],   // Red
    [100, 255, 100],   // Green
    [100, 100, 255],   // Blue
    [255, 150, 50],    // Orange
    [150, 255, 50],    // Yellow-green
    [50, 150, 255],    // Blue-cyan
    [255, 50, 150],    // Red-magenta
    [150, 50, 255]     // Blue-magenta
  ];
  
  // CONCENTRATED thickness spread - SIZE-SCALED but MUCH THICKER for intense overdraw
  const sizeScale = size / 12; // Scale factor based on size (12 is baseline)
  const maxThickness = Math.min(12, 3 + sizeScale * 2); // 3-12 pixels max, much thicker, scales with size
  
  // Tight thickness offsets - BOTH X and Y directions, extended for thicker text
  const baseThickness = [
    [0, 0], [1, 0], [0, 1], [1, 1], [-1, 0], [0, -1], [-1, -1],
    [2, 0], [0, 2], [-2, 0], [0, -2], [2, 1], [1, 2], [-2, -1], [-1, -2],
    [2, 2], [-2, -2], [2, -2], [-2, 2], [3, 0], [0, 3], [-3, 0], [0, -3],
    [3, 1], [1, 3], [-3, 1], [1, -3], [3, -1], [-1, 3], [-3, -1], [-1, -3],
    [3, 2], [2, 3], [-3, 2], [2, -3], [3, -2], [-2, 3], [-3, -2], [-2, -3],
    [4, 0], [0, 4], [-4, 0], [0, -4], [4, 1], [1, 4], [-4, -1], [-1, -4],
    [4, 2], [2, 4], [-4, 2], [2, -4], [4, -2], [-2, 4], [-4, -2], [-2, -4],
    [5, 0], [0, 5], [-5, 0], [0, -5], [5, 1], [1, 5], [-5, -1], [-1, -5],
    [5, 2], [2, 5], [-5, 2], [2, -5], [5, -2], [-2, 5], [-5, -2], [-2, -5],
    [6, 0], [0, 6], [-6, 0], [0, -6], [6, 1], [1, 6], [-6, -1], [-1, -6],
    [6, 2], [2, 6], [-6, 2], [2, -6], [6, -2], [-2, 6], [-6, -2], [-2, -6],
    [6, 3], [3, 6], [-6, 3], [3, -6], [6, -3], [-3, 6], [-6, -3], [-3, -6],
    [7, 0], [0, 7], [-7, 0], [0, -7], [7, 1], [1, 7], [-7, -1], [-1, -7],
    [7, 2], [2, 7], [-7, 2], [2, -7], [7, -2], [-2, 7], [-7, -2], [-2, -7],
    [8, 0], [0, 8], [-8, 0], [0, -8], [8, 1], [1, 8], [-8, -1], [-1, -8],
    [8, 2], [2, 8], [-8, 2], [2, -8], [8, -2], [-2, 8], [-8, -2], [-2, -8],
    [9, 0], [0, 9], [-9, 0], [0, -9], [9, 1], [1, 9], [-9, -1], [-1, -9],
    [10, 0], [0, 10], [-10, 0], [0, -10], [10, 1], [1, 10], [-10, -1], [-1, -10],
    [11, 0], [0, 11], [-11, 0], [0, -11], [11, 1], [1, 11], [-11, -1], [-1, -11],
    [12, 0], [0, 12], [-12, 0], [0, -12], [12, 1], [1, 12], [-12, -1], [-1, -12]
  ];
  
  // Filter and scale offsets to stay within maxThickness
  const thicknessOffsets = baseThickness
    .filter(([x, y]) => Math.abs(x) <= maxThickness && Math.abs(y) <= maxThickness)
    .map(([x, y]) => [x, y]); // Keep as-is, already tight
  
  // LIGHTWEIGHT OVERDRAW for performance testing - MUCH REDUCED
  const numLayers = 2; // Reduced from 3-6 to just 2 layers
  
  for (let layer = 0; layer < numLayers; layer++) {
    const copiesPerLayer = 3; // Reduced from 6-12 to just 3 copies
    
    for (let i = 0; i < copiesPerLayer; i++) {
      // Mix base neon color with random colors
      const useBaseColor = Math.random() < 0.3; // 30% chance to use base neon
      let r, g, b;
      
      if (useBaseColor) {
        [r, g, b] = baseNeonColor;
      } else {
        const colorIndex = (i + layer * 2 + Math.floor(Math.random() * colors.length)) % colors.length;
        [r, g, b] = colors[colorIndex];
      }
      
      // Vary opacity for depth - scale with text size and apply opacity blink
      const baseAlpha = (30 + Math.random() * 70) * opacityBlink; // 30-100 range with opacity blink
      const layerAlpha = baseAlpha * (1 - layer * 0.06); // Fade deeper layers more gradually
      const glitchAlpha = glitchIntensity > 0.6 ? layerAlpha * (0.7 + Math.random() * 0.6) : layerAlpha;
      
      ink(r, g, b, glitchAlpha);
      
      // LIGHTWEIGHT thickness - use fewer offsets for performance
      const maxOffsets = Math.min(5, thicknessOffsets.length); // Use up to 5 offsets (was 20)
      const numOffsets = Math.max(3, Math.min(maxOffsets, 3 + Math.floor(sizeScale * 1))); // 3-5 offsets for lighter text
      
      // Select offsets randomly from the tight set
      const selectedOffsets = [];
      for (let j = 0; j < numOffsets; j++) {
        const randomIndex = Math.floor(Math.random() * thicknessOffsets.length);
        selectedOffsets.push(thicknessOffsets[randomIndex]);
      }
      
      for (const [offsetX, offsetY] of selectedOffsets) {
        // ENHANCED random scatter - more noticeable in both X and Y
        const scatterRange = Math.min(2, 1 + glitchIntensity * 1.5); // Max 2 pixel scatter - increased
        const randomOffsetX = (Math.random() - 0.5) * scatterRange;
        const randomOffsetY = (Math.random() - 0.5) * scatterRange;
        
        const vhsOptions = { ...options };
        if (x !== undefined) vhsOptions.x = x + offsetX + randomOffsetX + wiggleX;
        if (y !== undefined) vhsOptions.y = y + offsetY + randomOffsetY + wiggleY;
        
        write(text, vhsOptions);
      }
    }
  }
  
  // Draw final clean copy with base neon color - with RAPID blinking and WIGGLE! Never disappears
  const rapidBlink = Math.sin(frameCount * 0.4 + blinkSeed * 0.1) * 0.3 + 0.7; // Super fast blink, 0.4-1.0 range
  const cleanAlpha = (140 + Math.random() * 60) * rapidBlink * opacityBlink; // Always visible, 56-200 range
  ink(baseNeonColor[0] * rapidBlink, baseNeonColor[1] * rapidBlink, baseNeonColor[2] * rapidBlink, cleanAlpha);
  
  // Apply wiggle to the final clean copy too - scale wiggle with text size
  const finalOptions = { ...options };
  const wiggleScale = Math.max(1, sizeScale * 0.8); // Bigger text = more wiggle
  if (finalOptions.x !== undefined) finalOptions.x += wiggleX * wiggleScale;
  if (finalOptions.y !== undefined) finalOptions.y += wiggleY * wiggleScale;
  write(text, finalOptions);
}

// Helper function to draw a dynamic star with crazy lines and particle trails
function drawDynamicStar(x, y, size, frame, api) {
  const { line, ink } = api;
  const time = frame * 0.1; // Animation speed
  const numRays = 16; // More rays for crazier effect
  const innerRadius = size * 0.3;
  const outerRadius = size * 1.2; // Bigger outer radius
  
  // Pulsing effect
  const pulse = 1 + Math.sin(time * 3) * 0.3;
  const currentInner = innerRadius * pulse;
  const currentOuter = outerRadius * pulse;
  
  // Rotation
  const rotation = time * 2;
  
  // Draw crazy star rays with multiple layers
  for (let layer = 0; layer < 3; layer++) {
    for (let i = 0; i < numRays; i++) {
      const angle = (i / numRays) * Math.PI * 2 + rotation + (layer * 0.1);
      
      // Different ray lengths for each layer
      const rayLength = currentOuter * (1 - layer * 0.2);
      
      // Outer ray
      const outerX = x + Math.cos(angle) * rayLength;
      const outerY = y + Math.sin(angle) * rayLength;
      
      // Inner point between rays
      const innerAngle = angle + (Math.PI / numRays);
      const innerX = x + Math.cos(innerAngle) * currentInner;
      const innerY = y + Math.sin(innerAngle) * currentInner;
      
      // Rainbow color for each ray
      const colorIndex = (i + layer + Math.floor(time * 2)) % 9;
      switch (colorIndex) {
        case 0: ink(255, 100, 100); break; // Red
        case 1: ink(255, 150, 50); break;  // Orange
        case 2: ink(255, 255, 100); break; // Yellow
        case 3: ink(100, 255, 100); break; // Green
        case 4: ink(100, 150, 255); break; // Blue
        case 5: ink(150, 100, 255); break; // Indigo
        case 6: ink(255, 100, 255); break; // Violet
        case 7: ink(100, 255, 255); break; // Cyan
        case 8: ink(255, 150, 200); break; // Pink
      }
      
      // Draw main ray
      line(x, y, outerX, outerY);
      
      // Draw connecting lines between rays
      if (i < numRays - 1) {
        const nextAngle = ((i + 1) / numRays) * Math.PI * 2 + rotation + (layer * 0.1);
        const nextOuterX = x + Math.cos(nextAngle) * rayLength;
        const nextOuterY = y + Math.sin(nextAngle) * rayLength;
        line(outerX, outerY, nextOuterX, nextOuterY);
      }
      
      // Particle trail effect - random lines around the star
      for (let p = 0; p < 3; p++) {
        const trailAngle = angle + (Math.random() - 0.5) * 0.5;
        const trailLength = rayLength * (0.5 + Math.random() * 0.5);
        const trailX = x + Math.cos(trailAngle) * trailLength;
        const trailY = y + Math.sin(trailAngle) * trailLength;
        
        // Dimmer color for trails
        const trailAlpha = 100 + Math.random() * 100;
        switch (colorIndex) {
          case 0: ink(255, 100, 100, trailAlpha); break;
          case 1: ink(255, 150, 50, trailAlpha); break;
          case 2: ink(255, 255, 100, trailAlpha); break;
          case 3: ink(100, 255, 100, trailAlpha); break;
          case 4: ink(100, 150, 255, trailAlpha); break;
          case 5: ink(150, 100, 255, trailAlpha); break;
          case 6: ink(255, 100, 255, trailAlpha); break;
          case 7: ink(100, 255, 255, trailAlpha); break;
          case 8: ink(255, 150, 200, trailAlpha); break;
        }
        line(x, y, trailX, trailY);
      }
    }
  }
}

export function paint({ api, frameIndex = 0, frameTime = 0, simCount = 0n }) {
  const { wipe, ink, line, box, circle, write, point, blur, scroll, zoom, spin, text, gizmo } = api;

  // Use injected frame data for continuity
  frameCount = frameIndex;

  // Clean gradient background with cyberpunk aesthetic
  if (frameCount <= 1) {
    // wipe(0, 0, 0); // Complete black for first 30 frames
  }
  //} else if (frameCount === 31) {
  //  wipe(5, 10, 15); // Very dark blue-black base
  // }

  // Background persistence mode - no gradient to preserve previous frames
  // const gradientShift = Math.sin(frameCount * 0.01) * 8;
  
  // Gradient disabled for background persistence
  // for (let y = 80; y < (2048 - 80); y += 4) {
  //   const intensity = 5 + gradientShift + (y / 2048) * 10;
  //   ink(intensity, intensity + 3, intensity + 12, 8);
  //   line(0, y, 2048, y);
  // }

  // CYBERPUNK COLOR BAR at top - neo-matrix themed!
  const colorBarHeight = 80; // Height of the color strip
  const numColorSegments = 20; // Number of color bands
  const segmentWidth = 2048 / numColorSegments; // Width of each color segment
  
  // Color animation - shift colors over time
  const colorTime = frameCount * 0.02;
  const colorShift = frameCount * 0.1; // How fast colors cycle
  const hueRotation = frameCount * 0.05; // Global hue rotation
  
  for (let segment = 0; segment < numColorSegments; segment++) {
    const x = segment * segmentWidth;
    
    // Create animated color cycling
    const colorIndex = (segment + Math.floor(colorShift)) % 8;
    const colorPhase = (segment * 0.5 + colorTime) * Math.PI * 2;
    
    // TOP border - cooler cyberpunk color palette (blues, cyans, purples)
    let r, g, b;
    switch (colorIndex) {
      case 0:  [r, g, b] = [50, 200, 255];   break; // Electric Cyan
      case 1:  [r, g, b] = [100, 150, 255];  break; // Blue-Purple  
      case 2:  [r, g, b] = [0, 255, 200];    break; // Cyan-Green
      case 3:  [r, g, b] = [150, 100, 255];  break; // Purple
      case 4:  [r, g, b] = [0, 150, 255];    break; // Ocean Blue
      case 5:  [r, g, b] = [100, 255, 200];  break; // Mint
      case 6:  [r, g, b] = [200, 100, 255];  break; // Violet
      case 7:  [r, g, b] = [50, 255, 150];   break; // Aqua-Green
    }
    
    // Apply hue rotation for dynamic color shifting
    const hueShift = (hueRotation + segment * 0.3) % (Math.PI * 2);
    const rotatedR = r * Math.cos(hueShift) - g * Math.sin(hueShift);
    const rotatedG = r * Math.sin(hueShift) + g * Math.cos(hueShift);
    const rotatedB = b; // Keep blue channel stable for cyberpunk aesthetic
    
    // Clamp values
    const finalR = Math.max(0, Math.min(255, rotatedR));
    const finalG = Math.max(0, Math.min(255, rotatedG));
    const finalB = Math.max(0, Math.min(255, rotatedB));
    
    // Oscillating opacity per segment - each segment has different phase
    const opacityPhase = colorPhase + segment * 0.8;
    const opacity = 120 + Math.sin(opacityPhase) * 80; // 40-200 range
    
    ink(finalR, finalG, finalB, opacity);
    
    // Draw the color segment as a filled rectangle
    box(x, 0, segmentWidth + 1, colorBarHeight); // +1 to prevent gaps
    
    // Add some vertical gradient within each segment for extra richness
    for (let y = 0; y < colorBarHeight; y += 4) {
      const gradientFactor = 1 - (y / colorBarHeight) * 0.2; // Less gradient for sharper cyberpunk look
      ink(finalR * gradientFactor, finalG * gradientFactor, finalB * gradientFactor, opacity * 0.6);
      line(x, y, x + segmentWidth, y);
    }
  }
  
  // Matrix-style digital sparkles on the color bar
  for (let sparkle = 0; sparkle < 12; sparkle++) {
    const sparkleX = (Math.sin(frameCount * 0.08 + sparkle * 1.2) * 0.5 + 0.5) * 2048;
    const sparkleY = Math.random() * colorBarHeight;
    const sparkleSize = 1 + Math.random() * 3;
    
    // Green matrix sparkles
    ink(0, 255, 100, 180 + Math.random() * 75);
    circle(sparkleX, sparkleY, sparkleSize);
  }

  // LEFT SIDE Cyberpunk border - warmer color palette (oranges, reds, yellows)
  const leftBorderWidth = 80; // Match the top border height
  const leftSegments = 20;
  const leftSegmentHeight = 2048 / leftSegments;

  const leftColors = [
    [255, 150, 50],   // Orange
    [255, 200, 100],  // Yellow-Orange
    [255, 100, 150],  // Hot Pink
    [255, 50, 100],   // Red-Pink
    [200, 255, 100],  // Yellow-Green
    [255, 100, 200],  // Magenta
    [255, 200, 50],   // Gold
    [150, 255, 100],  // Lime Green
  ];  for (let i = 0; i < leftSegments; i++) {
    const y = i * leftSegmentHeight;
    const colorIndex = (i + Math.floor(frameCount * 0.1)) % leftColors.length;
    const [r, g, b] = leftColors[colorIndex];
    
    // Individual oscillating opacity for each segment - MUCH FASTER!
    const segmentPhase = frameCount * 0.25 + i * 0.4; // Increased from 0.06 to 0.25
    const opacity = 60 + Math.sin(segmentPhase) * 90; // Wider range: 0-150
    
    // Hue rotation per segment
    const segmentHueShift = (frameCount * 0.03 + i * 0.2) % (Math.PI * 2);
    const rotatedR = Math.max(0, Math.min(255, r * Math.cos(segmentHueShift) - g * Math.sin(segmentHueShift)));
    const rotatedG = Math.max(0, Math.min(255, r * Math.sin(segmentHueShift) + g * Math.cos(segmentHueShift)));
    
    ink(rotatedR, rotatedG, b, opacity);
    box(0, y, leftBorderWidth, leftSegmentHeight + 1);
    
    // Horizontal gradient within each segment
    for (let x = 0; x < leftBorderWidth; x += 3) {
      const gradientFactor = 1 - (x / leftBorderWidth) * 0.3;
      ink(rotatedR * gradientFactor, rotatedG * gradientFactor, b * gradientFactor, opacity * 0.7);
      line(x, y, x, y + leftSegmentHeight);
    }
  }

  // RIGHT SIDE Cyberpunk border - warmer color palette (reds, pinks, oranges)
  const rightBorderWidth = 80; // Match the top border height
  const rightX = 2048 - rightBorderWidth;
  const rightSegments = 20;
  const rightSegmentHeight = 2048 / rightSegments;

  const rightColors = [
    [255, 0, 150],    // Hot Pink (keep original hot pink)
    [255, 100, 50],   // Red-Orange
    [255, 150, 100],  // Peach
    [255, 0, 255],    // Magenta (keep original)
    [255, 200, 150],  // Light Orange
    [255, 50, 0],     // Red
    [255, 100, 0],    // Orange-Red
    [255, 150, 200],  // Pink-Orange
  ];  for (let i = 0; i < rightSegments; i++) {
    const y = i * rightSegmentHeight;
    const colorIndex = (i + Math.floor(frameCount * 0.12)) % rightColors.length;
    const [r, g, b] = rightColors[colorIndex];
    
    // Different oscillating pattern for right side - MUCH FASTER!
    const segmentPhase = frameCount * 0.3 + i * 0.5; // Increased from 0.08 to 0.3  
    const opacity = 50 + Math.sin(segmentPhase) * 100; // Wider range: 0-150
    
    // Hue rotation per segment
    const segmentHueShift = (frameCount * 0.04 + i * 0.3) % (Math.PI * 2);
    const rotatedR = Math.max(0, Math.min(255, r * Math.cos(segmentHueShift) - g * Math.sin(segmentHueShift)));
    const rotatedG = Math.max(0, Math.min(255, r * Math.sin(segmentHueShift) + g * Math.cos(segmentHueShift)));
    
    ink(rotatedR, rotatedG, b, opacity);
    box(rightX, y, rightBorderWidth, rightSegmentHeight + 1);
    
    // Horizontal gradient from right edge inward
    for (let x = 0; x < rightBorderWidth; x += 3) {
      const gradientFactor = 1 - (x / rightBorderWidth) * 0.3;
      ink(rotatedR * gradientFactor, rotatedG * gradientFactor, b * gradientFactor, opacity * 0.7);
      line(rightX + rightBorderWidth - x, y, rightX + rightBorderWidth - x, y + rightSegmentHeight);
    }
  }

  // BOTTOM Cyberpunk border - warmer color palette (yellows, oranges, reds)
  const bottomBorderHeight = 80; // Match the top border height
  const bottomY = 2048 - bottomBorderHeight;
  const bottomSegments = 20;
  const bottomSegmentWidth = 2048 / bottomSegments;

  const bottomColors = [
    [200, 150, 0],    // Dark Gold
    [255, 200, 50],   // Golden Yellow
    [255, 150, 0],    // Orange
    [255, 100, 50],   // Red-Orange
    [200, 255, 100],  // Yellow-Green
    [255, 255, 0],    // Bright Yellow
    [255, 200, 100],  // Light Orange
    [150, 100, 0],    // Dark Yellow-Brown
  ];  for (let i = 0; i < bottomSegments; i++) {
    const x = i * bottomSegmentWidth;
    const colorIndex = (i + Math.floor(frameCount * 0.11)) % bottomColors.length;
    const [r, g, b] = bottomColors[colorIndex];
    
    // Bottom border oscillating pattern - MUCH FASTER!
    const segmentPhase = frameCount * 0.28 + i * 0.6; // Increased from 0.07 to 0.28
    const opacity = 40 + Math.sin(segmentPhase) * 110; // Wider range: 0-150
    
    // Hue rotation per segment
    const segmentHueShift = (frameCount * 0.035 + i * 0.25) % (Math.PI * 2);
    const rotatedR = Math.max(0, Math.min(255, r * Math.cos(segmentHueShift) - g * Math.sin(segmentHueShift)));
    const rotatedG = Math.max(0, Math.min(255, r * Math.sin(segmentHueShift) + g * Math.cos(segmentHueShift)));
    
    ink(rotatedR, rotatedG, b, opacity);
    box(x, bottomY, bottomSegmentWidth + 1, bottomBorderHeight);
    
    // Vertical gradient within each segment
    for (let y = 0; y < bottomBorderHeight; y += 3) {
      const gradientFactor = 1 - (y / bottomBorderHeight) * 0.3;
      ink(rotatedR * gradientFactor, rotatedG * gradientFactor, b * gradientFactor, opacity * 0.7);
      line(x, bottomY + bottomBorderHeight - y, x + bottomSegmentWidth, bottomY + bottomBorderHeight - y);
    }
  }

  // Add cyberpunk sparkles to all borders - each border has themed sparkles
  for (let sparkle = 0; sparkle < 25; sparkle++) {
    const sparkleSize = 1 + Math.random() * 2;
    const sparkleAlpha = 120 + Math.random() * 135;
    
    // Left border sparkles - Electric Blue theme
    if (sparkle < 8) {
      const sparkleX = Math.random() * leftBorderWidth;
      const sparkleY = Math.random() * 2048;
      ink(0, 200, 255, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }
    
    // Right border sparkles - Hot Pink theme
    if (sparkle >= 8 && sparkle < 16) {
      const sparkleX = rightX + Math.random() * rightBorderWidth;
      const sparkleY = Math.random() * 2048;
      ink(255, 0, 200, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }
    
    // Bottom border sparkles - Matrix Green theme
    if (sparkle >= 16) {
      const sparkleX = Math.random() * 2048;
      const sparkleY = bottomY + Math.random() * bottomBorderHeight;
      ink(0, 255, 100, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }
  }

  // 3D Starfield background - flying through space! (Reduced for performance)
  const numStars = 256; // Reduced from 1024 for performance
  const speed = 32; // Movement speed through space
  
  // Initialize stars on first frame if not in state
  if (!global.stars) {
    // console.log('🌟 Initializing starfield...'); // Commented for recording performance
    global.stars = [];
    for (let i = 0; i < numStars; i++) {
      global.stars.push({
        x: (Math.random() - 0.5) * 4000, // Random X position in 3D space
        y: (Math.random() - 0.5) * 4000, // Random Y position in 3D space  
        z: Math.random() * 2000 + 100,   // Random Z depth
        type: i % 8                      // Star visual type
      });
    }
  }
  
  // Update and render each star
  for (let i = 0; i < numStars; i++) {
    const star = global.stars[i];
    
    // Move star towards camera
    star.z -= speed;
    
    // Reset star when it passes camera
    if (star.z <= 0) {
      star.x = (Math.random() - 0.5) * 4000;
      star.y = (Math.random() - 0.5) * 4000;
      star.z = 2000 + Math.random() * 1000;
    }
    
    // 3D to 2D projection
    const scale = 1024 / star.z; // Perspective scaling
    const screenX = star.x * scale + 1024; // Center on screen
    const screenY = star.y * scale + 1024;
    
    // Skip stars outside screen bounds
    if (screenX < -100 || screenX > 2148 || screenY < -100 || screenY > 2148) continue;
    
    // Size and brightness based on distance (closer = bigger/brighter)
    const distance = star.z / 2000; // 0-1 range
    const starSize = Math.max(1, Math.floor((1 - distance) * 12)); // 1-12 pixels
    const brightness = Math.floor(50 + (1 - distance) * 200); // 50-250 brightness
    
    // Twinkling effect
    const twinkle = Math.sin(frameCount * 0.1 + i * 0.2) * 0.3 + 0.7;
    const finalBrightness = brightness * twinkle;
    
    // Different star types with 3D scaling
    switch (star.type) {
      case 0: // Blue-white circles
        ink(finalBrightness, finalBrightness, finalBrightness * 1.2);
        circle(screenX, screenY, starSize);
        break;
      case 1: // Warm white with glow
        ink(finalBrightness * 1.1, finalBrightness, finalBrightness * 0.9);
        circle(screenX, screenY, starSize);
        if (starSize > 3) {
          ink(finalBrightness * 0.4, finalBrightness * 0.4, finalBrightness * 0.4, 80);
          circle(screenX, screenY, starSize + 2);
        }
        break;
      case 2: // Blue diamonds
        ink(finalBrightness * 0.8, finalBrightness * 0.9, finalBrightness * 1.3);
        for (let r = 0; r < starSize; r++) {
          line(screenX - r, screenY, screenX, screenY - r);
          line(screenX, screenY - r, screenX + r, screenY);
          line(screenX + r, screenY, screenX, screenY + r);
          line(screenX, screenY + r, screenX - r, screenY);
        }
        break;
      case 3: // Yellow plus signs
        ink(finalBrightness * 1.2, finalBrightness * 1.1, finalBrightness * 0.7);
        box(screenX - starSize, screenY - 1, starSize * 2, 3);
        box(screenX - 1, screenY - starSize, 3, starSize * 2);
        break;
      case 4: // Red pulsing squares
        ink(finalBrightness * 1.3, finalBrightness * 0.8, finalBrightness * 0.8);
        const pulseSize = starSize + Math.sin(frameCount * 0.1 + i) * 2;
        box(screenX - pulseSize/2, screenY - pulseSize/2, pulseSize, pulseSize);
        break;
      case 5: // Green hexagonal stars
        ink(finalBrightness * 0.9, finalBrightness * 1.2, finalBrightness * 0.9);
        for (let angle = 0; angle < 6; angle++) {
          const x1 = screenX + Math.cos(angle * Math.PI / 3) * starSize;
          const y1 = screenY + Math.sin(angle * Math.PI / 3) * starSize;
          const x2 = screenX + Math.cos((angle + 1) * Math.PI / 3) * starSize;
          const y2 = screenY + Math.sin((angle + 1) * Math.PI / 3) * starSize;
          line(screenX, screenY, x1, y1);
          line(x1, y1, x2, y2);
        }
        break;
      case 6: // Cyan rotating triangles
        ink(finalBrightness, finalBrightness * 1.3, finalBrightness * 1.3);
        const rotation = frameCount * 0.05 + i * 0.5;
        for (let tri = 0; tri < 3; tri++) {
          const angle = rotation + tri * (Math.PI * 2 / 3);
          const x = screenX + Math.cos(angle) * starSize;
          const y = screenY + Math.sin(angle) * starSize;
          line(screenX, screenY, x, y);
        }
        break;
      case 7: // Magenta with motion trails
        ink(finalBrightness * 1.2, finalBrightness * 0.8, finalBrightness * 1.2);
        circle(screenX, screenY, starSize);
        // Motion trail effect based on Z movement
        if (starSize > 2) {
          const trailLength = (1 - distance) * 8; // Longer trails for closer stars
          for (let t = 1; t <= 3; t++) {
            const trailAlpha = 100 - t * 25;
            ink(finalBrightness * 0.8, finalBrightness * 0.4, finalBrightness * 0.8, trailAlpha);
            circle(screenX, screenY + t * trailLength, Math.max(1, starSize - t));
          }
        }
        break;
    }
  }

  // Bouncing Colorful Star Instances around the screen (Reduced for performance)
  const numBouncingStars = 25; // Reduced from 25 for performance
  
  // Initialize bouncing stars if not in state
  if (!global.bouncingStars) {
    // console.log('🌈 Initializing bouncing stars...'); // Commented for recording performance
    global.bouncingStars = [];
    for (let i = 0; i < numBouncingStars; i++) {
      global.bouncingStars.push({
        x: Math.random() * 2048,
        y: Math.random() * 2048,
        vx: (Math.random() - 0.5) * 8, // Velocity X: -4 to +4
        vy: (Math.random() - 0.5) * 8, // Velocity Y: -4 to +4
        size: 5 + Math.random() * 15,  // Size: 5-20 pixels
        colorIndex: i % 12,            // Color variety
        rotationSpeed: (Math.random() - 0.5) * 0.2,
        rotation: 0,
        pulseOffset: Math.random() * Math.PI * 2
      });
    }
  }
  
  // Update and render bouncing stars
  for (let i = 0; i < numBouncingStars; i++) {
    const star = global.bouncingStars[i];
    
    // Update position
    star.x += star.vx;
    star.y += star.vy;
    
    // Bounce off screen edges
    if (star.x <= star.size || star.x >= 2048 - star.size) {
      star.vx = -star.vx;
      star.x = Math.max(star.size, Math.min(2048 - star.size, star.x));
    }
    if (star.y <= star.size || star.y >= 2048 - star.size) {
      star.vy = -star.vy;
      star.y = Math.max(star.size, Math.min(2048 - star.size, star.y));
    }
    
    // Update rotation
    star.rotation += star.rotationSpeed;
    
    // Pulsing size effect
    const pulseFactor = Math.sin(frameCount * 0.08 + star.pulseOffset) * 0.3 + 1.0; // 0.7-1.3x
    const currentSize = star.size * pulseFactor;
    
    // Color based on index with brightness variation
    const brightness = 150 + Math.sin(frameCount * 0.06 + i * 0.5) * 100; // 50-250
    let r, g, b;
    switch (star.colorIndex) {
      case 0:  [r, g, b] = [255, 100, 100]; break; // Red
      case 1:  [r, g, b] = [255, 150, 50]; break;  // Orange
      case 2:  [r, g, b] = [255, 255, 100]; break; // Yellow
      case 3:  [r, g, b] = [100, 255, 100]; break; // Green
      case 4:  [r, g, b] = [100, 150, 255]; break; // Blue
      case 5:  [r, g, b] = [150, 100, 255]; break; // Purple
      case 6:  [r, g, b] = [255, 100, 255]; break; // Magenta
      case 7:  [r, g, b] = [100, 255, 255]; break; // Cyan
      case 8:  [r, g, b] = [255, 200, 100]; break; // Gold
      case 9:  [r, g, b] = [200, 255, 150]; break; // Light Green
      case 10: [r, g, b] = [255, 150, 200]; break; // Pink
      case 11: [r, g, b] = [150, 200, 255]; break; // Light Blue
    }
    
    // Apply brightness scaling
    const finalR = (r * brightness) / 255;
    const finalG = (g * brightness) / 255;
    const finalB = (b * brightness) / 255;
    
    // Different star shapes based on index
    const shapeType = i % 6;
    switch (shapeType) {
      case 0: // Filled circles with glow
        ink(finalR, finalG, finalB);
        circle(star.x, star.y, currentSize);
        ink(finalR * 0.5, finalG * 0.5, finalB * 0.5, 100);
        circle(star.x, star.y, currentSize + 3);
        break;
      case 1: // Rotating squares
        ink(finalR, finalG, finalB);
        const halfSize = currentSize / 2;
        const cos = Math.cos(star.rotation);
        const sin = Math.sin(star.rotation);
        // Draw rotated square using lines
        const corners = [
          [-halfSize, -halfSize], [halfSize, -halfSize], 
          [halfSize, halfSize], [-halfSize, halfSize]
        ];
        for (let j = 0; j < 4; j++) {
          const [x1, y1] = corners[j];
          const [x2, y2] = corners[(j + 1) % 4];
          const rx1 = x1 * cos - y1 * sin + star.x;
          const ry1 = x1 * sin + y1 * cos + star.y;
          const rx2 = x2 * cos - y2 * sin + star.x;
          const ry2 = x2 * sin + y2 * cos + star.y;
          line(rx1, ry1, rx2, ry2);
        }
        break;
      case 2: // Plus shapes
        ink(finalR, finalG, finalB);
        line(star.x - currentSize, star.y, star.x + currentSize, star.y);
        line(star.x, star.y - currentSize, star.x, star.y + currentSize);
        break;
      case 3: // Triangular stars
        ink(finalR, finalG, finalB);
        for (let angle = 0; angle < 6; angle++) {
          const a = (angle * Math.PI / 3) + star.rotation;
          const x = star.x + Math.cos(a) * currentSize;
          const y = star.y + Math.sin(a) * currentSize;
          line(star.x, star.y, x, y);
        }
        break;
      case 4: // Diamond shapes
        ink(finalR, finalG, finalB);
        for (let r = 0; r < currentSize; r++) {
          line(star.x - r, star.y, star.x, star.y - r);
          line(star.x, star.y - r, star.x + r, star.y);
          line(star.x + r, star.y, star.x, star.y + r);
          line(star.x, star.y + r, star.x - r, star.y);
        }
        break;
      case 5: // Multi-layer circles
        for (let layer = 0; layer < 3; layer++) {
          const layerSize = currentSize - layer * 2;
          const layerAlpha = 255 - layer * 60;
          ink(finalR, finalG, finalB, layerAlpha);
          if (layerSize > 0) circle(star.x, star.y, layerSize);
        }
        break;
    }
  }

  if (Math.random() > 0.98) {
    ink("rainbow", 8).box(0, 0, 2048, 2048); // Clear with black ink to prevent trails
  } else if (Math.random() > 0.94) {
    ink(0, 24).box(256, 256, 2048-512, 2048-512);
  }

  // Apply subtle blur for polish
  if (frameCount % 2 === 0) blur(8); // Reduced blur strength to reduce processing load

  // Spin and scroll effects
  // if (Math.random() > 0.5) {
  //    spin(-1);
  //  } else if (Math.random() > 0.5) {
  //    spin(1);
  //  }

  // Dynamic cardinal direction scroll - changes every 1 second (60 frames)
  const scrollCycle = Math.floor(frameCount / 60) % 4; // 4 directions, 1 second each
  
  switch (scrollCycle) {
    case 0: scroll(0, 2); break;  // Up
    case 1: scroll(2, 0); break;   // Right  
    case 2: scroll(0, 2); break;   // Down
    case 3: scroll(-2, 0); break;  // Left
  }
  // zoom(0.99);

  // Center point for all text
  const centerX = 1024; // Half of 2048 for proper centering
  
  // TOP AESTHETIC.COMPUTER MARQUEE - small text like "hosted by"
  const topMarqueeY = 120; // At the top
  const topMarqueeText = "AESTHETIC.COMPUTER AESTHETIC.COMPUTER AESTHETIC.COMPUTER AESTHETIC.COMPUTER "; // Repeated text
  const topMarqueeSpeed = 2; // Speed for small text
  
  // Dynamic scroll direction that changes every second (60 frames)
  const scrollDirection = Math.floor(frameCount / 60) % 8; // Changes every 1 second, 8 cardinal directions
  let scrollX = 0, scrollY = 0;
  
  // 8 cardinal directions: N, NE, E, SE, S, SW, W, NW
  switch (scrollDirection) {
    case 0: [scrollX, scrollY] = [0, -1]; break;    // North
    case 1: [scrollX, scrollY] = [1, -1]; break;    // North-East
    case 2: [scrollX, scrollY] = [1, 0]; break;     // East
    case 3: [scrollX, scrollY] = [1, 1]; break;     // South-East
    case 4: [scrollX, scrollY] = [0, 1]; break;     // South
    case 5: [scrollX, scrollY] = [-1, 1]; break;    // South-West
    case 6: [scrollX, scrollY] = [-1, 0]; break;    // West
    case 7: [scrollX, scrollY] = [-1, -1]; break;   // North-West
  }
  
  // Update marquee offset with dynamic direction
  marqueeOffset += topMarqueeSpeed;
  
  // Calculate text width for smooth looping with small size
  const topCharWidth = 8 * 6; // Character width for size 6
  const topTextWidth = topMarqueeText.length * topCharWidth;
  
  // Create a full-width marquee area
  const topMarqueeWidth = 2048; // Full width
  const topMarqueeStartX = 0;
  const topMarqueeEndX = topMarqueeWidth;
  
  // Start text fully onscreen at left edge, scroll to completely off the right side
  const topTotalTravelDistance = topMarqueeWidth + topTextWidth;
  if (marqueeOffset > topTotalTravelDistance) {
    marqueeOffset = 0;
  }
  
  // Position text to start from left edge of visible area
  const topTextStartX = topMarqueeStartX - marqueeOffset;
  
  // Draw the top scrolling text
  const topChars = topMarqueeText.split('');
  
  // Subtle blinking for top marquee
  const topMarqueeBlink = Math.sin(frameCount * 0.2) * 0.2 + 0.8; // 0.6-1.0 intensity
  
  ink(150, 150, 150); // Gray like "hosted by"
  topChars.forEach((char, index) => {
    const charX = topTextStartX + (index * topCharWidth);
    
    // Apply scroll direction offset
    const scrollOffsetX = (frameCount * scrollX * 0.5) % 2048;
    const scrollOffsetY = (frameCount * scrollY * 0.5) % 100;
    
    // Only draw characters that are visible within the marquee area
    if (charX >= topMarqueeStartX - 100 && charX <= topMarqueeEndX + 100) {
      if (char !== ' ' && char !== '.') {
        const charOptions = { 
          x: charX + scrollOffsetX, 
          y: topMarqueeY + scrollOffsetY, 
          size: 6 
        };
        
        // Apply blinking
        ink(150 * topMarqueeBlink, 150 * topMarqueeBlink, 150 * topMarqueeBlink);
        writeVHS(char, charOptions, api);
      } else if (char === '.') {
        const charOptions = { 
          x: charX + scrollOffsetX, 
          y: topMarqueeY + scrollOffsetY, 
          size: 6 
        };
        
        ink(150 * topMarqueeBlink, 150 * topMarqueeBlink, 150 * topMarqueeBlink);
        writeVHS('.', charOptions, api);
      }
    }
  });

  // INTELLIGENT LAYOUT for 2048x2048 canvas
  // Vertical sections: Top marquee (0-200), Main titles (200-600), Date (600-900), Time (900-1200), Venue (1200-1600), Footer (1600-2048)
  
  // MAIN TITLE - AESTHETIC COMPUTER - most prominent with white/black/neon oscillation
  
  // AESTHETIC with white/black/neon color scheme
  const aestheticSizeOscillation = 32 + Math.sin(frameCount * 0.08) * 6; // Size 26-38
  const aestheticColorPhase = frameCount * 0.1;
  
  // Cycle between white, neon green, black, neon cyan
  let aestheticR, aestheticG, aestheticB;
  const colorCycle = Math.sin(aestheticColorPhase);
  
  if (colorCycle > 0.5) {
    // Neon green
    aestheticR = 0 + Math.sin(aestheticColorPhase * 2) * 100;
    aestheticG = 255;
    aestheticB = 0 + Math.sin(aestheticColorPhase * 2) * 150;
  } else if (colorCycle > 0) {
    // White
    aestheticR = 255;
    aestheticG = 255;
    aestheticB = 255;
  } else if (colorCycle > -0.5) {
    // Neon cyan
    aestheticR = 0 + Math.sin(aestheticColorPhase * 2) * 100;
    aestheticG = 255;
    aestheticB = 255;
  } else {
    // Black with white outline
    aestheticR = 0;
    aestheticG = 0;
    aestheticB = 0;
  }
  
  ink(aestheticR, aestheticG, aestheticB);
  const aestheticText = "AESTHETIC";
  const aestheticY = 250; // Moved up from 400 to prevent overlap
  
  // Write AESTHETIC with extra thickness for prominence
  const aestheticOptions = { center: "x", y: aestheticY, size: aestheticSizeOscillation };
  
  // Multiple overlapping layers for extra thickness and glow
  for (let layer = 0; layer < 5; layer++) {
    const layerOffset = layer * 0.8;
    const layerAlpha = 255 - layer * 30; // Fade each layer
    ink(aestheticR, aestheticG, aestheticB, layerAlpha);
    
    // Slight offset for thickness effect
    writeVHS(aestheticText, { 
      center: "x", 
      y: aestheticY + layerOffset, 
      x: centerX + layerOffset,
      size: aestheticSizeOscillation 
    }, api);
  }
  
  // COMPUTER with complementary white/black/neon oscillation
  const computerSizeOscillation = 32 + Math.sin(frameCount * 0.06 + Math.PI) * 4; // Size 28-36, opposite phase
  const computerColorPhase = frameCount * 0.08 + Math.PI; // Offset phase from AESTHETIC
  
  let computerR, computerG, computerB;
  const computerColorCycle = Math.sin(computerColorPhase);
  
  if (computerColorCycle > 0.5) {
    // White
    computerR = 255;
    computerG = 255;  
    computerB = 255;
  } else if (computerColorCycle > 0) {
    // Neon magenta
    computerR = 255;
    computerG = 0 + Math.sin(computerColorPhase * 2) * 100;
    computerB = 255;
  } else if (computerColorCycle > -0.5) {
    // Neon yellow
    computerR = 255;
    computerG = 255;
    computerB = 0 + Math.sin(computerColorPhase * 2) * 100;
  } else {
    // Black with white outline
    computerR = 0;
    computerG = 0;
    computerB = 0;
  }
  
  ink(computerR, computerG, computerB);
  const computerText = "COMPUTER";
  const computerY = 420; // Moved down from 380 for more spacing
  writeVHS(computerText, { center: "x", y: computerY, size: computerSizeOscillation }, api);

  // DATE SECTION - bigger text sizes and moved up
  ink(255, 200, 100); // Warm gold
  const septemberText = "SEPTEMBER";
  const septemberY = 700; // Moved up from 750 to give more space for 30
  writeVHS(septemberText, { center: "x", y: septemberY, size: 24 }, api); // Increased from size 20 to 24
  
  ink(255, 255, 255);
  const dayText = "30";
  const dayY = 800; // Moved up closer to September (700 + 100 = 800)
  writeVHS(dayText, { center: "x", y: dayY, size: 56 }, api); // Increased from size 48 to 56

  // TIME SECTION - moved down more and bigger text
  const leftCenter = centerX / 2; // Center of left half (512)
  const rightCenter = centerX + (centerX / 2); // Center of right half (1536)
  
  ink(150, 150, 150);
  const doorsText = "DOORS";
  const programText = "PROGRAM";
  const timeY = 1000; // Moved down more from 950
  writeVHS(doorsText, { x: leftCenter, center: "x", y: timeY, size: 10 }, api); // Increased from size 8 to 10
  writeVHS(programText, { x: rightCenter, center: "x", y: timeY, size: 10 }, api); // Increased from size 8 to 10
  
  ink(255, 255, 255);
  const time7Text = "7PM";
  const time8Text = "8PM";
  const timesY = timeY + 60;
  writeVHS(time7Text, { x: leftCenter, center: "x", y: timesY, size: 20 }, api); // Increased from size 16 to 20
  writeVHS(time8Text, { x: rightCenter, center: "x", y: timesY, size: 20 }, api); // Increased from size 16 to 20

  // VENUE SECTION - EL CID moved up and bigger text
  ink(255, 100, 100); // Red accent
  const elCidText = "EL CID";
  const elCidY = 1350; // Moved up from 1450 
  writeVHS(elCidText, { center: "x", y: elCidY, size: 32 }, api); // Increased from size 28 to 32
  
  ink(200, 200, 200);
  const addressText = "4212 W Sunset Blvd";
  const addressY = 1650; // Moved down from 1600
  writeVHS(addressText, { center: "x", y: addressY, size: 12 }, api); // Increased from size 10 to 12

  // FOOTER SECTION - above bottom left rainbow border (which starts at bottom 80px)
  ink(150, 150, 150);
  const hostedText = "Hosted by Catalyst LA";
  const hostedX = 100; // Left margin above rainbow border
  const hostedY = 1880; // Above the bottom rainbow border margin (2048 - 80 - some padding)
  writeVHS(hostedText, { x: hostedX, y: hostedY, size: 6 }, api);

  // OPTIONAL DEBUG LAYOUT BOXES - separate flashing with different colors
  const showDebug = DEBUG_MODE || (Math.sin(frameCount * 0.1) > 0.8); // Flicker occasionally
  if (showDebug) {
    ink(100, 100, 100, 50); // Semi-transparent gray
    
    // Section dividers
    box(0, 200, 2048, 2); // Top section boundary
    box(0, 600, 2048, 2); // Title section boundary  
    box(0, 900, 2048, 2); // Date section boundary
    box(0, 1100, 2048, 2); // Time section boundary (moved down)
    box(0, 1400, 2048, 2); // Venue section boundary
    box(0, 1850, 2048, 2); // Footer section boundary
    
    // Midpoint reference line
    ink(0, 255, 0, 100); // Green midpoint line
    box(0, 1024, 2048, 1); // Screen midpoint (1024)
    
    // Text bounding boxes with individual flashing and different colors
    // AESTHETIC box - Red flash
    const aestheticFlash = Math.sin(frameCount * 0.15) > 0.6;
    if (aestheticFlash) {
      ink(255, 0, 0, 60); // Red
      const aestheticWidth = aestheticText.length * aestheticSizeOscillation * 0.6; // Better width estimate
      const aestheticHeight = aestheticSizeOscillation * 1.2; // Height based on size
      const aestheticBoxX = centerX - aestheticWidth / 2;
      box(aestheticBoxX, aestheticY - aestheticHeight * 0.3, aestheticWidth, aestheticHeight);
    }
    
    // COMPUTER box - Orange flash  
    const computerFlash = Math.sin(frameCount * 0.18 + 0.5) > 0.6;
    if (computerFlash) {
      ink(255, 128, 0, 60); // Orange
      const computerWidth = computerText.length * computerSizeOscillation * 0.6; // Better width estimate
      const computerHeight = computerSizeOscillation * 1.2; // Height based on size
      const computerBoxX = centerX - computerWidth / 2;
      box(computerBoxX, computerY - computerHeight * 0.3, computerWidth, computerHeight);
    }
    
    // SEPTEMBER box - Yellow flash
    const septemberFlash = Math.sin(frameCount * 0.12 + 1.0) > 0.6;
    if (septemberFlash) {
      ink(255, 255, 0, 60); // Yellow
      const septemberWidth = septemberText.length * 24 * 0.6; // Size 24
      const septemberHeight = 24 * 1.2;
      const septemberBoxX = centerX - septemberWidth / 2;
      box(septemberBoxX, septemberY - septemberHeight * 0.3, septemberWidth, septemberHeight);
    }
    
    // Day "30" box - Green flash
    const dayFlash = Math.sin(frameCount * 0.22 + 1.5) > 0.6;
    if (dayFlash) {
      ink(0, 255, 0, 60); // Green
      const dayWidth = dayText.length * 56 * 0.6; // Size 56
      const dayHeight = 56 * 1.2;
      const dayBoxX = centerX - dayWidth / 2;
      box(dayBoxX, dayY - dayHeight * 0.3, dayWidth, dayHeight);
    }
    
    // DOORS/PROGRAM boxes - Cyan flash
    const timeFlash = Math.sin(frameCount * 0.16 + 2.0) > 0.6;
    if (timeFlash) {
      ink(0, 255, 255, 60); // Cyan
      const doorsWidth = doorsText.length * 10 * 0.6; // Size 10
      const programWidth = programText.length * 10 * 0.6; // Size 10
      const timeHeight = 10 * 1.2;
      box(leftCenter - doorsWidth / 2, timeY - timeHeight * 0.3, doorsWidth, timeHeight); // DOORS box
      box(rightCenter - programWidth / 2, timeY - timeHeight * 0.3, programWidth, timeHeight); // PROGRAM box
    }
    
    // 7PM/8PM boxes - Blue flash
    const timesFlash = Math.sin(frameCount * 0.14 + 2.5) > 0.6;
    if (timesFlash) {
      ink(0, 0, 255, 60); // Blue
      const time7Width = time7Text.length * 20 * 0.6; // Size 20
      const time8Width = time8Text.length * 20 * 0.6; // Size 20
      const timesHeight = 20 * 1.2;
      box(leftCenter - time7Width / 2, timesY - timesHeight * 0.3, time7Width, timesHeight); // 7PM box
      box(rightCenter - time8Width / 2, timesY - timesHeight * 0.3, time8Width, timesHeight); // 8PM box
    }
    
    // EL CID box - Magenta flash
    const elCidFlash = Math.sin(frameCount * 0.19 + 3.0) > 0.6;
    if (elCidFlash) {
      ink(255, 0, 255, 60); // Magenta
      const elCidWidth = text.width(elCidText, 32); // Use actual text width
      const elCidHeight = text.height(elCidText, 32); // Use actual text height
      const elCidBoxX = centerX - elCidWidth / 2;
      box(elCidBoxX, elCidY - elCidHeight * 0.1, elCidWidth, elCidHeight);
    }
    
    // Address box - Purple flash
    const addressFlash = Math.sin(frameCount * 0.13 + 3.5) > 0.6;
    if (addressFlash) {
      ink(128, 0, 255, 60); // Purple
      const addressWidth = text.width(addressText, 12); // Use actual text width
      const addressHeight = text.height(addressText, 12); // Use actual text height
      const addressBoxX = centerX - addressWidth / 2;
      box(addressBoxX, addressY - addressHeight * 0.1, addressWidth, addressHeight);
    }
    
    // Catalyst LA box - Pink flash
    const hostedFlash = Math.sin(frameCount * 0.17 + 4.0) > 0.6;
    if (hostedFlash) {
      ink(255, 0, 128, 60); // Pink
      const hostedWidth = text.width(hostedText, 6); // Use actual text width
      const hostedHeight = text.height(hostedText, 6); // Use actual text height
      box(hostedX, hostedY - hostedHeight * 0.1, hostedWidth, hostedHeight);
    }
  }
}