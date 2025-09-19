// Build Instructions:
// cd /workspaces/aesthetic-computer/reference/tools/recording && node tape.mjs elcid-flyer.mjs 240 --mp4 --resolution 2048
// TODO:
// [-] Finish copy editing / typography
// [ ] Add flair

// Track frame count for animation - will be injected by frame renderer
let frameCount = 0;

// Debug mode toggle
const DEBUG_MODE = true; // ENABLED for debugging text.box sizing issues

// Helper function for VHS-style rainbow overdraw text with SIZE-SCALED thickness and fast opacity blinking
function writeVHS(text, options, api) {
  const { write, ink } = api;
  const { x, y, center, size = 12 } = options; // Default size if not specified

  // Glitchy blink effect - MUCH FASTER blinking!
  const blinkSeed = text.length + (options.x || 0) + (options.y || 0);
  const blinkChance = Math.sin(frameCount * 0.15 + blinkSeed * 0.03) * 0.5 + 0.5; // 3x faster

  // MASSIVE FLUORESCENT BLINKING SYSTEM - multiple overlapping cycles
  const ultraFastBlink = Math.sin(frameCount * 0.8 + blinkSeed * 0.15) * 0.5 + 0.5; // Ultra fast cycle
  const megaBlink = Math.sin(frameCount * 1.2 + blinkSeed * 0.2) * 0.5 + 0.5; // Mega fast cycle  
  const superBlink = Math.sin(frameCount * 0.6 + blinkSeed * 0.1) * 0.5 + 0.5; // Super cycle
  const hyperBlink = Math.sin(frameCount * 1.8 + blinkSeed * 0.25) * 0.5 + 0.5; // Hyper fast
  
  // Combine all blink cycles for MAXIMUM fluorescent effect
  const combinedBlink = (ultraFastBlink + megaBlink + superBlink + hyperBlink) / 4;
  const intenseBlink = Math.max(ultraFastBlink, megaBlink, superBlink, hyperBlink); // Take the brightest
  
  // Enhanced opacity blinking - much more dramatic
  const opacityBlink = Math.sin(frameCount * 0.35 + blinkSeed * 0.08) * 0.4 + 0.6; // 0.2-1.0 range - more dramatic!
  const baseOpacity = 200 + (blinkChance * 55) + (intenseBlink * 50); // 200-305 base opacity - ULTRA BRIGHT!

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

  // Much more varied colors for maximum psychedelic effect - BRIGHTER!
  const colors = [
    [255, 80, 80],     // Brighter hot red
    [255, 140, 0],     // Brighter orange  
    [255, 240, 0],     // Brighter gold
    [240, 255, 80],    // Brighter lime green
    [0, 255, 140],     // Brighter spring green
    [0, 240, 255],     // Brighter sky blue
    [140, 180, 255],   // Brighter blue
    [180, 80, 255],    // Brighter purple
    [255, 80, 240],    // Brighter hot pink
    [255, 140, 255],   // Brighter magenta
    [255, 255, 140],   // Brighter electric yellow
    [140, 255, 255],   // Brighter cyan
    [255, 180, 240],   // Brighter pink
    [240, 255, 180],   // Brighter light green
    [180, 240, 255],   // Brighter light blue
    [255, 240, 180],   // Brighter peach
    [255, 80, 140],    // Brighter deep pink
    [140, 255, 80],    // Brighter neon green
    [80, 140, 255],    // Brighter electric blue
    [255, 255, 80],    // Brighter bright yellow
    [255, 80, 255],    // Brighter electric magenta
    [80, 255, 255],    // Brighter bright cyan
    [255, 140, 140],   // Brighter red
    [140, 255, 140],   // Brighter green
    [140, 140, 255],   // Brighter blue
    [255, 180, 80],    // Brighter orange
    [180, 255, 80],    // Brighter yellow-green
    [80, 180, 255],    // Brighter blue-cyan
    [255, 80, 180],    // Brighter red-magenta
    [180, 80, 255]     // Brighter blue-magenta
  ];

  // MASSIVE THICKNESS SPREAD - EXTREME DOUBLING for fluorescent effect!
  const sizeScale = size / 12; // Scale factor based on size (12 is baseline)
  const maxThickness = Math.min(30, 10 + sizeScale * 6); // 10-30 pixels max - EXTREME THICKNESS!

  // EXTENSIVE thickness offsets - MUCH MORE SPREAD for massive doubling effect
  const baseThickness = [
    // Core offsets
    [0, 0], [1, 0], [0, 1], [1, 1], [-1, 0], [0, -1], [-1, -1],
    [2, 0], [0, 2], [-2, 0], [0, -2], [2, 1], [1, 2], [-2, -1], [-1, -2],
    [2, 2], [-2, -2], [2, -2], [-2, 2], [3, 0], [0, 3], [-3, 0], [0, -3],
    [3, 1], [1, 3], [-3, 1], [1, -3], [3, -1], [-1, 3], [-3, -1], [-1, -3],
    [3, 2], [2, 3], [-3, 2], [2, -3], [3, -2], [-2, 3], [-3, -2], [-2, -3],
    [3, 3], [-3, -3], [3, -3], [-3, 3],
    // Extended spread for massive doubling
    [4, 0], [0, 4], [-4, 0], [0, -4], [4, 1], [1, 4], [-4, -1], [-1, -4],
    [4, 2], [2, 4], [-4, 2], [2, -4], [4, -2], [-2, 4], [-4, -2], [-2, -4],
    [4, 3], [3, 4], [-4, 3], [3, -4], [4, -3], [-3, 4], [-4, -3], [-3, -4],
    [4, 4], [-4, -4], [4, -4], [-4, 4],
    [5, 0], [0, 5], [-5, 0], [0, -5], [5, 1], [1, 5], [-5, -1], [-1, -5],
    [5, 2], [2, 5], [-5, 2], [2, -5], [5, -2], [-2, 5], [-5, -2], [-2, -5],
    [5, 3], [3, 5], [-5, 3], [3, -5], [5, -3], [-3, 5], [-5, -3], [-3, -5],
    [5, 4], [4, 5], [-5, 4], [4, -5], [5, -4], [-4, 5], [-5, -4], [-4, -5],
    [5, 5], [-5, -5], [5, -5], [-5, 5],
    // More extreme spread
    [6, 0], [0, 6], [-6, 0], [0, -6], [6, 1], [1, 6], [-6, -1], [-1, -6],
    [6, 2], [2, 6], [-6, 2], [2, -6], [6, -2], [-2, 6], [-6, -2], [-2, -6],
    [6, 3], [3, 6], [-6, 3], [3, -6], [6, -3], [-3, 6], [-6, -3], [-3, -6],
    [6, 4], [4, 6], [-6, 4], [4, -6], [6, -4], [-4, 6], [-6, -4], [-4, -6],
    [6, 5], [5, 6], [-6, 5], [5, -6], [6, -5], [-5, 6], [-6, -5], [-5, -6],
    [6, 6], [-6, -6], [6, -6], [-6, 6],
    [7, 0], [0, 7], [-7, 0], [0, -7], [7, 1], [1, 7], [-7, -1], [-1, -7],
    [7, 2], [2, 7], [-7, 2], [2, -7], [7, -2], [-2, 7], [-7, -2], [-2, -7],
    [7, 3], [3, 7], [-7, 3], [3, -7], [7, -3], [-3, 7], [-7, -3], [-3, -7],
    [7, 4], [4, 7], [-7, 4], [4, -7], [7, -4], [-4, 7], [-7, -4], [-4, -7],
    [7, 5], [5, 7], [-7, 5], [5, -7], [7, -5], [-5, 7], [-7, -5], [-5, -7],
    [7, 6], [6, 7], [-7, 6], [6, -7], [7, -6], [-6, 7], [-7, -6], [-6, -7],
    [7, 7], [-7, -7], [7, -7], [-7, 7],
    // Ultra extreme spread for maximum fluorescent effect
    [8, 0], [0, 8], [-8, 0], [0, -8], [8, 2], [2, 8], [-8, -2], [-2, -8],
    [8, 4], [4, 8], [-8, 4], [4, -8], [8, -4], [-4, 8], [-8, -4], [-4, -8],
    [8, 6], [6, 8], [-8, 6], [6, -8], [8, -6], [-6, 8], [-8, -6], [-6, -8],
    [8, 8], [-8, -8], [8, -8], [-8, 8],
    [9, 0], [0, 9], [-9, 0], [0, -9], [9, 3], [3, 9], [-9, -3], [-3, -9],
    [9, 6], [6, 9], [-9, 6], [6, -9], [9, -6], [-6, 9], [-9, -6], [-6, -9],
    [9, 9], [-9, -9], [9, -9], [-9, 9],
    [10, 0], [0, 10], [-10, 0], [0, -10], [10, 2], [2, 10], [-10, -2], [-2, -10],
    [10, 4], [4, 10], [-10, 4], [4, -10], [10, -4], [-4, 10], [-10, -4], [-4, -10],
    [10, 6], [6, 10], [-10, 6], [6, -10], [10, -6], [-6, 10], [-10, -6], [-6, -10],
    [10, 8], [8, 10], [-10, 8], [8, -10], [10, -8], [-8, 10], [-10, -8], [-8, -10],
    [10, 10], [-10, -10], [10, -10], [-10, 10],
    // Maximum spread for ultimate doubling
    [12, 0], [0, 12], [-12, 0], [0, -12], [12, 4], [4, 12], [-12, -4], [-4, -12],
    [12, 8], [8, 12], [-12, 8], [8, -12], [12, -8], [-8, 12], [-12, -8], [-8, -12],
    [12, 12], [-12, -12], [12, -12], [-12, 12],
    [15, 0], [0, 15], [-15, 0], [0, -15], [15, 5], [5, 15], [-15, -5], [-5, -15],
    [15, 10], [10, 15], [-15, 10], [10, -15], [15, -10], [-10, 15], [-15, -10], [-10, -15],
    [15, 15], [-15, -15], [15, -15], [-15, 15],
    [18, 0], [0, 18], [-18, 0], [0, -18], [18, 6], [6, 18], [-18, -6], [-6, -18],
    [18, 12], [12, 18], [-18, 12], [12, -18], [18, -12], [-12, 18], [-18, -12], [-12, -18],
    [18, 18], [-18, -18], [18, -18], [-18, 18],
    [20, 0], [0, 20], [-20, 0], [0, -20], [20, 10], [10, 20], [-20, -10], [-10, -20],
    [20, 20], [-20, -20], [20, -20], [-20, 20],
    [25, 0], [0, 25], [-25, 0], [0, -25], [25, 25], [-25, -25], [25, -25], [-25, 25],
    [30, 0], [0, 30], [-30, 0], [0, -30], [30, 30], [-30, -30], [30, -30], [-30, 30]
  ];

  // Filter and scale offsets to stay within maxThickness
  const thicknessOffsets = baseThickness
    .filter(([x, y]) => Math.abs(x) <= maxThickness && Math.abs(y) <= maxThickness)
    .map(([x, y]) => [x, y]); // Keep as-is, already tight

  // FOCUSED OVERDRAW for EXTREME thickness - fewer layers but MASSIVE spread per layer
  const numLayers = 3; // Increased to 3 layers for more fluorescent effect

  for (let layer = 0; layer < numLayers; layer++) {
    const copiesPerLayer = 4; // Increased from 3 to 4 copies for more fluorescent layering

    for (let i = 0; i < copiesPerLayer; i++) {
      // Mix base neon color with random colors - FAVOR RAINBOW COLORS
      const useBaseColor = Math.random() < 0.1; // Only 10% chance to use base neon (was 30%)
      let r, g, b;

      if (useBaseColor) {
        [r, g, b] = baseNeonColor;
      } else {
        const colorIndex = (i + layer * 2 + Math.floor(Math.random() * colors.length)) % colors.length;
        [r, g, b] = colors[colorIndex];
      }

      // EXTREME FLUORESCENT OPACITY - multiple blinking layers combined
      const fluorBoost = (intenseBlink > 0.8) ? 100 : 0; // Extra boost for peak blinks
      const megaBoost = (combinedBlink > 0.7) ? 80 : 0; // Combined blink boost
      const ultraBoost = (ultraFastBlink > 0.9 || hyperBlink > 0.95) ? 120 : 0; // Ultra peak boost
      
      const baseAlpha = (50 + Math.random() * 100) * opacityBlink + fluorBoost + megaBoost + ultraBoost; // 50-150 base + boosts
      const layerAlpha = baseAlpha * (1 - layer * 0.03); // Much less fade between layers
      const glitchAlpha = glitchIntensity > 0.6 ? layerAlpha * (0.8 + Math.random() * 0.4) : layerAlpha;

      // ENHANCED COLOR CYCLING for extreme fluorescent effect
      let finalR = r, finalG = g, finalB = b;
      
      if (intenseBlink > 0.999) {
        // Ultra bright white flash - much rarer (only ~2% of time)
        finalR = 255; finalG = 255; finalB = 255;
      } else if (combinedBlink > 0.99) {
        // Electric fluorescent boost - rare (only ~5% of time)
        finalR = Math.min(255, r + 100);
        finalG = Math.min(255, g + 100); 
        finalB = Math.min(255, b + 100);
      } else if (ultraFastBlink > 0.98 || hyperBlink > 0.97) {
        // Neon boost - less frequent (only ~8-10% of time)
        finalR = Math.min(255, r + 80);
        finalG = Math.min(255, g + 80);
        finalB = Math.min(255, b + 80);
      }

      ink(finalR, finalG, finalB, glitchAlpha);

      // MASSIVE THICKNESS SPREAD - use MANY more offsets for extreme doubling
      const maxOffsets = Math.min(15, thicknessOffsets.length); // Use up to 15 offsets (was 5) - MASSIVE increase!
      const numOffsets = Math.max(8, Math.min(maxOffsets, 8 + Math.floor(sizeScale * 3))); // 8-15 offsets for EXTREME thickness

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

        // INDIVIDUAL ALPHA VARIATION for each thickness layer - makes highlights vary in opacity!
        const thicknessAlphaVariation = 0.6 + Math.random() * 0.8; // 0.6-1.4x variation
        const distanceFromCenter = Math.sqrt(offsetX * offsetX + offsetY * offsetY);
        const distanceFade = Math.max(0.3, 1 - (distanceFromCenter / 20)); // Fade based on distance from center
        const flickerAlpha = Math.sin(frameCount * 0.2 + offsetX + offsetY) * 0.3 + 0.7; // Flicker effect
        const individualAlpha = glitchAlpha * thicknessAlphaVariation * distanceFade * flickerAlpha;
        
        // Apply individual alpha for this thickness layer
        ink(finalR, finalG, finalB, individualAlpha);

        const vhsOptions = { ...options };
        if (x !== undefined) vhsOptions.x = x + offsetX + randomOffsetX + wiggleX;
        if (y !== undefined) vhsOptions.y = y + offsetY + randomOffsetY + wiggleY;

        write(text, vhsOptions);
      }
    }
  }

  // ULTRA FLUORESCENT final copy with EXTREME blinking and color cycling
  const ultraRapidBlink = Math.sin(frameCount * 0.6 + blinkSeed * 0.15) * 0.4 + 0.6; // 0.2-1.0 range - more dramatic
  const megaRapidBlink = Math.sin(frameCount * 1.0 + blinkSeed * 0.2) * 0.5 + 0.5; // Secondary blink cycle
  const hyperRapidBlink = Math.sin(frameCount * 1.4 + blinkSeed * 0.25) * 0.5 + 0.5; // Tertiary super fast
  
  // Extreme clean alpha - never disappears but pulses wildly
  const cleanAlpha = (180 + Math.random() * 75) * ultraRapidBlink * opacityBlink + (intenseBlink * 100); // 180-255 + boost
  
  // EXTREME COLOR CYCLING for final copy
  let finalCleanR = baseNeonColor[0], finalCleanG = baseNeonColor[1], finalCleanB = baseNeonColor[2];
  
  if (megaRapidBlink > 0.95 || hyperRapidBlink > 0.98) {
    // Ultra bright white fluorescent flash
    finalCleanR = 255; finalCleanG = 255; finalCleanB = 255;
  } else if (ultraRapidBlink > 0.8 || intenseBlink > 0.85) {
    // Electric neon boost
    finalCleanR = Math.min(255, baseNeonColor[0] * 1.5);
    finalCleanG = Math.min(255, baseNeonColor[1] * 1.5);
    finalCleanB = Math.min(255, baseNeonColor[2] * 1.5);
  } else {
    // Enhanced base color with blink modulation
    finalCleanR = Math.min(255, baseNeonColor[0] * (1 + ultraRapidBlink * 0.8));
    finalCleanG = Math.min(255, baseNeonColor[1] * (1 + ultraRapidBlink * 0.8));
    finalCleanB = Math.min(255, baseNeonColor[2] * (1 + ultraRapidBlink * 0.8));
  }
  
  // REMOVED: Final clean copy that was overriding all the colorful layers
  // This was making all text appear as a single color instead of the rainbow effect
  // ink(finalCleanR, finalCleanG, finalCleanB, cleanAlpha);
  // write(text, finalOptions);
}

// Helper function to draw a dynamic star with crazy lines and particle trails - ENHANCED!
function drawDynamicStar(x, y, size, frame, api) {
  const { line, ink } = api;
  const time = frame * 0.15; // Faster animation speed
  const numRays = 24; // More rays for even crazier effect (was 16)
  const innerRadius = size * 0.4; // Slightly larger inner
  const outerRadius = size * 1.8; // MUCH bigger outer radius (was 1.2)

  // Enhanced pulsing effect with multiple layers
  const pulse = 1 + Math.sin(time * 4) * 0.5; // More dramatic pulsing
  const breathe = 1 + Math.sin(time * 2.3 + 1.2) * 0.3; // Secondary breathing
  const currentInner = innerRadius * pulse * breathe;
  const currentOuter = outerRadius * pulse * breathe;

  // Faster, more chaotic rotation
  const rotation = time * 3;
  const wobbleRotation = Math.sin(time * 1.7) * 0.5; // Wobbling rotation

  // Draw crazy star rays with MORE layers
  for (let layer = 0; layer < 5; layer++) { // More layers (was 3)
    for (let i = 0; i < numRays; i++) {
      const angle = (i / numRays) * Math.PI * 2 + rotation + wobbleRotation + (layer * 0.15);

      // Much more varied ray lengths for each layer
      const rayLength = currentOuter * (1 - layer * 0.15) * (0.7 + Math.random() * 0.6); // Random length variation

      // Outer ray with some chaos
      const outerX = x + Math.cos(angle) * rayLength;
      const outerY = y + Math.sin(angle) * rayLength;

      // Inner point between rays - also chaotic
      const innerAngle = angle + (Math.PI / numRays) + Math.sin(time + i) * 0.2;
      const innerX = x + Math.cos(innerAngle) * currentInner;
      const innerY = y + Math.sin(innerAngle) * currentInner;

      // MASSIVELY EXPANDED polychromatic color system
      const colorIndex = (i + layer + Math.floor(time * 3)) % 16; // 16 colors instead of 9
      const colorShift = Math.sin(time * 0.8 + i * 0.3) * 127 + 128; // Color shifting
      const layerFade = 255 - (layer * 30); // Layer fade effect
      
      switch (colorIndex) {
        case 0: ink(255, 80 + colorShift*0.3, 150, layerFade); break;  // Hot Pink
        case 1: ink(255, 180 + colorShift*0.2, 80, layerFade); break;  // Orange
        case 2: ink(255, 255, 80 + colorShift*0.4, layerFade); break;  // Electric Yellow
        case 3: ink(150 + colorShift*0.3, 255, 80, layerFade); break;  // Lime
        case 4: ink(80, 255, 180 + colorShift*0.2, layerFade); break;  // Spring Green
        case 5: ink(80, 255, 255, layerFade); break;                   // Cyan
        case 6: ink(80 + colorShift*0.3, 180, 255, layerFade); break;  // Sky Blue
        case 7: ink(150, 80, 255, layerFade); break;                   // Electric Purple
        case 8: ink(255, 80, 255, layerFade); break;                   // Magenta
        case 9: ink(255, 150 + colorShift*0.2, 200, layerFade); break; // Rose
        case 10: ink(255, 200, 80 + colorShift*0.4, layerFade); break; // Gold
        case 11: ink(200, 255, 120, layerFade); break;                 // Yellow-Green
        case 12: ink(120, 255, 200 + colorShift*0.2, layerFade); break; // Aqua
        case 13: ink(120, 200 + colorShift*0.3, 255, layerFade); break; // Light Blue
        case 14: ink(180 + colorShift*0.2, 120, 255, layerFade); break; // Lavender
        case 15: ink(255, 120, 180 + colorShift*0.3, layerFade); break; // Pink
      }

      // Draw main ray
      line(x, y, outerX, outerY);

      // Draw connecting lines between rays for web effect
      if (i < numRays - 1) {
        const nextAngle = ((i + 1) / numRays) * Math.PI * 2 + rotation + wobbleRotation + (layer * 0.15);
        const nextRayLength = currentOuter * (1 - layer * 0.15) * (0.7 + Math.random() * 0.6);
        const nextOuterX = x + Math.cos(nextAngle) * nextRayLength;
        const nextOuterY = y + Math.sin(nextAngle) * nextRayLength;
        line(outerX, outerY, nextOuterX, nextOuterY);
      }

      // MORE particle trail effects - more chaotic
      for (let p = 0; p < 5; p++) { // More particles (was 3)
        const trailAngle = angle + (Math.random() - 0.5) * 1.0; // More random spread
        const trailLength = rayLength * (0.3 + Math.random() * 0.8);
        const trailX = x + Math.cos(trailAngle) * trailLength;
        const trailY = y + Math.sin(trailAngle) * trailLength;

        // More varied trail alpha
        const trailAlpha = 80 + Math.random() * 120;
        line(x + (Math.random()-0.5)*4, y + (Math.random()-0.5)*4, trailX, trailY); // Slightly offset origin for more chaos
      }
    }
  }
}

// Helper function to draw animated robot faces with different emotions
function drawRobotFace(x, y, size, emotion, frame, api) {
  const { ink, circle, box, line } = api;
  
  // Calculate face dimensions
  const faceSize = size * 0.8; // Face takes up 80% of the square
  const centerX = x + size / 2;
  const centerY = y + size / 2;
  
  // Eye dimensions
  const eyeSize = faceSize * 0.15;
  const eyeY = centerY - faceSize * 0.15;
  const leftEyeX = centerX - faceSize * 0.2;
  const rightEyeX = centerX + faceSize * 0.2;
  
  // Mouth dimensions
  const mouthWidth = faceSize * 0.4;
  const mouthHeight = faceSize * 0.1;
  const mouthY = centerY + faceSize * 0.2;
  
  // Colorful blinking effect - each face has unique timing
  const blinkSeed = x + y;
  const blinkTime = frame * 0.3 + blinkSeed * 0.1;
  const colorShift = frame * 0.2 + blinkSeed * 0.05;
  
  // Convert emotion to numeric value for calculations
  let emotionValue = 0;
  switch (emotion) {
    case 'happy': emotionValue = 0; break;
    case 'angry': emotionValue = 1; break;
    case 'medium': emotionValue = 2; break;
    case 'sad': emotionValue = 3; break;
    default: emotionValue = 0; break;
  }
  
  // Dynamic color cycling for robot features
  const hue = (colorShift + emotionValue * 90) % 360;
  const eyeR = Math.max(0, Math.min(255, 128 + Math.sin(hue * Math.PI / 180) * 127));
  const eyeG = Math.max(0, Math.min(255, 128 + Math.sin((hue + 120) * Math.PI / 180) * 127));
  const eyeB = Math.max(0, Math.min(255, 128 + Math.sin((hue + 240) * Math.PI / 180) * 127));
  
  // Blinking alpha animation with safety checks
  const blinkPhase = Math.sin(blinkTime);
  const eyeAlpha = Math.max(45, Math.min(255, 150 + blinkPhase * 105)); // 45-255 range for dramatic blinking
  
  // Ensure no NaN values
  if (isNaN(eyeR) || isNaN(eyeG) || isNaN(eyeB) || isNaN(eyeAlpha)) {
    return; // Skip drawing if invalid values
  }
  
  // Draw eyes based on emotion
  ink(eyeR, eyeG, eyeB, eyeAlpha);
  
  switch (emotion) {
    case 'happy':
      // Round happy eyes
      circle(leftEyeX, eyeY, eyeSize);
      circle(rightEyeX, eyeY, eyeSize);
      break;
    case 'angry':
      // Angular angry eyes
      box(leftEyeX - eyeSize/2, eyeY - eyeSize/2, eyeSize, eyeSize);
      box(rightEyeX - eyeSize/2, eyeY - eyeSize/2, eyeSize, eyeSize);
      // Angry eyebrows
      ink(255, 0, 0, eyeAlpha);
      line(leftEyeX - eyeSize, eyeY - eyeSize, leftEyeX + eyeSize/2, eyeY - eyeSize/2);
      line(rightEyeX + eyeSize, eyeY - eyeSize, rightEyeX - eyeSize/2, eyeY - eyeSize/2);
      break;
    case 'sad':
      // Droopy sad eyes
      circle(leftEyeX, eyeY + eyeSize/4, eyeSize * 0.8);
      circle(rightEyeX, eyeY + eyeSize/4, eyeSize * 0.8);
      break;
    case 'medium':
    default:
      // Neutral rectangular eyes
      box(leftEyeX - eyeSize/2, eyeY - eyeSize/3, eyeSize, eyeSize * 0.6);
      box(rightEyeX - eyeSize/2, eyeY - eyeSize/3, eyeSize, eyeSize * 0.6);
      break;
  }
  
  // Draw mouth based on emotion with animated colors
  const mouthR = Math.max(0, Math.min(255, 128 + Math.sin((hue + 60) * Math.PI / 180) * 127));
  const mouthG = Math.max(0, Math.min(255, 128 + Math.sin((hue + 180) * Math.PI / 180) * 127));
  const mouthB = Math.max(0, Math.min(255, 128 + Math.sin((hue + 300) * Math.PI / 180) * 127));
  const mouthAlpha = Math.max(0, Math.min(255, eyeAlpha * 0.9));
  
  // Ensure no NaN values for mouth
  if (isNaN(mouthR) || isNaN(mouthG) || isNaN(mouthB) || isNaN(mouthAlpha)) {
    return; // Skip mouth drawing if invalid values
  }
  
  ink(mouthR, mouthG, mouthB, mouthAlpha);
  
  switch (emotion) {
    case 'happy':
      // Smiling mouth (arc)
      for (let i = 0; i < mouthWidth; i++) {
        const progress = i / mouthWidth;
        const curve = Math.sin(progress * Math.PI) * mouthHeight;
        const mouthX = centerX - mouthWidth/2 + i;
        circle(mouthX, mouthY - curve, 2);
      }
      break;
    case 'angry':
      // Frowning mouth
      for (let i = 0; i < mouthWidth; i++) {
        const progress = i / mouthWidth;
        const curve = Math.sin(progress * Math.PI) * mouthHeight;
        const mouthX = centerX - mouthWidth/2 + i;
        circle(mouthX, mouthY + curve, 2);
      }
      break;
    case 'sad':
      // Downturned mouth
      line(centerX - mouthWidth/2, mouthY, centerX, mouthY + mouthHeight);
      line(centerX, mouthY + mouthHeight, centerX + mouthWidth/2, mouthY);
      break;
    case 'medium':
    default:
      // Straight line mouth
      line(centerX - mouthWidth/2, mouthY, centerX + mouthWidth/2, mouthY);
      break;
  }
}

// Helper function to draw a border square with background color and robot face
function drawBorderSquare(x, y, squareSize, colorIndex, segmentIndex, frame, api) {
  const { ink, box } = api;
  
  // Color palettes for different border sides
  const colorPalettes = {
    top: [
      [25, 100, 128], [50, 75, 128], [0, 128, 100], [75, 50, 128],
      [0, 75, 128], [50, 128, 100], [100, 50, 128], [25, 128, 75]
    ],
    left: [
      [128, 75, 25], [128, 100, 50], [128, 50, 75], [128, 25, 50],
      [100, 128, 50], [128, 50, 100], [128, 100, 25], [75, 128, 50]
    ],
    right: [
      [128, 0, 75], [128, 50, 25], [128, 75, 50], [128, 0, 128],
      [128, 100, 75], [128, 25, 0], [128, 50, 0], [128, 75, 100]
    ],
    bottom: [
      [100, 75, 0], [128, 100, 25], [128, 75, 0], [128, 50, 25],
      [100, 128, 50], [128, 128, 0], [128, 100, 50], [75, 50, 0]
    ]
  };
  
  // Determine which border this square belongs to
  let palette, borderType;
  if (y <= 80) {
    palette = colorPalettes.top;
    borderType = 'top';
  } else if (x <= 80) {
    palette = colorPalettes.left;
    borderType = 'left';
  } else if (x >= 2048 - 80) {
    palette = colorPalettes.right;
    borderType = 'right';
  } else {
    palette = colorPalettes.bottom;
    borderType = 'bottom';
  }
  
  // Get base color from palette
  const [r, g, b] = palette[colorIndex % palette.length];
  
  // Apply hue rotation and oscillating opacity
  const hueRotation = (frame * 0.05 + segmentIndex * 0.3) % (Math.PI * 2);
  const rotatedR = Math.max(0, Math.min(255, r * Math.cos(hueRotation) - g * Math.sin(hueRotation)));
  const rotatedG = Math.max(0, Math.min(255, r * Math.sin(hueRotation) + g * Math.cos(hueRotation)));
  
  // Individual square opacity animation
  const opacityPhase = frame * 0.25 + segmentIndex * 0.4;
  const opacity = Math.max(0, Math.min(255, 60 + Math.sin(opacityPhase) * 90));
  
  // Ensure no NaN values before drawing
  if (isNaN(rotatedR) || isNaN(rotatedG) || isNaN(b) || isNaN(opacity)) {
    return; // Skip drawing if invalid values
  }
  
  // Draw background square
  ink(rotatedR, rotatedG, b, opacity);
  box(x, y, squareSize, squareSize);
  
  // Choose emotion based on square position and time for variety
  const emotions = ['happy', 'angry', 'medium', 'sad'];
  const emotionIndex = (segmentIndex + Math.floor(frame * 0.05)) % emotions.length;
  const emotion = emotions[emotionIndex];
  
  // Draw robot face on top of background
  drawRobotFace(x, y, squareSize, emotion, frame, api);
}

export function paint({ api, frameIndex = 0, frameTime = 0, simCount = 0n }) {
  const { wipe, ink, line, box, circle, write, point, blur, scroll, zoom, spin, text, contrast, gizmo } = api;

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

  // CYBERPUNK ROBOT FACE BORDER at top - neo-matrix themed!
  const colorBarHeight = 80; // Height of the color strip
  const squareSize = 80; // Size of each square (same as height for perfect squares)
  const numSquares = Math.floor(2048 / squareSize); // Number of squares that fit

  // Color animation - shift colors over time
  const colorTime = frameCount * 0.02;
  const colorShift = frameCount * 0.1; // How fast colors cycle

  // TOP border - robot face squares
  for (let square = 0; square < numSquares; square++) {
    const x = square * squareSize;
    const colorIndex = (square + Math.floor(colorShift)) % 8;
    
    // Draw border square with robot face
    drawBorderSquare(x, 0, squareSize, colorIndex, square, frameCount, api);
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

  // LEFT SIDE Cyberpunk robot face border - warmer color palette (oranges, reds, yellows)
  const leftBorderWidth = 80; // Match the top border height
  const leftSquareSize = 80; // Square size same as width
  const leftNumSquares = Math.floor(2048 / leftSquareSize); // Number of squares vertically

  // LEFT border - robot face squares
  for (let square = 0; square < leftNumSquares; square++) {
    const y = square * leftSquareSize;
    const colorIndex = (square + Math.floor(frameCount * 0.1)) % 8;
    
    // Draw border square with robot face
    drawBorderSquare(0, y, leftSquareSize, colorIndex, square, frameCount, api);
  }

  // RIGHT SIDE Cyberpunk robot face border - warmer color palette (reds, pinks, oranges)
  const rightBorderWidth = 80; // Match the top border height
  const rightX = 2048 - rightBorderWidth;
  const rightSquareSize = 80; // Square size same as width
  const rightNumSquares = Math.floor(2048 / rightSquareSize); // Number of squares vertically

  // RIGHT border - robot face squares
  for (let square = 0; square < rightNumSquares; square++) {
    const y = square * rightSquareSize;
    const colorIndex = (square + Math.floor(frameCount * 0.12)) % 8;
    
    // Draw border square with robot face
    drawBorderSquare(rightX, y, rightSquareSize, colorIndex, square, frameCount, api);
  }

  // BOTTOM Cyberpunk robot face border - warmer color palette (yellows, oranges, reds)
  const bottomBorderHeight = 80; // Match the top border height
  const bottomY = 2048 - bottomBorderHeight;
  const bottomSquareSize = 80; // Square size same as height
  const bottomNumSquares = Math.floor(2048 / bottomSquareSize); // Number of squares horizontally

  // BOTTOM border - robot face squares
  for (let square = 0; square < bottomNumSquares; square++) {
    const x = square * bottomSquareSize;
    const colorIndex = (square + Math.floor(frameCount * 0.11)) % 8;
    
    // Draw border square with robot face
    drawBorderSquare(x, bottomY, bottomSquareSize, colorIndex, square, frameCount, api);
  }

  // Add cyberpunk sparkles to all borders - each border has themed sparkles
  for (let sparkle = 0; sparkle < 25; sparkle++) {
    const sparkleSize = 1 + Math.random() * 2;
    const sparkleAlpha = 120 + Math.random() * 135;

    // Left border sparkles - Electric Blue theme
    if (sparkle < 8) {
      const sparkleX = Math.random() * 80; // leftBorderWidth = 80
      const sparkleY = Math.random() * 2048;
      ink(0, 200, 255, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }

    // Right border sparkles - Hot Pink theme
    if (sparkle >= 8 && sparkle < 16) {
      const sparkleX = rightX + Math.random() * 80; // rightBorderWidth = 80
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
        box(screenX - pulseSize / 2, screenY - pulseSize / 2, pulseSize, pulseSize);
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
        vx: (Math.random() - 0.5) * 12, // Faster velocity X: -6 to +6
        vy: (Math.random() - 0.5) * 12, // Faster velocity Y: -6 to +6
        size: 15 + Math.random() * 35,  // LARGER Size: 15-50 pixels (was 5-20)
        colorIndex: i % 20,             // More color variety (was 12)
        rotationSpeed: (Math.random() - 0.5) * 0.4, // Faster rotation
        rotation: 0,
        pulseOffset: Math.random() * Math.PI * 2,
        wobblePhase: Math.random() * Math.PI * 2, // For wonky movement
        colorShift: Math.random() * Math.PI * 2   // For color cycling
      });
    }
  }

  // Update and render bouncing stars
  for (let i = 0; i < numBouncingStars; i++) {
    const star = global.bouncingStars[i];

    // WONKY movement - add wobble and chaotic behavior
    const wobbleX = Math.sin(frameCount * 0.1 + star.wobblePhase) * 3;
    const wobbleY = Math.cos(frameCount * 0.12 + star.wobblePhase + 1.5) * 2.5;
    const chaoticX = Math.sin(frameCount * 0.15 + i * 0.7) * 1.5;
    const chaoticY = Math.cos(frameCount * 0.17 + i * 0.9) * 1.8;

    // Update position with wonky movement
    star.x += star.vx + wobbleX + chaoticX;
    star.y += star.vy + wobbleY + chaoticY;

    // Bounce off screen edges with more chaotic bouncing
    if (star.x <= star.size || star.x >= 2048 - star.size) {
      star.vx = -star.vx * (0.8 + Math.random() * 0.4); // Random bounce energy
      star.x = Math.max(star.size, Math.min(2048 - star.size, star.x));
      star.wobblePhase += 0.5; // Change wobble pattern on bounce
    }
    if (star.y <= star.size || star.y >= 2048 - star.size) {
      star.vy = -star.vy * (0.8 + Math.random() * 0.4); // Random bounce energy
      star.y = Math.max(star.size, Math.min(2048 - star.size, star.y));
      star.wobblePhase += 0.3; // Change wobble pattern on bounce
    }

    // Update rotation with more chaos
    star.rotation += star.rotationSpeed * (1 + Math.sin(frameCount * 0.08 + i) * 0.5);

    // Enhanced pulsing size effect - MORE DRAMATIC
    const pulseFactor = Math.sin(frameCount * 0.12 + star.pulseOffset) * 0.6 + 1.0; // 0.4-1.6x - much more dramatic!
    const breatheFactor = Math.sin(frameCount * 0.05 + star.pulseOffset * 1.3) * 0.3 + 1.0; // Additional breathing
    const currentSize = star.size * pulseFactor * breatheFactor;

    // MASSIVELY EXPANDED POLYCHROMATIC COLOR SYSTEM with cycling
    const colorCycle = Math.sin(frameCount * 0.08 + star.colorShift) * 0.5 + 0.5;
    const brightness = 180 + Math.sin(frameCount * 0.1 + i * 0.3) * 75; // 105-255 - brighter!
    const saturation = 0.8 + Math.sin(frameCount * 0.06 + i * 0.8) * 0.2; // 0.6-1.0 saturation variation
    
    let r, g, b;
    switch (star.colorIndex % 20) { // 20 colors instead of 12
      case 0: [r, g, b] = [255, 50, 150]; break;  // Hot Pink
      case 1: [r, g, b] = [255, 100, 50]; break;  // Burnt Orange
      case 2: [r, g, b] = [255, 255, 50]; break;  // Electric Yellow
      case 3: [r, g, b] = [150, 255, 50]; break;  // Lime Green
      case 4: [r, g, b] = [50, 255, 150]; break;  // Spring Green
      case 5: [r, g, b] = [50, 255, 255]; break;  // Cyan
      case 6: [r, g, b] = [50, 150, 255]; break;  // Sky Blue
      case 7: [r, g, b] = [100, 50, 255]; break;  // Electric Purple
      case 8: [r, g, b] = [200, 50, 255]; break;  // Violet
      case 9: [r, g, b] = [255, 50, 200]; break;  // Magenta
      case 10: [r, g, b] = [255, 150, 100]; break; // Peach
      case 11: [r, g, b] = [200, 255, 100]; break; // Yellow-Green
      case 12: [r, g, b] = [100, 255, 200]; break; // Aqua
      case 13: [r, g, b] = [100, 200, 255]; break; // Light Blue
      case 14: [r, g, b] = [150, 100, 255]; break; // Lavender
      case 15: [r, g, b] = [255, 100, 150]; break; // Rose
      case 16: [r, g, b] = [255, 200, 50]; break;  // Gold
      case 17: [r, g, b] = [200, 255, 50]; break;  // Chartreuse
      case 18: [r, g, b] = [50, 255, 100]; break;  // Mint
      case 19: [r, g, b] = [255, 50, 100]; break;  // Crimson
    }

    // Apply color cycling for even more wonkiness
    const cycleMod = colorCycle * 255;
    r = Math.min(255, r + Math.sin(frameCount * 0.05 + i) * cycleMod * 0.3);
    g = Math.min(255, g + Math.sin(frameCount * 0.07 + i + 2) * cycleMod * 0.3);
    b = Math.min(255, b + Math.sin(frameCount * 0.06 + i + 4) * cycleMod * 0.3);

    // Apply brightness and saturation scaling
    const finalR = Math.min(255, (r * brightness * saturation) / 255);
    const finalG = Math.min(255, (g * brightness * saturation) / 255);
    const finalB = Math.min(255, (b * brightness * saturation) / 255);

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
    ink("rainbow", 12).box(0, 0, 2048, 2048); // Clear with black ink to prevent trails
  } else if (Math.random() > 0.90) {
    ink(0, 24).box(128, 128, 2048 - 256, 2048 - 256);
  }

  // Apply subtle blur for polish
  blur(8); // Reduced blur strength to reduce processing load

  // Spin and scroll effects
  // if (Math.random() > 0.5) {
  //    spin(-1);
  //  } else if (Math.random() > 0.5) {
  //    spin(1);
  //  }

  // Dynamic cardinal direction scroll - changes every 1 second (60 frames)
  const scrollCycle = Math.floor(frameCount / 240) % 4; // 4 directions, 1 second each

  switch (scrollCycle) {
    case 0: scroll(0, 1); break;  // Up
    case 1: scroll(1, 0); break;   // Right  
    case 2: scroll(0, -1); break;   // Down
    case 3: scroll(-1, 0); break;  // Left
  }

  const spinCycle = Math.floor(frameCount / 80) % 4; // 4 directions, 1 second each

  switch (spinCycle) {
    case 0: break; 
    case 1: spin(-1); break;
    case 2: break; 
    case 3: spin(1); break;
  }

  zoom(0.999);

  // Center point for all text
  const centerX = 1024; // Half of 2048 for proper centering

  // INTELLIGENT LAYOUT for 2048x2048 canvas
  // Vertical sections: Main titles (200-600), Date (600-900), Time (900-1200), Venue (1200-1600), Footer (1600-2048)

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
  const timeY = 990; // Moved up slightly to create more space below
  writeVHS(doorsText, { x: leftCenter, center: "x", y: timeY, size: 14 }, api); // Increased from size 10 to 14
  writeVHS(programText, { x: rightCenter, center: "x", y: timeY, size: 14 }, api); // Increased from size 10 to 14

  ink(255, 255, 255);
  const time7Text = "7PM";
  const time8Text = "8PM";
  const timesY = timeY + 80; // Increased gap from 60 to 80 pixels
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
  writeVHS(hostedText, { x: hostedX, y: hostedY, size: 10 }, api); // Increased from size 6 to 10

  // OPTIONAL DEBUG LAYOUT BOXES - CHAOTIC BLINKING with constantly changing colors
  const showDebug = DEBUG_MODE; // Always show when debug mode is on
  if (showDebug) {
    ink(100, 100, 100, 30); // Semi-transparent gray for section dividers

    // Section dividers
    box(0, 200, 2048, 2); // Top section boundary
    box(0, 600, 2048, 2); // Title section boundary  
    box(0, 900, 2048, 2); // Date section boundary
    box(0, 1100, 2048, 2); // Time section boundary (moved down)
    box(0, 1400, 2048, 2); // Venue section boundary
    box(0, 1850, 2048, 2); // Footer section boundary

    // Midpoint reference line
    ink(0, 255, 0, 80); // Green midpoint line - low opacity
    box(0, 1024, 2048, 1); // Screen midpoint (1024)

    // CHAOTIC COLOR-CHANGING TEXT BOUNDING BOXES - mostly invisible with rare bright flashes
    // AESTHETIC box - Chaotic rainbow colors - mostly invisible with rare bright flashes
    const aestheticChaos = Math.sin(frameCount * 0.05 + 0.1) * 0.5 + 0.5; // Much slower
    const aestheticFlash = Math.sin(frameCount * 0.3 + 0.7) * 0.5 + 0.5; // Flash cycle
    const aestheticRareBlink = aestheticFlash > 0.95 ? 25 : 1; // Rare bright flash or nearly invisible
    const aestheticR = Math.floor(128 + Math.sin(frameCount * 0.04) * 127);
    const aestheticG = Math.floor(128 + Math.sin(frameCount * 0.06 + 1.2) * 127);
    const aestheticB = Math.floor(128 + Math.sin(frameCount * 0.07 + 2.4) * 127);
    ink(aestheticR, aestheticG, aestheticB, aestheticRareBlink); // 1 or 25 opacity - mostly invisible!
    console.log(`🔍 DEBUG: AESTHETIC text="${aestheticText}" size=${aestheticSizeOscillation}`);
    const aestheticTextBox = text.box(aestheticText, { x: centerX, y: aestheticY }, undefined, aestheticSizeOscillation);
    console.log(`🔍 DEBUG: AESTHETIC text.box result:`, aestheticTextBox);
    if (aestheticTextBox && aestheticTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = aestheticTextBox.box;
      // Center the box horizontally since we're using center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: AESTHETIC drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // COMPUTER box - Chaotic magenta/cyan colors - mostly invisible with rare bright flashes
    const computerChaos = Math.sin(frameCount * 0.06 + 1.5) * 0.5 + 0.5; // Much slower
    const computerFlash = Math.sin(frameCount * 0.25 + 2.3) * 0.5 + 0.5; // Flash cycle
    const computerRareBlink = computerFlash > 0.92 ? 28 : 1; // Rare bright flash or nearly invisible
    const computerR = Math.floor(64 + Math.sin(frameCount * 0.035 + 3.1) * 191);
    const computerG = Math.floor(64 + Math.sin(frameCount * 0.055 + 4.7) * 191);
    const computerB = Math.floor(128 + Math.sin(frameCount * 0.075 + 1.8) * 127);
    ink(computerR, computerG, computerB, computerRareBlink); // 1 or 28 opacity - mostly invisible!
    console.log(`🔍 DEBUG: COMPUTER text="${computerText}" size=${computerSizeOscillation}`);
    const computerTextBox = text.box(computerText, { x: centerX, y: computerY }, undefined, computerSizeOscillation);
    console.log(`🔍 DEBUG: COMPUTER text.box result:`, computerTextBox);
    if (computerTextBox && computerTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = computerTextBox.box;
      // Center the box horizontally since we're using center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: COMPUTER drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // SEPTEMBER box - Chaotic yellow/purple colors - mostly invisible with rare bright flashes
    const septemberChaos = Math.sin(frameCount * 0.07 + 2.8) * 0.5 + 0.5; // Much slower
    const septemberFlash = Math.sin(frameCount * 0.35 + 4.1) * 0.5 + 0.5; // Flash cycle
    const septemberRareBlink = septemberFlash > 0.93 ? 22 : 1; // Rare bright flash or nearly invisible
    const septemberR = Math.floor(128 + Math.sin(frameCount * 0.045 + 5.2) * 127);
    const septemberG = Math.floor(100 + Math.sin(frameCount * 0.065 + 0.9) * 155);
    const septemberB = Math.floor(64 + Math.sin(frameCount * 0.055 + 3.7) * 191);
    ink(septemberR, septemberG, septemberB, septemberRareBlink); // 1 or 22 opacity - mostly invisible!
    console.log(`🔍 DEBUG: SEPTEMBER text="${septemberText}" size=24`);
    const septemberTextBox = text.box(septemberText, { x: centerX, y: septemberY }, undefined, 24);
    console.log(`🔍 DEBUG: SEPTEMBER text.box result:`, septemberTextBox);
    if (septemberTextBox && septemberTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = septemberTextBox.box;
      // Center the box horizontally since we're using center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: SEPTEMBER drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // Day "30" box - Chaotic orange/blue colors - mostly invisible with rare bright flashes
    const dayChaos = Math.sin(frameCount * 0.045 + 1.7) * 0.5 + 0.5; // Much slower
    const dayFlash = Math.sin(frameCount * 0.28 + 3.6) * 0.5 + 0.5; // Flash cycle
    const dayRareBlink = dayFlash > 0.94 ? 30 : 1; // Rare bright flash or nearly invisible
    const dayR = Math.floor(200 + Math.sin(frameCount * 0.025 + 2.4) * 55);
    const dayG = Math.floor(100 + Math.sin(frameCount * 0.075 + 1.1) * 155);
    const dayB = Math.floor(50 + Math.sin(frameCount * 0.065 + 4.9) * 205);
    ink(dayR, dayG, dayB, dayRareBlink); // 1 or 30 opacity - mostly invisible!
    console.log(`🔍 DEBUG: DAY text="${dayText}" size=56`);
    const dayTextBox = text.box(dayText, { x: centerX, y: dayY }, undefined, 56);
    console.log(`🔍 DEBUG: DAY text.box result:`, dayTextBox);
    if (dayTextBox && dayTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = dayTextBox.box;
      // Center the box horizontally since we're using center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: DAY drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // DOORS/PROGRAM boxes - Chaotic teal/pink colors - mostly invisible with rare bright flashes
    const doorsChaos = Math.sin(frameCount * 0.065 + 0.8) * 0.5 + 0.5; // Much slower
    const doorsFlash = Math.sin(frameCount * 0.32 + 5.2) * 0.5 + 0.5; // Flash cycle
    const doorsBlink = doorsFlash > 0.91 ? 26 : 1; // Rare bright flash or nearly invisible
    const doorsR = Math.floor(80 + Math.sin(frameCount * 0.035 + 1.6) * 175);
    const doorsG = Math.floor(150 + Math.sin(frameCount * 0.078 + 3.2) * 105);
    const doorsB = Math.floor(120 + Math.sin(frameCount * 0.058 + 2.1) * 135);
    ink(doorsR, doorsG, doorsB, doorsBlink); // 1 or 26 opacity - mostly invisible!
    console.log(`🔍 DEBUG: DOORS text="${doorsText}" size=10 at leftCenter=${leftCenter}`);
    const doorsTextBox = text.box(doorsText, { x: leftCenter, y: timeY }, undefined, 10);
    console.log(`🔍 DEBUG: DOORS text.box result:`, doorsTextBox);
    if (doorsTextBox && doorsTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = doorsTextBox.box;
      // Center the box horizontally since DOORS uses center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: DOORS drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }
    
    // PROGRAM uses same chaotic colors but different phase - subtle opacity
    const programR = Math.floor(150 + Math.sin(frameCount * 0.042 + 0.5) * 105);
    const programG = Math.floor(80 + Math.sin(frameCount * 0.072 + 5.1) * 175);
    const programB = Math.floor(60 + Math.sin(frameCount * 0.052 + 2.8) * 195);
    ink(programR, programG, programB, doorsBlink); // 1 or 26 opacity - mostly invisible!
    console.log(`🔍 DEBUG: PROGRAM text="${programText}" size=10 at rightCenter=${rightCenter}`);
    const programTextBox = text.box(programText, { x: rightCenter, y: timeY }, undefined, 10);
    console.log(`🔍 DEBUG: PROGRAM text.box result:`, programTextBox);
    if (programTextBox && programTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = programTextBox.box;
      // Center the box horizontally since PROGRAM uses center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: PROGRAM drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // 7PM/8PM boxes - Chaotic violet/lime colors - mostly invisible with rare bright flashes
    const timesChaos = Math.sin(frameCount * 0.075 + 3.4) * 0.5 + 0.5; // Much slower
    const timesFlash = Math.sin(frameCount * 0.27 + 0.3) * 0.5 + 0.5; // Flash cycle
    const timesBlink = timesFlash > 0.92 ? 24 : 1; // Rare bright flash or nearly invisible
    const times7R = Math.floor(120 + Math.sin(frameCount * 0.03 + 4.6) * 135);
    const times7G = Math.floor(200 + Math.sin(frameCount * 0.08 + 2.3) * 55);
    const times7B = Math.floor(180 + Math.sin(frameCount * 0.05 + 1.4) * 75);
    ink(times7R, times7G, times7B, timesBlink); // 1 or 24 opacity - mostly invisible!
    console.log(`🔍 DEBUG: 7PM text="${time7Text}" size=20 at leftCenter=${leftCenter}`);
    const time7TextBox = text.box(time7Text, { x: leftCenter, y: timesY }, undefined, 20);
    console.log(`🔍 DEBUG: 7PM text.box result:`, time7TextBox);
    if (time7TextBox && time7TextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = time7TextBox.box;
      // Center the box horizontally since 7PM uses center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: 7PM drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }
    
    // 8PM uses different chaotic color phase - subtle opacity
    const times8R = Math.floor(200 + Math.sin(frameCount * 0.032 + 1.8) * 55);
    const times8G = Math.floor(80 + Math.sin(frameCount * 0.068 + 4.2) * 175);
    const times8B = Math.floor(160 + Math.sin(frameCount * 0.048 + 3.1) * 95);
    ink(times8R, times8G, times8B, timesBlink); // 1 or 24 opacity - mostly invisible!
    console.log(`🔍 DEBUG: 8PM text="${time8Text}" size=20 at rightCenter=${rightCenter}`);
    const time8TextBox = text.box(time8Text, { x: rightCenter, y: timesY }, undefined, 20);
    console.log(`🔍 DEBUG: 8PM text.box result:`, time8TextBox);
    if (time8TextBox && time8TextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = time8TextBox.box;
      // Center the box horizontally since 8PM uses center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: 8PM drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // EL CID box - Chaotic electric blue/hot pink colors - mostly invisible with rare bright flashes
    const elCidChaos = Math.sin(frameCount * 0.055 + 4.2) * 0.5 + 0.5; // Much slower
    const elCidFlash = Math.sin(frameCount * 0.29 + 1.9) * 0.5 + 0.5; // Flash cycle
    const elCidBlink = elCidFlash > 0.93 ? 28 : 1; // Rare bright flash or nearly invisible
    const elCidR = Math.floor(100 + Math.sin(frameCount * 0.028 + 6.1) * 155);
    const elCidG = Math.floor(50 + Math.sin(frameCount * 0.082 + 2.7) * 205);
    const elCidB = Math.floor(200 + Math.sin(frameCount * 0.062 + 4.8) * 55);
    ink(elCidR, elCidG, elCidB, elCidBlink); // 1 or 28 opacity - mostly invisible!
    console.log(`🔍 DEBUG: EL CID text="${elCidText}" size=32`);
    const elCidTextBox = text.box(elCidText, { x: centerX, y: elCidY }, undefined, 32);
    console.log(`🔍 DEBUG: EL CID text.box result:`, elCidTextBox);
    if (elCidTextBox && elCidTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = elCidTextBox.box;
      // Center the box horizontally since EL CID uses center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: EL CID drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // Address box - Chaotic neon green/purple colors - mostly invisible with rare bright flashes
    const addressChaos = Math.sin(frameCount * 0.085 + 5.7) * 0.5 + 0.5; // Much slower
    const addressFlash = Math.sin(frameCount * 0.31 + 2.4) * 0.5 + 0.5; // Flash cycle
    const addressBlink = addressFlash > 0.94 ? 27 : 1; // Rare bright flash or nearly invisible
    const addressR = Math.floor(50 + Math.sin(frameCount * 0.038 + 3.9) * 205);
    const addressG = Math.floor(180 + Math.sin(frameCount * 0.058 + 1.2) * 75);
    const addressB = Math.floor(100 + Math.sin(frameCount * 0.078 + 5.6) * 155);
    ink(addressR, addressG, addressB, addressBlink); // 1 or 27 opacity - mostly invisible!
    console.log(`🔍 DEBUG: ADDRESS text="${addressText}" size=12`);
    const addressTextBox = text.box(addressText, { x: centerX, y: addressY }, undefined, 12);
    console.log(`🔍 DEBUG: ADDRESS text.box result:`, addressTextBox);
    if (addressTextBox && addressTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = addressTextBox.box;
      // Center the box horizontally since address uses center: "x"
      const centeredBoxX = boxX - boxW / 2;
      console.log(`🔍 DEBUG: ADDRESS drawing box at (${centeredBoxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(centeredBoxX, boxY, boxW, boxH);
    }

    // Catalyst LA box - Chaotic fire colors (red/orange/yellow) - mostly invisible with rare bright flashes
    const hostedChaos = Math.sin(frameCount * 0.095 + 6.3) * 0.5 + 0.5; // Much slower
    const hostedFlash = Math.sin(frameCount * 0.33 + 4.7) * 0.5 + 0.5; // Flash cycle
    const hostedBlink = hostedFlash > 0.95 ? 20 : 1; // Rare bright flash or nearly invisible
    const hostedR = Math.floor(200 + Math.sin(frameCount * 0.04 + 2.1) * 55);
    const hostedG = Math.floor(80 + Math.sin(frameCount * 0.066 + 4.3) * 175);
    const hostedB = Math.floor(20 + Math.sin(frameCount * 0.052 + 1.7) * 100);
    ink(hostedR, hostedG, hostedB, hostedBlink); // 1 or 20 opacity - mostly invisible!
    console.log(`🔍 DEBUG: HOSTED text="${hostedText}" size=10 at position (${hostedX}, ${hostedY})`);
    const hostedTextBox = text.box(hostedText, { x: hostedX, y: hostedY }, undefined, 10);
    console.log(`🔍 DEBUG: HOSTED text.box result:`, hostedTextBox);
    if (hostedTextBox && hostedTextBox.box) {
      const { x: boxX, y: boxY, width: boxW, height: boxH } = hostedTextBox.box;
      // Note: hostedText doesn't use center: "x", so use the actual x position
      console.log(`🔍 DEBUG: HOSTED drawing box at (${boxX}, ${boxY}) size ${boxW}x${boxH}`);
      box(boxX, boxY, boxW, boxH);
    }

    // if (frameCount % 8 === 0) contrast(1.05); // Reduced blur strength to reduce processing load
  }
}