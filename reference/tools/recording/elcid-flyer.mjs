// Build Instructions:
// cd /workspaces/aesthetic-computer/reference/tools/recording && node tape.mjs elcid-flyer.mjs 240 --mp4 --resolution 2048
// TODO:
// [-] Finish copy editing / typography
// [ ] Add flair

// Track frame count for animation
let frameCount = 0;
let marqueeOffset = 0; // For scrolling text

// Debug mode toggle
const DEBUG_MODE = false;

// Helper function for VHS-style rainbow overdraw text with OPTIMIZED THICK effect and blinky glitch
function writeVHS(text, options, api) {
  const { write, ink } = api;
  const { x, y, center, size } = options;
  
  // Glitchy blink effect - MUCH FASTER blinking!
  const blinkSeed = text.length + (options.x || 0) + (options.y || 0);
  const blinkChance = Math.sin(frameCount * 0.15 + blinkSeed * 0.03) * 0.5 + 0.5; // 3x faster
  
  // Don't make important info like times completely disappear
  const isImportantText = text.includes('PM') || text.includes('DOORS') || text.includes('START') || text.includes('EL CID');
  
  // Sometimes skip rendering entirely for glitch effect (but not for important text) - more frequent blackouts
  if (!isImportantText && blinkChance < 0.2) { // Increased from 0.1 to 0.2 for more blackouts
    return; // Complete blackout glitch only for non-essential text
  }
  
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
  
  // SUBTLE SINE WAVE WIGGLE - seed-based randomness for each text block
  const wiggleSeedX = (textHash * 17) % 1000;
  const wiggleSeedY = (textHash * 23) % 1000;
  const wiggleSpeedX = 0.03 + (wiggleSeedX / 1000) * 0.02; // 0.03-0.05 speed
  const wiggleSpeedY = 0.04 + (wiggleSeedY / 1000) * 0.03; // 0.04-0.07 speed
  const wiggleAmplitudeX = 2 + (wiggleSeedX / 1000) * 3; // 2-5 pixel amplitude
  const wiggleAmplitudeY = 1.5 + (wiggleSeedY / 1000) * 2.5; // 1.5-4 pixel amplitude
  
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
  
  // PERFORMANCE OPTIMIZED thickness spread - focused selection
  const thicknessOffsets = [
    [0, 0], [1, 0], [0, 1], [1, 1], [-1, 0], [0, -1], 
    [2, 0], [0, 2], [-2, 0], [0, -2], [2, 2], [-2, -2],
    [3, 1], [1, 3], [-3, 1], [1, -3], [3, 3], [-3, -3],
    [4, 0], [0, 4], [-4, 0], [0, -4], [4, 4], [-4, -4],
    [6, 2], [2, 6], [-6, 2], [2, -6], [6, 6], [-6, -6],
    [8, 0], [0, 8], [-8, 0], [0, -8], [8, 4], [-8, -4],
    [12, 0], [0, 12], [-12, 0], [0, -12], [16, 0], [0, 16], [-16, 0], [0, -16]
  ];
  
  // OPTIMIZED OVERDRAW - still massive but performance conscious
  const minLayers = 4; // Reduced from 8
  const maxLayers = glitchIntensity > 0.7 ? 6 : minLayers; // Reduced from 12
  
  for (let layer = 0; layer < maxLayers; layer++) {
    const copiesPerLayer = 2 + Math.floor(glitchIntensity * 2); // 2-4 copies (was 4-7)
    
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
      
      // Vary opacity for depth
      const baseAlpha = 50 + Math.random() * 100; // 50-150 range
      const layerAlpha = baseAlpha * (1 - layer * 0.08); // Fade deeper layers
      const glitchAlpha = glitchIntensity > 0.6 ? layerAlpha * (0.6 + Math.random() * 0.8) : layerAlpha;
      
      ink(r, g, b, glitchAlpha);
      
      // STRATEGIC thickness offsets - fewer but well-chosen
      const numOffsets = 4 + Math.floor(glitchIntensity * 6); // 4-10 offsets (was 8-24)
      const selectedOffsets = [];
      
      // Always include core offsets for consistency
      selectedOffsets.push([0, 0], [1, 0], [0, 1], [-1, 0], [0, -1]);
      
      // Add random selections for variety
      for (let j = selectedOffsets.length; j < numOffsets; j++) {
        const randomIndex = Math.floor(Math.random() * thicknessOffsets.length);
        selectedOffsets.push(thicknessOffsets[randomIndex]);
      }
      
      for (const [offsetX, offsetY] of selectedOffsets) {
        // Reduced scattering for performance
        const scatterRange = 1 + glitchIntensity * 3; // 1-4 pixel scatter (was 2-10)
        const randomOffsetX = (Math.random() - 0.5) * scatterRange;
        const randomOffsetY = (Math.random() - 0.5) * scatterRange;
        
        const vhsOptions = { ...options };
        if (x !== undefined) vhsOptions.x = x + offsetX + randomOffsetX + wiggleX;
        if (y !== undefined) vhsOptions.y = y + offsetY + randomOffsetY + wiggleY;
        
        write(text, vhsOptions);
      }
    }
  }
  
  // Draw final clean copy with base neon color - with RAPID blinking and WIGGLE!
  const rapidBlink = Math.sin(frameCount * 0.3 + blinkSeed * 0.1) * 0.3 + 0.7; // Super fast blink
  const cleanAlpha = glitchIntensity > 0.8 ? (120 + Math.random() * 80) * rapidBlink : 200 * rapidBlink;
  ink(baseNeonColor[0] * rapidBlink, baseNeonColor[1] * rapidBlink, baseNeonColor[2] * rapidBlink, cleanAlpha);
  
  // Apply wiggle to the final clean copy too
  const finalOptions = { ...options };
  if (finalOptions.x !== undefined) finalOptions.x += wiggleX;
  if (finalOptions.y !== undefined) finalOptions.y += wiggleY;
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

export function paint({ api }) {
  const { wipe, ink, line, box, circle, write, point, blur, scroll, zoom, spin, text, gizmo } = api;

  frameCount++;

  // Clean gradient background
  if (frameCount === 1) {
    wipe(10, 15, 35); // Deep blue base
  }

  // Subtle animated background gradient
  const gradientShift = Math.sin(frameCount * 0.01) * 10;
  
  // Only draw gradient in the middle area, avoiding top and bottom borders
  for (let y = 80; y < (2048 - 80); y += 4) {
    const intensity = 10 + gradientShift + (y / 2048) * 15;
    ink(intensity, intensity + 5, intensity + 20, 8);
    line(0, y, 2048, y);
  }

  // POLYCHROME COLOR BAR at top - will blur and drift down beautifully!
  const colorBarHeight = 80; // Height of the color strip
  const numColorSegments = 20; // Number of color bands
  const segmentWidth = 2048 / numColorSegments; // Width of each color segment
  
  // Color animation - shift colors over time
  const colorTime = frameCount * 0.02;
  const colorShift = frameCount * 0.1; // How fast colors cycle
  
  for (let segment = 0; segment < numColorSegments; segment++) {
    const x = segment * segmentWidth;
    
    // Create animated color cycling
    const colorIndex = (segment + Math.floor(colorShift)) % 12;
    const colorPhase = (segment * 0.5 + colorTime) * Math.PI * 2;
    
    // Base color palette
    let r, g, b;
    switch (colorIndex) {
      case 0:  [r, g, b] = [255, 0, 100];   break; // Hot Pink
      case 1:  [r, g, b] = [255, 100, 0];   break; // Orange  
      case 2:  [r, g, b] = [255, 255, 0];   break; // Yellow
      case 3:  [r, g, b] = [100, 255, 0];   break; // Lime
      case 4:  [r, g, b] = [0, 255, 100];   break; // Green
      case 5:  [r, g, b] = [0, 255, 255];   break; // Cyan
      case 6:  [r, g, b] = [0, 100, 255];   break; // Blue
      case 7:  [r, g, b] = [100, 0, 255];   break; // Purple
      case 8:  [r, g, b] = [255, 0, 255];   break; // Magenta
      case 9:  [r, g, b] = [255, 150, 200]; break; // Pink
      case 10: [r, g, b] = [200, 255, 150]; break; // Light Green
      case 11: [r, g, b] = [150, 200, 255]; break; // Light Blue
    }
    
    // Add pulsing intensity
    const pulseIntensity = Math.sin(colorPhase) * 0.3 + 0.7; // 0.4-1.0 range
    const finalR = r * pulseIntensity;
    const finalG = g * pulseIntensity;
    const finalB = b * pulseIntensity;
    
    // Add some opacity variation for blending
    const alpha = 200 + Math.sin(colorPhase * 1.5) * 50; // 150-250 alpha
    
    ink(finalR, finalG, finalB, alpha);
    
    // Draw the color segment as a filled rectangle
    box(x, 0, segmentWidth + 1, colorBarHeight); // +1 to prevent gaps
    
    // Add some vertical gradient within each segment for extra richness
    for (let y = 0; y < colorBarHeight; y += 4) {
      const gradientFactor = 1 - (y / colorBarHeight) * 0.3; // Slightly brighter at top
      ink(finalR * gradientFactor, finalG * gradientFactor, finalB * gradientFactor, alpha * 0.6);
      line(x, y, x + segmentWidth, y);
    }
  }
  
  // Optional: Add some sparkle effects on the color bar
  for (let sparkle = 0; sparkle < 15; sparkle++) {
    const sparkleX = (Math.sin(frameCount * 0.05 + sparkle * 0.8) * 0.5 + 0.5) * 2048;
    const sparkleY = Math.random() * colorBarHeight;
    const sparkleSize = 2 + Math.random() * 4;
    
    // White sparkles
    ink(255, 255, 255, 150 + Math.random() * 100);
    circle(sparkleX, sparkleY, sparkleSize);
  }

  // LEFT SIDE Rainbow border with different palette (Cool colors - blues/greens/purples)
  const leftBorderWidth = 80; // Match the top border height
  const leftSegments = 20;
  const leftSegmentHeight = 2048 / leftSegments;
  
  const leftColors = [
    [0, 100, 255],    // Deep Blue
    [0, 200, 255],    // Bright Blue
    [0, 255, 200],    // Cyan
    [0, 255, 100],    // Aqua
    [50, 255, 150],   // Sea Green
    [100, 255, 100],  // Bright Green
    [150, 255, 50],   // Lime Green
    [100, 200, 255],  // Light Blue
    [150, 100, 255],  // Purple
    [200, 50, 255],   // Violet
    [100, 150, 255],  // Periwinkle
    [50, 255, 255]    // Electric Cyan
  ];
  
  for (let i = 0; i < leftSegments; i++) {
    const y = i * leftSegmentHeight;
    const colorIndex = (i + Math.floor(frameCount * 0.1)) % leftColors.length;
    const [r, g, b] = leftColors[colorIndex];
    
    // Pulsing intensity
    const pulse = Math.sin(frameCount * 0.08 + i * 0.3) * 0.3 + 0.7;
    const finalR = r * pulse;
    const finalG = g * pulse;
    const finalB = b * pulse;
    const alpha = 180 + Math.sin(frameCount * 0.06 + i * 0.2) * 50;
    
    ink(finalR, finalG, finalB, alpha);
    box(0, y, leftBorderWidth, leftSegmentHeight + 1);
    
    // Horizontal gradient within each segment
    for (let x = 0; x < leftBorderWidth; x += 3) {
      const gradientFactor = 1 - (x / leftBorderWidth) * 0.4;
      ink(finalR * gradientFactor, finalG * gradientFactor, finalB * gradientFactor, alpha * 0.7);
      line(x, y, x, y + leftSegmentHeight);
    }
  }

  // RIGHT SIDE Rainbow border with different palette (Warm colors - reds/oranges/yellows)
  const rightBorderWidth = 80; // Match the top border height
  const rightX = 2048 - rightBorderWidth;
  const rightSegments = 20;
  const rightSegmentHeight = 2048 / rightSegments;
  
  const rightColors = [
    [255, 50, 50],    // Red
    [255, 100, 0],    // Red-Orange
    [255, 150, 0],    // Orange
    [255, 200, 0],    // Orange-Yellow
    [255, 255, 0],    // Yellow
    [255, 255, 100],  // Light Yellow
    [255, 200, 150],  // Peach
    [255, 150, 150],  // Pink
    [255, 100, 200],  // Hot Pink
    [255, 50, 255],   // Magenta
    [200, 100, 255],  // Purple-Pink
    [255, 0, 128]     // Deep Pink
  ];
  
  for (let i = 0; i < rightSegments; i++) {
    const y = i * rightSegmentHeight;
    const colorIndex = (i + Math.floor(frameCount * 0.12)) % rightColors.length;
    const [r, g, b] = rightColors[colorIndex];
    
    // Different pulsing pattern
    const pulse = Math.sin(frameCount * 0.09 + i * 0.4) * 0.4 + 0.6;
    const finalR = r * pulse;
    const finalG = g * pulse;
    const finalB = b * pulse;
    const alpha = 160 + Math.sin(frameCount * 0.07 + i * 0.25) * 60;
    
    ink(finalR, finalG, finalB, alpha);
    box(rightX, y, rightBorderWidth, rightSegmentHeight + 1);
    
    // Horizontal gradient from right edge inward
    for (let x = 0; x < rightBorderWidth; x += 3) {
      const gradientFactor = 1 - (x / rightBorderWidth) * 0.4;
      ink(finalR * gradientFactor, finalG * gradientFactor, finalB * gradientFactor, alpha * 0.7);
      line(rightX + rightBorderWidth - x, y, rightX + rightBorderWidth - x, y + rightSegmentHeight);
    }
  }

  // BOTTOM Rainbow border with different palette (Earth tones - browns/golds/greens)
  const bottomBorderHeight = 80; // Match the top border height
  const bottomY = 2048 - bottomBorderHeight;
  const bottomSegments = 20;
  const bottomSegmentWidth = 2048 / bottomSegments;
  
  const bottomColors = [
    [139, 69, 19],    // Saddle Brown
    [160, 82, 45],    // Brown
    [205, 133, 63],   // Peru
    [222, 184, 135],  // Burlywood
    [238, 203, 173],  // Peach Puff
    [255, 215, 0],    // Gold
    [255, 255, 0],    // Yellow
    [154, 205, 50],   // Yellow Green
    [124, 252, 0],    // Lawn Green
    [50, 205, 50],    // Lime Green
    [34, 139, 34],    // Forest Green
    [85, 107, 47]     // Dark Olive Green
  ];
  
  for (let i = 0; i < bottomSegments; i++) {
    const x = i * bottomSegmentWidth;
    const colorIndex = (i + Math.floor(frameCount * 0.11)) % bottomColors.length;
    const [r, g, b] = bottomColors[colorIndex];
    
    // Yet another pulsing pattern
    const pulse = Math.sin(frameCount * 0.07 + i * 0.5) * 0.35 + 0.65;
    const finalR = r * pulse;
    const finalG = g * pulse;
    const finalB = b * pulse;
    const alpha = 170 + Math.sin(frameCount * 0.08 + i * 0.3) * 55;
    
    ink(finalR, finalG, finalB, alpha);
    box(x, bottomY, bottomSegmentWidth + 1, bottomBorderHeight);
    
    // Vertical gradient within each segment
    for (let y = 0; y < bottomBorderHeight; y += 3) {
      const gradientFactor = 1 - (y / bottomBorderHeight) * 0.4;
      ink(finalR * gradientFactor, finalG * gradientFactor, finalB * gradientFactor, alpha * 0.7);
      line(x, bottomY + bottomBorderHeight - y, x + bottomSegmentWidth, bottomY + bottomBorderHeight - y);
    }
  }

  // Add sparkles to all borders
  for (let sparkle = 0; sparkle < 25; sparkle++) {
    const sparkleSize = 1 + Math.random() * 3;
    const sparkleAlpha = 100 + Math.random() * 155;
    
    // Left border sparkles
    if (sparkle < 8) {
      const sparkleX = Math.random() * leftBorderWidth;
      const sparkleY = Math.random() * 2048;
      ink(200, 255, 255, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }
    
    // Right border sparkles
    if (sparkle >= 8 && sparkle < 16) {
      const sparkleX = rightX + Math.random() * rightBorderWidth;
      const sparkleY = Math.random() * 2048;
      ink(255, 255, 200, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }
    
    // Bottom border sparkles
    if (sparkle >= 16) {
      const sparkleX = Math.random() * 2048;
      const sparkleY = bottomY + Math.random() * bottomBorderHeight;
      ink(255, 215, 100, sparkleAlpha);
      circle(sparkleX, sparkleY, sparkleSize);
    }
  }

  // 3D Starfield background - flying through space!
  const numStars = 150;
  const speed = 5; // Movement speed through space
  
  // Initialize stars on first frame
  if (frameCount === 1) {
    api.stars = [];
    for (let i = 0; i < numStars; i++) {
      api.stars.push({
        x: (Math.random() - 0.5) * 4000, // Random X position in 3D space
        y: (Math.random() - 0.5) * 4000, // Random Y position in 3D space  
        z: Math.random() * 2000 + 100,   // Random Z depth
        type: i % 8                      // Star visual type
      });
    }
  }
  
  // Update and render each star
  for (let i = 0; i < numStars; i++) {
    const star = api.stars[i];
    
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

  // Bouncing Colorful Star Instances around the screen
  const numBouncingStars = 25;
  
  // Initialize bouncing stars on first frame
  if (frameCount === 1) {
    api.bouncingStars = [];
    for (let i = 0; i < numBouncingStars; i++) {
      api.bouncingStars.push({
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
    const star = api.bouncingStars[i];
    
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

  // Center point for all text
  const centerX = 1024; // Half of 2048 for proper centering
  
  // MAIN TITLE - Scrolling AESTHETIC.COMPUTER
  const titleY = 180; // Moved up from 250
  
  // Scrolling "AESTHETIC COMPUTER" marquee - bigger text for better spacing
  const marqueeY = titleY + 120; // More space from title
  const marqueeText = "AESTHETIC COMPUTER AESTHETIC COMPUTER "; // Doubled up text
  const marqueeSpeed = 4; // Faster speed for bigger text
  
  // Update marquee offset
  marqueeOffset += marqueeSpeed;
  
  // Calculate text width for smooth looping with bigger size
  const charWidth = 8 * 28; // Character width for larger size 28
  const textWidth = marqueeText.length * charWidth;
  
  // Create a visible marquee area
  const marqueeWidth = 1600; // Width of visible area
  const marqueeStartX = (centerX - marqueeWidth / 2);
  const marqueeEndX = marqueeStartX + marqueeWidth;
  
  // Start text fully onscreen at left edge, scroll to completely off the right side
  const totalTravelDistance = marqueeWidth + textWidth;
  if (marqueeOffset > totalTravelDistance) {
    marqueeOffset = 0;
  }
  
  // Position text to start from left edge of visible area
  const textStartX = marqueeStartX - marqueeOffset;
  
  // Draw the scrolling text with DOUBLE/BLINK effects
  const chars = marqueeText.split('');
  
  // Blinking intensity for the whole marquee - MUCH FASTER AND BRIGHTER!
  const marqueeBlink = Math.sin(frameCount * 0.5) * 0.3 + 0.7; // 0.4-1.0 intensity, much faster
  const doubleOffset = Math.sin(frameCount * 0.6) * 8; // ±8 pixel offset, faster and bigger
  
  chars.forEach((char, index) => {
    const charX = textStartX + (index * charWidth);
    
    // Only draw characters that are visible within the marquee area
    if (charX >= marqueeStartX - 100 && charX <= marqueeEndX + 100) {
      if (char !== ' ' && char !== '.') {
        // Cycle through rainbow colors - BRIGHTER!
        const colorIndex = index % 9;
        let r, g, b;
        switch (colorIndex) {
          case 0: [r, g, b] = [255, 150, 150]; break; // Brighter Red
          case 1: [r, g, b] = [255, 200, 100]; break; // Brighter Orange
          case 2: [r, g, b] = [255, 255, 150]; break; // Brighter Yellow
          case 3: [r, g, b] = [150, 255, 150]; break; // Brighter Green
          case 4: [r, g, b] = [150, 200, 255]; break; // Brighter Blue
          case 5: [r, g, b] = [200, 150, 255]; break; // Brighter Indigo
          case 6: [r, g, b] = [255, 150, 255]; break; // Brighter Violet
          case 7: [r, g, b] = [150, 255, 255]; break; // Brighter Cyan
          case 8: [r, g, b] = [255, 200, 230]; break; // Brighter Pink
        }
        
        // Apply blinking intensity - BRIGHTER BASE!
        const blinkR = r * (marqueeBlink * 1.2); // 20% brighter
        const blinkG = g * (marqueeBlink * 1.2);
        const blinkB = b * (marqueeBlink * 1.2);
        
        // MARQUEE CHARACTER WIGGLE - each character gets its own wiggle pattern
        const charSeed = char.charCodeAt(0) + index * 17;
        const charWiggleX = Math.sin(frameCount * (0.04 + (charSeed % 100) * 0.0001) + charSeed * 0.02) * (1 + (charSeed % 50) * 0.04); // 1-3 pixels
        const charWiggleY = Math.sin(frameCount * (0.05 + (charSeed % 80) * 0.0001) + charSeed * 0.03) * (0.5 + (charSeed % 40) * 0.03); // 0.5-2 pixels
        
        // Draw DOUBLED characters for thick effect with WIGGLE - BRIGHTER!
        // Main character - BRIGHTEST
        ink(Math.min(255, blinkR), Math.min(255, blinkG), Math.min(255, blinkB));
        // write(char, { x: charX + charWiggleX, y: marqueeY + charWiggleY, size: 28 });
        
        // Doubled/offset character for thickness - BRIGHT
        ink(Math.min(255, blinkR * 0.9), Math.min(255, blinkG * 0.9), Math.min(255, blinkB * 0.9), 200);
        // write(char, { x: charX + doubleOffset + charWiggleX, y: marqueeY + doubleOffset * 0.5 + charWiggleY, size: 28 });
        
        // Third layer for extra pop - ALWAYS VISIBLE NOW
        ink(Math.min(255, blinkR * 0.6), Math.min(255, blinkG * 0.6), Math.min(255, blinkB * 0.6), 160);
        // write(char, { x: charX - doubleOffset * 0.5 + charWiggleX, y: marqueeY - doubleOffset * 0.3 + charWiggleY, size: 28 });
        
        // Fourth layer for extra thickness during bright moments
        if (marqueeBlink > 0.6) {
          ink(Math.min(255, blinkR * 0.4), Math.min(255, blinkG * 0.4), Math.min(255, blinkB * 0.4), 120);
          // write(char, { x: charX + doubleOffset * 0.3 + charWiggleX, y: marqueeY + doubleOffset * 0.8 + charWiggleY, size: 28 });
        }
        
      } else if (char === '.') {
        // Dynamic star instead of period
        // Cycling rainbow color for the star
        const starColorIndex = (frameCount + index * 3) % 9;
        let starR, starG, starB;
        switch (starColorIndex) {
          case 0: [starR, starG, starB] = [255, 100, 100]; break; // Red
          case 1: [starR, starG, starB] = [255, 150, 50]; break;  // Orange
          case 2: [starR, starG, starB] = [255, 255, 100]; break; // Yellow
          case 3: [starR, starG, starB] = [100, 255, 100]; break; // Green
          case 4: [starR, starG, starB] = [100, 150, 255]; break; // Blue
          case 5: [starR, starG, starB] = [150, 100, 255]; break; // Indigo
          case 6: [starR, starG, starB] = [255, 100, 255]; break; // Violet
          case 7: [starR, starG, starB] = [100, 255, 255]; break; // Cyan
          case 8: [starR, starG, starB] = [255, 150, 200]; break; // Pink
        }
        
        // Apply blinking to star too
        ink(starR * marqueeBlink, starG * marqueeBlink, starB * marqueeBlink);
        
        // Period wiggle (same as character wiggle)
        const periodSeed = '.'.charCodeAt(0) + index * 17;
        const periodWiggleX = Math.sin(frameCount * (0.04 + (periodSeed % 100) * 0.0001) + periodSeed * 0.02) * (1 + (periodSeed % 50) * 0.04);
        const periodWiggleY = Math.sin(frameCount * (0.05 + (periodSeed % 80) * 0.0001) + periodSeed * 0.03) * (0.5 + (periodSeed % 40) * 0.03);
        
        // Draw the period character first with bigger size and wiggle
        // write('.', { x: charX + periodWiggleX, y: marqueeY + periodWiggleY, size: 28 });
        
        // Draw dynamic star at character position - positioned below the period and further right
        // Move it further right to be between AESTHETIC and COMPUTER
        const textBottom = marqueeY + (28 * 6) + 12; // Adjusted for bigger text
        drawDynamicStar(charX + 56 + periodWiggleX, textBottom + periodWiggleY, 12, frameCount + index * 10, api);
      }
    }
  });
  
    // "COMPUTER" at bottom - white (same as BRING YOUR OWN) - bigger and spaced more
  ink(255, 255, 255);
  const computerText = "COMPUTER";
  const computerY = marqueeY + 180; // More space from marquee
  // console.log(`🎨 About to render "${computerText}" at scale 32, pos ${centerX},${computerY}`);
  // writeVHS(computerText, { center: "x", y: computerY, size: 32 }, api);
  // console.log(`🎨 Finished rendering "${computerText}"`);
  
    // Debug box for "COMPUTER" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 255, 100, 8); // Less opaque yellow
    const textBox = text.box(computerText, { x: centerX, y: computerY }, undefined, 32);
    console.log(`📏 "${computerText}" - Size: 20, Box: ${textBox?.box?.width}x${textBox?.box?.height}, Pos: ${textBox?.box?.x},${textBox?.box?.y}`);
    console.log(`📏 COMPUTER calc: scale/6 = ${20/6}, blockWidth = 6 * ${20/6} = ${6 * (20/6)}, length = ${computerText.length}, expected = ${computerText.length * (6 * (20/6))}`);
    console.log(`🎨 Canvas info available`);
    if (textBox && textBox.box) {
      // Use the text.box dimensions directly (no scaling needed)
      console.log(`📏 COMPUTER dimensions: text.box=${textBox.box.width}x${textBox.box.height}`);
      
      const debugX = centerX - textBox.box.width / 2;  // Center the debug box
      console.log(`📦 Drawing debug box at ${debugX},${textBox.box.y} with size ${textBox.box.width}x${textBox.box.height} (direct text.box size)`);
      box(debugX, textBox.box.y, textBox.box.width, textBox.box.height);
    }
  }
  
  // DATE SECTION - Elegant spacing
  const dateY = 700; // Moved up from 850
  
  // Month name
  ink(255, 200, 100); // Warm gold
  const septemberText = "SEPTEMBER";
  const septemberY = dateY + 80;
  // writeVHS(septemberText, { center: "x", y: septemberY, size: 20 }, api);
  
  // Debug box for "SEPTEMBER" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(100, 255, 100, 30); // Less opaque green
    const textBox = text.box(septemberText, { x: centerX, y: septemberY }, undefined, 16);
    if (textBox && textBox.box) {
      // Adjust for centered text positioning
      const debugX = centerX - textBox.box.width / 2;
      const debugY = septemberY;
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }
  
  // Day number - make it prominent
  ink(255, 255, 255);
  const dayText = "30";
  const dayY = dateY + 220;
  // writeVHS(dayText, { center: "x", y: dayY, size: 40 }, api);

  // Debug box for "30" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(100, 100, 255, 30); // Less opaque blue
    const textBox = text.box(dayText, { x: centerX, y: dayY }, undefined, 32);
    if (textBox && textBox.box) {
      // Adjust for centered text positioning
      const debugX = centerX - textBox.box.width / 2;
      const debugY = dayY;
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }

  // TIME SECTION - Better typography hierarchy
  const timeY = 1100; // Moved up from 1280
  
  // Position doors section on left half, program on right half  
  const leftCenter = centerX / 2; // Center of left half (512)
  const rightCenter = centerX + (centerX / 2); // Center of right half (1536)
  
  // "Doors" and "Program" labels - smaller, elegant
  ink(150, 150, 150);
  const doorsText = "DOORS";
  const programText = "START";
  // writeVHS(doorsText, { x: leftCenter, center: "x", y: timeY, size: 8 }, api);
  // writeVHS(programText, { x: rightCenter, center: "x", y: timeY, size: 8 }, api);
  
  // Debug boxes for doors/program labels using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 255, 100, 8); // Less opaque yellow
    const doorsBox = text.box(doorsText, { x: leftCenter, y: timeY }, undefined, 6);
    const programBox = text.box(programText, { x: rightCenter, y: timeY }, undefined, 6);
    if (doorsBox && doorsBox.box) {
      const debugX = leftCenter - doorsBox.box.width / 2;
      box(debugX, doorsBox.box.y, doorsBox.box.width, doorsBox.box.height);
    }
    if (programBox && programBox.box) {
      const debugX = rightCenter - programBox.box.width / 2;
      box(debugX, programBox.box.y, programBox.box.width, programBox.box.height);
    }
  }
  
  // Times - larger, prominent
  ink(255, 255, 255);
  const time7Text = "7PM";
  const time8Text = "8PM";
  const timesY = timeY + 60;
  // writeVHS(time7Text, { x: leftCenter, center: "x", y: timesY, size: 16 }, api);
  // writeVHS(time8Text, { x: rightCenter, center: "x", y: timesY, size: 16 }, api);
  
  // Debug boxes for times using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 200, 100, 30); // Less opaque orange
    const time7Box = text.box(time7Text, { x: leftCenter, y: timesY }, undefined, 12);
    const time8Box = text.box(time8Text, { x: rightCenter, y: timesY }, undefined, 12);
    if (time7Box && time7Box.box) {
      const debugX = leftCenter - time7Box.box.width / 2;
      box(debugX, time7Box.box.y, time7Box.box.width, time7Box.box.height);
    }
    if (time8Box && time8Box.box) {
      const debugX = rightCenter - time8Box.box.width / 2;
      box(debugX, time8Box.box.y, time8Box.box.width, time8Box.box.height);
    }
  }
  
  // VENUE SECTION
  const venueY = 1280; // Moved up from 1480
  
  // "EL CID" - Bold venue name (increased spacing to prevent overlap)
  ink(0, 0, 0, 150); // Shadow
  const elCidText = "EL CID";
  const elCidY = venueY + 104; // Increased from 64 to 104 to prevent overlap
  // writeVHS(elCidText, { center: "x", y: elCidY, size: 24 }, api);
  
  ink(255, 100, 100); // Red accent
  const elCidMainY = venueY + 100; // Increased from 60 to 100 to prevent overlap
  // writeVHS(elCidText, { center: "x", y: elCidMainY, size: 24 }, api);

  // Debug box for "EL CID" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 0, 0, 30); // Less opaque red
    const textBox = text.box(elCidText, { x: centerX, y: elCidMainY }, undefined, 20);
    if (textBox && textBox.box) {
      // Adjust for centered text positioning
      const debugX = centerX - textBox.box.width / 2;
      const debugY = elCidMainY;
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }  // Address - clean and readable (more spacing to prevent box overlap)
  ink(200, 200, 200);
  const addressText = "4212 W Sunset Blvd";
  const addressY = venueY + 280; // Increased from 220 to 280 to prevent debug box touching
  // writeVHS(addressText, { center: "x", y: addressY, size: 10 }, api);

  // Debug box for address using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(200, 100, 255, 30); // Less opaque purple
    const textBox = text.box(addressText, { x: centerX, y: addressY }, undefined, 8);
    if (textBox && textBox.box) {
      // Adjust for centered text positioning
      const debugX = centerX - textBox.box.width / 2;
      const debugY = addressY;
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }

  // "Hosted by Catalyst LA" - positioned in bottom left corner, within safe zone
  ink(150, 150, 150);
  const hostedText = "Hosted by Catalyst LA";
  const hostedX = 150;
  const hostedY = 1850;
  // writeVHS(hostedText, { x: hostedX, y: hostedY, size: 6 }, api);

  // Debug box for "hosted by" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(100, 200, 100, 30); // Less opaque light green
    const textBox = text.box(hostedText, { x: hostedX, y: hostedY }, undefined, 4);
    if (textBox && textBox.box) {
      box(textBox.box.x, textBox.box.y, textBox.box.width, textBox.box.height);
    }
  }

  // Minimalist decorative elements
  const time = frameCount * 0.02;
  
  // Subtle corner accents
  const accentAlpha = 100 + Math.sin(time) * 50;
  ink(100, 200, 255, accentAlpha);
  
  // Top corners
  line(100, 100, 200, 100);
  line(100, 100, 100, 200);
  line(1948, 100, 1848, 100);
  line(1948, 100, 1948, 200);
  
  // Bottom corners
  line(100, 1948, 200, 1948);
  line(100, 1948, 100, 1848);
  line(1948, 1948, 1848, 1948);
  line(1948, 1948, 1948, 1848);

  // Apply subtle blur for polish
  // if (frameCount % 32 === 0) blur(24); // Reduced blur strength to reduce processing load

  if (frameCount % 16 === 0) zoom(0.9);

  // Dynamic scrolling - sometimes X, sometimes Y, sometimes both
 // const scrollTime = frameCount * 0.01;
 // const scrollPhase = Math.floor(scrollTime / 0.5) % 4; // Change direction every 0.5 seconds
 // 
 // switch (scrollPhase) {
 //   case 0: 
 //     scroll(0, 2); // Vertical down
 //     break;
 //   case 1:
 //     scroll(2, 0); // Horizontal right
 //     break;
 //   case 2:
 //     scroll(0, -2); // Vertical up
 //     break;
 //   case 3:
 //     scroll(-2, 2); // Diagonal
 //     break;
 // }

  // if (Math.random() > 0.5) {
  //   spin(-1);
  // } else if (Math.random() > 0.5) {
  //   spin(1);
  // }
  // spin(1);
  
  // Manual cleanup to ensure blur buffers are released each frame
  // if (api.cleanupBlurBuffers) {
  //   api.cleanupBlurBuffers();
  // }
}