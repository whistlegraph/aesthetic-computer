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

// Helper function for VHS-style rainbow overdraw text with THICK effect
function writeVHS(text, options, api) {
  const { write, ink } = api;
  const { x, y, center, size } = options;
  
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
  
  // MEMORY-EFFICIENT thick effect - fewer writes but strategic positioning
  const thicknessOffsets = [
    [0, 0], [1, 0], [0, 1], [1, 1], [-1, 0], [0, -1]
  ];
  
  // Much reduced overdraw for memory efficiency
  for (let layer = 0; layer < 2; layer++) { // Only 2 layers
    for (let i = 0; i < 2; i++) { // Only 2 copies per layer
      const colorIndex = (i + layer * 2 + Math.floor(Math.random() * colors.length)) % colors.length;
      const [r, g, b] = colors[colorIndex];
      
      // Simpler opacity
      const alpha = 100 + Math.random() * 100; // 100-200 range
      
      ink(r, g, b, alpha);
      
      // Apply only essential thickness offsets - minimal for memory
      for (const [offsetX, offsetY] of thicknessOffsets.slice(0, 2)) { // Only 2 offsets
        // Minimal scattering for efficiency
        const scatterRange = 2; // Fixed small scatter
        const randomOffsetX = (Math.random() - 0.5) * scatterRange;
        const randomOffsetY = (Math.random() - 0.5) * scatterRange;
        
        const vhsOptions = { ...options };
        if (x !== undefined) vhsOptions.x = x + offsetX + randomOffsetX;
        if (y !== undefined) vhsOptions.y = y + offsetY + randomOffsetY;
        
        write(text, vhsOptions);
      }
    }
  }
  
  // Draw final clean copy on top with slight transparency for blend
  ink(255, 255, 255, 180);
  write(text, options);
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
  for (let y = 0; y < 2048; y += 4) {
    const intensity = 10 + gradientShift + (y / 2048) * 15;
    ink(intensity, intensity + 5, intensity + 20, 8);
    line(0, y, 2048, y);
  }

  // Center point for all text
  const centerX = 1024; // Half of 2048 for proper centering
  
  // MAIN TITLE - BRING YOUR OWN + Scrolling AESTHETIC.COMPUTER
  const titleY = 250;
  
  // "BRING YOUR OWN" at top - white - made bigger and spaced more
  ink(255, 255, 255);
  const bringYourOwnText = "BRING YOUR OWN";
  writeVHS(bringYourOwnText, { center: "x", y: titleY, size: 16 }, api);
  
    // Debug box for "BRING YOUR OWN" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 255, 100, 8); // Less opaque yellow
    const textBox = text.box(bringYourOwnText, { x: centerX, y: titleY }, undefined, 16);
    console.log(`📏 "${bringYourOwnText}" - Size: 16, Box: ${textBox?.box?.width}x${textBox?.box?.height}, Pos: ${textBox?.box?.x},${textBox?.box?.y}`);
    console.log(`📏 Calculated: scale/6 = ${16/6}, blockWidth = 6 * ${16/6} = ${6 * (16/6)}, length = ${bringYourOwnText.length}, expected width = ${bringYourOwnText.length * (6 * (16/6))}`);
    if (textBox && textBox.box) {
      const debugX = centerX - textBox.box.width / 2;
      const debugY = titleY;  // Use the actual Y position where text is drawn
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }
  
  // Scrolling "AESTHETIC.COMPUTER" marquee - bigger text for better spacing
  const marqueeY = titleY + 120; // More space from title
  const marqueeText = "AESTHETIC.COMPUTER ";
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
  
  // Draw the scrolling text
  const chars = marqueeText.split('');
  
  chars.forEach((char, index) => {
    const charX = textStartX + (index * charWidth);
    
    // Only draw characters that are visible within the marquee area
    if (charX >= marqueeStartX - 100 && charX <= marqueeEndX + 100) {
      if (char !== ' ' && char !== '.') {
        // Cycle through rainbow colors
        const colorIndex = index % 9;
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
        
        write(char, { x: charX, y: marqueeY, size: 28 });
      } else if (char === '.') {
        // Dynamic star instead of period
        // Cycling rainbow color for the star
        const starColorIndex = (frameCount + index * 3) % 9;
        switch (starColorIndex) {
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
        
        // Draw the period character first with bigger size
        write('.', { x: charX, y: marqueeY, size: 28 });
        
        // Draw dynamic star at character position - positioned below the period and further right
        // Move it further right to be between AESTHETIC and COMPUTER
        const textBottom = marqueeY + (28 * 6) + 12; // Adjusted for bigger text
        drawDynamicStar(charX + 56, textBottom, 12, frameCount + index * 10, api);
      }
    }
  });
  
    // "COMPUTER" at bottom - white (same as BRING YOUR OWN) - bigger and spaced more
  ink(255, 255, 255);
  const computerText = "COMPUTER";
  const computerY = marqueeY + 180; // More space from marquee
  // console.log(`🎨 About to render "${computerText}" at scale 20, pos ${centerX},${computerY}`);
  writeVHS(computerText, { center: "x", y: computerY, size: 20 }, api);
  // console.log(`🎨 Finished rendering "${computerText}"`);
  
    // Debug box for "COMPUTER" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 255, 100, 8); // Less opaque yellow
    const textBox = text.box(computerText, { x: centerX, y: computerY }, undefined, 20);
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
  
  // "led by @jeffrey" - positioned under COMPUTER, moved down more to prevent box overlap
  ink(150, 150, 150);
  const jeffreyText = "led by @jeffrey";
  const jeffreyY = marqueeY + 320; // Moved down further from 280 to 320 to prevent debug box touching
  writeVHS(jeffreyText, { center: "x", y: jeffreyY, size: 8 }, api);

    // Debug box for "led by @jeffrey" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 255, 100, 8); // Less opaque yellow
    const textBox = text.box(jeffreyText, { x: centerX, y: jeffreyY }, undefined, 6);
    if (textBox && textBox.box) {
      const debugX = centerX - textBox.box.width / 2;
      box(debugX, textBox.box.y, textBox.box.width, textBox.box.height);
    }
  }

  // DATE SECTION - Elegant spacing
  const dateY = 850;
  
  // "on" before date - positioned higher
  ink(180, 180, 180);
  const onText = "on";
  writeVHS(onText, { center: "x", y: dateY, size: 12 }, api);
  
  // Debug box for "on" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(255, 100, 100, 30); // Less opaque red
    const textBox = text.box(onText, { x: centerX, y: dateY }, undefined, 8);
    if (textBox && textBox.box) {
      // Adjust for centered text positioning
      const debugX = centerX - textBox.box.width / 2;
      const debugY = dateY;
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }
  
  // Month name
  ink(255, 200, 100); // Warm gold
  const septemberText = "SEPTEMBER";
  const septemberY = dateY + 80;
  writeVHS(septemberText, { center: "x", y: septemberY, size: 20 }, api);
  
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
  writeVHS(dayText, { center: "x", y: dayY, size: 40 }, api);

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
  const timeY = 1280;
  
  // Position doors section on left half, program on right half  
  const leftCenter = centerX / 2; // Center of left half (512)
  const rightCenter = centerX + (centerX / 2); // Center of right half (1536)
  
  // "Doors" and "Program" labels - smaller, elegant
  ink(150, 150, 150);
  const doorsText = "DOORS";
  const programText = "PROGRAM";
  writeVHS(doorsText, { x: leftCenter, center: "x", y: timeY, size: 8 }, api);
  writeVHS(programText, { x: rightCenter, center: "x", y: timeY, size: 8 }, api);
  
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
  writeVHS(time7Text, { x: leftCenter, center: "x", y: timesY, size: 16 }, api);
  writeVHS(time8Text, { x: rightCenter, center: "x", y: timesY, size: 16 }, api);
  
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
  const venueY = 1480;
  
  // "to" before venue name - moved up to prevent overlap with EL CID
  ink(180, 180, 180);
  const toText = "to";
  writeVHS(toText, { center: "x", y: venueY, size: 12 }, api);
  
  // Debug box for "to" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(100, 255, 255, 30); // Less opaque cyan
    const textBox = text.box(toText, { x: centerX, y: venueY }, undefined, 8);
    if (textBox && textBox.box) {
      // Adjust for centered text positioning
      const debugX = centerX - textBox.box.width / 2;
      const debugY = venueY;
      box(debugX, debugY, textBox.box.width, textBox.box.height);
    }
  }
  
  // "EL CID" - Bold venue name (increased spacing to prevent overlap)
  ink(0, 0, 0, 150); // Shadow
  const elCidText = "EL CID";
  const elCidY = venueY + 104; // Increased from 64 to 104 to prevent overlap
  writeVHS(elCidText, { center: "x", y: elCidY, size: 24 }, api);
  
  ink(255, 100, 100); // Red accent
  const elCidMainY = venueY + 100; // Increased from 60 to 100 to prevent overlap
  writeVHS(elCidText, { center: "x", y: elCidMainY, size: 24 }, api);

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
  writeVHS(addressText, { center: "x", y: addressY, size: 10 }, api);

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

  // "hosted by instagram.com/catalyst_la_" - positioned in bottom left corner, within safe zone
  ink(150, 150, 150);
  const hostedText = "hosted by instagram.com/catalyst_la_";
  const hostedX = 150;
  const hostedY = 1850;
  writeVHS(hostedText, { x: hostedX, y: hostedY, size: 8 }, api);

  // Debug box for "hosted by" using proper text.box API
  if (DEBUG_MODE && text && text.box) {
    ink(100, 200, 100, 30); // Less opaque light green
    const textBox = text.box(hostedText, { x: hostedX, y: hostedY }, undefined, 6);
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
  blur(24); // TEMPORARILY DISABLED FOR TESTING
  scroll(0, 1);
  // spin(1);
}