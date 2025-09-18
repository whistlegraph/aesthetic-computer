// Track frame count for animation
let frameCount = 0;
let marqueeOffset = 0; // For scrolling text

// Debug mode toggle
const DEBUG_MODE = true;

// Helper function to estimate text dimensions more accurately
function estimateTextDimensions(text, size) {
  // Based on the disk.mjs investigation: blockWidth * scale is more accurate
  // Using more realistic character width ratios for better estimation
  const avgCharWidth = size * 0.6; // Slightly wider than original 0.6
  const width = text.length * avgCharWidth;
  const height = size * 1.4; // Slightly taller for better coverage
  return { width, height };
}

// Helper function to draw a dynamic star
function drawDynamicStar(x, y, size, frame, api) {
  const { line } = api;
  const time = frame * 0.1; // Animation speed
  const numRays = 8;
  const innerRadius = size * 0.3;
  const outerRadius = size * 0.8;
  
  // Pulsing effect
  const pulse = 1 + Math.sin(time * 3) * 0.2;
  const currentInner = innerRadius * pulse;
  const currentOuter = outerRadius * pulse;
  
  // Rotation
  const rotation = time * 2;
  
  // Draw star rays as lines
  for (let i = 0; i < numRays; i++) {
    const angle = (i / numRays) * Math.PI * 2 + rotation;
    
    // Outer ray
    const outerX = x + Math.cos(angle) * currentOuter;
    const outerY = y + Math.sin(angle) * currentOuter;
    
    // Inner point between rays
    const innerAngle = angle + (Math.PI / numRays);
    const innerX = x + Math.cos(innerAngle) * currentInner;
    const innerY = y + Math.sin(innerAngle) * currentInner;
    
    // Draw ray from center to outer point
    line(x, y, outerX, outerY);
    
    // Draw connecting lines to create star shape
    if (i < numRays - 1) {
      const nextAngle = ((i + 1) / numRays) * Math.PI * 2 + rotation;
      const nextInnerAngle = nextAngle + (Math.PI / numRays);
      const nextInnerX = x + Math.cos(nextInnerAngle) * currentInner;
      const nextInnerY = y + Math.sin(nextInnerAngle) * currentInner;
      
      line(outerX, outerY, nextInnerX, nextInnerY);
    } else {
      // Connect last ray to first
      const firstAngle = rotation;
      const firstInnerAngle = firstAngle + (Math.PI / numRays);
      const firstInnerX = x + Math.cos(firstInnerAngle) * currentInner;
      const firstInnerY = y + Math.sin(firstInnerAngle) * currentInner;
      
      line(outerX, outerY, firstInnerX, firstInnerY);
    }
  }
}

export function paint({ api }) {
  const { wipe, ink, line, box, circle, write, point, blur, scroll, zoom, text, gizmo } = api;

  frameCount++;

  // Clean gradient background
  if (frameCount === 1) {
    wipe(10, 15, 35); // Deep blue base
  }

  // Subtle animated background gradient
  const gradientShift = Math.sin(frameCount * 0.01) * 10;
  for (let y = 0; y < 2048; y += 4) {
    const intensity = 10 + gradientShift + (y / 2048) * 15;
    ink(intensity, intensity + 5, intensity + 20, 30);
    line(0, y, 2048, y);
  }

  // Center point for all text
  const centerX = 1024; // Half of 2048 for proper centering
  
  // MAIN TITLE - BRING YOUR OWN + Scrolling AESTHETIC.COMPUTER
  const titleY = 250;
  
  // "BRING YOUR OWN" at top - white
  ink(255, 255, 255);
  const bringYourOwnText = "BRING YOUR OWN";
  write(bringYourOwnText, { center: "x", y: titleY, size: 12 });
  
  // Debug box for "BRING YOUR OWN" 
  if (DEBUG_MODE) {
    ink(255, 255, 0, 100); // Semi-transparent yellow
    const dims = estimateTextDimensions(bringYourOwnText, 12);
    box(centerX - dims.width/2, titleY - dims.height/2, dims.width, dims.height);
  }
  
  // Scrolling "AESTHETIC.COMPUTER" marquee
  const marqueeY = titleY + 100;
  const marqueeText = "AESTHETIC.COMPUTER ";
  const marqueeSpeed = 3; // pixels per frame
  
  // Update marquee offset
  marqueeOffset += marqueeSpeed;
  
  // Use proper character width - each character is approximately 6px at size 1, 
  // so at size 24 it should be 6 * 24 = 144px per character, but let's use a more accurate estimate
  const charWidth = 7 * 24; // More realistic character width for size 24
  const textWidth = marqueeText.length * charWidth;
  if (marqueeOffset > textWidth) {
    marqueeOffset = 0;
  }
  
  // Create a visible marquee area
  const marqueeWidth = 1600; // Width of visible area
  const marqueeStartX = (centerX - marqueeWidth / 2);
  
  // Draw multiple instances of the text to create seamless loop
  for (let i = -1; i <= 3; i++) {
    const x = marqueeStartX - marqueeOffset + (i * textWidth);
    
    // Rainbow colors for each character
    const chars = marqueeText.split('');
    
    chars.forEach((char, index) => {
      const charX = x + (index * charWidth); // Use consistent character width
      
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
        
        // Only draw if character is within visible area
        if (charX >= marqueeStartX - 100 && charX <= marqueeStartX + marqueeWidth + 100) {
          write(char, { x: charX, y: marqueeY, size: 24 });
        }
      } else if (char === '.') {
        // Dynamic star instead of period
        if (charX >= marqueeStartX - 100 && charX <= marqueeStartX + marqueeWidth + 100) {
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
          
          // Draw dynamic star at character position
          drawDynamicStar(charX + 12, marqueeY, 12, frameCount + index * 10, api);
        }
      }
    });
  }
  
  // "COMPUTER" at bottom - white (same as BRING YOUR OWN) - lower and larger
  ink(255, 255, 255);
  const computerText = "COMPUTER";
  const computerY = marqueeY + 160;
  write(computerText, { center: "x", y: computerY, size: 16 });
  
  // Debug box for "COMPUTER"
  if (DEBUG_MODE) {
    ink(0, 255, 255, 100); // Semi-transparent cyan
    const estimatedWidth = computerText.length * 16 * 0.6;
    const estimatedHeight = 16 * 1.2;
    box(centerX - estimatedWidth/2, computerY - estimatedHeight/2, estimatedWidth, estimatedHeight);
  }
  
  // "led by @jeffrey" - positioned under COMPUTER, moved down more
  ink(150, 150, 150);
  const jeffreyText = "led by @jeffrey";
  const jeffreyY = marqueeY + 280; // Moved down further from 240 to 280
  write(jeffreyText, { center: "x", y: jeffreyY, size: 6 });

  // Debug box for "led by @jeffrey"
  if (DEBUG_MODE) {
    ink(255, 0, 255, 100); // Semi-transparent magenta
    const estimatedWidth = jeffreyText.length * 6 * 0.6;
    const estimatedHeight = 6 * 1.2;
    box(centerX - estimatedWidth/2, jeffreyY - estimatedHeight/2, estimatedWidth, estimatedHeight);
  }

  // DATE SECTION - Elegant spacing
  const dateY = 850;
  
  // "on" before date - positioned higher
  ink(180, 180, 180);
  const onText = "on";
  write(onText, { center: "x", y: dateY, size: 8 });
  
  // Debug box for "on"
  if (DEBUG_MODE) {
    ink(255, 100, 100, 100); // Semi-transparent red
    const estimatedWidth = onText.length * 8 * 0.6;
    const estimatedHeight = 8 * 1.2;
    box(centerX - estimatedWidth/2, dateY - estimatedHeight/2, estimatedWidth, estimatedHeight);
  }
  
  // Month name
  ink(255, 200, 100); // Warm gold
  const septemberText = "SEPTEMBER";
  const septemberY = dateY + 80;
  write(septemberText, { center: "x", y: septemberY, size: 16 });
  
  // Debug box for "SEPTEMBER"
  if (DEBUG_MODE) {
    ink(100, 255, 100, 100); // Semi-transparent green
    const estimatedWidth = septemberText.length * 16 * 0.6;
    const estimatedHeight = 16 * 1.2;
    box(centerX - estimatedWidth/2, septemberY - estimatedHeight/2, estimatedWidth, estimatedHeight);
  }
  
  // Day number - make it prominent
  ink(255, 255, 255);
  const dayText = "30";
  const dayY = dateY + 220;
  write(dayText, { center: "x", y: dayY, size: 32 });

  // Debug box for "30"
  if (DEBUG_MODE) {
    ink(100, 100, 255, 100); // Semi-transparent blue
    const estimatedWidth = dayText.length * 32 * 0.6;
    const estimatedHeight = 32 * 1.2;
    box(centerX - estimatedWidth/2, dayY - estimatedHeight/2, estimatedWidth, estimatedHeight);
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
  write(doorsText, { x: leftCenter, center: "x", y: timeY, size: 6 });
  write(programText, { x: rightCenter, center: "x", y: timeY, size: 6 });
  
  // Debug boxes for doors/program labels
  if (DEBUG_MODE) {
    ink(255, 255, 100, 100); // Semi-transparent yellow
    const doorsWidth = doorsText.length * 6 * 0.6;
    const doorsHeight = 6 * 1.2;
    const programWidth = programText.length * 6 * 0.6;
    const programHeight = 6 * 1.2;
    box(leftCenter - doorsWidth/2, timeY - doorsHeight/2, doorsWidth, doorsHeight);
    box(rightCenter - programWidth/2, timeY - programHeight/2, programWidth, programHeight);
  }
  
  // Times - larger, prominent
  ink(255, 255, 255);
  const time7Text = "7PM";
  const time8Text = "8PM";
  const timesY = timeY + 60;
  write(time7Text, { x: leftCenter, center: "x", y: timesY, size: 12 });
  write(time8Text, { x: rightCenter, center: "x", y: timesY, size: 12 });
  
  // Debug boxes for times
  if (DEBUG_MODE) {
    ink(255, 200, 100, 100); // Semi-transparent orange
    const time7Width = time7Text.length * 12 * 0.6;
    const time7Height = 12 * 1.2;
    const time8Width = time8Text.length * 12 * 0.6;
    const time8Height = 12 * 1.2;
    box(leftCenter - time7Width/2, timesY - time7Height/2, time7Width, time7Height);
    box(rightCenter - time8Width/2, timesY - time8Height/2, time8Width, time8Height);
  }
  
  // VENUE SECTION
  const venueY = 1480;
  
  // "to" before venue name
  ink(180, 180, 180);
  const toText = "to";
  write(toText, { center: "x", y: venueY, size: 8 });
  
  // Debug box for "to"
  if (DEBUG_MODE) {
    ink(100, 255, 255, 100); // Semi-transparent cyan
    const estimatedWidth = toText.length * 8 * 0.6;
    const estimatedHeight = 8 * 1.2;
    box(centerX - estimatedWidth/2, venueY - estimatedHeight/2, estimatedWidth, estimatedHeight);
  }
  
  // "EL CID" - Bold venue name (smaller size)
  ink(0, 0, 0, 150); // Shadow
  const elCidText = "EL CID";
  const elCidY = venueY + 64;
  write(elCidText, { center: "x", y: elCidY, size: 20 });
  
  ink(255, 100, 100); // Red accent
  const elCidMainY = venueY + 60;
  write(elCidText, { center: "x", y: elCidMainY, size: 20 });

  // Debug box for "EL CID"
  if (DEBUG_MODE) {
    ink(255, 0, 0, 100); // Semi-transparent red
    const dims = estimateTextDimensions(elCidText, 20);
    box(centerX - dims.width/2, elCidMainY - dims.height/2, dims.width, dims.height);
  }  // Address - clean and readable (more spacing)
  ink(200, 200, 200);
  const addressText = "4212 W Sunset Blvd";
  const addressY = venueY + 220;
  write(addressText, { center: "x", y: addressY, size: 8 });

  // Debug box for address
  if (DEBUG_MODE) {
    ink(200, 100, 255, 100); // Semi-transparent purple
    const estimatedWidth = addressText.length * 8 * 0.6;
    const estimatedHeight = 8 * 1.2;
    box(centerX - estimatedWidth/2, addressY - estimatedHeight/2, estimatedWidth, estimatedHeight);
  }

  // "hosted by the catalyst la" - positioned in bottom left corner, within safe zone
  ink(150, 150, 150);
  const hostedText = "hosted by the catalyst la";
  const hostedX = 150;
  const hostedY = 1850;
  write(hostedText, { x: hostedX, y: hostedY, size: 6 });

  // Debug box for "hosted by"
  if (DEBUG_MODE) {
    ink(100, 200, 100, 100); // Semi-transparent light green
    const estimatedWidth = hostedText.length * 6 * 0.6;
    const estimatedHeight = 6 * 1.2;
    box(hostedX, hostedY - estimatedHeight/2, estimatedWidth, estimatedHeight);
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
  blur(3);
  
  scroll(0, 1);
}