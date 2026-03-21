// Gradient Test, 2025.08.03
// Testing gradient support in ink() function with masking
// 📚 DOCUMENTATION: See /reports/ink-function-gradient-documentation.md for complete usage guide
// Demonstrates: ink("gradient:color1-color2-color3") syntax with mask integration

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, mask, unmask }) {
  wipe("black"); // Clear the background.
  
  const margin = 20;
  const spacing = screen.height / 8;
  
  // Test 1: Simple blue to white gradient line
  ink("gradient:blue-white");
  line(margin, spacing, screen.width - margin, spacing);
  
  // Test 2: Multi-color gradient line
  ink("gradient:red-yellow-blue");
  line(margin, spacing * 2, screen.width - margin, spacing * 2);
  
  // Test 3: Gradient boxes with mask testing
  const boxSize = 80;
  const boxSpacing = 100;
  
  // First box: Gradient without mask
  ink("gradient:green-purple");
  box(margin, spacing * 3, boxSize, boxSize);
  
  // Second box: Same gradient with mask
  const maskBox = { x: margin + boxSpacing, y: spacing * 3, width: boxSize, height: boxSize };
  mask(maskBox);
  ink("gradient:green-purple");
  box(margin + boxSpacing, spacing * 3, boxSize, boxSize);
  unmask();
  
  // Third box: Different gradient with mask (vertical orientation)
  mask({ x: margin + boxSpacing * 2, y: spacing * 3, width: boxSize, height: boxSize });
  ink("gradient:cyan-magenta");
  box(margin + boxSpacing * 2, spacing * 3, boxSize, boxSize);
  unmask();
  
  // Test 4: Smaller boxes to show mask gradient behavior
  const smallBoxSize = 40;
  for (let i = 0; i < 3; i++) {
    const boxX = margin + i * (smallBoxSize + 10);
    const boxY = spacing * 5;
    
    // Set mask to each small box individually
    mask({ x: boxX, y: boxY, width: smallBoxSize, height: smallBoxSize });
    ink("gradient:red-yellow-blue");
    box(boxX, boxY, smallBoxSize, smallBoxSize);
    unmask();
  }
  
  // Test 5: Line with mask
  mask({ x: margin, y: spacing * 6 - 10, width: 200, height: 20 });
  ink("gradient:orange-purple");
  line(margin, spacing * 6, screen.width - margin, spacing * 6);
  unmask();
  
  // Test 6: Info text
  ink("white");
  write("Gradient Test with Masking", { x: margin, y: spacing * 7 });
  ink("gray");
  write(`Screen: ${screen.width}x${screen.height}`, { x: margin, y: spacing * 7 + 15 });
  
  if (pen) {
    ink("white");
    circle(pen.x, pen.y, 6); // Paint a cursor
  }
}

export { paint };
