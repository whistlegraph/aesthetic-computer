// Rotate Text Demo - MatrixChunky8 Testing
// Testing rotation issues at different X positions

let angle = 0;

function paint({ wipe, ink, write, screen, line }) {
  wipe(20, 20, 40);
  
  // Test M and O specifically since they're problematic
  const testStrings = ["MMMMM", "OOOOO", "TTTTT", "PROMPT", "TAP 2 PROMPT"];
  
  ink(200, 200, 200);
  write("Testing M, O, T at 270° rotation:", { x: 10, y: 10 });
  
  // Test each string at 270 degrees
  let xPos = 20;
  for (let i = 0; i < testStrings.length; i++) {
    const str = testStrings[i];
    const colors = [[255,100,100], [100,255,100], [100,100,255], [255,255,100], [255,100,255]];
    const color = colors[i % colors.length];
    
    ink(...color);
    write(str, { x: xPos, y: 80, rotation: 270 }, undefined, undefined, false, "MatrixChunky8");
    
    // Label above
    ink(150, 150, 150);
    write(str.substring(0, 1), { x: xPos - 2, y: 30 });
    
    xPos += 20;
  }
  
  // Also test at 0 degrees for comparison
  ink(200, 200, 200);
  write("Same strings at 0° for comparison:", { x: 10, y: 150 });
  
  xPos = 20;
  for (let i = 0; i < testStrings.length; i++) {
    const str = testStrings[i];
    const colors = [[255,100,100], [100,255,100], [100,100,255], [255,255,100], [255,100,255]];
    const color = colors[i % colors.length];
    
    ink(...color);
    write(str, { x: xPos, y: 170 }, undefined, undefined, false, "MatrixChunky8");
    
    xPos += str.length * 5 + 10;
  }
}

export { paint };
