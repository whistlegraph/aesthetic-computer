// Text Layout Debug Test Piece
// Tests the exact KidLisp HUD rendering scenario to identify the single column issue

const { floor } = Math;

// Sample KidLisp-style text with color codes (simplified for testing)
const COLOR_CODED_TEXT = `\\cyan\\fade\\lime\\:\\blue\\red-blue-black-blue-red\\reset\\
\\yellow\\ink\\reset\\ \\orange\\test\\reset\\ \\pink\\123\\reset\\`;

// Text that contains timing expressions that cause blinking
const TIMING_TEXT = `\\cyan\\fade\\lime\\:\\blue\\red-blue-black-blue-red\\reset\\
\\yellow\\(\\reset\\\\orange\\1s...\\reset\\ \\pink\\test\\reset\\ \\lime\\123\\reset\\\\yellow\\)\\reset\\`;

const CLEAN_TEXT = `fade:red-blue-black-blue-red
ink test 123`;

let testMode = 0; // 0: clean text, 1: color-coded text, 2: buffer test
let frameCount = 0;

function boot({ help }) {
  help.choose(["clean", "colors", "buffer"]);
}

function paint({ wipe, ink, write, screen, paste, painting, help }) {
  wipe(20, 20, 40); // Dark blue background
  
  frameCount++;
  
  // Test title
  ink("white").write("KidLisp HUD Text Layout Debug", 10, 10);
  ink("yellow").write(`Mode: ${["Clean Text", "Color-Coded Text", "Buffer Test"][testMode]}`, 10, 30);
  ink("gray").write("Press numbers 1-3 to switch modes", 10, 50);
  
  const startY = 80;
  
  if (testMode === 0) {
    // Test 1: Clean text (should work correctly)
    ink("lime").write("TEST 1: Clean text (expected to work):", 10, startY);
    
    // Test different rendering approaches
    ink("white").write(CLEAN_TEXT, 10, startY + 20);
    
    ink("cyan").write("Same text with bounds:", 10, startY + 60);
    write(CLEAN_TEXT, { x: 10, y: startY + 80 }, null, 200, false);
    
  } else if (testMode === 1) {
    // Test 2: Color-coded text (the problematic case)
    ink("red").write("TEST 2: Timing-based blinking (the bug):", 10, startY);
    
    // This should demonstrate the blinking character issue
    write(TIMING_TEXT, { x: 10, y: startY + 20 }, null, 200, false);
    
    // Show what it should look like (clean text for comparison)
    ink("gray").write("Should be stable like this:", 10, startY + 60);
    ink("white").write(CLEAN_TEXT, 10, startY + 80);
    
    // Analysis
    ink("yellow").write("The 't' in 'test' and '3' in '123' should NOT blink!", 10, startY + 100);
    ink("orange").write("This is caused by timing expressions in color codes.", 10, startY + 120);
    
    // Character by character breakdown
    ink("yellow").write("Character positions should be:", 10, startY + 140);
    const chars = "fade:red-blue-black-blue-red";
    for (let i = 0; i < Math.min(chars.length, 20); i++) {
      const x = 10 + i * 6;
      ink(i % 3 === 0 ? "red" : i % 3 === 1 ? "green" : "blue")
        .write(chars[i], x, startY + 160);
    }
    
  } else if (testMode === 2) {
    // Test 3: Buffer compositing test
    ink("magenta").write("TEST 3: Buffer compositing test:", 10, startY);
    
    // Create a small buffer and render text to it
    const buffer = painting(150, 50, (p) => {
      p.wipe(0, 0, 0, 0); // Transparent background
      p.ink("white").write("Buffer text", 0, 0);
      p.ink("red").write(COLOR_CODED_TEXT, 0, 20);
    });
    
    paste(buffer, 10, startY + 20);
    
    // Show buffer dimensions
    ink("yellow").write(`Buffer: ${buffer.width}×${buffer.height}px`, 10, startY + 80);
  }
  
  // Debug info
  ink("gray").write(`Frame: ${frameCount}`, 10, screen.height - 30);
  ink("gray").write(`Screen: ${screen.width}×${screen.height}px`, 10, screen.height - 15);
}

function act({ event: e, help }) {
  if (e.is("keyboard")) {
    if (e.key === "1") {
      testMode = 0;
      help.choose(["clean"]);
    } else if (e.key === "2") {
      testMode = 1;
      help.choose(["colors"]);
    } else if (e.key === "3") {
      testMode = 2;
      help.choose(["buffer"]);
    }
  }
}

export { boot, paint, act };