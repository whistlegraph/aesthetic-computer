// kidlisp-in-js.mjs, 24.08.31.17.35
// Simple split-screen KidLisp showcase with $code shorthand support

function paint({ kidlisp, wipe, screen, paintCount, clock, num, ink, line, system }) {
  console.log("🎨 PAINT START - Frame:", paintCount);
  
  const hw = Math.floor(screen.width / 2);
  const h = screen.height;
  
  // TEST 1: Just lines without KidLisp
  if (paintCount < 60) {
    console.log("🧪 TEST 1: Lines only (no KidLisp)");
    wipe("gray");
    ink("yellow");
    line(0, 0, 100, 100);
    ink("blue");
    line(50, 0, 50, screen.height);
    console.log("✅ Lines drawn without KidLisp");
    return;
  }
  
  // TEST 3: Try direct KidLisp evaluation without the painting buffer system
  if (paintCount < 120) {
    console.log("🧪 TEST 3: Direct KidLisp evaluation");
    wipe("gray");
    
    // Try to access the global KidLisp instance directly
    if (system && system.kidlisp) {
      console.log("🎯 Using direct KidLisp evaluation");
      try {
        // Left half - set a clip region and evaluate directly
        system.kidlisp.parse("(wipe green) (ink blue) (box 20 20 60 60)");
        if (system.kidlisp.ast) {
          system.kidlisp.evaluate(system.kidlisp.ast, { wipe, ink, box: (x, y, w, h) => {
            // Only draw in left half
            if (x < hw) {
              ink("blue");
              // Implement simple box drawing
              for (let i = 0; i < w; i++) {
                for (let j = 0; j < h; j++) {
                  // This is a hack - we'd need proper box function
                }
              }
            }
          }}, system.kidlisp.localEnv);
        }
      } catch (e) {
        console.error("Direct KidLisp error:", e);
      }
    }
    
    // Draw overlay lines
    ink("yellow");
    line(0, 0, 100, 100);
    ink("blue");  
    line(50, 0, 50, screen.height);
    console.log("✅ Direct evaluation test");
    return;
  }
  
  // TEST 2: Original KidLisp then lines (should fail)
  console.log("🧪 TEST 2: Original KidLisp then lines");
  
  kidlisp(0, 0, hw, h, "(wipe green) (ink blue) (box 20 20 60 60)");
  kidlisp(hw, 0, hw, h, "$air");
  
  ink("yellow");
  line(0, 0, 100, 100);
  ink("blue");
  line(50, 0, 50, screen.height);
  console.log("✅ Original method (should not see overlay lines)");
}