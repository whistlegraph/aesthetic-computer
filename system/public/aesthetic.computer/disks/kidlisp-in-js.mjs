// kidlisp-in-js.mjs, 24.08.31.17.35
// Simple split-screen KidLisp showcase with $code shorthand support

function paint({ kidlisp, wipe, screen, paintCount, clock, num }) {
  wipe("gray");
  const hw = Math.floor(screen.width / 2);
  const h = screen.height;
  
  // Dynamic KidLisp generation for left half - changes every frame!
  const frame = Number(paintCount); // Convert BigInt to number
  const time = clock.time();
  
  // Generate random geometric patterns and colors
  const colors = ["red", "blue", "green", "yellow", "purple", "orange", "cyan", "magenta"];
  const shapes = ["line", "box", "circle"];
  
  // Create a different pattern based on frame count
  const colorIndex = frame % colors.length;
  const shapeIndex = (frame % shapes.length);
  const repeatCount = 3 + (frame % 15); // 3-17 repetitions
  const effectIndex = frame % 4;
  
  let effects = "";
  switch(effectIndex) {
    case 0: effects = "(blur 1)"; break;
    case 1: effects = "(zoom 1.1)"; break;
    case 2: effects = "(blur 2) (zoom 1.01)"; break;
    case 3: effects = ""; break;
  }
  
  // Simple test to debug unquoted color words issue
  if (frame % 120 < 40) {
    // Test 1: Unquoted color word at start
    kidlisp(0, 0, hw, h, "blue");
  } else if (frame % 120 < 80) {
    // Test 2: Dynamic color and shape generation with accumulation
    const testCodeOptions = [
      // Test different approaches for rainbow/zebra
      "blue",
      "red (ink rainbow) (box 10 10 50 50)",
      "green (ink zebra) (circle 100 100 30)",
      "rainbow (circle 150 100 20)", // Test rainbow as direct function
      "zebra (box 200 50 30 30)", // Test zebra as direct function
      "purple (ink red) (oval 50 150 40 20)", // Test CSS color word + ink command
    ];
    const testCode = testCodeOptions[(frame / 40) % testCodeOptions.length | 0];
    kidlisp(0, 0, hw, h, testCode);
  } else {
    // Test 3: Quoted version for comparison
    kidlisp(0, 0, hw, h, "(wipe green) (ink blue) (box 20 20 60 60)");
  }
  
  kidlisp(hw, 0, hw, h, "$air"); // Right half: Test $code shorthand with animation support
}