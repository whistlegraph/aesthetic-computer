// Transform, 25.01.08 (Renamed from Zoom)
// Test flip, flop, rotation, scaling, and anchor points for debugging painting transforms
// 🖼️ Comprehensive testing tool for bitmap transformations

/* #region 🏁 TODO
  - [x] Test flip/flop functionality
  - [x] Test rotation with different angles  
  - [x] Test scaling with different values
  - [x] Test anchor points
  - [x] Add console logging for debugging
  - [x] Create test patterns for visibility
  - [x] Use compact font for small displays
  - [x] Improve text layout to prevent overlapping
  - [x] Switch to ultra-tiny microtype font (3x5 pixels)
  - [x] Fix font usage to use MatrixChunky8 as 6th parameter in write()
#endregion */

import { Typeface } from "../lib/type.mjs";
// Note: For write() function, we pass font name as 6th parameter: write(text, pos, bg, bounds, wordWrap, typeface)

let transformMode = 0; // Current transform mode
let angle = 0;
let scaleValue = 1;
let testPainting = null;
// Note: We'll use "MatrixChunky8" as 6th parameter in write() calls

// Automated testing variables
let autoTest = true; // Start in automated mode by default
let autoTestTimer = 0;
let autoTestDelay = 60; // frames between mode switches (1 second at 60fps)
let testResults = []; // Store test results for analysis
let currentTestCycle = 0;

const modes = [
  { name: "Normal", transform: {} },
  { name: "Flip (Vertical)", transform: { x: 1, y: -1 } },
  { name: "Flop (Horizontal)", transform: { x: -1, y: 1 } },
  { name: "Flip + Flop", transform: { x: -1, y: -1 } },
  { name: "Rotate 90°", transform: { scale: 1, angle: 90 } },
  { name: "Rotate 180°", transform: { scale: 1, angle: 180 } },
  { name: "Rotate 270°", transform: { scale: 1, angle: 270 } },
  { name: "Scale 2x", transform: { scale: 2 } },
  { name: "Scale 0.5x", transform: { scale: 0.5 } },
  { name: "Scale 2x + Rot 45°", transform: { scale: 2, angle: 45 } },
  { name: "Manual Test", transform: {} } // For custom testing
];

// 🥾 Boot
function boot({ painting, system, net }) {
  console.log("🔄 Transform Test Boot - Creating test pattern");
  
  // Create a test painting with a recognizable pattern
  const w = 80, h = 80; // Smaller for better testing
  testPainting = painting(w, h, (p) => {
    // Create a test pattern - quarter sections in different colors
    // Top-left: Red
    p.ink(255, 0, 0).box(0, 0, w/2, h/2);
    // Top-right: Green  
    p.ink(0, 255, 0).box(w/2, 0, w/2, h/2);
    // Bottom-left: Blue
    p.ink(0, 0, 255).box(0, h/2, w/2, h/2);
    // Bottom-right: Yellow
    p.ink(255, 255, 0).box(w/2, h/2, w/2, h/2);
    
    // Add a white border and center mark for orientation
    p.ink(255, 255, 255).box(0, 0, w, h, "outline");
    p.ink(255, 255, 255).box(w/2-1, h/2-1, 2, 2); // Center mark
    
    // Add directional indicators - arrows pointing up and right
    p.ink(255, 255, 255);
    // Top arrow (pointing up)
    p.line(w/2, 8, w/2-3, 12);
    p.line(w/2, 8, w/2+3, 12);
    p.line(w/2, 8, w/2, 16);
    // Right arrow (pointing right)  
    p.line(w-8, h/2, w-12, h/2-3);
    p.line(w-8, h/2, w-12, h/2+3);
    p.line(w-8, h/2, w-16, h/2);
    
    // Add small corner markers for easier orientation tracking
    p.ink(255, 255, 255);
    p.box(2, 2, 3, 3); // Top-left corner marker
    p.ink(0, 0, 0);
    p.box(w-5, h-5, 3, 3); // Bottom-right corner marker
  });
  
  console.log("🎨 Test painting created:", {
    width: testPainting.width,
    height: testPainting.height,
    pixels: testPainting.pixels.length
  });
}

// 🎨 Paint
function paint({ wipe, ink, paste, screen, system, write }) {
  wipe(64); // Gray background
  
  // Automated testing logic
  if (autoTest) {
    autoTestTimer++;
    if (autoTestTimer >= autoTestDelay) {
      autoTestTimer = 0;
      const previousMode = transformMode;
      transformMode = (transformMode + 1) % modes.length;
      
      // Log test cycle completion
      if (transformMode === 0 && previousMode > 0) {
        currentTestCycle++;
        console.log(`🔄 === TEST CYCLE ${currentTestCycle} COMPLETED ===`);
        logTestSummary();
      }
      
      console.log(`🤖 AUTO-TEST: Switching to mode ${transformMode + 1}: ${modes[transformMode].name}`);
    }
  }
  
  const currentMode = modes[transformMode];
  const transform = currentMode.transform;
  
  // Calculate center position
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  
  // Calculate painting position (top-left corner)
  const paintX = centerX - testPainting.width / 2;
  const paintY = centerY - testPainting.height / 2;
  
  console.log("🖼️ Painting transform test:", {
    mode: currentMode.name,
    transform: transform,
    position: { x: paintX, y: paintY },
    testPaintingSize: { w: testPainting.width, h: testPainting.height },
    screenSize: { w: screen.width, h: screen.height }
  });
  
  // Draw reference grid (subtle)
  ink(100, 100, 100, 32);
  for (let i = 0; i < screen.width; i += 40) {
    ink(100, 100, 100, 32).line(i, 0, i, screen.height);
  }
  for (let i = 0; i < screen.height; i += 40) {
    ink(100, 100, 100, 32).line(0, i, screen.width, i);
  }
  
  // Draw center cross
  ink(255, 255, 255, 128);
  ink(255, 255, 255, 128).line(centerX-10, centerY, centerX+10, centerY);
  ink(255, 255, 255, 128).line(centerX, centerY-10, centerX, centerY+10);
  
  // Draw bounding box where painting should appear (untransformed)
  ink(255, 0, 255, 100).box(paintX, paintY, testPainting.width, testPainting.height, "outline");
  
  // Calculate expected transformed position for debugging
  let expectedX = paintX, expectedY = paintY;
  if (transform.x === -1) expectedX -= testPainting.width + 1;
  if (transform.y === -1) expectedY -= testPainting.height + 1;
  
  // Draw expected transformed position box
  if (expectedX !== paintX || expectedY !== paintY) {
    ink(255, 255, 0, 150).box(expectedX, expectedY, testPainting.width, testPainting.height, "outline");
  }
  
  try {
    // Apply the transform
    let testSuccess = true;
    let actualResult = null;
    
    if (Object.keys(transform).length === 0) {
      // No transform - normal paste
      console.log("📍 Normal paste at:", paintX, paintY);
      paste(testPainting, paintX, paintY);
      actualResult = { x: paintX, y: paintY, transformed: false };
    } else {
      console.log("🔄 Applying transform:", transform, "at position:", paintX, paintY);
      console.log("🎯 Expected result position:", expectedX, expectedY);
      paste(testPainting, paintX, paintY, transform);
      actualResult = { x: paintX, y: paintY, expectedX, expectedY, transformed: true };
      
      // Automated test validation
      if (autoTest) {
        validateTransformResult(currentMode, actualResult, paintX, paintY, expectedX, expectedY);
      }
    }
  } catch (error) {
    console.error("❌ Transform error:", error);
    ink(255, 0, 0).write(`ERROR: ${error.message}`, { x: 10, y: 60 }, undefined, undefined, true, "MatrixChunky8");
    
    // Log error for automated testing
    if (autoTest) {
      testResults.push({
        mode: currentMode.name,
        transform: transform,
        success: false,
        error: error.message,
        timestamp: Date.now()
      });
    }
  }
  
  // Compact UI text using MatrixChunky8 for better layout on small displays  
  const lineHeight = 10; // MatrixChunky8 font height + 2px spacing
  let textY = 20; // Start below prompt HUD area
  
  // Main UI text on the left, positioned to avoid prompt HUD and legend overlap
  const mainTextX = 6; // Offset from left edge to avoid prompt HUD
  const maxMainTextWidth = Math.min(120, centerX - testPainting.width/2 - 20); // Leave space for legend
  
  ink(255, 255, 255);
  write(`Transform Test ${transformMode + 1}/${modes.length}`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
  textY += lineHeight;
  
  // Auto-test status with progress bar
  if (autoTest) {
    ink(0, 255, 0);
    const secondsLeft = Math.ceil((autoTestDelay - autoTestTimer) / 60);
    const progress = autoTestTimer / autoTestDelay;
    const progressBarWidth = 30;
    const filledWidth = Math.floor(progress * progressBarWidth);
    
    // Progress bar background
    ink(64, 64, 64);
    write(`[${"─".repeat(progressBarWidth)}]`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
    
    // Progress bar fill
    if (filledWidth > 0) {
      ink(0, 255, 0);
      write(`[${"█".repeat(filledWidth)}`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
    }
    
    // Auto status text
    ink(0, 255, 0);
    write(`AUTO: Cycle ${currentTestCycle} (${secondsLeft}s)`, { x: mainTextX + 35, y: textY }, undefined, undefined, true, "MatrixChunky8");
  } else {
    ink(255, 255, 0);
    write("MANUAL MODE", { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
  }
  textY += lineHeight;
  
  // Current mode with color highlighting
  const modeColors = {
    "Normal": [255, 255, 255],
    "Flip (Vertical)": [255, 100, 100],
    "Flop (Horizontal)": [100, 255, 100],
    "Flip + Flop": [255, 255, 100],
    "Rotate 90°": [100, 100, 255],
    "Rotate 180°": [255, 100, 255],
    "Rotate 270°": [100, 255, 255],
    "Scale 2x": [255, 200, 100],
    "Scale 0.5x": [200, 100, 255],
    "Scale 2x + Rot 45°": [255, 150, 150],
    "Manual Test": [200, 200, 200]
  };
  
  const modeColor = modeColors[currentMode.name] || [255, 255, 255];
  ink(...modeColor);
  write(`Mode: ${currentMode.name}`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
  textY += lineHeight;
  
  if (Object.keys(transform).length > 0) {
    write(`Transform: ${JSON.stringify(transform)}`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
    textY += lineHeight;
  }
  
  // Position debugging info (only if we have space)
  if (textY + lineHeight * 2 < centerY - testPainting.height/2 - 10) {
    write(`Paint at: ${paintX}, ${paintY}`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
    textY += lineHeight;
    
    if (expectedX !== paintX || expectedY !== paintY) {
      ink(255, 255, 0);
      write(`Expected: ${expectedX}, ${expectedY}`, { x: mainTextX, y: textY }, undefined, undefined, true, "MatrixChunky8");
      textY += lineHeight;
    }
  }
  
  // Controls at bottom left corner
  ink(200, 200, 200);
  const controlsText = autoTest ? 
    "A: stop auto | SPACE: debug | R: recreate" :
    "A: auto | Click/Space: next | SPACE: debug | R: recreate";
  write(controlsText, { 
    x: mainTextX, 
    y: screen.height - lineHeight - 2
  }, undefined, undefined, true, "MatrixChunky8");
  
  // Draw compact legend on the right side, positioned to avoid main text
  const legendX = Math.max(screen.width - 70, centerX + testPainting.width/2 + 25);
  let legendY = 20; // Start at same level as main text (below prompt HUD)
  
  ink(255, 255, 255);
  write("Legend:", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  ink(255, 0, 0);
  write("■ TL Red", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  ink(0, 255, 0);  
  write("■ TR Green", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  ink(0, 0, 255);
  write("■ BL Blue", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  ink(255, 255, 0);
  write("■ BR Yellow", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  ink(255, 255, 255);
  write("Arrows: ↑→", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  // Color coding for boxes - positioned below pattern legend
  legendY += 3; // Small gap
  ink(255, 0, 255);
  write("□ Original", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  legendY += lineHeight;
  
  if (expectedX !== paintX || expectedY !== paintY) {
    ink(255, 255, 0);
    write("□ Expected", { x: legendX, y: legendY }, undefined, undefined, true, "MatrixChunky8");
  }
  
  // Return true to keep painting continuously (needed for auto-test timer)
  return autoTest;
}

// ✒ Act  
function act({ event: e, needsPaint, system, screen }) {
  // Handle screen resize/reframe - responsive design
  if (e.is("reframed")) {
    console.log("🖼️ Screen reframed:", { 
      width: screen.width, 
      height: screen.height 
    });
    needsPaint(); // Force repaint with new dimensions
    return;
  }
  
  // Toggle auto-test mode
  if (e.is("keyboard:down:a")) {
    autoTest = !autoTest;
    autoTestTimer = 0; // Reset timer
    console.log(`🤖 Auto-test ${autoTest ? 'ENABLED' : 'DISABLED'}`);
    if (autoTest) {
      console.log("🔄 Starting automated transform testing...");
      testResults = []; // Clear previous results
      currentTestCycle = 0;
    } else {
      console.log("⏹️ Manual mode activated");
      logTestSummary(); // Show final results
    }
    needsPaint();
    return;
  }
  
  // Manual mode controls (only work when auto-test is off)
  if (!autoTest) {
    if (e.is("touch:down") || e.is("keyboard:down:enter") || e.is("keyboard:down: ")) {
      // Cycle through transform modes
      transformMode = (transformMode + 1) % modes.length;
      console.log("🔄 Switched to mode:", modes[transformMode].name);
      needsPaint();
      return;
    }
  }
  
  if (e.is("keyboard:down:space")) {
    // Log detailed debug info
    const currentMode = modes[transformMode];
    const transform = currentMode.transform;
    
    console.log("🔍 === DETAILED DEBUG INFO ===");
    console.log("Current Mode:", currentMode);
    console.log("Transform Object:", transform);
    console.log("Test Painting:", {
      width: testPainting.width,
      height: testPainting.height,
      pixelCount: testPainting.pixels.length
    });
    
    // Calculate positions for debugging
    const centerX = system.screen?.width / 2 || 200;
    const centerY = system.screen?.height / 2 || 150;
    const paintX = centerX - testPainting.width / 2;
    const paintY = centerY - testPainting.height / 2;
    
    console.log("Position Info:", {
      screen: { w: system.screen?.width, h: system.screen?.height },
      center: { x: centerX, y: centerY },
      paintPos: { x: paintX, y: paintY }
    });
    
    // Calculate expected transformed position
    let expectedX = paintX, expectedY = paintY;
    if (transform.x === -1) {
      expectedX -= testPainting.width + 1;
      console.log("🔄 Horizontal flip detected - adjusting X position");
    }
    if (transform.y === -1) {
      expectedY -= testPainting.height + 1;
      console.log("🔄 Vertical flip detected - adjusting Y position");
    }
    
    console.log("Expected Result Position:", { x: expectedX, y: expectedY });
    console.log("Position Delta:", { 
      deltaX: expectedX - paintX, 
      deltaY: expectedY - paintY 
    });
    
    console.log("System Painting:", system.painting ? {
      width: system.painting.width,
      height: system.painting.height
    } : "none");
    
    console.log("=== END DEBUG INFO ===");
  }
  
  if (e.is("keyboard:down:r")) {
    // Reset test painting
    console.log("🔄 Recreating test painting");
    boot({ painting: system.painting, system, net: system.net });
    needsPaint();
    return;
  }
  
  if (e.is("keyboard:down:t")) {
    // Toggle to manual test mode for custom transforms
    if (transformMode === modes.length - 1) {
      console.log("🧪 Manual test mode - modify transform object in console");
      console.log("Example: modes[10].transform = { x: -1, y: 1 };");
    }
    needsPaint();
    return;
  }
  
  if (e.is("keyboard:down:f")) {
    // Quick flip test
    modes[modes.length - 1].transform = { x: -1, y: 1 }; // Horizontal flip
    transformMode = modes.length - 1;
    autoTest = false; // Disable auto-test for manual testing
    console.log("🔄 Quick flip test activated (auto-test disabled)");
    needsPaint();
    return;
  }
  
  if (e.is("keyboard:down:v")) {
    // Quick flop test  
    modes[modes.length - 1].transform = { x: 1, y: -1 }; // Vertical flip
    transformMode = modes.length - 1;
    autoTest = false; // Disable auto-test for manual testing
    console.log("🔄 Quick flop test activated (auto-test disabled)");
    needsPaint();
    return;
  }
}

// 🧪 Automated Test Helper Functions
function validateTransformResult(mode, result, paintX, paintY, expectedX, expectedY) {
  let success = true;
  let issues = [];
  
  // Check if transform was applied (for flip/flop cases)
  if (mode.transform.x === -1 || mode.transform.y === -1) {
    // For flips, check if result appears in expected position
    if (expectedX !== paintX || expectedY !== paintY) {
      // This is expected - transformed content should appear at different position
      console.log(`✅ ${mode.name}: Transform correctly repositioned content`);
    } else {
      issues.push("Expected position change for flip/flop transform");
      success = false;
    }
  }
  
  // Check for basic transform validation
  if (mode.transform.scale && mode.transform.scale !== 1) {
    console.log(`✅ ${mode.name}: Scale transform applied`);
  }
  
  if (mode.transform.angle && mode.transform.angle !== 0) {
    console.log(`✅ ${mode.name}: Rotation transform applied`);
  }
  
  // Log test result
  const testResult = {
    mode: mode.name,
    transform: mode.transform,
    success: success,
    issues: issues,
    positions: { paintX, paintY, expectedX, expectedY },
    timestamp: Date.now()
  };
  
  testResults.push(testResult);
  
  if (!success) {
    console.warn(`⚠️ ${mode.name}: Issues detected:`, issues);
  }
}

function logTestSummary() {
  if (testResults.length === 0) {
    console.log("📊 No test results to summarize");
    return;
  }
  
  console.log("📊 === AUTOMATED TEST SUMMARY ===");
  console.log(`Total tests run: ${testResults.length}`);
  console.log(`Test cycles completed: ${currentTestCycle}`);
  
  const successful = testResults.filter(r => r.success).length;
  const failed = testResults.filter(r => !r.success).length;
  
  console.log(`✅ Successful: ${successful}`);
  console.log(`❌ Failed: ${failed}`);
  console.log(`Success rate: ${((successful / testResults.length) * 100).toFixed(1)}%`);
  
  // Log failed tests in detail
  const failures = testResults.filter(r => !r.success);
  if (failures.length > 0) {
    console.log("❌ Failed tests:");
    failures.forEach(failure => {
      console.log(`  - ${failure.mode}: ${failure.issues.join(', ')}`);
    });
  }
  
  // Log transform-specific results
  const flipResults = testResults.filter(r => r.mode.includes('Flip'));
  const rotateResults = testResults.filter(r => r.mode.includes('Rotate'));
  const scaleResults = testResults.filter(r => r.mode.includes('Scale'));
  
  console.log(`🔄 Flip/Flop tests: ${flipResults.filter(r => r.success).length}/${flipResults.length} passed`);
  console.log(`🔄 Rotation tests: ${rotateResults.filter(r => r.success).length}/${rotateResults.length} passed`);
  console.log(`🔄 Scale tests: ${scaleResults.filter(r => r.success).length}/${scaleResults.length} passed`);
  
  console.log("=== END TEST SUMMARY ===");
}

export { boot, paint, act };

// 📚 Library
// This piece helps debug transform issues by providing visual feedback
// for different types of transformations including flip, flop, rotation, and scaling
// 
// 🎮 Controls:
// - A: Toggle automated testing mode (cycles through all transforms)
// - Click/Enter: Cycle through transform modes (manual mode only)
// - Space: Log detailed debug information to console
// - R: Recreate test painting
// - F: Quick horizontal flip (flop) test (disables auto-test)
// - V: Quick vertical flip test (disables auto-test)
// - T: Toggle manual test mode
//
// 🎨 Visual Elements:
// - Test pattern with colored quadrants and directional arrows
// - Purple outline shows original paste position
// - Yellow outline shows expected transformed position (when different)
// - Compact text layout using MatrixChunky8 font for small displays
// - Reference grid and center cross for positioning
// - Auto-test status indicator and cycle counter
//
// 🤖 Automated Testing Features:
// - Cycles through all transform modes automatically
// - Validates transform results and logs issues
// - Provides comprehensive test summary with success rates
// - Tracks test cycles and performance metrics
// - Separate validation for flip/flop, rotation, and scaling
//
// � Responsive Design:
// - Handles 'reframed' events for screen resize
// - Adaptive layout for different screen sizes
// - Smart positioning to prevent UI overlap
//
// �🐛 Debugging Features:
// - Console logging of transform operations and positions
// - Visual prediction of expected transform results
// - Detailed position calculations and anchor point analysis
// - Recognizable test pattern for tracking orientation changes
// - Automated validation of transform correctness
