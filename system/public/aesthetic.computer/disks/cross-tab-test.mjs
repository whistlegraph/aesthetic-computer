// cross-tab-test.mjs, 2025.09.04
// Test piece for cross-tab painting synchronization

/* #region 📚 README 
This test piece demonstrates cross-tab painting sync:
1. Left side: Manual drawing area
2. Right side: KidLisp drawing commands
3. Open multiple tabs and see changes sync in real-time!

Usage:
- Open aesthetic.computer/cross-tab-test in multiple tabs
- Draw on left side with mouse/touch
- Watch KidLisp animations on right side  
- Changes should appear in all tabs immediately
#endregion */

let animFrame = 0;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ system }) {
  console.log("🧪 Cross-tab painting sync test booted!");
  
  // Enable painting sync logging
  if (system?.painting) {
    console.log("🎨 Current painting:", system.painting.width + "x" + system.painting.height);
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ 
  wipe, ink, line, box, circle, write, 
  screen, paintCount, 
  kidlisp // KidLisp function for embedded code
}) {
  animFrame = paintCount;
  
  // Clear screen
  wipe("black");
  
  // Split screen vertically
  const midX = Math.floor(screen.width / 2);
  
  // Left side: Manual drawing area
  ink("white");
  line(midX, 0, midX, screen.height); // Divider line
  
  ink("gray");
  write("Manual Drawing", { x: 10, y: 20 });
  write("(Draw with mouse/touch)", { x: 10, y: 40 });
  
  // Right side: KidLisp animations
  ink("gray");
  write("KidLisp Animation", { x: midX + 10, y: 20 });
  write("(Auto-updates across tabs)", { x: midX + 10, y: 40 });
  
  // KidLisp animation - this should sync across tabs
  if (kidlisp) {
    const animTime = animFrame * 0.1;
    const kidlispCode = `
      (ink "blue")
      (def t ${animTime})
      (def x (+ 50 (* 30 (cos t))))
      (def y (+ 80 (* 20 (sin (* t 1.5)))))
      (circle x y 10)
      
      (ink "red") 
      (def x2 (+ 50 (* 40 (cos (* t 0.7)))))
      (def y2 (+ 120 (* 30 (sin (* t 0.5)))))
      (box (- x2 5) (- y2 5) 10 10)
      
      (ink "yellow")
      (line 20 160 (+ 20 (* 60 (cos (* t 2)))) (+ 160 (* 15 (sin (* t 3)))))
    `;
    
    // Render KidLisp animation on right side
    kidlisp(
      midX + 10,  // x position
      60,         // y position  
      midX - 20,  // width
      screen.height - 80, // height
      kidlispCode
    );
  }
  
  // Status info at bottom
  ink("green");
  write(`Frame: ${paintCount}`, { x: 10, y: screen.height - 30 });
  
  ink("cyan");
  write("Open multiple tabs to test sync!", { x: 10, y: screen.height - 10 });
}

// 🧮 Sim (Runs once per logic frame)
function sim({ event }) {
  // Handle any simulation logic here
  // The nopaint system will handle drawing automatically
}

export { boot, sim, paint };
