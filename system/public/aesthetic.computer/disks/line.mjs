// Line, 22.09.19.12.44 (Redesigned 25.01.08)
// A clean, responsive line drawing brush with proper thickness support

/* #region 🏁 todos the 1px li
  + Current Version
  - [x] Remove laggy Race smoothing system
  - [x] Fix thickness support using improved pline
  - [x] Simplify the pipeline for better performance
  - [x] Add proper rounded end caps
  - [x] Direct integration with graph.mjs optimized functions
  + Future
  - [] Optional pressure sensitivity support
  - [] Texture brushes
  - [] Multiple color modes
#endregion */

let colorParams, // Processed color parameters
  thickness, // Line thickness
  antialias = false, // Antialiasing flag
  wasPainting = false; // Track previous painting state to detect transitions

const points = []; // Current gesture stroke points
const smoothedPoints = []; // Smoothed points for rendering
let strokeToBake = null; // Function to bake the current stroke

// Export to the `about` piece / show documentation
function about({ colon, params, num }) {
  const color = num.parseColor(params);
  let name = num.findColor(color);
  let alpha = 1;
  
  if (color.length === 2 || color.length === 4)
    alpha = (color[color.length - 1] / 255).toFixed(1);
  if (color.length === 2) {
    if (color[0] === 0) name = "black";
    else if (color[0] === 255) name = "white";
    else name = "gray";
  }

  if (!name) name = "*COLOR*";

  let text = `paint ${colon[0] || 1}px ${name} lines`;
  if (alpha < 1) text += ` with ${alpha} alpha`;
  return `${text}.`;
}

// 🥾 Boot - Initialize the brush
function boot({ params, num, colon }) {
  colorParams = num.parseColor(params);
  thickness = parseInt(colon[0]) || 1; // Set line thickness with colon param
  
  // Check for antialiasing flag
  antialias = colon.includes("aa") || colon.includes("antialias");
  
  // Ensure thickness is within reasonable bounds
  thickness = Math.max(1, Math.min(thickness, 50));
  
  // Initialize state tracking
  wasPainting = false;
}

// 🧮 Sim - No simulation needed for the streamlined version
function sim() {
  // Removed the laggy Race system - direct input handling instead
}

// 🎨 Paint - Render the current stroke
function paint({ pen, ink, page, paste, screen, num, system: { nopaint } }) {
  // Track state changes to detect new strokes
  const isPainting = nopaint.is("painting");
  
  // If we just started painting (transition from not-painting to painting), start new stroke
  if (isPainting && !wasPainting) {
    points.length = 0;
    smoothedPoints.length = 0;
  }
  
  // Update state tracking
  wasPainting = isPainting;
  
  // Add current brush position if we're actively painting
  if (isPainting) {
    // Use transformed brush coordinates
    addPoint(num, nopaint.brush.x, nopaint.brush.y);
  }

  // Only render if we have points
  if (points.length > 0) {
    let currentStroke = points;
    
    // Add current brush position for preview if still painting
    if (isPainting) {
      currentStroke = [...points, { x: nopaint.brush.x, y: nopaint.brush.y }];
    }
    
    // Paint to the buffer
    page(nopaint.buffer).wipe(255, 0);
    
    if (thickness === 1) {
      if (antialias) {
        // Use antialiased lines with smoothing for smooth 1px lines
        const smoothed = currentStroke.length > 2 ? smoothStroke(currentStroke) : currentStroke;
        for (let i = 0; i < smoothed.length - 1; i++) {
          const p1 = smoothed[i];
          const p2 = smoothed[i + 1];
          ink(colorParams).line(p1.x, p1.y, p2.x, p2.y, true);
        }
      } else {
        // Use pixel-perfect rendering for crisp 1px lines (no smoothing for crispness)
        ink(colorParams).pppline(currentStroke);
      }
    } else {
      // For thick lines, always apply smoothing and use fast spinal rendering
      const smoothed = currentStroke.length > 2 ? smoothStroke(currentStroke) : currentStroke;
      ink(colorParams).pline(smoothed, thickness);
    }
    
    page(screen);
    
    // Set up the baking function (like rect does)
    strokeToBake = () => {
      paste(nopaint.buffer);
      page(nopaint.buffer).wipe(255, 0);
      strokeToBake = null;
    };
  }
}

// 📦 Bake - Commit the stroke to the canvas
function bake() {
  strokeToBake?.();
}

// ✒ Act - Handle user input
// 🔍 Preview - Show what the brush looks like
function preview({ ink, wipe }) {
  wipe("red").ink("blue").write("line", { center: "xy" });
}

const system = "nopaint";

export { about, boot, paint, sim, bake, system, preview };

// 📚 Library (Helper functions)

function addPoint(num, x, y, pressure) {
  let color;
  
  // Handle different color modes
  if (colorParams.length === 0) {
    color = num.randIntArr(255, 3); // Random color if none specified
  } else if (colorParams[0] === "rainbow") {
    color = "rainbow"; // Special rainbow mode
  } else {
    color = colorParams; // Use specified color
  }
  
  points.push({ x, y, color, pressure });
}

// Adaptive smoothing for all line types
function smoothStroke(rawPoints) {
  if (rawPoints.length < 3) return rawPoints;
  
  const smoothed = [rawPoints[0]]; // Keep first point exactly
  
  // Apply adaptive smoothing based on stroke velocity and direction changes
  for (let i = 1; i < rawPoints.length - 1; i++) {
    const prev = rawPoints[i - 1];
    const curr = rawPoints[i];
    const next = rawPoints[i + 1];
    
    // Calculate movement vectors
    const dx1 = curr.x - prev.x;
    const dy1 = curr.y - prev.y;
    const dx2 = next.x - curr.x;
    const dy2 = next.y - curr.y;
    
    // Calculate velocities (distance between points)
    const vel1 = Math.sqrt(dx1 * dx1 + dy1 * dy1);
    const vel2 = Math.sqrt(dx2 * dx2 + dy2 * dy2);
    
    // Calculate direction change (dot product)
    const dot = dx1 * dx2 + dy1 * dy2;
    const cosAngle = vel1 > 0 && vel2 > 0 ? dot / (vel1 * vel2) : 1;
    
    // Smooth more aggressively for:
    // 1. Sharp direction changes (low cosAngle)
    // 2. High velocity changes
    // 3. Very small movements (jitter)
    
    const directionChange = 1 - cosAngle; // 0 = no change, 2 = complete reversal
    const velocityChange = Math.abs(vel1 - vel2);
    const isJitter = vel1 < 2 && vel2 < 2;
    
    let smoothingStrength = 0.25; // Base smoothing
    
    if (isJitter) {
      smoothingStrength = 0.6; // Strong smoothing for jitter
    } else if (directionChange > 0.3) {
      smoothingStrength = 0.4; // Medium smoothing for direction changes
    } else if (velocityChange > 5) {
      smoothingStrength = 0.35; // Medium smoothing for velocity changes
    }
    
    // Apply smoothing with calculated strength
    smoothed.push({
      x: curr.x * (1 - smoothingStrength) + 
         (prev.x + next.x) * smoothingStrength / 2,
      y: curr.y * (1 - smoothingStrength) + 
         (prev.y + next.y) * smoothingStrength / 2,
      color: curr.color
    });
  }
  
  smoothed.push(rawPoints[rawPoints.length - 1]); // Keep last point exactly
  return smoothed;
}

// Simplified light smoothing for thick lines only (backup function)
function lightSmooth(rawPoints) {
  if (rawPoints.length < 4) return rawPoints;
  
  const smoothed = [rawPoints[0]]; // Keep first point
  
  // Light smoothing - just reduce the most extreme jitter
  for (let i = 1; i < rawPoints.length - 1; i++) {
    const prev = rawPoints[i - 1];
    const curr = rawPoints[i];
    const next = rawPoints[i + 1];
    
    // Only smooth if the point creates a sharp angle
    const dx1 = curr.x - prev.x;
    const dy1 = curr.y - prev.y;
    const dx2 = next.x - curr.x;
    const dy2 = next.y - curr.y;
    
    // Calculate dot product to detect sharp angles
    const dot = dx1 * dx2 + dy1 * dy2;
    const len1 = Math.sqrt(dx1 * dx1 + dy1 * dy1);
    const len2 = Math.sqrt(dx2 * dx2 + dy2 * dy2);
    
    if (len1 > 0 && len2 > 0) {
      const cosAngle = dot / (len1 * len2);
      
      // Only smooth sharp angles (cosAngle < 0.7 means angle > ~45 degrees)
      if (cosAngle < 0.7) {
        smoothed.push({
          x: (prev.x + curr.x * 2 + next.x) / 4,
          y: (prev.y + curr.y * 2 + next.y) / 4
        });
      } else {
        smoothed.push(curr);
      }
    } else {
      smoothed.push(curr);
    }
  }
  
  smoothed.push(rawPoints[rawPoints.length - 1]); // Keep last point
  return smoothed;
}
