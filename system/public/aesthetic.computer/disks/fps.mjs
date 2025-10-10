// fps, 2024.2.15.16.28.02.162
// The most basic first person environment.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [o] Make sure pointer lock movement is the right numbers.
  - [] Rewrite this module in the lisp as an initial production piece.
  + Done
  - [x] Get Pointer Lock working.
  - [x] Add spinning cube and gradient triangle.
#endregion */

let cube, triangle, filledTriangle, texturedQuad, quadTexture, groundPlane, groundTexture, groundWireframe, penLocked = false;
let showWireframes = true; // Toggle with 'V' key (start with wireframes ON)
let graphAPI; // Store graph API reference
let graphInstance; // Store graph instance for camera access
let systemInstance; // Store system reference for render stats access
let showDebugPanel = false; // Toggle with 'P' key (start OFF)
let frameTimes = []; // Track frame times for FPS calculation
let lastFrameTime = performance.now();

// Function to log detailed scene debug info
function logSceneDebug() {
  if (!graphInstance) {
    console.error("❌ Camera not available");
    return;
  }
  
  // Access render stats directly from system.fps.renderStats
  const stats = systemInstance?.fps?.renderStats;
  const hasStats = stats && typeof stats.originalTriangles === 'number';
  
  // Build warning messages
  let warnings = [];
  if (hasStats) {
    if (stats.pixelsDrawn > 50000) {
      warnings.push("High pixel count - large triangle(s) filling screen");
    }
    if (stats.subdividedTriangles > 10) {
      warnings.push("High subdivision - triangle edge at screen boundary");
    }
  }
  
  console.log(
    "%c FPS %c·%c Scene Debug %c\n\n" +
    
    "%c Camera \n" +
    `%c   position %c(${graphInstance.x.toFixed(2)}, ${graphInstance.y.toFixed(2)}, ${graphInstance.z.toFixed(2)})%c\n` +
    `%c   looking  %cpitch ${graphInstance.rotX.toFixed(1)}° · yaw ${graphInstance.rotY.toFixed(1)}° · roll ${graphInstance.rotZ.toFixed(1)}°%c\n\n` +
    
    "%c Objects \n" +
    `%c   cube      %cat (${cube.position[0]}, ${cube.position[1]}, ${cube.position[2]}) · spinning · wireframe%c\n` +
    `%c   triangle  %cat (${triangle.position[0]}, ${triangle.position[1]}, ${triangle.position[2]}) · gradient · wireframe%c\n` +
    `%c   triangle  %cat (${filledTriangle.position[0]}, ${filledTriangle.position[1]}, ${filledTriangle.position[2]}) · filled · gradient%c\n` +
    `%c   quad      %cat (${texturedQuad.position[0]}, ${texturedQuad.position[1]}, ${texturedQuad.position[2]}) · textured · subdivided%c\n` +
    `%c   ground    %cat (${groundPlane.position[0]}, ${groundPlane.position[1]}, ${groundPlane.position[2]}) · 6×6 units · textured%c\n\n` +
    
    (hasStats 
      ? "%c Performance \n" +
        `%c   triangles %c${stats.originalTriangles} original · ${stats.clippedTriangles} clipped · ${stats.subdividedTriangles} subdivided · ${stats.trianglesRejected || 0} rejected%c\n` +
        `%c   pixels    %c${stats.pixelsDrawn?.toLocaleString() || 0}${stats.pixelsDrawn > 50000 ? " %c⚠%c" : "%c %c"}%c drawn this frame%c\n` +
        `%c   wireframe %c${stats.wireframeSegmentsTotal} segments%c\n`
      : "%c Performance \n" +
        `%c   waiting for first frame to render%c\n`
    ) +
    
    (warnings.length > 0 
      ? "\n%c ⚠ Warning \n" +
        `%c   ${warnings.join("\n   ")}%c\n`
      : ""
    ),
    
    // Title styles - pink and cyan like Aesthetic.Computer
    "background: rgba(199, 21, 133, 0.8); color: rgb(252, 231, 243); font-weight: bold; font-size: 11px; padding: 2px 6px; border-radius: 3px 0 0 3px;",
    "color: #4ecdc4; font-weight: bold; font-size: 11px;",
    "background: rgba(78, 205, 196, 0.8); color: rgb(10, 20, 40); font-weight: bold; font-size: 11px; padding: 2px 6px; border-radius: 0 3px 3px 0;",
    "",
    
    // Camera section - cyan theme
    "color: #4ecdc4; font-weight: bold; font-size: 12px;",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    
    // Objects section - pink theme
    "color: #ff6b9d; font-weight: bold; font-size: 12px;",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #ffc107; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    
    // Performance section - green/yellow theme
    ...(hasStats 
      ? [
          "color: #6ee7b7; font-weight: bold; font-size: 12px;",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;",
          stats.pixelsDrawn > 50000 ? "color: #ffc107; font-weight: bold;" : "", "",
          "color: #6c757d; font-size: 10px;", "",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", ""
        ]
      : [
          "color: #6ee7b7; font-weight: bold; font-size: 12px;",
          "color: #6c757d; font-size: 10px; font-style: italic;", ""
        ]
    ),
    
    // Warning section - yellow/red
    ...(warnings.length > 0 
      ? [
          "color: #ffc107; font-weight: bold; font-size: 12px;",
          "color: #ffb74d; font-size: 10px;", ""
        ]
      : []
    )
  );
}


function boot({ Form, CUBEL, QUAD, penLock, system }) {
  penLock();
  
  // Store system and graph instance for camera and stats access
  systemInstance = system;
  graphInstance = system?.fps?.doll?.cam;
  
  // Enable clipped wireframes by default if available
  // Note: graphAPI will be set in paint function
  
  // Create a gradient quad (6 vertices = 2 triangles)
  const quadPositions = [
    // Triangle 1
    [-1, -1, 0, 1], // Bottom Left
    [-1, 1, 0, 1],  // Top Left
    [1, 1, 0, 1],   // Top Right
    // Triangle 2
    [-1, -1, 0, 1], // Bottom Left
    [1, 1, 0, 1],   // Top Right
    [1, -1, 0, 1],  // Bottom Right
  ];
  
  // Gradient colors: corners go Red, Green, Blue, Yellow
  const quadColors = [
    [1.0, 0.0, 0.0, 1.0], // Bottom Left: RED
    [0.0, 1.0, 0.0, 1.0], // Top Left: GREEN
    [0.0, 0.0, 1.0, 1.0], // Top Right: BLUE
    [1.0, 0.0, 0.0, 1.0], // Bottom Left: RED (repeated)
    [0.0, 0.0, 1.0, 1.0], // Top Right: BLUE (repeated)
    [1.0, 1.0, 0.0, 1.0], // Bottom Right: YELLOW
  ];
  
  texturedQuad = new Form(
    { type: "triangle", positions: quadPositions, colors: quadColors },
    { pos: [-2, 0.5, -6], rot: [0, 0, 0], scale: 1 }
  );
  
  // Create a simple ground plane - just one quad (2 triangles)
  // Very small size to avoid clipping issues
  const groundSize = 3; // Very small ground plane (6x6 units total)
  
  const groundPositions = [
    // Triangle 1 (counter-clockwise winding when viewed from above)
    [-groundSize, -1.5, -groundSize, 1], // Back Left
    [-groundSize, -1.5, groundSize, 1],  // Front Left  
    [groundSize, -1.5, groundSize, 1],   // Front Right
    // Triangle 2
    [-groundSize, -1.5, -groundSize, 1], // Back Left
    [groundSize, -1.5, groundSize, 1],   // Front Right
    [groundSize, -1.5, -groundSize, 1],  // Back Right
  ];
  
  // UV coordinates tile the texture across the ground
  const groundTexCoords = [
    [0, 0],
    [0, 2],  // Tile 2x in each direction
    [2, 2],
    [0, 0],
    [2, 2],
    [2, 0]
  ];
  
  // White colors (6 vertices = 2 triangles)
  const groundColors = [
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0]
  ];
  
  groundPlane = new Form(
    { type: "triangle", positions: groundPositions, colors: groundColors, texCoords: groundTexCoords },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  
  // Disable fading for the ground plane so it stays visible
  groundPlane.noFade = true;
  
  console.log("🌍 Ground plane created:", groundPositions.length / 3, "triangles");
  
  // Create wireframe version of ground plane for debugging
  const wireframePositions = [];
  const wireframeColors = [];
  
  // Convert each triangle to 3 lines (only 2 triangles now)
  for (let i = 0; i < groundPositions.length; i += 3) {
    const v0 = groundPositions[i];
    const v1 = groundPositions[i + 1];
    const v2 = groundPositions[i + 2];
    
    // Line 1: v0 to v1
    wireframePositions.push(v0, v1);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]); // Yellow
    
    // Line 2: v1 to v2
    wireframePositions.push(v1, v2);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]);
    
    // Line 3: v2 to v0
    wireframePositions.push(v2, v0);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]);
  }
  
  groundWireframe = new Form(
    { type: "line", positions: wireframePositions, colors: wireframeColors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  groundWireframe.noFade = true;
  
  // Create a spinning wireframe cube
  cube = new Form(
    CUBEL,
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
  
  // Create a gradient wireframe triangle to the right (as line pairs)
  const trianglePositions = [
    // Line 1: Top to Bottom-right
    [0, 0.6, 0, 1], [0.5, -0.3, 0, 1],
    // Line 2: Bottom-right to Bottom-left
    [0.5, -0.3, 0, 1], [-0.5, -0.3, 0, 1],
    // Line 3: Bottom-left to Top
    [-0.5, -0.3, 0, 1], [0, 0.6, 0, 1],
  ];
  
  const triangleColors = [
    [1.0, 0.0, 0.0, 1.0], [0.0, 1.0, 0.0, 1.0],     // RED to GREEN
    [0.0, 1.0, 0.0, 1.0], [0.0, 0.0, 1.0, 1.0],     // GREEN to BLUE  
    [0.0, 0.0, 1.0, 1.0], [1.0, 0.0, 0.0, 1.0],     // BLUE to RED
  ];
  
  triangle = new Form(
    { type: "line", positions: trianglePositions, colors: triangleColors },
    { pos: [2, 0.5, -4], rot: [0, 0, 0], scale: 0.7 }
  );
  
  // Create a FILLED gradient triangle on the LEFT side of the cube
  const filledTriPositions = [
    [0, 0.6, 0, 1],      // Top vertex
    [0.5, -0.3, 0, 1],   // Bottom-right vertex
    [-0.5, -0.3, 0, 1],  // Bottom-left vertex
  ];
  
  const filledTriColors = [
    [1.0, 0.0, 0.0, 1.0],    // RED at top
    [0.0, 1.0, 0.0, 1.0],    // GREEN at bottom-right
    [0.0, 0.0, 1.0, 1.0],    // BLUE at bottom-left
  ];
  
  filledTriangle = new Form(
    { type: "triangle", positions: filledTriPositions, colors: filledTriColors },
    { pos: [-2, 0.5, -4], rot: [0, 0, 0], scale: 0.7 }
  );
}

function sim() {
  // Rotate the cube around its local center
  cube.rotation[0] += 0.3;
  cube.rotation[1] += 0.5;
  
  // Rotate the wireframe triangle around its local center
  triangle.rotation[2] += 1.0; // Fast spin around Z axis
  triangle.rotation[1] += 0.2; // Slight Y wobble
  
  // Rotate the filled triangle differently
  filledTriangle.rotation[0] += 0.4; // X axis spin
  filledTriangle.rotation[2] += 0.6; // Z axis spin
  
  // Rotate the textured quad
  texturedQuad.rotation[1] += 0.5; // Spin on Y axis
}

function paint({ wipe, ink, painting, screen, line: drawLine, box: drawBox, clearWireframeBuffer, drawBufferedWireframes, getRenderStats, setShowClippedWireframes }) {
  // Store render stats function for debug logging  
  if (!graphAPI) {
    graphAPI = { getRenderStats };
  }
  
  // Calculate FPS
  const now = performance.now();
  const deltaTime = now - lastFrameTime;
  lastFrameTime = now;
  
  // Keep last 60 frame times for smoothed FPS
  frameTimes.push(deltaTime);
  if (frameTimes.length > 60) frameTimes.shift();
  
  const avgFrameTime = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const currentFPS = Math.round(1000 / avgFrameTime);
  
  // FIRST: Set wireframe visibility BEFORE any rendering happens
  if (setShowClippedWireframes) {
    setShowClippedWireframes(showWireframes);
  }
  
  // SECOND: Clear wireframe buffer at start of frame
  if (clearWireframeBuffer) {
    clearWireframeBuffer();
  }
  
  if (!paint.frameCount) paint.frameCount = 0;
  paint.frameCount++;
  
  // Create ground texture with a checkerboard pattern if it doesn't exist
  if (!groundTexture) {
    const texSize = 64;
    groundTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box, line } = api;
      
      // Fill with dark yellow/orange
      wipe(100, 80, 0); // Darker yellow base
      
      // Draw dark blue checkerboard squares
      const squareSize = 8;
      ink(0, 0, 80); // Dark blue ink
      for (let y = 0; y < texSize / squareSize; y++) {
        for (let x = 0; x < texSize / squareSize; x++) {
          if ((x + y) % 2 === 1) {
            box(x * squareSize, y * squareSize, squareSize, squareSize);
          }
        }
      }
    });
    
    groundPlane.texture = groundTexture;
    console.log("🎨 Ground texture created:", groundTexture?.width, "x", groundTexture?.height, "=", groundTexture?.pixels?.length, "bytes");
    console.log("🎨 First few pixels:", groundTexture?.pixels?.slice(0, 16));
    console.log("🎨 Ground plane has texture:", !!groundPlane.texture);
  }
  
  // Create checkerboard texture for quad if it doesn't exist
  if (!quadTexture) {
    const texSize = 64;
    quadTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box } = api;
      wipe(50, 50, 50); // Dark gray base
      
      // Draw checkerboard pattern
      const squareSize = 8;
      for (let y = 0; y < texSize / squareSize; y++) {
        for (let x = 0; x < texSize / squareSize; x++) {
          if ((x + y) % 2 === 0) {
            ink(200, 200, 200); // Light gray squares
            box(x * squareSize, y * squareSize, squareSize, squareSize);
          }
        }
      }
    });
    
    // Assign texture to quad - now with perspective-correct mapping!
    texturedQuad.texture = quadTexture;
  }
  
  wipe("gray")
    .form(groundPlane) // Large textured ground plane
    .form(texturedQuad) // Textured quad with perspective-correct mapping
    .ink("red").form(cube)
    .ink("cyan").form(triangle)
    .ink("yellow").form(filledTriangle);
  
  // Draw wireframes on top AFTER all other forms to ensure they're always visible
  if (showWireframes) {
    // Note: Ground plane wireframes are now generated automatically via buffered system
    // No need to render separate groundWireframe form
    
    // Draw all buffered clipped wireframes
    if (drawBufferedWireframes) {
      drawBufferedWireframes();
    }
  }
  
  const debugFont = "MatrixChunky8";

  // Draw debug panel in top-right corner
  if (showDebugPanel) {
    const stats = system?.fps?.renderStats || {
      originalTriangles: 0,
      clippedTriangles: 0,
      subdividedTriangles: 0,
      wireframeSegmentsTotal: 0,
      wireframeSegmentsTextured: 0,
      wireframeSegmentsGradient: 0,
      pixelsDrawn: 0,
      trianglesRejected: 0,
    };

    const charWidth = 4;
    const lineHeight = 8;
    const title = "FPS DEBUG";
    const panelPadding = 6;
    const spacingAfterTitle = 2;
    const marginFromEdge = 8;

    const lines = [
      { color: "lime", text: `FPS: ${currentFPS}` },
      { color: "orange", text: `Frame: ${avgFrameTime.toFixed(2)}ms` },
      { color: "red", text: `Pixels: ${stats.pixelsDrawn}` },
      { color: "yellow", text: `Wireframes: ${showWireframes ? "ON" : "OFF"}` },
      { color: "cyan", text: `Original Tris: ${stats.originalTriangles}` },
      { color: "magenta", text: `Clipped Tris: ${stats.clippedTriangles}` },
      { color: "yellow", text: `Subdivided: ${stats.subdividedTriangles}` },
      { color: "red", text: `Rejected: ${stats.trianglesRejected || 0}` },
      { color: "white", text: `WF Total: ${stats.wireframeSegmentsTotal}` },
      { color: "white", text: `- Textured: ${stats.wireframeSegmentsTextured}` },
      { color: "white", text: `- Gradient: ${stats.wireframeSegmentsGradient}` },
      { color: "gray", text: "V: Wireframe" },
      { color: "gray", text: "P: Panel" },
      { color: "gray", text: "L: Log Debug" },
    ];

    const measureWidth = (text) => text.length * charWidth;
    const maxLineWidth = Math.max(measureWidth(title), ...lines.map((line) => measureWidth(line.text)));
    const minPanelWidth = 120;
    const panelWidth = Math.max(minPanelWidth, panelPadding * 2 + maxLineWidth);
    const panelHeight = panelPadding * 2 + lineHeight + spacingAfterTitle + lines.length * lineHeight;
    const panelX = screen.width - panelWidth - marginFromEdge;
    const panelY = 10;

    // Semi-transparent background
    ink(0, 0, 0, 180);
    drawBox(panelX, panelY, panelWidth, panelHeight);

    // Panel border
    ink(255, 255, 0, 255);
    drawLine(panelX, panelY, panelX + panelWidth, panelY);
    drawLine(panelX + panelWidth, panelY, panelX + panelWidth, panelY + panelHeight);
    drawLine(panelX + panelWidth, panelY + panelHeight, panelX, panelY + panelHeight);
    drawLine(panelX, panelY + panelHeight, panelX, panelY);

    // Title
    let textY = panelY + panelPadding;
    ink("white").write(title, { x: panelX + panelPadding, y: textY }, undefined, undefined, false, debugFont);
    textY += lineHeight + spacingAfterTitle;

    // Content lines
    lines.forEach(({ color, text }) => {
      ink(color).write(text, { x: panelX + panelPadding, y: textY }, undefined, undefined, false, debugFont);
      textY += lineHeight;
    });
  } else {
    // Minimal indicator when debug panel is off
    ink("white").write(`V:Wire P:Debug L:Log`, 10, 10, undefined, undefined, false, debugFont);
  }
  
  // Note: Crosshair is now rendered via DOM element in bios.mjs when pointer lock is enabled
}

function act({ event: e, penLock, setShowClippedWireframes }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;
  if (!penLocked && e.is("touch")) penLock();
  
  // Toggle wireframe mode with 'V' key
  if (e.is("keyboard:down:v")) {
    showWireframes = !showWireframes;
    // Toggle clipped triangle wireframes if available
    if (setShowClippedWireframes) {
      setShowClippedWireframes(showWireframes);
    }
  }
  
  // Toggle debug panel with 'P' key
  if (e.is("keyboard:down:p")) {
    showDebugPanel = !showDebugPanel;
  }
  
  // Log scene debug info with 'L' key
  if (e.is("keyboard:down:l")) {
    logSceneDebug();
  }
}

export const system = "fps";
export { boot, sim, paint, act };
