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
let showDebugPanel = true; // Toggle with 'D' key

function boot({ Form, CUBEL, QUAD, penLock, graph }) {
  penLock();
  
  // Store graph API for later use
  graphAPI = graph;
  
  // Enable clipped wireframes by default if available
  if (graphAPI?.setShowClippedWireframes) {
    graphAPI.setShowClippedWireframes(showWireframes);
  }
  
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
  // FIRST: Set wireframe visibility BEFORE any rendering happens
  if (setShowClippedWireframes) {
    setShowClippedWireframes(showWireframes);
  }
  
  // SECOND: Clear wireframe buffer at start of frame
  if (clearWireframeBuffer) {
    clearWireframeBuffer();
  }
  
  // Debug: log once per second
  if (!paint.frameCount) paint.frameCount = 0;
  paint.frameCount++;
  if (paint.frameCount % 60 === 1) {
    console.log(`🔧 FPS Paint Frame ${paint.frameCount}: showWireframes=${showWireframes}`);
    console.log(`  Direct functions:`, {
      hasDrawBufferedWireframes: !!drawBufferedWireframes,
      hasGetRenderStats: !!getRenderStats,
      hasClearWireframeBuffer: !!clearWireframeBuffer,
      hasSetShowClippedWireframes: !!setShowClippedWireframes,
    });
  }
  
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
    ink("yellow").form(groundWireframe); // Ground plane wireframe
    
    // Draw all buffered clipped wireframes
    if (drawBufferedWireframes) {
      if (paint.frameCount % 60 === 1) {
        console.log(`🎨 About to call drawBufferedWireframes()`);
      }
      drawBufferedWireframes();
      if (paint.frameCount % 60 === 1) {
        const stats = getRenderStats?.() || {};
        console.log(`📊 After drawing: Total wireframe segments=${stats.wireframeSegmentsTotal}`);
      }
    }
  }
  
  const debugFont = "MatrixChunky8";

  // Draw debug panel in top-right corner
  if (showDebugPanel) {
    const stats = getRenderStats?.() || {
      originalTriangles: 0,
      clippedTriangles: 0,
      subdividedTriangles: 0,
      wireframeSegmentsTotal: 0,
      wireframeSegmentsTextured: 0,
      wireframeSegmentsGradient: 0,
      wireframeSegmentsClipped: 0,
      wireframeSegmentsOther: 0,
    };

    const charWidth = 4;
    const lineHeight = 8;
    const title = "FPS DEBUG";
    const panelPadding = 6;
    const spacingAfterTitle = 2;
    const marginFromEdge = 8;

    const lines = [
      { color: "yellow", text: `Wireframes: ${showWireframes ? "ON" : "OFF"}` },
      { color: "cyan", text: `Original Tris: ${stats.originalTriangles}` },
      { color: "magenta", text: `Clipped Tris: ${stats.clippedTriangles}` },
      { color: "yellow", text: `Subdivided Tris: ${stats.subdividedTriangles}` },
      { color: "white", text: `WF Total: ${stats.wireframeSegmentsTotal}` },
      { color: "white", text: `- Textured: ${stats.wireframeSegmentsTextured}` },
      { color: "white", text: `- Gradient: ${stats.wireframeSegmentsGradient}` },
      { color: "white", text: `- Clipped: ${stats.wireframeSegmentsClipped}` },
      { color: "white", text: `- Other: ${stats.wireframeSegmentsOther}` },
      { color: "green", text: `Ground: ${(groundPlane?.vertices?.length || 0) / 3} tris` },
      { color: "orange", text: `Cam: Y:0 Z:0` },
      { color: "gray", text: "Press V: Wireframe" },
      { color: "gray", text: "Press D: Panel" },
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
    ink("white").write(`V:Wire D:Debug`, 10, 10, undefined, undefined, false, debugFont);
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
    console.log(`🔧 Toggled showWireframes to: ${showWireframes}`);
    // Toggle clipped triangle wireframes if available
    if (setShowClippedWireframes) {
      setShowClippedWireframes(showWireframes);
      console.log(`🔧 Called setShowClippedWireframes(${showWireframes})`);
    }
  }
  
  // Toggle debug panel with 'D' key
  if (e.is("keyboard:down:d")) {
    showDebugPanel = !showDebugPanel;
  }
}

export const system = "fps";
export { boot, sim, paint, act };
