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

function boot({ Form, CUBEL, QUAD, penLock }) {
  penLock();
  
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
  
  // Create a ground plane (horizontal, at y = -1.5, below camera)
  // Subdivide it into a grid to reduce clipping artifacts
  const groundSize = 16; // Bigger ground (32x32 units total)
  const gridDivisions = 8; // 8x8 grid = 128 triangles (more subdivision)
  const cellSize = (groundSize * 2) / gridDivisions;
  
  const groundPositions = [];
  const groundColors = [];
  
  // Generate grid of quads (2 triangles each)
  for (let gz = 0; gz < gridDivisions; gz++) {
    for (let gx = 0; gx < gridDivisions; gx++) {
      const x1 = -groundSize + gx * cellSize;
      const x2 = -groundSize + (gx + 1) * cellSize;
      const z1 = -groundSize + gz * cellSize;
      const z2 = -groundSize + (gz + 1) * cellSize;
      
      // Triangle 1 (facing UP)
      groundPositions.push(
        [x1, -1.5, z1, 1], // Back Left
        [x2, -1.5, z2, 1], // Front Right
        [x1, -1.5, z2, 1]  // Front Left
      );
      
      // Triangle 2
      groundPositions.push(
        [x1, -1.5, z1, 1], // Back Left
        [x2, -1.5, z1, 1], // Back Right
        [x2, -1.5, z2, 1]  // Front Right
      );
      
      // White colors (6 vertices per quad)
      for (let i = 0; i < 6; i++) {
        groundColors.push([1.0, 1.0, 1.0, 1.0]);
      }
    }
  }
  
  groundPlane = new Form(
    { type: "triangle", positions: groundPositions, colors: groundColors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  
  // Disable fading for the ground plane so it stays visible
  groundPlane.noFade = true;
  
  console.log("🌍 Ground plane created:", groundPositions.length / 3, "triangles");
  
  // Create wireframe version of ground plane for debugging
  const wireframePositions = [];
  const wireframeColors = [];
  
  // Convert each triangle to 3 lines
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

function paint({ wipe, ink, painting }) {
  // Create ground texture with a checkerboard pattern if it doesn't exist
  if (!groundTexture) {
    const texSize = 64;
    groundTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box, line } = api;
      
      // Fill with white
      wipe(255, 255, 255); // Pure white
      
      // Draw black checkerboard squares
      const squareSize = 8;
      ink(0, 0, 0); // Black ink
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
  
  // Draw wireframes on top if enabled
  if (showWireframes) {
    ink("yellow").form(groundWireframe); // Ground plane wireframe
    ink("white").write("WIREFRAME (V to toggle)", 10, 10);
  }
  
  // Note: Crosshair is now rendered via DOM element in bios.mjs when pointer lock is enabled
}

function act({ event: e, penLock }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;
  if (!penLocked && e.is("touch")) penLock();
  
  // Toggle wireframe mode with 'V' key
  if (e.is("keyboard:down:v")) {
    showWireframes = !showWireframes;
  }
}

export const system = "fps";
export { boot, sim, paint, act };
