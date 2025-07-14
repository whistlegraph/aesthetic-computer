// A* Pathfinding Algorithm Demonstration, 2025.7.12

let grid = [];
let openSet = [];
let closedSet = [];
let path = [];
let start = null;
let end = null;
let current = null;
let running = false;
let complete = false;

// Path animation state
let pathAnimating = false;
let pathAnimationIndex = 0;
let pathAnimationTimer = 0;
let pathAnimationSpeed = 2; // Frames between each path cell coloring (faster crawling)

// Timing
let stepTimer = 0;
let stepInterval = 0; // Run every frame for maximum speed
let stepsPerFrame = 2; // Reduced to 2 steps per frame for more visible process
let restartTimer = 0;
let restartInterval = 120; // Auto-restart every 2 seconds (120 frames at 60fps)
let frameCount = 0; // Global frame counter for decay effect
let pathsFound = 0; // Count paths found on current maze
let pathsPerMaze = 999; // Keep going indefinitely on same maze

// Timer settings for auto-restart
let progressTimer = 0;
let progressDuration = 900; // 15 seconds at 60fps (15 * 60)

// Dynamic resolution and palette settings
let currentPaletteIndex = 0;
let resolutionVariations = [
  { cellSizeRange: [4, 8], minGridW: 15, minGridH: 12 },   // High resolution, small cells
  { cellSizeRange: [8, 12], minGridW: 12, minGridH: 10 },  // Medium resolution
  { cellSizeRange: [12, 20], minGridW: 8, minGridH: 6 },   // Lower resolution, big cells
  { cellSizeRange: [6, 10], minGridW: 20, minGridH: 15 },  // Medium-high resolution
  { cellSizeRange: [10, 16], minGridW: 10, minGridH: 8 }   // Medium-low resolution
];
let currentResolutionIndex = 0;

// Grid settings
let CELL_SIZE = 8; // Base cell size for calculations
let CELL_WIDTH = 8; // Actual cell width (can be stretched)
let CELL_HEIGHT = 8; // Actual cell height (can be stretched)
let GRID_WIDTH, GRID_HEIGHT;
let lastScreenWidth = 0;
let lastScreenHeight = 0;

// Cell types
const CELL_EMPTY = 0;
const CELL_OBSTACLE = 1;
const CELL_START = 2;
const CELL_END = 3;
const CELL_OPEN = 4;
const CELL_CLOSED = 5;
const CELL_PATH = 6;

// Utility functions
function removeFromArray(arr, element) {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i] === element) {
      arr.splice(i, 1);
      break;
    }
  }
}

// Vibrant Crayola-inspired colors for individual path cells
const crayolaColors = [
  [255, 20, 147], // Deep Pink
  [50, 205, 50], // Lime Green
  [255, 140, 0], // Dark Orange
  [138, 43, 226], // Blue Violet
  [255, 215, 0], // Gold
  [220, 20, 60], // Crimson
  [0, 191, 255], // Deep Sky Blue
  [255, 69, 0], // Red Orange
  [148, 0, 211], // Dark Violet
  [0, 255, 127], // Spring Green
  [255, 105, 180], // Hot Pink
  [30, 144, 255], // Dodger Blue
  [255, 165, 0], // Orange
  [199, 21, 133], // Medium Violet Red
  [127, 255, 0], // Chartreuse
  [255, 20, 147], // Deep Pink (repeat)
  [106, 90, 205], // Slate Blue
  [255, 127, 80], // Coral
  [154, 205, 50], // Yellow Green
  [255, 182, 193] // Light Pink
];

// Color pairs for path gradients (start color -> end color)
const pathColorPairs = [
  [[255, 20, 147], [138, 43, 226]], // Deep Pink -> Blue Violet
  [[50, 205, 50], [255, 215, 0]], // Lime Green -> Gold
  [[255, 140, 0], [220, 20, 60]], // Dark Orange -> Crimson
  [[0, 191, 255], [148, 0, 211]], // Deep Sky Blue -> Dark Violet
  [[255, 69, 0], [0, 255, 127]], // Red Orange -> Spring Green
  [[255, 105, 180], [30, 144, 255]], // Hot Pink -> Dodger Blue
  [[255, 165, 0], [199, 21, 133]], // Orange -> Medium Violet Red
  [[127, 255, 0], [106, 90, 205]], // Chartreuse -> Slate Blue
  [[255, 127, 80], [154, 205, 50]], // Coral -> Yellow Green
  [[255, 182, 193], [255, 20, 147]] // Light Pink -> Deep Pink
];

// Color lerping function
function lerpColor(color1, color2, t) {
  return [
    Math.round(color1[0] + (color2[0] - color1[0]) * t),
    Math.round(color1[1] + (color2[1] - color1[1]) * t),
    Math.round(color1[2] + (color2[2] - color1[2]) * t)
  ];
}

// Exploration color pairs for gradient flood effects
const explorationColorPairs = [
  [[30, 60, 120], [80, 150, 255]], // Deep Blue -> Bright Blue
  [[60, 30, 120], [150, 80, 255]], // Deep Purple -> Bright Purple
  [[120, 60, 30], [255, 150, 80]], // Deep Orange -> Bright Orange
  [[30, 120, 60], [80, 255, 150]], // Deep Green -> Bright Green
  [[120, 30, 60], [255, 80, 150]], // Deep Magenta -> Bright Magenta
];

// Multiple palette themes for variety - Crayola-inspired full spectrum
const colorPalettes = [
  {
    name: "sunset",
    backgroundColor: [255, 200, 120], // Warm peach
    obstacleColor: [120, 60, 30], // Burnt orange
    pathColorPairs: [
      [[255, 69, 0], [255, 215, 0]], // Red Orange -> Gold
      [[220, 20, 60], [255, 105, 180]], // Crimson -> Hot Pink
      [[255, 140, 0], [255, 255, 0]], // Dark Orange -> Yellow
      [[199, 21, 133], [255, 20, 147]], // Medium Violet Red -> Deep Pink
      [[255, 99, 71], [255, 165, 0]], // Tomato -> Orange
      [[205, 92, 92], [255, 182, 193]], // Indian Red -> Light Pink
      [[178, 34, 34], [255, 127, 80]], // Fire Brick -> Coral
      [[139, 69, 19], [255, 218, 185]], // Saddle Brown -> Peach Puff
      [[184, 134, 11], [255, 239, 213]], // Dark Goldenrod -> Papaya Whip
      [[160, 82, 45], [255, 228, 196]] // Saddle Brown -> Bisque
    ],
    explorationColorPairs: [
      [[200, 100, 50], [255, 180, 120]], // Burnt orange -> Light peach
      [[180, 80, 40], [240, 160, 100]], // Dark orange -> Medium peach
      [[160, 90, 60], [220, 140, 80]], // Brown orange -> Light brown
      [[140, 70, 35], [200, 120, 60]], // Dark brown -> Medium brown
      [[120, 60, 30], [180, 100, 50]], // Very dark brown -> Dark orange
    ]
  },
  {
    name: "ocean",
    backgroundColor: [173, 216, 230], // Light blue
    obstacleColor: [25, 25, 112], // Midnight blue
    pathColorPairs: [
      [[0, 191, 255], [0, 255, 255]], // Deep Sky Blue -> Cyan
      [[30, 144, 255], [0, 206, 209]], // Dodger Blue -> Dark Turquoise
      [[0, 100, 0], [0, 255, 127]], // Dark Green -> Spring Green
      [[72, 61, 139], [138, 43, 226]], // Dark Slate Blue -> Blue Violet
      [[70, 130, 180], [135, 206, 250]], // Steel Blue -> Light Sky Blue
      [[32, 178, 170], [64, 224, 208]], // Light Sea Green -> Turquoise
      [[95, 158, 160], [175, 238, 238]], // Cadet Blue -> Pale Turquoise
      [[102, 205, 170], [127, 255, 212]], // Medium Aquamarine -> Aquamarine
      [[0, 128, 128], [0, 255, 255]], // Teal -> Cyan
      [[65, 105, 225], [100, 149, 237]] // Royal Blue -> Cornflower Blue
    ],
    explorationColorPairs: [
      [[100, 150, 200], [150, 200, 255]], // Medium blue -> Light blue
      [[80, 120, 180], [130, 180, 230]], // Dark blue -> Medium light blue
      [[60, 100, 160], [110, 160, 210]], // Darker blue -> Medium blue
      [[40, 80, 140], [90, 140, 190]], // Deep blue -> Blue
      [[20, 60, 120], [70, 120, 170]], // Very deep blue -> Dark blue
    ]
  },
  {
    name: "forest",
    backgroundColor: [144, 238, 144], // Light green
    obstacleColor: [34, 139, 34], // Forest green
    pathColorPairs: [
      [[255, 215, 0], [255, 255, 0]], // Gold -> Yellow
      [[255, 165, 0], [255, 140, 0]], // Orange -> Dark Orange
      [[255, 69, 0], [220, 20, 60]], // Red Orange -> Crimson
      [[148, 0, 211], [138, 43, 226]], // Dark Violet -> Blue Violet
      [[255, 20, 147], [255, 105, 180]], // Deep Pink -> Hot Pink
      [[50, 205, 50], [124, 252, 0]], // Lime Green -> Lawn Green
      [[34, 139, 34], [0, 255, 0]], // Forest Green -> Lime
      [[107, 142, 35], [154, 205, 50]], // Olive Drab -> Yellow Green
      [[85, 107, 47], [173, 255, 47]], // Dark Olive Green -> Green Yellow
      [[128, 128, 0], [255, 255, 224]] // Olive -> Light Yellow
    ],
    explorationColorPairs: [
      [[100, 150, 100], [150, 200, 150]], // Medium green -> Light green
      [[80, 120, 80], [130, 180, 130]], // Dark green -> Medium light green
      [[60, 100, 60], [110, 160, 110]], // Darker green -> Medium green
      [[40, 80, 40], [90, 140, 90]], // Deep green -> Green
      [[20, 60, 20], [70, 120, 70]], // Very deep green -> Dark green
    ]
  },
  {
    name: "cosmic",
    backgroundColor: [75, 0, 130], // Indigo
    obstacleColor: [25, 25, 112], // Midnight blue
    pathColorPairs: [
      [[255, 0, 255], [255, 20, 147]], // Magenta -> Deep Pink
      [[0, 255, 255], [0, 191, 255]], // Cyan -> Deep Sky Blue
      [[255, 255, 0], [255, 215, 0]], // Yellow -> Gold
      [[255, 127, 80], [255, 69, 0]], // Coral -> Red Orange
      [[50, 205, 50], [0, 255, 127]], // Lime Green -> Spring Green
      [[138, 43, 226], [148, 0, 211]], // Blue Violet -> Dark Violet
      [[255, 105, 180], [199, 21, 133]], // Hot Pink -> Medium Violet Red
      [[127, 255, 212], [64, 224, 208]], // Aquamarine -> Turquoise
      [[255, 218, 185], [255, 182, 193]], // Peach Puff -> Light Pink
      [[240, 230, 140], [255, 250, 205]] // Khaki -> Lemon Chiffon
    ],
    explorationColorPairs: [
      [[150, 50, 200], [200, 100, 255]], // Purple -> Light purple
      [[120, 30, 180], [180, 80, 230]], // Dark purple -> Medium purple
      [[100, 20, 160], [160, 60, 210]], // Darker purple -> Medium light purple
      [[80, 10, 140], [140, 40, 190]], // Deep purple -> Purple
      [[60, 0, 120], [120, 20, 170]], // Very deep purple -> Dark purple
    ]
  },
  {
    name: "candy",
    backgroundColor: [255, 228, 225], // Misty rose
    obstacleColor: [139, 69, 19], // Saddle brown
    pathColorPairs: [
      [[255, 105, 180], [255, 182, 193]], // Hot Pink -> Light Pink
      [[255, 20, 147], [255, 192, 203]], // Deep Pink -> Pink
      [[138, 43, 226], [221, 160, 221]], // Blue Violet -> Plum
      [[255, 127, 80], [255, 218, 185]], // Coral -> Peach Puff
      [[255, 215, 0], [255, 255, 224]], // Gold -> Light Yellow
      [[50, 205, 50], [144, 238, 144]], // Lime Green -> Light Green
      [[0, 191, 255], [173, 216, 230]], // Deep Sky Blue -> Light Blue
      [[255, 140, 0], [255, 239, 213]], // Dark Orange -> Papaya Whip
      [[199, 21, 133], [255, 240, 245]], // Medium Violet Red -> Lavender Blush
      [[255, 69, 0], [255, 228, 196]] // Red Orange -> Bisque
    ],
    explorationColorPairs: [
      [[200, 150, 180], [255, 200, 220]], // Light pink -> Very light pink
      [[180, 130, 160], [230, 180, 200]], // Medium pink -> Light pink
      [[160, 110, 140], [210, 160, 180]], // Darker pink -> Medium pink
      [[140, 90, 120], [190, 140, 160]], // Deep pink -> Pink
      [[120, 70, 100], [170, 120, 140]], // Very deep pink -> Dark pink
    ]
  },
  {
    name: "autumn",
    backgroundColor: [255, 218, 185], // Peach puff
    obstacleColor: [101, 67, 33], // Dark brown
    pathColorPairs: [
      [[220, 20, 60], [255, 99, 71]], // Crimson -> Tomato
      [[255, 140, 0], [255, 165, 0]], // Dark Orange -> Orange
      [[184, 134, 11], [255, 215, 0]], // Dark Goldenrod -> Gold
      [[139, 69, 19], [210, 180, 140]], // Saddle Brown -> Tan
      [[128, 0, 0], [205, 92, 92]], // Maroon -> Indian Red
      [[255, 69, 0], [255, 127, 80]], // Red Orange -> Coral
      [[160, 82, 45], [222, 184, 135]], // Saddle Brown -> Burlywood
      [[165, 42, 42], [240, 128, 128]], // Brown -> Light Coral
      [[178, 34, 34], [250, 128, 114]], // Fire Brick -> Salmon
      [[199, 21, 133], [255, 182, 193]] // Medium Violet Red -> Light Pink
    ],
    explorationColorPairs: [
      [[180, 120, 60], [220, 160, 100]], // Brown -> Light brown
      [[160, 100, 40], [200, 140, 80]], // Dark brown -> Medium brown
      [[140, 80, 20], [180, 120, 60]], // Darker brown -> Brown
      [[120, 60, 0], [160, 100, 40]], // Deep brown -> Dark brown
      [[100, 40, 0], [140, 80, 20]], // Very deep brown -> Darker brown
    ]
  }
];

let currentPalette = colorPalettes[0]; // Start with sunset theme

// A* Node class
class Node {
  constructor(x, y) {
    this.x = x;
    this.y = y;
    this.g = 0; // Cost from start
    this.h = 0; // Heuristic (estimated cost to end)
    this.f = 0; // Total cost (g + h)
    this.parent = null;
    this.type = CELL_EMPTY;
    this.intensity = 1.0; // For fade effect
    this.lastUpdated = 0; // Frame when this cell was last updated
    // Permanent color blending for exploration trails
    this.openBlend = 0.0; // How much blue (open) has been blended in (0-1)
    this.closedBlend = 0.0; // How much orange (closed) has been blended in (0-1)
    this.openGradientColor = null; // Gradient color for open exploration
    this.closedGradientColor = null; // Gradient color for closed exploration
    this.pathColor = null; // Individual path color for this cell
    this.pathDirection = null; // Direction this path cell was entered from
    this.pathIntensity = 0.0; // How "baked in" this path color is (0-1)
  }

  // Calculate heuristic using Manhattan distance
  calculateHeuristic(end) {
    this.h = Math.abs(this.x - end.x) + Math.abs(this.y - end.y);
  }

  // Get neighbors (4-directional movement)
  getNeighbors() {
    const neighbors = [];
    const directions = [
      { x: -1, y: 0 }, // Left
      { x: 1, y: 0 },  // Right
      { x: 0, y: -1 }, // Up
      { x: 0, y: 1 },  // Down
    ];

    for (let dir of directions) {
      const newX = this.x + dir.x;
      const newY = this.y + dir.y;

      if (newX >= 0 && newX < GRID_WIDTH && newY >= 0 && newY < GRID_HEIGHT) {
        const neighbor = grid[newY][newX];
        if (neighbor.type !== CELL_OBSTACLE) {
          neighbors.push(neighbor);
        }
      }
    }

    return neighbors;
  }
}

function initializeGrid(screenWidth, screenHeight) {
  // Validate input parameters
  if (!screenWidth || !screenHeight || screenWidth <= 0 || screenHeight <= 0) {
    console.warn("Invalid screen dimensions, using defaults");
    screenWidth = 800;
    screenHeight = 600;
  }
  
  // Use dynamic resolution variations
  const resVariation = resolutionVariations[currentResolutionIndex];
  const [minCellSize, maxCellSize] = resVariation.cellSizeRange;
  const { minGridW, minGridH } = resVariation;
  
  // Calculate optimal cell size within the specified range
  let bestCellSize = minCellSize;
  let bestWaste = Infinity;
  
  for (let testSize = minCellSize; testSize <= maxCellSize; testSize++) {
    const testWidth = Math.floor(screenWidth / testSize);
    const testHeight = Math.floor(screenHeight / testSize);
    
    // Calculate wasted space (pixels not used)
    const wasteX = screenWidth - (testWidth * testSize);
    const wasteY = screenHeight - (testHeight * testSize);
    const totalWaste = wasteX + wasteY;
    
    // Prefer sizes that minimize waste and create reasonable grid dimensions
    if (totalWaste < bestWaste && testWidth >= minGridW && testHeight >= minGridH) {
      bestWaste = totalWaste;
      bestCellSize = testSize;
    }
  }
  
  CELL_SIZE = bestCellSize;
  GRID_WIDTH = Math.floor(screenWidth / CELL_SIZE);
  GRID_HEIGHT = Math.floor(screenHeight / CELL_SIZE);
  
  // Ensure minimum grid size based on current resolution variation
  GRID_WIDTH = Math.max(minGridW, GRID_WIDTH);
  GRID_HEIGHT = Math.max(minGridH, GRID_HEIGHT);
  
  // Recalculate cell size to fill screen exactly (stretch to fit)
  const actualCellWidth = screenWidth / GRID_WIDTH;
  const actualCellHeight = screenHeight / GRID_HEIGHT;
  
  // Store both dimensions for perfect screen fit
  CELL_WIDTH = actualCellWidth;
  CELL_HEIGHT = actualCellHeight;

  // Create the grid
  grid = [];
  for (let y = 0; y < GRID_HEIGHT; y++) {
    grid[y] = [];
    for (let x = 0; x < GRID_WIDTH; x++) {
      grid[y][x] = new Node(x, y);
    }
  }

  // Add some obstacles (about 20% of the grid)
  const obstacleCount = Math.floor(GRID_WIDTH * GRID_HEIGHT * 0.2);
  for (let i = 0; i < obstacleCount; i++) {
    const x = Math.floor(Math.random() * GRID_WIDTH);
    const y = Math.floor(Math.random() * GRID_HEIGHT);
    grid[y][x].type = CELL_OBSTACLE;
  }
}

function findStartAndEnd() {
  // Define screen sections for start and end placement
  // Divide screen into quadrants to ensure distance
  const halfWidth = Math.floor(GRID_WIDTH / 2);
  const halfHeight = Math.floor(GRID_HEIGHT / 2);
  const quarterWidth = Math.floor(GRID_WIDTH / 4);
  const quarterHeight = Math.floor(GRID_HEIGHT / 4);
  
  // Define corner sections with some padding from edges
  const sections = [
    { // Top-left
      xMin: quarterWidth * 0.2, 
      xMax: halfWidth - quarterWidth * 0.2,
      yMin: quarterHeight * 0.2, 
      yMax: halfHeight - quarterHeight * 0.2
    },
    { // Top-right  
      xMin: halfWidth + quarterWidth * 0.2, 
      xMax: GRID_WIDTH - quarterWidth * 0.2,
      yMin: quarterHeight * 0.2, 
      yMax: halfHeight - quarterHeight * 0.2
    },
    { // Bottom-left
      xMin: quarterWidth * 0.2, 
      xMax: halfWidth - quarterWidth * 0.2,
      yMin: halfHeight + quarterHeight * 0.2, 
      yMax: GRID_HEIGHT - quarterHeight * 0.2
    },
    { // Bottom-right
      xMin: halfWidth + quarterWidth * 0.2, 
      xMax: GRID_WIDTH - quarterWidth * 0.2,
      yMin: halfHeight + quarterHeight * 0.2, 
      yMax: GRID_HEIGHT - quarterHeight * 0.2
    }
  ];
  
  // Try different section combinations
  const sectionPairs = [
    [0, 3], // Top-left to Bottom-right (diagonal)
    [1, 2], // Top-right to Bottom-left (diagonal)  
    [0, 1], // Top-left to Top-right (horizontal)
    [2, 3], // Bottom-left to Bottom-right (horizontal)
    [0, 2], // Top-left to Bottom-left (vertical)
    [1, 3]  // Top-right to Bottom-right (vertical)
  ];
  
  let attempts = 0;
  do {
    // Pick a random section pair
    const pairIndex = Math.floor(Math.random() * sectionPairs.length);
    const [startSectionIdx, endSectionIdx] = sectionPairs[pairIndex];
    const startSection = sections[startSectionIdx];
    const endSection = sections[endSectionIdx];
    
    // Find start point in first section
    const startX = Math.floor(Math.random() * (startSection.xMax - startSection.xMin) + startSection.xMin);
    const startY = Math.floor(Math.random() * (startSection.yMax - startSection.yMin) + startSection.yMin);
    
    // Find end point in second section  
    const endX = Math.floor(Math.random() * (endSection.xMax - endSection.xMin) + endSection.xMin);
    const endY = Math.floor(Math.random() * (endSection.yMax - endSection.yMin) + endSection.yMin);
    
    // Ensure coordinates are within bounds
    if (startX >= 0 && startX < GRID_WIDTH && startY >= 0 && startY < GRID_HEIGHT &&
        endX >= 0 && endX < GRID_WIDTH && endY >= 0 && endY < GRID_HEIGHT) {
      
      // Check if both points are valid (not obstacles)
      if (grid[startY][startX].type === CELL_EMPTY && 
          grid[endY][endX].type === CELL_EMPTY) {
        
        // Additional distance check - ensure they're at least half screen apart
        const distance = Math.sqrt((endX - startX) ** 2 + (endY - startY) ** 2);
        const minDistance = Math.min(GRID_WIDTH, GRID_HEIGHT) * 0.4; // At least 40% of screen
        
        if (distance >= minDistance) {
          start = grid[startY][startX];
          end = grid[endY][endX];
          start.type = CELL_START;
          end.type = CELL_END;
          return true;
        }
      }
    }
    
    attempts++;
  } while (attempts < 200); // Increased attempts for better success rate
  
  return false;
}

function resetAlgorithm() {
  // Clear algorithm state
  openSet = [];
  closedSet = [];
  path = [];
  current = null;
  running = false;
  complete = false;
  
  // Reset path animation state
  pathAnimating = false;
  pathAnimationIndex = 0;
  pathAnimationTimer = 0;
  
  // Reset only start and end cells - keep all paths and blending permanently
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      if (cell.type === CELL_START || cell.type === CELL_END) {
        cell.type = CELL_EMPTY;
      }
      // Keep path cells permanent - don't reset them
      // Reset algorithm properties but keep all blending and paths
      cell.g = 0;
      cell.h = 0;
      cell.f = 0;
      cell.parent = null;
      cell.intensity = 1.0;
      cell.lastUpdated = 0;
      // Keep openBlend, closedBlend, and path cells for permanent trails
    }
  }
}

function clearBoard() {
  // Cycle to next palette and resolution variation
  currentPaletteIndex = (currentPaletteIndex + 1) % colorPalettes.length;
  currentPalette = colorPalettes[currentPaletteIndex];
  
  currentResolutionIndex = (currentResolutionIndex + 1) % resolutionVariations.length;
  
  // Reinitialize grid with new resolution
  initializeGrid(lastScreenWidth, lastScreenHeight);
  
  // Reset counters
  pathsFound = 0;
  progressTimer = 0;
}

function startAlgorithm() {
  resetAlgorithm();
  
  // Reset the restart timer when manually starting
  restartTimer = 0;
  
  // Check if we need a new maze after finding enough paths
  if (pathsFound >= pathsPerMaze) {
    pathsFound = 0;
    // Generate a new maze by reinitializing the grid with new obstacles
    initializeGrid(lastScreenWidth, lastScreenHeight);
  }
  
  if (!findStartAndEnd()) {
    // If we can't find start/end, try a few more times
    for (let attempts = 0; attempts < 10; attempts++) {
      // Clear some obstacles to make room
      const clearCount = Math.floor(GRID_WIDTH * GRID_HEIGHT * 0.1);
      for (let i = 0; i < clearCount; i++) {
        const x = Math.floor(Math.random() * GRID_WIDTH);
        const y = Math.floor(Math.random() * GRID_HEIGHT);
        if (grid[y][x].type === CELL_OBSTACLE) {
          grid[y][x].type = CELL_EMPTY;
        }
      }
      
      if (findStartAndEnd()) {
        break;
      }
    }
  }
  
  // Initialize A* algorithm if we have valid start/end
  if (start && end) {
    openSet = [start];
    start.calculateHeuristic(end);
    start.f = start.h;
    running = true;
  }
}

function step() {
  if (!running || openSet.length === 0) {
    running = false;
    return;
  }

  // Find node with lowest f score (optimized)
  let winner = 0;
  let lowestF = openSet[0].f;
  for (let i = 1; i < openSet.length; i++) {
    if (openSet[i].f < lowestF) {
      winner = i;
      lowestF = openSet[i].f;
    }
  }

  current = openSet[winner];

  // Check if we reached the goal
  if (current === end) {
    // Increment path counter for maze regeneration
    pathsFound++;
    
    // Build the path array
    path = [];
    let temp = current;
    while (temp) {
      path.push(temp);
      temp = temp.parent;
    }
    path.reverse();
    
    // Start path animation instead of coloring all at once
    pathAnimating = true;
    pathAnimationIndex = 0;
    pathAnimationTimer = 0;
    
    running = false;
    complete = true;
    return;
  }

  // Move current from open to closed set (optimized removal)
  openSet[winner] = openSet[openSet.length - 1];
  openSet.pop();
  closedSet.push(current);
  
  // Blend closed color into the cell permanently with gradient
  if (current !== start && current !== end && current.type !== CELL_OBSTACLE) {
    // Use gradient coloring for closed exploration based on current path
    const explorationPairIndex = pathsFound % currentPalette.explorationColorPairs.length;
    const [darkColor, brightColor] = currentPalette.explorationColorPairs[explorationPairIndex];
    
    // Store gradient information for this cell
    if (!current.closedGradientColor) {
      // Determine gradient position based on distance from start
      const distanceFromStart = Math.abs(current.x - start.x) + Math.abs(current.y - start.y);
      const maxDistance = GRID_WIDTH + GRID_HEIGHT; // Approximate max distance
      const gradientT = Math.min(1.0, distanceFromStart / (maxDistance * 0.5));
      current.closedGradientColor = lerpColor(darkColor, brightColor, gradientT);
    }
    
    current.closedBlend = Math.min(1.0, current.closedBlend + 0.15); // Accumulate closed blending
  }

  // Check all neighbors (pre-calculate tentative g)
  const tentativeG = current.g + 1;
  const neighbors = current.getNeighbors();
  
  for (let neighbor of neighbors) {
    // Skip if neighbor is in closed set or is an obstacle
    if (closedSet.includes(neighbor) || neighbor.type === CELL_OBSTACLE) {
      continue;
    }

    const inOpenSet = openSet.includes(neighbor);
    
    // If this path to neighbor is better than any previous one
    if (!inOpenSet) {
      openSet.push(neighbor);
      // Blend open color into the neighbor permanently with gradient
      if (neighbor !== start && neighbor !== end && neighbor.type !== CELL_OBSTACLE) {
        // Use different gradient for open exploration
        const explorationPairIndex = pathsFound % currentPalette.explorationColorPairs.length;
        const [darkColor, brightColor] = currentPalette.explorationColorPairs[explorationPairIndex];
        
        // Store gradient information for this cell
        if (!neighbor.openGradientColor) {
          // Lighter gradient for open set
          const distanceFromStart = Math.abs(neighbor.x - start.x) + Math.abs(neighbor.y - start.y);
          const maxDistance = GRID_WIDTH + GRID_HEIGHT;
          const gradientT = Math.min(1.0, distanceFromStart / (maxDistance * 0.3));
          neighbor.openGradientColor = lerpColor(brightColor, darkColor, gradientT * 0.6); // Lighter blend
        }
        
        neighbor.openBlend = Math.min(1.0, neighbor.openBlend + 0.1); // Accumulate open blending
      }
    } else if (tentativeG >= neighbor.g) {
      continue; // This is not a better path
    }

    // This path is the best until now. Record it!
    neighbor.parent = current;
    neighbor.g = tentativeG;
    neighbor.calculateHeuristic(end);
    neighbor.f = neighbor.g + neighbor.h;
  }
}

function animatePath() {
  if (!pathAnimating || !path || path.length === 0) {
    return;
  }
  
  pathAnimationTimer++;
  
  // Color next path cell every few frames
  if (pathAnimationTimer >= pathAnimationSpeed) {
    pathAnimationTimer = 0;
    
    if (pathAnimationIndex < path.length) {
      const cell = path[pathAnimationIndex];
      
      if (cell !== start && cell !== end) {
        cell.type = CELL_PATH;
        cell.intensity = 1.0;
        cell.lastUpdated = frameCount;
        
        // Calculate color gradient from start to end of path
        const pathProgress = pathAnimationIndex / Math.max(1, path.length - 1);
        
        // Select color pair based on current path
        const colorPairIndex = pathsFound % currentPalette.pathColorPairs.length;
        const [startColor, endColor] = currentPalette.pathColorPairs[colorPairIndex];
        
        // Lerp between start and end colors
        cell.pathColor = lerpColor(startColor, endColor, pathProgress);
        
        // Add direction information for potential future directional effects
        if (pathAnimationIndex > 0 && pathAnimationIndex < path.length - 1) {
          const prev = path[pathAnimationIndex - 1];
          const next = path[pathAnimationIndex + 1];
          const deltaX = next.x - prev.x;
          const deltaY = next.y - prev.y;
          // Store direction as angle in radians
          cell.pathDirection = Math.atan2(deltaY, deltaX);
        }
      }
      
      pathAnimationIndex++;
    } else {
      // Path animation complete
      pathAnimating = false;
    }
  }
}

function getCellColor(cell) {
  // Use current palette colors
  const backgroundColor = currentPalette.backgroundColor;
  const obstacleColor = currentPalette.obstacleColor;
  const palettePathColorPairs = currentPalette.pathColorPairs;
  const paletteExplorationColorPairs = currentPalette.explorationColorPairs;
  
  // Start with base colors for special cell types
  switch (cell.type) {
    case CELL_OBSTACLE:
      return obstacleColor;
    case CELL_START:
      // Use the start color from the current path's color pair
      const startColorPairIndex = pathsFound % palettePathColorPairs.length;
      const [startColor, endColor] = palettePathColorPairs[startColorPairIndex];
      return startColor;
    case CELL_END:
      // Use the end color from the current path's color pair
      const endColorPairIndex = pathsFound % palettePathColorPairs.length;
      const [startColorEnd, endColorEnd] = palettePathColorPairs[endColorPairIndex];
      return endColorEnd;
    case CELL_PATH:
      // Use the individual path color stored in the cell
      if (cell.pathColor) {
        return cell.pathColor;
      } else {
        // Fallback to default color if no individual color is set
        const colorIndex = pathsFound % pathColors.length;
        return pathColors[colorIndex];
      }
  }
  
  // For empty cells, blend exploration colors into background
  let finalColor = [...backgroundColor];
  
  // Blend open exploration (gradient colors) - much stronger
  if (cell.openBlend > 0) {
    const openColor = cell.openGradientColor || defaultOpenColor;
    for (let i = 0; i < 3; i++) {
      finalColor[i] = Math.round(
        finalColor[i] * (1 - cell.openBlend) + openColor[i] * cell.openBlend
      );
    }
  }
  
  // Blend closed exploration (gradient colors) on top - much stronger
  if (cell.closedBlend > 0) {
    const closedColor = cell.closedGradientColor || defaultClosedColor;
    for (let i = 0; i < 3; i++) {
      finalColor[i] = Math.round(
        finalColor[i] * (1 - cell.closedBlend) + closedColor[i] * cell.closedBlend
      );
    }
  }
  
  return finalColor;
}

// 🥾 Boot
function boot({ screen }) {
  lastScreenWidth = screen.width;
  lastScreenHeight = screen.height;
  initializeGrid(screen.width, screen.height);
  startAlgorithm();
}

// 🎨 Paint
function paint({ wipe, ink, box, write, screen }) {
  // Check if screen size changed and reinitialize if needed
  if (screen.width !== lastScreenWidth || screen.height !== lastScreenHeight) {
    lastScreenWidth = screen.width;
    lastScreenHeight = screen.height;
    initializeGrid(screen.width, screen.height);
    startAlgorithm();
  }
  
  wipe(25, 25, 35); // Dark artistic background to match new color scheme

  // Draw grid with stretched cells for perfect fit
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      const color = getCellColor(cell);
      
      ink(color[0], color[1], color[2]);
      box(x * CELL_WIDTH, y * CELL_HEIGHT, CELL_WIDTH, CELL_HEIGHT);
    }
  }
  
}

// 🎪 Act
function act({ event: e }) {
  if (e.is("pointerdown") || e.is("click") || e.is("tap")) {
    startAlgorithm();
  }
}

// 🧮 Sim
function sim() {
  frameCount++; // Increment global frame counter
  progressTimer++; // Increment progress timer
  
  // Check if it's time to clear the board
  if (progressTimer >= progressDuration) {
    clearBoard();
    startAlgorithm();
    return;
  }
  
  if (running) {
    // Process multiple steps per frame for faster execution
    for (let i = 0; i < stepsPerFrame; i++) {
      if (running) { // Check if still running after each step
        step();
      }
    }
  }
  
  // Handle path animation
  if (pathAnimating) {
    animatePath();
  }
  
  // Auto-restart timer - only restart when not animating a path
  if (!pathAnimating) {
    restartTimer++;
    if (restartTimer >= restartInterval) {
      restartTimer = 0;
      startAlgorithm();
    }
  }
}
