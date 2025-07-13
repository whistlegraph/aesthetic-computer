// A* Pathfinding, 2025.7.12

let grid = [];
let algos = [];
let paths = [];
let runs = 0;
let maxRuns = 4;
let runColor = 0;

let boardTimer = 0;
let boardInterval = 360; // Slightly faster board mutations - about 6 seconds at 60fps
let boardActive = true;
let paletteTimer = 0;
let paletteInterval = 600;

// Separate timer for resolution changes (more frequent than other mutations)
let resolutionTimer = 0;
let resolutionInterval = 180; // About 3 seconds at 60fps - faster resolution changes

// Timer for full iterations/resets
let iterationTimer = 0;
let iterationInterval = 120; // 2 seconds at 60fps (120-180 frames for 2-3 seconds)

class Algo {
  constructor(runId, colorIndex) {
    this.runId = runId;
    this.colorIndex = colorIndex;
    this.openSet = [];
    this.closedSet = [];
    this.path = [];
    this.current = null;
    this.start = null;
    this.end = null;
    this.running = false;
    this.complete = false;
    this.stepCounter = 0;
    this.pathProgress = 0; // For animated path rendering
    this.animatedPath = []; // Currently visible portion of path
  }

  findPoints() {
    let startX, startY, endX, endY;
    let minDistance = Math.min(GRID_WIDTH, GRID_HEIGHT) * 0.3;
    let attempts = 0;

    do {
      startX = Math.floor(Math.random() * GRID_WIDTH);
      startY = Math.floor(Math.random() * GRID_HEIGHT);
      endX = Math.floor(Math.random() * GRID_WIDTH);
      endY = Math.floor(Math.random() * GRID_HEIGHT);

      attempts++;
      if (attempts > 50) {
        minDistance *= 0.8;
        attempts = 0;
      }

      const startValid = grid[startY][startX].type !== CELL_OBSTACLE;
      const endValid = grid[endY][endX].type !== CELL_OBSTACLE;
      const distanceOk =
        Math.abs(startX - endX) + Math.abs(startY - endY) >= minDistance;

      let tooCloseToOthers = false;
      for (let other of algos) {
        if (other !== this && other.running && other.start && other.end) {
          const distToOtherStart =
            Math.abs(startX - other.start.x) + Math.abs(startY - other.start.y);
          const distToOtherEnd =
            Math.abs(endX - other.end.x) + Math.abs(endY - other.end.y);
          if (distToOtherStart < 3 || distToOtherEnd < 3) {
            tooCloseToOthers = true;
            break;
          }
        }
      }

      if (startValid && endValid && distanceOk && !tooCloseToOthers) {
        break;
      }
    } while (attempts < 100);

    this.start = grid[startY][startX];
    this.end = grid[endY][endX];
    return this.start && this.end;
  }

  initialize() {
    if (!this.findPoints()) {
      return false; // Couldn't find valid start/end
    }

    this.openSet = [this.start];
    this.closedSet = [];
    this.path = [];
    this.current = null;
    this.running = true;
    this.complete = false;
    this.stepCounter = 0;

    this.start.calculateHeuristic(this.end);
    this.start.f = this.start.h;

    return true;
  }
}

let CELL_SIZE = 16;
let CELL_WIDTH = 16;
let CELL_HEIGHT = 16;
let GRID_WIDTH, GRID_HEIGHT;
let STEP_DELAY = 0;
let stepCounter = 0;
let RESET_DELAY = 0; // No delay between scenarios for maximum speed

// Non-uniform cell sizing for aesthetic variety
let cellWidths = [];  // Individual width for each column
let cellHeights = []; // Individual height for each row

// Performance optimization function
function optimize() {
  // Always run at maximum speed
  STEP_DELAY = 0;
}

// Batch processing for very large grids
function stepBatch() {
  for (let instance of algos) {
    if (!instance.running || instance.complete) continue;

    stepOne(instance);
  }
}

function stepOne(instance) {
  const totalCells = GRID_WIDTH * GRID_HEIGHT;
  const batchSize = totalCells > 8000 ? 10 : totalCells > 4000 ? 5 : 1;

  for (let batch = 0; batch < batchSize; batch++) {
    if (instance.openSet.length === 0) {
      instance.running = false;
      instance.complete = true;
      return;
    }

    let winner = 0;
    for (let i = 1; i < instance.openSet.length; i++) {
      if (instance.openSet[i].f < instance.openSet[winner].f) {
        winner = i;
      }
    }

    instance.current = instance.openSet[winner];

    if (instance.current === instance.end) {
      instance.path = [];
      let temp = instance.current;
      const visited = new Set();
      const maxPathLength = Math.min(GRID_WIDTH * GRID_HEIGHT, 1000);

      while (temp && instance.path.length < maxPathLength) {
        // Create unique key for cycle detection
        const nodeKey = `${temp.x}-${temp.y}`;
        
        // Check for circular reference
        if (visited.has(nodeKey)) {
          // Silently break on circular reference in batch mode
          break;
        }

        visited.add(nodeKey);
        instance.path.push(temp);
        
        // Extra safety check
        if (temp.parent === temp) {
          // Silently break on self-reference in batch mode
          break;
        }
        
        temp = temp.parent;
      }

      // Safety check for extremely long paths
      if (instance.path.length >= maxPathLength) {
        console.warn("Batch path length exceeded maximum, truncating");
      }

      instance.path.reverse();

      // Store color memory for this path
      remember(instance);

      instance.running = false;
      instance.complete = true;
      return;
    }

    // Move current from open to closed
    instance.openSet.splice(winner, 1);
    instance.closedSet.push(instance.current);

    // Add visit history to this cell
    if (!instance.current.visitHistory.includes(instance.runId)) {
      instance.current.visitHistory.push(instance.runId);
    }

    // Leave permanent exploration trace in batch mode too
    const runColors = getRunColors(instance.colorIndex);
    instance.current.colorMemory.push({
      color: runColors.closed,
      intensity: 0.15, // Subtle batch exploration trace
      runId: instance.runId,
      type: "explored",
      permanent: true,
    });

    // Check neighbors
    const neighbors = getNeighbors(instance.current);
    for (let neighbor of neighbors) {
      if (
        instance.closedSet.includes(neighbor) ||
        neighbor.type === CELL_OBSTACLE
      ) {
        continue;
      }

      const tentativeG = instance.current.g + 1;

      if (!instance.openSet.includes(neighbor)) {
        instance.openSet.push(neighbor);

        // Add visit history to open set cells
        if (!neighbor.visitHistory.includes(instance.runId)) {
          neighbor.visitHistory.push(instance.runId);
        }
      } else if (tentativeG >= neighbor.g) {
        continue;
      }

      neighbor.g = tentativeG;
      neighbor.calculateHeuristic(instance.end);
      neighbor.f = neighbor.g + neighbor.h;
      // Safety check: prevent circular parent references
      if (neighbor !== instance.current) {
        neighbor.parent = instance.current;
      }
    }
  }
}

// Animation variables
let animationProgress = 0;
let lastStepTime = 0;

// Dynamic visual variables
let currentPalette = 0;
let currentDensity = 0;
let screenWidth = 800;
let screenHeight = 600;

// Cell types
const CELL_EMPTY = 0;
const CELL_OBSTACLE = 1;
const CELL_START = 2;
const CELL_END = 3;
const CELL_OPEN = 4;
const CELL_CLOSED = 5;
const CELL_PATH = 6;

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

    // Color memory system for multi-path visualization
    this.colorMemory = []; // Array of {color, intensity, runId} objects
    this.pathHistory = []; // Which runs this cell was part of
    this.visitHistory = []; // Which runs visited this cell (open/closed)
  }

  // Calculate heuristic using Manhattan distance
  calculateHeuristic(end) {
    this.h = Math.abs(this.x - end.x) + Math.abs(this.y - end.y);
  }

  // Get neighbors
  getNeighbors() {
    const neighbors = [];
    const directions = [
      { x: -1, y: 0 },
      { x: 1, y: 0 },
      { x: 0, y: -1 },
      { x: 0, y: 1 },
      // Diagonal movement (optional)
      { x: -1, y: -1 },
      { x: 1, y: -1 },
      { x: -1, y: 1 },
      { x: 1, y: 1 },
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

// Utility functions
function heuristic(a, b) {
  return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
}

function removeFromArray(arr, element) {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i] === element) {
      arr.splice(i, 1);
      return;
    }
  }
}

// Helper function for batch algorithm
function getNeighbors(node) {
  return node.getNeighbors();
}

function initializeGrid(screenWidth, screenHeight) {
  // Use full screen width and height - no margins, no gaps
  const availableWidth = screenWidth;
  const availableHeight = screenHeight;

  // Safety check for screen dimensions
  if (availableWidth <= 0 || availableHeight <= 0) {
    console.warn("Invalid screen dimensions:", availableWidth, availableHeight);
    // Use fallback dimensions
    const fallbackWidth = 800;
    const fallbackHeight = 600;
    GRID_WIDTH = 20;
    GRID_HEIGHT = 15;
    CELL_WIDTH = fallbackWidth / GRID_WIDTH;
    CELL_HEIGHT = fallbackHeight / GRID_HEIGHT;
    CELL_SIZE = Math.min(CELL_WIDTH, CELL_HEIGHT);
    return;
  }

  // Dynamic grid density with much wider range for aesthetic variety
  const densityOptions = [4, 6, 8, 10, 12, 16, 20, 24, 32, 40]; // From very high density to chunky
  const targetCellSize = densityOptions[currentDensity % densityOptions.length];

  // Calculate grid dimensions with more flexible aspect ratios
  GRID_WIDTH = Math.floor(availableWidth / targetCellSize);
  GRID_HEIGHT = Math.floor(availableHeight / targetCellSize);

  // Safety checks for grid dimensions
  GRID_WIDTH = Math.max(10, Math.min(GRID_WIDTH, 1000)); // Min 10, max 1000
  GRID_HEIGHT = Math.max(8, Math.min(GRID_HEIGHT, 1000)); // Min 8, max 1000

  // For aesthetic purposes, sometimes make grids more rectangular
  // Add extreme stretching for scanline effects, but skip when using native density (currentDensity = 0)
  const stretchMode = currentDensity === 0 ? 4 : currentDensity % 5; // Use natural proportions for native density
  if (stretchMode === 0) {
    // Ultra-wide scanlines (extreme horizontal stretch)
    GRID_WIDTH = Math.floor(GRID_WIDTH * 2.5); // Reduced from 3x
    GRID_HEIGHT = Math.max(5, Math.floor(GRID_HEIGHT * 0.4)); // Increased from 0.3
  } else if (stretchMode === 1) {
    // Ultra-tall scanlines (extreme vertical stretch)
    GRID_WIDTH = Math.max(8, Math.floor(GRID_WIDTH * 0.4)); // Increased from 0.3
    GRID_HEIGHT = Math.floor(GRID_HEIGHT * 2.5); // Reduced from 3x
  } else if (stretchMode === 2) {
    // Wide grid (landscape aesthetic)
    GRID_WIDTH = Math.floor(GRID_WIDTH * 1.5);
  } else if (stretchMode === 3) {
    // Tall grid (portrait aesthetic)
    GRID_HEIGHT = Math.floor(GRID_HEIGHT * 1.5);
  }
  // Otherwise keep natural proportions (stretchMode === 4 or default)

  // Final safety checks after stretching
  GRID_WIDTH = Math.max(5, GRID_WIDTH);
  GRID_HEIGHT = Math.max(4, GRID_HEIGHT);

  // Calculate exact cell dimensions to stretch perfectly to screen edges
  // Allow rectangular cells for aesthetic stretching
  const cellWidth = availableWidth / GRID_WIDTH;
  const cellHeight = availableHeight / GRID_HEIGHT;

  // Store both width and height separately for perfect stretch
  CELL_WIDTH = cellWidth;
  CELL_HEIGHT = cellHeight;
  CELL_SIZE = Math.min(cellWidth, cellHeight); // Keep for compatibility

  grid = [];
  for (let y = 0; y < GRID_HEIGHT; y++) {
    grid[y] = [];
    for (let x = 0; x < GRID_WIDTH; x++) {
      grid[y][x] = new Node(x, y);
    }
  }

  // Initialize non-uniform cell sizes for visual variety
  initCellSizes();

  // Add initial obstacles (about 25% of the grid)
  const obstacleCount = Math.floor(GRID_WIDTH * GRID_HEIGHT * 0.25);
  let placed = 0;

  while (placed < obstacleCount) {
    const x = Math.floor(Math.random() * GRID_WIDTH);
    const y = Math.floor(Math.random() * GRID_HEIGHT);
    const cell = grid[y][x];

    if (cell.type === CELL_EMPTY) {
      cell.type = CELL_OBSTACLE;
      placed++;
    }
  }
}

// Get cell rectangle for non-uniform grid
function getCellRect(x, y) {
  // Calculate exact pixel positions to avoid gaps
  const totalWidthWeight = cellWidths.reduce((sum, w) => sum + w, 0);
  const totalHeightWeight = cellHeights.reduce((sum, h) => sum + h, 0);
  
  // Calculate exact start positions
  let px = 0;
  for (let i = 0; i < x; i++) {
    px += Math.floor((cellWidths[i] / totalWidthWeight) * screenWidth);
  }
  
  let py = 0;
  for (let i = 0; i < y; i++) {
    py += Math.floor((cellHeights[i] / totalHeightWeight) * screenHeight);
  }
  
  // Calculate cell dimensions, ensuring last cell fills to edge
  let cellWidth = Math.floor((cellWidths[x] / totalWidthWeight) * screenWidth);
  let cellHeight = Math.floor((cellHeights[y] / totalHeightWeight) * screenHeight);
  
  // For last column/row, extend to screen edge to eliminate gaps
  if (x === GRID_WIDTH - 1) {
    cellWidth = screenWidth - px;
  }
  if (y === GRID_HEIGHT - 1) {
    cellHeight = screenHeight - py;
  }
  
  return {
    x: px,
    y: py,
    w: cellWidth,
    h: cellHeight
  };
}

// Add a single square (not full row/column)
function addSquare(x, y, type = 'empty') {
  if (x >= 0 && x < GRID_WIDTH && y >= 0 && y < GRID_HEIGHT) {
    if (type === 'obstacle') {
      grid[y][x].type = CELL_OBSTACLE;
    } else {
      grid[y][x].type = CELL_EMPTY;
    }
    grid[y][x].history = [];
  }
}

// Remove a single square (make it empty)
function removeSquare(x, y) {
  if (x >= 0 && x < GRID_WIDTH && y >= 0 && y < GRID_HEIGHT) {
    grid[y][x].type = CELL_EMPTY;
    grid[y][x].history = [];
  }
}

function newScenario() {
  // Never do full resets - always preserve obstacles and color memory

  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];

      // Always reset algorithm-specific properties
      cell.g = 0;
      cell.h = 0;
      cell.f = 0;
      cell.parent = null;

      // Only clear start/end types, keep everything else
      if (cell.type === CELL_START || cell.type === CELL_END) {
        cell.type = CELL_EMPTY;
      }
    }
  }

  // Clear any previous algorithm instances
  algos = [];

  // Decide whether to run parallel algorithms or sequential
  const shouldRunParallel = Math.random() < 0.6; // 60% chance of parallel execution
  const numParallelAlgorithms = shouldRunParallel
    ? Math.floor(Math.random() * 2) + 2
    : 1; // 2-3 parallel or 1 sequential

  // Create algorithm instances
  for (let i = 0; i < numParallelAlgorithms; i++) {
    const instance = new Algo(runs + i, (runs + i) % 4);
    if (instance.initialize()) {
      algos.push(instance);
    }
  }

  // If no instances were created successfully, create at least one with relaxed constraints
  if (algos.length === 0) {
    const fallbackInstance = new Algo(runs, runs % 4);
    // Simplified initialization for fallback
    let startX = Math.floor(Math.random() * GRID_WIDTH);
    let startY = Math.floor(Math.random() * GRID_HEIGHT);
    let endX = Math.floor(Math.random() * GRID_WIDTH);
    let endY = Math.floor(Math.random() * GRID_HEIGHT);

    // Find valid positions
    while (grid[startY][startX].type === CELL_OBSTACLE) {
      startX = Math.floor(Math.random() * GRID_WIDTH);
      startY = Math.floor(Math.random() * GRID_HEIGHT);
    }
    while (grid[endY][endX].type === CELL_OBSTACLE) {
      endX = Math.floor(Math.random() * GRID_WIDTH);
      endY = Math.floor(Math.random() * GRID_HEIGHT);
    }

    fallbackInstance.start = grid[startY][startX];
    fallbackInstance.end = grid[endY][endX];
    fallbackInstance.openSet = [fallbackInstance.start];
    fallbackInstance.running = true;
    fallbackInstance.start.calculateHeuristic(fallbackInstance.end);
    fallbackInstance.start.f = fallbackInstance.start.h;

    algos.push(fallbackInstance);
  }
}

function resetAlgorithm() {
  // Clear all algorithm instances
  algos = [];

  // Clear algorithm-specific cell types
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      if (
        cell.type === CELL_OPEN ||
        cell.type === CELL_CLOSED ||
        cell.type === CELL_PATH ||
        cell.type === CELL_START ||
        cell.type === CELL_END
      ) {
        cell.type = CELL_EMPTY;
      }
      cell.g = 0;
      cell.h = 0;
      cell.f = 0;
      cell.parent = null;
    }
  }
}

function startAlgorithm() {
  // Algorithms are now started in generateRandomScenario()
  // This function is kept for compatibility but does nothing
}

function autoRestart() {
  // Full reset with new board resolution and complete restart
  
  // Change grid resolution for visual variety
  currentDensity = (currentDensity + 1) % 10; // Cycle through different densities
  
  // Complete reset - clear everything
  runs = 0;
  algos = [];
  paths = [];
  stepCounter = 0;
  boardTimer = 0;
  resolutionTimer = 0;
  iterationTimer = 0;
  
  // Occasionally change palette for variety
  if (Math.random() < 0.3) {
    currentPalette = Math.floor(Math.random() * palettes.length);
  }
  
  // Reinitialize grid with new resolution
  initializeGrid(screenWidth, screenHeight);
  optimize();
  newScenario();
}

// Get color scheme for a specific run - more vibrant and varied
function getRunColors(runId) {
  const colorSchemes = [
    // Run 0: Electric Blue/Cyan
    {
      path: [0, 150, 255],
      closed: [0, 100, 200],
      open: [50, 200, 255],
    },
    // Run 1: Neon Green/Lime
    {
      path: [50, 255, 50],
      closed: [30, 200, 30],
      open: [100, 255, 100],
    },
    // Run 2: Hot Orange/Red
    {
      path: [255, 100, 0],
      closed: [200, 80, 0],
      open: [255, 150, 50],
    },
    // Run 3: Vivid Purple/Magenta
    {
      path: [200, 50, 255],
      closed: [150, 30, 200],
      open: [220, 100, 255],
    },
    // Run 4: Bright Yellow
    {
      path: [255, 255, 0],
      closed: [200, 200, 0],
      open: [255, 255, 100],
    },
    // Run 5: Hot Pink
    {
      path: [255, 20, 147],
      closed: [200, 15, 100],
      open: [255, 100, 180],
    },
    // Run 6: Turquoise
    {
      path: [64, 224, 208],
      closed: [40, 180, 160],
      open: [100, 255, 230],
    },
    // Run 7: Coral
    {
      path: [255, 127, 80],
      closed: [200, 100, 60],
      open: [255, 160, 120],
    },
  ];

  return colorSchemes[runId % colorSchemes.length];
}

// Color palettes for obstacles and UI elements - much more variety!
const palettes = [
  // Palette 0: Classic Dark (Cyber Purple/Teal)
  {
    obstacle: [
      [80, 60, 120],
      [120, 100, 160],
    ],
    empty: [
      [40, 50, 65],
      [60, 70, 85],
    ],
    background: [15, 20, 25],
  },
  // Palette 1: Light Mode (Dark on Light)
  {
    obstacle: [
      [20, 30, 40],
      [60, 70, 80],
    ],
    empty: [
      [220, 230, 240],
      [240, 245, 250],
    ],
    background: [250, 250, 255],
  },
  // Palette 2: High Contrast (Pure B&W)
  {
    obstacle: [
      [0, 0, 0],
      [40, 40, 40],
    ],
    empty: [
      [200, 200, 200],
      [255, 255, 255],
    ],
    background: [128, 128, 128],
  },
  // Palette 3: Neon on Black
  {
    obstacle: [
      [255, 0, 255],
      [0, 255, 255],
    ],
    empty: [
      [10, 10, 10],
      [30, 30, 30],
    ],
    background: [0, 0, 0],
  },
  // Palette 4: Warm Sunset
  {
    obstacle: [
      [200, 80, 20],
      [255, 150, 80],
    ],
    empty: [
      [255, 220, 180],
      [255, 240, 200],
    ],
    background: [255, 200, 150],
  },
  // Palette 5: Cool Ocean
  {
    obstacle: [
      [0, 50, 100],
      [20, 80, 140],
    ],
    empty: [
      [150, 200, 255],
      [200, 230, 255],
    ],
    background: [100, 150, 200],
  },
  // Palette 6: Retro Green Screen
  {
    obstacle: [
      [0, 100, 0],
      [0, 150, 0],
    ],
    empty: [
      [0, 20, 0],
      [0, 40, 0],
    ],
    background: [0, 15, 0],
  },
  // Palette 7: Amber Terminal
  {
    obstacle: [
      [255, 150, 0],
      [255, 200, 50],
    ],
    empty: [
      [50, 30, 0],
      [80, 50, 0],
    ],
    background: [20, 15, 0],
  },
  // Palette 8: Hot Pink/Cyan
  {
    obstacle: [
      [255, 20, 147],
      [0, 255, 255],
    ],
    empty: [
      [255, 240, 245],
      [240, 255, 255],
    ],
    background: [255, 230, 240],
  },
  // Palette 9: Deep Purple
  {
    obstacle: [
      [100, 0, 200],
      [150, 50, 255],
    ],
    empty: [
      [240, 230, 255],
      [250, 240, 255],
    ],
    background: [230, 220, 250],
  },
  // Palette 10: Forest/Nature
  {
    obstacle: [
      [34, 139, 34],
      [0, 100, 0],
    ],
    empty: [
      [240, 255, 240],
      [220, 255, 220],
    ],
    background: [200, 255, 200],
  },
  // Palette 11: Fire/Lava
  {
    obstacle: [
      [255, 69, 0],
      [255, 140, 0],
    ],
    empty: [
      [25, 25, 25],
      [50, 25, 25],
    ],
    background: [40, 20, 20],
  },
  // Palette 12: Ice/Arctic
  {
    obstacle: [
      [70, 130, 180],
      [100, 149, 237],
    ],
    empty: [
      [240, 248, 255],
      [230, 240, 250],
    ],
    background: [220, 235, 255],
  },
  // Palette 13: Vaporwave
  {
    obstacle: [
      [255, 0, 128],
      [128, 0, 255],
    ],
    empty: [
      [255, 192, 203],
      [221, 160, 221],
    ],
    background: [255, 182, 193],
  },
  // Palette 14: Matrix Green
  {
    obstacle: [
      [0, 255, 0],
      [50, 255, 50],
    ],
    empty: [
      [0, 0, 0],
      [10, 20, 10],
    ],
    background: [0, 10, 0],
  },
  // Palette 15: Sepia/Vintage
  {
    obstacle: [
      [101, 67, 33],
      [139, 90, 43],
    ],
    empty: [
      [245, 222, 179],
      [255, 228, 181],
    ],
    background: [250, 235, 215],
  },
];

function stepAlgorithm() {
  // Step all active algorithm instances
  for (let instance of algos) {
    if (!instance.running || instance.complete) continue;

    step(instance);
  }
}

function step(instance) {
  if (instance.openSet.length === 0) {
    instance.running = false;
    instance.complete = true;
    return; // No solution
  }

  // Find node with lowest f score
  let winner = 0;
  for (let i = 1; i < instance.openSet.length; i++) {
    if (instance.openSet[i].f < instance.openSet[winner].f) {
      winner = i;
    }
  }

  instance.current = instance.openSet[winner];

  // Check if we reached the goal
  if (instance.current === instance.end) {
    buildPath(instance);

    // Store color memory for this path
    remember(instance);

    instance.running = false;
    instance.complete = true;
    return;
  }

  // Move current from open to closed set
  removeFromArray(instance.openSet, instance.current);
  instance.closedSet.push(instance.current);

  // Add visit history to this cell
  if (!instance.current.visitHistory.includes(instance.runId)) {
    instance.current.visitHistory.push(instance.runId);
  }

  // Leave a permanent trace of exploration (subtle but visible)
  const runColors = getRunColors(instance.colorIndex);
  instance.current.colorMemory.push({
    color: runColors.closed,
    intensity: 0.2, // Subtle exploration trace
    runId: instance.runId,
    type: "explored",
    permanent: true, // Don't fade this
  });

  // Check all neighbors
  const neighbors = instance.current.getNeighbors();

  for (let neighbor of neighbors) {
    // Skip if neighbor is in closed set
    if (instance.closedSet.includes(neighbor)) continue;

    // Calculate tentative g score
    const isDiagonal =
      Math.abs(neighbor.x - instance.current.x) === 1 &&
      Math.abs(neighbor.y - instance.current.y) === 1;
    const tentativeG = instance.current.g + (isDiagonal ? 1.414 : 1); // √2 for diagonal movement

    // If this path to neighbor is better than any previous one
    if (!instance.openSet.includes(neighbor)) {
      instance.openSet.push(neighbor);

      // Add visit history to open set cells
      if (!neighbor.visitHistory.includes(instance.runId)) {
        neighbor.visitHistory.push(instance.runId);
      }

      // Leave a subtle trace when adding to open set
      const runColors = getRunColors(instance.colorIndex);
      neighbor.colorMemory.push({
        color: runColors.open,
        intensity: 0.1, // Very subtle open trace
        runId: instance.runId,
        type: "considered",
        permanent: true, // Don't fade
      });
    } else if (tentativeG >= neighbor.g) {
      continue; // This is not a better path
    }

    // This path is the best until now. Record it!
    // Safety check: prevent circular parent references
    if (neighbor !== instance.current) {
      neighbor.parent = instance.current;
      neighbor.g = tentativeG;
      neighbor.calculateHeuristic(instance.end);
      neighbor.f = neighbor.g + neighbor.h;
    }
  }
}

function buildPath(instance) {
  instance.path = [];
  let current = instance.current;
  const visited = new Set(); // Track visited nodes to prevent infinite loops
  const maxPathLength = Math.min(GRID_WIDTH * GRID_HEIGHT, 1000); // Reasonable safety limit

  while (current && instance.path.length < maxPathLength) {
    // Create a unique key for this node to detect cycles
    const nodeKey = `${current.x}-${current.y}`;
    
    // Check for circular reference
    if (visited.has(nodeKey)) {
      // Silently break on circular reference - this is expected behavior
      break;
    }

    visited.add(nodeKey);
    instance.path.push(current);
    
    // Extra safety: ensure parent is different from current
    if (current.parent === current) {
      // Silently break on self-reference
      break;
    }
    
    current = current.parent;
  }

  // Safety check for extremely long paths
  if (instance.path.length >= maxPathLength) {
    console.warn("Path length exceeded maximum, truncating");
  }

  instance.path.reverse();
  
  // Initialize animated path
  instance.pathProgress = 0;
  instance.animatedPath = [];
}

// Store the completed path in color memory (permanent paths)
function remember(instance) {
  const runColors = getRunColors(instance.colorIndex);

  // Store the actual path cells with high intensity - PERMANENT
  for (let cell of instance.path) {
    if (cell !== instance.start && cell !== instance.end) {
      cell.pathHistory.push(instance.runId);
      cell.colorMemory.push({
        color: runColors.path,
        intensity: 0.8, // Strong path visibility
        runId: instance.runId,
        type: "path",
        permanent: true, // NEVER fade paths
      });
    }
  }
  
  // Store this path in global history
  paths.push({
    path: [...instance.path],
    runId: instance.runId,
    colors: runColors,
    start: { x: instance.start.x, y: instance.start.y },
    end: { x: instance.end.x, y: instance.end.y },
  });
}

// Age color memories but preserve permanent traces
function ageColorMemories() {
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      if (cell.colorMemory && cell.colorMemory.length > 0) {
        // Only fade non-permanent memories
        for (let memory of cell.colorMemory) {
          if (!memory.permanent) {
            memory.intensity *= 0.98; // Fade temporary traces
          }
        }
        // Remove only faded non-permanent memories
        cell.colorMemory = cell.colorMemory.filter(m => 
          m.permanent || m.intensity > 0.1
        );
      }
    }
  }
}

// Color utility functions
function lerp(start, end, t) {
  return start + (end - start) * t;
}

function lerpColor(color1, color2, t) {
  return [
    Math.round(lerp(color1[0], color2[0], t)),
    Math.round(lerp(color1[1], color2[1], t)),
    Math.round(lerp(color1[2], color2[2], t)),
  ];
}

function getCellColor(cell, time) {
  const pulse = Math.sin(time * 0.01) * 0.5 + 0.5;  
  const fastPulse = Math.sin(time * 0.05) * 0.3 + 0.7;  
  const palette = palettes[currentPalette % palettes.length];

  // Start with base palette color
  let baseColor = lerpColor(palette.empty[0], palette.empty[1], pulse * 0.2);
  let totalIntensity = 0.3;

  // Blend ALL color memories (paths, exploration traces, etc.)
  if (cell.colorMemory && cell.colorMemory.length > 0) {
    let blendedColor = [0, 0, 0];
    let totalWeight = 0;

    for (let memory of cell.colorMemory) {
      const weight = memory.intensity;
      blendedColor[0] += memory.color[0] * weight;
      blendedColor[1] += memory.color[1] * weight;
      blendedColor[2] += memory.color[2] * weight;
      totalWeight += weight;
    }

    if (totalWeight > 0) {
      baseColor = lerpColor(baseColor, [
        blendedColor[0] / totalWeight,
        blendedColor[1] / totalWeight,
        blendedColor[2] / totalWeight,
      ], Math.min(0.8, totalWeight)); // Stronger blending for more traces
      totalIntensity = Math.min(1.0, totalIntensity + totalWeight * 0.3);
    }
  }

  let currentStateColor = baseColor;
  let activityLevel = totalIntensity;

  // Show current algorithm state with high visibility
  for (let instance of algos) {
    if (!instance.running) continue;

    if (cell === instance.start) {
      currentStateColor = [255, 255, 255]; // Bright white for start
      activityLevel = 1.0;
      break;
    } else if (cell === instance.end) {
      currentStateColor = [255, 100, 100]; // Bright red for end
      activityLevel = 1.0;
      break;
    } else if (cell === instance.current) {
      currentStateColor = [255, 255, 0]; // Bright yellow for current exploring cell
      activityLevel = fastPulse; // Fast pulsing
      break;
    } else if (instance.animatedPath && instance.animatedPath.includes(cell)) {
      // Show animated path as it builds
      const runColors = getRunColors(instance.colorIndex);
      currentStateColor = runColors.path;
      activityLevel = 0.9;
      break;
    } else if (instance.openSet.includes(cell)) {
      // Show open set with visible activity
      const runColors = getRunColors(instance.colorIndex);
      currentStateColor = lerpColor(baseColor, runColors.open, 0.6);
      activityLevel = Math.max(activityLevel, 0.7 * pulse); // Pulsing open cells
    } else if (instance.closedSet.includes(cell)) {
      // Show closed set with moderate activity
      const runColors = getRunColors(instance.colorIndex);
      currentStateColor = lerpColor(baseColor, runColors.closed, 0.4);
      activityLevel = Math.max(activityLevel, 0.5);
    } else {
      // Check if cell is a close neighbor to current (localized effect)
      if (instance.current) {
        const distance = Math.abs(cell.x - instance.current.x) + Math.abs(cell.y - instance.current.y);
        if (distance === 1) {
          // Immediate neighbors get subtle glow
          const runColors = getRunColors(instance.colorIndex);
          currentStateColor = lerpColor(baseColor, runColors.open, 0.2);
          activityLevel = Math.max(activityLevel, 0.6);
        }
      }
    }
  }

  // Handle obstacles
  if (cell.type === CELL_OBSTACLE) {
    currentStateColor = lerpColor(
      palette.obstacle[0],
      palette.obstacle[1],
      pulse * 0.3,
    );
    activityLevel = 0.8;
  }

  return [
    Math.round(currentStateColor[0] * activityLevel),
    Math.round(currentStateColor[1] * activityLevel),
    Math.round(currentStateColor[2] * activityLevel),
  ];
}

// 🥾 Boot
function boot({ screen }) {
  screenWidth = screen.width;
  screenHeight = screen.height;

  currentPalette = Math.floor(Math.random() * 6);
  currentDensity = 0; // Always start with highest density (native pixel density)

  initializeGrid(screen.width, screen.height);
  optimize();
  newScenario();
}

// 🎨 Paint
function paint({ wipe, ink, box, write, screen, paste }) {
  // Use palette-specific background color
  const palette = palettes[currentPalette % palettes.length];
  const bg = palette.background;
  wipe(bg[0], bg[1], bg[2]);

  const currentTime = Date.now();

  // Safety check for grid state
  if (!grid || grid.length === 0 || GRID_WIDTH <= 0 || GRID_HEIGHT <= 0) {
    // Draw a simple message instead of black screen
    ink(100, 100, 100);
    write("Initializing...", screen.width / 2 - 50, screen.height / 2);
    return;
  }

  // Non-uniform box rendering for aesthetic grid variety
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      const rect = getCellRect(x, y);

      // Get animated color for this cell
      const color = getCellColor(cell, currentTime);
      ink(color[0], color[1], color[2]);

      // Draw cell with non-uniform size (may be thick/thin)
      box(rect.x, rect.y, rect.w, rect.h);
    }
  }
}

// 🎪 Act
function act({ event: e, screen }) {
  if (e.is("pointerdown") || e.is("click") || e.is("tap")) {
    // Reset everything and create a new board
    runs = 0;
    algos = [];
    paths = [];
    stepCounter = 0;
    boardTimer = 0;
    resolutionTimer = 0;
    iterationTimer = 0; // Reset iteration timer
    
    // Change to a new random palette and density
    currentPalette = Math.floor(Math.random() * palettes.length);
    currentDensity = 0; // Always start with highest density (native pixel density)
    
    // Reinitialize with new settings
    initializeGrid(screen.width, screen.height);
    optimize();
    newScenario();
  }
}

// 🧮 Sim
function sim() {
  // Safety check: ensure grid exists before running algorithm
  if (!grid || grid.length === 0 || GRID_WIDTH <= 0 || GRID_HEIGHT <= 0) {
    console.warn("Invalid grid state, restarting...");
    // Force a restart with fallback values
    screenWidth = screenWidth || 800;
    screenHeight = screenHeight || 600;
    currentPalette = Math.floor(Math.random() * 6);
    currentDensity = 0; // Always start with highest density (native pixel density)
    iterationTimer = 0; // Reset iteration timer on safety restart
    initializeGrid(screenWidth, screenHeight);
    optimize();
    newScenario();
    return;
  }

  // Age color memories for dynamic fading
  ageColorMemories();

  // Frequently vary cell sizes for dynamic wavy visual effect!
  if (Math.random() < 0.02) { // 2% chance per frame for frequent waviness
    varyColumnWidths();
  }
  if (Math.random() < 0.02) { // 2% chance per frame for frequent waviness
    varyRowHeights();
  }

  // Dynamic board manipulation over time - much slower and more stable!
  boardTimer++;
  if (boardTimer >= boardInterval) {
    boardTimer = 0;
    // Much lower chance of board manipulation (25% chance) for more stability
    if (Math.random() < 0.25) {
      mutateBoard();
    }
  }

  // Separate timer for resolution changes - more frequent to make grid size changes more noticeable
  resolutionTimer++;
  if (resolutionTimer >= resolutionInterval) {
    resolutionTimer = 0;
    // Higher chance (60%) for resolution changes since they're more visible and less "watery"
    if (Math.random() < 0.6) {
      mutateResolution();
    }
  }

  // Timer for full iterations - reset everything every 2-3 seconds with new resolution
  iterationTimer++;
  if (iterationTimer >= iterationInterval) {
    // Vary the interval slightly for 2-3 second range
    iterationInterval = 120 + Math.floor(Math.random() * 60); // 2-3 seconds (120-180 frames)
    autoRestart(); // Full reset with new resolution
    return; // Exit early since we just reset everything
  }

  // Check if any algorithms are running
  const anyRunning = algos.some((instance) => instance.running);
  const allComplete =
    algos.length > 0 && algos.every((instance) => instance.complete);

  if (anyRunning) {
    for (let instance of algos) {
      if (instance.running && !instance.complete) {
        instance.stepCounter++;
        if (instance.stepCounter >= STEP_DELAY) {
          const totalCells = GRID_WIDTH * GRID_HEIGHT;
          // Run multiple steps per frame for maximum speed
          const stepsPerFrame = totalCells > 4000 ? 8 : 15;
          for (let i = 0; i < stepsPerFrame && !instance.done; i++) {
            if (totalCells > 4000) {
              stepOne(instance);
            } else {
              step(instance);
            }
          }
          instance.stepCounter = 0;
        }
      }
    }
  } else if (allComplete) {
    // Animate completed paths
    for (let instance of algos) {
      if (instance.complete && instance.path.length > 0) {
        // Very fast path animation for maximum speed
        const animSpeed = Math.max(5.0, instance.path.length * 0.3); // Scale with path length for instant reveal
        instance.pathProgress += animSpeed;
        const targetLength = Math.min(instance.path.length, Math.floor(instance.pathProgress));
        instance.animatedPath = instance.path.slice(0, targetLength);
        
        // Path animation complete
        if (targetLength >= instance.path.length) {
          instance.pathProgress = instance.path.length;
        }
      }
    }
    
    // Start countdown to restart when all paths are fully animated
    const allPathsAnimated = algos.every(instance => 
      !instance.complete || instance.pathProgress >= instance.path.length
    );
    
    if (allPathsAnimated) {
      newScenario(); // Quick restart with same grid - full resets handled by iteration timer
    }
  } else if (algos.length === 0) {
    newScenario();
  }
}

// Dynamic board manipulation functions - like a spreadsheet


// Initialize uniform sizes, but can be varied later
function initCellSizes() {
  cellWidths = new Array(GRID_WIDTH).fill(1);
  cellHeights = new Array(GRID_HEIGHT).fill(1);
  
  // Sometimes add aesthetic variations during init
  if (Math.random() < 0.3) {
    varyColumnWidths();
  }
  if (Math.random() < 0.3) {
    varyRowHeights();
  }
}

// Add aesthetic variations to column widths
function varyColumnWidths() {
  const baseWidth = screenWidth / GRID_WIDTH;
  for (let x = 0; x < GRID_WIDTH; x++) {
    // Random thickness between 0.3x and 2.5x
    const variation = 0.3 + Math.random() * 2.2;
    cellWidths[x] = variation;
  }
  // Normalize so total width still fits screen
  const totalWidth = cellWidths.reduce((sum, w) => sum + w, 0);
  const scale = GRID_WIDTH / totalWidth;
  for (let x = 0; x < GRID_WIDTH; x++) {
    cellWidths[x] *= scale;
  }
}

// Add aesthetic variations to row heights
function varyRowHeights() {
  const baseHeight = screenHeight / GRID_HEIGHT;
  for (let y = 0; y < GRID_HEIGHT; y++) {
    // Random thickness between 0.3x and 2.5x
    const variation = 0.3 + Math.random() * 2.2;
    cellHeights[y] = variation;
  }
  // Normalize so total height still fits screen
  const totalHeight = cellHeights.reduce((sum, h) => sum + h, 0);
  const scale = GRID_HEIGHT / totalHeight;
  for (let y = 0; y < GRID_HEIGHT; y++) {
    cellHeights[y] *= scale;
  }
}

// Shift all content in a column by offset (with wrapping)
function shiftColumn(colIndex, offset) {
  if (colIndex < 0 || colIndex >= GRID_WIDTH) return false;
  
  // Extract the column data (everything except coordinates)
  const columnData = [];
  for (let y = 0; y < GRID_HEIGHT; y++) {
    const cell = grid[y][colIndex];
    columnData.push({
      type: cell.type,
      colorMemory: [...cell.colorMemory],
      pathHistory: [...cell.pathHistory],
      visitHistory: [...cell.visitHistory],
      g: cell.g,
      h: cell.h,
      f: cell.f
    });
  }
  
  // Shift the data with wrapping
  for (let y = 0; y < GRID_HEIGHT; y++) {
    const sourceIndex = (y - offset + GRID_HEIGHT) % GRID_HEIGHT;
    const cell = grid[y][colIndex];
    const sourceData = columnData[sourceIndex];
    
    cell.type = sourceData.type;
    cell.colorMemory = sourceData.colorMemory;
    cell.pathHistory = sourceData.pathHistory;
    cell.visitHistory = sourceData.visitHistory;
    cell.g = sourceData.g;
    cell.h = sourceData.h;
    cell.f = sourceData.f;
    // Keep parent as null to avoid circular references
    cell.parent = null;
  }
  
  updateAlgos();
  return true;
}

// Shift all content in a row by offset (with wrapping)
function shiftRow(rowIndex, offset) {
  if (rowIndex < 0 || rowIndex >= GRID_HEIGHT) return false;
  
  // Extract the row data (everything except coordinates)
  const rowData = [];
  for (let x = 0; x < GRID_WIDTH; x++) {
    const cell = grid[rowIndex][x];
    rowData.push({
      type: cell.type,
      colorMemory: [...cell.colorMemory],
      pathHistory: [...cell.pathHistory],
      visitHistory: [...cell.visitHistory],
      g: cell.g,
      h: cell.h,
      f: cell.f
    });
  }
  
  // Shift the data with wrapping
  for (let x = 0; x < GRID_WIDTH; x++) {
    const sourceIndex = (x - offset + GRID_WIDTH) % GRID_WIDTH;
    const cell = grid[rowIndex][x];
    const sourceData = rowData[sourceIndex];
    
    cell.type = sourceData.type;
    cell.colorMemory = sourceData.colorMemory;
    cell.pathHistory = sourceData.pathHistory;
    cell.visitHistory = sourceData.visitHistory;
    cell.g = sourceData.g;
    cell.h = sourceData.h;
    cell.f = sourceData.f;
    // Keep parent as null to avoid circular references
    cell.parent = null;
  }
  
  updateAlgos();
  return true;
}

// Add a single square to the grid (expand grid size)
function addSquareGrid() {
  if (GRID_WIDTH >= 200 || GRID_HEIGHT >= 200) return false;
  
  // Randomly choose to expand width or height
  if (Math.random() < 0.5) {
    // Add to width
    const newCol = [];
    for (let y = 0; y < GRID_HEIGHT; y++) {
      const newCell = new Node(GRID_WIDTH, y);
      if (Math.random() < 0.1) newCell.type = CELL_OBSTACLE;
      newCol.push(newCell);
    }
    
    // Add column to grid
    for (let y = 0; y < GRID_HEIGHT; y++) {
      grid[y].push(newCol[y]);
    }
    
    GRID_WIDTH++;
    cellWidths.push(1); // New column gets standard width
  } else {
    // Add to height
    const newRow = [];
    for (let x = 0; x < GRID_WIDTH; x++) {
      const newCell = new Node(x, GRID_HEIGHT);
      if (Math.random() < 0.1) newCell.type = CELL_OBSTACLE;
      newRow.push(newCell);
    }
    
    grid.push(newRow);
    GRID_HEIGHT++;
    cellHeights.push(1); // New row gets standard height
  }
  
  updateAlgos();
  return true;
}

// Remove a single square from the grid (shrink grid size)
function removeSquareGrid() {
  if (GRID_WIDTH <= 5 || GRID_HEIGHT <= 5) return false;
  
  // Randomly choose to shrink width or height
  if (Math.random() < 0.5 && GRID_WIDTH > 5) {
    // Remove from width (remove last column)
    for (let y = 0; y < GRID_HEIGHT; y++) {
      grid[y].pop();
    }
    GRID_WIDTH--;
    cellWidths.pop();
  } else if (GRID_HEIGHT > 5) {
    // Remove from height (remove last row)
    grid.pop();
    GRID_HEIGHT--;
    cellHeights.pop();
  }
  
  updateAlgos();
  return true;
}

// Dynamic board manipulation functions - like a spreadsheet
function clearRow(rowIndex) {
  if (rowIndex < 0 || rowIndex >= GRID_HEIGHT) return false;

  // Clear the row and set to empty
  for (let x = 0; x < GRID_WIDTH; x++) {
    const cell = grid[rowIndex][x];
    cell.type = CELL_EMPTY;
    cell.g = 0;
    cell.h = 0;
    cell.f = 0;
    cell.parent = null;
    // Keep color memory for visual continuity
  }

  // Update any running algorithms that reference these cells
  updateAlgos();
  return true;
}

function clearColumn(colIndex) {
  if (colIndex < 0 || colIndex >= GRID_WIDTH) return false;

  // Clear the column and set to empty
  for (let y = 0; y < GRID_HEIGHT; y++) {
    const cell = grid[y][colIndex];
    cell.type = CELL_EMPTY;
    cell.g = 0;
    cell.h = 0;
    cell.f = 0;
    cell.parent = null;
    // Keep color memory for visual continuity
  }

  // Update any running algorithms that reference these cells
  updateAlgos();
  return true;
}

function insertRowAt(rowIndex) {
  if (rowIndex < 0 || rowIndex > GRID_HEIGHT) return false;
  if (GRID_HEIGHT >= 200) return false; // Limit max height

  // Choose a source row to copy from (prefer nearby rows)
  let sourceRowIndex;
  if (rowIndex === 0) {
    // If inserting at top, copy from first existing row
    sourceRowIndex = 0;
  } else if (rowIndex >= GRID_HEIGHT) {
    // If inserting at bottom, copy from last existing row
    sourceRowIndex = GRID_HEIGHT - 1;
  } else {
    // If inserting in middle, copy from the row above
    sourceRowIndex = rowIndex - 1;
  }

  // Create new row by copying the source row
  const newRow = [];
  for (let x = 0; x < GRID_WIDTH; x++) {
    const sourceCell = grid[sourceRowIndex][x];
    const newCell = new Node(x, rowIndex);
    
    // Copy properties from source cell
    newCell.type = sourceCell.type;
    newCell.colorMemory = [...sourceCell.colorMemory]; // Deep copy color memory
    newCell.pathHistory = [...sourceCell.pathHistory];
    newCell.visitHistory = [...sourceCell.visitHistory];
    
    newRow.push(newCell);
  }

  // Insert the new row
  grid.splice(rowIndex, 0, newRow);
  GRID_HEIGHT++;

  // Update Y coordinates for all cells below the inserted row
  for (let y = rowIndex + 1; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      grid[y][x].y = y;
    }
  }

  // Recalculate cell dimensions
  CELL_HEIGHT = screenHeight / GRID_HEIGHT;

  // Copy height value from source row or use default
  let newHeight;
  if (sourceRowIndex < cellHeights.length) {
    newHeight = cellHeights[sourceRowIndex];
  } else {
    newHeight = 1; // Default height
  }
  cellHeights.splice(rowIndex, 0, newHeight);

  updateAlgos();
  return true;
}

function insertColumnAt(colIndex) {
  if (colIndex < 0 || colIndex > GRID_WIDTH) return false;
  if (GRID_WIDTH >= 200) return false; // Limit max width

  // Choose a source column to copy from (prefer nearby columns)
  let sourceColIndex;
  if (colIndex === 0) {
    // If inserting at left, copy from first existing column
    sourceColIndex = 0;
  } else if (colIndex >= GRID_WIDTH) {
    // If inserting at right, copy from last existing column
    sourceColIndex = GRID_WIDTH - 1;
  } else {
    // If inserting in middle, copy from the column to the left
    sourceColIndex = colIndex - 1;
  }

  // Insert new column in each row by copying from source column
  for (let y = 0; y < GRID_HEIGHT; y++) {
    const sourceCell = grid[y][sourceColIndex];
    const newCell = new Node(colIndex, y);
    
    // Copy properties from source cell
    newCell.type = sourceCell.type;
    newCell.colorMemory = [...sourceCell.colorMemory]; // Deep copy color memory
    newCell.pathHistory = [...sourceCell.pathHistory];
    newCell.visitHistory = [...sourceCell.visitHistory];
    
    grid[y].splice(colIndex, 0, newCell);
  }

  GRID_WIDTH++;

  // Update X coordinates for all cells to the right of the inserted column
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = colIndex + 1; x < GRID_WIDTH; x++) {
      grid[y][x].x = x;
    }
  }

  // Recalculate cell dimensions
  CELL_WIDTH = screenWidth / GRID_WIDTH;

  // Copy width value from source column or use default
  let newWidth;
  if (sourceColIndex < cellWidths.length) {
    newWidth = cellWidths[sourceColIndex];
  } else {
    newWidth = 1; // Default width
  }
  cellWidths.splice(colIndex, 0, newWidth);

  updateAlgos();
  return true;
}

function removeRow(rowIndex) {
  if (rowIndex < 0 || rowIndex >= GRID_HEIGHT) return false;
  if (GRID_HEIGHT <= 5) return false; // Keep minimum height

  // Remove the row
  grid.splice(rowIndex, 1);
  GRID_HEIGHT--;

  // Remove corresponding height value
  cellHeights.splice(rowIndex, 1);

  // Update Y coordinates for all cells below the removed row
  for (let y = rowIndex; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      grid[y][x].y = y;
    }
  }

  // Recalculate cell dimensions
  CELL_HEIGHT = screenHeight / GRID_HEIGHT;

  updateAlgos();
  return true;
}

function removeColumn(colIndex) {
  if (colIndex < 0 || colIndex >= GRID_WIDTH) return false;
  if (GRID_WIDTH <= 5) return false; // Keep minimum width

  // Remove column from each row
  for (let y = 0; y < GRID_HEIGHT; y++) {
    grid[y].splice(colIndex, 1);
  }

  GRID_WIDTH--;

  // Remove corresponding width value
  cellWidths.splice(colIndex, 1);

  // Update X coordinates for all cells to the right of the removed column
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = colIndex; x < GRID_WIDTH; x++) {
      grid[y][x].x = x;
    }
  }

  // Recalculate cell dimensions
  CELL_WIDTH = screenWidth / GRID_WIDTH;

  updateAlgos();
  return true;
}

function updateAlgos() {
  for (let instance of algos) {
    // Check if start/end positions are still valid
    if (
      instance.start &&
      (instance.start.x >= GRID_WIDTH || instance.start.y >= GRID_HEIGHT)
    ) {
      instance.running = false;
      instance.complete = true;
    }
    if (
      instance.end &&
      (instance.end.x >= GRID_WIDTH || instance.end.y >= GRID_HEIGHT)
    ) {
      instance.running = false;
      instance.complete = true;
    }

    // Clean up open and closed sets
    instance.openSet = instance.openSet.filter(
      (cell) =>
        cell.x < GRID_WIDTH &&
        cell.y < GRID_HEIGHT &&
        cell.x >= 0 &&
        cell.y >= 0,
    );
    instance.closedSet = instance.closedSet.filter(
      (cell) =>
        cell.x < GRID_WIDTH &&
        cell.y < GRID_HEIGHT &&
        cell.x >= 0 &&
        cell.y >= 0,
    );

    // Clear invalid parents to prevent circular references
    for (let y = 0; y < GRID_HEIGHT; y++) {
      for (let x = 0; x < GRID_WIDTH; x++) {
        const cell = grid[y][x];
        if (
          cell.parent &&
          (cell.parent.x >= GRID_WIDTH ||
            cell.parent.y >= GRID_HEIGHT ||
            cell.parent.x < 0 ||
            cell.parent.y < 0)
        ) {
          cell.parent = null;
        }
      }
    }
  }
}

function mutateResolution() {
  if (!boardActive) return;

  // Focus only on operations that change grid resolution (number of rows/columns)
  const resolutionOperations = [
    // Growth operations (make grid bigger)
    () => insertRowAt(Math.floor(Math.random() * (GRID_HEIGHT + 1))),
    () => insertColumnAt(Math.floor(Math.random() * (GRID_WIDTH + 1))),
    () => insertRowAt(Math.floor(Math.random() * (GRID_HEIGHT + 1))), // Double weight
    () => insertColumnAt(Math.floor(Math.random() * (GRID_WIDTH + 1))), // Double weight

    // Shrinking operations (make grid smaller) - less frequent to prevent tiny grids
    () => removeRow(Math.floor(Math.random() * GRID_HEIGHT)),
    () => removeColumn(Math.floor(Math.random() * GRID_WIDTH)),
  ];

  // Pick a random resolution operation
  const operation = resolutionOperations[Math.floor(Math.random() * resolutionOperations.length)];
  operation();

  // Recalculate performance settings after grid size changes
  optimize();
}

function mutateBoard() {
  if (!boardActive) return;

  // More aggressive operations - favor clearing and shifting more than resolution changes
  const operations = [
    // Clear operations (help open up paths)
    () => clearRow(Math.floor(Math.random() * GRID_HEIGHT)),
    () => clearColumn(Math.floor(Math.random() * GRID_WIDTH)),
    () => clearRow(Math.floor(Math.random() * GRID_HEIGHT)), // Double weight
    () => clearColumn(Math.floor(Math.random() * GRID_WIDTH)), // Double weight
    () => clearRow(Math.floor(Math.random() * GRID_HEIGHT)), // Triple weight
    () => clearColumn(Math.floor(Math.random() * GRID_WIDTH)), // Triple weight

    // Grid shifting operations (aesthetic feature)
    () => shiftColumn(Math.floor(Math.random() * GRID_WIDTH), Math.floor(Math.random() * 5) - 2),
    () => shiftRow(Math.floor(Math.random() * GRID_HEIGHT), Math.floor(Math.random() * 5) - 2),
    () => shiftColumn(Math.floor(Math.random() * GRID_WIDTH), Math.floor(Math.random() * 5) - 2), // Double weight
    () => shiftRow(Math.floor(Math.random() * GRID_HEIGHT), Math.floor(Math.random() * 5) - 2), // Double weight

    // Single square operations (precise manipulation)
    () => addSquare(Math.floor(Math.random() * GRID_WIDTH), Math.floor(Math.random() * GRID_HEIGHT), 'obstacle'),
    () => removeSquare(Math.floor(Math.random() * GRID_WIDTH), Math.floor(Math.random() * GRID_HEIGHT)),
    () => addSquare(Math.floor(Math.random() * GRID_WIDTH), Math.floor(Math.random() * GRID_HEIGHT), 'empty'),

    // Add some obstacles dynamically
    () => addWalls(),
    () => addWalls(),
  ];

  // Pick a random operation
  const operation = operations[Math.floor(Math.random() * operations.length)];
  operation();

  // Recalculate performance settings after grid size changes
  optimize();
}

function addWalls() {
  // Add a few random obstacles to the current board
  const numObstacles = Math.floor(Math.random() * 5) + 1; // 1-5 obstacles

  for (let i = 0; i < numObstacles; i++) {
    const x = Math.floor(Math.random() * GRID_WIDTH);
    const y = Math.floor(Math.random() * GRID_HEIGHT);
    const cell = grid[y][x];

    // Only place obstacle if cell is empty and not being used by algorithms
    if (cell.type === CELL_EMPTY) {
      let isInUse = false;
      for (let instance of algos) {
        if (
          cell === instance.start ||
          cell === instance.end ||
          instance.openSet.includes(cell) ||
          instance.closedSet.includes(cell)
        ) {
          isInUse = true;
          break;
        }
      }

      if (!isInUse) {
        cell.type = CELL_OBSTACLE;
      }
    }
  }
}
