// A* Pathfinding, 2025.7.12
// Animated visualization of the A* pathfinding algorithm

/* 📝 Engineering Notes
  Automatically generates a random maze with start/end points.
  Watch the A* algorithm find the optimal path step by step.
  The visualization automatically restarts with a new scenario.
*/

let grid = [];
let openSet = [];
let closedSet = [];
let path = [];
let current = null;
let start = null;
let end = null;
let algorithmRunning = false;
let algorithmComplete = false;
let currentStep = 0;
let animationDelay = 0;
let restartTimer = 0;

let CELL_SIZE = 16;
let GRID_WIDTH, GRID_HEIGHT;
let UI_MARGIN = 0; // No UI text needed
let STEP_DELAY = 1; // Frames between algorithm steps (fastest animation)
let stepCounter = 0;
let resetCountdown = 0;
let RESET_DELAY = 60; // Frames to wait before reset (1 second at 60fps)

// Animation variables
let animationProgress = 0;
let lastStepTime = 0;

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
  }

  // Calculate heuristic using Manhattan distance
  calculateHeuristic(end) {
    this.h = Math.abs(this.x - end.x) + Math.abs(this.y - end.y);
  }

  // Get neighbors
  getNeighbors() {
    const neighbors = [];
    const directions = [
      { x: -1, y: 0 }, { x: 1, y: 0 },
      { x: 0, y: -1 }, { x: 0, y: 1 },
      // Diagonal movement (optional)
      { x: -1, y: -1 }, { x: 1, y: -1 },
      { x: -1, y: 1 }, { x: 1, y: 1 }
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

function reconstructPath(node) {
  path = [];
  let current = node;
  while (current) {
    path.push(current);
    current = current.parent;
  }
  path.reverse();
}

function removeFromArray(arr, element) {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i] === element) {
      arr.splice(i, 1);
      return;
    }
  }
}

function initializeGrid(screenWidth, screenHeight) {
  // Use full screen width and height
  const availableWidth = screenWidth;
  const availableHeight = screenHeight;
  
  // Calculate grid dimensions to fit the screen perfectly
  GRID_WIDTH = Math.max(30, Math.floor(availableWidth / 12));
  GRID_HEIGHT = Math.max(20, Math.floor(availableHeight / 12));
  
  // Calculate cell size to perfectly fill the screen
  CELL_SIZE = Math.min(
    Math.floor(availableWidth / GRID_WIDTH),
    Math.floor(availableHeight / GRID_HEIGHT)
  );
  
  grid = [];
  for (let y = 0; y < GRID_HEIGHT; y++) {
    grid[y] = [];
    for (let x = 0; x < GRID_WIDTH; x++) {
      grid[y][x] = new Node(x, y);
    }
  }
}

function generateRandomScenario() {
  // Clear all cells first
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      grid[y][x].type = CELL_EMPTY;
      grid[y][x].g = 0;
      grid[y][x].h = 0;
      grid[y][x].f = 0;
      grid[y][x].parent = null;
    }
  }
  
  // Place start and end points
  start = grid[Math.floor(GRID_HEIGHT * 0.2)][Math.floor(GRID_WIDTH * 0.1)];
  end = grid[Math.floor(GRID_HEIGHT * 0.8)][Math.floor(GRID_WIDTH * 0.9)];
  
  start.type = CELL_START;
  end.type = CELL_END;
  
  // Generate random obstacles (about 25% of the grid)
  const obstacleCount = Math.floor(GRID_WIDTH * GRID_HEIGHT * 0.25);
  let placed = 0;
  
  while (placed < obstacleCount) {
    const x = Math.floor(Math.random() * GRID_WIDTH);
    const y = Math.floor(Math.random() * GRID_HEIGHT);
    const cell = grid[y][x];
    
    if (cell.type === CELL_EMPTY && cell !== start && cell !== end) {
      // Don't block the immediate area around start and end
      const distToStart = Math.abs(x - start.x) + Math.abs(y - start.y);
      const distToEnd = Math.abs(x - end.x) + Math.abs(y - end.y);
      
      if (distToStart > 2 && distToEnd > 2) {
        cell.type = CELL_OBSTACLE;
        placed++;
      }
    }
  }
}

function resetAlgorithm() {
  openSet = [];
  closedSet = [];
  path = [];
  current = null;
  algorithmRunning = false;
  algorithmComplete = false;
  currentStep = 0;
  stepCounter = 0;
  resetCountdown = 0;
  
  // Clear algorithm-specific cell types
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      if (cell.type === CELL_OPEN || cell.type === CELL_CLOSED || cell.type === CELL_PATH) {
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
  if (!start || !end) return;
  
  resetAlgorithm();
  algorithmRunning = true;
  
  openSet.push(start);
  start.calculateHeuristic(end);
  start.f = start.h;
}

function autoRestart() {
  generateRandomScenario();
  resetAlgorithm();
  startAlgorithm();
}

function stepAlgorithm() {
  if (!algorithmRunning || algorithmComplete) return;
  
  if (openSet.length === 0) {
    algorithmRunning = false;
    algorithmComplete = true;
    return; // No solution
  }
  
  // Find node with lowest f score
  let winner = 0;
  for (let i = 1; i < openSet.length; i++) {
    if (openSet[i].f < openSet[winner].f) {
      winner = i;
    }
  }
  
  current = openSet[winner];
  
  // Check if we reached the goal
  if (current === end) {
    reconstructPath(current);
    algorithmRunning = false;
    algorithmComplete = true;
    return;
  }
  
  // Move current from open to closed set
  removeFromArray(openSet, current);
  closedSet.push(current);
  
  // Check all neighbors
  const neighbors = current.getNeighbors();
  
  for (let neighbor of neighbors) {
    // Skip if neighbor is in closed set
    if (closedSet.includes(neighbor)) continue;
    
    // Calculate tentative g score
    const isDiagonal = Math.abs(neighbor.x - current.x) === 1 && Math.abs(neighbor.y - current.y) === 1;
    const tentativeG = current.g + (isDiagonal ? 1.414 : 1); // √2 for diagonal movement
    
    // If this path to neighbor is better than any previous one
    if (!openSet.includes(neighbor)) {
      openSet.push(neighbor);
    } else if (tentativeG >= neighbor.g) {
      continue; // This is not a better path
    }
    
    // This path is the best until now. Record it!
    neighbor.parent = current;
    neighbor.g = tentativeG;
    neighbor.calculateHeuristic(end);
    neighbor.f = neighbor.g + neighbor.h;
  }
  
  currentStep++;
  lastStepTime = Date.now();
}

// Color utility functions
function lerp(start, end, t) {
  return start + (end - start) * t;
}

function lerpColor(color1, color2, t) {
  return [
    Math.round(lerp(color1[0], color2[0], t)),
    Math.round(lerp(color1[1], color2[1], t)),
    Math.round(lerp(color1[2], color2[2], t))
  ];
}

function getCellColor(cell, time) {
  const pulse = Math.sin(time * 0.005) * 0.5 + 0.5;
  const fastPulse = Math.sin(time * 0.02) * 0.3 + 0.7;
  
  if (cell.type === CELL_OBSTACLE) {
    // Dark purple-gray with subtle animation
    const base = [60, 40, 80];
    const highlight = [80, 60, 120];
    return lerpColor(base, highlight, pulse * 0.3);
  } else if (cell.type === CELL_START || cell === start) {
    // Bright cyan-green with pulse
    const base = [0, 255, 150];
    const highlight = [100, 255, 200];
    return lerpColor(base, highlight, pulse);
  } else if (cell.type === CELL_END || cell === end) {
    // Bright magenta-red with pulse
    const base = [255, 50, 150];
    const highlight = [255, 100, 200];
    return lerpColor(base, highlight, pulse);
  } else if (path.includes(cell)) {
    // Golden yellow path with sparkle effect
    const base = [255, 215, 0];
    const highlight = [255, 255, 100];
    return lerpColor(base, highlight, fastPulse);
  } else if (closedSet.includes(cell)) {
    // Deep blue-purple for explored cells
    const base = [80, 60, 180];
    const highlight = [120, 100, 220];
    return lerpColor(base, highlight, pulse * 0.5);
  } else if (openSet.includes(cell)) {
    // Bright teal for frontier cells with animation
    const base = [0, 200, 200];
    const highlight = [50, 255, 255];
    return lerpColor(base, highlight, fastPulse);
  } else if (cell === current) {
    // Bright white-yellow for current cell
    const base = [255, 255, 150];
    const highlight = [255, 255, 255];
    return lerpColor(base, highlight, fastPulse);
  } else {
    // Dark background cells with very subtle variation
    const base = [20, 20, 25];
    const highlight = [30, 30, 40];
    return lerpColor(base, highlight, pulse * 0.1);
  }
}

// 🥾 Boot
function boot({ screen }) {
  initializeGrid(screen.width, screen.height);
  generateRandomScenario();
  startAlgorithm();
}

// 🎨 Paint
function paint({ wipe, ink, box, write, screen }) {
  wipe(10, 10, 15); // Very dark background
  
  const currentTime = Date.now();
  
  // Draw grid stretching to fill entire screen
  for (let y = 0; y < GRID_HEIGHT; y++) {
    for (let x = 0; x < GRID_WIDTH; x++) {
      const cell = grid[y][x];
      const drawX = x * CELL_SIZE;
      const drawY = y * CELL_SIZE;
      
      // Get animated color for this cell
      const color = getCellColor(cell, currentTime);
      ink(color[0], color[1], color[2]);
      
      // Draw cell as a full colored box with no border
      box(drawX, drawY, CELL_SIZE, CELL_SIZE);
    }
  }
}

// 🎪 Act
function act({ event: e, screen }) {
  // Removed interactive controls - now fully automated
}

// 🧮 Sim
function sim() {
  // Step through the algorithm automatically when running (with delay for slower animation)
  if (algorithmRunning && !algorithmComplete) {
    stepCounter++;
    if (stepCounter >= STEP_DELAY) {
      stepAlgorithm();
      stepCounter = 0;
    }
  } else if (algorithmComplete) {
    // Start countdown to restart
    resetCountdown++;
    if (resetCountdown >= RESET_DELAY) {
      autoRestart();
    }
  }
}

// 📰 Meta
function meta() {
  return {
    title: "A* Pathfinding",
    desc: "Animated visualization of the A* pathfinding algorithm. Watch as it automatically finds optimal paths through randomly generated mazes.",
  };
}
