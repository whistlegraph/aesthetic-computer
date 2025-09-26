// gameoflife.mjs - Conway's Game of Life with pixel manipulation
// For recording gifs and mp4s with the orchestrator 

const { floor, random } = Math;

let time = 0;
let cellSize = 4; // Each cell is 4x4 pixels
let grid = null;
let nextGrid = null;
let width = 0;
let height = 0;
let generation = 0;

// Color schemes that cycle over time
const colorSchemes = [
  { alive: [0, 255, 0], dead: [0, 20, 0] },       // Green matrix
  { alive: [255, 255, 255], dead: [0, 0, 0] },   // Classic B&W
  { alive: [255, 100, 0], dead: [20, 5, 0] },    // Fire
  { alive: [0, 150, 255], dead: [0, 10, 30] },   // Blue
  { alive: [255, 0, 255], dead: [30, 0, 30] },   // Magenta
];

function paint({ api, frameIndex = 0, frameTime = 0, simCount = 0n }) {
  const { screen } = api;
  time += 0.02;
  
  // Initialize grid on first frame
  if (!grid) {
    initializeGrid(screen);
  }
  
  // Update game of life every few frames for visibility
  if (frameIndex % 3 === 0) {
    updateGameOfLife();
    generation++;
    
    // Reset with new random pattern every 200 generations
    if (generation % 200 === 0) {
      seedRandomPattern();
    }
  }
  
  // Choose color scheme based on time
  const schemeIndex = floor(time * 0.1) % colorSchemes.length;
  const scheme = colorSchemes[schemeIndex];
  
  // Render the grid to pixels
  renderGridToPixels(screen, scheme);
  
  // Add some sparkle effects
  addSparkleEffects(screen, time);
}

function initializeGrid(screen) {
  width = floor(screen.width / cellSize);
  height = floor(screen.height / cellSize);
  
  grid = new Array(height);
  nextGrid = new Array(height);
  
  for (let y = 0; y < height; y++) {
    grid[y] = new Array(width);
    nextGrid[y] = new Array(width);
    for (let x = 0; x < width; x++) {
      grid[y][x] = false;
      nextGrid[y][x] = false;
    }
  }
  
  // Seed with some interesting patterns
  seedInitialPatterns();
}

function seedInitialPatterns() {
  // Clear grid
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      grid[y][x] = false;
    }
  }
  
  // Add glider guns
  addGliderGun(10, 10);
  if (width > 100) addGliderGun(width - 50, 10);
  if (height > 100) addGliderGun(10, height - 50);
  
  // Add some random noise
  for (let i = 0; i < width * height * 0.1; i++) {
    const x = floor(random() * width);
    const y = floor(random() * height);
    grid[y][x] = true;
  }
}

function seedRandomPattern() {
  // Create interesting random patterns
  const patternType = floor(random() * 3);
  
  // Clear grid first
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      grid[y][x] = false;
    }
  }
  
  if (patternType === 0) {
    // Random soup
    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) {
        grid[y][x] = random() < 0.4;
      }
    }
  } else if (patternType === 1) {
    // Centered explosion
    const centerX = floor(width / 2);
    const centerY = floor(height / 2);
    const radius = floor(Math.min(width, height) / 4);
    
    for (let y = centerY - radius; y < centerY + radius; y++) {
      for (let x = centerX - radius; x < centerX + radius; x++) {
        if (x >= 0 && x < width && y >= 0 && y < height) {
          const dist = Math.sqrt((x - centerX) ** 2 + (y - centerY) ** 2);
          grid[y][x] = random() < 0.6 && dist < radius;
        }
      }
    }
  } else {
    // Vertical stripes with noise
    for (let x = 0; x < width; x += 8) {
      for (let y = 0; y < height; y++) {
        if (x < width) {
          grid[y][x] = random() < 0.7;
          if (x + 1 < width) grid[y][x + 1] = random() < 0.7;
        }
      }
    }
  }
}

function addGliderGun(startX, startY) {
  // Gosper glider gun pattern
  const pattern = [
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1],
    [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ];
  
  for (let y = 0; y < pattern.length; y++) {
    for (let x = 0; x < pattern[y].length; x++) {
      const gridX = startX + x;
      const gridY = startY + y;
      if (gridX < width && gridY < height && gridX >= 0 && gridY >= 0) {
        grid[gridY][gridX] = pattern[y][x] === 1;
      }
    }
  }
}

function updateGameOfLife() {
  // Calculate next generation
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const neighbors = countNeighbors(x, y);
      const isAlive = grid[y][x];
      
      if (isAlive) {
        // Live cell survives with 2 or 3 neighbors
        nextGrid[y][x] = neighbors === 2 || neighbors === 3;
      } else {
        // Dead cell becomes alive with exactly 3 neighbors
        nextGrid[y][x] = neighbors === 3;
      }
    }
  }
  
  // Swap grids
  const temp = grid;
  grid = nextGrid;
  nextGrid = temp;
}

function countNeighbors(x, y) {
  let count = 0;
  for (let dy = -1; dy <= 1; dy++) {
    for (let dx = -1; dx <= 1; dx++) {
      if (dx === 0 && dy === 0) continue; // Skip self
      
      const nx = x + dx;
      const ny = y + dy;
      
      // Wrap around edges for toroidal topology
      const wrappedX = (nx + width) % width;
      const wrappedY = (ny + height) % height;
      
      if (grid[wrappedY][wrappedX]) {
        count++;
      }
    }
  }
  return count;
}

function renderGridToPixels(screen, colorScheme) {
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const isAlive = grid[y][x];
      const color = isAlive ? colorScheme.alive : colorScheme.dead;
      
      // Fill the cell area
      for (let py = 0; py < cellSize; py++) {
        for (let px = 0; px < cellSize; px++) {
          const screenX = x * cellSize + px;
          const screenY = y * cellSize + py;
          
          if (screenX < screen.width && screenY < screen.height) {
            const pixelIndex = (screenY * screen.width + screenX) * 4;
            screen.pixels[pixelIndex] = color[0];     // R
            screen.pixels[pixelIndex + 1] = color[1]; // G
            screen.pixels[pixelIndex + 2] = color[2]; // B
            screen.pixels[pixelIndex + 3] = 255;      // A
          }
        }
      }
    }
  }
}

function addSparkleEffects(screen, time) {
  // Add occasional sparkles on living cells
  const sparkleCount = 20;
  
  for (let i = 0; i < sparkleCount; i++) {
    const x = floor(random() * width);
    const y = floor(random() * height);
    
    if (grid[y][x]) { // Only sparkle on living cells
      const sparkleX = x * cellSize + floor(random() * cellSize);
      const sparkleY = y * cellSize + floor(random() * cellSize);
      
      if (sparkleX < screen.width && sparkleY < screen.height) {
        const pixelIndex = (sparkleY * screen.width + sparkleX) * 4;
        const intensity = 128 + floor(127 * Math.sin(time * 5 + i));
        
        screen.pixels[pixelIndex] = 255;     // R
        screen.pixels[pixelIndex + 1] = 255; // G
        screen.pixels[pixelIndex + 2] = 255; // B
        screen.pixels[pixelIndex + 3] = intensity; // A
      }
    }
  }
}

function act({ event, api }) {
  // Reset with new pattern on spacebar
  if (event.key === " " || event.key === "Spacebar") {
    seedRandomPattern();
    generation = 0;
  }
  
  // Adjust cell size
  if (event.key === "ArrowUp") {
    cellSize = Math.min(8, cellSize + 1);
    initializeGrid(api.screen);
  }
  if (event.key === "ArrowDown") {
    cellSize = Math.max(2, cellSize - 1);
    initializeGrid(api.screen);
  }
}

// Export the piece
export { paint, act };