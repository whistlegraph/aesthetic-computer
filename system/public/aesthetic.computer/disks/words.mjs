// Words, 2026.01.20
// A Word Munchers-style educational word game.

/* #region 📚 README 
  Classic Word Munchers gameplay!
  - Grid shows whole words
  - A category/criteria is shown at top (rhymes, synonyms, endings, etc.)
  - Move muncher with arrow keys or tap adjacent cells
  - Press SPACE or tap your cell to munch
  - Munch all words that match the criteria
  - Wrong munch = lose a life!
  - Clear all correct words to win
#endregion */

/* #region 🏁 TODO 
  - [] Add sound effects for munching
  - [] Add enemy "troggles" that roam the grid
  - [] Add more category types
  - [] Add difficulty progression
#endregion */

// Category definitions with correct/incorrect words
const CATEGORIES = [
  {
    type: "rhymes",
    prompt: "Rhymes with CAT",
    correct: ["bat", "hat", "mat", "rat", "sat", "flat", "chat", "that"],
    wrong: ["dog", "cup", "tree", "book", "run", "big", "sky", "lamp", "frog", "ship"],
  },
  {
    type: "rhymes",
    prompt: "Rhymes with TREE",
    correct: ["bee", "see", "free", "knee", "key", "flea", "plea", "glee"],
    wrong: ["cat", "dog", "run", "hat", "book", "lamp", "frog", "ship", "cup", "map"],
  },
  {
    type: "ending",
    prompt: "Ends with -ING",
    correct: ["sing", "ring", "king", "wing", "bring", "thing", "swing", "spring"],
    wrong: ["cat", "dog", "tree", "book", "lamp", "frog", "ship", "cup", "hat", "run"],
  },
  {
    type: "ending",
    prompt: "Ends with -TION",
    correct: ["action", "nation", "motion", "notion", "potion", "ration", "station", "option"],
    wrong: ["cat", "running", "happy", "tree", "book", "lamp", "quick", "ship", "bright", "dog"],
  },
  {
    type: "starting",
    prompt: "Starts with UN-",
    correct: ["undo", "unfit", "unfair", "unlock", "unseen", "untie", "unpack", "unreal"],
    wrong: ["cat", "over", "happy", "tree", "only", "under", "lamp", "open", "upper", "dog"],
  },
  {
    type: "synonym",
    prompt: "Means HAPPY",
    correct: ["glad", "joyful", "merry", "cheery", "jolly", "content", "pleased", "elated"],
    wrong: ["sad", "angry", "tired", "cold", "fast", "big", "small", "dark", "loud", "wet"],
  },
  {
    type: "synonym",
    prompt: "Means BIG",
    correct: ["large", "huge", "giant", "vast", "great", "grand", "massive", "immense"],
    wrong: ["tiny", "small", "fast", "sad", "happy", "cold", "hot", "dark", "loud", "wet"],
  },
  {
    type: "antonym",
    prompt: "Opposite of HOT",
    correct: ["cold", "cool", "chilly", "frozen", "icy", "frigid", "frosty", "freezing"],
    wrong: ["warm", "burning", "fast", "big", "happy", "sad", "loud", "dark", "wet", "dry"],
  },
  {
    type: "category",
    prompt: "Animals",
    correct: ["cat", "dog", "bird", "fish", "frog", "bear", "lion", "wolf"],
    wrong: ["tree", "book", "lamp", "chair", "table", "house", "car", "phone", "shirt", "cup"],
  },
  {
    type: "category",
    prompt: "Colors",
    correct: ["red", "blue", "green", "yellow", "orange", "purple", "pink", "brown"],
    wrong: ["cat", "dog", "tree", "book", "lamp", "chair", "run", "happy", "big", "fast"],
  },
];

const GRID_COLS = 5;
const GRID_ROWS = 4;
const TOTAL_CELLS = GRID_COLS * GRID_ROWS;

let grid = []; // Array of { word, isCorrect, munched }
let category = null;
let muncher = { row: 0, col: 0 };
let lives = 3;
let score = 0;
let level = 1;
let gameState = "playing"; // "playing", "won", "lost"
let cellWidth = 38;
let cellHeight = 28;
let gridOffset = { x: 0, y: 0 };
let flashWrong = 0; // Timer for wrong munch flash
let lastMunchCorrect = true;

const shuffle = (arr) => {
  const a = [...arr];
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
};

const generateLevel = () => {
  // Pick a random category
  category = CATEGORIES[Math.floor(Math.random() * CATEGORIES.length)];

  // Decide how many correct words (scales with level, 3-6)
  const numCorrect = Math.min(3 + Math.floor(level / 2), 6);
  const numWrong = TOTAL_CELLS - numCorrect;

  // Pick words
  const correctWords = shuffle(category.correct).slice(0, numCorrect);
  const wrongWords = shuffle(category.wrong).slice(0, numWrong);

  // Build grid items
  const items = [
    ...correctWords.map((w) => ({ word: w, isCorrect: true, munched: false })),
    ...wrongWords.map((w) => ({ word: w, isCorrect: false, munched: false })),
  ];

  // Shuffle and assign to grid
  grid = shuffle(items);

  // Reset muncher to center-ish
  muncher = { row: Math.floor(GRID_ROWS / 2), col: Math.floor(GRID_COLS / 2) };
  gameState = "playing";
};

const getGridIndex = (row, col) => row * GRID_COLS + col;

const getCellAt = (row, col) => {
  if (row < 0 || row >= GRID_ROWS || col < 0 || col >= GRID_COLS) return null;
  return grid[getGridIndex(row, col)];
};

const moveMuncher = (dr, dc) => {
  if (gameState !== "playing") return;
  const newRow = muncher.row + dr;
  const newCol = muncher.col + dc;
  if (newRow >= 0 && newRow < GRID_ROWS && newCol >= 0 && newCol < GRID_COLS) {
    muncher.row = newRow;
    muncher.col = newCol;
  }
};

const munch = () => {
  if (gameState !== "playing") return;

  const cell = getCellAt(muncher.row, muncher.col);
  if (!cell || cell.munched) return;

  cell.munched = true;

  if (cell.isCorrect) {
    score += 10 * level;
    lastMunchCorrect = true;

    // Check win condition
    const remaining = grid.filter((c) => c.isCorrect && !c.munched);
    if (remaining.length === 0) {
      level += 1;
      generateLevel();
    }
  } else {
    // Wrong munch!
    lives -= 1;
    flashWrong = 30;
    lastMunchCorrect = false;

    if (lives <= 0) {
      gameState = "lost";
    }
  }
};

const getCellFromPos = (x, y) => {
  const col = Math.floor((x - gridOffset.x) / cellWidth);
  const row = Math.floor((y - gridOffset.y) / cellHeight);
  if (row < 0 || row >= GRID_ROWS || col < 0 || col >= GRID_COLS) return null;
  return { row, col };
};

// 🥾 Boot
function boot({ resize }) {
  resize(192, 144);
  generateLevel();
}

// 🎨 Paint
function paint({ wipe, ink, box, write, screen }) {
  // Flash red on wrong munch
  if (flashWrong > 0) {
    wipe(80, 20, 20);
    flashWrong -= 1;
  } else {
    wipe(20, 25, 40);
  }

  // Calculate layout
  cellWidth = Math.floor((screen.width - 4) / GRID_COLS);
  cellHeight = 24;
  gridOffset.x = Math.floor((screen.width - cellWidth * GRID_COLS) / 2);
  gridOffset.y = 28;

  // Header: Category prompt
  ink("yellow").write(category.prompt.toUpperCase(), { x: 4, y: 4 });

  // Header: Lives and Score
  ink("red").write("♥".repeat(lives), { x: screen.width - 24, y: 4 });
  ink("gray").write(`L${level}`, { x: screen.width - 50, y: 4 });

  // Draw grid
  for (let row = 0; row < GRID_ROWS; row++) {
    for (let col = 0; col < GRID_COLS; col++) {
      const idx = getGridIndex(row, col);
      const cell = grid[idx];
      const x = gridOffset.x + col * cellWidth;
      const y = gridOffset.y + row * cellHeight;

      const isMuncherHere = muncher.row === row && muncher.col === col;

      // Cell background
      if (cell.munched) {
        ink(30, 35, 50).box(x + 1, y + 1, cellWidth - 2, cellHeight - 2);
      } else if (isMuncherHere) {
        ink(80, 180, 80).box(x + 1, y + 1, cellWidth - 2, cellHeight - 2);
      } else {
        ink(50, 55, 75).box(x + 1, y + 1, cellWidth - 2, cellHeight - 2);
      }

      // Cell border
      ink(70, 75, 95).box(x, y, cellWidth, cellHeight, "outline");

      // Word text (if not munched)
      if (!cell.munched) {
        const textColor = isMuncherHere ? [20, 40, 20] : [200, 200, 220];
        ink(...textColor).write(cell.word.toUpperCase(), {
          x: x + 3,
          y: y + Math.floor(cellHeight / 2) - 3,
        });
      }
    }
  }

  // Instructions at bottom
  const instY = gridOffset.y + GRID_ROWS * cellHeight + 6;
  ink(100, 100, 120).write("ARROWS/TAP:MOVE  SPACE/TAP:MUNCH", { x: 4, y: instY });

  // Score
  ink("cyan").write(`SCORE: ${score}`, { x: 4, y: instY + 12 });

  // Game over / win messages
  if (gameState === "lost") {
    ink(0, 0, 0, 180).box(0, 0, screen.width, screen.height);
    ink("red").write("GAME OVER!", { x: 60, y: 60 });
    ink("white").write(`FINAL SCORE: ${score}`, { x: 50, y: 75 });
    ink("gray").write("TAP TO RESTART", { x: 55, y: 95 });
  }
}

// 🎪 Act
function act({ event }) {
  // Restart on game over
  if (gameState === "lost" && event.is("touch")) {
    lives = 3;
    score = 0;
    level = 1;
    generateLevel();
    return;
  }

  // Keyboard controls
  if (event.is("keyboard:down:arrowup")) moveMuncher(-1, 0);
  if (event.is("keyboard:down:arrowdown")) moveMuncher(1, 0);
  if (event.is("keyboard:down:arrowleft")) moveMuncher(0, -1);
  if (event.is("keyboard:down:arrowright")) moveMuncher(0, 1);
  if (event.is("keyboard:down:space")) munch();

  // Touch controls
  if (event.is("touch")) {
    const cell = getCellFromPos(event.x, event.y);
    if (cell) {
      // If tapping current cell, munch
      if (cell.row === muncher.row && cell.col === muncher.col) {
        munch();
      } else {
        // Move toward tapped cell (one step at a time)
        const dr = Math.sign(cell.row - muncher.row);
        const dc = Math.sign(cell.col - muncher.col);
        // Prefer vertical or horizontal
        if (dr !== 0) {
          moveMuncher(dr, 0);
        } else if (dc !== 0) {
          moveMuncher(0, dc);
        }
      }
    }
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Words",
    desc: "A Word Munchers-style educational word game.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
