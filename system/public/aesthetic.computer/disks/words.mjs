// Words, 2026.01.20
// A Word Munchers-style tap and drag word game laboratory.

/* #region 📚 README 
  Inspired by the classic Word Munchers educational game!
  - Find hidden words in the grid by dragging in straight lines
  - Words can be horizontal, vertical, or diagonal
  - Tap and drag to select letters, release to submit
#endregion */

/* #region 🏁 TODO 
  - [] Add sound effects for munching
  - [] Add scoring system
  - [] Add levels with different word categories
  - [] Add timer mode
#endregion */

const DEFAULT_WORDS = [
  "munch",
  "arcade",
  "vector",
  "sprite",
  "laser",
  "maze",
  "retro",
  "byte",
  "pixel",
  "sound",
  "bonus",
  "math",
  "orbit",
  "shield",
  "power",
  "score",
  "combo",
  "quest",
  "grid",
  "logic",
];

const DIRECTIONS = [
  [0, 1],
  [1, 0],
  [0, -1],
  [-1, 0],
  [1, 1],
  [1, -1],
  [-1, 1],
  [-1, -1],
];

let puzzle = null;
let selection = null;
let cellSize = 16;
let gridOffset = { x: 0, y: 0 };
let maxWords = 8;
let words = [...DEFAULT_WORDS];
let found = new Set();
let gridSize = 10;

const shuffle = (list) => {
  const arr = [...list];
  for (let i = arr.length - 1; i > 0; i -= 1) {
    const j = Math.floor(Math.random() * (i + 1));
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
  return arr;
};

const pickWords = () => {
  const trimmed = words.map((w) => w.trim().toLowerCase()).filter(Boolean);
  return shuffle(trimmed).slice(0, maxWords);
};

const buildEmptyGrid = (size) =>
  Array.from({ length: size }, () => Array.from({ length: size }, () => null));

const placeWord = (grid, word) => {
  const size = grid.length;
  const letters = [...word];
  const attempts = 150;

  for (let tries = 0; tries < attempts; tries += 1) {
    const [dr, dc] = DIRECTIONS[Math.floor(Math.random() * DIRECTIONS.length)];
    const startRow = Math.floor(Math.random() * size);
    const startCol = Math.floor(Math.random() * size);

    const endRow = startRow + dr * (letters.length - 1);
    const endCol = startCol + dc * (letters.length - 1);

    if (endRow < 0 || endRow >= size || endCol < 0 || endCol >= size) continue;

    let canPlace = true;
    for (let i = 0; i < letters.length; i += 1) {
      const row = startRow + dr * i;
      const col = startCol + dc * i;
      const cell = grid[row][col];
      if (cell && cell !== letters[i]) {
        canPlace = false;
        break;
      }
    }

    if (!canPlace) continue;

    for (let i = 0; i < letters.length; i += 1) {
      const row = startRow + dr * i;
      const col = startCol + dc * i;
      grid[row][col] = letters[i];
    }

    return { placed: true, startRow, startCol, dr, dc };
  }

  return { placed: false };
};

const fillGrid = (grid) => {
  const size = grid.length;
  const alphabet = "abcdefghijklmnopqrstuvwxyz";
  for (let row = 0; row < size; row += 1) {
    for (let col = 0; col < size; col += 1) {
      if (!grid[row][col]) {
        grid[row][col] = alphabet[Math.floor(Math.random() * alphabet.length)];
      }
    }
  }
};

const generatePuzzle = () => {
  const selectedWords = pickWords();
  const maxLen = Math.max(6, ...selectedWords.map((w) => w.length));
  gridSize = Math.max(8, Math.min(12, maxLen + 3));
  const grid = buildEmptyGrid(gridSize);
  const placements = new Map();

  selectedWords.forEach((word) => {
    const result = placeWord(grid, word);
    if (result.placed) placements.set(word, result);
  });

  fillGrid(grid);
  return { grid, words: selectedWords, placements };
};

const getCellFromPos = (x, y) => {
  const col = Math.floor((x - gridOffset.x) / cellSize);
  const row = Math.floor((y - gridOffset.y) / cellSize);
  if (row < 0 || row >= gridSize || col < 0 || col >= gridSize) return null;
  return { row, col };
};

const getLineCells = (start, end) => {
  const dr = Math.sign(end.row - start.row);
  const dc = Math.sign(end.col - start.col);

  if (dr === 0 && dc === 0) return [start];

  if (
    start.row !== end.row &&
    start.col !== end.col &&
    Math.abs(end.row - start.row) !== Math.abs(end.col - start.col)
  ) {
    return [];
  }

  const cells = [];
  let row = start.row;
  let col = start.col;

  while (true) {
    cells.push({ row, col });
    if (row === end.row && col === end.col) break;
    row += dr;
    col += dc;
  }

  return cells;
};

const getWordFromCells = (cells) =>
  cells.map((c) => puzzle.grid[c.row][c.col]).join("");

// 🥾 Boot
function boot({ screen, resize }) {
  resize(192, 192);
  puzzle = generatePuzzle();

  // Calculate layout
  cellSize = Math.floor(Math.min(screen.width, screen.height - 40) / gridSize);
  gridOffset.x = Math.floor((screen.width - cellSize * gridSize) / 2);
  gridOffset.y = 24;
}

// 🎨 Paint
function paint({ wipe, ink, box, line, write, screen }) {
  wipe(20, 20, 35);

  // Title
  ink("cyan").write("WORD MUNCHERS", { x: 4, y: 4 });
  ink("gray").write(`${found.size}/${puzzle.words.length}`, { x: screen.width - 30, y: 4 });

  // Draw grid background
  ink(30, 30, 50).box(
    gridOffset.x - 2,
    gridOffset.y - 2,
    cellSize * gridSize + 4,
    cellSize * gridSize + 4
  );

  // Draw cells
  for (let row = 0; row < gridSize; row += 1) {
    for (let col = 0; col < gridSize; col += 1) {
      const x = gridOffset.x + col * cellSize;
      const y = gridOffset.y + row * cellSize;
      const letter = puzzle.grid[row][col];
      const key = `${row},${col}`;

      // Check if this cell is part of a found word
      let isFound = false;
      for (const word of found) {
        const placement = puzzle.placements.get(word);
        if (placement) {
          for (let i = 0; i < word.length; i++) {
            if (
              placement.startRow + placement.dr * i === row &&
              placement.startCol + placement.dc * i === col
            ) {
              isFound = true;
              break;
            }
          }
        }
        if (isFound) break;
      }

      // Check if cell is in current selection
      let isSelected = false;
      let isStart = false;
      if (selection?.cells) {
        selection.cells.forEach((c, idx) => {
          if (c.row === row && c.col === col) {
            isSelected = true;
            if (idx === 0) isStart = true;
          }
        });
      }

      // Cell background
      if (isFound) {
        ink(60, 140, 80).box(x + 1, y + 1, cellSize - 2, cellSize - 2);
      } else if (isStart) {
        ink(180, 120, 60).box(x + 1, y + 1, cellSize - 2, cellSize - 2);
      } else if (isSelected) {
        ink(80, 160, 180).box(x + 1, y + 1, cellSize - 2, cellSize - 2);
      } else {
        ink(40, 45, 65).box(x + 1, y + 1, cellSize - 2, cellSize - 2);
      }

      // Letter
      const textColor = isFound ? [180, 255, 200] : isSelected ? [220, 255, 255] : [200, 200, 220];
      ink(...textColor).write(letter.toUpperCase(), {
        x: x + Math.floor(cellSize / 2) - 3,
        y: y + Math.floor(cellSize / 2) - 3,
      });
    }
  }

  // Draw word list at bottom
  const listY = gridOffset.y + cellSize * gridSize + 6;
  let listX = 4;
  puzzle.words.forEach((word, i) => {
    const isWordFound = found.has(word);
    if (isWordFound) {
      ink(100, 200, 120);
    } else {
      ink(120, 120, 140);
    }
    write(word.toUpperCase(), { x: listX, y: listY + Math.floor(i / 4) * 10 });
    listX += 46;
    if ((i + 1) % 4 === 0) listX = 4;
  });

  // Win message
  if (found.size === puzzle.words.length) {
    ink(255, 220, 100).write("ALL MUNCHED! TAP TO RESTART", {
      x: 20,
      y: screen.height - 12,
    });
  }
}

// 🎪 Act
function act({ event, screen }) {
  if (event.is("touch")) {
    // Check for restart on win
    if (found.size === puzzle.words.length) {
      found.clear();
      puzzle = generatePuzzle();
      cellSize = Math.floor(Math.min(screen.width, screen.height - 40) / gridSize);
      gridOffset.x = Math.floor((screen.width - cellSize * gridSize) / 2);
      return;
    }

    const cell = getCellFromPos(event.x, event.y);
    if (cell) {
      selection = { start: cell, cells: [cell] };
    }
  }

  if (event.is("draw") && selection) {
    const cell = getCellFromPos(event.x, event.y);
    if (cell) {
      const cells = getLineCells(selection.start, cell);
      if (cells.length > 0) {
        selection.cells = cells;
      }
    }
  }

  if (event.is("lift") && selection) {
    const letters = getWordFromCells(selection.cells);
    const reversed = letters.split("").reverse().join("");

    const match = puzzle.words.find(
      (w) => (w === letters || w === reversed) && !found.has(w)
    );

    if (match) {
      found.add(match);
    }

    selection = null;
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Words",
    desc: "A Word Munchers-style tap and drag word game laboratory.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
