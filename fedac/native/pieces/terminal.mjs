// terminal.mjs — Tile-based terminal emulator for AC native
// Renders a PTY process (bash, claude, etc.) through the ac-native graphics system.
// Uses system.pty API to spawn and communicate with the child process.

let grid = null;
let cols = 0, rows = 0;
let cellW = 6, cellH = 10; // font_1 (6x10) cell size
let started = false;
let shiftHeld = false;
let ctrlHeld = false;
let altHeld = false;
let cursorBlink = 0;

// ANSI 16-color palette → RGB
const COLORS = [
  [0, 0, 0],         // 0 black
  [170, 0, 0],       // 1 red
  [0, 170, 0],       // 2 green
  [170, 85, 0],      // 3 yellow
  [0, 0, 170],       // 4 blue
  [170, 0, 170],     // 5 magenta
  [0, 170, 170],     // 6 cyan
  [170, 170, 170],   // 7 white
  [85, 85, 85],      // 8 bright black
  [255, 85, 85],     // 9 bright red
  [85, 255, 85],     // 10 bright green
  [255, 255, 85],    // 11 bright yellow
  [85, 85, 255],     // 12 bright blue
  [255, 85, 255],    // 13 bright magenta
  [85, 255, 255],    // 14 bright cyan
  [255, 255, 255],   // 15 bright white
];

function fgColor(idx, bold) {
  if (idx >= 0 && idx < 8 && bold) return COLORS[idx + 8];
  if (idx >= 0 && idx < 16) return COLORS[idx];
  return COLORS[7]; // default fg
}

function bgColor(idx) {
  if (idx >= 0 && idx < 16) return COLORS[idx];
  return COLORS[0]; // default bg
}

function boot({ system, screen, params }) {
  // Determine grid size from screen
  cols = Math.floor(screen.width / cellW);
  rows = Math.floor(screen.height / cellH);

  // What to spawn — default to bash, "claude" param spawns claude
  let cmd = "/bin/bash";
  let args = [];
  if (params?.[0] === "claude") {
    cmd = "claude";
    args = [];
  } else if (params?.[0]) {
    cmd = params[0];
  }

  system.pty.spawn(cmd, args, cols, rows);
  started = true;
}

function paint({ wipe, ink, box, write, system, screen }) {
  wipe(0); // black background
  cursorBlink++;

  const pty = system.pty;
  if (!pty.active) {
    ink(170, 170, 170);
    write("terminal exited", { x: 10, y: 10, font: 1 });
    write("press any key to restart", { x: 10, y: 24, font: 1 });
    return;
  }

  // Cache grid data when available
  if (pty.grid) grid = pty.grid;
  if (!grid) return;

  const ptyCols = pty.cols || cols;
  const ptyRows = pty.rows || rows;

  // Render each cell
  for (let y = 0; y < ptyRows; y++) {
    for (let x = 0; x < ptyCols; x++) {
      const i = (y * ptyCols + x) * 4;
      const ch = grid[i];
      const fg = grid[i + 1];
      const bg = grid[i + 2];
      const bold = grid[i + 3];

      const px = x * cellW;
      const py = y * cellH;

      // Draw background if not black
      if (bg > 0) {
        const [br, bg2, bb] = bgColor(bg);
        ink(br, bg2, bb);
        box(px, py, cellW, cellH);
      }

      // Draw character if printable
      if (ch > 32 && ch < 127) {
        const [fr, fg2, fb] = fgColor(fg, bold);
        ink(fr, fg2, fb);
        write(String.fromCharCode(ch), { x: px, y: py, font: 1 });
      }
    }
  }

  // Blinking cursor
  if (Math.floor(cursorBlink / 30) % 2 === 0) {
    const cx = (pty.cursorX || 0) * cellW;
    const cy = (pty.cursorY || 0) * cellH;
    ink(170, 170, 170, 180);
    box(cx, cy, cellW, cellH);
  }
}

// Keyboard → PTY input mapping
const SHIFT_MAP = {
  "1":"!", "2":"@", "3":"#", "4":"$", "5":"%",
  "6":"^", "7":"&", "8":"*", "9":"(", "0":")",
  "-":"_", "=":"+", "[":"{", "]":"}", "\\":"|",
  ";":":", "'":'"', ",":"<", ".":">", "/":"?", "`":"~",
};

function keyToAnsi(key) {
  // Arrow keys
  if (key === "arrowup") return "\x1b[A";
  if (key === "arrowdown") return "\x1b[B";
  if (key === "arrowright") return "\x1b[C";
  if (key === "arrowleft") return "\x1b[D";
  // Special keys
  if (key === "home") return "\x1b[H";
  if (key === "end") return "\x1b[F";
  if (key === "pageup") return "\x1b[5~";
  if (key === "pagedown") return "\x1b[6~";
  if (key === "insert") return "\x1b[2~";
  if (key === "delete") return "\x1b[3~";
  if (key === "enter" || key === "return") return "\r";
  if (key === "backspace") return "\x7f";
  if (key === "tab") return "\t";
  if (key === "escape") return "\x1b";
  // F-keys
  if (key === "f1") return "\x1bOP";
  if (key === "f2") return "\x1bOQ";
  if (key === "f3") return "\x1bOR";
  if (key === "f4") return "\x1bOS";
  if (key === "f5") return "\x1b[15~";
  if (key === "f6") return "\x1b[17~";
  if (key === "f7") return "\x1b[18~";
  if (key === "f8") return "\x1b[19~";
  if (key === "f9") return "\x1b[20~";
  if (key === "f10") return "\x1b[21~";
  if (key === "f11") return "\x1b[23~";
  if (key === "f12") return "\x1b[24~";
  return null;
}

function act({ event: e, system }) {
  // Modifier tracking
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }
  if (e.is("keyboard:down:control")) { ctrlHeld = true; return; }
  if (e.is("keyboard:up:control")) { ctrlHeld = false; return; }
  if (e.is("keyboard:down:alt")) { altHeld = true; return; }
  if (e.is("keyboard:up:alt")) { altHeld = false; return; }

  if (!e.is("keyboard:down")) return;
  const key = e.key;
  if (!key) return;

  // If terminal exited, any key restarts
  if (!system.pty.active && started) {
    system.pty.spawn("/bin/bash", [], cols, rows);
    grid = null;
    return;
  }

  // Triple-escape exits to notepat
  if (key === "escape" && !ctrlHeld) {
    // Let escape pass through to PTY (vim, etc.)
  }

  // Ctrl+key → control character
  if (ctrlHeld && key.length === 1) {
    const code = key.toLowerCase().charCodeAt(0);
    if (code >= 97 && code <= 122) { // a-z
      system.pty.write(String.fromCharCode(code - 96));
      return;
    }
  }

  // Special keys → ANSI sequences
  const ansi = keyToAnsi(key);
  if (ansi) {
    system.pty.write(ansi);
    cursorBlink = 0;
    return;
  }

  // Printable characters
  if (key.length === 1) {
    let ch = key;
    if (shiftHeld) {
      if (SHIFT_MAP[ch]) ch = SHIFT_MAP[ch];
      else ch = ch.toUpperCase();
    }
    system.pty.write(ch);
    cursorBlink = 0;
  }
}

function leave({ system }) {
  if (system.pty.active) {
    system.pty.kill();
  }
}

export { boot, paint, act, leave };
