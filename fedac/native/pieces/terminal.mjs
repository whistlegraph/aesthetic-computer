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
let lastExitCode = -1;
let lastCmd = "";

// ANSI 16-color palette → RGB
const COLORS = [
  [0, 0, 0],         // 0 black
  [170, 0, 0],       // 1 red
  [0, 170, 0],       // 2 green
  [170, 85, 0],      // 3 yellow/brown
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

const DEFAULT_FG = [170, 170, 170]; // default foreground (light grey)
const DEFAULT_BG = [0, 0, 0];       // default background (black)
const CELL_SIZE = 10; // elements per cell: ch, fg, bg, bold, fg_r, fg_g, fg_b, bg_r, bg_g, bg_b

function fgColor(idx, bold, r, g, b) {
  if (idx === 255) return [r, g, b]; // truecolor
  if (idx === 16) return bold ? COLORS[15] : DEFAULT_FG; // default
  if (idx >= 0 && idx < 8 && bold) return COLORS[idx + 8];
  if (idx >= 0 && idx < 16) return COLORS[idx];
  return DEFAULT_FG;
}

function bgColor(idx, r, g, b) {
  if (idx === 255) return [r, g, b]; // truecolor
  if (idx === 16) return DEFAULT_BG; // default
  if (idx >= 0 && idx < 16) return COLORS[idx];
  return DEFAULT_BG;
}

// Map common Unicode block/box drawing chars to ASCII replacements
// Claude Code uses these heavily for UI
function charReplace(ch) {
  if (ch >= 32 && ch < 127) return String.fromCharCode(ch);
  // Box drawing
  if (ch === 0x2500 || ch === 0x2501) return "-";  // ─ ━
  if (ch === 0x2502 || ch === 0x2503) return "|";  // │ ┃
  if (ch === 0x250C || ch === 0x250D || ch === 0x250E || ch === 0x250F) return "+"; // ┌
  if (ch === 0x2510 || ch === 0x2511 || ch === 0x2512 || ch === 0x2513) return "+"; // ┐
  if (ch === 0x2514 || ch === 0x2515 || ch === 0x2516 || ch === 0x2517) return "+"; // └
  if (ch === 0x2518 || ch === 0x2519 || ch === 0x251A || ch === 0x251B) return "+"; // ┘
  if (ch === 0x251C || ch === 0x2523) return "+";  // ├
  if (ch === 0x2524 || ch === 0x252B) return "+";  // ┤
  if (ch === 0x252C || ch === 0x2533) return "+";  // ┬
  if (ch === 0x2534 || ch === 0x253B) return "+";  // ┴
  if (ch === 0x253C || ch === 0x254B) return "+";  // ┼
  // Block elements
  if (ch === 0x2588) return "#";  // █ full block
  if (ch === 0x2591) return ".";  // ░ light shade
  if (ch === 0x2592) return ":";  // ▒ medium shade
  if (ch === 0x2593) return "#";  // ▓ dark shade
  if (ch >= 0x2580 && ch <= 0x259F) return "#"; // other block elements
  // Arrows
  if (ch === 0x2190) return "<";  // ←
  if (ch === 0x2191) return "^";  // ↑
  if (ch === 0x2192) return ">";  // →
  if (ch === 0x2193) return "v";  // ↓
  // Bullets and symbols
  if (ch === 0x2022) return "*";  // •
  if (ch === 0x25CF) return "*";  // ●
  if (ch === 0x25CB) return "o";  // ○
  if (ch === 0x2714) return "+";  // ✔
  if (ch === 0x2718) return "x";  // ✘
  if (ch === 0x25B6 || ch === 0x25BA) return ">"; // ▶ ►
  if (ch === 0x25C0 || ch === 0x25C4) return "<"; // ◀ ◄
  if (ch === 0x2026) return "..."; // …
  if (ch === 0x00B7) return ".";   // ·
  // Emoji/misc — show as ?
  if (ch > 127) return "?";
  return null;
}

function boot({ system, screen, params }) {
  cols = Math.floor(screen.width / cellW);
  rows = Math.floor(screen.height / cellH);

  // What to spawn
  let cmd = "/bin/sh";
  let args = [];
  if (params?.[0] === "claude") {
    // Use full path to native Claude binary
    cmd = "/bin/claude";
    args = [];
  } else if (params?.[0]) {
    cmd = params[0];
  }

  lastCmd = cmd;
  system.pty.spawn(cmd, args, cols, rows);
  started = true;
}

function paint({ wipe, ink, box, write, system, screen }) {
  wipe(0); // black background
  cursorBlink++;

  const pty = system.pty;
  if (!pty.active) {
    if (pty.exitCode !== undefined) lastExitCode = pty.exitCode;
    ink(170, 170, 170);
    write("terminal exited", { x: 10, y: 10, font: 1 });

    if (lastExitCode === 127) {
      ink(255, 85, 85);
      write(`'${lastCmd}' not found (exit 127)`, { x: 10, y: 24, font: 1 });
      ink(170, 85, 0);
      write("command missing from initramfs", { x: 10, y: 38, font: 1 });
    } else if (lastExitCode === 126) {
      ink(255, 85, 85);
      write(`'${lastCmd}' permission denied (exit 126)`, { x: 10, y: 24, font: 1 });
    } else if (lastExitCode > 128) {
      ink(255, 85, 85);
      write(`killed by signal ${lastExitCode - 128}`, { x: 10, y: 24, font: 1 });
    } else if (lastExitCode >= 0) {
      ink(lastExitCode === 0 ? 85 : 255, lastExitCode === 0 ? 255 : 170, 85);
      write(`exit code: ${lastExitCode}`, { x: 10, y: 24, font: 1 });
    }

    // Show last grid content
    if (grid) {
      const ptyCols = cols;
      let textY = 56;
      ink(85, 85, 85);
      write("last output:", { x: 10, y: textY, font: 1 });
      textY += 14;
      for (let y = 0; y < Math.min(rows, 10); y++) {
        let line = "";
        for (let x = 0; x < ptyCols; x++) {
          const ch = grid[(y * ptyCols + x) * CELL_SIZE];
          const rep = charReplace(ch);
          if (rep) line += rep;
          else if (line.length > 0) line += " ";
        }
        line = line.trimEnd();
        if (line.length > 0) {
          ink(170, 100, 60);
          write(line, { x: 10, y: textY, font: 1 });
          textY += 12;
        }
      }
    }

    ink(85, 85, 85);
    write("enter: retry   esc: back", { x: 10, y: screen.height - 16, font: 1 });
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
      const i = (y * ptyCols + x) * CELL_SIZE;
      const ch = grid[i];
      const fg = grid[i + 1];
      const bg = grid[i + 2];
      const bold = grid[i + 3];
      const fg_r = grid[i + 4], fg_g = grid[i + 5], fg_b = grid[i + 6];
      const bg_r = grid[i + 7], bg_g = grid[i + 8], bg_b = grid[i + 9];

      const px = x * cellW;
      const py = y * cellH;

      // Draw background if not default black
      if (bg !== 0 && bg !== 16) {
        const [br, bg2, bb] = bgColor(bg, bg_r, bg_g, bg_b);
        ink(br, bg2, bb);
        box(px, py, cellW, cellH);
      }

      // Draw character (ASCII + Unicode replacements)
      if (ch > 32) {
        const rep = charReplace(ch);
        if (rep) {
          const [fr, fg2, fb] = fgColor(fg, bold, fg_r, fg_g, fg_b);
          ink(fr, fg2, fb);
          write(rep, { x: px, y: py, font: 1 });
        }
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
  if (key === "arrowup") return "\x1b[A";
  if (key === "arrowdown") return "\x1b[B";
  if (key === "arrowright") return "\x1b[C";
  if (key === "arrowleft") return "\x1b[D";
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
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }
  if (e.is("keyboard:down:control")) { ctrlHeld = true; return; }
  if (e.is("keyboard:up:control")) { ctrlHeld = false; return; }
  if (e.is("keyboard:down:alt")) { altHeld = true; return; }
  if (e.is("keyboard:up:alt")) { altHeld = false; return; }

  if (!e.is("keyboard:down")) return;
  const key = e.key;
  if (!key) return;

  // If terminal exited: escape goes back, enter retries
  if (!system.pty.active && started) {
    if (key === "escape" || key === "backspace") {
      system?.jump?.("prompt");
      return;
    }
    if (key === "enter" || key === "return") {
      system.pty.spawn(lastCmd || "/bin/sh", [], cols, rows);
      grid = null;
      return;
    }
    return;
  }

  // Ctrl+key → control character
  if (ctrlHeld && key.length === 1) {
    const code = key.toLowerCase().charCodeAt(0);
    if (code >= 97 && code <= 122) {
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
