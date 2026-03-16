// split.mjs — Side-by-side PTY split view for AC native
// Left pane: system.pty  (default: claude)
// Right pane: system.pty2 (default: /bin/sh)
// Ctrl+N: toggle focus  |  Ctrl+W: exit to prompt

let leftGrid = null, rightGrid = null;
let leftCols = 0, rightCols = 0;
let rows = 0;
const cellW = 6, cellH = 10;
const CELL_SIZE = 10;
let activePane = 0; // 0=left, 1=right
let shiftHeld = false, ctrlHeld = false, altHeld = false;
let cursorBlink = 0;
let leftStarted = false, rightStarted = false;
let leftLastExitCode = -1, rightLastExitCode = -1;
let leftCmd = "/bin/claude", rightCmd = "/bin/sh";

// ANSI 16-color palette
const COLORS = [
  [0, 0, 0],       [170, 0, 0],     [0, 170, 0],     [170, 85, 0],
  [0, 0, 170],     [170, 0, 170],   [0, 170, 170],   [170, 170, 170],
  [85, 85, 85],    [255, 85, 85],   [85, 255, 85],   [255, 255, 85],
  [85, 85, 255],   [255, 85, 255],  [85, 255, 255],  [255, 255, 255],
];
const DEFAULT_FG = [170, 170, 170];
const DEFAULT_BG = [0, 0, 0];

function fgColor(idx, bold, r, g, b) {
  if (idx === 255) return [r, g, b];
  if (idx === 16) return bold ? COLORS[15] : DEFAULT_FG;
  if (idx >= 0 && idx < 8 && bold) return COLORS[idx + 8];
  if (idx >= 0 && idx < 16) return COLORS[idx];
  return DEFAULT_FG;
}

function bgColor(idx, r, g, b) {
  if (idx === 255) return [r, g, b];
  if (idx === 16) return DEFAULT_BG;
  if (idx >= 0 && idx < 16) return COLORS[idx];
  return DEFAULT_BG;
}

const UNICODE_MAP = {
  0x2500:"-",0x2502:"|",0x250C:"+",0x2510:"+",0x2514:"+",0x2518:"+",
  0x251C:"+",0x2524:"+",0x252C:"+",0x2534:"+",0x253C:"+",
  0x2550:"=",0x2551:"|",0x2554:"+",0x2557:"+",0x255A:"+",0x255D:"+",
  0x2560:"+",0x2563:"+",0x2566:"+",0x2569:"+",0x256C:"+",
  0x256D:"+",0x256E:"+",0x256F:"+",0x2570:"+",
  0x2588:"#",0x2591:".",0x2592:":",0x2593:"#",
  0x2190:"<",0x2191:"^",0x2192:">",0x2193:"v",
  0x2022:"*",0x25CF:"*",0x25A0:"#",0x25B2:"^",0x25B6:">",0x25BC:"v",
  0x2714:"+",0x2715:"x",0x2713:"+",0x2717:"x",
  0x2026:"...",0x2018:"'",0x2019:"'",0x201C:'"',0x201D:'"',
  0x2013:"-",0x2014:"--",
  0x280B:"|",0x2819:"/",0x2838:"-",0x2830:"\\",
  0x2826:"|",0x2807:"/",0x280E:"-",0x2821:"\\",
};

function charReplace(ch) {
  if (ch >= 32 && ch < 127) return String.fromCharCode(ch);
  const mapped = UNICODE_MAP[ch];
  if (mapped) return mapped;
  if (ch >= 0x2800 && ch <= 0x28FF) return ".";
  if (ch >= 0x2500 && ch <= 0x257F) return "+";
  if (ch >= 0x2580 && ch <= 0x259F) return "#";
  if (ch >= 0x25A0 && ch <= 0x25FF) return "*";
  if (ch >= 0x2700 && ch <= 0x27BF) return "*";
  if (ch >= 0x1F000) return " ";
  if (ch > 127) return " ";
  return null;
}

function cmdToArgs(cmd) {
  // "/bin/claude" → ["/bin/claude", "claude"]
  // "/bin/sh"     → ["/bin/sh", "sh"]
  const name = cmd.split("/").pop();
  return [cmd, name];
}

function boot({ system, screen, params }) {
  // params[0] = left cmd name (e.g. "claude"), params[1] = right cmd name
  const lname = (params && params[0]) || "claude";
  const rname = (params && params[1]) || "sh";
  leftCmd  = lname.startsWith("/") ? lname : "/bin/" + lname;
  rightCmd = rname.startsWith("/") ? rname : "/bin/" + rname;

  const halfW = Math.floor((screen.width - 1) / 2);
  leftCols  = Math.floor(halfW / cellW);
  rightCols = Math.floor((screen.width - halfW - 1) / cellW);
  rows = Math.floor(screen.height / cellH);

  const [lcmd, larg] = cmdToArgs(leftCmd);
  const [rcmd, rarg] = cmdToArgs(rightCmd);
  system.pty.spawn(lcmd, [larg], leftCols, rows);
  system.pty2.spawn(rcmd, [rarg], rightCols, rows);
  leftStarted = rightStarted = true;
  activePane = 0;
}

function renderPane(pty, grid, xOff, paneCols, paneRows, isActive, ink, box, write) {
  const T = __theme.update();

  if (!pty.active) {
    ink(T.fgDim, T.fgDim, T.fgDim);
    write("terminal exited", { x: xOff + 4, y: 4, font: 1 });
    return;
  }

  if (!grid) return;

  const useCols = pty.cols || paneCols;
  const useRows = pty.rows || paneRows;

  for (let y = 0; y < useRows; y++) {
    for (let x = 0; x < useCols; x++) {
      const i = (y * useCols + x) * CELL_SIZE;
      const ch   = grid[i];
      const fg   = grid[i + 1];
      const bg   = grid[i + 2];
      const bold = grid[i + 3];
      const fg_r = grid[i + 4], fg_g = grid[i + 5], fg_b = grid[i + 6];
      const bg_r = grid[i + 7], bg_g = grid[i + 8], bg_b = grid[i + 9];

      const px = xOff + x * cellW;
      const py = y * cellH;

      const [br, bg2, bb] = bgColor(bg, bg_r, bg_g, bg_b);
      if (br !== 0 || bg2 !== 0 || bb !== 0) {
        ink(br, bg2, bb);
        box(px, py, cellW, cellH);
      }
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
  if (isActive && Math.floor(cursorBlink / 30) % 2 === 0) {
    const cx = xOff + (pty.cursorX || 0) * cellW;
    const cy = (pty.cursorY || 0) * cellH;
    ink(T.fg, T.fg, T.fg, 180);
    box(cx, cy, cellW, cellH);
  }
}

function paint({ wipe, ink, box, write, system, screen }) {
  const T = __theme.update();
  const w = screen.width, h = screen.height;
  wipe(T.bgDim[0], T.bgDim[1], T.bgDim[2]);
  cursorBlink++;

  const halfW = Math.floor((w - 1) / 2);

  // Cache grids
  if (system.pty.grid)  leftGrid  = system.pty.grid;
  if (system.pty2.grid) rightGrid = system.pty2.grid;

  // Resize check — left
  const newLeftCols  = Math.floor(halfW / cellW);
  const newRightCols = Math.floor((w - halfW - 1) / cellW);
  const newRows = Math.floor(h / cellH);
  if (newLeftCols !== leftCols || newRows !== rows) {
    leftCols = newLeftCols; rows = newRows;
    if (leftCols > 0 && rows > 0) system.pty.resize(leftCols, rows);
  }
  if (newRightCols !== rightCols) {
    rightCols = newRightCols;
    if (rightCols > 0 && rows > 0) system.pty2.resize(rightCols, rows);
  }

  // Render left pane
  renderPane(system.pty, leftGrid, 0, leftCols, rows, activePane === 0,
             ink, box, write);

  // Divider
  const divX = halfW;
  ink(activePane === 0 ? 80 : 40, activePane === 0 ? 160 : 80, activePane === 1 ? 160 : 80);
  box(divX, 0, 1, h);

  // Render right pane
  renderPane(system.pty2, rightGrid, halfW + 1, rightCols, rows, activePane === 1,
             ink, box, write);

  // Status bar hint (bottom of divider)
  ink(60, 60, 70);
  write("^N:swap ^W:exit", { x: divX - 44, y: h - 10, font: 1 });
}

const SHIFT_MAP = {
  "1":"!","2":"@","3":"#","4":"$","5":"%",
  "6":"^","7":"&","8":"*","9":"(","0":")",
  "-":"_","=":"+","[":"{","]":"}","\\":"|",
  ";":":","'":'"',",":"<",".":">","/":"?","`":"~",
};

function keyToAnsi(key) {
  if (key === "arrowup")    return "\x1b[A";
  if (key === "arrowdown")  return "\x1b[B";
  if (key === "arrowright") return "\x1b[C";
  if (key === "arrowleft")  return "\x1b[D";
  if (key === "home")       return "\x1b[H";
  if (key === "end")        return "\x1b[F";
  if (key === "pageup")     return "\x1b[5~";
  if (key === "pagedown")   return "\x1b[6~";
  if (key === "insert")     return "\x1b[2~";
  if (key === "delete")     return "\x1b[3~";
  if (key === "enter" || key === "return") return "\r";
  if (key === "backspace")  return "\x7f";
  if (key === "tab")        return "\t";
  if (key === "escape")     return "\x1b";
  if (key === "f1")  return "\x1bOP";
  if (key === "f2")  return "\x1bOQ";
  if (key === "f3")  return "\x1bOR";
  if (key === "f4")  return "\x1bOS";
  if (key === "f5")  return "\x1b[15~";
  if (key === "f6")  return "\x1b[17~";
  if (key === "f7")  return "\x1b[18~";
  if (key === "f8")  return "\x1b[19~";
  if (key === "f9")  return "\x1b[20~";
  if (key === "f10") return "\x1b[21~";
  if (key === "f11") return "\x1b[23~";
  if (key === "f12") return "\x1b[24~";
  return null;
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:shift"))   { shiftHeld = true;  return; }
  if (e.is("keyboard:up:shift"))     { shiftHeld = false; return; }
  if (e.is("keyboard:down:control")) { ctrlHeld = true;   return; }
  if (e.is("keyboard:up:control"))   { ctrlHeld = false;  return; }
  if (e.is("keyboard:down:alt"))     { altHeld = true;    return; }
  if (e.is("keyboard:up:alt"))       { altHeld = false;   return; }

  if (!e.is("keyboard:down")) return;
  const key = e.key;
  if (!key) return;

  // Ctrl+N — toggle active pane
  if (ctrlHeld && key === "n") {
    activePane = activePane === 0 ? 1 : 0;
    return;
  }

  // Ctrl+W — exit to prompt
  if (ctrlHeld && key === "w") {
    system.jump("prompt");
    return;
  }

  const activePty = activePane === 0 ? system.pty : system.pty2;

  // Ctrl+key → control character
  if (ctrlHeld && key.length === 1) {
    const code = key.toLowerCase().charCodeAt(0);
    if (code >= 97 && code <= 122) {
      activePty.write(String.fromCharCode(code - 96));
      return;
    }
  }

  const ansi = keyToAnsi(key);
  if (ansi) {
    activePty.write(ansi);
    cursorBlink = 0;
    return;
  }

  if (key === "space") {
    activePty.write(" ");
    cursorBlink = 0;
    return;
  }

  if (key.length === 1) {
    let ch = key;
    if (shiftHeld) {
      if (SHIFT_MAP[ch]) ch = SHIFT_MAP[ch];
      else ch = ch.toUpperCase();
    }
    activePty.write(ch);
    cursorBlink = 0;
  }
}

function leave({ system }) {
  if (system.pty.active)  system.pty.kill();
  if (system.pty2.active) system.pty2.kill();
}

export { boot, paint, act, leave };
