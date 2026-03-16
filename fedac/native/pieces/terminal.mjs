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
let detectedUrl = ""; // URL detected in terminal output for QR display

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

// Map Unicode to ASCII replacements for our 6x10 bitmap font.
// Claude Code uses heavy Unicode UI — we map everything we can.
const UNICODE_MAP = {
  // Box drawing (light)
  0x2500: "-", 0x2501: "=", 0x2502: "|", 0x2503: "|",
  0x250C: "+", 0x250D: "+", 0x250E: "+", 0x250F: "+",
  0x2510: "+", 0x2511: "+", 0x2512: "+", 0x2513: "+",
  0x2514: "+", 0x2515: "+", 0x2516: "+", 0x2517: "+",
  0x2518: "+", 0x2519: "+", 0x251A: "+", 0x251B: "+",
  0x251C: "+", 0x251D: "+", 0x2523: "+", 0x2524: "+",
  0x2525: "+", 0x252B: "+", 0x252C: "+", 0x2533: "+",
  0x2534: "+", 0x253B: "+", 0x253C: "+", 0x254B: "+",
  // Box drawing (double)
  0x2550: "=", 0x2551: "|", 0x2552: "+", 0x2553: "+",
  0x2554: "+", 0x2555: "+", 0x2556: "+", 0x2557: "+",
  0x2558: "+", 0x2559: "+", 0x255A: "+", 0x255B: "+",
  0x255C: "+", 0x255D: "+", 0x255E: "+", 0x255F: "+",
  0x2560: "+", 0x2561: "+", 0x2562: "+", 0x2563: "+",
  0x2564: "+", 0x2565: "+", 0x2566: "+", 0x2567: "+",
  0x2568: "+", 0x2569: "+", 0x256A: "+", 0x256B: "+",
  0x256C: "+",
  // Box drawing (rounded corners)
  0x256D: "+", 0x256E: "+", 0x256F: "+", 0x2570: "+",
  // Block elements
  0x2580: "#", 0x2581: "_", 0x2582: "_", 0x2583: "_",
  0x2584: "#", 0x2585: "#", 0x2586: "#", 0x2587: "#",
  0x2588: "#", 0x2589: "#", 0x258A: "#", 0x258B: "#",
  0x258C: "#", 0x258D: "#", 0x258E: "#", 0x258F: "#",
  0x2590: "#", 0x2591: ".", 0x2592: ":", 0x2593: "#",
  0x2594: "-", 0x2595: "|",
  // Arrows
  0x2190: "<", 0x2191: "^", 0x2192: ">", 0x2193: "v",
  0x2194: "<>", 0x2195: "^v", 0x2196: "\\", 0x2197: "/",
  0x2198: "\\", 0x2199: "/",
  0x21B5: "<-", // ↵ return
  0x21E6: "<=", 0x21E8: "=>",
  // Bullets and symbols
  0x2022: "*", 0x2023: ">", 0x25CF: "*", 0x25CB: "o",
  0x25A0: "#", 0x25A1: "[]", 0x25AA: ".", 0x25AB: ".",
  0x25B2: "^", 0x25B6: ">", 0x25BA: ">", 0x25BC: "v",
  0x25C0: "<", 0x25C4: "<",
  0x25C6: "*", 0x25C7: "*", 0x25CA: "<>",
  0x25E6: "o", 0x25EF: "O",
  // Check marks, crosses, stars
  0x2714: "+", 0x2715: "x", 0x2716: "x", 0x2718: "x",
  0x2713: "+", 0x2717: "x",
  0x2605: "*", 0x2606: "*", 0x2764: "<3",
  // Math / misc symbols
  0x2026: "...", 0x00B7: ".", 0x2219: ".",
  0x00D7: "x", 0x00F7: "/", 0x2260: "!=", 0x2264: "<=",
  0x2265: ">=", 0x221E: "inf", 0x2248: "~=",
  // Spinners / braille (Claude Code progress indicators)
  0x280B: "|", 0x2819: "/", 0x2838: "-", 0x2830: "\\",
  0x2826: "|", 0x2807: "/", 0x280E: "-", 0x2821: "\\",
  // Braille patterns — map all to dots/blocks
  // (range 0x2800-0x28FF used by Claude spinners)
  // General punctuation
  0x2018: "'", 0x2019: "'", 0x201C: '"', 0x201D: '"',
  0x2013: "-", 0x2014: "--", 0x2015: "--",
  0x2039: "<", 0x203A: ">",
  // Emoji shorthand (common in Claude output)
  0x1F512: "[lock]", 0x1F513: "[open]",
  0x1F4E6: "[pkg]", 0x1F4DD: "[note]",
  0x1F680: "[go]", 0x1F50D: "[?]",
  0x2728: "*", 0x26A0: "!!", 0x2699: "[*]",
  0x1F916: "[bot]", 0x1F4AC: "[..]",
  0x1F4C1: "[dir]", 0x1F4C4: "[doc]",
  0x1F527: "[fix]", 0x1F41B: "[bug]",
  0x2705: "[ok]", 0x274C: "[no]",
  0x1F6D1: "[stop]",
  // Currency
  0x00A3: "L", 0x00A5: "Y", 0x20AC: "E",
  // Latin extensions
  0x00E9: "e", 0x00E8: "e", 0x00EA: "e", 0x00EB: "e",
  0x00E0: "a", 0x00E1: "a", 0x00E2: "a", 0x00E4: "a",
  0x00F2: "o", 0x00F3: "o", 0x00F4: "o", 0x00F6: "o",
  0x00F9: "u", 0x00FA: "u", 0x00FB: "u", 0x00FC: "u",
  0x00ED: "i", 0x00EE: "i", 0x00EF: "i",
  0x00F1: "n", 0x00E7: "c", 0x00DF: "ss",
};

function charReplace(ch) {
  if (ch >= 32 && ch < 127) return String.fromCharCode(ch);
  const mapped = UNICODE_MAP[ch];
  if (mapped) return mapped;
  // Braille block (0x2800-0x28FF) — Claude spinners
  if (ch >= 0x2800 && ch <= 0x28FF) return ".";
  // Box drawing range catch-all
  if (ch >= 0x2500 && ch <= 0x257F) return "+";
  // Block elements catch-all
  if (ch >= 0x2580 && ch <= 0x259F) return "#";
  // Geometric shapes catch-all
  if (ch >= 0x25A0 && ch <= 0x25FF) return "*";
  // Dingbats catch-all
  if (ch >= 0x2700 && ch <= 0x27BF) return "*";
  // CJK / emoji — skip silently (don't show ?)
  if (ch >= 0x1F000) return " ";
  // Everything else unmapped — blank rather than ?
  if (ch > 127) return " ";
  return null;
}

function boot({ system, screen, params }) {
  cols = Math.floor(screen.width / cellW);
  rows = Math.floor(screen.height / cellH);

  // What to spawn — check params
  let cmd = "/bin/sh";
  let args = [];
  // params come from colon-separated jump (e.g. "terminal:claude" → params=["claude"])
  // Also check lastCmd global in case params didn't propagate
  const p0 = (params && params.length > 0 ? params[0] : null) || globalThis.__terminalCmd || null;
  globalThis.__terminalCmd = undefined;
  console.log("[terminal] params:", JSON.stringify(params), "p0:", p0, "len:", params ? params.length : "null");
  if (p0 === "claude") {
    // Claude Code native binary
    // Auth: reads ANTHROPIC_API_KEY from /mnt/config.json "claude_api_key" field
    // If no key, Claude will show OAuth URL (QR code auto-detected in terminal)
    cmd = "/bin/claude";
    args = ["claude"];
  } else if (p0) {
    cmd = p0;
  }

  lastCmd = cmd;
  system.pty.spawn(cmd, args, cols, rows);
  started = true;
}

function paint({ wipe, ink, box, write, qr, system, screen }) {
  const T = __theme.update();
  const w = screen.width, h = screen.height;
  wipe(T.bgDim[0], T.bgDim[1], T.bgDim[2]);
  cursorBlink++;

  const pty = system.pty;
  if (!pty.active) {
    if (pty.exitCode !== undefined) lastExitCode = pty.exitCode;
    ink(T.fgDim, T.fgDim, T.fgDim);
    write("terminal exited", { x: 10, y: 10, font: 1 });

    if (lastExitCode === 127) {
      ink(T.err[0], T.err[1], T.err[2]);
      write(`'${lastCmd}' not found (exit 127)`, { x: 10, y: 24, font: 1 });
      ink(T.warn[0], T.warn[1], T.warn[2]);
      write("command missing from initramfs", { x: 10, y: 38, font: 1 });
    } else if (lastExitCode === 126) {
      ink(T.err[0], T.err[1], T.err[2]);
      write(`'${lastCmd}' permission denied (exit 126)`, { x: 10, y: 24, font: 1 });
    } else if (lastExitCode > 128) {
      ink(T.err[0], T.err[1], T.err[2]);
      write(`killed by signal ${lastExitCode - 128}`, { x: 10, y: 24, font: 1 });
    } else if (lastExitCode >= 0) {
      ink(lastExitCode === 0 ? T.ok[0] : T.err[0], lastExitCode === 0 ? T.ok[1] : T.warn[1], lastExitCode === 0 ? T.ok[2] : T.warn[2]);
      write(`exit code: ${lastExitCode}`, { x: 10, y: 24, font: 1 });
    }

    // Show last grid content
    if (grid) {
      const ptyCols = cols;
      let textY = 56;
      ink(T.fgMute, T.fgMute, T.fgMute);
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
          ink(T.warn[0], T.warn[1], T.warn[2]);
          write(line, { x: 10, y: textY, font: 1 });
          textY += 12;
        }
      }
    }

    ink(T.fgMute, T.fgMute, T.fgMute);
    write("enter: retry   esc: back", { x: 10, y: screen.height - 16, font: 1 });
    return;
  }

  // Cache grid data when available
  if (pty.grid) grid = pty.grid;
  if (!grid) return;

  const ptyCols = pty.cols || cols;
  const ptyRows = pty.rows || rows;

  // Check for terminal resize (screen changed since last frame)
  const newCols = Math.floor(w / cellW);
  const newRows = Math.floor(h / cellH);
  if (newCols !== cols || newRows !== rows) {
    cols = newCols;
    rows = newRows;
    if (cols > 0 && rows > 0) system.pty.resize(cols, rows);
  }

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
      const [br, bg2, bb] = bgColor(bg, bg_r, bg_g, bg_b);
      if (br !== 0 || bg2 !== 0 || bb !== 0) {
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

  // Scan grid for https:// URLs (for QR code display)
  // Only scan every 30 frames to avoid performance hit
  if (cursorBlink % 30 === 1) {
    // Build lines, trim trailing spaces, then join without breaks
    // so wrapped URLs are reconstructed across terminal lines
    let lines = [];
    for (let y = 0; y < ptyRows; y++) {
      let line = "";
      for (let x = 0; x < ptyCols; x++) {
        const ch = grid[(y * ptyCols + x) * CELL_SIZE];
        line += (ch >= 32 && ch < 127) ? String.fromCharCode(ch) : " ";
      }
      lines.push(line.trimEnd());
    }
    // Join wrapped lines: if a line fills the full width, the next line
    // is a continuation (no space between them)
    let fullText = "";
    for (let i = 0; i < lines.length; i++) {
      fullText += lines[i];
      // Add space between lines UNLESS this line was full-width (wrapped)
      if (lines[i].length < ptyCols) fullText += " ";
    }
    // Find the last https:// URL in the joined text
    const urlMatch = fullText.match(/https?:\/\/[^\s]+/g);
    detectedUrl = urlMatch ? urlMatch[urlMatch.length - 1] : "";
  }

  // Render QR code for detected URL (bottom-right corner)
  if (detectedUrl && typeof qr === "function") {
    // Use system.qrEncode to get actual module count
    const qrData = system.qrEncode(detectedUrl);
    if (qrData && qrData.size > 0) {
      const qrScale = Math.max(1, Math.min(3, Math.floor(Math.min(w, h) / 4 / qrData.size)));
      const qrSize = (qrData.size + 4) * qrScale;
      const qrX = w - qrSize - 8;
      const qrY = h - qrSize - 20;
      ink(0, 0, 0, 200);
      box(qrX - 4, qrY - 16, qrSize + 8, qrSize + 24);
      qr(detectedUrl, qrX, qrY, qrScale);
      ink(T.link[0], T.link[1], T.link[2]);
      write("scan to open", { x: qrX, y: qrY - 12, font: 1 });
    }
  } else if (cursorBlink % 300 === 0 && detectedUrl) {
    // Debug: log URL detection status periodically
    system.log("[qr] url=" + detectedUrl.substring(0, 60) + "... len=" + detectedUrl.length);
  }

  // Blinking cursor
  if (Math.floor(cursorBlink / 30) % 2 === 0) {
    const cx = (pty.cursorX || 0) * cellW;
    const cy = (pty.cursorY || 0) * cellH;
    ink(T.fg, T.fg, T.fg, 180);
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

  // Ctrl+N — open split view (left=current cmd, right=sh)
  if (ctrlHeld && key === "n") {
    const name = lastCmd.split("/").pop() || "claude";
    system.jump("split:" + name);
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

  // Space bar
  if (key === "space") {
    system.pty.write(" ");
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
