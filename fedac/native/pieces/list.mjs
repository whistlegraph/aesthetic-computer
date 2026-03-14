// list.mjs — Shows all available pieces and commands on the native system
// Press escape or backspace to return to prompt.

let frame = 0;
let scrollY = 0;

// Piece descriptions (known pieces get descriptions, others show name only)
const PIECE_DESC = {
  "notepat":       "synthesizer instrument with touch grid",
  "os":            "system update panel (OTA flash)",
  "wifi":          "network picker and saved credentials",
  "claude":        "AI assistant (QR login + Claude Code)",
  "terminal":      "PTY terminal emulator (sh, claude)",
  "geo":           "IP-based geolocation",
  "chat":          "real-time chat on aesthetic.computer",
  "laer-klokken":  "clock room chat (warm theme)",
  "machine":       "hardware & software info",
  "roz":           "generative art viewer",
  "prompt":        "command prompt (home)",
  "list":          "this screen",
  "clock":         "melody clock with UTC sync",
  "brick-breaker": "paddle and ball game",
  "gostop":        "go/stop rhythm game",
  "beat":          "mouse percussion instrument",
  "shh":           "noise drone instrument",
  "dync":          "percussive pad instrument",
  "chart":         "diagram sketch",
  "f3ral3xp":      "feral expression",
  "3x3":           "3x3 ortholinear pad",
  "hop":           "game sketch",
  "error":         "error display screen",
  "404":           "page not found",
  "hw":            "hello world",
  "ptt":           "push to talk sketch",
};

let PIECES = []; // Populated dynamically in boot

const COMMANDS = [
  { name: "off",      desc: "power off the machine" },
  { name: "reboot",   desc: "restart the system" },
  { name: "hi",       desc: "show current logged-in user" },
  { name: "bye",      desc: "log out current user" },
  { name: "version",  desc: "show current OS version hash" },
  { name: "ssh",      desc: "start SSH server on port 22" },
  { name: "clear",    desc: "clear command history" },
  { name: "help",     desc: "show quick help" },
];

const CODES = [
  { name: "$roz", desc: "generative art pattern (KidLisp)" },
];

function boot({ system }) {
  // Discover pieces dynamically from filesystem
  const names = (system?.listPieces?.() || []).filter(n => n !== "prompt" && n !== "lisp" && n !== "cc");
  names.sort();
  PIECES = names.map(name => ({ name, desc: PIECE_DESC[name] || null }));
}

function act({ event: e, system }) {
  if (e.is("keyboard:down")) {
    const key = e.key;
    if (key === "escape" || key === "backspace") {
      system?.jump?.("prompt");
      return;
    }
    if (key === "arrowdown") scrollY += 14;
    if (key === "arrowup") scrollY = Math.max(0, scrollY - 14);
  }
}

function paint({ wipe, ink, box, line, write, screen }) {
  frame++;
  const T = __theme.update();
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const W = screen.width;
  const H = screen.height;
  const font = "6x10";
  const charW = 6;
  const lineH = 13;
  const pad = 6;
  let y = pad - scrollY;

  // Title
  ink(T.accent[0], T.accent[1], T.accent[2]);
  write("ac/native commands", { x: pad, y, size: 1, font });
  y += lineH + 4;

  // Section: Pieces
  ink(T.ok[0], T.ok[1], T.ok[2]);
  write(`pieces (${PIECES.length})`, { x: pad, y, size: 1, font });
  y += lineH;
  ink(T.border[0], T.border[1], T.border[2]);
  line(pad, y - 2, W - pad, y - 2);

  for (const p of PIECES) {
    if (y > H + 20) break;
    if (y > -lineH) {
      ink(T.fg, T.fg - 20, T.fg + 15);
      write(p.name, { x: pad, y, size: 1, font });
      if (p.desc) {
        ink(T.fgMute, T.fgMute, T.fgMute + 5);
        write(p.desc, { x: pad, y: y + 11, size: 1, font });
        y += lineH * 2;
      } else {
        y += lineH;
      }
    } else {
      y += p.desc ? lineH * 2 : lineH;
    }
  }

  y += 6;

  // Section: Commands
  ink(T.link[0], T.link[1], T.link[2]);
  write("commands", { x: pad, y, size: 1, font });
  y += lineH;
  ink(T.border[0], T.border[1], T.border[2]);
  line(pad, y - 2, W - pad, y - 2);

  for (const c of COMMANDS) {
    if (y > H + 20) break;
    if (y > -lineH) {
      ink(T.fg - 10, T.fg, T.fg + 20);
      write(c.name, { x: pad, y, size: 1, font });
      ink(T.fgMute, T.fgMute, T.fgMute + 5);
      write(c.desc, { x: pad, y: y + 11, size: 1, font });
    }
    y += lineH * 2;
  }

  y += 6;

  // Section: Code aliases
  ink(T.warn[0], T.warn[1], T.warn[2]);
  write("$codes", { x: pad, y, size: 1, font });
  y += lineH;
  ink(T.border[0], T.border[1], T.border[2]);
  line(pad, y - 2, W - pad, y - 2);

  for (const c of CODES) {
    if (y > H + 20) break;
    if (y > -lineH) {
      ink(T.warn[0], T.warn[1] - 20, T.warn[2] + 40);
      write(c.name, { x: pad, y, size: 1, font });
      ink(T.fgMute, T.fgMute, T.fgMute + 5);
      write(c.desc, { x: pad, y: y + 11, size: 1, font });
    }
    y += lineH * 2;
  }

  y += 8;

  // Footer hint
  if (y > -lineH && y < H + 20) {
    ink(T.fgMute, T.fgMute - 10, T.fgMute + 10);
    write("anything else -> kidlisp", { x: pad, y, size: 1, font });
    y += lineH;
    ink(T.fgMute - 10, T.fgMute - 15, T.fgMute);
    write("esc to return", { x: pad, y, size: 1, font });
  }
}

export { boot, act, paint };
