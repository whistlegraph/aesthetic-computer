// prompt.mjs — Native command-line piece for fedac
// Text input at top-left, pink block cursor, no prompt prefix.
// Anything typed (except "notepat") is evaluated as KidLisp source.

let input = "";
let cursor = 0; // cursor position within input
let cursorVisible = true;
let cursorFrame = 0;
let history = [];
let historyIndex = -1;
let message = "";
let messageFrame = 0;
let shiftHeld = false;
let frame = 0;
let tabMatches = []; // current tab completion candidates
let tabIndex = -1;   // cycling index for tab
let tabPrefix = "";   // what was typed before tab

// All commands (for tab completion and 'list')
const COMMANDS = [
  "notepat", "np", "os", "update", "net", "wifi", "version", "ver",
  "reboot", "clear", "cls", "help", "claude", "cl", "ssh", "list",
  "chat", "hi", "login", "bye", "logout", "machine", "laer-klokken",
];
// Piece names (jumpable .mjs pieces, excluding prompt itself and lisp engine)
const PIECES = ["notepat", "os", "wifi", "claude", "chat", "list", "machine", "laer-klokken"];
// $code aliases
const CODE_NAMES = ["$roz"];

// WiFi auto-connect state
const AC_SSID = "aesthetic.computer";
const AC_PASS = "aesthetic.computer";
const FALLBACK_WIFI = [
  { ssid: "ATT2AWTpcr", pass: "t84q%7%g2h8u" },
  { ssid: "ATTcifXGXi", pass: "=dvt%mnk8h6z" },
];
const CREDS_PATH = "/mnt/wifi_creds.json";
let savedCreds = [];
let autoConnectFrame = 0;
let connectStartFrame = -9999;

const SHIFT_MAP = {
  "1":"!", "2":"@", "3":"#", "4":"$", "5":"%",
  "6":"^", "7":"&", "8":"*", "9":"(", "0":")",
  "-":"_", "=":"+", "[":"{", "]":"}", "\\":"|",
  ";":":", "'":'"', ",":"<", ".":">", "/":"?", "`":"~",
};

// KidLisp syntax highlighting colors
const KL_KEYWORDS = new Set([
  "ink","line","circle","box","wipe","fade","write","grid","plot",
  "spin","zoom","scroll","contrast","noise","blend","mirror",
  "w","h","w/2","h/2","t","frame","pi","tau",
  "sin","cos","tan","abs","sqrt","floor","ceil","round","min","max","pow","mod",
  "?","...",
]);
const KL_COLORS = new Set([
  "red","green","blue","cyan","magenta","yellow","white","black",
  "orange","pink","purple","gray","grey","rainbow",
]);

function klTokenColor(token) {
  if (KL_COLORS.has(token)) return [255, 140, 100]; // warm orange for color names
  if (KL_KEYWORDS.has(token)) return [120, 180, 255]; // blue for keywords
  if (/^-?\d+(\.\d+)?$/.test(token)) return [180, 220, 140]; // green for numbers
  if (/^[a-z]/.test(token) && token.includes("/")) return [180, 220, 140]; // w/2 etc
  return null; // default
}

// Built-in $code aliases
const CODES = {
  "$roz": `fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)`,
};

function boot({ system }) {
  message = "";
  // Restore input from KidLisp return (backspace/escape preserves source)
  if (globalThis.__promptRestore) {
    input = globalThis.__promptRestore;
    cursor = input.length;
    globalThis.__promptRestore = undefined;
  }
  // Load saved WiFi credentials
  try {
    const raw = system?.readFile?.(CREDS_PATH);
    if (raw) savedCreds = JSON.parse(raw);
  } catch (_) {}
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorFrame = 0;
    cursorVisible = true;

    if (key === "tab") {
      // Tab completion
      const prefix = input.slice(0, cursor).toLowerCase();
      if (prefix.length > 0) {
        if (tabPrefix !== prefix) {
          // New prefix — build match list
          tabPrefix = prefix;
          const all = [...COMMANDS, ...CODE_NAMES];
          tabMatches = all.filter(c => c.startsWith(prefix) && c !== prefix);
          tabIndex = -1;
        }
        if (tabMatches.length > 0) {
          tabIndex = (tabIndex + 1) % tabMatches.length;
          const match = tabMatches[tabIndex];
          input = match + input.slice(cursor);
          cursor = match.length;
        }
      }
      return;
    }
    // Any non-tab key resets tab state
    tabPrefix = "";
    tabMatches = [];
    tabIndex = -1;

    if (key === "enter" || key === "return") {
      const cmd = input.trim();
      if (cmd.length > 0) {
        history.unshift(cmd);
        historyIndex = -1;
        execute(cmd, system);
        input = "";
        cursor = 0;
      }
    } else if (key === "backspace") {
      if (cursor > 0) {
        input = input.slice(0, cursor - 1) + input.slice(cursor);
        cursor--;
      }
    } else if (key === "delete") {
      if (cursor < input.length) {
        input = input.slice(0, cursor) + input.slice(cursor + 1);
      }
    } else if (key === "escape") {
      input = "";
      cursor = 0;
    } else if (key === "arrowleft") {
      if (cursor > 0) cursor--;
    } else if (key === "arrowright") {
      if (cursor < input.length) cursor++;
    } else if (key === "home") {
      cursor = 0;
    } else if (key === "end") {
      cursor = input.length;
    } else if (key === "arrowup") {
      if (history.length > 0 && historyIndex < history.length - 1) {
        historyIndex++;
        input = history[historyIndex];
        cursor = input.length;
      }
    } else if (key === "arrowdown") {
      if (historyIndex > 0) {
        historyIndex--;
        input = history[historyIndex];
        cursor = input.length;
      } else if (historyIndex === 0) {
        historyIndex = -1;
        input = "";
        cursor = 0;
      }
    } else if (key === "space") {
      input = input.slice(0, cursor) + " " + input.slice(cursor);
      cursor++;
    } else if (key.length === 1) {
      const ch = shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
      input = input.slice(0, cursor) + ch + input.slice(cursor);
      cursor++;
    }
  }
}

function execute(cmd, system) {
  const lower = cmd.toLowerCase();

  // Navigation commands
  if (lower === "notepat" || lower === "np") {
    message = "~> notepat";
    messageFrame = 0;
    system?.jump?.("notepat");
    return;
  }
  if (lower === "os" || lower === "update") {
    message = "~> os";
    messageFrame = 0;
    system?.jump?.("os");
    return;
  }
  if (lower === "net" || lower === "wifi") {
    message = "~> wifi";
    messageFrame = 0;
    system?.jump?.("wifi");
    return;
  }
  if (lower === "version" || lower === "ver") {
    message = system?.version || "unknown";
    messageFrame = 0;
    return;
  }
  if (lower === "reboot") {
    system?.reboot?.();
    return;
  }
  if (lower === "clear" || lower === "cls") {
    history = [];
    message = "";
    return;
  }
  if (lower === "help") {
    message = "kidlisp | notepat | os | net | claude | list";
    messageFrame = 0;
    return;
  }
  if (lower === "list") {
    message = "~> list";
    messageFrame = 0;
    system?.jump?.("list");
    return;
  }
  if (lower === "claude" || lower === "cl") {
    message = "~> claude";
    messageFrame = 0;
    system?.jump?.("claude");
    return;
  }
  if (lower === "ssh") {
    if (system?.sshStarted) {
      message = "ssh running on port 22";
    } else {
      system?.startSSH?.();
      message = "starting ssh...";
    }
    messageFrame = 0;
    return;
  }
  if (lower === "chat") {
    message = "~> chat";
    messageFrame = 0;
    system?.jump?.("chat");
    return;
  }
  if (lower === "hi" || lower === "login") {
    const raw = system?.readFile?.("/mnt/config.json");
    if (raw) {
      try {
        const cfg = JSON.parse(raw);
        if (cfg.handle) {
          message = "logged in as @" + cfg.handle;
        } else if (cfg.email) {
          message = "logged in as " + cfg.email;
        } else {
          message = "config exists but no user";
        }
      } catch (_) { message = "config parse error"; }
    } else {
      message = "not logged in — flash from an authenticated CLI";
    }
    messageFrame = 0;
    return;
  }
  if (lower === "bye" || lower === "logout") {
    const raw = system?.readFile?.("/mnt/config.json");
    let cfg = {};
    if (raw) try { cfg = JSON.parse(raw); } catch (_) {}
    const had = cfg.handle || cfg.email;
    delete cfg.handle;
    delete cfg.email;
    delete cfg.sub;
    system?.writeFile?.("/mnt/config.json", JSON.stringify(cfg, null, 2));
    message = had ? "logged out (was @" + had + ")" : "already logged out";
    messageFrame = 0;
    return;
  }

  if (lower === "machine") {
    message = "~> machine";
    messageFrame = 0;
    system?.jump?.("machine");
    return;
  }
  if (lower === "laer-klokken") {
    message = "~> laer-klokken";
    messageFrame = 0;
    system?.jump?.("laer-klokken");
    return;
  }

  // Check for built-in $code aliases
  if (CODES[lower]) {
    message = "~> " + lower;
    messageFrame = 0;
    globalThis.__kidlispSource = CODES[lower];
    globalThis.__kidlispLabel = lower;
    system?.jump?.("lisp");
    return;
  }

  // Everything else → KidLisp evaluator
  message = "~> lisp";
  messageFrame = 0;
  globalThis.__kidlispSource = cmd;
  globalThis.__kidlispLabel = cmd; // full source, no truncation
  system?.jump?.("lisp");
}

// Draw syntax-highlighted KidLisp text
function drawHighlighted(text, x0, y0, charW, ink, write, font) {
  // Tokenize by spaces and parens, preserving positions
  let cx = x0;
  let i = 0;
  while (i < text.length) {
    // Skip whitespace
    if (text[i] === " ") {
      cx += charW;
      i++;
      continue;
    }
    // Parens get special color
    if (text[i] === "(" || text[i] === ")") {
      ink(180, 120, 200); // purple parens
      write(text[i], { x: cx, y: y0, size: 1, font });
      cx += charW;
      i++;
      continue;
    }
    // Colon (fade separator)
    if (text[i] === ":") {
      ink(140, 140, 160);
      write(":", { x: cx, y: y0, size: 1, font });
      cx += charW;
      i++;
      continue;
    }
    // Collect token
    let token = "";
    const start = i;
    while (i < text.length && text[i] !== " " && text[i] !== "(" && text[i] !== ")") {
      token += text[i];
      i++;
    }
    const color = klTokenColor(token.toLowerCase());
    if (color) {
      ink(color[0], color[1], color[2]);
    } else {
      ink(220, 220, 230); // default white
    }
    write(token, { x: cx, y: y0, size: 1, font });
    cx += token.length * charW;
  }
}

function paint({ wipe, ink, box, write, screen, paintCount, wifi, system }) {
  frame++;
  wipe(40, 20, 60);

  const W = screen.width;
  const H = screen.height;

  // WiFi auto-connect (runs every frame, same logic as notepat)
  if (wifi && !wifi.connected) {
    autoConnectFrame++;
    const knownCreds = [
      { ssid: AC_SSID, pass: AC_PASS },
      ...FALLBACK_WIFI,
      ...savedCreds.filter(c => c.ssid !== AC_SSID && !FALLBACK_WIFI.find(f => f.ssid === c.ssid)),
    ];
    const knownSSIDs = new Set(knownCreds.map(c => c.ssid));
    const isConnecting = wifi.state === 3 || wifi.state === 4;
    const isIdle = !isConnecting;

    // Timeout connecting after 5s
    if (isConnecting && frame - connectStartFrame > 300) {
      wifi.disconnect?.();
      autoConnectFrame = -60;
    }

    if (isIdle) {
      if (autoConnectFrame % 300 === 0) wifi.scan?.();
      if (autoConnectFrame % 300 === 150) {
        const nets = wifi.networks || [];
        const matches = nets
          .filter(n => n.ssid && knownSSIDs.has(n.ssid))
          .sort((a, b) => b.signal - a.signal);
        if (matches.length > 0) {
          const best = matches[0];
          const cred = knownCreds.find(c => c.ssid === best.ssid);
          wifi.connect(cred.ssid, cred.pass);
          connectStartFrame = frame;
        }
      }
    }
  }

  const charW = 6; // 6x10 font
  const charH = 10;
  const lineH = 12;
  const x0 = 4;
  const y0 = 4;
  const font = "6x10";

  // Cursor blink
  cursorFrame++;
  if (cursorFrame % 30 === 0) cursorVisible = !cursorVisible;

  // Input text with syntax highlighting
  drawHighlighted(input, x0, y0, charW, ink, write, font);

  // Pink block cursor with wifi status glow
  if (cursorVisible) {
    const cx = x0 + cursor * charW;

    // WiFi glow: subtle colored shadow behind cursor
    if (wifi?.connected) {
      // Connected: faint green glow left edge
      const pulse = 20 + Math.floor(10 * Math.sin(frame * 0.05));
      ink(30, 80 + pulse, 40, 60);
      box(cx - 1, y0, 1, charH, true);
    } else if (wifi) {
      // Disconnected: faint red pulse left edge
      const pulse = frame % 90 < 45 ? 40 : 15;
      ink(80 + pulse, 20, 20, 50);
      box(cx - 1, y0, 1, charH, true);
    }

    ink(220, 80, 140, 180);
    box(cx, y0, charW, charH, true);
    // Draw character under cursor if not at end
    if (cursor < input.length) {
      ink(255, 255, 255);
      write(input[cursor], { x: cx, y: y0, size: 1, font });
    }
  }

  // Ghosted tab completion suggestions below input
  let completionH = 0;
  {
    const prefix = input.slice(0, cursor).toLowerCase();
    if (prefix.length > 0) {
      const all = [...COMMANDS, ...CODE_NAMES];
      const matches = all.filter(c => c.startsWith(prefix) && c !== prefix);
      if (matches.length > 0) {
        const sugY = y0 + lineH + 1;
        for (let mi = 0; mi < Math.min(matches.length, 6); mi++) {
          const m = matches[mi];
          const sy = sugY + mi * (charH + 1);
          // Ghosted: dim text, highlight the matching prefix portion
          ink(80, 60, 100);
          write(m.slice(0, prefix.length), { x: x0, y: sy, size: 1, font });
          ink(60, 45, 75);
          write(m.slice(prefix.length), { x: x0 + prefix.length * charW, y: sy, size: 1, font });
          // Highlight current tab selection
          if (tabMatches.length > 0 && tabIndex >= 0 && matches[mi] === tabMatches[tabIndex]) {
            ink(140, 80, 160, 40);
            box(x0 - 1, sy - 1, m.length * charW + 2, charH + 2, true);
          }
        }
        completionH = Math.min(matches.length, 6) * (charH + 1) + 4;
      }
    }
  }

  // History below input (also syntax highlighted)
  let hy = y0 + lineH + 4 + completionH;
  for (let i = 0; i < history.length && hy < H - 20; i++) {
    // Dim the history entries
    const entry = history[i];
    const lower = entry.toLowerCase();
    // Navigation commands in dim purple, KidLisp source highlighted but dimmed
    if (["notepat","np","os","update","net","wifi","version","ver","help","claude","cl","ssh","reboot","clear","cls","list","machine","laer-klokken"].includes(lower)) {
      ink(80, 60, 100);
      write(entry, { x: x0, y: hy, size: 1, font });
    } else {
      // KidLisp history — dim highlight
      drawHighlighted(entry, x0, hy, charW,
        (r, g, b) => ink(Math.floor(r * 0.4), Math.floor(g * 0.4), Math.floor(b * 0.4)),
        write, font);
    }
    hy += lineH;
  }

  // Message (bottom)
  if (message.length > 0) {
    ink(160, 140, 180);
    write(message, { x: x0, y: H - 14, size: 1, font });
  }
}

function sim() {}

export { boot, paint, act, sim };
