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
let T = __theme.update(); // global theme (auto dark/light)
let tabMatches = []; // current tab completion candidates
let tabIndex = -1;   // cycling index for tab
let tabPrefix = "";   // what was typed before tab

// Discovered piece names (populated in boot via system.listPieces)
let PIECE_NAMES = [];
// Built-in non-piece commands
const BUILTIN_COMMANDS = [
  "version", "reboot", "off", "clear", "help", "ssh", "hi", "bye", "ls", "papers", "login",
];
// All completable commands (built in boot)
let COMMANDS = [];
// $code aliases
const CODE_NAMES = ["$roz"];
// Piece descriptions (for tab completion display)
const PIECE_DESC = {
  "notepat":       "synthesizer instrument",
  "os":            "system update (OTA)",
  "wifi":          "network picker",
  "claude":        "AI assistant",
  "terminal":      "PTY terminal",
  "geo":           "geolocation",
  "chat":          "real-time chat",
  "laer-klokken":  "clock room chat",
  "machine":       "hardware info",
  "roz":           "generative art",
  "list":          "all commands",
  "ls":            "→ list",
  "papers":        "papers site",
  "clock":         "melody clock",
  "brick-breaker": "paddle + ball game",
  "gostop":        "go/stop rhythm",
  "beat":          "percussion",
  "shh":           "noise drone",
  "dync":          "percussive pad",
  "chart":         "diagram sketch",
  "f3ral3xp":      "feral expression",
  "3x3":           "ortholinear pad",
  "print":         "printer / thermal",
  "theme":         "prompt theme",
  "voice":         "system voice",
  "login":         "switch identity",
  "dark":          "dark mode",
  "light":         "light mode",
  "auto":          "auto dark/light",
  "error":         "error screen",
  "404":           "not found",
};

// WiFi auto-connect state
const AC_SSID = "aesthetic.computer";
const AC_PASS = "aesthetic.computer";
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
  // Load saved theme from config (persists across reboots)
  try {
    const raw = system?.readFile?.("/mnt/config.json");
    if (raw) {
      const cfg = JSON.parse(raw);
      if (cfg.theme) __theme.apply(cfg.theme);
      if (cfg.darkMode === "dark") __theme._forceDark = true;
      else if (cfg.darkMode === "light") __theme._forceDark = false;
      if (cfg.darkMode) { __theme._lastCheck = 0; __theme.update(); }
    }
  } catch (_) {}
  // Discover all available pieces dynamically
  PIECE_NAMES = (system?.listPieces?.() || []).filter(n => n !== "prompt" && n !== "lisp" && n !== "cc");
  PIECE_NAMES.sort();
  COMMANDS = [...PIECE_NAMES, ...BUILTIN_COMMANDS, ...CODE_NAMES];
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

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorFrame = 0;
    cursorVisible = true;

    // Note: keystroke voicing is handled by C input loop for zero-latency TTS.
    // (system_mode "prompt" triggers tts_speak_cached in ac-native.c)

    if (key === "tab") {
      // Tab completion
      const prefix = input.slice(0, cursor).toLowerCase();
      if (prefix.length > 0) {
        if (tabPrefix !== prefix) {
          // New prefix — build match list
          tabPrefix = prefix;
          tabMatches = COMMANDS.filter(c => c.startsWith(prefix) && c !== prefix);
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
  // Handle colon params: "clock:cdefg" → piece="clock", rest passed via jump
  const colonIdx = lower.indexOf(":");
  const baseName = colonIdx >= 0 ? lower.slice(0, colonIdx) : lower;
  // Also handle space params: "clock cdefg" → piece="clock"
  const spaceIdx = lower.indexOf(" ");
  const baseWord = spaceIdx >= 0 ? lower.slice(0, spaceIdx) : lower;

  // Built-in commands (non-piece)
  if (lower === "version") {
    message = system?.version || "unknown";
    messageFrame = 0;
    return;
  }
  if (lower === "reboot") {
    system?.reboot?.();
    return;
  }
  if (lower === "off" || lower === "shutdown" || lower === "poweroff") {
    message = "shutting down...";
    messageFrame = 0;
    system?.poweroff?.();
    return;
  }
  if (baseWord === "login") {
    const code = cmd.slice(spaceIdx > 0 ? spaceIdx + 1 : cmd.length).trim();
    message = "~> login";
    messageFrame = 0;
    system?.jump?.(code ? "login:" + code : "login");
    return;
  }
  if (lower === "clear") {
    history = [];
    message = "";
    return;
  }
  if (lower === "help") {
    message = "type a piece name or kidlisp — tab to complete";
    messageFrame = 0;
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
  if (lower === "hi") {
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
  if (lower === "bye") {
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

  // Dark/light mode override
  if (lower === "dark") {
    __theme._forceDark = true;
    __theme._lastCheck = 0;
    __theme.update();
    system?.saveConfig?.("darkMode", "dark");
    message = "dark mode";
    messageFrame = 0;
    return;
  }
  if (lower === "light") {
    __theme._forceDark = false;
    __theme._lastCheck = 0;
    __theme.update();
    system?.saveConfig?.("darkMode", "light");
    message = "light mode";
    messageFrame = 0;
    return;
  }
  if (lower === "auto") {
    __theme._forceDark = undefined;
    __theme._lastCheck = 0;
    __theme.update();
    system?.saveConfig?.("darkMode", "auto");
    message = "auto dark/light (LA time)";
    messageFrame = 0;
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

  // "claude TOKEN" — associate Claude Code OAuth token with your handle
  if (baseWord === "claude" && spaceIdx > 0) {
    const token = cmd.slice(spaceIdx + 1).trim();
    if (token.startsWith("sk-ant-")) {
      message = "saving token...";
      messageFrame = 0;
      // Read handle from config
      let handle = "";
      let acToken = "";
      try {
        const raw = system?.readFile?.("/mnt/config.json");
        if (raw) {
          const cfg = JSON.parse(raw);
          handle = cfg.handle || "";
          acToken = cfg.token || "";
        }
      } catch (_) {}
      if (!handle) {
        message = "not logged in — flash from authenticated CLI first";
        messageFrame = 0;
        return;
      }
      // POST to API
      const url = "https://aesthetic.computer/.netlify/functions/claude-token";
      const body = JSON.stringify({ token });
      const headers = JSON.stringify({ "Content-Type": "application/json", "Authorization": "Bearer " + acToken });
      system?.fetchPost?.(url, body, headers);
      // Also save locally for immediate use
      system?.writeFile?.("/claude-token", token);
      message = "token saved for @" + handle;
      messageFrame = 0;
      return;
    }
  }

  // "git TOKEN" — associate GitHub PAT with your handle
  if (baseWord === "git" && spaceIdx > 0) {
    const pat = cmd.slice(spaceIdx + 1).trim();
    if (pat.startsWith("ghp_")) {
      message = "saving github token...";
      messageFrame = 0;
      let handle = "";
      let acToken = "";
      try {
        const raw = system?.readFile?.("/mnt/config.json");
        if (raw) {
          const cfg = JSON.parse(raw);
          handle = cfg.handle || "";
          acToken = cfg.token || "";
        }
      } catch (_) {}
      if (!handle) {
        message = "not logged in — flash from authenticated CLI first";
        messageFrame = 0;
        return;
      }
      const url = "https://aesthetic.computer/.netlify/functions/claude-token";
      const body = JSON.stringify({ githubPat: pat });
      const headers = JSON.stringify({ "Content-Type": "application/json", "Authorization": "Bearer " + acToken });
      system?.fetchPost?.(url, body, headers);
      system?.writeFile?.("/github-pat", pat);
      message = "github token saved for @" + handle;
      messageFrame = 0;
      return;
    }
  }

  // "papers" opens papers.aesthetic.computer
  if (lower === "papers") {
    message = "~> papers.aesthetic.computer";
    messageFrame = 0;
    // Native can't open a browser — show the URL as a message
    return;
  }

  // "ls" is an alias for "list"
  if (baseName === "ls" || baseWord === "ls") {
    message = "~> list";
    messageFrame = 0;
    system?.jump?.("list");
    return;
  }

  // "code" is an alias for "claude" piece
  if (baseName === "code" || baseWord === "code") {
    message = "~> code";
    messageFrame = 0;
    system?.jump?.("claude");
    return;
  }

  // Dynamic piece jump — check if the command matches any discovered piece
  if (PIECE_NAMES.includes(baseName) || PIECE_NAMES.includes(baseWord)) {
    const pieceName = PIECE_NAMES.includes(baseName) ? baseName : baseWord;
    message = "~> " + pieceName;
    messageFrame = 0;
    system?.jump?.(cmd); // pass full cmd so colon/space params work
    return;
  }

  // Everything else → KidLisp evaluator
  message = "~> lisp";
  messageFrame = 0;
  globalThis.__kidlispSource = cmd;
  globalThis.__kidlispLabel = cmd;
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
      ink(T.fg, T.fg, T.fg + 10); // default text
    }
    write(token, { x: cx, y: y0, size: 1, font });
    cx += token.length * charW;
  }
}

function paint({ wipe, ink, box, write, screen, paintCount, wifi, system }) {
  frame++;
  const T = __theme.update();
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const W = screen.width;
  const H = screen.height;

  // WiFi auto-connect (runs every frame, same logic as notepat)
  if (wifi && !wifi.connected) {
    autoConnectFrame++;
    const knownCreds = [
      { ssid: AC_SSID, pass: AC_PASS },
      ...savedCreds.filter(c => c.ssid !== AC_SSID),
    ];
    const knownSSIDs = new Set(knownCreds.map(c => c.ssid));
    const isConnecting = wifi.state === 3 || wifi.state === 4;
    const isIdle = !isConnecting;

    // Timeout connecting after 3s
    if (isConnecting && frame - connectStartFrame > 180) {
      wifi.disconnect?.();
      autoConnectFrame = -30;
    }

    if (isIdle) {
      if (autoConnectFrame <= 1 || autoConnectFrame % 120 === 0) wifi.scan?.();
      if (autoConnectFrame % 120 === 60) {
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

  // Pure block cursor
  if (cursorVisible) {
    const cx = x0 + cursor * charW;
    ink(T.cursor[0], T.cursor[1], T.cursor[2]);
    box(cx, y0, charW, charH, true);
    // Draw character under cursor if not at end
    if (cursor < input.length) {
      ink(T.dark ? 255 : 255, T.dark ? 255 : 255, T.dark ? 255 : 255);
      write(input[cursor], { x: cx, y: y0, size: 1, font });
    }
  }

  // Ghosted tab completion suggestions below input (with descriptions)
  let completionH = 0;
  {
    const prefix = input.slice(0, cursor).toLowerCase();
    if (prefix.length > 0) {
      const matches = COMMANDS.filter(c => c.startsWith(prefix) && c !== prefix);
      if (matches.length > 0) {
        const sugY = y0 + lineH + 1;
        for (let mi = 0; mi < Math.min(matches.length, 8); mi++) {
          const m = matches[mi];
          const sy = sugY + mi * (charH + 1);
          // Ghosted: dim text, highlight the matching prefix portion
          ink(T.fgDim, T.fgDim - 20, T.fgDim + 20);
          write(m.slice(0, prefix.length), { x: x0, y: sy, size: 1, font });
          ink(T.fgMute, T.fgMute - 10, T.fgMute + 10);
          write(m.slice(prefix.length), { x: x0 + prefix.length * charW, y: sy, size: 1, font });
          // Show description after the name
          const desc = PIECE_DESC[m];
          if (desc) {
            ink(T.fgMute - 20, T.fgMute - 20, T.fgMute - 10);
            write(" " + desc, { x: x0 + m.length * charW, y: sy, size: 1, font });
          }
          // Highlight current tab selection
          if (tabMatches.length > 0 && tabIndex >= 0 && matches[mi] === tabMatches[tabIndex]) {
            const fullLen = desc ? m.length + 1 + desc.length : m.length;
            ink(T.accent[0], T.accent[1], T.accent[2], 40);
            box(x0 - 1, sy - 1, fullLen * charW + 2, charH + 2, true);
          }
        }
        completionH = Math.min(matches.length, 8) * (charH + 1) + 4;
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
    if (COMMANDS.includes(lower) || BUILTIN_COMMANDS.includes(lower)) {
      ink(T.fgMute, T.fgMute - 10, T.fgMute + 10);
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
    ink(T.fgDim, T.fgDim - 10, T.fgDim + 20);
    write(message, { x: x0, y: H - 14, size: 1, font });
  }
}

function sim() {}

const system = "prompt";
export { boot, paint, act, sim, system };
