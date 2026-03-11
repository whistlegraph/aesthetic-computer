// prompt.mjs — Native command-line piece for fedac
// Text input at top-left, pink block cursor, no prompt prefix.
// Anything typed (except "notepat") is evaluated as KidLisp source.

let input = "";
let cursorVisible = true;
let cursorFrame = 0;
let history = [];
let historyIndex = -1;
let message = "";
let messageFrame = 0;
let shiftHeld = false;

const SHIFT_MAP = {
  "1":"!", "2":"@", "3":"#", "4":"$", "5":"%",
  "6":"^", "7":"&", "8":"*", "9":"(", "0":")",
  "-":"_", "=":"+", "[":"{", "]":"}", "\\":"|",
  ";":":", "'":'"', ",":"<", ".":">", "/":"?", "`":"~",
};

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
  message = "aesthetic.computer " + (system?.version || "");
  // Restore input from KidLisp return (backspace/escape preserves source)
  if (globalThis.__promptRestore) {
    input = globalThis.__promptRestore;
    globalThis.__promptRestore = undefined;
  }
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorFrame = 0;
    cursorVisible = true;

    if (key === "enter" || key === "return") {
      const cmd = input.trim();
      if (cmd.length > 0) {
        history.unshift(cmd);
        historyIndex = -1;
        execute(cmd, system);
        input = "";
      }
    } else if (key === "backspace") {
      input = input.slice(0, -1);
    } else if (key === "escape") {
      if (input.length > 0) {
        input = "";
      } else {
        system?.jump?.("notepat");
      }
    } else if (key === "arrowup") {
      if (history.length > 0 && historyIndex < history.length - 1) {
        historyIndex++;
        input = history[historyIndex];
      }
    } else if (key === "arrowdown") {
      if (historyIndex > 0) {
        historyIndex--;
        input = history[historyIndex];
      } else if (historyIndex === 0) {
        historyIndex = -1;
        input = "";
      }
    } else if (key === "space") {
      input += " ";
    } else if (key.length === 1) {
      if (shiftHeld) {
        input += SHIFT_MAP[key] ?? key.toUpperCase();
      } else {
        input += key;
      }
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
    message = "type kidlisp or $roz | claude | esc:notepat";
    messageFrame = 0;
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
  globalThis.__kidlispLabel = cmd.length > 20 ? cmd.slice(0, 17) + "..." : cmd;
  system?.jump?.("lisp");
}

function paint({ wipe, ink, box, write, screen, paintCount }) {
  wipe(40, 20, 60);

  const W = screen.width;
  const H = screen.height;
  const charW = 6; // 6x10 font
  const charH = 10;
  const lineH = 12;
  const x0 = 4;
  const y0 = 4;

  // Cursor blink
  cursorFrame++;
  if (cursorFrame % 30 === 0) cursorVisible = !cursorVisible;

  // Input text
  ink(220, 220, 230);
  write(input, { x: x0, y: y0, size: 1, font: "6x10" });

  // Pink block cursor after text
  if (cursorVisible) {
    const cx = x0 + input.length * charW;
    ink(220, 80, 140);
    box(cx, y0, charW, charH, true);
  }

  // History below input
  let hy = y0 + lineH + 4;
  for (let i = 0; i < history.length && hy < H - 20; i++) {
    ink(80, 60, 100);
    write(history[i], { x: x0, y: hy, size: 1, font: "6x10" });
    hy += lineH;
  }

  // Message (bottom)
  if (message.length > 0) {
    ink(160, 140, 180);
    write(message, { x: x0, y: H - 14, size: 1, font: "6x10" });
  }
}

function sim() {}

export { boot, paint, act, sim };
