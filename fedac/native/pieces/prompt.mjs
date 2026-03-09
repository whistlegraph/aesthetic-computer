// prompt.mjs — Native command-line piece for fedac
// Text input at top-left, pink block cursor, no prompt prefix.

let input = "";
let cursorVisible = true;
let cursorFrame = 0;
let history = [];
let historyIndex = -1;
let message = "";
let messageFrame = 0;

function boot({ system }) {
  message = "aesthetic.computer " + (system?.version || "");
}

function act({ event: e, system }) {
  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorFrame = 0;
    cursorVisible = true;

    if (key === "enter" || key === "return") {
      const cmd = input.trim().toLowerCase();
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
    } else if (key.length === 1) {
      input += key;
    }
  }
}

function execute(cmd, system) {
  if (cmd === "help") {
    message = "type a piece name + enter (try: notepat)";
    messageFrame = 0;
    return;
  }
  if (cmd === "version" || cmd === "ver") {
    message = system?.version || "unknown";
    messageFrame = 0;
    return;
  }
  if (cmd === "reboot") {
    system?.reboot?.();
    return;
  }
  if (cmd === "clear" || cmd === "cls") {
    history = [];
    message = "";
    return;
  }
  if (system?.jump) {
    message = "~> " + cmd;
    messageFrame = 0;
    system.jump(cmd);
  } else {
    message = "jump unavailable";
    messageFrame = 0;
  }
}

function paint({ wipe, ink, box, write, screen, paintCount }) {
  wipe(10, 10, 14);

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
    ink(50, 50, 60);
    write(history[i], { x: x0, y: hy, size: 1, font: "6x10" });
    hy += lineH;
  }

  // Message
  if (message.length > 0) {
    ink(140, 140, 80);
    write(message, { x: x0, y: H - 16, size: 1, font: "6x10" });
  }

  // Hint
  ink(60, 60, 70);
  write("esc:back", { x: W - 52, y: H - 6, size: 1, font: "6x10" });
}

function sim() {}

export { boot, paint, act, sim };
