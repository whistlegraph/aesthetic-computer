// laer-klokken.mjs — Native "Learn the Clock" chat room
// Connects to wss://chat-clock.aesthetic.computer/ (same room as web laer-klokken)
// Warm terracotta theme. Press escape to return to prompt.

const CHAT_URL = "wss://chat-clock.aesthetic.computer/";
const SHIFT_MAP = {
  "1":"!","2":"@","3":"#","4":"$","5":"%","6":"^","7":"&","8":"*","9":"(","0":")",
  "-":"_","=":"+","[":"{","]":"}",";":":","'":'"',",":"<",".":">","/":"?","\\":"|","`":"~",
};

let frame = 0;
let messages = [];
let inputText = "";
let cursor = 0;
let cursorBlink = 0;
let shiftHeld = false;
let handle = "";
let sub = "";
let connected = false;
let scrollOffset = 0;

// Warm terracotta theme (matches web laer-klokken)
const T = {
  bg: [140, 75, 45],
  header: [255, 200, 160],
  status: [100, 200, 140],
  statusWarn: [255, 220, 120],
  statusOff: [255, 140, 120],
  handle: [255, 180, 140],
  inputBg: [120, 60, 35],
  inputBorder: [180, 110, 70],
  prompt: [255, 180, 120],
  inputText: [255, 245, 230],
  cursorColor: [255, 140, 80],
  myMsg: [255, 200, 160],
  otherHandle: [100, 220, 180],
  msgText: [255, 245, 230],
  scroll: [200, 140, 100],
  empty: [180, 120, 80],
  hint: [160, 100, 65],
  divider: [160, 95, 60],
};

function boot({ system }) {
  const raw = system?.readFile?.("/mnt/config.json");
  if (raw) {
    try {
      const cfg = JSON.parse(raw);
      handle = cfg.handle || "";
      sub = cfg.sub || "";
    } catch (_) {}
  }
  system?.ws?.connect(CHAT_URL);
}

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorBlink = 0;

    if (key === "escape") { system?.jump?.("prompt"); return; }

    if (key === "enter" || key === "return") {
      const text = inputText.trim();
      if (text.length > 0 && system?.ws?.connected) {
        system.ws.send(JSON.stringify({
          type: "chat:message",
          content: { sub: sub || "anonymous", text, handle: handle || "anon" },
        }));
        messages.push({ from: handle || "me", text, when: Date.now() });
        scrollOffset = 0;
        sound?.synth({ type: "sine", tone: 660, duration: 0.05, volume: 0.1, attack: 0.002, decay: 0.04 });
      }
      inputText = "";
      cursor = 0;
      return;
    }

    if (key === "backspace") {
      if (cursor > 0) {
        inputText = inputText.slice(0, cursor - 1) + inputText.slice(cursor);
        cursor--;
      }
      return;
    }
    if (key === "arrowleft") { if (cursor > 0) cursor--; return; }
    if (key === "arrowright") { if (cursor < inputText.length) cursor++; return; }
    if (key === "home") { cursor = 0; return; }
    if (key === "end") { cursor = inputText.length; return; }
    if (key === "arrowup") { scrollOffset = Math.min(scrollOffset + 1, Math.max(0, messages.length - 3)); return; }
    if (key === "arrowdown") { scrollOffset = Math.max(0, scrollOffset - 1); return; }

    if (key === "space") {
      inputText = inputText.slice(0, cursor) + " " + inputText.slice(cursor);
      cursor++;
      return;
    }
    if (key.length === 1) {
      const ch = shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
      inputText = inputText.slice(0, cursor) + ch + inputText.slice(cursor);
      cursor++;
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, system, sound, wifi }) {
  frame++;
  cursorBlink++;
  wipe(...T.bg);

  const W = screen.width;
  const H = screen.height;
  const font = "6x10";
  const charW = 6;
  const charH = 10;
  const pad = 4;

  // Process incoming WebSocket messages
  const wsMsgs = system?.ws?.messages;
  if (wsMsgs?.length) {
    for (const raw of wsMsgs) {
      try {
        const msg = JSON.parse(raw);
        const parseContent = (c) => (typeof c === "string" ? JSON.parse(c) : c);
        if (msg.type === "connected") {
          connected = true;
          const content = parseContent(msg.content);
          for (const m of (content?.messages || [])) {
            if (m.from && m.text) messages.push({ from: m.from, text: m.text, when: m.when || 0 });
          }
        } else if (msg.type === "message") {
          const m = parseContent(msg.content);
          if (m?.from && m?.text) {
            messages.push({ from: m.from, text: m.text, when: Date.now() });
            sound?.synth({ type: "triangle", tone: 520, duration: 0.06, volume: 0.08, attack: 0.002, decay: 0.05 });
          }
        } else if (msg.from && msg.text) {
          messages.push({ from: msg.from, text: msg.text, when: Date.now() });
        }
      } catch (_) {}
    }
  }

  // Reconnect if dropped
  if (wifi?.connected && !system?.ws?.connected && !system?.ws?.connecting && frame % 120 === 0) {
    system?.ws?.connect(CHAT_URL);
  }

  // Header
  ink(...T.header);
  write("laer-klokken", { x: pad, y: 2, size: 1, font });

  const wsOk = system?.ws?.connected;
  if (wsOk) {
    ink(...T.status);
    write("connected", { x: pad + 13 * charW, y: 2, size: 1, font });
  } else if (wifi?.connected) {
    ink(...T.statusWarn);
    write("connecting...", { x: pad + 13 * charW, y: 2, size: 1, font });
  } else {
    ink(...T.statusOff);
    write("offline", { x: pad + 13 * charW, y: 2, size: 1, font });
  }

  if (handle) {
    ink(...T.handle);
    const hLabel = "@" + handle;
    write(hLabel, { x: W - pad - hLabel.length * charW, y: 2, size: 1, font });
  }

  ink(...T.divider);
  line(0, 13, W, 13);

  // Input area
  const inputY = H - charH - 6;
  ink(...T.inputBg);
  box(0, inputY - 3, W, charH + 8, true);
  ink(...T.inputBorder);
  line(0, inputY - 3, W, inputY - 3);

  ink(...T.prompt);
  write(">", { x: pad, y: inputY, size: 1, font });

  ink(...T.inputText);
  const maxInputChars = Math.floor((W - pad * 2 - charW * 2) / charW);
  const displayInput = inputText.length > maxInputChars
    ? inputText.slice(inputText.length - maxInputChars)
    : inputText;
  const inputStartX = pad + charW + 2;
  write(displayInput, { x: inputStartX, y: inputY, size: 1, font });

  if (cursorBlink % 40 < 25) {
    const displayCursor = inputText.length > maxInputChars ? maxInputChars : cursor;
    ink(...T.cursorColor, 180);
    box(inputStartX + displayCursor * charW, inputY, charW, charH, true);
    if (cursor < inputText.length) {
      ink(255, 255, 255);
      write(inputText[cursor], { x: inputStartX + displayCursor * charW, y: inputY, size: 1, font });
    }
  }

  // Messages
  const msgTop = 15;
  const msgBottom = inputY - 6;
  const lineH = charH + 3;
  const maxVisible = Math.floor((msgBottom - msgTop) / lineH);
  const startIdx = Math.max(0, messages.length - maxVisible - scrollOffset);
  const endIdx = Math.min(messages.length, startIdx + maxVisible);

  let my = msgTop;
  for (let i = startIdx; i < endIdx; i++) {
    const msg = messages[i];
    if (my + lineH > msgBottom) break;

    const fromLabel = (msg.from || "?") + ": ";
    const isMe = msg.from === handle || msg.from === "me";
    ink(...(isMe ? T.myMsg : T.otherHandle));
    write(fromLabel, { x: pad, y: my, size: 1, font });

    const textX = pad + fromLabel.length * charW;
    const maxChars = Math.floor((W - textX - pad) / charW);
    ink(...T.msgText);
    write(msg.text.slice(0, maxChars), { x: textX, y: my, size: 1, font });

    my += lineH;
  }

  if (scrollOffset > 0) {
    ink(...T.scroll);
    write("^ scroll ^", { x: Math.floor(W / 2) - 30, y: msgTop, size: 1, font });
  }

  if (messages.length === 0 && connected) {
    ink(...T.empty);
    write("no messages yet", { x: pad, y: Math.floor(H / 2) - 5, size: 1, font });
  } else if (!connected && !wifi?.connected) {
    ink(...T.empty);
    write("connect to wifi first", { x: pad, y: Math.floor(H / 2) - 5, size: 1, font });
  }

  ink(...T.hint);
  write("esc:back", { x: W - 9 * charW - pad, y: inputY, size: 1, font });
}

function leave() {}

export { boot, act, paint, leave };
