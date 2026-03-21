// chat.mjs — Real-time chat client for aesthetic.computer
// Connects to a chat room via system.ws. Default: "system"
// Other rooms: "clock" (laer-klokken), "sotce"
// Press escape to return to prompt.

const CHAT_ROOMS = {
  system: "wss://chat-system.aesthetic.computer/",
  clock: "wss://chat-clock.aesthetic.computer/",
  sotce: "wss://chat.sotce.net/",
};

let roomName = "system"; // set via boot params or override
const SHIFT_MAP = {
  "1":"!","2":"@","3":"#","4":"$","5":"%","6":"^","7":"&","8":"*","9":"(","0":")",
  "-":"_","=":"+","[":"{","]":"}",";":":","'":'"',",":"<",".":">","/":"?","\\":"|","`":"~",
};

let frame = 0;
let messages = [];     // [{from, text, when}]
let inputText = "";
let cursor = 0;
let cursorBlink = 0;
let shiftHeld = false;
let handle = "";       // current user handle from config
let sub = "";          // auth0 sub from config
let token = "";        // auth token for chat server authorization
let connected = false;

// Pixel-based scroll with inertial physics
let scroll = 0;           // pixel offset (0 = bottom, positive = scrolled up)
let scrollVelocity = 0;
let isFlinging = false;
let isDragging = false;
let dragStartPos = null;
let lastDragY = 0;
const SCROLL_FRICTION = 0.92;
const SCROLL_MIN_VELOCITY = 0.5;
const BOUNCE_STIFFNESS = 0.15;
const BOUNCE_DAMPING = 0.7;
const DRAG_THRESHOLD = 5;

function boot({ system, params }) {
  // Allow room override via params: system.jump("chat:clock")
  if (params && params[0] && CHAT_ROOMS[params[0]]) {
    roomName = params[0];
  }

  // Read user identity from config
  try {
    var raw = system.readFile("/mnt/config.json");
    if (raw) {
      var cfg = JSON.parse(raw);
      handle = cfg.handle || "";
      sub = cfg.sub || "";
      token = cfg.token || "";
    }
  } catch (_) {}
  // Connect to chat room WebSocket
  var url = CHAT_ROOMS[roomName] || CHAT_ROOMS.system;
  console.log("[chat] connecting to " + url);
  if (system && system.ws) system.ws.connect(url);
}

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorBlink = 0;

    if (key === "escape") {
      if (system && system.jump) system.jump("prompt");
      return;
    }

    if (key === "enter" || key === "return") {
      var text = inputText.trim();
      var wsOk = system && system.ws && system.ws.connected;
      console.log("[chat] send: len=" + text.length + " ws=" + wsOk);
      if (text.length > 0 && wsOk) {
        var msg = JSON.stringify({
          type: "chat:message",
          content: { sub: sub || "anonymous", text: text, handle: handle || "anon", token: token },
        });
        system.ws.send(msg);
        console.log("[chat] sent: " + msg.substring(0, 120));
        // Add locally immediately
        messages.push({ from: "@" + (handle || "me"), text: text, when: Date.now() });
        scroll = 0; // scroll to bottom
        if (sound && sound.synth) sound.synth({ type: "sine", tone: 880, duration: 0.04, volume: 0.1, attack: 0.002, decay: 0.03 });
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
    if (key === "arrowup") { scroll += 13; return; }
    if (key === "arrowdown") { scroll = Math.max(0, scroll - 13); return; }

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

  // Touch scrolling
  if (e.is("touch")) {
    const y = e.pointer?.y ?? e.y ?? 0;
    dragStartPos = y;
    lastDragY = y;
    isDragging = false;
    isFlinging = false;
    scrollVelocity = 0;
  }

  if (e.is("draw")) {
    const y = e.pointer?.y ?? e.y ?? 0;
    if (dragStartPos !== null) {
      if (!isDragging && Math.abs(y - dragStartPos) > DRAG_THRESHOLD) {
        isDragging = true;
      }
      if (isDragging) {
        const dy = lastDragY - y; // drag up = positive scroll
        scroll += dy;
        scrollVelocity = dy;
      }
      lastDragY = y;
    }
  }

  if (e.is("lift")) {
    if (isDragging && Math.abs(scrollVelocity) > SCROLL_MIN_VELOCITY) {
      isFlinging = true;
    }
    isDragging = false;
    dragStartPos = null;
  }
}

function paint({ wipe, ink, box, line, write, screen, system, sound, wifi }) {
  frame++;
  cursorBlink++;
  wipe(20, 18, 30);

  const W = screen.width;
  const H = screen.height;
  const font = "6x10";
  const charW = 6;
  const charH = 10;
  const pad = 4;

  // Process incoming WebSocket messages
  const wsMsgs = (system && system.ws ? system.ws.messages : null);
  if (wsMsgs && wsMsgs.length) {
    for (const raw of wsMsgs) {
      try {
        const msg = JSON.parse(raw);
        const parseContent = (c) => (typeof c === "string" ? JSON.parse(c) : c);
        if (msg.type === "connected") {
          connected = true;
          const content = parseContent(msg.content);
          const history = (content && content.messages) || [];
          // Load message history
          for (const m of history) {
            if (m.from && m.text) {
              messages.push({ from: m.from, text: m.text, when: m.when || 0 });
            }
          }
        } else if (msg.type === "message") {
          const m = parseContent(msg.content);
          if (m && m.from && m.text) {
            messages.push({ from: m.from, text: m.text, when: Date.now() });
            (sound && sound.synth)({ type: "triangle", tone: 660, duration: 0.05, volume: 0.08, attack: 0.002, decay: 0.04 });
          }
        } else if (msg.from && msg.text) {
          messages.push({ from: msg.from, text: msg.text, when: Date.now() });
        }
      } catch (_) {}
    }
  }

  // Reconnect if dropped
  if (wifi && wifi.connected && system && system.ws && !system.ws.connected && !system.ws.connecting && frame % 120 === 0) {
    const url = CHAT_ROOMS[roomName] || CHAT_ROOMS.system;
    if (system && system.ws) system.ws.connect(url);
  }

  // Header — show room name
  ink(140, 120, 180);
  write(roomName === "system" ? "chat" : roomName, { x: pad, y: 2, size: 1, font });

  // Connection status
  const wsOk = (system && system.ws && system.ws.connected);
  if (wsOk) {
    ink(60, 160, 80);
    write("connected", { x: pad + 5 * charW, y: 2, size: 1, font });
  } else if ((wifi && wifi.connected)) {
    ink(200, 200, 80);
    write("connecting...", { x: pad + 5 * charW, y: 2, size: 1, font });
  } else {
    ink(200, 80, 80);
    write("offline", { x: pad + 5 * charW, y: 2, size: 1, font });
  }

  // User identity
  if (handle) {
    ink(100, 80, 120);
    const hLabel = "@" + handle;
    write(hLabel, { x: W - pad - hLabel.length * charW, y: 2, size: 1, font });
  }

  // Divider
  ink(50, 40, 60);
  line(0, 13, W, 13);

  // Input area at bottom
  const inputY = H - charH - 6;
  ink(35, 28, 45);
  box(0, inputY - 3, W, charH + 8, true);
  ink(50, 40, 60);
  line(0, inputY - 3, W, inputY - 3);

  // Input prompt
  ink(120, 80, 160);
  write(">", { x: pad, y: inputY, size: 1, font });

  // Input text
  ink(220, 210, 240);
  const maxInputChars = Math.floor((W - pad * 2 - charW * 2) / charW);
  const displayInput = inputText.length > maxInputChars
    ? inputText.slice(inputText.length - maxInputChars)
    : inputText;
  const inputStartX = pad + charW + 2;
  write(displayInput, { x: inputStartX, y: inputY, size: 1, font });

  // Cursor
  if (cursorBlink % 40 < 25) {
    const displayCursor = inputText.length > maxInputChars
      ? maxInputChars
      : cursor;
    ink(220, 80, 140, 180);
    box(inputStartX + displayCursor * charW, inputY, charW, charH, true);
    if (cursor < inputText.length) {
      ink(255, 255, 255);
      write(inputText[cursor], { x: inputStartX + displayCursor * charW, y: inputY, size: 1, font });
    }
  }

  // Messages area (pixel-based scroll)
  const msgTop = 15;
  const msgBottom = inputY - 6;
  const lineH = charH + 3;
  const chatHeight = msgBottom - msgTop;
  const totalContentH = messages.length * lineH;

  // Render messages from bottom up, offset by scroll
  // scroll=0 means latest messages visible at bottom
  for (let i = 0; i < messages.length; i++) {
    const msg = messages[i];
    // Position: bottom-aligned, scrolled
    const baseY = msgBottom - (messages.length - i) * lineH;
    const my = baseY + Math.floor(scroll);
    if (my + lineH < msgTop || my > msgBottom) continue; // clip

    // From label
    var displayFrom = msg.from || "?";
    if (displayFrom[0] !== "@" && displayFrom !== "?") displayFrom = "@" + displayFrom;
    var fromLabel = displayFrom + ": ";
    var isMe = displayFrom === "@" + handle || msg.from === handle || msg.from === "me";
    ink(isMe ? 140 : 100, isMe ? 120 : 160, isMe ? 180 : 140);
    write(fromLabel, { x: pad, y: my, size: 1, font });

    // Message text (wrap if needed)
    const textX = pad + fromLabel.length * charW;
    const maxTextW = W - textX - pad;
    const maxChars = Math.floor(maxTextW / charW);
    ink(200, 195, 210);
    write(msg.text.slice(0, maxChars), { x: textX, y: my, size: 1, font });
  }

  // Scroll indicator
  if (scroll > 1) {
    ink(100, 80, 120);
    write("^ scroll ^", { x: Math.floor(W / 2) - 30, y: msgTop, size: 1, font });
  }

  // Empty state
  if (messages.length === 0 && connected) {
    ink(70, 60, 80);
    write("no messages yet", { x: pad, y: Math.floor(H / 2) - 5, size: 1, font });
  } else if (!connected && !(wifi && wifi.connected)) {
    ink(70, 60, 80);
    write("connect to wifi first", { x: pad, y: Math.floor(H / 2) - 5, size: 1, font });
  }

  // Hint
  ink(50, 40, 60);
  write("esc:back", { x: W - 9 * charW - pad, y: inputY, size: 1, font });
}

function sim() {
  if (!isFlinging) return;

  const lineH = 13; // charH(10) + 3
  const maxScroll = Math.max(0, messages.length * lineH - 100); // approx chat area

  const outOfBounds = scroll < 0 || scroll > maxScroll;

  if (outOfBounds) {
    const target = scroll < 0 ? 0 : maxScroll;
    const displacement = scroll - target;
    scrollVelocity -= displacement * BOUNCE_STIFFNESS;
    scrollVelocity *= BOUNCE_DAMPING;
    scroll += scrollVelocity;

    if (Math.abs(displacement) < 0.5 && Math.abs(scrollVelocity) < SCROLL_MIN_VELOCITY) {
      scroll = target;
      scrollVelocity = 0;
      isFlinging = false;
    }
  } else {
    scrollVelocity *= SCROLL_FRICTION;
    scroll += scrollVelocity;

    if (scroll < 0 || scroll > maxScroll) {
      scrollVelocity *= 0.5;
    }

    if (Math.abs(scrollVelocity) < SCROLL_MIN_VELOCITY) {
      scrollVelocity = 0;
      isFlinging = false;
    }
  }
}

function leave({ system }) {
  // Don't disconnect WS — notepat may want it
}

export { boot, act, paint, sim, leave };
