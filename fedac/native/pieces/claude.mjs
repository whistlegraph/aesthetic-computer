// claude.mjs — Claude Code client for AC native
// Uses OAuth refresh tokens from Claude Code subscription for inference.
// Token is stored in /mnt/config.json as claudeRefreshToken.

let input = "";
let cursorVisible = true;
let cursorFrame = 0;
let shiftHeld = false;

let mode = "chat"; // "setup" | "scan" | "chat"
let messages = []; // {role, content} conversation history
let displayLines = []; // rendered output lines
let scrollY = 0;
let streaming = false;
let accessToken = "";
let refreshToken = "";
let statusMsg = "";
let statusFrame = 0;
let responseBuffer = "";
let pendingRefresh = false;
let pendingMessage = false;
let cachedSystem = null;

const SHIFT_MAP = {
  "1":"!", "2":"@", "3":"#", "4":"$", "5":"%",
  "6":"^", "7":"&", "8":"*", "9":"(", "0":")",
  "-":"_", "=":"+", "[":"{", "]":"}", "\\":"|",
  ";":":", "'":'"', ",":"<", ".":">", "/":"?", "`":"~",
};

const MODEL = "claude-sonnet-4-20250514";
const TOKEN_URL = "https://console.anthropic.com/api/oauth/token";
const MESSAGES_URL = "https://api.anthropic.com/v1/messages";

function boot({ system }) {
  // Load saved refresh token from config.raw JSON
  const raw = system?.config?.raw;
  if (raw) {
    try {
      const cfg = JSON.parse(raw);
      refreshToken = cfg.claudeRefreshToken || "";
    } catch (e) { refreshToken = ""; }
  }
  if (refreshToken) {
    mode = "chat";
    statusMsg = "refreshing token...";
    statusFrame = 0;
    // Auto-refresh on boot
    pendingRefresh = true;
  } else {
    mode = "setup";
    statusMsg = "[s] scan QR  [t] type token";
    statusFrame = 0;
  }
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  if (e.is("keyboard:down")) {
    const key = e.key;
    cursorFrame = 0;
    cursorVisible = true;

    if (key === "escape") {
      if (input.length > 0) {
        input = "";
      } else {
        system?.jump?.("prompt");
      }
      return;
    }

    // Setup mode: S to scan QR, T to type token
    if (mode === "setup" && input.length === 0) {
      if (key === "s") {
        mode = "scan";
        statusMsg = "opening camera...";
        statusFrame = 0;
        system?.scanQR?.();
        return;
      }
      if (key === "t") {
        statusMsg = "paste refresh token (sk-ant-ort01-...)";
        statusFrame = 0;
        return;
      }
    }

    // Scan mode: Escape to cancel
    if (mode === "scan") {
      if (key === "escape") {
        system?.scanQRStop?.();
        mode = "setup";
        statusMsg = "[s] scan QR  [t] type token";
        statusFrame = 0;
      }
      return; // ignore all other keys while scanning
    }

    if (key === "enter" || key === "return") {
      const cmd = input.trim();
      if (cmd.length === 0) return;
      input = "";

      if (mode === "setup") {
        if (cmd.startsWith("sk-ant-ort01-")) {
          refreshToken = cmd;
          // Save to config via globalThis (will be picked up by ac-native)
          saveToken(refreshToken, system);
          mode = "chat";
          statusMsg = "token saved — refreshing...";
          statusFrame = 0;
        } else {
          statusMsg = "invalid token (need sk-ant-ort01-...)";
          statusFrame = 0;
        }
        return;
      }

      // Chat mode
      if (cmd === "/clear") {
        messages = [];
        displayLines = [];
        scrollY = 0;
        statusMsg = "cleared";
        statusFrame = 0;
        return;
      }
      if (cmd === "/token") {
        mode = "setup";
        statusMsg = "[s] scan QR  [t] type token";
        statusFrame = 0;
        return;
      }
      if (cmd === "/scan") {
        mode = "scan";
        statusMsg = "opening camera...";
        statusFrame = 0;
        system?.scanQR?.();
        return;
      }

      if (streaming) {
        statusMsg = "waiting for response...";
        statusFrame = 0;
        return;
      }

      // Add user message
      messages.push({ role: "user", content: cmd });
      addDisplayLine("you", cmd);

      // Need access token? Refresh first, then sendMessage will
      // fire automatically when refresh completes (see sim handler)
      if (!accessToken) {
        refreshAccessToken(system);
      } else {
        sendMessage(system);
      }
      return;
    }

    if (key === "backspace") {
      input = input.slice(0, -1);
      return;
    }

    if (key === "arrowup") {
      scrollY = Math.min(scrollY + 1, Math.max(0, displayLines.length - 5));
      return;
    }
    if (key === "arrowdown") {
      scrollY = Math.max(0, scrollY - 1);
      return;
    }

    if (key === "space") {
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

function refreshAccessToken(system) {
  if (!refreshToken) {
    statusMsg = "no refresh token";
    mode = "setup";
    return;
  }
  streaming = true;
  pendingRefresh = true;
  pendingMessage = false;
  statusMsg = "refreshing token...";
  statusFrame = 0;

  const body = JSON.stringify({
    grant_type: "refresh_token",
    refresh_token: refreshToken,
    client_id: "9d1c250a-e61b-44b4-b630-94a3a99f9e18",
  });
  const headers = JSON.stringify({});
  system?.fetchPost?.(TOKEN_URL, body, headers);
}

function sendMessage(system) {
  streaming = true;
  pendingRefresh = false;
  pendingMessage = true;
  statusMsg = "thinking...";
  statusFrame = 0;

  const body = JSON.stringify({
    model: MODEL,
    max_tokens: 1024,
    messages: messages,
  });
  const headers = JSON.stringify({
    "x-api-key": accessToken,
    "anthropic-version": "2023-06-01",
  });
  system?.fetchPost?.(MESSAGES_URL, body, headers);
}

function sim({ system }) {
  cachedSystem = system;

  // Poll QR scan results
  if (mode === "scan") {
    if (system?.qrPending) {
      statusMsg = "scanning for QR code...";
    }
    const qrResult = system?.qrResult;
    const qrError = system?.qrError;
    if (qrResult && qrResult.startsWith("sk-ant-ort01-")) {
      refreshToken = qrResult;
      saveToken(refreshToken, system);
      mode = "chat";
      statusMsg = "token scanned — refreshing...";
      statusFrame = 0;
      pendingRefresh = true;
    } else if (qrResult) {
      mode = "setup";
      statusMsg = "QR not a refresh token";
      statusFrame = 0;
    } else if (qrError) {
      mode = "setup";
      statusMsg = "scan: " + qrError;
      statusFrame = 0;
    }
    return;
  }

  // Auto-refresh on boot (triggered once from boot())
  if (pendingRefresh && !streaming && refreshToken && !accessToken) {
    refreshAccessToken(system);
    return;
  }

  // Poll fetch results
  if (system?.fetchPending === false && streaming) {
    const result = system?.fetchResult;
    const error = system?.fetchError;

    if (error) {
      streaming = false;
      const emsg = error.length > 50 ? error.slice(0, 47) + "..." : error;
      statusMsg = "error: " + emsg;
      statusFrame = 0;
      // If auth error, try refreshing
      if (error.includes("401") || error.includes("403") || error.includes("22")) {
        accessToken = "";
        statusMsg = "token expired — refreshing...";
        refreshAccessToken(system);
      }
      return;
    }

    if (result) {
      try {
        const data = JSON.parse(result);

        // Token refresh response
        if (pendingRefresh && data.access_token) {
          accessToken = data.access_token;
          streaming = false;
          pendingRefresh = false;
          statusMsg = "ready";
          statusFrame = 0;
          // If we have a pending user message, send it now
          if (messages.length > 0 && messages[messages.length - 1].role === "user") {
            sendMessage(system);
          }
          return;
        }

        // Messages API response
        if (pendingMessage && data.content) {
          streaming = false;
          pendingMessage = false;
          const text = data.content
            .filter(b => b.type === "text")
            .map(b => b.text)
            .join("\n");
          messages.push({ role: "assistant", content: text });
          addDisplayLine("claude", text);
          statusMsg = "";
          return;
        }

        // Error response from API
        if (data.error) {
          streaming = false;
          pendingRefresh = false;
          pendingMessage = false;
          const emsg = data.error.message || JSON.stringify(data.error);
          statusMsg = "api: " + (emsg.length > 40 ? emsg.slice(0, 37) + "..." : emsg);
          statusFrame = 0;
          if (emsg.includes("Invalid API Key") || emsg.includes("authentication")
              || emsg.includes("invalid_grant")) {
            accessToken = "";
            refreshToken = "";
            mode = "setup";
            statusMsg = "token invalid — paste new one";
          }
          return;
        }
      } catch (e) {
        streaming = false;
        pendingRefresh = false;
        pendingMessage = false;
        statusMsg = "parse error";
        statusFrame = 0;
      }
    } else {
      streaming = false;
      pendingRefresh = false;
      pendingMessage = false;
      statusMsg = "empty response";
      statusFrame = 0;
    }
  }
}

function addDisplayLine(role, text) {
  // Word-wrap text at ~50 chars per line
  const prefix = role === "you" ? "> " : "  ";
  const maxW = 48;
  const words = text.split(/\s+/);
  let line = prefix;
  for (const word of words) {
    if (line.length + word.length + 1 > maxW && line.length > prefix.length) {
      displayLines.push({ role, text: line });
      line = "  " + word;
    } else {
      line += (line.length > prefix.length ? " " : "") + word;
    }
  }
  if (line.length > 0) displayLines.push({ role, text: line });
  displayLines.push({ role: "sep", text: "" }); // spacer
  scrollY = 0; // scroll to bottom
}

function saveToken(token, system) {
  system?.saveConfig?.("claudeRefreshToken", token);
}

function paint({ wipe, ink, box, line, write, screen, system }) {
  wipe(20, 18, 28);

  const W = screen.width;
  const H = screen.height;
  const charW = 6;
  const charH = 10;
  const lineH = 12;
  const x0 = 4;
  const y0 = 4;

  // Cursor blink
  cursorFrame++;
  if (cursorFrame % 30 === 0) cursorVisible = !cursorVisible;

  // Title bar
  ink(80, 120, 200);
  const title = mode === "setup" ? "claude :: setup"
              : mode === "scan" ? "claude :: scan QR"
              : "claude :: chat";
  write(title, { x: x0, y: y0, size: 1, font: "6x10" });

  // Scan mode: show camera preview + scanning overlay
  if (mode === "scan") {
    // Render camera preview (fills screen, text overlaid on top)
    if (system?.cameraBlit) {
      system.cameraBlit(0, 0, W, H);
    }

    // Semi-transparent overlay bar at bottom
    ink(0, 0, 0, 140);
    box(0, H - 30, W, 30, true);

    const dots = ".".repeat((Math.floor(cursorFrame / 15) % 3) + 1);
    ink(100, 160, 255);
    write("scan QR" + dots, { x: x0, y: H - 26, size: 1, font: "6x10" });
    ink(80, 80, 100);
    write("[esc] cancel", { x: x0, y: H - 14, size: 1, font: "6x10" });

    // Crosshair guides
    const cx = Math.floor(W / 2), cy = Math.floor(H / 2);
    const sz = Math.min(W, H) / 4;
    ink(100, 200, 255, 120);
    // Corner brackets
    line(cx - sz, cy - sz, cx - sz + 10, cy - sz);
    line(cx - sz, cy - sz, cx - sz, cy - sz + 10);
    line(cx + sz, cy - sz, cx + sz - 10, cy - sz);
    line(cx + sz, cy - sz, cx + sz, cy - sz + 10);
    line(cx - sz, cy + sz, cx - sz + 10, cy + sz);
    line(cx - sz, cy + sz, cx - sz, cy + sz - 10);
    line(cx + sz, cy + sz, cx + sz - 10, cy + sz);
    line(cx + sz, cy + sz, cx + sz, cy + sz - 10);

    return;
  }

  // Display lines (above input)
  const inputY = H - 30;
  const displayAreaTop = y0 + lineH + 4;
  const maxVisible = Math.floor((inputY - displayAreaTop) / lineH);

  const startIdx = Math.max(0, displayLines.length - maxVisible - scrollY);
  const endIdx = Math.min(displayLines.length, startIdx + maxVisible);

  let dy = displayAreaTop;
  for (let i = startIdx; i < endIdx; i++) {
    const dl = displayLines[i];
    if (dl.role === "sep") { dy += 4; continue; }
    if (dl.role === "you") {
      ink(180, 200, 140);
    } else {
      ink(160, 180, 220);
    }
    write(dl.text, { x: x0, y: dy, size: 1, font: "6x10" });
    dy += lineH;
  }

  // Input line
  const iy = inputY;
  ink(220, 220, 230);
  write(input, { x: x0, y: iy, size: 1, font: "6x10" });

  // Cursor
  if (cursorVisible) {
    const cx = x0 + input.length * charW;
    ink(100, 160, 255);
    box(cx, iy, charW, charH, true);
  }

  // Status bar
  if (statusMsg) {
    ink(120, 100, 60);
    write(statusMsg, { x: x0, y: H - 14, size: 1, font: "6x10" });
  }

  // Streaming indicator
  if (streaming) {
    const dots = ".".repeat((Math.floor(cursorFrame / 15) % 3) + 1);
    ink(100, 160, 255);
    write(dots, { x: W - 24, y: y0, size: 1, font: "6x10" });
  }
}

export { boot, paint, act, sim };
