// claude.mjs — Claude Code client for AC native
// Uses bundled Claude Code CLI (`claude -p`) as the inference backend.
// Auth handled by Claude Code's own OAuth system (setup-token or API key).
// On first run, user enters a token via `claude setup-token` flow.

let input = "";
let cursorVisible = true;
let cursorFrame = 0;
let shiftHeld = false;

let mode = "chat"; // "setup" | "scan" | "chat"
let displayLines = [];
let scrollY = 0;
let streaming = false;
let authenticated = false;
let statusMsg = "";
let statusFrame = 0;

const SHIFT_MAP = {
  "1":"!", "2":"@", "3":"#", "4":"$", "5":"%",
  "6":"^", "7":"&", "8":"*", "9":"(", "0":")",
  "-":"_", "=":"+", "[":"{", "]":"}", "\\":"|",
  ";":":", "'":'"', ",":"<", ".":">", "/":"?", "`":"~",
};

function boot({ system }) {
  // Check if Claude Code is already authenticated by running a quick test
  statusMsg = "checking auth...";
  streaming = true;
  // Try a trivial prompt — if it works, we're authenticated
  const script = `
import { execSync } from 'child_process';
try {
  execSync('claude -p "hi" --max-budget-usd 0.001', {
    encoding: 'utf8', timeout: 15000,
    env: { ...process.env, HOME: '/tmp' },
  });
  process.stdout.write('AUTH_OK');
} catch (e) {
  const msg = e.stderr || e.message || '';
  if (msg.includes('auth') || msg.includes('API key') || msg.includes('token')
      || msg.includes('login') || msg.includes('401') || msg.includes('credentials')) {
    process.stdout.write('AUTH_NEEDED');
  } else {
    // Some other error but auth may be fine
    process.stdout.write('AUTH_OK');
  }
}
`;
  system?.execNode?.(script);
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
        system?.scanQR?.();
        return;
      }
      if (key === "t") {
        statusMsg = "paste token from claude setup-token";
        return;
      }
    }

    // Scan mode
    if (mode === "scan") {
      if (key === "escape") {
        system?.scanQRStop?.();
        mode = "setup";
        statusMsg = "[s] scan QR  [t] type token";
      }
      return;
    }

    if (key === "enter" || key === "return") {
      const cmd = input.trim();
      if (cmd.length === 0) return;
      input = "";

      // Setup: accept API key or setup token
      if (mode === "setup") {
        if (cmd.startsWith("sk-ant-")) {
          // Save API key for claude -p via env
          system?.saveConfig?.("claudeApiKey", cmd);
          authenticated = true;
          mode = "chat";
          statusMsg = "key saved — ready";
          return;
        }
        statusMsg = "need sk-ant-... key";
        return;
      }

      // Chat commands
      if (cmd === "/clear") {
        displayLines = [];
        scrollY = 0;
        statusMsg = "cleared";
        return;
      }
      if (cmd === "/key" || cmd === "/login") {
        mode = "setup";
        statusMsg = "[s] scan QR  [t] type token";
        return;
      }
      if (cmd === "/scan") {
        mode = "scan";
        statusMsg = "opening camera...";
        system?.scanQR?.();
        return;
      }

      if (streaming) {
        statusMsg = "waiting...";
        return;
      }

      if (!authenticated) {
        mode = "setup";
        statusMsg = "[s] scan QR  [t] type token";
        return;
      }

      // Send message via claude -p
      addDisplayLine("you", cmd);
      sendMessage(cmd, system);
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

function sendMessage(prompt, system) {
  streaming = true;
  statusMsg = "thinking...";

  // Load API key from config if saved
  let apiKeyEnv = "";
  const raw = system?.config?.raw;
  if (raw) {
    try {
      const cfg = JSON.parse(raw);
      if (cfg.claudeApiKey) apiKeyEnv = cfg.claudeApiKey;
    } catch (e) {}
  }

  // Escape prompt for shell
  const escaped = prompt.replace(/\\/g, "\\\\").replace(/'/g, "'\\''");

  const script = `
import { execSync } from 'child_process';
try {
  const env = { ...process.env, HOME: '/tmp' };
  ${apiKeyEnv ? `env.ANTHROPIC_API_KEY = ${JSON.stringify(apiKeyEnv)};` : ""}
  const out = execSync("claude -p '" + ${JSON.stringify(escaped)} + "'", {
    encoding: 'utf8',
    timeout: 120000,
    env,
  });
  process.stdout.write(out);
} catch (e) {
  const stderr = e.stderr || '';
  if (stderr) process.stderr.write(stderr);
  else process.stderr.write(e.message || 'claude -p failed');
  process.exit(1);
}
`;
  system?.execNode?.(script);
}

function sim({ system }) {
  // Poll QR scan
  if (mode === "scan") {
    if (system?.qrPending) statusMsg = "scanning...";
    const qr = system?.qrResult;
    const qe = system?.qrError;
    if (qr && qr.startsWith("sk-ant-")) {
      system?.saveConfig?.("claudeApiKey", qr);
      authenticated = true;
      mode = "chat";
      statusMsg = "key scanned — ready";
    } else if (qr) {
      mode = "setup";
      statusMsg = "QR not a valid key";
    } else if (qe) {
      mode = "setup";
      statusMsg = "scan: " + qe;
    }
    return;
  }

  // Poll exec results (reuses fetch slot)
  if (system?.fetchPending === false && streaming) {
    streaming = false;
    const result = system?.fetchResult;
    const error = system?.fetchError;

    // Boot auth check
    if (result === "AUTH_OK") {
      authenticated = true;
      mode = "chat";
      statusMsg = "ready";
      return;
    }
    if (result === "AUTH_NEEDED") {
      authenticated = false;
      mode = "setup";
      statusMsg = "[s] scan QR  [t] type token";
      return;
    }

    // Normal response
    if (error) {
      const emsg = error.length > 60 ? error.slice(0, 57) + "..." : error;
      if (error.includes("auth") || error.includes("API key") || error.includes("401")) {
        authenticated = false;
        mode = "setup";
        statusMsg = "auth failed — enter key";
      } else {
        statusMsg = "error: " + emsg;
      }
      return;
    }

    if (result && result.trim().length > 0) {
      addDisplayLine("claude", result.trim());
      statusMsg = "";
    } else {
      statusMsg = "empty response";
    }
  }
}

function addDisplayLine(role, text) {
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
  displayLines.push({ role: "sep", text: "" });
  scrollY = 0;
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

  cursorFrame++;
  if (cursorFrame % 30 === 0) cursorVisible = !cursorVisible;

  // Title
  ink(80, 120, 200);
  const title = mode === "setup" ? "claude :: setup"
              : mode === "scan" ? "claude :: scan QR"
              : "claude";
  write(title, { x: x0, y: y0, size: 1, font: "6x10" });

  // Scan mode
  if (mode === "scan") {
    if (system?.cameraBlit) system.cameraBlit(0, 0, W, H);
    ink(0, 0, 0, 140);
    box(0, H - 30, W, 30, true);
    const dots = ".".repeat((Math.floor(cursorFrame / 15) % 3) + 1);
    ink(100, 160, 255);
    write("scan QR" + dots, { x: x0, y: H - 26, size: 1, font: "6x10" });
    ink(80, 80, 100);
    write("[esc] cancel", { x: x0, y: H - 14, size: 1, font: "6x10" });
    const cx = Math.floor(W / 2), cy = Math.floor(H / 2);
    const sz = Math.min(W, H) / 4;
    ink(100, 200, 255, 120);
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

  // Display lines
  const inputY = H - 30;
  const displayAreaTop = y0 + lineH + 4;
  const maxVisible = Math.floor((inputY - displayAreaTop) / lineH);
  const startIdx = Math.max(0, displayLines.length - maxVisible - scrollY);
  const endIdx = Math.min(displayLines.length, startIdx + maxVisible);

  let dy = displayAreaTop;
  for (let i = startIdx; i < endIdx; i++) {
    const dl = displayLines[i];
    if (dl.role === "sep") { dy += 4; continue; }
    ink(dl.role === "you" ? [180, 200, 140] : [160, 180, 220]);
    write(dl.text, { x: x0, y: dy, size: 1, font: "6x10" });
    dy += lineH;
  }

  // Input
  ink(220, 220, 230);
  write(input, { x: x0, y: inputY, size: 1, font: "6x10" });
  if (cursorVisible) {
    ink(100, 160, 255);
    box(x0 + input.length * charW, inputY, charW, charH, true);
  }

  // Status
  if (statusMsg) {
    ink(120, 100, 60);
    write(statusMsg, { x: x0, y: H - 14, size: 1, font: "6x10" });
  }

  // Streaming dots
  if (streaming) {
    const dots = ".".repeat((Math.floor(cursorFrame / 15) % 3) + 1);
    ink(100, 160, 255);
    write(dots, { x: W - 24, y: y0, size: 1, font: "6x10" });
  }
}

export { boot, paint, act, sim };
