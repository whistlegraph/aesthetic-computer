// claude.mjs — Claude Code client for AC native
// Uses bundled Claude Code CLI (`claude -p`) as the inference backend.
// Auth: runs `claude auth login` which gives a URL the user opens on their phone.
// Credentials persist in /tmp/.claude/ (restored from config.json across reboots).

let input = "";
let cursorVisible = true;
let cursorFrame = 0;
let shiftHeld = false;

let mode = "checking"; // "checking" | "login" | "waiting" | "chat"
let displayLines = [];
let scrollY = 0;
let streaming = false;
let statusMsg = "checking auth...";
let loginUrl = "";
let loginPollTimer = 0;

const SHIFT_MAP = {
  "1":"!", "2":"@", "3":"#", "4":"$", "5":"%",
  "6":"^", "7":"&", "8":"*", "9":"(", "0":")",
  "-":"_", "=":"+", "[":"{", "]":"}", "\\":"|",
  ";":":", "'":'"', ",":"<", ".":">", "/":"?", "`":"~",
};

function boot({ system }) {
  // Restore saved credentials from config, then check auth
  streaming = true;
  const raw = system?.config?.raw;
  let restorePart = "";
  if (raw) {
    try {
      const cfg = JSON.parse(raw);
      if (cfg.claudeCredentials) {
        restorePart = `
import { writeFileSync, mkdirSync } from 'fs';
mkdirSync('/tmp/.claude', { recursive: true });
writeFileSync('/tmp/.claude/.credentials.json', ${JSON.stringify(JSON.stringify(cfg.claudeCredentials))});
`;
      }
    } catch (e) {}
  }

  const script = `
${restorePart}
import { execSync } from 'child_process';
try {
  const out = execSync('claude auth status', {
    encoding: 'utf8', timeout: 10000,
    env: { ...process.env, HOME: '/tmp' },
  });
  process.stdout.write(out.includes('"loggedIn": true') || out.includes('"loggedIn":true') ? 'AUTH_OK' : 'AUTH_NEEDED');
} catch (e) { process.stdout.write('AUTH_NEEDED'); }
`;
  system?.execNode?.(script);
}

function startLogin(system) {
  mode = "waiting";
  streaming = true;
  statusMsg = "starting login...";
  loginUrl = "";

  // Run claude auth login, capture the URL it outputs, then wait for completion
  const script = `
import { execSync, spawn } from 'child_process';
const env = { ...process.env, HOME: '/tmp', BROWSER: '', DISPLAY: '' };
try {
  // Run login and capture output (it will block until user completes OAuth)
  const out = execSync('claude auth login 2>&1', {
    encoding: 'utf8', timeout: 300000, env,
  });
  // If we get here, login succeeded
  // Save credentials for persistence
  try {
    const creds = require('fs').readFileSync('/tmp/.claude/.credentials.json', 'utf8');
    // Output both success marker and credentials
    process.stdout.write('LOGIN_OK:' + creds);
  } catch (e) {
    process.stdout.write('LOGIN_OK:{}');
  }
} catch (e) {
  const output = (e.stdout || '') + (e.stderr || '');
  // Extract URL from output
  const match = output.match(/visit:\\s*(https:\\/\\/[^\\s]+)/i);
  if (match) {
    process.stdout.write('LOGIN_URL:' + match[1]);
  } else {
    process.stderr.write('login failed: ' + (e.message || '').slice(0, 200));
    process.exit(1);
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

    if (mode === "checking" || mode === "waiting") return;

    // Login screen: press enter to start login
    if (mode === "login") {
      if (key === "enter" || key === "return") {
        startLogin(system);
      }
      return;
    }

    // Chat mode
    if (key === "enter" || key === "return") {
      const cmd = input.trim();
      if (cmd.length === 0) return;
      input = "";

      if (cmd === "/clear") {
        displayLines = [];
        scrollY = 0;
        statusMsg = "cleared";
        return;
      }
      if (cmd === "/logout") {
        streaming = true;
        statusMsg = "logging out...";
        const script = `
import { execSync } from 'child_process';
import { unlinkSync } from 'fs';
try {
  execSync('claude auth logout 2>&1', {
    encoding: 'utf8', timeout: 10000,
    env: { ...process.env, HOME: '/tmp' },
  });
} catch (e) {}
try { unlinkSync('/tmp/.claude/.credentials.json'); } catch (e) {}
process.stdout.write('LOGGED_OUT');
`;
        system?.execNode?.(script);
        system?.saveConfig?.("claudeCredentials", null);
        return;
      }
      if (cmd === "/login") {
        mode = "login";
        statusMsg = "press enter to start login";
        return;
      }

      if (streaming) { statusMsg = "waiting..."; return; }

      addDisplayLine("you", cmd);
      sendMessage(cmd, system);
      return;
    }

    if (key === "backspace") { input = input.slice(0, -1); return; }
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
      input += shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
    }
  }
}

function sendMessage(prompt, system) {
  streaming = true;
  statusMsg = "thinking...";
  const escaped = prompt.replace(/\\/g, "\\\\").replace(/'/g, "'\\''");
  const script = `
import { execSync } from 'child_process';
try {
  const out = execSync("claude -p '" + ${JSON.stringify(escaped)} + "'", {
    encoding: 'utf8', timeout: 120000,
    env: { ...process.env, HOME: '/tmp' },
  });
  process.stdout.write(out);
} catch (e) {
  process.stderr.write(e.stderr || e.message || 'failed');
  process.exit(1);
}
`;
  system?.execNode?.(script);
}

function sim({ system }) {
  if (system?.fetchPending === false && streaming) {
    streaming = false;
    const result = system?.fetchResult;
    const error = system?.fetchError;

    // Auth check result
    if (result === "AUTH_OK") {
      mode = "chat";
      statusMsg = "ready";
      return;
    }
    if (result === "AUTH_NEEDED") {
      mode = "login";
      statusMsg = "press enter to login with your claude account";
      return;
    }

    // Login URL received (login is still in progress)
    if (result && result.startsWith("LOGIN_URL:")) {
      loginUrl = result.slice(10);
      mode = "waiting";
      statusMsg = "open this URL on your phone to login";
      // The login command is still running in background via execNode
      // It will complete when the user finishes OAuth
      // We need to poll for completion
      loginPollTimer = 0;
      return;
    }

    // Login completed
    if (result && result.startsWith("LOGIN_OK:")) {
      const credsStr = result.slice(9);
      try {
        const creds = JSON.parse(credsStr);
        system?.saveConfig?.("claudeCredentials", creds);
      } catch (e) {}
      mode = "chat";
      loginUrl = "";
      statusMsg = "logged in — ready";
      return;
    }

    // Logout
    if (result === "LOGGED_OUT") {
      mode = "login";
      statusMsg = "logged out — press enter to login";
      displayLines = [];
      return;
    }

    // Chat response
    if (error) {
      const emsg = error.length > 60 ? error.slice(0, 57) + "..." : error;
      if (error.includes("auth") || error.includes("401") || error.includes("credentials")) {
        mode = "login";
        statusMsg = "session expired — press enter to re-login";
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

  // Poll login completion while waiting
  if (mode === "waiting" && !streaming) {
    loginPollTimer++;
    if (loginPollTimer % 120 === 0) { // every ~2 seconds at 60fps
      streaming = true;
      const script = `
import { execSync } from 'child_process';
try {
  const out = execSync('claude auth status', {
    encoding: 'utf8', timeout: 5000,
    env: { ...process.env, HOME: '/tmp' },
  });
  if (out.includes('"loggedIn": true') || out.includes('"loggedIn":true')) {
    // Save credentials
    try {
      const creds = require('fs').readFileSync('/tmp/.claude/.credentials.json', 'utf8');
      process.stdout.write('LOGIN_OK:' + creds);
    } catch (e) { process.stdout.write('AUTH_OK'); }
  } else {
    process.stdout.write('STILL_WAITING');
  }
} catch (e) { process.stdout.write('STILL_WAITING'); }
`;
      system?.execNode?.(script);
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

function paint({ wipe, ink, box, line, write, screen }) {
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
  const title = mode === "login" ? "claude :: login"
              : mode === "waiting" ? "claude :: waiting for login..."
              : mode === "checking" ? "claude"
              : "claude";
  write(title, { x: x0, y: y0, size: 1, font: "6x10" });

  // Login screen
  if (mode === "login") {
    let hy = y0 + lineH + 12;
    ink(160, 160, 180);
    write("login with your claude account", { x: x0, y: hy, size: 1, font: "6x10" });
    hy += lineH + 8;
    ink(100, 180, 120);
    write("press [enter] to start", { x: x0, y: hy, size: 1, font: "6x10" });
    hy += lineH + 4;
    ink(100, 100, 120);
    write("a login URL will appear —", { x: x0, y: hy, size: 1, font: "6x10" });
    hy += lineH;
    write("open it on your phone to sign in", { x: x0, y: hy, size: 1, font: "6x10" });
  }

  // Waiting for OAuth — show URL
  if (mode === "waiting" && loginUrl) {
    let hy = y0 + lineH + 8;
    ink(200, 180, 100);
    write("open this URL on your phone:", { x: x0, y: hy, size: 1, font: "6x10" });
    hy += lineH + 4;

    // Wrap the URL across lines
    const urlChars = Math.floor((W - x0 * 2) / charW);
    for (let i = 0; i < loginUrl.length; i += urlChars) {
      ink(120, 180, 255);
      write(loginUrl.slice(i, i + urlChars), { x: x0, y: hy, size: 1, font: "6x10" });
      hy += lineH;
    }
    hy += 4;
    ink(100, 100, 120);
    const dots = ".".repeat((Math.floor(cursorFrame / 20) % 3) + 1);
    write("waiting for you to sign in" + dots, { x: x0, y: hy, size: 1, font: "6x10" });
  }

  // Display lines (chat)
  if (mode === "chat" || displayLines.length > 0) {
    const inputY = H - 30;
    const displayAreaTop = y0 + lineH + 4;
    const maxVisible = Math.floor((inputY - displayAreaTop) / lineH);
    const startIdx = Math.max(0, displayLines.length - maxVisible - scrollY);
    const endIdx = Math.min(displayLines.length, startIdx + maxVisible);

    let dy = displayAreaTop;
    for (let i = startIdx; i < endIdx; i++) {
      const dl = displayLines[i];
      if (dl.role === "sep") { dy += 4; continue; }
      if (dl.role === "you") ink(180, 200, 140);
      else ink(160, 180, 220);
      write(dl.text, { x: x0, y: dy, size: 1, font: "6x10" });
      dy += lineH;
    }
  }

  // Input line (chat mode only)
  if (mode === "chat") {
    const inputY = H - 30;
    ink(220, 220, 230);
    write(input, { x: x0, y: inputY, size: 1, font: "6x10" });
    if (cursorVisible) {
      ink(100, 160, 255);
      box(x0 + input.length * charW, inputY, charW, charH, true);
    }
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
