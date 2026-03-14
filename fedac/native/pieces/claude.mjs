// claude.mjs — Claude Code with built-in device auth curtain
// If credentials exist → launch terminal with claude.
// Otherwise → show device code + QR, poll for phone auth, then launch.

const API_URL = "https://aesthetic.computer/api/device-auth";
let mode = "checking"; // checking | auth | launching | terminal
let state = "requesting"; // requesting | waiting | approved | error
let code = "";
let authUrl = "";
let pollUrl = "";
let error = "";
let frame = 0;
let pollFrame = 0;

function boot({ system, wifi }) {
  // Check if Claude credentials already exist
  let hasCreds = false;
  try {
    const creds = system.readFile("/mnt/claude-credentials.json");
    if (creds && creds.length > 10) hasCreds = true;
  } catch (e) { /* file doesn't exist */ }

  if (hasCreds) {
    // Credentials found — skip curtain, go straight to terminal
    console.log("[claude] credentials found, launching terminal");
    mode = "terminal";
    system.jump("terminal:claude");
    return;
  }
  console.log("[claude] no credentials, showing auth curtain");

  // No credentials — show auth curtain
  mode = "auth";
  if (!wifi || !wifi.connected) {
    state = "error";
    error = "connect to wifi first";
    return;
  }
  system.fetch(API_URL + "?action=request");
}

function paint({ wipe, ink, box, write, qr, screen, system, wifi }) {
  if (mode === "terminal") return; // jumped away
  frame++;
  const T = __theme.update();
  const W = screen.width, H = screen.height;
  const font = "6x10";

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Title
  ink(T.accent[0], T.accent[1], T.accent[2]);
  write("claude", { x: 10, y: 10, size: 2, font: "matrix" });

  if (state === "requesting") {
    ink(T.fgDim, T.fgDim, T.fgDim);
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    write("getting code" + dots, { x: 10, y: 40, size: 1, font });

    if (system.fetchResult) {
      try {
        const data = JSON.parse(system.fetchResult);
        if (data.code) {
          code = data.code;
          authUrl = data.authUrl;
          pollUrl = data.pollUrl;
          state = "waiting";
          pollFrame = frame;
        } else {
          state = "error";
          error = data.error || "no code returned";
        }
      } catch (e) {
        state = "error";
        error = "bad response";
      }
    }
    if (system.fetchError) {
      state = "error";
      error = "network error";
    }

  } else if (state === "waiting") {
    ink(T.fg, T.fg, T.fg);
    write("scan to login:", { x: 10, y: 40, size: 1, font });

    // Code in large text
    ink(T.warn[0], T.warn[1], T.warn[2]);
    write(code, { x: 10, y: 56, size: 2, font: "matrix" });

    // URL below code
    ink(T.link[0], T.link[1], T.link[2]);
    const shortUrl = authUrl.replace("https://", "");
    write(shortUrl, { x: 10, y: 80, size: 1, font });

    // QR code (right side)
    if (typeof qr === "function" && authUrl) {
      const qrScale = 3;
      const modules = authUrl.length < 80 ? 33 : 41;
      const qrSize = (modules + 4) * qrScale;
      const qrX = W - qrSize - 10;
      const qrY = 36;
      ink(255, 255, 255);
      box(qrX - 6, qrY - 6, qrSize + 12, qrSize + 12);
      qr(authUrl, qrX, qrY, qrScale);
    }

    // Polling status
    const dots = ".".repeat((Math.floor(frame / 30) % 3) + 1);
    ink(T.fgMute, T.fgMute, T.fgMute);
    write("waiting for login" + dots, { x: 10, y: H - 30, size: 1, font });

    // Expiry countdown
    const elapsed = Math.floor((frame - pollFrame) / 60);
    const remaining = Math.max(0, 600 - elapsed);
    const min = Math.floor(remaining / 60);
    const sec = remaining % 60;
    ink(T.fgMute, T.fgMute, T.fgMute);
    write(`expires in ${min}:${String(sec).padStart(2, "0")}`, { x: 10, y: H - 16, size: 1, font });

    // Poll every 2 seconds
    if (frame % 120 === 60 && !system.fetchPending) {
      system.fetch(pollUrl);
    }

    // Check poll result
    if (system.fetchResult && frame > pollFrame + 30) {
      try {
        const data = JSON.parse(system.fetchResult);
        if (data.status === "approved" && data.credentials) {
          try {
            system.writeFile("/mnt/claude-credentials.json", JSON.stringify(data.credentials));
          } catch (e) {
            console.log("[claude] Failed to save credentials:", e);
          }
          state = "approved";
        }
      } catch (e) {
        // Ignore parse errors during polling
      }
    }

    if (remaining <= 0) {
      state = "error";
      error = "code expired - try again";
    }

  } else if (state === "approved") {
    ink(T.ok[0], T.ok[1], T.ok[2]);
    write("logged in!", { x: 10, y: 40, size: 2, font: "matrix" });
    ink(T.fgDim, T.fgDim, T.fgDim);
    write("launching claude...", { x: 10, y: 70, size: 1, font });

    // Launch after brief pause
    if (frame % 120 === 0) {
      mode = "terminal";
      system.jump("terminal:claude");
    }

  } else if (state === "error") {
    ink(T.err[0], T.err[1], T.err[2]);
    write(error, { x: 10, y: 40, size: 1, font });
    ink(T.fgMute, T.fgMute, T.fgMute);
    write("enter: retry  esc: back", { x: 10, y: 60, size: 1, font });
  }
}

function act({ event: e, system, wifi }) {
  if (mode === "terminal") return;
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (state !== "approved") {
      system.jump("prompt");
    }
  }
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    if (state === "error") {
      state = "requesting";
      if (wifi && wifi.connected) {
        system.fetch(API_URL + "?action=request");
      } else {
        error = "connect to wifi first";
      }
    }
  }
}

export { boot, paint, act };
