// claude.mjs — Claude Code with built-in device auth curtain
// If valid credentials exist → launch terminal with claude.
// Otherwise → show device code + QR, poll for phone auth, then launch.

const API_URL = "https://aesthetic.computer/api/device-auth";
let mode = "auth"; // auth | terminal
let state = "checking"; // checking | requesting | waiting | approved | error
let code = "";
let authUrl = "";
let pollUrl = "";
let error = "";
let frame = 0;
let pollFrame = 0;

function boot() {
  // Don't do anything in boot — all logic in paint where APIs are reliable
  console.log("[claude] boot");
}

function paint({ wipe, ink, box, write, screen, system, wifi }) {
  frame++;
  if (mode === "terminal") return;

  var W = screen.width;
  var H = screen.height;
  var font = "6x10";

  // Dark background
  wipe(20, 20, 30);

  // Title
  ink(100, 180, 255);
  write("claude", { x: 10, y: 10, size: 2, font: "matrix" });

  // One-time credential check on first frame
  if (state === "checking") {
    console.log("[claude] checking credentials...");
    var hasCreds = false;
    try {
      var raw = system.readFile("/mnt/claude-credentials.json");
      console.log("[claude] readFile returned: " + (raw ? raw.length + " bytes" : "null"));
      if (raw && raw.length > 20) {
        var parsed = JSON.parse(raw);
        if (parsed && parsed.claudeAiOauth && parsed.claudeAiOauth.accessToken) {
          hasCreds = true;
          console.log("[claude] valid credentials found");
        } else {
          console.log("[claude] file exists but no accessToken, clearing");
          system.writeFile("/mnt/claude-credentials.json", "");
        }
      }
    } catch (e) {
      console.log("[claude] creds error: " + e);
      try { system.writeFile("/mnt/claude-credentials.json", ""); } catch (_) {}
    }

    if (hasCreds) {
      mode = "terminal";
      system.jump("terminal:claude");
      return;
    }

    // No valid creds — start device auth
    if (wifi && wifi.connected) {
      state = "requesting";
      console.log("[claude] requesting device code...");
      system.fetch(API_URL + "?action=request");
    } else {
      state = "error";
      error = "connect to wifi first";
      console.log("[claude] no wifi");
    }
    return;
  }

  if (state === "requesting") {
    ink(140, 140, 160);
    var dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    write("getting code" + dots, { x: 10, y: 40, size: 1, font: font });

    if (system.fetchResult) {
      try {
        var data = JSON.parse(system.fetchResult);
        console.log("[claude] got response: " + system.fetchResult.substring(0, 100));
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
        console.log("[claude] parse error: " + e);
      }
    }
    if (system.fetchError) {
      state = "error";
      error = "network error";
    }

  } else if (state === "waiting") {
    ink(220, 220, 230);
    write("scan to login:", { x: 10, y: 40, size: 1, font: font });

    // Code in large text
    ink(255, 200, 60);
    write(code, { x: 10, y: 56, size: 2, font: "matrix" });

    // URL below code
    ink(100, 180, 255);
    var shortUrl = authUrl.replace("https://", "");
    write(shortUrl, { x: 10, y: 80, size: 1, font: font });

    // QR code (bottom-right corner)
    if (system.qrEncode && authUrl) {
      var qrData = system.qrEncode(authUrl);
      if (qrData && qrData.size > 0) {
        var qrSz = qrData.size;
        var maxQr = Math.min(W / 3, H / 2);
        var scale = Math.floor(maxQr / (qrSz + 4));
        if (scale < 1) scale = 1;
        if (scale > 4) scale = 4;
        var totalPx = (qrSz + 4) * scale;
        var qrX = W - totalPx - 6;
        var qrY = H - totalPx - 6;
        // White background with quiet zone
        ink(255, 255, 255);
        box(qrX, qrY, totalPx, totalPx, true);
        // Draw dark modules
        ink(0, 0, 0);
        for (var my = 0; my < qrSz; my++) {
          for (var mx = 0; mx < qrSz; mx++) {
            if (qrData.modules[my * qrSz + mx]) {
              box(qrX + (mx + 2) * scale, qrY + (my + 2) * scale, scale, scale, true);
            }
          }
        }
        // Label above QR
        ink(100, 100, 120);
        write("scan with phone", { x: qrX, y: qrY - 12, size: 1, font: font });
      }
    }

    // Polling status
    var dots2 = ".".repeat((Math.floor(frame / 30) % 3) + 1);
    ink(100, 100, 120);
    write("waiting for login" + dots2, { x: 10, y: H - 30, size: 1, font: font });

    // Expiry countdown
    var elapsed = Math.floor((frame - pollFrame) / 60);
    var remaining = Math.max(0, 600 - elapsed);
    var min = Math.floor(remaining / 60);
    var sec = remaining % 60;
    write("expires in " + min + ":" + (sec < 10 ? "0" : "") + sec, { x: 10, y: H - 16, size: 1, font: font });

    // Poll every 2 seconds
    if (frame % 120 === 60 && !system.fetchPending) {
      system.fetch(pollUrl);
    }

    // Check poll result
    if (system.fetchResult && frame > pollFrame + 30) {
      try {
        var pdata = JSON.parse(system.fetchResult);
        if (pdata.status === "approved" && pdata.credentials) {
          try {
            system.writeFile("/mnt/claude-credentials.json", JSON.stringify(pdata.credentials));
          } catch (e) {
            console.log("[claude] save error: " + e);
          }
          state = "approved";
        }
      } catch (e) {}
    }

    if (remaining <= 0) {
      state = "error";
      error = "code expired - try again";
    }

  } else if (state === "approved") {
    ink(80, 255, 120);
    write("logged in!", { x: 10, y: 40, size: 2, font: "matrix" });
    ink(140, 140, 160);
    write("launching claude...", { x: 10, y: 70, size: 1, font: font });
    if (frame % 120 === 0) {
      mode = "terminal";
      system.jump("terminal:claude");
    }

  } else if (state === "error") {
    ink(255, 80, 80);
    write(error, { x: 10, y: 40, size: 1, font: font });
    ink(100, 100, 120);
    write("enter: retry  esc: back", { x: 10, y: 60, size: 1, font: font });
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
