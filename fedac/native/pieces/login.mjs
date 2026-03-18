// login.mjs — AC Native identity switching piece
// Opens browser for Auth0 login, then rewrites config.json and reboots.
//
// Flow:
// 1. POST to device-pair API → get pairing code
// 2. Open browser to device-pair-login page (Auth0 flow)
// 3. Browser closes → poll device-pair API for claimed credentials
// 4. Write new config.json → reboot

const API_BASE = "https://aesthetic.computer/.netlify/functions/device-pair";
let state = "init"; // init | requesting | waiting | browsing | polling | success | error
let pairCode = "";
let message = "";
let frame = 0;
let pollAttempts = 0;
let newHandle = "";

function boot({ system }) {
  state = "requesting";
  message = "requesting pairing code...";
  // POST to create a device pair code
  const body = JSON.stringify({ action: "create" });
  const headers = JSON.stringify({ "Content-Type": "application/json" });
  system?.fetchPost?.(API_BASE, body, headers);
}

function act({ event: e, system }) {
  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (state !== "browsing") {
      system?.jump?.("prompt");
    }
  }

  // Retry on error
  if (state === "error" && (e.is("keyboard:down:enter") || e.is("keyboard:down:return"))) {
    state = "requesting";
    message = "requesting pairing code...";
    const body = JSON.stringify({ action: "create" });
    const headers = JSON.stringify({ "Content-Type": "application/json" });
    system?.fetchPost?.(API_BASE, body, headers);
  }
}

function paint({ wipe, ink, write, screen, system }) {
  frame++;
  const T = __theme.update();
  const w = screen.width, h = screen.height;
  const pad = 10;
  const font = "font_1";

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Title
  ink(T.fg, T.fg + 10, T.fg);
  write("login", { x: pad, y: 10, size: 2, font: "matrix" });

  // Current identity
  let currentHandle = "";
  try {
    const raw = system?.readFile?.("/mnt/config.json");
    if (raw) currentHandle = JSON.parse(raw).handle || "";
  } catch (_) {}
  if (currentHandle) {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write("current: @" + currentHandle, { x: pad, y: 34, size: 1, font });
  }

  const stateY = 52;

  // Step 1: Wait for pair code from API
  if (state === "requesting") {
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    ink(200, 180, 100);
    write("requesting code" + dots, { x: pad, y: stateY, size: 1, font });

    // Poll for fetch result
    if (system?.fetchResult !== undefined && system?.fetchResult !== null) {
      try {
        const data = JSON.parse(system.fetchResult);
        if (data.code) {
          pairCode = data.code;
          state = "waiting";
          message = "";
        } else {
          state = "error";
          message = data.message || "failed to get code";
        }
      } catch (err) {
        state = "error";
        message = "bad response";
      }
    }
    if (system?.fetchError) {
      state = "error";
      message = "network error";
    }
  }

  // Step 2: Got code — open browser
  if (state === "waiting") {
    ink(255, 255, 255);
    write("code: " + pairCode, { x: pad, y: stateY, size: 2, font: "matrix" });
    ink(120, 200, 140);
    write("opening browser...", { x: pad, y: stateY + 24, size: 1, font });

    // Open browser (this blocks until Firefox exits)
    state = "browsing";
    const url = "https://aesthetic.computer/api/device-pair-login?code=" + pairCode;
    system?.openBrowser?.(url);
    // Browser closed — start polling
    state = "polling";
    pollAttempts = 0;
    message = "checking login status...";
    system?.fetch?.(API_BASE + "?code=" + pairCode);
  }

  // Step 3: Poll for claimed credentials
  if (state === "polling") {
    const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
    ink(120, 180, 255);
    write("checking" + dots, { x: pad, y: stateY, size: 1, font });
    ink(T.fgMute);
    write("code: " + pairCode, { x: pad, y: stateY + 14, size: 1, font });

    if (system?.fetchResult !== undefined && system?.fetchResult !== null) {
      try {
        const data = JSON.parse(system.fetchResult);
        if (data.status === "claimed" && data.handle) {
          // Success! Write new config
          newHandle = data.handle;
          const cfg = {
            handle: data.handle,
            piece: "notepat",
            sub: data.sub || "",
            email: data.email || "",
            token: data.token || "",
          };
          // Write main config
          system?.writeFile?.("/mnt/config.json", JSON.stringify(cfg));

          // Write device tokens if present
          if (data.claudeToken) {
            system?.writeFile?.("/claude-token", data.claudeToken);
            system?.saveConfig?.("claudeToken", data.claudeToken);
          }
          if (data.githubPat) {
            system?.writeFile?.("/github-pat", data.githubPat);
            system?.saveConfig?.("githubPat", data.githubPat);
          }

          state = "success";
          message = "identity switched to @" + data.handle;
        } else if (data.status === "pending") {
          // Not yet — retry
          pollAttempts++;
          if (pollAttempts < 30) {
            // Poll again (paint runs at ~60fps, so delay via frame count)
            if (frame % 120 === 0) {
              system?.fetch?.(API_BASE + "?code=" + pairCode);
            }
          } else {
            state = "error";
            message = "login timed out";
          }
        } else {
          state = "error";
          message = data.message || "code expired";
        }
      } catch (err) {
        state = "error";
        message = "bad response";
      }
    }
    if (system?.fetchError) {
      pollAttempts++;
      if (pollAttempts < 10) {
        // Retry
        if (frame % 120 === 0) {
          system?.fetch?.(API_BASE + "?code=" + pairCode);
        }
      } else {
        state = "error";
        message = "network error during poll";
      }
    }
  }

  // Step 4: Success — reboot countdown
  if (state === "success") {
    ink(80, 255, 120);
    write("logged in as @" + newHandle, { x: pad, y: stateY, size: 2, font: "matrix" });
    ink(200, 200, 100);
    write("rebooting...", { x: pad, y: stateY + 24, size: 1, font });

    // Reboot after a brief pause
    if (frame % 180 === 0) {
      system?.reboot?.();
    }
  }

  // Error state
  if (state === "error") {
    ink(255, 80, 80);
    write("error: " + message, { x: pad, y: stateY, size: 1, font });
    ink(T.fgMute);
    write("enter: retry  esc: back", { x: pad, y: stateY + 14, size: 1, font });
  }

  // Bottom hint
  if (state !== "browsing" && state !== "success") {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write("esc: back", { x: pad, y: h - 12, size: 1, font });
  }
}

function sim() {}

export { boot, paint, act, sim };
