// login.mjs — AC Native identity switching piece
// Usage: `login ABCD12` (device key from aesthetic.computer `device-key` command)
//
// Flow:
// 1. User generates a device key on their phone (web prompt: `device-key`)
// 2. On native device, types `login ABCD12`
// 3. Device polls device-pair API with that code
// 4. Gets back handle + tokens, writes config.json, reboots

const API_BASE = "https://aesthetic.computer/.netlify/functions/device-pair";
let state = "init"; // init | polling | success | error
let pairCode = "";
let message = "";
let frame = 0;
let pollAttempts = 0;
let newHandle = "";
let successFrame = 0;

function boot({ system, params, colon }) {
  // Get code from params: `login ABCD12` or `login:ABCD12`
  const code = params?.[0] || colon?.[0] || "";
  if (!code || code.length < 4) {
    state = "error";
    message = "usage: login CODE";
    return;
  }
  pairCode = code.toUpperCase();
  state = "polling";
  pollAttempts = 0;
  system?.fetch?.(API_BASE + "?code=" + pairCode);
}

function act({ event: e, system, params, colon }) {
  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (state !== "success") {
      system?.jump?.("prompt");
    }
  }

  // Retry on error
  if (state === "error" && (e.is("keyboard:down:enter") || e.is("keyboard:down:return"))) {
    if (pairCode) {
      state = "polling";
      pollAttempts = 0;
      system?.fetch?.(API_BASE + "?code=" + pairCode);
    } else {
      system?.jump?.("prompt");
    }
  }
}

function paint({ wipe, ink, write, screen, system, sound }) {
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

  // Poll for claimed credentials
  if (state === "polling") {
    const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
    ink(120, 180, 255);
    write("checking key" + dots, { x: pad, y: stateY, size: 1, font });
    ink(255, 255, 255);
    write(pairCode, { x: pad, y: stateY + 16, size: 2, font: "matrix" });

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
          if (data.claudeToken) cfg.claudeToken = data.claudeToken;
          if (data.githubPat) cfg.githubPat = data.githubPat;

          // Write main config
          system?.writeFile?.("/mnt/config.json", JSON.stringify(cfg));

          // Write device tokens
          if (data.claudeToken) system?.writeFile?.("/claude-token", data.claudeToken);
          if (data.githubPat) system?.writeFile?.("/github-pat", data.githubPat);

          state = "success";
          successFrame = frame;
          sound?.synth?.({ type: "sine", tone: 660, duration: 0.15, volume: 0.12, attack: 0.005, decay: 0.1 });
        } else if (data.status === "pending") {
          // Not claimed yet — keep polling
          pollAttempts++;
          if (pollAttempts >= 60) {
            state = "error";
            message = "key expired — generate a new one";
          }
        } else if (data.message) {
          state = "error";
          message = data.message;
        } else {
          state = "error";
          message = "key not found or expired";
        }
      } catch (err) {
        state = "error";
        message = "bad response from server";
      }
    }
    if (system?.fetchError) {
      state = "error";
      message = "network error — check wifi";
    }

    // Re-poll every ~2 seconds (120 frames at 60fps) if still pending
    if (state === "polling" && frame % 120 === 0 && pollAttempts > 0) {
      system?.fetch?.(API_BASE + "?code=" + pairCode);
    }
  }

  // Success — reboot
  if (state === "success") {
    ink(80, 255, 120);
    write("logged in as @" + newHandle, { x: pad, y: stateY, size: 2, font: "matrix" });

    const elapsed = frame - successFrame;
    if (elapsed < 120) {
      ink(200, 200, 100);
      write("rebooting in " + Math.ceil((120 - elapsed) / 60) + "...", { x: pad, y: stateY + 24, size: 1, font });
    } else {
      system?.reboot?.();
    }
  }

  // Error state
  if (state === "error") {
    ink(255, 80, 80);
    write("error", { x: pad, y: stateY, size: 1, font });
    ink(200, 120, 100);
    write(message, { x: pad, y: stateY + 14, size: 1, font });
    ink(T.fgMute);
    if (pairCode) {
      write("enter: retry  esc: back", { x: pad, y: stateY + 30, size: 1, font });
    } else {
      write("on your phone, type: device-key", { x: pad, y: stateY + 30, size: 1, font });
      write("then here: login CODE", { x: pad, y: stateY + 44, size: 1, font });
      write("esc: back", { x: pad, y: stateY + 60, size: 1, font });
    }
  }

  // Bottom hint
  if (state !== "success") {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write("esc: back", { x: pad, y: h - 12, size: 1, font });
  }
}

function sim() {}

export { boot, paint, act, sim };
