// link.mjs — AC Native identity pairing piece
//
// Two flows:
//   `link`           — device generates a code, displays it, polls for
//                      a claim. Type `link CODE` on prompt.ac (logged in)
//                      to claim it.
//   `link ABCD12`    — device polls an already-displayed web code that the
//                      user generated on prompt.ac via the `link` piece.
//
// On success: writes /mnt/config.json (handle, sub, email, token, optional
// claudeToken/githubPat), writes /claude-token + /github-pat, then reboots
// so init.sh re-stages everything from a clean state.

const API_BASE = "https://aesthetic.computer/.netlify/functions/device-pair";
let state = "init"; // init | creating | polling | success | error
let pairCode = "";
let message = "";
let frame = 0;
let pollAttempts = 0;
let newHandle = "";
let successFrame = 0;
let createdLocally = false; // true when the device generated the code itself
let lastFetchAt = 0;

function startCreate(system) {
  state = "creating";
  pairCode = "";
  pollAttempts = 0;
  createdLocally = true;
  lastFetchAt = frame;
  const body = JSON.stringify({ action: "create" });
  const headers = JSON.stringify({ "Content-Type": "application/json" });
  system?.fetchPost?.(API_BASE, body, headers);
}

function startPoll(system, code) {
  state = "polling";
  pairCode = (code || "").toUpperCase();
  pollAttempts = 0;
  lastFetchAt = frame;
  system?.fetch?.(API_BASE + "?code=" + pairCode);
}

function boot({ system, params, colon }) {
  // Get code from params: `link ABCD12` or `link:ABCD12`
  const code = params?.[0] || colon?.[0] || "";
  if (code && code.length >= 4) {
    // Consumer mode — claim a web-generated code.
    startPoll(system, code);
  } else {
    // Producer mode — generate a code on this device, wait for claim.
    startCreate(system);
  }
}

function act({ event: e, system }) {
  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (state !== "success") {
      system?.jump?.("prompt");
    }
  }

  // Retry on error
  if (state === "error" && (e.is("keyboard:down:enter") || e.is("keyboard:down:return"))) {
    if (pairCode) {
      startPoll(system, pairCode);
    } else if (createdLocally) {
      startCreate(system);
    } else {
      system?.jump?.("prompt");
    }
  }
}

function drawHints(ink, write, T, h) {
  if (state !== "success") {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write("esc: back", { x: 10, y: h - 12, size: 1, font: "font_1" });
  }
}

function paint({ wipe, ink, write, screen, system, sound }) {
  frame++;
  const T = __theme.update();
  const h = screen.height;
  const pad = 10;
  const font = "font_1";

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Title
  ink(T.fg, T.fg + 10, T.fg);
  write("link", { x: pad, y: 10, size: 2, font: "matrix" });

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

  // Creating: posting device-pair {action:"create"} to get a fresh code.
  if (state === "creating") {
    const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
    ink(120, 180, 255);
    write("requesting code" + dots, { x: pad, y: stateY, size: 1, font });

    if (system?.fetchResult) {
      try {
        const data = JSON.parse(system.fetchResult);
        if (data.code) {
          startPoll(system, data.code);
        } else {
          state = "error";
          message = data.message || "no code in response";
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
    return drawHints(ink, write, T, h);
  }

  // Poll for claimed credentials.
  if (state === "polling") {
    if (createdLocally) {
      // Producer mode — code is the call to action.
      ink(255, 220, 80);
      write(pairCode, { x: pad, y: stateY, size: 3, font: "matrix" });
      ink(160, 200, 255);
      write("on prompt.ac type: link " + pairCode, { x: pad, y: stateY + 30, size: 1, font });
      const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
      ink(T.fgMute, T.fgMute + 10, T.fgMute);
      write("waiting" + dots, { x: pad, y: stateY + 46, size: 1, font });
    } else {
      // Consumer mode — code came from the web, just confirm we're checking.
      const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
      ink(120, 180, 255);
      write("checking code" + dots, { x: pad, y: stateY, size: 1, font });
      ink(255, 255, 255);
      write(pairCode, { x: pad, y: stateY + 16, size: 2, font: "matrix" });
    }

    if (system?.fetchResult) {
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
          pollAttempts++;
          // 10-min server TTL ÷ ~2s polls = ~300 attempts before expiry.
          if (pollAttempts >= 300) {
            state = "error";
            message = "code expired — try again";
          }
        } else if (data.message) {
          state = "error";
          message = data.message;
        } else {
          state = "error";
          message = "code not found or expired";
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

    // Re-poll every ~2 seconds (120 frames at 60fps).
    if (state === "polling" && (frame - lastFetchAt) >= 120) {
      lastFetchAt = frame;
      system?.fetch?.(API_BASE + "?code=" + pairCode);
    }
    return drawHints(ink, write, T, h);
  }

  // Success — reboot
  if (state === "success") {
    ink(80, 255, 120);
    write("linked as @" + newHandle, { x: pad, y: stateY, size: 2, font: "matrix" });

    const elapsed = frame - successFrame;
    if (elapsed < 120) {
      ink(200, 200, 100);
      write("rebooting in " + Math.ceil((120 - elapsed) / 60) + "...", { x: pad, y: stateY + 24, size: 1, font });
    } else {
      system?.reboot?.();
    }
    return;
  }

  // Error state
  if (state === "error") {
    ink(255, 80, 80);
    write("error", { x: pad, y: stateY, size: 1, font });
    ink(200, 120, 100);
    write(message, { x: pad, y: stateY + 14, size: 1, font });
    ink(T.fgMute);
    if (pairCode || createdLocally) {
      write("enter: retry  esc: back", { x: pad, y: stateY + 30, size: 1, font });
    } else {
      write("link        — generate a code here", { x: pad, y: stateY + 30, size: 1, font });
      write("link CODE   — claim a code from prompt.ac", { x: pad, y: stateY + 44, size: 1, font });
      write("esc: back", { x: pad, y: stateY + 60, size: 1, font });
    }
    return drawHints(ink, write, T, h);
  }

}

function sim() {}

export { boot, paint, act, sim };
