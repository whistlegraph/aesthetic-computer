// os.mjs — AC Native OS management piece
// Shows current version, checks for updates, downloads + flashes + reboots.
// Jumped to from prompt.mjs via "os" command or from notepat OS button.

const OS_BASE_URL = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/";
const OS_VERSION_URL = OS_BASE_URL + "native-notepat-latest.version";
const OS_VMLINUZ_URL = OS_BASE_URL + "native-notepat-latest.vmlinuz";
const OS_VMLINUZ_BYTES = 37_000_000;

let state = "idle"; // idle | checking | up-to-date | available | downloading | flashing | rebooting | error
let currentVersion = "";
let remoteVersion = "";
let progress = 0;
let errorMsg = "";
let fetchPending = false;
let checkFrame = 0;
let flashTargetIdx = 0;
let frame = 0;

function boot({ system }) {
  currentVersion = system?.version || "unknown";
  // Auto-check on boot if online
  if (system?.fetchPending === false) {
    state = "checking";
    fetchPending = true;
    checkFrame = 0;
    system?.fetch?.(OS_VERSION_URL);
  }
}

function act({ event: e, sound, system }) {
  if (!e.is("touch") && !e.is("keyboard:down")) return;

  // Escape goes back to prompt
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }

  if (!e.is("touch")) return;
  const { x, y } = e;
  const w = e.screen?.width || 320;

  if (state === "available") {
    // Cycle target button
    const cb = globalThis.__osCycleBtn;
    if (cb && y >= cb.y && y <= cb.y + cb.h && x >= cb.x && x <= cb.x + cb.w) {
      const targets = system?.flashTargets || [];
      flashTargetIdx = (flashTargetIdx + 1) % Math.max(1, targets.length);
      sound?.synth({ type: "sine", tone: 440, duration: 0.04, volume: 0.12, attack: 0.002, decay: 0.03 });
      return;
    }
    // Update now button
    const ub = globalThis.__osUpdateBtn;
    if (ub && y >= ub.y && y <= ub.y + ub.h && x >= ub.x && x <= ub.x + ub.w) {
      const targets = system?.flashTargets || [];
      const tgt = targets[flashTargetIdx];
      const device = tgt?.device || undefined;
      globalThis.__osFlashDevice = device;
      state = "downloading";
      progress = 0;
      system?.fetchBinary?.(OS_VMLINUZ_URL, "/tmp/vmlinuz.new", OS_VMLINUZ_BYTES);
      return;
    }
  }

  // Tap to retry on error or re-check when up-to-date/idle
  if (state === "error" || state === "up-to-date" || state === "idle") {
    if (!system?.fetchPending) {
      state = "checking";
      fetchPending = true;
      checkFrame = frame;
      system?.fetch?.(OS_VERSION_URL);
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, system, wifi }) {
  frame++;
  const w = screen.width, h = screen.height;
  const dark = true;

  wipe(dark ? 12 : 240, dark ? 16 : 235, dark ? 24 : 230);

  const pad = 10;

  // Title
  ink(200, 220, 200);
  write("ac/native", { x: pad, y: 10, size: 2, font: "matrix" });

  // Connection status
  if (!wifi?.connected) {
    ink(200, 80, 80);
    write("offline", { x: pad, y: 34, size: 1, font: "font_1" });
    ink(100, 80, 80);
    write("connect wifi first", { x: pad, y: 46, size: 1, font: "font_1" });
  } else {
    ink(100, 110, 100);
    write("current", { x: pad, y: 34, size: 1, font: "font_1" });
    ink(180, 180, 180);
    const maxChars = Math.floor((w - pad * 2) / 6) - 10;
    write(currentVersion.slice(0, maxChars), { x: pad, y: 46, size: 1, font: "font_1" });
  }

  const stateY = 66;

  if (state === "checking") {
    ink(200, 200, 80);
    write("checking...", { x: pad, y: stateY, size: 1, font: "font_1" });
  } else if (state === "up-to-date") {
    ink(80, 200, 120);
    write("up to date!", { x: pad, y: stateY, size: 1, font: "font_1" });
    ink(100, 110, 100);
    write(remoteVersion, { x: pad, y: stateY + 14, size: 1, font: "font_1" });
  } else if (state === "available") {
    ink(100, 110, 100);
    write("available", { x: pad, y: stateY, size: 1, font: "font_1" });
    ink(255, 200, 60);
    write(remoteVersion, { x: pad, y: stateY + 14, size: 1, font: "font_1" });

    // Flash target selector
    const targets = system?.flashTargets || [];
    if (targets.length > 0) {
      if (flashTargetIdx >= targets.length) flashTargetIdx = 0;
      const tgt = targets[flashTargetIdx];
      const tgtLabel = (tgt?.label || "?") + " (" + (tgt?.device || "?") + ")";
      const isBoot = tgt?.device === system?.bootDevice;
      ink(80, 100, 120);
      write("target:", { x: pad, y: stateY + 30, size: 1, font: "font_1" });
      ink(isBoot ? 80 : 200, isBoot ? 200 : 200, isBoot ? 255 : 80);
      write(tgtLabel, { x: pad + 48, y: stateY + 30, size: 1, font: "font_1" });
      if (isBoot) {
        ink(60, 140, 200);
        write("(current boot)", { x: pad, y: stateY + 42, size: 1, font: "font_1" });
      }
      if (targets.length > 1) {
        ink(120, 120, 140);
        const cycX = w - pad - 36;
        box(cycX, stateY + 28, 32, 14, true);
        ink(220, 220, 240);
        write("next", { x: cycX + 4, y: stateY + 31, size: 1, font: "font_1" });
        globalThis.__osCycleBtn = { x: cycX, y: stateY + 28, w: 32, h: 14 };
      } else {
        globalThis.__osCycleBtn = null;
      }
    }

    // Update button
    const btnW = Math.min(120, w - pad * 4);
    const btnX = Math.floor((w - btnW) / 2);
    const btnY = stateY + (targets.length > 0 ? 58 : 34);
    ink(60, 180, 100);
    box(btnX, btnY, btnW, 18, true);
    ink(240, 255, 240);
    write("update now", { x: btnX + Math.floor((btnW - 60) / 2), y: btnY + 4, size: 1, font: "font_1" });
    globalThis.__osUpdateBtn = { x: btnX, y: btnY, w: btnW, h: 18 };

  } else if (state === "downloading") {
    ink(120, 140, 120);
    write("downloading...", { x: pad, y: stateY, size: 1, font: "font_1" });
    const barW = w - pad * 2, barH = 8, barY = stateY + 16;
    ink(30, 40, 50);
    box(pad, barY, barW, barH, true);
    ink(60, 180, 100);
    box(pad, barY, Math.round(barW * (progress || 0)), barH, true);
    ink(160);
    write(Math.round((progress || 0) * 100) + "%", { x: pad, y: barY + 12, size: 1, font: "font_1" });

  } else if (state === "flashing") {
    const phase = system?.flashPhase ?? 0;
    const phaseText = phase === 1 ? "writing EFI..."
                    : phase === 2 ? "syncing to disk..."
                    : phase === 3 ? "verifying..."
                    : phase === 4 ? "done" : "preparing...";
    ink(...(phase === 3 ? [100, 200, 255] : [255, 160, 60]));
    write(phaseText, { x: pad, y: stateY, size: 1, font: "font_1" });
    ink(100);
    write("-> " + (globalThis.__osFlashDevice || system?.bootDevice || "?"), { x: pad, y: stateY + 14, size: 1, font: "font_1" });
    ink(140);
    write("do not power off", { x: pad, y: stateY + 28, size: 1, font: "font_1" });

  } else if (state === "rebooting") {
    const mb = ((system?.flashVerifiedBytes ?? 0) / 1048576).toFixed(1);
    ink(80, 255, 120);
    write(`verified ${mb}MB`, { x: pad, y: stateY, size: 1, font: "font_1" });
    ink(200, 100, 60);
    write("rebooting...", { x: pad, y: stateY + 14, size: 1, font: "font_1" });

  } else if (state === "error") {
    ink(220, 80, 80);
    write(("error: " + errorMsg).slice(0, Math.floor((w - pad * 2) / 6)), { x: pad, y: stateY, size: 1, font: "font_1" });
    ink(120);
    write("tap to retry", { x: pad, y: stateY + 14, size: 1, font: "font_1" });

  } else {
    // idle
    ink(100);
    write("tap to check for updates", { x: pad, y: stateY, size: 1, font: "font_1" });
  }

  // Bottom hint
  ink(60, 80, 60);
  write("esc: back", { x: pad, y: h - 12, size: 1, font: "font_1" });

  // === State machine: poll fetch/flash results ===

  // Version check result
  if (fetchPending && system?.fetchResult !== undefined && system?.fetchResult !== null) {
    const raw = (typeof system.fetchResult === "string" ? system.fetchResult : "").trim();
    fetchPending = false;
    if (!raw || raw.length < 5) {
      state = "error";
      errorMsg = "bad version response";
    } else {
      remoteVersion = raw;
      state = (raw === currentVersion) ? "up-to-date" : "available";
    }
  }
  if (fetchPending && system?.fetchError) {
    fetchPending = false;
    state = "error";
    errorMsg = "fetch failed";
  }
  // Timeout
  if (fetchPending && frame - checkFrame > 600) {
    fetchPending = false;
    state = "error";
    errorMsg = "timeout";
  }

  // Download progress
  if (state === "downloading") {
    progress = system?.fetchBinaryProgress ?? progress;
    if (system?.fetchBinaryDone) {
      if (system?.fetchBinaryOk) {
        state = "flashing";
        const dev = globalThis.__osFlashDevice;
        if (dev) system?.flashUpdate?.("/tmp/vmlinuz.new", dev);
        else system?.flashUpdate?.("/tmp/vmlinuz.new");
      } else {
        state = "error";
        errorMsg = "download failed";
      }
    }
  }

  // Flash progress
  if (state === "flashing" && system?.flashDone) {
    if (system?.flashOk) {
      state = "rebooting";
      globalThis.__osRebootFrame = frame + 120;
    } else {
      state = "error";
      errorMsg = "flash verify failed";
    }
  }

  // Reboot trigger
  if (globalThis.__osRebootFrame && frame >= globalThis.__osRebootFrame) {
    globalThis.__osRebootFrame = 0;
    system?.reboot?.();
  }
}

function sim() {}

export { boot, paint, act, sim };
