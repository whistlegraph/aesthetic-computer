// os.mjs — AC Native OS management piece
// Shows current version, checks for updates, downloads + flashes + reboots.
// Jumped to from prompt.mjs via "os" command or from notepat OS button.

const OS_BASE_URL = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/";
const OS_VERSION_URL = OS_BASE_URL + "native-notepat-latest.version";
const OS_VMLINUZ_URL = OS_BASE_URL + "native-notepat-latest.vmlinuz";
const OS_VMLINUZ_BYTES = 37_000_000;

// States: idle | checking | up-to-date | available | downloading | flashing
//         | confirm-reboot | shutting-down | error
let state = "idle";
let currentVersion = "";
let remoteVersion = "";
let progress = 0;
let errorMsg = "";
let fetchPending = false;
let checkFrame = 0;
let flashTargetIdx = 0;
let frame = 0;
let shutdownFrame = 0; // frame counter for shutdown animation
let flashedMB = 0;     // verified MB for display during confirm/shutdown

// Telemetry lines that scroll by during flash
const telemetry = [];
let telemetryScroll = 0;

function addTelemetry(msg) {
  telemetry.push({ text: msg, frame, y: 0 });
  if (telemetry.length > 50) telemetry.shift();
}

function boot({ system }) {
  currentVersion = system?.version || "unknown";
  // Default flash target: prefer non-boot device (e.g., NVMe when booting from USB)
  const targets = system?.flashTargets || [];
  const bootDev = system?.bootDevice;
  const nonBootIdx = targets.findIndex(t => t.device !== bootDev);
  if (nonBootIdx >= 0) flashTargetIdx = nonBootIdx;
  // Auto-check on boot if online
  if (system?.fetchPending === false) {
    state = "checking";
    fetchPending = true;
    checkFrame = 0;
    system?.fetch?.(OS_VERSION_URL);
  }
}

function act({ event: e, sound, system }) {
  if (!e.is("keyboard:down")) return;

  // Escape goes back to prompt (not during flash or shutdown)
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (state !== "flashing" && state !== "shutting-down" && state !== "downloading") {
      system?.jump?.("prompt");
      return;
    }
  }

  // Reboot confirmation: y = reboot, n = back to prompt
  if (state === "confirm-reboot") {
    if (e.is("keyboard:down:y") || e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
      state = "shutting-down";
      shutdownFrame = 0;
      sound?.synth({ type: "triangle", tone: 784, duration: 0.1, volume: 0.15, attack: 0.003, decay: 0.08 });
      return;
    }
    if (e.is("keyboard:down:n") || e.is("keyboard:down:escape")) {
      system?.jump?.("prompt");
      return;
    }
    return;
  }

  // Touch-like key shortcuts for available state
  if (state === "available") {
    if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
      const targets = system?.flashTargets || [];
      const tgt = targets[flashTargetIdx];
      const device = tgt?.device || undefined;
      globalThis.__osFlashDevice = device;
      state = "downloading";
      progress = 0;
      telemetry.length = 0;
      addTelemetry("fetching " + OS_VMLINUZ_URL.split("/").pop());
      system?.fetchBinary?.(OS_VMLINUZ_URL, "/tmp/vmlinuz.new", OS_VMLINUZ_BYTES);
      return;
    }
    if (e.is("keyboard:down:tab")) {
      const targets = system?.flashTargets || [];
      flashTargetIdx = (flashTargetIdx + 1) % Math.max(1, targets.length);
      sound?.synth({ type: "sine", tone: 440, duration: 0.04, volume: 0.12, attack: 0.002, decay: 0.03 });
      return;
    }
  }

  // Tap to retry on error or re-check when up-to-date/idle
  if (state === "error" || state === "up-to-date" || state === "idle") {
    if (e.is("keyboard:down:enter") || e.is("keyboard:down:return") || e.is("keyboard:down:space")) {
      if (!system?.fetchPending) {
        state = "checking";
        fetchPending = true;
        checkFrame = frame;
        system?.fetch?.(OS_VERSION_URL);
      }
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, system, wifi }) {
  frame++;
  const w = screen.width, h = screen.height;
  const pad = 10;
  const font = "font_1";

  // === Shutdown animation (full-screen takeover) ===
  if (state === "shutting-down") {
    shutdownFrame++;
    const t = shutdownFrame / 120; // 2 second animation

    // Background: dark blue fading to black
    const bg = Math.max(0, Math.floor(20 * (1 - t)));
    wipe(0, bg, Math.floor(bg * 1.5));

    // Scrolling telemetry lines flying upward
    for (let i = 0; i < telemetry.length; i++) {
      const entry = telemetry[i];
      const baseY = h - (shutdownFrame - i * 3) * 2;
      if (baseY < -10 || baseY > h + 10) continue;
      const alpha = Math.max(0, Math.min(255, Math.floor(200 * (1 - t))));
      const green = 80 + (i * 37 % 120);
      ink(60, green, 100, alpha);
      write(entry.text, { x: pad + (i % 3) * 2, y: baseY, size: 1, font });
    }

    // Central message
    if (t < 0.8) {
      const pulse = Math.floor(180 + 75 * Math.sin(shutdownFrame * 0.2));
      ink(pulse, 255, pulse, Math.floor(255 * (1 - t / 0.8)));
      write("launching new os", { x: pad, y: h / 2 - 20, size: 2, font: "matrix" });

      // Build name
      const buildName = remoteVersion.split(" ")[0] || "update";
      ink(255, 200, 60, Math.floor(200 * (1 - t / 0.8)));
      write(buildName, { x: pad, y: h / 2 + 4, size: 1, font });
    }

    // Verified size
    if (t < 0.6) {
      ink(80, 200, 120, Math.floor(180 * (1 - t / 0.6)));
      write(`verified ${flashedMB}MB`, { x: pad, y: h / 2 + 20, size: 1, font });
    }

    // Progress bar shrinking to nothing
    const barW = Math.max(0, Math.floor((w - pad * 2) * (1 - t)));
    if (barW > 0) {
      ink(60, 180, 100, Math.floor(200 * (1 - t)));
      box(pad, h - 8, barW, 4, true);
    }

    // Trigger reboot after animation
    if (shutdownFrame >= 150) { // 2.5 seconds
      system?.reboot?.();
    }
    return;
  }

  // === Normal UI ===
  wipe(12, 16, 24);

  // Responsive: use half-width columns on wide screens
  const wide = w > 260;
  const colW = wide ? Math.floor((w - pad * 3) / 2) : w - pad * 2;
  const col2X = wide ? pad + colW + pad : pad;

  // Title
  ink(200, 220, 200);
  write("ac/native", { x: pad, y: 10, size: 2, font: "matrix" });

  // Connection status
  if (!wifi?.connected) {
    ink(200, 80, 80);
    write("offline", { x: pad, y: 34, size: 1, font });
    ink(100, 80, 80);
    write("connect wifi first", { x: pad, y: 46, size: 1, font });
  } else {
    ink(100, 110, 100);
    write("current", { x: pad, y: 34, size: 1, font });
    ink(180, 180, 180);
    const maxChars = Math.floor(colW / 6);
    write(currentVersion.slice(0, maxChars), { x: pad, y: 46, size: 1, font });
  }

  // Machine hint (hw/sw info moved to dedicated 'machine' piece)
  {
    const sX = wide ? col2X : pad;
    const sY = wide ? 34 : 58;
    ink(60, 70, 80);
    write("machine: hw + sw info", { x: sX, y: sY, size: 1, font });
  }

  const stateY = 66;

  if (state === "checking") {
    ink(200, 200, 80);
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    write("checking" + dots, { x: pad, y: stateY, size: 1, font });

  } else if (state === "up-to-date") {
    ink(80, 200, 120);
    write("up to date!", { x: pad, y: stateY, size: 1, font });
    ink(100, 110, 100);
    write(remoteVersion, { x: pad, y: stateY + 14, size: 1, font });
    ink(80, 80, 100);
    write("enter: recheck  esc: back", { x: pad, y: stateY + 30, size: 1, font });

  } else if (state === "available") {
    ink(100, 110, 100);
    write("available", { x: pad, y: stateY, size: 1, font });
    ink(255, 200, 60);
    write(remoteVersion, { x: pad, y: stateY + 14, size: 1, font });

    // Flash target selector
    const targets = system?.flashTargets || [];
    if (targets.length > 0) {
      if (flashTargetIdx >= targets.length) flashTargetIdx = 0;
      const tgt = targets[flashTargetIdx];
      const tgtLabel = (tgt?.label || "?") + " (" + (tgt?.device || "?") + ")";
      const isBoot = tgt?.device === system?.bootDevice;
      ink(80, 100, 120);
      write("target:", { x: pad, y: stateY + 30, size: 1, font });
      ink(isBoot ? 80 : 200, isBoot ? 200 : 200, isBoot ? 255 : 80);
      write(tgtLabel, { x: pad + 48, y: stateY + 30, size: 1, font });
      if (isBoot) {
        ink(60, 140, 200);
        write("(current boot)", { x: pad, y: stateY + 42, size: 1, font });
      }
      if (targets.length > 1) {
        ink(80, 80, 100);
        write("tab: next target", { x: pad, y: stateY + 56, size: 1, font });
      }
    }

    // Keyboard hint
    const hintY = stateY + (targets.length > 0 ? 72 : 34);
    ink(60, 180, 100);
    write("enter: install update", { x: pad, y: hintY, size: 1, font });
    ink(80, 80, 100);
    write("esc: back", { x: pad, y: hintY + 14, size: 1, font });

  } else if (state === "downloading") {
    ink(120, 140, 120);
    const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
    write("downloading" + dots, { x: pad, y: stateY, size: 1, font });

    // File info
    const expectedMB = (OS_VMLINUZ_BYTES / 1048576).toFixed(0);
    const dlMB = ((progress || 0) * OS_VMLINUZ_BYTES / 1048576).toFixed(1);
    ink(80, 100, 80);
    write(`${dlMB} / ${expectedMB} MB`, { x: pad, y: stateY + 14, size: 1, font });

    // Progress bar
    const barW = w - pad * 2, barH = 8, barY = stateY + 30;
    ink(30, 40, 50);
    box(pad, barY, barW, barH, true);
    ink(60, 180, 100);
    box(pad, barY, Math.round(barW * (progress || 0)), barH, true);
    ink(160);
    write(Math.round((progress || 0) * 100) + "%", { x: pad, y: barY + 12, size: 1, font });

    // Target info
    ink(60, 70, 80);
    write("-> " + (globalThis.__osFlashDevice || system?.bootDevice || "?"), { x: pad, y: barY + 26, size: 1, font });

  } else if (state === "flashing") {
    const phase = system?.flashPhase ?? 0;
    const phaseIcons = ["...", ">>>", "~~~", "???", "!!!"];
    const phaseNames = ["preparing", "writing EFI", "syncing to disk", "verifying", "complete"];
    const phaseText = phaseNames[phase] || "preparing";
    const icon = phaseIcons[phase] || "...";

    // Phase indicator with animation
    const dots = ".".repeat((Math.floor(frame / 10) % 3) + 1);
    ink(...(phase === 3 ? [100, 200, 255] : phase === 4 ? [80, 255, 120] : [255, 160, 60]));
    write(`${icon} ${phaseText}${phase < 4 ? dots : ""}`, { x: pad, y: stateY, size: 1, font });

    // Target device
    ink(100);
    write("-> " + (globalThis.__osFlashDevice || system?.bootDevice || "?"), { x: pad, y: stateY + 14, size: 1, font });

    // Phase progress visualization
    const phases = ["prepare", "write", "sync", "verify"];
    let px = pad;
    for (let i = 0; i < phases.length; i++) {
      const active = i === phase || (phase === 4 && i === 3);
      const done = i < phase || phase === 4;
      ink(done ? 60 : 30, done ? 140 : 40, done ? 80 : 50);
      const pw = Math.floor((w - pad * 2 - 12) / 4);
      box(px, stateY + 30, pw, 6, true);
      if (active && !done) {
        // Animated fill
        const fill = Math.floor(pw * ((frame % 60) / 60));
        ink(255, 200, 60);
        box(px, stateY + 30, fill, 6, true);
      }
      px += pw + 4;
    }

    // Warning
    ink(140);
    write("do not power off", { x: pad, y: stateY + 44, size: 1, font });

    // Scrolling telemetry at bottom
    const telY = stateY + 60;
    const maxLines = Math.floor((h - telY - 14) / 10);
    const startIdx = Math.max(0, telemetry.length - maxLines);
    for (let i = startIdx; i < telemetry.length; i++) {
      const lineY = telY + (i - startIdx) * 10;
      const age = frame - telemetry[i].frame;
      const brightness = Math.max(40, Math.min(120, 120 - age));
      ink(brightness, Math.floor(brightness * 1.2), brightness);
      write(telemetry[i].text, { x: pad, y: lineY, size: 1, font });
    }

  } else if (state === "confirm-reboot") {
    // Flash complete — ask user to reboot
    const mb = flashedMB;
    ink(80, 255, 120);
    write("update installed!", { x: pad, y: stateY, size: 2, font: "matrix" });

    ink(120, 200, 140);
    write(`verified ${mb}MB written`, { x: pad, y: stateY + 24, size: 1, font });

    ink(200, 180, 100);
    write(remoteVersion, { x: pad, y: stateY + 38, size: 1, font });

    // Flash diagnostics
    const dst = system?.flashDst || "?";
    const sameDev = system?.flashSameDevice ? "same-dev" : "cross-dev";
    ink(80, 80, 100);
    write(`${dst} (${sameDev})`, { x: pad, y: stateY + 50, size: 1, font });

    // Reboot prompt — pulsing
    const pulse = Math.floor(200 + 55 * Math.sin(frame * 0.1));
    ink(pulse, pulse, 255);
    write("reboot now?", { x: pad, y: stateY + 58, size: 2, font: "matrix" });

    // Warn if flashed to non-boot device (e.g. USB→NVMe: remove USB first)
    const targets = system?.flashTargets || [];
    const tgt = targets[flashTargetIdx];
    const flashedToBoot = !tgt || tgt.device === system?.bootDevice;
    if (!flashedToBoot) {
      ink(255, 180, 60);
      write("remove USB before rebooting!", { x: pad, y: stateY + 80, size: 1, font });
    }

    const hintY = flashedToBoot ? stateY + 80 : stateY + 94;
    ink(60, 200, 80);
    write("y: reboot to new os", { x: pad, y: hintY, size: 1, font });
    ink(140, 100, 80);
    write("n: back to prompt", { x: pad, y: hintY + 14, size: 1, font });

    // Scrolling telemetry in background
    const telY = hintY + 32;
    const maxLines = Math.floor((h - telY - 14) / 10);
    const startIdx = Math.max(0, telemetry.length - maxLines);
    for (let i = startIdx; i < telemetry.length; i++) {
      const lineY = telY + (i - startIdx) * 10;
      ink(40, 50, 45);
      write(telemetry[i].text, { x: pad, y: lineY, size: 1, font });
    }

  } else if (state === "error") {
    ink(220, 80, 80);
    write(("error: " + errorMsg).slice(0, Math.floor((w - pad * 2) / 6)), { x: pad, y: stateY, size: 1, font });
    ink(120);
    write("enter: retry  esc: back", { x: pad, y: stateY + 14, size: 1, font });

  } else {
    // idle
    ink(100);
    write("enter: check for updates", { x: pad, y: stateY, size: 1, font });
  }

  // Bottom hint (not during shutdown)
  if (state !== "shutting-down") {
    ink(60, 80, 60);
    write("esc: back", { x: pad, y: h - 12, size: 1, font });
  }

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
    const prevProgress = progress;
    progress = system?.fetchBinaryProgress ?? progress;
    // Telemetry on progress milestones
    if (progress > 0 && Math.floor(progress * 10) > Math.floor(prevProgress * 10)) {
      const pct = Math.round(progress * 100);
      addTelemetry(`download ${pct}% (${(progress * OS_VMLINUZ_BYTES / 1048576).toFixed(1)}MB)`);
    }
    if (system?.fetchBinaryDone) {
      if (system?.fetchBinaryOk) {
        addTelemetry("download complete, starting flash...");
        state = "flashing";
        const dev = globalThis.__osFlashDevice;
        addTelemetry("target: " + (dev || system?.bootDevice || "auto"));
        if (dev) system?.flashUpdate?.("/tmp/vmlinuz.new", dev);
        else system?.flashUpdate?.("/tmp/vmlinuz.new");
      } else {
        state = "error";
        errorMsg = "download failed";
      }
    }
  }

  // Flash progress — track phase transitions for telemetry
  if (state === "flashing") {
    const phase = system?.flashPhase ?? 0;
    const prevPhase = globalThis.__osLastFlashPhase ?? -1;
    if (phase !== prevPhase) {
      globalThis.__osLastFlashPhase = phase;
      const names = ["preparing flash", "writing EFI image", "syncing to disk", "verifying bytes", "flash complete"];
      addTelemetry(names[phase] || `phase ${phase}`);
      if (phase === 1) addTelemetry("dst: EFI/BOOT/BOOTX64.EFI");
    }
    if (system?.flashDone) {
      // Capture flash log from C layer
      const flog = system?.flashLog || [];
      for (const line of flog) addTelemetry("[c] " + line);
      if (system?.flashDst) addTelemetry("wrote: " + system.flashDst);
      addTelemetry("same_dev=" + (system?.flashSameDevice ? "yes" : "no"));

      if (system?.flashOk) {
        flashedMB = ((system?.flashVerifiedBytes ?? 0) / 1048576).toFixed(1);
        addTelemetry(`verified OK: ${flashedMB}MB`);
        state = "confirm-reboot";
      } else {
        state = "error";
        errorMsg = "flash verify failed";
        addTelemetry("VERIFY FAILED");
        for (const line of flog) addTelemetry("[c] " + line);
      }
    }
  }
}

function sim() {}

export { boot, paint, act, sim };
