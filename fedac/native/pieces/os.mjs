// os.mjs — AC Native OS management piece
// Shows current version, checks for updates, downloads + flashes + reboots.
// Jumped to from prompt.mjs via "os" command or from notepat OS button.

const OS_BASE_URL = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/";
const OS_VERSION_URL = OS_BASE_URL + "native-notepat-latest.version";
const OS_VMLINUZ_URL = OS_BASE_URL + "native-notepat-latest.vmlinuz";
const OS_INITRAMFS_URL = OS_BASE_URL + "native-notepat-latest.initramfs.cpio.gz";
let remoteSize = 0; // parsed from version file (line 2)

// Kernel no longer embeds initramfs (Phase 2 — loaded externally via EFI stub
// `initrd=\initramfs.cpio.gz` from the ESP root). OTA must download BOTH the
// kernel and the initramfs and flash them atomically, otherwise the device
// boots a new kernel against a stale initramfs and code-path drift follows.
let initramfsDownloaded = false;

// States: idle | checking | up-to-date | available
//         | downloading (kernel) | downloading-initramfs | flashing
//         | confirm-reboot | shutting-down | error
//         | devices | clone-confirm | cloning
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

// Device manager state
let deviceIdx = 0;
let lastTargetCount = -1; // track hot-plug changes
let cloneTarget = null;

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
      // Block reboot if USB live media still present on a cross-device flash —
      // firmware will boot the stale USB kernel instead of the freshly-flashed one
      const targets = system?.flashTargets || [];
      const tgt = targets[flashTargetIdx];
      const flashedToBoot = !tgt || tgt.device === system?.bootDevice;
      if (!flashedToBoot) {
        const usbStillAttached = targets.some(t => t.removable && t.device === system?.bootDevice);
        if (usbStillAttached) {
          sound?.synth({ type: "square", tone: 180, duration: 0.14, volume: 0.12, attack: 0.003, decay: 0.12 });
          return;
        }
      }
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
    if (e.is("keyboard:down:y") || e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
      const targets = system?.flashTargets || [];
      const tgt = targets[flashTargetIdx];
      const device = tgt?.device || undefined;
      globalThis.__osFlashDevice = device;
      state = "downloading";
      progress = 0;
      telemetry.length = 0;
      addTelemetry("fetching " + OS_VMLINUZ_URL.split("/").pop());
      system?.fetchBinary?.(OS_VMLINUZ_URL, "/tmp/vmlinuz.new", (remoteSize || 93_000_000));
      return;
    }
    if (e.is("keyboard:down:n")) {
      system?.jump?.("prompt");
      return;
    }
    if (e.is("keyboard:down:tab")) {
      const targets = system?.flashTargets || [];
      flashTargetIdx = (flashTargetIdx + 1) % Math.max(1, targets.length);
      sound?.synth({ type: "sine", tone: 440, duration: 0.04, volume: 0.12, attack: 0.002, decay: 0.03 });
      return;
    }
  }

  // 'd' to enter device manager from idle/up-to-date/available/error
  if (state === "idle" || state === "up-to-date" || state === "available" || state === "error") {
    if (e.is("keyboard:down:d")) {
      state = "devices";
      deviceIdx = 0;
      sound?.synth({ type: "sine", tone: 523, duration: 0.05, volume: 0.1, attack: 0.002, decay: 0.04 });
      return;
    }
  }

  // Device manager controls
  if (state === "devices") {
    const targets = system?.flashTargets || [];
    if (e.is("keyboard:down:tab") || e.is("keyboard:down:arrowdown")) {
      deviceIdx = (deviceIdx + 1) % Math.max(1, targets.length);
      sound?.synth({ type: "sine", tone: 440, duration: 0.04, volume: 0.1, attack: 0.002, decay: 0.03 });
      return;
    }
    if (e.is("keyboard:down:arrowup")) {
      deviceIdx = (deviceIdx - 1 + targets.length) % Math.max(1, targets.length);
      sound?.synth({ type: "sine", tone: 440, duration: 0.04, volume: 0.1, attack: 0.002, decay: 0.03 });
      return;
    }
    // 'c' to clone current OS to selected device
    if (e.is("keyboard:down:c")) {
      const tgt = targets[deviceIdx];
      if (tgt && tgt.device !== system?.bootDevice) {
        cloneTarget = tgt;
        state = "clone-confirm";
        sound?.synth({ type: "triangle", tone: 660, duration: 0.08, volume: 0.12, attack: 0.003, decay: 0.06 });
      } else {
        sound?.synth({ type: "square", tone: 220, duration: 0.1, volume: 0.08, attack: 0.005, decay: 0.08 });
      }
      return;
    }
    // 'u' to update selected device from CDN
    if (e.is("keyboard:down:u")) {
      const tgt = targets[deviceIdx];
      if (tgt) {
        flashTargetIdx = deviceIdx;
        state = "checking";
        fetchPending = true;
        checkFrame = frame;
        system?.fetch?.(OS_VERSION_URL);
      }
      return;
    }
    if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
      state = "idle";
      return;
    }
    return;
  }

  // Clone confirmation
  if (state === "clone-confirm") {
    if (e.is("keyboard:down:y") || e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
      state = "cloning";
      telemetry.length = 0;
      addTelemetry("cloning to " + cloneTarget.device);
      // Clone = flash the currently running kernel to the target device
      // The running kernel is at /mnt/EFI/BOOT/BOOTX64.EFI or KERNEL.EFI
      const bootKernel = "/mnt/EFI/BOOT/BOOTX64.EFI";
      globalThis.__osFlashDevice = cloneTarget.device;
      system?.flashUpdate?.(bootKernel, cloneTarget.device);
      sound?.synth({ type: "triangle", tone: 784, duration: 0.1, volume: 0.12, attack: 0.003, decay: 0.08 });
      return;
    }
    if (e.is("keyboard:down:n") || e.is("keyboard:down:escape")) {
      state = "devices";
      return;
    }
    return;
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
  const T = __theme.update();
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
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Responsive: use half-width columns on wide screens
  const wide = w > 260;
  const colW = wide ? Math.floor((w - pad * 3) / 2) : w - pad * 2;
  const col2X = wide ? pad + colW + pad : pad;

  // Title
  ink(T.fg, T.fg + 10, T.fg);
  write("ac/native", { x: pad, y: 10, size: 2, font: "matrix" });

  // Connection status
  if (!wifi?.connected) {
    ink(T.err[0], T.err[1], T.err[2]);
    write("offline", { x: pad, y: 34, size: 1, font });
    ink(T.fgMute, T.fgMute - 10, T.fgMute - 10);
    write("connect wifi first", { x: pad, y: 46, size: 1, font });
  } else {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write("current", { x: pad, y: 34, size: 1, font });
    ink(T.fgDim, T.fgDim, T.fgDim);
    const maxChars = Math.floor(colW / 6);
    write(currentVersion.slice(0, maxChars), { x: pad, y: 46, size: 1, font });
  }

  // Machine hint
  {
    const sX = wide ? col2X : pad;
    const sY = wide ? 34 : 58;
    ink(T.fgMute, T.fgMute + 5, T.fgMute + 10);
    write("machine: hw + sw info", { x: sX, y: sY, size: 1, font });
  }

  const stateY = 66;

  if (state === "checking") {
    ink(T.warn[0], T.warn[1], T.warn[2]);
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    write("checking" + dots, { x: pad, y: stateY, size: 1, font });

  } else if (state === "up-to-date") {
    ink(T.ok[0], T.ok[1], T.ok[2]);
    write("up to date!", { x: pad, y: stateY, size: 1, font });
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write(remoteVersion, { x: pad, y: stateY + 14, size: 1, font });
    ink(T.fgMute, T.fgMute, T.fgMute + 10);
    write("enter: recheck  esc: back", { x: pad, y: stateY + 30, size: 1, font });

  } else if (state === "available") {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write("available", { x: pad, y: stateY, size: 1, font });
    ink(T.warn[0], T.warn[1], T.warn[2]);
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

    // Install confirmation prompt
    const hintY = stateY + (targets.length > 0 ? 72 : 34);
    const pulse = Math.floor(180 + 75 * Math.sin(frame * 0.08));
    ink(pulse, 255, pulse);
    write("install? y/n", { x: pad, y: hintY, size: 1, font });
    ink(80, 80, 100);
    write("esc: back", { x: pad, y: hintY + 14, size: 1, font });

  } else if (state === "downloading") {
    ink(120, 140, 120);
    const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
    write("downloading" + dots, { x: pad, y: stateY, size: 1, font });

    // File info
    const expectedMB = ((remoteSize || 93_000_000) / 1048576).toFixed(0);
    const dlMB = ((progress || 0) * (remoteSize || 93_000_000) / 1048576).toFixed(1);
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
    // USB still attached? flashTargets re-enumerates each frame so removal is live
    const usbStillAttached = !flashedToBoot &&
      targets.some(t => t.removable && t.device === system?.bootDevice);
    const rebootBlocked = usbStillAttached;
    if (!flashedToBoot) {
      if (usbStillAttached) {
        // Blink warning until USB is removed
        const blink = Math.floor(frame / 12) % 2 === 0;
        if (blink) {
          ink(255, 180, 60);
          write("⚠ unplug USB before rebooting", { x: pad, y: stateY + 80, size: 1, font });
        } else {
          ink(255, 60, 60);
          write("    reboot blocked    ", { x: pad, y: stateY + 80, size: 1, font });
        }
      } else {
        // Live media removed — clear the warning, show ready state
        ink(100, 220, 120);
        write("✓ USB removed, safe to reboot", { x: pad, y: stateY + 80, size: 1, font });
      }
    }

    const hintY = flashedToBoot ? stateY + 80 : stateY + 94;
    if (rebootBlocked) {
      ink(120, 80, 80);
      write("y: disabled (remove USB)", { x: pad, y: hintY, size: 1, font });
    } else {
      ink(60, 200, 80);
      write("y: reboot to new os", { x: pad, y: hintY, size: 1, font });
    }
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
    ink(T.err[0], T.err[1], T.err[2]);
    write(("error: " + errorMsg).slice(0, Math.floor((w - pad * 2) / 6)), { x: pad, y: stateY, size: 1, font });
    ink(T.fgMute);
    write("enter: retry  esc: back", { x: pad, y: stateY + 14, size: 1, font });

  } else if (state === "devices") {
    // === Device manager ===
    const targets = system?.flashTargets || [];
    const bootDev = system?.bootDevice;

    // Hot-plug detection: play sound on change
    if (targets.length !== lastTargetCount && lastTargetCount >= 0) {
      // Would play sound here but we don't have sound ref in paint
    }
    lastTargetCount = targets.length;

    ink(T.fg, T.fg + 10, T.fg);
    write("devices", { x: pad, y: stateY, size: 2, font: "matrix" });

    if (targets.length === 0) {
      ink(T.fgMute);
      write("no devices found", { x: pad, y: stateY + 24, size: 1, font });
    } else {
      if (deviceIdx >= targets.length) deviceIdx = 0;
      const rowH = 20;
      for (let i = 0; i < targets.length; i++) {
        const tgt = targets[i];
        const ry = stateY + 24 + i * rowH;
        const isBoot = tgt.device === bootDev;
        const selected = i === deviceIdx;

        // Selection indicator
        if (selected) {
          ink(40, 60, 80);
          box(pad - 2, ry - 2, w - pad * 2 + 4, rowH - 2, true);
        }

        // Device label + path
        ink(selected ? 255 : T.fgMute, selected ? 255 : T.fgMute, selected ? 255 : T.fgMute);
        write((selected ? "> " : "  ") + (tgt.label || "?"), { x: pad, y: ry, size: 1, font });
        ink(80, 80, 100);
        write(tgt.device, { x: pad + 100, y: ry, size: 1, font });

        // Boot indicator
        if (isBoot) {
          ink(60, 200, 120);
          write("boot", { x: w - pad - 30, y: ry, size: 1, font });
        } else if (tgt.removable) {
          ink(100, 140, 200);
          write("usb", { x: w - pad - 24, y: ry, size: 1, font });
        }
      }

      // Actions for selected device
      const actY = stateY + 24 + targets.length * rowH + 8;
      const sel = targets[deviceIdx];
      const isBootDev = sel?.device === bootDev;

      ink(80, 80, 100);
      write("tab/arrows: select", { x: pad, y: actY, size: 1, font });

      if (!isBootDev) {
        ink(100, 200, 140);
        write("c: clone current os", { x: pad, y: actY + 14, size: 1, font });
      }
      ink(100, 160, 220);
      write("u: update from cloud", { x: pad, y: actY + 28, size: 1, font });
    }

  } else if (state === "clone-confirm") {
    ink(T.warn[0], T.warn[1], T.warn[2]);
    write("clone os?", { x: pad, y: stateY, size: 2, font: "matrix" });

    ink(T.fgMute + 20, T.fgMute + 20, T.fgMute);
    write("from: " + (system?.bootDevice || "?"), { x: pad, y: stateY + 24, size: 1, font });
    write("  to: " + (cloneTarget?.label || "?") + " (" + (cloneTarget?.device || "?") + ")", { x: pad, y: stateY + 38, size: 1, font });

    ink(T.fg);
    write(currentVersion, { x: pad, y: stateY + 56, size: 1, font });

    ink(255, 180, 60);
    write("this will overwrite the target!", { x: pad, y: stateY + 74, size: 1, font });

    const pulse = Math.floor(200 + 55 * Math.sin(frame * 0.1));
    ink(pulse, 255, pulse);
    write("y: clone  n: cancel", { x: pad, y: stateY + 92, size: 1, font });

  } else if (state === "cloning") {
    // Reuse flashing UI
    const phase = system?.flashPhase ?? 0;
    const phaseNames = ["preparing", "writing EFI", "syncing", "verifying", "complete"];
    const dots = ".".repeat((Math.floor(frame / 10) % 3) + 1);
    ink(255, 160, 60);
    write("cloning" + (phase < 4 ? dots : "!"), { x: pad, y: stateY, size: 1, font });
    ink(120);
    write(phaseNames[phase] || "...", { x: pad, y: stateY + 14, size: 1, font });
    ink(80);
    write("-> " + (cloneTarget?.device || "?"), { x: pad, y: stateY + 28, size: 1, font });
    ink(140);
    write("do not power off", { x: pad, y: stateY + 44, size: 1, font });

  } else {
    // idle
    ink(T.fgMute);
    write("enter: check for updates", { x: pad, y: stateY, size: 1, font });
    ink(T.fgMute - 20, T.fgMute, T.fgMute + 10);
    write("d: devices", { x: pad, y: stateY + 14, size: 1, font });
  }

  // Bottom hint (not during shutdown)
  if (state !== "shutting-down") {
    ink(T.fgMute, T.fgMute + 10, T.fgMute);
    write(state === "devices" ? "esc: back to os" : "esc: back", { x: pad, y: h - 12, size: 1, font });
  }

  // === State machine: poll fetch/flash results ===

  // Version check result (from .version file: "name hash-ts\nsize")
  if (fetchPending && system?.fetchResult !== undefined && system?.fetchResult !== null) {
    const raw = (typeof system.fetchResult === "string" ? system.fetchResult : "").trim();
    fetchPending = false;
    if (!raw || raw.length < 5) {
      state = "error";
      errorMsg = "bad version response";
    } else {
      const lines = raw.split("\n");
      remoteVersion = lines[0].trim();
      if (lines[1]) remoteSize = parseInt(lines[1].trim()) || 0;
      state = (remoteVersion === currentVersion) ? "up-to-date" : "available";
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

  // Download progress (kernel)
  if (state === "downloading") {
    const prevProgress = progress;
    progress = system?.fetchBinaryProgress ?? progress;
    if (progress > 0 && Math.floor(progress * 10) > Math.floor(prevProgress * 10)) {
      const pct = Math.round(progress * 100);
      addTelemetry(`kernel ${pct}% (${(progress * (remoteSize || 13_000_000) / 1048576).toFixed(1)}MB)`);
    }
    if (system?.fetchBinaryDone) {
      if (system?.fetchBinaryOk) {
        addTelemetry("kernel downloaded, fetching initramfs...");
        state = "downloading-initramfs";
        progress = 0;
        initramfsDownloaded = false;
        // Initramfs size isn't in the .version file today; use a generous
        // default so the progress bar doesn't stall at 100% prematurely.
        system?.fetchBinary?.(OS_INITRAMFS_URL, "/tmp/initramfs.cpio.gz.new", 336_000_000);
      } else {
        state = "error";
        errorMsg = "kernel download failed";
      }
    }
  }

  // Download progress (initramfs) — kicks off flash once both files are local
  if (state === "downloading-initramfs") {
    const prevProgress = progress;
    progress = system?.fetchBinaryProgress ?? progress;
    if (progress > 0 && Math.floor(progress * 10) > Math.floor(prevProgress * 10)) {
      const pct = Math.round(progress * 100);
      addTelemetry(`initramfs ${pct}%`);
    }
    if (system?.fetchBinaryDone) {
      if (system?.fetchBinaryOk) {
        initramfsDownloaded = true;
        addTelemetry("initramfs downloaded, starting flash...");
        state = "flashing";
        const dev = globalThis.__osFlashDevice;
        addTelemetry("target: " + (dev || system?.bootDevice || "auto"));
        // Four-arg flashUpdate: (kernelSrc, device, initramfsSrc)
        // Device may be omitted — null lets C auto-detect the boot device.
        system?.flashUpdate?.("/tmp/vmlinuz.new", dev || null, "/tmp/initramfs.cpio.gz.new");
      } else {
        state = "error";
        errorMsg = "initramfs download failed";
      }
    }
  }

  // Clone progress
  if (state === "cloning") {
    if (system?.flashDone) {
      if (system?.flashOk) {
        flashedMB = ((system?.flashVerifiedBytes ?? 0) / 1048576).toFixed(1);
        state = "confirm-reboot";
      } else {
        state = "error";
        errorMsg = "clone verify failed";
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
