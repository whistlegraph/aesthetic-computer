// wifi.mjs — AC Native WiFi management piece
// Shows available networks, connects, manages saved credentials.
// Jumped to from prompt.mjs via "net" or "wifi" command.
// Respects __theme (dark/light mode + presets).

const AC_SSID = "aesthetic.computer";
const AC_PASS = "aesthetic.computer";
const CREDS_PATH = "/mnt/wifi_creds.json";
const SHIFT_MAP = {
  "1":"!","2":"@","3":"#","4":"$","5":"%","6":"^","7":"&","8":"*","9":"(","0":")",
  "-":"_","=":"+","[":"{","]":"}",";":":","'":'"',",":"<",".":">","/":"?","\\":"|","`":"~",
};

let selectedIdx = -1;
let password = "";
let passwordMode = false;
let shiftHeld = false;
let cursorBlink = 0;
let savedCreds = [];
let frame = 0;
let connectingSSID = ""; // track which SSID we're connecting to
let lastState = 0;       // track state changes for feedback
let showLogs = false;    // toggle with 'l' key
let logScroll = 0;       // scroll offset for log view

function boot({ system }) {
  // Load saved credentials
  try {
    const raw = system?.readFile?.(CREDS_PATH);
    if (raw) savedCreds = JSON.parse(raw);
  } catch (_) {}
}

function act({ event: e, sound, system, wifi }) {
  if (!e.is("touch") && !e.is("keyboard:down") && !e.is("keyboard:up")) return;

  // Shift tracking
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }

  // Password input mode
  if (passwordMode) {
    if (e.is("keyboard:down:escape")) {
      passwordMode = false;
      password = "";
      return;
    }
    if (e.is("keyboard:down:enter")) {
      const merged = globalThis.__wifiMergedList || [];
      const entry = merged[selectedIdx];
      if (entry && password.length > 0) {
        wifi?.connect?.(entry.ssid, password);
        connectingSSID = entry.ssid;
        // Save credentials
        if (!savedCreds.find(c => c.ssid === entry.ssid)) {
          savedCreds.push({ ssid: entry.ssid, pass: password });
          system?.writeFile?.(CREDS_PATH, JSON.stringify(savedCreds));
        }
        passwordMode = false;
        password = "";
      }
      return;
    }
    if (e.is("keyboard:down:backspace")) {
      password = password.slice(0, -1);
      return;
    }
    if (e.is("keyboard:down")) {
      const key = e.key;
      if (key && key.length === 1) {
        const ch = shiftHeld ? (SHIFT_MAP[key] || key.toUpperCase()) : key;
        password += ch;
      }
      return;
    }
    return; // Block all other events in password mode
  }

  // Escape goes back to prompt
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (showLogs) { showLogs = false; return; }
    system?.jump?.("prompt");
    return;
  }

  // Toggle log view
  if (e.is("keyboard:down:l") && !passwordMode) {
    showLogs = !showLogs;
    logScroll = 0;
    return;
  }

  const merged = globalThis.__wifiMergedList || [];

  // Keyboard navigation
  if (e.is("keyboard:down:arrowdown")) {
    let next = selectedIdx + 1;
    while (next < merged.length && merged[next].type === "separator") next++;
    if (next < merged.length) {
      selectedIdx = next;
      sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.02 });
    }
    return;
  }
  if (e.is("keyboard:down:arrowup")) {
    let prev = selectedIdx - 1;
    while (prev >= 0 && merged[prev].type === "separator") prev--;
    if (prev >= 0) {
      selectedIdx = prev;
      sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.02 });
    }
    return;
  }
  if (e.is("keyboard:down:enter")) {
    if (selectedIdx >= 0 && selectedIdx < merged.length) {
      connectEntry(merged[selectedIdx], wifi, sound, system);
    }
    return;
  }

  // Touch: select or connect to network
  if (e.is("touch")) {
    const { x, y } = e;
    const rowH = 16;
    const listY = 44;

    const tappedRow = Math.floor((y - listY) / rowH);
    if (tappedRow >= 0 && tappedRow < merged.length) {
      const entry = merged[tappedRow];
      if (entry.type === "separator") return;

      if (tappedRow === selectedIdx) {
        // Second tap: connect
        connectEntry(entry, wifi, sound, system);
      } else {
        selectedIdx = tappedRow;
        sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.02 });
      }
    }
  }
}

function connectEntry(entry, wifi, sound, system) {
  const allSaved = [
    { ssid: AC_SSID, pass: AC_PASS },
    ...savedCreds,
  ];
  const cred = allSaved.find(c => c.ssid === entry.ssid);

  if (cred) {
    wifi?.connect?.(cred.ssid, cred.pass);
    connectingSSID = cred.ssid;
    sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.15, attack: 0.005, decay: 0.06 });
  } else if (entry.encrypted) {
    passwordMode = true;
    password = "";
    cursorBlink = 0;
  } else {
    wifi?.connect?.(entry.ssid, "");
    connectingSSID = entry.ssid;
    sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.15, attack: 0.005, decay: 0.06 });
  }
}

function paint({ wipe, ink, box, write, screen, system, wifi, sound }) {
  frame++;
  const T = __theme.update();
  const w = screen.width, h = screen.height;

  // Play sound on state transitions
  if (wifi && wifi.state !== lastState) {
    if (wifi.state === 4 && lastState === 3) { // CONNECTED after CONNECTING
      sound?.synth({ type: "sine", tone: 880, duration: 0.15, volume: 0.12, attack: 0.005, decay: 0.1 });
    } else if (wifi.state === 5 && lastState === 3) { // FAILED after CONNECTING
      sound?.synth({ type: "sine", tone: 220, duration: 0.2, volume: 0.12, attack: 0.005, decay: 0.15 });
    }
    lastState = wifi ? wifi.state : 0;
  }

  // Password entry fullscreen
  if (passwordMode && wifi) {
    cursorBlink++;
    const merged = globalThis.__wifiMergedList || [];
    const entry = merged[selectedIdx];
    const ssid = entry ? entry.ssid : "?";

    wipe(T.bg[0], T.bg[1], T.bg[2]);

    ink(T.fg, T.fg, T.fg);
    const titleX = Math.max(10, (w - ssid.length * 18) / 2);
    write(ssid, { x: titleX, y: h / 2 - 50, size: 2, font: "matrix" });

    ink(T.fgDim, T.fgDim, T.fgDim);
    write("enter password:", { x: 20, y: h / 2 - 24, size: 2, font: "font_1" });

    // Password field
    ink(T.bgAlt[0], T.bgAlt[1], T.bgAlt[2]);
    box(18, h / 2 - 6, w - 36, 18, true);
    ink(T.border[0], T.border[1], T.border[2]);
    box(18, h / 2 - 6, w - 36, 18, "outline");

    const cursor = (cursorBlink % 60) < 35 ? "|" : "";
    ink(T.fg, T.fg, T.fg);
    write(password + cursor, { x: 22, y: h / 2 - 2, size: 1, font: "font_1" });

    ink(T.fgMute, T.fgMute, T.fgMute);
    write("Enter: connect    Esc: cancel", { x: 20, y: h / 2 + 22, size: 1, font: "font_1" });

    if (wifi.state === 3) {
      ink(T.warn[0], T.warn[1], T.warn[2]);
      write("connecting...", { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    } else if (wifi.state === 5) {
      ink(T.err[0], T.err[1], T.err[2]);
      write("failed: " + (wifi.status || "?"), { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    }
    return;
  }

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Title
  ink(T.fg, T.fg, T.fg);
  write("WiFi Networks", { x: 20, y: 12, size: 2, font: "matrix" });

  // Status line
  ink(T.fgDim, T.fgDim, T.fgDim);
  const statusStr = wifi ? ((wifi.status || "scanning...") + (wifi.iface ? " [" + wifi.iface + "]" : "")) : "no wifi";
  write(statusStr, { x: 20, y: 30, size: 1, font: "font_1" });

  if (!wifi) {
    ink(T.err[0], T.err[1], T.err[2]);
    write("wifi not available", { x: 20, y: 60, size: 1, font: "font_1" });
    ink(T.fgMute, T.fgMute, T.fgMute);
    write("esc: back", { x: 10, y: h - 12, size: 1, font: "font_1" });
    return;
  }

  // Build merged network list
  const rawNets = wifi.networks || [];
  const ssidBest = new Map();
  for (const n of rawNets) {
    if (!n.ssid) continue;
    const prev = ssidBest.get(n.ssid);
    if (!prev || n.signal > prev.signal) ssidBest.set(n.ssid, n);
  }
  const scannedNets = [...ssidBest.values()].sort((a, b) => b.signal - a.signal);
  const scannedSSIDs = new Set(scannedNets.map(n => n.ssid));
  const allSaved = [
    { ssid: AC_SSID, pass: AC_PASS },
    ...savedCreds.filter(c => c.ssid !== AC_SSID),
  ];
  const offlineSaved = allSaved.filter(c => !scannedSSIDs.has(c.ssid));

  const rowH = 16;
  const listY = 44;
  const totalRows = scannedNets.length + (offlineSaved.length > 0 ? 1 + offlineSaved.length : 0);
  const maxRows = Math.min(totalRows, Math.floor((h - listY - 60) / rowH));
  globalThis.__wifiMergedList = [];

  let row = 0;

  // Scanned networks
  for (let i = 0; i < scannedNets.length && row < maxRows; i++, row++) {
    const net = scannedNets[i];
    const ry = listY + row * rowH;
    const isSaved = allSaved.find(c => c.ssid === net.ssid);
    const isSelected = row === selectedIdx;
    const isConnecting = wifi.state === 3 && connectingSSID === net.ssid;
    const isConnected = wifi.connected && wifi.ssid === net.ssid;

    if (isSelected) {
      ink(T.bar[0], T.bar[1], T.bar[2]);
      box(10, ry, w - 20, rowH, true);
    }

    // Signal bars
    const bars = net.signal > -50 ? 4 : net.signal > -60 ? 3 : net.signal > -70 ? 2 : 1;
    for (let b = 0; b < 4; b++) {
      if (b < bars) ink(T.ok[0], T.ok[1], T.ok[2]);
      else ink(T.bgDim[0], T.bgDim[1], T.bgDim[2]);
      box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
    }

    // SSID
    if (isConnected) ink(T.ok[0], T.ok[1], T.ok[2]);
    else if (isSaved) ink(T.ok[0], T.ok[1], T.ok[2]);
    else ink(T.fg, T.fg, T.fg);
    const ssidDisplay = net.ssid.length > 26 ? net.ssid.slice(0, 25) + "~" : net.ssid;
    write(ssidDisplay, { x: 36, y: ry + 2, size: 1, font: "font_1" });

    // Right side status
    if (isConnected) {
      ink(T.ok[0], T.ok[1], T.ok[2]);
      write("connected", { x: w - 60, y: ry + 2, size: 1, font: "font_1" });
    } else if (isConnecting) {
      const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
      ink(T.warn[0], T.warn[1], T.warn[2]);
      write("connecting" + dots, { x: w - 72, y: ry + 2, size: 1, font: "font_1" });
    } else if (isSelected) {
      ink(T.link[0], T.link[1], T.link[2]);
      write("connect", { x: w - 52, y: ry + 2, size: 1, font: "font_1" });
    } else if (isSaved) {
      ink(T.fgMute, T.fgMute, T.fgMute);
      write("saved", { x: w - 40, y: ry + 2, size: 1, font: "font_1" });
    } else if (net.encrypted) {
      ink(T.fgMute, T.fgMute, T.fgMute);
      write("*", { x: w - 20, y: ry + 2, size: 1, font: "font_1" });
    }

    globalThis.__wifiMergedList.push({ type: "scan", idx: i, ssid: net.ssid, encrypted: net.encrypted });
  }

  // Saved/preset networks not in scan
  if (offlineSaved.length > 0 && row < maxRows) {
    const sepY = listY + row * rowH;
    ink(T.fgMute, T.fgMute, T.fgMute);
    write("-- saved (not in range) --", { x: 20, y: sepY + 2, size: 1, font: "font_1" });
    globalThis.__wifiMergedList.push({ type: "separator" });
    row++;

    for (let i = 0; i < offlineSaved.length && row < maxRows; i++, row++) {
      const cred = offlineSaved[i];
      const ry = listY + row * rowH;
      const isPreset = cred.ssid === AC_SSID;
      const isSelected = row === selectedIdx;

      if (isSelected) {
        ink(T.bar[0], T.bar[1], T.bar[2]);
        box(10, ry, w - 20, rowH, true);
      }

      for (let b = 0; b < 4; b++) {
        ink(T.bgDim[0], T.bgDim[1], T.bgDim[2]);
        box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
      }

      ink(T.fgDim, T.fgDim, T.fgDim);
      write(cred.ssid, { x: 36, y: ry + 2, size: 1, font: "font_1" });

      if (isSelected) {
        ink(T.link[0], T.link[1], T.link[2]);
        write("connect", { x: w - 52, y: ry + 2, size: 1, font: "font_1" });
      } else if (isPreset) {
        ink(T.fgMute, T.fgMute, T.fgMute);
        write("preset", { x: w - 44, y: ry + 2, size: 1, font: "font_1" });
      } else {
        ink(T.fgMute, T.fgMute, T.fgMute);
        write("saved", { x: w - 40, y: ry + 2, size: 1, font: "font_1" });
      }

      globalThis.__wifiMergedList.push({ type: "saved", ssid: cred.ssid, pass: cred.pass });
    }
  }

  // Connection status panel (bottom area)
  const panelY = h - 52;

  if (wifi.connected && wifi.ip) {
    // Connected: show SSID, IP, signal, SSH
    const okBg = T.dark ? [20, 40, 20] : [220, 245, 220];
    const okBorder = T.dark ? [40, 60, 40] : [180, 220, 180];
    ink(okBg[0], okBg[1], okBg[2]);
    box(10, panelY, w - 20, 40, true);
    ink(okBorder[0], okBorder[1], okBorder[2]);
    box(10, panelY, w - 20, 40, "outline");

    ink(T.ok[0], T.ok[1], T.ok[2]);
    write(wifi.ssid || "connected", { x: 16, y: panelY + 4, size: 1, font: "font_1" });

    ink(T.fg, T.fg, T.fg);
    write(wifi.ip, { x: 16, y: panelY + 16, size: 1, font: "font_1" });

    // Signal strength
    if (wifi.signal) {
      const sig = wifi.signal;
      const qual = sig > -50 ? "excellent" : sig > -60 ? "good" : sig > -70 ? "fair" : "weak";
      ink(T.fgDim, T.fgDim, T.fgDim);
      write(sig + " dBm (" + qual + ")", { x: w / 2, y: panelY + 4, size: 1, font: "font_1" });
    }

    if (system?.sshStarted) {
      ink(T.link[0], T.link[1], T.link[2]);
      write("ssh root@" + wifi.ip, { x: w / 2, y: panelY + 16, size: 1, font: "font_1" });
    }
  } else if (wifi.state === 3) {
    // Connecting
    const dots = ".".repeat((Math.floor(frame / 15) % 3) + 1);
    const warnBg = T.dark ? [30, 30, 15] : [255, 250, 220];
    ink(warnBg[0], warnBg[1], warnBg[2]);
    box(10, panelY, w - 20, 40, true);
    ink(T.warn[0], T.warn[1], T.warn[2]);
    write("connecting" + dots, { x: 16, y: panelY + 4, size: 1, font: "font_1" });
    ink(T.fgDim, T.fgDim, T.fgDim);
    write((connectingSSID || wifi.status || ""), { x: 16, y: panelY + 16, size: 1, font: "font_1" });
  } else if (wifi.state === 5) {
    // Failed
    const errBg = T.dark ? [40, 15, 15] : [255, 230, 230];
    ink(errBg[0], errBg[1], errBg[2]);
    box(10, panelY, w - 20, 40, true);
    ink(T.err[0], T.err[1], T.err[2]);
    write("failed", { x: 16, y: panelY + 4, size: 1, font: "font_1" });
    ink(T.fgDim, T.fgDim, T.fgDim);
    write((connectingSSID ? connectingSSID + ": " : "") + (wifi.status || "unknown error"), { x: 16, y: panelY + 16, size: 1, font: "font_1" });
  } else if (wifi.state === 1) {
    // Scanning
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    ink(T.bgAlt[0], T.bgAlt[1], T.bgAlt[2]);
    box(10, panelY, w - 20, 40, true);
    ink(T.fgDim, T.fgDim, T.fgDim);
    write("scanning" + dots, { x: 16, y: panelY + 12, size: 1, font: "font_1" });
  }

  // Log view (fullscreen overlay, toggle with 'l')
  if (showLogs && wifi) {
    const logs = wifi.logs || [];
    ink(T.bg[0], T.bg[1], T.bg[2]);
    box(0, 0, w, h, true);

    ink(T.fg, T.fg, T.fg);
    write("WiFi Logs", { x: 20, y: 8, size: 2, font: "matrix" });
    ink(T.fgMute, T.fgMute, T.fgMute);
    write(logs.length + " entries", { x: w - 70, y: 12, size: 1, font: "font_1" });

    const logY = 28;
    const lineH = 10;
    const maxLines = Math.floor((h - logY - 16) / lineH);
    const start = Math.max(0, logs.length - maxLines - logScroll);
    const end = Math.min(logs.length, start + maxLines);

    for (let i = start; i < end; i++) {
      const ly = logY + (i - start) * lineH;
      const line = logs[i] || "";
      // Color by content
      if (line.includes("fail") || line.includes("error") || line.includes("killed")) {
        ink(T.err[0], T.err[1], T.err[2]);
      } else if (line.includes("Connected") || line.includes("success")) {
        ink(T.ok[0], T.ok[1], T.ok[2]);
      } else if (line.includes("WPA:") || line.includes("DHCP")) {
        ink(T.warn[0], T.warn[1], T.warn[2]);
      } else {
        ink(T.fgDim, T.fgDim, T.fgDim);
      }
      write(line.slice(0, 60), { x: 8, y: ly, size: 1, font: "font_1" });
    }

    ink(T.fgMute, T.fgMute, T.fgMute);
    write("l: close   esc: back", { x: w - 120, y: h - 10, size: 1, font: "font_1" });
    return; // skip normal rendering below
  }

  // Bottom hints
  ink(T.fgMute, T.fgMute, T.fgMute);
  write("l: logs  esc: back", { x: w - 100, y: h - 10, size: 1, font: "font_1" });
  if (wifi.iface) {
    ink(T.fgMute, T.fgMute, T.fgMute);
    write(wifi.iface, { x: 16, y: h - 10, size: 1, font: "font_1" });
  }

  // Rescan every ~10s when not connected
  if (frame % 600 === 0 && !wifi.connected) {
    wifi.scan?.();
  }
}

function sim() {}

export { boot, paint, act, sim };
