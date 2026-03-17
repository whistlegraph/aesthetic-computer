// wifi.mjs — AC Native WiFi management piece
// Shows available networks, connects, manages saved credentials.
// Jumped to from prompt.mjs via "net" or "wifi" command.

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
      const nets = wifi?.networks || [];
      const merged = globalThis.__wifiMergedList || [];
      const entry = merged[selectedIdx];
      if (entry && password.length > 0) {
        wifi?.connect?.(entry.ssid, password);
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
    system?.jump?.("prompt");
    return;
  }

  // Touch: select or connect to network
  if (e.is("touch")) {
    const { x, y } = e;
    const rowH = 16;
    const listY = 44;
    const merged = globalThis.__wifiMergedList || [];

    const tappedRow = Math.floor((y - listY) / rowH);
    if (tappedRow >= 0 && tappedRow < merged.length) {
      const entry = merged[tappedRow];
      if (entry.type === "separator") return;

      if (tappedRow === selectedIdx) {
        // Second tap: connect
        const allSaved = [
          { ssid: AC_SSID, pass: AC_PASS },
          ...savedCreds,
        ];
        const cred = allSaved.find(c => c.ssid === entry.ssid);

        if (cred) {
          wifi?.connect?.(cred.ssid, cred.pass);
          sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.15, attack: 0.005, decay: 0.06 });
        } else if (entry.encrypted) {
          passwordMode = true;
          password = "";
          cursorBlink = 0;
        } else {
          wifi?.connect?.(entry.ssid, "");
        }
      } else {
        selectedIdx = tappedRow;
        sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.02 });
      }
    }
  }
}

function paint({ wipe, ink, box, write, screen, system, wifi }) {
  frame++;
  const w = screen.width, h = screen.height;

  // Password entry fullscreen
  if (passwordMode && wifi) {
    cursorBlink++;
    const merged = globalThis.__wifiMergedList || [];
    const entry = merged[selectedIdx];
    const ssid = entry ? entry.ssid : "?";

    wipe(10, 10, 15);

    ink(220, 220, 230);
    const titleX = Math.max(10, (w - ssid.length * 18) / 2);
    write(ssid, { x: titleX, y: h / 2 - 50, size: 2, font: "matrix" });

    ink(120, 120, 130);
    write("enter password:", { x: 20, y: h / 2 - 24, size: 2, font: "font_1" });

    // Password field
    ink(25, 25, 30);
    box(18, h / 2 - 6, w - 36, 18, true);
    ink(70, 70, 80);
    box(18, h / 2 - 6, w - 36, 18, "outline");

    const cursor = (cursorBlink % 60) < 35 ? "|" : "";
    ink(220, 220, 230);
    write(password + cursor, { x: 22, y: h / 2 - 2, size: 1, font: "font_1" });

    ink(80, 80, 90);
    write("Enter: connect    Esc: cancel", { x: 20, y: h / 2 + 22, size: 1, font: "font_1" });

    if (wifi.state === 3) {
      ink(200, 200, 80);
      write("connecting...", { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    } else if (wifi.state === 5) {
      ink(220, 80, 80);
      write("failed: " + (wifi.status || "?"), { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    }
    return;
  }

  wipe(10, 10, 15);

  // Title
  ink(220, 220, 230);
  write("WiFi Networks", { x: 20, y: 12, size: 2, font: "matrix" });

  // Status
  ink(120, 120, 130);
  const statusStr = wifi ? ((wifi.status || "scanning...") + (wifi.iface ? " [" + wifi.iface + "]" : "")) : "no wifi";
  write(statusStr, { x: 20, y: 30, size: 1, font: "font_1" });

  if (!wifi) {
    ink(200, 80, 80);
    write("wifi not available", { x: 20, y: 60, size: 1, font: "font_1" });
    ink(60, 80, 60);
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
  const maxRows = Math.min(totalRows, Math.floor((h - listY - 30) / rowH));
  globalThis.__wifiMergedList = [];

  let row = 0;

  // Scanned networks
  for (let i = 0; i < scannedNets.length && row < maxRows; i++, row++) {
    const net = scannedNets[i];
    const ry = listY + row * rowH;
    const isSaved = allSaved.find(c => c.ssid === net.ssid);
    const isSelected = row === selectedIdx;

    if (isSelected) {
      ink(40, 55, 80);
      box(10, ry, w - 20, rowH, true);
    }

    // Signal bars
    const bars = net.signal > -50 ? 4 : net.signal > -60 ? 3 : net.signal > -70 ? 2 : 1;
    for (let b = 0; b < 4; b++) {
      if (b < bars) ink(80, 200, 80);
      else ink(40, 40, 45);
      box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
    }

    // SSID
    if (isSaved) ink(100, 220, 100);
    else ink(220, 220, 230);
    const ssidDisplay = net.ssid.length > 26 ? net.ssid.slice(0, 25) + "~" : net.ssid;
    write(ssidDisplay, { x: 36, y: ry + 2, size: 1, font: "font_1" });

    // Right side
    if (isSelected) {
      ink(80, 180, 255);
      write("connect", { x: w - 52, y: ry + 2, size: 1, font: "font_1" });
    } else if (isSaved) {
      ink(60, 160, 60);
      write("saved", { x: w - 40, y: ry + 2, size: 1, font: "font_1" });
    } else if (net.encrypted) {
      ink(80, 80, 90);
      write("*", { x: w - 20, y: ry + 2, size: 1, font: "font_1" });
    }

    globalThis.__wifiMergedList.push({ type: "scan", idx: i, ssid: net.ssid, encrypted: net.encrypted });
  }

  // Saved/preset networks not in scan
  if (offlineSaved.length > 0 && row < maxRows) {
    const sepY = listY + row * rowH;
    ink(80, 80, 90);
    write("-- saved (not in range) --", { x: 20, y: sepY + 2, size: 1, font: "font_1" });
    globalThis.__wifiMergedList.push({ type: "separator" });
    row++;

    for (let i = 0; i < offlineSaved.length && row < maxRows; i++, row++) {
      const cred = offlineSaved[i];
      const ry = listY + row * rowH;
      const isPreset = cred.ssid === AC_SSID;
      const isSelected = row === selectedIdx;

      if (isSelected) {
        ink(35, 40, 55);
        box(10, ry, w - 20, rowH, true);
      }

      for (let b = 0; b < 4; b++) {
        ink(30, 30, 35);
        box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
      }

      ink(120, 120, 130);
      write(cred.ssid, { x: 36, y: ry + 2, size: 1, font: "font_1" });

      if (isSelected) {
        ink(80, 180, 255);
        write("connect", { x: w - 52, y: ry + 2, size: 1, font: "font_1" });
      } else if (isPreset) {
        ink(80, 120, 160);
        write("preset", { x: w - 44, y: ry + 2, size: 1, font: "font_1" });
      } else {
        ink(60, 140, 60);
        write("saved", { x: w - 40, y: ry + 2, size: 1, font: "font_1" });
      }

      globalThis.__wifiMergedList.push({ type: "saved", ssid: cred.ssid, pass: cred.pass });
    }
  }

  // Connected info
  if (wifi.connected && wifi.ip) {
    ink(80, 200, 80);
    const sshLabel = system?.sshStarted ? "  ssh root@" + wifi.ip : "";
    write("connected: " + wifi.ip + sshLabel, { x: 20, y: h - 26, size: 1, font: "font_1" });
  }

  // Bottom
  ink(60, 80, 60);
  write("esc: back", { x: w - 60, y: h - 26, size: 1, font: "font_1" });

  // Rescan every ~10s when not connected
  if (frame % 600 === 0 && !wifi.connected) {
    wifi.scan?.();
  }
}

function sim() {}

export { boot, paint, act, sim };
