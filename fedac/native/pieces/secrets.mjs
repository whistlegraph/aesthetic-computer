// secrets.mjs — Show config.json secrets status on AC Native
// Values hidden by default, tap/enter on a row to reveal, tap again to hide.
// Jumped to from prompt via "secrets" command.

let items = []; // { key, status, value, revealed }
let selectedIdx = 0;
let frame = 0;
let storageDevice = "";

function boot({ system }) {
  storageDevice = system?.bootDevice || "?";
  items = [];

  // Read config.json
  const raw = system?.readFile?.("/mnt/config.json");
  let cfg = {};
  if (raw) {
    try { cfg = JSON.parse(raw); } catch {}
  }

  // Handle
  items.push({
    key: "handle",
    status: cfg.handle ? "set" : "missing",
    value: cfg.handle ? "@" + cfg.handle : "",
    revealed: true, // always show handle
  });

  // AC token
  items.push({
    key: "ac token",
    status: cfg.token ? "set (" + cfg.token.length + " chars)" : "missing",
    value: cfg.token || "",
    revealed: false,
  });

  // User sub
  items.push({
    key: "user sub",
    status: cfg.sub ? "set" : "missing",
    value: cfg.sub || "",
    revealed: false,
  });

  // Email
  items.push({
    key: "email",
    status: cfg.email ? "set" : "missing",
    value: cfg.email || "",
    revealed: false,
  });

  // Claude credentials
  const cc = cfg.claudeCreds;
  if (cc) {
    const tokenPreview = cc.claudeAiOauth?.accessToken ? "token:" + cc.claudeAiOauth.accessToken.length + "chars" : "no token";
    items.push({
      key: "claude creds",
      status: "set (" + tokenPreview + ")",
      value: JSON.stringify(cc).slice(0, 200),
      revealed: false,
    });
  } else {
    items.push({ key: "claude creds", status: "missing", value: "", revealed: false });
  }

  // Claude state
  items.push({
    key: "claude state",
    status: cfg.claudeState ? "set" : "missing",
    value: cfg.claudeState ? JSON.stringify(cfg.claudeState).slice(0, 200) : "",
    revealed: false,
  });

  // WiFi saved creds
  const wifiRaw = system?.readFile?.("/mnt/wifi_creds.json");
  if (wifiRaw) {
    try {
      const nets = JSON.parse(wifiRaw);
      const ssids = nets.map(n => n.ssid).join(", ");
      items.push({
        key: "wifi creds",
        status: nets.length + " networks",
        value: ssids,
        revealed: false,
      });
    } catch {
      items.push({ key: "wifi creds", status: "parse error", value: wifiRaw.slice(0, 100), revealed: false });
    }
  } else {
    items.push({ key: "wifi creds", status: "no file", value: "", revealed: false });
  }

  // Piece
  items.push({
    key: "default piece",
    status: cfg.piece || "notepat",
    value: cfg.piece || "notepat",
    revealed: true,
  });

  // Boot device
  items.push({
    key: "boot device",
    status: storageDevice,
    value: storageDevice,
    revealed: true,
  });

  // Version
  items.push({
    key: "version",
    status: system?.version || "unknown",
    value: system?.version || "unknown",
    revealed: true,
  });
}

function act({ event: e, sound, system }) {
  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }

  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:tab")) {
    selectedIdx = (selectedIdx + 1) % Math.max(1, items.length);
    sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.025 });
    return;
  }
  if (e.is("keyboard:down:arrowup")) {
    selectedIdx = (selectedIdx - 1 + items.length) % Math.max(1, items.length);
    sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.025 });
    return;
  }

  // Toggle reveal
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return") || e.is("keyboard:down:space")) {
    if (items[selectedIdx]) {
      items[selectedIdx].revealed = !items[selectedIdx].revealed;
      sound?.synth({ type: "sine", tone: items[selectedIdx].revealed ? 523 : 392, duration: 0.04, volume: 0.08, attack: 0.002, decay: 0.03 });
    }
    return;
  }
}

function paint({ wipe, ink, box, write, screen }) {
  frame++;
  const T = __theme.update();
  const w = screen.width, h = screen.height;
  const pad = 10;
  const font = "font_1";

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  ink(T.fg, T.fg + 10, T.fg);
  write("secrets", { x: pad, y: 10, size: 2, font: "matrix" });

  ink(T.fgMute, T.fgMute, T.fgMute + 10);
  write(storageDevice + " /mnt/config.json", { x: pad, y: 34, size: 1, font });

  const listY = 50;
  const rowH = 22;

  for (let i = 0; i < items.length; i++) {
    const item = items[i];
    const ry = listY + i * rowH;
    const selected = i === selectedIdx;

    if (ry > h - 20) break;

    // Selection highlight
    if (selected) {
      ink(30, 45, 55);
      box(pad - 2, ry - 2, w - pad * 2 + 4, rowH - 2, true);
    }

    // Key name
    ink(selected ? 200 : 120, selected ? 200 : 120, selected ? 220 : 140);
    write(item.key, { x: pad, y: ry, size: 1, font });

    // Status (always visible)
    const statusColor = item.status.startsWith("missing") || item.status.startsWith("no ")
      ? [200, 80, 80] : [80, 180, 100];
    ink(...statusColor);
    write(item.status, { x: pad + 90, y: ry, size: 1, font });

    // Value (only if revealed and has content)
    if (item.revealed && item.value) {
      ink(160, 160, 120);
      const maxChars = Math.floor((w - pad * 2 - 90) / 6);
      write(item.value.slice(0, maxChars), { x: pad + 4, y: ry + 10, size: 1, font });
    } else if (!item.revealed && item.value) {
      ink(60, 60, 70);
      write("enter to reveal", { x: pad + 90, y: ry + 10, size: 1, font });
    }
  }

  // Bottom
  ink(T.fgMute, T.fgMute + 10, T.fgMute);
  write("arrows: select  enter: reveal/hide  esc: back", { x: pad, y: h - 12, size: 1, font });
}

function sim() {}

export { boot, paint, act, sim };
