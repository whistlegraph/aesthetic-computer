// Machines, 2026.03.13
// Dashboard for monitoring ac-native devices remotely.
// Shows live status, logs, and supports remote commands (jump, reboot, update).

const { max, min, floor } = Math;

const ONLINE_THRESHOLD_MS = 90_000;
const HEARTBEAT_FADE_MS = 3000;
const RECONNECT_MS = 5000;
const SESSION_HOST = "session-server.aesthetic.computer";

// ── State ──────────────────────────────────────────────────
let machines = [];
let selectedMachine = null;
let logs = [];
let machineScroll = 0;
let logScroll = 0;
let liveWs = null;
let authToken = null;
let loading = true;
let error = null;
let disposed = false;
let reconnectTimer = null;
let ellipsisTicker;

// Commands
let confirmReboot = null; // machineId being confirmed
let commandPending = null; // { machineId, cmd, sentAt }
let jumpInputActive = false;
let jumpText = "";

// UI
let backBtn = null;
let jumpBtn = null;
let updateBtn = null;
let rebootBtn = null;
let confirmYesBtn = null;
let confirmNoBtn = null;
let loginBtn = null;

// ── Helpers ────────────────────────────────────────────────
function formatUptime(seconds) {
  if (!seconds || seconds < 0) return "-";
  const h = floor(seconds / 3600);
  const m = floor((seconds % 3600) / 60);
  const s = floor(seconds % 60);
  if (h > 0) return `${h}h ${m}m`;
  if (m > 0) return `${m}m ${s}s`;
  return `${s}s`;
}

function shortHash(h) {
  return h ? h.slice(0, 7) : "?";
}

function machineLabel(m) {
  return m.label || m.machineId || "unknown";
}

function machineStatus(m) {
  if (m.status === "crashed") return "crashed";
  if (!m.lastSeen) return "offline";
  if (Date.now() - new Date(m.lastSeen).getTime() > ONLINE_THRESHOLD_MS)
    return "offline";
  return "online";
}

function statusColor(status) {
  if (status === "online") return [80, 220, 80];
  if (status === "crashed") return [220, 60, 60];
  return [100, 100, 100];
}

function updateMachineInList(machineId, updates) {
  const idx = machines.findIndex((m) => m.machineId === machineId);
  if (idx >= 0) Object.assign(machines[idx], updates);
}

// ── WebSocket ──────────────────────────────────────────────
function connectViewer(dev) {
  if (disposed || !authToken) return;
  const protocol = dev ? "ws" : "wss";
  const host = dev ? "localhost:8889" : SESSION_HOST;
  const url = `${protocol}://${host}/machines?role=viewer&token=${encodeURIComponent(authToken)}`;

  try {
    liveWs = new WebSocket(url);
  } catch {
    scheduleReconnect(dev);
    return;
  }

  liveWs.onopen = () => {};

  liveWs.onmessage = (e) => {
    try {
      const msg = JSON.parse(e.data);
      handleWsMessage(msg);
    } catch {}
  };

  liveWs.onclose = () => {
    liveWs = null;
    scheduleReconnect(dev);
  };

  liveWs.onerror = () => {};
}

function scheduleReconnect(dev) {
  if (disposed) return;
  clearTimeout(reconnectTimer);
  reconnectTimer = setTimeout(() => connectViewer(dev), RECONNECT_MS);
}

function handleWsMessage(msg) {
  switch (msg.type) {
    case "machines-state":
      // Merge live state with fetched machines
      if (msg.machines) {
        for (const live of msg.machines) {
          updateMachineInList(live.machineId, {
            ...live,
            status: "online",
            lastSeen: new Date(),
          });
        }
      }
      break;

    case "heartbeat":
      updateMachineInList(msg.machineId, {
        uptime: msg.uptime,
        currentPiece: msg.currentPiece,
        battery: msg.battery,
        charging: msg.charging,
        fps: msg.fps,
        lastSeen: new Date(),
        status: "online",
        _heartbeatAt: Date.now(),
      });
      break;

    case "machine-registered":
      // New or updated machine
      {
        const idx = machines.findIndex(
          (m) => m.machineId === msg.machineId,
        );
        if (idx >= 0) {
          Object.assign(machines[idx], msg, {
            lastSeen: new Date(),
            status: "online",
          });
        } else {
          machines.unshift({
            ...msg,
            lastSeen: new Date(),
            status: "online",
          });
        }
      }
      break;

    case "device-connected":
      updateMachineInList(msg.machineId, {
        status: "online",
        lastSeen: new Date(),
      });
      break;

    case "status-change":
      updateMachineInList(msg.machineId, { status: msg.status });
      break;

    case "log":
      if (selectedMachine?.machineId === msg.machineId) {
        logs.unshift({
          level: msg.level,
          message: msg.message,
          type: msg.logType,
          when: msg.when,
        });
        if (logs.length > 200) logs.length = 200;
      }
      break;

    case "command-ack":
      if (
        commandPending &&
        commandPending.machineId === msg.machineId &&
        commandPending.cmd === msg.command
      ) {
        commandPending.status = "acked";
        commandPending.resolvedAt = Date.now();
      }
      break;

    case "command-response":
      if (msg.command === "request-logs" && msg.data) {
        if (selectedMachine?.machineId === msg.machineId) {
          // Prepend fetched logs (data is raw text from device)
          const logText = typeof msg.data === "string" ? msg.data : JSON.stringify(msg.data);
          const fetched = logText
            .split("\n")
            .filter(Boolean)
            .map((line) => ({
              level: "info",
              message: line,
              type: "log",
              when: new Date().toISOString(),
            }));
          logs.unshift(...fetched);
          if (logs.length > 500) logs.length = 500;
        }
      }
      break;
  }
}

function sendCommand(machineId, cmd, args = {}) {
  if (!liveWs || liveWs.readyState !== WebSocket.OPEN) return;
  liveWs.send(JSON.stringify({ type: "command", machineId, cmd, args }));
  commandPending = { machineId, cmd, sentAt: Date.now(), status: null };
}

// ── Lifecycle ──────────────────────────────────────────────
async function boot({
  params,
  user,
  handle,
  authorize,
  gizmo,
  net,
  store,
  hud,
  dev,
}) {
  disposed = false;
  loading = true;
  error = null;
  machines = [];
  selectedMachine = null;
  logs = [];
  machineScroll = 0;
  logScroll = 0;
  confirmReboot = null;
  commandPending = null;
  jumpInputActive = false;
  jumpText = "";

  ellipsisTicker = new gizmo.EllipsisTicker();

  if (!user) {
    loading = false;
    error = "log-in";
    return;
  }

  hud.label("machines");

  try {
    authToken = await authorize();
    const res = await fetch("/api/machines", {
      headers: { Authorization: `Bearer ${authToken}` },
    });
    const data = await res.json();
    if (data.success !== false) {
      machines = data.machines || data.devices || [];
    } else {
      error = data.error || "Failed to load machines";
    }
  } catch (err) {
    error = err.message || "Network error";
  }

  loading = false;
  connectViewer(dev);
}

function paint({ wipe, ink, write, box, line, screen, help, pen }) {
  wipe(20, 18, 28);

  if (loading) {
    ink(180).write(
      `Loading${ellipsisTicker.text(help.repeat)}`,
      { center: "xy" },
      screen,
    );
    return;
  }

  if (error === "log-in") {
    ink(160).write("Log in to view machines.", { center: "xy" }, screen);
    return;
  }

  if (error) {
    ink(220, 80, 80).write(error, { center: "xy" }, screen);
    return;
  }

  if (confirmReboot) {
    paintRebootConfirm({ ink, screen });
    return;
  }

  if (selectedMachine) {
    paintDetail({ ink, line, screen, help });
  } else {
    paintList({ ink, line, screen });
  }
}

function paintList({ ink, line, screen }) {
  const M = 6;
  const ROW_H = 28;
  let y = 4;

  // Header
  ink(140, 160, 220).write("MACHINES", { x: M, y });
  y += 14;
  ink(40, 40, 55).line(0, y, screen.width, y);
  y += 4;

  const contentTop = y;

  if (machines.length === 0) {
    ink(100).write("No machines found.", { x: M, y: y + 8 });
    return;
  }

  // Machine cards
  for (let i = 0; i < machines.length; i++) {
    const m = machines[i];
    const cardY = contentTop + i * ROW_H + machineScroll;

    // Skip off-screen
    if (cardY + ROW_H < contentTop || cardY > screen.height) continue;

    const status = machineStatus(m);
    const sc = statusColor(status);

    // Status dot
    const dotChar = status === "offline" ? "o" : "*";
    ink(...sc).write(dotChar, { x: M, y: cardY + 1 });

    // Label
    const labelColor = status === "online" ? [240, 240, 240] : [120, 120, 120];
    ink(...labelColor).write(machineLabel(m), { x: M + 10, y: cardY });

    // Version hash (right-aligned area)
    const hashStr = `${m.buildName || "?"} ${shortHash(m.gitHash)}`;
    ink(80, 100, 130).write(hashStr, { x: screen.width - M - hashStr.length * 6, y: cardY });

    // Second line: IP, uptime, status
    const ip = m.ip || "-";
    const up = status === "online" ? `up ${formatUptime(m.uptime)}` : "";
    const line2 = `  ${ip}  ${up}`;
    ink(80, 80, 100).write(line2, { x: M + 8, y: cardY + 10 });

    // Status text
    ink(...sc).write(status, {
      x: screen.width - M - status.length * 6,
      y: cardY + 10,
    });

    // Heartbeat flash
    if (m._heartbeatAt && Date.now() - m._heartbeatAt < HEARTBEAT_FADE_MS) {
      const alpha = 1 - (Date.now() - m._heartbeatAt) / HEARTBEAT_FADE_MS;
      const g = floor(40 * alpha);
      if (g > 0) ink(20 + g, 18 + g, 28 + g * 2).box(0, cardY - 2, screen.width, ROW_H);
    }
  }

  // Bottom summary
  const online = machines.filter((m) => machineStatus(m) === "online").length;
  const offline = machines.length - online;
  const summary = `${online} online  ${offline} offline`;
  ink(80, 80, 100).write(summary, {
    x: M,
    y: screen.height - 14,
  });

  // Scrollbar
  const totalH = machines.length * ROW_H;
  const viewH = screen.height - contentTop - 18;
  if (totalH > viewH) {
    const thumbH = max(8, floor((viewH / totalH) * viewH));
    const thumbY =
      contentTop + floor((-machineScroll / totalH) * viewH);
    ink(60, 60, 80).box(screen.width - 3, thumbY, 2, thumbH);
  }
}

function paintDetail({ ink, line, screen, help }) {
  const M = 6;
  const m = selectedMachine;
  const status = machineStatus(m);
  const sc = statusColor(status);
  let y = 4;

  // Header: back + label + status
  ink(100, 180, 255).write("<", { x: M, y });
  ink(240).write(machineLabel(m), { x: M + 12, y });
  ink(...sc).write(status, {
    x: screen.width - M - status.length * 6,
    y,
  });

  y += 14;
  ink(40, 40, 55).line(0, y, screen.width, y);
  y += 6;

  // Info pairs
  const batStr = m.battery >= 0
    ? `${m.battery}%${m.charging ? " charging" : ""}`
    : "-";
  const info = [
    ["Version", m.version || `${m.buildName || "?"} ${shortHash(m.gitHash)}`],
    ["Piece", m.currentPiece || "notepat"],
    ["IP", m.ip || "-"],
    ["WiFi", m.wifiSSID || "-"],
    ["Battery", batStr],
    ["Uptime", status === "online" ? formatUptime(m.uptime) : "-"],
  ];
  if (m.fps > 0) info.push(["FPS", `${m.fps}`]);

  if (m.hw) {
    if (m.hw.display) info.push(["Display", m.hw.display]);
    if (m.hw.sampleRate) info.push(["Audio", `${m.hw.sampleRate / 1000}kHz`]);
    if (m.hw.keyboard) info.push(["Keyboard", m.hw.keyboard]);
  }

  for (const [label, value] of info) {
    ink(80, 100, 130).write(`${label}:`, { x: M, y });
    ink(200).write(value, { x: M + (label.length + 1) * 6 + 4, y });
    y += 11;
  }

  y += 4;
  ink(40, 40, 55).line(0, y, screen.width, y);
  y += 6;

  // Commands header
  ink(140, 160, 220).write("COMMANDS", { x: M, y });
  y += 14;

  // Command buttons (simple text buttons)
  const btnY = y;
  const btnH = 12;

  if (jumpInputActive) {
    ink(80).write("Piece:", { x: M, y: btnY });
    ink(255).write(jumpText + "_", { x: M + 42, y: btnY });
    y += btnH + 4;
  } else {
    // Jump button
    ink(100, 200, 255).write("[Jump]", { x: M, y: btnY });
    // Update button
    ink(100, 220, 100).write("[Update]", { x: M + 44, y: btnY });
    // Reboot button
    ink(220, 100, 100).write("[Reboot]", { x: M + 100, y: btnY });
    y += btnH + 4;
  }

  // Command pending indicator
  if (commandPending && commandPending.machineId === m.machineId) {
    const age = Date.now() - commandPending.sentAt;
    if (commandPending.status) {
      ink(100, 220, 100).write(
        `${commandPending.cmd}: ${commandPending.status}`,
        { x: M, y },
      );
    } else if (age < 10000) {
      ink(200, 200, 100).write(
        `${commandPending.cmd}${ellipsisTicker.text(help.repeat)}`,
        { x: M, y },
      );
    }
    y += 12;
  }

  y += 2;
  ink(40, 40, 55).line(0, y, screen.width, y);
  y += 6;

  // Logs header
  ink(140, 160, 220).write("LOGS", { x: M, y });

  if (status === "online") {
    ink(60, 180, 60).write("live", { x: M + 36, y });
  }
  y += 14;

  const logTop = y;

  if (logs.length === 0) {
    ink(70).write("No logs yet.", { x: M, y });
    return;
  }

  // Render log lines
  const maxLogLines = floor((screen.height - logTop - 4) / 10);
  const visible = logs.slice(0, max(maxLogLines + floor(-logScroll / 10), 50));

  for (let i = 0; i < visible.length; i++) {
    const logY = logTop + i * 10 + logScroll;
    if (logY + 10 < logTop || logY > screen.height) continue;

    const entry = visible[i];
    const time = entry.when
      ? new Date(entry.when).toLocaleTimeString("en-US", {
          hour12: false,
          hour: "2-digit",
          minute: "2-digit",
          second: "2-digit",
        })
      : "--:--:--";

    const levelColor =
      entry.level === "error" || entry.level === "fatal"
        ? [220, 80, 80]
        : entry.level === "warn"
          ? [220, 180, 60]
          : [70, 70, 90];

    ink(...levelColor).write(time, { x: M, y: logY });
    ink(160).write(
      (entry.message || "").slice(0, floor((screen.width - M - 60) / 6)),
      { x: M + 54, y: logY },
    );
  }
}

function paintRebootConfirm({ ink, screen }) {
  // Semi-transparent overlay
  ink(10, 8, 18).box(0, 0, screen.width, screen.height);

  const m = machines.find((m) => m.machineId === confirmReboot);
  const label = m ? machineLabel(m) : confirmReboot;

  const cy = floor(screen.height / 2);
  ink(220, 100, 100).write(`Reboot ${label}?`, { center: "x", y: cy - 16 }, screen);
  ink(100, 220, 100).write("[Yes]", { x: floor(screen.width / 2) - 30, y: cy + 8 });
  ink(160).write("[No]", { x: floor(screen.width / 2) + 10, y: cy + 8 });
}

function act({ event: e, needsPaint, jump, pen }) {
  if (e.is("reframed")) {
    backBtn = null;
    jumpBtn = null;
    updateBtn = null;
    rebootBtn = null;
    confirmYesBtn = null;
    confirmNoBtn = null;
    loginBtn = null;
    needsPaint();
    return;
  }

  // Scroll
  if (e.is("scroll")) {
    if (selectedMachine) {
      logScroll = min(0, logScroll - e.y);
    } else {
      const ROW_H = 28;
      const maxScroll = -max(0, machines.length * ROW_H - 200);
      machineScroll = max(maxScroll, min(0, machineScroll - e.y));
    }
    needsPaint();
    return;
  }

  // Keyboard shortcuts
  if (e.is("keyboard:down:escape")) {
    if (jumpInputActive) {
      jumpInputActive = false;
      jumpText = "";
    } else if (confirmReboot) {
      confirmReboot = null;
    } else if (selectedMachine) {
      selectedMachine = null;
      logs = [];
      logScroll = 0;
    }
    needsPaint();
    return;
  }

  if (e.is("keyboard:down:r") && !jumpInputActive) {
    // Refresh
    refreshMachines();
    needsPaint();
    return;
  }

  // Jump text input
  if (jumpInputActive) {
    if (e.is("keyboard:down:enter") && jumpText.length > 0) {
      sendCommand(selectedMachine.machineId, "jump", { piece: jumpText });
      jumpInputActive = false;
      jumpText = "";
      needsPaint();
      return;
    }
    if (e.is("keyboard:down:backspace")) {
      jumpText = jumpText.slice(0, -1);
      needsPaint();
      return;
    }
    // Printable character
    const keyMatch = e.key?.match?.(/^keyboard:down:(.+)$/);
    if (keyMatch && keyMatch[1].length === 1) {
      jumpText += keyMatch[1];
      needsPaint();
      return;
    }
    return;
  }

  // Reboot confirmation
  if (confirmReboot && e.is("touch")) {
    const px = pen?.x || 0;
    const py = pen?.y || 0;
    const cy = floor(e.screen?.height || 200) / 2;
    const midX = floor((e.screen?.width || 320) / 2);

    // Yes button
    if (py >= cy + 4 && py <= cy + 20 && px >= midX - 30 && px < midX + 6) {
      sendCommand(confirmReboot, "reboot");
      confirmReboot = null;
      needsPaint();
      return;
    }
    // No button
    if (py >= cy + 4 && py <= cy + 20 && px >= midX + 10 && px < midX + 40) {
      confirmReboot = null;
      needsPaint();
      return;
    }
    return;
  }

  // Touch on list items or detail buttons
  if (e.is("touch")) {
    const px = pen?.x || 0;
    const py = pen?.y || 0;
    const M = 6;

    if (error === "log-in") {
      jump("prompt");
      return;
    }

    if (selectedMachine) {
      // Back button
      if (py < 18 && px < 20) {
        selectedMachine = null;
        logs = [];
        logScroll = 0;
        needsPaint();
        return;
      }

      // Command buttons (at row ~after info)
      // Approximate: commands area starts around y=120-160 depending on hw info
      // We use the text positions: [Jump] starts at M, [Update] at M+44, [Reboot] at M+100
      const cmdY = findCommandY();
      if (py >= cmdY && py < cmdY + 14) {
        if (px >= M && px < M + 40) {
          // Jump
          jumpInputActive = true;
          jumpText = "";
          needsPaint();
          return;
        }
        if (px >= M + 44 && px < M + 96) {
          // Update
          sendCommand(selectedMachine.machineId, "update");
          needsPaint();
          return;
        }
        if (px >= M + 100 && px < M + 152) {
          // Reboot
          confirmReboot = selectedMachine.machineId;
          needsPaint();
          return;
        }
      }
    } else {
      // Tap on machine card
      const contentTop = 22;
      const ROW_H = 28;
      const idx = floor((py - contentTop - machineScroll) / ROW_H);
      if (idx >= 0 && idx < machines.length) {
        selectedMachine = machines[idx];
        logs = [];
        logScroll = 0;
        fetchLogs(selectedMachine.machineId);
        needsPaint();
        return;
      }
    }
  }
}

function sim() {
  ellipsisTicker?.sim();

  // Clear stale command pending after 10s
  if (commandPending && Date.now() - commandPending.sentAt > 10000) {
    if (!commandPending.status) commandPending.status = "timeout";
  }
  if (
    commandPending?.resolvedAt &&
    Date.now() - commandPending.resolvedAt > 3000
  ) {
    commandPending = null;
  }
}

function leave({ store }) {
  disposed = true;
  clearTimeout(reconnectTimer);
  if (liveWs) {
    liveWs.onclose = null;
    liveWs.close();
    liveWs = null;
  }
  store["machines:scroll"] = machineScroll;
  store.persist("machines:scroll");
}

// ── API helpers ────────────────────────────────────────────
async function refreshMachines() {
  if (!authToken) return;
  try {
    const res = await fetch("/api/machines", {
      headers: { Authorization: `Bearer ${authToken}` },
    });
    const data = await res.json();
    if (data.success !== false) {
      machines = data.machines || data.devices || [];
    }
  } catch {}
}

async function fetchLogs(machineId) {
  if (!authToken) return;
  try {
    const res = await fetch(
      `/api/machine-logs?machineId=${encodeURIComponent(machineId)}&limit=100`,
      { headers: { Authorization: `Bearer ${authToken}` } },
    );
    const data = await res.json();
    if (data.logs) logs = data.logs;
  } catch {}

  // Also request live logs from device
  sendCommand(machineId, "request-logs", { lines: 50 });
}

function findCommandY() {
  // Estimate where command buttons are based on info fields
  const m = selectedMachine;
  let y = 24; // after header + divider
  let infoCount = 6; // base info fields (version, piece, ip, wifi, battery, uptime)
  if (m?.fps > 0) infoCount++;
  if (m?.hw) {
    if (m.hw.display) infoCount++;
    if (m.hw.sampleRate) infoCount++;
    if (m.hw.keyboard) infoCount++;
  }
  y += infoCount * 11 + 10 + 14; // info rows + gaps + COMMANDS header
  return y;
}

export { boot, paint, act, sim, leave };
export const noBios = true;
