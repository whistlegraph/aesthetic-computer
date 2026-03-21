// FF1, 2026.1.24
// Send pieces to FF1 Art Computer via the Electron bridge.
// Usage: `ff1 starfield` or `ff1 wipe:red` or `ff1 $mycode`

/* #region 📚 README
  This piece integrates with the Aesthetic Computer Electron app's FF1 bridge
  to send artwork to FF1 devices on your local network.

  The bridge runs at localhost:19999 and provides:
  - /status - Check if bridge is running
  - /discover - Find FF1 devices on the network
  - /cast - Send a URL/piece to an FF1 device
  
  This works from aesthetic.computer (HTTPS) because localhost requests
  are allowed from secure contexts (not blocked by Mixed Content).
#endregion */

import { ff1 as bridge } from "./common/ff1.mjs";

let status = "checking"; // checking, connected, disconnected, sending, sent, error
let statusMessage = "Checking for desktop app...";
let devices = [];
let selectedDevice = null;
let pieceToSend = null;
let animFrame = 0;
let lastSendResult = null;

// Colors for different states
const colors = {
  checking: [128, 128, 128],
  connected: [74, 222, 128],
  disconnected: [248, 113, 113],
  sending: [250, 204, 21],
  sent: [74, 222, 128],
  error: [248, 113, 113],
};

export const desc = "Send pieces to FF1 Art Computer";

export function boot({ params, net }) {
  // Get the piece name from params (e.g., "ff1 starfield" -> "starfield")
  pieceToSend = params.join(" ") || null;
  
  // Check for the bridge
  checkBridge();
}

async function checkBridge() {
  const result = await bridge.checkBridge();
  
  if (result.available) {
    status = "connected";
    statusMessage = "Desktop app connected!";
    // Auto-discover devices
    await discoverDevices();
    
    // If we have a piece to send and devices, auto-send
    if (pieceToSend && devices.length > 0) {
      await sendToFF1(pieceToSend);
    }
  } else {
    status = "disconnected";
    statusMessage = result.error || "Desktop app not running";
  }
}

async function discoverDevices() {
  statusMessage = "Discovering FF1 devices...";
  devices = await bridge.discoverDevices(true);
  
  if (devices.length > 0) {
    selectedDevice = devices[0];
    statusMessage = `Found ${devices.length} FF1 device${devices.length > 1 ? "s" : ""}`;
  } else {
    statusMessage = "No FF1 devices found on network";
  }
}

async function sendToFF1(piece) {
  if (!selectedDevice) {
    status = "error";
    statusMessage = "No FF1 device selected";
    return;
  }
  
  status = "sending";
  const deviceName = selectedDevice.info?.deviceId || selectedDevice.ip;
  statusMessage = `Sending "${piece}" to ${deviceName}...`;
  
  const result = await bridge.cast(piece, selectedDevice);
  lastSendResult = result;
  
  if (result.ok) {
    status = "sent";
    statusMessage = `✓ Sent "${piece}" to FF1!`;
  } else {
    status = "error";
    statusMessage = `Failed: ${result.error || "Unknown error"}`;
  }
}

export function sim({ num }) {
  animFrame += 1;
}

export function paint({ wipe, ink, write, screen, num }) {
  // Background based on status
  const bg = status === "sent" ? [20, 40, 20] : [26, 26, 26];
  wipe(bg);
  
  // Title
  ink(255).write("FF1 Art Computer", { x: 12, y: 12 });
  
  // Status indicator with pulsing animation
  const pulse = Math.sin(animFrame * 0.1) * 0.3 + 0.7;
  const statusColor = colors[status].map((c) => Math.floor(c * pulse));
  ink(statusColor).write(
    status === "checking" ? "●" : status === "connected" || status === "sent" ? "●" : "○",
    { x: 12, y: 30 }
  );
  ink(200).write(` ${statusMessage}`, { x: 22, y: 30 });
  
  // Piece info
  if (pieceToSend) {
    ink(150).write("Piece:", { x: 12, y: 56 });
    ink(250, 204, 21).write(` ${pieceToSend}`, { x: 60, y: 56 });
  } else {
    ink(100).write("Usage: ff1 <piece>", { x: 12, y: 56 });
    ink(100).write("Example: ff1 starfield", { x: 12, y: 72 });
  }
  
  // Device list
  if (devices.length > 0) {
    ink(150).write("Devices:", { x: 12, y: 96 });
    devices.forEach((device, i) => {
      const name = device.info?.deviceId || device.deviceId || `Device ${i + 1}`;
      const ip = device.ip || "";
      const isSelected = device === selectedDevice;
      ink(isSelected ? [74, 222, 128] : [100, 100, 100]).write(
        `${isSelected ? "→ " : "  "}${name} (${ip})`,
        { x: 12, y: 112 + i * 16 }
      );
    });
  }
  
  // Instructions at bottom
  if (status === "disconnected") {
    ink(248, 113, 113).write(
      "Download Aesthetic Computer app:",
      { x: 12, y: screen.height - 44 }
    );
    ink(100).write(
      "github.com/whistlegraph/aesthetic-computer/releases",
      { x: 12, y: screen.height - 28 }
    );
  }
  
  if (status === "connected" && !pieceToSend) {
    ink(100).write(
      "Enter: ff1 <piece-name> to send",
      { x: 12, y: screen.height - 28 }
    );
  }
}

export function act({ event: e, jump }) {
  // Press Enter to return to prompt
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:enter")) {
    jump("prompt");
  }
  
  // R to refresh/retry
  if (e.is("keyboard:down:r")) {
    status = "checking";
    statusMessage = "Retrying...";
    checkBridge();
  }
  
  // Number keys to select device
  if (e.is("keyboard:down:1") && devices[0]) selectedDevice = devices[0];
  if (e.is("keyboard:down:2") && devices[1]) selectedDevice = devices[1];
  if (e.is("keyboard:down:3") && devices[2]) selectedDevice = devices[2];
  
  // S to send again
  if (e.is("keyboard:down:s") && pieceToSend && selectedDevice) {
    sendToFF1(pieceToSend);
  }
}

export function meta() {
  return {
    title: "FF1 · Aesthetic Computer",
    desc: "Send artwork to FF1 Art Computer",
  };
}
