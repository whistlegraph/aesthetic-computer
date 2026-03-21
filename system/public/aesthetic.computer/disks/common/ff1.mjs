// FF1 Bridge API, 2026.1.24
// Common module for communicating with FF1 Art Computer via Electron bridge.
//
// Usage in a piece:
//   import { ff1 } from "../disks/common/ff1.mjs";
//   
//   // Check if bridge is available
//   const status = await ff1.checkBridge();
//   
//   // Discover devices
//   const devices = await ff1.discoverDevices();
//   
//   // Send a piece to FF1
//   await ff1.cast("starfield", devices[0]);

const FF1_BRIDGE_URL = "http://127.0.0.1:19999";

// Cache for bridge status
let bridgeStatus = null;
let lastStatusCheck = 0;
const STATUS_CACHE_MS = 5000;

// Cache for discovered devices
let discoveredDevices = [];
let lastDiscovery = 0;
const DISCOVERY_CACHE_MS = 10000;

/**
 * Check if the Electron FF1 bridge is running
 * @returns {Promise<{available: boolean, version?: string, error?: string}>}
 */
export async function checkBridge(force = false) {
  // Return cached result if recent
  if (!force && bridgeStatus && Date.now() - lastStatusCheck < STATUS_CACHE_MS) {
    return bridgeStatus;
  }
  
  try {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 3000);
    
    const response = await fetch(`${FF1_BRIDGE_URL}/status`, {
      signal: controller.signal,
    });
    clearTimeout(timeout);
    
    if (response.ok) {
      const data = await response.json();
      bridgeStatus = {
        available: data.ok === true,
        version: data.version,
      };
    } else {
      bridgeStatus = { available: false, error: "Bridge returned error" };
    }
  } catch (e) {
    bridgeStatus = {
      available: false,
      error: e.name === "AbortError" ? "Timeout" : "Not running",
    };
  }
  
  lastStatusCheck = Date.now();
  return bridgeStatus;
}

/**
 * Discover FF1 devices on the local network
 * @returns {Promise<Array<{ip: string, deviceId: string, info: object}>>}
 */
export async function discoverDevices(force = false) {
  // Return cached result if recent
  if (!force && discoveredDevices.length > 0 && Date.now() - lastDiscovery < DISCOVERY_CACHE_MS) {
    return discoveredDevices;
  }
  
  // Check bridge first
  const status = await checkBridge();
  if (!status.available) {
    return [];
  }
  
  try {
    const response = await fetch(`${FF1_BRIDGE_URL}/discover`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({}),
    });
    
    const data = await response.json();
    if (data.ok && data.devices) {
      discoveredDevices = data.devices;
      lastDiscovery = Date.now();
    }
  } catch (e) {
    console.warn("[ff1] Discovery failed:", e);
  }
  
  return discoveredDevices;
}

/**
 * Get the first discovered device (convenience method)
 * @returns {Promise<object|null>}
 */
export async function getDefaultDevice() {
  const devices = await discoverDevices();
  return devices[0] || null;
}

/**
 * Build a DP-1 playlist for a URL
 * @param {string} url - URL to display
 * @param {number} duration - Duration in seconds (0 = infinite)
 * @returns {object} DP-1 playlist object
 */
export function buildPlaylist(url, duration = 0) {
  return {
    dpVersion: "1.0.0",
    items: [{
      source: url,
      duration: duration,
      license: "open",
      provenance: {
        type: "offChainURI",
        uri: url,
      },
    }],
  };
}

/**
 * Cast a URL or piece to an FF1 device
 * @param {string} urlOrPiece - Full URL or piece name (e.g., "starfield")
 * @param {object} device - Device object from discoverDevices()
 * @param {object} options - Options like { duration: 0 }
 * @returns {Promise<{ok: boolean, error?: string, response?: object}>}
 */
export async function cast(urlOrPiece, device = null, options = {}) {
  // Get default device if not specified
  if (!device) {
    device = await getDefaultDevice();
    if (!device) {
      return { ok: false, error: "No FF1 device available" };
    }
  }
  
  // Build URL if it's a piece name
  let url = urlOrPiece;
  if (!urlOrPiece.startsWith("http")) {
    url = `https://aesthetic.computer/${urlOrPiece}`;
  }
  
  // Build playlist
  const playlist = buildPlaylist(url, options.duration || 0);
  
  // Build request payload
  const payload = {
    deviceId: device.info?.deviceId || device.deviceId,
    deviceIp: device.ip,
    command: "displayPlaylist",
    request: {
      playlist: playlist,
      intent: { action: "now_display" },
    },
  };
  
  try {
    const response = await fetch(`${FF1_BRIDGE_URL}/cast`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    
    const result = await response.json();
    return {
      ok: result.ok === true,
      error: result.error,
      response: result.response,
    };
  } catch (e) {
    return { ok: false, error: e.message };
  }
}

/**
 * Cast KidLisp code to FF1 via device.kidlisp.com
 * @param {string} code - KidLisp source code
 * @param {object} device - Device object
 * @returns {Promise<{ok: boolean, error?: string}>}
 */
export async function castKidlisp(code, device = null) {
  // Encode code for URL
  const encoded = encodeURIComponent(code);
  // Use device.kidlisp.com for FF1-optimized display
  const url = `https://device.kidlisp.com/?code=${encoded}`;
  return cast(url, device);
}

/**
 * Stop playback on FF1 device (send blank screen)
 * @param {object} device - Device object
 * @returns {Promise<{ok: boolean, error?: string}>}
 */
export async function stop(device = null) {
  return cast("https://aesthetic.computer/blank", device);
}

// Export as namespace object for convenient import
export const ff1 = {
  checkBridge,
  discoverDevices,
  getDefaultDevice,
  buildPlaylist,
  cast,
  castKidlisp,
  stop,
  BRIDGE_URL: FF1_BRIDGE_URL,
};

export default ff1;
