/**
 * KidLisp Editor - FF1 Art Computer Integration
 * 
 * Sends artwork URLs to Feral File FF1 devices using the DP-1 protocol.
 * 
 * Connection Methods (in priority order):
 *   1. Electron Bridge: localhost:19999 (Aesthetic Computer desktop app)
 *   2. Session Server Proxy: localhost:8889 (dev) or aesthetic.computer/api/ff1/proxy
 *   3. Cloud Relay: artwork-info.feral-file.workers.dev (requires topicID + apiKey)
 * 
 * The Electron app can discover and communicate with FF1 devices on the local
 * network, bypassing browser security restrictions.
 */

import { state, setState, events } from './state.js';

// ============================================
// FF1 CONFIGURATION
// ============================================

const FF1_CONFIG_KEY = 'kidlisp-ff1-config';
const ELECTRON_BRIDGE_URL = 'http://127.0.0.1:19999';

// Bridge status
let electronBridgeAvailable = null; // null = not checked, true/false = checked
let electronBridgeDevices = [];

// Default configuration
// To use FF1 with kidlisp.com:
// - Best: Install Aesthetic Computer desktop app (auto-discovers FF1s)
// - Alternative: Get Topic ID from FF1 app > Settings > Developer
const defaultConfig = {
  deviceId: '',           // e.g., 'FF1-DVVEKLZA' (from Electron bridge or manual)
  deviceIp: '',           // Direct IP fallback (rarely needed)
  useDirectApi: false,    // false = use session-server proxy (recommended)
  relayApiKey: '',        // Optional API key for cloud relay
  relayTopicId: '',       // Topic ID from FF1 app settings (for cloud relay)
  lastConnected: null,    // Timestamp of last successful connection
  deviceInfo: null,       // Cached device info
  preferElectronBridge: true, // Try Electron bridge first
};

let config = { ...defaultConfig };

// ============================================
// CONFIG PERSISTENCE
// ============================================

export function loadConfig() {
  try {
    const stored = localStorage.getItem(FF1_CONFIG_KEY);
    if (stored) {
      config = { ...defaultConfig, ...JSON.parse(stored) };
    }
  } catch (e) {
    console.warn('⚠️ Failed to load FF1 config:', e);
  }
  return config;
}

export function saveConfig(newConfig) {
  config = { ...config, ...newConfig };
  localStorage.setItem(FF1_CONFIG_KEY, JSON.stringify(config));
  console.log('💾 FF1 config saved:', config);
  return config;
}

export function getConfig() {
  return { ...config };
}

// ============================================
// ELECTRON BRIDGE (Aesthetic Computer Desktop App)
// ============================================

/**
 * Check if the Aesthetic Computer Electron app is running
 * with FF1 bridge enabled
 */
export async function checkElectronBridge() {
  // Safari blocks HTTP requests from HTTPS pages (mixed content).
  // Skip on production HTTPS to avoid repeated console errors.
  if (location.protocol === 'https:' && location.hostname !== 'localhost') {
    electronBridgeAvailable = false;
    return { available: false };
  }
  try {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 2000);

    const response = await fetch(`${ELECTRON_BRIDGE_URL}/status`, {
      signal: controller.signal
    });
    clearTimeout(timeout);

    if (response.ok) {
      const data = await response.json();
      electronBridgeAvailable = data.ok === true;
      console.log('🌉 Electron FF1 bridge detected:', data);
      return { available: true, ...data };
    }
  } catch (e) {
    // Bridge not running (connection refused, timeout, etc.)
    electronBridgeAvailable = false;
  }

  return { available: false };
}

/**
 * Get the Electron bridge status
 */
export function getElectronBridgeStatus() {
  return {
    available: electronBridgeAvailable,
    checked: electronBridgeAvailable !== null,
    devices: electronBridgeDevices,
  };
}

/**
 * Discover FF1 devices via Electron bridge
 */
export async function discoverDevicesViaBridge() {
  if (!electronBridgeAvailable) {
    const status = await checkElectronBridge();
    if (!status.available) {
      return { ok: false, error: 'Electron bridge not available' };
    }
  }
  
  try {
    const response = await fetch(`${ELECTRON_BRIDGE_URL}/discover`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({})
    });
    
    const data = await response.json();
    if (data.ok && data.devices) {
      electronBridgeDevices = data.devices;
      console.log('🔍 FF1 devices discovered via bridge:', data.devices);
      
      // Auto-configure first device if none set
      if (data.devices.length > 0 && !config.deviceId) {
        const first = data.devices[0];
        saveConfig({
          deviceId: first.deviceId || first.info?.deviceId,
          deviceIp: first.ip,
          deviceInfo: first.info,
        });
      }
    }
    return data;
  } catch (e) {
    console.warn('⚠️ Failed to discover devices via bridge:', e);
    return { ok: false, error: e.message };
  }
}

/**
 * Send playlist via Electron bridge
 */
async function sendPlaylistViaBridge(playlist) {
  const payload = {
    deviceId: config.deviceId,
    deviceIp: config.deviceIp,
    command: 'displayPlaylist',
    request: {
      playlist: playlist,
      intent: { action: 'now_display' }
    }
  };
  
  console.log('🌉 Sending to FF1 via Electron bridge:', payload);
  
  const response = await fetch(`${ELECTRON_BRIDGE_URL}/cast`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload)
  });
  
  const result = await response.json();
  
  if (!result.ok) {
    throw new Error(result.error || 'Bridge cast failed');
  }
  
  // Update last connected timestamp
  saveConfig({ lastConnected: Date.now() });
  
  return result.response;
}

// ============================================
// DP-1 PLAYLIST BUILDER
// ============================================

/**
 * Build a DP-1 playlist item from a URL
 * @param {string} url - The URL to display
 * @param {number} duration - Duration in seconds (0 = infinite)
 * @param {object} options - Additional options
 */
function buildUrlItem(url, duration = 0, options = {}) {
  return {
    source: url,
    duration: duration,
    license: options.license || 'open',
    provenance: {
      type: 'offChainURI',
      uri: options.provenanceUri || url
    }
  };
}

/**
 * Build a DP-1 playlist
 * @param {Array} items - Array of playlist items
 */
function buildPlaylist(items) {
  return {
    dpVersion: '1.0.0',
    items: items
  };
}

// ============================================
// FF1 DEVICE API
// ============================================

/**
 * Get the device API URL (tries IP first, then mDNS)
 */
function getDeviceUrl() {
  if (!config.deviceId && !config.deviceIp) {
    throw new Error('FF1 device ID not configured');
  }
  
  // Prefer direct IP if available (more reliable than mDNS)
  if (config.deviceIp) {
    return `http://${config.deviceIp}:1111/api/cast`;
  }
  
  return `http://${config.deviceId}.local:1111/api/cast`;
}

/**
 * Get the device info URL
 */
function getDeviceInfoUrl() {
  if (config.deviceIp) {
    return `http://${config.deviceIp}:1111/api/info`;
  }
  return `http://${config.deviceId}.local:1111/api/info`;
}

/**
 * Get the relay API URL
 */
function getRelayUrl() {
  return 'https://artwork-info.feral-file.workers.dev/api/cast';
}

/**
 * Get the session-server proxy URL (for browser-based access)
 */
function getProxyUrl() {
  // Use session server as proxy to bypass CORS
  const isDev = window.location.hostname === 'localhost';
  return isDev 
    ? 'http://localhost:8889/ff1/cast'
    : 'https://aesthetic.computer/api/ff1/proxy';
}

/**
 * Send a playlist to FF1 using direct device API
 * @param {object} playlist - The DP-1 playlist to send
 */
async function sendPlaylistDirect(playlist) {
  const url = getDeviceUrl();
  
  const payload = {
    command: 'displayPlaylist',
    request: {
      playlist: playlist,
      intent: {
        action: 'now_display'
      }
    }
  };

  console.log('📡 Sending to FF1 device:', url, payload);

  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(payload)
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`FF1 device error: ${response.status} - ${errorText}`);
  }

  return await response.json();
}

/**
 * Send a playlist to FF1 via session-server proxy (bypasses CORS)
 * This is the recommended method for browser-based access
 * @param {object} playlist - The DP-1 playlist to send
 */
async function sendPlaylistViaProxy(playlist) {
  if (!config.relayTopicId) {
    throw new Error('FF1 Topic ID not configured - get this from your FF1 app');
  }

  const url = getProxyUrl();
  
  const payload = {
    topicID: config.relayTopicId,
    apiKey: config.relayApiKey || undefined,
    command: 'displayPlaylist',
    request: {
      dp1_call: playlist,
      intent: {
        action: 'now_display'
      }
    }
  };

  console.log('📡 Sending to FF1 via session proxy:', url, payload);

  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(payload)
  });

  const result = await response.json();
  
  if (!response.ok || !result.success) {
    throw new Error(result.error || `FF1 proxy error: ${response.status}`);
  }

  return result.response;
}

/**
 * Send a playlist to FF1 using relay API (direct - may have CORS issues)
 * @param {object} playlist - The DP-1 playlist to send
 */
async function sendPlaylistViaRelay(playlist) {
  if (!config.relayApiKey || !config.relayTopicId) {
    throw new Error('FF1 relay API key and topic ID not configured');
  }

  const url = getRelayUrl();
  
  const payload = {
    command: 'displayPlaylist',
    request: {
      playlist: playlist,
      intent: {
        action: 'now_display'
      }
    }
  };

  console.log('📡 Sending to FF1 via relay:', url, payload);

  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'API-KEY': config.relayApiKey,
      'topicID': config.relayTopicId
    },
    body: JSON.stringify(payload)
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`FF1 relay error: ${response.status} - ${errorText}`);
  }

  return await response.json();
}

/**
 * Send a playlist to FF1 (tries methods in priority order)
 * Priority: 1. Electron Bridge, 2. Session Proxy, 3. Direct API
 * @param {object} playlist - The DP-1 playlist to send
 */
export async function sendPlaylist(playlist) {
  // Strategy 1: Try Electron bridge first (best for local devices)
  if (config.preferElectronBridge !== false) {
    // Check bridge if not yet checked
    if (electronBridgeAvailable === null) {
      await checkElectronBridge();
    }
    
    if (electronBridgeAvailable) {
      try {
        console.log('🌉 Trying Electron bridge...');
        return await sendPlaylistViaBridge(playlist);
      } catch (e) {
        console.warn('⚠️ Electron bridge failed, trying fallback:', e.message);
      }
    }
  }
  
  // Strategy 2: Try direct device API if configured
  if (config.useDirectApi && config.deviceIp) {
    try {
      console.log('📡 Trying direct device API...');
      return await sendPlaylistDirect(playlist);
    } catch (e) {
      console.warn('⚠️ Direct API failed, trying proxy:', e.message);
    }
  }
  
  // Strategy 3: Use session-server proxy (cloud relay)
  console.log('☁️ Using session proxy...');
  return sendPlaylistViaProxy(playlist);
}

/**
 * Get the current connection method that will be used
 */
export function getConnectionMethod() {
  if (config.preferElectronBridge !== false && electronBridgeAvailable) {
    return 'electron-bridge';
  }
  if (config.useDirectApi && config.deviceIp) {
    return 'direct';
  }
  return 'proxy';
}

// ============================================
// HIGH-LEVEL API FOR KIDLISP
// ============================================

/**
 * Send a URL to display on FF1
 * @param {string} url - The URL to display
 * @param {number} duration - Duration in seconds (0 = infinite)
 */
export async function sendUrl(url, duration = 0) {
  const item = buildUrlItem(url, duration);
  const playlist = buildPlaylist([item]);
  return sendPlaylist(playlist);
}

/**
 * Send KidLisp code to FF1 by constructing a device.kidlisp.com URL
 * Uses the device subdomain for proper FF1 display settings (density=1, tv mode, etc.)
 * @param {string} codeId - The code ID from Aesthetic.Computer
 */
export async function sendKidLispCode(codeId) {
  if (!codeId) {
    throw new Error('No code ID provided - code must be uploaded first');
  }
  
  // Use the device.kidlisp.com subdomain with FF1-optimized display settings
  // This applies: density=1, tv=true, nogap=true, nolabel=true
  const deviceUrl = `https://device.kidlisp.com/${codeId}`;
  
  console.log('🎨 Sending KidLisp code to FF1:', deviceUrl);
  
  return sendUrl(deviceUrl, 999999999);
}

/**
 * Send a "clear" or black screen to FF1
 */
export async function clearDisplay() {
  // Send a minimal black screen URL or use a dedicated clear endpoint
  const url = 'https://aesthetic.computer/blank';
  return sendUrl(url, 0);
}

// ============================================
// CONNECTION TEST
// ============================================

/**
 * Test connection to FF1 device
 * Returns device info if successful
 */
export async function testConnection() {
  if (!config.deviceId && !config.deviceIp) {
    return { success: false, error: 'Device ID not configured' };
  }
  
  try {
    // Try to fetch device info/status
    const url = getDeviceInfoUrl();
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 5000);
    
    const response = await fetch(url, {
      method: 'GET',
      signal: controller.signal
    });
    
    clearTimeout(timeoutId);
    
    if (response.ok) {
      const info = await response.json();
      // Cache device info
      config.deviceInfo = info;
      config.lastConnected = Date.now();
      saveConfig(config);
      return { success: true, info };
    } else {
      return { success: false, error: `HTTP ${response.status}` };
    }
  } catch (e) {
    // If mDNS fails, try with .local TLD variations or suggest IP
    if (e.name === 'AbortError') {
      return { 
        success: false, 
        error: 'Connection timeout - device may be offline or mDNS not working. Try entering the device IP address.',
        suggestion: 'ip'
      };
    }
    return { success: false, error: e.message };
  }
}

/**
 * Get current device status (what's playing, etc.)
 */
export async function getDeviceStatus() {
  if (!isConfigured()) {
    return null;
  }
  
  try {
    const url = config.deviceIp 
      ? `http://${config.deviceIp}:1111/api/status`
      : `http://${config.deviceId}.local:1111/api/status`;
      
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 3000);
    
    const response = await fetch(url, {
      method: 'GET',
      signal: controller.signal
    });
    
    clearTimeout(timeoutId);
    
    if (response.ok) {
      return await response.json();
    }
  } catch (e) {
    console.warn('Failed to get device status:', e);
  }
  return null;
}

// ============================================
// STATE HELPERS
// ============================================

/**
 * Check if FF1 is properly configured
 * - Electron bridge: auto-discovers devices (no config needed)
 * - Cloud relay: requires topicId from FF1 app
 */
export function isConfigured() {
  // If Electron bridge is available with devices, we're good
  if (electronBridgeAvailable && (electronBridgeDevices.length > 0 || config.deviceId || config.deviceIp)) {
    return true;
  }
  // Otherwise, need topic ID for cloud relay
  return !!config.relayTopicId;
}

/**
 * Get comprehensive FF1 connection status for UI
 */
export function getConnectionStatus() {
  const method = getConnectionMethod();
  
  return {
    configured: isConfigured(),
    method,
    electronBridge: {
      available: electronBridgeAvailable,
      checked: electronBridgeAvailable !== null,
      deviceCount: electronBridgeDevices.length,
    },
    device: {
      id: config.deviceId,
      ip: config.deviceIp,
      info: config.deviceInfo,
      lastConnected: config.lastConnected,
    },
    cloudRelay: {
      configured: !!config.relayTopicId,
      hasApiKey: !!config.relayApiKey,
    },
    // User-friendly status message
    message: getStatusMessage(method),
  };
}

function getStatusMessage(method) {
  if (method === 'electron-bridge') {
    const count = electronBridgeDevices.length;
    if (count > 0) {
      return `Connected via AC app (${count} device${count > 1 ? 's' : ''} found)`;
    }
    return 'AC app connected - scanning for devices...';
  }
  if (method === 'direct') {
    return `Direct connection to ${config.deviceIp || config.deviceId}`;
  }
  if (config.relayTopicId) {
    return 'Using cloud relay';
  }
  return 'Not configured - install AC app or enter Topic ID';
}

/**
 * Get download URL for Aesthetic Computer app
 */
export function getAppDownloadUrl() {
  const platform = navigator.platform.toLowerCase();
  if (platform.includes('mac')) {
    return 'https://github.com/whistlegraph/aesthetic-computer/releases/latest/download/Aesthetic-Computer.dmg';
  }
  if (platform.includes('win')) {
    return 'https://github.com/whistlegraph/aesthetic-computer/releases/latest/download/Aesthetic-Computer-Setup.exe';
  }
  // Linux / other
  return 'https://github.com/whistlegraph/aesthetic-computer/releases/latest';
}

/**
 * Check if the current platform is FF1
 */
export function isFF1Platform() {
  return state.currentPlatform === 'ff1';
}

/**
 * Get a friendly status string for the device
 */
export function getStatusText() {
  if (!isConfigured()) {
    return 'Not configured';
  }
  if (config.lastConnected) {
    const ago = Date.now() - config.lastConnected;
    if (ago < 60000) return 'Connected';
    if (ago < 3600000) return `Connected ${Math.floor(ago/60000)}m ago`;
  }
  return config.deviceId || 'FF1 Ready';
}

// ============================================
// INITIALIZATION
// ============================================

// Load config on module init
loadConfig();

console.log('🖼️ FF1 module initialized', isConfigured() ? '(configured)' : '(not configured)');
