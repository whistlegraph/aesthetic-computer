/**
 * KidLisp Editor - FF1 Art Computer Integration
 * 
 * Sends artwork URLs to Feral File FF1 devices using the DP-1 protocol.
 * 
 * API Reference: https://docs.feralfile.com/ff1/send-playlists
 * 
 * Device Discovery Notes:
 * - The FF1 iOS app uses Bluetooth Low Energy (BLE) for device discovery
 * - Web browsers cannot use BLE scanning, so we use:
 *   1. Direct mDNS: http://{deviceId}.local:1111/api/cast (same WiFi network)
 *   2. Cloud Relay: https://artwork-info.feral-file.workers.dev/api/cast (with API key)
 */

import { state, setState, events } from './state.js';

// ============================================
// FF1 CONFIGURATION
// ============================================

const FF1_CONFIG_KEY = 'kidlisp-ff1-config';

// Default configuration
const defaultConfig = {
  deviceId: '',           // e.g., 'FF1-DVVEKLZA'
  deviceIp: '',           // Direct IP fallback (if mDNS doesn't work)
  useDirectApi: true,     // true = direct device API, false = relay
  relayApiKey: '',        // For relay mode
  relayTopicId: '',       // For relay mode
  lastConnected: null,    // Timestamp of last successful connection
  deviceInfo: null,       // Cached device info
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
 * Send a playlist to FF1 using relay API
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
 * Send a playlist to FF1 (auto-selects direct vs relay)
 * @param {object} playlist - The DP-1 playlist to send
 */
export async function sendPlaylist(playlist) {
  if (config.useDirectApi) {
    return sendPlaylistDirect(playlist);
  } else {
    return sendPlaylistViaRelay(playlist);
  }
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
 */
export function isConfigured() {
  const hasDevice = !!(config.deviceId || config.deviceIp);
  const hasRelay = !!(config.relayApiKey && config.relayTopicId);
  return hasDevice || hasRelay;
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
  return config.deviceId || config.deviceIp || 'Configured';
}

// ============================================
// INITIALIZATION
// ============================================

// Load config on module init
loadConfig();

console.log('🖼️ FF1 module initialized', isConfigured() ? '(configured)' : '(not configured)');
