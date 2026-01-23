/**
 * FF1 Bridge Server for Aesthetic Computer Electron App
 * 
 * Provides a local HTTP server that allows kidlisp.com (running in a browser)
 * to communicate with FF1 devices on the local network.
 * 
 * The browser can't directly access local network devices due to CORS/security,
 * but this Electron-based bridge can relay requests to FF1 devices.
 * 
 * Endpoints:
 *   GET  /status          - Check if bridge is running, get version
 *   GET  /devices         - List discovered FF1 devices
 *   POST /cast            - Send a playlist to an FF1 device
 *   POST /discover        - Trigger device discovery
 */

const http = require('http');
const https = require('https');
const dns = require('dns');
const dgram = require('dgram');
const { networkInterfaces } = require('os');

const BRIDGE_PORT = 19999;
const FF1_API_PORT = 1111;
const MDNS_PORT = 5353;
const MDNS_ADDRESS = '224.0.0.251';

// Discovered FF1 devices: { deviceId: { ip, port, lastSeen, info } }
const discoveredDevices = new Map();

// CORS headers for browser access
function corsHeaders() {
  return {
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type',
    'Content-Type': 'application/json',
  };
}

// Send JSON response
function jsonResponse(res, statusCode, data) {
  res.writeHead(statusCode, corsHeaders());
  res.end(JSON.stringify(data));
}

// Parse JSON body from request
function parseBody(req) {
  return new Promise((resolve, reject) => {
    let body = '';
    req.on('data', chunk => body += chunk);
    req.on('end', () => {
      try {
        resolve(body ? JSON.parse(body) : {});
      } catch (e) {
        reject(new Error('Invalid JSON'));
      }
    });
    req.on('error', reject);
  });
}

// Make HTTP request to FF1 device
function fetchFF1(ip, port, path, payload) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: ip,
      port: port || FF1_API_PORT,
      path: path,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      timeout: 5000,
    };

    const req = http.request(options, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          resolve({ status: res.statusCode, data: JSON.parse(data) });
        } catch {
          resolve({ status: res.statusCode, data: { raw: data } });
        }
      });
    });

    req.on('error', reject);
    req.on('timeout', () => {
      req.destroy();
      reject(new Error('Request timeout'));
    });

    req.write(JSON.stringify(payload));
    req.end();
  });
}

// Try to resolve FF1 device via mDNS name
function resolveFF1ByName(deviceId) {
  return new Promise((resolve) => {
    const hostname = `${deviceId}.local`;
    dns.lookup(hostname, { family: 4 }, (err, address) => {
      if (err) {
        resolve(null);
      } else {
        resolve(address);
      }
    });
  });
}

// Scan local network for FF1 devices
async function scanForDevices() {
  console.log('[ff1-bridge] Scanning for FF1 devices...');
  
  // Get local network interfaces
  const interfaces = networkInterfaces();
  const localSubnets = [];
  
  for (const name of Object.keys(interfaces)) {
    for (const iface of interfaces[name]) {
      if (iface.family === 'IPv4' && !iface.internal) {
        // Extract subnet (e.g., 192.168.1.x -> 192.168.1)
        const parts = iface.address.split('.');
        if (parts.length === 4) {
          localSubnets.push(parts.slice(0, 3).join('.'));
        }
      }
    }
  }
  
  // Scan common IPs for FF1 devices (check port 1111)
  const foundDevices = [];
  const scanPromises = [];
  
  for (const subnet of localSubnets) {
    // Scan .1 to .254
    for (let i = 1; i <= 254; i++) {
      const ip = `${subnet}.${i}`;
      scanPromises.push(
        fetchFF1(ip, FF1_API_PORT, '/api/info', {})
          .then(result => {
            if (result.status === 200 && result.data) {
              console.log(`[ff1-bridge] Found FF1 at ${ip}:`, result.data);
              foundDevices.push({ ip, info: result.data });
            }
          })
          .catch(() => {}) // Ignore connection errors
      );
    }
  }
  
  // Wait for all scans with a timeout
  await Promise.race([
    Promise.all(scanPromises),
    new Promise(resolve => setTimeout(resolve, 10000)) // 10s timeout
  ]);
  
  // Update discovered devices
  for (const device of foundDevices) {
    const deviceId = device.info?.deviceId || device.info?.device_id || `FF1-${device.ip}`;
    discoveredDevices.set(deviceId, {
      ip: device.ip,
      port: FF1_API_PORT,
      lastSeen: Date.now(),
      info: device.info,
    });
  }
  
  console.log(`[ff1-bridge] Scan complete. Found ${foundDevices.length} device(s)`);
  return foundDevices;
}

// Quick probe for a specific FF1 device
async function probeDevice(deviceIdOrIp) {
  // Check if it's already an IP
  const ipRegex = /^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$/;
  let ip = ipRegex.test(deviceIdOrIp) ? deviceIdOrIp : null;
  
  // Try mDNS resolution if it looks like a device ID
  if (!ip && deviceIdOrIp.startsWith('FF1-')) {
    ip = await resolveFF1ByName(deviceIdOrIp);
  }
  
  // Check cached devices
  if (!ip) {
    const cached = discoveredDevices.get(deviceIdOrIp);
    if (cached) {
      ip = cached.ip;
    }
  }
  
  if (!ip) {
    return null;
  }
  
  try {
    const result = await fetchFF1(ip, FF1_API_PORT, '/api/info', {});
    if (result.status === 200) {
      const deviceId = result.data?.deviceId || result.data?.device_id || deviceIdOrIp;
      discoveredDevices.set(deviceId, {
        ip,
        port: FF1_API_PORT,
        lastSeen: Date.now(),
        info: result.data,
      });
      return { ip, info: result.data };
    }
  } catch (e) {
    console.log(`[ff1-bridge] Probe failed for ${deviceIdOrIp}:`, e.message);
  }
  
  return null;
}

// Handle incoming HTTP requests
async function handleRequest(req, res) {
  const url = new URL(req.url, `http://localhost:${BRIDGE_PORT}`);
  const path = url.pathname;
  
  // CORS preflight
  if (req.method === 'OPTIONS') {
    res.writeHead(204, corsHeaders());
    res.end();
    return;
  }
  
  try {
    // GET /status - Check if bridge is running
    if (req.method === 'GET' && path === '/status') {
      jsonResponse(res, 200, {
        ok: true,
        service: 'ff1-bridge',
        version: '1.0.0',
        deviceCount: discoveredDevices.size,
      });
      return;
    }
    
    // GET /devices - List discovered devices
    if (req.method === 'GET' && path === '/devices') {
      const devices = [];
      for (const [id, data] of discoveredDevices) {
        devices.push({
          deviceId: id,
          ip: data.ip,
          port: data.port,
          lastSeen: data.lastSeen,
          info: data.info,
        });
      }
      jsonResponse(res, 200, { ok: true, devices });
      return;
    }
    
    // POST /discover - Trigger device discovery
    if (req.method === 'POST' && path === '/discover') {
      const body = await parseBody(req);
      
      // If a specific device is provided, just probe it
      if (body.deviceId || body.deviceIp) {
        const device = await probeDevice(body.deviceId || body.deviceIp);
        if (device) {
          jsonResponse(res, 200, { ok: true, found: 1, devices: [device] });
        } else {
          jsonResponse(res, 200, { ok: true, found: 0, devices: [] });
        }
        return;
      }
      
      // Full network scan
      const devices = await scanForDevices();
      jsonResponse(res, 200, { ok: true, found: devices.length, devices });
      return;
    }
    
    // POST /cast - Send playlist to FF1
    if (req.method === 'POST' && path === '/cast') {
      const body = await parseBody(req);
      const { deviceId, deviceIp, command, request } = body;
      
      // Find the device
      let ip = deviceIp;
      if (!ip && deviceId) {
        const cached = discoveredDevices.get(deviceId);
        if (cached) {
          ip = cached.ip;
        } else {
          // Try mDNS resolution
          ip = await resolveFF1ByName(deviceId);
        }
      }
      
      // If still no IP, try the first discovered device
      if (!ip && discoveredDevices.size > 0) {
        const firstDevice = discoveredDevices.values().next().value;
        ip = firstDevice.ip;
      }
      
      if (!ip) {
        jsonResponse(res, 400, {
          ok: false,
          error: 'No FF1 device found. Run /discover first or provide deviceId/deviceIp.',
        });
        return;
      }
      
      // Build payload
      const payload = {
        command: command || 'displayPlaylist',
        request: request || {},
      };
      
      console.log(`[ff1-bridge] Casting to ${ip}:${FF1_API_PORT}`, payload);
      
      const result = await fetchFF1(ip, FF1_API_PORT, '/api/cast', payload);
      
      if (result.status === 200) {
        jsonResponse(res, 200, { ok: true, response: result.data });
      } else {
        jsonResponse(res, result.status, {
          ok: false,
          error: `FF1 returned ${result.status}`,
          details: result.data,
        });
      }
      return;
    }
    
    // 404 for unknown routes
    jsonResponse(res, 404, { ok: false, error: 'Not found' });
    
  } catch (e) {
    console.error('[ff1-bridge] Error handling request:', e);
    jsonResponse(res, 500, { ok: false, error: e.message });
  }
}

// Create and start the bridge server
let server = null;

function startBridge() {
  if (server) {
    console.log('[ff1-bridge] Bridge already running');
    return;
  }
  
  server = http.createServer(handleRequest);
  
  server.listen(BRIDGE_PORT, '127.0.0.1', () => {
    console.log(`[ff1-bridge] 🌉 FF1 Bridge listening on http://127.0.0.1:${BRIDGE_PORT}`);
  });
  
  server.on('error', (e) => {
    if (e.code === 'EADDRINUSE') {
      console.warn(`[ff1-bridge] Port ${BRIDGE_PORT} already in use, bridge disabled`);
    } else {
      console.error('[ff1-bridge] Server error:', e);
    }
    server = null;
  });
  
  // Do an initial device scan after startup
  setTimeout(() => {
    console.log('[ff1-bridge] Running initial device discovery...');
    scanForDevices();
  }, 5000);
}

function stopBridge() {
  if (server) {
    server.close();
    server = null;
    console.log('[ff1-bridge] Bridge stopped');
  }
}

function isRunning() {
  return server !== null;
}

module.exports = {
  startBridge,
  stopBridge,
  isRunning,
  scanForDevices,
  probeDevice,
  getDevices: () => Array.from(discoveredDevices.entries()).map(([id, data]) => ({
    deviceId: id, ...data
  })),
};
