#!/usr/bin/env node
/**
 * FF1 Local Proxy Server
 * 
 * This runs a simple local HTTP proxy that allows the kidlisp.com editor
 * to communicate with your FF1 device without CORS issues.
 * 
 * Usage:
 *   node ff1-proxy.mjs [deviceHost]
 * 
 * Examples:
 *   node ff1-proxy.mjs                          # Uses default FF1-DVVEKLZA.local:1111
 *   node ff1-proxy.mjs 192.168.1.100            # Use specific IP (port defaults to 1111)
 *   node ff1-proxy.mjs 192.168.1.100:1111       # Use specific IP and port
 *   node ff1-proxy.mjs FF1-MYDEVICE.local       # Use specific mDNS hostname
 * 
 * Find your FF1's IP address:
 *   - Check your router's connected devices list
 *   - On the FF1 device settings screen
 *   - Use: arp -a | grep -i ff1  (on macOS/Linux on same network)
 */

import http from 'http';

let deviceInput = process.argv[2] || 'localhost';  // Via SSH tunnel to FF1-DVVEKLZA (192.168.1.162)
// Add port if not specified
const DEFAULT_DEVICE_HOST = deviceInput.includes(':') ? deviceInput : `${deviceInput}:1111`;
const PROXY_PORT = 3333;

const server = http.createServer(async (req, res) => {
  // CORS headers for browser access
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type, API-KEY, topicID');

  // Handle preflight
  if (req.method === 'OPTIONS') {
    res.writeHead(204);
    res.end();
    return;
  }

  // Health check
  if (req.url === '/health') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ status: 'ok', device: DEFAULT_DEVICE_HOST }));
    return;
  }

  // Proxy /api/cast to device
  if (req.url.startsWith('/api/cast')) {
    let body = '';
    req.on('data', chunk => { body += chunk; });
    req.on('end', () => {
      const url = new URL(`http://${DEFAULT_DEVICE_HOST}${req.url}`);
      
      const options = {
        hostname: url.hostname,
        port: url.port || 1111,
        path: url.pathname + url.search,
        method: req.method,
        headers: {
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(body),
        },
        timeout: 10000,
      };

      console.log(`[Proxy] ${req.method} -> http://${DEFAULT_DEVICE_HOST}${req.url}`);

      const proxyReq = http.request(options, (proxyRes) => {
        let responseBody = '';
        proxyRes.on('data', chunk => { responseBody += chunk; });
        proxyRes.on('end', () => {
          console.log(`[Proxy] <- ${proxyRes.statusCode} ${responseBody.substring(0, 100)}`);
          res.writeHead(proxyRes.statusCode, {
            'Content-Type': proxyRes.headers['content-type'] || 'application/json',
          });
          res.end(responseBody);
        });
      });

      proxyReq.on('error', (err) => {
        console.error(`[Proxy] Error: ${err.message}`);
        res.writeHead(502, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ 
          error: 'Failed to connect to FF1 device',
          details: err.message,
          device: DEFAULT_DEVICE_HOST 
        }));
      });

      proxyReq.on('timeout', () => {
        proxyReq.destroy();
        res.writeHead(504, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Request to FF1 device timed out' }));
      });

      proxyReq.write(body);
      proxyReq.end();
    });
    return;
  }

  // Unknown endpoint
  res.writeHead(404, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({ error: 'Not found', endpoints: ['/health', '/api/cast'] }));
});

server.listen(PROXY_PORT, () => {
  console.log(`
╔════════════════════════════════════════════════════════════╗
║           FF1 Local Proxy Server Running                   ║
╠════════════════════════════════════════════════════════════╣
║                                                            ║
║  Proxy URL:    http://localhost:${PROXY_PORT}                       ║
║  Device:       ${DEFAULT_DEVICE_HOST.padEnd(40)}  ║
║                                                            ║
║  Endpoints:                                                ║
║    GET  /health     - Check proxy status                   ║
║    POST /api/cast   - Send playlist to FF1                 ║
║                                                            ║
╚════════════════════════════════════════════════════════════╝

In kidlisp.com settings, set:
  Device URL: http://localhost:${PROXY_PORT}
  Use Direct API: ✓

Press Ctrl+C to stop.
`);
});
