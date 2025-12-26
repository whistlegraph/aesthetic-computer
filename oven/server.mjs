#!/usr/bin/env node
// Oven Server
// Main Express server for the unified bake processing service

import 'dotenv/config';
import express from 'express';
import https from 'https';
import http from 'http';
import fs from 'fs';
import { execSync } from 'child_process';
import { WebSocketServer } from 'ws';
import { healthHandler, bakeHandler, statusHandler, bakeCompleteHandler, bakeStatusHandler, getActiveBakes, getIncomingBakes, getRecentBakes, subscribeToUpdates, cleanupStaleBakes } from './baker.mjs';
import { grabHandler, grabGetHandler, grabIPFSHandler, grabPiece, getCachedOrGenerate, getActiveGrabs, getRecentGrabs, getLatestKeepThumbnail, getLatestIPFSUpload, getAllLatestIPFSUploads, setNotifyCallback, IPFS_GATEWAY } from './grabber.mjs';

const app = express();
const PORT = process.env.PORT || 3002;
const dev = process.env.NODE_ENV === 'development';

// Get git version at startup (from env var set during deploy, or try git)
let GIT_VERSION = process.env.OVEN_VERSION || 'unknown';
if (GIT_VERSION === 'unknown') {
  try {
    GIT_VERSION = execSync('git rev-parse --short HEAD', { encoding: 'utf8' }).trim();
  } catch (e) {
    // Not a git repo, that's fine
  }
}
console.log(`📦 Oven version: ${GIT_VERSION}`);

// Parse JSON bodies
app.use(express.json());

// Unified dashboard HTML - everything is a "bake"
app.get('/', (req, res) => {
  res.setHeader('Content-Type', 'text/html');
  res.send(`<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>🔥 Oven</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    ::-webkit-scrollbar { display: none; }
    
    body {
      font-family: monospace;
      font-size: 14px;
      background: #000;
      color: #fff;
      min-height: 100vh;
    }
    
    /* Header */
    header {
      position: sticky;
      top: 0;
      background: #000;
      border-bottom: 2px solid yellow;
      padding: 0.8em 1em;
      display: flex;
      align-items: center;
      justify-content: space-between;
      z-index: 100;
    }
    
    .logo {
      font-size: 1.5em;
      color: yellow;
      text-decoration: none;
    }
    
    .status {
      display: flex;
      align-items: center;
      gap: 1em;
      font-size: 0.85em;
    }
    
    .status-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background: #666;
    }
    
    .status-dot.connected { background: #4f4; }
    .status-dot.disconnected { background: #f44; animation: pulse 1s infinite; }
    
    .version { opacity: 0.5; }
    
    /* Queue stats */
    .stats {
      display: flex;
      gap: 1.5em;
      padding: 0.8em 1em;
      background: #111;
      border-bottom: 1px solid #333;
      font-size: 0.85em;
    }
    
    .stat {
      display: flex;
      align-items: center;
      gap: 0.4em;
    }
    
    .stat-num {
      color: yellow;
      font-weight: bold;
    }
    
    /* Bake grid */
    .bakes {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
      gap: 1em;
      padding: 1em;
    }
    
    /* Bake card */
    .bake {
      background: #111;
      border: 1px solid #333;
      border-radius: 4px;
      overflow: hidden;
      transition: border-color 0.2s;
    }
    
    .bake:hover { border-color: #555; }
    .bake.active { border-color: yellow; }
    .bake.queued { border-color: #66f; }
    .bake.error { border-color: #f44; }
    
    /* Preview area */
    .bake-preview {
      aspect-ratio: 16/9;
      background: #000;
      display: flex;
      align-items: center;
      justify-content: center;
      position: relative;
      overflow: hidden;
    }
    
    .bake-preview img,
    .bake-preview video {
      width: 100%;
      height: 100%;
      object-fit: contain;
    }
    
    .bake-preview .loading {
      position: absolute;
      inset: 0;
      display: flex;
      align-items: center;
      justify-content: center;
      background: rgba(0,0,0,0.8);
    }
    
    .spinner {
      width: 24px;
      height: 24px;
      border: 2px solid #333;
      border-top-color: yellow;
      border-radius: 50%;
      animation: spin 1s linear infinite;
    }
    
    @keyframes spin { to { transform: rotate(360deg); } }
    @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
    
    /* Bake info */
    .bake-info {
      padding: 0.8em;
    }
    
    .bake-title {
      display: flex;
      align-items: center;
      gap: 0.5em;
      margin-bottom: 0.4em;
    }
    
    .bake-title a {
      color: #fff;
      text-decoration: none;
      font-weight: bold;
    }
    
    .bake-title a:hover { color: yellow; }
    
    .bake-type {
      font-size: 0.7em;
      padding: 0.2em 0.5em;
      background: #333;
      border-radius: 2px;
      text-transform: uppercase;
    }
    
    .bake-type.tape { background: #306; }
    .bake-type.icon { background: #063; }
    .bake-type.preview { background: #630; }
    .bake-type.grab { background: #360; }
    
    .bake-meta {
      font-size: 0.8em;
      opacity: 0.6;
      display: flex;
      gap: 1em;
    }
    
    .bake-status {
      margin-top: 0.5em;
      font-size: 0.75em;
      padding: 0.3em 0.6em;
      background: rgba(255,255,0,0.1);
      border: 1px solid yellow;
      display: inline-block;
    }
    
    .bake-status.queued {
      background: rgba(100,100,255,0.1);
      border-color: #66f;
      color: #66f;
    }
    
    .bake-status.error {
      background: rgba(255,0,0,0.1);
      border-color: #f44;
      color: #f44;
    }
    
    .bake-links {
      margin-top: 0.5em;
      font-size: 0.75em;
      display: flex;
      gap: 0.8em;
    }
    
    .bake-links a {
      color: #66f;
      text-decoration: none;
    }
    
    .bake-links a:hover { text-decoration: underline; }
    
    .empty {
      grid-column: 1 / -1;
      text-align: center;
      padding: 3em;
      opacity: 0.5;
    }
    .filters {
      display: flex;
      gap: 1em;
      padding: 0.5em 1em;
      background: #0a0a0a;
      border-bottom: 1px solid #222;
      font-size: 0.85em;
    }
    
    .filters label {
      display: flex;
      align-items: center;
      gap: 0.3em;
      cursor: pointer;
      opacity: 0.7;
    }
    
    .filters label:hover { opacity: 1; }
    
    .filters input[type="checkbox"] {
      accent-color: yellow;
    }
  </style>
</head>
<body>
  <header>
    <a href="/" class="logo">🔥 oven</a>
    <div class="status">
      <div class="status-dot" id="ws-dot"></div>
      <span id="ws-text">Connecting...</span>
      <span class="version" id="version"></span>
    </div>
  </header>
  
  <div class="stats">
    <div class="stat"><span class="stat-num" id="queue-count">0</span> queued</div>
    <div class="stat"><span class="stat-num" id="active-count">0</span> baking</div>
    <div class="stat"><span class="stat-num" id="complete-count">0</span> complete</div>
  </div>
  
  <div class="filters">
    <label><input type="checkbox" id="show-tape" checked onchange="applyFilters()"> tape</label>
    <label><input type="checkbox" id="show-grab" checked onchange="applyFilters()"> grab</label>
    <label><input type="checkbox" id="show-icon" checked onchange="applyFilters()"> icon</label>
    <label><input type="checkbox" id="show-preview" checked onchange="applyFilters()"> preview</label>
  </div>
  
  <div class="bakes" id="bakes"></div>
  
  <script>
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    let ws, reconnectAttempts = 0, serverVersion = null;
    let allBakes = []; // Store all bakes for filtering
    
    function connect() {
      ws = new WebSocket(protocol + '//' + location.host + '/ws');
      
      ws.onopen = () => {
        document.getElementById('ws-dot').className = 'status-dot connected';
        document.getElementById('ws-text').textContent = 'Connected';
        reconnectAttempts = 0;
      };
      
      ws.onclose = () => {
        document.getElementById('ws-dot').className = 'status-dot disconnected';
        document.getElementById('ws-text').textContent = 'Reconnecting...';
        const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
        reconnectAttempts++;
        setTimeout(connect, delay);
      };
      
      ws.onerror = () => ws.close();
      
      ws.onmessage = (event) => {
        const data = JSON.parse(event.data);
        if (data.version && serverVersion && data.version !== serverVersion) {
          location.reload();
          return;
        }
        serverVersion = data.version;
        if (data.version) document.getElementById('version').textContent = 'v' + data.version;
        render(data);
      };
    }
    connect();
    
    function render(data) {
      // Combine all bakes into unified list
      allBakes = []; // Reset global array
      
      // Incoming/queued bakes (tapes waiting)
      (data.incoming || []).forEach(b => allBakes.push({
        type: 'tape', status: 'queued', id: b.code,
        title: '!' + b.code, url: 'https://aesthetic.computer/!' + b.code,
        startTime: b.detectedAt, details: b.details || 'Waiting...'
      }));
      
      // Active bakes (tapes processing)
      (data.active || []).forEach(b => allBakes.push({
        type: 'tape', status: 'active', id: b.code || b.slug,
        title: '!' + (b.code || b.slug), url: 'https://aesthetic.computer/!' + (b.code || b.slug),
        startTime: b.startTime, details: b.details || b.status
      }));
      
      // Active grabs
      (data.grabs?.active || []).forEach(g => allBakes.push({
        type: 'grab', status: 'active', id: g.id,
        title: g.piece, url: 'https://aesthetic.computer/' + g.piece,
        startTime: g.startTime, details: g.format + ' capturing...'
      }));
      
      // Recent grabs (completed)
      const ipfsThumbs = data.grabs?.ipfsThumbs || {};
      (data.grabs?.recent || []).forEach(g => {
        const ipfsCid = g.ipfsCid || ipfsThumbs[g.piece]?.ipfsCid;
        // Use IPFS if available, otherwise generate preview via oven endpoint
        const previewUrl = ipfsCid 
          ? IPFS_GATEWAY + '/ipfs/' + ipfsCid 
          : 'https://oven.aesthetic.computer/icon/128x128/' + g.piece + '.png';
        allBakes.push({
          type: 'grab', status: g.status === 'failed' ? 'error' : 'complete', id: g.id,
          title: g.piece, url: 'https://aesthetic.computer/' + g.piece,
          preview: previewUrl,
          size: g.size, format: g.format, error: g.error,
          completedAt: g.completedAt,
          links: ipfsCid ? [{ label: 'IPFS', url: 'ipfs://' + ipfsCid }] : []
        });
      });
      
      // Recent bakes (tapes completed)
      (data.recent || []).forEach(b => {
        const code = b.code || b.slug || 'unknown';
        const links = [];
        if (b.atprotoRkey) {
          const handle = b.userHandle ? b.userHandle + '.at.aesthetic.computer' : 'art.at.aesthetic.computer';
          links.push({ label: '🦋 AT', url: 'https://pdsls.dev/at://' + handle + '/computer.aesthetic.tape/' + b.atprotoRkey });
        }
        allBakes.push({
          type: 'tape', status: b.error ? 'error' : 'complete', id: code,
          title: '!' + code, url: 'https://aesthetic.computer/!' + code,
          preview: b.thumbnailUrl, video: b.mp4Url, error: b.error,
          completedAt: b.completedAt, links
        });
      });
      
      // Update stats
      const queued = allBakes.filter(b => b.status === 'queued').length;
      const active = allBakes.filter(b => b.status === 'active').length;
      const complete = allBakes.filter(b => b.status === 'complete' || b.status === 'error').length;
      document.getElementById('queue-count').textContent = queued;
      document.getElementById('active-count').textContent = active;
      document.getElementById('complete-count').textContent = complete;
      
      // Apply filters and render
      applyFilters();
    }
    
    function applyFilters() {
      const showTypes = {
        tape: document.getElementById('show-tape').checked,
        grab: document.getElementById('show-grab').checked,
        icon: document.getElementById('show-icon').checked,
        preview: document.getElementById('show-preview').checked
      };
      
      const filtered = allBakes.filter(b => showTypes[b.type]);
      renderBakes(filtered);
    }
    
    function renderBakes(bakes) {
      const container = document.getElementById('bakes');
      if (bakes.length === 0) {
        container.innerHTML = '<div class="empty">No bakes match filters</div>';
        return;
      }
      
      container.innerHTML = bakes.map(bake => {
        let previewContent;
        if (bake.video) {
          previewContent = '<video autoplay loop muted playsinline src="' + bake.video + '"></video>';
        } else if (bake.preview) {
          previewContent = '<img src="' + bake.preview + '" loading="lazy" onerror="this.parentElement.innerHTML=\\'<span style=color:#f44>Load failed</span>\\'">';
        } else if (bake.status === 'active' || bake.status === 'queued') {
          previewContent = '<div class="loading"><div class="spinner"></div></div>';
        } else {
          const info = bake.type === 'tape' ? 'Thumbnail not available' :
                       bake.type === 'grab' ? (bake.format || 'grab') + ' · ' + (bake.size ? (bake.size/1024).toFixed(1) + 'KB' : 'no IPFS') :
                       'Preview pending';
          previewContent = '<div style="color:#666;font-size:0.8em;text-align:center;padding:1em;">' + info + '</div>';
        }
        
        let html = '<div class="bake ' + bake.status + '">';
        html += '<div class="bake-preview">' + previewContent + '</div>';
        html += '<div class="bake-info">';
        html += '<div class="bake-title">';
        html += '<a href="' + bake.url + '" target="_blank">' + bake.title + '</a>';
        html += '<span class="bake-type ' + bake.type + '">' + bake.type + '</span>';
        html += '</div>';
        html += '<div class="bake-meta">';
        if (bake.format) html += '<span>' + bake.format + '</span>';
        if (bake.size) html += '<span>' + (bake.size/1024).toFixed(1) + ' KB</span>';
        if (bake.startTime) html += '<span>' + formatDuration(Date.now() - bake.startTime) + '</span>';
        if (bake.completedAt) html += '<span>' + formatAge(bake.completedAt) + ' ago</span>';
        html += '</div>';
        if (bake.status === 'active' || bake.status === 'queued') {
          html += '<div class="bake-status ' + bake.status + '">' + (bake.details || bake.status) + '</div>';
        }
        if (bake.error) html += '<div class="bake-status error">' + bake.error + '</div>';
        if (bake.links && bake.links.length) {
          html += '<div class="bake-links">' + bake.links.map(l => '<a href="' + l.url + '" target="_blank">' + l.label + '</a>').join('') + '</div>';
        }
        html += '</div></div>';
        return html;
      }).join('');
    }
    
    const IPFS_GATEWAY = 'https://ipfs.aesthetic.computer';
    
    function formatDuration(ms) {
      const s = Math.floor(ms / 1000);
      if (s < 60) return s + 's';
      const m = Math.floor(s / 60);
      return m + 'm ' + (s % 60) + 's';
    }
    
    function formatAge(dateStr) {
      const ms = Date.now() - new Date(dateStr).getTime();
      const s = Math.floor(ms / 1000);
      if (s < 60) return s + 's';
      const m = Math.floor(s / 60);
      if (m < 60) return m + 'm';
      const h = Math.floor(m / 60);
      if (h < 24) return h + 'h';
      const d = Math.floor(h / 24);
      return d + 'd';
    }
    
    fetch('/status').then(r => r.json()).then(render);
  </script>
</body>
</html>
`);
});

// API endpoints
app.get('/health', healthHandler);

// Override status to include grabs
app.get('/status', async (req, res) => {
  await cleanupStaleBakes();
  res.json({
    version: GIT_VERSION,
    incoming: Array.from(getIncomingBakes().values()),
    active: Array.from(getActiveBakes().values()),
    recent: getRecentBakes(),
    grabs: {
      active: getActiveGrabs(),
      recent: getRecentGrabs(),
      ipfsThumbs: getAllLatestIPFSUploads()
    }
  });
});

app.post('/bake', bakeHandler);
app.post('/bake-complete', bakeCompleteHandler);
app.post('/bake-status', bakeStatusHandler);

// Icon endpoint - small square thumbnails (compatible with grab.aesthetic.computer)
// GET /icon/{width}x{height}/{piece}.png
// Uses 24h Spaces cache to avoid regenerating on every request
app.get('/icon/:size/:piece.png', async (req, res) => {
  const { size, piece } = req.params;
  const [width, height] = size.split('x').map(n => parseInt(n) || 128);
  const w = Math.min(width, 512);
  const h = Math.min(height, 512);
  
  try {
    const { cdnUrl, fromCache, buffer } = await getCachedOrGenerate('icons', piece, w, h, async () => {
      const result = await grabPiece(piece, {
        format: 'png',
        width: w,
        height: h,
        density: 1,
      });
      if (!result.success) throw new Error(result.error);
      return result.buffer;
    });
    
    if (fromCache && cdnUrl) {
      res.setHeader('X-Cache', 'HIT');
      res.setHeader('Cache-Control', 'public, max-age=86400');
      return res.redirect(302, cdnUrl);
    }
    
    res.setHeader('Content-Type', 'image/png');
    res.setHeader('Content-Length', buffer.length);
    res.setHeader('Cache-Control', 'public, max-age=3600');
    res.setHeader('X-Cache', 'MISS');
    res.send(buffer);
  } catch (error) {
    console.error('Icon handler error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Preview endpoint - larger social media images (compatible with grab.aesthetic.computer)
// GET /preview/{width}x{height}/{piece}.png
// Uses 24h Spaces cache to avoid regenerating on every request
app.get('/preview/:size/:piece.png', async (req, res) => {
  const { size, piece } = req.params;
  const [width, height] = size.split('x').map(n => parseInt(n) || 1200);
  const w = Math.min(width, 1920);
  const h = Math.min(height, 1080);
  
  try {
    const { cdnUrl, fromCache, buffer } = await getCachedOrGenerate('previews', piece, w, h, async () => {
      const result = await grabPiece(piece, {
        format: 'png',
        width: w,
        height: h,
        density: 1,
      });
      if (!result.success) throw new Error(result.error);
      return result.buffer;
    });
    
    if (fromCache && cdnUrl) {
      res.setHeader('X-Cache', 'HIT');
      res.setHeader('Cache-Control', 'public, max-age=86400');
      return res.redirect(302, cdnUrl);
    }
    
    res.setHeader('Content-Type', 'image/png');
    res.setHeader('Content-Length', buffer.length);
    res.setHeader('Cache-Control', 'public, max-age=3600');
    res.setHeader('X-Cache', 'MISS');
    res.send(buffer);
  } catch (error) {
    console.error('Preview handler error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Grab endpoint - capture screenshots/GIFs from KidLisp pieces
app.post('/grab', grabHandler);
app.get('/grab/:format/:width/:height/:piece', grabGetHandler);
app.post('/grab-ipfs', grabIPFSHandler);

// Grab status endpoint
app.get('/grab-status', (req, res) => {
  res.json({
    active: getActiveGrabs(),
    recent: getRecentGrabs()
  });
});

// Live collection thumbnail endpoint - redirects to most recent kept WebP
// Use this as the collection imageUri for a dynamic thumbnail
app.get('/keeps/latest', (req, res) => {
  const latest = getLatestKeepThumbnail();
  if (!latest) {
    return res.status(404).json({ 
      error: 'No keeps have been captured yet',
      hint: 'Mint a keep with --thumbnail flag to populate this endpoint'
    });
  }
  
  // Redirect to IPFS gateway
  const gatewayUrl = `${IPFS_GATEWAY}/ipfs/${latest.ipfsCid}`;
  res.redirect(302, gatewayUrl);
});

// Get latest thumbnail for a specific piece
app.get('/keeps/latest/:piece', (req, res) => {
  const latest = getLatestIPFSUpload(req.params.piece);
  if (!latest) {
    return res.status(404).json({ 
      error: `No keeps captured for piece: ${req.params.piece}`,
      hint: `Mint ${req.params.piece} with --thumbnail flag to populate this endpoint`
    });
  }
  
  const gatewayUrl = `${IPFS_GATEWAY}/ipfs/${latest.ipfsCid}`;
  res.redirect(302, gatewayUrl);
});

// Get all latest thumbnails as JSON (for debugging/monitoring)
app.get('/keeps/all', (req, res) => {
  res.json({
    latest: getLatestKeepThumbnail(),
    byPiece: getAllLatestIPFSUploads()
  });
});

// 404 handler
app.use((req, res) => {
  res.status(404).json({ error: 'Not found' });
});

// Error handler
app.use((err, req, res, next) => {
  console.error('❌ Server error:', err);
  res.status(500).json({ error: 'Internal server error', message: err.message });
});

// Create server and WebSocket
let server;
if (dev) {
  // Load local SSL certs in development mode
  const httpsOptions = {
    key: fs.readFileSync('../ssl-dev/localhost-key.pem'),
    cert: fs.readFileSync('../ssl-dev/localhost.pem'),
  };
  
  server = https.createServer(httpsOptions, app);
  server.listen(PORT, () => {
    console.log(`🔥 Oven server running on https://localhost:${PORT} (dev mode)`);
  });
} else {
  // Production - plain HTTP (Caddy handles SSL)
  server = http.createServer(app);
  server.listen(PORT, () => {
    console.log(`🔥 Oven server running on http://localhost:${PORT}`);
  });
}

// WebSocket server
const wss = new WebSocketServer({ server, path: '/ws' });

// Wire up grabber notifications to broadcast to all WebSocket clients
setNotifyCallback(() => {
  wss.clients.forEach((client) => {
    if (client.readyState === 1) { // OPEN
      client.send(JSON.stringify({
        version: GIT_VERSION,
        incoming: Array.from(getIncomingBakes().values()),
        active: Array.from(getActiveBakes().values()),
        recent: getRecentBakes(),
        grabs: {
          active: getActiveGrabs(),
          recent: getRecentGrabs(),
          ipfsThumbs: getAllLatestIPFSUploads()
        }
      }));
    }
  });
});

wss.on('connection', async (ws) => {
  console.log('📡 WebSocket client connected');
  
  // Clean up stale bakes before sending initial state
  await cleanupStaleBakes();
  
  // Send initial state
  ws.send(JSON.stringify({
    version: GIT_VERSION,
    incoming: Array.from(getIncomingBakes().values()),
    active: Array.from(getActiveBakes().values()),
    recent: getRecentBakes(),
    grabs: {
      active: getActiveGrabs(),
      recent: getRecentGrabs(),
      ipfsThumbs: getAllLatestIPFSUploads()
    }
  }));
  
  // Subscribe to updates
  const unsubscribe = subscribeToUpdates((update) => {
    if (ws.readyState === 1) { // OPEN
      ws.send(JSON.stringify({
        version: GIT_VERSION,
        incoming: Array.from(getIncomingBakes().values()),
        active: Array.from(getActiveBakes().values()),
        recent: getRecentBakes(),
        grabs: {
          active: getActiveGrabs(),
          recent: getRecentGrabs(),
          ipfsThumbs: getAllLatestIPFSUploads()
        }
      }));
    }
  });
  
  ws.on('close', () => {
    console.log('📡 WebSocket client disconnected');
    unsubscribe();
  });
});


