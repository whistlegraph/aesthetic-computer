#!/usr/bin/env node
// Oven Server
// Main Express server for the tape processing service

import 'dotenv/config';
import express from 'express';
import https from 'https';
import http from 'http';
import fs from 'fs';
import { WebSocketServer } from 'ws';
import { healthHandler, bakeHandler, statusHandler, bakeCompleteHandler, bakeStatusHandler, getActiveBakes, getIncomingBakes, getRecentBakes, subscribeToUpdates, cleanupStaleBakes } from './baker.mjs';

const app = express();
const PORT = process.env.PORT || 3002;
const dev = process.env.NODE_ENV === 'development';

// Parse JSON bodies
app.use(express.json());

// Status dashboard HTML
app.get('/', (req, res) => {
  res.setHeader('Content-Type', 'text/html');
  res.send(`<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>🔥 Oven Status</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }
    
    ::-webkit-scrollbar {
      display: none;
    }
    
    body {
      font-size: 14px;
      font-family: monospace;
      -webkit-text-size-adjust: none;
      background: #000;
      color: #fff;
      line-height: 1.4;
    }
    
    /* Corner word */
    .corner-word {
      position: fixed;
      top: 0.5em;
      left: 0.5em;
      font-size: 2em;
      color: yellow;
      text-decoration: none;
      z-index: 1000;
      font-weight: normal;
    }
    
    .corner-word:hover {
      opacity: 0.7;
    }
    
    /* Main content */
    .container {
      max-width: 1400px;
      margin: 0 auto;
      padding: 3em 1em 1em;
    }
    
    header {
      text-align: center;
      padding: 1em 0;
      border-bottom: 2px solid yellow;
      margin-bottom: 1em;
    }
    
    h1 {
      font-size: 1.5em;
      font-weight: normal;
      margin: 0 0 0.3em 0;
      color: yellow;
    }
    
    .subtitle {
      font-size: 0.85em;
      opacity: 0.7;
      margin: 0.3em 0;
      color: lime;
    }
    
    h2 {
      font-size: 1.2em;
      font-weight: normal;
      margin: 1.5em 0 0.5em 0;
      color: yellow;
    }
    
    .status {
      font-size: 0.85em;
      opacity: 0.7;
      margin-bottom: 1em;
      color: lime;
    }
    
    .bake-card {
      background: #111;
      border: 1px solid #333;
      padding: 1em;
      margin-bottom: 1em;
    }
    
    .bake-card.active {
      border-color: yellow;
      background: rgba(255, 255, 0, 0.05);
    }
    
    .bake-card a {
      color: yellow;
      text-decoration: none;
    }
    
    .bake-card a:hover {
      text-decoration: underline;
    }
    
    .slug {
      font-size: 1em;
      margin-bottom: 0.5em;
      color: yellow;
    }
    
    .meta {
      font-size: 0.85em;
      opacity: 0.6;
      margin-top: 0.5em;
    }
    
    .media-preview {
      margin-top: 1em;
    }
    
    .media-preview video {
      width: 100%;
      max-width: 512px;
      image-rendering: pixelated;
      image-rendering: crisp-edges;
      border: 1px solid #333;
    }
    
    .status-badge {
      display: inline-block;
      padding: 0.3em 0.6em;
      font-size: 0.75em;
      background: rgba(255, 255, 0, 0.1);
      border: 1px solid yellow;
      margin-top: 0.5em;
      color: yellow;
    }
    
    .status-incoming {
      background: rgba(100, 100, 255, 0.1);
      border-color: #66f;
      color: #66f;
      animation: pulse 2s ease-in-out infinite;
    }
    
    .spinner {
      display: inline-block;
      width: 0.8em;
      height: 0.8em;
      border: 2px solid #333;
      border-top-color: yellow;
      border-radius: 50%;
      animation: spin 1s linear infinite;
      margin-right: 0.5em;
      vertical-align: middle;
    }
    
    @keyframes spin {
      to { transform: rotate(360deg); }
    }
    
    .progress {
      margin-top: 0.5em;
      height: 3px;
      background: #333;
      overflow: hidden;
    }
    
    .progress-bar {
      height: 100%;
      background: yellow;
      animation: pulse 1.5s ease-in-out infinite;
    }
    
    @keyframes pulse {
      0%, 100% { opacity: 1; }
      50% { opacity: 0.5; }
    }
    
    .empty {
      opacity: 0.5;
      font-style: italic;
    }
  </style>
</head>
<body>
  <a href="/" class="corner-word">oven</a>
  
  <div class="container">
    <header>
      <h1>Oven Status</h1>
      <p class="subtitle" id="ws-status">⚪ Connecting...</p>
    </header>
  
  <h2>� Incoming (<span id="incoming-count">0</span>)</h2>
  <div id="incoming-bakes"></div>
  
  <h2>�🔨 Currently Baking (<span id="active-count">0</span>)</h2>
  <div id="active-bakes"></div>
  
  <h2>✅ Recently Completed (<span id="recent-count">0</span>)</h2>
  <div id="recent-bakes"></div>
  </div>

  <script>
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const ws = new WebSocket(\`\${protocol}//\${window.location.host}/ws\`);
    
    ws.onopen = () => {
      document.getElementById('ws-status').innerHTML = '🟢 Connected';
    };
    
    ws.onclose = () => {
      document.getElementById('ws-status').innerHTML = '🔴 Disconnected';
    };
    
    ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      updateDashboard(data);
    };
    
    function updateDashboard(data) {
      // Update incoming bakes
      const incomingBakes = data.incoming || [];
      document.getElementById('incoming-count').textContent = incomingBakes.length;
      const incomingHtml = incomingBakes.length === 0 
        ? '<p class="empty">No incoming tapes</p>'
        : incomingBakes.map(bake => \`
          <div class="bake-card active">
            <div class="slug">!<a href="https://aesthetic.computer/!\${bake.code}" target="_blank">\${bake.code}</a></div>
            <div class="status-badge status-incoming"><span class="spinner"></span>\${bake.details || 'Waiting...'}</div>
            <div class="meta">Detected \${formatDuration(Date.now() - bake.detectedAt)} ago</div>
          </div>
        \`).join('');
      document.getElementById('incoming-bakes').innerHTML = incomingHtml;
      
      // Update active bakes
      const activeBakes = data.active || [];
      document.getElementById('active-count').textContent = activeBakes.length;
      const activeHtml = activeBakes.length === 0 
        ? '<p class="empty">No active bakes</p>'
        : activeBakes.map(bake => {
          const statusText = bake.details || bake.status;
          return \`
          <div class="bake-card active">
            <div class="slug">!<a href="https://aesthetic.computer/!\${bake.code || bake.slug}" target="_blank">\${bake.code || bake.slug}</a></div>
            <div class="status-badge status-\${bake.status}"><span class="spinner"></span>\${statusText}</div>
            <div class="meta">Duration: \${formatDuration(Date.now() - bake.startTime)}</div>
          </div>
        \`;
        }).join('');
      document.getElementById('active-bakes').innerHTML = activeHtml;
      
      // Update recent bakes
      const recentBakes = data.recent || [];
      document.getElementById('recent-count').textContent = recentBakes.length;
      const recentHtml = recentBakes.length === 0
        ? '<p class="empty">No recent bakes</p>'
        : recentBakes.map(bake => {
          const code = bake.code || bake.slug || 'unknown';
          // Build ATProto link using pdsls.dev format for proper record viewing
          // Anonymous tapes: art.at.aesthetic.computer
          // User tapes: {handle}.at.aesthetic.computer (based on user)
          let atprotoLink = '';
          if (bake.atprotoRkey) {
            const pdsHandle = bake.userHandle 
              ? \`\${bake.userHandle}.at.aesthetic.computer\`
              : 'art.at.aesthetic.computer';
            atprotoLink = \`<a href="https://pdsls.dev/at://\${pdsHandle}/computer.aesthetic.tape/\${bake.atprotoRkey}" target="_blank" style="color: #66f; text-decoration: none;">🦋 ATProto</a>\`;
          }
          
          return \`
          <div class="bake-card">
            <div class="slug">!<a href="https://aesthetic.computer/!\${code}" target="_blank">\${code}</a> \${atprotoLink}</div>
            \${bake.mp4Url && bake.thumbnailUrl ? \`
              <div class="media-preview">
                <video autoplay loop muted playsinline>
                  <source src="\${bake.mp4Url}" type="video/mp4">
                </video>
              </div>
            \` : ''}
            \${bake.error ? \`<div class="meta" style="color: #d32f2f;">Error: \${bake.error}</div>\` : ''}
          </div>
        \`;
        }).join('');
      document.getElementById('recent-bakes').innerHTML = recentHtml;
    }
    
    function formatTime(timestamp) {
      return new Date(timestamp).toLocaleTimeString();
    }
    
    function formatDuration(ms) {
      const seconds = Math.floor(ms / 1000);
      if (seconds < 60) return \`\${seconds}s\`;
      const minutes = Math.floor(seconds / 60);
      const remainingSeconds = seconds % 60;
      return \`\${minutes}m \${remainingSeconds}s\`;
    }
    
    // Request initial data
    fetch('/status').then(r => r.json()).then(updateDashboard);
  </script>
</body>
</html>
`);
});

// API endpoints
app.get('/health', healthHandler);
app.get('/status', statusHandler);
app.post('/bake', bakeHandler);
app.post('/bake-complete', bakeCompleteHandler);
app.post('/bake-status', bakeStatusHandler);

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

wss.on('connection', async (ws) => {
  console.log('📡 WebSocket client connected');
  
  // Clean up stale bakes before sending initial state
  await cleanupStaleBakes();
  
  // Send initial state
  ws.send(JSON.stringify({
    incoming: Array.from(getIncomingBakes().values()),
    active: Array.from(getActiveBakes().values()),
    recent: getRecentBakes()
  }));
  
  // Subscribe to updates
  const unsubscribe = subscribeToUpdates((update) => {
    if (ws.readyState === 1) { // OPEN
      ws.send(JSON.stringify({
        incoming: Array.from(getIncomingBakes().values()),
        active: Array.from(getActiveBakes().values()),
        recent: getRecentBakes()
      }));
    }
  });
  
  ws.on('close', () => {
    console.log('📡 WebSocket client disconnected');
    unsubscribe();
  });
});


