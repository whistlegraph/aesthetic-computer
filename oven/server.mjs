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
import { grabHandler, grabGetHandler, grabIPFSHandler, grabPiece, getCachedOrGenerate, getActiveGrabs, getRecentGrabs, getLatestKeepThumbnail, getLatestIPFSUpload, getAllLatestIPFSUploads, setNotifyCallback, setLogCallback, cleanupStaleGrabs, clearAllActiveGrabs, getQueueStatus, getCurrentProgress, getAllProgress, getConcurrencyStatus, IPFS_GATEWAY, generateKidlispOGImage, getOGImageCacheStatus, getFrozenPieces, clearFrozenPiece, getLatestOGImageUrl, regenerateOGImagesBackground, generateKidlispBackdrop, getLatestBackdropUrl, APP_SCREENSHOT_PRESETS, generateNotepatOGImage, getLatestNotepatOGUrl } from './grabber.mjs';
import archiver from 'archiver';
import { createBundle, createJSPieceBundle, createM4DBundle, generateDeviceHTML, prewarmCache, getCacheStatus, setSkipMinification } from './bundler.mjs';

const app = express();
const PORT = process.env.PORT || 3002;
const dev = process.env.NODE_ENV === 'development';

// Track server start time for uptime display
const SERVER_START_TIME = Date.now();

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
console.log(`🕐 Server started at: ${new Date(SERVER_START_TIME).toISOString()}`);

// Activity log buffer for streaming to clients
const activityLogBuffer = [];
const MAX_ACTIVITY_LOG = 100;
let wss = null; // Will be set after server starts

function addServerLog(type, icon, msg) {
  const entry = { time: new Date().toISOString(), type, icon, msg };
  activityLogBuffer.unshift(entry);
  if (activityLogBuffer.length > MAX_ACTIVITY_LOG) {
    activityLogBuffer.pop();
  }
  // Broadcast to connected clients if wss exists and has clients
  if (wss && wss.clients) {
    const logMsg = JSON.stringify({ logEntry: entry });
    wss.clients.forEach(client => {
      if (client.readyState === 1) client.send(logMsg);
    });
  }
}

// Export for use in other modules
export { addServerLog };

// Log server startup
addServerLog('info', '🔥', 'Oven server starting...');

// ===== SHARED PROGRESS UI COMPONENTS =====
// Shared CSS for progress indicators across all oven dashboards
const PROGRESS_UI_CSS = `
  /* Oven Progress UI - shared across all dashboards */
  .oven-loading {
    position: absolute;
    inset: 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    background: rgba(0,0,0,0.85);
    color: #888;
    text-align: center;
    padding: 10px;
    z-index: 10;
  }
  .oven-loading .preview-img {
    width: 80px;
    height: 80px;
    image-rendering: pixelated;
    border: 1px solid #333;
    margin-bottom: 8px;
    display: none;
    background: #111;
  }
  .oven-loading .loading-text {
    font-size: 12px;
    color: #fff;
  }
  .oven-loading .progress-text {
    font-size: 11px;
    margin-top: 8px;
    color: #88ff88;
    font-family: monospace;
    max-width: 150px;
    word-break: break-word;
  }
  .oven-loading .progress-bar {
    width: 80%;
    max-width: 150px;
    height: 4px;
    background: #333;
    border-radius: 2px;
    margin: 8px auto 0;
    overflow: hidden;
  }
  .oven-loading .progress-bar-fill {
    height: 100%;
    background: #88ff88;
    width: 0%;
    transition: width 0.3s ease;
  }
  .oven-loading.error {
    color: #f44;
  }
  .oven-loading.success {
    color: #4f4;
  }
`;

// Shared JavaScript for progress polling and UI updates
const PROGRESS_UI_JS = `
  // Shared progress state
  let progressPollInterval = null;
  
  // Update any loading indicator with progress data
  function updateOvenLoadingUI(container, data, queueInfo) {
    if (!container) return;
    
    const loadingText = container.querySelector('.loading-text');
    const progressText = container.querySelector('.progress-text');
    const progressBar = container.querySelector('.progress-bar-fill');
    const previewImg = container.querySelector('.preview-img');
    
    // Check if item is in queue and get position
    let queuePosition = null;
    if (queueInfo && queueInfo.length > 0 && data.piece) {
      const queueItem = queueInfo.find(q => q.piece === data.piece);
      if (queueItem) {
        queuePosition = queueItem.position;
      }
    }
    
    // Map stage to friendly text
    const stageText = {
      'loading': '🚀 Loading piece...',
      'waiting-content': '⏳ Waiting for render...',
      'settling': '⏸️ Settling...',
      'capturing': '📸 Capturing...',
      'encoding': '🔄 Processing...',
      'uploading': '☁️ Uploading...',
      'queued': queuePosition ? '⏳ In queue (#' + queuePosition + ')...' : '⏳ In queue...',
    };
    
    if (loadingText && data.stage) {
      loadingText.textContent = stageText[data.stage] || data.stage;
    }
    if (progressText && data.stageDetail) {
      progressText.textContent = data.stageDetail;
    }
    if (progressBar && data.percent != null) {
      progressBar.style.width = data.percent + '%';
    }
    // Show streaming preview
    if (previewImg && data.previewFrame) {
      previewImg.src = 'data:image/jpeg;base64,' + data.previewFrame;
      previewImg.style.display = 'block';
    }
  }
  
  // Create loading HTML structure
  function createOvenLoadingHTML(initialText = '🔥 Loading...') {
    return '<img class="preview-img" alt="preview">' +
           '<span class="loading-text">' + initialText + '</span>' +
           '<div class="progress-text"></div>' +
           '<div class="progress-bar"><div class="progress-bar-fill"></div></div>';
  }
  
  // Start polling /grab-status for progress updates
  function startProgressPolling(callback, intervalMs = 150) {
    stopProgressPolling();
    progressPollInterval = setInterval(async () => {
      try {
        const res = await fetch('/grab-status');
        const data = await res.json();
        if (callback && data.progress) {
          callback(data);
        }
      } catch (err) {
        // Ignore polling errors
      }
    }, intervalMs);
  }
  
  function stopProgressPolling() {
    if (progressPollInterval) {
      clearInterval(progressPollInterval);
      progressPollInterval = null;
    }
  }
`;

// Parse JSON bodies
app.use(express.json());

// CORS headers for cross-origin image loading (needed for canvas pixel validation)
app.use((req, res, next) => {
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
  if (req.method === 'OPTIONS') {
    return res.sendStatus(200);
  }
  next();
});

// Oven TV dashboard — live-updating visual bake monitor
app.get('/', (req, res) => {
  res.setHeader('Content-Type', 'text/html');
  res.send(OVEN_TV_HTML);
});

const OVEN_TV_HTML = `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>oven</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    :root {
      --bg: #f7f7f7;
      --bg-deep: #ececec;
      --bg-card: #fff;
      --bg-hover: #f0f0f0;
      --text: #111;
      --text-secondary: #555;
      --text-muted: #888;
      --text-dim: #aaa;
      --border: #ddd;
      --border-subtle: #e8e8e8;
      --accent: rgb(205, 92, 155);
      --accent-hover: rgb(220, 110, 170);
      --success: #2a9a2a;
      --error: #c44;
      --preview-bg: #e0e0e0;
      --overlay-bg: rgba(255,255,255,0.92);
      --scrollbar: transparent;
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --bg: #1e1e1e;
        --bg-deep: #161616;
        --bg-card: #252526;
        --bg-hover: #2a2a2a;
        --text: #d4d4d4;
        --text-secondary: #888;
        --text-muted: #666;
        --text-dim: #444;
        --border: #3e3e42;
        --border-subtle: #2e2e32;
        --accent: rgb(205, 92, 155);
        --accent-hover: rgb(225, 115, 175);
        --success: #4caf50;
        --error: #f44;
        --preview-bg: #111;
        --overlay-bg: rgba(0,0,0,0.88);
      }
    }

    * { box-sizing: border-box; margin: 0; padding: 0; }
    ::-webkit-scrollbar { display: none; }

    body {
      font-family: 'SF Mono', 'Monaco', 'Menlo', 'Consolas', monospace;
      font-size: 12px;
      background: var(--bg);
      color: var(--text);
      height: 100vh;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }

    .status-bar {
      background: var(--bg-deep);
      border-bottom: 2px solid var(--accent);
      padding: 5px 12px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      flex-shrink: 0;
      gap: 8px;
    }
    .status-bar .title { color: var(--accent); font-weight: bold; font-size: 1.05em; }
    .status-bar .stats { display: flex; gap: 12px; color: var(--text-muted); font-size: 0.9em; }
    .status-bar .stats span { white-space: nowrap; }
    .status-bar .stats .active { color: var(--success); }
    .status-bar .stats .queued { color: var(--accent); }
    .sb-btn {
      background: var(--bg-card); color: var(--text-secondary); border: 1px solid var(--border);
      padding: 3px 8px; cursor: pointer; font-family: inherit; font-size: 0.85em;
      border-radius: 3px; text-decoration: none; display: inline-block;
    }
    .sb-btn:hover { color: var(--text); border-color: var(--accent); }

    .hero {
      flex: 1;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 16px;
      padding: 16px;
      min-height: 0;
      overflow: hidden;
      flex-wrap: wrap;
    }
    .hero.idle { color: var(--text-dim); font-size: 1.2em; }
    .hero-card {
      background: var(--bg-card);
      border: 2px solid var(--border);
      border-radius: 6px;
      display: flex;
      flex-direction: column;
      align-items: center;
      padding: 10px;
      min-width: 160px;
      max-width: 260px;
      flex: 1;
    }
    .hero-card.capturing { border-color: var(--accent); }
    .hero-card .preview {
      width: 120px;
      height: 120px;
      background: var(--preview-bg);
      border-radius: 3px;
      overflow: hidden;
      display: flex;
      align-items: center;
      justify-content: center;
      margin-bottom: 8px;
    }
    .hero-card .preview img {
      width: 100%;
      height: 100%;
      object-fit: contain;
      image-rendering: pixelated;
    }
    .hero-card .preview .placeholder { color: var(--text-dim); font-size: 1.6em; }
    .hero-card .info { text-align: center; width: 100%; }
    .hero-card .piece-name { color: var(--accent); font-weight: bold; margin-bottom: 3px; font-size: 0.95em; }
    .hero-card .stage { color: var(--text-muted); font-size: 0.85em; margin-bottom: 6px; }
    .hero-card .progress-bar {
      width: 100%;
      height: 4px;
      background: var(--border);
      border-radius: 2px;
      overflow: hidden;
    }
    .hero-card .progress-bar .fill {
      height: 100%;
      background: var(--accent);
      transition: width 0.3s ease;
    }

    .strip {
      background: var(--bg-deep);
      border-top: 1px solid var(--border);
      padding: 5px 12px;
      flex-shrink: 0;
    }
    .strip-label {
      color: var(--text-muted);
      font-size: 0.75em;
      text-transform: uppercase;
      letter-spacing: 1px;
      margin-bottom: 4px;
    }
    .strip-items {
      display: flex;
      gap: 6px;
      overflow-x: auto;
      padding-bottom: 2px;
    }
    .strip-item {
      background: var(--bg-card);
      border: 1px solid var(--border);
      border-radius: 3px;
      padding: 2px 8px;
      white-space: nowrap;
      font-size: 0.85em;
      flex-shrink: 0;
    }
    .strip-item.queue { color: var(--accent); }
    .strip-empty { color: var(--text-dim); font-size: 0.85em; padding: 2px 0; }

    .history {
      background: var(--bg-deep);
      border-top: 1px solid var(--border);
      flex: 1;
      min-height: 0;
      overflow-y: auto;
      padding: 0;
    }
    .history .strip-label { padding: 5px 12px 3px; }
    .history-row {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 5px 12px;
      border-bottom: 1px solid var(--border-subtle);
    }
    .history-row:hover { background: var(--bg-hover); }
    .history-row .h-thumb {
      width: 44px;
      height: 44px;
      border-radius: 3px;
      background: var(--bg-card);
      flex-shrink: 0;
      overflow: hidden;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .history-row .h-thumb img {
      width: 100%;
      height: 100%;
      object-fit: cover;
      image-rendering: pixelated;
    }
    .history-row .h-thumb .h-none { color: var(--text-dim); font-size: 1em; }
    .history-row .h-main { flex: 1; min-width: 0; }
    .history-row .h-piece { font-weight: bold; font-size: 0.9em; }
    .history-row .h-piece a { color: inherit; text-decoration: none; }
    .history-row .h-piece a:hover { text-decoration: underline; }
    .history-row .h-meta { color: var(--text-muted); font-size: 0.78em; margin-top: 2px; display: flex; gap: 10px; flex-wrap: wrap; }
    .history-row .h-meta span { white-space: nowrap; }
    .history-row .h-error { color: var(--error); font-size: 0.78em; margin-top: 2px; opacity: 0.8; }
    .history-row .h-links { margin-top: 2px; display: flex; gap: 8px; }
    .history-row .h-links a { color: var(--accent); font-size: 0.78em; text-decoration: none; }
    .history-row .h-links a:hover { text-decoration: underline; }
    .history-row .h-right {
      flex-shrink: 0;
      text-align: right;
      font-size: 0.8em;
    }
    .history-row .h-status-done { color: var(--success); }
    .history-row .h-status-failed { color: var(--error); }
    .history-row .h-status-other { color: var(--text-muted); }
    .history-row .h-ago { color: var(--text-dim); font-size: 0.85em; }

    .capture-bar { display: none; }

    .log-overlay {
      display: none;
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background: var(--overlay-bg);
      z-index: 200;
      padding: 32px 16px 16px;
      overflow-y: auto;
    }
    .log-overlay.open { display: block; }
    .log-overlay .close {
      position: fixed;
      top: 6px;
      right: 12px;
      background: none;
      border: none;
      color: var(--accent);
      font-size: 1.3em;
      cursor: pointer;
      font-family: inherit;
    }
    .log-entry {
      padding: 1px 0;
      font-size: 0.82em;
      color: var(--text-secondary);
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .log-entry .time { color: var(--text-muted); }
    .log-entry.error { color: var(--error); }
    .log-entry.success { color: var(--success); }
  </style>
</head>
<body>

  <div class="status-bar">
    <span class="title">oven</span>
    <div class="stats">
      <span class="active" id="stat-active">0/6 active</span>
      <span class="queued" id="stat-queued">0 queued</span>
      <span id="stat-uptime">--</span>
      <span id="stat-version">--</span>
    </div>
    <div style="display:flex;gap:4px">
      <a href="/tools" class="sb-btn">Tools</a>
      <button class="sb-btn" id="log-btn" onclick="document.getElementById('log-overlay').classList.toggle('open')">Log</button>
    </div>
  </div>

  <div class="hero idle" id="hero">Waiting for grabs...</div>

  <div class="strip" id="queue-strip">
    <div class="strip-label">Up Next</div>
    <div class="strip-items" id="queue-items">
      <span class="strip-empty">No items queued</span>
    </div>
  </div>

  <div class="history" id="history">
    <div class="strip-label">Recent</div>
    <div id="history-items">
      <span class="strip-empty">No recent grabs</span>
    </div>
  </div>

  <div class="log-overlay" id="log-overlay">
    <button class="close" onclick="this.parentElement.classList.remove('open')">x</button>
    <div id="log-entries"></div>
  </div>

  <script>
    let serverVersion = null;
    let ws = null;
    let reconnectTimer = null;

    function connect() {
      const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
      ws = new WebSocket(proto + '//' + location.host + '/ws');

      ws.onopen = () => {
        document.getElementById('stat-version').textContent = 'connected';
        document.getElementById('stat-version').style.color = 'var(--success)';
        if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null; }
      };

      ws.onclose = () => {
        document.getElementById('stat-version').textContent = 'disconnected';
        document.getElementById('stat-version').style.color = 'var(--error)';
        reconnectTimer = setTimeout(connect, 2000);
      };

      ws.onmessage = (event) => {
        const data = JSON.parse(event.data);

        if (data.logEntry) { addLog(data.logEntry); return; }
        if (serverVersion && data.version && data.version !== serverVersion) {
          location.reload();
          return;
        }
        serverVersion = data.version;

        if (data.recentLogs) {
          data.recentLogs.forEach(addLog);
        }

        updateStatusBar(data);
        renderHero(data.grabProgress || {});
        renderQueue(data.grabs?.queue || []);
        renderHistory(data.grabs?.recent || []);
      };
    }

    function updateStatusBar(data) {
      const c = data.concurrency || {};
      document.getElementById('stat-active').textContent = (c.active || 0) + '/' + (c.max || 6) + ' active';
      document.getElementById('stat-queued').textContent = (c.queueDepth || 0) + ' queued';

      if (data.uptime) {
        const s = Math.floor(data.uptime / 1000);
        const m = Math.floor(s / 60);
        const h = Math.floor(m / 60);
        const d = Math.floor(h / 24);
        let upStr;
        if (d > 0) upStr = d + 'd ' + (h % 24) + 'h';
        else if (h > 0) upStr = h + 'h ' + (m % 60) + 'm';
        else upStr = m + 'm ' + (s % 60) + 's';
        document.getElementById('stat-uptime').textContent = 'up ' + upStr;
      }
      if (data.version) {
        document.getElementById('stat-version').textContent = data.version;
        document.getElementById('stat-version').style.color = 'var(--text-muted)';
      }
    }

    function renderHero(grabProgress) {
      const hero = document.getElementById('hero');
      const entries = Object.entries(grabProgress).filter(([, p]) => p.stage);

      if (entries.length === 0) {
        hero.className = 'hero idle';
        hero.innerHTML = 'Waiting for grabs...';
        return;
      }

      hero.className = 'hero';
      hero.innerHTML = entries.map(([grabId, p]) => {
        const previewSrc = p.previewFrame
          ? 'data:image/jpeg;base64,' + p.previewFrame
          : '';
        const previewHTML = previewSrc
          ? '<img src="' + previewSrc + '" alt="preview">'
          : '<span class="placeholder">...</span>';
        const stageLabel = p.stage ? (p.stage.charAt(0).toUpperCase() + p.stage.slice(1)) : '';
        const detail = p.stageDetail || '';
        const pct = p.percent || 0;

        return '<div class="hero-card' + (p.stage === 'capturing' ? ' capturing' : '') + '">' +
          '<div class="preview">' + previewHTML + '</div>' +
          '<div class="info">' +
            '<div class="piece-name">' + esc(p.piece || grabId) + '</div>' +
            '<div class="stage">' + esc(stageLabel + (detail ? ' — ' + detail : '')) + '</div>' +
            '<div class="progress-bar"><div class="fill" style="width:' + pct + '%"></div></div>' +
          '</div>' +
        '</div>';
      }).join('');
    }

    function renderQueue(queue) {
      const el = document.getElementById('queue-items');
      if (!queue || queue.length === 0) {
        el.innerHTML = '<span class="strip-empty">No items queued</span>';
        return;
      }
      el.innerHTML = queue.map((item, i) =>
        '<div class="strip-item queue">' +
          '#' + (i + 1) + ' ' + esc(item.piece || '?') +
          ' <span style="color:var(--text-muted)">(' + esc(item.format || '?') + ')</span>' +
          (item.estimatedWait ? ' <span style="color:var(--text-dim)">~' + Math.ceil(item.estimatedWait / 1000) + 's</span>' : '') +
        '</div>'
      ).join('');
    }

    function renderHistory(recent) {
      const el = document.getElementById('history-items');
      if (!recent || recent.length === 0) {
        el.innerHTML = '<span class="strip-empty" style="padding:5px 12px">No recent grabs</span>';
        return;
      }
      el.innerHTML = recent.slice(0, 30).map(grab => {
        const thumbImg = grab.cdnUrl
          ? '<img src="' + esc(grab.cdnUrl) + '" alt="">'
          : '<span class="h-none">--</span>';

        const pieceClass = grab.status === 'failed' ? 'h-status-failed' : '';

        const dur = grab.duration ? Math.round(grab.duration / 1000) + 's' : '';
        const dim = grab.dimensions ? grab.dimensions.width + 'x' + grab.dimensions.height : '';
        const fmt = (grab.format || '').toUpperCase();
        const size = grab.size ? (grab.size > 1024*1024 ? (grab.size/1024/1024).toFixed(1)+'MB' : Math.round(grab.size/1024)+'KB') : '';

        const metaParts = [fmt, dim, dur, size].filter(Boolean);
        const metaHTML = metaParts.map(m => '<span>' + esc(m) + '</span>').join('');

        const errorHTML = grab.error
          ? '<div class="h-error">' + esc(grab.error) + '</div>'
          : '';

        let linksHTML = '';
        if (grab.cdnUrl) {
          linksHTML = '<div class="h-links">' +
            '<a href="' + esc(grab.cdnUrl) + '" target="_blank">Open</a>' +
            '<a href="' + esc(grab.cdnUrl) + '" download>Download</a>' +
          '</div>';
        }

        const statusClass = grab.status === 'complete' ? 'h-status-done' :
                             grab.status === 'failed' ? 'h-status-failed' : 'h-status-other';
        const statusLabel = grab.status === 'complete' ? 'done' :
                            grab.status === 'failed' ? 'failed' :
                            esc(grab.status || '?');

        const ago = grab.completedAt ? timeAgo(grab.completedAt) : '';

        const pieceName = esc(grab.piece || grab.id || '?');
        const pieceLink = grab.cdnUrl
          ? '<a href="' + esc(grab.cdnUrl) + '" target="_blank">' + pieceName + '</a>'
          : pieceName;

        return '<div class="history-row">' +
          '<div class="h-thumb">' + thumbImg + '</div>' +
          '<div class="h-main">' +
            '<div class="h-piece ' + pieceClass + '">' + pieceLink + '</div>' +
            '<div class="h-meta">' + metaHTML + '</div>' +
            errorHTML +
            linksHTML +
          '</div>' +
          '<div class="h-right">' +
            '<div class="' + statusClass + '">' + statusLabel + '</div>' +
            '<div class="h-ago">' + esc(ago) + '</div>' +
          '</div>' +
        '</div>';
      }).join('');
    }

    function timeAgo(ts) {
      const s = Math.floor((Date.now() - ts) / 1000);
      if (s < 60) return s + 's ago';
      const m = Math.floor(s / 60);
      if (m < 60) return m + 'm ago';
      const h = Math.floor(m / 60);
      if (h < 24) return h + 'h ago';
      return Math.floor(h / 24) + 'd ago';
    }

    function addLog(entry) {
      if (!entry) return;
      const el = document.getElementById('log-entries');
      const div = document.createElement('div');
      div.className = 'log-entry' + (entry.type === 'error' ? ' error' : entry.type === 'success' ? ' success' : '');
      const time = entry.time ? new Date(entry.time).toLocaleTimeString() : '';
      div.innerHTML = '<span class="time">' + time + '</span> ' + esc((entry.icon || '') + ' ' + (entry.msg || ''));
      el.prepend(div);
      while (el.children.length > 200) el.removeChild(el.lastChild);
    }

    function esc(s) {
      if (!s) return '';
      return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
    }

    connect();
  </script>
</body>
</html>`;

// Tools submenu — links to OG images, app screenshots, bundles, status pages
app.get('/tools', (req, res) => {
  res.setHeader('Content-Type', 'text/html');
  res.send(`<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>oven / tools</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    :root {
      --bg: #f7f7f7; --text: #111; --text-muted: #888; --text-dim: #aaa;
      --accent: rgb(205, 92, 155); --border: #ddd;
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --bg: #1e1e1e; --text: #d4d4d4; --text-muted: #666; --text-dim: #444;
        --accent: rgb(205, 92, 155); --border: #3e3e42;
      }
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: 'SF Mono', 'Monaco', 'Menlo', 'Consolas', monospace; font-size: 12px; background: var(--bg); color: var(--text); padding: 20px; }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    h1 { color: var(--accent); margin-bottom: 20px; font-size: 1.1em; }
    h1 a { color: var(--text-muted); }
    h2 { color: var(--text-muted); margin: 16px 0 6px; font-size: 0.9em; text-transform: uppercase; letter-spacing: 1px; }
    .links { display: flex; flex-direction: column; gap: 4px; margin-left: 10px; }
    .links a { padding: 2px 0; }
    .desc { color: var(--text-dim); font-size: 0.85em; margin-left: 8px; }
    hr { border: none; border-top: 1px solid var(--border); margin: 12px 0; }
  </style>
</head>
<body>
  <h1><a href="/">oven</a> / tools</h1>

  <h2>OG Images</h2>
  <div class="links">
    <div><a href="/kidlisp-og.png">/kidlisp-og.png</a><span class="desc">KidLisp OG image</span></div>
    <div><a href="/kidlisp-og">/kidlisp-og</a><span class="desc">KidLisp OG HTML page</span></div>
    <div><a href="/kidlisp-og/status">/kidlisp-og/status</a><span class="desc">OG cache status</span></div>
    <div><a href="/kidlisp-og/preview">/kidlisp-og/preview</a><span class="desc">OG preview</span></div>
    <div><a href="/og-preview">/og-preview</a><span class="desc">OG preview (alt)</span></div>
    <div><a href="/notepat-og.png">/notepat-og.png</a><span class="desc">Notepat OG image</span></div>
    <div><a href="/kidlisp-backdrop.webp">/kidlisp-backdrop.webp</a><span class="desc">KidLisp backdrop animation</span></div>
    <div><a href="/kidlisp-backdrop">/kidlisp-backdrop</a><span class="desc">KidLisp backdrop page</span></div>
  </div>

  <h2>App Screenshots</h2>
  <div class="links">
    <div><a href="/app-screenshots">/app-screenshots</a><span class="desc">Screenshot dashboard</span></div>
  </div>

  <h2>Bundles</h2>
  <div class="links">
    <div><a href="/bundle-status">/bundle-status</a><span class="desc">Bundle cache status</span></div>
    <div><a href="/bundle-html?piece=prompt">/bundle-html?piece=...</a><span class="desc">Generate HTML bundle (SSE)</span></div>
  </div>

  <h2>Grabs</h2>
  <div class="links">
    <div><a href="/grab-status">/grab-status</a><span class="desc">Active grabs + queue (JSON)</span></div>
    <div><a href="/api/frozen">/api/frozen</a><span class="desc">Frozen pieces list</span></div>
    <div><a href="/keeps/all">/keeps/all</a><span class="desc">All latest IPFS uploads</span></div>
    <div><a href="/keeps/latest">/keeps/latest</a><span class="desc">Latest keep thumbnail</span></div>
  </div>

  <h2>Status</h2>
  <div class="links">
    <div><a href="/health">/health</a><span class="desc">Health check</span></div>
    <div><a href="/status">/status</a><span class="desc">Server status + recent bakes</span></div>
  </div>
</body>
</html>`);
});

// API endpoints
app.get('/health', healthHandler);

// Override status to include grabs
app.get('/status', async (req, res) => {
  await cleanupStaleBakes();
  res.json({
    version: GIT_VERSION,
    serverStartTime: SERVER_START_TIME,
    uptime: Date.now() - SERVER_START_TIME,
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
      // Handle case where grabPiece returns from its own cache (cdnUrl but no buffer)
      if (result.cached && result.cdnUrl && !result.buffer) {
        // Fetch the buffer from the CDN URL
        const response = await fetch(result.cdnUrl);
        if (!response.ok) throw new Error(`Failed to fetch cached icon: ${response.status}`);
        return Buffer.from(await response.arrayBuffer());
      }
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

// Animated WebP Icon endpoint - small animated square favicons
// GET /icon/{width}x{height}/{piece}.webp
// Uses 7-day Spaces cache since animated icons are expensive to generate
app.get('/icon/:size/:piece.webp', async (req, res) => {
  const { size, piece } = req.params;
  const [width, height] = size.split('x').map(n => parseInt(n) || 128);
  // Keep animated icons small for performance (max 128x128)
  const w = Math.min(width, 128);
  const h = Math.min(height, 128);
  
  // Query params for customization
  const frames = Math.min(parseInt(req.query.frames) || 30, 60); // Default 30 frames, max 60
  const fps = Math.min(parseInt(req.query.fps) || 15, 30); // Default 15 fps, max 30
  
  try {
    const cacheKey = `${piece}-${w}x${h}-f${frames}-fps${fps}`;
    const { cdnUrl, fromCache, buffer } = await getCachedOrGenerate('animated-icons', cacheKey, w, h, async () => {
      const result = await grabPiece(piece, {
        format: 'webp',
        width: w,
        height: h,
        density: 1,
        frames: frames,
        fps: fps,
      });
      if (!result.success) throw new Error(result.error);
      // Handle case where grabPiece returns from its own cache (cdnUrl but no buffer)
      if (result.cached && result.cdnUrl && !result.buffer) {
        const response = await fetch(result.cdnUrl);
        if (!response.ok) throw new Error(`Failed to fetch cached icon: ${response.status}`);
        return Buffer.from(await response.arrayBuffer());
      }
      return result.buffer;
    }, 'webp');
    
    if (fromCache && cdnUrl) {
      res.setHeader('X-Cache', 'HIT');
      res.setHeader('Cache-Control', 'public, max-age=604800'); // 7 days
      return res.redirect(302, cdnUrl);
    }
    
    res.setHeader('Content-Type', 'image/webp');
    res.setHeader('Content-Length', buffer.length);
    res.setHeader('Cache-Control', 'public, max-age=86400'); // 1 day for fresh
    res.setHeader('X-Cache', 'MISS');
    res.send(buffer);
  } catch (error) {
    console.error('Animated icon handler error:', error);
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
      // Handle case where grabPiece returns from its own cache (cdnUrl but no buffer)
      if (result.cached && result.cdnUrl && !result.buffer) {
        const response = await fetch(result.cdnUrl);
        if (!response.ok) throw new Error(`Failed to fetch cached preview: ${response.status}`);
        return Buffer.from(await response.arrayBuffer());
      }
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
    recent: getRecentGrabs(),
    queue: getQueueStatus(),
    progress: getCurrentProgress(),
    grabProgress: getAllProgress(),
    concurrency: getConcurrencyStatus(),
  });
});

// Cleanup stale grabs (grabs stuck for > 5 minutes)
app.post('/grab-cleanup', (req, res) => {
  const result = cleanupStaleGrabs();
  addServerLog('cleanup', '🧹', `Manual cleanup: ${result.cleaned} stale grabs removed`);
  res.json({
    success: true,
    ...result
  });
});

// Emergency clear all active grabs (admin only)
app.post('/grab-clear', (req, res) => {
  const result = clearAllActiveGrabs();
  addServerLog('cleanup', '🗑️', `Emergency clear: ${result.cleared} grabs force-cleared`);
  res.json({
    success: true,
    ...result
  });
});

// Frozen pieces API - get list of frozen pieces
app.get('/api/frozen', (req, res) => {
  res.json({
    frozen: getFrozenPieces()
  });
});

// Clear a piece from the frozen list
app.delete('/api/frozen/:piece', async (req, res) => {
  const piece = decodeURIComponent(req.params.piece);
  const result = await clearFrozenPiece(piece);
  addServerLog('cleanup', '✅', `Cleared frozen piece: ${piece}`);
  res.json(result);
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

// =============================================================================
// KidLisp.com OG Preview Image Endpoint
// =============================================================================

// Fast static PNG endpoint - redirects instantly to CDN (for social media crawlers)
// Use this URL in og:image and twitter:image meta tags
app.get('/kidlisp-og.png', async (req, res) => {
  try {
    const layout = req.query.layout || 'mosaic';
    
    // Get cached URL without triggering generation (fast!)
    const url = await getLatestOGImageUrl(layout);
    
    if (url) {
      // Redirect to CDN - instant response
      res.setHeader('Cache-Control', 'public, max-age=3600');
      res.setHeader('X-Cache', 'CDN');
      return res.redirect(301, url);
    }
    
    // No cached image yet - trigger background regeneration and serve a recent fallback
    addServerLog('warn', '⚠️', `OG cache miss for ${layout}, triggering regen`);
    
    // Trigger async regeneration (don't await)
    regenerateOGImagesBackground().catch(err => {
      addServerLog('error', '❌', `Async OG regen failed: ${err.message}`);
    });
    
    // Use yesterday's image as fallback (likely exists)
    const yesterday = new Date(Date.now() - 86400000).toISOString().split('T')[0];
    const fallbackUrl = `https://art.aesthetic.computer/og/kidlisp/${yesterday}-${layout}.png`;
    
    res.setHeader('Cache-Control', 'public, max-age=300'); // Short cache for fallback
    return res.redirect(302, fallbackUrl);
    
  } catch (error) {
    console.error('KidLisp OG PNG error:', error);
    // Ultimate fallback - yesterday's mosaic
    const yesterday = new Date(Date.now() - 86400000).toISOString().split('T')[0];
    return res.redirect(302, `https://art.aesthetic.computer/og/kidlisp/${yesterday}-mosaic.png`);
  }
});

// Dynamic OG image for kidlisp.com - rotates daily based on top hits
// Supports multiple layout options: featured, mosaic, filmstrip, code-split
app.get('/kidlisp-og', async (req, res) => {
  try {
    const layout = req.query.layout || 'featured';
    const force = req.query.force === 'true';
    
    // Validate layout
    const validLayouts = ['featured', 'mosaic', 'filmstrip', 'code-split'];
    if (!validLayouts.includes(layout)) {
      return res.status(400).json({
        error: 'Invalid layout',
        valid: validLayouts,
      });
    }
    
    addServerLog('info', '🖼️', `KidLisp OG request: ${layout}${force ? ' (force)' : ''}`);
    
    const result = await generateKidlispOGImage(layout, force);
    
    if (result.cached && result.url) {
      // Redirect to CDN URL for cached images
      addServerLog('success', '📦', `OG cache hit → ${result.url.split('/').pop()}`);
      res.setHeader('X-Cache', 'HIT');
      res.setHeader('Cache-Control', 'public, max-age=3600');
      return res.redirect(302, result.url);
    }
    
    // Fresh generation - return the buffer directly
    addServerLog('success', '🎨', `OG generated: ${layout} (${result.featuredPiece?.code || 'mosaic'})`);
    res.setHeader('Content-Type', 'image/png');
    res.setHeader('Content-Length', result.buffer.length);
    res.setHeader('Cache-Control', 'public, max-age=86400'); // 24hr cache
    res.setHeader('X-Cache', 'MISS');
    res.setHeader('X-OG-Layout', layout);
    res.setHeader('X-OG-Generated', result.generatedAt);
    if (result.featuredPiece) {
      res.setHeader('X-OG-Featured', result.featuredPiece.code);
    }
    res.send(result.buffer);
    
  } catch (error) {
    console.error('KidLisp OG error:', error);
    addServerLog('error', '❌', `OG error: ${error.message}`);
    res.status(500).json({ 
      error: 'Failed to generate OG image',
      message: error.message 
    });
  }
});

// OG image cache status endpoint
app.get('/kidlisp-og/status', (req, res) => {
  res.json({
    ...getOGImageCacheStatus(),
    availableLayouts: ['featured', 'mosaic', 'filmstrip', 'code-split'],
    usage: {
      recommended: '/kidlisp-og.png (instant, for og:image tags)',
      withLayout: '/kidlisp-og.png?layout=mosaic',
      dynamic: '/kidlisp-og (may regenerate on-demand)',
      forceRegenerate: '/kidlisp-og?force=true',
    },
    note: 'Use /kidlisp-og.png for social media meta tags - it redirects instantly to cached CDN images'
  });
});

// Preview all OG images (generalized for kidlisp, notepat, etc)
app.get('/og-preview', (req, res) => {
  const baseUrl = req.protocol + '://' + req.get('host');

  const ogImages = [
    {
      name: 'KidLisp',
      slug: 'kidlisp-og',
      prodUrls: [
        'https://kidlisp.com',
        'https://aesthetic.computer/kidlisp'
      ],
      layouts: ['featured', 'mosaic', 'filmstrip', 'code-split'],
      description: 'Dynamic layouts featuring recent KidLisp pieces'
    },
    {
      name: 'Notepat',
      slug: 'notepat-og',
      prodUrls: [
        'https://notepat.com',
        'https://aesthetic.computer/notepat'
      ],
      layouts: null, // Single layout
      description: 'Split-layout chromatic piano interface'
    }
  ];

  res.setHeader('Content-Type', 'text/html');
  res.send(`<!DOCTYPE html>
<html>
<head>
  <title>OG Image Preview</title>
  <style>
    body { font-family: monospace; background: #1a1a2e; color: white; padding: 20px; max-width: 1400px; margin: 0 auto; }
    h1 { color: #88ff88; }
    h2 { color: #ffaa00; margin-top: 40px; }
    h3 { color: #ff88aa; margin-top: 20px; }
    .note { background: #2a2a4e; padding: 16px; border-radius: 8px; margin: 20px 0; line-height: 1.6; }
    .note code { background: #3a3a5e; padding: 2px 6px; border-radius: 4px; color: #88ffaa; }
    .og-section { border: 2px solid #333; padding: 20px; border-radius: 8px; margin: 30px 0; background: #16162e; }
    .prod-urls { margin: 15px 0; }
    .prod-urls a {
      display: inline-block;
      color: #88ccff;
      text-decoration: none;
      background: #2a2a4e;
      padding: 6px 12px;
      border-radius: 4px;
      margin: 4px 4px 4px 0;
    }
    .prod-urls a:hover { background: #3a3a5e; }
    .layout { margin: 20px 0; padding: 15px; background: #0a0a1e; border-radius: 6px; }
    .layout h4 { color: #ffcc66; margin: 0 0 10px 0; }
    .layout img { max-width: 100%; border: 2px solid #444; border-radius: 4px; }
    .layout .actions { margin: 10px 0; }
    .layout .actions a {
      color: #88ccff;
      margin-right: 15px;
      text-decoration: none;
    }
    .layout .actions a:hover { text-decoration: underline; }
    .single-image { margin: 20px 0; }
    .single-image img { max-width: 100%; border: 2px solid #444; border-radius: 4px; }
    .back-link { display: inline-block; margin-top: 40px; color: #888; text-decoration: none; }
    .back-link:hover { color: #aaa; }
  </style>
</head>
<body>
  <h1>🖼️ OG Image Preview Dashboard</h1>
  <div class="note">
    <strong>About:</strong> This page shows all Open Graph (OG) images used for social media previews.<br>
    <strong>Usage:</strong> Use the <code>.png</code> endpoints in meta tags for instant CDN redirects (no timeouts).<br>
    <strong>Testing:</strong> Click production URLs below to verify OG tags are working correctly.
  </div>

  ${ogImages.map(og => `
    <div class="og-section">
      <h2>${og.name}</h2>
      <p style="color: #aaa; margin: 10px 0;">${og.description}</p>

      <div class="prod-urls">
        <strong style="color: #88ff88;">Production URLs:</strong><br>
        ${og.prodUrls.map(url => `<a href="${url}" target="_blank">${url} →</a>`).join(' ')}
      </div>

      <div class="note">
        <strong>OG Endpoint:</strong> <code>${baseUrl}/${og.slug}.png</code>
      </div>

      ${og.layouts ? `
        <h3>Layouts:</h3>
        ${og.layouts.map(layout => `
          <div class="layout">
            <h4>${layout.charAt(0).toUpperCase() + layout.slice(1)}</h4>
            <div class="actions">
              <a href="${baseUrl}/${og.slug}?layout=${layout}&force=true">Force Regenerate</a>
              <a href="${baseUrl}/${og.slug}/status">Cache Status</a>
            </div>
            <img src="${baseUrl}/${og.slug}?layout=${layout}" alt="${layout} layout" loading="lazy">
          </div>
        `).join('')}
      ` : `
        <div class="single-image">
          <div class="actions">
            <a href="${baseUrl}/${og.slug}.png?force=true">Force Regenerate</a>
          </div>
          <img src="${baseUrl}/${og.slug}.png" alt="${og.name} OG image" loading="lazy">
        </div>
      `}
    </div>
  `).join('')}

  <a href="/" class="back-link">← Back to Oven Dashboard</a>
</body>
</html>`);
});

// Legacy redirect for old kidlisp preview URL
app.get('/kidlisp-og/preview', (req, res) => {
  res.redirect(302, '/og-preview');
});

// Notepat branded OG image for notepat.com
app.get('/notepat-og.png', async (req, res) => {
  try {
    const force = req.query.force === 'true';
    
    addServerLog('info', '🎹', `Notepat OG request${force ? ' (force)' : ''}`);
    
    const result = await generateNotepatOGImage(force);
    
    if (result.cached && result.url) {
      // Proxy the image back instead of redirecting (iOS crawlers won't follow 301s on og:image)
      addServerLog('success', '📦', `Notepat OG cache hit → proxying`);
      try {
        const cdnResponse = await fetch(result.url);
        if (!cdnResponse.ok) throw new Error(`CDN fetch failed: ${cdnResponse.status}`);
        const buffer = Buffer.from(await cdnResponse.arrayBuffer());
        res.setHeader('Content-Type', 'image/png');
        res.setHeader('Content-Length', buffer.length);
        res.setHeader('Cache-Control', 'public, max-age=604800'); // 7-day cache
        res.setHeader('X-Cache', 'HIT');
        return res.send(buffer);
      } catch (fetchErr) {
        // Fall back to redirect if proxy fails
        addServerLog('warn', '⚠️', `Notepat OG proxy failed, falling back to redirect: ${fetchErr.message}`);
        return res.redirect(301, result.url);
      }
    }

    // Fresh generation - return the buffer directly
    addServerLog('success', '🎨', `Notepat OG generated`);
    res.setHeader('Content-Type', 'image/png');
    res.setHeader('Content-Length', result.buffer.length);
    res.setHeader('Cache-Control', 'public, max-age=604800'); // 7-day cache
    res.setHeader('X-Cache', 'MISS');
    res.send(result.buffer);
    
  } catch (error) {
    console.error('Notepat OG error:', error);
    addServerLog('error', '❌', `Notepat OG error: ${error.message}`);
    res.status(500).json({ 
      error: 'Failed to generate Notepat OG image',
      message: error.message 
    });
  }
});

// =============================================================================
// KidLisp Backdrop - Animated WebP for login screens, Auth0, etc.
// =============================================================================

// Fast redirect to CDN-cached 2048px animated webp
app.get('/kidlisp-backdrop.webp', async (req, res) => {
  try {
    // Get cached URL without triggering generation (fast!)
    const url = await getLatestBackdropUrl();
    
    if (url) {
      res.setHeader('Cache-Control', 'public, max-age=3600');
      res.setHeader('X-Cache', 'CDN');
      return res.redirect(301, url);
    }
    
    // No cached backdrop - generate synchronously (first request will be slow)
    addServerLog('warn', '⚠️', 'Backdrop cache miss, generating...');
    
    const result = await generateKidlispBackdrop(false);
    if (result.url) {
      res.setHeader('Cache-Control', 'public, max-age=3600');
      res.setHeader('X-Cache', 'MISS');
      return res.redirect(302, result.url);
    }
    
    res.status(503).json({ error: 'Backdrop generation in progress, try again shortly' });
    
  } catch (error) {
    console.error('Backdrop error:', error);
    res.status(500).json({ error: 'Failed to get backdrop', message: error.message });
  }
});

// Dynamic backdrop generation (may regenerate on-demand)
app.get('/kidlisp-backdrop', async (req, res) => {
  try {
    const force = req.query.force === 'true';
    
    addServerLog('info', '🖼️', `Backdrop request${force ? ' (force)' : ''}`);
    
    const result = await generateKidlispBackdrop(force);
    
    if (result.url) {
      addServerLog('success', '🎨', `Backdrop: ${result.piece} → ${result.cached ? 'cached' : 'generated'}`);
      res.setHeader('Cache-Control', 'public, max-age=3600');
      res.setHeader('X-Cache', result.cached ? 'HIT' : 'MISS');
      res.setHeader('X-Backdrop-Piece', result.piece || 'unknown');
      return res.redirect(302, result.url);
    }
    
    res.status(500).json({ error: 'Failed to generate backdrop' });
    
  } catch (error) {
    console.error('Backdrop error:', error);
    addServerLog('error', '❌', `Backdrop error: ${error.message}`);
    res.status(500).json({ error: 'Failed to generate backdrop', message: error.message });
  }
});

// =============================================================================
// App Store Screenshots - Generate screenshots for Google Play / App Store
// =============================================================================

// App screenshots dashboard
app.get('/app-screenshots', (req, res) => {
  const piece = req.query.piece || 'prompt';
  const presets = Object.entries(APP_SCREENSHOT_PRESETS);
  
  res.setHeader('Content-Type', 'text/html');
  res.send(`<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>📱 App Store Screenshots - Oven</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: monospace;
      font-size: 14px;
      background: #0a0a12;
      color: #fff;
      min-height: 100vh;
      padding: 20px;
    }
    header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      flex-wrap: wrap;
      gap: 1em;
      padding-bottom: 20px;
      border-bottom: 2px solid #333;
      margin-bottom: 20px;
    }
    h1 { color: #88ff88; font-size: 1.5em; }
    .controls {
      display: flex;
      gap: 1em;
      align-items: center;
      flex-wrap: wrap;
    }
    input, select, button {
      font-family: monospace;
      font-size: 14px;
      padding: 8px 12px;
      border: 1px solid #444;
      background: #1a1a2e;
      color: #fff;
      border-radius: 4px;
    }
    button {
      cursor: pointer;
      background: #2a2a4e;
    }
    button:hover { background: #3a3a5e; border-color: #88ff88; }
    button:disabled { opacity: 0.5; cursor: not-allowed; }
    .btn-primary { background: #226622; border-color: #88ff88; }
    .btn-primary:hover { background: #338833; }
    
    .requirements {
      background: #1a1a2e;
      padding: 15px;
      border-radius: 8px;
      margin-bottom: 20px;
      border: 1px solid #333;
    }
    .requirements h3 { color: #ffaa00; margin-bottom: 10px; }
    .requirements ul { list-style: none; }
    .requirements li { margin: 5px 0; padding-left: 20px; position: relative; }
    .requirements li::before { content: '✓'; position: absolute; left: 0; color: #88ff88; }
    
    .category { margin-bottom: 30px; }
    .category h2 {
      color: #ffaa00;
      margin-bottom: 15px;
      padding-bottom: 10px;
      border-bottom: 1px solid #333;
    }
    .screenshots {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
      gap: 20px;
    }
    .screenshot {
      background: #1a1a2e;
      border: 1px solid #333;
      border-radius: 8px;
      overflow: hidden;
    }
    .screenshot:hover { border-color: #88ff88; }
    .screenshot-preview {
      background: #000;
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 200px;
      position: relative;
    }
    .screenshot-preview img {
      max-width: 100%;
      max-height: 300px;
      object-fit: contain;
    }
    .screenshot-preview .loading {
      position: absolute;
      color: #888;
      text-align: center;
      padding: 10px;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
    }
    .screenshot-preview .loading .preview-img {
      width: 80px;
      height: 80px;
      image-rendering: pixelated;
      border: 1px solid #333;
      margin-bottom: 8px;
      display: none;
    }
    .screenshot-preview .loading .progress-text {
      font-size: 11px;
      margin-top: 8px;
      color: #88ff88;
      font-family: monospace;
      max-width: 150px;
      word-break: break-word;
    }
    .screenshot-preview .loading .progress-bar {
      width: 80%;
      max-width: 150px;
      height: 4px;
      background: #333;
      border-radius: 2px;
      margin: 8px auto 0;
      overflow: hidden;
    }
    .screenshot-preview .loading .progress-bar-fill {
      height: 100%;
      background: #88ff88;
      width: 0%;
      transition: width 0.3s ease;
    }
    .screenshot-preview .error {
      color: #ff4444;
      padding: 20px;
      text-align: center;
    }
    .screenshot-info {
      padding: 15px;
    }
    .screenshot-info h4 { margin-bottom: 8px; }
    .screenshot-info .dims {
      color: #888;
      font-size: 12px;
      margin-bottom: 10px;
    }
    .screenshot-info .actions {
      display: flex;
      gap: 8px;
      flex-wrap: wrap;
    }
    .screenshot-info .actions a, .screenshot-info .actions button {
      font-size: 12px;
      padding: 6px 10px;
      text-decoration: none;
      color: #88ccff;
    }
    .status {
      position: fixed;
      bottom: 20px;
      right: 20px;
      background: #2a2a4e;
      padding: 15px 20px;
      border-radius: 8px;
      border: 1px solid #444;
      display: none;
    }
    .status.show { display: block; }
    .status.success { border-color: #88ff88; }
    .status.error { border-color: #ff4444; }
    
    .back-link {
      color: #88ccff;
      text-decoration: none;
      margin-bottom: 20px;
      display: inline-block;
    }
    .back-link:hover { text-decoration: underline; }
  </style>
</head>
<body>
  <a href="/" class="back-link">← Back to Oven Dashboard</a>
  
  <header>
    <h1>📱 App Store Screenshots</h1>
    <div class="controls">
      <label>Piece: <input type="text" id="piece-input" value="${piece}" placeholder="prompt"></label>
      <button onclick="changePiece()">Load</button>
      <button onclick="regenerateAll()" class="btn-primary">🔄 Regenerate All</button>
      <button onclick="downloadZip()" class="btn-primary">📦 Download ZIP</button>
    </div>
  </header>
  
  <div class="requirements">
    <h3>📋 Google Play Requirements</h3>
    <ul>
      <li>PNG or JPEG, max 8MB each</li>
      <li>16:9 or 9:16 aspect ratio</li>
      <li>Phone: 320-3840px per side, 1080px min for promotion</li>
      <li>7" Tablet: 320-3840px per side</li>
      <li>10" Tablet: 1080-7680px per side</li>
      <li>2-8 screenshots per category required</li>
    </ul>
  </div>
  
  <div class="category">
    <h2>📱 Phone Screenshots</h2>
    <div class="screenshots">
      ${presets.filter(([k, v]) => v.category === 'phone').map(([key, preset]) => `
        <div class="screenshot" data-preset="${key}">
          <div class="screenshot-preview">
            <span class="loading" data-loading="${key}">
              <img class="preview-img" alt="preview">
              <span class="loading-text">🔥 Loading...</span>
              <div class="progress-text" data-progress-text="${key}"></div>
              <div class="progress-bar"><div class="progress-bar-fill" data-progress-bar="${key}"></div></div>
            </span>
            <img src="/app-screenshots/${key}/${piece}.png" 
                 alt="${preset.label}"
                 data-img="${key}"
                 onload="this.previousElementSibling.style.display='none'"
                 onerror="this.style.display='none'; this.previousElementSibling.innerHTML='❌ Failed to load'">
          </div>
          <div class="screenshot-info">
            <h4>${preset.label}</h4>
            <div class="dims">${preset.width} × ${preset.height}px</div>
            <div class="actions">
              <a href="/app-screenshots/${key}/${piece}.png" download="${piece}-${key}.png">⬇️ Download</a>
              <button onclick="regenerate('${key}')">🔄 Regenerate</button>
            </div>
          </div>
        </div>
      `).join('')}
    </div>
  </div>
  
  <div class="category">
    <h2>📱 7-inch Tablet Screenshots</h2>
    <div class="screenshots">
      ${presets.filter(([k, v]) => v.category === 'tablet7').map(([key, preset]) => `
        <div class="screenshot" data-preset="${key}">
          <div class="screenshot-preview">
            <span class="loading" data-loading="${key}">
              <img class="preview-img" alt="preview">
              <span class="loading-text">🔥 Loading...</span>
              <div class="progress-text" data-progress-text="${key}"></div>
              <div class="progress-bar"><div class="progress-bar-fill" data-progress-bar="${key}"></div></div>
            </span>
            <img src="/app-screenshots/${key}/${piece}.png" 
                 alt="${preset.label}"
                 data-img="${key}"
                 onload="this.previousElementSibling.style.display='none'"
                 onerror="this.style.display='none'; this.previousElementSibling.innerHTML='❌ Failed to load'">
          </div>
          <div class="screenshot-info">
            <h4>${preset.label}</h4>
            <div class="dims">${preset.width} × ${preset.height}px</div>
            <div class="actions">
              <a href="/app-screenshots/${key}/${piece}.png" download="${piece}-${key}.png">⬇️ Download</a>
              <button onclick="regenerate('${key}')">🔄 Regenerate</button>
            </div>
          </div>
        </div>
      `).join('')}
    </div>
  </div>
  
  <div class="category">
    <h2>📱 10-inch Tablet Screenshots</h2>
    <div class="screenshots">
      ${presets.filter(([k, v]) => v.category === 'tablet10').map(([key, preset]) => `
        <div class="screenshot" data-preset="${key}">
          <div class="screenshot-preview">
            <span class="loading" data-loading="${key}">
              <img class="preview-img" alt="preview">
              <span class="loading-text">🔥 Loading...</span>
              <div class="progress-text" data-progress-text="${key}"></div>
              <div class="progress-bar"><div class="progress-bar-fill" data-progress-bar="${key}"></div></div>
            </span>
            <img src="/app-screenshots/${key}/${piece}.png" 
                 alt="${preset.label}"
                 data-img="${key}"
                 onload="this.previousElementSibling.style.display='none'"
                 onerror="this.style.display='none'; this.previousElementSibling.innerHTML='❌ Failed to load'">
          </div>
          <div class="screenshot-info">
            <h4>${preset.label}</h4>
            <div class="dims">${preset.width} × ${preset.height}px</div>
            <div class="actions">
              <a href="/app-screenshots/${key}/${piece}.png" download="${piece}-${key}.png">⬇️ Download</a>
              <button onclick="regenerate('${key}')">🔄 Regenerate</button>
            </div>
          </div>
        </div>
      `).join('')}
    </div>
  </div>
  
  <div id="status" class="status"></div>
  
  <script>
    const currentPiece = '${piece}';
    
    function showStatus(msg, type = 'info') {
      const el = document.getElementById('status');
      el.textContent = msg;
      el.className = 'status show ' + type;
      setTimeout(() => el.className = 'status', 3000);
    }
    
    function changePiece() {
      const piece = document.getElementById('piece-input').value.trim() || 'prompt';
      window.location.href = '/app-screenshots?piece=' + encodeURIComponent(piece);
    }
    
    document.getElementById('piece-input').addEventListener('keydown', (e) => {
      if (e.key === 'Enter') changePiece();
    });
    
    async function regenerate(preset) {
      showStatus('Regenerating ' + preset + '... (this takes ~30s)');
      console.log('🔄 Starting regeneration for:', preset);
      
      // Show loading indicator and hide current image
      const card = document.querySelector('[data-preset="' + preset + '"]');
      const img = card.querySelector('[data-img]');
      const loading = card.querySelector('.loading');
      
      img.style.display = 'none';
      loading.style.display = 'flex';
      loading.innerHTML = '<img class="preview-img" alt="preview"><span class="loading-text">🔄 Regenerating...</span><div class="progress-text"></div><div class="progress-bar"><div class="progress-bar-fill" style="width:0%"></div></div>';
      
      const startTime = Date.now();
      
      try {
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), 120000); // 2 min timeout
        
        console.log('📡 Fetching with force=true...');
        const res = await fetch('/app-screenshots/' + preset + '/' + currentPiece + '.png?force=true&t=' + Date.now(), {
          signal: controller.signal,
          cache: 'no-store',
          headers: { 'Cache-Control': 'no-cache' }
        });
        clearTimeout(timeoutId);
        
        const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
        console.log('📡 Response received after ' + elapsed + 's, status:', res.status);
        
        if (res.ok) {
          // Force reload the image with cache-busting
          const newSrc = '/app-screenshots/' + preset + '/' + currentPiece + '.png?t=' + Date.now();
          console.log('🖼️ Setting new image src:', newSrc);
          img.src = newSrc;
          img.style.display = 'block';
          loading.style.display = 'none';
          showStatus('✅ ' + preset + ' regenerated in ' + elapsed + 's!', 'success');
        } else {
          const error = await res.text();
          console.error('❌ Regeneration failed:', res.status, error);
          loading.innerHTML = '❌ Failed: ' + (error || res.status);
          showStatus('❌ Failed to regenerate: ' + res.status, 'error');
        }
      } catch (err) {
        const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
        console.error('❌ Regeneration error after ' + elapsed + 's:', err);
        if (err.name === 'AbortError') {
          loading.innerHTML = '⏱️ Timeout - still processing?';
          showStatus('⏱️ Request timed out - try refreshing', 'error');
        } else {
          loading.innerHTML = '❌ ' + err.message;
          showStatus('❌ ' + err.message, 'error');
        }
      }
    }
    
    async function regenerateAll() {
      const presets = ${JSON.stringify(Object.keys(APP_SCREENSHOT_PRESETS))};
      showStatus('Regenerating all screenshots...');
      
      for (const preset of presets) {
        showStatus('Regenerating ' + preset + '...');
        try {
          await fetch('/app-screenshots/' + preset + '/' + currentPiece + '.png?force=true');
        } catch (err) {
          console.error('Failed:', preset, err);
        }
      }
      
      showStatus('✅ All screenshots regenerated! Reloading...', 'success');
      setTimeout(() => window.location.reload(), 1000);
    }
    
    function downloadZip() {
      showStatus('Preparing ZIP download...');
      window.location.href = '/app-screenshots/download/' + currentPiece;
    }
    
    // WebSocket for real-time progress updates
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    let ws = null;
    let reconnectAttempts = 0;
    
    function connectWebSocket() {
      ws = new WebSocket(protocol + '//' + location.host + '/ws');
      
      ws.onopen = () => {
        console.log('📡 WebSocket connected');
        reconnectAttempts = 0;
      };
      
      ws.onclose = () => {
        console.log('📡 WebSocket disconnected, reconnecting...');
        const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 10000);
        reconnectAttempts++;
        setTimeout(connectWebSocket, delay);
      };
      
      ws.onerror = () => ws.close();
      
      ws.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          
          // Check if there's active grab progress for our piece
          if (data.grabs && data.grabs.active) {
            const activeGrab = data.grabs.active.find(g => 
              g.piece === currentPiece || g.piece === '$' + currentPiece
            );
            
            if (activeGrab) {
              // Find which preset this matches (by dimensions)
              for (const [preset, config] of Object.entries(${JSON.stringify(APP_SCREENSHOT_PRESETS)})) {
                if (activeGrab.dimensions && 
                    activeGrab.dimensions.width === config.width && 
                    activeGrab.dimensions.height === config.height) {
                  updateProgressUI(preset, activeGrab.status, null);
                }
              }
            }
          }
        } catch (err) {
          console.error('WebSocket parse error:', err);
        }
      };
    }
    
    // Poll for detailed progress since grabs report to /grab-status
    async function pollProgress() {
      try {
        const res = await fetch('/grab-status');
        const data = await res.json();
        
        if (data.progress && data.progress.piece) {
          const piece = data.progress.piece;
          if (piece === currentPiece || piece === '$' + currentPiece) {
            // Check queue position for this piece
            let queuePosition = null;
            if (data.queue && data.queue.length > 0) {
              const queueItem = data.queue.find(q => q.piece === piece);
              if (queueItem) queuePosition = queueItem.position;
            }
            
            // Find matching preset by checking dimensions in active grabs
            if (data.active && data.active.length > 0) {
              const activeGrab = data.active.find(g => 
                g.piece === currentPiece || g.piece === '$' + currentPiece
              );
              if (activeGrab && activeGrab.dimensions) {
                for (const [preset, config] of Object.entries(${JSON.stringify(APP_SCREENSHOT_PRESETS)})) {
                  if (activeGrab.dimensions.width === config.width && 
                      activeGrab.dimensions.height === config.height) {
                    updateProgressUI(preset, data.progress.stage, data.progress.percent, data.progress.stageDetail, null, queuePosition);
                    break;
                  }
                }
              }
            }
            
            // Fallback: update all visible loading indicators with generic progress
            document.querySelectorAll('.loading[data-loading]').forEach(el => {
              if (el.style.display !== 'none') {
                const progressText = el.querySelector('.progress-text');
                const progressBar = el.querySelector('.progress-bar-fill');
                const previewImg = el.querySelector('.preview-img');
                
                if (progressText && data.progress.stageDetail) {
                  progressText.textContent = data.progress.stageDetail;
                }
                if (progressBar && data.progress.percent) {
                  progressBar.style.width = data.progress.percent + '%';
                }
                // Display streaming preview if available
                if (previewImg && data.progress.previewFrame) {
                  previewImg.src = 'data:image/jpeg;base64,' + data.progress.previewFrame;
                  previewImg.style.display = 'block';
                }
              }
            });
          }
        }
      } catch (err) {
        // Ignore polling errors
      }
    }
    
    function updateProgressUI(preset, stage, percent, detail, previewFrame, queuePosition) {
      const loading = document.querySelector('[data-loading="' + preset + '"]');
      if (!loading || loading.style.display === 'none') return;
      
      const progressText = loading.querySelector('.progress-text');
      const progressBar = loading.querySelector('.progress-bar-fill');
      const previewImg = loading.querySelector('.preview-img');
      
      // Map stage to friendly text
      const stageText = {
        'loading': '🚀 Loading piece...',
        'waiting-content': '⏳ Waiting for render...',
        'settling': '⏸️ Settling...',
        'capturing': '📸 Capturing...',
        'encoding': '🔄 Processing...',
        'uploading': '☁️ Uploading...',
        'queued': queuePosition ? '⏳ In queue (#' + queuePosition + ')...' : '⏳ In queue...',
      };
      
      if (progressText) {
        progressText.textContent = detail || stageText[stage] || stage || '';
      }
      if (progressBar && percent != null) {
        progressBar.style.width = percent + '%';
      }
      // Show streaming preview
      if (previewImg && previewFrame) {
        previewImg.src = 'data:image/jpeg;base64,' + previewFrame;
        previewImg.style.display = 'block';
      }
    }
    
    // Start WebSocket and polling
    connectWebSocket();
    const pollInterval = setInterval(pollProgress, 150); // Poll fast for smooth previews
    
    // Cleanup on page unload
    window.addEventListener('beforeunload', () => {
      clearInterval(pollInterval);
      if (ws) ws.close();
    });
  </script>
</body>
</html>`);
});

// Individual app screenshot endpoint
app.get('/app-screenshots/:preset/:piece.png', async (req, res) => {
  const { preset, piece } = req.params;
  const force = req.query.force === 'true';
  
  const presetConfig = APP_SCREENSHOT_PRESETS[preset];
  if (!presetConfig) {
    return res.status(400).json({ 
      error: 'Invalid preset', 
      valid: Object.keys(APP_SCREENSHOT_PRESETS) 
    });
  }
  
  const { width, height } = presetConfig;
  
  try {
    addServerLog('capture', '📱', `App screenshot: ${piece} (${preset} ${width}×${height}${force ? ' FORCE' : ''})`);
    
    const { cdnUrl, fromCache, buffer } = await getCachedOrGenerate(
      'app-screenshots', 
      `${piece}-${preset}`, 
      width, 
      height, 
      async () => {
        const result = await grabPiece(piece, {
          format: 'png',
          width,
          height,
          density: 4, // Pixel art - larger art pixels (4x)
          viewportScale: 1, // Capture at exact output size
          skipCache: force,
        });
        
        if (!result.success) throw new Error(result.error);
        
        // Handle cached result (cdnUrl but no buffer)
        if (result.cached && result.cdnUrl && !result.buffer) {
          const response = await fetch(result.cdnUrl);
          if (!response.ok) throw new Error(`Failed to fetch cached screenshot: ${response.status}`);
          return Buffer.from(await response.arrayBuffer());
        }
        
        return result.buffer;
      },
      'png',  // ext
      force   // skipCache - pass force flag to skip CDN cache
    );
    
    if (fromCache && cdnUrl && !force) {
      res.setHeader('X-Cache', 'HIT');
      res.setHeader('Cache-Control', 'public, max-age=604800'); // 7 days
      return res.redirect(302, cdnUrl);
    }
    
    res.setHeader('Content-Type', 'image/png');
    res.setHeader('Content-Length', buffer.length);
    // When force=true, prevent caching
    res.setHeader('Cache-Control', force ? 'no-store, no-cache, must-revalidate' : 'public, max-age=86400');
    res.setHeader('X-Cache', force ? 'REGENERATED' : 'MISS');
    res.setHeader('X-Screenshot-Preset', preset);
    res.setHeader('X-Screenshot-Dimensions', `${width}x${height}`);
    res.send(buffer);
    
  } catch (error) {
    console.error('App screenshot error:', error);
    addServerLog('error', '❌', `App screenshot failed: ${piece} ${preset} - ${error.message}`);
    res.status(500).json({ error: error.message });
  }
});

// Bulk ZIP download endpoint
app.get('/app-screenshots/download/:piece', async (req, res) => {
  const { piece } = req.params;
  const presets = Object.entries(APP_SCREENSHOT_PRESETS);
  
  addServerLog('info', '📦', `Generating ZIP for ${piece} (${presets.length} screenshots)`);
  
  res.setHeader('Content-Type', 'application/zip');
  res.setHeader('Content-Disposition', `attachment; filename="${piece}-app-screenshots.zip"`);
  
  const archive = archiver('zip', { zlib: { level: 9 } });
  archive.pipe(res);
  
  for (const [presetKey, preset] of presets) {
    try {
      const { cdnUrl, buffer } = await getCachedOrGenerate(
        'app-screenshots',
        `${piece}-${presetKey}`,
        preset.width,
        preset.height,
        async () => {
          const result = await grabPiece(piece, {
            format: 'png',
            width: preset.width,
            height: preset.height,
            density: 4, // Pixel art - larger art pixels (4x)
            viewportScale: 1, // Capture at exact output size
          });
          
          if (!result.success) throw new Error(result.error);
          
          if (result.cached && result.cdnUrl && !result.buffer) {
            const response = await fetch(result.cdnUrl);
            if (!response.ok) throw new Error(`Failed to fetch: ${response.status}`);
            return Buffer.from(await response.arrayBuffer());
          }
          
          return result.buffer;
        }
      );
      
      // Get buffer from CDN if we only have URL
      let imageBuffer = buffer;
      if (!imageBuffer && cdnUrl) {
        const response = await fetch(cdnUrl);
        if (response.ok) {
          imageBuffer = Buffer.from(await response.arrayBuffer());
        }
      }
      
      if (imageBuffer) {
        const filename = `${preset.category}/${piece}-${presetKey}.png`;
        archive.append(imageBuffer, { name: filename });
        addServerLog('success', '✅', `Added to ZIP: ${filename}`);
      }
    } catch (err) {
      console.error(`Failed to add ${presetKey} to ZIP:`, err);
      addServerLog('error', '❌', `ZIP: Failed ${presetKey} - ${err.message}`);
    }
  }
  
  archive.finalize();
});

// JSON API for app screenshots status
app.get('/api/app-screenshots/:piece', async (req, res) => {
  const { piece } = req.params;
  const screenshots = {};
  
  for (const [key, preset] of Object.entries(APP_SCREENSHOT_PRESETS)) {
    screenshots[key] = {
      ...preset,
      url: `/app-screenshots/${key}/${piece}.png`,
      downloadUrl: `/app-screenshots/${key}/${piece}.png?download=true`,
    };
  }
  
  res.json({
    piece,
    presets: screenshots,
    zipUrl: `/app-screenshots/download/${piece}`,
    dashboardUrl: `/app-screenshots?piece=${piece}`,
  });
});

// ─── Bundle HTML endpoint ───────────────────────────────────────────

app.get('/bundle-html', async (req, res) => {
  const code = req.query.code;
  const piece = req.query.piece;
  const format = req.query.format || 'html';
  const nocache = req.query.nocache === '1' || req.query.nocache === 'true';
  const nocompress = req.query.nocompress === '1' || req.query.nocompress === 'true';
  const nominify = req.query.nominify === '1' || req.query.nominify === 'true';
  const inline = req.query.inline === '1' || req.query.inline === 'true';
  const density = parseInt(req.query.density) || null;
  const mode = req.query.mode;

  // Device mode: simple iframe wrapper (fast path)
  if (mode === 'device') {
    const pieceCode = code || piece;
    if (!pieceCode) return res.status(400).send('Missing code or piece parameter');
    return res.set({ 'Content-Type': 'text/html; charset=utf-8', 'Cache-Control': 'public, max-age=60' }).send(generateDeviceHTML(pieceCode, density));
  }

  setSkipMinification(nominify);

  const isJSPiece = !!piece;
  const bundleTarget = piece || code;
  if (!bundleTarget) {
    return res.status(400).json({ error: "Missing 'code' or 'piece' parameter.", usage: { kidlisp: "/bundle-html?code=39j", javascript: "/bundle-html?piece=notepat" } });
  }

  // M4D mode: .amxd binary
  if (format === 'm4d') {
    try {
      const onProgress = (p) => console.log(`[bundler] m4d ${p.stage}: ${p.message}`);
      const { binary, filename } = await createM4DBundle(bundleTarget, isJSPiece, onProgress, density);
      res.set({ 'Content-Type': 'application/octet-stream', 'Content-Disposition': `attachment; filename="${filename}"`, 'Cache-Control': 'no-cache' });
      return res.send(binary);
    } catch (error) {
      console.error('M4D bundle failed:', error);
      return res.status(500).json({ error: error.message });
    }
  }

  // SSE streaming mode
  if (format === 'stream') {
    res.set({ 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-cache', 'Connection': 'keep-alive', 'X-Accel-Buffering': 'no' });
    res.flushHeaders();

    const sendEvent = (type, data) => {
      res.write(`event: ${type}\ndata: ${JSON.stringify(data)}\n\n`);
      if (typeof res.flush === 'function') res.flush();
    };

    try {
      const onProgress = (p) => sendEvent('progress', p);
      const { html, filename, sizeKB } = isJSPiece
        ? await createJSPieceBundle(bundleTarget, onProgress, nocompress, density)
        : await createBundle(bundleTarget, onProgress, nocompress, density);
      sendEvent('complete', { filename, content: Buffer.from(html).toString('base64'), sizeKB });
    } catch (error) {
      console.error('Bundle failed:', error);
      sendEvent('error', { error: error.message });
    }
    return res.end();
  }

  // Non-streaming modes (json, html download, inline)
  try {
    const progressLog = [];
    const onProgress = (p) => { progressLog.push(p.message); console.log(`[bundler] ${p.stage}: ${p.message}`); };
    const result = isJSPiece
      ? await createJSPieceBundle(bundleTarget, onProgress, nocompress, density)
      : await createBundle(bundleTarget, onProgress, nocompress, density);
    const { html, filename, sizeKB, mainSource, authorHandle, userCode, packDate, depCount } = result;

    if (format === 'json' || format === 'base64') {
      return res.json({ filename, content: Buffer.from(html).toString('base64'), sizeKB, progress: progressLog, sourceCode: mainSource, authorHandle, userCode, packDate, depCount });
    }

    const headers = { 'Content-Type': 'text/html; charset=utf-8', 'Cache-Control': 'public, max-age=3600' };
    if (!inline) headers['Content-Disposition'] = `attachment; filename="${filename}"`;
    return res.set(headers).send(html);
  } catch (error) {
    console.error('Bundle failed:', error);
    return res.status(500).json({ error: error.message });
  }
});

// Prewarm the core bundle cache (called by deploy.sh after restart)
app.post('/bundle-prewarm', async (req, res) => {
  try {
    addServerLog('info', '📦', 'Bundle prewarm started...');
    const result = await prewarmCache();
    addServerLog('success', '📦', `Bundle cache ready: ${result.fileCount} files in ${result.elapsed}ms (${result.commit})`);
    res.json(result);
  } catch (error) {
    addServerLog('error', '❌', `Bundle prewarm failed: ${error.message}`);
    res.status(500).json({ error: error.message });
  }
});

// Cache status
app.get('/bundle-status', (req, res) => {
  res.json(getCacheStatus());
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
    addServerLog('success', '🔥', `Oven server ready (v${GIT_VERSION.slice(0,8)})`);
    
    // Start background OG image regeneration after a short delay
    setTimeout(() => {
      addServerLog('info', '🖼️', 'Starting background OG regeneration...');
      regenerateOGImagesBackground().then(() => {
        addServerLog('success', '🖼️', 'OG images ready for social sharing');
      }).catch(err => {
        addServerLog('error', '❌', `OG regen failed: ${err.message}`);
      });
    }, 10000); // Wait 10s for server to fully initialize
    
    // Schedule periodic regeneration (every 6 hours)
    setInterval(() => {
      addServerLog('info', '🖼️', 'Scheduled OG regeneration starting...');
      regenerateOGImagesBackground().catch(err => {
        addServerLog('error', '❌', `Scheduled OG regen failed: ${err.message}`);
      });
    }, 6 * 60 * 60 * 1000); // 6 hours
  });
}

// WebSocket server
wss = new WebSocketServer({ server, path: '/ws' });

// Wire up grabber notifications to broadcast to all WebSocket clients
setNotifyCallback(() => {
  wss.clients.forEach((client) => {
    if (client.readyState === 1) { // OPEN
      client.send(JSON.stringify({
        version: GIT_VERSION,
        serverStartTime: SERVER_START_TIME,
        uptime: Date.now() - SERVER_START_TIME,
        incoming: Array.from(getIncomingBakes().values()),
        active: Array.from(getActiveBakes().values()),
        recent: getRecentBakes(),
        grabs: {
          active: getActiveGrabs(),
          recent: getRecentGrabs(),
          queue: getQueueStatus(),
          ipfsThumbs: getAllLatestIPFSUploads()
        },
        grabProgress: getAllProgress(),
        concurrency: getConcurrencyStatus(),
      }));
    }
  });
});

// Wire up grabber log messages to broadcast to clients
setLogCallback((type, icon, msg) => {
  addServerLog(type, icon, msg);
});

wss.on('connection', async (ws) => {
  console.log('📡 WebSocket client connected');
  addServerLog('info', '📡', 'Dashboard client connected');
  
  // Clean up stale bakes before sending initial state
  await cleanupStaleBakes();
  
  // Send initial state with recent logs
  ws.send(JSON.stringify({
    version: GIT_VERSION,
    serverStartTime: SERVER_START_TIME,
    uptime: Date.now() - SERVER_START_TIME,
    incoming: Array.from(getIncomingBakes().values()),
    active: Array.from(getActiveBakes().values()),
    recent: getRecentBakes(),
    grabs: {
      active: getActiveGrabs(),
      recent: getRecentGrabs(),
      queue: getQueueStatus(),
      ipfsThumbs: getAllLatestIPFSUploads()
    },
    grabProgress: getAllProgress(),
    concurrency: getConcurrencyStatus(),
    frozen: getFrozenPieces(),
    recentLogs: activityLogBuffer.slice(0, 50) // Send last 50 log entries
  }));
  
  // Subscribe to updates
  const unsubscribe = subscribeToUpdates((update) => {
    if (ws.readyState === 1) { // OPEN
      ws.send(JSON.stringify({
        version: GIT_VERSION,
        serverStartTime: SERVER_START_TIME,
        uptime: Date.now() - SERVER_START_TIME,
        incoming: Array.from(getIncomingBakes().values()),
        active: Array.from(getActiveBakes().values()),
        recent: getRecentBakes(),
        grabs: {
          active: getActiveGrabs(),
          recent: getRecentGrabs(),
          queue: getQueueStatus(),
          ipfsThumbs: getAllLatestIPFSUploads()
        },
        grabProgress: getAllProgress(),
        concurrency: getConcurrencyStatus(),
        frozen: getFrozenPieces()
      }));
    }
  });
  
  ws.on('close', () => {
    console.log('📡 WebSocket client disconnected');
    unsubscribe();
  });
});

// Graceful shutdown handling
async function shutdown(signal) {
  console.log(`\n🛑 Received ${signal}, shutting down gracefully...`);
  
  // Close WebSocket connections
  wss.clients.forEach(ws => ws.close());
  
  // Close HTTP server
  server.close(() => {
    console.log('✅ HTTP server closed');
  });
  
  // Close browser if open
  try {
    const { closeBrowser } = await import('./grabber.mjs');
    await closeBrowser?.();
    console.log('✅ Browser closed');
  } catch (e) {
    // Browser close is optional
  }
  
  // Exit after a short delay
  setTimeout(() => {
    console.log('👋 Goodbye!');
    process.exit(0);
  }, 500);
}

process.on('SIGTERM', () => shutdown('SIGTERM'));
process.on('SIGINT', () => shutdown('SIGINT'));