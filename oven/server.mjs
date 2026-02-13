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
import { grabHandler, grabGetHandler, grabIPFSHandler, grabPiece, getCachedOrGenerate, getActiveGrabs, getRecentGrabs, getLatestKeepThumbnail, getLatestIPFSUpload, getAllLatestIPFSUploads, setNotifyCallback, setLogCallback, cleanupStaleGrabs, clearAllActiveGrabs, getQueueStatus, getCurrentProgress, IPFS_GATEWAY, generateKidlispOGImage, getOGImageCacheStatus, getFrozenPieces, clearFrozenPiece, getLatestOGImageUrl, regenerateOGImagesBackground, generateKidlispBackdrop, getLatestBackdropUrl, APP_SCREENSHOT_PRESETS, generateNotepatOGImage, getLatestNotepatOGUrl } from './grabber.mjs';
import archiver from 'archiver';

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
    
    /* Activity Log */
    .activity-panel {
      position: fixed;
      bottom: 0;
      left: 0;
      right: 0;
      background: #0a0a0a;
      border-top: 2px solid #333;
      max-height: 200px;
      overflow: hidden;
      transition: max-height 0.3s ease;
      z-index: 99;
    }
    
    .activity-panel.collapsed {
      max-height: 32px;
    }
    
    .activity-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 0.4em 1em;
      background: #111;
      border-bottom: 1px solid #333;
      cursor: pointer;
      user-select: none;
    }
    
    .activity-header:hover {
      background: #1a1a1a;
    }
    
    .activity-title {
      display: flex;
      align-items: center;
      gap: 0.5em;
    }
    
    .activity-title .pulse {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background: #4f4;
      animation: pulse 2s infinite;
    }
    
    .activity-stats {
      display: flex;
      gap: 1em;
      font-size: 0.75em;
      opacity: 0.7;
    }
    
    .activity-stats span {
      display: flex;
      align-items: center;
      gap: 0.3em;
    }
    
    .activity-controls {
      display: flex;
      gap: 0.5em;
      align-items: center;
    }
    
    .activity-controls button {
      background: #222;
      border: 1px solid #333;
      color: #888;
      padding: 0.2em 0.5em;
      font-size: 0.75em;
      cursor: pointer;
      border-radius: 3px;
    }
    
    .activity-controls button:hover {
      background: #333;
      color: #fff;
    }
    
    .activity-log {
      height: 168px;
      overflow-y: auto;
      font-size: 0.75em;
      padding: 0.5em;
    }
    
    .activity-log::-webkit-scrollbar {
      width: 6px;
    }
    
    .activity-log::-webkit-scrollbar-thumb {
      background: #333;
      border-radius: 3px;
    }
    
    .log-entry {
      padding: 0.2em 0.4em;
      border-radius: 2px;
      margin-bottom: 0.2em;
      display: flex;
      gap: 0.5em;
      align-items: flex-start;
    }
    
    .log-entry:hover {
      background: #1a1a1a;
    }
    
    .log-time {
      color: #666;
      flex-shrink: 0;
      font-family: monospace;
    }
    
    .log-icon {
      flex-shrink: 0;
    }
    
    .log-msg {
      flex: 1;
      word-break: break-word;
    }
    
    .log-entry.info .log-msg { color: #aaa; }
    .log-entry.success .log-msg { color: #4f4; }
    .log-entry.warning .log-msg { color: #fa0; }
    .log-entry.error .log-msg { color: #f44; }
    .log-entry.capture .log-msg { color: #6bf; }
    .log-entry.queue .log-msg { color: #b6f; }
    
    /* Add padding to bakes grid when activity panel is open */
    .bakes {
      padding-bottom: 220px;
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
    
    /* Frozen Pieces Panel */
    .frozen-panel {
      background: #1a0f0f;
      border: 1px solid #3a1a1a;
      border-radius: 6px;
      margin: 0.5em 1em;
    }
    
    .frozen-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 0.5em 0.8em;
      cursor: pointer;
      border-bottom: 1px solid #3a1a1a;
    }
    
    .frozen-header:hover { background: #2a1515; }
    
    .frozen-title {
      display: flex;
      align-items: center;
      gap: 0.8em;
    }
    
    .frozen-count {
      background: #f44;
      color: #000;
      font-weight: bold;
      padding: 0.1em 0.5em;
      border-radius: 10px;
      font-size: 0.85em;
    }
    
    .frozen-list {
      max-height: 300px;
      overflow-y: auto;
      padding: 0.5em;
    }
    
    .frozen-item {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 0.4em 0.6em;
      background: #200a0a;
      border-radius: 4px;
      margin-bottom: 0.3em;
      font-size: 0.9em;
      gap: 0.8em;
    }
    
    .frozen-item:hover { background: #2a1212; }
    
    .frozen-preview {
      width: 48px;
      height: 48px;
      object-fit: cover;
      border-radius: 4px;
      border: 1px solid #3a1a1a;
      flex-shrink: 0;
    }
    
    .frozen-no-preview {
      width: 48px;
      height: 48px;
      display: flex;
      align-items: center;
      justify-content: center;
      background: #1a0808;
      border-radius: 4px;
      border: 1px solid #3a1a1a;
      flex-shrink: 0;
      font-size: 1.5em;
      opacity: 0.5;
    }
    
    .frozen-item-name {
      color: #f88;
      font-family: monospace;
      flex: 1;
      min-width: 0;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    
    .frozen-item-info {
      color: #888;
      font-size: 0.8em;
      display: flex;
      gap: 0.8em;
      align-items: center;
    }
    
    .frozen-item-actions {
      display: flex;
      gap: 0.5em;
    }
    
    .frozen-item-actions a, .frozen-item-actions button {
      color: #888;
      text-decoration: none;
      font-size: 0.85em;
      background: none;
      border: 1px solid #444;
      padding: 0.2em 0.5em;
      border-radius: 3px;
      cursor: pointer;
    }
    
    .frozen-item-actions a:hover, .frozen-item-actions button:hover {
      color: #fff;
      border-color: #666;
    }
    
    .frozen-item-actions .clear-btn { color: #4f4; border-color: #4f4; }
    .frozen-item-actions .clear-btn:hover { background: #4f4; color: #000; }
    
    ${PROGRESS_UI_CSS}
  </style>
</head>
<body>
  <header>
    <a href="/" class="logo">🔥 oven</a>
    <nav style="display:flex;gap:1em;font-size:0.85em;">
      <a href="/app-screenshots?piece=prompt" style="color:#88f;text-decoration:none;">📱 App Screenshots</a>
      <a href="/og-preview" style="color:#88f;text-decoration:none;">🖼️ OG Images</a>
    </nav>
    <div class="status">
      <div class="status-dot" id="ws-dot"></div>
      <span id="ws-text">Connecting...</span>
      <span id="uptime" style="opacity:0.5"></span>
      <span class="version" id="version"></span>
    </div>
  </header>
  
  <div id="reboot-banner" style="display:none;background:rgba(255,200,0,0.15);color:#fa0;padding:0.4em 1em;text-align:center;font-size:0.85em;border-bottom:1px solid #333;">
    ⚡ Server recently rebooted · queue restored from database
  </div>
  
  <div class="manual-capture" style="padding:0.8em 1em;background:#0a0a0a;border-bottom:1px solid #222;">
    <form id="capture-form" style="display:flex;gap:0.5em;align-items:center;flex-wrap:wrap;">
      <input type="text" id="capture-piece" placeholder="piece (e.g. $roz or wiggle)" 
        style="flex:1;min-width:150px;padding:0.4em 0.6em;background:#111;border:1px solid #333;color:#fff;font-family:monospace;border-radius:3px;">
      <select id="capture-format" style="padding:0.4em;background:#111;border:1px solid #333;color:#fff;font-family:monospace;border-radius:3px;">
        <option value="png">PNG (still)</option>
        <option value="webp" selected>WebP (animated)</option>
        <option value="gif">GIF (animated)</option>
      </select>
      <select id="capture-resolution" style="padding:0.4em;background:#111;border:1px solid #333;color:#fff;font-family:monospace;border-radius:3px;">
        <option value="128">128×128</option>
        <option value="256">256×256</option>
        <option value="512" selected>512×512</option>
        <option value="768">768×768</option>
        <option value="1024">1024×1024</option>
        <option value="custom">Custom...</option>
      </select>
      <input type="number" id="capture-width" value="512" min="64" max="2048" step="1"
        style="display:none;width:60px;padding:0.4em;background:#111;border:1px solid #333;color:#fff;font-family:monospace;border-radius:3px;text-align:center;"
        title="Width">
      <span id="custom-x" style="display:none;opacity:0.5;">×</span>
      <input type="number" id="capture-height" value="512" min="64" max="2048" step="1"
        style="display:none;width:60px;padding:0.4em;background:#111;border:1px solid #333;color:#fff;font-family:monospace;border-radius:3px;text-align:center;"
        title="Height">
      <input type="number" id="capture-duration" value="12" min="1" max="12" step="1"
        style="width:50px;padding:0.4em;background:#111;border:1px solid #333;color:#fff;font-family:monospace;border-radius:3px;text-align:center;"
        title="Duration in seconds (max 12)">
      <span style="opacity:0.5;font-size:0.85em;">sec</span>
      <button type="submit" style="padding:0.4em 1em;background:#fa0;color:#000;border:none;font-family:monospace;font-weight:bold;border-radius:3px;cursor:pointer;">
        🔥 Bake
      </button>
      <a href="https://aesthetic.computer/prompt" style="margin-left:auto;color:#666;text-decoration:none;font-size:0.85em;" title="Back to prompt">← prompt</a>
    </form>
    <div id="capture-status" style="display:none;margin-top:0.5em;font-size:0.85em;"></div>
    <div id="capture-preview" style="display:none;margin-top:0.5em;position:relative;height:100px;background:#0a0a0a;border-radius:4px;overflow:hidden;">
      <div class="oven-loading" style="position:relative;height:100%;">
        <img class="preview-img" alt="preview">
        <span class="loading-text">🔥 Loading...</span>
        <div class="progress-text"></div>
        <div class="progress-bar"><div class="progress-bar-fill"></div></div>
      </div>
    </div>
  </div>
  
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
  
  <!-- Frozen Pieces Panel -->
  <div class="frozen-panel" id="frozen-panel" style="display:none;">
    <div class="frozen-header" onclick="toggleFrozenPanel()">
      <div class="frozen-title">
        <span>🥶 Frozen Pieces</span>
        <span class="frozen-count" id="frozen-count">0</span>
      </div>
      <span id="frozen-toggle">▶</span>
    </div>
    <div class="frozen-list" id="frozen-list" style="display:none;"></div>
  </div>
  
  <div class="bakes" id="bakes"></div>
  
  <!-- Activity Log Panel -->
  <div class="activity-panel" id="activity-panel">
    <div class="activity-header" onclick="toggleActivityPanel()">
      <div class="activity-title">
        <div class="pulse" id="activity-pulse"></div>
        <span>📜 Activity</span>
        <div class="activity-stats">
          <span id="log-count">0 events</span>
          <span id="log-errors" style="color:#f44;display:none;">0 errors</span>
          <span id="log-success" style="color:#4f4;display:none;">0 success</span>
        </div>
      </div>
      <div class="activity-controls">
        <button onclick="event.stopPropagation();clearActivityLog();" title="Clear log">🗑️</button>
        <span id="activity-toggle">▼</span>
      </div>
    </div>
    <div class="activity-log" id="activity-log"></div>
  </div>
  
  <script>
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    let ws, reconnectAttempts = 0, serverVersion = null;
    let allBakes = []; // Store all bakes for filtering
    let activityLog = []; // Store activity entries
    const MAX_LOG_ENTRIES = 200;
    let activityCollapsed = false;
    
    // HTML escape helper
    function escapeHtml(str) {
      if (!str) return '';
      return String(str)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#039;');
    }
    
    ${PROGRESS_UI_JS}
    
    // Activity log functions
    function addLogEntry(type, icon, msg) {
      const entry = {
        time: new Date(),
        type,
        icon,
        msg
      };
      activityLog.unshift(entry);
      if (activityLog.length > MAX_LOG_ENTRIES) {
        activityLog.pop();
      }
      renderActivityLog();
      
      // Pulse the indicator
      const pulse = document.getElementById('activity-pulse');
      pulse.style.background = type === 'error' ? '#f44' : type === 'success' ? '#4f4' : '#6bf';
      setTimeout(() => { pulse.style.background = '#4f4'; }, 500);
    }
    
    function renderActivityLog() {
      const container = document.getElementById('activity-log');
      const countEl = document.getElementById('log-count');
      const errorsEl = document.getElementById('log-errors');
      const successEl = document.getElementById('log-success');
      
      const errorCount = activityLog.filter(e => e.type === 'error').length;
      const successCount = activityLog.filter(e => e.type === 'success').length;
      
      countEl.textContent = activityLog.length + ' events';
      
      if (errorCount > 0) {
        errorsEl.textContent = errorCount + ' errors';
        errorsEl.style.display = 'inline';
      } else {
        errorsEl.style.display = 'none';
      }
      
      if (successCount > 0) {
        successEl.textContent = successCount + ' success';
        successEl.style.display = 'inline';
      } else {
        successEl.style.display = 'none';
      }
      
      container.innerHTML = activityLog.map(e => {
        const time = e.time.toLocaleTimeString('en-US', { hour12: false });
        return '<div class="log-entry ' + e.type + '">' +
          '<span class="log-time">' + time + '</span>' +
          '<span class="log-icon">' + e.icon + '</span>' +
          '<span class="log-msg">' + e.msg + '</span>' +
        '</div>';
      }).join('');
    }
    
    function clearActivityLog() {
      activityLog = [];
      renderActivityLog();
    }
    
    function toggleActivityPanel() {
      const panel = document.getElementById('activity-panel');
      const toggle = document.getElementById('activity-toggle');
      activityCollapsed = !activityCollapsed;
      panel.classList.toggle('collapsed', activityCollapsed);
      toggle.textContent = activityCollapsed ? '▲' : '▼';
    }
    
    // Frozen pieces panel
    let frozenCollapsed = true;
    let frozenPieces = [];
    
    function toggleFrozenPanel() {
      frozenCollapsed = !frozenCollapsed;
      const list = document.getElementById('frozen-list');
      const toggle = document.getElementById('frozen-toggle');
      list.style.display = frozenCollapsed ? 'none' : 'block';
      toggle.textContent = frozenCollapsed ? '▶' : '▼';
    }
    
    function updateFrozenPanel(pieces) {
      frozenPieces = pieces || [];
      const panel = document.getElementById('frozen-panel');
      const countEl = document.getElementById('frozen-count');
      const listEl = document.getElementById('frozen-list');
      
      if (frozenPieces.length === 0) {
        panel.style.display = 'none';
        return;
      }
      
      panel.style.display = 'block';
      countEl.textContent = frozenPieces.length;
      
      listEl.innerHTML = frozenPieces.map(p => {
        const timeSince = formatTimeAgo(p.lastAttempt);
        const previewHtml = p.previewUrl 
          ? '<img class="frozen-preview" src="' + p.previewUrl + '?t=' + Date.now() + '" alt="frozen preview">' 
          : '<span class="frozen-no-preview">🖼️</span>';
        return '<div class="frozen-item">' +
          previewHtml +
          '<span class="frozen-item-name">' + escapeHtml(p.piece) + '</span>' +
          '<span class="frozen-item-info">' +
            '<span>' + p.attempts + ' attempt' + (p.attempts > 1 ? 's' : '') + '</span>' +
            '<span>' + timeSince + '</span>' +
          '</span>' +
          '<span class="frozen-item-actions">' +
            '<a href="https://aesthetic.computer/' + encodeURIComponent(p.piece) + '" target="_blank" title="View piece">👁️</a>' +
            '<button class="clear-btn" onclick="clearFrozenPiece(\\''+escapeHtml(p.piece)+'\\');" title="Clear from list">✓</button>' +
          '</span>' +
        '</div>';
      }).join('');
    }
    
    function formatTimeAgo(timestamp) {
      const diff = Date.now() - timestamp;
      const mins = Math.floor(diff / 60000);
      const hours = Math.floor(mins / 60);
      const days = Math.floor(hours / 24);
      if (days > 0) return days + 'd ago';
      if (hours > 0) return hours + 'h ago';
      if (mins > 0) return mins + 'm ago';
      return 'just now';
    }
    
    async function clearFrozenPiece(piece) {
      try {
        const res = await fetch('/api/frozen/' + encodeURIComponent(piece), { method: 'DELETE' });
        if (res.ok) {
          addLogEntry('success', '✅', 'Cleared frozen piece: ' + piece);
          // Refresh the frozen list
          const dataRes = await fetch('/api/frozen');
          const data = await dataRes.json();
          updateFrozenPanel(data.frozen);
        } else {
          addLogEntry('error', '❌', 'Failed to clear frozen piece: ' + piece);
        }
      } catch (err) {
        addLogEntry('error', '❌', 'Error clearing frozen piece: ' + err.message);
      }
    }
    
    // Log initial connection
    addLogEntry('info', '🔌', 'Dashboard loaded, connecting to server...');
    
    // Resolution selector handler
    document.getElementById('capture-resolution').addEventListener('change', (e) => {
      const widthInput = document.getElementById('capture-width');
      const heightInput = document.getElementById('capture-height');
      const customX = document.getElementById('custom-x');
      
      if (e.target.value === 'custom') {
        widthInput.style.display = 'block';
        heightInput.style.display = 'block';
        customX.style.display = 'inline';
      } else {
        widthInput.style.display = 'none';
        heightInput.style.display = 'none';
        customX.style.display = 'none';
        const size = parseInt(e.target.value);
        widthInput.value = size;
        heightInput.value = size;
      }
    });
    
    // Manual capture form handler
    let captureInProgress = false;
    const captureForm = document.getElementById('capture-form');
    const captureBtn = captureForm.querySelector('button[type="submit"]');
    
    captureForm.addEventListener('submit', async (e) => {
      e.preventDefault();
      
      // Prevent multiple submissions
      if (captureInProgress) return;
      
      const piece = document.getElementById('capture-piece').value.trim();
      const format = document.getElementById('capture-format').value;
      const duration = parseInt(document.getElementById('capture-duration').value) || 12;
      const resolution = document.getElementById('capture-resolution').value;
      const width = resolution === 'custom' 
        ? parseInt(document.getElementById('capture-width').value) || 512
        : parseInt(resolution);
      const height = resolution === 'custom'
        ? parseInt(document.getElementById('capture-height').value) || 512
        : parseInt(resolution);
      const statusEl = document.getElementById('capture-status');
      
      if (!piece) {
        statusEl.style.display = 'block';
        statusEl.style.color = '#f44';
        statusEl.textContent = '❌ Please enter a piece name';
        return;
      }
      
      // Lock the form
      captureInProgress = true;
      captureBtn.disabled = true;
      captureBtn.textContent = '⏳ Capturing...';
      captureBtn.style.opacity = '0.5';
      
      // Show progress preview
      const previewEl = document.getElementById('capture-preview');
      const loadingEl = previewEl.querySelector('.oven-loading');
      previewEl.style.display = 'block';
      statusEl.style.display = 'none';
      
      // Start polling for progress updates
      const pollInterval = setInterval(async () => {
        try {
          const res = await fetch('/grab-status');
          const data = await res.json();
          if (data.progress && data.progress.piece) {
            // Check if this is our capture
            if (data.progress.piece === piece || data.progress.piece === '$' + piece) {
              updateOvenLoadingUI(loadingEl, data.progress, data.queue);
            }
          }
        } catch (e) {}
      }, 150);
      
      addLogEntry('capture', '📸', 'Manual capture started: ' + piece + ' (' + width + '×' + height + ' ' + format + ')');
      
      try {
        const url = '/grab/' + format + '/' + width + '/' + height + '/' + encodeURIComponent(piece) + 
          '?duration=' + (duration * 1000);
        
        const response = await fetch(url);
        clearInterval(pollInterval);
        
        if (response.ok) {
          statusEl.style.display = 'block';
          statusEl.style.color = '#4f4';
          statusEl.textContent = '✅ Capture complete! Check the grid below.';
          previewEl.style.display = 'none';
          addLogEntry('success', '✅', 'Capture complete: ' + piece);
          setTimeout(() => { statusEl.style.display = 'none'; }, 3000);
        } else {
          const err = await response.json();
          statusEl.style.display = 'block';
          statusEl.style.color = '#f44';
          statusEl.textContent = '❌ ' + (err.error || 'Capture failed');
          previewEl.style.display = 'none';
          addLogEntry('error', '❌', 'Capture failed: ' + piece + ' - ' + (err.error || 'Unknown error'));
        }
      } catch (err) {
        clearInterval(pollInterval);
        statusEl.style.display = 'block';
        statusEl.style.color = '#f44';
        statusEl.textContent = '❌ ' + err.message;
        previewEl.style.display = 'none';
        addLogEntry('error', '❌', 'Capture error: ' + err.message);
      } finally {
        // Unlock the form
        captureInProgress = false;
        captureBtn.disabled = false;
        captureBtn.textContent = '📸 Capture';
        captureBtn.style.opacity = '1';
      }
    });
    
    // Update duration field visibility based on format
    document.getElementById('capture-format').addEventListener('change', (e) => {
      const durationInput = document.getElementById('capture-duration');
      const secLabel = durationInput.nextElementSibling;
      if (e.target.value === 'png') {
        durationInput.style.opacity = '0.3';
        secLabel.style.opacity = '0.3';
        durationInput.title = 'Duration not used for stills';
      } else {
        durationInput.style.opacity = '1';
        secLabel.style.opacity = '0.5';
        durationInput.title = 'Duration in seconds';
      }
    });
    
    function connect() {
      ws = new WebSocket(protocol + '//' + location.host + '/ws');
      
      ws.onopen = () => {
        document.getElementById('ws-dot').className = 'status-dot connected';
        document.getElementById('ws-text').textContent = 'Connected';
        reconnectAttempts = 0;
        addLogEntry('success', '✅', 'WebSocket connected to server');
      };
      
      ws.onclose = () => {
        document.getElementById('ws-dot').className = 'status-dot disconnected';
        document.getElementById('ws-text').textContent = 'Reconnecting...';
        addLogEntry('warning', '⚠️', 'WebSocket disconnected, reconnecting...');
        const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
        reconnectAttempts++;
        setTimeout(connect, delay);
      };
      
      ws.onerror = () => {
        addLogEntry('error', '❌', 'WebSocket error');
        ws.close();
      };
      
      ws.onmessage = (event) => {
        const data = JSON.parse(event.data);
        
        // Handle streaming log entries from server
        if (data.logEntry) {
          const e = data.logEntry;
          const entry = {
            time: new Date(e.time),
            type: e.type,
            icon: e.icon,
            msg: e.msg
          };
          activityLog.unshift(entry);
          if (activityLog.length > MAX_LOG_ENTRIES) activityLog.pop();
          renderActivityLog();
          // Pulse indicator
          const pulse = document.getElementById('activity-pulse');
          pulse.style.background = e.type === 'error' ? '#f44' : e.type === 'success' ? '#4f4' : '#6bf';
          setTimeout(() => { pulse.style.background = '#4f4'; }, 500);
          return;
        }
        
        // Handle initial log history
        if (data.recentLogs) {
          data.recentLogs.forEach(e => {
            activityLog.push({
              time: new Date(e.time),
              type: e.type,
              icon: e.icon,
              msg: e.msg
            });
          });
          if (activityLog.length > MAX_LOG_ENTRIES) {
            activityLog = activityLog.slice(0, MAX_LOG_ENTRIES);
          }
          renderActivityLog();
        }
        
        if (data.version && serverVersion && data.version !== serverVersion) {
          location.reload();
          return;
        }
        serverVersion = data.version;
        if (data.version) document.getElementById('version').textContent = 'v' + data.version;
        
        // Update uptime display
        if (data.uptime) {
          document.getElementById('uptime').textContent = '⏱️ ' + formatUptime(data.uptime);
          // Show reboot banner if server started less than 2 minutes ago
          const rebootBanner = document.getElementById('reboot-banner');
          if (data.uptime < 120000) {
            rebootBanner.style.display = 'block';
          } else {
            rebootBanner.style.display = 'none';
          }
        }
        
        // Update frozen pieces panel
        if (data.frozen) {
          updateFrozenPanel(data.frozen);
        }
        
        render(data);
      };
    }
    connect();
    
    function formatUptime(ms) {
      const s = Math.floor(ms / 1000);
      if (s < 60) return s + 's';
      const m = Math.floor(s / 60);
      if (m < 60) return m + 'm ' + (s % 60) + 's';
      const h = Math.floor(m / 60);
      if (h < 24) return h + 'h ' + (m % 60) + 'm';
      const d = Math.floor(h / 24);
      return d + 'd ' + (h % 24) + 'h';
    }
    
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
        // Priority: IPFS > Spaces CDN > fallback icon
        const previewUrl = ipfsCid 
          ? IPFS_GATEWAY + '/ipfs/' + ipfsCid 
          : g.cdnUrl || ('https://oven.aesthetic.computer/icon/128x128/' + g.piece + '.png');
        
        // Build links array
        const links = [];
        // Source indicator (keep, manual, etc)
        if (g.source === 'keep') {
          if (g.keepId) {
            links.push({ label: '🎫 Keep #' + g.keepId, url: 'https://objkt.com/tokens/KT1WRvHWcF6rVjNGEVrQu86cKvTCRPapSaHG/' + g.keepId });
          } else {
            links.push({ label: '🎫 Keep', url: 'https://objkt.com/collection/KT1WRvHWcF6rVjNGEVrQu86cKvTCRPapSaHG' });
          }
        } else if (g.source === 'manual') {
          links.push({ label: '✋ Manual', url: null });
        }
        if (ipfsCid) {
          links.push({ label: '📌 IPFS', url: 'https://ipfs.aesthetic.computer/ipfs/' + ipfsCid });
        }
        if (g.cdnUrl) {
          links.push({ label: '☁️ CDN', url: g.cdnUrl });
        }
        // Show git version for dedup tracking
        if (g.gitVersion) {
          links.push({ label: '🏷️ ' + g.gitVersion.slice(0, 7), url: 'https://github.com/whistlegraph/aesthetic-computer/commit/' + g.gitVersion });
        }
        
        allBakes.push({
          type: 'grab', status: g.status === 'failed' ? 'error' : 'complete', id: g.id,
          title: g.piece, url: 'https://aesthetic.computer/' + g.piece,
          preview: previewUrl,
          size: g.size, format: g.format, error: g.error,
          completedAt: g.completedAt,
          dimensions: g.dimensions,
          captureKey: g.captureKey,
          source: g.source,
          keepId: g.keepId,
          links
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
        if (bake.dimensions) html += '<span>' + bake.dimensions.width + '×' + bake.dimensions.height + '</span>';
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
    progress: getCurrentProgress()
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
    const fallbackUrl = `https://art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/og/kidlisp/${yesterday}-${layout}.png`;
    
    res.setHeader('Cache-Control', 'public, max-age=300'); // Short cache for fallback
    return res.redirect(302, fallbackUrl);
    
  } catch (error) {
    console.error('KidLisp OG PNG error:', error);
    // Ultimate fallback - yesterday's mosaic
    const yesterday = new Date(Date.now() - 86400000).toISOString().split('T')[0];
    return res.redirect(302, `https://art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/og/kidlisp/${yesterday}-mosaic.png`);
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
          ipfsThumbs: getAllLatestIPFSUploads()
        }
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
      ipfsThumbs: getAllLatestIPFSUploads()
    },
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
          ipfsThumbs: getAllLatestIPFSUploads()
        },
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