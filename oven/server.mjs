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
import { grabHandler, grabGetHandler, grabIPFSHandler, grabPiece, getCachedOrGenerate, getActiveGrabs, getRecentGrabs, getLatestKeepThumbnail, getLatestIPFSUpload, getAllLatestIPFSUploads, setNotifyCallback, setLogCallback, cleanupStaleGrabs, clearAllActiveGrabs, getQueueStatus, getCurrentProgress, IPFS_GATEWAY, generateKidlispOGImage, getOGImageCacheStatus } from './grabber.mjs';

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
  </style>
</head>
<body>
  <header>
    <a href="/" class="logo">🔥 oven</a>
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
      
      statusEl.style.display = 'block';
      statusEl.style.color = '#fa0';
      statusEl.textContent = '🔥 Starting capture...';
      addLogEntry('capture', '📸', 'Manual capture started: ' + piece + ' (' + width + '×' + height + ' ' + format + ')');
      
      try {
        const url = '/grab/' + format + '/' + width + '/' + height + '/' + encodeURIComponent(piece) + 
          '?duration=' + (duration * 1000);
        
        statusEl.textContent = '📸 Capturing ' + piece + ' (' + width + '×' + height + ' ' + format + ', ' + duration + 's)...';
        
        const response = await fetch(url);
        if (response.ok) {
          statusEl.style.color = '#4f4';
          statusEl.textContent = '✅ Capture complete! Check the grid below.';
          addLogEntry('success', '✅', 'Capture complete: ' + piece);
          setTimeout(() => { statusEl.style.display = 'none'; }, 3000);
        } else {
          const err = await response.json();
          statusEl.style.color = '#f44';
          statusEl.textContent = '❌ ' + (err.error || 'Capture failed');
          addLogEntry('error', '❌', 'Capture failed: ' + piece + ' - ' + (err.error || 'Unknown error'));
        }
      } catch (err) {
        statusEl.style.color = '#f44';
        statusEl.textContent = '❌ ' + err.message;
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
      default: '/kidlisp-og',
      withLayout: '/kidlisp-og?layout=mosaic',
      forceRegenerate: '/kidlisp-og?force=true',
    }
  });
});

// Preview all OG layouts (for testing/comparison)
app.get('/kidlisp-og/preview', (req, res) => {
  const layouts = ['featured', 'mosaic', 'filmstrip', 'code-split'];
  const baseUrl = req.protocol + '://' + req.get('host');
  
  res.setHeader('Content-Type', 'text/html');
  res.send(`<!DOCTYPE html>
<html>
<head>
  <title>KidLisp OG Preview</title>
  <style>
    body { font-family: monospace; background: #1a1a2e; color: white; padding: 20px; }
    h1 { color: #88ff88; }
    .layout { margin: 20px 0; }
    .layout h2 { color: #ffaa00; }
    .layout img { max-width: 100%; border: 2px solid #444; }
    .layout a { color: #88ccff; }
  </style>
</head>
<body>
  <h1>🖼️ KidLisp.com OG Preview Layouts</h1>
  <p>These are the available OG image layouts for social previews.</p>
  ${layouts.map(layout => `
    <div class="layout">
      <h2>${layout.charAt(0).toUpperCase() + layout.slice(1)}</h2>
      <p><a href="${baseUrl}/kidlisp-og?layout=${layout}&force=true">Force regenerate</a></p>
      <img src="${baseUrl}/kidlisp-og?layout=${layout}" alt="${layout} layout" loading="lazy">
    </div>
  `).join('')}
  <p style="margin-top: 40px;">
    <a href="${baseUrl}/kidlisp-og/status">View cache status</a>
  </p>
</body>
</html>`);
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
        }
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