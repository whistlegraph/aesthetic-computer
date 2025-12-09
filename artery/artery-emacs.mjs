#!/usr/bin/env node
/**
 * 🎨 Artery Emacs Bridge
 * 
 * Connects artery services to Emacs via JSON lines on stdin/stdout.
 * Designed to be spawned as a subprocess from artery.el
 * 
 * Protocol:
 *   Input (from Emacs):
 *     {"type": "eval", "code": "..."}     - Eval JS in DAW
 *     {"type": "connect-daw"}              - Connect to DAW CDP
 *     {"type": "disconnect-daw"}           - Disconnect from DAW
 *     {"type": "get-state"}                - Get current state
 * 
 *   Output (to Emacs):
 *     {"type": "daw-state", "bpm": 120, "playing": true}
 *     {"type": "daw-log", "timestamp": "...", "logType": "info", "message": "..."}
 *     {"type": "daw-connected", "title": "...", "url": "..."}
 *     {"type": "daw-disconnected"}
 *     {"type": "server-status", "status": "running|error|unknown"}
 *     {"type": "error", "message": "..."}
 */

import * as readline from 'readline';
import DAWDebugger from './daw-debug.mjs';
import http from 'http';
import https from 'https';

// State
let dawDebugger = null;
let dawStateInterval = null;
let serverCheckInterval = null;

// Send JSON message to Emacs
function emit(msg) {
  console.log(JSON.stringify(msg));
}

// Check AC server status
async function checkServerStatus() {
  const serverUrl = process.env.AC_SERVER_URL || 'https://localhost:8888';
  
  return new Promise((resolve) => {
    const protocol = serverUrl.startsWith('https') ? https : http;
    const req = protocol.get(serverUrl, {
      timeout: 3000,
      rejectUnauthorized: false // Allow self-signed certs in dev
    }, (res) => {
      resolve(res.statusCode < 400 ? 'running' : 'error');
    });
    
    req.on('error', () => resolve('error'));
    req.on('timeout', () => {
      req.destroy();
      resolve('error');
    });
  });
}

// Connect to DAW
async function connectDAW() {
  if (dawDebugger && dawDebugger.connected) {
    emit({ type: 'error', message: 'Already connected to DAW' });
    return;
  }
  
  dawDebugger = new DAWDebugger({
    json: true,
    onLog: (entry) => {
      emit({
        type: 'daw-log',
        timestamp: new Date(entry.timestamp).toISOString(),
        logType: entry.type,
        message: entry.text,
        source: entry.source
      });
    },
    onConnect: (targetInfo) => {
      emit({
        type: 'daw-connected',
        title: targetInfo.title,
        url: targetInfo.url
      });
      
      // Start polling DAW state
      if (dawStateInterval) clearInterval(dawStateInterval);
      dawStateInterval = setInterval(async () => {
        try {
          const state = await dawDebugger.getDAWState();
          if (state) {
            emit({
              type: 'daw-state',
              bpm: state.bpm,
              playing: state.playing,
              time: state.time
            });
          }
        } catch (e) {
          // Ignore polling errors
        }
      }, 500);
    },
    onDisconnect: () => {
      emit({ type: 'daw-disconnected' });
      if (dawStateInterval) {
        clearInterval(dawStateInterval);
        dawStateInterval = null;
      }
    }
  });
  
  try {
    await dawDebugger.connect();
  } catch (err) {
    emit({ type: 'error', message: `DAW connection failed: ${err.message}` });
    dawDebugger = null;
  }
}

// Disconnect from DAW
function disconnectDAW() {
  if (dawDebugger) {
    dawDebugger.disconnect();
    dawDebugger = null;
  }
  if (dawStateInterval) {
    clearInterval(dawStateInterval);
    dawStateInterval = null;
  }
  emit({ type: 'daw-disconnected' });
}

// Evaluate code in DAW
async function evalInDAW(code) {
  if (!dawDebugger || !dawDebugger.connected) {
    emit({ type: 'error', message: 'Not connected to DAW' });
    return;
  }
  
  try {
    const result = await dawDebugger.evaluate(code);
    emit({ type: 'eval-result', result });
  } catch (err) {
    emit({ type: 'error', message: `Eval failed: ${err.message}` });
  }
}

// Get current state
async function getState() {
  const serverStatus = await checkServerStatus();
  
  let dawState = null;
  if (dawDebugger && dawDebugger.connected) {
    try {
      dawState = await dawDebugger.getDAWState();
    } catch (e) {
      // Ignore
    }
  }
  
  emit({
    type: 'state',
    server: serverStatus,
    daw: dawDebugger?.connected ? {
      connected: true,
      title: dawDebugger.targetInfo?.title,
      ...dawState
    } : { connected: false }
  });
}

// Handle incoming messages from Emacs
async function handleMessage(msg) {
  try {
    const { type, ...params } = msg;
    
    switch (type) {
      case 'connect-daw':
        await connectDAW();
        break;
        
      case 'disconnect-daw':
        disconnectDAW();
        break;
        
      case 'eval':
        await evalInDAW(params.code);
        break;
        
      case 'get-state':
        await getState();
        break;
        
      default:
        emit({ type: 'error', message: `Unknown message type: ${type}` });
    }
  } catch (err) {
    emit({ type: 'error', message: err.message });
  }
}

// --- Main ---

// Start server status polling
serverCheckInterval = setInterval(async () => {
  const status = await checkServerStatus();
  emit({ type: 'server-status', status });
}, 5000);

// Initial server check
checkServerStatus().then(status => {
  emit({ type: 'server-status', status });
});

// Auto-connect to DAW on startup
setTimeout(connectDAW, 1000);

// Read JSON lines from stdin
const rl = readline.createInterface({
  input: process.stdin,
  terminal: false
});

rl.on('line', async (line) => {
  try {
    const msg = JSON.parse(line);
    await handleMessage(msg);
  } catch (e) {
    emit({ type: 'error', message: `Parse error: ${e.message}` });
  }
});

// Handle shutdown
process.on('SIGINT', () => {
  disconnectDAW();
  if (serverCheckInterval) clearInterval(serverCheckInterval);
  process.exit(0);
});

process.on('SIGTERM', () => {
  disconnectDAW();
  if (serverCheckInterval) clearInterval(serverCheckInterval);
  process.exit(0);
});

// Send ready message
emit({ type: 'ready', version: '1.0.0' });

console.error('🎨 Artery Emacs Bridge started');
