#!/usr/bin/env node
// Live reload server for process-tree.js development
// Watches for file changes and notifies connected clients via WebSocket

import { WebSocketServer } from 'ws';
import { watch } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PORT = 5556;

const wss = new WebSocketServer({ port: PORT });
const clients = new Set();

wss.on('connection', (ws) => {
  clients.add(ws);
  console.log(`🔌 Client connected (${clients.size} total)`);
  
  ws.on('close', () => {
    clients.delete(ws);
    console.log(`🔌 Client disconnected (${clients.size} total)`);
  });
});

function broadcast(message) {
  const data = JSON.stringify(message);
  clients.forEach(client => {
    if (client.readyState === 1) { // WebSocket.OPEN
      client.send(data);
    }
  });
}

// Watch process-tree.js, ast-tree.js, and dev.html
const filesToWatch = ['process-tree.js', 'ast-tree.js', 'dev.html'];
let debounceTimer = null;

filesToWatch.forEach(file => {
  const filePath = join(__dirname, file);
  watch(filePath, (eventType) => {
    if (eventType === 'change') {
      // Debounce rapid changes
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(() => {
        console.log(`📝 ${file} changed, reloading clients...`);
        broadcast({ type: 'reload', file });
      }, 100);
    }
  });
});

console.log(`🔄 Live reload server running on ws://localhost:${PORT}`);
console.log(`👀 Watching: ${filesToWatch.join(', ')}`);
