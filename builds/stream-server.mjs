#!/usr/bin/env node
// Build Stream Server - WebSocket server for streaming build progress
import { WebSocketServer } from 'ws';
import http from 'http';
import express from 'express';

const PORT = 8890;
const app = express();
app.use(express.text());

const clients = new Set();

// HTTP endpoint to receive log lines
app.post('/log', (req, res) => {
  const line = req.body;
  broadcast({ type: 'log', line, timestamp: Date.now() });
  res.status(200).send('OK');
});

// HTTP endpoint to send build status updates
app.post('/status', express.json(), (req, res) => {
  broadcast({ type: 'status', ...req.body, timestamp: Date.now() });
  res.status(200).send('OK');
});

// Create HTTP + WebSocket server
const server = http.createServer(app);
const wss = new WebSocketServer({ server, path: '/stream' });

wss.on('connection', (ws) => {
  console.log('📡 Client connected');
  clients.add(ws);
  
  // Send welcome message
  ws.send(JSON.stringify({ 
    type: 'connected', 
    message: 'Build stream connected',
    timestamp: Date.now() 
  }));
  
  ws.on('close', () => {
    console.log('📡 Client disconnected');
    clients.delete(ws);
  });
  
  ws.on('error', (err) => {
    console.error('WebSocket error:', err);
    clients.delete(ws);
  });
});

function broadcast(data) {
  const message = JSON.stringify(data);
  clients.forEach(client => {
    if (client.readyState === 1) { // OPEN
      client.send(message);
    }
  });
}

server.listen(PORT, () => {
  console.log(`🔥 Build stream server running on http://localhost:${PORT}`);
  console.log(`   POST /log - Send log lines`);
  console.log(`   POST /status - Send status updates`);
  console.log(`   WebSocket: ws://localhost:${PORT}/stream`);
});
