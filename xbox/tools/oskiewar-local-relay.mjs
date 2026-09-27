#!/usr/bin/env node
// Explicit LAN-only development relay. Uses the production protocol and no analytics.
import http from 'node:http';
import { WebSocketServer } from 'ws';
import { OskiewarLiveManager } from '../../session-server/oskiewar-live-manager.mjs';
const options = Object.fromEntries(process.argv.slice(2).map(arg => arg.replace(/^--/, '').split('=')));
const host = options.host || '127.0.0.1';
const port = Number(options.port || 7793);
if (!/^\d+\.\d+\.\d+\.\d+$/.test(host) || !Number.isInteger(port) || port < 1024 || port > 65535) throw Error('Use --host=LAN_IP --port=PORT');
const manager = new OskiewarLiveManager({ analytics: { capture() {} } });
const server = http.createServer((_req, res) => {
  res.writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' });
  res.end(JSON.stringify({ service: 'oskiewar-local-relay', rooms: manager.rooms.size }));
});
const sockets = new WebSocketServer({ server, maxPayload: 16384, perMessageDeflate: false });
sockets.on('connection', (ws, req) => {
  req.socket.setNoDelay(true);
  if (!manager.handleConnection(ws, req)) ws.close(4404, 'Unknown route');
});
const heartbeat = setInterval(() => {
  for (const ws of sockets.clients) {
    if (ws.isAlive === false) ws.terminate();
    else { ws.isAlive = false; ws.ping(); }
  }
}, 15000);
server.listen(port, host, () => console.log(`Oskiewar LAN relay ws://${host}:${port}/oskiewar-live`));
function shutdown() {
  clearInterval(heartbeat);
  for (const ws of sockets.clients) ws.terminate();
  sockets.close(); server.close();
}
process.on('SIGTERM', shutdown);
process.on('SIGINT', shutdown);
