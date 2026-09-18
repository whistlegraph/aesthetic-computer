#!/usr/bin/env node
// Read-only LAN telemetry bridge. No control forwarding, logs, audio, or credentials.
import http from 'node:http';
import { pathToFileURL } from 'node:url';

export function createSpatialDataServer(hosts, { fetcher = fetch, now = Date.now, interval = 200 } = {}) {
  const slots = hosts.map(host => ({ host, data: null, connected: false,
    receivedAt: null, changedAt: null, error: null, pending: false }));
  const clients = new Set();
  const snapshot = () => ({ updatedAt: new Date(now()).toISOString(), seats: slots.map(s => ({
    host: s.host, connected: s.connected,
    stale: !s.connected || s.changedAt === null || now() - s.changedAt > 2000,
    ageMs: s.changedAt === null ? null : Math.max(0, now() - s.changedAt),
    receivedAt: s.receivedAt, data: s.data, error: s.error,
  })) });
  async function poll(s) {
    if (s.pending) return;
    s.pending = true;
    try {
      const r = await fetcher(`http://${s.host}/pieces/spatial-rehearsal-status.json`,
        { signal: AbortSignal.timeout(1500), cache: 'no-store' });
      if (!r.ok) throw Error(`HTTP ${r.status}`);
      const data = JSON.parse(await r.text());
      if (!Number.isFinite(data.audioTime) || !Number.isInteger(data.seat)) throw Error('Invalid seat status');
      if (!s.data || data.audioTime !== s.data.audioTime) s.changedAt = now();
      s.data = data; s.connected = true; s.error = null; s.receivedAt = new Date(now()).toISOString();
    } catch (e) { s.connected = false; s.error = e.message; }
    finally { s.pending = false; }
  }
  const pollAll = () => Promise.all(slots.map(poll));
  const server = http.createServer((req, res) => {
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Cache-Control', 'no-store');
    if (req.method === 'OPTIONS') {
      res.setHeader('Access-Control-Allow-Methods', 'GET, OPTIONS');
      res.writeHead(204); return res.end();
    }
    if (req.method !== 'GET') { res.writeHead(405); return res.end('Read only\n'); }
    const path = new URL(req.url, 'http://localhost').pathname;
    if (path === '/events') {
      res.writeHead(200, { 'Content-Type': 'text/event-stream', Connection: 'keep-alive' });
      res.write('retry: 1000\n\n');
      res.write(`event: seats\ndata: ${JSON.stringify(snapshot())}\n\n`);
      clients.add(res); req.on('close', () => clients.delete(res)); return;
    }
    if (path === '/api/seats' || path === '/health' || path === '/') {
      res.writeHead(200, { 'Content-Type': 'application/json' });
      return res.end(JSON.stringify(path === '/api/seats' ? snapshot() : {
        service: 'AC spatial data', readOnly: true, hosts, snapshot: '/api/seats', events: '/events',
      }));
    }
    res.writeHead(404); res.end('Not found\n');
  });
  const timer = setInterval(() => {
    void pollAll();
    const event = `event: seats\ndata: ${JSON.stringify(snapshot())}\n\n`;
    for (const res of clients) if (!res.write(event)) { clients.delete(res); res.destroy(); }
  }, interval);
  timer.unref();
  server.on('close', () => { clearInterval(timer); for (const res of clients) res.end(); });
  void pollAll();
  return { server, snapshot, pollAll };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const args = process.argv.slice(2), hosts = [];
  let bind = '127.0.0.1', port = 8787;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--bind') bind = args[++i];
    else if (args[i] === '--port') port = Number(args[++i]);
    else hosts.push(args[i]);
  }
  if (!hosts.length || hosts.some(h => !/^[a-zA-Z0-9.-]+(?::\d+)?$/.test(h)) || !Number.isInteger(port) || port < 1 || port > 65535)
    throw Error('usage: spatial-data.mjs [--bind 127.0.0.1] [--port 8787] HOST...');
  const { server } = createSpatialDataServer(hosts);
  server.listen(port, bind, () => console.log(`Read-only spatial data: http://${bind}:${port}/api/seats and /events`));
}
