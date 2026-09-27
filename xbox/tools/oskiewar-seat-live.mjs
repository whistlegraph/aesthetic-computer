#!/usr/bin/env node
// One live game source for the Xbox and a loopback-connected native Mac.
import { createServer } from 'node:http';
import { createHash } from 'node:crypto';
import { readFile, writeFile, rename, mkdir } from 'node:fs/promises';
import { resolve, join } from 'node:path';
import { spawn } from 'node:child_process';

const sourcePath = resolve(process.argv[2]);
const portalTool = resolve(process.argv[3]);
const stateDir = resolve(process.argv[4]);
const port = Number(process.env.OSKIEWAR_LIVE_PORT || 5273);
await mkdir(stateDir, { recursive: true });
let accepted = null, pending = false, observed = '', changedAt = 0;
let retryAt = 0;
const status = { source: sourcePath, xbox: null, macos: null, error: null };
const saveStatus = () => writeFile(join(stateDir, 'status.json'), JSON.stringify(status, null, 2));
const run = (args) => new Promise((resolveResult, reject) => {
  const child = spawn(process.execPath, args, { stdio: ['ignore', 'pipe', 'pipe'] });
  let output = '';
  child.stdout.on('data', data => { output += data; });
  child.stderr.on('data', data => { output += data; });
  const timer = setTimeout(() => child.kill('SIGTERM'), 90000);
  child.on('error', reject);
  child.on('close', code => {
    clearTimeout(timer);
    if (code === 0) resolveResult(output.trim());
    else reject(new Error(output.trim() || `child exited ${code}`));
  });
});
const server = createServer(async (req, res) => {
  res.setHeader('Cache-Control', 'no-store');
  if (req.method === 'GET' && req.url === '/status') {
    res.setHeader('Content-Type', 'application/json');
    return res.end(JSON.stringify(status));
  }
  if (req.method === 'GET' && req.url === '/game.js') {
    if (!accepted) { res.writeHead(503); return res.end('waiting for Xbox'); }
    const tag = `"${accepted.revision}"`;
    res.setHeader('ETag', tag);
    if (req.headers['if-none-match'] === tag) { res.writeHead(304); return res.end(); }
    res.setHeader('Content-Type', 'application/javascript');
    return res.end(accepted.payload);
  }
  if (req.method === 'POST' && req.url === '/ack') {
    let data = '';
    for await (const chunk of req) {
      data += chunk;
      if (data.length > 4096) { res.writeHead(413); return res.end(); }
    }
    try {
      const ack = JSON.parse(data);
      if (ack.surface !== 'macos' || ack.revision !== accepted?.revision || typeof ack.ok !== 'boolean')
        throw new Error('invalid acknowledgement');
      status.macos = { revision: ack.revision, ok: ack.ok, at: new Date().toISOString(), error: String(ack.error || '').slice(0, 1000) };
      await saveStatus();
      console.log(`macos ${ack.ok ? 'ready' : 'rejected'} ${ack.revision}`);
      res.writeHead(204); return res.end();
    } catch { res.writeHead(400); return res.end(); }
  }
  res.writeHead(404); res.end();
});
await new Promise(resolveReady => server.listen(port, '127.0.0.1', resolveReady));
console.log(`watching ${sourcePath}; loopback :${port}`);
async function tick() {
  if (pending || Date.now() < retryAt) return;
  pending = true;
  try {
    const source = await readFile(sourcePath, 'utf8');
    const revision = createHash('sha256').update(source).digest('hex').slice(0, 16);
    if (revision !== observed) { observed = revision; changedAt = Date.now(); return; }
    if (revision === accepted?.revision || Date.now() - changedAt < 500) return;
    const payload = '// @bundle-qr\nglobalThis.__oskiewarOpponent = "freeskate";\n' +
      `globalThis.__oskiewarLiveRevision = "${revision}";\n` + source;
    const candidate = join(stateDir, 'candidate.js');
    await writeFile(candidate, payload);
    await run(['--check', candidate]);
    status.error = null;
    status.pending = revision;
    await saveStatus();
    const receipt = await run([portalTool, 'hot-deploy', candidate]);
    accepted = { revision, payload };
    status.xbox = { revision, ok: true, at: new Date().toISOString(), receipt };
    status.pending = null;
    await writeFile(join(stateDir, 'accepted.js.tmp'), payload);
    await rename(join(stateDir, 'accepted.js.tmp'), join(stateDir, 'accepted.js'));
    await saveStatus();
    console.log(`xbox ready ${revision}; serving same revision to macos`);
  } catch (error) {
    status.error = error.message;
    retryAt = Date.now() + 5000;
    await saveStatus();
    console.error(error.message);
  } finally { pending = false; }
}
setInterval(tick, 250);
await tick();
