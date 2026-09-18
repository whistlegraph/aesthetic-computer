#!/usr/bin/env node
// Explicit-host LAN rehearsal controller; never assumes mDNS names are unique.
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

const [action, ...args] = process.argv.slice(2);
const scoreOption = args.indexOf('--score');
let scoreName = 'monosine';
if (scoreOption >= 0) {
  scoreName = args[scoreOption + 1];
  args.splice(scoreOption, 2);
  if (!/^[a-z0-9-]+$/.test(scoreName || '')) throw Error('Invalid score name');
}
const allowMissing = args.includes('--allow-missing');
if (allowMissing) args.splice(args.indexOf('--allow-missing'), 1);
if (allowMissing && action !== 'cue') throw Error('--allow-missing is only for cue');
const hosts = args;
if (!['deploy', 'status', 'arm', 'cue', 'stop', 'identify', 'beeps'].includes(action) || hosts.length < 1 ||
    hosts.some(h => !/^[a-zA-Z0-9.-]+(?::\d+)?$/.test(h))) {
  console.error('usage: node tools/spatial-rehearsal.mjs deploy|status|arm|cue|stop|identify|beeps HOST...');
  process.exit(1);
}
if (action === 'deploy' && (hosts.length < 2 || hosts.length > 16)) throw Error('deploy needs 2–16 hosts');
const native = new URL('../', import.meta.url);
async function request(host, path, value) {
  const r = await fetch(`http://${host}${path}`, {
    signal: AbortSignal.timeout(6000),
    ...(value === undefined ? {} : { method: 'PUT', body: value }),
  });
  if (!r.ok) throw Error(`${host} ${path}: HTTP ${r.status}`);
  return r.text();
}
const id = `${Date.now()}-${Math.random().toString(16).slice(2)}`;
const states = () => Promise.all(hosts.map(async host =>
  JSON.parse(await request(host, '/pieces/spatial-rehearsal-status.json'))));
const command = (action, targets = hosts) => Promise.all(targets.map(host =>
  request(host, '/pieces/spatial-rehearsal-command.json', JSON.stringify({ id: `${id}-${action}`, action }))));
async function waitFor(phase, timeout = 5000) {
  const until = Date.now() + timeout;
  while (Date.now() < until) {
    const values = await states();
    if (values.every(s => s.phase === phase)) return values;
    if (phase !== 'stopped' && values.some(s => ['error'].includes(s.phase))) throw Error(JSON.stringify(values));
    await delay(200);
  }
  throw Error(`Timed out waiting for ${phase}: ${JSON.stringify(await states())}`);
}
if (action === 'deploy') {
  const lib = (await readFile(new URL('lib/spatial-rehearsal.mjs', native), 'utf8')).replace(/^export /gm, '');
  const entry = await readFile(new URL('pieces/spatial-rehearsal.mjs', native), 'utf8');
  // Bundle into a fresh piece load; QuickJS caches ordinary /lib imports.
  const bundle = entry.replace(/^import .*spatial-rehearsal.mjs';$/m, lib);
  const scorePath = scoreName === 'monosine'
    ? new URL('../../../grants/culturehub-la-2026/monosine.nsscore', import.meta.url)
    : new URL(`scores/${scoreName}.nsscore`, native);
  const score = await readFile(scorePath, 'utf8');
  const parsedScore = JSON.parse(score);
  const order = parsedScore.seatOrder || hosts.map((_, i) => i);
  if (order.length !== hosts.length) throw Error('Provide hosts in score seatOrder');
  const totalSeats = Math.max(hosts.length, ...order.map(i => i + 1));
  const backupDir = new URL(`../../../.tmp/spatial-rehearsal/${id}/`, import.meta.url);
  await mkdir(backupDir, { recursive: true });
  // Stage every device before jumping any device. Preserve replaced files.
  await Promise.all(hosts.map(async (host, index) => {
    const seat = order[index];
    const status = JSON.parse(await request(host, '/status'));
    if (!status.build || !status.piece) throw Error(`${host} is not an AC Native endpoint`);
    const files = {
      'spatial-rehearsal.mjs': bundle,
      'spatial-rehearsal.nsscore': score,
      'spatial-rehearsal-config.json': JSON.stringify({ seat, seats: totalSeats, machineName: status.name, ip: status.ip || host, maxSeconds: parsedScore.dur }),
      'spatial-rehearsal-command.json': JSON.stringify({ id, action: 'arm' }),
    };
    for (const [name, body] of Object.entries(files)) {
      let old;
      try { old = await request(host, '/pieces/' + name); }
      catch (e) { if (!e.message.includes('HTTP 404')) throw e; }
      if (old !== undefined) await writeFile(new URL(`${host.replaceAll(':', '_')}-${name}`, backupDir), old);
      await request(host, '/pieces/' + name, body);
      if (await request(host, '/pieces/' + name) !== body) throw Error(`${host}: readback differs for ${name}`);
    }
    console.log(`${host}: seat ${seat + 1}/${hosts.length}, staged and readback verified`);
  }));
  await Promise.all(hosts.map(host => request(host, '/jump/spatial-rehearsal', '')));
  console.log(`Loaded silently. Backups: ${fileURLToPath(backupDir)}\nEach seat is output-only. Use beeps or cue with the same host list.`);
} else if (action === 'status') {
  const results = await Promise.allSettled(hosts.map(async host => {
    const status = JSON.parse(await request(host, '/pieces/spatial-rehearsal-status.json'));
    console.log(JSON.stringify({ host, ...status }));
  }));
  for (const r of results) if (r.status === 'rejected') { console.error(r.reason.message); process.exitCode = 1; }
} else {
  if (action === 'cue' || action === 'beeps') {
    const before = await states();
    if (before.some(s => s.error || s.microphone?.hot || s.microphone?.recording))
      throw Error('Every seat must be healthy with microphone closed');
    const expected = before[0].seatOrder || Array.from({length: before[0].seats}, (_, i) => i);
    if (before.some(s => JSON.stringify(s.seatOrder || Array.from({length:s.seats}, (_, i)=>i)) !== JSON.stringify(expected)) ||
        (!allowMissing && hosts.length !== expected.length) || new Set(before.map(s => s.seat)).size !== hosts.length ||
        before.some(s => !expected.includes(s.seat)))
      throw Error('Supply the complete ensemble with unique seats');
    if (allowMissing) console.log('Missing seats stay silent at their assigned positions: ' + expected.filter(i => !before.some(s => s.seat === i)).map(i => i + 1).join(', '));
    const clocks = await Promise.all(hosts.map(async host => {
      let best = null;
      for (let i = 0; i < 7; i++) {
        const probeId = `${id}-clock-${i}`;
        const sent = performance.now() / 1000;
        await request(host, '/pieces/spatial-rehearsal-command.json', JSON.stringify({ id: probeId, action: 'clock' }));
        let reply;
        const deadline = sent + 3;
        do {
          try { reply = JSON.parse(await request(host, '/pieces/spatial-rehearsal-clock.json')); } catch (_) { /* retry */ }
          if (reply?.id === probeId) break;
          if (performance.now() / 1000 >= deadline) throw Error(`${host}: clock probe timed out`);
          await delay(10);
        } while (true);
        const received = performance.now() / 1000;
        const rtt = received - sent;
        if (!best || rtt < best.rtt) best = { rtt, offset: reply.audioTime - (sent + received) / 2 };
      }
      return best;
    }));
    const start = performance.now() / 1000 + 5;
    try {
      await Promise.all(hosts.map((host, i) => request(host, '/pieces/spatial-rehearsal-command.json', JSON.stringify({
        id: `${id}-prepare`, action: 'prepare', mode: action === 'beeps' ? 'beeps' : 'score',
        startAt: start + clocks[i].offset, networkHalfRttMs: clocks[i].rtt * 500,
      }))));
      await waitFor('prepared', 2500);
      if (start - performance.now() / 1000 < 2) throw Error('Prepare acknowledgments arrived too late');
      await command('play');
      await waitFor('countdown', 2000);
      console.log(`All ${hosts.length} seats scheduled for ${action === 'beeps' ? 'louder beeps' : 'spatial score'}; microphones closed. Run lasts ${action === 'beeps' ? 20 : before[0].scoreDuration} seconds.`);
      for (let i = 0; i < hosts.length; i++) console.log(`${hosts[i]}: minimum clock round trip ${(clocks[i].rtt * 1000).toFixed(1)} ms`);
    } catch (e) {
      await command('stop');
      throw e;
    }
  } else {
    await command(action);
    console.log(`${action}: ${hosts.join(', ')}`);
  }
}
