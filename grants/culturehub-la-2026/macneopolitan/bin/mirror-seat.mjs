#!/usr/bin/env node
// Mirror one native trio seat onto an extra AC OS machine, and drive that
// machine from this Mac's run records so it lands on the room's downbeat.
//
// The fleet plan is exactly six seats, and the conductor (run-full-trio.py)
// only ever addresses those six. A seventh machine — Will's Chromebook on
// 2026-09-24 — can still echo a seat: copy the seat's staged files over the
// LAN, jump it into trio-fleet, then replay the conductor's cue to it. This
// tool never writes to a real seat; it only GETs from the source and PUTs to
// the mirror.
//
//   node bin/mirror-seat.mjs sync   [--from=192.168.1.237] [--to=192.168.1.246]
//   node bin/mirror-seat.mjs relay  [...]      # sync, then follow run records
//
// Records: run-full-trio.py saves OUT/full-trio-<id>.json with startEpoch
// (wall clock) and nativeCountdown once every seat has been told to play.
// The relay watches the Shelf song folders for those, measures the mirror's
// audio clock the way the conductor does (clock command, RTT/2), and sends
// prepare / play / keepalive / stop to the mirror only.
import {readdirSync, readFileSync, statSync, existsSync} from 'node:fs';
import {createHash, randomUUID} from 'node:crypto';
import {resolve} from 'node:path';

const args = process.argv.slice(2);
const cmd = args.shift();
const opt = Object.fromEntries(args.filter(a => a.startsWith('--') && a.includes('=')).map(a => { const i = a.indexOf('='); return [a.slice(2, i), a.slice(i + 1)]; }));
const FROM = `http://${opt.from ?? '192.168.1.237'}`;   // seat 4, LEFT FRONT (ac5)
const TO = `http://${opt.to ?? '192.168.1.246'}`;       // the mirror
const SHELF = opt.shelf ?? '/Users/jas/Shelf';
const log = (...a) => console.log(new Date().toISOString().slice(11, 23), ...a);

async function get(base, path, {timeout = 8000, buffer = false} = {}) {
  const r = await fetch(base + path, {signal: AbortSignal.timeout(timeout)});
  if (!r.ok) throw Error(`GET ${base}${path}: ${r.status}`);
  return buffer ? Buffer.from(await r.arrayBuffer()) : await r.text();
}
async function put(base, path, body, timeout = 30000) {
  const r = await fetch(base + path, {method: 'PUT', body, signal: AbortSignal.timeout(timeout)});
  if (!r.ok) throw Error(`PUT ${base}${path}: ${r.status}`);
}
const sha = b => createHash('sha256').update(b).digest('hex');
const json = async (base, path) => JSON.parse(await get(base, path, {timeout: 3000}));
const sleep = ms => new Promise(r => setTimeout(r, ms));

async function status(base) { try { return await json(base, '/pieces/trio-fleet-status.json'); } catch { return null; } }
async function command(base, action, extra = {}) {
  const c = {id: randomUUID().replaceAll('-', ''), action, ...extra};
  await put(base, '/pieces/trio-fleet-command.json', JSON.stringify(c), 3000);
  return c.id;
}

// Copy the source seat's staged files to the mirror byte for byte.
async function sync() {
  const cfgText = await get(FROM, '/pieces/trio-fleet-config.json');
  const cfg = JSON.parse(cfgText);
  let mirrorCfg = null;
  try { mirrorCfg = await json(TO, '/pieces/trio-fleet-config.json'); } catch {}
  const mirrorStatus = await status(TO);
  if (mirrorCfg?.arrangementHash === cfg.arrangementHash && mirrorStatus?.arrangementHash === cfg.arrangementHash && mirrorStatus?.phase === 'ready') {
    log(`mirror already carries ${cfg.label} (${cfg.arrangementHash.slice(0, 12)}), ready`);
    return cfg;
  }
  log(`syncing seat ${cfg.seat} ${cfg.label} (${cfg.arrangementHash.slice(0, 12)}) ${FROM} -> ${TO}`);
  for (const part of cfg.center?.parts ?? []) {
    const bytes = await get(FROM, part, {buffer: true, timeout: 60000});
    await put(TO, part, bytes, 60000);
    const back = await get(TO, part, {buffer: true, timeout: 60000});
    if (sha(back) !== sha(bytes)) throw Error(`${part}: mirror copy differs`);
    log(`  ${part} ${bytes.length} bytes ok`);
  }
  // The six seats run the `mono` OTA channel, whose runtime has
  // sound.volume.setMonoOutput (which speaker side carries the fold). A
  // mirror on the main channel lacks it, and the piece calls it in boot()
  // outside its try — the mirror then sat in phase "loading" forever
  // (2026-09-24). Guard those calls in the mirror's copy only.
  const code = Buffer.from((await get(FROM, '/pieces/trio-fleet.mjs')).replace(
    /([\w.]+)\.setMonoOutput\(([^)]*)\);/g, 'try{$1.setMonoOutput($2);}catch{}'));
  await put(TO, '/pieces/trio-fleet.mjs', code);
  if (sha(await get(TO, '/pieces/trio-fleet.mjs', {buffer: true})) !== sha(code)) throw Error('piece copy differs');
  await put(TO, '/pieces/trio-fleet-config.json', cfgText);
  await command(TO, 'idle');
  await put(TO, '/jump/trio-fleet', '');
  log('  piece + config pushed, jumped to trio-fleet; waiting for ready');
  for (let i = 0; i < 120; i++) {
    await sleep(500);
    const s = await status(TO);
    if (s?.arrangementHash === cfg.arrangementHash && s.phase === 'ready') { log(`  mirror ready as ${s.receiverId}`); return cfg; }
    if (s?.error) throw Error(`mirror error: ${s.error}`);
  }
  throw Error('mirror did not become ready in 60 s');
}

// audioTime(mirror) - wallclock, from the best of a few clock probes (RTT/2),
// the same contract the conductor applies to the six seats.
async function clockOffset() {
  let best = null;
  for (let i = 0; i < 8; i++) {
    const a = Date.now() / 1000;
    const id = await command(TO, 'clock');
    let clock = null;
    for (let j = 0; j < 40; j++) { await sleep(25); try { clock = await json(TO, '/pieces/trio-fleet-clock.json'); } catch {} if (clock?.id === id) break; }
    const b = Date.now() / 1000;
    if (clock?.id !== id) continue;
    const s = {offset: clock.audioTime - (a + b) / 2, rtt: b - a};
    if (!best || s.rtt < best.rtt) best = s;
  }
  if (!best) throw Error('mirror clock probe failed');
  return best;
}

function findRecords() {
  const out = [];
  for (const dir of readdirSync(SHELF, {withFileTypes: true})) {
    if (!dir.isDirectory() || !dir.name.startsWith('culturehub')) continue;
    const d = resolve(SHELF, dir.name);
    for (const f of readdirSync(d)) if (/^full-trio-[0-9a-f]+\.json$/.test(f)) out.push(resolve(d, f));
  }
  return out;
}

async function relay() {
  let cfg = await sync();
  const done = new Set(findRecords().map(p => p + ':' + statSync(p).mtimeMs));  // only cues issued after we start
  let lastSyncCheck = Date.now();
  log(`relaying cues for ${cfg.label}; watching ${SHELF}/culturehub*/full-trio-*.json`);
  for (;;) {
    await sleep(200);
    if (Date.now() - lastSyncCheck > 10000) {
      lastSyncCheck = Date.now();
      try { const src = await json(FROM, '/pieces/trio-fleet-config.json'); if (src.arrangementHash !== cfg.arrangementHash) { log('source arrangement changed — resyncing'); cfg = await sync(); } } catch {}
    }
    for (const p of findRecords()) {
      const key = p + ':' + statSync(p).mtimeMs;
      if (done.has(key)) continue;
      done.add(key);
      let rec; try { rec = JSON.parse(readFileSync(p, 'utf8')); } catch { continue; }
      if (!rec.startEpoch || !rec.nativeCountdown || rec.cleanup || rec.error) continue;
      if (rec.arrangementHash !== cfg.arrangementHash) { log(`cue ${rec.runId}: arrangement ${rec.arrangementHash.slice(0, 12)} != mirror ${cfg.arrangementHash.slice(0, 12)} — resync then skip`); try { cfg = await sync(); } catch (e) { log(String(e)); } continue; }
      const lead = rec.startEpoch - Date.now() / 1000;
      if (lead < 2.5) { log(`cue ${rec.runId}: downbeat in ${lead.toFixed(1)} s — too late for the mirror`); continue; }
      try {
        const clk = await clockOffset();
        const startAt = rec.startEpoch + clk.offset;
        await command(TO, 'prepare', {arrangementHash: cfg.arrangementHash, startAt, runId: rec.runId});
        let s = null; for (let i = 0; i < 40; i++) { await sleep(50); s = await status(TO); if (s?.phase === 'prepared' && s.runId === rec.runId) break; }
        if (s?.phase !== 'prepared') throw Error(`mirror did not prepare: ${JSON.stringify(s)}`);
        await command(TO, 'play', {runId: rec.runId});
        log(`cue ${rec.runId}: prepared + play, downbeat in ${(rec.startEpoch - Date.now() / 1000).toFixed(2)} s (clock offset ${clk.offset.toFixed(3)}, rtt ${(clk.rtt * 1000).toFixed(0)} ms)`);
        const end = rec.startEpoch + (rec.duration ?? cfg.duration + 2);
        while (Date.now() / 1000 < end + 0.5) {
          await command(TO, 'keepalive', {runId: rec.runId}).catch(e => log('keepalive:', e.message));
          await sleep(500);
          try { const r2 = JSON.parse(readFileSync(p, 'utf8')); if (r2.cleanup) { log('conductor cleaned up — stopping mirror'); break; } } catch {}
        }
        await command(TO, 'stop').catch(() => {});
        const fin = await status(TO);
        log(`cue ${rec.runId}: finished; mirror phase=${fin?.phase} startLateMs=${fin?.startLateMs}`);
      } catch (e) { log(`cue ${rec.runId}: ${e.message}`); await command(TO, 'stop').catch(() => {}); }
    }
  }
}

if (cmd === 'sync') await sync();
else if (cmd === 'relay') await relay();
else { console.error('Use mirror-seat.mjs sync|relay [--from=IP] [--to=IP] [--shelf=DIR]'); process.exit(2); }
