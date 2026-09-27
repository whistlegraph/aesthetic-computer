#!/usr/bin/env node
// Ring lights for a native ring score (Act I): follows LEFT FRONT's rehearsal
// status over the LAN and drives neo's DMX bridge (TRIO_DMX, tunnelled to
// 127.0.0.1:8790) — each seat's fixture in that seat's score colour, a slow
// candle-like breath under it, and a walk around the ring on the beat.
// Runs until the seat leaves `playing` (or --for seconds), then cancels.
//   node bin/ring-lights.mjs [--seat=192.168.1.237] [--level=72] [--for=60]
import { writeFileSync } from 'node:fs';
const arg = (k, d) => { const m = process.argv.find((a) => a.startsWith(`--${k}=`)); return m ? m.slice(k.length + 3) : d; };
const SEAT = arg('seat', '192.168.1.237'), LEVEL = +arg('level', 72), FOR = +arg('for', 0), DMX = process.env.TRIO_DMX || 'http://127.0.0.1:8790';
const get = async (url) => { const r = await fetch(url, { signal: AbortSignal.timeout(1500) }); if (!r.ok) throw Error(`HTTP ${r.status} ${url}`); return r.json(); };
const post = async (path, data) => { const r = await fetch(DMX + path, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(data), signal: AbortSignal.timeout(1500) }); if (!r.ok) throw Error(`HTTP ${r.status} ${path}`); return r.text(); };
const clamp = (v) => Math.max(0, Math.min(128, Math.round(v)));
function noise(t, period, seed) { const c = Math.floor(t / period), p = t / period - c, s = p * p * (3 - 2 * p); const v = (i) => { const n = Math.sin(i * 127.1 + seed * 311.7) * 43758.5453; return (n - Math.floor(n)) * 2 - 1; }; return v(c) * (1 - s) + v(c + 1) * s; }
let run = null, commands = 0, skipped = 0, started = Date.now(), fixtures = [], colors = [];
try {
  const st = await get(DMX + '/state'); if (Date.now() / 1000 - st.bridgeSeen > 3) throw Error('DMX bridge not seen'); fixtures = st.fixtures.filter((f) => Number.isFinite(f.targetSeat));
  console.log(`ring lights: ${fixtures.length} fixtures (${fixtures.map((f) => `${f.address}→seat ${f.targetSeat}`).join(', ')}); following ${SEAT}`);
  const t0 = Date.now(); let step = -1;
  while (true) {
    if (FOR && Date.now() - t0 > FOR * 1000) break;
    let s; try { s = await get(`http://${SEAT}/pieces/spatial-rehearsal-status.json`); } catch { await new Promise((r) => setTimeout(r, 250)); continue; }
    const playing = s.phase === 'playing' && Number.isFinite(s.scoreTime);
    if (!playing) { if (run) break; if (Date.now() - started > 120000) throw Error('no ring score started within two minutes'); await new Promise((r) => setTimeout(r, 250)); continue; }
    if (run && run !== s.runId) break; run = s.runId;
    if (!colors.length) colors = s.seatColors || [[255, 110, 110], [255, 180, 70], [120, 220, 130], [95, 170, 255], [200, 130, 255], [255, 240, 200]];
    const t = s.scoreTime, dur = s.scoreDuration || 774, fade = Math.min(1, t / 2, Math.max(0, (dur - t) / 3));
    const beat = 60 / (s.bpm || 96), k = Math.floor(t / beat);
    const bridge = await get(DMX + '/state');
    if (bridge.queueDepth === 0 && k !== step) {   // one fixture per beat, walking the ring; the others breathe underneath
      step = k; const f = fixtures[k % fixtures.length]; const c = colors[f.targetSeat % colors.length] || [255, 255, 255];
      const breath = 0.55 + 0.25 * noise(t, 1.9, f.address) + 0.08 * noise(t, 0.31, f.address + 7);
      const rgb = c.map((v) => clamp(v / 2 * breath * fade));
      await post('/command', { address: f.address, color: 'rgb', rgb, level: LEVEL, duration: Math.max(0.25, beat * 1.6) }); commands++;
    } else if (bridge.queueDepth) skipped++;
    await new Promise((r) => setTimeout(r, 60));
  }
} catch (e) { console.error('ring lights:', e.message || e); }
finally {
  try { await post('/cancel', {}); } catch {}
  const receipt = { run, commands, skipped, seconds: Math.round((Date.now() - started) / 1000) };
  try { writeFileSync(process.env.RING_LIGHTS_RECEIPT || '/dev/null', JSON.stringify(receipt)); } catch {}
  console.log(JSON.stringify(receipt));
}
