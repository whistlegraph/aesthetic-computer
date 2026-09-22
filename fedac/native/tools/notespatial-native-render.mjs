#!/usr/bin/env node
// notespatial-native-render — hear a ring .nsscore from the middle of the room.
//
// Six laptop feeds are synthesized exactly as the spatial-rehearsal piece
// routes them (lib/spatial-rehearsal.mjs voicePosition + sourceGain, the
// native linear attack/decay envelope), then each feed is placed at its
// seat around a listener at the center: seat 1 ahead, numbers clockwise.
// Placement is measured KEMAR HRTF (tools/hrir, via ffmpeg afir) or, with
// --fast, a parametric head (delay, shadow, tilt) computed here.
//
// Built for iterating: render a slice, or a section, or audio only.
//
//   node notespatial-native-render.mjs [score.nsscore] [--out x.mp4]
//        [--from 7:20] [--to 8:00] [--section 5] [--fps 12] [--size 640x360]
//        [--audio-only] [--fast]

import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { voicePosition, sourceGain } from '../lib/spatial-rehearsal.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const flag = k => args.includes('--' + k);
const opt = (k, d) => { const i = args.indexOf('--' + k); return i >= 0 ? args[i + 1] : d; };
const positional = args.filter((a, i) => !a.startsWith('--') && !(i > 0 && args[i - 1].startsWith('--') && !['audio-only', 'fast'].includes(args[i - 1].slice(2))));
const scorePath = positional[0] || join(HERE, '../scores/notespatial-native.nsscore');
const score = JSON.parse(readFileSync(scorePath, 'utf8'));
const SEATS = score.seats || 6;
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;
const parseT = s => s === undefined ? undefined : s.includes(':') ? s.split(':').reduce((a, b) => a * 60 + +b, 0) : +s;

let from = parseT(opt('from')) ?? 0, to = parseT(opt('to')) ?? score.dur;
if (opt('section')) {
  const m = score.movements?.[+opt('section') - 1];
  if (!m) throw Error('no such section');
  from = m.t0; to = Math.min(score.dur, m.t1 + 1.5);
}
const FPS = +opt('fps', 12), [W, H] = opt('size', '640x360').split('x').map(Number);
const outPath = resolve(opt('out', join(HERE, '../../../grants/culturehub-la-2026/notespatial-native-sim' + (opt('section') ? '-' + opt('section') : from || to !== score.dur ? `-${mmss(from).replace(':', 'm')}-${mmss(to).replace(':', 'm')}` : '') + (flag('audio-only') ? '.wav' : '.mp4'))));
const work = join(tmpdir(), 'notespatial-render');
mkdirSync(work, { recursive: true });
const tick = (() => { let t = performance.now(); return label => { const n = performance.now(); console.log(`${label.padEnd(28)} ${((n - t) / 1000).toFixed(1)} s`); t = n; }; })();

// ── 1 · the six feeds ────────────────────────────────────────────────
const SR = 44100, TAIL = 1.5;
const span = to - from, N = Math.ceil((span + TAIL) * SR);
const feeds = Array.from({ length: SEATS }, () => new Float32Array(N));
let seed = 20260924;
const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296 * 2 - 1;
const BLOCK = 256;
let voiced = 0;
score.lanes.forEach((lane, li) => {
  const pinned = Number.isFinite(lane.az);
  let gains = null;
  if (pinned) { const p = voicePosition(score, li, 0); gains = Array.from({ length: SEATS }, (_, k) => sourceGain(score, p, k, SEATS)); }
  for (const e of lane.events) {
    if (e.t + e.dur <= from || e.t >= to) continue;
    voiced++;
    const g = Math.min(.65, Math.max(0, e.g * (score.gain ?? .35)));
    const s0 = Math.round((e.t - from) * SR), len = Math.round(e.dur * SR);
    const attack = (e.attack ?? .01) * SR, decay = (e.decay ?? .06) * SR, decayStart = Math.max(0, len - decay);
    const inc = (e.hz || 220) / SR;
    const wave = e.wave;
    let phase = 0;
    for (let b = 0; b < len; b += BLOCK) {
      const bl = Math.min(BLOCK, len - b);
      const gk = pinned ? gains : (() => { const p = voicePosition(score, li, e.t + b / SR); return Array.from({ length: SEATS }, (_, k) => sourceGain(score, p, k, SEATS)); })();
      // synthesize the block once, then route it
      const buf = new Float32Array(bl);
      for (let i = 0; i < bl; i++) {
        const n = b + i;
        let env = attack > 0 && n < attack ? n / attack : 1;
        if (decay > 0 && n > decayStart) env *= Math.max(0, 1 - (n - decayStart) / decay);
        let v;
        if (wave === 'noise') v = rnd();
        else {
          const f = phase - Math.floor(phase);
          v = wave === 'triangle' ? 2 * Math.abs(2 * f - 1) - 1 : wave === 'sawtooth' ? 2 * f - 1 : wave === 'square' ? (f < .5 ? 1 : -1) : Math.sin(phase * Math.PI * 2);
          phase += inc;
        }
        buf[i] = v * env * g;
      }
      for (let k = 0; k < SEATS; k++) {
        const gain = gk[k];
        if (gain <= 0) continue;
        const F = feeds[k];
        for (let i = 0, n = s0 + b; i < bl; i++, n++) if (n >= 0 && n < N) F[n] += buf[i] * gain;
      }
    }
  }
});
// headroom: the loudest seat sample sits at -6 dBFS before the head is applied
let peak = 0;
for (const F of feeds) for (let i = 0; i < N; i += 7) peak = Math.max(peak, Math.abs(F[i]));
const norm = peak > 0 ? .5 / peak : 1;
for (const F of feeds) for (let i = 0; i < N; i++) F[i] *= norm;
tick(`feeds: ${voiced} events, ${mmss(span)}`);

// ── 2 · the head ─────────────────────────────────────────────────────
const seatAz = k => k / SEATS * 360; // degrees clockwise from ahead
function writeStereoWav(path, L, R, n) { // peak-normalized to -0.45 dBFS, 16-bit
  let pk = 0;
  for (let i = 0; i < n; i++) pk = Math.max(pk, Math.abs(L[i]), Math.abs(R[i]));
  const g = pk > 0 ? .95 / pk : 1;
  const pcm = Buffer.alloc(44 + n * 4);
  pcm.write('RIFF', 0); pcm.writeUInt32LE(36 + n * 4, 4); pcm.write('WAVE', 8); pcm.write('fmt ', 12);
  pcm.writeUInt32LE(16, 16); pcm.writeUInt16LE(1, 20); pcm.writeUInt16LE(2, 22); pcm.writeUInt32LE(SR, 24);
  pcm.writeUInt32LE(SR * 4, 28); pcm.writeUInt16LE(4, 32); pcm.writeUInt16LE(16, 34); pcm.write('data', 36); pcm.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) { pcm.writeInt16LE(Math.round(L[i] * g * 32767), 44 + i * 4); pcm.writeInt16LE(Math.round(R[i] * g * 32767), 46 + i * 4); }
  writeFileSync(path, pcm);
}
const binPath = join(HERE, 'hrir/kemar-compact.bin'), idxPath = join(HERE, 'hrir/kemar-compact.json');
const useHrtf = !flag('fast') && existsSync(binPath) && existsSync(idxPath);
const binaural = join(work, 'binaural.wav');
if (useHrtf) {
  const idx = JSON.parse(readFileSync(idxPath, 'utf8')), raw = readFileSync(binPath), taps = idx.taps;
  const el0 = idx.elevations.find(e => e.el === 0);
  const ir = azDeg => { // right hemisphere is measured; the left is the mirror with ears swapped
    let az = ((azDeg % 360) + 360) % 360, swap = false;
    if (az > 180) { az = 360 - az; swap = true; }
    const a = el0.azimuths.reduce((b, c) => Math.abs(c.az - az) < Math.abs(b.az - az) ? c : b);
    const L = new Float32Array(taps), R = new Float32Array(taps);
    for (let i = 0; i < taps; i++) { L[i] = raw.readInt16LE(a.off + i * 2) / 32768; R[i] = raw.readInt16LE(a.off + taps * 2 + i * 2) / 32768; }
    return swap ? [R, L] : [L, R];
  };
  const inputs = [], graph = [];
  for (let k = 0; k < SEATS; k++) {
    writeFileSync(join(work, `seat${k}.f32`), Buffer.from(feeds[k].buffer));
    const [L, R] = ir(seatAz(k));
    writeFileSync(join(work, `ir${k}L.f32`), Buffer.from(L.buffer));
    writeFileSync(join(work, `ir${k}R.f32`), Buffer.from(R.buffer));
    inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(work, `seat${k}.f32`));
  }
  for (let k = 0; k < SEATS; k++) for (const ear of 'LR')
    inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(work, `ir${k}${ear}.f32`));
  for (let k = 0; k < SEATS; k++) {
    graph.push(`[${k}:a]asplit[s${k}a][s${k}b]`);
    graph.push(`[s${k}a][${SEATS + k * 2}:a]afir=gtype=none:dry=1:wet=1[l${k}]`);
    graph.push(`[s${k}b][${SEATS + k * 2 + 1}:a]afir=gtype=none:dry=1:wet=1[r${k}]`);
  }
  graph.push(Array.from({ length: SEATS }, (_, k) => `[l${k}]`).join('') + `amix=inputs=${SEATS}:normalize=0[L]`);
  graph.push(Array.from({ length: SEATS }, (_, k) => `[r${k}]`).join('') + `amix=inputs=${SEATS}:normalize=0[R]`);
  graph.push('[L][R]join=inputs=2:channel_layout=stereo[out]');
  const mixed = join(work, 'binaural.f32');
  const r = spawnSync('ffmpeg', ['-y', '-v', 'error', ...inputs, '-filter_complex', graph.join(';'), '-map', '[out]', '-f', 'f32le', '-c:a', 'pcm_f32le', mixed], { stdio: 'inherit' });
  if (r.status !== 0) throw Error('ffmpeg binaural mix failed');
  const st = new Float32Array(readFileSync(mixed).buffer), n = st.length >> 1;
  const L = new Float32Array(n), R = new Float32Array(n);
  for (let i = 0; i < n; i++) { L[i] = st[i * 2]; R[i] = st[i * 2 + 1]; }
  writeStereoWav(binaural, L, R, n);
  tick('head: measured KEMAR HRTF');
} else {
  // Parametric head: interaural delay, far-ear shadow (one-pole), and a tilt
  // toward the front so rear seats read as behind.
  const L = new Float32Array(N), R = new Float32Array(N);
  for (let k = 0; k < SEATS; k++) {
    const az = seatAz(k) * Math.PI / 180, s = Math.sin(az), c = Math.cos(az);
    const itd = Math.round(Math.abs(s) * .00066 * SR);
    const nearGain = 1, farGain = 10 ** (-(4 * Math.abs(s)) / 20), rear = c < 0 ? .78 : 1;
    const cutoff = 1 - Math.exp(-2 * Math.PI * (7000 - 4500 * Math.abs(s)) / SR);
    const [near, far] = s >= 0 ? [R, L] : [L, R];
    let lp = 0;
    const F = feeds[k];
    for (let i = 0; i < N; i++) {
      near[i] += F[i] * nearGain * rear;
      lp += cutoff * (F[i] - lp);
      const j = i + itd;
      if (j < N) far[j] += lp * farGain * rear;
    }
  }
  writeStereoWav(binaural, L, R, N);
  tick('head: parametric');
}

if (flag('audio-only')) {
  spawnSync('cp', [binaural, outPath]);
  console.log(outPath);
  process.exit(0);
}

// ── 3 · the picture ──────────────────────────────────────────────────
const fontSrc = readFileSync(join(HERE, '../src/font-6x10.h'), 'utf8');
const GLYPHS = [...fontSrc.matchAll(/\{((?:0x[0-9A-F]{2},?\s*){10})\}/g)].map(m => m[1].match(/0x[0-9A-F]{2}/g).map(h => parseInt(h, 16)));
const frame = Buffer.alloc(W * H * 3);
const px = (x, y, r, g, b) => { if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3; frame[o] = r; frame[o + 1] = g; frame[o + 2] = b; };
const rect = (x, y, w, h, c) => { for (let j = y; j < y + h; j++) for (let i = x; i < x + w; i++) px(i | 0, j | 0, ...c); };
const circle = (cx, cy, r, c, fill = true) => {
  for (let j = -r; j <= r; j++) for (let i = -r; i <= r; i++) { const d = i * i + j * j; if (d <= r * r && (fill || d >= (r - 1.5) * (r - 1.5))) px((cx + i) | 0, (cy + j) | 0, ...c); }
};
const line = (x0, y0, x1, y1, c) => { const n = Math.max(1, Math.ceil(Math.hypot(x1 - x0, y1 - y0))); for (let i = 0; i <= n; i++) px((x0 + (x1 - x0) * i / n) | 0, (y0 + (y1 - y0) * i / n) | 0, ...c); };
const text = (s, x, y, c, scale = 1) => {
  for (let ci = 0; ci < s.length; ci++) {
    const g = GLYPHS[s.charCodeAt(ci) - 32];
    if (g) for (let r = 0; r < 10; r++) for (let b = 0; b < 6; b++) if (g[r] & (0x80 >> b)) rect(x + (ci * 6 + b) * scale, y + r * scale, scale, scale, c);
  }
};
const ascii = s => s.replace(/·/g, '-').replace(/—/g, '-').replace(/[^\x20-\x7e]/g, '');
const mix = (a, b, u) => a.map((v, i) => Math.round(v + (b[i] - v) * u));

const frames = Math.ceil((span + TAIL) * FPS), perFrame = SR / FPS;
const cursors = score.lanes.map(() => 0);
const cx = 170, cy = 190, R = 118;
const floor = (a, r = R) => [cx + Math.sin(a) * r, cy - Math.cos(a) * r];
const ff = spawn('ffmpeg', ['-y', '-v', 'error', '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-s', `${W}x${H}`, '-r', String(FPS), '-i', 'pipe:0',
  '-i', binaural, '-c:v', 'libx264', '-preset', 'veryfast', '-crf', '23', '-pix_fmt', 'yuv420p', '-c:a', 'aac', '-b:a', '192k', '-shortest', outPath], { stdio: ['pipe', 'inherit', 'inherit'] });
const write = buf => new Promise(res => ff.stdin.write(buf, () => res()) || ff.stdin.once('drain', res));

for (let f = 0; f < frames; f++) {
  const tRel = f / FPS, t = from + tRel;
  frame.fill(10, 0, W * H * 3);
  for (let i = 0; i < W * H; i++) frame[i * 3 + 2] = 14;
  // seats lit by their own feed's RMS in this frame
  const s0 = Math.floor(f * perFrame), s1 = Math.min(N, Math.floor((f + 1) * perFrame));
  circle(cx, cy, R, [60, 66, 82], false);
  for (let k = 0; k < SEATS; k++) {
    let acc = 0; const F = feeds[k];
    for (let i = s0; i < s1; i++) acc += F[i] * F[i];
    const rms = Math.sqrt(acc / Math.max(1, s1 - s0)) / norm; // undo the headroom scale so levels compare across renders
    const lvl = Math.min(1, Math.sqrt(rms) * 2.6);
    const [x, y] = floor(k / SEATS * Math.PI * 2);
    rect(x - 19, y - 14, 38, 28, mix([34, 38, 50], [255, 222, 110], lvl));
    rect(x - 22, y + 15, 44, 2, [110, 120, 140]);
    text(String(k + 1), x - 6, y - 10, lvl > .55 ? [20, 22, 30] : [225, 230, 240], 2);
  }
  circle(cx, cy, 5, [200, 210, 225]);
  text('YOU', cx - 9, cy + 9, [150, 160, 180]);
  line(cx, cy - 9, cx, cy - 22, [150, 160, 180]);
  // the sources: every lane with a sounding event, at its position in the ring
  score.lanes.forEach((lane, li) => {
    const ev = lane.events;
    while (cursors[li] < ev.length && ev[cursors[li]].t + ev[cursors[li]].dur < t) cursors[li]++;
    let on = false;
    for (let i = cursors[li]; i < ev.length && ev[i].t <= t; i++) if (ev[i].t + ev[i].dur > t) { on = true; break; }
    if (!on) return;
    const p = voicePosition(score, li, t), [x, y] = floor(p.angle, R * .72);
    circle(x, y, Number.isFinite(lane.az) ? 3 : 5, lane.color.map(c => Math.round(c * .9 + 25)));
  });
  // panel
  const X = 330;
  text(ascii(score.name).toUpperCase(), X, 24, [235, 238, 245], 2);
  const mv = (score.movements || []).find(m => t >= m.t0 && t < m.t1) || (score.movements || []).at(-1);
  if (mv) {
    text(ascii(mv.name), X, 62, [255, 222, 110], 2);
    const words = ascii(mv.sub || '').split(' '); let row = '', y = 90;
    for (const w of words) { if ((row + ' ' + w).length > 48) { text(row, X, y, [170, 180, 200]); y += 13; row = w; } else row = row ? row + ' ' + w : w; }
    text(row, X, y, [170, 180, 200]);
  }
  text(`${mmss(Math.min(t, score.dur))} / ${mmss(score.dur)}`, X, 150, [235, 238, 245], 2);
  text(`${score.bpm || ''} BPM  ring of ${SEATS}  ${useHrtf ? 'KEMAR HRTF' : 'parametric head'}`, X, 176, [120, 130, 150]);
  // timeline with section doors and the playhead
  const TX = X, TW = 290, TY = 300;
  (score.movements || []).forEach((m, i) => rect(TX + m.t0 / score.dur * TW, TY, Math.max(1, (m.t1 - m.t0) / score.dur * TW - 1), 10, i % 2 ? [48, 54, 70] : [62, 70, 90]));
  if (from > 0 || to < score.dur) rect(TX + from / score.dur * TW, TY - 4, Math.max(1, (to - from) / score.dur * TW), 2, [255, 222, 110]);
  rect(TX + Math.min(t, score.dur) / score.dur * TW, TY - 3, 2, 16, [255, 90, 90]);
  (score.movements || []).forEach(m => text(ascii(m.name).split(' ')[0], TX + m.t0 / score.dur * TW, TY + 14, [120, 130, 150]));
  await write(Buffer.from(frame));
}
ff.stdin.end();
await new Promise(res => ff.on('close', res));
tick(`video: ${frames} frames at ${FPS} fps`);
console.log(outPath);
