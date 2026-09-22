#!/usr/bin/env node
// notespatial-native-render — hear a ring .nsscore from the middle of the
// room, and watch the notation fly in.
//
// Sound: the laptop feeds are synthesized exactly as the spatial-rehearsal
// piece routes them (lib/spatial-rehearsal.mjs voicePosition + sourceGain,
// the native linear attack/decay envelope), then each feed is placed at
// its seat around a listener at the center: seat 1 ahead, numbers
// clockwise, a held laptop at the center if the score has one. Placement
// is measured KEMAR HRTF (tools/hrir, via ffmpeg afir) or, with --fast, a
// parametric head computed here.
//
// Picture: a radial timeline seen from above. Every note starts far
// outside the ring and flies inward along the radial of the laptop that
// will sound it, as a bar whose length is its duration, arriving on the
// machine at the moment it sounds and flowing into it; the held laptop's
// stream comes up from the bottom gap. Melodic notes carry their name
// and frequency. Each laptop box shows a miniature of its own screen:
// its notes approaching the front. Tempo, note rate and voice count read
// at the right.
//
//   node notespatial-native-render.mjs [score.nsscore] [--out x.mp4]
//        [--from 7:20] [--to 8:00] [--section 5] [--fps 30] [--size 960x540]
//        [--audio-only] [--fast] [--light | --dark]   (theme follows macOS unless given)
//        [--plan]   top-down instead of the isometric view

import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { voicePosition, sourceGain, ringSeats } from '../lib/spatial-rehearsal.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const FLAGS = ['audio-only', 'fast', 'light', 'dark', 'plan', 'iso'];
const flag = k => args.includes('--' + k);
const opt = (k, d) => { const i = args.indexOf('--' + k); return i >= 0 ? args[i + 1] : d; };
const positional = args.filter((a, i) => !a.startsWith('--') && !(i > 0 && args[i - 1].startsWith('--') && !FLAGS.includes(args[i - 1].slice(2))));
const scorePath = positional[0] || join(HERE, '../scores/notespatial-native.nsscore');
const score = JSON.parse(readFileSync(scorePath, 'utf8'));
const SEATS = score.seats || 6, CENTER = Number.isInteger(score.center) ? score.center : -1, RING = ringSeats(score, SEATS);
const ringIndex = k => k > CENTER && CENTER >= 0 ? k - 1 : k;
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;
const parseT = s => s === undefined ? undefined : s.includes(':') ? s.split(':').reduce((a, b) => a * 60 + +b, 0) : +s;

let from = parseT(opt('from')) ?? 0, to = parseT(opt('to')) ?? score.dur;
if (opt('section')) {
  const m = score.movements?.[+opt('section') - 1];
  if (!m) throw Error('no such section');
  from = m.t0; to = Math.min(score.dur, m.t1 + 1.5);
}
const FPS = +opt('fps', 30), [W, H] = opt('size', '960x540').split('x').map(Number);
const suffix = opt('section') ? '-' + opt('section') : (from || to !== score.dur) ? `-${mmss(from).replace(':', 'm')}-${mmss(to).replace(':', 'm')}` : '';
const outPath = resolve(opt('out', join(HERE, '../../../grants/culturehub-la-2026/notespatial-native-sim' + suffix + (flag('audio-only') ? '.wav' : '.mp4'))));
const work = join(tmpdir(), 'notespatial-render');
mkdirSync(work, { recursive: true });
const tick = (() => { let t = performance.now(); return label => { const n = performance.now(); console.log(`${label.padEnd(30)} ${((n - t) / 1000).toFixed(1)} s`); t = n; }; })();

// ── 1 · the feeds ────────────────────────────────────────────────────
const SR = 44100, TAIL = 1.5;
const span = to - from, N = Math.ceil((span + TAIL) * SR);
const feeds = Array.from({ length: SEATS }, () => new Float32Array(N));
let seed = 20260924;
const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296 * 2 - 1;
const BLOCK = 256;
let voiced = 0;
const gainsAt = (li, t) => { const p = voicePosition(score, li, t); return Array.from({ length: SEATS }, (_, k) => sourceGain(score, p, k, SEATS)); };
score.lanes.forEach((lane, li) => {
  const still = lane.center || (Number.isFinite(lane.az) && !score.fieldShift);
  const fixed = still ? gainsAt(li, 0) : null;
  for (const e of lane.events) {
    if (e.t + e.dur <= from || e.t >= to) continue;
    voiced++;
    const g = Math.min(.65, Math.max(0, e.g * (score.gain ?? .35)));
    const s0 = Math.round((e.t - from) * SR), len = Math.round(e.dur * SR);
    const attack = (e.attack ?? .01) * SR, decay = (e.decay ?? .06) * SR, decayStart = Math.max(0, len - decay);
    const inc = (e.hz || 220) / SR, wave = e.wave;
    let phase = 0;
    for (let b = 0; b < len; b += BLOCK) {
      const bl = Math.min(BLOCK, len - b), gk = fixed || gainsAt(li, e.t + b / SR);
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
let peak = 0;
for (const F of feeds) for (let i = 0; i < N; i += 7) peak = Math.max(peak, Math.abs(F[i]));
const norm = peak > 0 ? .5 / peak : 1;
for (const F of feeds) for (let i = 0; i < N; i++) F[i] *= norm;
tick(`feeds: ${voiced} events, ${mmss(span)}`);

// ── 2 · the head ─────────────────────────────────────────────────────
// Ring seats sit at their azimuth; a held center laptop is a small speaker
// just ahead of the listener, so it comes from straight in front, close.
const seatAzDeg = k => k === CENTER ? 0 : ringIndex(k) / RING * 360;
function writeStereoWav(path, L, R, n) {
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
  const ir = azDeg => {
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
    const [L, R] = ir(seatAzDeg(k));
    writeFileSync(join(work, `ir${k}L.f32`), Buffer.from(L.buffer));
    writeFileSync(join(work, `ir${k}R.f32`), Buffer.from(R.buffer));
    inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(work, `seat${k}.f32`));
  }
  for (let k = 0; k < SEATS; k++) for (const ear of 'LR') inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(work, `ir${k}${ear}.f32`));
  for (let k = 0; k < SEATS; k++) {
    const w = k === CENTER ? 1.25 : 1; // the small speaker is nearer than the ring
    graph.push(`[${k}:a]asplit[s${k}a][s${k}b]`);
    graph.push(`[s${k}a][${SEATS + k * 2}:a]afir=gtype=none:dry=1:wet=${w}[l${k}]`);
    graph.push(`[s${k}b][${SEATS + k * 2 + 1}:a]afir=gtype=none:dry=1:wet=${w}[r${k}]`);
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
  const L = new Float32Array(N), R = new Float32Array(N);
  for (let k = 0; k < SEATS; k++) {
    const az = seatAzDeg(k) * Math.PI / 180, s = Math.sin(az), c = Math.cos(az);
    const itd = Math.round(Math.abs(s) * .00066 * SR);
    const farGain = 10 ** (-(4 * Math.abs(s)) / 20), rear = c < 0 ? .78 : 1, near = k === CENTER ? 1.25 : 1;
    const cutoff = 1 - Math.exp(-2 * Math.PI * (7000 - 4500 * Math.abs(s)) / SR);
    const [nearEar, farEar] = s >= 0 ? [R, L] : [L, R];
    let lp = 0;
    const F = feeds[k];
    for (let i = 0; i < N; i++) {
      nearEar[i] += F[i] * rear * near;
      lp += cutoff * (F[i] - lp);
      const j = i + itd;
      if (j < N) farEar[j] += lp * farGain * rear * near;
    }
  }
  writeStereoWav(binaural, L, R, N);
  tick('head: parametric');
}
if (flag('audio-only')) { spawnSync('cp', [binaural, outPath]); console.log(outPath); process.exit(0); }

// ── 3 · the picture ──────────────────────────────────────────────────
const fontSrc = readFileSync(join(HERE, '../src/font-6x10.h'), 'utf8');
const GLYPHS = [...fontSrc.matchAll(/\{((?:0x[0-9A-F]{2},?\s*){10})\}/g)].map(m => m[1].match(/0x[0-9A-F]{2}/g).map(h => parseInt(h, 16)));
const frame = Buffer.alloc(W * H * 3);
const px = (x, y, r, g, b) => { if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3; frame[o] = r; frame[o + 1] = g; frame[o + 2] = b; };
const blend = (x, y, c, a) => { if (x < 0 || y < 0 || x >= W || y >= H) return; const o = ((y | 0) * W + (x | 0)) * 3; frame[o] += (c[0] - frame[o]) * a; frame[o + 1] += (c[1] - frame[o + 1]) * a; frame[o + 2] += (c[2] - frame[o + 2]) * a; };
const rect = (x, y, w, h, c) => { for (let j = y | 0; j < (y + h) | 0; j++) for (let i = x | 0; i < (x + w) | 0; i++) px(i, j, ...c); };
const disc = (cx, cy, r, c, a = 1) => { const R2 = r * r; for (let j = -r; j <= r; j++) for (let i = -r; i <= r; i++) if (i * i + j * j <= R2) blend(cx + i, cy + j, c, a); };
const ring = (cx, cy, r, c, a = 1) => { const n = Math.max(24, Math.ceil(r * 5)); for (let i = 0; i < n; i++) { const t = i / n * Math.PI * 2; blend(cx + Math.cos(t) * r, cy + Math.sin(t) * r, c, a); } };
const line = (x0, y0, x1, y1, c, a = 1) => { const n = Math.max(1, Math.ceil(Math.hypot(x1 - x0, y1 - y0))); for (let i = 0; i <= n; i++) blend(x0 + (x1 - x0) * i / n, y0 + (y1 - y0) * i / n, c, a); };
const text = (s, x, y, c, scale = 1) => {
  for (let ci = 0; ci < s.length; ci++) {
    const g = GLYPHS[s.charCodeAt(ci) - 32];
    if (g) for (let r = 0; r < 10; r++) for (let b = 0; b < 6; b++) if (g[r] & (0x80 >> b)) rect(x + (ci * 6 + b) * scale, y + r * scale, scale, scale, c);
  }
};
const ascii = s => s.replace(/·/g, '-').replace(/—/g, '-').replace(/[^\x20-\x7e]/g, '');
const mix = (a, b, u) => a.map((v, i) => Math.round(v + (b[i] - v) * u));
const NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const noteOf = hz => { const m = Math.round(69 + 12 * Math.log2(hz / 440)); return NAMES[m % 12] + (Math.floor(m / 12) - 1); };
const MELODIC = /voice|walk|theme|answer|top/;

// geometry: ring centered a little left, panel at the right
// Isometric by default: the floor is an ellipse seen from a raised seat
// behind the audience, streams run along the floor to each laptop, and the
// held laptop's stream drops straight down from above into the hands. The
// plan is turned half a step so the vertical axis falls in a gap.
const ISO = !flag('plan');
const cx = Math.round(W * .38), cy = Math.round(H * (ISO ? .6 : .52)), R = Math.round(H * (ISO ? .3 : .24)), R_OUT = Math.round(H * (ISO ? .62 : .5)) + 8, LOOK = 3.2;
const SQ = ISO ? .5 : 1, ROT = ISO ? Math.PI / RING : 0;
const floor = (a, r, h = 0) => [cx + Math.sin(a + ROT) * r, cy - Math.cos(a + ROT) * r * SQ - h];
const seatAngle = k => ringIndex(k) / RING * Math.PI * 2;
const CENTER_STREAM = Math.PI; // in plan view the held laptop's stream comes up from the bottom gap
const CENTER_DOCK = ISO ? 16 : 18, CENTER_TOP = ISO ? cy - 28 : null;
// a point `r` along a stream: ring streams lie on the floor, the center stream stands up
// the performer wanders a little with the held laptop; the center stream follows the hands
let hx = cx, hy = cy;
const wander = t => { hx = cx + (ISO ? 26 : 22) * Math.sin(t * .13) + 9 * Math.sin(t * .31 + 2); hy = cy + (ISO ? 9 : 12) * Math.sin(t * .17 + 1) + 4 * Math.sin(t * .41); };
const streamPoint = (pos, r) => pos.center ? (ISO ? [hx, hy - 14 - (r - CENTER_DOCK) * .78] : floor(CENTER_STREAM, r).map((v, i) => v + (i ? hy - cy : hx - cx))) : floor(pos.angle, r);
function figure(x, y, ink) { // a stick figure holding the laptop at chest height
  const headY = y - 34, hipY = y - 6, footY = y + 12;
  ring(x, headY, 4, ink); disc(x, headY, 3, ink, .35);
  bar(x, headY + 4, x, hipY, 1.5, ink, 1);
  bar(x, hipY, x - 6, footY, 1.5, ink, 1); bar(x, hipY, x + 6, footY, 1.5, ink, 1);
  bar(x, headY + 10, x - 14, headY + 20, 1.5, ink, 1); bar(x, headY + 10, x + 14, headY + 20, 1.5, ink, 1);
  return [x, headY + 20]; // where the hands are
}
// theme: light or dark; follows the system appearance unless told
const systemDark = (() => { try { return spawnSync('defaults', ['read', '-g', 'AppleInterfaceStyle'], { encoding: 'utf8' }).stdout.trim() === 'Dark'; } catch { return true; } })();
const LIGHT = flag('light') || (!flag('dark') && !systemDark);
const T = LIGHT
  ? { bg: [247, 245, 239], ink: [28, 30, 38], dim: [118, 122, 136], accent: [176, 108, 0], ringLine: [150, 154, 168], box: [226, 223, 214], boxTint: .22, seg: [[214, 211, 202], [200, 197, 188]], play: [220, 60, 60], streamTint: .18, shade: .82 }
  : { bg: [9, 9, 13], ink: [235, 238, 245], dim: [120, 130, 150], accent: [255, 222, 110], ringLine: [70, 76, 96], box: [30, 34, 46], boxTint: .18, seg: [[62, 70, 90], [48, 54, 70]], play: [255, 90, 90], streamTint: .22, shade: 1 };
const tone = c => c.map(v => Math.round(v * T.shade)); // seat colors sit a little darker on paper
const frames = Math.ceil((span + TAIL) * FPS), perFrame = SR / FPS, dt = 1 / FPS;
const all = score.lanes.flatMap((l, i) => l.events.map(e => ({ ...e, lane: i }))).sort((a, b) => a.t - b.t);
let lo = 0; // sliding window over `all`
const tempoAt = t => { let b = null; for (const x of score.tempo || []) { if (x.t <= t) b = x.bpm; else break; } return b; };
// anti-aliased thick segment
function bar(x0, y0, x1, y1, w, c, a) {
  const minx = Math.floor(Math.min(x0, x1) - w), maxx = Math.ceil(Math.max(x0, x1) + w), miny = Math.floor(Math.min(y0, y1) - w), maxy = Math.ceil(Math.max(y0, y1) + w);
  const dx = x1 - x0, dy = y1 - y0, L2 = dx * dx + dy * dy || 1e-6, hw = w / 2;
  for (let y = Math.max(0, miny); y <= Math.min(H - 1, maxy); y++) for (let x = Math.max(0, minx); x <= Math.min(W - 1, maxx); x++) {
    const u = Math.max(0, Math.min(1, ((x + .5 - x0) * dx + (y + .5 - y0) * dy) / L2));
    const d = Math.hypot(x + .5 - (x0 + dx * u), y + .5 - (y0 + dy * u));
    const cov = Math.max(0, Math.min(1, hw + .5 - d));
    if (cov > 0) blend(x, y, c, a * cov);
  }
}
const light = new Array(SEATS).fill(0), flash = new Array(SEATS).fill(0), landed = new Set();
const SEAT_COLORS = score.seatColors || Array.from({ length: SEATS }, (_, k) => k === CENTER ? [255, 240, 200] : [[255, 110, 110], [255, 180, 70], [120, 220, 130], [95, 170, 255], [200, 130, 255], [110, 220, 230]][ringIndex(k) % 6]);
// a note wears the color of where it lands; between two seats it blends by routed power
const destColor = (li, t) => { const gk = gainsAt(li, t); let c = [0, 0, 0], sum = 0; for (let k = 0; k < SEATS; k++) { const w = gk[k] * gk[k]; if (w > 0) { sum += w; c = c.map((v, i) => v + SEAT_COLORS[k][i] * w); } } return sum > 0 ? c.map(v => Math.round(v / sum)) : [200, 200, 200]; };
const ff = spawn('ffmpeg', ['-y', '-v', 'error', '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-s', `${W}x${H}`, '-r', String(FPS), '-i', 'pipe:0',
  '-i', binaural, '-c:v', 'libx264', '-preset', 'veryfast', '-crf', '21', '-pix_fmt', 'yuv420p', '-c:a', 'aac', '-b:a', '192k', '-shortest', outPath], { stdio: ['pipe', 'inherit', 'inherit'] });
const write = buf => new Promise(res => ff.stdin.write(buf, () => res()) || ff.stdin.once('drain', res));

for (let f = 0; f < frames; f++) {
  const t = from + f / FPS;
  wander(t);
  for (let i = 0; i < W * H; i++) { const o = i * 3; frame[o] = T.bg[0]; frame[o + 1] = T.bg[1]; frame[o + 2] = T.bg[2]; }
  // faint streams and the two circles
  for (let k = 0; k < SEATS; k++) { const pos = k === CENTER ? { center: true } : { angle: seatAngle(k) }, r0 = k === CENTER ? CENTER_DOCK : R + 18; const [x0, y0] = streamPoint(pos, r0), [x1, y1] = streamPoint(pos, R_OUT); bar(x0, y0, x1, y1, 1, mix(T.bg, SEAT_COLORS[k], T.streamTint), 1); }
  { const n = 160; for (let i = 0; i < n; i++) { const [x0, y0] = floor(i / n * Math.PI * 2, R), [x1, y1] = floor((i + 1) / n * Math.PI * 2, R); bar(x0, y0, x1, y1, 1, T.ringLine, .9); } }
  // seat levels from the feeds, smoothed: fast up, slow down
  const s0 = Math.floor(f * perFrame), s1 = Math.min(N, Math.floor((f + 1) * perFrame));
  for (let k = 0; k < SEATS; k++) {
    let acc = 0; const F = feeds[k]; for (let i = s0; i < s1; i++) acc += F[i] * F[i];
    const inst = Math.min(1, Math.sqrt(Math.sqrt(acc / Math.max(1, s1 - s0)) / norm) * 2.6);
    light[k] = inst > light[k] ? inst : light[k] + (inst - light[k]) * (1 - Math.exp(-dt / .22));
    flash[k] *= Math.exp(-dt / .18);
  }
  // the notes in flight
  while (lo < all.length && all[lo].t + all[lo].dur + .5 < t) { landed.delete(all[lo]); lo++; }
  let sounding = 0, recent = 0;
  const labels = new Map();
  const speed = (R_OUT - R - 6) / LOOK; // px per second along a stream
  for (let i = lo; i < all.length && all[i].t <= t + LOOK; i++) {
    const e = all[i], lane = score.lanes[e.lane];
    const until = e.t - t, end = until + e.dur;
    if (until <= 0 && end > 0) sounding++;
    if (until <= 0 && until > -2) recent++;
    if (end <= 0) continue;
    if (until <= 0 && !landed.has(e)) { landed.add(e); const gk = gainsAt(e.lane, e.t); for (let k = 0; k < SEATS; k++) if (gk[k] * gk[k] >= .5) flash[k] = Math.min(1, flash[k] + Math.sqrt(e.g) * 1.6); }
    const pos = voicePosition(score, e.lane, e.t), color = tone(destColor(e.lane, e.t));
    const a = pos.center ? CENTER_STREAM : pos.angle, dock = pos.center ? CENTER_DOCK : R + 6;
    const head = dock + Math.max(0, until) * speed, tail = Math.min(R_OUT + 20, dock + end * speed);
    if (tail <= head + .5) continue;
    const [x0, y0] = streamPoint(pos, head), [x1, y1] = streamPoint(pos, tail);
    const w = Math.max(1.5, Math.sqrt(e.g) * 5.5);
    const born = Math.min(1, (LOOK - until) / .45); // fade in as it enters the field
    const alpha = (until > 0 ? .3 + .6 * (1 - until / LOOK) : .95) * born;
    bar(x0, y0, x1, y1, w, color, alpha);
    if (until <= 0) disc(x0 | 0, y0 | 0, Math.round(w * .9 + 1.5), color, .8);
    if (e.note && (pos.center || MELODIC.test(lane.name)) && until > 0 && until < LOOK * .85) {
      // one label per stream: the next note to land
      const key = Math.round(a * 100), prev = labels.get(key);
      if (!prev || until < prev.until) {
        const [lx, ly] = streamPoint(pos, head + w + 6);
        const side = pos.center ? 1 : Math.sin(a + ROT) >= -0.05 ? 1 : -1, s = `${e.note} ${Math.round(e.hz)}`;
        labels.set(key, { until, x: side > 0 ? lx + w + 5 : lx - w - 5 - s.length * 6, y: ly - 5, s, color, alpha: Math.min(1, (LOOK * .85 - until) / .4) });
      }
    }
  }
  for (const l of labels.values()) text(l.s, l.x | 0, l.y | 0, mix(T.bg, mix(T.dim, l.color, .75), Math.max(0, Math.min(1, l.alpha))));
  // the machines
  const order = Array.from({ length: SEATS }, (_, k) => k).sort((p, q) => (p === CENTER ? hy : floor(seatAngle(p), R)[1]) - (q === CENTER ? hy : floor(seatAngle(q), R)[1]));
  for (const k of order) {
    const isC = k === CENTER;
    let [x, y] = isC ? [hx, hy] : floor(seatAngle(k), R);
    if (isC) { const [fx, fy] = figure(hx, hy + 14, mix(T.ink, T.dim, .3)); x = fx; y = fy - 6; }
    const bw = isC ? 30 : 46, bh = isC ? 20 : 32, lit = light[k];
    const sc = tone(SEAT_COLORS[k]);
    if (flash[k] > .02) for (let o = 1; o <= 4; o++) { const al = flash[k] * (1 - o / 5) * .5; bar(x - bw / 2 - o, y - bh / 2 - o, x + bw / 2 + o, y - bh / 2 - o, 1, sc, al); bar(x - bw / 2 - o, y + bh / 2 + o, x + bw / 2 + o, y + bh / 2 + o, 1, sc, al); bar(x - bw / 2 - o, y - bh / 2 - o, x - bw / 2 - o, y + bh / 2 + o, 1, sc, al); bar(x + bw / 2 + o, y - bh / 2 - o, x + bw / 2 + o, y + bh / 2 + o, 1, sc, al); }
    rect(x - bw / 2, y - bh / 2, bw, bh, mix(mix(T.box, sc, T.boxTint), sc, lit));
    rect(x - bw / 2 - 3, y + bh / 2 + 1, bw + 6, 2, T.dim);
    // the machine's own screen: its notes approaching the front edge
    for (let i = lo; i < all.length && all[i].t <= t + LOOK; i++) {
      const e = all[i]; if (e.t < t) continue;
      const g = sourceGain(score, voicePosition(score, e.lane, e.t), k, SEATS);
      if (g * g < .5) continue;
      const u = (e.t - t) / LOOK, midi = e.hz > 0 ? 69 + 12 * Math.log2(e.hz / 440) : 72;
      const sx = x + Math.max(-1, Math.min(1, (midi - 72) / 24)) * (bw * .4) * (.3 + .7 * (1 - u));
      const sy = y - bh / 2 + 3 + (1 - u) * (bh - 6);
      blend(sx, sy, lit > .5 ? T.bg : sc, .9);
    }
    if (isC) text('C', x - 3, y - 5, lit > .55 ? T.bg : T.ink, 1); else text(String(k + 1), x - 6, y - 10, lit > .55 ? T.bg : T.ink, 2);
  }
  // panel
  const X = Math.round(W * .68), col0 = T.ink, dim = T.dim;
  text(ascii(score.name).toUpperCase(), X, 26, col0, 2);
  const mv = (score.movements || []).find(m => t >= m.t0 && t < m.t1) || (score.movements || []).at(-1);
  if (mv) {
    text(ascii(mv.name), X, 64, T.accent, 2);
    const words = ascii(mv.sub || '').split(' '); let row = '', y = 92;
    for (const w of words) { if ((row + ' ' + w).length > Math.floor((W - X - 16) / 6)) { text(row, X, y, mix(T.ink, T.dim, .4)); y += 13; row = w; } else row = row ? row + ' ' + w : w; }
    text(row, X, y, mix(T.ink, T.dim, .4));
  }
  text(`${mmss(Math.min(Math.max(0, t), score.dur))} / ${mmss(score.dur)}`, X, 156, col0, 2);
  const bpm = tempoAt(t);
  text(bpm ? `${bpm} BPM` : 'FREE TIME', X, 190, T.accent, 2);
  text(`${(recent / 2).toFixed(1).padStart(5)} notes/s   ${String(sounding).padStart(2)} voices`, X, 214, dim);
  text(`ring of ${RING}${CENTER >= 0 ? ' + held center' : ''}   ${useHrtf ? 'KEMAR HRTF' : 'parametric head'}`, X, 230, dim);
  for (let k = 0; k < SEATS; k++) { const isC = k === CENTER; disc(X + 4, 262 + k * 13, 3, tone(SEAT_COLORS[k])); text(isC ? 'C   held, center, small speaker' : `${k + 1}   ${ringIndex(k) === 0 ? 'front' : 'at ' + Math.round(ringIndex(k) / RING * 360) + ' deg'}`, X + 14, 257 + k * 13, dim); }
  // timeline
  const TX = X, TW = W - X - 16, TY = H - 40;
  (score.movements || []).forEach((m, i) => rect(TX + m.t0 / score.dur * TW, TY, Math.max(1, (m.t1 - m.t0) / score.dur * TW - 1), 10, T.seg[i % 2]));
  if (from > 0 || to < score.dur) rect(TX + from / score.dur * TW, TY - 4, Math.max(1, (to - from) / score.dur * TW), 2, T.accent);
  bar(TX + Math.min(Math.max(0, t), score.dur) / score.dur * TW, TY - 3, TX + Math.min(Math.max(0, t), score.dur) / score.dur * TW, TY + 13, 2, T.play, 1);
  (score.movements || []).forEach(m => text(ascii(m.name).split(' ')[0], TX + m.t0 / score.dur * TW, TY + 14, dim));
  await write(Buffer.from(frame));
}
ff.stdin.end();
await new Promise(res => ff.on('close', res));
tick(`video: ${frames} frames at ${FPS} fps`);
console.log(outPath);
