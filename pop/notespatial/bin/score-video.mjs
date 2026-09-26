#!/usr/bin/env node
// score-video.mjs — an mp4 score of climbalift, for analysis: every event the
// composer logged, drawn as a scrolling piano roll (one row per layer, colored
// by seat, pitch as height), the form ribbon with the level arc and the
// silences, the ring lit by what is sounding, and the master's waveform, all
// synced to the mastered audio.
//
//   node pop/notespatial/bin/score-video.mjs --score out/climbalift-score.json \
//        --audio out/climbalift-v8.flac --out out/climbalift-v8-score.mp4 [--fps 30] [--size 1280x720]
//
// No dependencies beyond ffmpeg: frames are rasterized here (rects, lines and
// the 6×10 X11 bitmap font from pop/bin/finalize.mjs) and piped to libx264.

import { readFileSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';
import { resolve } from 'node:path';

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf('--' + k); return i >= 0 ? args[i + 1] : d; };
const score = JSON.parse(readFileSync(resolve(opt('score')), 'utf8'));
const AUDIO = resolve(opt('audio')), OUT = resolve(opt('out', 'score.mp4'));
const FPS = +opt('fps', 30), [W, H] = opt('size', '1280x720').split('x').map(Number);

// ── audio for the waveform: 4 kHz mono ─────────────────────────────────
const ASR = 4000;
const dec = spawnSync('ffmpeg', ['-v', 'error', '-i', AUDIO, '-f', 'f32le', '-ac', '1', '-ar', String(ASR), '-'], { maxBuffer: 2 ** 31 - 1 });
const audio = new Float32Array(dec.stdout.buffer, dec.stdout.byteOffset, dec.stdout.length / 4);
const DUR = audio.length / ASR;
let apk = 0; for (const v of audio) apk = Math.max(apk, Math.abs(v));

// ── the font (public-domain X11 6×10, as in pop/bin/finalize.mjs) ───────
const FONT_W = 6, FONT_H = 10;
const FONT = Buffer.from(
  'AAAAAAAAAAAAAAAgICAgIAAgAAAAUFBQAAAAAAAAAFBQ+FD4UFAAAAAgcKBwKHAgAAAASKhQIFCo' +
  'kAAAAECgoECokGgAAAAgICAAAAAAAAAAECBAQEAgEAAAAEAgEBAQIEAAAAAAiFD4UIgAAAAAACAg' +
  '+CAgAAAAAAAAAAAAMCBAAAAAAAD4AAAAAAAAAAAAAAAgcCAAAAgIECBAgIAAAAAgUIiIiFAgAAAA' +
  'IGCgICAg+AAAAHCICDBAgPgAAAD4CBAwCIhwAAAAEDBQkPgQEAAAAPiAsMgIiHAAAAAwQICwyIhw' +
  'AAAA+AgQECBAQAAAAHCIiHCIiHAAAABwiJhoCBBgAAAAACBwIAAgcCAAAAAgcCAAMCBAAAAIECBA' +
  'IBAIAAAAAAD4APgAAAAAAEAgEAgQIEAAAABwiBAgIAAgAAAAcIiYqLCAcAAAACBQiIj4iIgAAADw' +
  'SEhwSEjwAAAAcIiAgICIcAAAAPBISEhISPAAAAD4gIDwgID4AAAA+ICA8ICAgAAAAHCIgICYiHAA' +
  'AACIiIj4iIiIAAAAcCAgICAgcAAAADgQEBAQkGAAAACIkKDAoJCIAAAAgICAgICA+AAAAIiI2KiI' +
  'iIgAAACIiMiomIiIAAAAcIiIiIiIcAAAAPCIiPCAgIAAAABwiIiIiKhwCAAA8IiI8KCQiAAAAHCI' +
  'gHAIiHAAAAD4ICAgICAgAAAAiIiIiIiIcAAAAIiIiFBQUCAAAACIiIioqNiIAAAAiIhQIFCIiAAA' +
  'AIiIUCAgICAAAAD4CBAgQID4AAAAcEBAQEBAcAAAAICAQCAQCAgAAABwEBAQEBBwAAAAIFCIAAAA' +
  'AAAAAAAAAAAAAAD4ACAQAAAAAAAAAAAAAABwCHiIeAAAAICAsMiIyLAAAAAAAHCIgIhwAAAACAho' +
  'mIiYaAAAAAAAcIj4gHAAAAAwSEDwQEBAAAAAAAB4iIh4CIhwAICAsMiIiIgAAAAgAGAgICBwAAAA' +
  'CAAYCAgISEgwAICAiJDgkIgAAABgICAgICBwAAAAAADQqKioiAAAAAAAsMiIiIgAAAAAAHCIiIhw' +
  'AAAAAACwyIjIsICAAAAAaJiImGgICAAAALDIgICAAAAAAABwgHAI8AAAAEBA8EBASDAAAAAAAIiI' +
  'iJhoAAAAAACIiFBQIAAAAAAAiIioqFAAAAAAAIhQIFCIAAAAAACIiJhoCIhwAAAA+BAgQPgAAAAY' +
  'IBBgECAYAAAAICAgICAgIAAAAGAQIBggEGAAAABIqJAAAAAAAAA=', 'base64');
const glyph = (ch, x, y) => { const c = ch.charCodeAt(0); if (c < 32 || c > 126) return false; const row = FONT[(c - 32) * FONT_H + y]; return row !== undefined && (row & (0x80 >> x)) !== 0; };

// ── raster ──────────────────────────────────────────────────────────────
const frame = new Uint8Array(W * H * 3);
const px = (x, y, r, g, b, a = 1) => { if (x < 0 || y < 0 || x >= W || y >= H) return; const i = (y * W + x) * 3; if (a >= 1) { frame[i] = r; frame[i + 1] = g; frame[i + 2] = b; } else { frame[i] += (r - frame[i]) * a; frame[i + 1] += (g - frame[i + 1]) * a; frame[i + 2] += (b - frame[i + 2]) * a; } };
const rect = (x, y, w, h, [r, g, b], a = 1) => { x = Math.round(x); y = Math.round(y); w = Math.round(w); h = Math.round(h); for (let j = Math.max(0, y); j < Math.min(H, y + h); j++) for (let i = Math.max(0, x); i < Math.min(W, x + w); i++) px(i, j, r, g, b, a); };
const line = (x0, y0, x1, y1, [r, g, b], a = 1) => { x0 = Math.round(x0); y0 = Math.round(y0); x1 = Math.round(x1); y1 = Math.round(y1); const dx = Math.abs(x1 - x0), dy = -Math.abs(y1 - y0), sx = x0 < x1 ? 1 : -1, sy = y0 < y1 ? 1 : -1; let e = dx + dy; for (;;) { px(x0, y0, r, g, b, a); if (x0 === x1 && y0 === y1) break; const e2 = 2 * e; if (e2 >= dy) { e += dy; x0 += sx; } if (e2 <= dx) { e += dx; y0 += sy; } } };
const text = (s, x, y, scale, [r, g, b], a = 1) => { for (let k = 0; k < s.length; k++) for (let gy = 0; gy < FONT_H; gy++) for (let gx = 0; gx < FONT_W; gx++) if (glyph(s[k], gx, gy)) rect(x + (k * FONT_W + gx) * scale, y + gy * scale, scale, scale, [r, g, b], a); };
const textW = (s, scale) => s.length * FONT_W * scale;

// ── colors ──────────────────────────────────────────────────────────────
const SEAT = [[255, 110, 110], [255, 180, 70], [120, 220, 130], [95, 170, 255], [200, 130, 255], [255, 240, 200], [170, 170, 170]];
const SEATNAME = ['1 front', '2', '3', '4', '5', 'C', 'SUB'];
const BG = [12, 12, 16], GRID = [40, 40, 48], DIM = [120, 120, 130], INK = [235, 235, 240];
const LAYERS = ['lead', 'voice', 'ghost', 'walk', 'guitar', 'pad', 'wub', 'bass', 'sub', 'drums', 'fx', 'bed'];
const SECCOL = ['#7a6cff', '#5aa9ff', '#4fd1c5', '#ffd166', '#6b6b7a', '#5aa9ff', '#4fd1c5', '#ff6b6b', '#6b6b7a', '#4fd1c5', '#ff3d7f', '#8899aa'].map(h => [1, 3, 5].map(i => parseInt(h.slice(i, i + 2), 16)));

// ── geometry ────────────────────────────────────────────────────────────
const TOP = 56, ROLL_Y = 78, ROLL_H = 516, ROW_H = ROLL_H / LAYERS.length, LABEL_W = 78, PLAY_X = LABEL_W + 300, PPS = 72; // pixels per second
const t2x = (t, now) => PLAY_X + (t - now) * PPS;
const tMin = now => now - (PLAY_X - LABEL_W) / PPS, tMax = now => now + (W - PLAY_X) / PPS;
const BOT_Y = ROLL_Y + ROLL_H + 8;

// events sorted by time; [layer, seat, t, dur, hz, gain, kind]
const EV = score.events.slice().sort((a, b) => a[2] - b[2]);
const rowOf = Object.fromEntries(LAYERS.map((l, i) => [l, i]));
const FORM = score.form, SEC = score.sec, MUTES = score.mutes, ARC = score.arc, TR = score.tr || {};
const secAt = t => { for (const [n] of FORM) { const s = SEC[n]; if (t >= s.t && t < s.end) return s; } return null; };
const arcAt = t => { const s = secAt(t); if (!s) return .35; const [a, b] = ARC[s.name]; return a + (b - a) * (t - s.t) / (s.end - s.t); };
const lower = t => { let lo = 0, hi = EV.length; while (lo < hi) { const m = (lo + hi) >> 1; if (EV[m][2] < t) lo = m + 1; else hi = m; } return lo; };
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}.${String(Math.floor((s % 1) * 10))}`;

// ── one frame ───────────────────────────────────────────────────────────
function draw(now) {
  frame.fill(0); rect(0, 0, W, H, BG);
  const s = secAt(now), tr = s ? (TR[s.name] || 0) : 0;
  // header
  text('CLIMBALIFT', 12, 10, 3, INK); text(mmss(now), 12, 40, 1, DIM);
  if (s) { const bar = Math.floor((now - s.t) / s.barLen) + 1, beat = Math.floor(((now - s.t) % s.barLen) / s.beat) + 1; text(`${s.name.toUpperCase()}  bar ${bar}/${s.bars}  beat ${beat}  ${s.bpm} BPM  key +${tr}  arc ${arcAt(now).toFixed(2)}`, 230, 12, 2, INK); }
  // form ribbon with the arc and the silences
  const rx = 230, rw = W - rx - 12, ry = 34, rh = 16;
  FORM.forEach(([n], i) => { const sc = SEC[n]; const x0 = rx + sc.t / DUR * rw, x1 = rx + sc.end / DUR * rw; rect(x0, ry, x1 - x0 - 1, rh, SECCOL[i], .35); const [a, b] = ARC[n]; line(x0, ry + rh - a * rh, x1, ry + rh - b * rh, SECCOL[i], 1); if (x1 - x0 > 30) text(n, x0 + 2, ry + rh + 2, 1, DIM); });
  for (const [a, b] of MUTES) { const x0 = rx + a / DUR * rw, x1 = rx + b / DUR * rw; rect(x0, ry, Math.max(1, x1 - x0), rh, [0, 0, 0], .7); }
  rect(rx + now / DUR * rw, ry - 2, 2, rh + 4, INK);
  // the roll: grid of bars and section lines
  const t0 = tMin(now), t1 = tMax(now);
  rect(LABEL_W, ROLL_Y, W - LABEL_W, ROLL_H, [16, 16, 22]);
  for (const [n] of FORM) { const sc = SEC[n]; for (let b = 0; b < sc.bars; b++) { const t = sc.t + b * sc.barLen; if (t < t0 || t > t1) continue; const x = t2x(t, now); rect(x, ROLL_Y, 1, ROLL_H, b === 0 ? [90, 90, 110] : GRID, b === 0 ? 1 : .8); if (b === 0) text(n, x + 4, ROLL_Y + 2, 2, [150, 150, 170]); if (b % 4 === 0 && b) text(String(b + 1), x + 2, ROLL_Y + ROLL_H - 12, 1, GRID); } }
  for (const [a, b] of MUTES) { if (b < t0 || a > t1) continue; rect(t2x(a, now), ROLL_Y, Math.max(1, (b - a) * PPS), ROLL_H, [0, 0, 0], .55); }
  LAYERS.forEach((l, i) => { const y = ROLL_Y + i * ROW_H; rect(0, y, W, 1, GRID); rect(0, y, LABEL_W, ROW_H, [20, 20, 26]); text(l, 6, y + ROW_H / 2 - 5, 2, DIM); });
  // events in the window; count what sounds now per seat and per layer
  const seatNow = new Float64Array(7), layerNow = new Float64Array(LAYERS.length);
  for (let k = lower(t0 - 12); k < EV.length; k++) {
    const [layer, seat, t, d, f, g, kind] = EV[k]; if (t > t1) break; if (t + d < t0) continue;
    const row = rowOf[layer]; if (row === undefined) continue;
    const y0 = ROLL_Y + row * ROW_H, live = t <= now && now < t + d;
    if (live) { seatNow[seat] += g; layerNow[row] += g; }
    const h = f > 0 ? Math.max(0, Math.min(1, Math.log2(f / 40) / Math.log2(6000 / 40))) : null;
    const yy = h === null ? y0 + ROW_H * (.35 + .3 * ((kind.length * 7) % 5) / 5) : y0 + ROW_H - 4 - h * (ROW_H - 8);
    const x = t2x(t, now), w = Math.max(2, d * PPS), col = SEAT[seat] || DIM, a = Math.min(1, .3 + g * 1.2);
    rect(x, yy - 1.5, w, 3, col, live ? 1 : a * .7); if (live) rect(x, yy - 3, w, 6, col, .35);
  }
  // playhead
  rect(PLAY_X, ROLL_Y, 2, ROLL_H, [255, 255, 255], .9);
  // the ring, lit by what sounds now
  const cx = 130, cy = BOT_Y + 54, rxr = 96, ryr = 40;
  text('the ring', 12, BOT_Y, 1, DIM);
  for (let k = 0; k < 5; k++) { const ang = -Math.PI / 2 + k / 5 * Math.PI * 2, x = cx + Math.cos(ang) * rxr, y = cy + Math.sin(ang) * ryr, lit = Math.min(1, seatNow[k] * .8); rect(x - 9, y - 6, 18, 12, SEAT[k], .15 + .85 * lit); text(String(k + 1), x - 3, y - 5, 1, lit > .5 ? [0, 0, 0] : DIM); }
  { const lit = Math.min(1, seatNow[5] * .8); rect(cx - 7, cy - 5, 14, 10, SEAT[5], .15 + .85 * lit); text('C', cx - 3, cy - 5, 1, lit > .5 ? [0, 0, 0] : DIM); }
  { const lit = Math.min(1, seatNow[6] * .8); rect(cx - 12, cy + ryr + 14, 24, 8, SEAT[6], .15 + .85 * lit); text('SUB', cx - 9, cy + ryr + 13, 1, lit > .5 ? [0, 0, 0] : DIM); }
  // layers sounding now, as a bar chart
  LAYERS.forEach((l, i) => { const v = Math.min(1, layerNow[i] * .6); rect(260 + i * 30, BOT_Y + 100 - v * 84, 22, v * 84, [90, 200, 220], .8); text(l.slice(0, 4), 260 + i * 30, BOT_Y + 104, 1, DIM); });
  text('sounding now', 260, BOT_Y, 1, DIM);
  // the master's waveform over the same window, and its momentary level
  const wx = 640, ww = W - wx - 12, wy = BOT_Y + 14, wh = 84;
  rect(wx, wy, ww, wh, [16, 16, 22]); text('master', wx, BOT_Y, 1, DIM);
  for (let i = 0; i < ww; i++) { const t = t0 + (t1 - t0) * (i / ww); const a0 = Math.floor(t * ASR), a1 = Math.floor((t + (t1 - t0) / ww) * ASR); if (a0 < 0 || a1 >= audio.length) continue; let pk = 0; for (let j = a0; j <= a1; j++) pk = Math.max(pk, Math.abs(audio[j])); const hh = pk / apk * wh * .95; rect(wx + i, wy + wh / 2 - hh / 2, 1, Math.max(1, hh), t <= now ? [200, 200, 210] : [80, 80, 95]); }
  { let e = 0, n = 0; for (let j = Math.max(0, Math.floor((now - .4) * ASR)); j < Math.min(audio.length, Math.floor(now * ASR)); j++) { e += audio[j] * audio[j]; n++; } const db = n ? 10 * Math.log10(e / n + 1e-12) : -99; text(`${db.toFixed(1)} dBFS (400 ms rms)`, wx, wy + wh + 4, 1, DIM); const m = Math.max(0, Math.min(1, (db + 40) / 40)); rect(wx + 220, wy + wh + 4, m * 300, 8, m > .85 ? [255, 90, 90] : [90, 200, 120]); }
  rect(wx + (now - t0) / (t1 - t0) * ww, wy, 2, wh, [255, 255, 255], .9);
}

// ── encode ──────────────────────────────────────────────────────────────
const frames = Math.ceil(DUR * FPS);
const ff = spawn('ffmpeg', ['-y', '-v', 'error', '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-s', `${W}x${H}`, '-r', String(FPS), '-i', 'pipe:0', '-i', AUDIO, '-map', '0:v', '-map', '1:a', '-c:v', 'libx264', '-preset', 'veryfast', '-crf', '20', '-pix_fmt', 'yuv420p', '-c:a', 'aac', '-b:a', '192k', '-shortest', OUT], { stdio: ['pipe', 'inherit', 'inherit'] });
let f = 0;
const pump = () => { while (f < frames) { draw(f / FPS); f++; if (!ff.stdin.write(Buffer.from(frame))) { ff.stdin.once('drain', pump); return; } if (f % 300 === 0) process.stdout.write(`\r${mmss(f / FPS)} / ${mmss(DUR)}`); } ff.stdin.end(); };
pump();
ff.on('close', code => { console.log(`\n${code === 0 ? OUT : 'ffmpeg failed ' + code}`); });
