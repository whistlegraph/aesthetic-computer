#!/usr/bin/env node
// 📦 rax scene export: one frame of a room, as the real renderer draws it,
// written to a .glb with vertex colours (world metres, y up).
//
//   node rax/export/scene.mjs [room] [out.glb] [--url https://aesthetic.computer/rax/]
//
// It opens the room as a guest (who only watches), reaches into the page's
// own Projector and, for one whole frame, keeps every model handed to
// `draw()` in world space, before any culling, clipping or lens. The sky
// (drawn with `bends` off) is left out; overlays never pass through draw.
import { chromium } from 'playwright';
import { writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';

const args = process.argv.slice(2);
const flag = (name, fallback) => { const i = args.indexOf(name); return i >= 0 ? args.splice(i, 2)[1] : fallback; };
const base = flag('--url', 'https://aesthetic.computer/rax/');
const wait = Number(flag('--wait', 12000));
const [room = 'lobby', out = `rax-${room}.glb`] = args;

const browser = await chromium.launch({ headless: true,
  ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}),
  args: ['--enable-unsafe-webgpu', '--disable-background-timer-throttling', '--disable-renderer-backgrounding'] });
try {
  const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
  page.on('pageerror', (e) => console.error('page:', e.message));
  await page.goto(`${base}?room=${encodeURIComponent(room)}`);
  await page.locator('canvas').waitFor({ state: 'visible' });
  await page.waitForTimeout(wait); // the room arrives, builds stream in
  const tris = await page.evaluate(async () => {
    const { Projector } = await import(new URL('./project.mjs', location.href).href);
    const P = Projector.prototype, draw = P.draw, frame = P.frame;
    let state = 'armed'; const kept = [];
    P.frame = function (...a) {
      if (state === 'armed') state = 'on'; else if (state === 'on') state = 'done';
      return frame.apply(this, a);
    };
    P.draw = function (v, x = 0, y = 0, z = 0, yaw = 0, pitch = 0, scale = 1, roll = 0) {
      if (state === 'on' && this.bends !== false) {
        const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch), cr = Math.cos(roll), sr = Math.sin(roll);
        const part = new Float32Array(v.length);
        for (let k = 0; k < v.length; k += 6) {
          const lx = v[k] * scale, ly = v[k + 1] * scale, lz = v[k + 2] * scale;
          const rx = lx * cr - ly * sr, ry = lx * sr + ly * cr, py = ry * cp - lz * sp, pz = ry * sp + lz * cp;
          part[k] = rx * cy + pz * sy + x; part[k + 1] = py + y; part[k + 2] = -rx * sy + pz * cy + z;
          let r = v[k + 3], g = v[k + 4], b = v[k + 5];
          if (r > 1.5) r -= 2; if (g > 3.5) g -= 4; // see-through / screen marks
          part[k + 3] = r; part[k + 4] = g; part[k + 5] = b;
        }
        kept.push(part);
      }
      return draw.call(this, v, x, y, z, yaw, pitch, scale, roll);
    };
    const until = Date.now() + 8000;
    while (state !== 'done' && Date.now() < until) await new Promise((r) => setTimeout(r, 30));
    P.draw = draw; P.frame = frame;
    const n = kept.reduce((s, p) => s + p.length, 0), all = new Float32Array(n);
    let o = 0; for (const p of kept) { all.set(p, o); o += p.length; }
    return { state, data: Array.from(all) };
  });
  if (tris.state !== 'done') throw new Error(`capture did not finish (${tris.state})`);
  const glb = toGlb(Float32Array.from(tris.data));
  await writeFile(out, glb);
  console.log(`${room}: ${tris.data.length / 18} triangles → ${out} (${(glb.length / 1e6).toFixed(1)} MB)`);
} finally { await browser.close(); }

// Interleaved xyz+rgb triangles → one glTF mesh with COLOR_0, flat (unindexed).
function toGlb(v) {
  const count = v.length / 6, pos = new Float32Array(count * 3), col = new Float32Array(count * 3);
  const min = [Infinity, Infinity, Infinity], max = [-Infinity, -Infinity, -Infinity];
  for (let i = 0; i < count; i++) for (let j = 0; j < 3; j++) {
    const p = v[i * 6 + j]; pos[i * 3 + j] = p; min[j] = Math.min(min[j], p); max[j] = Math.max(max[j], p);
    col[i * 3 + j] = Math.max(0, Math.min(1, v[i * 6 + 3 + j]));
  }
  const bin = Buffer.concat([Buffer.from(pos.buffer), Buffer.from(col.buffer)]);
  const json = { asset: { version: '2.0', generator: 'rax/export/scene.mjs' }, scene: 0, scenes: [{ nodes: [0] }],
    nodes: [{ mesh: 0, name: 'rax' }], meshes: [{ name: 'rax', primitives: [{ attributes: { POSITION: 0, COLOR_0: 1 }, material: 0 }] }],
    materials: [{ name: 'rax-vertex', pbrMetallicRoughness: { baseColorFactor: [1, 1, 1, 1], metallicFactor: 0, roughnessFactor: 0.9 }, doubleSided: true }],
    buffers: [{ byteLength: bin.length }],
    bufferViews: [{ buffer: 0, byteOffset: 0, byteLength: pos.byteLength }, { buffer: 0, byteOffset: pos.byteLength, byteLength: col.byteLength }],
    accessors: [{ bufferView: 0, componentType: 5126, count, type: 'VEC3', min, max }, { bufferView: 1, componentType: 5126, count, type: 'VEC3' }] };
  const pad = (b, c) => Buffer.concat([b, Buffer.alloc((4 - (b.length % 4)) % 4, c)]);
  const j = pad(Buffer.from(JSON.stringify(json)), 0x20), b = pad(bin, 0);
  const head = Buffer.alloc(12); head.writeUInt32LE(0x46546c67, 0); head.writeUInt32LE(2, 4); head.writeUInt32LE(12 + 8 + j.length + 8 + b.length, 8);
  const chunk = (buf, type) => { const h = Buffer.alloc(8); h.writeUInt32LE(buf.length, 0); h.writeUInt32LE(type, 4); return Buffer.concat([h, buf]); };
  return Buffer.concat([head, chunk(j, 0x4e4f534a), chunk(b, 0x004e4942)]);
}
