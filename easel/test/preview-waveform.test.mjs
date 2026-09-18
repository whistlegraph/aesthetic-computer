import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import vm from 'node:vm';

const source = readFileSync(new URL('../desktop/preview-waveform.js', import.meta.url), 'utf8');
function harness() {
  let now = 0, serial = 0, samples = [], reads = 0, pending = null;
  const tasks = new Map(), elements = [];
  const node = () => ({
    attributes: {}, events: {}, hidden: false,
    classList: { values: new Set(), add(v) { this.values.add(v); }, remove(v) { this.values.delete(v); }, contains(v) { return this.values.has(v); }, toggle(v, on) { if (on) this.add(v); else this.remove(v); } },
    setAttribute(k, v) { this.attributes[k] = v; }, append() {},
    addEventListener(k, fn) { this.events[k] = fn; }, removeEventListener() {},
  });
  const document = { ...node(), body: node(), documentElement: { style: { setProperty() {} } },
    createElementNS() { const n = node(); elements.push(n); return n; }, getElementById() { return null; } };
  const preview = { ...node(), muted: false, isAudioMuted() { return this.muted; },
    async executeJavaScript() { reads++; return pending ? pending : samples; } };
  const schedule = (fn, ms) => { const id = ++serial; tasks.set(id, { at: now + ms, fn }); return id; };
  const motion = { matches: false };
  const window = node();
  vm.runInNewContext(source, { window, document, performance: { now: () => now },
    matchMedia: () => motion, ResizeObserver: class { observe() {} disconnect() {} },
    queueMicrotask() {}, setTimeout: schedule, clearTimeout: id => tasks.delete(id),
    requestAnimationFrame: fn => schedule(() => fn(now), 16), cancelAnimationFrame: id => tasks.delete(id) });
  window.installPreviewWaveform(preview);
  preview.events['dom-ready']();
  const flush = async () => { for (let i = 0; i < 5; i++) await Promise.resolve(); };
  return { preview, document, motion, line: elements[0], path: elements[1], tasks,
    samples(value) { samples = value; }, pending(value) { pending = value; }, reads: () => reads,
    async advance(ms) {
      await flush(); const end = now + ms;
      while (true) {
        const next = [...tasks.entries()].sort((a, b) => a[1].at - b[1].at)[0];
        if (!next || next[1].at > end) break;
        now = next[1].at; tasks.delete(next[0]); next[1].fn(); await flush();
      }
      now = end; await flush();
    },
  };
}
const wave = amplitude => Array.from({ length: 512 }, (_, i) => (i % 2 ? amplitude : -amplitude));
const points = h => [...(h.path.attributes.d || '').matchAll(/[ML]([\d.]+) ([\d.]+)/g)].map(m => ({ y: +m[1], x: +m[2] }));
const sounding = h => h.line.classList.contains('sounding');

test('short sound appears at the bottom within a frame, travels up, then expires', async () => {
  const h = harness(); await h.advance(505);
  h.samples(wave(.04)); await h.advance(16);
  assert(sounding(h));
  let peaks = points(h).filter(p => Math.abs(p.x - 16) > 2);
  assert(peaks.length && peaks.every(p => p.y > 508), 'onset starts at bottom, not stretched over the window');
  h.samples([]); await h.advance(100);
  peaks = points(h).filter(p => Math.abs(p.x - 16) > 2);
  assert(peaks.every(p => p.y < 503), 'silent bottom flattens promptly');
  await h.advance(1900);
  peaks = points(h).filter(p => Math.abs(p.x - 16) > 2);
  assert(peaks.every(p => p.y > 250 && p.y < 260), 'sound crosses the middle after two seconds');
  await h.advance(2100);
  assert.equal(sounding(h), false, 'no flatline hold after history has left the top');
});

test('normalizes quiet and loud sounds without inflating noise or rewriting history', async () => {
  const widths = [];
  for (const level of [.03, .8]) {
    const h = harness(); await h.advance(20); h.samples(wave(level)); await h.advance(32);
    widths.push(Math.max(...points(h).map(p => p.x)) - Math.min(...points(h).map(p => p.x)));
    const old = points(h).filter(p => p.x !== 16).map(p => p.x);
    h.samples(wave(.001)); await h.advance(16);
    for (const x of old) assert(points(h).some(p => p.x === x));
  }
  assert(widths.every(w => w > 20 && w < 24));
  assert(Math.abs(widths[0] - widths[1]) < .1);
  const h = harness(); h.samples(wave(.0001)); await h.advance(100);
  assert.equal(sounding(h), false);
  h.samples(wave(.001)); await h.advance(20);
  assert(Math.max(...points(h).map(p => Math.abs(p.x - 16))) < 1, 'gain is capped near silence');
});

test('mute, hidden windows, navigation and reduced motion clear scrolling history', async () => {
  for (const stop of [h => { h.preview.muted = true; }, h => { h.document.hidden = true; }, h => h.preview.events['did-start-loading']()]) {
    const h = harness(); h.samples(wave(.2)); await h.advance(32); assert(sounding(h));
    stop(h); await h.advance(32); assert.equal(sounding(h), false);
  }
  const h = harness(); h.motion.matches = true; h.samples(wave(.2)); await h.advance(40);
  assert(sounding(h)); assert.equal(h.path.attributes.d, 'M0 16H511');
  h.samples([]); await h.advance(40); assert.equal(sounding(h), false);
});

test('slow guest reads do not overlap and old replies cannot repopulate a new page', async () => {
  const h = harness(); await h.advance(20);
  let resolve; h.pending(new Promise(r => { resolve = r; })); await h.advance(20);
  const reads = h.reads(); await h.advance(500); assert.equal(h.reads(), reads);
  h.preview.events['did-start-loading'](); h.pending(null); resolve(wave(.5)); await h.advance(20);
  assert.equal(sounding(h), false);
  h.preview.events['did-stop-loading'](); h.samples(wave(.5)); await h.advance(32); assert(sounding(h));
});
