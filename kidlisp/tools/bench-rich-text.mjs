#!/usr/bin/env node
// Renderer bookkeeping benchmark. Draw calls are counted, not rasterized.
import { performance } from 'node:perf_hooks';
import { pathToFileURL } from 'node:url';
import { resolve } from 'node:path';
const modulePath = process.argv[2] || resolve('system/public/aesthetic.computer/lib/rich-text.mjs');
const { RichTextFlow } = await import(pathToFileURL(resolve(modulePath)));
for (const count of [295, 10000]) {
  const body = Array.from({ length: count }, (_, i) => `word${i}`).join(' ');
  const words = [...body.matchAll(/\S+/g)].map((m, i) => ({ start: m.index, end: m.index + m[0].length, fromMs: i * 250, toMs: (i + 1) * 250 }));
  let wraps = 0, draws = 0;
  const api = { screen: { width: 330, height: 90 }, ink() {}, box() {}, write() { draws++; }, send() {}, needsPaint() {}, text: {
    width: text => text.length * 6,
    box(text, _at, width, scale) {
      wraps++;
      const columns = Math.max(1, Math.floor(width / (6 * scale)));
      const lines = [], charMap = [];
      for (let i = 0; i < text.length; i += columns) {
        lines.push(text.slice(i, i + columns));
        charMap.push(Array.from({ length: Math.min(columns, text.length - i) }, (_, j) => i + j));
      }
      return { lines, charMap };
    },
  } };
  const flow = new RichTextFlow();
  const handle = { kidlispData: true, url: 'https://example.com/episode.json', value: { title: 'reader', body, words, audio: { url: 'episode.mp3' } } };
  const values = [{ kind: 'listen', data: handle }];
  const start = performance.now(); flow.paint(api, values); const prepareMs = performance.now() - start;
  flow.playing = true;
  const times = [];
  for (let batch = 0; batch < 60; batch++) {
    const t = performance.now();
    for (let repeat = 0; repeat < 100; repeat++) {
      flow.time = ((batch * 100 + repeat) % count) * .25 + .1;
      flow.paint(api, values);
    }
    times.push((performance.now() - t) / 100);
  }
  times.sort((a, b) => a - b);
  console.log(JSON.stringify({ words: count, prepareMs: +prepareMs.toFixed(3), medianFrameMs: +times[30].toFixed(4), p95BatchFrameMs: +times[57].toFixed(4), wraps, draws, excludes: 'glyph rasterization, interpreter, audio decoding, GPU presentation' }));
}
