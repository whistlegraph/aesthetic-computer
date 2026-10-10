#!/usr/bin/env node
// Performance-test KidLisp pieces on the Xbox devkit: build the native live
// script, hot-deploy it, wait, then read the KIDLISP telemetry lines back
// into one table, with a Device Portal screenshot per piece.
//
//   node xbox/tools/kidlisp-bench.mjs [seconds=70] [density|auto] [piece.lisp ...]
//
// Needs the Device Portal environment (xbox/tools/live.mjs reads it from the
// vault) and Native BIOS running. Output: a table on stdout and
// kidlisp/build/xbox-bench/<piece>.png.
import { execFileSync } from 'node:child_process';
import { mkdirSync, readFileSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const [secondsArg = '70', density = 'auto', ...pieces] = process.argv.slice(2);
const seconds = Math.max(10, Number(secondsArg) || 70);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const run = (args, opts = {}) => execFileSync('node', args, { cwd: root, encoding: 'utf8', stdio: ['ignore', 'pipe', 'inherit'], ...opts });

const perPiece = Math.floor((seconds * 1000) / Math.max(1, pieces.length || 3));
const built = JSON.parse(run(['kidlisp/tools/native-tv-script.mjs', 'draw', String(perPiece), 'kidlisp/build/kidlisp-tv.js', density, ...pieces]));
console.log(`built ${built.out} (${built.kb} KB) for ${built.pieces.join(', ')}, ${perPiece} ms each, density ${density}`);
const deployed = run(['xbox/tools/live.mjs', 'hot-deploy', 'kidlisp/build/kidlisp-tv.js']).trim().split('\n').at(-1);
console.log(deployed);
const generation = Number((deployed.match(/generation (\d+)/) || [])[1] || 0);

mkdirSync(resolve(root, 'kidlisp/build/xbox-bench'), { recursive: true });
const shots = new Set();
const started = Date.now();
while (Date.now() - started < seconds * 1000) {
  await sleep(perPiece / 2);
  const index = Math.min(built.pieces.length - 1, Math.floor((Date.now() - started) / perPiece));
  const name = built.pieces[index];
  if (!shots.has(name)) { shots.add(name); try { run(['xbox/tools/live.mjs', 'screenshot', `kidlisp/build/xbox-bench/${name}.png`]); console.log(`screenshot kidlisp/build/xbox-bench/${name}.png`); } catch {} }
}
const log = run(['xbox/tools/live.mjs', 'logs', '2000']);
const after = generation ? log.split(`AC_NATIVE_LIVE_READY bytes=`).filter((part) => part.includes(`generation=${generation}`)).at(-1) || log : log;
const samples = [...after.matchAll(/AC_NATIVE_JS KIDLISP (\{.*\})/g)].map((m) => JSON.parse(m[1]));
const bench = (after.match(/AC_NATIVE_JS KIDLISP_BENCH (\{.*\})/) || [])[1];
if (bench) console.log('bench', bench);
const rows = {};
for (const s of samples) { (rows[s.piece] ||= []).push(s); }
console.log('\n| piece | frames | ms/frame (median) | fps | density | draw ms | ops/frame | counts |');
console.log('|---|---|---|---|---|---|---|---|');
for (const [piece, list] of Object.entries(rows)) {
  const steady = list.filter((s) => s.frame > 1);
  const ms = (steady.length ? steady : list).map((s) => s.ms).sort((a, b) => a - b);
  const median = ms[Math.floor(ms.length / 2)];
  const last = list.at(-1);
  console.log(`| ${piece} | ${last.frame} | ${median} | ${(1000 / median).toFixed(1)} | ${last.density ?? 1} | ${last.drawMs} | ${last.ops} | ${last.counts.replace(/"/g, '')} |`);
}
const errors = samples.filter((s) => s.err).map((s) => `${s.piece}: ${s.err}`);
if (errors.length) console.log('\nerrors:', [...new Set(errors)].join('; '));
