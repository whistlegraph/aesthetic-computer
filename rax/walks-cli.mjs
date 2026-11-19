#!/usr/bin/env node
// Read rax's walk log.
//   node rax/walks-cli.mjs [hours=24]          who visited, how long, how far
//   node rax/walks-cli.mjs --path <visit>      one visit's motion, every tick, as JSON
// Reads RAX_STATE_DIR (default ~/.local/share/rax), where the server writes.
import { readdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { stateHome } from './env.mjs';
import { readMotion, readWalks } from './walks.mjs';

const dir = join(stateHome(), 'walks');
const args = process.argv.slice(2);
const want = args[0] === '--path' ? args[1] : null;
const hours = want ? 35 * 24 : Number(args[0]) || 24;
const since = Date.now() - hours * 3600000;
const first = new Date(since).toISOString().slice(0, 10);
const names = (await readdir(dir).catch(() => [])).filter((n) => n.slice(0, 10) >= first).sort();
const lines = [], motion = new Map();
for (const name of names) {
  if (name.endsWith('.jsonl')) lines.push(...(await readFile(join(dir, name), 'utf8')).split('\n'));
  if (name.endsWith('.motion')) readMotion(await readFile(join(dir, name)), motion);
}
const visits = readWalks(lines, motion).filter((v) => v.end >= since);
if (want) {
  const v = visits.find((v) => v.id === want);
  if (!v) { console.error(`No visit ${want}`); process.exit(1); }
  console.log(JSON.stringify({ ...v, path: motion.get(v.id) || [] }));
} else if (!visits.length) console.log(`No walks in the last ${hours} hours (${dir}).`);
else {
  const clock = (t) => new Date(t).toISOString().slice(5, 16).replace('T', ' ');
  for (const v of visits) {
    const secs = Math.round((v.end - v.start) / 1000);
    console.log([v.id, clock(v.start), (v.room || '?').padEnd(12), (v.who || 'unnamed').padEnd(24),
      `${secs}s`.padStart(6), v.seated ? `${v.distance.toFixed(1)}u`.padStart(8) : 'watched'.padStart(8),
      v.left ? '' : 'still in'].join('  '));
  }
}
