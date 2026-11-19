#!/usr/bin/env node
// Read rax's walk log.
//   node rax/walks-cli.mjs [hours=24]            who visited, how long, how far
//   node rax/walks-cli.mjs --path <visit>        one visit's motion, every tick, as JSON
//   node rax/walks-cli.mjs --json [--hours 24] [--visit <id>] [--points 600]
//     the same as JSON (the analytics MCP's rax_walks): visits in the window,
//     or one visit with its path thinned to at most `points` samples.
// Reads RAX_STATE_DIR (default ~/.local/share/rax), where the server writes.
import { readdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { stateHome } from './env.mjs';
import { readMotion, readWalks } from './walks.mjs';

const dir = join(stateHome(), 'walks');
const args = process.argv.slice(2);
const flag = (name) => (args.includes(name) ? args[args.indexOf(name) + 1] : undefined);
const json = args.includes('--json');
const want = flag('--path') || flag('--visit') || null;
const hours = want ? 35 * 24 : Number(json ? flag('--hours') : args[0]) || 24;
const points = Math.max(2, Math.min(5000, Number(flag('--points')) || 600));
const since = Date.now() - hours * 3600000;
const first = new Date(since).toISOString().slice(0, 10);
const names = (await readdir(dir).catch(() => [])).filter((n) => n.slice(0, 10) >= first).sort();
const lines = [], motion = new Map();
for (const name of names) {
  if (name.endsWith('.jsonl')) lines.push(...(await readFile(join(dir, name), 'utf8')).split('\n'));
  if (name.endsWith('.motion')) readMotion(await readFile(join(dir, name)), motion);
}
const visits = readWalks(lines, motion).filter((v) => v.end >= since);
const row = (v) => ({ visit: v.id, room: v.room, who: v.who, guest: v.guest, seated: v.seated,
  entered: new Date(v.start).toISOString(), seconds: Math.round((v.end - v.start) / 1000),
  distance: Math.round(v.distance * 10) / 10, ticks: v.ticks, ...(v.seam ? { seam: v.seam } : {}), stillIn: !v.left });
if (want) {
  const v = visits.find((v) => v.id === want);
  if (!v) { console.error(`No visit ${want}`); process.exit(1); }
  const path = motion.get(v.id) || [];
  if (!json) console.log(JSON.stringify({ ...v, path }));
  else {
    const step = Math.max(1, Math.ceil(path.length / points));
    console.log(JSON.stringify({ ...row(v), every: step, path: path.filter((_, i) => i % step === 0 || i === path.length - 1)
      .map((p) => [p.t, p.x, p.y, p.z, p.yaw]), columns: ['t', 'x', 'y', 'z', 'yaw'] }));
  }
} else if (json) {
  const people = new Map();
  for (const v of visits) if (v.who) people.set(v.who, (people.get(v.who) || 0) + 1);
  console.log(JSON.stringify({ hours, since: new Date(since).toISOString(), visits: visits.map(row),
    people: [...people].map(([who, n]) => ({ who, visits: n })),
    note: 'A visit is one connection. Guests only watch in production, so only handles have a distance and a path. Kept 35 days; nothing before 2026-11-18.' }));
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
