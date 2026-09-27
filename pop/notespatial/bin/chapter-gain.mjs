#!/usr/bin/env node
// chapter-gain.mjs — narrow the composed level arc without flattening it.
// Measures each chapter's integrated loudness in the print, then writes an
// ffmpeg volume expression that pulls every chapter toward a pivot by a ratio
// (1.7 → a 10 dB spread becomes ~6), ramping 3 s either side of each downbeat.
//   node chapter-gain.mjs <print.wav> <ratio> <pivot_lufs> > expr.txt
import { readFileSync } from 'node:fs'; import { spawnSync } from 'node:child_process';
const [file, ratioArg = '1.7', pivotArg = '-20'] = process.argv.slice(2);
const ratio = +ratioArg, pivot = +pivotArg, s = JSON.parse(readFileSync(new URL('../../../fedac/native/scores/notespatial-native.nsscore', import.meta.url)));
const gains = s.movements.map(m => {
  const r = spawnSync('ffmpeg', ['-hide_banner', '-nostats', '-ss', String(m.t0), '-t', String(m.t1 - m.t0), '-i', file, '-af', 'ebur128', '-f', 'null', '-'], { encoding: 'utf8' });
  const I = +[...r.stderr.matchAll(/I:\s+(-?[\d.]+) LUFS/g)].pop()[1];
  return { name: m.name, t0: m.t0, I, g: +((pivot - I) * (1 - 1 / ratio)).toFixed(2) };
});
console.error(gains.map(x => `${x.name.split(' ')[0]} ${x.I} → ${x.g > 0 ? '+' : ''}${x.g} dB`).join('\n'));
let expr = String(gains[0].g);
for (let k = 1; k < gains.length; k++) expr += `+(${(gains[k].g - gains[k - 1].g).toFixed(2)})*clip((t-(${(gains[k].t0 - 3).toFixed(2)}))/6,0,1)`;
process.stdout.write(`pow(10,(${expr})/20)`);
