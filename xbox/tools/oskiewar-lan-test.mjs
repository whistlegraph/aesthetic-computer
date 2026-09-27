#!/usr/bin/env node
// Today's explicit two-device matchup. The normal deploy removes this override.
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const address = process.argv[2];
if (!address) throw new Error('Usage: node xbox/tools/oskiewar-lan-test.mjs <ac-host:port> [--hot]');
const url = new URL(address.includes('://') ? address : 'http://' + address);
if (!/^[a-zA-Z0-9.-]+$/.test(url.hostname)) throw new Error('Invalid AC host');
const expires = new Date(); expires.setHours(24, 0, 0, 0);
const room = 'ow-lantest924';
// Preserve a lowered curtain through a paired hot reload.
let curtain = false;
try {
  const response = await fetch(url.origin + '/pieces/oskiewar-control.json', {signal:AbortSignal.timeout(2000)});
  if (response.ok) curtain = (await response.json()).curtain === true;
} catch {}
const config = { room, expires: expires.getTime(), seat: 0, curtain, inputSendHz: 30 };
if (process.argv.includes('--profile-phases')) config.phaseProfile = true;
const inputRateArg = process.argv.find(arg => arg.startsWith('--input-hz='));
if (inputRateArg) {
  const inputSendHz = Number(inputRateArg.split('=')[1]);
  if (![20, 30, 60].includes(inputSendHz)) throw new Error('Input rate must be 20, 30 or 60 Hz');
  config.inputSendHz = inputSendHz;
}
const source = await readFile(path.join(root, 'xbox/live/oskiewar.js'), 'utf8');
const override = await readFile(path.join(root, 'xbox/live/lan-test.js'), 'utf8');
const out = path.join(root, 'fedac/native/build/oskiewar-lan-test');
await mkdir(out, {recursive:true});
const xboxFile = path.join(out, 'oskiewar-lan-test.js');
await writeFile(xboxFile, '// @bundle-qr\nglobalThis.__oskiewarLanTest = ' + JSON.stringify(config) + ';\n' + source + '\n' + override);
execFileSync('ssh', ['-o', 'BatchMode=yes', '-o', 'ConnectTimeout=5', 'root@' + url.hostname,
  'cat > /tmp/oskiewar-lan-test.json'], {input: JSON.stringify({...config, seat:1})});
execFileSync('node', ['fedac/native/tools/oskiewar-live.mjs', url.origin], {cwd:root, stdio:'inherit'});
execFileSync('node', ['xbox/tools/live.mjs', process.argv.includes('--hot') ? 'hot-deploy' : 'deploy', xboxFile],
  {cwd:root, stdio:'inherit'});
console.log('xbox vs ac: ' + room + '; expires ' + expires.toISOString());
