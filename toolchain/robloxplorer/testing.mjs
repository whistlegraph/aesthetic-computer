import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { readFile, mkdir, writeFile, unlink, readdir, stat, open } from 'node:fs/promises';
import { homedir } from 'node:os';
import { resolve } from 'node:path';
import { randomUUID } from 'node:crypto';
const exec = promisify(execFile);
export const root = resolve(import.meta.dirname, '../..');
const arena = resolve(root, 'roblox/arena');
const captures = resolve(arena, 'build/captures');
const bundle = 'com.roblox.RobloxPlayer';
const run = (file, args, options = {}) => exec(file, args, {cwd: root, timeout: 30000, maxBuffer: 4 * 1024 * 1024, ...options});
const apple = async source => (await run('/usr/bin/osascript', ['-e', source])).stdout.trim();
export async function target() { return JSON.parse(await readFile(resolve(arena, 'target.json'), 'utf8')); }
export function launchURL(placeId, serverId) {
  if (!/^[1-9]\d*$/.test(String(placeId))) throw new Error('Invalid place ID');
  if (serverId !== undefined && !/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/i.test(serverId)) throw new Error('Invalid server job ID');
  return `roblox://placeId=${placeId}${serverId ? `&gameInstanceId=${serverId}` : ''}`;
}
async function frontmost() {
  return apple('tell application "System Events"\nset frontApp to first application process whose frontmost is true\nreturn (get bundle identifier of frontApp) as text\nend tell');
}
export async function launch({serverId, browser = false} = {}) {
  if (typeof browser !== 'boolean') throw new Error('browser must be boolean');
  const t = await target();
  const url = launchURL(t.placeId, serverId);
  await run('/usr/bin/open', browser ? ['-a', 'Google Chrome', url.replace('roblox://', 'https://www.roblox.com/games/start?')] : [url]);
  if (!browser) await apple('tell application id "com.roblox.RobloxPlayer" to activate');
  return {requested: true, url, target: t.name, joined: 'Verify with roblox_capture; launching does not guarantee admission or a fresh server.'};
}
export async function status() {
  let running = false;
  try { await run('/usr/bin/pgrep', ['-x', 'RobloxPlayer']); running = true; } catch {}
  return {target: await target(), running, focused: (await frontmost()) === bundle};
}
export async function capture({focus = true, expected = []} = {}) {
  if (typeof focus !== 'boolean') throw new Error('focus must be boolean');
  if (!Array.isArray(expected) || expected.length > 12 || expected.some(x => typeof x !== 'string' || !x.length || x.length > 120)) throw new Error('Expected up to 12 short UI strings');
  await mkdir(captures, {recursive: true});
  const name = `${Date.now()}-${randomUUID().slice(0, 8)}`;
  const path = resolve(captures, `${name}.jpg`);
  // Use this host explicitly. The fleet alias "local" may point at another Mac.
  const host = (await run('/usr/sbin/scutil', ['--get', 'LocalHostName'])).stdout.trim();
  const quote = value => "'" + value.replaceAll("'", "'\\''") + "'";
  const shell = [process.execPath, resolve(root,'slab/bin/frame.mjs'), host, '--json', '--out', path].map(quote).join(' ');
  // Keep focus + capture in one GUI automation transaction. Returning to separate
  // shell calls can restore the invoking Terminal window between the two steps.
  const source = `${focus ? 'tell application "System Events" to set frontmost of process "Roblox" to true\n' : ''}
  do shell script ${JSON.stringify(shell)}`;
  const frame = JSON.parse(await apple(source));
  if (frame.capture !== 'ok' || frame.meta?.frontmost?.bundle !== bundle) {
    await unlink(path).catch(() => {});
    throw new Error('Roblox capture unavailable or focus changed; frame discarded');
  }
  const bytes = await readFile(path);
  const ocr = (frame.ocr || []).map(x => ({text: x.t, x: x.cx, y: x.cy}));
  const all = ocr.map(x => x.text).join('\n').toLowerCase();
  const evidence = {at: new Date().toISOString(), path, app: bundle, ocr,
    checks: expected.map(text => ({text, found: all.includes(text.toLowerCase())})),
    interpretation: 'OCR checks are evidence only; inspect the image for layout, contrast, clipping, and actual game state.'};
  await writeFile(resolve(captures, `${name}.json`), JSON.stringify(evidence, null, 2));
  return {evidence, image: {type: 'image', mimeType: 'image/jpeg', data: bytes.toString('base64')}};
}
export async function clientLog() {
  const dir = resolve(homedir(), 'Library/Logs/Roblox');
  const candidates = (await readdir(dir)).filter(x => x.includes('_Player_') && x.endsWith('.log')).sort().slice(-50);
  const files = await Promise.all(candidates.map(async name => ({name, info: await stat(resolve(dir,name))})));
  files.sort((a,b) => b.info.mtimeMs-a.info.mtimeMs);
  if (!files.length) return {events: [], status:'No Roblox player log'};
  const latest = files[0];
  const file = await open(resolve(dir,latest.name), 'r');
  const buffer = Buffer.alloc(Math.min(latest.info.size, 256*1024));
  try { await file.read(buffer,0,buffer.length,Math.max(0,latest.info.size-buffer.length)); }
  finally { await file.close(); }
  // Never return general Roblox logs: URLs, auth tickets and unrelated sessions can be present.
  const events = [...buffer.toString().matchAll(/AC_ARENA_(?:READY|SWING|THROW|BLAST|AUDIO|KNOCKBACK|MEDIA)\b[^\r\n]*/g)].map(x=>x[0]).slice(-40);
  return {events, status:'Only instrumented AC arena events; audio loaded/playing does not prove audible output.'};
}
const keys = { nade: 3, forward: 13, back: 1, left: 0, right: 2, jump: 49, escape: 53};
export async function control({action, milliseconds = 100} = {}) {
  if (!Object.hasOwn(keys, action)) throw new Error('Unknown gameplay action');
  if (!Number.isInteger(milliseconds) || milliseconds < 50 || milliseconds > 1500) throw new Error('Duration must be 50–1500ms');
  if (await frontmost() !== bundle) throw new Error('Roblox must be frontmost before gameplay input');
  const code = keys[action];
  // Fixed keycodes only. No chat text, arbitrary AppleScript, or shell inputs.
  const letter = {forward: 'w', back: 's', left: 'a', right: 'd'}[action];
  const input = letter ? `key down "${letter}"
    try
      delay ${milliseconds / 1000}
    on error messageText
      key up "${letter}"
      error messageText
    end try
    key up "${letter}"` : `key code ${code}`;
  await apple(`tell application "System Events"
    set frontApp to first application process whose frontmost is true
    if (get bundle identifier of frontApp) is not "${bundle}" then error "Focus changed"
    ${input}
  end tell`);
  return capture({focus: false});
}
export async function api({command, version} = {}) {
  if (!['inspect', 'test', 'poll', 'logs'].includes(command)) throw new Error('Unsupported test API command');
  if (version !== undefined && (command !== 'test' || !/^[1-9]\d*$/.test(String(version)))) throw new Error('Version is only valid for test');
  const args = [resolve(arena, 'api.mjs'), command, ...(version === undefined ? [] : [String(version)])];
  try { return JSON.parse((await run(process.execPath, args)).stdout); }
  catch (error) { throw new Error(error.stdout?.trim() || error.stderr?.trim() || 'Roblox test API failed'); }
}
