#!/usr/bin/env node
// Development target only. No cookie credentials are needed after bootstrap.
import { readFileSync, mkdirSync, writeFileSync, copyFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { config, request, main as explorer, id } from '../../toolchain/robloxplorer/robloxplorer.mjs';
const directory = import.meta.dirname;
const target = JSON.parse(readFileSync(resolve(directory, 'target.json')));
const base = `universes/${id(target.universeId)}`;
const place = `${base}/places/${id(target.placeId)}`;
const { key } = config();
if (!key) throw new Error('Roblox credential missing.');
const headers = { 'x-api-key': key, 'Content-Type': 'application/json' };
async function cloud(path, method = 'GET', body) {
  if (!path.startsWith(`${base}/`) && path !== base && !path.startsWith(`${base}?`) && path !== `${base}:publishMessage`) throw new Error('Wrong development target.');
  return request(`https://apis.roblox.com/cloud/v2/${path}`, { method, headers, ...(body ? { body: JSON.stringify(body) } : {}) });
}
const build = resolve(directory, 'build');
mkdirSync(build, { recursive: true });
const save = (name, value) => writeFileSync(resolve(build, name), JSON.stringify(value, null, 2) + '\n');
async function run() {
  const [command, arg] = process.argv.slice(2);
  if (command === 'inspect') return { universe: await cloud(base), place: await cloud(place) };
  if (command === 'announce') {
    const version = id(arg || JSON.parse(readFileSync(resolve(build, 'latest-release.json'))).result.versionNumber);
    return cloud(`${base}:publishMessage`, 'POST', { topic: 'ac-arena-release', message: JSON.stringify({ version: Number(version) }) });
  }
  if (command === 'configure') {
    const body = { displayName: target.name, description: 'Aesthetic Network development arena. Queue in the lobby, lob contact grenades and blast your rival out of the ring. Private test.' };
    await cloud(`${base}?updateMask=displayName,description`, 'PATCH', body);
    return cloud(`${place}?updateMask=displayName,description,serverSize`, 'PATCH', { ...body, serverSize: 12 });
  }
  if (command === 'publish') {
    const result = await explorer(['publish', resolve(build, 'arena.rbxlx'), target.universeId, target.placeId, '--live']);
    copyFileSync(resolve(build, 'arena.rbxlx'), resolve(build, `arena-v${result.result.versionNumber}.rbxlx`));
    save(`release-${result.result.versionNumber}.json`, { at: new Date().toISOString(), ...result });
    save('latest-release.json', result);
    return result;
  }
  if (command === 'test') {
    const version = id(arg || JSON.parse(readFileSync(resolve(build, 'latest-release.json'))).result.versionNumber);
    const result = await cloud(`${place}/versions/${version}/luau-execution-session-tasks`, 'POST', {
      script: readFileSync(resolve(directory, 'tests/headless.luau'), 'utf8'),
    });
    save('latest-task.json', result);
    return { path: result.path, state: result.state };
  }
  if (command === 'poll' || command === 'logs') {
    const task = JSON.parse(readFileSync(resolve(build, 'latest-task.json')));
    // Never follow a response-supplied URL or send credentials outside this exact place.
    if (!new RegExp(`^${place}/versions/[1-9][0-9]*/luau-execution-sessions/[A-Za-z0-9-]+/tasks/[A-Za-z0-9-]+$`).test(task.path)) throw new Error('Unexpected task resource path.');
    const result = await cloud(task.path + (command === 'logs' ? '/logs' : '?view=FULL'));
    save(command === 'logs' ? 'latest-logs.json' : 'latest-task.json', result);
    if (['FAILED', 'CANCELLED'].includes(result.state)) process.exitCode = 1;
    const { script, ...output } = result;
    return output;
  }
  throw new Error('Usage: node roblox/arena/api.mjs inspect|configure|publish|announce|test [version]|poll|logs');
}
run().then(result => console.log(JSON.stringify(result, null, 2))).catch(error => { console.error(error.message); process.exitCode = 1; });
