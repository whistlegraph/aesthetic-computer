import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp, mkdir, writeFile, readFile, chmod, rm, symlink} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {spawnSync} from 'node:child_process';
const launcher = new URL('../bin/aesel', import.meta.url).pathname;
async function setup(t, id = 'computer.aesthetic.aesel.native') {
  const root = await mkdtemp(join(tmpdir(), 'aesel-launcher-'));
  t.after(() => rm(root, {recursive:true, force:true}));
  const bin = join(root, 'bin'), app = join(root, 'Aesel Native.app'), output = join(root, 'argv');
  await mkdir(bin); await mkdir(join(app, 'Contents'), {recursive:true});
  await writeFile(join(app, 'Contents/Info.plist'), `<?xml version="1.0"?><plist version="1.0"><dict><key>CFBundleIdentifier</key><string>${id}</string></dict></plist>`);
  await writeFile(join(bin, 'open'), '#!/bin/sh\nprintf "%s\\0" "$@" > "$EASEL_TEST_ARGS"\n');
  await chmod(join(bin, 'open'), 0o755);
  await symlink(launcher, join(bin, 'aesel'));
  const env = {...process.env, PATH:bin+':'+process.env.PATH, AESEL_DESKTOP_APP:app, EASEL_TEST_ARGS:output};
  return {app, output, env, run: args => spawnSync(join(bin, 'aesel'), args, {env, encoding:'utf8'})};
}
test('installed symlink opens the native bundle without creating a second instance', {skip:process.platform !== 'darwin'}, async t => {
  const f = await setup(t), result = f.run([]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual((await readFile(f.output)).toString().split('\0').slice(0,-1), ['-a',f.app]);
});
test('an explicit Electron bundle is rejected instead of opened', {skip:process.platform !== 'darwin'}, async t => {
  const f = await setup(t, 'computer.aesthetic.easel'), result = f.run([]);
  assert.notEqual(result.status,0); assert.match(result.stderr,/Electron app is retired/);
  await assert.rejects(readFile(f.output),{code:'ENOENT'});
});
test('help and unsupported legacy options never launch an app', async t => {
  const f = await setup(t);
  assert.match(f.run(['--help']).stdout,/native Aesel/);
  for (const args of [['.'], ['--cwd','/tmp'], ['--dev'], ['--backend','ac']]) assert.notEqual(f.run(args).status,0);
  await assert.rejects(readFile(f.output),{code:'ENOENT'});
});
test('missing native install points to native download and terminal command', {skip:process.platform !== 'darwin'}, async t => {
  const f = await setup(t); f.env.AESEL_DESKTOP_APP = f.app + '.missing';
  const result = f.run([]);
  assert.notEqual(result.status,0); assert.match(result.stderr,/https:\/\/aesel.app or run a/);
});
