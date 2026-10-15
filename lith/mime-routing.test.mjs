import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile, writeFile, mkdtemp, rm } from 'node:fs/promises';
import { spawn, spawnSync } from 'node:child_process';
import { createServer } from 'node:net';
import { once } from 'node:events';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

test('MIME top-level posts and legacy redirects preserve service and static routes', {timeout:15000}, async t => {
  const binary = process.env.CADDY_BIN || 'caddy';
  if (spawnSync(binary, ['version']).error?.code === 'ENOENT') return t.skip('Caddy is required for the HTTP routing check');
  const socket = createServer(); socket.listen(0, '127.0.0.1'); await once(socket, 'listening');
  const port = socket.address().port; await new Promise(resolve => socket.close(resolve));
  const root = fileURLToPath(new URL('../system/public', import.meta.url));
  const source = await readFile(new URL('./Caddyfile', import.meta.url), 'utf8');
  // Exercise the production MIME routes on loopback, with the upstream replaced
  // by a marker. Certificates and machine discovery belong to the public host.
  const routes = source.split('\nmime.ac {\n')[1].split('\n}\n')[0]
    .replace(/^    import machine_discovery\n/m, '')
    .replace(/^    tls \{[\s\S]*?^    \}\n/m, '')
    .replace(/\/opt\/ac\/system\/public(\/mime)?/g, (_, suffix = '') => JSON.stringify(root + suffix))
    .replaceAll('import lith_proxy', 'respond "proxy fallback"');
  const dir = await mkdtemp(join(tmpdir(), 'mime-routing-'));
  const config = join(dir, 'Caddyfile');
  await writeFile(config, `{\n admin off\n auto_https off\n persist_config off\n}\nhttp://127.0.0.1:${port} {\n${routes}\n}\n`);
  const server = spawn(binary, ['run', '--config', config, '--adapter', 'caddyfile'], {stdio:['ignore','ignore','pipe']});
  let logs = ''; server.stderr.on('data', chunk => { logs += chunk; });
  t.after(async () => {
    if (server.exitCode === null) { const exited = once(server, 'exit'); server.kill('SIGTERM'); await exited; }
    await rm(dir, {recursive:true, force:true});
  });
  const base = `http://127.0.0.1:${port}`;
  for (let attempt = 0; ; attempt++) {
    try { await fetch(base); break; }
    catch { if (attempt === 100 || server.exitCode !== null) throw new Error(logs); await new Promise(resolve=>setTimeout(resolve,30)); }
  }
  for (const code of ['qst','nes~qst','mood~abc','chat~abc','laer-klokken~nvb','laklok~abc','wgHafar','ow-chette767','piece_abc']) {
    const response = await fetch(base + '/' + code);
    assert.equal(response.status, 200, code);
    assert.match(await response.text(), /function postFromPath/);
    const legacy = await fetch(base + '/p/' + code + '?keep=1', {redirect:'manual'});
    assert.equal(legacy.status, 308, code);
    assert.equal(legacy.headers.get('location'), '/' + code + '?keep=1');
  }
  for (const path of ['/api/mime','/nes-player','/nes-player/','/nes-player/player.mjs','/mime-play/','/mime-layout.json']) {
    assert.equal(await (await fetch(base + path)).text(), 'proxy fallback', path);
  }
  for (const path of ['/terms','/support','/mime-types']) {
    const response = await fetch(base + path);
    assert.equal(response.status,200,path);
    assert.doesNotMatch(await response.text(), /function postFromPath/, path);
  }
  assert.equal((await fetch(base + '/favicon.svg')).status,200);
  assert.equal((await fetch(base + '/prompt',{redirect:'manual'})).headers.get('location'),'https://aesthetic.computer/prompt');
});
