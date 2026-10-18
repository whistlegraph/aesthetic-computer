import test from 'node:test';
import { createHash } from 'node:crypto';
import assert from 'node:assert/strict';
import { readFile, writeFile, mkdtemp, rm } from 'node:fs/promises';
import { spawn, spawnSync } from 'node:child_process';
import { createServer } from 'node:net';
import { once } from 'node:events';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

test('No Paint classic serves the complete archived export without crossing studio routes', {timeout:30000}, async t => {
  const binary = process.env.CADDY_BIN || 'caddy';
  if (spawnSync(binary, ['version']).error?.code === 'ENOENT') return t.skip('Caddy is required for the HTTP routing check');
  const socket = createServer(); socket.listen(0, '127.0.0.1'); await once(socket, 'listening');
  const port = socket.address().port; await new Promise(resolve => socket.close(resolve));
  const root = fileURLToPath(new URL('../system/public', import.meta.url));
  const source = await readFile(new URL('./Caddyfile', import.meta.url), 'utf8');
  const routes = source.split('\nnopaint.art {\n')[1].split('\n}\n')[0]
    .replace(/^    import (?:machine_discovery|lith_access_log)\n/gm, '')
    .replace(/^    tls \{[\s\S]*?^    \}\n/m, '')
    .replace(/\/opt\/ac\/system\/public(\/[^\s]+)?/g, (_, suffix = '') => JSON.stringify(root + suffix))
    .replaceAll('import lith_proxy', 'respond "proxy fallback"');
  const dir = await mkdtemp(join(tmpdir(), 'nopaint-routing-'));
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
    try { await fetch(base + '/classic/'); break; }
    catch { if (attempt === 100 || server.exitCode !== null) throw new Error(logs); await new Promise(resolve=>setTimeout(resolve,30)); }
  }
  for (const path of ['/classic', '/covid', '/covid/']) {
    for (const query of ['', '?keep=1']) {
      const response = await fetch(base + path + query, {redirect:'manual'});
      assert.equal(response.status, 308, path);
      assert.equal(response.headers.get('location'), '/classic/' + query, path);
    }
  }
  const page = await fetch(base + '/classic/');
  assert.equal(page.status, 200);
  const html = await page.text();
  assert.match(html, /No Paint: Summer 2021/);
  for (const name of ['image-path', 'workspace-theme']) {
    const response = await fetch(base + `/easel/src/${name}.mjs`);
    assert.equal(response.status, 200);
    assert.match(response.headers.get('content-type'), /javascript/);
    assert.equal(await response.text(), await readFile(new URL(`../aesel/src/${name}.mjs`, import.meta.url), 'utf8'));
  }
  assert.doesNotMatch(html, /src="scripts\/register-sw.js"/);
  const manifest = JSON.parse(await readFile(new URL('../toolchain/nopaint/classic-export.json', import.meta.url)));
  for (const file of manifest.files) {
    const path = '/classic/' + file.path.split('/').map(encodeURIComponent).join('/');
    const response = await fetch(base + path);
    assert.equal(response.status, 200, path);
    const bytes = Buffer.from(await response.arrayBuffer());
    assert.equal(createHash('sha256').update(bytes).digest('hex'), file.sha256, path);
  }
  for (const path of ['/media/example.webm', '/api/nopaint', '/nopaint:123']) {
    assert.equal(await (await fetch(base + path)).text(), 'proxy fallback', path);
  }
  assert.equal((await fetch(base + '/classic/not-a-file')).status, 404);
  assert.match(await (await fetch(base + '/gallery/')).text(), /No Paint/);
});

test('the original save hostname maps only its three legacy endpoints to AC', {timeout:15000}, async t => {
  const binary = process.env.CADDY_BIN || 'caddy';
  if (spawnSync(binary, ['version']).error?.code === 'ENOENT') return t.skip('Caddy is required for the HTTP routing check');
  const socket = createServer(); socket.listen(0, '127.0.0.1'); await once(socket, 'listening');
  const port = socket.address().port; await new Promise(resolve => socket.close(resolve));
  const root = fileURLToPath(new URL('../system/public', import.meta.url));
  const source = await readFile(new URL('./Caddyfile', import.meta.url), 'utf8');
  const routes = source.split('\nserver.nopaint.art {\n')[1].split('\n}\n')[0]
    .replace(/^    import (?:machine_discovery|lith_access_log)\n/gm, '')
    .replace(/^    tls \{[\s\S]*?^    \}\n/m, '')
    .replace(/\/opt\/ac\/system\/public(\/[^\s]+)?/g, (_, suffix = '') => JSON.stringify(root + suffix))
    .replaceAll('import lith_proxy', 'respond "{method} {uri}"');
  const dir = await mkdtemp(join(tmpdir(), 'nopaint-routing-'));
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
    try { await fetch(base + '/get-code'); break; }
    catch { if (attempt === 100 || server.exitCode !== null) throw new Error(logs); await new Promise(resolve=>setTimeout(resolve,30)); }
  }

  for (const [path, method] of [['/get-code', 'GET'], ['/upload-painting', 'POST'], ['/painting/Classic12345.png', 'GET']]) {
    const response = await fetch(base + path, {method, ...(method === 'POST' ? {body:'code=Classic12345&painting=AA=='} : {})});
    assert.equal(response.status, 200);
    assert.equal(await response.text(), `${method} /api/nopaint-classic${path}`);
  }
  assert.equal((await fetch(base + '/api/anything')).status, 404);
  assert.equal((await fetch(base + '/')).status, 404);
});
