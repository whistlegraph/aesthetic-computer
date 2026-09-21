import test from 'node:test';
import assert from 'node:assert/strict';
import net from 'node:net';
import { mkdtemp, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { Machine } from '../bin/puppet.mjs';

const exec = promisify(execFile);
test('Frame memory transport preserves fragmented binary images, modes, and errors', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'frame-test-'));
  const previous = { sock: process.env.SLAB_FRAME_SOCK, config: process.env.SLAB_PUPPET_CONFIG };
  process.env.SLAB_FRAME_SOCK = join(dir, 'frame.sock');
  process.env.SLAB_PUPPET_CONFIG = join(dir, 'machines.json');
  await writeFile(process.env.SLAB_PUPPET_CONFIG, JSON.stringify({ machines: { fixture: { local: true } } }));
  const requests = [];
  let envelope = { capture: 'ok', crop: { x: 40, y: 60, w: 100, h: 80 } };
  const jpg = Buffer.from([0xff, 0xd8, 0, 10, 0x80, 0xff, 0xd9]);
  const server = net.createServer(sock => {
    let request = '';
    sock.on('data', chunk => {
      request += chunk;
      if (!request.includes('\n')) return;
      requests.push(JSON.parse(request));
      if (envelope === null) { sock.end(); return; }
      const json = Buffer.from(JSON.stringify(envelope));
      const packet = Buffer.concat([Buffer.from(`ACF1 ${json.length} ${jpg.length}\n`), json, jpg]);
      sock.write(packet.subarray(0, 9));
      setImmediate(() => sock.end(packet.subarray(9)));
    });
  });
  await new Promise(resolve => server.listen(process.env.SLAB_FRAME_SOCK, resolve));
  t.after(async () => {
    await new Promise(resolve => server.close(resolve));
    for (const [key, value] of [['SLAB_FRAME_SOCK', previous.sock], ['SLAB_PUPPET_CONFIG', previous.config]]) {
      if (value === undefined) delete process.env[key]; else process.env[key] = value;
    }
    await rm(dir, { recursive: true, force: true });
  });
  const { captureFrame } = await import(`../bin/frame.mjs?test=${Date.now()}`);
  const result = await captureFrame('fixture', { memory: true, noOCR: true, noVisual: true, crop: [40,60,100,80], quietOverlay: true });
  assert.deepEqual(result.env, envelope);
  assert.deepEqual(result.jpg, jpg);
  assert.equal(requests[0].mode, 'window noocr novisual quiet-overlay crop=40,60,100,80');
  await assert.rejects(captureFrame('missing', { memory: true }), /unknown machine/);
  envelope = { error: 'fixture transport failure' };
  await assert.rejects(captureFrame('fixture', { memory: true, pressAt: [1,2] }), /fixture transport failure/);
  assert.equal(requests.length, 2, 'failed action must not be sent again');
  envelope = null;
  await assert.rejects(captureFrame('fixture', { memory: true, pressAt: [1,2] }), /closed before returning/);
  assert.equal(requests.length, 3, 'disconnected action must not be retried');
  envelope = { capture: 'guard', nativeInput: { observationId:'observed', status:'guarded' } };
  const guard = { observationId:'observed' };
  const guarded = await captureFrame('fixture', { memory:true, session:'fixture', nativeGuard:guard });
  assert.equal(guarded.env.capture,'guard');
  const token=requests.at(-1).mode.match(/native-guard=([^ ]+)/)[1];
  assert.deepEqual(JSON.parse(Buffer.from(token,'base64').toString()),guard);
  const count=requests.length;
  await assert.rejects(captureFrame('fixture',{nativeGuard:guard,nativeClick:guard}),/not multiple operations/);
  await assert.rejects(captureFrame('fixture',{nativeClick:guard,pressAt:[1,2]}),/cannot be combined/);
  assert.equal(requests.length,count,'conflicting actions rejected before publishing a request');
  envelope=null;
  await assert.rejects(captureFrame('fixture',{memory:true,nativeClick:{observationId:'observed',x:20,y:30,count:1,settleMs:0}}),/closed before returning/);
  assert.equal(requests.length,count+1,'resident click not replayed on disconnect');
  envelope = { capture:'verified', nativeInput:{observationId:'observed',status:'dispatched',kind:'drag',releasePosted:true,durationMs:8} };
  const drag={observationId:'observed',x:20,y:30,count:1,holdMs:0,settleMs:0,drag:{x:60,y:70,durationMs:8}};
  await captureFrame('fixture',{memory:true,nativeDrag:drag});
  const dragToken=requests.at(-1).mode.match(/native-drag=([^ ]+)/)[1];
  assert.deepEqual(JSON.parse(Buffer.from(dragToken,'base64').toString()),drag);
  await assert.rejects(captureFrame('fixture',{nativeClick:guard,nativeDrag:drag}),/not multiple operations/);
  envelope = null;
  const dragCount=requests.length;
  await assert.rejects(captureFrame('fixture',{memory:true,nativeDrag:drag}),/closed before returning/);
  assert.equal(requests.length,dragCount+1,'resident drag not replayed on disconnect');
  envelope = { capture: 'ok' };
  const times = { memory: [], cli: [] };
  for (let i = 0; i < 7; i++) {
    let start = performance.now();
    await captureFrame('fixture', { memory: true });
    times.memory.push(performance.now() - start);
    start = performance.now();
    const output = await exec(process.execPath, ['slab/bin/frame.mjs', 'fixture', '--json', '--out', join(dir, 'shot.jpg')]);
    assert.equal(JSON.parse(output.stdout).capture, 'ok');
    times.cli.push(performance.now() - start);
  }
  for (const [name, values] of Object.entries(times)) {
    t.diagnostic(`${name} median transport overhead: ${values.sort((a,b) => a-b)[3].toFixed(2)}ms (mock capture, 7 runs)`);
  }
});

function browser() {
  const machine = new Machine('fixture', {});
  machine.session = async () => 'session';
  machine.analysisScan = () => {};
  const calls = [];
  machine.call = async (method, params) => { calls.push({method, params}); return {data: 'fresh-image'}; };
  return {machine, calls};
}
test('native-only Puppet machines never enter the browser reconnect loop', async () => {
  const machine = new Machine('native', { local: true });
  machine.connect = () => { throw new Error('must not connect'); };
  await machine.connectLoop();
  assert.equal(machine.info().lastError, null);
  await assert.rejects(machine.ensureConnected(), /no browser configured/);
});

test('Frame MCP forwards explicit contour skipping and omits contours on reframe diff probes', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'frame-mcp-visual-'));
  const socket = join(dir, 'frame.sock'), config = join(dir, 'machines.json');
  await writeFile(config, JSON.stringify({ machines: { fixture: { local: true } } }));
  const modes = [];
  const server = net.createServer(sock => sock.once('data', data => {
    const { mode } = JSON.parse(String(data));
    modes.push(mode);
    const env = { capture: 'ok', capture_scope: 'window', diff: [], diff_baseline: 'matched',
      visual_suppressed: 'requested', observation: { session: mode.match(/session=([^ ]+)/)[1] } };
    const json = Buffer.from(JSON.stringify(env)), jpg = Buffer.from([255,216,255,217]);
    sock.end(Buffer.concat([Buffer.from(`ACF1 ${json.length} ${jpg.length}\n`), json, jpg]));
  }));
  await new Promise(resolve => server.listen(socket, resolve));
  t.after(async () => { await new Promise(resolve => server.close(resolve)); await rm(dir, { recursive: true, force: true }); });
  for (const name of ['frame', 'frame_reframe']) {
    const result = await new Promise((resolve, reject) => {
      const child = execFile(process.execPath, ['slab/bin/frame-mcp.mjs'], {
        env: { ...process.env, SLAB_FRAME_SOCK: socket, SLAB_PUPPET_CONFIG: config },
      }, (error, stdout) => error ? reject(error) : resolve(JSON.parse(stdout).result));
      child.stdin.end(JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'tools/call', params: {
        name, arguments: { machine: 'fixture', ...(name === 'frame' ? { visual: false } : {}) },
      } }) + '\n');
    });
    assert.ok(!result.isError, JSON.stringify(result));
    assert.match(modes.at(-1), /\bnovisual\b/);
    if (name === 'frame_reframe') {
      assert.match(modes.at(-1), /\bdiff\b/);
      assert.match(result.content[0].text, /unchanged/);
    }
  }
  assert.equal(modes.length, 2, 'unchanged reframe must not request another capture');
});
test('Puppet uses recent in-memory JPEG without a CDP screenshot', async () => {
  const {machine, calls} = browser();
  machine.liveFrames.set('session', {data: 'live-image', at: Date.now()});
  assert.equal(await machine.shot(null, {format: 'jpeg'}), 'live-image');
  assert.equal(calls.length, 0);
});
test('Puppet falls back for stale, missing, PNG, and explicitly fresh captures', async () => {
  for (const mode of ['stale', 'missing', 'png', 'fresh']) {
    const {machine, calls} = browser();
    if (mode !== 'missing') machine.liveFrames.set('session', {data: 'old-image', at: Date.now() - (mode === 'stale' ? 1000 : 0)});
    assert.equal(await machine.shot(null, {format: mode === 'png' ? 'png' : 'jpeg', fresh: mode === 'fresh'}), 'fresh-image');
    assert.equal(calls[0].method, 'Page.captureScreenshot');
  }
});
test('Puppet invalidates only the acted-on browser session', () => {
  const {machine} = browser();
  for (const method of ['Input.dispatchMouseEvent', 'Input.dispatchKeyEvent', 'Runtime.evaluate', 'Page.navigate', 'Page.reload']) {
    machine.liveFrames.set('session', {data: 'before'});
    machine.liveFrames.set('other', {data: 'other'});
    machine.invalidateFrame(method, 'session');
    assert.equal(machine.liveFrames.has('session'), false);
    assert.equal(machine.liveFrames.has('other'), true);
  }
  machine.invalidateFrame('Page.screencastFrameAck', 'other');
  assert.equal(machine.liveFrames.has('other'), true);
});

test('Puppet compact list retains exact page IDs and URLs with a full-state escape hatch', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'puppet-list-'));
  const socket = join(dir, 'puppet.sock');
  const state = { fixture: { connected: true, lazy: false, managedLifecycle: false, lastActiveAt: null, lastError: null,
    targets: ['https://example.test/path?q=a,b'],
    pages: [{ id: 'exact-page-id', title: 'Page, title', url: 'https://example.test/path?q=a,b' }] } };
  const server = net.createServer(sock => sock.once('data', () => sock.end(JSON.stringify({ result: state }) + '\n')));
  await new Promise(resolve => server.listen(socket, resolve));
  t.after(async () => { await new Promise(resolve => server.close(resolve)); await rm(dir, { recursive: true, force: true }); });
  const call = full => new Promise((resolve, reject) => {
    const child = execFile(process.execPath, ['slab/bin/puppet-mcp.mjs'], {
      env: { ...process.env, SLAB_PUPPET_SOCK: socket },
    }, (error, stdout) => error ? reject(error) : resolve(JSON.parse(stdout).result.content[0].text));
    child.stdin.end(JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name: 'puppet_list', arguments: { full } } }) + '\n');
  });
  const compact = await call(false), full = await call(true);
  assert.match(compact, /pages\[1\]\{machine,id,title,url\}/);
  assert.match(compact, /fixture,exact-page-id,"Page, title","https:\/\/example.test\/path\?q=a,b"/);
  assert.deepEqual(JSON.parse(full), state);
  assert.ok(Buffer.byteLength(compact) < Buffer.byteLength(full));
  t.diagnostic(`Puppet fixture list bytes: ${Buffer.byteLength(full)} → ${Buffer.byteLength(compact)}`);
});
