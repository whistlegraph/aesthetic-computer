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
  const result = await captureFrame('fixture', { memory: true, noOCR: true, crop: [40,60,100,80], quietOverlay: true });
  assert.deepEqual(result.env, envelope);
  assert.deepEqual(result.jpg, jpg);
  assert.equal(requests[0].mode, 'window noocr quiet-overlay crop=40,60,100,80');
  await assert.rejects(captureFrame('missing', { memory: true }), /unknown machine/);
  envelope = { error: 'fixture transport failure' };
  await assert.rejects(captureFrame('fixture', { memory: true, pressAt: [1,2] }), /fixture transport failure/);
  assert.equal(requests.length, 2, 'failed action must not be sent again');
  envelope = null;
  await assert.rejects(captureFrame('fixture', { memory: true, pressAt: [1,2] }), /closed before returning/);
  assert.equal(requests.length, 3, 'disconnected action must not be retried');
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
