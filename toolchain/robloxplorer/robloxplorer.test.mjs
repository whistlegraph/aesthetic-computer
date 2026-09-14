import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { id, main, publishPlan, request } from './robloxplorer.mjs';

test('IDs cannot inject paths or query parameters', () => {
  for (const value of ['', '0', '-1', '1?other=2', '../2', undefined]) assert.throws(() => id(value));
  assert.equal(id('1366409382'), '1366409382');
});

test('publish produces a local plan and rejects unsupported inputs', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'robloxplorer-'));
  try {
    const file = join(dir, 'place.rbxlx');
    writeFileSync(file, '<roblox version="4"></roblox>');
    const original = globalThis.fetch;
    globalThis.fetch = () => { throw new Error('Unexpected network access'); };
    try {
      const plan = await main(['publish', file, '123', '456']);
      assert.equal(plan.live, false);
      assert.equal(plan.contentType, 'application/xml');
      assert.match(plan.url, /\/123\/places\/456\/versions\?versionType=Published$/);
      assert.equal(plan.sha256.length, 64);
      assert.equal(plan.body, undefined);
      await assert.rejects(main(['publish', file, '123', '456', '--lve']));
      assert.throws(() => publishPlan('script.lua', '123', '456'));
      writeFileSync(file, '');
      assert.throws(() => publishPlan(file, '123', '456'));
    } finally { globalThis.fetch = original; }
  } finally { rmSync(dir, { recursive: true, force: true }); }
});

test('requests reject redirects and redact server errors', async () => {
  await assert.rejects(request('https://apis.roblox.com/test', {}, async (_, options) => {
    assert.equal(options.redirect, 'error');
    assert.ok(options.signal);
    return { ok: false, status: 403, text: async () => 'private credential details' };
  }), { message: 'Roblox HTTP 403' });
});

test('explicit live publishing sends file bytes and returns a version', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'robloxplorer-live-'));
  const original = globalThis.fetch;
  const oldKey = process.env.ROBLOX_API_KEY;
  try {
    const file = join(dir, 'place.rbxl');
    const bytes = Buffer.from([1, 2, 3]);
    writeFileSync(file, bytes);
    process.env.ROBLOX_API_KEY = 'test-only-key';
    globalThis.fetch = async (address, options) => {
      assert.match(address, /\/123\/places\/456\/versions/);
      assert.equal(options.method, 'POST');
      assert.equal(options.headers['x-api-key'], 'test-only-key');
      assert.equal(options.headers['Content-Type'], 'application/octet-stream');
      assert.deepEqual(options.body, bytes);
      return { ok: true, status: 200, json: async () => ({ versionNumber: 7 }) };
    };
    const result = await main(['publish', file, '123', '456', '--live']);
    assert.equal(result.result.versionNumber, 7);
    assert.equal(JSON.stringify(result).includes('test-only-key'), false);
  } finally {
    globalThis.fetch = original;
    if (oldKey === undefined) delete process.env.ROBLOX_API_KEY;
    else process.env.ROBLOX_API_KEY = oldKey;
    rmSync(dir, { recursive: true, force: true });
  }
});
