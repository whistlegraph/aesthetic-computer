import assert from 'node:assert/strict';
import test, { mock } from 'node:test';
mock.module('../backend/authorization.mjs', { exports: { authorize: async headers => headers?.authorization ? { sub: 'auth0|fixture' } : null } });
const { handler } = await import('../netlify/functions/oskiewar-submission.mjs');
const { pseudonym } = await import('../netlify/functions/oskiewar-consent.mjs');
test('submission bridge authenticates, strips filenames/identity/retention, forwards capability, fails closed', async () => {
  const saved = { ...process.env }; const originalFetch = globalThis.fetch;
  try {
    process.env.REGARDE_GATEWAY_URL = 'https://gate.invalid/v0/gateway';
    process.env.REGARDE_SUBJECT_SALT = 'fixture'; process.env.REGARDE_GATEWAY_TOKEN = 'deployer';
    let seen;
    globalThis.fetch = async (url, options) => { seen = { url: String(url), ...options }; return Response.json({ manifest: [{ hash: 'sha256:fixture' }], retention: 'pilot_deadline', purge_at: '2030-01-01T00:00:00.000Z' }, { status: 201 }); };
    const event = { httpMethod: 'POST', headers: { authorization: 'Bearer fixture' }, body: JSON.stringify({ capability: 'signed-capability', subject: 'imposter', retention: 'forever', files: [{source:'appearance', base64: btoa('fixture'), filename: 'person.jpg'}] }) };
    assert.equal((await handler({ ...event, headers: {} })).statusCode, 401);
    assert.equal(seen, undefined);
    const result = await handler(event); assert.equal(result.statusCode, 201);
    assert.equal(seen.url, 'https://gate.invalid/v0/submission');
    assert.deepEqual(JSON.parse(seen.body), { subject: pseudonym('auth0|fixture', 'fixture'), capability: 'signed-capability', files: [{source:'appearance', base64:btoa('fixture')}] });
    assert.equal(seen.headers.Authorization, 'Bearer deployer');
    globalThis.fetch = async () => Response.json({error:'expired'}, {status:403});
    assert.equal((await handler(event)).statusCode, 403);
    globalThis.fetch = async () => { throw Error('offline'); };
    assert.equal((await handler(event)).statusCode, 502);
    delete process.env.REGARDE_GATEWAY_TOKEN;
    assert.equal((await handler(event)).statusCode, 503);
  } finally { globalThis.fetch = originalFetch; for (const key of ['REGARDE_GATEWAY_URL','REGARDE_SUBJECT_SALT','REGARDE_GATEWAY_TOKEN']) { if (saved[key] === undefined) delete process.env[key]; else process.env[key] = saved[key]; } }
});
