import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { evaluate } from './evaluate.mjs';
import { createHandler, decisionRequest } from './coach.mjs';

const report = { room: 'private-room', frames: 100, seconds: 5, rounds: 1,
  fighters: [{ seat: 0, name: '@PRIVATE', hitsTaken: 10, blocks: 0,
    attacks: [{ kind: 'punch', thrown: 5, landed: 1 }], notes: ['private note'] }] };
const request = decisionRequest(report);
const result = { answers: { practice: { type: 'choice', choice: 'defense',
  probabilities: { defense: .9, observe: .1 } } } };
const call = { jsonrpc: '2.0', id: 42, method: 'tools/call',
  params: { name: 'coach_jev', arguments: { seat: 0 } } };

test('Gateway protocol sends typed questions and requires credentials', async () => {
  let calls = 0;
  const options = { apiKey: 'test-key', fetchImpl: async (url, init) => {
    calls++;
    assert.equal(url, 'https://ai-gateway.vercel.sh/v4/ai/evaluation-model');
    assert.equal(init.headers['ai-model-id'], 'typesafe-ai/jev');
    assert.equal(init.headers.Authorization, 'Bearer test-key');
    assert.equal(init.redirect, 'error');
    assert.deepEqual(JSON.parse(init.body), { ...request,
      providerOptions: { gateway: { zeroDataRetention: true } } });
    return Response.json(result);
  } };
  assert.deepEqual(await evaluate(request, options), result);
  await assert.rejects(evaluate(request, { ...options, apiKey: '' }), /AI_GATEWAY_API_KEY/);
  assert.equal(calls, 1);
});

test('rejects unknown decisions and does not echo upstream errors', async () => {
  await assert.rejects(evaluate(request, { apiKey: 'x', fetchImpl: async () =>
    Response.json({ answers: { practice: { type: 'choice', choice: 'publish' } } }) }), /Unknown choice/);
  await assert.rejects(evaluate(request, { apiKey: 'x', fetchImpl: async () =>
    new Response('sensitive request echoed', { status: 401 }) }), /^Error: Jev gateway returned HTTP 401\.$/);
});

test('coaching evidence excludes room, names and prose; requires an observed seat', () => {
  assert.doesNotMatch(JSON.stringify(request), /private|PRIVATE/);
  assert.equal(request.state.fighters[0].hitsTaken, 10);
  assert.throws(() => decisionRequest(report, 1), /No fighter/);
  assert.throws(() => decisionRequest(report, 2), /seat/);
  assert.throws(() => decisionRequest({ ...report, frames: 0 }), /No fighter/);
});

test('new tool uses the existing ledger and preserves workshop tool routing', async () => {
  const seen = [];
  const handler = createHandler({ coachHandler: async message => {
    seen.push(message);
    if (message.method === 'tools/list') return { result: { tools: [{ name: 'coach_workshop' }] } };
    return { result: { content: [{ type: 'text', text: JSON.stringify(report) }] } };
  }, evaluateImpl: async input => { assert.deepEqual(input, request); return result; } });
  const listed = await handler({ method: 'tools/list' });
  assert.deepEqual(listed.result.tools.map(t => t.name), ['coach_workshop', 'coach_jev']);
  const response = await handler(call);
  const content = JSON.parse(response.result.content[0].text);
  assert.equal(content.practice, 'defense');
  assert.equal(content.cue, 'Practice blocking the next incoming attack.');
  assert.equal(seen.at(-1).params.name, 'coach_analyze');
  await handler({ ...call, params: { name: 'coach_workshop', arguments: { op: 'inspect' } } });
  assert.equal(seen.at(-1).params.arguments.op, 'inspect');
});

test('unseated coach errors pass through without an API call', async () => {
  const error = { result: { isError: true, content: [{ type: 'text', text: 'Call coach_in first' }] } };
  const handler = createHandler({ coachHandler: async () => error,
    evaluateImpl: async () => { assert.fail('must not call Jev'); } });
  assert.equal(await handler(call), error);
});

test('overlapping decisions do not issue duplicate requests', async () => {
  let resolve;
  const handler = createHandler({ coachHandler: async () => ({ result: {
    content: [{ type: 'text', text: JSON.stringify(report) }] } }),
  evaluateImpl: () => new Promise(done => { resolve = done; }) });
  const first = handler(call);
  await Promise.resolve();
  const second = await handler(call);
  assert.equal(second.result.isError, true);
  resolve(result);
  assert.equal((await first).result.isError, undefined);
});

test('portable launcher serves MCP from outside the checkout without credentials', () => {
  const launcher = fileURLToPath(new URL('./run.sh', import.meta.url));
  const messages = [
    { jsonrpc: '2.0', id: 1, method: 'initialize', params: {} },
    { jsonrpc: '2.0', id: 2, method: 'tools/list' },
  ];
  const child = spawnSync('/bin/bash', [launcher], {
    cwd: tmpdir(), encoding: 'utf8', timeout: 5000,
    env: { ...process.env, JEV_NODE: process.execPath,
      JEV_ENV_FILE: '/nonexistent-jev-test.env', AI_GATEWAY_API_KEY: '' },
    input: messages.map(m => JSON.stringify(m)).join('\n') + '\n',
  });
  assert.equal(child.status, 0, child.stderr);
  const replies = child.stdout.trim().split('\n').map(line => JSON.parse(line));
  assert.ok(replies.find(r => r.id === 1)?.result.serverInfo);
  const names = replies.find(r => r.id === 2).result.tools.map(t => t.name);
  assert.ok(names.includes('coach_workshop'));
  assert.ok(names.includes('coach_jev'));
});
