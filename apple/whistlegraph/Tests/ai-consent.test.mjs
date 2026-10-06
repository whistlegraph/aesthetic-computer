import test from 'node:test';
import assert from 'node:assert/strict';
import {createAIConsentGate} from '../Resources/Web/ai-consent.mjs';
import {runPersonalTurn} from '../../../aesel/src/personal-relay.mjs';
import {MusicalInputSocket} from '../../../aesel/src/musical-input-socket.mjs';

const inference = 'https://aesthetic.computer/api/easel-inference';
const musical = 'https://aesthetic.computer/api/easel-musical-jev';
const sessions = 'https://help.aesthetic.computer/api/aesel/sessions';
const pause = () => new Promise(resolve => setTimeout(resolve, 0));
const isAbort = error => error.name === 'AbortError';
const routes = [inference, musical, sessions, sessions + '/session-1/turn',
  sessions + '/session-1?after=12', sessions + '/session-1/respond'];

for (const route of routes) test('denies before network: ' + route, async () => {
  let calls = 0, required = 0;
  const gate = createAIConsentGate({fetch: async () => { calls++; return Response.json({}); }, onRequired: () => required++});
  await assert.rejects(gate.fetch(route, {method: 'POST', body: 'private input'}), /Allow AI creation/);
  assert.equal(calls, 0);
  assert.equal(required, 1);
});

for (const [label, input] of [
  ['URL', new URL(inference)], ['Request', new Request(musical)], ['relative path', '/api/easel-inference'],
]) test('denies Fetch ' + label + ' input before network', async () => {
  const gate = createAIConsentGate({fetch: () => { assert.fail('Request must not reach network'); }});
  await assert.rejects(gate.fetch(input), /Allow AI creation/);
});

test('only explicit true grants consent, and an allowed request preserves its body and headers', async () => {
  const calls = [];
  const gate = createAIConsentGate({fetch: async (input, options) => {
    calls.push({input, options});
    return new Response('data: complete\n\n', {status: 201, headers: {'x-request-id': 'fixture'}});
  }});
  for (const value of [undefined, null, 1, 'true', {}]) {
    gate.setAllowed(value);
    await assert.rejects(gate.fetch(inference), /Allow AI creation/);
  }
  gate.setAllowed(true);
  const body = JSON.stringify({messages: [{role: 'user', content: 'private prompt'}]});
  const response = await gate.fetch(inference, {method: 'POST', headers: {Authorization: 'fixture'}, body});
  assert.equal(response.status, 201);
  assert.equal(response.headers.get('x-request-id'), 'fixture');
  assert.equal(await response.text(), 'data: complete\n\n');
  assert.equal(calls.length, 1);
  assert.equal(calls[0].options.body, body);
  assert.equal(calls[0].options.headers.Authorization, 'fixture');
  gate.setAllowed(false);
  assert.equal(calls[0].options.signal.aborted, false, 'completed streams release their controller');
});

test('revocation aborts requests still waiting for response headers', async () => {
  let signal;
  const gate = createAIConsentGate({allowed: true, fetch: (_input, options) => {
    signal = options.signal;
    return new Promise((_resolve, reject) => signal.addEventListener('abort', () => reject(signal.reason), {once: true}));
  }});
  const rejection = assert.rejects(gate.fetch(inference), isAbort);
  gate.setAllowed(false);
  await rejection;
  assert.equal(signal.aborted, true);
  await assert.rejects(gate.fetch(inference), /Allow AI creation/);
});

test('revocation rejects late responses even when a transport ignores abort', async () => {
  let respond, cancelled = false;
  const gate = createAIConsentGate({allowed: true, fetch: () => new Promise(resolve => respond = resolve)});
  const rejection = assert.rejects(gate.fetch(inference), isAbort);
  gate.setAllowed(false);
  respond(new Response(new ReadableStream({cancel() { cancelled = true; }})));
  await rejection;
  assert.equal(cancelled, true);
});

test('revocation errors a waiting stream and cancels its reader', {timeout: 2000}, async () => {
  let cancelled = false;
  const gate = createAIConsentGate({allowed: true, fetch: async () => new Response(new ReadableStream({
    cancel() { cancelled = true; },
  }))});
  const response = await gate.fetch(inference);
  const reader = response.body.getReader();
  const rejection = assert.rejects(reader.read(), isAbort);
  gate.setAllowed(false);
  await rejection;
  assert.equal(cancelled, true);
});

test('revocation discards queued response chunks, even before the caller starts reading', {timeout: 2000}, async () => {
  const gate = createAIConsentGate({allowed: true, fetch: async () => new Response(new ReadableStream({
    start(stream) { stream.enqueue(new TextEncoder().encode('private buffered output')); },
  }))});
  const response = await gate.fetch(inference);
  await pause();
  gate.setAllowed(false);
  gate.setAllowed(true);
  await assert.rejects(response.text(), isAbort, 'granting again cannot revive an old stream');
});

test('upstream abort and response cancellation propagate to the transport', {timeout: 2000}, async () => {
  const signals = [];
  const gate = createAIConsentGate({allowed: true, fetch: async (_input, options) => {
    signals.push(options.signal);
    return new Response(new ReadableStream({}));
  }});
  const upstream = new AbortController();
  const response = await gate.fetch(new Request(inference, {signal: upstream.signal}));
  const rejection = assert.rejects(response.body.getReader().read(), isAbort);
  upstream.abort();
  await rejection;
  assert.equal(signals[0].aborted, true);
  const next = await gate.fetch(inference);
  await next.body.cancel();
  assert.equal(signals[1].aborted, true);
});

test('already aborted upstream signals do not start a request', async () => {
  const gate = createAIConsentGate({allowed: true, fetch: () => assert.fail('No request expected')});
  await assert.rejects(gate.fetch(inference, {signal: AbortSignal.abort()}), isAbort);
});

test('no-body responses are preserved and removed from active requests', async () => {
  let signal;
  const response = new Response(null, {status: 204});
  const gate = createAIConsentGate({allowed: true, fetch: async (_input, options) => { signal = options.signal; return response; }});
  assert.equal(await gate.fetch(inference), response);
  gate.setAllowed(false);
  assert.equal(signal.aborted, false);
});

test('interrupt remains usable after revocation; other personal relay endpoints stay denied', async () => {
  const calls = [];
  const gate = createAIConsentGate({fetch: async input => { calls.push(input); return Response.json({ok: true}); }});
  const response = await gate.fetch(sessions + '/session-1/interrupt', {method: 'POST', body: '{}'});
  assert.deepEqual(await response.json(), {ok: true});
  await assert.rejects(gate.fetch(sessions + '/session-1/turn'), /Allow AI creation/);
  await assert.rejects(gate.fetch(sessions + '/session-1/respond'), /Allow AI creation/);
  assert.equal(calls.length, 1);
});

test('account, credit balances and purchases retain ordinary fetch behavior', async () => {
  const calls = [], response = Response.json({ok: true});
  const gate = createAIConsentGate({fetch: async (input, options) => { calls.push({input, options}); return response; }});
  const signal = new AbortController().signal;
  for (const route of ['/api/easel-credits', '/api/handle?for=fixture', '/api/whistlegraph-iap']) {
    assert.equal(await gate.fetch('https://aesthetic.computer' + route, {signal}), response);
  }
  assert.equal(await gate.fetch('https://aesthetic.us.auth0.com/userinfo', {signal}), response);
  gate.setAllowed(false);
  assert.equal(calls.length, 4);
  assert.ok(calls.every(call => call.options.signal === signal));
  assert.equal(signal.aborted, false);
});

test('real personal-relay lifecycle can interrupt after consent revocation', {timeout: 2000}, async () => {
  const calls = [], controller = new AbortController();
  let startedPoll;
  const polling = new Promise(resolve => startedPoll = resolve);
  const gate = createAIConsentGate({allowed: true, fetch: async (input, options) => {
    calls.push(input);
    if (input === sessions) return Response.json({thread: {id: 'session-1'}});
    if (input.endsWith('/turn') || input.endsWith('/interrupt')) return Response.json({ok: true});
    startedPoll();
    return new Promise((_resolve, reject) => options.signal.addEventListener('abort', () => reject(options.signal.reason), {once: true}));
  }});
  const rejection = assert.rejects(runPersonalTurn({token: 'fixture', model: 'anthropic/claude-opus-5',
    content: 'private prompt', fetch: gate.fetch, signal: controller.signal, pollMs: 0}), isAbort);
  await polling;
  gate.setAllowed(false);
  controller.abort(); // engine syncAIConsent also interrupts its owning server.
  await rejection;
  await pause();
  assert.deepEqual(calls, [sessions, sessions + '/session-1/turn', sessions + '/session-1?after=0', sessions + '/session-1/interrupt']);
});

test('musical socket stays closed before grant and suspending it cancels pending observations', {timeout: 2000}, async () => {
  const sockets = [], sent = [];
  class Socket {
    constructor(url) { this.url = url; this.readyState = 1; sockets.push(this); }
    send(message) { sent.push(JSON.parse(message)); }
    close() { this.readyState = 3; this.onclose?.(); }
  }
  const gate = createAIConsentGate({fetch: () => assert.fail('No HTTP fallback expected')});
  const socket = new MusicalInputSocket({token: () => gate.allowed ? 'fixture' : null, WebSocketImpl: Socket, fetchImpl: gate.fetch});
  const send = (...args) => { gate.require(); return socket.fetch(...args); };
  socket.resume();
  assert.equal(sockets.length, 0);
  assert.throws(() => send(musical, {}), /Allow AI creation/);
  gate.setAllowed(true); socket.resume();
  assert.equal(sockets[0].url, 'wss://aesthetic.computer/api/easel-musical-stream');
  sockets[0].onopen();
  sockets[0].onmessage({data: JSON.stringify({type: 'ready'})});
  const rejection = assert.rejects(send(musical, {body: JSON.stringify({sessionId: 'observation', sequence: 1})}), /socket_closed/);
  gate.setAllowed(false); socket.suspend(); // same pair used by engine syncAIConsent
  await rejection;
  assert.equal(sockets[0].readyState, 3);
  assert.equal(socket.pending.size, 0);
  assert.deepEqual(sent.map(message => message.type), ['authenticate', 'observation']);
  assert.throws(() => send(musical, {}), /Allow AI creation/);
});
