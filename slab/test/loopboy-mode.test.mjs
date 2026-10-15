import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import { mkdtemp, mkdir, readFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { readLoopboyMode } from '../lib/loopboy-mode.mjs';

const script = new URL('../bin/prox-mcp.mjs', import.meta.url).pathname;
async function fixture(t, agent = 'codex') {
  const home = await mkdtemp(join(tmpdir(), 'loopboy-mode-'));
  t.after(() => rm(home, { recursive: true, force: true }));
  const env = { ...process.env, HOME: home, SLAB_HOME: join(home, '.local/share/slab') };
  const sid = 'cccccccc-1111-2222-3333-444444444444';
  const slab = join(home, '.config/slab');
  const active = join(env.SLAB_HOME, 'state/active-prompts');
  await mkdir(join(slab, 'ledger'), { recursive: true });
  await mkdir(active, { recursive: true });
  const marker = { session_id: sid, agent_pid: process.pid, claude_pid: process.pid,
    agent_type: agent, provider_session_id: 'unchanged-provider-thread', tty: 'ttys006',
    cwd: home, subject: 'keep the history', state: 'working', loopboy_contact: '' };
  await writeFile(join(active, sid), JSON.stringify(marker));
  const entry = { id: sid, name: 'koker', host: 'test', kind: 'session', agentType: agent,
    status: 'working', updated: Date.now(), cwd: home };
  await writeFile(join(slab, 'ledger/local.json'), JSON.stringify({ host: 'test', entries: [entry] }));
  async function call(name, args, extraEnv = {}) {
    const child = spawn(process.execPath, [script], { env: { ...env, ...extraEnv }, stdio: ['pipe', 'pipe', 'pipe'] });
    let output = '';
    child.stdout.on('data', chunk => output += chunk);
    child.stdin.end(JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }) + '\n');
    const [code] = await once(child, 'close');
    assert.equal(code, 0);
    return JSON.parse(output).result;
  }
  return { home, env, sid, slab, active, marker, entry, call,
    config: async () => JSON.parse(await readFile(join(slab, 'loopboy.json'), 'utf8')),
    current: async () => JSON.parse(await readFile(join(active, sid), 'utf8')) };
}

for (const agent of ['claude', 'codex', 'aesel']) {
  test(`${agent} enters, exits, and reenters Loopboy without a new process or conversation`, async t => {
    const f = await fixture(t, agent);
    for (let pass = 0; pass < 2; pass++) {
      const on = await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia', wake: true });
      assert.equal(on.isError, undefined, on.content[0].text);
      assert.match(on.content[0].text, /in place.*no automatic typing/);
      const cfg = await f.config();
      assert.equal(cfg.loops.fia.wake, false);
      assert.equal(cfg.loops.fia.delivery, 'inbox');
      assert.equal(cfg.loops.fia.sessionId, f.sid);
      assert.equal((await f.current()).loopboy_contact, 'fia');
      const off = await f.call('prox_unbind_notification', { handle: 'test:koker' }, { SLAB_LOOPBOY_CONTACT: 'fia' });
      assert.equal(off.isError, undefined, off.content[0].text);
      assert.deepEqual((await f.config()).loops, {});
      assert.equal((await readLoopboyMode(f.sid, f.env)).contact, '');
      const current = await f.current();
      for (const key of ['session_id', 'agent_pid', 'provider_session_id', 'tty', 'subject', 'state']) assert.equal(current[key], f.marker[key]);
      // Simulate the still-running old watcher writing its cached launch contact.
      await writeFile(join(f.active, f.sid), JSON.stringify({ ...current, loopboy_contact: 'fia' }));
      const rebind = await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'alex' });
      assert.equal(rebind.isError, undefined, rebind.content[0].text);
      assert.equal((await f.config()).loops.alex.sessionId, f.sid);
      await f.call('prox_unbind_notification', { handle: 'test:koker' });
    }
    assert.deepEqual((await f.config()).loops, {});
  });
}

test('live route ownership cannot be stolen; unrelated routes survive mode changes', async t => {
  const f = await fixture(t);
  const other = 'dddddddd-1111-2222-3333-444444444444';
  await writeFile(join(f.active, other), JSON.stringify({ session_id: other, agent_pid: process.pid }));
  const loops = { fia: { sessionId: other, wake: false }, alex: { sessionId: 'unrelated' } };
  await writeFile(join(f.slab, 'loopboy.json'), JSON.stringify({ loops }));
  const refused = await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia' });
  assert.equal(refused.isError, true);
  assert.match(refused.content[0].text, /already has a live listener/);
  assert.deepEqual((await f.config()).loops, loops);
  assert.equal((await f.current()).loopboy_contact, '');
  await f.call('prox_unbind_notification', { handle: 'test:koker' });
  assert.deepEqual((await f.config()).loops, loops);
});

test('mode changes refuse dead or ambiguous sessions without writing configuration', async t => {
  const f = await fixture(t);
  await writeFile(join(f.active, f.sid), JSON.stringify({ ...f.marker, agent_pid: 2147483647, claude_pid: 0 }));
  const dead = await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia' });
  assert.equal(dead.isError, true);
  assert.match(dead.content[0].text, /dead process/);
  await writeFile(join(f.slab, 'ledger/local.json'), JSON.stringify({ host: 'test', entries: [f.entry, { ...f.entry, id: 'another' }] }));
  const ambiguous = await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia' });
  assert.equal(ambiguous.isError, true);
  assert.match(ambiguous.content[0].text, /ambiguous/);
  assert.equal(await readLoopboyMode(f.sid, f.env), null);
});

test('exit preserves an explicitly assigned name despite a stale ledger', async t => {
  const f = await fixture(t);
  await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia', name: 'surizo' });
  const off = await f.call('prox_unbind_notification', { handle: 'test:surizo' });
  assert.equal(off.isError, undefined);
  assert.match(off.content[0].text, /test:surizo/);
  assert.equal((await readLoopboyMode(f.sid, f.env)).name, 'surizo');
});

test('launch refuses to replace a contact’s existing live prox before opening anything', async t => {
  const f = await fixture(t);
  await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia' });
  const result = await f.call('prox_launch', { host: 'test', agent: 'codex', loopboyContact: 'fia' });
  assert.equal(result.isError, true);
  assert.match(result.content[0].text, /already has a live prox.*without launching a replacement/);
  assert.equal((await f.config()).loops.fia.sessionId, f.sid);
  const off = await f.call('prox_unbind_notification', { handle: 'test:koker' });
  assert.equal(off.isError, undefined);
  assert.equal((await f.call('prox_unbind_notification', { handle: 'test:koker' })).isError, undefined);
});

test('passive wait delivers through the existing session without launch contact headers', async t => {
  const f = await fixture(t);
  await f.call('prox_bind_notification', { handle: 'test:koker', contact: 'fia' });
  const identity = { SLAB_PROMPT_SESSION_ID: f.sid, SLAB_LOOPBOY_CONTACT: '' };
  const inbox = join(f.env.SLAB_HOME, 'inbox', f.sid);
  await mkdir(inbox, { recursive: true });
  await writeFile(join(inbox, 'messages.jsonl'), JSON.stringify({ v: 1, id: 'arrival', ts: Date.now(),
    from: 'loopboy:fia', to_id: f.sid, text: 'A new message is available.', urgency: 'queue', kind: 'message' }) + '\n');
  const foreign = await f.call('prox_loopboy_wait', { contact: 'alex', timeoutSeconds: 0 }, identity);
  assert.equal(foreign.isError, true);
  const arrival = await f.call('prox_loopboy_wait', { timeoutSeconds: 0 }, identity);
  assert.equal(arrival.isError, undefined);
  assert.match(arrival.content[0].text, /A new message is available/);
  const empty = await f.call('prox_loopboy_wait', { timeoutSeconds: 0 }, identity);
  assert.match(empty.content[0].text, /No queued updates for fia/);
  // Exit revokes a currently waiting tool call, even with stale launch headers.
  const pending = f.call('prox_loopboy_wait', { timeoutSeconds: 5 }, { ...identity, SLAB_LOOPBOY_CONTACT: 'fia' });
  await new Promise(resolve => setTimeout(resolve, 150));
  await f.call('prox_unbind_notification', { handle: 'test:koker' });
  const stopped = await pending;
  assert.equal(stopped.isError, true);
  assert.match(stopped.content[0].text, /mode is off/);
  assert.equal((await f.current()).provider_session_id, f.marker.provider_session_id);
});

test('custom coded sources bind, emit and wait without an iMessage contact', async t => {
  const f = await fixture(t);
  const declaration = { event: 'fishery', source: 'studio', description: 'New visits and actions',
    config: { minutes: 5, scope: 'studio' }, expiresAt: new Date(Date.now() + 60000).toISOString() };
  const bound = await f.call('prox_bind_notification', { handle: 'test:koker', ...declaration });
  assert.equal(bound.isError, undefined, bound.content[0].text);
  const mode = await readLoopboyMode(f.sid, f.env);
  assert.equal(mode.contact, '');
  assert.equal(mode.routeKey, 'fishery:studio');
  assert.deepEqual(mode.config, declaration.config);
  assert.equal((await f.current()).loopboy_event, 'fishery');
  assert.equal((await f.current()).provider_session_id, 'unchanged-provider-thread');
  const emitted = await f.call('prox_loopboy_emit', { event: 'fishery', source: 'studio',
    text: 'New fish arrived', data: { property: 'aesthetic.computer' } });
  assert.equal(emitted.isError, undefined, emitted.content[0].text);
  const identity = { SLAB_PROMPT_SESSION_ID: f.sid };
  const mismatch = await f.call('prox_loopboy_wait', { contact: 'studio', timeoutSeconds: 0 }, identity);
  assert.equal(mismatch.isError, true);
  const received = await f.call('prox_loopboy_wait', { event: 'fishery', source: 'studio', timeoutSeconds: 0 }, identity);
  assert.match(received.content[0].text, /Loopboy event: fishery:studio/);
  assert.match(received.content[0].text, /New fish arrived/);
  assert.match(received.content[0].text, /aesthetic.computer/);
  assert.match((await f.call('prox_loopboy_wait', { timeoutSeconds: 0 }, identity)).content[0].text, /No queued updates/);
  await f.call('prox_unbind_notification', { handle: 'test:koker' });
  assert.equal((await f.call('prox_loopboy_emit', { event: 'fishery', source: 'studio', text: 'After exit' })).isError, true);
  assert.equal((await f.current()).loopboy_source, '');
});

test('event families isolate identical source names and expiry revokes delivery', async t => {
  const f = await fixture(t);
  await f.call('prox_bind_notification', { handle: 'test:koker', event: 'fishery', source: 'studio' });
  assert.equal((await f.call('prox_loopboy_emit', { event: 'build', source: 'studio', text: 'wrong family' })).isError, true);
  const path = join(f.env.SLAB_HOME, 'state/loopboy-modes', `${f.sid}.json`);
  const mode = await readLoopboyMode(f.sid, f.env);
  await writeFile(path, JSON.stringify({ ...mode, expiresAt: '2000-01-01T00:00:00Z' }));
  assert.equal((await f.call('prox_loopboy_emit', { event: 'fishery', source: 'studio', text: 'too late' })).isError, true);
  assert.equal((await f.call('prox_loopboy_wait', { timeoutSeconds: 0 }, { SLAB_PROMPT_SESSION_ID: f.sid })).isError, true);
  const refused = await f.call('prox_bind_notification', { handle: 'test:koker', event: 'fishery', source: 'studio', expiresAt: '2000-01-01T00:00:00Z' });
  assert.equal(refused.isError, true);
});

test('custom declarations reject invalid identities and oversized metadata before mutation', async t => {
  const f = await fixture(t);
  for (const binding of [
    { event: 'fishery', source: '../studio' },
    { event: 'fishery', source: 'studio', contact: 'fia' },
    { event: 'fishery', source: 'studio', config: { content: 'x'.repeat(4096) } },
    { event: 'fishery', source: 'studio', expiresAt: 'tomorrow' },
  ]) {
    const result = await f.call('prox_bind_notification', { handle: 'test:koker', ...binding });
    assert.equal(result.isError, true);
  }
  assert.equal(await readLoopboyMode(f.sid, f.env), null);
});

test('a changed binding generation ends an in-flight wait even for the same source', async t => {
  const f = await fixture(t);
  const args = { handle: 'test:koker', event: 'fishery', source: 'studio' };
  await f.call('prox_bind_notification', args);
  const pending = f.call('prox_loopboy_wait', { timeoutSeconds: 5 }, { SLAB_PROMPT_SESSION_ID: f.sid });
  await new Promise(resolve => setTimeout(resolve, 150));
  await f.call('prox_bind_notification', args);
  const result = await pending;
  assert.equal(result.isError, true);
  assert.match(result.content[0].text, /source changed/);
});

test('custom creation refuses an old host before making a launch request', async t => {
  const f = await fixture(t);
  const result = await f.call('prox_launch', { host: 'test', agent: 'codex',
    loopboy: { event: 'fishery', source: 'studio' } });
  assert.equal(result.isError, true);
  assert.match(result.content[0].text, /does not advertise custom Loopboy sources/);
  assert.equal(await readLoopboyMode(f.sid, f.env), null);
});

test('creation carries the custom source into the launch and binds the newborn marker', async t => {
  const f = await fixture(t);
  await writeFile(join(f.slab, 'ledger/local.json'), JSON.stringify({ host: 'test', ip: '127.0.0.1', loopboyProtocol: 2, entries: [f.entry] }));
  const hook = join(f.home, 'launch-fixture.mjs');
  await writeFile(hook, `import { writeFile, readFile } from 'node:fs/promises';
    import { join } from 'node:path';
    globalThis.fetch = async (url, options) => {
      if (!String(url).endsWith('/launch')) throw new Error('unexpected request');
      const body = JSON.parse(options.body);
      await writeFile(join(process.env.HOME, 'launch-body.json'), JSON.stringify(body));
      const id = 'new-session-1111-2222-3333-444444444444';
      await writeFile(join(process.env.SLAB_HOME, 'state/active-prompts', id), JSON.stringify({
        session_id: id, agent_pid: ${process.pid}, agent_type: 'codex', loopboy_event: body.loopboy.event, loopboy_source: body.loopboy.source
      }));
      const file = join(process.env.HOME, '.config/slab/ledger/local.json');
      const ledger = JSON.parse(await readFile(file, 'utf8'));
      ledger.entries.push({ id, name: 'newfish', host: 'test', agentType: 'codex', status: 'working' });
      await writeFile(file, JSON.stringify(ledger));
      return new Response(JSON.stringify({ ok: true, host: 'test', cwd: process.env.HOME, loopboy: body.loopboy, nudgeScreen: '' }));
    };`);
  const result = await f.call('prox_launch', { host: 'test', agent: 'codex',
    loopboy: { event: 'fishery', source: 'studio', config: { minutes: 5 } } }, { NODE_OPTIONS: `--import=${hook}` });
  assert.equal(result.isError, undefined, result.content[0].text);
  assert.match(result.content[0].text, /bound Loopboy source fishery:studio/);
  const config = await f.config();
  assert.equal(config.loops['fishery:studio'].sessionId, 'new-session-1111-2222-3333-444444444444');
  assert.deepEqual(config.loops['fishery:studio'].config, { minutes: 5 });
  const body = JSON.parse(await readFile(join(f.home, 'launch-body.json'), 'utf8'));
  assert.equal(body.loopboy.source, 'studio');
  assert.equal(body.loopboyContact, undefined);
});
