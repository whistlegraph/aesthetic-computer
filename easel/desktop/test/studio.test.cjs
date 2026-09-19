const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const studio = require('../studio.cjs');

const scratch = () => fs.mkdtempSync(path.join(os.tmpdir(), 'aesel-studio-'));

test('launch options read values and flags from argv', () => {
  const launch = studio.launchOptions(['--cwd', '/tmp/piece', '--no-autopublish', '--model']);
  assert.equal(launch.option('--cwd'), '/tmp/piece');
  assert.equal(launch.option('--model'), '');
  assert.equal(launch.option('--piece'), '');
  assert.equal(launch.flag('--no-autopublish'), true);
  assert.equal(launch.flag('--cwd'), true);
  assert.equal(launch.flag('--resume'), false);
});

test('session files are keyed by workspace and instance', () => {
  const a = studio.sessionFile('/state', '/work/a', 'default');
  assert.equal(path.dirname(a), path.join('/state', 'sessions'));
  assert.notEqual(a, studio.sessionFile('/state', '/work/a', 'window-11111111-1111-4111-8111-111111111111'));
  assert.equal(a, studio.sessionFile('/state', '/work/a', 'default'));
});

test('fresh continuation markers reopen oldest first and every marker is consumed', () => {
  const dir = scratch();
  const now = 1_000_000;
  const write = (name, marker) => fs.writeFileSync(path.join(dir, name + '.json.continue'), JSON.stringify(marker));
  write('b', { cwd: '/work/b', instance: 'window-11111111-1111-4111-8111-111111111111', host: 'studio', at: now - 500 });
  write('a', { cwd: '/work/a', instance: 'default', host: 'studio', at: now - 5000 });
  write('stale', { cwd: '/work/stale', instance: 'default', host: 'studio', at: now - studio.CONTINUE_WITHIN_MS });
  write('future', { cwd: '/work/future', instance: 'default', host: 'studio', at: now + 10 });
  write('other-host', { cwd: '/work/app', instance: 'default', host: 'piece-app-id', at: now - 10 });
  write('legacy', { cwd: '/work/legacy', at: now - 10 });
  write('broken', { cwd: 7, at: now });
  fs.writeFileSync(path.join(dir, 'session.json'), '{}');
  const found = studio.resumableSessions(dir, { host: 'studio', now });
  assert.deepEqual(found.map(s => [s.workspace, s.instance]), [
    ['/work/a', 'default'],
    ['/work/b', 'window-11111111-1111-4111-8111-111111111111'],
    ['/work/legacy', 'default'],
  ]);
  assert.deepEqual(fs.readdirSync(dir), ['session.json']);
  assert.deepEqual(studio.resumableSessions(dir, { host: 'studio', now }), []);
});

test('a piece app resumes only its own markers', () => {
  const dir = scratch();
  fs.writeFileSync(path.join(dir, 'x.json.continue'), JSON.stringify({ cwd: '/work/x', instance: 'default', host: 'app-1', at: 50 }));
  fs.writeFileSync(path.join(dir, 'y.json.continue'), JSON.stringify({ cwd: '/work/y', instance: 'default', host: 'studio', at: 50 }));
  assert.deepEqual(studio.resumableSessions(dir, { host: 'app-1', now: 100 }).map(s => s.workspace), ['/work/x']);
});

test('writeContinuation records what resumableSessions reads', () => {
  const dir = scratch();
  const file = path.join(dir, 'k.json.continue');
  studio.writeContinuation(file, { workspace: '/work/k', instance: 'default', host: 'studio', at: 400 });
  assert.equal((fs.statSync(file).mode & 0o777), 0o600);
  assert.deepEqual(studio.resumableSessions(dir, { host: 'studio', now: 401 }), [{ workspace: '/work/k', instance: 'default', at: 400 }]);
});

test('windows in one process take consecutive letters and dead owners are swept', () => {
  const dir = scratch();
  const live = new Set([100, 200]);
  const alive = pid => { if (!live.has(pid)) { const error = new Error('ESRCH'); error.code = 'ESRCH'; throw error; } };
  fs.writeFileSync(path.join(dir, 'A.json'), JSON.stringify({ pid: 999, instance: 'default', workspace: '/gone' }));
  fs.writeFileSync(path.join(dir, 'B.json'), JSON.stringify({ pid: 100, instance: 'default', workspace: '/other' }));
  const first = studio.claimAddress(dir, { pid: 200, instance: 'default', workspace: '/w1', alive });
  const second = studio.claimAddress(dir, { pid: 200, instance: 'window-11111111-1111-4111-8111-111111111111', workspace: '/w2', alive });
  assert.equal(first.label, 'A');
  assert.equal(second.label, 'C');
  assert.equal(JSON.parse(fs.readFileSync(second.path, 'utf8')).instance, 'window-11111111-1111-4111-8111-111111111111');
  studio.releaseAddress(first, { pid: 200, instance: 'default' });
  assert.equal(fs.existsSync(first.path), false);
  studio.releaseAddress(second, { pid: 200, instance: 'default' });
  assert.equal(fs.existsSync(second.path), true, 'another window of the same process keeps its letter');
  studio.releaseAddress(second, { pid: 200, instance: 'window-11111111-1111-4111-8111-111111111111' });
  assert.equal(fs.existsSync(second.path), false);
  assert.equal(fs.existsSync(path.join(dir, 'B.json')), true, 'a live owner in another process is untouched');
});

test('an exhausted alphabet falls back to the pid label', () => {
  const dir = scratch();
  const alive = () => {};
  for (let index = 0; index < 26; index++) fs.writeFileSync(path.join(dir, `${String.fromCharCode(65 + index)}.json`), JSON.stringify({ pid: 1, instance: 'default' }));
  const address = studio.claimAddress(dir, { pid: 4242, instance: 'default', workspace: '/w', alive });
  assert.deepEqual(address, { label: '4242', path: '' });
  studio.releaseAddress(address, { pid: 4242, instance: 'default' });
});
