import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

const source = readFileSync(new URL('../account.mjs', import.meta.url), 'utf8')
  .replace(/^import .*;$/m, '')
  .replace('export default function mountAccount', 'function mountAccount');

function fixture({ url = 'https://oskiewar.com/', callback, native = null,
  handle = 'tester', failNative = false } = {}) {
  const elements = new Map();
  const calls = [];
  let signed = false;
  const location = new URL(url);
  const account = { ready: false, signedIn: false, handle: '', colors: [] };
  const context = vm.createContext({
    URL, console, location,
    history: { replaceState(_state, _title, path) { location.href = new URL(path, location).href; } },
    document: { querySelector(selector) {
      if (!elements.has(selector)) elements.set(selector, {
        hidden: true, value: '', textContent: '', handlers: {},
        classList: { toggle() {} }, focus() {}, select() {},
        addEventListener(type, handler) { this.handlers[type] = handler; },
      });
      return elements.get(selector);
    } },
    __oskiewarAccount: account,
    otpSignIn: () => ({
      session() { calls.push('otp'); if (failNative) throw new Error('storage unavailable'); return native; },
      token: async () => 'native-token',
    }),
    auth0: { createAuth0Client: async () => ({
      checkSession: async () => { calls.push('silent'); },
      handleRedirectCallback: async (value) => {
        calls.push(['callback', value]);
        const result = await callback?.();
        signed = true;
        return result;
      },
      getUser: async () => signed ? { sub: 'fixture-user' } : undefined,
      loginWithRedirect: async options => { calls.push(['redirect', options]); },
    }) },
    fetch: async url => ({ ok: true, json: async () =>
      url.includes('/handle?') ? { handle } : { colors: [] } }),
  });
  const door = vm.runInContext(source + '\nmountAccount()', context);
  return { door, account, calls, location, context, elements };
}

test('web callback finishes before ready, overrides old OTP, and restores its room', async () => {
  let finish;
  const f = fixture({ url: 'https://oskiewar.com/?code=fixture&state=fixture',
    native: { sub: 'old-user' }, callback: () => new Promise(resolve => { finish = resolve; }) });
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(f.door.redirectPending, true);
  assert.equal(f.account.ready, false);
  finish({ appState: { returnTo: '/daffo394?map=station#play' } });
  await f.door.ready;
  assert.equal(f.account.signedIn, true);
  assert.equal(f.account.handle, '@TESTER');
  assert.equal(f.location.href, 'https://oskiewar.com/daffo394?map=station#play');
  assert.equal(f.calls.includes('otp'), false);
  assert.equal(f.calls.includes('silent'), false);
  await f.door.restore();
  assert.equal(f.calls.filter(call => call[0] === 'callback').length, 1);
});

test('callback uses captured URL even if another component changes the address', async () => {
  const original = 'https://oskiewar.com/?code=fixture&state=fixture';
  const f = fixture({ url: original });
  f.location.href = 'https://oskiewar.com/daffo394';
  await f.door.ready;
  assert.equal(f.calls.find(call => call[0] === 'callback')[1], original);
});

test('failed callback displays a retry and removes authorization parameters', async () => {
  const f = fixture({ url: 'https://oskiewar.com/?error=access_denied&state=fixture&opponent=dummy',
    callback: async () => { throw new Error('invalid state'); } });
  await f.door.ready;
  assert.equal(f.account.ready, true);
  assert.equal(f.account.signedIn, false);
  assert.equal(f.context.__oskiewarAccountOpen, true);
  assert.equal(f.elements.get('#account-panel').hidden, false);
  assert.match(f.elements.get('#account-note').textContent, /please try again/);
  assert.equal(f.location.search, '?opponent=dummy');
});

test('a newly signed-in player without a handle gets the handle form', async () => {
  const f = fixture({ url: 'https://oskiewar.com/?code=fixture&state=fixture', handle: '' });
  await f.door.ready;
  assert.equal(f.account.signedIn, true);
  assert.equal(f.context.__oskiewarAccountOpen, true);
  assert.equal(f.elements.get('#account-title').textContent, 'pick a handle');
});

test('callback return addresses cannot navigate away from Oskiewar', async () => {
  for (const returnTo of ['https://example.com/', '//example.com/']) {
    const f = fixture({ url: 'https://oskiewar.com/?code=fixture&state=fixture',
      callback: async () => ({ appState: { returnTo } }) });
    await f.door.ready;
    assert.equal(f.location.href, 'https://oskiewar.com/');
    assert.equal(f.account.signedIn, true);
  }
});

test('normal restoration keeps the OTP session and does not handle a callback', async () => {
  const f = fixture({ native: { sub: 'native-user' } });
  await f.door.ready;
  assert.equal(f.door.redirectPending, false);
  assert.equal(f.account.handle, '@TESTER');
  assert.equal(f.calls.length, 1);
  assert.equal(f.calls[0], 'otp');
});

test('storage failure still settles readiness', async () => {
  const f = fixture({ failNative: true });
  await f.door.ready;
  assert.equal(f.account.ready, true);
  assert.equal(f.account.signedIn, false);
});

test('web sign-in remembers the current room and query', async () => {
  const f = fixture({ url: 'https://oskiewar.com/daffo394?map=station' });
  await f.door.ready;
  await f.elements.get('#account-redirect').handlers.click();
  const options = f.calls.find(call => call[0] === 'redirect')[1];
  assert.equal(options.appState.returnTo, '/daffo394?map=station');
  assert.equal(options.authorizationParams.redirect_uri, 'https://oskiewar.com');
});


test('sign-in from the title returns home even with a legacy room address', async () => {
  const f = fixture({ url: 'https://oskiewar.com/daffo394' });
  f.context.__oskiewarTouch = { screen: 'title' };
  await f.door.ready;
  await f.elements.get('#account-redirect').handlers.click();
  const options = f.calls.find(call => call[0] === 'redirect')[1];
  assert.equal(options.appState.returnTo, '/');
});
