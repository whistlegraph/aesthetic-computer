// node system/tests/braincells-store.browser.mjs (uses installed Chrome)
// The store path: a signed-out visitor is locked out; a signed-in one picks a pack,
// and the page opens its own checkout instead of needing Whistlegraph.
import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';
import { chromium, devices } from 'playwright';
const root = fileURLToPath(new URL('../public/', import.meta.url));
const server = createServer(async (req, res) => {
  try {
    const path = new URL(req.url, 'http://localhost').pathname;
    res.setHeader('Content-Type', path.endsWith('.css') ? 'text/css' : path.endsWith('.svg') ? 'image/svg+xml' : path.endsWith('/') ? 'text/html' : 'application/javascript');
    res.end(await readFile(join(root, path, path.endsWith('/') ? 'index.html' : '')));
  } catch { res.statusCode = 404; res.end(); }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const origin = `http://127.0.0.1:${server.address().port}`;
const browser = await chromium.launch({ headless:true, channel:process.env.PLAYWRIGHT_CHANNEL || 'chrome' });
try {
  const page = await browser.newPage({ ...devices['iPhone 13'] });
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  const secret = 'b'.repeat(64), sender = 'tz1inPpZMzFUv5mkmqDMEC8sYxmEq53vxRhw', calls = [];
  let intent = { id:'store', handle:'tester', status:'created', credits:1_000_000, usd:5, pack:'braincells-1m-v1',
    offers:[{id:'braincells-600k-v1',credits:600_000,usd:3},{id:'braincells-1m-v1',credits:1_000_000,usd:5}],
    recipient:'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE', payload:'05010000000474657374' };
  let phase = 'confirming';
  await page.route('**/api/easel-tezos', async route => {
    const request = route.request().postDataJSON(), auth = route.request().headers().authorization;
    calls.push(request.action);
    if (request.action === 'price') return route.fulfill({ json:{ usdPerTez:0.4 } });
    if (request.action === 'create') {
      assert.equal(auth, 'Bearer ac-access-token', 'create uses the AC login, never a checkout secret');
      return route.fulfill({ json:{ ...intent, checkoutURL:`https://aesthetic.computer/braincells/#${secret}` } });
    }
    assert.equal(auth, `Bearer ${secret}`);
    if (request.action === 'quote') {
      assert.equal(request.pack, 'braincells-600k-v1');
      intent = { ...intent, status:'quoted', sender, pack:request.pack, credits:600_000, usd:3, amountMutez:'7500000', expiresAt:new Date(Date.now() + 900_000).toISOString() };
    }
    await route.fulfill({ json:request.action === 'confirm' ? { ...intent, status:phase } : intent });
  });
  await page.route('**/handle?for=*', route => route.fulfill({ json:{ handle:'tester' } }));
  // Sign-in fixture: the real Auth0 dance is covered by the signup e2e tests.
  await page.route('**/aesthetic.computer/lib/auth0-otp.mjs', route => route.fulfill({ contentType:'application/javascript', body:`
    const read = () => JSON.parse(localStorage.getItem('fixture-otp') || 'null');
    export function otpSignIn() { return {
      session: read, token: async () => read()?.access || null, forget: () => localStorage.removeItem('fixture-otp'),
    }; }` }));
  await page.route('**/vendor/beacon-sdk.min.js', route => route.fulfill({ contentType:'application/javascript', body:`
    window.operations = [];
    window.beacon = { DAppClient:class {
      async getActiveAccount(){ return {address:'${sender}',publicKey:'fixture-key',network:{type:'mainnet'}}; }
      async requestSignPayload(){ return {signature:'fixture-signature'}; }
      async requestOperation(request){ window.operations.push(request); return {transactionHash:'o'+'a'.repeat(50)}; }
    }};` }));

  await page.goto(`${origin}/braincells/`);
  // Signed out: locked. One way forward, nothing to pick or read.
  await page.getByRole('link', { name:'Sign in', exact:true }).waitFor();
  assert.equal(await page.getByRole('link', { name:'Sign in', exact:true }).getAttribute('href'), 'https://aesthetic.computer/');
  for (const hidden of ['#lede', '#pack-label', '#price', '#connect'])
    assert.equal(await page.locator(hidden).isVisible(), false, `${hidden} is locked away when signed out`);
  assert.equal(await page.locator('input').count(), 0, 'no sign-in form on this page');
  assert.ok(!calls.includes('create'), 'visiting the store opens no checkout');

  // Signed in on aesthetic.computer (fixture session), then back here.
  await page.evaluate(() => localStorage.setItem('fixture-otp', JSON.stringify({ sub:'auth0|tester', email:'t@example.com', access:'ac-access-token' })));
  await page.reload();
  await page.getByText('For @tester', { exact:true }).waitFor();
  await page.getByRole('heading', { name:'1,000,000 braincells', exact:true }).waitFor();
  await page.getByText('$5, about 12.5 tez, plus the wallet’s network fee.', { exact:true }).waitFor();
  assert.equal(await page.getByRole('link', { name:'Sign in' }).isVisible(), false);

  await page.getByLabel('Amount', { exact:true }).selectOption('braincells-600k-v1');
  await page.getByRole('heading', { name:'600,000 braincells', exact:true }).waitFor();
  await page.getByRole('button', { name:'Connect wallet', exact:true }).click();
  await page.getByRole('button', { name:'Pay 7.5 tez', exact:true }).waitFor();
  assert.deepEqual(calls.filter(c => c !== 'price'), ['create', 'status', 'quote']);
  assert.equal(await page.evaluate(() => window.operations.length), 0, 'connecting never sends funds');

  await page.getByRole('button', { name:'Pay 7.5 tez', exact:true }).click();
  await page.getByText('Payment seen. Waiting for three Tezos confirmations…', { exact:true }).waitFor();
  phase = 'credited';
  await page.getByRole('button', { name:'Check payment', exact:true }).click();
  await page.getByText('Braincells added. They’re ready to use in AC.', { exact:true }).waitFor();
  assert.equal(await page.getByRole('link', { name:'Return to Whistlegraph' }).isVisible(), false, 'web buyers are not sent to the app');

  await page.getByRole('button', { name:'Buy more braincells', exact:true }).click();
  await page.getByText('For @tester', { exact:true }).waitFor();
  await page.getByRole('button', { name:'Connect wallet', exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:'Check payment' }).isVisible(), false);
  assert.deepEqual(errors, []);
  console.log('PASS: locked when signed out, store when signed in, pack choice, self-opened checkout, payment, buy more.');
} finally { await browser.close(); server.close(); }
