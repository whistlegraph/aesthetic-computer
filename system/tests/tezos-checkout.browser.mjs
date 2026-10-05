// node system/tests/tezos-checkout.browser.mjs (uses installed Chrome)
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
  const secret = 'a'.repeat(64), sender = 'tz1inPpZMzFUv5mkmqDMEC8sYxmEq53vxRhw';
  let intent = { id:'test', handle:'tester', status:'created', credits:1_000_000, usd:5,
    recipient:'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE', payload:'05010000000474657374' };
  let phase = 'confirming';
  await page.route('**/api/easel-tezos', async route => {
    assert.equal(route.request().headers().authorization, `Bearer ${secret}`);
    const request = route.request().postDataJSON();
    if (request.action === 'quote') {
      assert.equal(request.address, sender); assert.equal(request.signature, 'fixture-signature');
      intent = { ...intent, status:'quoted', sender, amountMutez:'12500000', expiresAt:new Date(Date.now() + 900_000).toISOString() };
    }
    const value = request.action === 'confirm' ? { ...intent, status:phase } : intent;
    await route.fulfill({ json:value });
  });
  // Wallet fixture only: real signatures, ledger matching and retries are tested server-side.
  await page.route('**/vendor/beacon-sdk.min.js', route => route.fulfill({ contentType:'application/javascript', body:`
    window.operations = [];
    window.beacon = { DAppClient:class {
      constructor(config) { if(config.network.type!=='mainnet')throw Error('wrong network'); }
      async getActiveAccount(){ return {address:'${sender}',publicKey:'fixture-key',network:{type:'mainnet'}}; }
      async requestSignPayload(request){ if(request.signingType!=='micheline')throw Error('wrong signing type'); return {signature:'fixture-signature'}; }
      async requestOperation(request){ window.operations.push(request); return {transactionHash:'o'+'a'.repeat(50)}; }
    }};` }));
  await page.goto(`${origin}/braincells/#${secret}`);
  await page.getByRole('button', { name:'Connect wallet', exact:true }).click();
  await page.getByRole('button', { name:'Pay 12.5 tez', exact:true }).waitFor();
  assert.equal(new URL(page.url()).hash, '');
  assert.equal(await page.evaluate(() => window.operations.length), 0, 'connecting/signing never sends funds');
  await page.getByRole('button', { name:'Pay 12.5 tez', exact:true }).click();
  await page.getByText('Payment seen. Waiting for three Tezos confirmations…', { exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:'Pay 12.5 tez', exact:true }).isVisible(), false);
  const operations = await page.evaluate(() => window.operations);
  assert.equal(operations.length, 1);
  assert.deepEqual(operations[0].operationDetails, [{ kind:'transaction', destination:intent.recipient, amount:'12500000' }]);
  await page.reload();
  await page.getByText('Payment seen. Waiting for three Tezos confirmations…', { exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:'Pay 12.5 tez', exact:true }).isVisible(), false, 'reload cannot offer a second payment');
  phase = 'credited';
  await page.getByRole('button', { name:'Check payment', exact:true }).click();
  await page.getByText('Braincells added. They’re ready to use in AC.', { exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:'Check payment', exact:true }).isVisible(), false);
  assert.deepEqual(errors, []);
  console.log('PASS: connect, exact quote, explicit payment, reload recovery, pending and credited states.');
} finally { await browser.close(); server.close(); }
