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
 for (const [pack, credits, usd, amount] of [['braincells-600k-v1',600_000,3,'7500000'], ['braincells-1m-v1',1_000_000,5,'12500000']]) {
  const tez = String(Number(amount) / 1e6);
  const page = await browser.newPage({ ...devices['iPhone 13'] });
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  const secret = 'a'.repeat(64), sender = 'tz1inPpZMzFUv5mkmqDMEC8sYxmEq53vxRhw';
  let intent = { id:'test', handle:'tester', status:'created', credits:1_000_000, usd:5, pack:'braincells-1m-v1',
    offers:[{id:'braincells-600k-v1',credits:600_000,usd:3},{id:'braincells-1m-v1',credits:1_000_000,usd:5}],
    recipient:'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE', payload:'05010000000474657374' };
  let phase = 'confirming';
  await page.route('**/api/easel-tezos', async route => {
    assert.equal(route.request().headers().authorization, `Bearer ${secret}`);
    const request = route.request().postDataJSON();
    if (request.action === 'quote') {
      assert.equal(request.address, sender); assert.equal(request.signature, 'fixture-signature'); assert.equal(request.pack, pack);
      intent = { ...intent, status:'quoted', sender, pack, credits, usd, amountMutez:amount, expiresAt:new Date(Date.now() + 900_000).toISOString() };
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
  await page.getByLabel('Amount', {exact:true}).selectOption(pack);
  await page.getByRole('heading', {name:`${credits.toLocaleString('en-US')} braincells`,exact:true}).waitFor();
  await page.reload();
  assert.equal(await page.getByLabel('Amount', {exact:true}).inputValue(),pack,'selected pack survives reload');
  await page.getByRole('button', { name:'Connect wallet', exact:true }).click();
  await page.getByRole('button', { name:`Pay ${tez} tez`, exact:true }).waitFor();
  assert.equal(new URL(page.url()).hash, '');
  assert.equal(await page.evaluate(() => window.operations.length), 0, 'connecting/signing never sends funds');
  await page.getByRole('button', { name:`Pay ${tez} tez`, exact:true }).click();
  await page.getByText('Payment seen. Waiting for three Tezos confirmations…', { exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:`Pay ${tez} tez`, exact:true }).isVisible(), false);
  const operations = await page.evaluate(() => window.operations);
  assert.equal(operations.length, 1);
  assert.deepEqual(operations[0].operationDetails, [{ kind:'transaction', destination:intent.recipient, amount }]);
  await page.reload();
  await page.getByText('Payment seen. Waiting for three Tezos confirmations…', { exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:`Pay ${tez} tez`, exact:true }).isVisible(), false, 'reload cannot offer a second payment');
  phase = 'credited';
  await page.getByRole('button', { name:'Check payment', exact:true }).click();
  await page.getByText('Braincells added. They’re ready to use in AC.', { exact:true }).waitFor();
  assert.equal(await page.getByRole('button', { name:'Check payment', exact:true }).isVisible(), false);
  assert.deepEqual(errors, []);
  console.log(`PASS: $${usd} pack, connect, exact quote, explicit payment, reload recovery, pending and credited states.`);
  await page.close();
 }
} finally { await browser.close(); server.close(); }
