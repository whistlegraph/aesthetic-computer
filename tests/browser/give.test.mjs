// Local Give UI checks. Stripe and subscriber responses are mocked; no payments are created.
// Run: node tests/browser/give.test.mjs (uses installed Chrome).
import { chromium } from 'playwright';
import { readFile, mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createServer } from 'node:http';
import assert from 'node:assert/strict';
const root = fileURLToPath(new URL('../../system/public/give.aesthetic.computer', import.meta.url));
const shots = await mkdtemp(join(tmpdir(), 'ac-give-review-'));
console.log('Screenshots:', shots);
const server = createServer(async (req, res) => {
  try {
    const name = new URL(req.url, 'http://localhost').pathname;
    res.setHeader('Content-Type', name.endsWith('.mjs') ? 'text/javascript' : name.endsWith('.svg') ? 'image/svg+xml' : 'text/html');
    const alias = ['/', '/da', '/de', '/es', '/cn'].includes(name);
    res.end(await readFile(root + (alias ? '/index.html' : name)));
  } catch { res.writeHead(404).end(); }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const base = `http://127.0.0.1:${server.address().port}`;
let browser;
try {
  browser = await chromium.launch({headless:true, channel:process.env.AC_BROWSER_CHANNEL || 'chrome'});
  const page = await browser.newPage({viewport:{width:1200,height:1000}, colorScheme:'dark'});
  const errors = []; page.on('pageerror', error=>errors.push(error.message));
  let count = 6, countError = false;
  await page.route('**/api/gives?summary=subscribers', route => route.fulfill({status:countError?503:200, json:{activeSubscribers:count}}));
  let payloads=[];
  await page.route('**/api/give', route => { payloads.push(route.request().postDataJSON()); return route.fulfill({status:503,json:{error:'Unavailable'}}); });
  const requests=[]; page.on('request', req=>requests.push(req.url()));
  await page.goto(base);
  await page.waitForFunction(()=>document.querySelector('#count').textContent==='6');
  await page.screenshot({path:join(shots, 'desktop.png'),fullPage:true});
  assert.equal(await page.locator('#give').textContent(),'Give $8 / month');
  await page.locator('#give').click();
  await page.locator('#error').waitFor({state:'visible'});
  assert.deepEqual(payloads.at(-1),{amount:800,currency:'usd',recurring:true});
  await page.locator('input[value="once"]').check();
  await page.locator('#amount').fill('12.34');
  await page.locator('#give').click();
  await page.waitForFunction(()=>!document.querySelector('#give').disabled);
  assert.deepEqual(payloads.at(-1),{amount:1234,currency:'usd',recurring:false});
  await page.locator('#currency').selectOption('dkk');
  await page.locator('#give').click();
  await page.waitForFunction(()=>!document.querySelector('#give').disabled);
  assert.deepEqual(payloads.at(-1),{amount:5000,currency:'dkk',recurring:false});
  const calls = payloads.length;
  await page.locator('#amount').fill('0'); await page.locator('#give').click();
  assert.equal(payloads.length,calls);
  await page.setViewportSize({width:375,height:812});
  await page.goto(base); await page.waitForFunction(()=>document.querySelector('#count').textContent==='6');
  await page.screenshot({path:join(shots, 'mobile.png'),fullPage:true});
  for(const lang of ['en','da','de','es','zh']) {
    await page.locator('#language').selectOption(lang);
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true,lang+' overflow');
  }
  await page.setViewportSize({width:320,height:700});
  for (const currency of ['usd','dkk']) {
    await page.locator('#currency').selectOption(currency);
    for (const lang of ['en','da','de','es','zh']) {
      await page.locator('#language').selectOption(lang);
      assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true,`${lang}/${currency} at 320px`);
    }
  }
  await page.setViewportSize({width:375,height:812});
  await page.locator('#currency').selectOption('usd');
  await page.emulateMedia({colorScheme:'light'}); await page.locator('#language').selectOption('en');
  await page.screenshot({path:join(shots, 'light.png'),fullPage:true});
  count=0; await page.reload(); await page.waitForFunction(()=>document.querySelector('#count').textContent==='0');
  count=1; await page.reload(); await page.waitForFunction(()=>document.querySelector('#subscriber-label').textContent==='monthly subscriber');
  countError=true; await page.reload(); await page.waitForFunction(()=>document.querySelector('#subscriber-label').textContent==='Subscriber count unavailable');
  assert.equal(await page.locator('#count').textContent(),'—');
  assert.equal(await page.locator('#give').isEnabled(),true);
  await page.goto(base+'/thanks.html?amount=8&currency=usd');
  await page.locator('#thanks').waitFor({state:'visible'});
  assert.deepEqual(errors,[]);
  assert.equal(requests.some(url=>/auth0|fonts|tv\?|chat|coingecko/.test(url)),false);
  for (const [path, lang, currency] of [['da','da','dkk'], ['de','de','usd'], ['es','es','usd'], ['cn','zh','usd']]) {
    await page.goto(`${base}/${path}`);
    await page.waitForFunction((lang)=>document.documentElement.lang===lang, lang);
    assert.equal(await page.locator('#currency').inputValue(),currency);
  }
  await page.goto(base+'/?lang=toString&currency=eur');
  await page.waitForFunction(()=>document.querySelector('#subscriber-label').textContent==='Subscriber count unavailable');
  assert.equal(await page.locator('html').getAttribute('lang'),'en');
  assert.equal(await page.locator('#currency').inputValue(),'usd');
  await page.goto(base+'/?email=donor%40example.com&currency=dkk');
  await page.waitForFunction(()=>!location.search.includes('email'));
  await page.locator('#give').click();
  await page.waitForFunction(()=>!document.querySelector('#give').disabled);
  assert.deepEqual(payloads.at(-1),{amount:5000,currency:'dkk',recurring:true,email:'donor@example.com'});
  assert.deepEqual(errors,[]);
  console.log('PASS: language aliases, currency links, invalid language fallback, private email prefill.');
  // Auth loads only on demand; exercise both portal access and login redirect.
  await page.route('https://cdn.auth0.com/**', route=>route.fulfill({contentType:'text/javascript',body:`
    window.auth0 = { createAuth0Client: async () => ({
      isAuthenticated: async () => !new URLSearchParams(location.search).has('signedOut'),
      getTokenSilently: async () => 'test-token',
      handleRedirectCallback: async () => { window.callbackHandled = true; },
      loginWithRedirect: async () => { window.loginRequested = true; }
    }) };
  `}));
  let authorization;
  await page.route('**/api/give-portal', route=> {
    authorization = route.request().headers().authorization;
    return route.fulfill({json:{url:'https://billing.stripe.com/p/session/test-only'}});
  });
  await page.route('https://billing.stripe.com/**',route=>route.fulfill({body:'Portal stub'}));
  await page.goto(base);
  await page.locator('#manage').click(); await page.waitForURL('https://billing.stripe.com/**');
  assert.equal(authorization,'Bearer test-token');
  await page.goto(base+'/?code=test&state=test'); await page.waitForURL('https://billing.stripe.com/**');
  await page.goto(base+'/?signedOut=1'); await page.locator('#manage').click();
  await page.waitForFunction(()=>window.loginRequested===true);
  console.log('PASS: on-demand subscription portal, authenticated callback, and signed-out login redirect.');
  // Verify checkout redirect without creating a real Stripe session.
  await page.route('**/api/give', route=>route.fulfill({json:{url:'https://checkout.stripe.com/c/pay/test-only'}}));
  await page.route('https://checkout.stripe.com/**',route=>route.fulfill({body:'Checkout stub'}));
  await page.locator('#give').click(); await page.waitForURL('https://checkout.stripe.com/**');
  console.log('PASS: mobile + five languages, dark/light, USD/DKK, monthly/once, cents, invalid amount, errors, zero/singular counts, thanks redirect, checkout redirect; no eager external libraries.');
} finally { await browser?.close(); server.close(); }
