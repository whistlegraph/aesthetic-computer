// Signed-in defaults and mobile account controls through the real shell.
// Auth0, saved characters and deletion responses are isolated fixtures.
// Run: node xbox/live/tests/account-default-browser.mjs
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import puppeteer from 'puppeteer';
const root = fileURLToPath(new URL('../../../', import.meta.url)).replace(/\/$/, '');
const output = process.env.OSKIEWAR_TEST_OUTPUT || '/tmp/oskiewar-account-default';
await mkdir(output, { recursive: true });
const browser = await puppeteer.launch({
  executablePath: process.env.PUPPETEER_EXECUTABLE_PATH ||
    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true, args: ['--mute-audio', '--no-first-run'],
});
const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
async function open(path, { phone = false, failed = false, roomStatus = null, assetFailure = false, savedFighter = false } = {}) {
  const context = await browser.createBrowserContext();
  const page = await context.newPage();
  const errors = [], discoveries = [], deletionRequests = [];
  let deletionFails = false;
  await page.setViewport(phone ? { width: 390, height: 844, deviceScaleFactor: 3, isMobile: true, hasTouch: true }
    : { width: 1200, height: 700, deviceScaleFactor: 2 });
  page.on('pageerror', e => errors.push(e.message));
  await page.evaluateOnNewDocument(({ failed, roomStatus, savedFighter }) => {
    speechSynthesis.speak = () => {};
    let signed = savedFighter;
    window.__authCalls = [];
    window.auth0 = { createAuth0Client: async () => ({
      checkSession: async () => { await new Promise(r => setTimeout(r, 900)); },
      handleRedirectCallback: async () => {
        window.__authCalls.push('callback');
        await new Promise(r => setTimeout(r, 900));
        if (failed) throw new Error('invalid state');
        signed = true;
        return { appState: { returnTo: '/' } };
      },
      getUser: async () => signed ? { sub: 'test-user' } : undefined,
      getTokenSilently: async () => 'test-token',
    }) };
    window.__sockets = [];
    window.WebSocket = class {
      static OPEN = 1; static CONNECTING = 0; static CLOSED = 3;
      constructor(url) { this.readyState = 0; window.__sockets.push(url); }
      send() {} close() {} removeEventListener() {}
      addEventListener(type, listener) {
        if (type === 'message' && roomStatus !== null) setTimeout(() => listener({
          data: JSON.stringify({type: 'oskiewar:status', content: {live: roomStatus}})
        }), 100);
      }
    };
  }, { failed, roomStatus, savedFighter });
  await page.setRequestInterception(true);
  page.on('request', async req => {
    try {
      const u = new URL(req.url());
      if (assetFailure && u.pathname.endsWith('/props.png'))
        return req.respond({status:404,body:''});
      const json = body => req.respond({ status: 200, contentType: 'application/json',
        headers: { 'access-control-allow-origin': '*' }, body: JSON.stringify(body) });
      if (u.pathname === '/oskiewar-open-room') {
        discoveries.push(u.pathname);
        return json({ room: 'daffo394' });
      }
      if (u.pathname === '/api/oskiewar-generation') return json(savedFighter ? {
        status:'accepted', handle:'@tester', validUntil:Date.now()+600000,
        fighter:{version:1,recipe:'oskiewar-capsule-fighter-v1',hash:'a'.repeat(64),
          appearance:{skin:'#f1d9c9',hair:'#4a3c31',shirt:'#f0f0f0',pants:'#1c4ea6',shoes:'#000000',hairStyle:'long',beard:false,glasses:false,sleeves:'long'}},
      } : {status:'empty',handle:'@tester'});
      if (req.method() === 'OPTIONS') return req.respond({status:204,headers:{'access-control-allow-origin':'*','access-control-allow-headers':'authorization,content-type','access-control-allow-methods':'GET,POST'}});
      if (u.pathname === '/api/delete-erase-and-forget-me') {
        deletionRequests.push(req.method());
        if (deletionFails) return req.respond({status:503,contentType:'application/json',headers:{'access-control-allow-origin':'*'},body:JSON.stringify({message:'Service unavailable'})});
        if (req.method() === 'POST') return json({result:'Deleted!',purgeAfter:'2026-11-01',mailed:true});
        return json({graceDays:7,braincells:12,counts:{}});
      }
      if (u.pathname === '/handle') return json({ handle: 'tester' });
      if (u.pathname === '/api/handle-colors') return json({ colors: [] });
      if (u.pathname.includes('/api/') || u.hostname === 'session-server.aesthetic.computer')
        return json({});
      if (u.hostname !== 'oskiewar.com') return req.abort();
      let file;
      if (u.pathname === '/' || /^\/[a-z]+[0-9]+$/.test(u.pathname)) file = root + '/xbox/live/mac-test.html';
      else if (u.pathname.startsWith('/aesthetic.computer/')) file = root + '/system/public' + u.pathname;
      else if (u.pathname.startsWith('/ComicRelief')) file = root + '/system/public/papers.aesthetic.computer/foundry/fonts' + u.pathname;
      else file = root + '/xbox/live' + u.pathname;
      const body = await readFile(file);
      const contentType = file.endsWith('.html') ? 'text/html' : /\.m?js$/.test(file)
        ? 'application/javascript' : file.endsWith('.svg') ? 'image/svg+xml' : 'application/octet-stream';
      await req.respond({ status: 200, contentType, body });
    } catch { await req.respond({ status: 404, body: '' }); }
  });
  await page.goto('https://oskiewar.com' + path, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => window.__oskiewarAccount?.ready && window.__oskiewarTouch &&
    window.__oskiewarGraphicsThemeStatus,
    { timeout: 20000 });
  await pause(1100); // Exercise the 500 ms room URL updater after the first paint.
  const pixels = await page.evaluate(() => {
    const canvas = document.querySelector('canvas');
    const bounds = canvas.getBoundingClientRect();
    const density = navigator.maxTouchPoints > 0 ? Math.min(2, devicePixelRatio) : devicePixelRatio;
    return { actual: [canvas.width, canvas.height],
      expected: [Math.round(bounds.width * density),
        Math.round(bounds.height * density)] };
  });
  assert.deepEqual(pixels.actual, pixels.expected, 'canvas maps to exact display pixels');
  assert.equal(await page.evaluate(() => __oskiewarGraphicsThemeStatus),
    path.includes('renderer=canvas') && !assetFailure && !path.includes('graphics=flat') ? 'photorealistic' : 'flat');
  assert.deepEqual(errors, [], 'no browser exceptions');
  assert.deepEqual(discoveries, [], 'startup never discovers an unsolicited match');
  if (await page.evaluate(() => __oskiewarTouch.screen === 'title')) {
    await page.screenshot({path:`${output}/title-${phone?'ios':'web'}.png`});
    const button = await page.evaluate(() => {
      const b = __oskiewarTouch.titleButton, view = __fightHost.gameView();
      return {x:(b.x+b.width/2)/view.width*innerWidth,
        y:(b.y+b.height/2)/view.height*innerHeight};
    });
    await page.mouse.click(button.x, button.y);
    await page.waitForFunction(() => __oskiewarTouch.screen === 'game');
  }
  return { page, context, errors, deletionRequests, failDeletion: () => { deletionFails = true; } };
}
async function state(page) {
  return page.evaluate(() => ({ path: location.pathname + location.search,
    screen: __oskiewarTouch.screen, handle: __oskiewarAccount.handle,
    signedIn: __oskiewarAccount.signedIn, open: __oskiewarAccountOpen,
    callbacks: __authCalls.length, room: __oskiewarRoundBridge?.name || '' }));
}
try {
  for (const [path, phone] of [['/',false], ['/?touch&app=ios',true], ['/?code=fixture&state=fixture&app=ios',true]]) {
    const {page,context,errors,deletionRequests,failDeletion}=await open(path,{phone,savedFighter:true});
    await page.waitForFunction(()=>globalThis.__oskiewarFighterAppearance?.handle==='@tester');
    assert.equal(await page.evaluate(()=>__oskiewarLocalPractice),true);
    assert.equal(await page.evaluate(()=>__oskiewarFighterAppearance.appearance.shirt.join(',')),'240,240,240');
    assert.equal((await state(page)).signedIn,true);
    assert.equal(await page.$eval('#account-delete',e=>e.getClientRects().length),0,'deletion is absent from the game screen');
    assert.equal(await page.evaluate(()=>location.search.includes('practice')),false,'default keeps the current front door');
    await page.click('#game-menu');
    await page.waitForFunction(()=>__oskiewarTouch.menu==='level');
    await page.click('#account-handle');
    await page.waitForFunction(()=>document.querySelector('#account-settings').open);
    assert.equal(await page.$eval('#account-delete',e=>e.getClientRects().length>0),true);
    assert.equal(await page.evaluate(()=>__oskiewarAccountOpen),true);
    assert.deepEqual(deletionRequests,[],'opening account actions does not request deletion');
    await page.screenshot({path:`${output}/account-${phone?'ios':'web'}.png`});
    await page.keyboard.press('Escape');
    await page.waitForFunction(()=>!__oskiewarAccountOpen);
    if(phone) assert.equal(await page.evaluate(()=>sessionStorage.getItem('oskiewar-app')),'ios');
    await page.click('#game-menu');
    await page.waitForFunction(()=>__oskiewarTouch.menu===null);
    await pause(250); // Let the menu button's synthetic press release before another key.
    await page.waitForFunction(()=>__oskiewarTouch.screen==='game' && __oskiewarTouch.practiceFighter==='@tester' && __oskiewarTouch.practiceModel?.parts>0);
    assert.equal(await page.evaluate(()=>__oskiewarTouch.level),'painting');
    assert.deepEqual(await page.evaluate(()=>__sockets),[],'saved likeness stays local');
    await page.screenshot({path:`${output}/saved-${phone?'ios':'web'}.png`});
    async function press(key) { await page.keyboard.down(key); await pause(150); await page.keyboard.up(key); await pause(150); }
    await press('Escape');
    await page.waitForFunction(()=>__oskiewarTouch.menu==='level');
    await page.screenshot({path:`${output}/menu-${phone?'ios':'web'}.png`});
    await press('KeyD'); await press('KeyD'); await press('Enter');
    await page.waitForFunction(()=>__oskiewarTouch.level==='pool' && __oskiewarTouch.menu===null && __oskiewarTouch.practiceModel?.parts>0);
    assert.equal(await page.evaluate(()=>__oskiewarTouch.practiceFighter),'@tester','level switch keeps the saved fighter');
    await page.reload({waitUntil:'domcontentloaded'});
    await page.waitForFunction(()=>globalThis.__oskiewarFighterAppearance?.handle==='@tester');
    assert.equal(await page.evaluate(()=>__oskiewarLocalPractice),true);
    if(phone) {
      if (await page.evaluate(() => __oskiewarTouch.screen === 'title')) {
        await page.keyboard.press('Enter');
        await page.waitForFunction(() => __oskiewarTouch.screen === 'game');
      }
      await page.click('#game-menu');
      await page.waitForFunction(()=>__oskiewarTouch.menu==='level');
      await page.click('#account-handle');
      await page.click('#account-delete');
      await page.waitForFunction(()=>document.querySelector('#account-deletion p').textContent.includes('7 days'));
      assert.deepEqual(deletionRequests,['GET']);
      assert.equal(await page.$eval('#account-deletion [type="submit"]',e=>e.disabled),true);
      await page.screenshot({path:`${output}/delete-ios.png`});
      await page.click('#account-deletion [type="button"]');
      assert.deepEqual(deletionRequests,['GET'],'cancel does not delete');
      if(path.includes('code=')) {
        failDeletion(); await page.click('#account-delete');
        await page.waitForFunction(()=>document.querySelector('#account-deletion p').textContent.includes('Service unavailable'));
        await page.type('#account-deletion input','DELETE');
        assert.equal(await page.$eval('#account-deletion [type="submit"]',e=>e.disabled),true,'failed preview cannot submit');
      } else {
        await page.click('#account-delete');
        await page.waitForFunction(()=>document.querySelector('#account-deletion p').textContent.includes('7 days'));
        await page.type('#account-deletion input','DELETE');
        await page.click('#account-deletion [type="submit"]');
        await page.waitForFunction(()=>document.querySelector('#account-deletion p').textContent.includes('account is locked'));
        assert.deepEqual(deletionRequests,['GET','GET','POST']);
        assert.equal((await state(page)).signedIn,false);
        assert.equal(await page.evaluate(()=>globalThis.__oskiewarFighterAppearance),null);
      }
    }
    console.log(`PASS saved fighter: ${path}, reload, ${phone?'mobile account controls':'desktop'}`);
    assert.deepEqual(errors, [], "no browser errors through account operations");
    await context.close();
  }
} finally { await browser.close(); }
