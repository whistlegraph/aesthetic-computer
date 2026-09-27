// Real shell and renderer; isolated Auth0/relay responses, no live accounts or matches.
// Run: node xbox/live/tests/startup-browser.mjs
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import puppeteer from 'puppeteer';
const root = fileURLToPath(new URL('../../../', import.meta.url)).replace(/\/$/, '');
const output = process.env.OSKIEWAR_TEST_OUTPUT || '/tmp/oskiewar-startup';
await mkdir(output, { recursive: true });
const browser = await puppeteer.launch({
  executablePath: process.env.PUPPETEER_EXECUTABLE_PATH ||
    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true, args: ['--mute-audio', '--no-first-run'],
});
const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
async function open(path, { phone = false, failed = false, roomStatus = null, assetFailure = false } = {}) {
  const context = await browser.createBrowserContext();
  const page = await context.newPage();
  const errors = [], discoveries = [];
  await page.setViewport(phone ? { width: 390, height: 844, deviceScaleFactor: 3, isMobile: true, hasTouch: true }
    : { width: 1200, height: 700, deviceScaleFactor: 2 });
  page.on('pageerror', e => errors.push(e.message));
  await page.evaluateOnNewDocument(({ failed, roomStatus }) => {
    speechSynthesis.speak = () => {};
    let signed = false;
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
    window.WebSocket = class {
      static OPEN = 1; static CONNECTING = 0; static CLOSED = 3;
      constructor() { this.readyState = 0; }
      send() {} close() {} removeEventListener() {}
      addEventListener(type, listener) {
        if (type === 'message' && roomStatus !== null) setTimeout(() => listener({
          data: JSON.stringify({type: 'oskiewar:status', content: {live: roomStatus}})
        }), 100);
      }
    };
  }, { failed, roomStatus });
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
    return { actual: [canvas.width, canvas.height],
      expected: [Math.round(bounds.width * devicePixelRatio),
        Math.round(bounds.height * devicePixelRatio)] };
  });
  assert.deepEqual(pixels.actual, pixels.expected, 'canvas maps to exact display pixels');
  assert.equal(await page.evaluate(() => __oskiewarGraphicsThemeStatus),
    assetFailure || path.includes('graphics=flat') ? 'flat' : 'photorealistic');
  assert.deepEqual(errors, [], 'no browser exceptions');
  assert.deepEqual(discoveries, [], 'startup never discovers an unsolicited match');
  return { page, context };
}
async function state(page) {
  return page.evaluate(() => ({ path: location.pathname + location.search,
    screen: __oskiewarTouch.screen, handle: __oskiewarAccount.handle,
    signedIn: __oskiewarAccount.signedIn, open: __oskiewarAccountOpen,
    callbacks: __authCalls.length, room: __oskiewarRoundBridge?.name || '' }));
}
try {
  for (const phone of [false, true]) {
    const { page, context } = await open('/', { phone });
    assert.deepEqual(await state(page), { path: '/', screen: 'title', handle: '',
      signedIn: false, open: false, callbacks: 0, room: '' });
    await page.screenshot({ path: `${output}/title-${phone ? 'phone' : 'desktop'}.png` });
    const button = await page.evaluate(() => {
      const b = __oskiewarTouch.titleButton;
      const view = __fightHost.gameView();
      return { x: (b.x + b.width / 2) / view.width * innerWidth,
        y: (b.y + b.height / 2) / view.height * innerHeight };
    });
    assert.ok(Number.isFinite(button.x) && Number.isFinite(button.y));
    if (phone) await page.touchscreen.tap(button.x, button.y);
    else await page.mouse.click(button.x, button.y);
    await page.waitForFunction(() => __oskiewarTouch.screen !== 'title');
    console.log(`PASS ${phone ? 'phone' : 'desktop'}: root stays on title; Start enters play`);
    await context.close();
  }
  for (const [path, options] of [['/?graphics=flat', {}], ['/', {assetFailure:true}]]) {
    const {page,context}=await open(path, options);
    assert.equal((await state(page)).screen, 'title');
    console.log(`PASS ${options.assetFailure ? 'missing texture fallback' : 'explicit flat theme'}`);
    await context.close();
  }
  for (const failed of [false, true]) {
    const { page, context } = await open('/?code=fixture&state=fixture', { failed });
    const got = await state(page);
    assert.equal(got.path, '/');
    assert.equal(got.screen, 'title');
    assert.equal(got.callbacks, 1);
    assert.equal(got.signedIn, !failed);
    assert.equal(got.open, failed);
    assert.equal(got.handle, failed ? '' : '@TESTER');
    await page.screenshot({ path: `${output}/signin-${failed ? 'retry' : 'success'}.png` });
    console.log(`PASS sign-in ${failed ? 'retry' : 'success'} returns to title`);
    await context.close();
  }
  for (const live of [false, true]) {
    const { page, context } = await open('/simma356', { roomStatus: live });
    const got = await state(page);
    assert.equal(got.signedIn, false);
    assert.equal(got.path, live ? '/simma356' : '/');
    assert.equal(got.room, live ? 'simma356' : '');
    if (live) assert.notEqual(got.screen, 'title');
    else assert.equal(got.screen, 'title');
    await page.screenshot({ path: `${output}/room-${live ? 'live' : 'empty'}.png` });
    console.log(`PASS anonymous ${live ? 'live room stays open' : 'empty room returns to title'}`);
    await context.close();
  }
  const { page, context } = await open('/daffo394');
  assert.equal((await state(page)).room, 'daffo394');
  assert.notEqual((await state(page)).screen, 'title');
  console.log('PASS explicit room links still enter their room');
  await context.close();
} finally { await browser.close(); }
