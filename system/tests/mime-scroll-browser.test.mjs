import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';

// MIME_CDP_URL can point at a dedicated remote Chrome (e.g. Poorslice via SSH).
// All requests are fixtures; this test never posts analytics or changes live data.
test('MIME: center scrolling, explicit interaction, natural image sizing and deferred content', async () => {
  const browser = process.env.MIME_CDP_URL
    ? await chromium.connectOverCDP(process.env.MIME_CDP_URL)
    : await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL, headless: true });
  const html = await readFile(new URL('../public/mime/index.html', import.meta.url), 'utf8');
  const videoFixture = await readFile(new URL('./fixtures/mime-video.webm', import.meta.url));
  const requests = [];
  const context = await browser.newContext({ viewport: { width: 900, height: 900 }, hasTouch: true });
  const fixture = (code, type, media) => ({ code, when: new Date().toISOString(), name: '@test', replies: 0,
    file: { name: code, type, url: `https://mime.ac/fixture/${code}` }, ...(media ? { media } : {}) });
  const posts = [fixture('wide', 'image/svg+xml'), fixture('program', 'text/javascript', { kind: 'piece', code: 'test', url: 'https://aesthetic.computer/test' }),
    fixture('document', 'text/html'), fixture('longtext', 'text/plain'), fixture('below1', 'image/svg+xml'), fixture('below2', 'image/svg+xml'), fixture('deferred', 'text/plain'), fixture('video1', 'video/webm'), fixture('video2', 'video/webm')];
  await context.route('**/*', async route => {
    const u = new URL(route.request().url()); requests.push(u.pathname);
    if (u.hostname === 'mime.ac' && u.pathname === '/') return route.fulfill({ contentType: 'text/html', body: html });
    if (u.pathname === '/api/mime') return route.fulfill({ contentType: 'application/json', body: JSON.stringify({ recent: posts, hasMore: false }) });
    if (u.pathname === '/fixture/longtext' || u.pathname === '/fixture/deferred') return route.fulfill({ body: 'A long line of text\n'.repeat(200) });
    if (u.pathname === '/fixture/document' || u.pathname === '/test') return route.fulfill({ contentType: 'text/html', body: `<style>body{margin:0;background:#264;color:white;height:3000px}button{padding:30px}</style><button onclick="this.textContent='clicked'">Program button</button><script>addEventListener('wheel',e=>e.preventDefault(),{passive:false});addEventListener('touchmove',e=>e.preventDefault(),{passive:false});</script>` });
    if (/^\/fixture\/video[12]$/.test(u.pathname)) return route.fulfill({ contentType: 'video/webm', body: videoFixture });
    if (u.pathname.startsWith('/fixture/')) return route.fulfill({ contentType: 'image/svg+xml', body: '<svg xmlns="http://www.w3.org/2000/svg" width="800" height="300"><rect width="800" height="300" fill="#f9a"/><circle cx="400" cy="150" r="100" fill="#384"/></svg>' });
    return route.fulfill({ status: 200, body: '' });
  });
  const page = await context.newPage();
  await page.addInitScript(() => { window.mimeActions = []; window.acVisits = { action(name) { window.mimeActions.push(name); } }; });
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  try {
    await page.goto('https://mime.ac/?ac-automation=1');
    await page.locator('#wide img').waitFor();
    await page.waitForFunction(() => document.querySelector('#wide img')?.naturalWidth === 800);
    assert.ok(!requests.includes('/fixture/deferred'), 'offscreen text does not load eagerly');
    const imageBox = await page.locator('#wide .feed-media').boundingBox();
    assert.ok(Math.abs(imageBox.width / (imageBox.height - 2) - 800 / 300) < .05, 'image uses natural proportions');
    for (const id of ['program', 'document', 'longtext']) {
      const frame = page.locator(`#${id} .feed-media`);
      await frame.evaluate(el => el.scrollIntoView({ block: 'center' }));
      await page.waitForTimeout(450);
      const box = await frame.boundingBox();
      const y = Math.min(800, box.y + box.height / 2);
      await page.mouse.move(box.x + box.width / 2, y);
      const before = await page.evaluate(() => scrollY);
      await page.mouse.wheel(0, 180);
      await page.waitForFunction(previous => scrollY > previous + 60, before);
    }
    const program = page.locator('#program .feed-media');
    await program.evaluate(el => el.scrollIntoView({ block: 'center' }));
    await page.waitForTimeout(500);
    await page.locator('#program .media-toggle').click();
    assert.equal(await program.getAttribute('data-interacting'), 'true');
    await page.locator('#program iframe').waitFor();
    await page.frameLocator('#program iframe').getByRole('button', { name: 'Program button', exact: true }).click();
    await page.frameLocator('#program iframe').getByRole('button', { name: 'clicked', exact: true }).waitFor();
    await page.locator('#program .media-toggle').click();
    assert.equal(await program.getAttribute('data-interacting'), null);
    assert.deepEqual(await page.evaluate(() => window.mimeActions), ['mime_interact', 'mime_scroll_feed']);
    assert.equal(await page.locator('#program .piece-preview').evaluate(el => el.inert), true);
    await page.locator('#program .source-button').evaluate(el => el.addEventListener('click', event => event.preventDefault()));
    await page.locator('#program .source-button').click();
    assert.equal(await page.evaluate(() => window.mimeActions.at(-1)), 'mime_original_open');
    await page.locator('#document .media-toggle').click();
    await page.locator('#longtext').evaluate(el => el.scrollIntoView({ block: 'start' }));
    await page.waitForTimeout(1100);
    assert.equal(await page.locator('#document .feed-media').getAttribute('data-interacting'), null, 'scrolling away re-locks media');
    await page.locator('#deferred').scrollIntoViewIfNeeded();
    await page.waitForFunction(() => document.querySelector('#deferred pre')?.textContent.startsWith('A long'));
    for (const id of ['video1', 'video2']) {
      await page.locator(`#${id} .feed-media`).evaluate(el => el.scrollIntoView({ block: 'center' }));
      await page.waitForFunction(id => document.querySelector(`#${id} video`)?.readyState >= 2, id);
      await page.waitForFunction(id => !document.querySelector(`#${id} video`)?.paused, id);
      assert.equal(await page.locator(`#${id} .feed-media`).getAttribute('data-media-state'), 'ready');
      assert.equal(await page.locator('video').evaluateAll(videos => videos.filter(v => !v.paused).length), 1, 'one dominant tape plays');
      const box = await page.locator(`#${id} .feed-media`).boundingBox();
      assert.ok(Math.abs(box.width / (box.height - 2) - 16 / 9) < .05);
      await page.locator(`#${id} .media-toggle`).click();
      await page.locator(`#${id} video`).evaluate(video => video.pause());
      await page.waitForTimeout(1100);
      assert.equal(await page.locator(`#${id} video`).evaluate(video => video.paused), true, 'manual pause survives the autoplay scheduler');
      await page.locator(`#${id} .media-toggle`).click();
    }
    await page.setViewportSize({ width: 390, height: 844 });
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    await program.evaluate(el => el.scrollIntoView({ block: 'center' }));
    await page.waitForTimeout(500);
    const box = await program.boundingBox(); const x = box.x + box.width / 2, y = box.y + box.height / 2;
    const before = await page.evaluate(() => scrollY);
    const cdp = await context.newCDPSession(page);
    await cdp.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x, y }] });
    for (let i = 1; i <= 6; i++) {
      await cdp.send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: [{ x, y: y - i * 25 }] });
      await page.waitForTimeout(30);
    }
    await cdp.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await page.waitForFunction(previous => scrollY > previous + 50, before);
    await page.evaluate(() => scrollTo(0, 0));
    if (process.env.MIME_SCREENSHOT_DIR) {
      await mkdir(process.env.MIME_SCREENSHOT_DIR, { recursive: true });
      await page.screenshot({ path: `${process.env.MIME_SCREENSHOT_DIR}/mime-mobile.png` });
      await page.setViewportSize({ width: 900, height: 900 });
      await page.screenshot({ path: `${process.env.MIME_SCREENSHOT_DIR}/mime-desktop.png` });
    }
    assert.deepEqual(errors, []);
  } finally { await context.close(); await browser.close(); }
});
