import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';

test('packed Whistlegraphs retain attribution, aspect, isolated interaction and exact HTML downloads', async () => {
  const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL, headless: true });
  const context = await browser.newContext({ viewport: { width: 390, height: 844 }, acceptDownloads: true });
  const page = await context.newPage();
  const html = await readFile(new URL('../public/mime/index.html', import.meta.url), 'utf8');
  const artifact = `<!doctype html><style>body{margin:0;background:#234;color:white;display:grid;place-items:center;height:100vh}button{padding:24px}</style><button onclick="this.textContent='Spinning'">Spin tree</button><script>try { parent.document.body.dataset.escaped='yes'; } catch {} try { localStorage.setItem('escape','yes'); } catch {} document.body.dataset.running='yes';</script>`;
  const post = { code: 'pack', parent: null, name: '@artist', when: '2026-10-05T22:00:00Z', replies: 0,
    origin: { name: 'Whistlegraph', url: 'https://whistlegraph.app/' },
    file: { name: 'wgDefen-v7.html', type: 'text/html', url: '/artifact.html' },
    media: { kind: 'whistlegraph', code: 'wgDefen', title: 'Spinning tree', version: 7, aspect: '2:3', packed: true,
      url: 'https://ipfs.aesthetic.computer/ipfs/fixture', tokenUrl: 'https://teia.art/objkt/885469' } };
  await context.route('**/*', route => {
    const u = new URL(route.request().url());
    if (u.hostname === 'mime.ac' && u.pathname === '/') return route.fulfill({ contentType: 'text/html', body: html });
    if (u.pathname === '/api/mime') return route.fulfill({ contentType: 'application/json', body: JSON.stringify({ recent: [post, { ...post, code: 'pack2' }], hasMore: false }) });
    if (u.pathname === '/ipfs/fixture' || u.pathname === '/artifact.html') return route.fulfill({ contentType: 'text/html', body: artifact });
    return route.fulfill({ status: 200, body: '' });
  });
  try {
    await page.goto('https://mime.ac/?ac-automation=1');
    await page.locator('#pack iframe[data-ready]').waitFor();
    assert.equal(await page.locator('#pack .name').textContent(), '@artist');
    assert.equal(await page.locator('#pack .media-label').textContent(), 'Spinning tree');
    assert.equal(await page.locator('#pack .media-origin').textContent(), 'Whistlegraph');
    assert.equal(await page.locator('#pack .mime-type').textContent(), 'text/html');
    assert.equal(await page.locator('#pack time').getAttribute('datetime'), post.when);
    assert.equal(await page.locator('#pack iframe').getAttribute('sandbox'), 'allow-scripts');
    assert.equal(await page.locator('#pack .token-link').getAttribute('href'), post.media.tokenUrl);
    assert.equal(await page.locator('#pack .piece-preview').evaluate(el => el.inert), true);
    const framed = page.frames().find(frame => frame.url() === post.media.url);
    assert.equal(await framed.evaluate(() => document.body.dataset.running), 'yes', 'pack scripts run');
    assert.equal(await page.evaluate(() => document.body.dataset.escaped || localStorage.getItem('escape')), null);
    for (const viewport of [{ width: 390, height: 844 }, { width: 900, height: 900 }]) {
      await page.setViewportSize(viewport);
      const box = await page.locator('#pack .media-pack').boundingBox();
      assert.ok(Math.abs(box.width / box.height - 2 / 3) < .01, JSON.stringify(box));
      assert.ok(box.height <= viewport.height * .56, 'preview leaves room for feed metadata');
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
    }
    await page.locator('#pack .media-toggle').click();
    await page.frameLocator('#pack iframe').getByRole('button', { name: 'Spin tree' }).click();
    await page.frameLocator('#pack iframe').getByRole('button', { name: 'Spinning', exact: true }).waitFor();
    await page.locator('#pack .media-toggle').click();
    const downloaded = page.waitForEvent('download');
    await page.locator('#pack .download-pack').click();
    const download = await downloaded;
    assert.equal(download.suggestedFilename(), 'wgDefen-v7.html');
    assert.equal(await readFile(await download.path(), 'utf8'), artifact);
    await page.locator('#pack2 .media-pack').evaluate(el => el.scrollIntoView({ block: 'center' }));
    await page.locator('#pack2 iframe[data-ready]').waitFor();
    assert.equal(await page.locator('.piece-preview iframe').count(), 1, 'only the visible pack runs');
    if (process.env.MIME_SCREENSHOT_DIR) {
      await mkdir(process.env.MIME_SCREENSHOT_DIR, { recursive: true });
      await page.setViewportSize({ width: 390, height: 844 });
      await page.evaluate(() => scrollTo(0, 0));
      await page.screenshot({ path: `${process.env.MIME_SCREENSHOT_DIR}/mime-pack-mobile.png` });
    }
  } finally { await context.close(); await browser.close(); }
});
