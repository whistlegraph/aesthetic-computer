import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile, mkdir } from 'node:fs/promises';
import { createServer } from 'node:http';
import { chromium } from 'playwright';

// Serve repository assets only; PDF previews never need an external service.
test('paper feed fits pages, switches cards, zooms, and preserves list navigation', { timeout: 120000 }, async () => {
  const site = new URL('../../system/public/papers.aesthetic.computer/', import.meta.url);
  const types = { html: 'text/html', mjs: 'text/javascript', css: 'text/css', pdf: 'application/pdf', jpg: 'image/jpeg', png: 'image/png', otf: 'font/otf', woff: 'font/woff', wasm: 'application/wasm' };
  const server = createServer(async (request, response) => {
    const path = new URL(request.url, 'http://localhost').pathname;
    try {
      const file = new URL(path === '/' ? 'index.html' : '.' + path, site);
      if (!file.href.startsWith(site.href)) throw new Error('Outside site');
      response.setHeader('Content-Type', types[file.pathname.split('.').at(-1)] || 'application/octet-stream');
      response.end(await readFile(file));
    } catch { response.writeHead(404); response.end('Missing'); }
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const origin = `http://127.0.0.1:${server.address().port}`;
  const browser = await chromium.launch(process.env.AC_BROWSER_EXECUTABLE
    ? { executablePath: process.env.AC_BROWSER_EXECUTABLE, headless: true }
    : { channel: 'chrome', headless: true });
  try {
    const page = await browser.newPage({ viewport: { width: 1200, height: 900 }, hasTouch: true });
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.route('https://**/*', route => route.abort());
    await page.goto(origin);
    assert.equal(await page.locator('input, select').count(), 0);
    assert.equal(await page.locator('.created').evaluateAll(labels => labels.every(label => !label.querySelector('.paper-version') || label.firstElementChild.matches('.paper-version'))), true);
    const paper = page.locator('[data-key="kidlisp"] .paper-title a');
    const paperHref = await paper.getAttribute('href');
    const cardsHref = await page.locator('[data-key="kidlisp"] .fm a[href$="-cards.pdf"]').getAttribute('href');
    const loaded = () => page.locator('.preview-card[data-loaded="true"]').first().waitFor();
    const checkFit = async () => {
      const rect = await page.locator('.preview-card').first().boundingBox();
      const viewport = page.viewportSize();
      assert(rect.x >= 0 && rect.x + rect.width <= viewport.width + 1);
      assert(rect.y >= 60 && rect.y + rect.height <= viewport.height - 60);
    };
    await paper.click();
    await loaded();
    const paperPages = await page.locator('.preview-card').count();
    assert(paperPages > 1);
    await checkFit();
    assert.equal(await page.locator('.preview-card').first().evaluate(e => getComputedStyle(e).borderRadius), '12px');
    assert.equal(await page.getByRole('button', { name: 'Paper', exact: true }).getAttribute('aria-pressed'), 'true');
    const shots = process.env.AC_SHOT_DIR || '/tmp/papers-preview-check';
    await mkdir(shots, { recursive: true });
    await page.screenshot({ path: `${shots}/paper-fit.png` });
    await page.getByRole('button', { name: 'Cards', exact: true }).click();
    await loaded();
    await checkFit();
    assert.equal(new URL(await page.locator('.preview-download').getAttribute('href')).pathname, cardsHref);
    assert.equal(new URL(await page.locator('a.preview-open').getAttribute('href')).pathname, cardsHref);
    const downloadReady = page.waitForEvent('download');
    await page.locator('.preview-download').click();
    assert.equal((await downloadReady).suggestedFilename(), cardsHref.split('/').at(-1));
    await page.screenshot({ path: `${shots}/cards-fit.png` });
    await page.getByRole('button', { name: 'Paper', exact: true }).click();
    await loaded();
    assert.equal(await page.locator('.preview-card').count(), paperPages);
    assert.equal(new URL(await page.locator('a.preview-open').getAttribute('href')).pathname, paperHref);
    const before = await page.locator('.preview-card').first().boundingBox();
    await page.getByRole('button', { name: 'Zoom in', exact: true }).click();
    assert((await page.locator('.preview-card').first().boundingBox()).width > before.width);
    await page.getByRole('button', { name: 'Fit page to screen' }).click();
    await checkFit();
    await page.locator('.preview-card').last().scrollIntoViewIfNeeded();
    await page.waitForFunction(() => document.querySelector('.preview-card:last-child').dataset.loaded === 'true');
    await page.keyboard.press('Escape');
    assert.equal(await paper.evaluate(e => e === document.activeElement), true);
    for (const width of [320, 390]) {
      await page.setViewportSize({ width, height: 780 });
      await paper.click();
      await loaded();
      await checkFit();
      assert.equal(await page.evaluate(() => {
        const a = document.querySelector('.preview-actions').getBoundingClientRect();
        const z = document.querySelector('.preview-zoom').getBoundingClientRect();
        return z.right > a.left;
      }), false);
      await page.getByRole('button', { name: 'Cards', exact: true }).click();
      await loaded();
      await checkFit();
      await page.screenshot({ path: `${shots}/cards-${width}.png` });
      await page.touchscreen.tap(4, 300);
      assert.equal(await page.locator('.paper-preview').isVisible(), false);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false);
    }
    // A direct Cards link selects Cards immediately; papers without cards hide the switch.
    await page.locator('[data-key="kidlisp"] .paper-marquee').click();
    await page.locator('[data-key="kidlisp"] .fm a[href$="-cards.pdf"]').click();
    await loaded();
    assert.equal(await page.getByRole('button', { name: 'Cards', exact: true }).getAttribute('aria-pressed'), 'true');
    await page.keyboard.press('Escape');
    await page.locator('[data-key="joss-kidlisp"] .paper-title a').click();
    await loaded();
    assert.equal(await page.locator('.preview-formats').isVisible(), false);
    await page.keyboard.press('Escape');
    // Rapid switches must leave only the selected document in the feed.
    await paper.click();
    await page.getByRole('button', { name: 'Cards', exact: true }).click();
    await page.getByRole('button', { name: 'Paper', exact: true }).click();
    await loaded();
    assert.equal(await page.locator('.preview-card').count(), paperPages);
    await page.keyboard.press('Escape');
    await page.locator('[data-key="iccc-early-career-2026"] .paper-title a').click();
    await page.locator('.record-preview article').getByText('Abstract seed', { exact: true }).waitFor();
    assert.deepEqual(errors, []);
  } finally {
    await browser.close();
    server.closeAllConnections();
    await new Promise(resolve => server.close(resolve));
  }
});
