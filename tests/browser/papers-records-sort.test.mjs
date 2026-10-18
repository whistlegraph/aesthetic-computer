import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

test('written dates, revisions, compact rows, and non-PDF records', async () => {
  const site = process.env.AC_PAPERS_SITE || new URL('../../system/public/papers.aesthetic.computer/', import.meta.url).pathname;
  const browser = await chromium.launch(process.env.AC_BROWSER_EXECUTABLE ? { executablePath: process.env.AC_BROWSER_EXECUTABLE } : { channel: 'chrome' });
  const context = await browser.newContext({ viewport: { width: 1100, height: 900 } });
  const page = await context.newPage();
  let pdfRequests = 0;
  await context.route('**/*', async route => {
    const url = new URL(route.request().url());
    if (url.hostname !== 'papers.aesthetic.computer') return route.abort();
    if (url.pathname.endsWith('.pdf')) pdfRequests++;
    const path = url.pathname === '/' ? '/index.html' : url.pathname;
    try {
      const types = { html: 'text/html', mjs: 'text/javascript', css: 'text/css', jpg: 'image/jpeg', otf: 'font/otf' };
      await route.fulfill({ body: await readFile(site + path), contentType: types[path.split('.').at(-1)] || 'application/octet-stream' });
    } catch { await route.fulfill({ status: 404, body: 'Missing' }); }
  });
  try {
    await page.goto('https://papers.aesthetic.computer/');
    assert.equal(await page.locator('input, select, thead').count(), 0);
    const order = await page.locator('#pubs tbody tr').evaluateAll(rows => rows.map(row => ({
      date: row.dataset.created || '', record: !!row.querySelector('.paper-title a[href^="/records/"]')
    })));
    const papers = order.filter(row => !row.record);
    assert.deepEqual(order.slice(0, papers.length), papers, 'submission-only records follow every paper');
    const dates = papers.map(row => row.date);
    assert.deepEqual(dates, [...dates].sort().reverse(), 'papers use original dates, newest first');
    const row = page.locator('tr[data-key="os"]');
    assert.equal(await row.locator('.paper-version').textContent(), 'v' + await row.getAttribute('data-revisions'));
    assert.equal(await row.locator('.created').evaluate(e => e.firstElementChild.matches('.paper-version')), true);
    assert.match(await row.locator('.created time').textContent(), /^[A-Z][a-z]+ \d{1,2}, \d{4}$/);
    const brief = page.getByRole('link', { name: 'Early-career research statement', exact: true });
    await brief.click();
    await page.locator('.record-preview article').getByText('Working brief · PDF not yet built').waitFor();
    assert.ok(await page.locator('.record-preview article').getByText('Abstract seed', { exact: true }).isVisible());
    assert.equal(pdfRequests, 0);
    const popupReady = page.waitForEvent('popup');
    await page.locator('.record-preview').getByRole('link', { name: 'Open in tab' }).click();
    const popup = await popupReady;
    await popup.waitForLoadState();
    assert.equal(new URL(popup.url()).pathname, '/records/iccc-early-career-2026.html');
    await popup.getByRole('heading', { name: 'Abstract seed' }).waitFor();
    await popup.close();
    await page.keyboard.press('Escape');
    assert.equal(await brief.evaluate(a => a === document.activeElement), true);
    await page.setViewportSize({ width: 390, height: 780 });
    await brief.click();
    await page.locator('.record-preview article').getByText('Abstract seed', { exact: true }).waitFor();
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
    await page.getByRole('button', { name: 'Close record' }).click();
    for (const title of ['Sound/image full paper', 'Note(s)pat(ial) Native']) {
      await page.getByRole('link', { name: title, exact: true }).click();
      await page.locator('.record-preview article').getByText('Submission record · No public document available').waitFor();
      await page.keyboard.press('Escape');
    }
  } finally { await browser.close(); }
});
