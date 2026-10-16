import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

test('date sorting, filtering, persistence, and non-PDF records', async () => {
  const site = process.env.AC_PAPERS_SITE || new URL('../../system/public/papers.aesthetic.computer/', import.meta.url).pathname;
  const browser = await chromium.launch({ executablePath: process.env.AC_BROWSER_EXECUTABLE });
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
    const sort = page.getByRole('combobox', { name: 'Sort papers' });
    assert.equal(await sort.inputValue(), 'newest');
    async function checkDates(key, ascending) {
      const dates = await page.locator('#pubs tbody tr').evaluateAll((rows, key) => rows.map(row => row.dataset[key]?.slice(0, 10) || ''), key);
      const known = dates.filter(Boolean);
      assert.deepEqual(known, [...known].sort((a,b) => ascending ? a.localeCompare(b) : b.localeCompare(a)));
      assert.deepEqual(dates.slice(0, known.length), known, 'undated records stay last');
    }
    await checkDates('created', false);
    await sort.selectOption('oldest');
    await checkDates('created', true);
    await sort.selectOption('updated');
    await checkDates('updated', false);
    await page.reload();
    assert.equal(await sort.inputValue(), 'updated');
    await sort.selectOption('newest');
    const filter = page.getByRole('searchbox');
    await filter.fill('Early-career');
    assert.equal(await page.locator('#pubs tbody tr:visible').count(), 1);
    const brief = page.getByRole('link', { name: 'Early-career research statement', exact: true });
    await brief.click();
    await page.locator('.record-preview article').getByText('Working brief · PDF not yet built').waitFor();
    assert.ok(await page.locator('.record-preview article').getByText('Abstract seed', { exact: true }).isVisible());
    assert.equal(pdfRequests, 0);
    await page.screenshot({ path: '/tmp/ac-os-paper-release-shots/record-desktop.png' });
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
    await page.screenshot({ path: '/tmp/ac-os-paper-release-shots/record-phone.png' });
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
    await page.getByRole('button', { name: 'Close record' }).click();
    await filter.fill('');
    for (const title of ['Sound/image full paper', 'Note(s)pat(ial) Native']) {
      await page.getByRole('link', { name: title, exact: true }).click();
      await page.locator('.record-preview article').getByText('Submission record · No public document available').waitFor();
      await page.keyboard.press('Escape');
    }
    await sort.selectOption('submission');
    const submissionDates = await page.locator('#pubs tbody tr .wh').allTextContents();
    const known = submissionDates.map(t => t.match(/\d{4}-\d{2}-\d{2}/)?.[0]).filter(Boolean);
    assert.deepEqual(known, [...known].sort().reverse());
  } finally { await browser.close(); }
});
