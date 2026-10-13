import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {chromium} from 'playwright';

test('native HTML controls stay hidden before the editor scripts load', async () => {
  const html = await readFile(new URL('../Resources/Web/index.html', import.meta.url), 'utf8');
  const browser = await chromium.launch({channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome', headless: true});
  try {
    for (const native of [true, false]) {
      const page = await browser.newPage();
      await page.addInitScript(value => {window.__whistlegraphNativeShell = value;}, native);
      await page.route('**/*', route => route.fulfill(route.request().resourceType() === 'document'
        ? {contentType: 'text/html', body: html} : {contentType: 'application/javascript', body: ''}));
      await page.goto('https://workspace.test/');
      const visible = await page.locator('header.top').isVisible();
      assert.equal(visible, !native, 'native previews suppress the fallback HTML header even when native.js has not run');
      assert.equal(await page.locator('#initial').isVisible(), !native);
      await page.close();
    }
  } finally {await browser.close();}
});
