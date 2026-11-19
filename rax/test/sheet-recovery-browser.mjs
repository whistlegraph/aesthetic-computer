import assert from 'node:assert/strict';
import { once } from 'node:events';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';
const app = createRaxServer({ guests: true, prefix: '/rax' });
await app.ready; app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
const browser = await chromium.launch({ channel: 'chrome', headless: true });
try {
  const page = await browser.newPage({ viewport: { width: 465, height: 390 } });
  const errors = [];
  page.on('pageerror', e => errors.push(e.message));
  const sheet = await readFile(new URL('../../system/public/aesthetic.computer/lib/rax-sheet.mjs', import.meta.url), 'utf8');
  await page.route('**/sheet.mjs', route => route.fulfill({ contentType: 'text/javascript', body: sheet }));
  await page.route('**/host', route => route.fulfill({ contentType: 'text/html', body: `<!doctype html>
    <div id="aesthetic-computer">prompt</div><script type="module">
    import {raxSheet} from '/sheet.mjs';
    window.sheet=raxSheet;window.pauses=0;window.resumes=0;window.paints=0;
    window.acPAUSE=()=>{window.__acLoopPaused=true;window.pauses++};
    window.acRESUME=()=>{window.__acLoopPaused=false;window.resumes++};
    window.acMeterPaint=()=>window.paints++;
    addEventListener('popstate', e=>window.acRaxSheet.popstate(e));
    window.guestMessages=[];
    addEventListener('message', e=>{if(e.data?.type==='rax:sheet:hello')window.guestMessages.push(e.data)});
    raxSheet({action:'warm'});
    </script>` }));
  await page.goto(base + '/host');
  await page.waitForFunction(() => window.guestMessages?.length === 1);
  await page.evaluate(() => sheet({ action: 'show' }));
  await page.waitForFunction(() => document.querySelector('iframe').style.zIndex === '1000');
  const frame = page.frames().find(f => f.url().includes('embed=prompt'));
  const connected = () => frame.waitForFunction(() => document.querySelector('canvas')?.raxStats?.connection?.samples >= 1);
  await connected();
  const before = await page.evaluate(() => paints);
  for (const ws of app.rooms.get('lobby').clients.keys()) ws.terminate();
  await page.waitForFunction(() => guestMessages.length === 2, {}, { timeout: 15000 });
  await connected();
  await frame.waitForFunction(() => document.querySelector('canvas')?.raxStats?.bare === false);
  assert.equal(await frame.evaluate(() => document.querySelector('canvas').raxStats.bare), false, 'reloaded guest restores its HUD');
  assert.ok(await page.evaluate(n => paints > n + 10, before), 'native meter follows the new document');
  assert.equal(await page.evaluate(() => document.querySelector('iframe').style.zIndex), '1000');
  await page.evaluate(() => sheet({ action: 'hide' }));
  await page.waitForFunction(() => !window.__acLoopPaused && document.getElementById('aesthetic-computer').style.opacity === '');
  assert.equal(await page.evaluate(() => resumes), 1, 'prompt resumes once');
  // Re-enter after returning home; previous meter callbacks must not multiply.
  await page.evaluate(() => sheet({ action: 'show' }));
  await page.waitForFunction(() => document.querySelector('iframe').style.zIndex === '1000');
  await connected();
  assert.equal(await page.evaluate(() => pauses), 2);
  assert.deepEqual(errors, []);
  console.log('Sheet recovery passed: relay disconnect, guest reload, HUD, native meter, prompt resume and re-entry.');
} finally { await browser.close(); await app.close(); }
