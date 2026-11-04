import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';
const here = new URL('./', import.meta.url);
const css = await readFile(new URL('login.css', here), 'utf8');
const js = await readFile(new URL('fit.js', here), 'utf8');
const template = await readFile(new URL('template.html', here), 'utf8');
assert.ok(template.includes(css));
assert.ok(template.includes(js));
// Structural fixture, not a replica of Turnstile: the real challenge is an
// opaque iframe whose container may grow after validation or interaction.
const widget = `<main><section class="_prompt-box-outer"><header id="screen-header"><h1>Aesthetic.Computer</h1></header>
<form><label>Email <input name="email" type="email"></label><button data-action-button-primary>Continue</button></form>
<p>Create account</p><p>OR</p><button data-provider="google">Continue with Google</button>
<button data-provider="apple">Continue with Apple</button><div id="challenge"></div></section></main>`;
const html = template.replace('{%- auth0:head -%}', `<style>
body{margin:0}main{display:flex;justify-content:center}.\_prompt-box-outer{box-sizing:border-box;padding:40px;background:white}
form,label{display:block}input,button{box-sizing:border-box;width:100%;margin:10px 0}button{height:56px}p{text-align:center}
</style>`).replace('{%- auth0:widget -%}', widget);
const browser = await chromium.launch({ channel: 'chrome', headless: true });
try {
  const page = await browser.newPage();
  await page.route('**/*', (r) => r.abort());
  await page.setContent(html);
  await page.locator('input').fill('test@example.invalid');
  for (const [width, height] of [[900,700], [495,400], [300,250], [220,160], [390,844]]) {
    await page.setViewportSize({ width, height });
    await page.waitForFunction(() => {
      const r = document.querySelector('._prompt-box-outer').getBoundingClientRect();
      return Math.abs(document.querySelector('#ac-login-fit').getBoundingClientRect().height - innerHeight) < 1 &&
        r.x >= 11 && r.y >= 11 && r.right <= innerWidth - 11 && r.bottom <= innerHeight - 11;
    });
    assert.equal(await page.locator('input').inputValue(), 'test@example.invalid');
    assert.ok(await page.evaluate(() => document.documentElement.scrollHeight <= innerHeight));
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
  }
  await page.setViewportSize({ width: 495, height: 400 });
  await page.evaluate(() => {
    const e = document.createElement('p'); e.textContent = 'Please check your email address.';
    document.querySelector('form').append(e);
    const frame = document.createElement('iframe'); frame.title = 'Challenge fixture'; frame.width = 300; frame.height = 300;
    document.querySelector('#challenge').append(frame);
  });
  await page.waitForFunction(() => {
    const r = document.querySelector('iframe').getBoundingClientRect();
    return r.bottom <= innerHeight - 11 && r.right <= innerWidth - 11;
  });
  assert.equal(await page.locator('input').inputValue(), 'test@example.invalid');
  await page.locator('button[data-action-button-primary]').focus();
  assert.equal(await page.evaluate(() => document.activeElement.hasAttribute('data-action-button-primary')), true);
  // User zoom takes precedence over fitting, restoring the scrollable layout.
  await page.evaluate(() => { Object.defineProperty(window, 'devicePixelRatio', { value: 2 }); window.dispatchEvent(new Event('resize')); });
  await page.waitForFunction(() => !document.documentElement.classList.contains('ac-fit-ready'));
  console.log('Login fitting passed: five window sizes, retained input, focus, growing challenge, and zoom fallback.');
} finally { await browser.close(); }
