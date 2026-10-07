import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import puppeteer from 'puppeteer';

const [directory, mode] = process.argv.slice(2);
assert.ok(directory && ['--preview', '--live'].includes(mode), 'Usage: node verify.mjs private-evidence-directory --preview|--live');
const evidence = path.resolve(directory);
const intro = (await fs.readFile(new URL('./intro.txt', import.meta.url), 'utf8')).trim();
const before = await fs.readFile(path.join(evidence, 'before.html'), 'utf8');
const oldSource = await fs.readFile(path.join(evidence, 'live-before.php'), 'utf8');
const oldIntro = JSON.parse(oldSource.match(/json_decode\(<<<'TLFOLLOWDATA'\n([^]*?)\nTLFOLLOWDATA/)[1]).intros['/elementor-1878/'];
const phpString = JSON.stringify(oldIntro).replace(/[\u007f-\uffff]/g, c => '\\u' + c.charCodeAt(0).toString(16).padStart(4, '0'));
assert.equal(before.split(phpString).length, 2, 'Expected one current introduction in live HTML');
const preview = before.replace(phpString, () => JSON.stringify(intro));
const browser = await puppeteer.launch({headless: true, channel: 'chrome'});
const errors = [], results = [];
try {
  const page = await browser.newPage();
  page.on('pageerror', error => errors.push(error.message));
  let responseHTML = before;
  await page.setRequestInterception(true);
  page.on('request', request => {
    if (responseHTML && request.isNavigationRequest() && request.frame() === page.mainFrame())
      request.respond({status: 200, contentType: 'text/html', body: responseHTML});
    else request.continue();
  });
  const url = 'https://www.thomaslawson.com/elementor-1878/';
  async function navigate() {
    const response = await page.goto(url, {waitUntil: 'networkidle2'});
    assert.equal(response.status(), 200);
    await page.waitForSelector('.tl-follow-school-header p');
    await page.evaluate(() => document.fonts.ready);
  }
  async function documents() {
    return page.evaluate(() => ({
      links: [...document.querySelectorAll('main a')].map(a => ({text: a.textContent.trim(), href: a.href})),
      images: [...document.querySelectorAll('main img')].map(img => ({src: img.getAttribute('src'), alt: img.alt})),
      cards: document.querySelectorAll('.tl-follow-school article').length
    }));
  }
  await navigate();
  const baseline = await documents();
  assert.ok(baseline.cards > 0, 'Expected document cards');
  responseHTML = mode === '--preview' ? preview : null;
  for (const [width, height] of [[1440, 1000], [390, 844], [320, 568]]) {
    await page.setViewport({width, height, isMobile: width < 600, hasTouch: width < 600});
    await navigate();
    const state = await page.evaluate(() => ({
      intro: document.querySelector('.tl-follow-school-header p').textContent.trim(),
      body: document.body.innerText,
      overflow: document.documentElement.scrollWidth > innerWidth + 1,
      headings: [...document.querySelectorAll('main h1')].map(h => h.textContent.trim())
    }));
    assert.equal(state.intro, intro, 'Exact supplied paragraph');
    assert.equal(state.body.split(intro).length, 2, 'Introduction appears once');
    assert.ok(!state.body.includes(oldIntro), 'Generic introduction removed');
    assert.equal(state.overflow, false, 'No horizontal overflow');
    assert.deepEqual(await documents(), baseline, 'Document links and images preserved');
    await page.waitForFunction(() => [...document.querySelectorAll('.tl-follow-school-header img')].every(img => img.complete && img.naturalWidth > 0));
    await page.screenshot({path: path.join(evidence, `${mode.slice(2)}-${width}.png`), fullPage: true});
    results.push({width, exactIntro: true, introCount: 1, overflow: false, cards: baseline.cards, links: baseline.links.length, images: baseline.images.length, headings: state.headings});
  }
  assert.deepEqual(errors, [], 'No JavaScript errors');
  await fs.writeFile(path.join(evidence, `${mode.slice(2)}-verification.json`), JSON.stringify({verifiedAt: new Date().toISOString(), results, errors}, null, 2) + '\n');
  console.log(JSON.stringify({results, errors}, null, 2));
} finally {
  await browser.close();
}
