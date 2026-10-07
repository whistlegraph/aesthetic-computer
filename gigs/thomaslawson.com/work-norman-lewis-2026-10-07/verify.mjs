import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import puppeteer from 'puppeteer';

const [directory, mode] = process.argv.slice(2);
assert.ok(directory && ['preview', 'live'].includes(mode), 'Usage: node verify.mjs private-publish-directory preview|live');
const evidence = path.resolve(directory), origin = 'https://www.thomaslawson.com';
const route = '/art-in-context-norman-lewis/', indexRoute = '/art-in-a-broader-context/';
const base = await fs.readFile(path.join(evidence, 'art-in-a-broader-context-before.html'), 'utf8');
const content = await fs.readFile(path.join(evidence, 'content.html'), 'utf8');
const card = await fs.readFile(path.join(evidence, 'card.html'), 'utf8');
const css = await fs.readFile(new URL('./norman.css', import.meta.url), 'utf8');
const js = await fs.readFile(new URL('./index.js', import.meta.url), 'utf8');
const data = JSON.parse(await fs.readFile(path.join(evidence, '../entry-source.json'), 'utf8'));
const addCSS = html => html.replace('</head>', `<style>${css}</style></head>`);
const previewIndex = addCSS(base).replace('</main>', card + '</main>').replace('</body>', `<script>${js}</script></body>`);
const previewPage = addCSS(base)
  .replace(/<main\b[^>]*>[^]*?<\/main>/, () => content)
  .replace(/<body\b[^>]*>/, body => body.replace(/page-id-\d+/g, 'tl-norman-page'));
const browser = await puppeteer.launch({channel: 'chrome', headless: true});
const errors = [], results = [];
try {
  const page = await browser.newPage();
  page.on('pageerror', e => errors.push(e.message));
  let baselineMode = true;
  await page.setRequestInterception(true);
  page.on('request', async request => {
    const url = new URL(request.url());
    if (request.isNavigationRequest() && request.frame() === page.mainFrame()) {
      if (baselineMode) return request.respond({status: 200, contentType: 'text/html', body: base});
      if (mode === 'preview') return request.respond({status: 200, contentType: 'text/html', body: url.pathname === route ? previewPage : previewIndex});
    }
    if (mode === 'preview' && url.pathname.startsWith('/wp-content/uploads/tl-refresh/norman-lewis-1976/')) {
      const file = path.basename(url.pathname);
      const body = await fs.readFile(path.join(evidence, 'assets', file));
      return request.respond({status: 200, body, contentType: file.endsWith('.pdf') ? 'application/pdf' : file.endsWith('.jpg') ? 'image/jpeg' : 'image/png'});
    }
    return request.continue();
  });
  async function open(url) {
    const response = await page.goto(origin + url, {waitUntil: 'networkidle2'});
    assert.equal(response.status(), 200);
    await page.evaluate(() => document.fonts.ready);
  }
  async function indexLinks() { return page.$$eval('.tl-oct-project-index h2 a', links => links.map(a => new URL(a.href).pathname)); }
  await open(indexRoute);
  await page.waitForSelector('.tl-oct-project-index');
  const baseline = await indexLinks();
  assert.equal(baseline.length, 12);
  baselineMode = false;
  for (const [width, height] of [[1440, 1000], [390, 844], [320, 568]]) {
    await page.setViewport({width, height, isMobile: width < 600, hasTouch: width < 600});
    await open(route);
    assert.equal(await page.$eval('.tl-norman-intro', p => p.textContent), data.introduction);
    assert.equal(await page.$$eval('main h1', h => h.length), 1);
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1), false);
    await page.$eval('.tl-norman-install', el => el.scrollIntoView());
    await page.waitForFunction(() => [...document.querySelectorAll('.tl-norman img')].every(img => img.complete && img.naturalWidth > 0));
    const sizes = await page.$$eval('.tl-norman-views img', images => images.map(i => ({width: i.getBoundingClientRect().width, native: i.naturalWidth, alt: i.alt})));
    assert.equal(sizes.length, 3);
    sizes.forEach(i => { assert.ok(i.width <= i.native + 1); assert.ok(i.alt); });
    const pdf = await page.$eval('.tl-norman-catalogue a', a => a.href);
    assert.ok(pdf.endsWith('/norman-lewis-1976/catalogue.pdf'));
    await page.evaluate(() => scrollTo(0, 0));
    if (width !== 320) await page.screenshot({path: path.join(evidence, `${mode}-page-${width}.jpg`), type: 'jpeg', quality: 78, fullPage: true});
    await open(indexRoute);
    await page.waitForSelector('.tl-oct-project-index #tl-norman-index-entry');
    const links = await indexLinks();
    assert.deepEqual(links.filter(link => link !== route), baseline);
    assert.equal(links.filter(link => link === route).length, 1);
    const position = links.indexOf(route);
    assert.equal(links[position - 1], '/art-in-context-reallife-presents/');
    assert.equal(links[position + 1], '/pat-douthewaite-art-in-context/');
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1), false);
    await page.$eval('#tl-norman-index-entry', el => el.scrollIntoView({block: 'center'}));
    await page.waitForFunction(() => [...document.querySelectorAll('main img')].filter(img => {
      const r = img.getBoundingClientRect();
      return r.width > 0 && r.height > 0 && r.bottom > 0 && r.top < innerHeight;
    }).every(img => img.complete && img.naturalWidth > 0));
    if (width !== 320) await page.screenshot({path: path.join(evidence, `${mode}-index-${width}.jpg`), type: 'jpeg', quality: 78});
    await Promise.all([page.waitForNavigation({waitUntil: 'networkidle2'}), page.click('#tl-norman-index-entry h2 a')]);
    assert.equal(new URL(page.url()).pathname, route);
    assert.equal(await page.$eval('.tl-norman-intro', p => p.textContent), data.introduction);
    if (mode === 'live') {
      assert.equal(await page.title(), 'Norman Lewis: A Retrospective – Thomas Lawson');
      assert.equal(await page.$eval('link[rel="canonical"]', a => a.href), origin + route);
    }
    results.push({width, exactText: true, nativeImageSizing: true, indexOrder: true, indexLink: true, overflow: false});
  }
  assert.deepEqual(errors, []);
  await fs.writeFile(path.join(evidence, `${mode}-verification.json`), JSON.stringify({verifiedAt: new Date().toISOString(), results, errors}, null, 2) + '\n');
  console.log(JSON.stringify({results, errors}, null, 2));
} finally { await browser.close(); }
