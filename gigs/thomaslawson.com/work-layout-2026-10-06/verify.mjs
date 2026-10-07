import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import puppeteer from 'puppeteer';

const evidence = path.resolve(process.argv.find(arg => arg.startsWith('--evidence='))?.slice(11) || '');
if (!process.argv.some(arg => arg.startsWith('--evidence='))) throw new Error('Pass --evidence=<private directory>');
const mode = process.argv.includes('--live') ? 'live' : 'preview';
const smoke = process.argv.includes('--smoke');
const root = new URL('./', import.meta.url);
const js = await fs.readFile(new URL('layout.js', root), 'utf8');
const css = await fs.readFile(new URL('layout.css', root), 'utf8');
const periodURLs = JSON.parse(await fs.readFile(path.join(evidence, 'period-urls.json')));
const legacy = periodURLs.filter(url => !/202[02]-/.test(url));
const portrait = 'https://www.thomaslawson.com/beyond-the-studio-portraits-of-new-york/';
const urls = smoke ? [legacy[0], portrait] : [...legacy, portrait];
const viewports = smoke ? [[1440, 1000], [390, 844]] : [[1440, 1000], [768, 1024], [390, 844], [320, 568]];
const browser = await puppeteer.launch({executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: true});
const results = [], errors = [];
const normalize = text => (text || '').replace(/\s+/g, ' ').trim();
try {
  const page = await browser.newPage();
  page.on('pageerror', error => errors.push(error.message));
  async function screenshot(filename) {
    // Scrolling activates native lazy loading; don't record empty placeholders.
    await page.waitForFunction(() => [...document.querySelectorAll('main img')]
      .filter(image => {
        const rect = image.getBoundingClientRect();
        return rect.height > 0 && rect.bottom > 0 && rect.top < innerHeight;
      }).every(image => image.complete && image.naturalWidth > 0), {timeout: 15000});
    await page.screenshot({path: path.join(evidence, filename)});
  }
  if (mode === 'preview') {
    const pages = new Map();
    for (const url of urls) {
      const slug = new URL(url).pathname.split('/')[1];
      let html = await fs.readFile(path.join(evidence, `${slug}-before.html`), 'utf8');
      html = html.replace('</head>', `<style id="tl-layout-css">${css}</style></head>`)
        .replace('</body>', `<script id="tl-layout-js">${js}</script></body>`);
      pages.set(new URL(url).pathname, html);
    }
    await page.setRequestInterception(true);
    page.on('request', request => {
      const html = request.isNavigationRequest() && request.frame() === page.mainFrame()
        && pages.get(new URL(request.url()).pathname);
      if (html) request.respond({status: 200, contentType: 'text/html', body: html});
      else request.continue();
    });
  }
  for (const [width, height] of viewports) {
    await page.setViewport({width, height, isMobile: width < 600, hasTouch: width < 600});
    for (const url of urls) {
      const slug = new URL(url).pathname.split('/')[1];
      await page.goto(url, {waitUntil: 'networkidle2'});
      await page.evaluate(() => document.fonts.ready);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1), false, `${slug} ${width}: overflow`);
      if (url !== portrait) {
        const baseline = JSON.parse(await fs.readFile(path.join(evidence, `${slug}-baseline.json`)));
        const result = await page.evaluate(() => {
          const grid = document.querySelector('.tl-archive-grid');
          return {columns: getComputedStyle(grid).gridTemplateColumns.split(' ').length,
            works: [...grid.querySelectorAll('.tl-archive-work')].map(work => ({
              widget: work.querySelector('.elementor-widget-image').dataset.id,
              image: work.querySelector('img').getAttribute('src'),
              caption: work.querySelector('.tl-cap')?.innerText,
              width: work.getBoundingClientRect().width,
              imageWidth: work.querySelector('img').getBoundingClientRect().width,
              fit: getComputedStyle(work.querySelector('img')).objectFit
            }))};
        });
        assert.equal(result.works.length, baseline.images, `${slug}: artwork count`);
        assert.deepEqual(result.works.map(work => [work.widget, work.image, normalize(work.caption)]),
          baseline.works.map(work => [work.widget, work.image, normalize(work.caption)]), `${slug}: original order/images/captions`);
        assert.equal(result.columns, width >= 1000 ? 3 : width >= 700 ? 2 : 1, `${slug}: column count`);
        result.works.forEach(work => {
          assert.ok(work.imageWidth <= work.width + 1, `${slug}: image fits card`);
          assert.equal(work.fit, 'contain');
        });
        if (['inthestudio_2017-2020', 'inthestudio_1977-1979', 'elementor-428'].includes(slug) && [1440,390].includes(width)) {
          await screenshot(`${mode}-${slug}-${width}.png`);
        }
        // Exercise a moved native image control: keyboard open, Escape, focus return.
        if (slug === 'inthestudio_2017-2020') {
          const button = await page.$('.tl-archive-work .tl-quality-image-button');
          assert.ok(button, 'Image control retained');
          await button.focus(); await page.keyboard.press('Enter');
          await page.waitForSelector('.tl-quality-viewer[open]');
          await page.keyboard.press('Escape');
          await page.waitForSelector('.tl-quality-viewer', {hidden: true});
          assert.equal(await page.evaluate(() => document.activeElement.classList.contains('tl-quality-image-button')), true);
          const last = baseline.works.at(-1);
          const filename = new URL(last.image).pathname.split('/').pop().replace(/(?:-\d+x\d+|-scaled)(?=\.[^.]+$)/i, '');
          await page.evaluate(hash => { location.hash = hash; }, `tl-find=Artwork&tl-image=${encodeURIComponent(filename)}`);
          await page.waitForSelector('.tl-archive-work.tl-search-target');
          assert.equal(await page.$eval('.tl-archive-work.tl-search-target .elementor-widget-image', node => node.dataset.id), last.widget);
        }
        results.push({slug, width, ...result});
      } else {
        await page.$eval('.tl-portrait-play', element => element.scrollIntoView());
        await page.waitForFunction(() => {
          const image = document.querySelector('.tl-portrait-play img');
          return image.complete && image.naturalWidth > 0;
        });
        const result = await page.evaluate(() => {
          const project = document.querySelector('.tl-portrait-project');
          const title = project.querySelector('#tl-portrait-title');
          const context = project.querySelector(':scope > .tl-follow-context');
          const film = project.querySelector('#tl-portrait-film');
          const poster = film.querySelector('img');
          return {playerIds: document.querySelectorAll('#tl-portrait-film').length,
            title: title.textContent, titleY: title.getBoundingClientRect().top,
            imageY: project.querySelector('img').getBoundingClientRect().top,
            contextY: context.getBoundingClientRect().top,
            contextCount: [...document.querySelectorAll('.tl-follow-context')].filter(p => p.textContent.includes('Manhattan Municipal Building')).length,
            poster: poster.src, posterLoaded: poster.complete && poster.naturalWidth > 0,
            filmAfterIntro: !!film.previousElementSibling?.classList.contains('tl-oct-project-header'),
            photoRows: project.querySelectorAll(':scope > .elementor-top-section:not(.tl-oct-project-header)').length};
        });
        assert.equal(result.playerIds, 1); assert.equal(result.contextCount, 1);
        assert.ok(result.titleY < result.contextY && result.contextY < result.imageY);
        assert.ok(result.poster.includes('/portrait_9')); assert.equal(result.posterLoaded, true);
        assert.equal(result.filmAfterIntro, true); assert.equal(result.photoRows, 2);
        for (const [name, selector] of [['project', '.tl-portrait-project'], ['film', 'figure#tl-portrait-film']]) {
          await page.$eval(selector, element => element.scrollIntoView());
          if ([1440,390].includes(width)) await screenshot(`${mode}-portrait-${name}-${width}.png`);
        }
        await page.$eval('.tl-portrait-play', element => element.focus());
        await page.keyboard.press('Enter');
        await page.waitForSelector('#tl-portrait-film iframe');
        assert.ok((await page.$eval('#tl-portrait-film iframe', frame => frame.src)).includes('/1216482818'));
        await page.evaluate(() => scrollTo(0, document.body.scrollHeight));
        if ([1440,390].includes(width)) await screenshot(`${mode}-portrait-footer-${width}.png`);
        results.push({slug, width, ...result});
      }
      console.log(`PASS ${mode} ${width}px ${slug}`);
    }
  }
  assert.deepEqual(errors, [], 'No JavaScript errors');
  await fs.writeFile(path.join(evidence, `${mode}${smoke ? '-smoke' : ''}-results.json`), JSON.stringify({results, errors}, null, 2));
} finally { await browser.close(); }
