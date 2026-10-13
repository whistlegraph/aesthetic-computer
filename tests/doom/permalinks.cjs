const assert = require('node:assert/strict');
const { readFile } = require('node:fs/promises');
const puppeteer = require('puppeteer');
(async () => {
 const browser = await puppeteer.launch({ headless: true, executablePath: process.env.CHROME_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' });
 try {
  const page = await browser.newPage(); await page.setViewport({ width: 700, height: 700 });
  const html = await readFile('system/public/mime/index.html', 'utf8');
  const mood = (id, code) => ({ code: 'mood_' + id.repeat(24), parent: null, board: 'text/plain', name: '@tester', text: 'A mood ' + code, when: new Date().toISOString(), media: { kind: 'mood', code } });
  const posts = [mood('a', 'abc'), mood('b', 'def')], absent = mood('c', 'xyz');
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.setRequestInterception(true);
  page.on('request', async r => {
   const u = new URL(r.url());
   if (u.hostname === 'mime.ac' && (u.pathname === '/' || u.pathname.startsWith('/p/'))) return r.respond({ contentType: 'text/html', body: html });
   if (u.pathname === '/api/mime') {
    const code = u.searchParams.get('thread');
    const op = [...posts, absent].find(p => p.code === code || 'mood~' + p.media.code === code);
    return r.respond({ status: code && !op ? 404 : 200, contentType: 'application/json', body: JSON.stringify(code ? op ? { op, replies: [] } : { error: 'thread not found' } : { recent: posts, hasMore: false }) });
   }
   return r.respond({ body: '' });
  });
  await page.goto('https://mime.ac/?ac-automation=1');
  await page.waitForFunction(() => location.pathname === '/p/mood~abc');
  assert.equal(await page.$eval('.reel-command .media-label', el => el.getAttribute('aria-label')), 'mood~abc');
  await page.evaluate(() => { window.originalCard = document.querySelector('.feed-post'); document.querySelectorAll('.feed-post')[1].scrollIntoView(); });
  await page.waitForFunction(() => location.pathname === '/p/mood~def');
  assert.equal(await page.evaluate(() => window.originalCard === document.querySelector('.feed-post')), true, 'swipes do not recreate cards');
  await page.reload();
  await page.waitForFunction(() => document.querySelectorAll('.feed-post')[1]?.getBoundingClientRect().top === 0);
  assert.equal(new URL(page.url()).pathname, '/p/mood~def');
  await page.evaluate(code => { location.hash = '#/t/' + code; }, posts[1].code);
  await page.waitForSelector('#post');
  await page.goBack();
  await page.waitForFunction(() => document.querySelectorAll('.feed-post')[1]?.getBoundingClientRect().top === 0);
  await page.goto('https://mime.ac/p/mood~xyz?ac-automation=1');
  await page.waitForFunction(() => document.querySelector('.feed-post')?.dataset.permalink === 'mood~xyz');
  assert.equal(await page.$$eval('.feed-post', nodes => nodes.length), 3, 'permalink restores posts absent from current feed');
  await page.goto('https://mime.ac/p/mood~missing?ac-automation=1');
  await page.waitForFunction(() => document.body.textContent.includes('thread not found'));
  assert.deepEqual(errors, []);
  console.log('PASS stable mood labels, swipe URLs, reload, comment/back, absent and missing post links');
 } finally { await browser.close(); }
})().catch(e => { console.error(e); process.exitCode = 1; });
