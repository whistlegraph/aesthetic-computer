import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

const pub = new URL('../public/', import.meta.url);
async function fixture({ login = false, failRead = false } = {}) {
  const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL, headless: true });
  const page = await browser.newPage({ viewport: { width: 390, height: 844 }, colorScheme: 'dark' });
  const errors = [], writes = [], reads = [];
  page.on('pageerror', error => errors.push(error.message));
  const html = await readFile(new URL('mime/index.html', pub));
  const font = await readFile(new URL('whistlegraph.app/ComicRelief-Bold.ttf', pub));
  const video = await readFile(new URL('fixtures/mime-video.webm', import.meta.url));
  const now = new Date().toISOString();
  const posts = [
    { code: 'first', name: '@artist', text: '', when: now, replies: 1,
      file: { type: 'video/webm', name: 'motion.webm', url: '/fixture.webm' } },
    { code: 'second', name: '@reader', text: 'Space for a thought.', when: now, replies: 0,
      board: 'text/plain', media: { kind: 'mood', code: 'abc' } },
  ];
  const replies = { first: [{ code: 'reply1', parent: 'first', name: '@reader', when: now, text: 'I like how this moves.' }], second: [] };
  if (login) await page.addInitScript(() => {
    window.auth0 = { Auth0Client: class {
      async loginWithRedirect({ appState }) {
        sessionStorage.setItem('fixture-app-state', JSON.stringify(appState));
        location.href = '/?code=fixture&state=fixture&ac-automation=1';
      }
      async handleRedirectCallback() {
        localStorage.setItem('session-aesthetic', encodeURIComponent(btoa(JSON.stringify({ accessToken: 'fixture', account: { sub: 'fixture' } }))));
        return { appState: JSON.parse(sessionStorage.getItem('fixture-app-state')) };
      }
    } };
  });
  await page.route('**/*', async route => {
    const url = new URL(route.request().url());
    if (url.hostname === 'mime.ac' && (url.pathname === '/' || url.pathname.startsWith('/p/')))
      return route.fulfill({ contentType: 'text/html', body: html });
    if (url.pathname === '/api/mime') {
      if (url.searchParams.has('engagement')) return route.fulfill({ json: { ok: true } });
      if (url.searchParams.has('me')) return route.fulfill({ json: { handle: '@fixture' } });
      const code = url.searchParams.get('thread');
      if (code) {
        reads.push(code);
        if (failRead) { failRead = false; return route.fulfill({ status: 503, json: { error: 'offline' } }); }
        return route.fulfill({ json: { op: { ...posts.find(p => p.code === code || p.media?.code === code.split('~')[1]), replies: replies[code]?.length || 0 }, replies: replies[code] || [] } });
      }
      if (route.request().method() === 'POST') {
        const body = route.request().postDataJSON();
        writes.push({ body, auth: route.request().headers().authorization });
        if (!login && writes.length === 1) return route.fulfill({ status: 503, json: { error: 'Could not post — try again.' } });
        const reply = { code: 'reply' + (writes.length + 1), parent: body.parent, name: login ? '@fixture' : body.name, text: body.text, when: now };
        replies[body.parent].push(reply);
        return route.fulfill({ json: { code: reply.code, parent: body.parent, board: 'text/plain' } });
      }
      return route.fulfill({ json: { recent: posts, hasMore: false } });
    }
    if (url.pathname === '/fixture.webm') return route.fulfill({ contentType: 'video/webm', body: video });
    if (url.pathname.endsWith('ComicRelief-Bold.ttf')) return route.fulfill({ contentType: 'font/ttf', body: font });
    return route.fulfill({ body: '' });
  });
  await page.goto('https://mime.ac/?ac-automation=1');
  await page.locator('.mime-startup').waitFor({ state: 'detached' });
  return { browser, page, writes, reads, errors };
}

test('three-dot comments overlay pauses the card, posts in place and preserves drafts and navigation', async () => {
  const { browser, page, writes, errors } = await fixture();
  try {
    const bubble = page.locator('#first .reel-comments');
    assert.equal(await bubble.locator('[data-comment-count]').textContent(), '1');
    assert.equal(await bubble.locator('.comment-dots').isVisible(), false);
    assert.deepEqual(await bubble.evaluate(b => ({background: getComputedStyle(b).backgroundColor, shadow: getComputedStyle(b).boxShadow, filter: getComputedStyle(b.querySelector('svg')).filter})), {background:'rgba(0, 0, 0, 0)',shadow:'none',filter:'none'});
    const size = await bubble.boundingBox(); assert.ok(size.width >= 44 && size.height >= 44);
    await page.waitForFunction(() => !document.querySelector('#first video').paused);
    const address = page.url(), position = await page.evaluate(() => scrollY);
    await page.screenshot({ path: '/tmp/mime-comments-bubble.png' });
    await bubble.click();
    const dialog = page.getByRole('dialog', { name: 'Comments · 1', exact: true });
    await dialog.getByText('I like how this moves.').waitFor();
    assert.equal(await dialog.getByRole('button', { name: 'Retry', exact: true }).isVisible(), false);
    assert.equal(page.url(), address);
    assert.equal(await page.locator('#first video').evaluate(v => v.paused), true);
    await page.mouse.wheel(0, 300); assert.equal(await page.evaluate(() => scrollY), position);
    await dialog.getByRole('textbox', { name: 'Name (optional)' }).fill('Visitor');
    await dialog.getByRole('textbox', { name: 'Comment', exact: true }).fill('A little more motion.');
    await dialog.getByRole('button', { name: 'Post', exact: true }).click();
    await dialog.getByRole('status').filter({ hasText: 'Could not post' }).waitFor();
    assert.equal(await dialog.getByRole('textbox', { name: 'Comment', exact: true }).inputValue(), 'A little more motion.');
    await page.screenshot({ path: '/tmp/mime-comments-phone.png' });
    await dialog.getByRole('button', { name: 'Post', exact: true }).click();
    const updated = page.getByRole('dialog', { name: 'Comments · 2', exact: true });
    await updated.getByText('A little more motion.', { exact: true }).waitFor();
    assert.equal(await updated.getByRole('textbox', { name: 'Comment', exact: true }).inputValue(), '');
    assert.equal(await bubble.getAttribute('aria-label'), 'Comments · 2');
    assert.equal(await bubble.locator('[data-comment-count]').textContent(), '2');
    await page.keyboard.press('Escape');
    await page.locator('#comments-dialog').waitFor({ state: 'detached' });
    await page.waitForFunction(() => !history.state?.mimeComments);
    assert.equal(await bubble.evaluate(b => b === document.activeElement), true);
    assert.equal(await page.evaluate(() => scrollY), position);
    await page.waitForFunction(() => !document.querySelector('#first video').paused);
    await page.locator('#second').evaluate(e => e.scrollIntoView());
    await page.waitForFunction(() => Math.abs(document.querySelector('#second').getBoundingClientRect().top) < 1);
    assert.equal(await page.locator('#second .comment-dots').isVisible(), true);
    assert.equal(await page.locator('#second .comment-dots circle').first().evaluate(c=>getComputedStyle(c).animationName),'comment-bounce');
    await page.emulateMedia({reducedMotion:'reduce'});
    assert.equal(await page.locator('#second .comment-dots circle').first().evaluate(c=>getComputedStyle(c).animationName),'none');
    await page.locator('#second .reel-comments').click();
    const empty = page.getByRole('dialog', { name: 'Comments', exact: true });
    await empty.getByText('No comments yet.').waitFor();
    await empty.getByRole('textbox', { name: 'Comment', exact: true }).fill('Keep this thought.');
    await page.goBack();
    await page.locator('#comments-dialog').waitFor({ state: 'detached' });
    await page.locator('#second .reel-comments').click();
    await empty.getByRole('textbox', { name: 'Comment', exact: true }).waitFor();
    assert.equal(await empty.getByRole('textbox', { name: 'Comment', exact: true }).inputValue(), 'Keep this thought.');
    await page.setViewportSize({ width: 1280, height: 900 });
    const bounds = await empty.boundingBox(); assert.ok(bounds.width <= 600 && bounds.y >= 0 && bounds.y + bounds.height <= 900);
    await page.screenshot({ path: '/tmp/mime-comments-desktop.png' });
    await empty.getByRole('button', { name: 'Close comments' }).click();
    await page.locator('#comments-dialog').waitFor({ state: 'detached' });
    assert.equal(writes.length, 2);
    assert.deepEqual(writes.map(w => w.body), Array(2).fill({ name: 'Visitor', text: 'A little more motion.', parent: 'first' }));
    assert.deepEqual(errors, []);
  } finally { await browser.close(); }
});

test('comment loading retries and a draft returns to its post after sign-in', async () => {
  const { browser, page, writes, errors } = await fixture({ login: true, failRead: true });
  try {
    await page.locator('#first .reel-comments').click();
    let dialog = page.locator('#comments-dialog');
    await dialog.getByText('Could not load comments.').waitFor();
    await dialog.getByRole('button', { name: 'Retry', exact: true }).click();
    await dialog.getByRole('textbox', { name: 'Comment', exact: true }).fill('With my handle.');
    await dialog.getByRole('button', { name: 'Sign in', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#comments-dialog [name="commenter"]')?.readOnly);
    assert.equal(await dialog.getByRole('textbox', { name: 'Commenting as', exact: true }).inputValue(), '@fixture');
    assert.equal(await dialog.getByRole('button', { name: 'Sign in', exact: true }).isVisible(), false);
    assert.equal(await dialog.getByRole('textbox', { name: 'Comment', exact: true }).inputValue(), 'With my handle.');
    await dialog.getByRole('button', { name: 'Post', exact: true }).click();
    await dialog.getByText('With my handle.', { exact: true }).waitFor();
    assert.deepEqual(writes, [{ body: { name: '@fixture', text: 'With my handle.', parent: 'first' }, auth: 'Bearer fixture' }]);
    assert.deepEqual(errors, []);
  } finally { await browser.close(); }
});
