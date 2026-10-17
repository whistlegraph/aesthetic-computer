// Local piece overrides and synthetic MIME replies; never joins or posts to the room.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { createRequire } from 'node:module';
const { build } = createRequire(new URL('../../system/package.json', import.meta.url))('esbuild');
import { fileURLToPath } from 'node:url';
import { ACSession } from './ac-harness.mjs';
const pub = new URL('../../system/public/', import.meta.url);
const id = 'a'.repeat(24);
const messages = [{ id, from: '@reader', text: 'Hello from the clock room.', when: new Date().toISOString() },
  { id: 'b'.repeat(24), from: '@reader', text: 'No comments here.', when: new Date().toISOString() }];
const vectorHTML = await readFile(new URL('html/index.html', pub), 'utf8');
let chatPiece = await readFile(new URL('aesthetic.computer/disks/chat.mjs', pub), 'utf8');
chatPiece = chatPiece.replace('commentsModalOpen = false; return;', 'new BroadcastChannel("ac-comments-closed").postMessage(true); commentsModalOpen = false; return;');
const module = await readFile(new URL('aesthetic.computer/lib/chat-comments.mjs', pub), 'utf8');
let piece = await readFile(new URL('aesthetic.computer/disks/laklok.mjs', pub), 'utf8');
piece = piece.replace('function paint($) {', 'function paintOriginal($) {');
piece += `\nconst commentTest = new BroadcastChannel('ac-comments-test');
function paint($) { paintOriginal($); commentTest.postMessage({ width: $.screen.width, height: $.screen.height, messages: client.system.messages.map(m => ({ id:m.id, x:m.layout?.x, box:m.layout?.comments })) }); }\n`;
let chat = await readFile(new URL('aesthetic.computer/lib/chat.mjs', pub), 'utf8');
chat = chat.replace('connect(instanceName) {', `connect(instanceName) { this.system.messages.push(...${JSON.stringify(messages)}); this.system.connecting = false; return;`);
const bundled = await build({ entryPoints: [fileURLToPath(new URL('aesthetic.computer/disks/laklok.mjs', pub))], bundle: true, format: 'esm', platform: 'browser', write: false,
  plugins: [{ name: 'comment-fixture', setup(b) {
    b.onResolve({filter: /.*/}, args => args.kind === 'dynamic-import' || ['url', 'module', 'https'].includes(args.path) ? { path: args.path, external: true } : null);
    for (const [filter, contents] of [[/disks\/laklok\.mjs$/, piece], [/disks\/chat\.mjs$/, chatPiece], [/lib\/chat\.mjs$/, chat]])
      b.onLoad({ filter }, args => ({ contents, loader: 'js', resolveDir: args.path.slice(0, args.path.lastIndexOf('/')) }));
  }}] });
piece = bundled.outputFiles[0].text;
const ac = await ACSession.open();
try {
  ac.page.on("requestfailed", req => console.log("FAILED", req.url(), req.failure()?.errorText));
  await ac.page.setViewport({ width: 390, height: 844, deviceScaleFactor: 1 });
  await ac.page.setBypassServiceWorker(true);
  await ac.page.evaluateOnNewDocument(() => {
    window.acDEBUG = false;
    delete Navigator.prototype.serviceWorker;
    if (location.pathname === '/html/') window.WebSocket = class { constructor() { setTimeout(() => { this.onopen?.(); this.onmessage?.({ data: JSON.stringify({ type: 'connected', content: { messages: [], chatters: 1 } }) }); }, 50); } send() {} close() {} };
    new BroadcastChannel('ac-comments-closed').onmessage = () => window.commentsClosed = true;
    new BroadcastChannel('ac-comments-test').onmessage = ({ data }) => window.commentState = data;
  });
  await ac.page.setRequestInterception(true);
  ac.page.on('request', async req => {
    const url = new URL(req.url());
    const js = body => req.respond({ contentType: 'text/javascript', body });
    const json = body => req.respond({ contentType: 'application/json', body: JSON.stringify(body) });
    if (url.pathname === '/html/') return req.respond({ contentType: 'text/html', body: vectorHTML });
    if (url.pathname === '/api/chat-messages') return json({ messages });
    if (url.pathname.endsWith('/disks/laklok.mjs')) return js(piece);
    if (url.pathname.endsWith('/lib/chat.mjs')) return js(chat);
    if (url.pathname.endsWith('/lib/chat-comments.mjs')) return js(module);
    if (url.pathname.endsWith('/disks/chat.mjs')) return js(chatPiece);
    if (url.pathname === '/api/mime') {
      if (url.searchParams.has('chatCounts')) return json({ counts: { [id]: 2 } });
      return json({ replies: [{ name: '@one', text: 'First comment from MIME.' }, { name: '@two', text: '<script>literal text</script>' }] });
    }
    if (req.method() !== 'GET' || url.pathname === '/api/mail' || url.pathname.endsWith('/handle-colors')) return json({});
    if (url.pathname.startsWith('/aesthetic.computer/')) {
      try {
        const body = await readFile(new URL(url.pathname.slice(1), pub));
        const ext = url.pathname.split('.').pop();
        const contentType = ({ mjs: 'text/javascript', js: 'text/javascript', json: 'application/json', png: 'image/png', ttf: 'font/ttf', woff2: 'font/woff2' })[ext] || 'application/octet-stream';
        return req.respond({ contentType, body });
      } catch {}
    }
    return req.continue();
  });
  await ac.boot('laklok?test=1');
  await ac.page.waitForFunction(() => window.commentState?.messages.some(m => m.box), { timeout: 45000 });
  const point = await ac.page.evaluate(() => {
    const state = window.commentState, m = state.messages.find(m => m.box);
    const canvas = document.querySelector('#aesthetic-computer canvas').getBoundingClientRect();
    return { x: canvas.x + (m.x + m.box.x + m.box.width / 2) / state.width * canvas.width,
      y: canvas.y + (m.box.y + 4) / state.height * canvas.height };
  });
  await ac.page.screenshot({ path: '/tmp/laklok-comments-count.png' });
  await ac.page.mouse.click(point.x, point.y);
  await ac.page.waitForSelector('.ac-chat-comments[open]', { timeout: 15000 });
  await ac.page.waitForFunction(() => document.querySelector('.ac-chat-comments')?.textContent.includes('First comment from MIME.'));
  assert.equal(await ac.page.$eval('.comments-list', n => n.querySelectorAll('script').length), 0);
  await ac.page.screenshot({ path: '/tmp/laklok-comments-modal.png' });
  await ac.page.mouse.click(10, 10);
  await ac.page.waitForSelector('.ac-chat-comments', { hidden: true });
  await ac.page.waitForFunction(() => window.commentsClosed, { timeout: 5000 });
  await ac.page.mouse.click(point.x, point.y);
  await ac.page.waitForSelector('.ac-chat-comments[open]');
  await ac.page.keyboard.press('Escape');
  await ac.page.waitForSelector('.ac-chat-comments', { hidden: true });
  await ac.page.goto('https://aesthetic.computer/html/');
  await ac.page.waitForSelector('.comment-count:not([hidden])');
  assert.equal(await ac.page.$eval('.comment-count:not([hidden])', button => button.textContent), '2');
  await ac.page.click('.comment-count:not([hidden])');
  await ac.page.waitForFunction(() => document.querySelector('.ac-chat-comments')?.textContent.includes('First comment from MIME.'));
  await ac.page.screenshot({ path: '/tmp/laklok-vector-comments.png' });
  await ac.page.keyboard.press('Escape');
  await ac.page.waitForSelector('.ac-chat-comments', { hidden: true });
  console.log('PASS: raster and vector count opens MIME comments, escapes markup, dismisses and reopens.');
} catch (error) {
  console.log('State:', await ac.page.evaluate(() => window.commentState));
  await ac.page.screenshot({ path: '/tmp/laklok-comments-failure.png' });
  throw error;
} finally { await ac.browser.close(); }
