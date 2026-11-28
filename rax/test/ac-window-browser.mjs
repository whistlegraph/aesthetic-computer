// 🪟 An AC window (ac-window.mjs) against a stand-in for an AC page: a page of
// this server's own with one canvas of a known picture, which keeps every
// pointer and key it is given. The frame's picture comes on to the panel, the
// panel's mouse and the keyboard reach the page, one frame is alive at a
// time, and on an origin that cannot read AC the window is a card.
// (rax's own server serves no AC page: the real ones are only on aesthetic.computer.)
// node rax/test/ac-window-browser.mjs [shot.png]
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';
import { AC_WINDOWS, REST_MS } from '../ac-window.mjs';

const STANDIN = `<!doctype html><meta charset="utf-8"><title>stand-in</title>
<style>html,body{margin:0;overflow:hidden}canvas{position:fixed;inset:0;width:100%;height:100%}textarea{position:fixed;left:0;top:0;opacity:0}</style>
<canvas id="c" width="320" height="200"></canvas><textarea id="t"></textarea>
<script>
  const c = document.getElementById('c'), g = c.getContext('2d'), t = document.getElementById('t');
  window.seen = []; window.frames = 0;
  let mark = null;
  for (const type of ['pointerdown', 'pointermove', 'pointerup'])
    addEventListener(type, (e) => { seen.push([type, e.x / innerWidth, e.y / innerHeight, e.buttons]); if (type === 'pointerdown') mark = [e.x * 320 / innerWidth, e.y * 200 / innerHeight]; });
  // (As AC does: the text box takes the keyboard as the page loads, and again on a key if it has lost it: bios.mjs keyboard.focusHandler.)
  addEventListener('keydown', (e) => { seen.push(['keydown', e.key, e.isTrusted]); if (document.activeElement !== t) t.focus(); });
  t.focus();
  (function draw() {
    frames++;
    g.fillStyle = '#f00'; g.fillRect(0, 0, 160, 200);
    g.fillStyle = '#00f'; g.fillRect(160, 0, 160, 200);
    if (mark) { g.fillStyle = '#0f0'; g.fillRect(mark[0] - 12, mark[1] - 12, 24, 24); }
    requestAnimationFrame(draw);
  })();
</script>`;

const dir = await mkdtemp(join(tmpdir(), 'rax-ac-window-'));
const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'],
  verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
await app.ready;
// The stand-in is served beside rax, from its origin.
const [rax] = app.server.listeners('request');
app.server.removeAllListeners('request');
app.server.on('request', (req, res) => (req.url.startsWith('/standin')
  ? res.writeHead(200, { 'Content-Type': 'text/html', 'Cache-Control': 'no-store' }).end(STANDIN) : rax(req, res)));
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
const [chat, prompt] = AC_WINDOWS;
try {
  const errors = [];
  const open = async (path, standin) => {
    const page = await browser.newPage({ viewport: { width: 800, height: 500 } });
    page.setDefaultTimeout(15000);
    page.on('pageerror', (e) => errors.push(e.message));
    await page.addInitScript((standin) => {
      window.acTOKEN = 'jeffrey';
      if (standin) window.raxWindowURL = (piece) => `/standin?piece=${piece}`;
      window.opened = [];
      window.open = (url) => { window.opened.push(url); return null; };
    }, standin);
    await page.goto(base + path);
    await page.waitForFunction(() => window.raxDebug?.().player && window.raxWindows);
    return page;
  };
  const body = (room) => app.rooms.get(room).match.seats.find(Boolean).player;
  const stand = async (page, room, x, z) => {
    Object.assign(body(room), { x, z, y: 1.7, yaw: 0, pitch: 0 });
    body(room).life++;
    await page.waitForFunction(([x, z]) => { const p = raxDebug().player; return Math.hypot(p.x - x, p.z - z) < 0.5; }, [x, z]);
    await page.evaluate(() => raxLook({ yaw: 0, pitch: 0 }));
  };
  const frames = (page) => page.evaluate(() => [...document.querySelectorAll('iframe[data-rax-window]')].map((f) => f.dataset.raxWindow));
  // The colour of a panel's pixel, a fraction of the way across and down.
  const pixel = (page, id, fx, fy) => page.evaluate(([id, fx, fy]) => {
    const p = raxWindows.all.find((w) => w.def.id === id).panel, k = (Math.floor(fy * p.rows) * p.cols + Math.floor(fx * p.cols)) * 4;
    return [...p.pixels.slice(k, k + 3)];
  }, [id, fx, fy]);
  const inFrame = (page, id, f, arg) => page.evaluate(([id, f, arg]) => (0, eval)(`(${f})`).call(document.querySelector(`iframe[data-rax-window="${id}"]`).contentWindow, arg), [id, String(f), arg]);

  // 1. At home both windows stand, and are in the world's words as things to step into.
  const page = await open('/?debug', true);
  assert.equal((await page.evaluate(() => raxDebug().room)), 'origin');
  for (const w of AC_WINDOWS) assert.ok((await page.evaluate(() => raxDebug().panels)).includes(w.id), `${w.id} stands at home`);
  await stand(page, 'origin', chat.x, chat.z - 3.5);
  const near = await page.evaluate(() => rax.state().nearby.map((t) => t.name));
  for (const w of AC_WINDOWS) assert.ok(near.some((n) => n.startsWith(w.piece)), `${w.piece} is near, in words: ${near.join(' | ')}`);

  // 2. Near the chat's window its page is framed, alone, and its picture is on the panel.
  await page.waitForFunction(() => document.querySelectorAll('iframe[data-rax-window]').length === 1);
  assert.deepEqual(await frames(page), [chat.id], 'the nearest window is the one awake');
  await page.waitForFunction(() => raxWindows.all[0].seen);
  assert.deepEqual(await pixel(page, chat.id, 0.25, 0.5), [255, 0, 0], 'the page\'s left half, red');
  assert.deepEqual(await pixel(page, chat.id, 0.75, 0.5), [0, 0, 255], 'its right half, blue');
  assert.equal(await inFrame(page, chat.id, function () { return this.location.search; }), `?piece=${chat.piece}`);

  // 3. Stepped in, the page has the keyboard: what is typed is typed there, by the browser itself.
  await page.waitForFunction((id) => raxDebug().focus === `panel:${id}`, chat.id).catch(async (e) => { const d = await page.evaluate(() => raxDebug()); throw Error(`not aimed at the window: ${JSON.stringify({ focus: d.focus, player: d.player, origin: d.origin, forward: d.forward })}`); });
  await page.keyboard.press('ShiftLeft'); // (the first key engages the keyboard: client.mjs CONTROL_KEYS; F is not one)
  await page.keyboard.press('KeyF');
  await page.waitForFunction((id) => raxDebug().dock?.id === `panel:${id}` && raxDebug().dock.t === 1, chat.id).catch(async (e) => { const d = await page.evaluate(() => raxDebug()); throw Error(`not stepped in: ${JSON.stringify({ focus: d.focus, lock: d.lock, dock: d.dock, active: await page.evaluate(() => document.activeElement?.tagName) })}`); });
  await page.waitForFunction(() => document.activeElement?.dataset?.raxWindow);
  await page.keyboard.type('hi');
  assert.equal(await inFrame(page, chat.id, function () { return this.document.getElementById('t').value; }), 'hi');
  assert.ok((await inFrame(page, chat.id, function () { return this.seen; })).some((e) => e[0] === 'keydown' && e[1] === 'h' && e[2] === true), 'a real key, not a made one');

  // 4. A press on the panel is a press on the page, where the panel's mouse is: the middle.
  // (A finger on the glass, made by hand: a mouse presses only while it is captured, which a headless browser cannot do.)
  const finger = (type) => page.evaluate((type) => document.querySelector('canvas').dispatchEvent(new PointerEvent(type, { pointerType: 'touch', pointerId: 1, isPrimary: true, clientX: 400, clientY: 250, bubbles: true, cancelable: true })), type);
  await finger('pointerdown');
  await page.waitForTimeout(120);
  await finger('pointerup');
  await page.waitForFunction((id) => document.querySelector(`iframe[data-rax-window="${id}"]`).contentWindow.seen.some((e) => e[0] === 'pointerup'), chat.id);
  const presses = (await inFrame(page, chat.id, function () { return this.seen; })).filter((e) => e[0] === 'pointerdown');
  assert.equal(presses.length, 1);
  // (Near the middle: the face is fitted between the HUD's edges, so the picture's middle is not quite its own. The page marks where it was pressed, and the mark comes back on the panel there.)
  assert.ok(Math.abs(presses[0][1] - 0.5) < 0.2 && Math.abs(presses[0][2] - 0.5) < 0.2 && presses[0][3] === 1, `pressed about the middle: ${presses[0]}`);
  await page.waitForFunction(([fx, fy]) => { const p = raxWindows.all[0].panel, k = (Math.floor(fy * p.rows) * p.cols + Math.floor(fx * p.cols)) * 4; return p.pixels[k] === 0 && p.pixels[k + 1] === 255; }, presses[0].slice(1, 3));
  // (The click was on the world, which took the keyboard; the page has it back, and a key that does reach rax is handed on.)
  await page.keyboard.type('!');
  assert.equal(await inFrame(page, chat.id, function () { return this.document.getElementById('t').value; }), 'hi!');
  await page.evaluate(() => window.focus());
  await page.keyboard.type('x');
  await page.waitForFunction((id) => document.querySelector(`iframe[data-rax-window="${id}"]`).contentWindow.document.getElementById('t').value === 'hi!x', chat.id);
  if (process.argv[2]) await page.screenshot({ path: process.argv[2] });

  // 5. Escape, pressed in the page, is the way out: the eye comes back and the keys are rax's.
  await page.waitForFunction(() => document.activeElement?.dataset?.raxWindow);
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !raxDebug().dock);
  assert.equal(await page.evaluate(() => document.hasFocus() && !document.activeElement?.dataset?.raxWindow), true, 'the keyboard is the world\'s again');

  // 6. A page that goes to rax is turned back: the world is not put inside itself.
  await inFrame(page, chat.id, function () { this.history.pushState({}, '', '/rax~studio'); });
  await page.waitForFunction((id) => { const f = document.querySelector(`iframe[data-rax-window="${id}"]`); return f?.contentWindow.location.pathname === '/standin' && f.contentWindow.frames > 2; }, chat.id);
  assert.deepEqual(await frames(page), [chat.id]);

  // 7. Walked over to the prompt's window, that one wakes and the chat's sleeps on its last picture.
  await page.waitForFunction(() => raxWindows.all[0].seen && raxWindows.all[0].panel.pixels[0] === 255);
  await stand(page, 'origin', prompt.x, prompt.z - 2.5);
  await page.waitForFunction((id) => { const all = [...document.querySelectorAll('iframe[data-rax-window]')]; return all.length === 1 && all[0].dataset.raxWindow === id; }, prompt.id);
  await page.waitForFunction(() => raxWindows.all[1].seen);
  assert.deepEqual(await pixel(page, prompt.id, 0.75, 0.5), [0, 0, 255]);
  assert.deepEqual(await pixel(page, chat.id, 0.25, 0.1), [255, 0, 0], 'the sleeping window keeps its picture');
  assert.equal(await inFrame(page, prompt.id, function () { return this.location.search; }), `?piece=${prompt.piece}`);

  // 8. Far from both, the frame is taken down after a while.
  await stand(page, 'origin', 7, -7.2);
  assert.equal((await frames(page)).length, 1, 'not at once');
  await page.waitForFunction(() => !document.querySelector('iframe[data-rax-window]'), null, { timeout: REST_MS + 8000 });
  await page.close();

  // 9. They stand in the studio too, and in no other room.
  const studio = await open('/?debug&room=studio', true);
  for (const w of AC_WINDOWS) assert.ok((await studio.evaluate(() => raxDebug().panels)).includes(w.id), `${w.id} stands in the studio`);
  await studio.close();
  const other = await open('/?debug&room=elsewhere', true);
  await other.waitForTimeout(1500);
  assert.ok(!(await other.evaluate(() => raxDebug().panels)).some((id) => id.startsWith('ac-')), 'no window in another room');
  assert.deepEqual(await frames(other), []);
  await other.close();

  // 10. Where AC cannot be read (this server has none; rax.ac is another origin) a window is a card, no page is framed, and stepping in opens the piece on aesthetic.computer.
  const card = await open('/?debug', false);
  await stand(card, 'origin', chat.x, chat.z - 3.5);
  await card.waitForTimeout(1500);
  assert.deepEqual(await frames(card), [], 'no frame');
  assert.deepEqual(await pixel(card, chat.id, 0.9, 0.9), [15, 17, 24], 'the card');
  await card.waitForFunction((id) => raxDebug().focus === `panel:${id}`, chat.id);
  await card.keyboard.press('ShiftLeft');
  await card.keyboard.press('KeyF');
  await card.waitForFunction(() => window.opened.length > 0);
  assert.deepEqual(await card.evaluate(() => window.opened), [`https://aesthetic.computer/${chat.piece}`]);
  if (process.argv[3]) await card.screenshot({ path: process.argv[3] });
  await card.close();

  assert.deepEqual(errors, [], `no page errors: ${errors.join(' | ')}`);
  console.log('ac-window: ok');
} finally {
  await browser.close();
  await app.close?.();
  app.server.close();
  await rm(dir, { recursive: true, force: true });
}
