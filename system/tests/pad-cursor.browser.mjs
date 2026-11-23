// node system/tests/pad-cursor.browser.mjs
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome' });
try {
  const page = await browser.newPage({ viewport: { width: 800, height: 600 } });
  await page.route('http://localhost:4040/**', async route => {
    const name = new URL(route.request().url()).pathname.slice(1);
    if (name === 'docs.json') return route.fulfill({ contentType: 'application/json', body: JSON.stringify({
      prompts: { tape: { sig: 'tape [duration] [flags]', params: [{ name: 'duration', type: 'number', required: false }, { name: 'flags', type: 'flags', values: ['mic', 'nomic'], required: false }] } },
      pieces: { notepat: { sig: 'notepat' }, nopaint: { sig: 'nopaint' }, secret: { sig: 'secret', hidden: true } },
    }) });
    if (name === 'api/piece-hit') return route.fulfill({ contentType: 'application/json', body: '{"pieces":[]}' });
    if (['pad-cursor.mjs', 'pad-deck.mjs', 'pad-ring.mjs', 'pixel-font-data.mjs'].includes(name)) {
      return route.fulfill({ contentType: 'text/javascript', body: await readFile(new URL('../public/aesthetic.computer/lib/' + name, import.meta.url)) });
    }
    return route.fulfill({ contentType: 'text/html', body: '<style>*{margin:0}#piece{width:800px;height:600px}</style><canvas id="piece" width="200" height="150"></canvas>' });
  });
  await page.goto('http://localhost:4040/');
  const result = await page.evaluate(async () => {
    const { padCursor } = await import('/pad-cursor.mjs');
    const canvas = document.querySelector('#piece');
    const pad = { connected: true, id: 'standard', axes: [0, 0, 0, 0], buttons: Array.from({ length: 16 }, () => ({ pressed: false })) };
    Object.defineProperty(navigator, 'getGamepads', { value: () => [pad] });
    const events = [];
    for (const name of ['pointerdown', 'pointerup', 'click']) canvas.addEventListener(name, e => events.push(e.type));
    let name = 'prompt';
    padCursor({ canvas, piece: () => name });
    const face = document.querySelector('#pad-cursor');
    const step = () => new Promise(requestAnimationFrame);
    const sample = async (axis) => {
      pad.axes = [0, 0, 0, 0];
      pad.axes[axis] = 0.35;
      await step();
      const x = () => face.getBoundingClientRect().x;
      let last = x(), initial = last, moving = 0;
      for (let i = 0; i < 24; i++) {
        await step();
        if (x() > last) moving++;
        last = x();
      }
      return { moving, travel: last - initial };
    };
    const left = await sample(0);
    const center = [...face.getContext('2d').getImageData(1, 1, 1, 1).data];
    pad.buttons[0].pressed = true;
    await sample(0); // A drag.
    pad.buttons[0].pressed = false;
    await step();
    // Typing: the left stick's ring offers only what may come next.
    const field = document.createElement('input');
    field.id = 'software-keyboard-input';
    field.style.position = 'absolute';
    field.style.top = '0';
    document.body.append(field);
    field.focus();
    const tap = async (b, axes) => {
      pad.axes = axes;
      await step();
      pad.buttons[b].pressed = true;
      await step();
      pad.buttons[b].pressed = false;
      pad.axes = [0, 0, 0, 0];
      await step();
    };
    await new Promise(r => setTimeout(r, 200)); // /docs.json loads.
    await step();
    const typed = [];
    await tap(0, [0, -1, 0, 0]); typed.push(field.value); // Focused but not asked: A is the cursor's, nothing is written.
    await tap(2, [0, 0, 0, 0]); // X asks for the rings.
    await tap(0, [0, -1, 0, 0]); typed.push(field.value); // The top word: "tape" (shortest first).
    await tap(0, [0, -1, 0, 0]); typed.push(field.value); // Then its flags.
    await tap(2, [0, 0, 0, -1]); typed.push(field.value); // The wheel's first petal, X's place.
    await tap(2, [0, 0, 0, 0]); typed.push(field.value); // X alone deletes.
    const cursorHidden = !face.getClientRects().length;
    field.blur();
    await step();
    pad.axes = [0.1, 0, 0, 0];
    const idle = face.style.transform;
    await step();
    const still = idle === face.style.transform;
    const fullscreen = [];
    window.webkit = { messageHandlers: { fullscreen: { postMessage: m => fullscreen.push(m.on) } } };
    pad.buttons[8] = { pressed: true };
    await step(); await step();
    pad.buttons[8].pressed = false;
    await step();
    pad.buttons[8].pressed = true;
    await step();
    // Another piece gets the cursor too, unless it reads the pad itself.
    name = 'notepat';
    const inPiece = await sample(0);
    window.acPieceReadsPad = true;
    name = 'fight';
    pad.axes = [0.6, 0, 0, 0];
    await step(); await step();
    const padPieceHidden = !face.getClientRects().length;
    window.acPieceReadsPad = false;
    name = 'prompt';
    const toggles = [];
    window.acFpsHud = on => { toggles.push(on); localStorage.setItem('ac:fps-hud', on ? '1' : '0'); };
    pad.buttons[11] = { pressed: true };
    await step(); await step();
    pad.buttons[11].pressed = false;
    await step();
    pad.buttons[11].pressed = true;
    await step();
    window.acRaxSheet = { up: true };
    pad.buttons[11].pressed = false;
    await step();
    pad.buttons[11].pressed = true;
    await step();
    return { left, inPiece, padPieceHidden, typed, cursorHidden, center, size: face.width, fullscreen, events, still, toggles, hidden: !face.getClientRects().length };
  });
  assert.equal(result.left.moving, 24);
  assert.deepEqual(result.typed, ['', 'tape ', 'tape mic ', 'tape mic d', 'tape mic ']);
  assert.ok(result.cursorHidden);
  assert.equal(result.inPiece.moving, 24);
  assert.ok(result.padPieceHidden);
  assert.equal(result.size, 3);
  assert.deepEqual(result.center, [255, 255, 255, 255]);
  assert.deepEqual(result.fullscreen, [true, true]);
  assert.deepEqual(result.events, ['pointerdown', 'pointerup', 'click', 'pointerdown', 'pointerup', 'click']);
  assert.ok(result.still && result.hidden);
  assert.deepEqual(result.toggles, [true, false]);
  console.log('Prompt cursor moves every sampled frame. Drag, release, cursor in other pieces, none in pad pieces, ring words, letter wheel, dead zone and Rax handoff passed.');
} finally { await browser.close(); }
