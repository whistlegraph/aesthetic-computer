// node system/tests/pad-cursor.browser.mjs
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome' });
try {
  const page = await browser.newPage({ viewport: { width: 800, height: 600 } });
  await page.route('http://localhost:4040/**', async route => {
    const name = new URL(route.request().url()).pathname.slice(1);
    if (['pad-cursor.mjs', 'cursor-shape.mjs'].includes(name)) {
      return route.fulfill({ contentType: 'text/javascript', body: await readFile(new URL('../public/aesthetic.computer/lib/' + name, import.meta.url)) });
    }
    return route.fulfill({ contentType: 'text/html', body: '<style>*{margin:0}#piece{width:800px;height:600px}</style><canvas id="piece" width="200" height="150"></canvas>' });
  });
  await page.goto('http://localhost:4040/');
  const result = await page.evaluate(async () => {
    const { padCursor } = await import('/pad-cursor.mjs');
    const canvas = document.querySelector('#piece');
    const pad = { connected: true, id: 'standard', axes: [0, 0, 0, 0], buttons: [{ pressed: false }] };
    Object.defineProperty(navigator, 'getGamepads', { value: () => [pad] });
    const events = [];
    for (const name of ['pointerdown', 'pointerup', 'click']) canvas.addEventListener(name, e => events.push(e.type));
    padCursor({ canvas, piece: () => 'prompt' });
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
    pad.buttons[0].pressed = true;
    const right = await sample(2);
    pad.buttons[0].pressed = false;
    await step();
    pad.axes = [0.1, 0, 0, 0];
    const idle = face.style.transform;
    await step();
    const still = idle === face.style.transform;
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
    return { left, right, events, still, toggles, hidden: !face.getClientRects().length };
  });
  assert.equal(result.left.moving, 24);
  assert.equal(result.right.moving, 24);
  assert.ok(result.right.travel < result.left.travel * 0.5);
  assert.deepEqual(result.events, ['pointerdown', 'pointerup', 'click']);
  assert.ok(result.still && result.hidden);
  assert.deepEqual(result.toggles, [true, false]);
  console.log('Prompt cursor moves every sampled frame on both sticks. Drag, release, dead zone and Rax handoff passed.');
} finally { await browser.close(); }
