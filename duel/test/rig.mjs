import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { mkdir, mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { _electron as electron } from 'playwright';
import { createDuelServer } from '../server.mjs';

const accounts = process.argv.includes('--accounts');
const server = createDuelServer({
  prefix: '/dool',
  ...(accounts
    ? {}
    : {
        verifyIdentity: async () => {
          throw Error('anonymous test');
        },
      }),
});
server.server.listen(0, '127.0.0.1');
await once(server.server, 'listening');
const profile = await mkdtemp(join(tmpdir(), 'dool-rig-test-'));
const env = {
  ...process.env,
  DOOL_RIG_PROFILE: profile,
  DOOL_CLIENTS: '2',
  DUEL_URL: `http://127.0.0.1:${server.server.address().port}/dool/`,
};
delete env.ELECTRON_RUN_AS_NODE;
const executablePath = createRequire(
  new URL('../../ac-electron/package.json', import.meta.url),
)('electron');
const layoutOnly = process.argv.includes('--layout-only');
console.log('Starting the rig test…');
const app = await electron.launch({
  executablePath,
  timeout: 20000,
  args: [
    fileURLToPath(new URL('../../ac-electron/', import.meta.url)),
    '--dool-rig',
  ],
  env,
});
app.context().setDefaultTimeout(10000);
console.log('Electron connected.');
const watchdog = setTimeout(() => {
  console.error('Rig test exceeded 60 seconds.');
  app.process().kill('SIGKILL');
}, 60000);
const wait = async (test, label) => {
  const until = Date.now() + 15000;
  while (!(await test())) {
    if (Date.now() > until) throw Error(`Timed out: ${label}`);
    await delay(40);
  }
};
try {
  await wait(
    () =>
      app
        .context()
        .pages()
        .filter((p) => p.url().startsWith(env.DUEL_URL)).length === 2,
    'two clients',
  );
  console.log('Two clients loaded.');
  const unordered = app
    .context()
    .pages()
    .filter((p) => p.url().startsWith(env.DUEL_URL));
  await app.evaluate(({ BaseWindow }) =>
    Promise.all(
      BaseWindow.getAllWindows()[0].doolClients.map((v, i) =>
        v.webContents.executeJavaScript('window.rigTestIndex = ' + i),
      ),
    ),
  );
  const pages = [];
  for (const page of unordered)
    pages[await page.evaluate(() => window.rigTestIndex)] = page;
  const errors = [];
  for (const p of pages) {
    p.on('pageerror', (e) => errors.push(e.message));
    await p.waitForFunction(() => document.getElementById('game').doolStats);
    assert.equal(
      await p.evaluate(() => document.getElementById('game').dataset.engine),
      'webgpu',
    );
    // Keep the user's real pointer from steering this automated run.
    await p.evaluate(() =>
      document.addEventListener(
        'mousemove',
        (e) => {
          if (e.isTrusted) e.stopImmediatePropagation();
        },
        true,
      ),
    );
  }
  for (const page of pages)
    await page.evaluate(() => {
      window.rigKeys = [];
      window.rigLockReady = false;
      document.addEventListener('pointerlockchange', () => {
        window.rigLockReady = !!document.pointerLockElement;
        rigKeys.push(['lock', rigLockReady]);
      });
      for (const type of ['keydown', 'keyup'])
        document.addEventListener(type, (e) => rigKeys.push([type, e.code]));
      window.addEventListener('blur', () => rigKeys.push(['blur']));
    });
  console.log('Both clients rendered with WebGPU.');
  if (process.argv.includes('--accounts')) {
    for (const page of pages) {
      await page.waitForFunction(
        () => {
          const s = document.querySelector('canvas').doolStats;
          return (
            s.members.length === 1 &&
            s.members.every((m) => m.handle && m.colors?.length) &&
            s.connection.samples > 0
          );
        },
        {},
        { timeout: 20000 },
      );
    }
    const state = await pages[0].evaluate(
      () => document.querySelector('canvas').doolStats,
    );
    console.log(
      JSON.stringify({
        handles: state.members.map((m) => '@' + m.handle),
        ping: state.connection.label,
      }),
    );
    await pages[0].screenshot({ path: 'tmp/duel/room-handles-ping.png' });
  }
  const room = new URL(pages[0].url()).searchParams.get('room');
  assert.equal(new URL(pages[1].url()).searchParams.get('room'), room);
  const match = server.rooms.get(room).match;
  await wait(
    () => match.phase === (accounts ? 'waiting' : 'playing'),
    'player roles settled',
  );
  if (accounts) {
    const states = await Promise.all(
      pages.map((p) =>
        p.evaluate(() => document.querySelector('canvas').doolStats),
      ),
    );
    assert.deepEqual(states.map((s) => s.role).sort(), ['player', 'spectator']);
    assert.equal(match.seats.filter(Boolean).length, 1);
    console.log('One handle, one avatar; second pane is freecam.');
    console.log(
      await pages[0].evaluate(() =>
        Array.from(navigator.getGamepads())
          .filter(Boolean)
          .map((p) => ({
            id: p.id,
            mapping: p.mapping,
            axes: p.axes.length,
            buttons: p.buttons.length,
          })),
      ),
    );
  }
  if (accounts && process.argv.includes('--controls')) {
    const primary =
      (await pages[0].evaluate(
        () => document.querySelector('canvas').doolStats.role,
      )) === 'player'
        ? 0
        : 1;
    const watching = 1 - primary;
    await app.evaluate(async ({ app, BaseWindow }, index) => {
      const w = BaseWindow.getAllWindows()[0];
      app.focus({ steal: true });
      w.focus();
      await w.doolFocus(index);
    }, primary);
    await pages[primary].waitForFunction(() => document.hasFocus());
    const fake = async (page) =>
      page.evaluate(() => {
        window.testPad = {
          id: '8BitDo test input',
          index: 0,
          connected: true,
          mapping: 'standard',
          axes: [0, 0, 0, 0],
          buttons: Array.from({ length: 17 }, () => ({
            pressed: false,
            value: 0,
          })),
        };
        Object.defineProperty(navigator, 'getGamepads', {
          configurable: true,
          value: () => [window.testPad],
        });
      });
    await fake(pages[primary]);
    await delay(100);
    const player = match.seats.find(Boolean),
      x = player.player.x;
    await pages[primary].evaluate(() => (testPad.axes[0] = 0.8));
    await wait(() => Math.abs(player.player.x - x) > 0.3, 'gamepad movement');
    await pages[primary].evaluate(() => {
      testPad.axes[0] = 0;
      testPad.buttons[8].pressed = true;
    });
    await pages[watching].waitForFunction(() => document.hasFocus());
    await delay(250);
    await fake(pages[watching]);
    await delay(100);
    const old = await pages[watching].evaluate(
      () => document.querySelector('canvas').doolStats.camera,
    );
    const settledX = player.player.x;
    await pages[watching].evaluate(() => {
      testPad.axes[1] = -1;
      testPad.buttons[5].pressed = true;
    });
    await pages[watching].waitForFunction((old) => {
      const c = document.querySelector('canvas').doolStats.camera;
      return Math.hypot(c.x - old.x, c.y - old.y, c.z - old.z) > 0.3;
    }, old);
    assert.ok(
      Math.abs(player.player.x - settledX) < 0.05,
      'freecam cannot move the player',
    );
    await pages[watching].evaluate(() => {
      testPad.axes[1] = 0;
      testPad.buttons[5].pressed = false;
      testPad.buttons[9].pressed = true;
    });
    await wait(
      () =>
        app.evaluate(({ BaseWindow }) =>
          BaseWindow.getAllWindows()[0].isFullScreen() || BaseWindow.getAllWindows()[0].isSimpleFullScreen(),
        ),
      'controller fullscreen',
    );
    await delay(800);
    await pages[watching].screenshot({
      path: 'tmp/duel/spectator-handles.png',
    });
    await app.evaluate(({ BaseWindow }) => {
      BaseWindow.getAllWindows()[0].setFullScreen(false);
      BaseWindow.getAllWindows()[0].setSimpleFullScreen(false);
    });
    for (const page of pages)
      await page.evaluate(() => {
        delete navigator.getGamepads;
        delete window.testPad;
      });
    console.log(
      'Controller simulation passed: movement, pane switch, spectator flight, fullscreen.',
    );
  }
  const initial = match.seats.map((s) => s && { ...s.player });
  if (!layoutOnly) {
    await app.evaluate(({ app, BaseWindow }) => {
      const w = BaseWindow.getAllWindows()[0];
      app.focus({ steal: true });
      w.focus();
      w.doolClients[0].webContents.focus();
    });
    console.log('Capturing left client.');
    await pages[0].waitForFunction(() => document.hasFocus());
    await pages[0].locator('canvas').click();
    await pages[0].waitForFunction(() => window.rigLockReady);
    console.log('Left client captured.');
    await pages[0].keyboard.down('KeyD');
    await wait(
      () =>
        match.seats.some((s, i) => Math.abs(s.player.x - initial[i].x) > 0.5),
      'left client movement',
    );
    await pages[0].keyboard.up('KeyD');
    const leftSeat = match.seats.findIndex(
      (s, i) => Math.abs(s.player.x - initial[i].x) > 0.5,
    );
    assert.equal(
      match.seats[1 - leftSeat].player.x,
      initial[1 - leftSeat].x,
      'input stays with the selected client',
    );
    await pages[0].keyboard.press('KeyV');
    await pages[0].keyboard.press('Escape');
    await pages[0].waitForFunction(() => !document.pointerLockElement);
    await app.evaluate(({ BaseWindow }) =>
      BaseWindow.getAllWindows()[0].doolFocus(1),
    );
    await pages[1].locator('canvas').click();
    await pages[1].waitForFunction(() => window.rigLockReady);
    await pages[1].keyboard.down('KeyD');
    try {
      await wait(
        () =>
          Math.abs(
            match.seats[1 - leftSeat].player.x - initial[1 - leftSeat].x,
          ) > 0.5,
        'right client movement',
      );
    } catch (error) {
      console.log(
        'Input trace',
        JSON.stringify({
          leftSeat,
          players: match.seats.map((s) => ({ x: s.player.x, last: s.last })),
          pages: await Promise.all(
            pages.map((p) =>
              p.evaluate(() => ({
                keys: rigKeys,
                focus: document.hasFocus(),
                active: document.activeElement?.tagName,
                lock: !!document.pointerLockElement,
              })),
            ),
          ),
        }),
      );
      throw error;
    }
    await pages[1].keyboard.up('KeyD');
    await pages[1].dispatchEvent('canvas', 'mousedown', {
      button: 0,
      bubbles: true,
    });
    await pages[1].waitForFunction(
      () => document.querySelector('canvas').doolStats.promptOpen,
    );
    assert.equal(match.rockets.length, 0, 'empty hands open the prompt');
    await pages[1].dispatchEvent('canvas', 'mouseup', {
      button: 0,
      bubbles: true,
    });
    await pages[1].keyboard.press('Escape');
  }
  const layout = await app.evaluate(({ BaseWindow }) => {
    const w = BaseWindow.getAllWindows()[0];
    w.setContentSize(1100, 650);
    return BaseWindow.getAllWindows().length;
  });
  assert.equal(layout, 1);
  await wait(async () => {
    const widths = await Promise.all(
      pages.map((p) => p.evaluate(() => innerWidth)),
    );
    return widths[0] + widths[1] === 1099;
  }, 'resized panes');
  await app.evaluate(({ BaseWindow }) =>
    BaseWindow.getAllWindows()[0].setSize(420, 300),
  );
  await wait(async () => {
    const widths = await Promise.all(
      pages.map((p) => p.evaluate(() => innerWidth)),
    );
    return widths[0] + widths[1] === 419;
  }, 'compact Slab tile');
  await mkdir(new URL('../../tmp/duel/', import.meta.url), { recursive: true });
  console.log('Both clients resized, including a compact Slab tile.');
  assert.deepEqual(errors, []);
  await app.evaluate(({ BaseWindow }) => BaseWindow.getAllWindows()[0].close());
  await wait(
    () => !server.rooms.has(room),
    'closing the rig disconnects both clients',
  );
  console.log(
    layoutOnly
      ? 'Rig layout passed: two WebGPU clients, same room, compact resize, clean disconnect.'
      : 'Rig passed: one window, two independent WebGPU clients, same room, pointer capture, isolated controls, camera switch, prompt, resize, clean disconnect.',
  );
} finally {
  clearTimeout(watchdog);
  const stop = setTimeout(() => app.process().kill('SIGKILL'), 3000);
  await app.close().catch(() => {});
  clearTimeout(stop);
  await server.close();
  await rm(profile, { recursive: true, force: true });
}
