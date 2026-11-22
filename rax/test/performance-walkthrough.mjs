// Run on the machine being measured. Each sample counts newly drawn Rax
// frames, including stalls, in a visible play session against a local server.
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { appDoor } from '../../toolchain/mcp/acin-app.mjs';
import { createRaxServer } from '../server.mjs';
import { kioskPlan } from '../kiosks-plan.mjs';

const app = createRaxServer({ guests: true, listHandles: async () => [] });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = 'http://127.0.0.1:' + app.server.address().port;
const call = async (js) => {
  const result = await appDoor({ action: 'eval', js });
  if (!result.ok) throw Error(result.error);
  return result.value;
};
const original = (await appDoor({ action: 'info' })).url;
if (!original) {
  await app.close();
  throw Error('Open the native Aesthetic Computer app first.');
}
const evaluate = (fn, arg) => call('return (' + fn.toString() + ')(' + (JSON.stringify(arg) ?? '') + ')');
async function waitFor(fn, arg, timeout = 25000) {
  const until = Date.now() + timeout;
  while (Date.now() < until) {
    if (await evaluate(fn, arg)) return;
    await delay(150);
  }
  throw Error('Timed out waiting for ' + fn);
}
async function visit(path) {
  await call('setTimeout(() => location.replace(' + JSON.stringify(base + path) + '), 50); return true');
  await delay(600);
  await waitFor(() => !!window.raxDebug);
  await evaluate(() => {
    const pad = {
      connected: true,
      id: 'walkthrough',
      index: 0,
      axes: [0, 0, 0, 0],
      buttons: Array.from({ length: 17 }, () => ({ pressed: false, value: 0 })),
    };
    window.pad = pad;
    Object.defineProperty(navigator, 'getGamepads', { configurable: true, value: () => [pad] });
    document.querySelector('canvas').focus();
  });
}
try {
  async function press(i) {
    await evaluate(async (i) => {
      const next = () => new Promise(requestAnimationFrame);
      pad.buttons[i].pressed = true;
      await next();
      await next();
      pad.buttons[i].pressed = false;
      await next();
      await next();
    }, i);
  }
  async function measure(view) {
    const sample = await evaluate(async () => {
      const canvas = document.querySelector('canvas');
      const times = [],
        cpu = [],
        counts = [];
      let frame = canvas.raxStats,
        first = null,
        last = 0;
      await new Promise((resolve) => {
        const tick = (now) => {
          if (first === null) {
            first = last = now;
          } else if (canvas.raxStats !== frame) {
            frame = canvas.raxStats;
            times.push(now - last);
            last = now;
            cpu.push(frame.cpuMs);
            counts.push(frame.triangles);
          }
          if (now - first >= 4000) resolve();
          else requestAnimationFrame(tick);
        };
        requestAnimationFrame(tick);
      });
      const p95 = (arr) => arr.sort((a, b) => a - b)[Math.ceil(arr.length * 0.95) - 1];
      return {
        fps: +((1000 * times.length) / (last - first)).toFixed(1),
        p95FrameMs: +p95(times).toFixed(1),
        p95RenderCpuMs: +p95(cpu).toFixed(1),
        gapsOver25ms: times.filter((x) => x > 25).length,
        p95Triangles: p95(counts),
        backend: frame.backend,
        buffer: [canvas.width, canvas.height],
        room: raxDebug().room,
      };
    });
    console.log(JSON.stringify({ view, ...sample }));
  }
  await visit('/?room=lobby&debug');
  await waitFor(() => window.raxDebug?.().player && raxDebug().panels.includes('yard-notepat'));
  await delay(800);
  await measure('plaza');
  await press(3);
  await measure('third-person plaza');
  await press(3);
  const kiosk = kioskPlan('lobby').find((k) => k.id === 'yard-notepat');
  function place(x, z, yaw) {
    const body = app.rooms.get('lobby').match.seats.find(Boolean).player;
    Object.assign(body, { x, z, yaw, pitch: 0, y: 1.7, life: body.life + 1 });
  }
  place(kiosk.x, kiosk.z + 1.8, kiosk.yaw);
  await waitFor((x) => Math.abs(raxDebug().player.x - x) < 0.2, kiosk.x);
  await evaluate((yaw) => raxLook({ yaw, pitch: 0 }), kiosk.yaw);
  await waitFor(() => raxDebug().focus === 'panel:yard-notepat');
  await press(5);
  await waitFor(() => raxDebug().dock?.t === 1);
  await measure('Notepat');
  await press(10);
  await evaluate(() => {
    pad.buttons[6].value = 1;
  });
  await delay(500);
  await measure('Notepat 5x');
  await press(11);
  await measure('Notepat 5x with debug');
  await press(11);
  await evaluate(() => {
    pad.buttons[6].value = 0;
  });
  await press(1);
  await waitFor(() => !raxDebug().dock);
  place(6.5, 0, 90);
  await waitFor(() => raxDebug().player.x > 6);
  await evaluate(() => raxLook({ yaw: 90, pitch: 0 }));
  await evaluate(() => {
    pad.axes[1] = -1;
  });
  await measure('walk across east block seams');
  await evaluate(() => {
    pad.axes[1] = 0;
  });
  if (await evaluate(() => raxDebug().room === 'lobby'))
    throw Error('The walkthrough missed the block crossing');
  await waitFor(() => document.querySelector('canvas').raxStats.camera?.cinematic, null, 36000);
  await measure('idle sweep');
  await press(3);
  await waitFor(() => !document.querySelector('canvas').raxStats.camera?.cinematic);
  await visit('/?room=lobby&debug&film=1');
  await waitFor(() => window.raxDebug?.().seat < 0 && raxDebug().panels.includes('yard-notepat'));
  await evaluate(() => raxLook({ x: 1, y: 14, z: -15, yaw: 0, pitch: -26 }));
  await measure('drone over plaza');
} finally {
  const info = await appDoor({ action: 'info' });
  if (info.url?.startsWith(base + '/'))
    await call('setTimeout(() => location.replace(' + JSON.stringify(original) + '), 50); return true');
  await delay(120);

  await app.close();
}
