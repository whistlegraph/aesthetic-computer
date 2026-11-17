// 🧱 Spell "build-on-floor": @jeffrey, on his own floor, aims at a floor square
// and asks for a cube. The film shows where the cube lands against where he
// aimed, three times: a near square, the sky, and facing the elevator.
//
// Two stand-ins, both said here because the picture depends on them:
// - Aesel is a stand-in that answers every prompt with a red 2 m cube.
// - The page that films is a spectator, so it cannot draw @jeffrey's own
//   floor reticle. He paints the aimed square instead (X in the page paints
//   exactly the highlighted square), so the pink square is the aim.
// Every caption about where the cube landed is measured from the take.
import { connectBuilder } from '../../aesel.mjs';
import { direction, wallDistance } from '../../world.mjs';
import { CAR_Z, inCar } from '../../elevator.mjs';

const HANDLES = ['jeffrey'];
for (let i = 1; i < 40; i++) HANDLES.push(`neighbor${i.toString(36)}`);
const TOKEN = 'spell-build-on-floor-stand-in-aesel-0000';
const CUBE = { name: 'Cube', boxes: [{ x: 0, y: 1, z: 0, w: 2, h: 2, d: 2, color: 0xff593b }] };
const PINK = 6;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// The floor square under the crosshair, as client.mjs finds it each frame.
function highlighted(eye, yaw, pitch) {
  const dir = direction(yaw, pitch);
  if (!(dir.y < -0.02)) return null;
  const nearest = wallDistance(eye, dir, 60), t = -eye.y / dir.y;
  if (!(t > 0 && t < Math.min(60, nearest + 0.05))) return null;
  const i = Math.floor((eye.x + dir.x * t + 18) / 2), j = Math.floor((eye.z + dir.z * t + 18) / 2);
  return i >= 0 && i < 18 && j >= 0 && j < 18 ? { i, j, x: -18 + i * 2 + 1, z: -18 + j * 2 + 1 } : null;
}

export default {
  name: 'spell-build-on-floor',
  size: [1280, 720],
  fps: 60,
  offline: true,
  handles: HANDLES,
  // Before the camera: floor 12 is jeffrey's, and the stage's Aesel has a key.
  async set({ app, tower }) {
    await tower.change((t) => { t.arrive('jeffrey', 'film-jeffrey'); return t.take('jeffrey', 12); });
    app.builders.token = TOKEN;
  },
  async direct({ cast, camera, card, caption, wait, roll, fade, ride }) {
    const jeffrey = await cast('jeffrey', { at: [0, CAR_Z], facing: 0 });
    const aesel = connectBuilder({ url: jeffrey.base.replace(/^http/, 'ws') + '/builder', token: TOKEN, name: 'stand-in',
      generate: async () => { await sleep(900); return CUBE; } });
    try {
      await camera.sees('jeffrey');
      await fade(1, 0);
      await ride(jeffrey, 'home'); // up to floor 12, in the dark
      // What the page sends for each thing he does.
      const look = async (pitch, seconds = 0.5) => {
        const from = jeffrey.control.pitch || 0;
        for (let k = 1; k <= 15; k++) { jeffrey.control.pitch = from + ((pitch - from) * k) / 15; await sleep((seconds * 1000) / 15); }
      };
      const aimed = () => highlighted(jeffrey.me, jeffrey.control.yaw, jeffrey.control.pitch);
      const paint = (square, colour) => jeffrey.send({ type: 'paint', i: square.i, j: square.j, c: colour });
      const build = async () => {
        const accepted = jeffrey.until((m) => m.type === 'build-accepted' || (m.type === 'build-result' && m.error), 6000, 'the build to be accepted');
        const listed = jeffrey.until((m) => m.type === 'objects' && m.objects.length > 0 && !m.jobs.length, 12000, 'the cube');
        jeffrey.send({ type: 'prompt', prompt: 'a cube' });
        const a = await accepted;
        if (a.error) throw Error(`the build was refused: ${a.error}`);
        const cube = (await listed).objects.find((o) => o.id === a.objectId);
        return { id: cube.id, x: cube.x, z: cube.z };
      };
      const remove = async (cube) => {
        const gone = jeffrey.until((m) => m.type === 'objects' && !m.objects.length, 6000, 'the cube to go');
        jeffrey.send({ type: 'remove', objectId: cube.id });
        await gone;
      };
      const metres = (n) => `${Math.round(n * 10) / 10} m`;

      // 1. From the north-east corner, under the ceiling (a storey is 4 m):
      // him by the car, the aimed square and the far glass all in frame.
      camera.shot({ kind: 'still', at: [7.4, 3.4, 3.6], look: [1, 0.4, -2] });
      await wait(0.4);
      await roll();
      camera.open(0.5);
      await card('build on your floor', 'aim at a square, ask for a cube', 2.6);
      caption('@jeffrey, floor 12');
      await jeffrey.walkTo(1, 2.2, { pace: 0.4 });
      await jeffrey.face(180, 0.5);

      // 2. Aim at the square three metres ahead; paint it so the aim shows.
      await look(-30);
      const near = aimed();
      paint(near, PINK);
      caption('aim: the pink square, 3 m ahead');
      await wait(2);
      caption('enter: "a cube"');
      const first = await build();
      const past = Math.hypot(first.x - near.x, first.z - near.z);
      caption(past <= 1 ? 'the cube lands on the pink square' : `the cube lands ${metres(past)} past the pink square`);
      await wait(3);

      // 3. Take it away, look at the sky, ask again.
      caption('remove, look at the sky, "a cube"');
      await remove(first);
      await look(20, 0.7);
      await wait(0.6);
      const second = await build();
      caption(second.x === first.x && second.z === first.z ? 'same spot: the aim is not used' : `this time ${metres(Math.hypot(second.x - first.x, second.z - first.z))} from the last one`);
      await wait(3);
      await remove(second);
      paint(near, 0);

      // 4. Mid-floor, facing the elevator: the car fills the frame.
      camera.shot({ kind: 'still', at: [-4.6, 3.4, -0.6], look: [0.2, 1.1, 5.4] });
      caption('now facing the elevator');
      await look(-30, 0.3);
      await jeffrey.walkTo(0, 0, { pace: 0.45 });
      await jeffrey.face(0, 0.6);
      const floorAim = aimed();
      if (floorAim) paint(floorAim, PINK);
      await wait(1);
      caption('enter: "a cube"');
      const third = await build();
      caption(`the cube lands at the car, ${metres(Math.hypot(third.x, third.z - CAR_Z))} from its middle`);
      await wait(1.6);
      let home = true;
      try { await jeffrey.walkTo(0, CAR_Z, { pace: 0.45, patience: 3.5, turnFirst: 0 }); } catch { home = false; }
      caption(home && inCar(jeffrey.me) ? 'the car is still open' : `the cube stands in the car door: no way back in`);
      await wait(3);

      caption('');
      await card(past <= 1 ? 'the build follows the aim' : 'the build ignores the aim',
        past <= 1 ? 'it lands on the highlighted square' : '6 m ahead of your body, then the nearest gap', 2.6);
      await fade(1, 0.5);
      await wait(0.2);
    } finally {
      aesel.close();
    }
  },
};
