// 🛵 "Cabin bikes": @jeffrey gets on his bike in the courtyard and lifts off
// under the tower; two neighbours join him and they circle the atrium; the
// camera goes up until the small planet curves away, and the other planets
// stand in the sky. The neighbours are invented (as in tower.mjs): nobody
// real is put in a film. @jeffrey's bike wears his own #paintings.
import { skyOf, HOME } from '../../orbits.mjs';
import { phase } from '../../sky.mjs';

const HANDLES = ['jeffrey', 'neighbor1', 'neighbor2'];

// A point far along the nearest other planet's direction in the sky, seen from `from`.
function planetFrom(from) {
  const p = skyOf(HOME, Date.now(), phase())[0];
  const d = p.dir.y > 0.05 ? p.dir : { x: p.dir.x, y: 0.35, z: p.dir.z }; // (under the horizon now: look up that way anyway)
  return [from[0] + d.x * 300, from[1] + d.y * 300, from[2] + d.z * 300];
}

export default {
  name: 'bikes',
  size: [1280, 720],
  fps: 30,
  handles: HANDLES,
  async direct({ cast, camera, card, caption, wait, roll, fade }) {
    const jeffrey = await cast('jeffrey', { at: [-1.5, -6.2], facing: 20 });
    const one = await cast('neighbor1', { at: [-5.2, -6.4], facing: 30 });
    const two = await cast('neighbor2', { at: [4.6, -6.6], facing: 340 });
    await camera.sees('jeffrey');
    // 1. Out in the fields, looking up: the tower, and a planet in the sky.
    const field = [-30, 1.8, -38];
    camera.shot({ kind: 'move', from: field, to: [-26, 2.4, -33], look: planetFrom(field), seconds: 5, ease: 'out', within: false });
    fade(1, 0);
    await wait(0.4);
    await roll();
    camera.open(0.8);
    await card('rax', 'everyone has a cabin bike', 3.2);
    // 2. In the courtyard, behind him: B, and the bike is under him.
    camera.shot({ kind: 'follow', who: 'jeffrey', back: 4.2, up: 1.4, side: 1.2 });
    caption('press B');
    await wait(1.4);
    await jeffrey.mount();
    await wait(0.9);
    caption('space lifts it');
    await jeffrey.lift(1.6);
    // 3. Off he goes, round the atrium, as the other two get on.
    camera.shot({ kind: 'orbit', around: [0, 5, -3.5], radius: 12, height: 3.5, period: 18, phase: 0.6, within: false });
    caption('it flies where you look');
    const loop = async (who, lift, points) => { for (const [x, y, z] of points) await who.flyTo(x, y + lift, z, { pace: 0.7, within: 1.2 }); };
    const ring = [[4.5, 6, -5.5], [5, 8, 0], [-4.5, 9, -1], [-5, 7, -6.5], [0, 6, -7]];
    const flying = loop(jeffrey, 0, ring);
    await one.mount(); await two.mount();
    await Promise.all([one.lift(1.2), two.lift(1.6)]);
    caption('decorated with your #paintings');
    await Promise.all([flying, loop(one, -1.5, ring.slice(2).concat(ring.slice(0, 2))), loop(two, 1, ring.slice(4).concat(ring.slice(0, 4)))]);
    // 4. Up and away: the land wraps round its small planet, and the others hang in the sky.
    caption('');
    camera.shot({ kind: 'move', from: [0, 9, -24], to: [0, 70, -80], look: [0, 4, -2], seconds: 6, ease: 'inOut', within: false });
    const circling = Promise.all([loop(jeffrey, 0, ring), loop(one, -1.5, ring.slice(1)), loop(two, 1, ring.slice(3))]);
    await wait(2.5);
    caption('one small planet, and others going round it');
    await wait(3.5);
    const high = [0, 70, -80];
    camera.shot({ kind: 'move', from: high, to: [10, 74, -78], look: planetFrom(high), seconds: 4, ease: 'inOut', within: false });
    await wait(3.6);
    caption('');
    await circling.catch(() => {});
    await card('aesthetic.computer/rax', 'press B to ride', 2.8);
    await fade(1, 0.6);
    await wait(0.2);
  },
};
