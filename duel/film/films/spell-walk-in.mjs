// 🚶 Spell "walk-in": no words. @jeffrey is set down in the courtyard and
// walks north for the car. Two builds from before the tower stand where the
// tower's entrance and car now are; both are solid. A neighbour deletes them
// (anyone may, in the plaza) and the walk goes through.
// The cast is @jeffrey and an invented neighbour; the builds are a stage set.
const box = (x, y, z, w, h, d, color) => ({ x, y, z, w, h, d, color });
const BUILDS = [
  // Across the entrance (3.2 m wide, lobby front at z 1.2): six metres ahead
  // of the first spawn, where a prompt typed on arrival puts its build.
  { id: 'old-gate', x: 0, z: 1, shape: { name: 'old gate', boxes: [box(-1.3, 2, 0, 0.6, 4, 1, 0xd9534f), box(1.3, 2, 0, 0.6, 4, 1, 0xd9534f), box(0, 2, 0, 2, 4, 0.6, 0xe0b23a)] } },
  // Where the car now stands (its middle is at z 6.1).
  { id: 'old-tree', x: 0, z: 6, shape: { name: 'old tree', boxes: [box(0, 1.2, 0, 2.4, 2.4, 2.4, 0x8a5a2b), box(0, 3, 0, 2.8, 1.2, 2.8, 0x3fa55b)] } },
  // Off the walk: in nobody's way, left standing.
  { id: 'old-statue', x: -3, z: -1, shape: { name: 'old statue', boxes: [box(0, 0.8, 0, 1.6, 1.6, 1.6, 0x8e5bd6)] } },
];

export default {
  name: 'spell-walk-in',
  size: [1280, 720],
  fps: 60,
  offline: true,
  handles: ['jeffrey', 'neighbor_ada'],
  // Before the camera: the plaza as it was, with things already built in it.
  async set({ store }) {
    for (const b of BUILDS)
      await store.commit({ room: 'lobby', objectId: b.id, owner: 'built-before-the-tower', baseVersion: -1, x: b.x, z: b.z, prompt: b.shape.name }, b.shape);
  },
  async direct({ cast, camera, card, caption, wait, roll, fade }) {
    // The first visitor is set down at (0, -5) facing north; the mark is the same.
    const jeffrey = await cast('jeffrey', { at: [0, -5], facing: 0 });
    // Beside the entrance, in frame for the shot from the side.
    const ada = await cast('neighbor_ada', { at: [-1.9, 0.2], facing: 180 });
    await camera.sees('jeffrey');
    // Push north until stopped (or `seconds` pass); says where he ended.
    const north = async (seconds) => {
      jeffrey.control.yaw = 0;
      jeffrey.control.fwd = 0.5;
      let last = jeffrey.me.z, still = 0;
      for (let t = 0; t < seconds * 10 && still < 6; t++) {
        await wait(0.1);
        still = Math.abs(jeffrey.me.z - last) < 0.01 ? still + 1 : 0;
        last = jeffrey.me.z;
      }
      jeffrey.control.fwd = 0;
      await wait(0.15);
      return jeffrey.me.z;
    };
    // What the page sends on Delete with an object locked (client.mjs).
    const remove = (id) => ada.send({ type: 'remove', objectId: id });

    // 1. The courtyard from the south: the entrance, and what stands in it.
    camera.shot({ kind: 'still', at: [0.9, 2.3, -7.4], look: [0, 1.9, 1.2] });
    fade(1, 0);
    await wait(0.4);
    await roll();
    camera.open(0.5);
    await card('walk-in', 'no words: walk north to the car', 2.6);
    // 2. Behind him as he walks north and is stopped at the entrance.
    camera.shot({ kind: 'follow', who: 'jeffrey', back: 2.8, up: 0.9, side: 0.7 });
    caption('walk north');
    await north(4);
    caption('stopped: an old build fills the entrance');
    // 2a. The entrance from beside him: the build is solid, the door is behind it.
    camera.shot({ kind: 'move', from: [3.4, 1.9, -2.6], to: [2.4, 1.8, -1.6], look: [0, 1.6, 1.0], seconds: 3, ease: 'out' });
    await north(2.4);
    await wait(1.2);
    // 3. A neighbour deletes it: anyone in the plaza may.
    caption('@neighbor_ada: delete (anyone can, here)');
    await ada.face([0, 1], 0.4);
    await wait(0.9);
    remove('old-gate');
    await wait(2);
    // 4. Through the door, and stopped again at the car.
    camera.shot({ kind: 'move', from: [3.8, 1.8, 2.4], to: [3.0, 1.8, 3.2], look: [-0.6, 1.5, 5.2], seconds: 3.5, ease: 'out' });
    caption('through the door; a build stands in the car');
    // She follows him in, beside the walk, and stands by the car.
    const follows = (async () => {
      await wait(0.8);
      await ada.walkTo(-1.0, 0.5, { pace: 0.6 });
      await ada.walkTo(-1.0, 2.3, { pace: 0.6 });
      await ada.walkTo(-2.5, 3.9, { pace: 0.6 });
    })();
    await north(4);
    await north(1.6);
    await follows;
    caption('@neighbor_ada: delete');
    await ada.face([0, 6], 0.4);
    await wait(0.9);
    remove('old-tree');
    await wait(2);
    // 5. The car is clear: he walks in.
    // The camera stays in the lobby: a follow shot ends up inside the car's wall.
    caption('nothing in the way: into the car');
    await north(3);
    await jeffrey.face(180, 0.6);
    await wait(1.4);
    caption('');
    await card('walk-in', 'old builds are solid; anyone can delete them', 2.6);
    await fade(1, 0.5);
    await wait(0.2);
  },
};
