// 🏢 "AC Tower": @jeffrey crosses the plaza under the tower, steps into the
// glass elevator and rides it up the outside of the building to his floor.
// The cast is @jeffrey and a tower of invented neighbours: a film's floors are
// a stage set, not the real directory, so nobody real is placed on one.
const HANDLES = ['jeffrey'];
for (let i = 1; i < 3076; i++) HANDLES.push(`neighbor${i.toString(36)}`);

export default {
  name: 'tower',
  size: [1280, 720],
  fps: 60,
  handles: HANDLES,
  // Before the camera: 89 is jeffrey's, and his floor has paint on it.
  async set({ tower, store }) {
    const floor = await tower.change((t) => { t.arrive('jeffrey', 'film-jeffrey'); return t.take('jeffrey', 89); });
    const room = `floor-${floor.id}`;
    const pink = [[3, 3], [4, 3], [5, 3], [3, 4], [5, 4], [3, 5], [4, 5], [5, 5], [12, 12], [13, 12], [12, 13], [13, 13], [4, 13], [13, 4]];
    for (const [i, j] of pink) await store.paintTile(room, i, j, 1 + ((i + j) % 6));
  },
  async direct({ cast, camera, card, caption, wait, roll, fade, ride }) {
    const jeffrey = await cast('jeffrey', { at: [-1.2, -7.2], facing: 10 });
    await camera.sees('jeffrey');
    // 1. From the fields outside the plaza, looking up: the tower goes out of sight.
    camera.shot({ kind: 'move', from: [-34, 2.2, -44], to: [-27, 3.5, -36], look: [0, 62, 0], seconds: 4, ease: 'out', within: false });
    fade(1, 0);
    await wait(0.4);
    await roll();
    camera.open(0.6);
    await card('AC Tower', 'every handle has a floor', 2.8);
    await wait(0.6);
    // 2. Down to him, and behind him as he walks to the car.
    camera.shot({ kind: 'follow', who: 'jeffrey', back: 3.2, up: 0.9, side: 0.9 });
    caption('across the courtyard');
    await wait(1.3);
    await jeffrey.walkTo(0, -4.4, { pace: 0.4 });
    // 2a. The courtyard and the entrance, from beside a planter.
    camera.shot({ kind: 'move', from: [6.6, 2.4, -6.6], to: [5.2, 2.0, -5.6], look: [-0.6, 2.6, 1.2], seconds: 4, ease: 'out' });
    await jeffrey.walkTo(0, -1.2, { pace: 0.34, turnFirst: 0 });
    // 2b. The entrance: from inside the lobby, he comes in through the door.
    camera.shot({ kind: 'move', from: [4.6, 1.7, 3.4], to: [3.6, 1.7, 5.2], look: 'jeffrey', seconds: 4.5, ease: 'inOut' });
    caption('through the lobby');
    await jeffrey.walkTo(0, 3, { pace: 0.36, turnFirst: 0.3 });
    // 2c. The board by the desk.
    camera.shot({ kind: 'move', from: [-2.6, 1.7, 2.6], to: [-3.4, 1.8, 3.2], look: [-4.6, 1.9, 6.1], seconds: 2.4, ease: 'out' });
    await wait(2.2);
    camera.shot({ kind: 'follow', who: 'jeffrey', back: 3, up: 0.9, side: 0.8 });
    await jeffrey.walkTo(0, 6.2, { pace: 0.34, turnFirst: 0 });
    // 3. In the car with him: he turns to the glass.
    camera.shot({ kind: 'still', at: [-1.05, 1.9, 5.0], look: [0.6, 1.2, 7.4], rides: true, smooth: 3 }, { cut: false });
    await jeffrey.face(0, 0.7);
    caption('floor 89, please');
    await wait(1.2);
    // 4. Up the outside of the building: the ground falls away.
    caption('');
    const going = ride(jeffrey, 'home');
    // The car's screen over the door counts the floors and names them.
    await wait(0.5);
    camera.shot({ kind: 'still', at: [1.15, 1.45, 7.25], look: [-0.25, 2.55, 4.6], rides: true });
    await wait(3.4);
    camera.shot({ kind: 'still', at: [1.0, 2.15, 4.9], look: [-0.5, -0.4, 9.5], rides: true, smooth: 1.6 }, { cut: false });
    await going;
    // 5. His floor. He turns from the view and walks in.
    await wait(0.9);
    camera.shot({ kind: 'move', from: [3.4, 1.9, 2.2], to: [4.6, 2.2, -1.5], look: 'jeffrey', seconds: 5, ease: 'inOut' });
    caption('home: floor 89 of 3,076');
    await jeffrey.walkTo(0, 1, { pace: 0.36 });
    await jeffrey.walkTo(-4.5, -5.6, { pace: 0.34 });
    // 6. At the south glass, looking out; the camera goes over his shoulder.
    camera.shot({ kind: 'follow', who: 'jeffrey', back: 2.6, up: 0.8, side: -0.9, smooth: 2 }, { cut: false });
    await jeffrey.face(200, 0.9);
    await wait(2.4);
    caption('');
    await card('aesthetic.computer/rax', 'take the elevator', 2.4);
    await fade(1, 0.5);
    await wait(0.2);
  },
};
