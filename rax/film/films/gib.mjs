// 🩸 "gib": a gore test reel in the east room (armory.mjs: a rack in the
// middle). @shooter takes a pistol off the rack and ends four bodies, one
// each way gore.mjs takes a body apart: a head shot (the head comes off), an
// arm shot (the arm comes off), a body shot (a ragdoll thrown back), and a
// rocket (all apart). Then the camera looks over what is left on the floor.
export default {
  name: 'gib',
  size: [1280, 720],
  fps: 60,
  handles: ['shooter', 'headshot', 'armshot', 'bodyshot', 'rocketed'],
  async direct({ cast, camera, card, caption, wait, roll, fade }) {
    await camera.cut('east');
    const shooter = await cast('shooter', { room: 'east', at: [0, -3.4], facing: 0 });
    const victims = {
      headshot: await cast('headshot', { room: 'east', at: [0, 3], facing: 180 }),
      armshot: await cast('armshot', { room: 'east', at: [-2.2, 3], facing: 180 }),
      bodyshot: await cast('bodyshot', { room: 'east', at: [2.2, 3], facing: 180 }),
      rocketed: await cast('rocketed', { room: 'east', at: [0, 5.6], facing: 180 }),
    };
    await camera.sees('rocketed');
    // From the side, the line of them and the shooter.
    camera.shot({ kind: 'still', at: [6.4, 2.1, 0.4], look: [0, 1.1, 3] });
    fade(1, 0);
    await wait(0.4);
    await roll();
    camera.open(0.5);
    await card('gib', 'ragdolls, gibs, spurts and splats', 2.2);
    // He takes a pistol off the rack (walking over it picks it up).
    camera.shot({ kind: 'follow', who: 'shooter', back: 2.6, up: 0.8, side: 0.7 });
    await shooter.walkTo(0, -2, { pace: 0.35 });
    await wait(0.6);
    // Over his shoulder, then each in turn.
    camera.shot({ kind: 'still', at: [0.9, 2.0, -4.6], look: [0, 1.2, 3] });
    caption('a head shot');
    await shooter.aimAt([0, 1.62, 3]);
    await wait(0.4);
    shooter.fire();
    await wait(1.4);
    victims.headshot.leave(); // (gone before they stand up again: only what fell stays)
    await wait(1);
    camera.shot({ kind: 'still', at: [-5.6, 1.6, 1.0], look: [-2.2, 1.0, 3] });
    caption('an arm');
    await shooter.aimAt([-2.2 + 0.3, 1.05, 3]);
    await wait(0.6);
    shooter.fire();
    await wait(1.4);
    victims.armshot.leave();
    await wait(1);
    camera.shot({ kind: 'still', at: [5.6, 1.4, 1.0], look: [2.2, 0.9, 3.6] });
    caption('the body');
    await shooter.aimAt([2.2, 1.0, 3]);
    await wait(0.6);
    shooter.fire();
    await wait(1.4);
    victims.bodyshot.leave();
    await wait(1.2);
    camera.shot({ kind: 'still', at: [3.8, 2.6, 2.2], look: [0, 0.9, 5.6] });
    caption('a rocket');
    await shooter.aimAt([0, 0.9, 5.6]);
    await wait(0.6);
    shooter.rocket();
    await wait(1.4);
    victims.rocketed.leave();
    await wait(1.6);
    // What is left: down low over the floor.
    caption('');
    camera.shot({ kind: 'move', from: [-3.2, 0.9, 1.2], to: [3.2, 1.3, 1.6], look: [0, 0.1, 4], seconds: 5, ease: 'inOut' });
    await wait(5);
    await fade(1, 0.6);
  },
};
