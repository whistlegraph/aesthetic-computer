// 🛗 Spell "home": in the car, Enter, `home` (or your own number), Enter.
//
// What the film is for: a visitor who stops one step inside the car and
// presses Enter is turned to face the door (yaw 180, pitch 24) with the car's
// screen above the top of the frame, and that screen is the only place the
// typed line is drawn. From the middle of the car the same turn shows it, and
// `home` rides to floor 11.
//
// The cast is @jeffrey and ten invented neighbours, so his floor is 11.
const HANDLES = [];
for (let i = 1; i <= 10; i++) HANDLES.push(`neighbor${i.toString(36)}`);
HANDLES.push('jeffrey');

// Where Enter turns a rider (client.mjs: yaw = 180, pitch = 24), as a point
// for the camera to look at from an eye at [x, 1.7, z].
const EYE = 1.7, RISE = Math.tan((24 * Math.PI) / 180) * 10;
const enterView = (x, z) => [x, EYE + RISE, z - 10];
// The typed line on the car's screen (view.mjs: the screen hangs at z 4.72).
const SCREEN = [0, 2.7, 4.73];
// The camera stands at the rider's depth, a step to his side, so his own
// body is not in the lens: what matters is how far in from the door he is.
const CAM_X = -0.35, HIS_X = 0.55;
const STEP_IN = 4.95, MIDDLE = 6.1;

export default {
  name: 'spell-ride-home',
  size: [1280, 720],
  fps: 60,
  handles: HANDLES,
  async set({ tower }) {
    await tower.change((t) => t.arrive('jeffrey', 'film-jeffrey'));
  },
  async direct({ cast, camera, card, caption, wait, roll, fade, ride }) {
    const jeffrey = await cast('jeffrey', { at: [HIS_X, 2.4], facing: 0 });
    await camera.sees('jeffrey');
    // 0. The car from the lobby, him walking up to it.
    camera.shot({ kind: 'still', at: [2.9, 1.8, 2.0], look: [0, 1.7, 5.6] });
    fade(1, 0);
    await wait(0.4);
    await roll();
    camera.open(0.6);
    await card('spell: home', 'in the car: enter, home, enter', 2.6);
    // 1. One step outside the door the car hears nothing and says nothing.
    caption('outside the door: "home" gets no answer');
    await jeffrey.walkTo(HIS_X, 4.35, { pace: 0.34, within: 0.05 });
    jeffrey.send({ type: 'elevator', to: 'home' });
    await wait(2.2);
    // 2. One step in. Enter turns him to the door; this is his view.
    caption('one step in: enter');
    await jeffrey.walkTo(HIS_X, STEP_IN, { pace: 0.3, within: 0.04, turnFirst: 0 });
    await jeffrey.face(180, 0.5);
    camera.shot({ kind: 'still', at: [CAM_X, EYE, STEP_IN], look: enterView(CAM_X, STEP_IN) });
    caption('his view after enter: no screen, no typed line');
    await wait(3.4);
    // 2a. The screen is there: straight overhead, out of the frame he was given.
    // (A straight tilt up: the same bearing, the screen's own height.)
    camera.shot({ kind: 'still', at: [CAM_X, EYE, STEP_IN], look: [CAM_X, SCREEN[1], SCREEN[2]], smooth: 2.2 }, { cut: false });
    caption('the screen he types on is overhead');
    await wait(2.8);
    // 3. The middle of the car: the same turn, and the screen is in view.
    await jeffrey.walkTo(HIS_X, MIDDLE, { pace: 0.34, within: 0.05 });
    await jeffrey.face(180, 0.4);
    camera.shot({ kind: 'still', at: [CAM_X, EYE, MIDDLE], look: enterView(CAM_X, MIDDLE) });
    caption('from the middle the same enter shows it');
    await wait(3);
    // 4. home, Enter: the screen counts the floors up to 11.
    // (The ride stands him on the car's centre line, so the camera steps
    // ahead of him to keep his head out of the lens.)
    camera.shot({ kind: 'still', at: [CAM_X, EYE, MIDDLE - 0.45], look: SCREEN, rides: true, smooth: 2 }, { cut: false });
    caption('"home" + enter: up to floor 11');
    await ride(jeffrey, 'home');
    await wait(0.8);
    // 5. His floor.
    camera.shot({ kind: 'move', from: [3.4, 1.9, 2.4], to: [4.2, 2.1, 0.4], look: 'jeffrey', seconds: 3, ease: 'inOut' });
    caption('floor 11: home');
    await jeffrey.walkTo(0, 2.6, { pace: 0.36 });
    await wait(1.4);
    caption('');
    await fade(1, 0.5);
    await wait(0.2);
  },
};
