// 🪄 Spell "take 1111": on his own floor, 11, @jeffrey types `take 1111` on
// the car's line. The number becomes his (the neighbour pencilled in on 1111
// has never come, so they trade), but the car's screen goes on naming that
// neighbour. The cast is @jeffrey and invented neighbours only.
//
// The camera page is a real rax page, so its car screen is the real one. The
// typed line and the car's one-line answer live on the typist's page alone
// (an actor is a socket, not a page), so captions say them.
import { floorRoom } from '../../tower.mjs';
import { altitude } from '../../elevator.mjs';

const HANDLES = [];
for (let i = 1; i <= 1200; i++) HANDLES.push(i === 11 ? 'jeffrey' : `neighbor${i.toString(36)}`);
const ABSENT = HANDLES[1110]; // pencilled in on 1111; never arrives
let home = null; // @jeffrey's floor, known once the stage is set

// The car's screen over its door, square on, filling the frame.
const SCREEN = { kind: 'still', at: [0, 2.2, 6.75], look: [0, 2.6, 4.6], zoom: 1.25, rides: true };
// Out through the car's back glass, down at the world.
const GLASS = { kind: 'still', at: [0.9, 2.1, 5.2], look: [-0.6, -0.6, 9.5], rides: true };

export default {
  name: 'spell-take-1111',
  size: [1280, 720],
  fps: 60,
  offline: true, // invented handles: no colour lookups
  handles: HANDLES,
  async set({ tower }) {
    home = floorRoom(tower.tower.forHandle('jeffrey').id);
  },
  async direct({ cast, camera, card, caption, wait, roll, fade, ride }) {
    // He signs in on his own floor and stands in the car, beside the door.
    const jeffrey = await cast('jeffrey', { room: home, at: [-0.95, 6.4], facing: 180 });
    await camera.cut(home);
    await camera.sees('jeffrey');
    camera.shot(SCREEN);
    fade(1, 0);
    await wait(0.4);
    await roll();
    // The title sits on black, so the car's screen does not show through it.
    await card('take 1111', 'typed in the elevator car', 2.6);
    camera.open(0.5);
    // 1. Before: the screen names his floor, by founding order.
    caption('@jeffrey is home: floor 11');
    await wait(3);
    camera.shot(GLASS);
    caption(`floor 11 is ${altitude(11)} m up`);
    await wait(2.2);
    // 2. The spell.
    camera.shot(SCREEN);
    caption('he types: take 1111');
    await wait(2);
    const said = await jeffrey.take(1111);
    caption(`the car answers: ${said.note}`);
    await wait(3);
    // 3. The finding: the same screen, on his own floor, names someone else.
    caption(`but the screen now reads 1111 @${ABSENT}`);
    await wait(4);
    // 4. No ride happened: the floor was renumbered where it stands.
    camera.shot(GLASS);
    caption(`no ride: the floor is suddenly ${altitude(1111)} m up`);
    await wait(3);
    // 5. The number really is his: the ride down starts at 1111.
    camera.shot(SCREEN);
    caption('it is his: the ride to the plaza starts at 1111');
    await ride(jeffrey, 'g');
    await wait(0.8);
    caption('');
    await fade(1, 0.5);
    await card('take 1111 worked', 'the screen kept the old name', 2.4);
    await wait(0.2);
  },
};
