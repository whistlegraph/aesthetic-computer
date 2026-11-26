// 🧟 The horde: zombies that belong to no room. The world's clock walks them,
// like the ball (ball.mjs): a room only lends what stands there (walls,
// builds). At a block's line one goes on into the next block, so over time
// they wander the whole lap; where no block is open (or one is kept on
// another host) it turns back. A round that meets one (match.mjs
// stepBullets) ends it with a 'gib', and a new one is let loose a moment
// later where people gather.
import { DT, bodyBox } from './world.mjs';
import { SPAN } from './park.mjs';

export const ZOMBIE = { r: 0.4, speed: 1.5, eye: 1.7, count: 8, respawn: 3 };
const SIDES = [['east', 'x', 1], ['west', 'x', -1], ['north', 'z', 1], ['south', 'z', -1]];
const DEG = Math.PI / 180;
const respawnTicks = Math.round(ZOMBIE.respawn / DT);
const stuckTicks = Math.round(4 / DT);
const retryTicks = Math.round(1 / DT);

export function createHorde({ neighbours, boxes = () => [], start = ['lobby'], random = Math.random, count = ZOMBIE.count } = {}) {
  const zombies = [];
  let nextId = 1;
  const later = []; // ticks until each missing zombie is let loose again

  // (What stands in a room, asked once a step for each: not once a walker.)
  let held = new Map();
  const standing = (room) => {
    if (!held.has(room)) {
      let list = [];
      try { list = boxes(room) || []; } catch { /* an object not yet built: walk through it a moment */ }
      held.set(room, list);
    }
    return held.get(room);
  };
  const blocked = (room, x, z) => {
    for (const b of standing(room)) {
      if (!(b.yMax > 0.2 && b.yMin < 1.6)) continue;
      if (x > b.xMin - ZOMBIE.r && x < b.xMax + ZOMBIE.r && z > b.zMin - ZOMBIE.r && z < b.zMax + ZOMBIE.r) return true;
    }
    return false;
  };
  const retarget = (z) => { z.turn = 30 + Math.floor(random() * 120); };

  function spawn() {
    if (!start.length) return null;
    const first = Math.floor(random() * start.length);
    for (let k = 0; k < 20; k++) {
      const room = start[(first + k) % start.length];
      const x = (random() - 0.5) * (SPAN - 4), zz = (random() - 0.5) * (SPAN - 4);
      if (blocked(room, x, zz)) continue;
      const z = { id: nextId++, room, x, z: zz, yaw: random() * 360, vx: 0, vz: 0, turn: 0 };
      z.anchorX = x; z.anchorZ = zz; z.still = 0;
      retarget(z);
      zombies.push(z);
      return z;
    }
    return null;
  }
  for (let n = 0; n < count; n++) if (!spawn()) later.push(retryTicks);

  return {
    zombies,
    spawn,
    // Zombies standing in a room, as match.mjs meets them: the eye height
    // like a person's, so the same body box and hit zones serve.
    inRoom(room) {
      return zombies.filter((z) => z.room === room).map((z) => {
        const at = { x: z.x, y: ZOMBIE.eye, z: z.z, yaw: z.yaw };
        return { id: z.id, ...at, box: bodyBox(at) };
      });
    },
    kill(id) {
      const i = zombies.findIndex((z) => z.id === id);
      if (i < 0) return false;
      zombies.splice(i, 1);
      later.push(respawnTicks);
      return true;
    },
    step() {
      held = new Map();
      for (let i = later.length - 1; i >= 0; i--) {
        if (--later[i] > 0) continue;
        if (spawn()) later.splice(i, 1);
        else later[i] = retryTicks;
      }
      for (let i = zombies.length - 1; i >= 0; i--) {
        const z = zombies[i];
        // Builds can move around an existing walker. Turning in place cannot
        // get it out of a solid, and short steps in a tight pocket count too.
        if (Math.hypot(z.x - z.anchorX, z.z - z.anchorZ) >= ZOMBIE.r) {
          z.anchorX = z.x; z.anchorZ = z.z; z.still = 0;
        } else if (++z.still >= stuckTicks) {
          zombies.splice(i, 1);
          later.push(respawnTicks);
          continue;
        }
        if (--z.turn <= 0) { z.yaw = (z.yaw + (random() - 0.5) * 140 + 360) % 360; retarget(z); }
        const dx = Math.sin(z.yaw * DEG), dz = Math.cos(z.yaw * DEG), go = ZOMBIE.speed * DT;
        const nx = z.x + dx * go, nz = z.z + dz * go;
        if (blocked(z.room, nx, nz)) {
          z.yaw = (z.yaw + 100 + random() * 160) % 360; // (turn off whatever is in the way)
          z.vx = z.vz = 0;
          retarget(z);
          continue;
        }
        z.x = nx; z.z = nz; z.vx = dx * ZOMBIE.speed; z.vz = dz * ZOMBIE.speed;
        // Over a block's line: on into the next block, or back off the line.
        for (const [side, axis, sign] of SIDES) {
          if (z[axis] * sign <= SPAN / 2) continue;
          const near = neighbours(z.room)?.[side];
          const otherX = z.x - (axis === 'x' ? sign * SPAN : 0);
          const otherZ = z.z - (axis === 'z' ? sign * SPAN : 0);
          if (near && !blocked(near, otherX, otherZ)) { z.room = near; z[axis] -= sign * SPAN; }
          else {
            z[axis] = sign * SPAN / 2;
            z.yaw = axis === 'x' ? (360 - z.yaw) % 360 : (540 - z.yaw) % 360;
            retarget(z);
          }
        }
      }
    },
  };
}
