// bot-brain.mjs — a gun-fight bot that knows where it stands and picks a goal
// from it, every tick. The test bots used to walk at each other pressing
// shoot: unarmed, they never went for the gun, and once nobody had it the
// fight stopped. 26.10.14
//
// Status (read from the relay's spectator stream): am I alive, am I armed and
// with how much, am I on the board, where is the rival and are they armed,
// where is the one gun — in a hand, lying on the hill, or in the air.
// Goals, derived from status:
//   ARM     unarmed, gun lying      run to it; jump when under it (it hangs
//                                   over the hill, out of a standing reach)
//   CHASE   gun in the air          follow where it will land
//   ATTACK  armed                   offense: hold shooting range, face the
//                                   rival, fire when level; out of rounds
//                                   at close range, throw the gun at them
//   DEFEND  rival armed, I'm not    defense: keep away, jump their line of
//                                   fire, take the gun back if it drops
//   CENTRE  nobody has it, none out wait in the middle for it to return
// A bot returns the keys to hold and the keys to tap; the caller turns those
// into native key events.

// Mac app key codes (xbox/macos-native/main.swift keyMapping): A/D walk,
// W jumps, E is the item button (shoot), Return is the A button (throw).
export const KEYS = { left: 0, right: 2, up: 13, shoot: 14, throw: 36 };

const RANGE_NEAR = 300, RANGE_FAR = 700, LEVEL = 130;

export function createBrain(me, { log = () => {} } = {}) {
  const rival = me ? 0 : 1;
  let goal = "", lastShot = 0, lastJump = 0, lastThrow = 0;
  return function think(state, now) {
    const f = state?.fighters, a = f?.[me], b = f?.[rival], gun = state?.gun;
    const hold = new Set(), tap = [];
    if (!a || !b || !a.alive) { setGoal("DOWN"); return { hold, tap, goal }; }
    const mine = (a.gunAmmo || 0) > 0, theirs = (b.gunAmmo || 0) > 0;
    const dx = b.x - a.x, dy = b.y - a.y, toward = dx > 0 ? KEYS.right : KEYS.left;
    const away = dx > 0 ? KEYS.left : KEYS.right;
    const jump = (gap = 600) => { if (now - lastJump > gap) { tap.push(KEYS.up); lastJump = now; } };
    const goTo = (x, slack = 40) => { if (x > a.x + slack) hold.add(KEYS.right); else if (x < a.x - slack) hold.add(KEYS.left); };

    if (mine) {
      setGoal("ATTACK");
      const distance = Math.abs(dx);
      if (distance > RANGE_FAR) hold.add(toward);
      else if (distance < RANGE_NEAR && (a.gunAmmo || 0) > 1) hold.add(away);
      // Facing is set by the last direction moved: when backing off, turn
      // round with a tap toward the rival before firing.
      const facingRival = Math.sign(a.facing || 0) === Math.sign(dx);
      if (!facingRival && !hold.has(toward)) { hold.clear(); hold.add(toward); }
      if (facingRival && Math.abs(dy) < LEVEL && now - lastShot > 280) { tap.push(KEYS.shoot); lastShot = now; }
      if ((a.gunAmmo || 0) === 1 && distance < RANGE_NEAR && now - lastThrow > 800) {
        tap.push(KEYS.throw); lastThrow = now;
      }
    } else if (gun?.state === "lying") {
      setGoal(theirs ? "DEFEND" : "ARM");
      goTo(gun.x, 30);
      // The gun hangs over the hill: under it and on the ground, jump.
      if (Math.abs(gun.x - a.x) < 110 && a.grounded) jump(450);
    } else if (gun?.state === "flying") {
      setGoal("CHASE");
      goTo(gun.x, 30);
    } else if (theirs) {
      setGoal("DEFEND");
      // Keep away, and jump their line of fire when they face this way.
      if (Math.abs(dx) < RANGE_FAR) hold.add(away);
      const facingMe = Math.sign(b.facing || 0) === -Math.sign(dx);
      if (facingMe && Math.abs(dy) < LEVEL && a.grounded) jump(700);
    } else {
      setGoal("CENTRE");
      if (gun) goTo(gun.x || a.x);
    }
    return { hold, tap, goal };
  };
  function setGoal(next) { if (next !== goal) { log(`seat ${me} goal ${goal || "—"} → ${next}`); goal = next; } }
}
