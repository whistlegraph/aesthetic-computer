// 🦾 rax without eyes or a mouse: one surface for a screen reader, a keyboard
// and an agent driving the page (2026-11-27). The world is a canvas, which
// says nothing to any of them, so beside it stands:
//   - a text mirror (#rax-mirror): where you are, who and what is near,
//     what was last said — kept current, announced politely when it changes;
//   - a button for each thing in reach (walk to it, go through that side);
//   - window.rax: the same, as calls — state(), walkTo(), look(), move(),
//     use(), drop(), jump(), say(), stop(), and rax.help.
// Everything here reads the client's own state and writes the client's own
// inputs (a held stick, a yaw, a queued press): the server sees a player.
const deg = (rad) => (rad * 180) / Math.PI;
const round = (n, to = 10) => Math.round(n * to) / to;
const COMPASS = ['north', 'north-east', 'east', 'south-east', 'south', 'south-west', 'west', 'north-west'];
// Forward at yaw 0 is +z, which is north; +yaw turns toward +x, east (geography.mjs).
const bearingTo = (from, to) => deg(Math.atan2(to.x - from.x, to.z - from.z));
const compass = (bearing) => COMPASS[((Math.round(bearing / 45) % 8) + 8) % 8];
const turn = (a, b) => ((((b - a) % 360) + 540) % 360) - 180; // (shortest way from a to b, in degrees)
const clock = (rel) => {
  const a = Math.abs(rel);
  return a < 25 ? 'ahead' : a > 155 ? 'behind' : `${a < 65 ? 'ahead to the' : a > 115 ? 'behind to the' : 'to the'} ${rel > 0 ? 'right' : 'left'}`;
};
const EDGE = { north: [0, 1], south: [0, -1], east: [1, 0], west: [-1, 0] };

export function createAgentSurface({ canvas, read, act }) {
  let goal = null; // { x, z, within, until, done, last, stuckAt }
  let held = null; // { fwd, right, until }
  const finish = (result) => {
    const g = goal;
    goal = null;
    g?.done(result);
  };

  // What is here, measured from the body (or the drone's eye).
  function state() {
    const w = read();
    const me = w.player || w.eye || null;
    const near = (thing, kind, name, extra = {}) => {
      if (!me || !Number.isFinite(thing.x) || !Number.isFinite(thing.z)) return null;
      const bearing = bearingTo(me, thing);
      return {
        kind, name, id: thing.id ?? null,
        x: round(thing.x), z: round(thing.z),
        distance: round(Math.hypot(thing.x - me.x, thing.z - me.z)),
        compass: compass(bearing), where: clock(turn(w.yaw, bearing)),
        ...extra,
      };
    };
    const things = [];
    for (const p of w.people) things.push(near(p, 'person', p.name, { handle: p.handle || null }));
    for (const o of w.objects) things.push(near(o, 'thing', o.name));
    for (const s of w.screens) things.push(near(s, 'screen', s.name));
    const half = w.half || 0;
    for (const side of w.edges || []) {
      const [ex, ez] = EDGE[side] || [];
      if (ex === undefined || !me) continue;
      // The nearest point of that side: straight across from where you stand.
      const at = { x: ex ? ex * half : me.x, z: ez ? ez * half : me.z };
      things.push(near(at, 'way', `${side} side${w.neighbours?.[side] ? `, to ${w.neighbours[side]}` : ''}`, { side, to: w.neighbours?.[side] || null }));
    }
    const nearby = things.filter(Boolean).sort((a, b) => a.distance - b.distance);
    return {
      room: w.room, place: w.place, cube: w.cube || null, connected: w.connected, // (cube: { level, x, y, z } of the lattice, lattice.mjs, when the room is one)
      you: {
        name: w.name, handle: w.handle, signedIn: !!w.handle, seated: w.seat >= 0,
        x: me ? round(me.x) : null, y: me ? round(me.y) : null, z: me ? round(me.z) : null,
        yaw: round(w.yaw), pitch: round(w.pitch), facing: compass(w.yaw),
        health: w.player?.health ?? null, holding: w.holding || null,
      },
      nearby,
      said: w.said,
      notice: w.notice || null,
      walking: goal ? { x: goal.x, z: goal.z } : null,
    };
  }

  const sentence = (t) => `${t.name}, ${t.distance < 1.5 ? 'right here' : `${Math.round(t.distance)} metres ${t.where}`}`;
  function describe(s = state()) {
    const lines = [];
    lines.push(`${s.place}. ${s.connected ? '' : 'Not connected. '}You are ${s.you.name || 'a guest'}${s.you.seated ? '' : ', looking on'}, facing ${s.you.facing}.`);
    if (s.notice) lines.push(s.notice);
    const by = (kind) => s.nearby.filter((t) => t.kind === kind);
    const list = (label, items, most = 6) => items.length && lines.push(`${label}: ${items.slice(0, most).map(sentence).join('; ')}${items.length > most ? `; and ${items.length - most} more` : ''}.`);
    list('People', by('person'));
    list('Screens', by('screen'));
    list('Things', by('thing'));
    list('Ways out', by('way'));
    if (s.said.length) lines.push(`Last said: ${s.said.slice(-3).map((c) => `${c.name}: ${c.text}`).join(' / ')}`);
    return lines.join('\n');
  }

  // 🚶 Walking somewhere is turning to it and holding forward until there.
  function walkTo(target, { within = 1.5, seconds = 30 } = {}) {
    const s = state();
    let at = null;
    if (typeof target === 'string') {
      const wanted = target.toLowerCase();
      at = s.nearby.find((t) => t.id === target || t.side === wanted || t.handle === wanted || t.name?.toLowerCase() === wanted)
        || s.nearby.find((t) => t.name?.toLowerCase().includes(wanted));
      if (!at) return Promise.resolve({ ok: false, error: `nothing here called "${target}"`, nearby: s.nearby.map((t) => t.name) });
      if (at.kind === 'way') within = 0; // (a way out is walked through: the room changes under you)
    } else if (target && Number.isFinite(target.x) && Number.isFinite(target.z)) at = target;
    if (!at) return Promise.resolve({ ok: false, error: 'walkTo takes a name, an id, a side, or { x, z }' });
    if (s.you.x === null) return Promise.resolve({ ok: false, error: 'not in the room yet' });
    finish({ ok: false, error: 'replaced by another walk' });
    held = null;
    act.look({}); // (an input: a drone on its idle tour comes back to hand)
    return new Promise((done) => {
      goal = { x: at.x, z: at.z, side: at.side || null, room: s.room, within, until: performance.now() + seconds * 1000, done, last: null, stuckAt: performance.now() };
    });
  }

  // Called by the client each tick, before it builds the command: the stick
  // this surface holds, and the yaw it wants (or undefined to leave it).
  function drive(player, now, room) {
    if (goal) {
      if (!player) return { fwd: 0, right: 0 };
      if (room !== goal.room) { finish({ ok: true, arrived: true, room }); return { fwd: 0, right: 0 }; }
      const far = Math.hypot(goal.x - player.x, goal.z - player.z);
      if (far <= goal.within) { finish({ ok: true, arrived: true, distance: round(far) }); return { fwd: 0, right: 0 }; }
      if (now > goal.until) { finish({ ok: false, error: 'ran out of time', distance: round(far) }); return { fwd: 0, right: 0 }; }
      // Not moving for a second and a half: something is in the way.
      if (!goal.last || Math.hypot(player.x - goal.last.x, player.z - goal.last.z) > 0.4) { goal.last = { x: player.x, z: player.z }; goal.stuckAt = now; }
      else if (now - goal.stuckAt > 1500) { finish({ ok: false, error: 'blocked', distance: round(far) }); return { fwd: 0, right: 0 }; }
      // A way out is walked straight at its side, past the edge.
      const to = goal.side ? { x: player.x + EDGE[goal.side][0] * 4, z: player.z + EDGE[goal.side][1] * 4 } : goal;
      return { fwd: 1, right: 0, yaw: bearingTo(player, to) };
    }
    if (held) {
      if (now > held.until) held = null;
      else return { fwd: held.fwd, right: held.right };
    }
    return null;
  }

  const api = {
    help: 'rax.state() → where you are and what is near. rax.describe() → the same in words. rax.walkTo(name | id | side | {x,z}) → Promise once there. rax.look({ yaw, pitch } | name) turns. rax.move({ fwd, right, seconds }) holds the stick. rax.use(), rax.drop(), rax.jump(). rax.say(text). rax.elevator(floor | handle). rax.spectate() and rax.takeBody(). rax.stop(). Distances are metres; yaw is degrees, 0 = north (+z), 90 = east (+x).',
    state,
    describe: () => describe(),
    walkTo,
    look(at = {}) {
      if (typeof at === 'string') {
        const s = state(), wanted = at.toLowerCase();
        const t = s.nearby.find((n) => n.id === at || n.name?.toLowerCase().includes(wanted));
        if (!t || s.you.x === null) return { ok: false, error: `nothing here called "${at}"` };
        at = { yaw: bearingTo(s.you, t) };
      }
      act.look(at);
      return { ok: true, ...at };
    },
    move({ fwd = 0, right = 0, seconds = 1 } = {}) {
      finish({ ok: false, error: 'replaced by a move' });
      act.look({});
      const until = performance.now() + Math.min(Math.max(seconds, 0), 20) * 1000;
      held = { fwd: Math.max(-1, Math.min(1, fwd)), right: Math.max(-1, Math.min(1, right)), until };
      return new Promise((done) => setTimeout(() => done({ ok: true, you: state().you }), until - performance.now() + 60));
    },
    stop() { finish({ ok: false, error: 'stopped' }); held = null; return { ok: true }; },
    use: () => (act.press('use'), { ok: true }),
    drop: () => (act.press('drop'), { ok: true }),
    jump: () => (act.press('jump'), { ok: true }),
    say(text) {
      text = String(text ?? '').trim();
      if (!text) return { ok: false, error: 'nothing to say' };
      act.say(text);
      return { ok: true };
    },
    elevator: (to) => (act.elevator(String(to)), { ok: true }),
    signIn: () => (act.signIn(), { ok: true }),
    // 🚁 Step back to look on as a drone, and take the body again (signed in).
    spectate: () => (act.drone() ? { ok: true } : { ok: false, error: 'only a signed-in body can step back to a drone' }),
    takeBody: () => (act.body() ? { ok: true } : { ok: false, error: 'there is no body to take' }),
  };

  // 🪞 The mirror: text and buttons beside the canvas. Off screen, not hidden:
  // a screen reader and an agent's DOM read both find it; keyboard focus
  // brings it into view (it is not a trap for a sighted keyboard user).
  const mirror = document.createElement('section');
  mirror.id = 'rax-mirror';
  mirror.setAttribute('aria-label', 'Rax, in words');
  const style = document.createElement('style');
  style.textContent = `#rax-mirror{position:absolute;left:0;top:0;width:1px;height:1px;overflow:hidden;clip-path:inset(50%);white-space:pre-wrap}
#rax-mirror:focus-within{width:min(34rem,92vw);height:auto;max-height:80vh;overflow:auto;clip-path:none;z-index:50;padding:12px 14px;background:#111;color:#fff;font:15px/1.45 system-ui,sans-serif;border:2px solid #fff}
#rax-mirror button,#rax-mirror input{font:inherit;margin:3px 4px 3px 0;padding:6px 10px;min-height:32px;background:#fff;color:#111;border:2px solid #111;border-radius:4px}
#rax-mirror button:focus-visible,#rax-mirror input:focus-visible{outline:3px solid #ffd23f;outline-offset:2px}`;
  const heading = Object.assign(document.createElement('h1'), { textContent: 'Rax' });
  const words = Object.assign(document.createElement('p'), { id: 'rax-words' });
  const live = document.createElement('p'); // (only what changed: a place, a notice, a line of chat)
  live.id = 'rax-live';
  live.setAttribute('role', 'status');
  live.setAttribute('aria-live', 'polite');
  const reach = Object.assign(document.createElement('div'), { id: 'rax-reach' });
  reach.setAttribute('role', 'group');
  reach.setAttribute('aria-label', 'In reach');
  const actions = document.createElement('div');
  actions.setAttribute('role', 'group');
  actions.setAttribute('aria-label', 'Actions');
  const button = (label, press, parent = actions) => {
    const b = Object.assign(document.createElement('button'), { type: 'button', textContent: label });
    b.addEventListener('click', press);
    parent.append(b);
    return b;
  };
  const told = (said) => { live.textContent = ''; live.textContent = said; };
  button('Use', () => api.use());
  button('Drop', () => api.drop());
  button('Jump', () => api.jump());
  button('Stop walking', () => api.stop());
  button('Turn left', () => api.look({ yaw: read().yaw - 45 }));
  button('Turn right', () => api.look({ yaw: read().yaw + 45 }));
  button('Step forward', () => api.move({ fwd: 1, seconds: 1 }));
  const signIn = button('Sign in', () => api.signIn());
  const form = document.createElement('form');
  const line = Object.assign(document.createElement('input'), { type: 'text', id: 'rax-say', maxLength: 140, autocomplete: 'off' });
  line.setAttribute('aria-label', 'Say something to the room');
  form.append(line, Object.assign(document.createElement('button'), { type: 'submit', textContent: 'Say' }));
  form.addEventListener('submit', (e) => {
    e.preventDefault();
    if (api.say(line.value).ok) line.value = '';
  });
  // (keys typed here are for these controls, not the game)
  for (const type of ['keydown', 'keyup', 'keypress']) mirror.addEventListener(type, (e) => e.stopPropagation());
  mirror.append(style, heading, words, live, reach, actions, form);
  canvas.after(mirror);

  let shown = '', reachKey = '', lastPlace = '', lastNotice = '', lastSaid = 0;
  function refresh() {
    let s;
    try { s = state(); } catch { return; }
    const text = describe(s);
    if (text !== shown) words.textContent = shown = text;
    signIn.hidden = s.you.signedIn;
    // The buttons are rebuilt only when what is in reach changes, so focus holds.
    // (every way out, and the ten nearest things within forty metres)
    const inReach = [...s.nearby.filter((t) => t.kind !== 'way' && t.distance <= 40).slice(0, 10), ...s.nearby.filter((t) => t.kind === 'way')];
    const key = inReach.map((t) => `${t.kind}:${t.id ?? t.name}`).join('|');
    if (key !== reachKey) {
      reachKey = key;
      const focused = document.activeElement?.parentElement === reach ? document.activeElement.dataset.key : null;
      reach.replaceChildren();
      for (const t of inReach) {
        const b = button(`${t.kind === 'way' ? 'Go through the' : 'Walk to'} ${t.name}`, () =>
          walkTo(t.kind === 'way' ? t.side : t.id ?? { x: t.x, z: t.z }).then((r) => told(r.ok ? `Arrived: ${t.name}.` : `Did not get to ${t.name}: ${r.error}.`)), reach);
        b.dataset.key = `${t.kind}:${t.id ?? t.name}`;
        if (b.dataset.key === focused) b.focus();
      }
    }
    if (s.place !== lastPlace) { lastPlace = s.place; told(`${s.place}.`); }
    else if (s.notice && s.notice !== lastNotice) told(s.notice);
    else if (s.said.length && s.said.at(-1).at !== lastSaid) told(`${s.said.at(-1).name}: ${s.said.at(-1).text}`);
    lastNotice = s.notice || '';
    lastSaid = s.said.at(-1)?.at ?? lastSaid;
  }
  setInterval(refresh, 500);
  try { window.rax = api; } catch {}
  return { drive, api };
}
