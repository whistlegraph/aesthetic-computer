// 🧱 The builds this page has asked for, each with one honest state from the
// press to the end: asked (sent, the server's word awaited) → accepted (Aesel
// has it) → building (code is coming) → done, failed or cancelled; `refused`
// is an asking that never left the page (Aesel offline, no line). The view
// draws `rows()` as one stack at the top of the screen, wherever you look.
//
// Nothing asked for vanishes. Each asking carries a name of the page's own
// (`rid`) that the server says back (builders.mjs `rid`): a `prompt` unheard
// for ASK_MS is said once more under the same name, and again on a new socket
// once the room knows who we are; the server takes a name said twice as the
// one job. After the second silence the row reads "no answer". A build the
// server was heard to accept is followed in the room's own `objects` word, so
// a socket that dropped and came back picks it up, and one that ended while
// the line was down is read from what stands in the room.
//
// A socket that drops usually ends in the page loading itself again (client.mjs
// watchHealth), so an asking not yet answered is also written down (`kept`,
// the tab's own storage) and said again by the page that comes up after it.
export const ASK_MS = 4000; // an asking unheard this long is said once more, then given up
export const SLOW_MS = 10000; // what cannot be said twice (a $code stood again, a wall changed) waits this long: the server fetches before it answers
export const UNSENT_MS = 12000; // an asking that could not be put on the wire at all
export const LOST_MS = 200000; // an accepted build nobody has spoken of (the server's own limit is 180 s)
export const KEPT_MS = 20000; // an unanswered asking written down is said again by the next page only this soon after
const KEEP = { done: 6000, failed: 9000, refused: 6000, cancelled: 2500 }; // how long an ended row stays up
const LIVE = ['asked', 'accepted', 'building'];

// `ready(room)` checks that this connection can send to the original room.
// `kept`: { read(), write(text) } or null.
export function createBuilds({ send, ready = () => true, kept = null, now = () => performance.now(), clock = () => Date.now(), name = () => Math.random().toString(36).slice(2, 12) }) {
  let list = [], written = '';
  const live = (b) => LIVE.includes(b.state);
  const end = (b, state, note = '') => Object.assign(b, { state, note, endedAt: now() });
  const say = (b) => {
    if (!ready(b.room) || !send(b.room ? { ...b.message, in: b.room } : b.message)) return false;
    b.sentAt = now();
    b.tries++;
    return true;
  };
  // What the page before this one asked and never heard about: on the line again, said again once the room knows us.
  try {
    for (const k of JSON.parse(kept?.read() || '[]'))
      if (clock() - k.at < KEPT_MS && k.message?.rid)
        list.push({ rid: k.message.rid, text: k.text, room: k.room, message: k.message, state: 'asked', askedAt: now() - (clock() - k.at), sentAt: null, tries: 1, again: true,
          id: null, objectId: null, phase: '', parts: 0, heardAt: now(), note: '' });
  } catch {} // nothing kept, or not ours
  const keep = () => {
    if (!kept) return;
    const text = JSON.stringify(list.filter((b) => b.state === 'asked' && b.again).map((b) => ({ at: clock() - (now() - b.askedAt), text: b.text, room: b.room, message: b.message })));
    if (text !== written) kept.write((written = text));
  };
  // Whose answer is this? Its name, else the job's id. A server from before
  // names said none back: an acceptance, or a result with an id of its own,
  // is then the oldest asking still unheard (a refusal with neither is about
  // something else: a square painted, a thing removed).
  const find = (data) =>
    (data.rid && list.find((b) => b.rid === data.rid)) ||
    (data.id && list.find((b) => b.id === data.id)) ||
    (!data.rid && (data.type === 'build-accepted' || data.id) ? list.find((b) => b.state === 'asked' && b.sentAt !== null) : null) || null;
  return {
    get list() { return list; },
    // How many of this page's builds are on their way (three at once is the server's limit).
    get live() { return list.filter(live).length; },
    ask(message, text, room = null) {
      const rid = name();
      const b = { rid, text, room, message: { ...message, rid }, state: 'asked', askedAt: now(), sentAt: null, tries: 0, again: message.type === 'prompt',
        id: null, objectId: null, phase: '', parts: 0, heardAt: now(), note: '' };
      list.push(b);
      say(b);
      keep();
      return b;
    },
    // An asking that never left the page, or a refusal that names no asking of ours (a square painted where we may not).
    refuse(text, note) {
      const last = list.at(-1);
      if (last?.state === 'refused' && last.text === text && last.note === note) return Object.assign(last, { endedAt: now() }); // (pressed again: the same row, kept up)
      const b = { rid: null, text, state: 'refused', note, askedAt: now(), endedAt: now() };
      list.push(b);
      return b;
    },
    // What the server said. → the row it was about, if any.
    hear(data) {
      const b = find(data);
      if (data.type === 'build-accepted') {
        if (!b || !live(b)) return null;
        Object.assign(b, { state: b.state === 'building' ? 'building' : 'accepted', id: data.id, objectId: data.objectId || null, heardAt: now() });
        list = list.filter((x) => x === b || x.id !== b.id); // (the room spoke of it first, on a new socket: one row, ours)
        return b;
      }
      if (data.type !== 'build-result') return null;
      if (!b) return data.error && !data.again ? this.refuse('', data.error) : null; // (`again`: an old answer said twice)
      if (!live(b)) return null; // (said twice: the answer to an asking sent again)
      if (data.id) b.id = data.id;
      if (data.cancelled) return end(b, 'cancelled');
      if (data.error) return end(b, 'failed', data.error);
      Object.assign(b, { objectId: data.objectId || b.objectId, version: data.version, kidlisp: data.kidlisp || null });
      return end(b, 'done');
    },
    // The room's own account of what is being built and what stands in it.
    // `mine(job)`: is that job this player's? One we do not know (a page
    // loaded again mid-build, another tab) joins the line where it is.
    // → the rows whose work moved on a step (connecting, generating, validating…), for the ear.
    room(room, jobs, objects, mine = () => false) {
      const stepped = [];
      for (const j of jobs) {
        let b = list.find((x) => x.id === j.id);
        if (!b && mine(j)) list.push((b = { rid: null, text: j.prompt || '', room, state: 'accepted', askedAt: now(), id: j.id, objectId: j.objectId, phase: '', parts: 0, note: '' }));
        if (!b || !live(b)) continue;
        if (j.phase && j.phase !== 'connecting' && j.phase !== b.phase) stepped.push(b); // (`connecting` is the acceptance itself, heard already)
        Object.assign(b, { state: j.phase && j.phase !== 'connecting' ? 'building' : 'accepted', phase: j.phase || '', parts: j.preview?.boxes?.length || 0,
          startedAt: j.startedAt, version: j.version, x: j.x, z: j.z, room, heardAt: now() });
      }
      // Accepted here, and the room no longer speaks of it: it ended while we could not hear. What stands says how.
      for (const b of list) {
        if (!b.id || b.room !== room || !['accepted', 'building'].includes(b.state) || jobs.some((j) => j.id === b.id)) continue;
        const o = objects.find((x) => x.id === b.objectId);
        if (o && (b.version === undefined || o.version >= b.version)) end(Object.assign(b, { version: o.version }), 'done');
        else end(b, 'failed', 'it ended while the line was down, and nothing of it stands');
      }
      return stepped;
    },
    // A new socket, and the room knows who we are again: what was never answered is said again under its own name.
    rejoined() {
      for (const b of list) if (b.state === 'asked' && (b.again || b.sentAt === null)) { b.tries = Math.min(b.tries, 1); say(b); }
    },
    // Each frame. → what just happened, for the ear: [{ type: 'again' | 'lost', build }]
    tick() {
      const t = now(), out = [];
      for (const b of list) {
        if (b.state === 'asked' && b.sentAt === null) {
          if (say(b)) continue;
          if (t - b.askedAt > UNSENT_MS) out.push({ type: 'lost', build: end(b, 'failed', 'not sent: no line to the server') });
        } else if (b.state === 'asked' && t - b.sentAt > (b.again ? ASK_MS : SLOW_MS)) {
          if (b.again && b.tries < 2 && say(b)) out.push({ type: 'again', build: b });
          else if (!b.again || b.tries >= 2 || t - b.sentAt > ASK_MS + UNSENT_MS) out.push({ type: 'lost', build: end(b, 'failed', 'no answer from the server') }); // (the last: it could not be said again for want of a wire)
        } else if (['accepted', 'building'].includes(b.state) && t - b.heardAt > LOST_MS)
          out.push({ type: 'lost', build: end(b, 'failed', 'no word of how it ended') });
      }
      list = list.filter((b) => live(b) || t - b.endedAt < KEEP[b.state]);
      keep();
      return out;
    },
    // The line as the view draws it: what is on its way, then what just ended, four rows at the most.
    rows() {
      const t = now();
      return list.slice(-4).map((b) => ({
        state: b.state, text: b.text, note: b.note, phase: b.phase || '', parts: b.parts || 0, tries: b.tries || 0,
        version: b.version, kidlisp: b.kidlisp || null, objectId: b.objectId || null, id: b.id || null, rid: b.rid,
        seconds: Math.floor((t - b.askedAt) / 1000), fade: live(b) ? 1 : Math.max(0, Math.min(1, (KEEP[b.state] - (t - b.endedAt)) / 600)),
      }));
    },
  };
}
