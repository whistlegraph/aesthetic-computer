// 🏢 AC Tower: every handle has a floor.
//
// The tower stands on the plaza. It has as many floors as there are handles,
// and a floor's number is its address, the way a domain name is: one holder
// at a time, yours until you move, and you may ask for another. A floor's
// room and everything built in it follow its permanent id, never its number,
// so taking floor 89 carries your things up or down with you.
//
// A number handed out by the tower to a handle that has not yet come to rax
// is only pencilled in: someone who asks for it gets it, and the absent
// holder is given the asker's old number. A number is held for good once its
// handle has arrived or has asked for it by name.
import { mkdir, readFile, writeFile, rename } from 'node:fs/promises';
import { join } from 'node:path';
import { randomBytes } from 'node:crypto';
export * from './elevator.mjs';

export const GROUND = 'lobby'; // floor 0: the plaza the elevator leaves from
export const TOP = 9999; // the highest number anyone may ask for (40 km of tower)
const VALID = /^[a-z0-9]+([._][a-z0-9]+)*$/;
const key = (handle) => String(handle || '').replace(/^@/, '').toLowerCase();
export const validHandle = (handle) => VALID.test(key(handle)) && key(handle).length <= 16;

export const floorRoom = (id) => `floor-${id}`;
export const isFloorRoom = (room) => /^floor-[a-f0-9]{8}$/.test(room || '');

export class Tower {
  constructor(data = {}) {
    // floors[id] = { id, handle, number, at, held, accountId? }
    this.floors = structuredClone(data.floors || {});
    this.revision = data.revision || 0;
    this.index();
  }
  index() {
    this.byHandle = new Map();
    this.byNumber = new Map();
    for (const f of Object.values(this.floors)) {
      this.byHandle.set(f.handle, f);
      this.byNumber.set(f.number, f);
    }
  }
  toJSON() { return { version: 1, floors: this.floors, revision: this.revision }; }
  get count() { return this.byNumber.size; }
  get top() { let top = 0; for (const n of this.byNumber.keys()) if (n > top) top = n; return top; }
  floor(id) { return Object.hasOwn(this.floors, id || '') ? this.floors[id] : null; }
  forRoom(room) { return isFloorRoom(room) ? this.floor(room.slice(6)) : null; }
  forHandle(handle) { return this.byHandle.get(key(handle)) || null; }
  at(number) { return this.byNumber.get(number) || null; }
  lowestFree() { let n = 1; while (this.byNumber.has(n)) n++; return n; }
  // What a visitor may know of a floor.
  public(f) {
    return f ? { room: floorRoom(f.id), number: f.number, handle: f.handle, held: !!f.held } : null;
  }
  // "89", "#89", "@jeffrey" or "jeffrey": a number is tried before a handle,
  // so a handle made only of digits is reached with its @.
  resolve(text) {
    const q = String(text || '').trim().toLowerCase();
    if (/^#?\d{1,4}$/.test(q)) return this.at(Number(q.replace('#', ''))) || this.forHandle(q);
    return this.forHandle(q);
  }
  // Give every listed handle a floor, in the order given; known handles keep
  // theirs. Returns how many floors were added.
  admit(handles, now = Date.now()) {
    let added = 0, next = 1;
    for (const h of handles) {
      const who = key(h);
      if (!validHandle(who) || this.byHandle.has(who)) continue;
      while (this.byNumber.has(next)) next++;
      const id = randomBytes(4).toString('hex');
      if (this.floors[id]) continue; // a one in four billion repeat waits for the next sweep
      const f = (this.floors[id] = { id, handle: who, number: next, at: now, held: false });
      this.byHandle.set(who, f);
      this.byNumber.set(next, f);
      added++;
    }
    if (added) this.revision++;
    return added;
  }
  // A verified arrival: the handle's floor is theirs for good. A renamed
  // account keeps its floor under the new handle.
  arrive(handle, accountId = null, now = Date.now()) {
    const who = key(handle);
    if (!validHandle(who)) throw new Error('A floor needs a handle');
    const known = accountId && Object.values(this.floors).find((f) => f.accountId === accountId);
    let f = this.byHandle.get(who);
    if (known && known !== f) {
      if (f) { // the new handle was pencilled in: its number goes back
        delete this.floors[f.id];
      }
      known.handle = who;
      f = known;
      this.index();
      this.revision++;
    }
    if (!f) { this.admit([who], now); f = this.byHandle.get(who); }
    if (f.accountId && accountId && f.accountId !== accountId) throw new Error('Floor belongs to another account');
    if (!f.held || (accountId && !f.accountId)) {
      f.held = true;
      if (accountId) f.accountId = accountId;
      this.revision++;
    }
    return f;
  }
  // Ask for a number. Free: it is yours and your old one opens. Pencilled in
  // for someone who has not come: you trade numbers. Held: it stays theirs.
  take(handle, number) {
    const f = this.forHandle(handle);
    if (!f) throw new Error('Sign in for a floor first');
    if (!Number.isInteger(number) || number < 1 || number > TOP) throw new Error(`Floors run 1 to ${TOP}`);
    if (f.number === number) return f;
    const there = this.byNumber.get(number);
    if (there?.held) throw new Error(`Floor ${number} is @${there.handle}'s`);
    this.byNumber.delete(f.number);
    if (there) { there.number = f.number; this.byNumber.set(there.number, there); }
    f.number = number;
    f.held = true;
    this.byNumber.set(number, f);
    this.revision++;
    return f;
  }
  owns(f, member) {
    if (!f || !member?.handle) return false;
    return f.accountId ? f.accountId === member.accountId : f.handle === key(member.handle);
  }
  // The next occupied floor above or below a number (the tower may have gaps).
  step(number, dir) {
    let best = null;
    for (const n of this.byNumber.keys())
      if (dir > 0 ? n > number && (best === null || n < best) : n < number && (best === null || n > best)) best = n;
    return best === null ? null : this.byNumber.get(best);
  }
  // The directory: every floor from the ground up.
  directory() {
    return [...this.byNumber.values()].sort((a, b) => a.number - b.number)
      .map((f) => [f.number, f.handle, f.held ? 1 : 0]);
  }
}

// The ledger on disk, written whole and swapped in, one change at a time.
export class TowerStore {
  constructor(directory) {
    this.directory = directory;
    this.tower = new Tower();
    this.queue = Promise.resolve();
  }
  async load() {
    if (!this.directory) return;
    await mkdir(this.directory, { recursive: true, mode: 0o700 });
    try {
      this.tower = new Tower(JSON.parse(await readFile(join(this.directory, 'tower.json'), 'utf8')));
    } catch (e) {
      if (e.code !== 'ENOENT') throw e;
    }
  }
  change(fn) {
    const task = this.queue.then(async () => {
      const next = new Tower(this.tower.toJSON()), before = next.revision, result = fn(next);
      if (this.directory && next.revision !== before) {
        const path = join(this.directory, 'tower.json');
        await writeFile(path + '.tmp', JSON.stringify(next), { mode: 0o600 });
        await rename(path + '.tmp', path);
      }
      this.tower = next;
      return result;
    });
    this.queue = task.catch(() => {});
    return task;
  }
}
