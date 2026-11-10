import { guestName } from './guest-name.mjs';
import { Match } from './match.mjs';
import { MAX_PLAYERS } from './world.mjs';

// Connections are not people. Duplicate verified handles share one player slot.
export class Room {
  constructor(send) {
    this.send = send;
    this.match = new Match();
    this.clients = new Map();
    this.sessions = new Map();
    this.members = Array(MAX_PLAYERS).fill(null);
  }
  add(ws, pending = false) {
    const record = {
      ws,
      seat: -1,
      handle: null,
      colors: null,
      owner: null,
      ready: !pending,
      welcomed: false,
    };
    this.sessions.set(ws, record);
    this.clients.set(ws, -1);
    this.assign();
    return record;
  }
  assign() {
    const selected = [],
      seen = new Set();
    for (const r of this.sessions.values()) {
      if (!r.ready) continue;
      const key = r.handle?.toLowerCase() || r.owner || r;
      if (seen.has(key)) continue;
      seen.add(key);
      if (selected.length < MAX_PLAYERS) selected.push(r);
    }
    for (const r of this.sessions.values())
      if (r.seat >= 0 && !selected.includes(r)) {
        this.match.leave(r.seat);
        r.seat = -1;
      }
    for (const r of selected) if (r.seat < 0) r.seat = this.match.join();
    this.members = Array(MAX_PLAYERS).fill(null);
    for (const r of this.sessions.values()) {
      const previous = this.clients.get(r.ws);
      this.clients.set(r.ws, r.seat);
      if (r.seat >= 0) this.members[r.seat] = this.public(r);
      if (r.welcomed && previous !== r.seat)
        this.send(r.ws, {
          type: 'role',
          seat: r.seat,
          role: r.seat < 0 ? 'spectator' : 'player',
        });
    }
    this.broadcast();
  }
  public(r) {
    return {
      seat: r.seat,
      role: r.seat < 0 ? 'spectator' : 'player',
      handle: r.handle,
      name: r.handle ? null : guestName(r.owner),
      colors: r.colors,
      owner: r.owner,
    };
  }
  broadcast() {
    const unique = new Map();
    let spectators = 0;
    for (const r of this.sessions.values()) {
      if (!r.ready) continue;
      if (r.seat < 0) spectators++;
      const key = r.handle?.toLowerCase() || r.owner || r;
      if (!unique.has(key) || r.seat >= 0) unique.set(key, this.public(r));
    }
    const message = {
      type: 'presence',
      members: [...unique.values()],
      spectators,
    };
    for (const r of this.sessions.values())
      // A member still waiting for its handle has no name to show yet.
      if (r.welcomed) this.send(r.ws, { ...message, self: r.ready ? this.public(r) : null });
  }
  remove(ws) {
    const r = this.sessions.get(ws);
    if (!r) return;
    this.clients.delete(ws);
    this.sessions.delete(ws);
    if (r.seat >= 0) this.match.leave(r.seat);
    this.assign();
  }
}
