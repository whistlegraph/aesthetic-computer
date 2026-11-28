import { guestName } from './guest-name.mjs';
import { Match } from './match.mjs';
import { MAX_PLAYERS } from './world.mjs';

// Connections are not people. Duplicate verified handles share one player slot,
// held by the newest of them.
export class Room {
  // `guests`: seat a session with no handle too. The server turns it off, so
  // only a handle holds a body and the rest look on.
  constructor(send, { guests = true, connections = new Set() } = {}) {
    this.send = send;
    this.connections = connections;
    this.guests = guests;
    this.match = new Match();
    this.clients = new Map();
    this.sessions = new Map();
    this.members = Array(MAX_PLAYERS).fill(null);
  }
  add(ws, pending = false) {
    const record = {
      ws, room: this,
      seat: -1,
      handle: null,
      colors: null,
      owner: null,
      ready: !pending,
      welcomed: false,
    };
    this.connections.add(record);
    this.sessions.set(ws, record);
    this.clients.set(ws, -1);
    this.assign();
    return record;
  }
  // One person, one seat: a handle, else the guest's owner token, else the connection.
  person(r) {
    return r.handle?.toLowerCase() || r.owner || r;
  }
  // Set insertion order is connection order, even after a room crossing.
  counterpart(handle, except = null, seated = false) {
    if (!handle) return null;
    let newest = null;
    for (const r of this.connections)
      if (r !== except && r.ready && (!seated || r.seat >= 0) && r.handle?.toLowerCase() === handle.toLowerCase()) newest = r;
    return newest;
  }
  assign(handle = null) {
    const winners = new Map();
    for (const r of this.connections)
      if (r.handle && r.ready && !r.drone) winners.set(r.handle.toLowerCase(), r); // (🚁 a screen that chose to look on is never given the body: server.mjs `take-drone`)
    const affected = new Set([this]);
    if (handle) for (const r of this.connections)
      if (r.handle?.toLowerCase() === handle.toLowerCase()) affected.add(r.room);
    for (const room of affected) room.assignLocal(winners);
    for (const room of affected) room.broadcast();
  }
  assignLocal(winners) {
    const latest = new Map();
    for (const r of this.sessions.values()) {
      if (!r.ready || (r.handle ? winners.get(r.handle.toLowerCase()) !== r : !this.guests)) continue;
      latest.set(this.person(r), r);
    }
    for (const [key, r] of latest) {
      if (r.seat >= 0) continue;
      for (const o of this.sessions.values())
        if (o !== r && o.seat >= 0 && this.person(o) === key) {
          r.seat = o.seat; // Handed over: same seat, same player, no leave or join.
          o.seat = -1;
          this.match.handover(r.seat);
          break;
        }
    }
    const selected = [...latest.values()].slice(0, MAX_PLAYERS);
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
      if (r.welcomed) this.send(r.ws, { ...message, self: r.ready ? { ...this.public(r), drone: !!r.drone, canTakeBody: r.seat < 0 && (!!r.drone || !!this.counterpart(r.handle, r, true)) } : null });
  }
  // 🧵 Someone already known comes in from another room on the socket they
  // have: the same record, a new seat.
  adopt(record) {
    record.room = this;
    record.seat = -1;
    this.sessions.set(record.ws, record);
    this.clients.set(record.ws, -1);
    this.assign(record.handle);
    return record;
  }
  remove(ws, transferring = false) {
    const r = this.sessions.get(ws);
    if (!r) return;
    if (!transferring) this.connections.delete(r);
    this.clients.delete(ws);
    this.sessions.delete(ws);
    if (r.seat >= 0) this.match.leave(r.seat);
    this.assign(transferring ? null : r.handle);
  }
}
