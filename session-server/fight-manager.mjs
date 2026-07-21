// Menu Fighter coordinator. Reliable control and WebRTC signaling live here;
// frame inputs never do. All fan-out is explicitly room/match scoped.

import crypto from "node:crypto";
import {
  FIGHT_MANIFEST,
  compatibleManifest,
  manifestMismatch,
} from "../system/public/aesthetic.computer/lib/fight/protocol.mjs";
import {
  FIGHT_REGIONS,
  allowedOpponentRegions,
  normalizeFightRegion,
  sanitizeProbeSample,
  selectFightRoute,
} from "../system/public/aesthetic.computer/lib/fight/regions.mjs";

const QUEUE_TTL = 45_000;
const ROOM_TTL = 10 * 60_000;
const MATCH_TTL = 2 * 60 * 60_000;
const CHALLENGE_TTL = 60_000;

function handleKey(value) {
  const text = String(value || "").trim().replace(/^@/, "");
  return /^[a-z0-9][a-z0-9_-]{0,31}$/i.test(text) ? `@${text.toLowerCase()}` : null;
}
function cleanPayload(value) {
  if (!value) return {};
  if (typeof value === "string") {
    try { return JSON.parse(value); } catch { return {}; }
  }
  return value;
}

export class FightManager {
  constructor({ now = () => Date.now(), id = () => crypto.randomUUID(), manifest = FIGHT_MANIFEST } = {}) {
    this.now = now;
    this.id = id;
    this.manifest = manifest;
    this.sendWS = null;
    this.connections = new Map(); // ws -> authenticated identity, when present
    this.handleSockets = new Map();
    this.queue = new Map(); // ws -> ticket
    this.rooms = new Map();
    this.challenges = new Map();
    this.matches = new Map();
    this.matchBySocket = new Map();
  }

  setSendFunction(sendWS) { this.sendWS = sendWS; }
  send(wsId, type, content) { this.sendWS?.(wsId, type, content); }
  error(wsId, code, message, requestId) {
    this.send(wsId, "fight:error", { code, message, requestId: requestId || null });
    return false;
  }

  authenticate(wsId, identity) {
    const handle = handleKey(identity?.handle);
    if (!wsId || !identity?.accountId || !handle) {
      return this.error(wsId, "auth-required", "A verified AC handle is required for public play.");
    }
    const previous = this.handleSockets.get(handle);
    this.connections.set(wsId, { accountId: identity.accountId, handle, authenticatedAt: this.now() });
    this.handleSockets.set(handle, wsId);
    if (previous && previous !== wsId) this.rebind(previous, wsId);
    this.send(wsId, "fight:auth:ok", { handle, protocolVersion: this.manifest.protocolVersion });
    return true;
  }

  rebind(oldWs, newWs) {
    const ticket = this.queue.get(oldWs);
    if (ticket) {
      this.queue.delete(oldWs);
      ticket.wsId = newWs;
      this.queue.set(newWs, ticket);
    }
    for (const room of this.rooms.values()) {
      for (const seat of room.seats) if (seat?.wsId === oldWs) seat.wsId = newWs;
    }
    const matchId = this.matchBySocket.get(oldWs);
    if (matchId) {
      const match = this.matches.get(matchId);
      const player = match?.players.find((entry) => entry.wsId === oldWs);
      if (player) player.wsId = newWs;
      this.matchBySocket.delete(oldWs);
      this.matchBySocket.set(newWs, matchId);
    }
  }

  identity(wsId) { return this.connections.get(wsId) || null; }

  queueJoin(wsId, raw) {
    const data = cleanPayload(raw);
    const identity = this.identity(wsId);
    if (!identity) return this.error(wsId, "auth-required", "Sign in with an AC handle to find a match.", data.requestId);
    const mismatch = manifestMismatch(data.manifest, this.manifest);
    if (mismatch) return this.error(wsId, "incompatible-build", `Match ${mismatch} does not match this server.`, data.requestId);
    const now = this.now();
    const region = normalizeFightRegion(data.region);
    this.queue.set(wsId, {
      wsId,
      accountId: identity.accountId,
      handle: identity.handle,
      manifest: { ...data.manifest },
      region,
      mode: data.mode === "ranked" ? "ranked" : "casual",
      platform: String(data.platform || "web").slice(0, 24),
      joinedAt: this.queue.get(wsId)?.joinedAt || now,
      expiresAt: now + QUEUE_TTL,
    });
    this.send(wsId, "fight:queue:state", { state: "queued", region, joinedAt: now });
    this.matchAll();
    return true;
  }

  queueLeave(wsId, reason = "left") {
    if (!this.queue.delete(wsId)) return false;
    this.send(wsId, "fight:queue:state", { state: reason });
    return true;
  }

  compatible(a, b, now = this.now()) {
    if (!compatibleManifest(a.manifest, b.manifest)) return false;
    if (a.mode !== b.mode || a.accountId === b.accountId) return false;
    const aRegions = allowedOpponentRegions(a.region, now - a.joinedAt);
    const bRegions = allowedOpponentRegions(b.region, now - b.joinedAt);
    return aRegions.includes(b.region) && bRegions.includes(a.region);
  }

  matchAll() {
    this.sweep();
    const now = this.now();
    const tickets = [...this.queue.values()].sort((a, b) => a.joinedAt - b.joinedAt);
    const used = new Set();
    for (const a of tickets) {
      if (used.has(a.wsId) || !this.queue.has(a.wsId)) continue;
      const candidates = tickets.filter((b) => !used.has(b.wsId) && b.wsId !== a.wsId &&
        this.queue.has(b.wsId) && this.compatible(a, b, now));
      candidates.sort((x, y) => {
        const xLocal = x.region === a.region ? 0 : 1;
        const yLocal = y.region === a.region ? 0 : 1;
        return xLocal - yLocal || x.joinedAt - y.joinedAt;
      });
      const b = candidates[0];
      if (!b) continue;
      used.add(a.wsId); used.add(b.wsId);
      this.queue.delete(a.wsId); this.queue.delete(b.wsId);
      this.createMatch(a, b);
    }
  }

  createMatch(a, b, roomId = null) {
    const now = this.now();
    const match = {
      id: this.id(),
      revision: 1,
      players: [
        { wsId: a.wsId, handle: a.handle || "P1", accountId: a.accountId || null, seat: 0 },
        { wsId: b.wsId, handle: b.handle || "P2", accountId: b.accountId || null, seat: 1 },
      ],
      manifest: { ...this.manifest },
      seed: crypto.randomBytes(4).readUInt32LE(0),
      mode: a.mode || "casual",
      regions: [a.region || null, b.region || null],
      roomId,
      createdAt: now,
      expiresAt: now + MATCH_TTL,
      accepted: new Set(),
      probeNonce: this.id(),
      reports: {},
      route: { type: "probing", region: null },
    };
    this.matches.set(match.id, match);
    for (const player of match.players) this.matchBySocket.set(player.wsId, match.id);
    for (const player of match.players) {
      const opponent = match.players[1 - player.seat];
      this.send(player.wsId, "fight:match:proposal", {
        matchId: match.id,
        revision: match.revision,
        seat: player.seat,
        initiator: player.seat === 0,
        opponent: { handle: opponent.handle, region: match.regions[opponent.seat] },
        manifest: match.manifest,
        seed: match.seed,
        mode: match.mode,
        region: match.regions[player.seat],
        candidateRelayRegions: Object.keys(FIGHT_REGIONS),
        probeNonce: match.probeNonce,
      });
    }
    return match;
  }

  proposalAccept(wsId, raw) {
    const data = cleanPayload(raw);
    const match = this.memberMatch(wsId, data.matchId);
    if (!match) return this.error(wsId, "not-in-match", "That match is no longer available.", data.requestId);
    if (!compatibleManifest(data.manifest, match.manifest)) return this.error(wsId, "incompatible-build", "Match versions changed before start.");
    match.accepted.add(wsId);
    if (match.accepted.size === 2) {
      match.revision++;
      for (const player of match.players) this.send(player.wsId, "fight:match:start", {
        matchId: match.id, revision: match.revision, seat: player.seat,
        seed: match.seed, manifest: match.manifest, route: match.route,
      });
    }
    return true;
  }

  memberMatch(wsId, requestedId) {
    const id = requestedId || this.matchBySocket.get(wsId);
    const match = id && this.matches.get(id);
    return match?.players.some((player) => player.wsId === wsId) ? match : null;
  }

  signal(wsId, raw) {
    const data = cleanPayload(raw);
    const match = this.memberMatch(wsId, data.matchId);
    if (!match || !data.signal) return this.error(wsId, "bad-signal", "Invalid match signal.");
    const sender = match.players.find((player) => player.wsId === wsId);
    const target = match.players.find((player) => player.wsId !== wsId);
    // The peer gets no socket id, account id, IP, or unrelated lobby state.
    this.send(target.wsId, "fight:signal", {
      matchId: match.id, revision: match.revision, fromSeat: sender.seat, signal: data.signal,
    });
    return true;
  }

  routeReport(wsId, raw) {
    const data = cleanPayload(raw);
    const match = this.memberMatch(wsId, data.matchId);
    if (!match) return this.error(wsId, "not-in-match", "Unknown route probe.");
    const player = match.players.find((entry) => entry.wsId === wsId);
    const direct = sanitizeProbeSample(data.direct, match.probeNonce);
    const relays = {};
    for (const region of Object.keys(FIGHT_REGIONS)) {
      const sample = sanitizeProbeSample(data.relays?.[region], match.probeNonce);
      if (sample) relays[region] = sample;
    }
    // Missing, stale, or implausible samples are ignored rather than treated as fast.
    match.reports[player.seat] = { direct, relays };
    if (match.reports[0] && match.reports[1]) {
      match.route = selectFightRoute(match.reports);
      match.revision++;
      for (const member of match.players) this.send(member.wsId, "fight:route:selected", {
        matchId: match.id, revision: match.revision, route: match.route,
      });
    }
    return true;
  }

  roomCreate(wsId, raw = {}) {
    const data = cleanPayload(raw);
    const mismatch = manifestMismatch(data.manifest, this.manifest);
    if (mismatch) {
      this.error(wsId, "incompatible-build", `Room ${mismatch} does not match this server.`);
      return null;
    }
    const identity = this.identity(wsId);
    const code = this.uniqueCode();
    const now = this.now();
    const hostToken = this.id();
    const seat = { wsId, handle: identity?.handle || "P1", accountId: identity?.accountId || null, seat: 0 };
    const room = { id: this.id(), code, hostToken, seats: [seat, null], createdAt: now,
      expiresAt: now + ROOM_TTL, revision: 1 };
    this.rooms.set(code, room);
    this.send(wsId, "fight:room:created", { roomId: room.id, code, hostToken,
      expiresAt: room.expiresAt, seat: 0, revision: room.revision });
    return room;
  }

  uniqueCode() {
    const alphabet = "23456789ABCDEFGHJKMNPQRSTUVWXYZ";
    let code;
    do {
      const bytes = crypto.randomBytes(8);
      code = [...bytes].map((value) => alphabet[value % alphabet.length]).slice(0, 8).join("");
      code = `${code.slice(0, 4)}-${code.slice(4)}`;
    } while (this.rooms.has(code));
    return code;
  }

  roomJoin(wsId, raw) {
    const data = cleanPayload(raw);
    const mismatch = manifestMismatch(data.manifest, this.manifest);
    if (mismatch) return this.error(wsId, "incompatible-build", `Room ${mismatch} does not match this server.`);
    const code = String(data.code || "").toUpperCase().replace(/^VS\s*/, "");
    const room = this.rooms.get(code);
    if (!room || room.expiresAt <= this.now()) return this.error(wsId, "room-not-found", "VS room not found or expired.");
    if (room.seats.some((seat) => seat?.wsId === wsId)) return true;
    if (room.seats[1]) return this.error(wsId, "room-full", "VS room already has two fighters.");
    const identity = this.identity(wsId);
    room.seats[1] = { wsId, handle: identity?.handle || "P2", accountId: identity?.accountId || null, seat: 1 };
    room.revision++; room.expiresAt = this.now() + ROOM_TTL;
    for (const seat of room.seats) this.send(seat.wsId, "fight:room:ready", {
      roomId: room.id, code: room.code, revision: room.revision,
      players: room.seats.map((player) => ({ handle: player.handle, seat: player.seat })),
    });
    this.createMatch({ ...room.seats[0], region: data.region, mode: "casual" },
      { ...room.seats[1], region: data.region, mode: "casual" }, room.id);
    return true;
  }

  challengeCreate(wsId, raw) {
    const data = cleanPayload(raw);
    const identity = this.identity(wsId);
    if (!identity) return this.error(wsId, "auth-required", "A handled account is required to challenge.");
    const mismatch = manifestMismatch(data.manifest, this.manifest);
    if (mismatch) return this.error(wsId, "incompatible-build", `Challenge ${mismatch} does not match this server.`);
    const target = handleKey(data.target);
    const targetWs = target && this.handleSockets.get(target);
    if (!targetWs || targetWs === wsId) return this.error(wsId, "challenge-unavailable", "That handled player is not available.");
    const inviteId = this.id();
    const invite = {
      id: inviteId,
      fromHandle: identity.handle,
      toHandle: target,
      fromRegion: normalizeFightRegion(data.region),
      fromPlatform: String(data.platform || "web").slice(0, 24),
      manifest: { ...data.manifest },
      createdAt: this.now(),
      expiresAt: this.now() + CHALLENGE_TTL,
    };
    this.challenges.set(inviteId, invite);
    this.send(targetWs, "fight:challenge:incoming", { inviteId, from: identity.handle,
      manifest: this.manifest, region: invite.fromRegion, expiresAt: invite.expiresAt });
    this.send(wsId, "fight:challenge:sent", { inviteId, target });
    return true;
  }

  challengeReply(wsId, raw) {
    const data = cleanPayload(raw);
    const identity = this.identity(wsId);
    if (!identity) return this.error(wsId, "auth-required", "A handled account is required to answer a challenge.");
    const invite = this.challenges.get(String(data.inviteId || ""));
    if (!invite || invite.expiresAt <= this.now() || invite.toHandle !== identity.handle) {
      return this.error(wsId, "challenge-expired", "That fight request is no longer available.");
    }
    if (!data.accept) {
      this.challenges.delete(invite.id);
      const sourceWs = this.handleSockets.get(invite.fromHandle);
      if (sourceWs) this.send(sourceWs, "fight:challenge:declined", { inviteId: invite.id, target: identity.handle });
      this.send(wsId, "fight:challenge:closed", { inviteId: invite.id, state: "declined" });
      return true;
    }
    const mismatch = manifestMismatch(data.manifest, invite.manifest);
    if (mismatch) return this.error(wsId, "incompatible-build", `Challenge ${mismatch} does not match this client.`);
    const sourceWs = this.handleSockets.get(invite.fromHandle);
    if (!sourceWs || sourceWs === wsId) {
      this.challenges.delete(invite.id);
      return this.error(wsId, "challenge-unavailable", "The challenger is no longer available.");
    }
    this.challenges.delete(invite.id);
    const source = this.identity(sourceWs);
    const target = this.identity(wsId);
    this.send(sourceWs, "fight:challenge:accepted", { inviteId: invite.id, target: target.handle });
    this.send(wsId, "fight:challenge:accepted", { inviteId: invite.id, target: source.handle });
    this.createMatch({
      wsId: sourceWs, accountId: source.accountId, handle: source.handle,
      region: invite.fromRegion, mode: "casual", manifest: invite.manifest,
    }, {
      wsId, accountId: target.accountId, handle: target.handle,
      region: normalizeFightRegion(data.region), mode: "casual", manifest: invite.manifest,
    });
    return true;
  }

  lobbySnapshot(wsId) {
    const identity = this.identity(wsId);
    if (!identity) return this.error(wsId, "auth-required", "Sign in to view public fight presence.");
    const queued = [...this.queue.values()].map((ticket) => ({ handle: ticket.handle,
      region: ticket.region, mode: ticket.mode, queuedAt: ticket.joinedAt }));
    this.send(wsId, "fight:lobby:snapshot", { queued, count: queued.length, at: this.now() });
    return true;
  }

  dispatch(wsId, type, raw) {
    this.sweep();
    if (type === "fight:queue:join") return this.queueJoin(wsId, raw);
    if (type === "fight:queue:leave") return this.queueLeave(wsId);
    if (type === "fight:match:accept") return this.proposalAccept(wsId, raw);
    if (type === "fight:signal") return this.signal(wsId, raw);
    if (type === "fight:route:report") return this.routeReport(wsId, raw);
    if (type === "fight:room:create") return this.roomCreate(wsId, raw);
    if (type === "fight:room:join") return this.roomJoin(wsId, raw);
    if (type === "fight:challenge:create") return this.challengeCreate(wsId, raw);
    if (type === "fight:challenge:reply") return this.challengeReply(wsId, raw);
    if (type === "fight:lobby:get") return this.lobbySnapshot(wsId);
    return false;
  }

  cleanup(wsId) {
    const identity = this.connections.get(wsId);
    // A stale socket close after re-auth/rebind must not evict the live socket.
    if (identity && this.handleSockets.get(identity.handle) === wsId) this.handleSockets.delete(identity.handle);
    this.connections.delete(wsId);
    this.queue.delete(wsId);
    for (const [code, room] of this.rooms) {
      const index = room.seats.findIndex((seat) => seat?.wsId === wsId);
      if (index >= 0) {
        room.seats[index] = null;
        room.revision++;
        for (const seat of room.seats) if (seat) this.send(seat.wsId, "fight:room:peer-left", { roomId: room.id });
        if (!room.seats.some(Boolean)) this.rooms.delete(code);
      }
    }
    const matchId = this.matchBySocket.get(wsId);
    this.matchBySocket.delete(wsId);
    const match = matchId && this.matches.get(matchId);
    if (match) {
      const other = match.players.find((player) => player.wsId !== wsId);
      if (other) this.send(other.wsId, "fight:match:peer-left", { matchId, reconnectGraceMs: 10_000 });
    }
  }

  // Legacy close hook retained while old clients age out.
  leave(_handle, wsId) { this.cleanup(wsId); }

  sweep() {
    const now = this.now();
    for (const [wsId, ticket] of this.queue) if (ticket.expiresAt <= now) this.queueLeave(wsId, "expired");
    for (const [code, room] of this.rooms) if (room.expiresAt <= now) this.rooms.delete(code);
    for (const [id, challenge] of this.challenges) if (challenge.expiresAt <= now) this.challenges.delete(id);
    for (const [id, match] of this.matches) if (match.expiresAt <= now) {
      this.matches.delete(id);
      for (const player of match.players) if (this.matchBySocket.get(player.wsId) === id) this.matchBySocket.delete(player.wsId);
    }
  }
}
