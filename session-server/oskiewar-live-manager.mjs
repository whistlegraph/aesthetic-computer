// oskiewar Live Manager, 26.08.04
// Small, public, match-id-scoped relay for phone spectators and, since the
// frame numbers started riding in the payload, for telemetry agents too.

import {
  oskiewarEvent,
  oskiewarSurface,
} from "../system/public/aesthetic.computer/lib/oskiewar-analytics.mjs";
import { createPostHogEventCapture } from "../shared/posthog-event-capture.mjs";

const MATCH_WORD = "[bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou]";
const MATCH_NAME = new RegExp(
  `^(?:${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}|[a-z]{4,7}[0-9]{1,3})$`);
const MATCH_ID = new RegExp(
  `^ow-(?:${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}|[a-z]{4,7}[0-9]{1,3})$`);
const PHASES = new Set(["select", "intro", "fight", "round", "match", "replay"]);
const MAX_MESSAGE_BYTES = 8192;
const MAX_VIEWERS = 64;
// Agents watch the same fan-out as a phone but are counted and capped on their
// own, so a room full of spectators can never lock a maintainer out of the
// telemetry, and a stuck agent can never eat the spectator allowance.
const MAX_AGENTS = 4;
// Since versus became the front door every visitor holds a room while their
// tab is open, so the ceiling is concurrent visitors rather than concurrent
// fights being watched.
const MAX_ROOMS = 256;
const ROOM_TTL_MS = 10 * 60 * 1000;
const MIN_PUBLISH_INTERVAL_MS = 25;
// The challenger's presses. A pad state is a few dozen bytes and a hand can
// only change it so fast — the cap is generous for play and stingy for abuse.
const MAX_INPUT_BYTES = 640;
const MIN_INPUT_INTERVAL_MS = 15;
const INPUT_BUTTON = /^[A-Za-z]{1,16}$/;
const FIGHTER_NAME = /^@?[A-Z0-9_-]{1,24}$/i;

const finite = (value, limit = 1000000) =>
  Number.isFinite(value) && Math.abs(value) <= limit;
const integer = (value, low, high) =>
  Number.isInteger(value) && value >= low && value <= high;

export function canonicalMatchId(value) {
  const raw = String(value || "").toLowerCase();
  const name = raw.startsWith("ow-") ? raw.slice(3) : raw;
  return MATCH_NAME.test(name) ? `ow-${name}` : null;
}

function color(value) {
  return Array.isArray(value) && value.length === 3 &&
    value.every((channel) => integer(channel, 0, 255));
}

function fighter(value) {
  return value && typeof value === "object" &&
    typeof value.name === "string" && /^@?[A-Z0-9_-]{1,24}$/i.test(value.name) &&
    color(value.color) && finite(value.x) && finite(value.y) && finite(value.z) &&
    [-1, 1].includes(value.facing) && typeof value.alive === "boolean" &&
    typeof value.grounded === "boolean" && typeof value.ducking === "boolean" &&
    typeof value.blocking === "boolean" && integer(value.score, 0, 99) &&
    integer(value.roundWins, 0, 5) &&
    (value.attack === "" || /^[A-Z0-9 _-]{1,24}$/.test(value.attack));
}

function ball(value) {
  return value && typeof value === "object" &&
    typeof value.active === "boolean" && finite(value.x) &&
    finite(value.y) && finite(value.z) && finite(value.radius, 10000);
}

// How fast the publishing client is actually drawing. @jeffrey plays in a
// browser on an Xbox, which has no devtools, so this is the only way to read a
// console's frame rate from anywhere else. Every field is optional because a
// host publishes only the stages it measured, and the closed key list keeps a
// public payload from growing a channel nobody reviewed.
const PERF_KEYS = new Set(["fps", "frameMs", "renderMs", "hz"]);

function perf(value) {
  if (value === undefined) return true;
  if (!value || typeof value !== "object" || Array.isArray(value)) return false;
  if (Object.keys(value).some((key) => !PERF_KEYS.has(key))) return false;
  return (value.fps === undefined || integer(value.fps, 0, 1000)) &&
    (value.hz === undefined || integer(value.hz, 0, 1000)) &&
    (value.frameMs === undefined || finite(value.frameMs, 10000)) &&
    (value.renderMs === undefined || finite(value.renderMs, 10000));
}

export function validateOskiewarLiveState(value) {
  if (!value || value.format !== "ac.oskiewar.live" || value.version !== 1)
    return "Unsupported live state";
  if (!integer(value.seq, 0, 2147483647) || !finite(value.at, 10000000000000))
    return "Invalid sequence";
  if (!PHASES.has(value.phase)) return "Invalid phase";
  if (value.seriesId !== undefined && !MATCH_ID.test(value.seriesId))
    return "Invalid series ID";
  if (value.roundId !== undefined && !MATCH_ID.test(value.roundId))
    return "Invalid round ID";
  if (value.previousRoundId !== undefined && value.previousRoundId !== "" &&
      !MATCH_ID.test(value.previousRoundId)) return "Invalid previous round ID";
  if (value.nextRoundId !== undefined && !MATCH_ID.test(value.nextRoundId))
    return "Invalid next round ID";
  if (!Array.isArray(value.fighters) || value.fighters.length !== 2 ||
      value.fighters.some((entry) => !fighter(entry))) return "Invalid fighters";
  if (!ball(value.ball)) return "Invalid ball";
  if (value.balls !== undefined && (!Array.isArray(value.balls) ||
      value.balls.length < 1 || value.balls.length > 4 ||
      value.balls.some((entry) => !ball(entry)))) return "Invalid balls";
  if (!value.camera || typeof value.camera !== "object" ||
      !finite(value.camera.x) || !finite(value.camera.y) ||
      !finite(value.camera.width, 100000) || value.camera.width < 100)
    return "Invalid camera";
  if (value.wind !== undefined && (!value.wind ||
      ![-1, 1].includes(value.wind.direction) ||
      !integer(value.wind.mph, 0, 200))) return "Invalid wind";
  if (!value.round || typeof value.round !== "object" ||
      !integer(value.round.remainingMs, 0, 3600000) ||
      typeof value.round.result !== "string" || value.round.result.length > 80)
    return "Invalid round";
  if (!perf(value.perf)) return "Invalid performance";
  if (value.replayUrl !== undefined &&
      !/^\/api\/oskiewar-replays\?id=ow-[a-z0-9-]+$/.test(value.replayUrl))
    return "Invalid replay URL";
  return null;
}

function send(ws, type, content) {
  if (ws?.readyState !== 1) return false;
  try { ws.send(JSON.stringify({ type, content })); return true; }
  catch { return false; }
}

export class OskiewarLiveManager {
  constructor({
    now = () => Date.now(),
    analytics = createPostHogEventCapture({
      distinctId: "ac-oskiewar-session-aggregate",
      eventFactory: oskiewarEvent,
    }),
  } = {}) {
    this.rooms = new Map();
    this.now = now;
    this.analytics = analytics;
  }

  accepts(url) {
    try { return new URL(url, "http://session").pathname === "/oskiewar-live"; }
    catch { return false; }
  }

  handleConnection(ws, req) {
    if (!this.accepts(req?.url)) return false;
    // This route returns before the ordinary game-socket heartbeat setup.
    ws.isAlive = true;
    ws.on("pong", () => { ws.isAlive = true; });
    const url = new URL(req.url, "http://session");
    const matchId = canonicalMatchId(url.searchParams.get("match"));
    // Four roles, and everything unrecognized still watches. A phone that
    // scanned the round QR sends no role at all, and an older client that sends
    // one this build has never heard of must not be turned away at the door.
    const requestedRole = url.searchParams.get("role");
    const role = requestedRole === "publisher" ? "publisher"
      : requestedRole === "agent" ? "agent"
      : requestedRole === "challenger" ? "challenger" : "viewer";
    const surface = oskiewarSurface(url.searchParams.get("surface"));
    if (!matchId) {
      send(ws, "oskiewar:error", { message: "Invalid match ID" });
      ws.close?.(4400, "Invalid match ID");
      return true;
    }
    this.prune();
    let room = this.rooms.get(matchId);
    if (!room) {
      if (this.rooms.size >= MAX_ROOMS) {
        send(ws, "oskiewar:error", { message: "Live match capacity reached" });
        ws.close?.(4429, "Capacity reached");
        return true;
      }
      room = { matchId, publisher: null, publisherSurface: "unknown",
        challenger: null, viewers: new Set(), agents: new Set(), state: null,
        liveStarted: false, updatedAt: this.now(), publishedAt: 0,
        nudgedAt: 0, flaggedAt: 0, inputAt: 0 };
      this.rooms.set(matchId, room);
    }
    if (role === "publisher") this.addPublisher(room, ws, surface);
    else if (role === "agent") this.addAgent(room, ws);
    else if (role === "challenger") this.addChallenger(room, ws, surface);
    else this.addViewer(room, ws, surface);
    return true;
  }

  // Who is in the room, split by what they came for. The publishing game draws
  // an agent mark from `agents` alone, so a packed grandstand must never make
  // it claim a maintainer is linked in.
  audience(room) {
    return { count: room.viewers.size, agents: room.agents.size };
  }

  announceAudience(room) {
    send(room.publisher, "oskiewar:viewers", this.audience(room));
  }

  addPublisher(room, ws, surface) {
    if (room.publisher?.readyState === 1) {
      send(ws, "oskiewar:error", { message: "This match already has a publisher" });
      ws.close?.(4409, "Publisher already connected");
      return;
    }
    room.publisher = ws;
    room.publisherSurface = surface;
    room.updatedAt = this.now();
    send(ws, "oskiewar:ready", { matchId: room.matchId,
      viewers: room.viewers.size, agents: room.agents.size,
      maxHz: Math.floor(1000 / MIN_PUBLISH_INTERVAL_MS) });
    this.broadcastStatus(room);
    ws.on("message", (data) => this.publish(room, ws, data));
    ws.on("close", () => {
      if (room.publisher !== ws) return;
      room.publisher = null;
      room.updatedAt = this.now();
      this.broadcastStatus(room);
    });
    ws.on("error", () => {});
  }

  addViewer(room, ws, surface) {
    if (room.viewers.size >= MAX_VIEWERS) {
      send(ws, "oskiewar:error", { message: "This match has reached 64 viewers" });
      ws.close?.(4429, "Viewer capacity reached");
      return;
    }
    room.viewers.add(ws);
    room.updatedAt = this.now();
    this.analytics.capture("spectator_joined", {
      source_system: "session-server",
      surface,
      viewer_state: room.publisher?.readyState === 1 ? "live" : "waiting",
    });
    send(ws, "oskiewar:status", this.status(room));
    if (room.state) send(ws, "oskiewar:state", room.state);
    this.announceAudience(room);
    const remove = () => {
      room.viewers.delete(ws);
      room.updatedAt = this.now();
      this.announceAudience(room);
    };
    ws.on("close", remove);
    ws.on("error", remove);
  }

  // A telemetry watcher: same fan-out as a phone, because the frame numbers
  // ride in the state payload, but counted apart so the game can tell a machine
  // reading its performance from somebody watching the fight. Deliberately no
  // analytics — a maintainer attaching a debugger is not a spectator, and
  // filing it as one would quietly inflate the audience metric.
  addAgent(room, ws) {
    if (room.agents.size >= MAX_AGENTS) {
      send(ws, "oskiewar:error", { message: "This match has reached 4 agents" });
      ws.close?.(4429, "Agent capacity reached");
      return;
    }
    room.agents.add(ws);
    room.updatedAt = this.now();
    send(ws, "oskiewar:status", this.status(room));
    if (room.state) send(ws, "oskiewar:state", room.state);
    this.announceAudience(room);
    // The one lever an agent may pull: ask the publishing game to reload
    // itself. @jeffrey plays in Edge on an Xbox, where "refresh the page"
    // means finding a controller-driven address bar — so the deploy loop
    // wants a remote nudge. The relay forwards the bare instruction and
    // nothing else, at most once per five seconds per room, and the shell
    // decides when reloading is actually safe.
    ws.on("message", (data) => this.nudge(room, data));
    const remove = () => {
      room.agents.delete(ws);
      room.updatedAt = this.now();
      this.announceAudience(room);
    };
    ws.on("close", remove);
    ws.on("error", remove);
  }

  // The second chair. One per room, first come first served: the friend who
  // opened the shared URL sits down and their presses travel to the publishing
  // game, which runs the one authoritative simulation and streams the fight
  // back over the same fan-out every phone already reads. A denied seat closes
  // with 4409 so the client knows to stay and watch instead.
  addChallenger(room, ws, surface) {
    if (room.challenger?.readyState === 1) {
      send(ws, "oskiewar:error", { message: "This match already has a challenger" });
      ws.close?.(4409, "Challenger already seated");
      return;
    }
    room.challenger = ws;
    room.updatedAt = this.now();
    this.analytics.capture("challenger_joined", {
      source_system: "session-server",
      surface,
      viewer_state: room.publisher?.readyState === 1 ? "live" : "waiting",
    });
    send(ws, "oskiewar:seat", { matchId: room.matchId, seat: "challenger" });
    send(ws, "oskiewar:status", this.status(room));
    if (room.state) send(ws, "oskiewar:state", room.state);
    ws.on("message", (data) => this.relayInput(room, ws, data));
    const remove = () => {
      if (room.challenger !== ws) return;
      room.challenger = null;
      room.updatedAt = this.now();
      this.broadcastStatus(room);
    };
    ws.on("close", remove);
    ws.on("error", remove);
    this.broadcastStatus(room);
  }

  // A challenger's pad, forwarded to the publisher and nowhere else. The relay
  // checks shape, not meaning: short button names, a stick within its gimbal,
  // an optional handle and wardrobe so the host can dress the second fighter.
  // Anything malformed is dropped in silence — a fight must not stutter
  // because one packet came in bent.
  relayInput(room, ws, data) {
    if (room.challenger !== ws) return;
    if (Buffer.byteLength(data) > MAX_INPUT_BYTES) return;
    let message;
    try { message = JSON.parse(data.toString()); } catch { return; }
    if (message.type !== "oskiewar:input") return;
    const input = message.content;
    if (!input || typeof input !== "object" || Array.isArray(input)) return;
    if (!integer(input.seq, 0, 2147483647)) return;
    if (!Array.isArray(input.down) || input.down.length > 10 ||
        input.down.some((button) => typeof button !== "string" ||
          !INPUT_BUTTON.test(button))) return;
    if (!finite(input.leftX, 1.5) || !finite(input.leftY, 1.5)) return;
    if (input.name !== undefined && input.name !== "" &&
        (typeof input.name !== "string" || !FIGHTER_NAME.test(input.name)))
      return;
    if (input.colors !== undefined && (!Array.isArray(input.colors) ||
        input.colors.length > 4 || !input.colors.every(color))) return;
    const now = this.now();
    if (now - room.inputAt < MIN_INPUT_INTERVAL_MS) return;
    room.inputAt = now;
    room.updatedAt = now;
    send(room.publisher, "oskiewar:input", { seq: input.seq,
      down: input.down, leftX: input.leftX, leftY: input.leftY,
      name: typeof input.name === "string" ? input.name : "",
      colors: Array.isArray(input.colors) ? input.colors : [] });
  }

  nudge(room, data) {
    if (Buffer.byteLength(data) > 512) return;
    let message;
    try { message = JSON.parse(data.toString()); } catch { return; }
    if (message.type === "oskiewar:reload") {
      const now = this.now();
      if (room.nudgedAt && now - room.nudgedAt < 5000) return;
      room.nudgedAt = now;
      send(room.publisher, "oskiewar:reload", {});
      return;
    }
    // Render experiment flags: a closed little dictionary — short lowercase
    // names, booleans or small numbers, a handful at a time — forwarded to
    // the publishing game so an agent can measure what each layer costs on
    // the real machine. Four a second is faster than any experiment needs
    // and slow enough that a runaway probe cannot strobe somebody's screen.
    if (message.type !== "oskiewar:flags") return;
    const flags = message.content;
    if (!flags || typeof flags !== "object" || Array.isArray(flags)) return;
    const entries = Object.entries(flags);
    if (entries.length < 1 || entries.length > 8) return;
    for (const [key, value] of entries) {
      if (!/^[a-z][a-zA-Z0-9]{0,23}$/.test(key)) return;
      if (typeof value !== "boolean" &&
          !(Number.isFinite(value) && Math.abs(value) <= 64)) return;
    }
    const now = this.now();
    if (room.flaggedAt && now - room.flaggedAt < 250) return;
    room.flaggedAt = now;
    send(room.publisher, "oskiewar:flags", flags);
  }

  publish(room, ws, data) {
    if (room.publisher !== ws) return;
    if (Buffer.byteLength(data) > MAX_MESSAGE_BYTES) {
      send(ws, "oskiewar:error", { message: "Live state exceeds 8 KiB" });
      return;
    }
    let message;
    try { message = JSON.parse(data.toString()); }
    catch { return send(ws, "oskiewar:error", { message: "Invalid JSON" }); }
    if (message.type !== "oskiewar:state") return;
    let state = message.content;
    if (typeof state === "string") {
      try { state = JSON.parse(state); } catch { state = null; }
    }
    const invalid = validateOskiewarLiveState(state);
    if (invalid) return send(ws, "oskiewar:error", { message: invalid });
    const now = this.now();
    if (now - room.publishedAt < MIN_PUBLISH_INTERVAL_MS) return;
    if (room.state && state.seq <= room.state.seq) return;
    room.state = state;
    room.publishedAt = now;
    room.updatedAt = now;
    if (!room.liveStarted) {
      room.liveStarted = true;
      this.analytics.capture("live_started", {
        source_system: "session-server",
        surface: room.publisherSurface,
        phase: state.phase,
      });
    }
    for (const watcher of this.watchers(room))
      send(watcher, "oskiewar:state", state);
  }

  // Agents read the frame numbers out of the same state payload a phone gets,
  // so the fan-out is one list; only the counting is split. The challenger is
  // a watcher too — their own fighter reaches them the same way it reaches
  // the grandstand.
  *watchers(room) {
    yield* room.viewers;
    yield* room.agents;
    if (room.challenger) yield room.challenger;
  }

  status(room) {
    return { matchId: room.matchId, live: room.publisher?.readyState === 1,
      viewers: room.viewers.size, agents: room.agents.size,
      challenger: room.challenger?.readyState === 1,
      hasState: Boolean(room.state), updatedAt: room.updatedAt };
  }

  broadcastStatus(room) {
    const status = this.status(room);
    for (const watcher of this.watchers(room))
      send(watcher, "oskiewar:status", status);
  }

  prune() {
    const oldest = this.now() - ROOM_TTL_MS;
    for (const [matchId, room] of this.rooms) {
      if (!room.publisher && !room.challenger && room.viewers.size === 0 &&
          room.agents.size === 0 && room.updatedAt < oldest)
        this.rooms.delete(matchId);
    }
  }
}

export const OSKIEWAR_LIVE_LIMITS = Object.freeze({ MAX_MESSAGE_BYTES,
  MAX_VIEWERS, MAX_AGENTS, MAX_ROOMS, ROOM_TTL_MS, MIN_PUBLISH_INTERVAL_MS,
  MAX_INPUT_BYTES, MIN_INPUT_INTERVAL_MS });
