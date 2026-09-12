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
// How long a chair keeps a seat it has stopped talking from. A seated
// challenger is always speaking — a pad heartbeat every 250 ms on the streamed
// lane, an input packet every frame on the rollback one — so three seconds of
// nothing is a socket the far end already abandoned. It matters because a tab
// that is killed rather than closed sends no close frame, and the only other
// thing that would reap it is the shared 15 s ping sweep: two misses, so up to
// 30 s during which whoever just refreshed could not have their own chair
// back and was demoted to the grandstand instead.
const CHALLENGER_GHOST_MS = 3000;
// The rollback lane's own channel between the two seats: input frames with a
// few frames of redundancy, the match-start deal, and state hashes. Small,
// frequent, and never rate-limited — a dropped input packet is a rollback
// the other seat has to eat, so the relay forwards every one it is handed.
const MAX_NET_BYTES = 2048;
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

// Projectiles, as the flat number rows the game packs (see spectatorState).
// A watcher positions render objects straight off these, so they are bounded
// here the same way a fighter is: an exact row length, finite coordinates, an
// owner that is one of the two seats, and a flag word with no spare bits. The
// counts are the game's own caps — 24 rounds in the air, 12 lobs.
//
//   shot: x y z previousX previousY vx vy owner flags(spit|heavy|rubber)
//   lob:  x y z vx vy owner flags(rocket|exploding) blastRadius fuseMs
const projectileRows = (value, count, length, check) => {
  if (value === undefined) return true;
  return Array.isArray(value) && value.length <= count &&
    value.every((row) => Array.isArray(row) && row.length === length &&
      row.every((entry) => finite(entry, 10000000)) && check(row));
};

const shots = (value) => projectileRows(value, 24, 9, (row) =>
  integer(row[7], 0, 1) && integer(row[8], 0, 7));

const lobs = (value) => projectileRows(value, 12, 9, (row) =>
  integer(row[5], 0, 1) && integer(row[6], 0, 3) &&
  finite(row[7], 100000) && row[7] >= 0 && finite(row[8], 600000) && row[8] >= 0);

// The host's impact track:
//   mark: id x y z durationMs lifeMs flags(death|explosion)
// A watcher spawns render objects off these and keys duplicate suppression on
// the id, so the id has to be a real ascending integer, not any finite number.
const marks = (value) => projectileRows(value, 12, 7, (row) =>
  integer(row[0], 0, 2147483647) &&
  integer(row[4], 0, 60000) && integer(row[5], 0, 60000) &&
  integer(row[6], 0, 3));

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
      (value.round.elapsedMs !== undefined &&
        !integer(value.round.elapsedMs, 0, 3600000)) ||
      typeof value.round.result !== "string" || value.round.result.length > 80)
    return "Invalid round";
  if (!shots(value.shots)) return "Invalid shots";
  if (!lobs(value.lobs)) return "Invalid lobs";
  if (!marks(value.impacts)) return "Invalid impacts";
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

// A frame already serialized. The state fan-out is the one place in this file
// where the same bytes go to up to sixty-nine sockets, and stringifying the
// whole 2 KB payload once per watcher was most of the cost of a full room.
function sendRaw(ws, frame) {
  if (ws?.readyState !== 1) return false;
  try { ws.send(frame); return true; }
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
        challenger: null, challengerSeenAt: 0,
        viewers: new Set(), agents: new Set(), state: null,
        liveStarted: false, updatedAt: this.now(), publishedAt: 0,
        nudgedAt: 0, flaggedAt: 0, inputAt: 0, inputDown: null,
        flushTimer: null, flushedSeq: -1 };
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
    // A new publisher counts frames from its own zero, so the room lets go of
    // the old one's snapshot — otherwise the stale-frame guard in publish()
    // reads every frame of a claimed room as old until the successor
    // out-counts the dead host's sequence (measured: ~14 silent seconds after
    // each versus handoff, one for each second the old host had been live).
    // Dropping the snapshot also stops a mid-handoff spectator being handed a
    // freeze-frame of the previous fight, and stops the matchmaker offering a
    // chair beside a corpse.
    if (room.state) {
      room.state = null;
      room.publishedAt = 0;
    }
    // A frame still waiting on the trailing flush belongs to the host that
    // just left. It must not land on the room after the successor's first.
    clearTimeout(room.flushTimer);
    room.flushTimer = null;
    room.flushedSeq = -1;
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
      // And the frame it left behind goes with it. A room that keeps a dead
      // host's last state hands it to whoever opens the door next, who reads a
      // frozen `phase: "fight"` as a fight in progress — measured on ow-regga890,
      // where a host that had gone still answered `hasState: true` with a
      // seq 2857 frame of NOBODY vs NOBODY. A rejoining player is shown that
      // frame before anything else and takes their seat inside it. Takeover
      // already dropped the snapshot for exactly this reason; leaving is the
      // same event with nobody arriving to trigger it.
      room.state = null;
      room.publishedAt = 0;
      clearTimeout(room.flushTimer);
      room.flushTimer = null;
      room.flushedSeq = -1;
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
    // A chair is held by whoever is still speaking from it. An open socket is
    // not that: a killed tab sends no close frame, so the seat it left stays
    // technically occupied until the shared ping sweep reaps it two misses
    // later — and for that half minute the person who just refreshed is told
    // the chair is taken and demoted to the grandstand. They cannot rejoin as
    // themselves because the relay is still holding their own dead socket
    // against them. So a silent incumbent is evicted for the newcomer; a
    // playing one is not, and the newcomer still gets 4409 and stays to watch.
    const ghost = room.challenger &&
      this.now() - room.challengerSeenAt > CHALLENGER_GHOST_MS;
    if (room.challenger?.readyState === 1 && !ghost) {
      send(ws, "oskiewar:error", { message: "This match already has a challenger" });
      ws.close?.(4409, "Challenger already seated");
      return;
    }
    const departing = room.challenger;
    room.challenger = ws;
    // Stamped before the old socket is closed, so its `remove` handler — which
    // only fires for the seat it still holds — cannot clear the new one.
    room.challengerSeenAt = this.now();
    if (departing && departing !== ws) departing.close?.(4410, "Chair reclaimed");
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
    // Every word from the chair, before any shape check: a packet this relay
    // goes on to drop is still proof somebody is sitting there.
    room.challengerSeenAt = this.now();
    const bytes = Buffer.byteLength(data);
    if (bytes > MAX_NET_BYTES) return;
    let message;
    try { message = JSON.parse(data.toString()); } catch { return; }
    if (message.type === "oskiewar:net") return this.relayNet(room, ws, message);
    if (bytes > MAX_INPUT_BYTES) return;
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
    // The 15 ms floor exists to pace a stick, which moves continuously. A
    // button edge is a discrete event and must never be the thing it drops:
    // the client marks a frame as sent the moment the socket takes it, so a
    // press the relay swallowed here was invisible to both ends until the
    // next change or the idle heartbeat.
    const buttons = input.down.join(" ");
    const held = buttons === room.inputDown;
    if (held && now - room.inputAt < MIN_INPUT_INTERVAL_MS) return;
    room.inputDown = buttons;
    room.inputAt = now;
    room.updatedAt = now;
    send(room.publisher, "oskiewar:input", { seq: input.seq,
      down: input.down, leftX: input.leftX, leftY: input.leftY,
      name: typeof input.name === "string" ? input.name : "",
      colors: Array.isArray(input.colors) ? input.colors : [] });
  }

  // Seat to seat and nowhere else: a packet from the publisher reaches the
  // challenger, a packet from the challenger reaches the publisher, and the
  // grandstand never hears either. The relay checks only that the payload is
  // a small JSON object; what the two games say to each other is theirs.
  relayNet(room, ws, message) {
    const content = message.content;
    if (!content || typeof content !== "object" || Array.isArray(content)) return;
    const target = ws === room.publisher ? room.challenger
      : ws === room.challenger ? room.publisher : null;
    if (!target) return;
    room.updatedAt = this.now();
    send(target, "oskiewar:net", content);
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
    if (message.type === "oskiewar:net") {
      if (Buffer.byteLength(data) <= MAX_NET_BYTES) this.relayNet(room, ws, message);
      return;
    }
    if (message.type !== "oskiewar:state") return;
    let state = message.content;
    if (typeof state === "string") {
      try { state = JSON.parse(state); } catch { state = null; }
    }
    const invalid = validateOskiewarLiveState(state);
    if (invalid) return send(ws, "oskiewar:error", { message: invalid });
    const now = this.now();
    if (room.state && state.seq <= room.state.seq) return;
    room.state = state;
    room.updatedAt = now;
    // The 25 ms floor coalesces a burst. It must not throw the burst away,
    // and it used to: the gate ran BEFORE the store, so of two frames landing
    // in one instant — which is exactly what a host's catch-up tick emits —
    // the older one went out to the room and the newer one was gone for good.
    // Measured against the real relay that was a quarter of all frames on a
    // wired link and well over half on a busy one, and it fell hardest at the
    // moments the fight was busiest. Now the newest frame is always the one
    // kept, and a trailing flush hands it over a few milliseconds late.
    // Coalescing is allowed to cost latency. That is what it is for. It is
    // not allowed to cost information.
    if (now - room.publishedAt < MIN_PUBLISH_INTERVAL_MS) {
      this.armFlush(room);
      return;
    }
    this.flush(room);
  }

  // One trailing timer per room, never a queue of them: whatever is newest
  // when it fires is what the room gets. Unreferenced, because a pending
  // frame is not a reason to keep a process alive.
  armFlush(room) {
    if (room.flushTimer) return;
    room.flushTimer = setTimeout(() => {
      room.flushTimer = null;
      if (this.rooms.get(room.matchId) === room) this.flush(room);
    }, MIN_PUBLISH_INTERVAL_MS);
    room.flushTimer.unref?.();
  }

  flush(room) {
    const state = room.state;
    if (!state || state.seq === room.flushedSeq) return;
    room.flushedSeq = state.seq;
    room.publishedAt = this.now();
    if (!room.liveStarted) {
      room.liveStarted = true;
      this.analytics.capture("live_started", {
        source_system: "session-server",
        surface: room.publisherSurface,
        phase: state.phase,
      });
    }
    const frame = JSON.stringify({ type: "oskiewar:state", content: state });
    for (const watcher of this.watchers(room)) sendRaw(watcher, frame);
  }

  // Agents read the frame numbers out of the same state payload a phone gets,
  // so the fan-out is one list; only the counting is split. The challenger is
  // a watcher too — their own fighter reaches them the same way it reaches
  // the grandstand.
  *watchers(room) {
    // The challenger first. They are playing the fight; everyone after them
    // is watching it, and a seat that waited behind sixty-four spectators for
    // its own fighter was paying for their view.
    if (room.challenger) yield room.challenger;
    yield* room.viewers;
    yield* room.agents;
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

  // The bare front door's matchmaker: one room with a live publisher, an
  // empty second chair, and an untimed round — the timed rounds are the
  // recorded broadcast farm, and a visitor dropped into one would be watching
  // television, not taking a chair. Freshness is required on top of the open
  // socket because a wedged host can hold a connection long after its last
  // frame; of the rooms left standing, the most recently published one wins.
  openRoom() {
    this.prune();
    let open = null;
    for (const room of this.rooms.values()) {
      if (room.publisher?.readyState !== 1) continue;
      if (room.challenger?.readyState === 1) continue;
      if (room.state?.round?.timed !== false) continue;
      if (this.now() - room.publishedAt > 10000) continue;
      if (!open || room.publishedAt > open.publishedAt) open = room;
    }
    return open
      ? { matchId: open.matchId, room: open.matchId.replace(/^ow-/, "") }
      : null;
  }

  prune() {
    const oldest = this.now() - ROOM_TTL_MS;
    for (const [matchId, room] of this.rooms) {
      if (!room.publisher && !room.challenger && room.viewers.size === 0 &&
          room.agents.size === 0 && room.updatedAt < oldest) {
        clearTimeout(room.flushTimer);
        room.flushTimer = null;
        this.rooms.delete(matchId);
      }
    }
  }
}

export const OSKIEWAR_LIVE_LIMITS = Object.freeze({ MAX_MESSAGE_BYTES,
  MAX_VIEWERS, MAX_AGENTS, MAX_ROOMS, ROOM_TTL_MS, MIN_PUBLISH_INTERVAL_MS,
  MAX_INPUT_BYTES, MIN_INPUT_INTERVAL_MS, MAX_NET_BYTES,
  CHALLENGER_GHOST_MS });
