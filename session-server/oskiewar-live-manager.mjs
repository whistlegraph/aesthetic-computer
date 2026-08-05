// OSKIEWAR Live Manager, 26.08.04
// Small, public, match-id-scoped relay for phone spectators.

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
const MAX_ROOMS = 128;
const ROOM_TTL_MS = 10 * 60 * 1000;
const MIN_PUBLISH_INTERVAL_MS = 25;

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
    const role = url.searchParams.get("role") === "publisher" ? "publisher" : "viewer";
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
        viewers: new Set(), state: null, liveStarted: false,
        updatedAt: this.now(), publishedAt: 0 };
      this.rooms.set(matchId, room);
    }
    if (role === "publisher") this.addPublisher(room, ws, surface);
    else this.addViewer(room, ws, surface);
    return true;
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
      viewers: room.viewers.size, maxHz: Math.floor(1000 / MIN_PUBLISH_INTERVAL_MS) });
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
    send(room.publisher, "oskiewar:viewers", { count: room.viewers.size });
    const remove = () => {
      room.viewers.delete(ws);
      room.updatedAt = this.now();
      send(room.publisher, "oskiewar:viewers", { count: room.viewers.size });
    };
    ws.on("close", remove);
    ws.on("error", remove);
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
    for (const viewer of room.viewers) send(viewer, "oskiewar:state", state);
  }

  status(room) {
    return { matchId: room.matchId, live: room.publisher?.readyState === 1,
      viewers: room.viewers.size, hasState: Boolean(room.state),
      updatedAt: room.updatedAt };
  }

  broadcastStatus(room) {
    const status = this.status(room);
    for (const viewer of room.viewers) send(viewer, "oskiewar:status", status);
  }

  prune() {
    const oldest = this.now() - ROOM_TTL_MS;
    for (const [matchId, room] of this.rooms) {
      if (!room.publisher && room.viewers.size === 0 && room.updatedAt < oldest)
        this.rooms.delete(matchId);
    }
  }
}

export const OSKIEWAR_LIVE_LIMITS = Object.freeze({ MAX_MESSAGE_BYTES,
  MAX_VIEWERS, MAX_ROOMS, ROOM_TTL_MS, MIN_PUBLISH_INTERVAL_MS });
