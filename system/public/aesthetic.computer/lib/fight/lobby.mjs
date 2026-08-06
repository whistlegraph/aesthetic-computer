// Client state machine for authenticated public matchmaking and guest VS rooms.

import { FIGHT_MANIFEST, compatibleManifest } from "./protocol.mjs";
import { normalizeFightRegion } from "./regions.mjs";

function unpack(content) {
  if (typeof content !== "string") return content || {};
  try { return JSON.parse(content); } catch { return {}; }
}

export function inferFightRegion(timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone) {
  const zone = String(timeZone || "").toLowerCase();
  if (zone.startsWith("europe/")) return zone.includes("berlin") || zone.includes("warsaw") || zone.includes("vienna") ? "eu-central" : "eu-west";
  if (zone.includes("new_york") || zone.includes("detroit") || zone.includes("toronto")) return "us-east";
  return "us-west";
}

export function createFightLobby(handle = null, { region = inferFightRegion(), platform = "web", callbacks = {} } = {}) {
  const state = {
    handle: handle ? `@${String(handle).replace(/^@/, "")}` : null,
    authenticated: false,
    status: handle ? "authenticating" : "signed-out",
    region: normalizeFightRegion(region),
    platform,
    queueSince: null,
    opponent: null,
    match: null,
    route: null,
    health: null,
    room: null,
    error: null,
    invites: [],
    queued: [],
  };

  const api = {
    state,
    authenticate(server, token, requestId = globalThis.crypto?.randomUUID?.() || `${Date.now()}`) {
      if (!token) { state.status = "signed-out"; return false; }
      state.status = "authenticating";
      server?.send("fight:auth", { token, requestId });
      return true;
    },
    find(server, { mode = "casual" } = {}) {
      if (!state.authenticated) { state.status = "signed-out"; return false; }
      state.status = "queueing";
      state.error = null;
      server?.send("fight:queue:join", { manifest: FIGHT_MANIFEST, region: state.region, platform, mode });
      return true;
    },
    cancel(server) { server?.send("fight:queue:leave", {}); state.status = "ready"; },
    createRoom(server) { server?.send("fight:room:create", { manifest: FIGHT_MANIFEST, region: state.region }); },
    joinRoom(server, code) { server?.send("fight:room:join", { code, manifest: FIGHT_MANIFEST, region: state.region }); },
    challenge(server, target) {
      if (!state.authenticated) return false;
      server?.send("fight:challenge:create", {
        target, manifest: FIGHT_MANIFEST, region: state.region, platform,
      });
      return true;
    },
    replyChallenge(server, inviteId, accept) {
      if (!state.authenticated || !inviteId) return false;
      server?.send("fight:challenge:reply", {
        inviteId, accept: !!accept, manifest: FIGHT_MANIFEST,
        region: state.region, platform,
      });
      return true;
    },
    acceptMatch(server) {
      if (!state.match) return false;
      server?.send("fight:match:accept", { matchId: state.match.matchId, manifest: FIGHT_MANIFEST });
      return true;
    },
    signal(server, signal) {
      if (state.match) server?.send("fight:signal", { matchId: state.match.matchId, signal });
    },
    routeReport(server, report) {
      if (state.match) server?.send("fight:route:report", { matchId: state.match.matchId, ...report });
    },
    receive(type, content, server) {
      const data = unpack(content);
      if (type === "fight:auth:ok") {
        state.authenticated = true; state.handle = data.handle; state.status = "ready"; state.error = null;
      } else if (type === "fight:queue:state") {
        state.status = data.state === "queued" ? "waiting" : "ready";
        state.queueSince = data.joinedAt || null; state.region = data.region || state.region;
      } else if (type === "fight:match:proposal") {
        if (!compatibleManifest(data.manifest, FIGHT_MANIFEST)) {
          state.status = "error"; state.error = "incompatible build"; return true;
        }
        state.match = data; state.opponent = data.opponent; state.status = "matched";
        callbacks.onProposal?.(data, api, server);
      } else if (type === "fight:match:start") {
        state.status = "connecting"; callbacks.onStart?.(data, api, server);
      } else if (type === "fight:signal") {
        if (data.matchId === state.match?.matchId) callbacks.onSignal?.(data.signal, api, server);
      } else if (type === "fight:route:selected") {
        state.route = data.route; callbacks.onRoute?.(data.route);
      } else if (type === "fight:room:created" || type === "fight:room:ready") {
        state.room = data; callbacks.onRoom?.(data);
      } else if (type === "fight:challenge:incoming") {
        state.invites.push(data); callbacks.onInvite?.(data);
      } else if (type === "fight:challenge:sent") {
        state.status = "challenging"; callbacks.onChallengeSent?.(data);
      } else if (type === "fight:challenge:declined") {
        state.status = "ready"; state.error = `${data.target || "opponent"} declined`;
      } else if (type === "fight:lobby:snapshot") {
        state.queued = data.queued || [];
      } else if (type === "fight:match:peer-left") {
        state.status = "error"; state.error = "opponent disconnected";
      } else if (type === "fight:error") {
        state.status = "error"; state.error = data.message || data.code || "network error";
      } else return false;
      callbacks.onChange?.(state);
      return true;
    },
    leave(server) {
      if (state.status === "waiting") server?.send("fight:queue:leave", {});
      state.status = state.authenticated ? "ready" : "signed-out";
    },
  };
  return api;
}
