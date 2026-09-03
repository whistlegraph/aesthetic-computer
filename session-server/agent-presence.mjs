// Agent Presence, 26.09.03
// Tiny public relay: an AC surface announces itself under its handle's room,
// an agent attaches to the same room, and both sides hear who is in it. This
// lights the little agent mark on the aesthetic.computer interface — the same
// awareness oskiewar's telemetry antenna gives its debug bug, platform-wide.
// Presence only: no state rides through here, so the room cost stays a set of
// sockets and a label each.

const ROOM_NAME = /^[a-z0-9_-]{1,32}$/;
const LABEL = /^[a-z0-9 _-]{1,24}$/i;
const MAX_AGENTS = 4;
const MAX_SURFACES = 16;
const MAX_ROOMS = 256;
const ROOM_TTL_MS = 10 * 60 * 1000;
const MAX_MESSAGE_BYTES = 512;

export function canonicalRoom(value) {
  const raw = String(value || "").toLowerCase();
  const name = raw.startsWith("@") ? raw.slice(1) : raw;
  return ROOM_NAME.test(name) ? name : null;
}

function send(ws, type, content) {
  if (ws?.readyState !== 1) return false;
  try { ws.send(JSON.stringify({ type, content })); return true; }
  catch { return false; }
}

export class AgentPresenceManager {
  constructor({ now = () => Date.now() } = {}) {
    this.rooms = new Map();
    this.now = now;
  }

  accepts(url) {
    try { return new URL(url, "http://session").pathname === "/agent-presence"; }
    catch { return false; }
  }

  handleConnection(ws, req) {
    if (!this.accepts(req?.url)) return false;
    // This route returns before the ordinary game-socket heartbeat setup.
    ws.isAlive = true;
    ws.on("pong", () => { ws.isAlive = true; });
    const url = new URL(req.url, "http://session");
    const roomName = canonicalRoom(url.searchParams.get("room"));
    if (!roomName) {
      send(ws, "agent-presence:error", { message: "Invalid room" });
      ws.close?.(4400, "Invalid room");
      return true;
    }
    const role = url.searchParams.get("role") === "agent" ? "agent" : "surface";
    const rawLabel = url.searchParams.get("label") || role;
    const label = LABEL.test(rawLabel) ? rawLabel : role;

    this.prune();
    let room = this.rooms.get(roomName);
    if (!room) {
      if (this.rooms.size >= MAX_ROOMS) {
        send(ws, "agent-presence:error", { message: "Presence capacity reached" });
        ws.close?.(4429, "Capacity reached");
        return true;
      }
      room = { name: roomName, agents: new Map(), surfaces: new Map(),
        updatedAt: this.now(), helloAt: 0 };
      this.rooms.set(roomName, room);
    }

    const members = role === "agent" ? room.agents : room.surfaces;
    const cap = role === "agent" ? MAX_AGENTS : MAX_SURFACES;
    if (members.size >= cap) {
      send(ws, "agent-presence:error", { message: `Room full for ${role}s` });
      ws.close?.(4429, "Capacity reached");
      return true;
    }

    members.set(ws, { label });
    room.updatedAt = this.now();
    send(ws, "agent-presence:status", this.status(room));
    this.broadcast(room);

    ws.on("message", (data) => this.hello(room, ws, data));
    const remove = () => {
      members.delete(ws);
      room.updatedAt = this.now();
      this.broadcast(room);
    };
    ws.on("close", remove);
    ws.on("error", remove);
    return true;
  }

  // An agent may rename itself once connected; nothing else rides through.
  hello(room, ws, data) {
    if (Buffer.byteLength(data) > MAX_MESSAGE_BYTES) return;
    let message;
    try { message = JSON.parse(data.toString()); } catch { return; }
    if (message.type !== "agent-presence:hello") return;
    const entry = room.agents.get(ws) || room.surfaces.get(ws);
    if (!entry) return;
    const label = message.content?.label;
    if (typeof label !== "string" || !LABEL.test(label)) return;
    const now = this.now();
    if (room.helloAt && now - room.helloAt < 1000) return;
    room.helloAt = now;
    entry.label = label;
    this.broadcast(room);
  }

  status(room) {
    return {
      room: room.name,
      agents: [...room.agents.values()].map((entry) => entry.label),
      surfaces: room.surfaces.size,
      updatedAt: room.updatedAt,
    };
  }

  broadcast(room) {
    const status = this.status(room);
    for (const ws of [...room.agents.keys(), ...room.surfaces.keys()])
      send(ws, "agent-presence:status", status);
  }

  prune() {
    const oldest = this.now() - ROOM_TTL_MS;
    for (const [name, room] of this.rooms) {
      if (room.agents.size === 0 && room.surfaces.size === 0 &&
          room.updatedAt < oldest)
        this.rooms.delete(name);
    }
  }
}

export const AGENT_PRESENCE_LIMITS = Object.freeze({
  MAX_AGENTS, MAX_SURFACES, MAX_ROOMS, ROOM_TTL_MS, MAX_MESSAGE_BYTES,
});
