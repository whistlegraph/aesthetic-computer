// World Manager Tests, 2026.06.09
// Covers the arena→WorldManager generalization: per-world rooms, prefixed
// events, member-scoped broadcasts (the old `everyone()` lobby leak), cmd
// integration via pmove, takeover/reconnect, and snapshot delivery.

import { describe, it, expect, afterEach } from "vitest";
import { WorldManager, ARENA_CFG, ARENA_SPAWNS } from "../world-manager.mjs";
import {
  LAND_CFG,
  LAND_SPAWNS,
} from "../../system/public/aesthetic.computer/lib/land-world.mjs";
import { packCmd } from "../../system/public/aesthetic.computer/lib/pmove.mjs";

const managers = [];

function mockWorld(opts) {
  const sent = []; // every sendWS call: { wsId, type, content }
  const wm = new WorldManager(opts);
  wm.setSendFunctions({
    sendUDP: () => false, // force the WS path so `sent` sees snaps
    sendWS: (wsId, type, content) => sent.push({ wsId, type, content }),
    resolveUdpForHandle: () => null,
    isLive: () => true,
  });
  managers.push(wm);
  return { wm, sent };
}

function land() {
  return mockWorld({ prefix: "land", cfg: LAND_CFG, spawns: LAND_SPAWNS, icon: "🌾" });
}
function arena() {
  return mockWorld({ prefix: "arena", cfg: ARENA_CFG, spawns: ARENA_SPAWNS, icon: "🏟️" });
}

afterEach(() => {
  // Kill any tick loops started by playerJoin so vitest can exit.
  for (const wm of managers) {
    if (wm.tickInterval) { clearInterval(wm.tickInterval); wm.tickInterval = null; }
  }
  managers.length = 0;
});

describe("WorldManager (land)", () => {
  it("welcomes with land-prefixed events and the meadow cfg", () => {
    const { wm, sent } = land();
    wm.playerJoin("alice", 1);
    const welcome = sent.find((m) => m.type === "land:welcome");
    expect(welcome).toBeDefined();
    expect(welcome.wsId).toBe(1);
    expect(welcome.content.you).toBe("alice");
    expect(welcome.content.cfg.runSpeed).toBe(8);
    expect(welcome.content.cfg.gravity).toBe(40);
    expect(welcome.content.cfg.groundBounds.xMax).toBe(24);
  });

  it("broadcasts join/leave to world members only (no lobby leak)", () => {
    const { wm, sent } = land();
    wm.playerJoin("alice", 1);
    sent.length = 0;
    wm.playerJoin("bob", 2);
    // alice (ws 1) and bob (ws 2) hear about bob; nobody else can — the
    // manager only knows member wsIds, there is no everyone() hook anymore.
    const joins = sent.filter((m) => m.type === "land:join");
    expect(joins.map((m) => m.wsId).sort()).toEqual([1, 2]);
    sent.length = 0;
    wm.playerLeave("bob");
    const leaves = sent.filter((m) => m.type === "land:leave");
    expect(leaves.map((m) => m.wsId)).toEqual([1]);
  });

  it("integrates usercmds through pmove and moves the player", () => {
    const { wm } = land();
    wm.playerJoin("alice", 1);
    const rec = wm.players.get("alice");
    const x0 = rec.state.x, z0 = rec.state.z;
    // Two forward cmds 50ms apart (yaw 0 → straight line).
    wm.receiveCmd("alice", {
      firstSeq: 1,
      ack: 0,
      cmds: [
        packCmd({ ms: 50, fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0 }),
        packCmd({ ms: 100, fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0 }),
      ],
    });
    const moved = Math.hypot(rec.state.x - x0, rec.state.z - z0);
    expect(moved).toBeGreaterThan(0);
    expect(rec.lastCmdSeq).toBe(2);
  });

  it("delivers per-client snapshots to every player", () => {
    const { wm, sent } = land();
    wm.playerJoin("alice", 1);
    wm.playerJoin("bob", 2);
    sent.length = 0;
    wm.broadcastSnapshots();
    const snaps = sent.filter((m) => m.type === "land:snap");
    expect(snaps.map((m) => m.wsId).sort()).toEqual([1, 2]);
    for (const s of snaps) {
      expect(s.content.players.map((p) => p.h).sort()).toEqual(["alice", "bob"]);
      expect(s.content.deltaNum).toBe(0); // nothing acked yet → full snap
    }
  });

  it("takeover: same handle from a live second tab demotes the first", () => {
    const { wm, sent } = land();
    wm.playerJoin("alice", 1);
    sent.length = 0;
    wm.playerJoin("alice", 2); // second tab, ws 1 still live
    const takeover = sent.find((m) => m.type === "land:takeover");
    expect(takeover).toBeDefined();
    expect(takeover.wsId).toBe(1);
    expect(wm.players.get("alice").wsId).toBe(2);
    // Old tab keeps receiving snaps as a probe.
    sent.length = 0;
    wm.broadcastSnapshots();
    expect(sent.some((m) => m.type === "land:snap" && m.wsId === 1)).toBe(true);
  });

  it("reload race: close of the OLD wsId doesn't evict the rebound player", () => {
    const { wm } = land();
    wm.playerJoin("alice", 1);
    wm.playerJoin("alice", 2);     // rebind (new tab/reload)
    wm.playerLeave("alice", 1);    // old socket's close handler fires late
    expect(wm.players.has("alice")).toBe(true);
    wm.playerLeave("alice", 2);    // real leave
    expect(wm.players.has("alice")).toBe(false);
  });
});

describe("WorldManager (multi-world isolation)", () => {
  it("arena and land rosters are independent", () => {
    const a = arena();
    const l = land();
    a.wm.playerJoin("fighter", 1);
    l.wm.playerJoin("wanderer", 2);
    expect([...a.wm.players.keys()]).toEqual(["fighter"]);
    expect([...l.wm.players.keys()]).toEqual(["wanderer"]);
    // No cross-talk: land's sends never carry arena: types and vice versa.
    expect(a.sent.every((m) => m.type.startsWith("arena:"))).toBe(true);
    expect(l.sent.every((m) => m.type.startsWith("land:"))).toBe(true);
  });

  it("worlds use their own physics cfg", () => {
    const a = arena();
    const l = land();
    a.wm.playerJoin("fighter", 1);
    l.wm.playerJoin("wanderer", 2);
    expect(a.wm.cfg.runSpeed).toBe(10);
    expect(l.wm.cfg.runSpeed).toBe(8);
    expect(l.wm.cfg.obstacles.some((o) => o.tree)).toBe(true);
  });
});
