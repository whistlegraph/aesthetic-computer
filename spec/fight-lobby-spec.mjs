import assert from "node:assert/strict";
import { FightManager } from "../session-server/fight-manager.mjs";
import { createFightLobby } from "../system/public/aesthetic.computer/lib/fight/lobby.mjs";

const sent = [];
const manager = new FightManager();
manager.setSendFunction((wsId, type, content) => sent.push({ wsId, type, content }));
manager.join("@a", "wa");
manager.join("@b", "wb");
manager.join("@c", "wc");
assert.deepEqual(manager.snapshot("@a").players, ["@a", "@b"]);
assert.equal(manager.snapshot("@a").seat, 0);
assert.equal(manager.snapshot("@c").role, "spectator");
assert.equal(manager.snapshot("@c").count, 3);

// A stale socket close after a reload must not evict the new connection.
manager.join("@a", "wa2");
manager.leave("@a", "wa");
assert.equal(manager.snapshot("@a").seat, 0);
manager.leave("@a", "wa2");
assert.deepEqual(manager.snapshot("@b").players, ["@b", "@c"]);

const lobby = createFightLobby("@b");
assert.equal(lobby.receive("fight:roster", manager.snapshot("@b")), true);
assert.equal(lobby.state.count, 2);
assert.equal(lobby.state.seat, 0);
assert.equal(lobby.receive("unrelated", {}), false);
assert.ok(sent.some((message) => message.type === "fight:joined"));
console.log("fight lobby spec ok");
