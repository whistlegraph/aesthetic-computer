import assert from "node:assert/strict";
import { FightManager } from "../session-server/fight-manager.mjs";
import { FIGHT_MANIFEST } from "../system/public/aesthetic.computer/lib/fight/protocol.mjs";

let now = 1_000;
let serial = 0;
const sent = [];
const manager = new FightManager({ now: () => now, id: () => `id-${++serial}` });
manager.setSendFunction((wsId, type, content) => sent.push({ wsId, type, content }));

manager.authenticate("wa", { accountId: "a", handle: "@Alice" });
manager.authenticate("wb", { accountId: "b", handle: "@Bob" });
manager.authenticate("wc", { accountId: "c", handle: "@Carol" });
manager.queueJoin("wa", { manifest: FIGHT_MANIFEST, region: "us-west" });
manager.queueJoin("wb", { manifest: FIGHT_MANIFEST, region: "us-east" });
assert.equal(sent.filter((m) => m.type === "fight:match:proposal").length, 0);
manager.queueJoin("wc", { manifest: FIGHT_MANIFEST, region: "us-west" });
const proposals = sent.filter((m) => m.type === "fight:match:proposal");
assert.deepEqual(new Set(proposals.map((m) => m.wsId)), new Set(["wa", "wc"]));
assert.ok(!proposals.some((m) => m.wsId === "wb"));
const matchId = proposals[0].content.matchId;

sent.length = 0;
manager.signal("wa", { matchId, signal: { candidate: "private" } });
assert.deepEqual(sent.map((m) => m.wsId), ["wc"]);
assert.equal(sent[0].type, "fight:signal");

// Payload handles cannot impersonate a handled user; the server-bound socket wins.
manager.queueLeave("wb");
assert.equal(manager.queueJoin("guest", { handle: "@alice", manifest: FIGHT_MANIFEST, region: "us-west" }), false);
assert.equal(sent.at(-1).content.code, "auth-required");

// Adjacent-region expansion is controlled by queue age.
const expanded = new FightManager({ now: () => now, id: () => `wide-${++serial}` });
const expandedSent = [];
expanded.setSendFunction((wsId, type, content) => expandedSent.push({ wsId, type, content }));
expanded.authenticate("x", { accountId: "x", handle: "@x" });
expanded.authenticate("y", { accountId: "y", handle: "@y" });
expanded.queueJoin("x", { manifest: FIGHT_MANIFEST, region: "us-west" });
expanded.queueJoin("y", { manifest: FIGHT_MANIFEST, region: "us-east" });
assert.equal(expandedSent.filter((m) => m.type === "fight:match:proposal").length, 0);
now += 11_000;
expanded.matchAll();
assert.equal(expandedSent.filter((m) => m.type === "fight:match:proposal").length, 2);

// Re-authentication rebinds state and stale socket cleanup does not evict it.
expanded.authenticate("x2", { accountId: "x", handle: "@x" });
expanded.cleanup("x");
assert.equal(expanded.handleSockets.get("@x"), "x2");

// Guest VS rooms are scoped and do not require a public identity.
const rooms = new FightManager({ now: () => now, id: () => `room-${++serial}` });
const roomSent = [];
rooms.setSendFunction((wsId, type, content) => roomSent.push({ wsId, type, content }));
const room = rooms.roomCreate("guest-a", { manifest: FIGHT_MANIFEST });
rooms.roomJoin("guest-b", { code: room.code, region: "eu-west", manifest: FIGHT_MANIFEST });
assert.ok(roomSent.some((m) => m.wsId === "guest-a" && m.type === "fight:room:ready"));
assert.ok(roomSent.some((m) => m.wsId === "guest-b" && m.type === "fight:match:proposal"));

// Chat-originated handled challenges persist while the recipient moves from
// chat into Menu Fighter, then become a scoped match only on explicit accept.
const challenges = new FightManager({ now: () => now, id: () => `challenge-${++serial}` });
const challengeSent = [];
challenges.setSendFunction((wsId, type, content) => challengeSent.push({ wsId, type, content }));
challenges.authenticate("alice-fight", { accountId: "a", handle: "@alice" });
challenges.authenticate("bob-chat", { accountId: "b", handle: "@bob" });
challenges.challengeCreate("alice-fight", { target: "@bob", region: "us-west", manifest: FIGHT_MANIFEST });
const inviteId = challengeSent.find((m) => m.type === "fight:challenge:incoming").content.inviteId;
challenges.authenticate("bob-fight", { accountId: "b", handle: "@bob" });
challenges.cleanup("bob-chat");
assert.equal(challenges.challengeReply("bob-fight", {
  inviteId, accept: true, region: "us-east", manifest: FIGHT_MANIFEST,
}), true);
assert.deepEqual(new Set(challengeSent.filter((m) => m.type === "fight:match:proposal").map((m) => m.wsId)),
  new Set(["alice-fight", "bob-fight"]));

console.log("fight manager spec ok");
