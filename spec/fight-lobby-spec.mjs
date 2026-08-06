import assert from "node:assert/strict";
import { createFightLobby, inferFightRegion } from "../system/public/aesthetic.computer/lib/fight/lobby.mjs";
import { FIGHT_MANIFEST } from "../system/public/aesthetic.computer/lib/fight/protocol.mjs";

const sent = [];
const server = { send: (type, content) => sent.push({ type, content }) };
const lobby = createFightLobby("Alice", { region: "us-east" });
assert.equal(lobby.find(server), false);
lobby.authenticate(server, "secret", "request-1");
assert.equal(sent.at(-1).type, "fight:auth");
assert.equal(lobby.receive("fight:auth:ok", { handle: "@alice" }, server), true);
assert.equal(lobby.state.authenticated, true);
assert.equal(lobby.find(server), true);
assert.equal(sent.at(-1).type, "fight:queue:join");
assert.deepEqual(sent.at(-1).content.manifest, FIGHT_MANIFEST);
lobby.receive("fight:queue:state", { state: "queued", region: "us-east", joinedAt: 1 }, server);
assert.equal(lobby.state.status, "waiting");

let proposed = null;
const withCallbacks = createFightLobby("Alice", { callbacks: { onProposal: (data) => (proposed = data) } });
withCallbacks.receive("fight:auth:ok", { handle: "@alice" }, server);
withCallbacks.receive("fight:match:proposal", { matchId: "m1", manifest: FIGHT_MANIFEST,
  opponent: { handle: "@bob" }, seat: 0 }, server);
assert.equal(withCallbacks.state.status, "matched");
assert.equal(proposed.matchId, "m1");
assert.equal(inferFightRegion("Europe/Berlin"), "eu-central");
assert.equal(inferFightRegion("America/New_York"), "us-east");
assert.equal(lobby.receive("unrelated", {}), false);
console.log("fight lobby spec ok");
