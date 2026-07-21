import assert from "node:assert/strict";
import * as game from "../system/public/aesthetic.computer/lib/fight/sim.mjs";
import { createOnlineFight } from "../system/public/aesthetic.computer/lib/fight/online.mjs";

const control = [];
const bios = [];
const server = { send: (type, content) => control.push({ type, content }) };
const lobby = {
  state: { status: "matched", match: { matchId: "m1", seat: 0 }, route: null },
  acceptMatch(value) { value.send("fight:match:accept", { matchId: this.state.match.matchId }); },
  signal(value, signal) { value.send("fight:signal", signal); },
  routeReport(value, report) { value.send("fight:route:report", report); },
};

const online = createOnlineFight(game, { lobby, server, send: (message) => bios.push(message) });
const proposal = {
  matchId: "m1", seat: 0, seed: 7, initiator: true, probeNonce: "nonce-1",
};
online.proposal(proposal);
assert.equal(bios[0].type, "fight:rtc:create");
online.rtcEvent({ id: "m1", event: "health", data: {
  rttMs: 42, jitterMs: 3, packetLoss: 0.02, samples: 3,
} });
const report = control.find((message) => message.type === "fight:route:report");
assert.equal(report.content.direct.loss, 0.02);
assert.equal(report.content.direct.nonce, "nonce-1");

online.start();
online.rtcEvent({ id: "m1", event: "state", data: { state: "open" } });
assert.equal(lobby.state.status, "playing");

// A new connection opening cannot inherit the prior match's start signal.
online.close();
lobby.state.status = "matched";
lobby.state.match = { matchId: "m2", seat: 1 };
online.proposal({ ...proposal, matchId: "m2", seat: 1, probeNonce: "nonce-2" });
online.rtcEvent({ id: "m2", event: "state", data: { state: "open" } });
assert.equal(lobby.state.status, "matched");

online.close();
console.log("fight online spec ok");
