import assert from "node:assert/strict";
import { allowedOpponentRegions, sanitizeProbeSample, selectFightRoute } from
  "../system/public/aesthetic.computer/lib/fight/regions.mjs";

assert.deepEqual(allowedOpponentRegions("us-west", 0), ["us-west"]);
assert.ok(allowedOpponentRegions("us-west", 11_000).includes("us-east"));
assert.equal(sanitizeProbeSample({ nonce: "wrong", rttMs: 1, jitterMs: 1, loss: 0, samples: 4 }, "right"), null);
assert.equal(sanitizeProbeSample({ nonce: "n", rttMs: -1, jitterMs: 1, loss: 0, samples: 4 }, "n"), null);
const sample = (rttMs, nonce = "n") => ({ nonce, rttMs, jitterMs: 3, loss: 0.01, samples: 10 });
assert.equal(selectFightRoute({ 0: { direct: sample(40) }, 1: { direct: sample(44) } }).type, "direct");
const relay = selectFightRoute({
  0: { direct: null, relays: { "us-west": sample(30), "us-east": sample(70) } },
  1: { direct: null, relays: { "us-west": sample(90), "us-east": sample(45) } },
});
assert.deepEqual({ type: relay.type, region: relay.region }, { type: "relay", region: "us-east" });
assert.equal(selectFightRoute({ 0: { relays: {} }, 1: { relays: {} } }).type, "unavailable");
console.log("fight regions spec ok");

