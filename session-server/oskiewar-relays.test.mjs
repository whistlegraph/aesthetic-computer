import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { OSKIEWAR_RELAYS } from "../xbox/live/round-room.mjs";

// Clients choose among OSKIEWAR_RELAYS; deploys ship relays.json. A relay in
// one list and not the other is either a box nobody can reach or a host the
// clients dial that no deploy keeps current.
test("the clients' relays are exactly the deployed relays", () => {
  const deployed = JSON.parse(readFileSync(new URL("./relay/relays.json", import.meta.url), "utf8")).relays;
  const key = (relay) => relay.region + " " + relay.host;
  assert.deepEqual(OSKIEWAR_RELAYS.map(key).sort(), deployed.map(key).sort());
});
