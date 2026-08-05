import assert from "node:assert/strict";
import test from "node:test";
import { createPostHogEventCapture } from "../../shared/posthog-event-capture.mjs";
import {
  oskiewarEvent,
  oskiewarMatchCompleted,
  oskiewarReplayProperties,
} from "../public/aesthetic.computer/lib/oskiewar-analytics.mjs";

test("OSKIEWAR events discard identifiers, handles, scores, and content", () => {
  assert.deepEqual(
    oskiewarEvent("round_stored", {
      source_system: "lith",
      surface: "xbox",
      opponent_type: "dummy",
      duration_bucket: "30_59s",
      match_id: "ow-private-room",
      fighter: "@PRIVATE",
      score: 99,
      content: "private replay",
    }),
    {
      event: "ac_oskiewar_round_stored",
      properties: {
        duration_bucket: "30_59s",
        opponent_type: "dummy",
        source_system: "lith",
        surface: "xbox",
      },
    },
  );
  assert.equal(oskiewarEvent("unknown", {}), null);
});

test("replay properties reduce a demo to bounded categories", () => {
  const demo = {
    fighters: ["@PRIVATE", "dummy"],
    durationTicks: 1800,
    roundIndex: 2,
    winner: "@PRIVATE",
    finalRoundWins: [5, 1],
  };
  assert.deepEqual(oskiewarReplayProperties(demo, "xbox"), {
    source_system: "lith",
    surface: "xbox",
    opponent_type: "dummy",
    round_position: "followup",
    duration_bucket: "30_59s",
    result: "win",
  });
  assert.equal(oskiewarMatchCompleted(demo), true);
});

test("server events are opt-in, aggregated, and personless", async () => {
  const requests = [];
  const analytics = createPostHogEventCapture({
    projectToken: "phc_public_test_token",
    enabled: true,
    distinctId: "ac-oskiewar-test-aggregate",
    eventFactory: oskiewarEvent,
    flushIntervalMs: 60_000,
    fetchImpl: async (url, options) => {
      requests.push({ url, options });
      return { ok: true, status: 200 };
    },
  });
  analytics.capture("spectator_joined", {
    source_system: "session-server",
    surface: "web",
    viewer_state: "live",
  });
  analytics.capture("spectator_joined", {
    source_system: "session-server",
    surface: "web",
    viewer_state: "live",
  });
  assert.equal(await analytics.flush(), true);
  analytics.stop();

  const body = JSON.parse(requests[0].options.body);
  assert.equal(body.batch.length, 1);
  assert.equal(body.batch[0].properties.count, 2);
  assert.equal(body.batch[0].distinct_id, "ac-oskiewar-test-aggregate");
  assert.equal(body.batch[0].properties.$process_person_profile, false);
  assert.equal(body.batch[0].properties.$geoip_disable, true);
});
