import test from "node:test";
import assert from "node:assert/strict";
import {
  createEndpointAnalytics,
  durationBucket,
  endpointAggregate,
  lithSurfaceAggregate,
  statusClass,
} from "../../lith/product-analytics.mjs";

test("endpoint dimensions are bounded and contain no request data", () => {
  assert.equal(durationBucket(99), "under_100ms");
  assert.equal(durationBucket(2000), "2000ms_or_more");
  assert.equal(statusClass(204), "2xx");
  assert.deepEqual(endpointAggregate("device-token", 120, 201, "post"), {
    endpoint: "device-token",
    method: "POST",
    status_class: "2xx",
    duration_bucket: "100_499ms",
    analytics_class: "auth-account",
  });
});

test("Lith-native routes map to static names without resource parameters", () => {
  assert.deepEqual(
    lithSurfaceAggregate("/media/@private/file.png", 80, 200, "get"),
    {
      endpoint: "lith-media",
      method: "GET",
      status_class: "2xx",
      duration_bucket: "under_100ms",
      analytics_class: "content-media",
    },
  );
  assert.equal(lithSurfaceAggregate("/lith/deploy", 80, 200, "post"), null);
  assert.equal(
    lithSurfaceAggregate("/local-upload/private.txt", 80, 200, "put"),
    null,
  );
});

test("private and operational endpoint classes do not emit aggregates", () => {
  assert.equal(endpointAggregate("chat-messages", 10, 200, "get"), null);
  assert.equal(endpointAggregate("machine-logs", 10, 200, "get"), null);
  assert.equal(endpointAggregate("piece-log", 10, 200, "post"), null);
  assert.equal(
    endpointAggregate("new-unreviewed-handler", 10, 200, "get"),
    null,
  );
});

test("server capture is opt-in, batched, anonymous, and payload-free", async () => {
  const requests = [];
  const analytics = createEndpointAnalytics({
    projectToken: "phc_public_test_token",
    enabled: true,
    flushIntervalMs: 60_000,
    fetchImpl: async (url, options) => {
      requests.push({ url, options });
      return { ok: true, status: 200 };
    },
  });

  assert.equal(analytics.capture("device-token", 120, 201, "post"), true);
  assert.equal(analytics.capture("device-token", 120, 201, "post"), true);
  assert.equal(analytics.capture("chat-messages", 20, 200, "get"), false);
  assert.equal(
    analytics.captureSurface("/frame/private-piece", 20, 200, "get"),
    true,
  );
  assert.equal(await analytics.flush(), true);
  analytics.stop();

  assert.equal(requests.length, 1);
  const body = JSON.parse(requests[0].options.body);
  assert.equal(body.batch.length, 2);
  const endpointEvent = body.batch.find(
    (event) => event.properties.endpoint === "device-token",
  );
  assert.equal(endpointEvent.properties.count, 2);
  assert.equal(body.batch[0].properties.$process_person_profile, false);
  assert.equal(body.batch[0].properties.$geoip_disable, true);
  assert.equal(Object.hasOwn(body.batch[0].properties, "path"), false);
  assert.equal(Object.hasOwn(body.batch[0].properties, "error"), false);
  assert.equal(Object.hasOwn(body.batch[0].properties, "payload"), false);
});

test("invalid or disabled configuration stays inert", () => {
  assert.equal(createEndpointAnalytics({ enabled: false }).active, false);
  assert.equal(
    createEndpointAnalytics({
      enabled: true,
      projectToken: "phc_test",
      apiHost: "https://example.com",
    }).active,
    false,
  );
});
