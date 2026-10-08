import { test } from "node:test";
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";
import { collectPostHog, postHogIdentity, postHogQueries, postHogSignals } from "../posthog.mjs";
import { pieceProperties } from "../../../system/public/aesthetic.computer/lib/product-analytics.mjs";

const settings = { organizationId: "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", projectId: 123 };
const browser = { projectToken: "phc_fixture", uiHost: "https://us.posthog.com" };
const reply = value => new Response(typeof value === "string" ? value : JSON.stringify(value));

test("PostHog never reads with capture credentials or an unbound project", async () => {
  const fetchImpl = () => { throw new Error("Unexpected request"); };
  const missing = await collectPostHog("/unused", {}, { fetchImpl, env: { POSTHOG_PROJECT_TOKEN: "capture-only" } });
  assert.equal(missing.coverage[0].status, "unavailable");
  assert.equal(missing.coverage[0].reason, "missing-read-credential");
  const unbound = await collectPostHog("/unused", {}, { fetchImpl, env: { AESPATCHER_POSTHOG_READ_KEY: "private" } });
  assert.equal(unbound.coverage[0].reason, "missing-project-binding");
});

test("PostHog verifies the selected project against AC production before queries", async () => {
  const calls = [];
  const fetchImpl = async (url, options) => {
    calls.push({ url, options });
    if (url === "https://aesthetic.computer/api/product-analytics-config") return reply(browser);
    return reply({ id: 123, organization: settings.organizationId, api_token: browser.projectToken });
  };
  assert.equal(await postHogIdentity(settings, "private", fetchImpl), browser.uiHost);
  assert.equal(calls[0].options.headers, undefined);
  assert.equal(calls[1].options.headers.Authorization, "Bearer private");
  assert.ok(calls.every(call => call.options.redirect === "error"));
  await assert.rejects(postHogIdentity(settings, "private", async url => url === "https://aesthetic.computer/api/product-analytics-config"
    ? reply(browser)
    : reply({ id: 123, organization: settings.organizationId, api_token: "phc_otherproject" })), /project-mismatch/);
});

test("PostHog sends no authorization to an unrecognized production host", async () => {
  let requests = 0;
  await assert.rejects(postHogIdentity(settings, "private", async () => {
    requests++;
    return reply({ ...browser, uiHost: "https://unrelated.invalid" });
  }), /binding-unavailable/);
  assert.equal(requests, 1);
});

test("PostHog queries use actual piece categories and aggregate counts without identities", () => {
  const kind = pieceProperties("aesthetic.computer/disks/notepat").piece_kind;
  const queries = postHogQueries(["notepat"], ["docs"], { end: "2026-10-08T12:00:00Z" });
  assert.ok(queries[0].includes(`properties.piece_kind = '${kind}'`));
  assert.match(queries[1], /sum\(toFloat\(properties.count\)\)/);
  assert.doesNotMatch(queries.join(" "), /distinct_id|person_id|\$current_url|\$session_id|SELECT \*/);
  assert.throws(() => postHogQueries(["notepat'); DROP TABLE events"], ["docs"]), /invalid-bounds/);
});

test("PostHog treats usage as context and 5xx as a manual hypothesis", () => {
  const out = postHogSignals([{ results: [["notepat", "ac_piece_opened", 100], ["notepat", "ac_piece_interacted", 3]] },
    { results: [["docs", "5xx", 7]] }], ["notepat"], ["docs"]);
  assert.equal(out.metrics.length, 3);
  assert.equal(out.leads.length, 1);
  assert.equal(out.leads[0].route, null);
  assert.equal(out.leads[0].kind, "endpoint-error");
  assert.deepEqual(out.privateReports, []);
  assert.throws(() => postHogSignals([{ results: [["notepat", "ac_piece_opened", 3, "private identity"]] }, { results: [] }], ["notepat"], ["docs"]), /invalid-aggregate/);
  assert.throws(() => postHogSignals([{ results: [] }, { results: [["private-endpoint", "5xx", 3]] }], ["notepat"], ["docs"]), /invalid-aggregate/);
  assert.throws(() => postHogSignals([{ query_status: { complete: false } }, { results: [] }], ["notepat"], ["docs"]), /unavailable/);
});

test("PostHog transport failures never expose server diagnostics or credentials", async () => {
  const out = await collectPostHog("/unused", { posthog: settings }, { env: { AESPATCHER_POSTHOG_READ_KEY: "private" },
    fetchImpl: async () => { throw new Error("Bearer private: raw customer data"); } });
  assert.equal(out.coverage[0].status, "unavailable");
  assert.doesNotMatch(JSON.stringify(out), /Bearer|customer|raw/);
  assert.deepEqual(out.leads, []);
});

test("PostHog collector performs only bound metadata and two aggregate reads", async () => {
  const requests = [];
  const out = await collectPostHog(fileURLToPath(new URL("../../../", import.meta.url)), { posthog: settings }, {
    env: { AESPATCHER_POSTHOG_READ_KEY: "private" }, fetchImpl: async (url, options) => {
      requests.push({ url, options });
      if (url === "https://aesthetic.computer/api/product-analytics-config") return reply(browser);
      if (!options.body) return reply({ id: 123, organization: settings.organizationId, api_token: browser.projectToken });
      const payload = JSON.parse(options.body);
      assert.equal(payload.query.kind, "HogQLQuery");
      assert.match(payload.query.query, /^SELECT .* FROM events WHERE /);
      assert.doesNotMatch(options.body, /private|Bearer/);
      return reply({ results: [] });
    },
  });
  assert.equal(out.coverage[0].status, "available");
  assert.equal(requests.length, 4);
  assert.ok(requests.slice(2).every(call => call.url === "https://us.posthog.com/api/projects/123/query/"));
});
