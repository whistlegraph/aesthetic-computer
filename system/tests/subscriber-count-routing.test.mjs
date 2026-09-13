import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const appSource = await readFile(
  new URL("../netlify/functions/sotce-net.mjs", import.meta.url),
  "utf8",
);
const metricsSource = await readFile(
  new URL("../netlify/functions/metrics.mjs", import.meta.url),
  "utf8",
);

test("the public gate requests the overall subscriber count", () => {
  assert.match(
    appSource,
    /fetch\("\/sotce-net\/subscribers"\)/,
  );
  assert.match(
    appSource,
    /path === "\/subscribers"[\s\S]*getCumulativeSubscriptionCount/,
  );
});

test("internal metrics request the active subscriber count", () => {
  assert.match(metricsSource, /\/active-subscribers/);
  assert.match(
    appSource,
    /path === "\/active-subscribers"[\s\S]*getActiveSubscriptionCount/,
  );
});
