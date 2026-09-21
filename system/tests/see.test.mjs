import test from "node:test";
import assert from "node:assert/strict";
import {
  boot,
  act,
  paint,
  leave,
} from "../public/aesthetic.computer/disks/see.mjs";

test("see blocks retry taps until the server cooldown expires", async (t) => {
  let now = 1000000;
  let requests = 0;
  t.mock.method(Date, "now", () => now);
  t.mock.method(globalThis, "fetch", async () => {
    requests++;
    return Response.json(
      { ok: false, reason: "fallback_budget_exhausted", retry_after: 120 },
      {
        status: 503,
        headers: { "Retry-After": "60" },
      },
    );
  });
  const settle = () => new Promise((resolve) => setImmediate(resolve));
  const tap = () => act({ event: { is: (name) => name === "touch" } });
  boot({ params: ["synthetic", "square"], colon: [], hud: { label() {} } });
  await settle();
  tap();
  await settle();
  assert.equal(requests, 1);
  const labels = [];
  const write = (text) => labels.push(text);
  const ink = () => ({ write });
  paint({ wipe() {}, ink, write, screen: { width: 320, height: 240 } });
  assert.ok(labels.includes("retry in 60s"));
  assert.ok(!labels.includes("tap to retry"));
  now += 60000;
  tap();
  await settle();
  assert.equal(requests, 2);
  leave();
});
