import test from "node:test";
import assert from "node:assert/strict";
import {
  buildPostHogInventory,
  classifyFunction,
  LITH_ROUTE_FAMILIES,
} from "../../toolchain/analytics/posthog-inventory.mjs";

test("sensitive endpoint classes remain inventory or aggregate only", () => {
  assert.equal(classifyFunction("chat-messages").posthog, "inventory-only");
  assert.equal(
    classifyFunction("device-token").posthog,
    "aggregate-status-only",
  );
  assert.equal(classifyFunction("machine-logs").posthog, "inventory-only");
  assert.equal(
    classifyFunction("piece-log").posthog,
    "existing-lith-silo-only",
  );
});

test("inventory covers every function source without reading payloads", async () => {
  const inventory = await buildPostHogInventory();
  assert.equal(inventory.sourceFileCount, 165);
  assert.equal(inventory.handlerCount, 160);
  assert.equal(inventory.helperOrScriptCount, 4);
  assert.ok(inventory.functions.some((entry) => entry.name === "index"));
  assert.ok(
    inventory.functions.some((entry) => entry.name === "chat-messages"),
  );
  assert.equal(
    inventory.functions.filter((entry) => entry.class === "review-required")
      .length,
    0,
    "new function sources require an explicit analytics classification",
  );
  assert.ok(
    inventory.functions.every((entry) => !Object.hasOwn(entry, "payload")),
  );
});

test("top-level Lith route families are included", () => {
  const surfaces = new Set(LITH_ROUTE_FAMILIES.map((entry) => entry.surface));
  assert.ok(surfaces.has("function-api"));
  assert.ok(surfaces.has("media"));
  assert.ok(surfaces.has("lith-operations"));
  assert.ok(surfaces.has("site-and-pieces"));
});
