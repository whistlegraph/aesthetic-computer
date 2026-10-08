import test from "node:test";
import assert from "node:assert/strict";

// A minimal in-memory stand-in for the app-devices collection.
function fakeDb(rows) {
  const matches = (doc, filter) => Object.entries(filter).every(([k, v]) => {
    if (k === "push") return v.$exists ? doc.push !== undefined : true;
    if (k === "topics") return (doc.topics || []).includes(v);
    return doc[k] === v;
  });
  const updates = [];
  return { updates, collection: () => ({
    find: (filter) => ({ toArray: async () => rows.filter((d) => matches(d, filter)) }),
    updateMany: async (filter, update) => { updates.push({ filter, update }); return {}; },
  }) };
}

test("targets resolve through the registry; unreachable rows fail without pruning", async () => {
  for (const k of ["APNS_TEAM_ID", "APNS_KEY_ID", "APNS_KEY", "VAPID_PUBLIC_KEY", "VAPID_PRIVATE_KEY"]) delete process.env[k];
  const { sendToTarget } = await import("./push.mjs");
  const rows = [
    { _id: "whistlegraph:a", app: "whistlegraph", user: "u1", topics: ["testers"], push: { kind: "apns", token: "ab", env: "production" } },
    { _id: "whistlegraph:b", app: "whistlegraph", user: "u1", topics: [] },
    { _id: "aesel:c", app: "aesel", user: "u1", push: { kind: "apns", token: "cd", env: "sandbox" } },
    { _id: "sotce-net:d", app: "sotce-net", user: "u2", topics: ["testers"], push: { kind: "webpush", subscription: { endpoint: "https://x" } } },
  ];
  const db = fakeDb(rows), quiet = () => {};
  assert.deepEqual(await sendToTarget(db, { user: "u1" }, { title: "t", body: "b" }, quiet), { attempted: 2, succeeded: 0, failed: 2, pruned: 0 });
  assert.equal((await sendToTarget(db, { user: "u1", app: "aesel" }, { title: "t" }, quiet)).attempted, 1);
  assert.equal((await sendToTarget(db, { app: "whistlegraph", topic: "testers" }, { title: "t" }, quiet)).attempted, 1);
  assert.equal((await sendToTarget(db, { app: "whistlegraph", deviceId: "b" }, { title: "t" }, quiet)).attempted, 0, "a device without push is skipped");
  assert.equal(db.updates.length, 0, "unconfigured delivery never prunes registrations");
});
