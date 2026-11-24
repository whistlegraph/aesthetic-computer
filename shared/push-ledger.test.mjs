import test from "node:test";
import assert from "node:assert/strict";

// Every send leaves one ledger row in `push-sends` — counts and routing only,
// never the note's title or body.
function ledgerDb(tokens) {
  const rows = [];
  return { rows, collection: (name) => name === "push-sends"
    ? { insertOne: async (doc) => { rows.push(doc); return {}; }, createIndex: async () => {} }
    : { find: (filter) => ({ toArray: async () => tokens.filter((d) =>
          (filter.user === undefined || d.user === filter.user) &&
          (filter.topics === undefined || (d.topics || []).includes(filter.topics))) }),
        deleteMany: async () => ({}) } };
}

test("topic and person sends are recorded without their text", async () => {
  for (const k of ["APNS_TEAM_ID", "APNS_KEY_ID", "APNS_KEY", "VAPID_PUBLIC_KEY", "VAPID_PRIVATE_KEY"]) delete process.env[k];
  const { broadcastToTopic, sendToUser } = await import("./push.mjs");
  const db = ledgerDb([
    { user: "u1", kind: "apns", token: "ab", topics: ["mood"] },
    { user: "u1", kind: "webpush", subscription: { endpoint: "https://x" }, topics: [] },
    { user: "u2", kind: "apns", token: "cd", topics: ["mood"] },
  ]);
  const quiet = () => {};
  await broadcastToTopic(db, "mood", { title: "secret", body: "secret" }, quiet);
  await sendToUser(db, "u1", { title: "secret", body: "secret", data: { kind: "tell", piece: "mail" } }, {}, quiet);
  await sendToUser(db, "nobody", { title: "t", body: "b" }, {}, quiet);

  assert.equal(db.rows.length, 3);
  const [topic, person, empty] = db.rows;
  assert.equal(topic.via, "topic"); assert.equal(topic.topic, "mood");
  assert.deepEqual(topic.channels, { apns: 2 });
  assert.equal(topic.attempted, 2);
  assert.equal(person.via, "user"); assert.equal(person.user, "u1");
  assert.equal(person.kind, "tell"); assert.equal(person.piece, "mail");
  assert.deepEqual(person.channels, { apns: 1, webpush: 1 });
  assert.equal(empty.attempted, 0, "a send that reached no device is still a row");
  assert.ok(db.rows.every((r) => r.at instanceof Date));
  assert.ok(!JSON.stringify(db.rows).includes("secret"), "no title or body in the ledger");
});

test("a broken ledger never breaks a send", async () => {
  const { sendToUser } = await import("./push.mjs");
  const db = { collection: (name) => name === "push-sends"
    ? { insertOne: async () => { throw new Error("down"); } }
    : { find: () => ({ toArray: async () => [] }) } };
  const summary = await sendToUser(db, "u1", { title: "t" }, {}, () => {});
  assert.deepEqual(summary, { attempted: 0, succeeded: 0, failed: 0, pruned: 0 });
});
