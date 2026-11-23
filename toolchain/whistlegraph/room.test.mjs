import { test, mock } from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";

const rows = new Map(), mutes = new Map();
let currentHandle, recipients;
const alerts = [];
mock.module("../../shared/push.mjs", { exports: {
  webPushConfigured: () => true, apnsConfigured: () => true,
  sendToDevices: async (db, devices, note) => alerts.push({ devices, note }),
} });
const subscribers = new Set();
const preferences = {
  distinct: async (key, filter) => [...subscribers].filter(sub => sub !== filter._id.$ne),
  updateOne: async ({ _id }) => subscribers.add(_id),
  deleteOne: async ({ _id }) => subscribers.delete(_id),
  findOne: async ({ _id }) => subscribers.has(_id) ? { _id } : null,
};
const devices = {
  find: filter => { recipients = filter; return { toArray: async () => [] }; },
  findOne: async () => ({ _id: "phone" }),
};
mock.module("../../system/backend/authorization.mjs", { exports: {
  authorize: async headers => headers.authorization ? { sub: headers.authorization, email_verified: true } : null,
  handleFor: async () => currentHandle,
} });
const messages = {
  createIndex: async () => {},
  findOne: async ({ _id }) => rows.get(_id),
  async insertOne(row) {
    if (rows.has(row._id)) throw Object.assign(new Error(), { code: 11000 });
    rows.set(row._id, row);
  },
  updateOne: async ({ _id }, update) => Object.assign(rows.get(_id), update.$set),
};
mock.module("../../system/backend/database.mjs", { exports: { connect: async () => ({ db: {
  collection: name => ({ "whistlegraph-chat": messages, "whistlegraph-chat-app-subscribers": preferences, "push-tokens": devices }[name]) || {
    findOne: async ({ _id }) => mutes.get(_id),
    updateOne: async ({ _id }, update) => mutes.set(_id, update.$set),
  },
} }) } });
const { handler } = await import("../../system/netlify/functions/whistlegraph-chat.mjs");
const call = (authorization, action, body) => handler({
  httpMethod: "POST", headers: { authorization }, queryStringParameters: { action }, body: JSON.stringify(body),
});

test("posts require the actor's handle, retry once, and respect host moderation", async () => {
  process.env.WHISTLEGRAPH_ADMIN_SUBS = "host";
  const body = { browserDevice: "my-browser", requestId: randomUUID(), text: "hello", handle: "impersonated" };
  assert.equal((await call(null, "messages", body)).statusCode, 401);
  assert.equal((await call("member", "messages", body)).statusCode, 403);
  assert.equal((await call(null, "app-alerts", { enabled: true })).statusCode, 401);
  await call("reader", "app-alerts", { enabled: true });
  await call("unsubscribed", "app-alerts", { enabled: true });
  await call("unsubscribed", "app-alerts", { enabled: false });
  currentHandle = "member-handle";
  assert.equal((await call("member", "messages", body)).statusCode, 200);
  assert.equal((await call("member", "messages", body)).statusCode, 200);
  const [row] = rows.values();
  assert.equal(rows.size, 1);
  assert.equal(row.handle, currentHandle);
  assert.equal((await call("member", "remove", { id: row._id })).statusCode, 403);
  assert.equal((await call("host", "mute", { id: row._id })).statusCode, 200);
  assert.equal((await call("member", "messages", { ...body, requestId: randomUUID() })).statusCode, 403);
  assert.equal((await call("host", "remove", { id: row._id })).statusCode, 200);
  assert.equal(row.removed, true);
  assert.equal(alerts.length, 1);
  assert.deepEqual(recipients, {
    user: { $ne: "member" }, $or: [
      { kind: "webpush", topics: "chat-whistlegraph", deviceId: { $ne: "my-browser" } },
      { kind: "apns", user: { $in: ["reader"] } },
    ],
  });
  assert.equal(alerts[0].note.data.piece, "whistlegraph");
  assert.equal(alerts[0].note.body, "hello");
});
