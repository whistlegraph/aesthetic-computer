import { test, mock } from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";

const rows = new Map(), mutes = new Map();
let currentHandle;
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
  collection: name => name === "whistlegraph-chat" ? messages : {
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
  const body = { requestId: randomUUID(), text: "hello", handle: "impersonated" };
  assert.equal((await call(null, "messages", body)).statusCode, 401);
  assert.equal((await call("member", "messages", body)).statusCode, 403);
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
});
