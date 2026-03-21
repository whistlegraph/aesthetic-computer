import assert from "node:assert/strict";
import { createHandler } from "../system/netlify/functions/news-api.mjs";

function respond(statusCode, body, headers = {}) {
  const isJson = typeof body === "object" && body !== null;
  return {
    statusCode,
    headers: {
      "Content-Type": isJson ? "application/json" : "text/plain",
      ...headers,
    },
    body: isJson ? JSON.stringify(body) : body,
  };
}

function makeCollection(name, initial = []) {
  const docs = [...initial];
  let nextId = 1;

  function matches(doc, query) {
    if (!query || Object.keys(query).length === 0) return true;
    return Object.entries(query).every(([key, value]) => {
      if (value && typeof value === "object" && !Array.isArray(value)) {
        if (Object.hasOwn(value, "$ne")) {
          return doc[key] !== value.$ne;
        }
        if (Object.hasOwn(value, "$in")) {
          return value.$in.includes(doc[key]);
        }
      }
      return doc[key] === value;
    });
  }

  function applySort(items, sortSpec) {
    if (!sortSpec) return items;
    const entries = Object.entries(sortSpec);
    return items.sort((a, b) => {
      for (const [field, dir] of entries) {
        if (a[field] === b[field]) continue;
        return dir < 0 ? (b[field] ?? 0) - (a[field] ?? 0) : (a[field] ?? 0) - (b[field] ?? 0);
      }
      return 0;
    });
  }

  return {
    docs,
    async createIndex() {
      return null;
    },
    async findOne(query) {
      return docs.find((doc) => matches(doc, query)) || null;
    },
    find(query) {
      let sortSpec = null;
      let limitValue = null;
      const api = {
        sort(spec) {
          sortSpec = spec;
          return api;
        },
        limit(value) {
          limitValue = value;
          return api;
        },
        async toArray() {
          let results = docs.filter((doc) => matches(doc, query));
          results = applySort(results, sortSpec);
          if (limitValue !== null) results = results.slice(0, limitValue);
          return results;
        },
      };
      return api;
    },
    async insertOne(doc) {
      if (name === "news-votes") {
        const duplicate = docs.find(
          (existing) => existing.itemType === doc.itemType && existing.itemId === doc.itemId && existing.user === doc.user,
        );
        if (duplicate) {
          const error = new Error("Duplicate key");
          error.code = 11000;
          throw error;
        }
      }
      if (!doc._id) {
        doc._id = `doc-${nextId++}`;
      }
      docs.push(doc);
      return { insertedId: doc._id };
    },
    async updateOne(filter, update) {
      const target = docs.find((doc) => matches(doc, filter));
      if (!target) return { matchedCount: 0, modifiedCount: 0 };
      if (update?.$inc) {
        Object.entries(update.$inc).forEach(([key, value]) => {
          target[key] = (target[key] ?? 0) + value;
        });
      }
      return { matchedCount: 1, modifiedCount: 1 };
    },
  };
}

function makeDatabase() {
  const collections = new Map([
    ["news-posts", makeCollection("news-posts")],
    ["news-comments", makeCollection("news-comments")],
    ["news-votes", makeCollection("news-votes")],
  ]);
  return {
    db: {
      collection(name) {
        if (!collections.has(name)) collections.set(name, makeCollection(name));
        return collections.get(name);
      },
    },
    disconnect: async () => null,
  };
}

const database = makeDatabase();
const handler = createHandler({
  connect: async () => database,
  respond,
  authorize: async () => ({ sub: "user-1" }),
  generateUniqueCode: async () => "abc123",
});

async function testSubmitRequiresTitle() {
  const res = await handler({
    httpMethod: "POST",
    headers: {},
    queryStringParameters: { path: "submit" },
    body: "text=hello",
  });
  assert.equal(res.statusCode, 400, "submit should require title");
}

async function testSubmitCreatesPostAndVote() {
  const res = await handler({
    httpMethod: "POST",
    headers: {},
    queryStringParameters: { path: "submit" },
    body: "title=Hello&url=https%3A%2F%2Fexample.com&text=Hi",
  });
  const payload = JSON.parse(res.body);
  assert.equal(res.statusCode, 200, "submit should return ok");
  assert.equal(payload.code, "nabc123", "submit should return n-prefixed code");
  const posts = database.db.collection("news-posts").docs;
  const votes = database.db.collection("news-votes").docs;
  assert.equal(posts.length, 1, "post should be inserted");
  assert.equal(votes.length, 1, "vote should be inserted");
}

async function testCommentIncrementsCount() {
  const res = await handler({
    httpMethod: "POST",
    headers: {},
    queryStringParameters: { path: "comment" },
    body: "postCode=nabc123&text=Nice",
  });
  assert.equal(res.statusCode, 200, "comment should succeed");
  const post = database.db.collection("news-posts").docs[0];
  assert.equal(post.commentCount, 1, "comment should increment commentCount");
}

async function testDuplicateVoteReturnsDuplicateFlag() {
  const event = {
    httpMethod: "POST",
    headers: {},
    queryStringParameters: { path: "vote" },
    body: "itemType=post&itemId=nabc123&dir=1",
  };
  const first = await handler(event);
  assert.equal(first.statusCode, 200, "first vote should succeed");
  const second = await handler(event);
  const payload = JSON.parse(second.body);
  assert.equal(payload.duplicate, true, "duplicate vote should be flagged");
}

async function testGetPostsReturnsList() {
  const res = await handler({
    httpMethod: "GET",
    headers: {},
    queryStringParameters: { path: "posts", limit: "10" },
  });
  const payload = JSON.parse(res.body);
  assert.equal(res.statusCode, 200, "GET posts should succeed");
  assert.equal(payload.posts.length, 1, "GET posts should return inserted posts");
}

async function run() {
  await testSubmitRequiresTitle();
  await testSubmitCreatesPostAndVote();
  await testCommentIncrementsCount();
  await testDuplicateVoteReturnsDuplicateFlag();
  await testGetPostsReturnsList();
  console.log("✅ news-api tests passed");
}

run();
