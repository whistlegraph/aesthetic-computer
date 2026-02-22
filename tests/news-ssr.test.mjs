import assert from "node:assert/strict";
import { createHandler } from "../system/netlify/functions/news.mjs";

function respond(statusCode, body, headers = {}) {
  return {
    statusCode,
    headers: {
      "Content-Type": "text/html",
      ...headers,
    },
    body,
  };
}

function makeCollection(initial = []) {
  const docs = [...initial];

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
    async findOne(query) {
      return docs.find((doc) => matches(doc, query)) || null;
    },
  };
}

function makeDatabase() {
  const post = {
    code: "nabc123",
    title: "Hello News",
    url: "https://example.com",
    user: "user-1",
    when: new Date(),
    score: 5,
    commentCount: 1,
    status: "live",
  };
  const comment = {
    postCode: "nabc123",
    text: "Nice!",
    user: "user-1",
    when: new Date(),
    status: "live",
  };
  const handle = { _id: "user-1", handle: "tester" };
  const collections = new Map([
    ["news-posts", makeCollection([post])],
    ["news-comments", makeCollection([comment])],
    ["@handles", makeCollection([handle])],
  ]);
  return {
    db: {
      collection(name) {
        return collections.get(name) || makeCollection();
      },
    },
    disconnect: async () => null,
  };
}

const database = makeDatabase();
const handler = createHandler({
  connect: async () => database,
  respond,
});

async function testFrontPageRenders() {
  const res = await handler({
    httpMethod: "GET",
    headers: { host: "localhost:8888" },
    queryStringParameters: { path: "" },
  });
  assert.equal(res.statusCode, 200, "front page should render");
  assert.ok(res.body.includes("Hello News"), "post title should render");
  assert.ok(res.body.includes("/news.aesthetic.computer/main.css"), "assets should include news stylesheet");
  assert.ok(res.body.includes("/news.aesthetic.computer/new"), "nav should include local prefix");
}

async function testItemPageRendersComments() {
  const res = await handler({
    httpMethod: "GET",
    headers: { host: "localhost:8888" },
    queryStringParameters: { path: "nabc123" },
  });
  assert.equal(res.statusCode, 200, "item page should render");
  assert.ok(res.body.includes("Nice!"), "comment text should render");
}

async function run() {
  await testFrontPageRenders();
  await testItemPageRendersComments();
  console.log("✅ news SSR tests passed");
}

run();
