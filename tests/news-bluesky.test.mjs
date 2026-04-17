import assert from "node:assert/strict";
import {
  ingestFromActor,
  getConfiguredSources,
  __testing,
} from "../system/backend/news-bluesky-ingest.mjs";

const { projectPost, stripUrls, parseAtUri } = __testing;

// --- Fixtures ---------------------------------------------------------------

const DID = "did:plc:testartist";
const HANDLE = "artistnewsnetwork.bsky.social";

function makePostWithExternal(overrides = {}) {
  return {
    post: {
      uri: `at://${DID}/app.bsky.feed.post/rkey${overrides.rkey || "a"}`,
      cid: `bafyre${overrides.cid || "a"}`,
      author: { did: DID, handle: HANDLE },
      record: {
        $type: "app.bsky.feed.post",
        text: overrides.text ?? "A great exhibition opening this weekend https://gallery.example/show",
        createdAt: overrides.createdAt || "2026-04-10T12:00:00.000Z",
        embed: {
          $type: "app.bsky.embed.external",
          external: {
            uri: "https://gallery.example/show",
            title: "Gallery Show: Weekend Opening",
            description: "A new exhibition.",
          },
        },
      },
      embed: {
        $type: "app.bsky.embed.external#view",
        external: {
          uri: "https://gallery.example/show",
          title: "Gallery Show: Weekend Opening",
          description: "A new exhibition.",
        },
      },
    },
  };
}

function makePostWithFacetLink() {
  return {
    post: {
      uri: `at://${DID}/app.bsky.feed.post/facetpost`,
      cid: "bafyrefacet",
      author: { did: DID, handle: HANDLE },
      record: {
        text: "Check this out https://museum.example/piece",
        createdAt: "2026-04-10T13:00:00.000Z",
        facets: [
          {
            features: [
              { $type: "app.bsky.richtext.facet#link", uri: "https://museum.example/piece" },
            ],
          },
        ],
      },
    },
  };
}

function makePlainTextPost() {
  return {
    post: {
      uri: `at://${DID}/app.bsky.feed.post/plain`,
      cid: "bafyreplain",
      author: { did: DID, handle: HANDLE },
      record: {
        text: "Just thinking out loud today.",
        createdAt: "2026-04-10T14:00:00.000Z",
      },
    },
  };
}

function makeReply() {
  return {
    post: {
      uri: `at://${DID}/app.bsky.feed.post/reply`,
      cid: "bafyrereply",
      author: { did: DID, handle: HANDLE },
      record: {
        text: "Good point https://other.example/thing",
        createdAt: "2026-04-10T15:00:00.000Z",
        reply: { root: { uri: "at://x/y/z" }, parent: { uri: "at://x/y/z" } },
        embed: {
          $type: "app.bsky.embed.external",
          external: { uri: "https://other.example/thing", title: "Thing" },
        },
      },
      embed: {
        $type: "app.bsky.embed.external#view",
        external: { uri: "https://other.example/thing", title: "Thing" },
      },
    },
  };
}

function makeRepost() {
  return {
    reason: { $type: "app.bsky.feed.defs#reasonRepost" },
    post: {
      uri: `at://${DID}/app.bsky.feed.post/reposted`,
      cid: "bafyrerepost",
      author: { did: "did:plc:someone-else", handle: "someone.bsky.social" },
      record: {
        text: "Repost body https://x.example/reposted",
        createdAt: "2026-04-10T16:00:00.000Z",
        embed: {
          $type: "app.bsky.embed.external",
          external: { uri: "https://x.example/reposted", title: "Reposted" },
        },
      },
    },
  };
}

// --- Pure helpers -----------------------------------------------------------

function testStripUrls() {
  assert.equal(stripUrls("before https://a.com after"), "before after");
  assert.equal(stripUrls("only text"), "only text");
  assert.equal(stripUrls(""), "");
}

function testParseAtUri() {
  const parsed = parseAtUri("at://did:plc:abc/app.bsky.feed.post/rkey123");
  assert.deepEqual(parsed, {
    did: "did:plc:abc",
    collection: "app.bsky.feed.post",
    rkey: "rkey123",
  });
  assert.equal(parseAtUri("not-an-at-uri"), null);
}

function testProjectPostExternalEmbed() {
  const { post } = makePostWithExternal();
  const projected = projectPost(post);
  assert.equal(projected.title, "Gallery Show: Weekend Opening");
  assert.equal(projected.url, "https://gallery.example/show");
  assert.equal(projected.text, "A great exhibition opening this weekend");
}

function testProjectPostFacetLinkFallback() {
  const { post } = makePostWithFacetLink();
  const projected = projectPost(post);
  assert.ok(projected, "should project a post with a link facet");
  assert.equal(projected.url, "https://museum.example/piece");
  assert.equal(projected.title, "Check this out");
}

function testProjectPostPlainTextSkipped() {
  const { post } = makePlainTextPost();
  assert.equal(projectPost(post), null, "plain text posts with no link are skipped");
}

function testConfiguredSourcesDefault() {
  const sources = getConfiguredSources({});
  assert.ok(sources.includes("artistnewsnetwork.bsky.social"));
}

function testConfiguredSourcesOverride() {
  const sources = getConfiguredSources({
    NEWS_EXTERNAL_SOURCES: "a.bsky.social, did:plc:xyz ,",
  });
  assert.deepEqual(sources, ["a.bsky.social", "did:plc:xyz"]);
}

// --- Integration: ingestFromActor ------------------------------------------

function makePostsCollection() {
  const docs = [];
  return {
    docs,
    async findOne(query) {
      if (query?.["external.postUri"]) {
        return docs.find((d) => d.external?.postUri === query["external.postUri"]) || null;
      }
      return docs.find((d) => {
        return Object.entries(query).every(([k, v]) => d[k] === v);
      }) || null;
    },
    async insertOne(doc) {
      const existing = docs.find((d) => d.external?.postUri === doc.external?.postUri);
      if (existing) {
        const err = new Error("dup");
        err.code = 11000;
        throw err;
      }
      docs.push(doc);
      return { insertedId: doc.code };
    },
  };
}

function makeFakeAgent(feed) {
  return {
    app: {
      bsky: {
        actor: {
          async getProfile({ actor }) {
            return { data: { did: DID, handle: HANDLE } };
          },
        },
        feed: {
          async getAuthorFeed() {
            return { data: { feed } };
          },
        },
      },
    },
  };
}

async function testIngestInsertsOnlyExternalAndFacetPosts() {
  const posts = makePostsCollection();
  const database = {
    db: { collection: (name) => (name === "news-posts" ? posts : null) },
  };
  const feed = [
    makePostWithExternal({ rkey: "a" }),
    makePostWithFacetLink(),
    makePlainTextPost(),
    makeReply(),
    makeRepost(),
  ];
  let codeCounter = 0;
  const result = await ingestFromActor(database, HANDLE, {
    agent: makeFakeAgent(feed),
    generateCode: async () => `gen${++codeCounter}`,
  });
  assert.equal(result.inserted, 2, "should insert external + facet posts only");
  assert.equal(result.skipped, 3, "plain-text, reply, repost are skipped");
  assert.equal(posts.docs.length, 2);
  const first = posts.docs[0];
  assert.equal(first.external.source, "bsky");
  assert.equal(first.external.did, DID);
  assert.equal(first.external.handle, HANDLE);
  assert.ok(first.external.postUri.startsWith("at://"));
  assert.equal(first.user, null, "external posts have no AC user sub");
  assert.ok(first.atproto?.uri, "external posts mirror atproto fields");
}

async function testIngestDeduplicatesOnSecondRun() {
  const posts = makePostsCollection();
  const database = {
    db: { collection: (name) => (name === "news-posts" ? posts : null) },
  };
  const feed = [makePostWithExternal({ rkey: "dup" })];
  const agent = makeFakeAgent(feed);
  let codeCounter = 0;
  const genCode = async () => `dup${++codeCounter}`;

  const first = await ingestFromActor(database, HANDLE, { agent, generateCode: genCode });
  assert.equal(first.inserted, 1);

  const second = await ingestFromActor(database, HANDLE, { agent, generateCode: genCode });
  assert.equal(second.inserted, 0, "rerun should not insert duplicates");
  assert.equal(second.skipped, 1, "rerun should skip the already-ingested post");
  assert.equal(posts.docs.length, 1);
}

// --- SSR rendering for external posts --------------------------------------

async function testExternalAttributionRendersOnFrontPage() {
  const { createHandler } = await import("../system/netlify/functions/news.mjs");

  const externalPost = {
    code: "nextrnl",
    title: "External Headline",
    url: "https://gallery.example/show",
    user: null,
    when: new Date("2026-04-10T12:00:00Z"),
    score: 1,
    commentCount: 0,
    status: "live",
    external: {
      source: "bsky",
      did: DID,
      handle: HANDLE,
      postUri: `at://${DID}/app.bsky.feed.post/rkeyA`,
      postedAt: new Date("2026-04-10T12:00:00Z"),
      fetchedAt: new Date("2026-04-10T12:05:00Z"),
    },
  };

  const collections = new Map([
    ["news-posts", mockCollection([externalPost])],
    ["news-comments", mockCollection([])],
    ["@handles", mockCollection([])],
  ]);
  const database = {
    db: { collection: (name) => collections.get(name) || mockCollection([]) },
    disconnect: async () => null,
  };
  const handler = createHandler({
    connect: async () => database,
    respond: (statusCode, body, headers = {}) => ({ statusCode, headers, body }),
  });
  const res = await handler({
    httpMethod: "GET",
    headers: { host: "localhost:8888" },
    queryStringParameters: { path: "" },
  });
  assert.equal(res.statusCode, 200);
  assert.ok(res.body.includes("External Headline"), "title should render");
  assert.ok(
    res.body.includes("bsky.app/profile/artistnewsnetwork.bsky.social"),
    "handle should link to bluesky profile",
  );
  assert.ok(res.body.includes("@artistnewsnetwork"), "short handle should render");
  assert.ok(res.body.includes("news-external-handle"), "external handle class should render");
}

async function testExternalAttributionRendersOnItemPage() {
  const { createHandler } = await import("../system/netlify/functions/news.mjs");

  const externalPost = {
    code: "nexitem",
    title: "External Item",
    url: "https://gallery.example/item",
    user: null,
    when: new Date("2026-04-10T12:00:00Z"),
    score: 1,
    commentCount: 0,
    status: "live",
    external: {
      source: "bsky",
      did: DID,
      handle: HANDLE,
      postUri: `at://${DID}/app.bsky.feed.post/itemrkey`,
      postedAt: new Date("2026-04-10T12:00:00Z"),
      fetchedAt: new Date("2026-04-10T12:05:00Z"),
    },
  };
  const collections = new Map([
    ["news-posts", mockCollection([externalPost])],
    ["news-comments", mockCollection([])],
    ["@handles", mockCollection([])],
  ]);
  const database = {
    db: { collection: (name) => collections.get(name) || mockCollection([]) },
    disconnect: async () => null,
  };
  const handler = createHandler({
    connect: async () => database,
    respond: (statusCode, body, headers = {}) => ({ statusCode, headers, body }),
  });
  const res = await handler({
    httpMethod: "GET",
    headers: { host: "localhost:8888" },
    queryStringParameters: { path: "nexitem" },
  });
  assert.equal(res.statusCode, 200);
  assert.ok(res.body.includes("External Item"));
  assert.ok(
    res.body.includes("bsky.app/profile/did:plc:testartist/post/itemrkey"),
    "should link to original Bluesky post",
  );
  assert.ok(res.body.includes("news-external-attrib"), "item page should tag external attribution");
}

// Tiny mock collection factory (same shape news.mjs expects).
function mockCollection(initial) {
  const docs = [...initial];
  function matches(doc, query) {
    if (!query) return true;
    return Object.entries(query).every(([k, v]) => {
      if (v && typeof v === "object" && !Array.isArray(v)) {
        if (Object.hasOwn(v, "$ne")) return doc[k] !== v.$ne;
        if (Object.hasOwn(v, "$in")) return v.$in.includes(doc[k]);
      }
      return doc[k] === v;
    });
  }
  return {
    docs,
    find(query) {
      let sortSpec = null;
      let limitValue = null;
      const api = {
        sort(spec) { sortSpec = spec; return api; },
        limit(v) { limitValue = v; return api; },
        async toArray() {
          let out = docs.filter((d) => matches(d, query));
          if (sortSpec) {
            const entries = Object.entries(sortSpec);
            out = out.sort((a, b) => {
              for (const [f, dir] of entries) {
                if (a[f] === b[f]) continue;
                return dir < 0 ? (b[f] ?? 0) - (a[f] ?? 0) : (a[f] ?? 0) - (b[f] ?? 0);
              }
              return 0;
            });
          }
          if (limitValue !== null) out = out.slice(0, limitValue);
          return out;
        },
      };
      return api;
    },
    async findOne(query) {
      return docs.find((d) => matches(d, query)) || null;
    },
    aggregate() {
      return { async toArray() { return []; } };
    },
  };
}

// --- Runner ----------------------------------------------------------------

async function run() {
  testStripUrls();
  testParseAtUri();
  testProjectPostExternalEmbed();
  testProjectPostFacetLinkFallback();
  testProjectPostPlainTextSkipped();
  testConfiguredSourcesDefault();
  testConfiguredSourcesOverride();
  await testIngestInsertsOnlyExternalAndFacetPosts();
  await testIngestDeduplicatesOnSecondRun();
  await testExternalAttributionRendersOnFrontPage();
  await testExternalAttributionRendersOnItemPage();
  console.log("✅ news-bluesky tests passed");
}

run();
