import assert from "node:assert/strict";
import { test } from "node:test";
import { postMoodToBluesky } from "../backend/bluesky-mirror.mjs";
import { fetchBlueskyEngagement, resolveDidToHandle } from "../backend/bluesky-engagement.mjs";

const DID = "did:plc:k3k3wknzkcnekbnyde4dbatz";
const URI = `at://${DID}/app.bsky.feed.post/3mwec2oefr625`;
const CID = "bafyreif7345s5wcejbal2q7bxa5szldnvbdwrw2okahc2vd3hz4mps5mqy";
const WHEN = "2026-09-25T17:25:38.586Z";
const empty = { likes: 0, reposts: 0, replies: [], quoteCount: 0 };
const database = { db: { collection: () => ({ findOne: async () => ({
  identifier: "aesthetic.computer", appPassword: "test-only-password",
}) }) } };

function mockNetwork(t, respond) {
  const requests = [];
  t.mock.method(globalThis, "fetch", async (input, init) => {
    const request = new Request(input, init);
    requests.push(request);
    return respond(request);
  });
  return requests;
}

function post(overrides = {}) {
  return {
    uri: URI, cid: CID,
    author: { did: DID, handle: "aesthetic.computer" },
    record: { $type: "app.bsky.feed.post", text: "A mood", createdAt: WHEN },
    indexedAt: WHEN, likeCount: 3, repostCount: 2, quoteCount: 1,
    ...overrides,
  };
}

test("successful mood publication returns its reference for persistence", async (t) => {
  let published;
  const requests = mockNetwork(t, async (request) => {
    switch (new URL(request.url).pathname) {
      case "/xrpc/com.atproto.server.createSession":
        return Response.json({ did: DID, handle: "aesthetic.computer", accessJwt: "test-access", refreshJwt: "test-refresh" });
      case "/xrpc/com.atproto.repo.createRecord":
        published = await request.json();
        return Response.json({ uri: URI, cid: CID });
      default:
        throw new Error(`Unexpected request: ${request.url}`);
    }
  });
  const result = await postMoodToBluesky(database, "hello", "@jeffrey", "mood-rkey");
  assert.deepEqual(result, { uri: URI, cid: CID, rkey: "3mwec2oefr625" });
  assert.equal(published.repo, DID);
  assert.equal(published.collection, "app.bsky.feed.post");
  assert.equal(published.record.text, "@jeffrey: hello\n\nhttps://aesthetic.computer/moods~jeffrey~mood-rkey");
  assert.equal(requests.length, 2, "one login and one publication");
});

test("a rejected Bluesky login does not publish or return a success reference", async (t) => {
  const requests = mockNetwork(t, () => Response.json({ error: "AuthenticationRequired", message: "Invalid credentials" }, { status: 401 }));
  assert.equal(await postMoodToBluesky(database, "hello", "@jeffrey", "mood-rkey"), null);
  assert.equal(requests.length, 1);
});

test("engagement uses the public AppView and omits inaccessible replies", async (t) => {
  const requests = mockNetwork(t, (request) => {
    if (new URL(request.url).origin !== "https://public.api.bsky.app") {
      return Response.json({ error: "AuthMissing", message: "Authentication Required" }, { status: 401 });
    }
    return Response.json({ thread: {
      $type: "app.bsky.feed.defs#threadViewPost", post: post(),
      replies: [
        { $type: "app.bsky.feed.defs#notFoundPost", uri: `${URI}-deleted`, notFound: true },
        { $type: "app.bsky.feed.defs#threadViewPost", post: post({ uri: `${URI}-reply`, record: { $type: "app.bsky.feed.post", text: "hello back", createdAt: WHEN } }) },
      ],
    } });
  });
  const result = await fetchBlueskyEngagement(URI);
  assert.equal(result.likes, 3);
  assert.equal(result.reposts, 2);
  assert.equal(result.quoteCount, 1);
  assert.equal(result.replies.length, 1);
  assert.equal(result.replies[0].text, "hello back");
  assert.equal(result.webUrl, `https://bsky.app/profile/${DID}/post/3mwec2oefr625`);
  assert.equal(requests.length, 1);
  assert.equal(requests[0].headers.has("authorization"), false);
  assert.equal(new URL(requests[0].url).searchParams.get("uri"), URI);
});

test("DID-to-handle lookup also uses the public AppView", async (t) => {
  const requests = mockNetwork(t, request => new URL(request.url).origin === "https://public.api.bsky.app"
    ? Response.json({ did: DID, handle: "aesthetic.computer" })
    : Response.json({ error: "AuthMissing" }, { status: 401 }));
  assert.equal(await resolveDidToHandle(DID), "aesthetic.computer");
  assert.equal(requests.length, 1);
  assert.equal(requests[0].headers.has("authorization"), false);
});

test("unmirrored moods need no request; unavailable engagement stays nonfatal", async (t) => {
  const requests = mockNetwork(t, () => Response.json({ error: "NotFound" }, { status: 404 }));
  assert.deepEqual(await fetchBlueskyEngagement(null), empty);
  assert.equal(requests.length, 0);
  assert.deepEqual(await fetchBlueskyEngagement(URI), empty);
  assert.equal(requests.length, 1);
});
