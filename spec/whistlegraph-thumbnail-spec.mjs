import { createHandler } from "../system/netlify/functions/whistlegraph-thumbnail.mjs";

const event = (id, method = "GET") => ({ httpMethod: method, queryStringParameters: { id } });

describe("Whistlegraph thumbnail recovery", () => {
  it("keeps a durable canonical poster when one exists", async () => {
    let fetched = false;
    const handler = createHandler({
      loadPostsFn: () => new Map([["1234567890", {
        id: "1234567890",
        platform: "tiktok",
        media: "video",
        url: "https://www.tiktok.com/@whistlegraph/video/1234567890",
        thumb: "https://assets.aesthetic.computer/post.jpg",
      }]]),
      fetchFn: async () => { fetched = true; },
    });
    const response = await handler(event("1234567890"));
    expect(response.statusCode).toBe(302);
    expect(response.headers.Location).toBe("https://assets.aesthetic.computer/post.jpg");
    expect(fetched).toBeFalse();
  });

  it("recovers a fresh signed TikTok poster for a known missing asset", async () => {
    const handler = createHandler({
      loadPostsFn: () => new Map([["6747917653291175174", {
        id: "6747917653291175174",
        // Real older catalog rows can be mislabeled as audio even though the
        // trusted platform record and archived source are TikTok video.
        platform: "tiktok",
        media: "audio",
        url: "https://www.tiktok.com/@whistlegraph/video/6747917653291175174",
        thumb: null,
      }]]),
      fetchFn: async (_url, options = {}) => options.method === "HEAD"
        ? { ok: false, status: 403 }
        : {
            ok: true,
            status: 200,
            json: async () => ({ thumbnail_url: "https://p16-common-sign.tiktokcdn-us.com/poster.jpg?signature=test" }),
          },
      nowFn: () => 1,
    });
    const response = await handler(event("6747917653291175174"));
    expect(response.statusCode).toBe(302);
    expect(response.headers.Location).toContain("tiktokcdn-us.com/poster.jpg");
  });

  it("does not proxy arbitrary URLs or unknown post IDs", async () => {
    const handler = createHandler({ loadPostsFn: () => new Map() });
    expect((await handler(event("not-a-post"))).statusCode).toBe(400);
    expect((await handler(event("1234567890"))).statusCode).toBe(404);
  });
});
