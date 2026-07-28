import { readFileSync } from "node:fs";
import { handler as whistlegraphOgHandler } from "../system/netlify/functions/whistlegraph-og.mjs";

describe("Whistlegraph Open Graph previews", () => {
  it("uses the top visual post for work cards without generated thumbnails", async () => {
    const archive = JSON.parse(
      readFileSync(new URL("../system/public/whistlegraph.org/posts.json", import.meta.url), "utf8"),
    );
    const apple = archive.posts
      .filter((post) => (post.works || post.graphs || []).includes("appl") && post.thumb && post.media !== "audio")
      .sort((a, b) => (Number(b.views) || 0) - (Number(a.views) || 0))[0];
    const response = await whistlegraphOgHandler({ queryStringParameters: { code: "appl" } });

    expect(response.statusCode).toBe(200);
    expect(response.body).toContain('<meta property="og:title" content="Hey There, Apple">');
    expect(response.body).toContain(`<meta property="og:image" content="${apple.thumb}">`);
    expect(response.body).toContain('<meta property="og:type" content="website">');
    expect(response.body).not.toContain('<meta property="og:video"');
  });

  it("gives stable post links their own archive preview", async () => {
    const archive = JSON.parse(
      readFileSync(new URL("../system/public/whistlegraph.org/posts.json", import.meta.url), "utf8"),
    );
    const post = archive.posts.find((entry) => entry.thumb && entry.src && entry.media !== "audio");
    const response = await whistlegraphOgHandler({ queryStringParameters: { id: String(post.id) } });

    expect(response.statusCode).toBe(200);
    expect(response.body).toContain(`<meta property="og:image" content="${post.thumb}">`);
    expect(response.body).toContain(`<meta property="og:video" content="${post.src}">`);
    expect(response.body).toContain(`https://whistlegraph.org/post/${post.id}`);
  });

  it("routes stable post URLs through the preview handler", () => {
    const caddy = readFileSync(new URL("../lith/Caddyfile", import.meta.url), "utf8");

    expect(caddy).toContain("@wgpost path_regexp wgpost ^/post/([0-9]+)$");
    expect(caddy).toContain("/api/whistlegraph-og?id={re.wgpost.1}");
  });
});
