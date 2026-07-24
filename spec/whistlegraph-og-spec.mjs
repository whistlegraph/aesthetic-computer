import { readFileSync } from "node:fs";
import { handler as whistlegraphOgHandler } from "../system/netlify/functions/whistlegraph-og.mjs";

describe("Whistlegraph Open Graph previews", () => {
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
