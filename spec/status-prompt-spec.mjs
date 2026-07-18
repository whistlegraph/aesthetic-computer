import { collectStatus } from "../status-prompt/worker.mjs";

const json = (body, init = {}) => new Response(JSON.stringify(body), {
  status: 200,
  headers: { "content-type": "application/json", ...init.headers },
  ...init,
});

describe("status.prompt.ac checks", () => {
  it("covers core API, database, asset delivery, and upload signing", async () => {
    const calls = [];
    const fakeFetch = async (url) => {
      calls.push(url);
      if (url.includes("chat-messages")) return json({ messages: [] });
      if (url.includes("pals.png")) return new Response("png", { headers: { "content-type": "image/png" } });
      if (url.includes("presigned-upload-url")) return json({ uploadURL: "https://spaces.example/signed", slug: "new.png" });
      return new Response("ok");
    };

    const status = await collectStatus(fakeFetch);
    expect(status.ok).toBeTrue();
    expect(status.checks.map((check) => check.id)).toEqual([
      "front-door", "api-docs", "database-read", "asset-delivery", "upload-presign",
    ]);
    expect(calls.some((url) => url.includes("chat-messages"))).toBeTrue();
  });

  it("offers per-handle diagnostics only when explicitly requested", async () => {
    const fakeFetch = async (url) => {
      if (url.includes("chat-messages")) return json({ messages: [] });
      if (url.includes("pals.png")) return new Response("png", { headers: { "content-type": "image/png" } });
      if (url.includes("/api/profile/")) return json({ sub: "auth0|sol" });
      if (url.includes("media-collection")) return json({ files: ["https://user.aesthetic.computer/broken.png"] });
      if (url.includes("broken.png")) return new Response("AccessDenied", { status: 403 });
      if (url.includes("presigned-upload-url")) return json({ uploadURL: "https://spaces.example/signed", slug: "new.png" });
      return new Response("ok");
    };

    const status = await collectStatus(fakeFetch, "@sol");
    expect(status.ok).toBeFalse();
    expect(status.checks.find((check) => check.id === "latest-painting").detail).toBe("HTTP 403");
  });
});
