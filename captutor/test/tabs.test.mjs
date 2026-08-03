import assert from "node:assert/strict";
import { createServer } from "node:http";
import test from "node:test";

test("prepares one authored tab set and activates targets without detaching control", async (t) => {
  let tabs = [
    { id: "control", type: "page", title: "Fuser", url: "https://app.fuser.studio/w/me" },
    { id: "stray", type: "page", title: "Private browsing", url: "https://example.test/stray" },
  ];
  const activated = [];
  let next = 1;
  const server = createServer((request, response) => {
    const url = new URL(request.url, "http://127.0.0.1");
    if (url.pathname === "/json/list") {
      response.setHeader("content-type", "application/json");
      response.end(JSON.stringify(tabs));
      return;
    }
    if (url.pathname === "/json/new" && request.method === "PUT") {
      const target = {
        id: `new-${next++}`, type: "page", title: "loading",
        url: decodeURIComponent(url.search.slice(1)),
      };
      tabs.push(target);
      response.setHeader("content-type", "application/json");
      response.end(JSON.stringify(target));
      return;
    }
    const close = url.pathname.match(/^\/json\/close\/(.+)$/);
    if (close) {
      tabs = tabs.filter((target) => target.id !== close[1]);
      response.end("Target is closing");
      return;
    }
    const activate = url.pathname.match(/^\/json\/activate\/(.+)$/);
    if (activate) {
      activated.push(activate[1]);
      response.end("Target activated");
      return;
    }
    response.statusCode = 404;
    response.end();
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  t.after(() => server.close());
  process.env.CDP_HOST = "127.0.0.1";
  process.env.CDP_PORT = String(server.address().port);
  const { prepareTabs, activateTab } = await import(`../lib/tabs.mjs?test=${Date.now()}`);

  await prepareTabs([
    { name: "one", url: "https://motion.test/one" },
    { name: "two", url: "https://motion.test/two" },
  ]);
  await activateTab(["missing.example", "/two"]);

  assert.deepEqual(tabs.map((target) => target.url), [
    "https://app.fuser.studio/w/me",
    "https://motion.test/one",
    "https://motion.test/two",
  ]);
  assert.deepEqual(activated, ["control", "new-2"]);
});
