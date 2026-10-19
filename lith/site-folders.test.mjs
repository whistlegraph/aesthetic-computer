// node --test lith/site-folders.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync, readdirSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { siteFolders, caddyHosts, siteFolderRedirect } from "./site-folders.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const folders = siteFolders(readdirSync(join(here, "..", "system", "public")));
const hosts = caddyHosts(readFileSync(join(here, "Caddyfile"), "utf8"));
const where = (host, path, search) => siteFolderRedirect({ host, path, search }, { folders, hosts });

test("the real public tree names the sites and the Caddyfile names the hosts", () => {
  assert.ok(folders.has("nopaint.art") && folders.has("kidlisp.com"));
  assert.ok(!folders.has("aesthetic.computer"), "the apex's own runtime folder is not a site");
  assert.ok(hosts.has("nopaint.art") && hosts.has("kidlisp.com") && hosts.has("aesthetic.computer"));
});

test("a site folder under the apex redirects to its own host, path and query intact", () => {
  assert.equal(where("aesthetic.computer", "/nopaint.art/ai/"), "https://nopaint.art/ai/");
  assert.equal(where("aesthetic.computer", "/nopaint.art/ai/style.css", "?v=1"), "https://nopaint.art/ai/style.css?v=1");
  assert.equal(where("www.aesthetic.computer", "/kidlisp.com/"), "https://kidlisp.com/");
  assert.equal(where("prompt.ac", "/kidlisp.com/keeps.html"), "https://kidlisp.com/keeps.html");
});

test("No Paint's sign-in page stays on the apex: it is the Auth0 callback and Stripe return", () => {
  assert.equal(where("aesthetic.computer", "/nopaint.art/ai/signin.html", "?code=x&state=y"), null);
});

test("a folder with no host of its own is served from the apex", () => {
  const only = { folders: siteFolders(["downloads.example", "nopaint.art"]), hosts };
  assert.equal(siteFolderRedirect({ host: "aesthetic.computer", path: "/downloads.example/build-1.zip" }, only), null);
  assert.equal(siteFolderRedirect({ host: "aesthetic.computer", path: "/nopaint.art/ai/" }, only), "https://nopaint.art/ai/");
});

test("other hosts and ordinary apex paths are untouched", () => {
  assert.equal(where("nopaint.art", "/ai/"), null);
  assert.equal(where("aesthetic.computer", "/"), null);
  assert.equal(where("aesthetic.computer", "/prompt"), null);
  assert.equal(where("aesthetic.computer", "/api/tv"), null);
});
