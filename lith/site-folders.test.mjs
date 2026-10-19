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
  assert.equal(where("aesthetic.computer", "/nopaint.art/gallery/"), "https://nopaint.art/gallery/");
  assert.equal(where("aesthetic.computer", "/nopaint.art/gallery/gallery.json", "?v=1"), "https://nopaint.art/gallery/gallery.json?v=1");
  assert.equal(where("www.aesthetic.computer", "/kidlisp.com/"), "https://kidlisp.com/");
  assert.equal(where("prompt.ac", "/kidlisp.com/keeps.html"), "https://kidlisp.com/keeps.html");
});

test("the studio under nopaint.art/ai belongs to notepaint.art; the rest of the folder stays nopaint.art", () => {
  assert.ok(hosts.has("notepaint.art"), "the Caddyfile serves notepaint.art");
  assert.equal(where("aesthetic.computer", "/nopaint.art/ai/"), "https://notepaint.art/ai/");
  assert.equal(where("aesthetic.computer", "/nopaint.art/ai/app.mjs", "?v=1"), "https://notepaint.art/ai/app.mjs?v=1");
  assert.equal(where("aesthetic.computer", "/nopaint.art/gallery/"), "https://nopaint.art/gallery/");
  assert.equal(where("aesthetic.computer", "/nopaint.art/classic/"), "https://nopaint.art/classic/");
  // Without the host in Caddy, the folder's own host still gets it.
  const without = new Set([...hosts].filter((h) => h !== "notepaint.art"));
  assert.equal(siteFolderRedirect({ host: "aesthetic.computer", path: "/nopaint.art/ai/" }, { folders, hosts: without }), "https://nopaint.art/ai/");
});

test("No Paint's sign-in page stays on the apex: it is the Auth0 callback and Stripe return", () => {
  assert.equal(where("aesthetic.computer", "/nopaint.art/ai/signin.html", "?code=x&state=y"), null);
});

test("a folder with no host of its own is served from the apex", () => {
  const only = { folders: siteFolders(["downloads.example", "nopaint.art"]), hosts };
  assert.equal(siteFolderRedirect({ host: "aesthetic.computer", path: "/downloads.example/build-1.zip" }, only), null);
  assert.equal(siteFolderRedirect({ host: "aesthetic.computer", path: "/nopaint.art/gallery/" }, only), "https://nopaint.art/gallery/");
});

test("other hosts and ordinary apex paths are untouched", () => {
  assert.equal(where("nopaint.art", "/ai/"), null);
  assert.equal(where("aesthetic.computer", "/"), null);
  assert.equal(where("aesthetic.computer", "/prompt"), null);
  assert.equal(where("aesthetic.computer", "/api/tv"), null);
});
