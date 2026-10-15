import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { publicProducts } from "../shared/public-products.mjs";
import { properties } from "./machine-properties.mjs";
import { isPrivateProductPath } from "../shared/private-product-paths.mjs";
import { handler as apiDocs } from "../system/netlify/functions/api-docs.mjs";
import { handler as pieceDocs } from "../system/netlify/functions/docs.js";

const root = fileURLToPath(new URL("../", import.meta.url));
const read = file => fs.readFileSync(path.join(root, file), "utf8");
const event = path => ({ httpMethod: "GET", path, headers: { host: "aesthetic.computer" } });

test("product discovery retains purpose, access and only selected public interfaces", async () => {
  const json = JSON.parse((await apiDocs(event("/api-docs.json"), {})).body);
  assert.deepEqual(json.products, publicProducts);
  assert.deepEqual(json.products.map(p => p.id).sort(), ["choir", "mime", "nopaint", "oskiewar", "sotce-net"]);
  const directory = JSON.parse(read("system/public/network.json"));
  for (const product of publicProducts) {
    const entry = directory.properties.find(p => p.id === product.id);
    assert.equal(entry.url, product.url);
    assert.equal(entry.summary, product.summary);
    assert.equal(entry.access, product.access);
    assert.deepEqual(entry.endpoints, product.endpoints);
    assert.ok(product.summary && product.access);
    assert.doesNotMatch(JSON.stringify(entry), /\bsource\b|admin|whistlegraph-turn|userId|deviceId/i);
  }
  assert.deepEqual(json.products.flatMap(p => p.endpoints).map(e => [e.method, e.url, e.authentication]),
    [["GET", "https://mime.ac/api/mime?feed=1", "None"]]);
  const html = (await apiDocs({ ...event("/api"), headers: { accept: "text/html" } }, {})).body;
  for (const p of publicProducts) assert.ok(html.includes(p.url));
  assert.ok(properties.find(p => p.host === "whistlegraph.app").links.some(([, url]) => url === "https://mime.ac/"));
});

test("public docs omit private products regardless of curated entries", async () => {
  assert.equal((await pieceDocs(event("/docs/pieces:sotce-net"), {})).statusCode, 404);
  assert.equal((await pieceDocs(event("/docs/pieces:nopaint"), {})).statusCode, 404);
  assert.equal((await pieceDocs(event("/docs/pieces:notepat"), {})).statusCode, 200);
  assert.equal(isPrivateProductPath("system/public/aesthetic.computer/disks/oskiewar-future.mjs"), true);
  assert.equal(isPrivateProductPath("system/public/aesthetic.computer/lib/oskiewar-analytics.mjs"), false);
  assert.equal(isPrivateProductPath("system/public/nopaint.art/ai/app.mjs"), true);
  assert.equal(isPrivateProductPath("system/public/aesthetic.computer/systems/nopaint.mjs"), false);
  assert.equal(isPrivateProductPath("system/public/aesthetic.computer/lib/nopaint-pieces.mjs"), false);
  assert.equal(isPrivateProductPath("system/public/nopaint.art/gallery/gallery.json"), false);
});

test("published maps exclude handler inventories, hidden pieces and private source links", () => {
  const sitemap = read("system/public/sitemap.html");
  const catalog = read("system/public/llms-full.txt");
  for (const text of [sitemap, catalog]) {
    assert.doesNotMatch(text, /admin-rebake|agent-memory-ingest|\/api\/sotce-net|whistlegraph-turn|\/aa["\)]|_test-hook|cross-tab-test/);
    assert.doesNotMatch(text, /\/disks\/oskiewar[^\s"<)]*|\/mime\/index\.html/);
    assert.match(text, /notepat/);
  }
  for (const p of publicProducts) assert.ok(sitemap.includes(p.url));
  assert.match(sitemap, /https:\/\/mime\.ac\/api\/mime\?feed=1/);
  assert.match(catalog, /\[notepat\].*: A melodic keyboard instrument\./);
  assert.doesNotMatch(catalog, /: Notepat, 2024|Photo capture tool with filters|https:\/\/kidlisp\.com\/learn/);
});

test("installing private pieces and new handlers cannot change generated discovery", t => {
  const temp = fs.mkdtempSync(path.join(os.tmpdir(), "ac-public-discovery-"));
  t.after(() => fs.rmSync(temp, { recursive: true, force: true }));
  const put = (file, body) => {
    const dest = path.join(temp, file);
    fs.mkdirSync(path.dirname(dest), { recursive: true });
    fs.writeFileSync(dest, body);
  };
  const files = [
    "private-products.json", "shared/private-product-paths.mjs", "shared/public-products.mjs",
    "utilities/public-piece-catalog.mjs", "utilities/machine-properties.mjs",
    "utilities/sitemap.template.html", "utilities/update-sitemap.mjs",
    "utilities/generate-llms-txt.mjs", "utilities/generate-machine-manifests.mjs",
    "system/netlify/functions/docs.js", "system/netlify/functions/api-docs.mjs",
    "system/backend/http.mjs", "system/public/aesthetic.computer/lib/helpers.mjs",
    "system/public/aesthetic.computer/lib/prompt-commands.mjs", "lith/Caddyfile",
  ];
  for (const file of files) put(file, read(file));
  put("package.json", '{"type":"module"}');
  // Only hand-authored shared manifests are required in the public checkout.
  for (const p of properties) if (p.file && !isPrivateProductPath(`system/public/${p.file}`)) put(`system/public/${p.file}`, read(`system/public/${p.file}`));
  const disk = "system/public/aesthetic.computer/disks/";
  put(disk + "hello.mjs", '// Hello\n// Public example\nexport function meta() { return {desc: "Public example"}; }');
  put(disk + "scratch.mjs", '// Scratch\n// Hidden draft\nexport function paint() {}');
  put(disk + "aa.mjs", '// Bridge\n// Private host\nexport function meta() { return {}; }');
  const generate = () => {
    for (const script of ["generate-machine-manifests.mjs", "generate-llms-txt.mjs", "update-sitemap.mjs"]) {
      execFileSync(process.execPath, ["utilities/" + script], { cwd: temp, stdio: "pipe" });
    }
    return ["sitemap.html", "llms-full.txt", "network.json"].map(file => fs.readFileSync(path.join(temp, "system/public/", file), "utf8"));
  };
  const before = generate();
  put(disk + "oskiewar-secret.mjs", '// Secret\n// PRIVATE-SENTINEL\nexport function meta() { return {desc: "PRIVATE-SENTINEL"}; }');
  put("system/netlify/functions/future-private-admin.mjs", 'export function handler() { return "PRIVATE-SENTINEL"; }');
  const after = generate();
  assert.deepEqual(after, before);
  for (const text of after) assert.doesNotMatch(text, /PRIVATE-SENTINEL|future-private-admin|scratch\.mjs|\/aa["\)]/);
  assert.match(after[0], /hello/);
});
