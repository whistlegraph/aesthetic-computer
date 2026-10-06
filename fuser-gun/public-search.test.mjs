import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";
import { normalizeSearchResults, searchInstagram } from "./public-search.mjs";

const result = (url, title, rest = {}) => ({ url, title, ...rest });

test("accepts indexed authorship, deduplicates permalinks, rejects mentions and lookalike domains", () => {
  const data = { provider: "exa", results: [
    result("https://www.instagram.com/p/product1/", 'Areaware on Instagram: "New Cache Box"', { image: "https://scontent.cdninstagram.com/image.jpg", publishedDate: "2026-01-02" }),
    result("https://www.instagram.com/areaware/p/product1/?utm_source=search", "Same post"),
    result("https://instagram.com/p/product2/", "Areaware | New puzzle collection"),
    result("https://instagram.com/shop/p/product3/", 'Areaware on Instagram: "A reseller mention"'),
    result("https://instagram.com/p/product4/", 'Slowdown Studio on Instagram: "New @areaware puzzle"'),
    result("https://instagram.com.evil.test/p/product5/", 'Areaware on Instagram: "New vase"'),
    result("https://instagram.com/areaware/", "Areaware (@areaware)"),
  ] };
  const { media, excluded, profile } = normalizeSearchResults("areaware", data);
  assert.deepEqual(media.map((p) => p.id), ["product1", "product2"]);
  assert.equal(excluded.length, 3);
  assert.equal(profile.name, "Areaware");
  assert.equal(media[0].caption, "New Cache Box");
  assert.equal(media[0].timestamp, null);
  assert.equal(media[0].like_count, null);
  assert.equal(media[0].media_type, "UNKNOWN");
  assert.equal(media[0].discovery.indexed_published_date, "2026-01-02");
});

test("reels remain videos; thumbnails cannot point to a local or unrelated host", () => {
  const { media } = normalizeSearchResults("areaware", { results: [
    result("https://instagram.com/areaware/reel/reel1/", "New vase", { image: "http://127.0.0.1/private" }),
    result("https://instagram.com/areaware/p/post1/", "New vase", { image: "https://unrelated.test/image" }),
  ] });
  assert.equal(media[0].media_type, "VIDEO");
  assert.equal(media[0].thumbnail_url, null);
  assert.equal(media[1].media_url, null);
});

test("keyless search uses the Instagram domain and reports API errors without fixtures", async (t) => {
  t.mock.method(globalThis, "fetch", async (url, options) => {
    assert.match(url, /^https:\/\/mcp\.exa\.ai\//);
    assert.equal(options.headers.Authorization, undefined);
    const request = JSON.parse(options.body);
    assert.deepEqual(request.params.arguments.includeDomains, ["instagram.com"]);
    return new Response(`event: message\ndata: ${JSON.stringify({ id: 1, result: { content: [{ type: "text", text: JSON.stringify({ results: [] }) }] } })}\n\n`, { headers: { "content-type": "text/event-stream" } });
  });
  assert.equal((await searchInstagram("areaware", 3)).provider, "exa");
  globalThis.fetch.mock.mockImplementation(async () => new Response("limited", { status: 429 }));
  await assert.rejects(searchInstagram("areaware"), /HTTP 429; no fixture substituted/);
});

test("saved search evidence reaches the brief without fabricated counts, dates or Graph provenance", () => {
  const dir = mkdtempSync(join(tmpdir(), "ig-search-test-"));
  try {
    const evidence = join(dir, "search.json");
    writeFileSync(evidence, JSON.stringify({ provider: "exa", results: [result("https://instagram.com/areaware/p/cache1/", 'Areaware on Instagram: "New Cache Box collection"')] }));
    const run = (...args) => spawnSync(process.execPath, [join(import.meta.dirname, "ig-gun.mjs"), ...args], { encoding: "utf8", env: { ...process.env, IG_GUN_OUT: dir } });
    let r = run("scout", "areaware", "--search-results", evidence, "--no-media");
    assert.equal(r.status, 0, r.stderr);
    r = run("brief", "areaware", "--product", "Cache Box");
    assert.equal(r.status, 0, r.stderr);
    const output = join(dir, "areaware");
    assert.match(readFileSync(join(output, "brief.md"), "utf8"), /public-search indexed public search/);
    assert.doesNotMatch(readFileSync(join(output, "data/README.md"), "utf8"), /official Graph API/);
    assert.match(readFileSync(join(output, "data/signals.csv"), "utf8"), /engagement unknown/);
    assert.equal(JSON.parse(readFileSync(join(output, "provenance.json"))).reference[0].file, null);
    assert.notEqual(run("scout", "../outside", "--fixture").status, 0);
    writeFileSync(evidence, JSON.stringify({ results: [] }));
    assert.notEqual(run("scout", "areaware", "--search-results", evidence).status, 0);
    assert.equal(JSON.parse(readFileSync(join(output, "api.json"))).source, "public-search");
    assert.equal(JSON.parse(readFileSync(join(output, "search.json"))).results.length, 1);
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
