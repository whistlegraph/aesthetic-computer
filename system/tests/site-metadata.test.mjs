import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { metadata } from "../public/aesthetic.computer/lib/parse.mjs";
import { canonicalPageUrl } from "../public/aesthetic.computer/lib/site-metadata.mjs";
import { handler as whistlegraph } from "../netlify/functions/whistlegraph-og.mjs";

test("branded home metadata agrees before and after Laklok boots", () => {
  for (const slug of ["prompt", "laklok"]) {
    const meta = metadata("www.laklok.com", slug, { title: "Prompt", desc: "Enter anything." });
    assert.equal(meta.title, "Laklok");
    assert.match(meta.desc, /community conversation/);
  }
  assert.equal(metadata("p5.aesthetic.computer", "prompt", { title: "Prompt" }).title, "p5 · Aesthetic Computer");
  assert.equal(metadata("p5.aesthetic.computer", "notepat", { title: "Notepat" }).title, "Notepat · Aesthetic Computer");
  assert.equal(metadata("aesthetic.computer", "prompt", { title: "Prompt" }).title, "Prompt · Aesthetic Computer");
});

test("canonical URLs retain public identities while removing query credentials", () => {
  for (const pathname of ["/", "/$abc", "/@artist/work", "/notepat", "/%E2%98%80"]) {
    assert.equal(canonicalPageUrl("www.aesthetic.computer", pathname + "?code=private#token"), "https://aesthetic.computer" + pathname);
  }
  assert.equal(canonicalPageUrl("www.notepat.com", "/"), "https://notepat.com/");
  assert.equal(canonicalPageUrl("p5.prompt.ac", "/"), "https://p5.aesthetic.computer/");
  assert.equal(canonicalPageUrl("prompt.ac", "/line?code=secret"), "https://prompt.ac/line");
  assert.equal(canonicalPageUrl("attacker.invalid", "/"), null);
  assert.equal(new URL(canonicalPageUrl("aesthetic.computer", "//attacker.invalid/path")).host, "aesthetic.computer");
});

test("Whistlegraph work and post cards keep their own canonical URLs", async () => {
  const graphs = JSON.parse(fs.readFileSync(new URL("../public/whistlegraph.org/graphs.json", import.meta.url)));
  const posts = JSON.parse(fs.readFileSync(new URL("../public/whistlegraph.org/posts.json", import.meta.url)));
  const work = (Array.isArray(graphs) ? graphs : graphs.graphs)[0];
  const post = (Array.isArray(posts) ? posts : posts.posts)[0];
  for (const [queryStringParameters, canonical] of [
    [{ code: work.code }, `https://whistlegraph.org/${work.code}`],
    [{ id: String(post.id) }, `https://whistlegraph.org/post/${post.id}`],
    [{ code: "unknown-metadata-test" }, "https://whistlegraph.org/"],
  ]) {
    const response = await whistlegraph({ queryStringParameters });
    assert.equal(response.statusCode, 200);
    assert.deepEqual([...response.body.matchAll(/<link rel="canonical" href="([^"]+)"/g)].map(m => m[1]), [canonical]);
    assert.deepEqual([...response.body.matchAll(/<meta property="og:url" content="([^"]+)"/g)].map(m => m[1]), [canonical]);
    assert.equal([...response.body.matchAll(/<meta property="og:type"/g)].length, 1);
  }
});
