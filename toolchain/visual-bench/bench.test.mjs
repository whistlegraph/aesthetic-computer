// The visual bench, against a played AC: nothing real is reached or spent.
//
//   node --test toolchain/visual-bench/bench.test.mjs

import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { bench, canvas, models, quantile, summarize, EASEL_MODELS, easelBraincells, CANVASES } from "./bench.mjs";

const CATALOG = { models: [
  { id: "ac-openrouter:tencent/hy-image-v3.5-preview", provider: "openrouter", size: [256, 256], braincells: 4000, quote: "q-hy", available: true },
  { id: "ac-openrouter:tencent/hy-image-v3.5-preview@1024", provider: "openrouter", size: [1024, 1024], braincells: 4000, quote: "q-hy-1024", available: true },
  { id: "ac-openrouter:bytedance-seed/seedream-5-0-flash", provider: "openrouter", size: [256, 256], braincells: 8000, quote: "q-seed", available: true },
  { id: "ac-openrouter:microsoft/mai-image-2.5-pro", provider: "openrouter", size: [256, 256], braincells: 200000, quote: "q-mai", available: true },
  { id: "ac-openrouter:gone/model", size: [256, 256], braincells: 4000, quote: "q-gone", available: false },
] };
// A played AC: the catalog, and moves that answer with the painting they were sent.
function played({ failing = [] } = {}) {
  const posts = [];
  const fetch = async (url, init = {}) => {
    const json = (status, value) => ({ ok: status < 300, status, json: async () => value });
    if (!init.method || init.method === "GET") return json(200, CATALOG);
    const body = JSON.parse(init.body);
    posts.push({ url, body, auth: init.headers.Authorization });
    if (failing.includes(body.model)) return json(502, { error: "provider down" });
    await new Promise((resolve) => setTimeout(resolve, 5));
    if (url.endsWith("/api/nopaint-inference")) return json(200, { image: body.image, billing: { braincells: CATALOG.models.find((m) => m.id === body.model).braincells } });
    return json(200, { png: body.reference, braincells: 2600 });
  };
  return { fetch, posts };
}

test("the fixed paintings are the same bytes every time, and valid 256 × 256 PNGs", () => {
  for (const name of Object.keys(CANVASES)) {
    const a = canvas(name), b = canvas(name);
    assert.ok(a.equals(b));
    assert.equal(a.readUInt32BE(16), 256);
    assert.equal(a.readUInt32BE(20), 256);
  }
  assert.notDeepEqual(canvas("shapes"), canvas("landscape"));
});

test("the easel models are the ones AC's easel-image endpoint takes", () => {
  const source = readFileSync(new URL("../../system/backend/easel-images.mjs", import.meta.url), "utf8");
  const openai = JSON.parse(/openai: (\[[^\]]*\])/.exec(source)[1].replaceAll("'", '"'));
  assert.deepEqual(EASEL_MODELS.openai, openai);
  const flare = /FAL_FLARE = "([^"]+)"/.exec(readFileSync(new URL("../../aesel/media/picture/illy.mjs", import.meta.url), "utf8"))[1];
  assert.deepEqual(EASEL_MODELS.fal, [flare]);
  assert.ok(easelBraincells("gpt-image-2", "low") < easelBraincells("gpt-image-2", "medium"));
});

test("without --yes nothing is posted, and the plan prices every call", async () => {
  const { fetch, posts } = played(), said = [];
  const { plan, cost, results } = await bench({ fetch, say: (line) => said.push(line) });
  assert.equal(posts.length, 0);
  assert.equal(results, null);
  const nopaint = plan.filter((entry) => entry.path === "nopaint");
  assert.deepEqual(nopaint.map((entry) => entry.model), ["ac-openrouter:tencent/hy-image-v3.5-preview", "ac-openrouter:bytedance-seed/seedream-5-0-flash", "ac-openrouter:microsoft/mai-image-2.5-pro"]);
  assert.match(plan.find((entry) => entry.model.includes("mai")).skip, /over --max/);
  assert.match(plan.find((entry) => entry.provider === "fal").skip, /no image in/);
  const live = plan.filter((entry) => !entry.skip);
  assert.equal(cost, live.reduce((sum, entry) => sum + entry.braincells * 4, 0)); // 2 notes × 2 paintings
  assert.ok(said.at(-1).includes("Nothing was spent"));
});

test("a run asks for a bearer, sends each model the paintings and notes one at a time, and writes the table and sheets", async () => {
  const out = mkdtempSync(join(tmpdir(), "visual-bench-"));
  const { fetch, posts } = played({ failing: ["ac-openrouter:bytedance-seed/seedream-5-0-flash"] });
  await assert.rejects(bench({ fetch, yes: true, out, say: () => {} }), /bearer/);
  const { results, rows } = await bench({ fetch, yes: true, bearer: "operator-token", out, only: ["hy-image", "seedream", "gpt-image-2.5-flare"], say: () => {} });
  assert.equal(posts.length, 12); // 3 models × 2 notes × 2 paintings
  assert.ok(posts.every((post) => post.auth === "Bearer operator-token"));
  const move = posts.find((post) => post.url.endsWith("/api/nopaint-inference")).body;
  assert.match(move.requestId, /^[0-9a-f-]{36}$/);
  assert.equal(move.strength, 0.5);
  assert.equal(move.quote, "q-hy");
  assert.ok(Buffer.from(move.image, "base64").equals(canvas("landscape")) || Buffer.from(move.image, "base64").equals(canvas("shapes")));
  const edit = posts.find((post) => post.url.endsWith("/api/easel-image")).body;
  assert.equal(edit.model, "gpt-image-2.5-flare");
  assert.match(edit.jobId, /^[0-9a-f]{32}$/);
  assert.ok(edit.reference && /make it night|go abstract/.test(edit.prompt));
  assert.equal(results.filter((item) => item.error).length, 4);
  const seed = rows.find((row) => row.model.includes("seedream"));
  assert.equal(seed.failures, 4);
  assert.equal(seed.median, null);
  const hy = rows.find((row) => row.model.includes("hy-image"));
  assert.equal(hy.braincells, 4000);
  assert.ok(hy.median > 0 && hy.p90 >= hy.median);
  for (const file of ["table.md", "results.json", "sheet-make-it-night.html", "sheet-go-abstract.html", "img/input/landscape.png"]) assert.ok(existsSync(join(out, file)), file);
  const saved = results.find((item) => item.file);
  assert.ok(readFileSync(join(out, saved.file)).equals(canvas(saved.canvas)));
  assert.match(readFileSync(join(out, "table.md"), "utf8"), /\| ac-openrouter:tencent\/hy-image-v3.5-preview \| openrouter \| nopaint \| 4000 \|/);
  assert.match(readFileSync(join(out, "sheet-make-it-night.html"), "utf8"), /provider down/);
});

test("the numbers: median and p90 interpolate, and failures do not count toward time", () => {
  assert.equal(quantile([1, 2, 3, 4], 0.5), 2.5);
  assert.equal(quantile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 0.9), 91);
  const rows = summarize([{ path: "nopaint", model: "a", ms: 100, braincells: 4 }, { path: "nopaint", model: "a", ms: 300, braincells: 4 }, { path: "nopaint", model: "a", ms: 9e9, error: "x" }]);
  assert.deepEqual([rows[0].median, rows[0].failures, rows[0].calls], [200, 1, 3]);
});

test("models lists only what the catalog offers at the size", async () => {
  const list = await models({ paths: ["nopaint"], fetch: played().fetch, base: "x", size: 1024, quality: "low" });
  assert.deepEqual(list.map((entry) => entry.model), ["ac-openrouter:tencent/hy-image-v3.5-preview@1024"]);
});
