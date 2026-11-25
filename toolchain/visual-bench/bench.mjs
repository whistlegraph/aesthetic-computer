#!/usr/bin/env node
// visual-bench, 2026.11.25
// Which image model should AC's visual compute use? This sends the same few
// paintings with the same few notes (image in, note in, image out) to every
// image-edit model AC can reach through its own endpoints, times each call,
// and writes a table and a contact sheet per note for picking by eye.
//
//   node toolchain/visual-bench/bench.mjs                       the plan and its cost; nothing is spent
//   AC_BEARER=<token> node toolchain/visual-bench/bench.mjs --yes      run it
//
// Paths (each is AC's own endpoint, with its catalog, sign-in and billing):
//   nopaint   POST /api/nopaint-inference   every catalog model at --size (OpenRouter, fal)
//   easel     POST /api/easel-image         OpenAI gpt-image-2.x edits (reference image); fal Flare
//                                           only generates, so it is listed and skipped
//
// Options:
//   --yes                spend: without it nothing is posted (the catalog GET is free)
//   --paths nopaint,easel  which paths (default both)
//   --models a,b         only models whose id contains one of these
//   --runs N             calls per model, canvas and note (default 1)
//   --canvases N --notes N   how many of the fixed ones (default 2 and 2)
//   --max N              skip models that cost more than N Braincells a call (default 25000)
//   --size 256           the painting's size for nopaint (easel paints at 1024)
//   --quality low        easel quality (low, medium, high)
//   --out DIR            where results go (default tmp/visual-bench/<time>)
//   --base URL           default https://aesthetic.computer
//   --offline            plan from a catalog file (--catalog FILE) instead of asking
// The bearer is the operator's, given at run time (AC_BEARER or --bearer).
// This script never reads a token from disk.
//
// No Paint's caller: toolchain/nopaint/ai-bench.mjs (its tuning's models and notes).

import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { deflateSync, crc32 } from "node:zlib";
import { randomUUID, randomBytes } from "node:crypto";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

export const NOTES = ["make it night", "go abstract", "add a creature", "another style"];

// ---- fixed paintings ----------------------------------------------------------
// Drawn here, so every run on every machine sends the same bytes.
export function png(side, rgba) {
  const rows = [];
  for (let y = 0; y < side; y++) rows.push(Buffer.from([0]), rgba.subarray(y * side * 4, (y + 1) * side * 4));
  const chunk = (kind, data) => { const body = Buffer.concat([Buffer.from(kind), data]), out = Buffer.alloc(body.length + 8);
    out.writeUInt32BE(data.length, 0); body.copy(out, 4); out.writeUInt32BE(crc32(body), body.length + 4); return out; };
  const head = Buffer.alloc(13); head.writeUInt32BE(side, 0); head.writeUInt32BE(side, 4); head.set([8, 6, 0, 0, 0], 8);
  return Buffer.concat([Buffer.from("89504e470d0a1a0a", "hex"), chunk("IHDR", head), chunk("IDAT", deflateSync(Buffer.concat(rows))), chunk("IEND", Buffer.alloc(0))]);
}
function paint(side, pixel) {
  const rgba = Buffer.alloc(side * side * 4);
  for (let y = 0; y < side; y++) for (let x = 0; x < side; x++) rgba.set([...pixel(x / side, y / side), 255], (y * side + x) * 4);
  return rgba;
}
export const CANVASES = {
  // A sun over hills, on a sky.
  landscape: (u, v) => (u - 0.7) ** 2 + (v - 0.25) ** 2 < 0.008 ? [255, 210, 60]
    : v > 0.62 + 0.06 * Math.sin(u * 9) ? [60 + 40 * v, 140, 70] : [120 + 80 * v, 170 + 60 * v, 240],
  // A red square and a blue circle on paper.
  shapes: (u, v) => (u - 0.65) ** 2 + (v - 0.6) ** 2 < 0.04 ? [40, 70, 200]
    : u > 0.15 && u < 0.45 && v > 0.2 && v < 0.5 ? [220, 40, 40] : [245, 240, 228],
  // Loose strokes, as a No Paint painting often is.
  strokes: (u, v) => Math.abs(Math.sin(u * 12 + v * 5) - (v * 2 - 1)) < 0.08 ? [20, 20, 20]
    : Math.abs(Math.cos(v * 14) * 0.4 + 0.5 - u) < 0.03 ? [250, 120, 0] : [255, 255, 255],
};
export const canvas = (name, side = 256) => png(side, paint(side, CANVASES[name]));

// ---- the paths ------------------------------------------------------------------
// Easel's hosted models (system/backend/easel-images.mjs, hostedImageModels).
// The test checks this list against that file.
export const EASEL_MODELS = {
  openai: ["gpt-image-2.6-flare", "gpt-image-2.6-sunburst", "gpt-image-2.5-flare", "gpt-image-2.5-sunburst", "gpt-image-2"],
  fal: ["openai/gpt-image-2.5/flare/text-to-image"], // FAL_FLARE in aesel/media/picture/illy.mjs
};
// What an Easel image costs, as system/backend/easel-image-credits.mjs prices one
// that reports no usage: the output at its quality, the prompt, and the
// reference at a fifth of its $0.05 bound; twice that at 200,000 Braincells a dollar.
const OUTPUT_USD = { "gpt-image-2": [0.006, 0.053, 0.211], "gpt-image-2.5": [0.00588, 0.01317, 0.05268] };
export function easelBraincells(model, quality = "low", prompt = "") {
  const row = OUTPUT_USD[/gpt-image-2\.5/.test(model) ? "gpt-image-2.5" : "gpt-image-2"];
  const usd = row[{ low: 0, medium: 1, high: 2 }[quality]] + Math.ceil(Buffer.byteLength(prompt) * 1.25) * 5e-6 + 0.01;
  return Math.ceil(usd * 2 * 200_000);
}
export const easelPrompt = (note) => `Repaint this painting: ${note}. Keep it a painting, the same size and framing.`;

// Every model a path offers: { path, model, provider, braincells, quote?, skip? }.
export async function models({ paths, fetch, base, size, quality, catalog }) {
  const list = [];
  if (paths.includes("nopaint")) {
    const offered = catalog || await (await fetch(`${base}/api/nopaint-inference`, { headers: { Accept: "application/json" } })).json();
    for (const model of offered.models || []) {
      if (!model.available || model.size?.[0] !== size || model.size?.[1] !== size) continue;
      list.push({ path: "nopaint", model: model.id, provider: model.provider || model.id.split(":")[0], braincells: model.braincells, quote: model.quote,
        ...(model.timing?.averageSeconds ? { typical: model.timing.averageSeconds } : {}) });
    }
  }
  if (paths.includes("easel")) for (const [provider, ids] of Object.entries(EASEL_MODELS)) for (const model of ids)
    list.push({ path: "easel", model, provider, braincells: easelBraincells(model, quality, easelPrompt(NOTES[0])),
      ...(provider === "fal" ? { skip: "generates only; takes no image in" } : {}) });
  return list;
}

// One call: the painting and the note go in; { png (bytes), braincells } come back.
export async function call(entry, { image, note, fetch, base, bearer, quality, strength = 0.5 }) {
  const headers = { "Content-Type": "application/json", Authorization: `Bearer ${bearer}`, "X-AC-Client": "visual-bench" };
  const body = entry.path === "nopaint"
    ? { requestId: randomUUID(), strength, seed: 1234, hint: note, image: image.toString("base64"), quote: entry.quote, model: entry.model }
    : { jobId: randomBytes(16).toString("hex"), prompt: easelPrompt(note), provider: entry.provider, model: entry.model, size: "1024x1024", quality, reference: image.toString("base64") };
  const response = await fetch(`${base}/api/${entry.path === "nopaint" ? "nopaint-inference" : "easel-image"}`, { method: "POST", headers, body: JSON.stringify(body) });
  const value = await response.json().catch(() => ({}));
  const picture = entry.path === "nopaint" ? value.image : value.png;
  if (!response.ok || typeof picture !== "string") throw new Error(value.error || `HTTP ${response.status}`);
  return { png: Buffer.from(picture, "base64"), braincells: entry.path === "nopaint" ? value.billing?.braincells ?? entry.braincells : value.braincells ?? entry.braincells };
}

// ---- numbers ----------------------------------------------------------------------
export const quantile = (list, q) => {
  if (!list.length) return null;
  const sorted = list.toSorted((a, b) => a - b), at = (sorted.length - 1) * q, low = Math.floor(at);
  return sorted[low] + (sorted[Math.ceil(at)] - sorted[low]) * (at - low);
};
export function summarize(results) {
  const rows = new Map();
  for (const result of results) {
    const key = `${result.path} ${result.model}`;
    if (!rows.has(key)) rows.set(key, { path: result.path, model: result.model, provider: result.provider, calls: 0, failures: 0, ms: [], braincells: [] });
    const row = rows.get(key);
    row.calls++;
    if (result.error) row.failures++; else { row.ms.push(result.ms); row.braincells.push(result.braincells); }
  }
  return [...rows.values()].map((row) => ({ path: row.path, model: row.model, provider: row.provider, calls: row.calls, failures: row.failures,
    braincells: quantile(row.braincells, 0.5), median: quantile(row.ms, 0.5), p90: quantile(row.ms, 0.9) }))
    .sort((a, b) => (a.median ?? Infinity) - (b.median ?? Infinity));
}
const seconds = (ms) => ms === null ? "–" : `${(ms / 1000).toFixed(1)} s`;
export function table(rows) {
  return ["| model | provider | path | Braincells a call | median | p90 | failures |", "| --- | --- | --- | ---: | ---: | ---: | ---: |",
    ...rows.map((row) => `| ${row.model} | ${row.provider} | ${row.path} | ${row.braincells ?? "–"} | ${seconds(row.median)} | ${seconds(row.p90)} | ${row.failures}/${row.calls} |`)].join("\n") + "\n";
}
const slug = (text) => String(text).toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
// One page per note: a row per model, a column per painting, the input on top.
export function sheet(note, canvases, results) {
  const cell = (src, caption) => `<figure><img src="${src}" alt=""><figcaption>${caption}</figcaption></figure>`;
  const byModel = new Map();
  for (const result of results.filter((item) => item.note === note)) {
    if (!byModel.has(result.model)) byModel.set(result.model, []);
    byModel.get(result.model).push(result);
  }
  const rows = [...byModel].map(([model, list]) => `<tr><th>${model}</th>${canvases.map((name) => {
    const hit = list.find((item) => item.canvas === name && !item.error) || list.find((item) => item.canvas === name);
    return `<td>${hit?.file ? cell(hit.file, `${seconds(hit.ms)} · ${hit.braincells} BC`) : `<span class="no">${hit?.error || "–"}</span>`}</td>`;
  }).join("")}</tr>`);
  return `<!doctype html><meta charset="utf-8"><title>${note}</title>
<style>body{font:14px system-ui;margin:16px;background:#fff;color:#111}img{width:256px;height:256px;image-rendering:pixelated;display:block}
th{text-align:left;font-weight:600;padding-right:12px;max-width:200px;word-break:break-all}figure{margin:4px}figcaption{font-size:12px;color:#555}.no{color:#b00;font-size:12px}</style>
<h1>“${note}”</h1><table><tr><th>painting</th>${canvases.map((name) => `<td>${cell(`img/input/${name}.png`, name)}</td>`).join("")}</tr>${rows.join("")}</table>\n`;
}

// ---- the run ------------------------------------------------------------------------
export async function bench(options) {
  const { fetch = globalThis.fetch, base = "https://aesthetic.computer", bearer = null, yes = false, runs = 1, size = 256, quality = "low",
    paths = ["nopaint", "easel"], only = [], exact = false, max = 25000, notes = NOTES.slice(0, 2), canvases = Object.keys(CANVASES).slice(0, 2), out, catalog = null, say = console.log } = options;
  const all = await models({ paths, fetch, base, size, quality, catalog });
  // only: parts of a model id, or (exact) whole ids.
  const chosen = all.filter((entry) => !only.length || (exact ? only.includes(entry.model) : only.some((part) => entry.model.includes(part))));
  const plan = chosen.map((entry) => ({ ...entry, skip: entry.skip || (entry.braincells > max ? `costs ${entry.braincells} a call, over --max ${max}` : null) }));
  const each = runs * notes.length * canvases.length, live = plan.filter((entry) => !entry.skip);
  const cost = live.reduce((sum, entry) => sum + entry.braincells * each, 0);
  for (const entry of plan) say(`${entry.skip ? "skip" : "run "} ${entry.path.padEnd(7)} ${entry.model.padEnd(58)} ${String(entry.braincells).padStart(6)} BC${entry.skip ? `  (${entry.skip})` : `  × ${each}`}`);
  say(`${live.length} models × ${each} calls = ${live.length * each} calls, about ${cost.toLocaleString("en-US")} Braincells`);
  if (!yes) { say("Nothing was spent. Add --yes (with AC_BEARER) to run."); return { plan, cost, results: null }; }
  if (!bearer) throw new Error("Give a signed-in bearer at run time: AC_BEARER=<token>");
  mkdirSync(join(out, "img", "input"), { recursive: true });
  const images = Object.fromEntries(canvases.map((name) => [name, canvas(name, size)]));
  for (const [name, bytes] of Object.entries(images)) writeFileSync(join(out, "img", "input", `${name}.png`), bytes);
  const results = [];
  // One call at a time: the No Paint path refuses a second move from the same painter while one runs.
  for (const entry of live) for (let run = 1; run <= runs; run++) for (const note of notes) for (const name of canvases) {
    const started = performance.now(), result = { path: entry.path, model: entry.model, provider: entry.provider, note, canvas: name, run };
    try {
      const answer = await call(entry, { image: images[name], note, fetch, base, bearer, quality });
      result.ms = Math.round(performance.now() - started); result.braincells = answer.braincells;
      result.file = `img/${slug(entry.path + " " + entry.model)}/${name}-${slug(note)}-r${run}.png`;
      mkdirSync(join(out, "img", slug(entry.path + " " + entry.model)), { recursive: true });
      writeFileSync(join(out, result.file), answer.png);
    } catch (error) { result.ms = Math.round(performance.now() - started); result.error = String(error.message || error).slice(0, 200); }
    results.push(result);
    say(`${result.error ? "FAIL" : "ok  "} ${entry.model} "${note}" ${name} r${run}: ${seconds(result.ms)}${result.error ? ` ${result.error}` : ` · ${result.braincells} BC`}`);
    writeFileSync(join(out, "results.json"), JSON.stringify({ size, quality, notes, canvases, runs, results }, null, 2));
  }
  const rows = summarize(results);
  writeFileSync(join(out, "table.md"), table(rows));
  for (const note of notes) writeFileSync(join(out, `sheet-${slug(note)}.html`), sheet(note, canvases, results));
  say(`\n${table(rows)}\nspent ${results.reduce((sum, item) => sum + (item.braincells || 0), 0).toLocaleString("en-US")} Braincells · ${out}`);
  return { plan, cost, results, rows };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2), flag = (name) => args.includes(`--${name}`), value = (name, fallback) => { const at = args.indexOf(`--${name}`); return at >= 0 ? args[at + 1] : fallback; };
  const list = (name, fallback) => value(name, null)?.split(",").map((item) => item.trim()).filter(Boolean) ?? fallback;
  await bench({
    yes: flag("yes"), bearer: value("bearer", process.env.AC_BEARER || null), base: value("base", "https://aesthetic.computer"),
    paths: list("paths", ["nopaint", "easel"]), only: list("models", []), runs: Number(value("runs", 1)), max: Number(value("max", 25000)),
    size: Number(value("size", 256)), quality: value("quality", "low"),
    notes: list("note-list", NOTES.slice(0, Number(value("notes", 2)))), canvases: Object.keys(CANVASES).slice(0, Number(value("canvases", 2))),
    out: value("out", join("tmp", "visual-bench", new Date().toISOString().replace(/[:.]/g, "-"))),
    catalog: flag("offline") ? JSON.parse(readFileSync(value("catalog"), "utf8")) : null,
  }).catch((error) => { console.error(error.message); process.exit(1); });
}
