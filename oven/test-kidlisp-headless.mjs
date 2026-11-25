#!/usr/bin/env node
// Headless KidLisp check: renders a handful of pieces through
// kidlisp-headless.mjs (no Chrome) and asserts each one draws, decodes, and
// stays inside its time budget; the cases that must fall back to Chrome must
// throw the expected reason.
//
//   node oven/test-kidlisp-headless.mjs          # inline sources, offline
//   node oven/test-kidlisp-headless.mjs --live   # + published $codes (network)
//   node oven/test-kidlisp-headless.mjs --out DIR  # also write each webp

import { writeFile, mkdir } from "node:fs/promises";
import sharp from "sharp";
import { renderKidlispDetailed } from "./kidlisp-headless.mjs";

const args = process.argv.slice(2);
const live = args.includes("--live");
const outDir = args.includes("--out") ? args[args.indexOf("--out") + 1] : null;

// The profile tile and the oven poster, as grabber.mjs asks for them.
const TILE = { width: 128, frames: 30, fps: 7.5, playbackFps: 15 };
const POSTER = { width: 128, scale: 4, frames: 90, fps: 7.5, playbackFps: 15 };

const cases = [
  { name: "scroll (animated)", source: "blue, ink red, line 0 0 100 100\nscroll 100", opts: TILE, animated: true },
  { name: "still", source: "blue, ink red, line 0 0 100 100", opts: TILE, animated: false },
  { name: "random points", source: "black\nink purple\nrepeat 300 point", opts: TILE, animated: true },
  { name: "text", source: '(wipe black)\n(ink white)\n(write "HELLO 123" 4 4)', opts: TILE, minLit: 40 },
  { name: "3d orb", source: "(ink orange)\n(rotate y (* .5 time) (orb 0 .5 0 .42 6))", opts: TILE, kind: "3d" },
  { name: "poster 512²", source: "blue, ink red, line 0 0 100 100\nscroll 100", opts: POSTER, size: 512, budgetMs: 5000 },
  { name: "blank → Chrome", source: "(wipe black)", opts: TILE, throws: "blank" },
  { name: "embedded $code → Chrome", source: "($abc 0 0)", opts: TILE, throws: "unsupported" },
  { name: "non-ASCII text → Chrome", source: '(wipe black)\n(ink white)\n(write "héllo" 4 4)', opts: TILE, throws: "unsupported" },
];
if (live) for (const code of ["$hq5", "$gcb", "$u5v", "$mbw"]) cases.push({ name: code, source: code, opts: TILE, budgetMs: 8000 });

let failed = 0;
const fail = (c, why) => { failed++; console.log(`✗ ${c.name}: ${why}`); };

for (const c of cases) {
  const budget = c.budgetMs ?? 3000;
  const t0 = performance.now();
  let out;
  try {
    out = await renderKidlispDetailed(c.source, c.opts);
  } catch (error) {
    const ms = Math.round(performance.now() - t0);
    if (c.throws && error.reason === c.throws) console.log(`✓ ${c.name}: threw ${error.reason} in ${ms} ms`);
    else fail(c, `threw ${error.reason || "error"}: ${String(error.message).split("\n")[0].slice(0, 160)}`);
    continue;
  }
  const ms = Math.round(performance.now() - t0);
  if (c.throws) { fail(c, `expected ${c.throws}, rendered`); continue; }

  const s = out.stats;
  const meta = await sharp(out.webp, { animated: true }).metadata();
  const size = c.size ?? c.opts.width;
  const problems = [];
  if (meta.format !== "webp") problems.push(`format ${meta.format}`);
  const h = meta.pageHeight ?? meta.height;
  if (meta.width !== size || h !== size) problems.push(`decoded ${meta.width}×${h}, want ${size}²`);
  if (c.animated === true && !(meta.pages > 1)) problems.push(`want animation, got ${meta.pages} page(s)`);
  if (c.animated === false && (meta.pages ?? 1) !== 1) problems.push(`want a still, got ${meta.pages} pages`);
  if (c.kind && s.kind !== c.kind) problems.push(`kind ${s.kind}`);
  if (s.colours < 2) problems.push("one colour");
  if (c.minLit) {
    const f = out.frames[0];
    let lit = 0;
    for (let i = 0; i < f.length; i += 4) if (f[i] > 128) lit++;
    if (lit < c.minLit) problems.push(`only ${lit} lit pixels (text did not draw?)`);
  }
  if (ms > budget) problems.push(`${ms} ms > ${budget} ms budget`);
  if (problems.length) fail(c, problems.join("; "));
  else console.log(`✓ ${c.name}: ${s.kind} ${meta.width}² ×${meta.pages}f ${(out.webp.length / 1024).toFixed(1)} KB in ${ms} ms`);

  if (outDir) {
    await mkdir(outDir, { recursive: true });
    await writeFile(`${outDir}/${c.name.replace(/[^\w$]+/g, "-")}.webp`, out.webp);
  }
}

console.log(failed ? `\n${failed} failed (node ${process.version})` : `\nall ${cases.length} passed (node ${process.version})`);
process.exit(failed ? 1 : 0);
