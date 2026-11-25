#!/usr/bin/env node
// Dud checks, no Chrome: the re-bake backoff, the uniform-colour detector and
// the headless precheck's routing (grab-duds.mjs + kidlisp-headless.mjs).
//
//   node oven/test-duds.mjs            # offline (inline KidLisp sources)
//   node oven/test-duds.mjs --live     # + published $codes (reads store-kidlisp, never the oven)

import sharp from "sharp";
import {
  isSuspectDud, isStaleDud, nextDudAttempt, dudBackoffMs, dudWantsSourceCheck,
  parseBackoff, uniformCoverage, uniformFromImages, precheckVerdict, precheckColour,
  isJunkPieceName,
} from "./grab-duds.mjs";
import { renderKidlispDetailed } from "./kidlisp-headless.mjs";

const live = process.argv.includes("--live");
let failed = 0, passed = 0;
function check(name, ok, detail = "") {
  if (ok) { passed++; console.log(`✓ ${name}`); }
  else { failed++; console.log(`✗ ${name}${detail ? `: ${detail}` : ""}`); }
}

// ─── Backoff ────────────────────────────────────────────────────────────────
const MIN = 60000, HOUR = 60 * MIN, DAY = 24 * HOUR;
const LADDER = [10 * MIN, HOUR, DAY, Infinity];
const t0 = Date.UTC(2026, 10, 25);
const dud = (extra = {}) => ({ dud: true, size: 200, completedAt: new Date(t0), sourceHash: "aaa", ...extra });
const stale = (g, after, cur = {}) => isStaleDud(g, t0 + after, { backoff: LADDER, ...cur });

check("ladder parses", JSON.stringify(parseBackoff("10m,1h,1d,never")) === JSON.stringify(LADDER));
check("bad ladder → null", parseBackoff("10m,soon") === null);
check("try 1 trusted 9 min", !stale(dud(), 9 * MIN));
check("try 1 stale at 10 min", stale(dud(), 10 * MIN));
check("try 2 trusted 59 min", !stale(dud({ dudAttempt: 2 }), 59 * MIN));
check("try 2 stale at 1 h", stale(dud({ dudAttempt: 2 }), HOUR));
check("try 3 trusted 23 h", !stale(dud({ dudAttempt: 3 }), 23 * HOUR));
check("try 3 stale at 1 day", stale(dud({ dudAttempt: 3 }), DAY));
check("try 4 never stale (a year on)", !stale(dud({ dudAttempt: 4 }), 365 * DAY));
check("try 9 never stale", !stale(dud({ dudAttempt: 9 }), 365 * DAY));
check("final dud + same source → kept", !stale(dud({ dudAttempt: 4 }), 365 * DAY, { sourceHash: "aaa" }));
check("final dud + edited source → re-bake", stale(dud({ dudAttempt: 4 }), 11 * MIN, { sourceHash: "bbb" }));
check("no hash → never capped at a day", stale(dud({ dudAttempt: 4, sourceHash: undefined }), DAY) && !stale(dud({ dudAttempt: 4, sourceHash: undefined }), DAY - 1));
check("precheck dud re-judged on new renderer", stale(dud({ dudAttempt: 4, dudBy: "headless-h1" }), MIN, { headlessVersion: "h2" }));
check("precheck dud kept on same renderer", !stale(dud({ dudAttempt: 4, dudBy: "headless-h1" }), 365 * DAY, { headlessVersion: "h1" }));
check("dudBackoffMs rungs", [1, 2, 3, 4, 5].map((n) => dudBackoffMs(n, { backoff: LADDER })).join() === [10 * MIN, HOUR, DAY, Infinity, Infinity].join());
check("next try: none before → 1", nextDudAttempt(null, "aaa") === 1);
check("next try: same source → +1", nextDudAttempt(dud({ dudAttempt: 2 }), "aaa") === 3);
check("next try: legacy dud (no count) → 2", nextDudAttempt({ dud: true, size: 100 }, "aaa") === 2);
check("next try: edited source → 1", nextDudAttempt(dud({ dudAttempt: 3 }), "bbb") === 1);
check("source looked up only past rung 1", !dudWantsSourceCheck(dud({ dudAttempt: 4 }), t0 + 5 * MIN, LADDER) && dudWantsSourceCheck(dud({ dudAttempt: 4 }), t0 + 11 * MIN, LADDER));

// isSuspectDud: the explicit flag wins; only flagless (legacy) tiny records
// fall back to size.
check("dud:false tiny (smooth fade still) is trusted", !isSuspectDud({ dud: false, size: 300 }));
check("dud:false tiny never stale", !stale({ dud: false, size: 300, completedAt: new Date(t0) }, 30 * DAY));
check("dud:true large is a dud", isSuspectDud({ dud: true, size: 50000 }));
check("legacy flagless tiny is suspect", isSuspectDud({ size: 300 }));
check("legacy flagless large is not", !isSuspectDud({ size: 5000 }));

// ─── Uniform detector ───────────────────────────────────────────────────────
const W = 512;
function canvas(rgb, w = W, h = W) {
  const px = Buffer.alloc(w * h * 3);
  for (let i = 0; i < w * h; i++) px.set(rgb, i * 3);
  return { px, w, h, set(x, y, c) { if (x >= 0 && y >= 0 && x < w && y < h) px.set(c, (y * w + x) * 3); } };
}
const png = (c) => sharp(c.px, { raw: { width: c.w, height: c.h, channels: 3 } }).png().toBuffer();
const webp = (c, quality = 60) => sharp(c.px, { raw: { width: c.w, height: c.h, channels: 3 } }).webp({ quality }).toBuffer();
async function uniform(frames) { return uniformFromImages(await Promise.all(frames), sharp); }

{
  const flat = canvas([0, 0, 255]);
  const r = await uniform(Array.from({ length: 12 }, () => png(flat)));
  check("flat colour → uniform", r.isUniform && r.colour.join() === "0,0,255", JSON.stringify(r));
}
{
  const black = canvas([0, 0, 0]);
  const r = await uniform([png(black)]);
  check("flat black still → uniform", r.isUniform && r.colour.join() === "0,0,0");
}
{
  // $7zt-like: a blue ground with a few 1 px diagonal strokes near the top.
  const c = canvas([0, 0, 255]);
  for (const x0 of [80, 180, 280]) for (let i = 0; i < 50; i++) c.set(x0 + i, i, [255, 255, 255]);
  const r = await uniform(Array.from({ length: 8 }, () => png(c)));
  check("thin lines on flat ground → not uniform", !r.isUniform, `coverage ${r.coverage}`);
}
{
  // One horizontal 1 px line across a 512² frame.
  const c = canvas([0, 0, 0]);
  for (let x = 0; x < W; x++) c.set(x, 200, [255, 255, 255]);
  const r = await uniform([png(c)]);
  check("single 1 px line → not uniform", !r.isUniform, `coverage ${r.coverage}`);
}
{
  // A small mark: a 32×32 square (0.4% of 512²).
  const c = canvas([0, 0, 0]);
  for (let y = 250; y < 282; y++) for (let x = 250; x < 282; x++) c.set(x, y, [255, 0, 0]);
  const r = await uniform([png(c)]);
  check("small mark (32² of 512²) → not uniform", !r.isUniform, `coverage ${r.coverage}`);
  // A lone speck (4×4, 0.006%) is below the 0.2% line: still a flat dud.
  const d = canvas([0, 0, 0]);
  for (let y = 250; y < 254; y++) for (let x = 250; x < 254; x++) d.set(x, y, [255, 0, 0]);
  const s = await uniform([png(d)]);
  check("lone speck (4² of 512²) → uniform", s.isUniform, `coverage ${s.coverage}`);
}
{
  // $fasw-like: each frame flat, flashing between two colours.
  const a = canvas([49, 253, 61]), b = canvas([128, 109, 240]);
  const r = await uniform(Array.from({ length: 16 }, (_, i) => png(i % 2 ? a : b)));
  check("flashing flat frames → not uniform", !r.isUniform, `coverage ${r.coverage}`);
}
{
  // A flash on one frame in thirty still counts (1/30 ≫ 0.2%).
  const a = canvas([0, 0, 0]), b = canvas([255, 255, 255]);
  const r = await uniform(Array.from({ length: 30 }, (_, i) => png(i === 15 ? b : a)));
  check("one flash frame in 30 → not uniform", !r.isUniform, `coverage ${r.coverage}`);
}
{
  // A smooth `fade:` gradient (tiny file, many near colours).
  const c = canvas([0, 0, 0]);
  for (let y = 0; y < W; y++) for (let x = 0; x < W; x++) c.set(x, y, [Math.round((y / W) * 255), 40, 255 - Math.round((y / W) * 255)]);
  const buf = await png(c);
  const r = await uniform([buf]);
  check("smooth fade gradient → not uniform", !r.isUniform, `coverage ${r.coverage}`);
}
{
  // Lossy WebP noise on a flat colour (±2) must still read as flat.
  const c = canvas([255, 229, 197]);
  const r = await uniform([webp(c, 40)]);
  check("lossy webp of a flat colour → uniform", r.isUniform, `coverage ${r.coverage}`);
}
{
  // Pure function on raw pixels: tolerance edge.
  const px = new Uint8Array(100 * 3).fill(10);
  px[0] = 18; // within ±8
  check("uniformCoverage tolerance", uniformCoverage([{ data: px, channels: 3 }]).isUniform);
  px.fill(200, 0, 3); // one of 100 pixels off → 99%
  check("uniformCoverage 99% < 99.8%", !uniformCoverage([{ data: px, channels: 3 }]).isUniform);
}

// ─── Precheck routing (headless, no Chrome) ────────────────────────────────
const TILE = { width: 100, frames: 16, fps: 8, playbackFps: 15 };
async function outcome(source) {
  try { return { out: await renderKidlispDetailed(/^\$[0-9A-Za-z]+$/.test(source) ? source : { source }, TILE) }; }
  catch (error) { return { error }; }
}
const routes = [
  ["melody only", "(melody d.d..d.e..g..e.h..e.f..e.fd. 500)", "dud", [0, 0, 0]],
  ["prompt word saved as KidLisp", "chat", "dud", [0, 0, 0]],
  ["scanner junk: arithmetic", "1598*7215", "dud"],
  ["scanner junk: traversal", "../../../../../../etc/passwd", "dud"],
  ["scanner junk: template", "${''.class.superclass}", "dud"],
  ["black", "black", "dud", [0, 0, 0]],
  ["one flat colour", "(wipe 230 222 246)", "dud", [230, 222, 246]],
  ["a thin line", "(wipe black)(ink white)(line 0 10 99 10)", "drawn"],
  ["a box", "(wipe navy, box 50, 50, 20, 20)", "drawn"],
  ["embedded $code", "($abc 0 0)", "chrome"],
];
for (const [name, source, want, rgb] of routes) {
  const o = await outcome(source);
  const got = precheckVerdict(o);
  const colourOk = !rgb || precheckColour(o.error).join() === rgb.join();
  check(`precheck: ${name} → ${want}`, got === want && colourOk, `got ${got} ${o.error ? `(${o.error.reason}) ${precheckColour(o.error)}` : ""}`);
}
check("precheck: 404 source → missing", precheckVerdict({ error: Object.assign(new Error("KidLisp source HTTP 404 for $zz"), { reason: "source", status: 404 }) }) === "missing");
check("precheck: empty source → missing", precheckVerdict({ error: Object.assign(new Error("No KidLisp source for $zz"), { reason: "source" }) }) === "missing");
check("precheck: 502 source → chrome", precheckVerdict({ error: Object.assign(new Error("KidLisp source HTTP 502"), { reason: "source", status: 502 }) }) === "chrome");
check("precheck: timeout → chrome", precheckVerdict({ error: Object.assign(new Error("slow"), { reason: "timeout" }) }) === "chrome");
check("precheckColour parses message", precheckColour({ message: "Headless output is one colour (1,2,3) across 9 frames" }).join() === "1,2,3");

// ─── Junk names never reach a grab ──────────────────────────────────────────
for (const p of ["../../etc/passwd", "/etc/passwd", "$../../x", "${7*7}", "a;sleep 5", "-oProxyCommand=x", "$a b", "x|y", "<script>"])
  check(`junk name rejected: ${p}`, isJunkPieceName(p));
for (const p of ["$hq5", "$roz", "notepat", "@jeffrey/piece", "#abc", "!tape", "line~10~20", "prompt:opt", "clock", "$roz~2"])
  check(`real name kept: ${p}`, !isJunkPieceName(p));

if (live) {
  for (const [code, want] of [["$lix", "dud"], ["$isd", "dud"], ["$hq5", "drawn"], ["$phc", "drawn"]]) {
    const o = await outcome(code);
    check(`live ${code} → ${want}`, precheckVerdict(o) === want, `${precheckVerdict(o)} ${o.error?.reason ?? ""}`);
  }
}

console.log(failed ? `\n${failed} failed, ${passed} passed` : `\nall ${passed} passed`);
process.exit(failed ? 1 : 0);
