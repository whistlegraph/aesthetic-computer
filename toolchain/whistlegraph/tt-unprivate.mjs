#!/usr/bin/env node
// tt-unprivate.mjs
//
// Bulk-unprivate @whistlegraph TikTok posts by driving the REAL TikTok app on the
// REAL iPhone through macOS "iPhone Mirroring" — same approach + primitive as the
// Instagram grid archiver (portraits/jeffrey/bin/ig-archive-mirror.mjs). It's the
// app on the device, so it can't be flagged as a bot, and TikTok has no API for
// bulk privacy edits.
//
// Flow per cycle on TikTok → Settings → "Manage post visibility" → "Only you" tab:
//   shot+ocr → select each visible private row's circle (right edge) → "Next (N)"
//   → tap Next → tap "Everyone" → tap "Update". Applied posts leave the "Only you"
//   filter, so the list shrinks from the top — no scrolling / dedup needed.
//
// SAFETY: every cycle is gated. It only acts when the header reads "Manage post
// visibility" and it can see "Only you •" rows + a "Next (N)" counter. Any drift
// (wrong screen, no counter, sheet missing "Everyone"/"Update") HALTS the run.
//
//   node tt-unprivate.mjs --dry            # read + plan one cycle, no mutation
//   node tt-unprivate.mjs --max=1          # one real apply cycle (smoke test)
//   node tt-unprivate.mjs                  # run until the Only-you list is empty

import { execFileSync } from "child_process";
import { join } from "path";

const args = process.argv.slice(2);
const has = (n) => args.includes(n);
const val = (n) => { const a = args.find((x) => x.startsWith(n + "=")); return a ? a.slice(n.length + 1) : null; };

const DRY = has("--dry");
const MAX = val("--max") ? parseInt(val("--max"), 10) : Infinity;
const CIRCLE_X = val("--circle-x") ? parseFloat(val("--circle-x")) : 0.91;
const CIRCLE_DY = -0.033;            // circle center sits above the "Only you • date" line
const ROW_LO = 0.28, ROW_HI = 0.885; // list band (below tabs, above the Next bar)

const HERE = new URL(".", import.meta.url).pathname;
const TAP = join(HERE, "..", "macos", "iphone-tap", "iphone-tap");
const SHOT = "/tmp/tt-unpriv.png";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
function cli(...a) {
  try { return JSON.parse(execFileSync(TAP, a, { encoding: "utf8" })); }
  catch (e) { try { return JSON.parse(e.stdout?.toString() || ""); } catch { return { error: e.message }; } }
}
function read() {
  const s = cli("shot", SHOT); if (s.error) return { error: s.error };
  const o = cli("ocr", SHOT); if (o.error) return { error: o.error };
  return { lines: (o.lines || []).map((l) => ({ ...l, cx: l.x + l.w / 2, cy: l.y + l.h / 2 })) };
}
async function tap(fx, fy) { const r = cli("tap", String(fx), String(fy)); await sleep(300); return r; }
const find = (lines, re) => lines.find((l) => re.test(l.text.trim()));
const onManage = (lines) => lines.some((l) => /manage post visibility/i.test(l.text));
// A row = a list line reading "Only you • <date>" (the tab is bare "Only you", no •).
const rows = (lines) => lines
  .filter((l) => /only you\s*[•·]/i.test(l.text) && l.cy > ROW_LO && l.cy < ROW_HI)
  .sort((a, b) => a.cy - b.cy);
const nextCount = (lines) => { const n = find(lines, /^next\s*\(\d+\)/i); return n ? parseInt(n.text.match(/\((\d+)\)/)[1], 10) : 0; };

let cycle = 0, total = 0, halted = "";
console.log(`══ tt-unprivate ══  ${DRY ? "DRY-RUN" : "LIVE"}  max=${MAX === Infinity ? "∞" : MAX}\n`);

while (cycle < MAX) {
  cycle++;
  let r = read();
  if (r.error) { halted = `read: ${r.error}`; break; }
  if (!onManage(r.lines)) { halted = 'not on "Manage post visibility"'; break; }

  const rs = rows(r.lines);
  if (rs.length === 0) { console.log("✓ no private rows left — Only-you list is empty."); break; }

  // Select each visible private row (fresh screen after each apply → all unselected).
  for (const row of rs) await tap(CIRCLE_X, Math.max(0.30, row.cy + CIRCLE_DY));
  await sleep(400);

  r = read();
  const n = nextCount(r.lines);
  if (n === 0) { halted = `selected ${rs.length} rows but no "Next (N)" counter appeared`; break; }
  process.stdout.write(`[cycle ${cycle}] selected ${n} `);

  if (DRY) { console.log("→ (dry, not applying)"); break; }

  await tap(0.5, 0.919);                       // Next
  await sleep(700);
  r = read();
  const everyone = find(r.lines, /^everyone$/i);
  const update = find(r.lines, /^update$/i);
  if (!everyone || !update) { halted = `visibility sheet missing Everyone/Update (saw everyone=${!!everyone} update=${!!update})`; break; }
  await tap(everyone.cx, everyone.cy);          // Everyone
  await sleep(300);
  await tap(update.cx, update.cy);              // Update
  await sleep(1500);

  // verify: back on Manage list, selection cleared
  r = read();
  if (!onManage(r.lines)) { halted = "left Manage screen after Update"; break; }
  total += n;
  console.log(`→ set public ✓  (running total ${total})`);
  await sleep(800 + Math.random() * 700);       // gentle jitter
}

console.log(`\n── done. set ${total} posts to Everyone over ${cycle} cycle(s).${halted ? "  HALTED: " + halted : ""} ──`);
