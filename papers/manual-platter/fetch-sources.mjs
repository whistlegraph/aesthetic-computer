#!/usr/bin/env node
// fetch-sources.mjs — pull the open sources named in sources.json into
// sources/ (gitignored), convert each to text, and cut a contact sheet of
// sampled pages so the layouts can be looked at side by side.
//
// Only entries with "open_access": true and a direct "pdf" URL are fetched.
// The cite-only shelf is never fetched. Nothing is mirrored into the repo.
//
//   node papers/manual-platter/fetch-sources.mjs
//   node papers/manual-platter/fetch-sources.mjs --only wells-1913-little-wars
//   node papers/manual-platter/fetch-sources.mjs --no-sheets

import { readFileSync, mkdirSync, existsSync, rmSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = join(HERE, "sources");
const SHEETS = join(OUT, "sheets");
const arg = (name) =>
  process.argv.includes(name) ? process.argv[process.argv.indexOf(name) + 1] : null;
const only = arg("--only");
const wantSheets = !process.argv.includes("--no-sheets");
const SAMPLES = 12; // pages per contact sheet

const { sources } = JSON.parse(readFileSync(join(HERE, "sources.json"), "utf8"));
mkdirSync(SHEETS, { recursive: true });

const has = (bin) => spawnSync("which", [bin]).status === 0;
const tools = { pdftotext: has("pdftotext"), pdftoppm: has("pdftoppm"), pdfinfo: has("pdfinfo"), magick: has("magick") };
for (const [name, ok] of Object.entries(tools)) if (!ok) console.warn(`! ${name} not found`);

// Twelve pages spread evenly through the book, skipping the first and last
// few leaves (covers, blanks, library slips).
function samplePages(count) {
  const from = Math.min(count, 6), to = Math.max(from, count - 4);
  const n = Math.min(SAMPLES, to - from + 1);
  return Array.from({ length: n }, (_, i) => Math.round(from + (i * (to - from)) / Math.max(1, n - 1)));
}

function sheet(id, pdf) {
  const out = join(SHEETS, `${id}.png`);
  if (existsSync(out)) return;
  const info = spawnSync("pdfinfo", [pdf], { encoding: "utf8" }).stdout || "";
  const count = Number(/Pages:\s+(\d+)/.exec(info)?.[1] || 0);
  if (!count) return console.error("  ✗ no page count");
  const tmp = join(SHEETS, `.${id}`);
  mkdirSync(tmp, { recursive: true });
  const tiles = [];
  for (const page of samplePages(count)) {
    const stem = join(tmp, `p${String(page).padStart(4, "0")}`);
    const r = spawnSync("pdftoppm", ["-f", String(page), "-l", String(page), "-scale-to", "700", "-png", "-singlefile", pdf, stem]);
    if (r.status === 0) tiles.push(`${stem}.png`);
  }
  const m = spawnSync("magick", ["montage", ...tiles, "-tile", "6x", "-geometry", "+10+10", "-background", "white", out], { stdio: "inherit" });
  rmSync(tmp, { recursive: true, force: true });
  if (m.status === 0) console.log(`  ▦ sheets/${id}.png (${tiles.length} of ${count} pages)`);
}

let got = 0, skipped = 0;
for (const s of sources) {
  if (only && s.id !== only) continue;
  if (!s.open_access || !s.pdf) { skipped++; continue; }
  const pdf = join(OUT, `${s.id}.pdf`);
  const txt = join(OUT, `${s.id}.txt`);
  if (existsSync(pdf)) console.log(`· ${s.id} (cached)`);
  else {
    console.log(`↓ ${s.id} — ${s.pdf}`);
    const r = spawnSync("curl", ["-fsSL", "--max-time", "900", "-o", pdf, s.pdf], { stdio: "inherit" });
    if (r.status !== 0) { console.error("  ✗ fetch failed"); rmSync(pdf, { force: true }); continue; }
  }
  if (tools.pdftotext && !existsSync(txt)) {
    const c = spawnSync("pdftotext", ["-layout", pdf, txt], { stdio: "inherit" });
    if (c.status !== 0) console.error("  ✗ pdftotext failed");
  }
  if (wantSheets && tools.pdftoppm && tools.pdfinfo && tools.magick) sheet(s.id, pdf);
  got++;
}
console.log(`\n${got} fetched, ${skipped} cite-only (never fetched).`);
console.log("sources/ is gitignored.");
