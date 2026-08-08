#!/usr/bin/env node
// fetch-sources.mjs — pull the open-access sources named in sources.json into
// sources/ (gitignored) and convert each to text for the extractors.
//
// Only entries with "open_access": true and a direct "pdf" URL are fetched.
// Books and paywalled journal articles are cited, never mirrored — see README.
//
//   node papers/rhythm-platter/fetch-sources.mjs
//   node papers/rhythm-platter/fetch-sources.mjs --only toussaint-2005-bridges

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = join(HERE, "sources");
const only = process.argv.includes("--only")
  ? process.argv[process.argv.indexOf("--only") + 1]
  : null;

const { sources } = JSON.parse(readFileSync(join(HERE, "sources.json"), "utf8"));
mkdirSync(OUT, { recursive: true });

const hasPdftotext = spawnSync("which", ["pdftotext"]).status === 0;
if (!hasPdftotext) console.warn("! pdftotext not found — PDFs will be fetched but not converted");

let got = 0, skipped = 0;
for (const s of sources) {
  if (only && s.id !== only) continue;
  if (!s.open_access || !s.pdf) { skipped++; continue; }
  const pdf = join(OUT, `${s.id}.pdf`);
  const txt = join(OUT, `${s.id}.txt`);
  if (existsSync(txt)) { console.log(`· ${s.id} (cached)`); got++; continue; }

  console.log(`↓ ${s.id} — ${s.pdf}`);
  const r = spawnSync("curl", ["-fsSL", "--max-time", "60", "-o", pdf, s.pdf], { stdio: "inherit" });
  if (r.status !== 0) { console.error(`  ✗ fetch failed`); continue; }
  if (hasPdftotext) {
    const c = spawnSync("pdftotext", ["-layout", pdf, txt], { stdio: "inherit" });
    if (c.status !== 0) { console.error(`  ✗ pdftotext failed`); continue; }
  }
  got++;
}
console.log(`\n${got} available, ${skipped} cite-only (books / paywalled — never mirrored).`);
console.log(`sources/ is gitignored. Next: node papers/rhythm-platter/build-timelines.mjs`);
