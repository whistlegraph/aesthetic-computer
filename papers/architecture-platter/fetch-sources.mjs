#!/usr/bin/env node
// fetch-sources.mjs — pull the sources named in sources.json into sources/
// (gitignored) and convert each to text for search and citation work.
//
// Only entries with "open_access": true and a direct "pdf" URL are fetched.
// Nothing is mirrored into the repo — see README.
//
//   node papers/architecture-platter/fetch-sources.mjs
//   node papers/architecture-platter/fetch-sources.mjs --only palladio-1738-ware

import { readFileSync, mkdirSync, existsSync } from "node:fs";
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
  if (existsSync(pdf)) { console.log(`· ${s.id} (cached)`); }
  else {
    console.log(`↓ ${s.id} — ${s.pdf}`);
    const r = spawnSync("curl", ["-fsSL", "--max-time", "300", "-o", pdf, s.pdf], { stdio: "inherit" });
    if (r.status !== 0) { console.error(`  ✗ fetch failed`); continue; }
  }
  if (hasPdftotext && !existsSync(txt)) {
    const c = spawnSync("pdftotext", ["-layout", pdf, txt], { stdio: "inherit" });
    if (c.status !== 0) console.error(`  ✗ pdftotext failed (plate scans may have no text layer)`);
  }
  got++;
}
console.log(`\n${got} available, ${skipped} cite-only (never mirrored).`);
console.log(`sources/ is gitignored.`);
