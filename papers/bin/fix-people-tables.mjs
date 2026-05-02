#!/usr/bin/env node
// fix-people-tables.mjs — convert all lXl people tables to lXX vignette
// (works around tabularx natural-width overflow when both `l` columns
// contain long strings).

import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");

const SLUGS = [
  "arxiv-rhizome", "arxiv-sfpc", "arxiv-eyebeam", "arxiv-recurse",
  "arxiv-internet-archive", "arxiv-mellon", "arxiv-pioneer-works",
  "arxiv-new-inc", "arxiv-studio-museum", "arxiv-hathitrust",
  "arxiv-the-kitchen", "arxiv-machine-project", "arxiv-heavy-manners-library",
];

let fixed = 0;
for (const slug of SLUGS) {
  const dir = join(PAPERS_DIR, slug);
  const tex = readdirSync(dir).filter((f) => f.endsWith(".tex"));
  for (const f of tex) {
    if (/-(cards|da|es|zh|ja)\.tex$/.test(f)) continue;
    const path = join(dir, f);
    let src = readFileSync(path, "utf8");

    // Convert: \small\n\centering\n\begin{tabularx}{\columnwidth}{lXl}
    //   to:    \footnotesize\n\centering\n\setlength{\tabcolsep}{3pt}\n\begin{tabularx}{\columnwidth}{lXX}
    const before = src;
    src = src.replace(
      /\\small\n\\centering\n\\begin\{tabularx\}\{\\columnwidth\}\{lXl\}/g,
      "\\footnotesize\n\\centering\n\\setlength{\\tabcolsep}{3pt}\n\\begin{tabularx}{\\columnwidth}{lXX}",
    );
    // Also handle case where it's just \small without \centering
    src = src.replace(
      /\\begin\{tabularx\}\{\\columnwidth\}\{lXl\}/g,
      "\\begin{tabularx}{\\columnwidth}{lXX}",
    );
    if (src !== before) {
      writeFileSync(path, src, "utf8");
      console.log(`  ✓ ${slug}/${f}`);
      fixed++;
    }
  }
}
console.log(`fixed ${fixed} file(s)`);
