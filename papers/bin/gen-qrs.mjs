#!/usr/bin/env node
// gen-qrs.mjs — generate QR-code PNG for each AC paper, pointing to the
// paper's permalink at https://papers.aesthetic.computer/<siteName>.pdf
//
// Reads:    papers/arxiv-<slug>/  (must be registered in cli.mjs PAPER_MAP)
// Writes:   papers/arxiv-<slug>/figures/qr.png  (English)
//           papers/arxiv-<slug>/figures/qr-<lang>.png  (translations, if --langs)
//
// Usage:
//   node papers/bin/gen-qrs.mjs arxiv-rhizome
//   node papers/bin/gen-qrs.mjs --all
//   node papers/bin/gen-qrs.mjs --all --langs        also gen da/es/zh/ja
//
// Uses the homebrew `qrencode` CLI (no LaTeX package dependency).

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");

const SITE_URL = "https://papers.aesthetic.computer";
const LANGS = ["da", "es", "zh", "ja", "ru"];

// Mirror PAPER_MAP siteName → arxiv-<slug>. Keep in sync with cli.mjs.
// Only the dossiers need QRs for now; older AC papers already have other CTAs.
const PAPERS = {
  "arxiv-comp-strats": "comp-strats-26-arxiv",
  "arxiv-rhizome": "rhizome-dossier-26-arxiv",
  "arxiv-sfpc": "sfpc-dossier-26-arxiv",
  "arxiv-eyebeam": "eyebeam-dossier-26-arxiv",
  "arxiv-recurse": "recurse-dossier-26-arxiv",
  "arxiv-internet-archive": "internet-archive-dossier-26-arxiv",
  "arxiv-mellon": "mellon-dossier-26-arxiv",
  "arxiv-pioneer-works": "pioneer-works-dossier-26-arxiv",
  "arxiv-new-inc": "new-inc-dossier-26-arxiv",
  "arxiv-studio-museum": "studio-museum-dossier-26-arxiv",
  "arxiv-hathitrust": "hathitrust-dossier-26-arxiv",
  "arxiv-the-kitchen": "the-kitchen-dossier-26-arxiv",
  "arxiv-machine-project": "machine-project-dossier-26-arxiv",
  "arxiv-heavy-manners-library": "heavy-manners-library-dossier-26-arxiv",
  "arxiv-creative-time": "creative-time-dossier-26-arxiv",
  "arxiv-creative-capital": "creative-capital-dossier-26-arxiv",
  "arxiv-fraserin": "fraserin-essay-26-arxiv",
  "arxiv-microvision": "microvision-dossier-26-arxiv",
};

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (const a of argv) {
  if (a.startsWith("--")) flags[a.slice(2)] = true;
  else positional.push(a);
}

function genQr(url, outPath) {
  // -t PNG32: full RGBA, NOT a 1-bit indexed palette. The old default
  //   1-bit colormap PNG renders as a solid white block in several PDF
  //   viewers and in the thumbnail pipeline (palette-order ambiguity),
  //   which is why QRs appeared "all white / missing".
  // --foreground/--background: explicit opaque black-on-white so the
  //   code is visible on the cream/white paper regardless of renderer.
  // -s 10 = 10 px per module (large enough for IG screenshot scanning)
  // -m 2  = 2 module quiet zone (the white margin)
  // -l H  = high error correction (survives IG re-encoding)
  execFileSync("qrencode", [
    "-o", outPath,
    "-t", "PNG32",
    "--foreground=000000FF",
    "--background=FFFFFFFF",
    "-s", "10",
    "-m", "2",
    "-l", "H",
    url,
  ]);
}

function genForPaper(slug) {
  const siteName = PAPERS[slug];
  if (!siteName) {
    console.warn(`  · skip ${slug}: not in PAPERS map`);
    return;
  }
  const figuresDir = join(PAPERS_DIR, slug, "figures");
  if (!existsSync(figuresDir)) mkdirSync(figuresDir, { recursive: true });

  const enUrl = `${SITE_URL}/${siteName}.pdf`;
  genQr(enUrl, join(figuresDir, "qr.png"));
  console.log(`  ✓ ${slug}: ${enUrl}`);

  if (flags.langs) {
    for (const lang of LANGS) {
      const url = `${SITE_URL}/${siteName}-${lang}.pdf`;
      genQr(url, join(figuresDir, `qr-${lang}.png`));
      console.log(`    + ${lang}: ${url}`);
    }
  }
}

let targets = [];
if (flags.all) {
  for (const slug of Object.keys(PAPERS)) {
    if (existsSync(join(PAPERS_DIR, slug))) targets.push(slug);
  }
} else if (positional.length) {
  targets = positional;
} else {
  console.error(
    "usage: gen-qrs.mjs <arxiv-slug> [--langs]\n" +
      "       gen-qrs.mjs --all [--langs]",
  );
  process.exit(1);
}

console.log(`▸ ${targets.length} target(s) · langs=${!!flags.langs}`);
for (const t of targets) genForPaper(t);
