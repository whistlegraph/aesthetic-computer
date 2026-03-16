#!/usr/bin/env node
// papermill.mjs — Build script for translated papers
// Usage:
//   node papermill.mjs build          Build all translated PDFs
//   node papermill.mjs build da       Build only Danish translations
//   node papermill.mjs build es       Build only Spanish translations
//   node papermill.mjs build zh       Build only Chinese translations
//   node papermill.mjs deploy         Copy compiled PDFs to site directory
//   node papermill.mjs status         Show which translations exist

import { execSync } from "child_process";
import { readdirSync, existsSync, copyFileSync, mkdirSync } from "fs";
import { join, basename } from "path";

const PAPERS_DIR = new URL(".", import.meta.url).pathname;
const SITE_DIR = join(
  PAPERS_DIR,
  "../system/public/papers.aesthetic.computer",
);
const LANGS = ["da", "es", "zh"];
const LANG_NAMES = { da: "Danish", es: "Spanish", zh: "Chinese" };

// Map paper dir name to output PDF base name (matching existing site naming)
const PAPER_MAP = {
  "arxiv-ac": { base: "ac", siteName: "aesthetic-computer-26-arxiv" },
  "arxiv-api": { base: "api", siteName: "piece-api-26-arxiv" },
  "arxiv-archaeology": {
    base: "archaeology",
    siteName: "repo-archaeology-26-arxiv",
  },
  "arxiv-dead-ends": { base: "dead-ends", siteName: "dead-ends-26-arxiv" },
  "arxiv-diversity": {
    base: "diversity",
    siteName: "citation-diversity-audit-26",
  },
  "arxiv-goodiepal": {
    base: "goodiepal",
    siteName: "radical-computer-art-26-arxiv",
  },
  "arxiv-kidlisp": { base: "kidlisp", siteName: "kidlisp-26-arxiv" },
  "arxiv-kidlisp-reference": {
    base: "kidlisp-reference",
    siteName: "kidlisp-reference-26-arxiv",
  },
  "arxiv-network-audit": {
    base: "network-audit",
    siteName: "network-audit-26-arxiv",
  },
  "arxiv-notepat": { base: "notepat", siteName: "notepat-26-arxiv" },
  "arxiv-os": { base: "os", siteName: "ac-native-os-26-arxiv" },
  "arxiv-pieces": {
    base: "pieces",
    siteName: "pieces-not-programs-26-arxiv",
  },
  "arxiv-sustainability": {
    base: "sustainability",
    siteName: "who-pays-for-creative-tools-26-arxiv",
  },
  "arxiv-whistlegraph": {
    base: "whistlegraph",
    siteName: "whistlegraph-26-arxiv",
  },
};

function findTranslatedFiles() {
  const results = [];
  for (const [dir, info] of Object.entries(PAPER_MAP)) {
    const paperDir = join(PAPERS_DIR, dir);
    if (!existsSync(paperDir)) continue;
    for (const lang of LANGS) {
      const texFile = join(paperDir, `${info.base}-${lang}.tex`);
      const pdfFile = join(paperDir, `${info.base}-${lang}.pdf`);
      results.push({
        dir,
        lang,
        base: info.base,
        siteName: info.siteName,
        texFile,
        pdfFile,
        texExists: existsSync(texFile),
        pdfExists: existsSync(pdfFile),
        sitePdf: join(SITE_DIR, `${info.siteName}-${lang}.pdf`),
      });
    }
  }
  return results;
}

function buildPaper(entry) {
  if (!entry.texExists) {
    console.log(`  SKIP ${entry.dir}/${entry.base}-${entry.lang}.tex (not found)`);
    return false;
  }
  const paperDir = join(PAPERS_DIR, entry.dir);
  const texName = `${entry.base}-${entry.lang}`;
  console.log(`  BUILD ${entry.dir}/${texName}.tex ...`);
  try {
    // Run xelatex 2x (sufficient for most papers without bibtex changes)
    execSync(
      `cd "${paperDir}" && xelatex -interaction=nonstopmode "${texName}.tex" && xelatex -interaction=nonstopmode "${texName}.tex"`,
      { stdio: "pipe", timeout: 120000 },
    );
    console.log(`  OK    ${texName}.pdf`);
    return true;
  } catch (e) {
    console.error(`  FAIL  ${texName}.tex — ${e.message?.slice(0, 200)}`);
    // Try to get the log for debugging
    try {
      const log = execSync(
        `tail -30 "${join(paperDir, texName + ".log")}"`,
        { encoding: "utf8" },
      );
      console.error(`  LOG:\n${log}`);
    } catch (_) {}
    return false;
  }
}

function deployPaper(entry) {
  if (!entry.pdfExists) return false;
  mkdirSync(SITE_DIR, { recursive: true });
  copyFileSync(entry.pdfFile, entry.sitePdf);
  console.log(`  DEPLOY ${basename(entry.sitePdf)}`);
  return true;
}

// --- CLI ---
const [, , cmd, langFilter] = process.argv;

if (cmd === "status" || !cmd) {
  const files = findTranslatedFiles();
  console.log("\nPapermill Translation Status\n");
  console.log(
    "Paper".padEnd(30) +
      LANGS.map((l) => LANG_NAMES[l].padEnd(12)).join(""),
  );
  console.log("-".repeat(30 + LANGS.length * 12));
  let currentDir = "";
  for (const f of files) {
    if (f.dir !== currentDir) {
      currentDir = f.dir;
      process.stdout.write(f.dir.padEnd(30));
    }
    const status = f.pdfExists ? "PDF" : f.texExists ? "tex" : "---";
    process.stdout.write(status.padEnd(12));
    if (LANGS.indexOf(f.lang) === LANGS.length - 1) process.stdout.write("\n");
  }
  console.log();
} else if (cmd === "build") {
  const files = findTranslatedFiles().filter(
    (f) => !langFilter || f.lang === langFilter,
  );
  const toBuild = files.filter((f) => f.texExists);
  console.log(
    `\nBuilding ${toBuild.length} translated paper${toBuild.length !== 1 ? "s" : ""}...\n`,
  );
  let ok = 0,
    fail = 0;
  for (const entry of toBuild) {
    if (buildPaper(entry)) ok++;
    else fail++;
  }
  console.log(`\nDone: ${ok} built, ${fail} failed.\n`);
} else if (cmd === "deploy") {
  const files = findTranslatedFiles();
  const toDeploy = files.filter((f) => f.pdfExists);
  console.log(
    `\nDeploying ${toDeploy.length} translated PDF${toDeploy.length !== 1 ? "s" : ""}...\n`,
  );
  for (const entry of toDeploy) {
    deployPaper(entry);
  }
  console.log("\nDone.\n");
} else {
  console.log("Usage: node papermill.mjs [build [da|es|zh] | deploy | status]");
}
