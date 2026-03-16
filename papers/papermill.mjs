#!/usr/bin/env node
// papermill.mjs — Build script for translated papers
// Usage:
//   node papermill.mjs build              Build all translated PDFs
//   node papermill.mjs build da           Build only Danish translations
//   node papermill.mjs build es           Build only Spanish translations
//   node papermill.mjs build zh           Build only Chinese translations
//   node papermill.mjs build --format cards  Build cards-format PDFs
//   node papermill.mjs deploy             Copy compiled PDFs to site directory
//   node papermill.mjs sync-index         Extract titles from .tex files → translations.json
//   node papermill.mjs status             Show which translations exist

import { execSync } from "child_process";
import {
  readdirSync,
  existsSync,
  copyFileSync,
  readFileSync,
  writeFileSync,
  mkdirSync,
} from "fs";
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
  "arxiv-ac": {
    base: "ac",
    siteName: "aesthetic-computer-26-arxiv",
    paperId: "ac",
  },
  "arxiv-api": {
    base: "api",
    siteName: "piece-api-26-arxiv",
    paperId: "api",
  },
  "arxiv-archaeology": {
    base: "archaeology",
    siteName: "repo-archaeology-26-arxiv",
    paperId: "archaeology",
  },
  "arxiv-dead-ends": {
    base: "dead-ends",
    siteName: "dead-ends-26-arxiv",
    paperId: "dead-ends",
  },
  "arxiv-diversity": {
    base: "diversity",
    siteName: "citation-diversity-audit-26",
    paperId: "diversity",
  },
  "arxiv-folk-songs": {
    base: "folk-songs",
    siteName: "folk-songs-26-arxiv",
    paperId: "folk-songs",
  },
  "arxiv-goodiepal": {
    base: "goodiepal",
    siteName: "radical-computer-art-26-arxiv",
    paperId: "goodiepal",
  },
  "arxiv-kidlisp": {
    base: "kidlisp",
    siteName: "kidlisp-26-arxiv",
    paperId: "kidlisp",
  },
  "arxiv-kidlisp-reference": {
    base: "kidlisp-reference",
    siteName: "kidlisp-reference-26-arxiv",
    paperId: "kidlisp-ref",
  },
  "arxiv-network-audit": {
    base: "network-audit",
    siteName: "network-audit-26-arxiv",
    paperId: "network-audit",
  },
  "arxiv-notepat": {
    base: "notepat",
    siteName: "notepat-26-arxiv",
    paperId: "notepat",
  },
  "arxiv-os": {
    base: "os",
    siteName: "ac-native-os-26-arxiv",
    paperId: "os",
  },
  "arxiv-pieces": {
    base: "pieces",
    siteName: "pieces-not-programs-26-arxiv",
    paperId: "pieces",
  },
  "arxiv-sustainability": {
    base: "sustainability",
    siteName: "who-pays-for-creative-tools-26-arxiv",
    paperId: "who-pays",
  },
  "arxiv-whistlegraph": {
    base: "whistlegraph",
    siteName: "whistlegraph-26-arxiv",
    paperId: "whistlegraph",
  },
};

// --- File discovery ---

function findTranslatedFiles(format) {
  const results = [];
  for (const [dir, info] of Object.entries(PAPER_MAP)) {
    const paperDir = join(PAPERS_DIR, dir);
    if (!existsSync(paperDir)) continue;
    for (const lang of LANGS) {
      const suffix = format ? `-${format}` : "";
      const texBase = format
        ? `${info.base}-${format}-${lang}`
        : `${info.base}-${lang}`;
      const texFile = join(paperDir, `${texBase}.tex`);
      const pdfFile = join(paperDir, `${texBase}.pdf`);
      const siteBase = format
        ? `${info.siteName}-${format}-${lang}`
        : `${info.siteName}-${lang}`;
      results.push({
        dir,
        lang,
        format: format || "layout",
        base: info.base,
        siteName: info.siteName,
        texFile,
        pdfFile,
        texExists: existsSync(texFile),
        pdfExists: existsSync(pdfFile),
        sitePdf: join(SITE_DIR, `${siteBase}.pdf`),
      });
    }
  }
  return results;
}

function buildPaper(entry) {
  if (!entry.texExists) {
    console.log(
      `  SKIP ${entry.dir}/${basename(entry.texFile)} (not found)`,
    );
    return false;
  }
  const paperDir = join(PAPERS_DIR, entry.dir);
  const texName = basename(entry.texFile, ".tex");
  console.log(`  BUILD ${entry.dir}/${texName}.tex ...`);
  try {
    // Run xelatex + bibtex + xelatex + xelatex (full 3-pass build for citations)
    execSync(
      `cd "${paperDir}" && xelatex -interaction=nonstopmode "${texName}.tex" && bibtex "${texName}" 2>/dev/null; xelatex -interaction=nonstopmode "${texName}.tex" && xelatex -interaction=nonstopmode "${texName}.tex"`,
      { stdio: "pipe", timeout: 180000 },
    );
    console.log(`  OK    ${texName}.pdf`);
    return true;
  } catch (e) {
    console.error(`  FAIL  ${texName}.tex — ${e.message?.slice(0, 200)}`);
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

// --- Title extraction from .tex files ---

function extractTitleFromTex(texPath) {
  if (!existsSync(texPath)) return null;
  const content = readFileSync(texPath, "utf8");
  const lines = content.split("\n").slice(0, 250); // Title can be after preamble

  let titleParts = [];
  let subtitle = null;

  // Title pattern: any bold font + large fontsize + \color{*dark*} — can span multiple lines
  // Matches \acbold, \kidlispbold, \wgbold, etc.
  const titleRe =
    /\\[a-z]+bold\\fontsize\{[^}]+\}\{[^}]+\}\\selectfont\\color\{[a-z]+\}\s*(.+?)\s*\}\\par/;
  // Subtitle pattern: any light font + smaller fontsize + \color{*pink/brand*}
  const subtitleRe =
    /\\[a-z]+(?:light|font)\\fontsize\{[^}]+\}\{[^}]+\}\\selectfont\\color\{[a-z]+\}\s*(.+?)\s*\}\\par/;

  for (const line of lines) {
    const boldMatch = line.match(titleRe);
    if (boldMatch) {
      titleParts.push(boldMatch[1].replace(/\\par$/, "").trim());
    }

    const lightMatch = line.match(subtitleRe);
    if (lightMatch && !subtitle) {
      subtitle = lightMatch[1].replace(/\\par$/, "").trim();
    }
  }

  // Join multi-line titles (e.g., "Playable" + "Folk Songs")
  let title = titleParts.length > 0 ? titleParts.join(" ") : null;

  // Clean up LaTeX commands from extracted text
  if (title) title = cleanLatex(title);
  if (subtitle) subtitle = cleanLatex(subtitle);

  return { title, subtitle };
}

function cleanLatex(text) {
  return text
    .replace(/\\ac\{\}/g, "Aesthetic Computer")
    .replace(/\\acos\{\}/g, "AC Native OS")
    .replace(/\\np\{\}/g, "notepat")
    .replace(/\\acdot/g, ".")
    .replace(/\{\\color\{[^}]+\}([^}]*)\}/g, "$1") // {\color{acpink}text} → text
    .replace(/\\color\{[^}]+\}/g, "") // bare \color{...}
    .replace(/\\textsc\{([^}]+)\}/g, "$1")
    .replace(/\\textbf\{([^}]+)\}/g, "$1")
    .replace(/\\textit\{([^}]+)\}/g, "$1")
    .replace(/\\texttt\{([^}]+)\}/g, "$1")
    .replace(/\\emph\{([^}]+)\}/g, "$1")
    .replace(/\\url\{([^}]+)\}/g, "$1")
    .replace(/\\href\{[^}]+\}\{([^}]+)\}/g, "$1")
    .replace(/\\\\/g, "")
    .replace(/\\,/g, "")
    .replace(/\\&/g, "&")
    .replace(/---/g, "\u2014")
    .replace(/--/g, "\u2013")
    .replace(/``/g, "\u201c")
    .replace(/''/g, "\u201d")
    .replace(/~/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function syncIndex() {
  console.log("\nSync-index: extracting titles from .tex files...\n");
  const translations = {};

  for (const [dir, info] of Object.entries(PAPER_MAP)) {
    const paperDir = join(PAPERS_DIR, dir);
    if (!existsSync(paperDir)) continue;

    const paperId = info.paperId;
    const entry = {};

    // Extract English title + subtitle (base .tex file)
    const enTex = join(paperDir, `${info.base}.tex`);
    const enData = extractTitleFromTex(enTex);
    if (enData) {
      entry.en = {};
      if (enData.title) entry.en.title = enData.title;
      if (enData.subtitle) entry.en.subtitle = enData.subtitle;
    }

    // Extract translated titles
    for (const lang of LANGS) {
      const texPath = join(paperDir, `${info.base}-${lang}.tex`);
      const data = extractTitleFromTex(texPath);
      if (data && data.title) {
        entry[lang] = {};
        entry[lang].title = data.title;
        if (data.subtitle) entry[lang].subtitle = data.subtitle;
      }
    }

    if (Object.keys(entry).length > 0) {
      translations[paperId] = entry;
    }
  }

  const outPath = join(SITE_DIR, "translations.json");
  mkdirSync(SITE_DIR, { recursive: true });
  writeFileSync(outPath, JSON.stringify(translations, null, 2), "utf8");
  console.log(`  WROTE ${outPath}`);
  console.log(
    `  ${Object.keys(translations).length} papers, ${LANGS.length} languages\n`,
  );
  return translations;
}

// --- CLI ---
const args = process.argv.slice(2);
const cmd = args[0];
const langFilter = args.find((a) => LANGS.includes(a));
const formatFlag = args.includes("--format")
  ? args[args.indexOf("--format") + 1]
  : null;

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
    if (LANGS.indexOf(f.lang) === LANGS.length - 1)
      process.stdout.write("\n");
  }
  console.log();
} else if (cmd === "build") {
  const files = findTranslatedFiles(formatFlag).filter(
    (f) => !langFilter || f.lang === langFilter,
  );
  const toBuild = files.filter((f) => f.texExists);
  const label = formatFlag ? ` (${formatFlag} format)` : "";
  console.log(
    `\nBuilding ${toBuild.length} translated paper${toBuild.length !== 1 ? "s" : ""}${label}...\n`,
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
  // Also deploy cards format if any exist
  const cardsFiles = findTranslatedFiles("cards");
  const cardsToDeploy = cardsFiles.filter((f) => f.pdfExists);
  if (cardsToDeploy.length > 0) {
    console.log(
      `\nDeploying ${cardsToDeploy.length} cards PDF${cardsToDeploy.length !== 1 ? "s" : ""}...\n`,
    );
    for (const entry of cardsToDeploy) {
      deployPaper(entry);
    }
  }
  // Auto sync-index on deploy
  syncIndex();
  console.log("\nDone.\n");
} else if (cmd === "sync-index") {
  syncIndex();
} else {
  console.log(
    "Usage: node papermill.mjs [build [da|es|zh] [--format cards] | deploy | sync-index | status]",
  );
}
