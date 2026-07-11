#!/usr/bin/env node

import { existsSync, readFileSync, readdirSync, writeFileSync } from "fs";
import { join } from "path";

const PAPERS_DIR = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(PAPERS_DIR, "..");
const PLATTER_PATH = join(
  REPO_ROOT,
  "system/public/papers.aesthetic.computer/platter.html",
);
const INDEX_PATH = join(
  REPO_ROOT,
  "system/public/papers.aesthetic.computer/index.html",
);

function escapeJsString(text) {
  return String(text).replaceAll("\\", "\\\\").replaceAll('"', '\\"');
}

function titleFromFilename(file) {
  const withoutExt = file.replace(/\.md$/i, "");
  const withoutDate = withoutExt.replace(/^\d{4}-\d{2}-\d{2}-/, "");
  const words = withoutDate.replaceAll(/[-_]+/g, " ").trim();
  if (!words) return file;
  return words.replace(/\b[a-z]/g, (letter) => letter.toUpperCase());
}

function countFiles(dirPath, ext) {
  if (!existsSync(dirPath)) return 0;
  return readdirSync(dirPath).filter((name) => name.endsWith(ext)).length;
}

// Count immediate subdirectories of dirPath (optionally skipping some names).
function countDirs(dirPath, skip = []) {
  if (!existsSync(dirPath)) return 0;
  return readdirSync(dirPath, { withFileTypes: true }).filter(
    (entry) => entry.isDirectory() && !skip.includes(entry.name),
  ).length;
}

// Recursively count files whose name satisfies `match(name)`, up to `depth`.
function countFilesDeep(dirPath, match, depth = 4) {
  if (depth < 0 || !existsSync(dirPath)) return 0;
  let total = 0;
  for (const entry of readdirSync(dirPath, { withFileTypes: true })) {
    if (entry.isFile()) {
      if (match(entry.name)) total += 1;
    } else if (entry.isDirectory()) {
      total += countFilesDeep(join(dirPath, entry.name), match, depth - 1);
    }
  }
  return total;
}

// Does dirPath (or a descendant, up to `depth`) contain a file matching `exts`?
function hasFileDeep(dirPath, exts, depth = 3) {
  if (depth < 0 || !existsSync(dirPath)) return false;
  for (const entry of readdirSync(dirPath, { withFileTypes: true })) {
    if (entry.isFile()) {
      if (exts.some((ext) => entry.name.endsWith(ext))) return true;
    } else if (entry.isDirectory()) {
      if (hasFileDeep(join(dirPath, entry.name), exts, depth - 1)) return true;
    }
  }
  return false;
}

// Count top-level pop/ lanes that carry musical signals (.illy.txt or .np),
// skipping shared/tooling dirs so the number tracks actual music lanes.
function countPopLanes(popDir) {
  if (!existsSync(popDir)) return 0;
  const skip = [
    "bin",
    "lib",
    "dsp",
    "out",
    "samples",
    "references",
    "demos",
    "big-pictures",
    "voice",
  ];
  let lanes = 0;
  for (const entry of readdirSync(popDir, { withFileTypes: true })) {
    if (!entry.isDirectory() || skip.includes(entry.name)) continue;
    if (hasFileDeep(join(popDir, entry.name), [".illy.txt", ".np"], 2)) lanes += 1;
  }
  return lanes;
}

// Count released singles in pop/RELEASES.md via "## <name> — RELEASED" headings.
function countReleases(releasesPath) {
  if (!existsSync(releasesPath)) return 0;
  return (readFileSync(releasesPath, "utf8").match(/^## .*RELEASED/gm) || [])
    .length;
}

function getMarkdownFiles(repoDir) {
  const fullPath = join(REPO_ROOT, repoDir);
  if (!existsSync(fullPath)) return [];
  return readdirSync(fullPath)
    .filter((name) => name.endsWith(".md"))
    .sort((a, b) => b.localeCompare(a));
}

function parseArrayEntries(source) {
  const entries = [];
  const entryRe = /\["([^"]+)",\s*"([^"]+)"\],?/g;
  let match;
  while ((match = entryRe.exec(source)) !== null) {
    entries.push([match[1], match[2]]);
  }
  return entries;
}

function syncArray(html, arrayName, repoDir) {
  const arrRe = new RegExp(`const ${arrayName} = \\[([\\s\\S]*?)\\n\\s*\\];`, "m");
  const match = html.match(arrRe);
  if (!match) throw new Error(`Could not find ${arrayName} array in platter.html`);

  const existingEntries = parseArrayEntries(match[1]);
  const existingMap = new Map(existingEntries);
  const existingOrder = existingEntries.map(([file]) => file);
  const actualFiles = getMarkdownFiles(repoDir);

  const kept = existingOrder.filter((file) => actualFiles.includes(file));
  const added = actualFiles.filter((file) => !existingMap.has(file));
  const ordered = [...kept, ...added];

  const rendered = ordered
    .map((file) => {
      const title = existingMap.get(file) || titleFromFilename(file);
      return `            ["${escapeJsString(file)}","${escapeJsString(title)}"],`;
    })
    .join("\n");

  const nextBlock = `const ${arrayName} = [\n${rendered}\n        ];`;
  const nextHtml = html.replace(arrRe, nextBlock);
  return {
    html: nextHtml,
    count: ordered.length,
    added: added.length,
    removed: existingOrder.length - kept.length,
  };
}

function replaceOrThrow(html, pattern, replacement, label) {
  if (!pattern.test(html)) {
    throw new Error(`Could not update ${label}`);
  }
  return html.replace(pattern, replacement);
}

function main() {
  if (!existsSync(PLATTER_PATH)) {
    throw new Error(`Missing platter file: ${PLATTER_PATH}`);
  }

  let html = readFileSync(PLATTER_PATH, "utf8");
  const reports = syncArray(html, "reports", "reports");
  html = reports.html;
  const plans = syncArray(html, "plans", "plans");
  html = plans.html;
  const studies = syncArray(html, "studies", "studies");
  html = studies.html;

  const piecesMjs = countFiles(
    join(REPO_ROOT, "system/public/aesthetic.computer/disks"),
    ".mjs",
  );
  const piecesLisp = countFiles(
    join(REPO_ROOT, "system/public/aesthetic.computer/disks"),
    ".lisp",
  );
  const piecesTotal = piecesMjs + piecesLisp;
  const libModules = countFiles(
    join(REPO_ROOT, "system/public/aesthetic.computer/lib"),
    ".mjs",
  );
  const functionCount = countFiles(join(REPO_ROOT, "system/netlify/functions"), ".mjs");
  const sessionModules = countFiles(join(REPO_ROOT, "session-server"), ".mjs");
  const papersCount = existsSync(INDEX_PATH)
    ? (readFileSync(INDEX_PATH, "utf8").match(/class="p"\s+data-paper-id=/g) || [])
        .length
    : 0;
  const readingsCount = countFiles(
    join(REPO_ROOT, "system/public/assets/papers/readings/text"),
    ".txt",
  );

  // Pop / Music surface.
  const popDir = join(REPO_ROOT, "pop");
  const popLanes = countPopLanes(popDir);
  const popReleases = countReleases(join(popDir, "RELEASES.md"));
  const popMotionDrivers = (() => {
    if (!existsSync(popDir)) return 0;
    let n = 0;
    for (const entry of readdirSync(popDir, { withFileTypes: true })) {
      if (!entry.isDirectory()) continue;
      const binDir = join(popDir, entry.name, "bin");
      if (!existsSync(binDir)) continue;
      n += readdirSync(binDir).filter(
        (name) => name.startsWith("gen-motion-") && name.endsWith(".mjs"),
      ).length;
    }
    return n;
  })();

  // Marketing surface.
  const marketingDir = join(REPO_ROOT, "marketing");
  const marketingCampaigns = countDirs(join(marketingDir, "campaigns"));
  const marketingReelLanes = [
    "av-reels",
    "kidlisp-reels",
    "whistlegraph-seedance",
    "podcast",
  ].filter((name) => existsSync(join(marketingDir, name))).length;
  const marketingImagePrompts = countFilesDeep(
    join(marketingDir, "campaigns"),
    (name) => name.startsWith("cover-prompt") && name.endsWith(".txt"),
    4,
  );

  html = replaceOrThrow(
    html,
    /<div class="stats">[\s\S]*?<\/div>/,
    `<div class="stats">
            <span>${piecesTotal}</span> pieces · <span>${libModules}</span> lib modules · <span>${functionCount}</span> functions · <span>${plans.count}</span> plans · <span>${reports.count}</span> reports · <span>${studies.count}</span> studies · <span>${readingsCount}+</span> readings · <span>${papersCount}</span> papers
        </div>`,
    "stats block",
  );

  html = replaceOrThrow(
    html,
    /(<h2>Papers<\/h2>\s*<span class="count">)\d+ publications(<\/span>)/,
    `$1${papersCount} publications$2`,
    "papers count",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Pieces<\/h2>\s*<span class="count">)\d+ \.mjs \+ \d+ \.lisp(<\/span>)/,
    `$1${piecesMjs} .mjs + ${piecesLisp} .lisp$2`,
    "pieces count",
  );
  html = replaceOrThrow(
    html,
    /Full listing: \d+ \.mjs \+ \d+ \.lisp pieces/,
    `Full listing: ${piecesMjs} .mjs + ${piecesLisp} .lisp pieces`,
    "pieces listing count",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Servers & Services<\/h2>\s*<span class="count">)\d+ functions · 5 services · \d+ session modules(<\/span>)/,
    `$1${functionCount} functions · 5 services · ${sessionModules} session modules$2`,
    "servers count",
  );
  html = replaceOrThrow(
    html,
    /Netlify Functions \(\d+ serverless endpoints\)/,
    `Netlify Functions (${functionCount} serverless endpoints)`,
    "netlify functions label",
  );
  html = replaceOrThrow(
    html,
    /(<h2>APIs & Data Sources<\/h2>\s*<span class="count">)\d+ endpoints · 32\+ collections(<\/span>)/,
    `$1${functionCount} endpoints · 32+ collections$2`,
    "api endpoints count",
  );
  html = replaceOrThrow(
    html,
    /<span class="file">\d+ total<\/span> Netlify serverless functions/,
    `<span class="file">${functionCount} total</span> Netlify serverless functions`,
    "api total label",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Reports<\/h2>\s*<span class="count">)\d+ documents(<\/span>)/,
    `$1${reports.count} documents$2`,
    "reports count",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Plans<\/h2>\s*<span class="count">)\d+ documents(<\/span>)/,
    `$1${plans.count} documents$2`,
    "plans count",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Studies<\/h2>\s*<span class="count">)\d+ documents(<\/span>)/,
    `$1${studies.count} documents$2`,
    "studies count",
  );
  html = replaceOrThrow(
    html,
    /\d+ built-in pieces ·/,
    `${piecesTotal} built-in pieces ·`,
    "platform stats built-in pieces",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Pop \/ Music<\/h2>\s*<span class="count">)\d+ lanes · \d+ released · \d+ motion drivers(<\/span>)/,
    `$1${popLanes} lanes · ${popReleases} released · ${popMotionDrivers} motion drivers$2`,
    "pop count",
  );
  html = replaceOrThrow(
    html,
    /(<h2>Marketing<\/h2>\s*<span class="count">)\d+ campaigns · \d+ reel pipelines · \d+ image prompts(<\/span>)/,
    `$1${marketingCampaigns} campaigns · ${marketingReelLanes} reel pipelines · ${marketingImagePrompts} image prompts$2`,
    "marketing count",
  );

  writeFileSync(PLATTER_PATH, html, "utf8");

  console.log("\nSynced platter.html");
  console.log(`reports: ${reports.count} (${reports.added} added, ${reports.removed} removed)`);
  console.log(`plans:   ${plans.count} (${plans.added} added, ${plans.removed} removed)`);
  console.log(`studies: ${studies.count} (${studies.added} added, ${studies.removed} removed)`);
  console.log(
    `stats: pieces=${piecesTotal} (${piecesMjs}+${piecesLisp}), lib=${libModules}, functions=${functionCount}, papers=${papersCount}`,
  );
  console.log(
    `pop: ${popLanes} lanes, ${popReleases} released, ${popMotionDrivers} motion drivers`,
  );
  console.log(
    `marketing: ${marketingCampaigns} campaigns, ${marketingReelLanes} reel pipelines, ${marketingImagePrompts} image prompts`,
  );
}

main();
