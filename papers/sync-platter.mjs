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

  html = replaceOrThrow(
    html,
    /<div class="stats">[\s\S]*?<\/div>/,
    `<div class="stats">
            <span>${piecesTotal}</span> pieces · <span>${libModules}</span> lib modules · <span>${functionCount}</span> functions · <span>${plans.count}</span> plans · <span>${reports.count}</span> reports · <span>${studies.count}</span> studies · <span>30+</span> readings · <span>${papersCount}</span> papers
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

  writeFileSync(PLATTER_PATH, html, "utf8");

  console.log("\nSynced platter.html");
  console.log(`reports: ${reports.count} (${reports.added} added, ${reports.removed} removed)`);
  console.log(`plans:   ${plans.count} (${plans.added} added, ${plans.removed} removed)`);
  console.log(`studies: ${studies.count} (${studies.added} added, ${studies.removed} removed)`);
  console.log(
    `stats: pieces=${piecesTotal} (${piecesMjs}+${piecesLisp}), lib=${libModules}, functions=${functionCount}, papers=${papersCount}`,
  );
}

main();
