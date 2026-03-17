#!/usr/bin/env node
// papers cli — build, deploy, and track all AC papers
//
// Usage:
//   node papers/cli.mjs build [lang]     Build PDFs (all langs, or: en, da, es)
//   node papers/cli.mjs deploy           Copy built PDFs to site directory
//   node papers/cli.mjs publish          Build all + deploy + update index
//   node papers/cli.mjs status           Show build status for all papers
//   node papers/cli.mjs log              Show build log
//
// Examples:
//   node papers/cli.mjs build            Build everything (en + da + es)
//   node papers/cli.mjs build en         Build only English PDFs
//   node papers/cli.mjs publish          Full pipeline: build all → deploy → update index

import { execSync } from "child_process";
import {
  existsSync,
  copyFileSync,
  mkdirSync,
  readFileSync,
  writeFileSync,
  statSync,
} from "fs";
import { join, basename } from "path";

const PAPERS_DIR = new URL(".", import.meta.url).pathname;
const SITE_DIR = join(
  PAPERS_DIR,
  "../system/public/papers.aesthetic.computer",
);
const BUILDLOG = join(PAPERS_DIR, "BUILDLOG.md");
const METADATA_PATH = join(PAPERS_DIR, "metadata.json");
const LANGS = ["en", "da", "es"];
const LANG_NAMES = { en: "English", da: "Danish", es: "Spanish" };

function loadMetadata() {
  if (!existsSync(METADATA_PATH)) return {};
  return JSON.parse(readFileSync(METADATA_PATH, "utf8"));
}

function saveMetadata(meta) {
  writeFileSync(METADATA_PATH, JSON.stringify(meta, null, 2) + "\n", "utf8");
}

// Map paper dir → tex base name + site PDF name
const PAPER_MAP = {
  "arxiv-ac": {
    base: "ac",
    siteName: "aesthetic-computer-26-arxiv",
    title: "Aesthetic Computer '26",
  },
  "arxiv-api": {
    base: "api",
    siteName: "piece-api-26-arxiv",
    title: "From setup() to boot()",
  },
  "arxiv-archaeology": {
    base: "archaeology",
    siteName: "repo-archaeology-26-arxiv",
    title: "Repository Archaeology",
  },
  "arxiv-dead-ends": {
    base: "dead-ends",
    siteName: "dead-ends-26-arxiv",
    title: "Vestigial Features",
  },
  "arxiv-diversity": {
    base: "diversity",
    siteName: "citation-diversity-audit-26",
    title: "Citation Diversity Audit",
  },
  "arxiv-goodiepal": {
    base: "goodiepal",
    siteName: "radical-computer-art-26-arxiv",
    title: "Radical Computer Art",
  },
  "arxiv-kidlisp": {
    base: "kidlisp",
    siteName: "kidlisp-26-arxiv",
    title: "KidLisp '26",
  },
  "arxiv-kidlisp-reference": {
    base: "kidlisp-reference",
    siteName: "kidlisp-reference-26-arxiv",
    title: "KidLisp Language Reference",
  },
  "arxiv-network-audit": {
    base: "network-audit",
    siteName: "network-audit-26-arxiv",
    title: "Network Audit",
  },
  "arxiv-notepat": {
    base: "notepat",
    siteName: "notepat-26-arxiv",
    title: "notepat.com",
  },
  "arxiv-os": {
    base: "os",
    siteName: "ac-native-os-26-arxiv",
    title: "AC Native OS",
  },
  "arxiv-pieces": {
    base: "pieces",
    siteName: "pieces-not-programs-26-arxiv",
    title: "Pieces Not Programs",
  },
  "arxiv-sustainability": {
    base: "sustainability",
    siteName: "who-pays-for-creative-tools-26-arxiv",
    title: "Who Pays for Creative Tools?",
  },
  "arxiv-whistlegraph": {
    base: "whistlegraph",
    siteName: "whistlegraph-26-arxiv",
    title: "Whistlegraph",
  },
  "arxiv-plork": {
    base: "plork",
    siteName: "plorking-the-planet-26-arxiv",
    title: "PLOrk'ing the Planet",
  },
  "arxiv-folk-songs": {
    base: "folk-songs",
    siteName: "folk-songs-26-arxiv",
    title: "Playable Folk Songs",
  },
  "arxiv-complex": {
    base: "complex",
    siteName: "sucking-on-the-complex-26-arxiv",
    title: "Sucking on the Complex",
  },
};

function texName(base, lang) {
  return lang === "en" ? base : `${base}-${lang}`;
}

function sitePdfName(siteName, lang) {
  return lang === "en" ? `${siteName}.pdf` : `${siteName}-${lang}.pdf`;
}

function findAll(langFilter) {
  const results = [];
  for (const [dir, info] of Object.entries(PAPER_MAP)) {
    const paperDir = join(PAPERS_DIR, dir);
    if (!existsSync(paperDir)) continue;
    const langs = langFilter ? [langFilter] : LANGS;
    for (const lang of langs) {
      const tex = texName(info.base, lang);
      const texFile = join(paperDir, `${tex}.tex`);
      const pdfFile = join(paperDir, `${tex}.pdf`);
      const sitePdf = join(SITE_DIR, sitePdfName(info.siteName, lang));
      results.push({
        dir,
        lang,
        base: info.base,
        title: info.title,
        siteName: info.siteName,
        texFile,
        pdfFile,
        texExists: existsSync(texFile),
        pdfExists: existsSync(pdfFile),
        sitePdf,
        sitePdfExists: existsSync(sitePdf),
      });
    }
  }
  return results;
}

function buildOne(entry) {
  if (!entry.texExists) {
    console.log(`  SKIP ${entry.dir}/${texName(entry.base, entry.lang)}.tex (not found)`);
    return false;
  }
  const paperDir = join(PAPERS_DIR, entry.dir);
  const tex = texName(entry.base, entry.lang);
  console.log(`  BUILD ${entry.dir}/${tex}.tex ...`);
  try {
    execSync(
      `cd "${paperDir}" && xelatex -interaction=nonstopmode "${tex}.tex" && bibtex "${tex}" 2>/dev/null; xelatex -interaction=nonstopmode "${tex}.tex" && xelatex -interaction=nonstopmode "${tex}.tex"`,
      { stdio: "pipe", timeout: 180000 },
    );
    console.log(`  OK    ${tex}.pdf`);
    return true;
  } catch (e) {
    console.error(`  FAIL  ${tex}.tex — ${e.message?.slice(0, 200)}`);
    try {
      const log = execSync(`tail -20 "${join(paperDir, tex + ".log")}"`, {
        encoding: "utf8",
      });
      console.error(`  LOG:\n${log}`);
    } catch (_) {}
    return false;
  }
}

function deployOne(entry) {
  if (!entry.pdfExists) return false;
  mkdirSync(SITE_DIR, { recursive: true });
  copyFileSync(entry.pdfFile, entry.sitePdf);
  console.log(`  DEPLOY ${basename(entry.sitePdf)}`);
  return true;
}

function now() {
  return new Date().toISOString().replace("T", " ").slice(0, 16);
}

function appendBuildLog(built, failed) {
  const stamp = now();
  const lines = [`\n## ${stamp}\n`];
  if (built.length) {
    lines.push("Built:");
    for (const e of built)
      lines.push(`- ${e.title} [${e.lang}] → ${basename(e.sitePdf)}`);
  }
  if (failed.length) {
    lines.push("\nFailed:");
    for (const e of failed)
      lines.push(`- ${e.title} [${e.lang}]`);
  }
  lines.push("");

  if (!existsSync(BUILDLOG)) {
    writeFileSync(
      BUILDLOG,
      `# Papers Build Log\n\nGeneration history for all AC paper PDFs.\n${lines.join("\n")}`,
    );
  } else {
    const existing = readFileSync(BUILDLOG, "utf8");
    writeFileSync(BUILDLOG, existing + lines.join("\n"));
  }
  console.log(`\n  Build log updated: ${BUILDLOG}`);
}

function updateIndex(entries) {
  const indexPath = join(SITE_DIR, "index.html");
  if (!existsSync(indexPath)) {
    console.log("  SKIP index update (index.html not found)");
    return;
  }

  const meta = loadMetadata();

  // Importance ranking — curated order for 2026 impact
  const IMPORTANCE = {
    "aesthetic-computer-26-arxiv": 1,
    "kidlisp-26-arxiv": 2,
    "plorking-the-planet-26-arxiv": 3,
    "ac-native-os-26-arxiv": 4,
    "piece-api-26-arxiv": 5,
    "who-pays-for-creative-tools-26-arxiv": 6,
    "pieces-not-programs-26-arxiv": 7,
    "notepat-26-arxiv": 8,
    "radical-computer-art-26-arxiv": 9,
    "whistlegraph-26-arxiv": 10,
    "sucking-on-the-complex-26-arxiv": 11,
    "dead-ends-26-arxiv": 12,
    "folk-songs-26-arxiv": 13,
    "repo-archaeology-26-arxiv": 14,
    "network-audit-26-arxiv": 15,
    "kidlisp-reference-26-arxiv": 16,
    "citation-diversity-audit-26": 17,
  };

  // Collect deployed English PDFs sorted by importance
  const papers = [];
  for (const e of entries.filter((e) => e.lang === "en" && e.sitePdfExists)) {
    const stat = statSync(e.sitePdf);
    const m = meta[e.dir] || {};
    const rank = IMPORTANCE[e.siteName] || 99;
    papers.push({ ...e, mtime: stat.mtime, created: m.created || null, revisions: m.revisions || 0, rank });
  }
  papers.sort((a, b) => a.rank - b.rank);

  // Also include JOSS/ELS papers that aren't in PAPER_MAP
  const extraPdfs = [
    {
      file: "aesthetic-computer-26-joss.pdf",
      title: "Aesthetic Computer '26",
      detail: "JOSS Summary &middot; 2pp",
      metaKey: "joss-ac",
    },
    {
      file: "kidlisp-26-joss.pdf",
      title: "KidLisp '26",
      detail: "JOSS Summary &middot; 3pp",
      metaKey: "joss-kidlisp",
    },
    {
      file: "kidlisp-els-2026.pdf",
      title: "KidLisp (ELS 2026)",
      detail:
        "A Minimal Lisp for Generative Art with Social Composition &middot; ELS ACM SIGS 4pp",
      metaKey: "els-kidlisp",
    },
  ];
  const extras = [];
  for (const ex of extraPdfs) {
    const fp = join(SITE_DIR, ex.file);
    if (existsSync(fp)) {
      const stat = statSync(fp);
      const m = meta[ex.metaKey] || {};
      extras.push({ ...ex, mtime: stat.mtime, created: m.created || null, revisions: m.revisions || 0 });
    }
  }
  extras.sort((a, b) => b.mtime - a.mtime);

  // Paper detail descriptions (keyed by siteName)
  const DETAILS = {
    "radical-computer-art-26-arxiv":
      "Goodiepalian Approaches in Aesthetic Computer &middot; arXiv 5pp",
    "who-pays-for-creative-tools-26-arxiv":
      "Funding, Burnout, and Survival in Open-Source Creative Computing &middot; arXiv 5pp",
    "pieces-not-programs-26-arxiv":
      "The Piece as a Unit of Creative Cognition &middot; arXiv 4pp",
    "piece-api-26-arxiv":
      "Processing at the Core of the Piece API &middot; arXiv 7pp",
    "network-audit-26-arxiv":
      "Who Uses Aesthetic Computer and What Do They Make? &middot; arXiv 4pp",
    "kidlisp-reference-26-arxiv":
      "118 Built-ins in 12 Categories &middot; arXiv 4pp",
    "whistlegraph-26-arxiv":
      "Drawing, Singing, and the Graphic Score as Viral Form &middot; arXiv 4pp",
    "dead-ends-26-arxiv":
      "Dormant Paths, Evolutionary Branches, and Abandoned Approaches &middot; arXiv 4pp",
    "repo-archaeology-26-arxiv":
      'Tracing the Evolution of AC Through Its Git History &middot; arXiv 3pp &middot; <a href="/ac-repo-archaeology">interactive timeline</a>',
    "citation-diversity-audit-26":
      "Diversity and Inclusion in AC Paper Citations &middot; 4pp",
    "kidlisp-26-arxiv":
      "A Minimal Lisp for Generative Art on a Social Platform &middot; arXiv 6pp",
    "notepat-26-arxiv":
      "From Keyboard Toy to System Front Door &middot; arXiv 5pp",
    "ac-native-os-26-arxiv":
      "A Bare-Metal Creative Computing Operating System &middot; arXiv 5pp",
    "aesthetic-computer-26-arxiv":
      "A Mobile-First Runtime for Creative Computing &middot; arXiv 5pp",
    "plorking-the-planet-26-arxiv":
      "Laptop Orchestras, PLOrk Heritage, and Aesthetic Computer &middot; arXiv",
    "folk-songs-26-arxiv":
      "Oral Tradition Meets the Browser Keyboard &middot; arXiv",
    "sucking-on-the-complex-26-arxiv":
      "Platform Hegemony, Critique-as-Content, and Anti-Environments &middot; arXiv 5pp",
  };

  function fmtTime(d) {
    const m = d.toLocaleString("en-US", { month: "short", timeZone: "America/Los_Angeles" });
    const day = d.getDate();
    const h = String(d.getHours()).padStart(2, "0");
    const min = String(d.getMinutes()).padStart(2, "0");
    return `${m} ${day} ${h}:${min}`;
  }

  function fmtDate(d) {
    return `${d.slice(5, 7)}/${d.slice(8, 10)}`;
  }

  // Build paper entries HTML
  let papersHtml = "";
  for (const p of papers) {
    const detail = DETAILS[p.siteName] || "";
    const hasCards = existsSync(join(SITE_DIR, `${p.siteName}-cards.pdf`));
    const createdStr = p.created ? fmtDate(p.created) : "";
    const revStr = p.revisions > 0 ? `r${p.revisions}` : "";
    papersHtml += `
    <div class="p"${hasCards ? "" : ` data-no-cards="1"`}>
        <div class="title"><a href="/${p.siteName}.pdf" data-base="/${p.siteName}">${p.title}</a></div>
        <div class="detail">${detail}</div>
        <div class="meta-row"><span class="created" title="Created">${createdStr}</span><span class="revisions" title="Revisions">${revStr}</span><span class="updated" title="Last updated">${fmtTime(p.mtime)}</span></div>
    </div>\n`;
  }
  for (const ex of extras) {
    const createdStr = ex.created ? fmtDate(ex.created) : "";
    const revStr = ex.revisions > 0 ? `r${ex.revisions}` : "";
    papersHtml += `
    <div class="p">
        <div class="title"><a href="/${ex.file}">${ex.title}</a></div>
        <div class="detail">${ex.detail}</div>
        <div class="meta-row"><span class="created" title="Created">${createdStr}</span><span class="revisions" title="Revisions">${revStr}</span><span class="updated" title="Last updated">${fmtTime(ex.mtime)}</span></div>
    </div>\n`;
  }

  // Read current index, replace paper entries between markers
  let html = readFileSync(indexPath, "utf8");

  // Replace everything between the sub line and the footer
  const startMarker = "<!-- papers-start -->";
  const endMarker = "<!-- papers-end -->";

  if (html.includes(startMarker)) {
    const before = html.slice(0, html.indexOf(startMarker) + startMarker.length);
    const after = html.slice(html.indexOf(endMarker));
    html = before + "\n" + papersHtml + "\n    " + after;
  } else {
    // Add markers on first run — replace from first .p div to footer
    const firstP = html.indexOf('<div class="p">');
    const footer = html.indexOf('<div class="footer">');
    if (firstP !== -1 && footer !== -1) {
      html =
        html.slice(0, firstP) +
        startMarker +
        "\n" +
        papersHtml +
        "\n    " +
        endMarker +
        "\n\n    " +
        html.slice(footer);
    }
  }

  writeFileSync(indexPath, html);
  console.log(`  INDEX updated with ${papers.length + extras.length} papers sorted by last built.`);
}

function verify() {
  const indexPath = join(SITE_DIR, "index.html");
  if (!existsSync(indexPath)) {
    console.log("  SKIP verify (index.html not found)");
    return true;
  }
  const html = readFileSync(indexPath, "utf8");

  // Extract all href links to PDFs from the generated paper entries
  const hrefRe = /href="\/([^"]+\.pdf)"/g;
  let match;
  const linked = new Set();
  while ((match = hrefRe.exec(html)) !== null) {
    linked.add(match[1]);
  }

  // Also check cards links — only for papers that have cards (no data-no-cards attr)
  const paperBlockRe = /<div class="p"(?:(?!data-no-cards)[^>])*>[\s\S]*?data-base="\/([^"]+)"[\s\S]*?<\/div>\s*<\/div>/g;
  while ((match = paperBlockRe.exec(html)) !== null) {
    linked.add(`${match[1]}-cards.pdf`);
  }

  let ok = 0;
  let broken = 0;
  for (const pdf of [...linked].sort()) {
    const fp = join(SITE_DIR, pdf);
    if (existsSync(fp)) {
      ok++;
    } else {
      console.log(`  BROKEN  /${pdf}`);
      broken++;
    }
  }
  console.log(`  ${ok} OK, ${broken} broken link${broken !== 1 ? "s" : ""}`);
  if (broken > 0) {
    console.log(
      "\n  ⚠  Some papers have broken PDF links. Build cards or remove dead links.",
    );
  }
  return broken === 0;
}

// --- CLI ---
const [, , cmd, langFilter] = process.argv;

if (cmd === "status" || !cmd) {
  const files = findAll();
  console.log("\nPapers Build Status\n");
  console.log(
    "Paper".padEnd(32) + LANGS.map((l) => LANG_NAMES[l].padEnd(12)).join(""),
  );
  console.log("-".repeat(32 + LANGS.length * 12));
  let currentDir = "";
  for (const f of files) {
    if (f.dir !== currentDir) {
      currentDir = f.dir;
      process.stdout.write(f.dir.padEnd(32));
    }
    const hasTex = f.texExists;
    const hasPdf = f.sitePdfExists;
    const status = hasPdf ? "OK" : hasTex ? "tex" : "---";
    process.stdout.write(status.padEnd(12));
    if (LANGS.indexOf(f.lang) === LANGS.length - 1) process.stdout.write("\n");
  }
  console.log();
} else if (cmd === "build") {
  const filter = langFilter && LANGS.includes(langFilter) ? langFilter : null;
  const files = findAll(filter);
  const toBuild = files.filter((f) => f.texExists);
  console.log(
    `\nBuilding ${toBuild.length} paper${toBuild.length !== 1 ? "s" : ""}${filter ? ` (${LANG_NAMES[filter]})` : " (all languages)"}...\n`,
  );
  const built = [];
  const failed = [];
  for (const entry of toBuild) {
    if (buildOne(entry)) built.push(entry);
    else failed.push(entry);
  }
  console.log(`\nDone: ${built.length} built, ${failed.length} failed.\n`);
  if (built.length) appendBuildLog(built, failed);
} else if (cmd === "deploy") {
  const files = findAll();
  const toDeploy = files.filter((f) => f.pdfExists);
  console.log(
    `\nDeploying ${toDeploy.length} PDF${toDeploy.length !== 1 ? "s" : ""}...\n`,
  );
  for (const entry of toDeploy) {
    deployOne(entry);
  }
  console.log("\nDone.\n");
} else if (cmd === "publish") {
  // Full pipeline: build all → deploy → update index → verify
  console.log("\n=== PUBLISH: build all → deploy → update index → verify ===\n");

  const files = findAll();
  const toBuild = files.filter((f) => f.texExists);
  console.log(`Building ${toBuild.length} papers (all languages)...\n`);
  const built = [];
  const failed = [];
  for (const entry of toBuild) {
    if (buildOne(entry)) built.push(entry);
    else failed.push(entry);
  }
  console.log(`\nBuild: ${built.length} OK, ${failed.length} failed.\n`);

  // Re-scan after build to pick up new PDFs
  const deployFiles = findAll();
  const toDeploy = deployFiles.filter((f) => f.pdfExists);
  console.log(`Deploying ${toDeploy.length} PDFs...\n`);
  for (const entry of toDeploy) {
    deployOne(entry);
  }

  // Increment revisions in metadata for all built papers (English only to avoid double-counting)
  const meta = loadMetadata();
  const builtDirs = new Set(built.map((e) => e.dir));
  for (const dir of builtDirs) {
    if (!meta[dir]) meta[dir] = { created: new Date().toISOString().slice(0, 10), revisions: 0 };
    meta[dir].revisions = (meta[dir].revisions || 0) + 1;
  }
  saveMetadata(meta);
  console.log(`  METADATA updated (${builtDirs.size} papers incremented).\n`);

  // Update index
  console.log("Updating index...\n");
  const indexEntries = findAll();
  // Refresh sitePdfExists after deploy
  for (const e of indexEntries) {
    e.sitePdfExists = existsSync(e.sitePdf);
  }
  updateIndex(indexEntries);

  if (built.length) appendBuildLog(built, failed);

  // Verify all linked PDFs exist
  console.log("\n=== VERIFY ===\n");
  verify();

  console.log("\nPublish complete.\n");
} else if (cmd === "index") {
  console.log("\nUpdating index...\n");
  const indexEntries = findAll();
  for (const e of indexEntries) {
    e.sitePdfExists = existsSync(e.sitePdf);
  }
  updateIndex(indexEntries);
  console.log("\nDone.\n");
} else if (cmd === "verify") {
  console.log("\n=== VERIFY: checking all linked PDFs ===\n");
  const allOk = verify();
  process.exit(allOk ? 0 : 1);
} else if (cmd === "log") {
  if (existsSync(BUILDLOG)) {
    console.log(readFileSync(BUILDLOG, "utf8"));
  } else {
    console.log("No build log yet. Run 'build' or 'publish' first.");
  }
} else {
  console.log(`
papers cli — build, deploy, and track all AC papers

Usage:
  node papers/cli.mjs build [lang]     Build PDFs (all langs, or: en, da, es)
  node papers/cli.mjs deploy           Copy built PDFs to site directory
  node papers/cli.mjs publish          Build all + deploy + update index + verify
  node papers/cli.mjs status           Show build status for all papers
  node papers/cli.mjs verify           Check all linked PDFs exist
  node papers/cli.mjs log              Show build log
`);
}
