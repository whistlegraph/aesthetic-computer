#!/usr/bin/env node
// papers cli — build, deploy, and track all AC papers
//
// Usage:
//   node papers/cli.mjs build [lang]     Build PDFs (all langs, or: en, da, es, zh)
//   node papers/cli.mjs build --force    Rebuild everything (skip mtime check)
//   node papers/cli.mjs deploy           Copy built PDFs to site directory
//   node papers/cli.mjs publish          Build all + deploy + update index
//   node papers/cli.mjs publish --force  Full pipeline, force-rebuilding everything
//   node papers/cli.mjs status           Show build status for all papers
//   node papers/cli.mjs log              Show build log
//
// Builds are incremental: a paper is only rebuilt when its source files (.tex,
// .bib, .sty, figures/*) are newer than the output PDF. Pass --force to bypass.
//
// Examples:
//   node papers/cli.mjs build            Build changed papers (en + da + es + zh)
//   node papers/cli.mjs build en         Build changed English PDFs only
//   node papers/cli.mjs build --force    Rebuild everything
//   node papers/cli.mjs publish          Full incremental pipeline

import { execSync } from "child_process";
import {
  existsSync,
  copyFileSync,
  mkdirSync,
  readFileSync,
  writeFileSync,
  statSync,
  readdirSync,
} from "fs";
import { join, basename } from "path";

const PAPERS_DIR = new URL(".", import.meta.url).pathname;
const SITE_DIR = join(
  PAPERS_DIR,
  "../system/public/papers.aesthetic.computer",
);
const BUILDLOG = join(PAPERS_DIR, "BUILDLOG.md");
const METADATA_PATH = join(PAPERS_DIR, "metadata.json");
const LANGS = ["en", "da", "es", "zh"];
const LANG_NAMES = { en: "English", da: "Danish", es: "Spanish", zh: "Chinese" };

function loadMetadata() {
  if (!existsSync(METADATA_PATH)) return {};
  return JSON.parse(readFileSync(METADATA_PATH, "utf8"));
}

function saveMetadata(meta) {
  writeFileSync(METADATA_PATH, JSON.stringify(meta, null, 2) + "\n", "utf8");
}

// Translation key from paper dir (matches keys in index.html inline translations)
function translationKey(dir) {
  const key = dir.replace("arxiv-", "");
  const overrides = { "kidlisp-reference": "kidlisp-ref", "sustainability": "who-pays" };
  return overrides[key] || key;
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
  "arxiv-kidlisp-cards": {
    base: "kidlisp-cards",
    siteName: "kidlisp-cards-26-arxiv",
    title: "KidLisp Cards",
  },
  "arxiv-score-analysis": {
    base: "score-analysis",
    siteName: "reading-the-score-26-arxiv",
    title: "Reading the Score",
  },
  "arxiv-calarts": {
    base: "calarts",
    siteName: "calarts-callouts-papers-26-arxiv",
    title: "CalArts, Callouts, and Papers",
    psycho: true,
  },
  "arxiv-open-schools": {
    base: "open-schools",
    siteName: "open-schools-26-arxiv",
    title: "Get Closed Source Out of Schools",
  },
  "arxiv-futures": {
    base: "futures",
    siteName: "five-years-from-now-26-arxiv",
    title: "Five Years from Now",
  },
  "arxiv-identity": {
    base: "identity",
    siteName: "handle-identity-atproto-26-arxiv",
    title: "Handle Identity on the AT Protocol",
  },
  "arxiv-ucla-arts": {
    base: "ucla-arts",
    siteName: "ucla-arts-funding-26-arxiv",
    title: "Two Departments, One Building",
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
        psycho: !!info.psycho,
        texFile,
        pdfFile,
        texExists: existsSync(texFile),
        pdfExists: existsSync(pdfFile),
        sitePdf,
        sitePdfExists: existsSync(sitePdf),
      });
    }
    // Auto-detect cards version: if {base}-cards.tex exists, add it as a build entry
    if (!langFilter || langFilter === "en") {
      const cardsTex = join(paperDir, `${info.base}-cards.tex`);
      const cardsPdf = join(paperDir, `${info.base}-cards.pdf`);
      const cardsSitePdf = join(SITE_DIR, `${info.siteName}-cards.pdf`);
      if (existsSync(cardsTex)) {
        results.push({
          dir,
          lang: "cards",
          base: info.base,
          title: info.title,
          siteName: info.siteName,
          psycho: !!info.psycho,
          texFile: cardsTex,
          pdfFile: cardsPdf,
          texExists: true,
          pdfExists: existsSync(cardsPdf),
          sitePdf: cardsSitePdf,
          sitePdfExists: existsSync(cardsSitePdf),
        });
      }
    }
  }
  return results;
}

// Shared style files at the papers/ root — changes here affect all papers.
const SHARED_STY = [
  join(PAPERS_DIR, "ac-paper-layout.sty"),
  join(PAPERS_DIR, "ac-paper-cards.sty"),
].filter(existsSync);

// Collect mtimes of all source files that could affect a paper's output.
// Returns the most recent mtime (ms), or Infinity if any file is missing.
function sourcesMtime(entry) {
  const paperDir = join(PAPERS_DIR, entry.dir);
  const sources = [];

  // The .tex file itself
  sources.push(entry.texFile);

  // All .bib and .sty files in the paper directory
  try {
    for (const f of readdirSync(paperDir)) {
      if (f.endsWith(".bib") || f.endsWith(".sty")) {
        sources.push(join(paperDir, f));
      }
    }
  } catch {}

  // Figures directory (all files)
  const figDir = join(paperDir, "figures");
  try {
    for (const f of readdirSync(figDir)) {
      sources.push(join(figDir, f));
    }
  } catch {}

  // Shared style files
  sources.push(...SHARED_STY);

  let newest = 0;
  for (const src of sources) {
    try {
      const mt = statSync(src).mtimeMs;
      if (mt > newest) newest = mt;
    } catch {
      return Infinity; // missing source → must rebuild
    }
  }
  return newest;
}

// Returns true if the paper needs rebuilding (source newer than PDF, or no PDF).
function needsRebuild(entry) {
  if (!entry.pdfExists) return true;
  try {
    const pdfMtime = statSync(entry.pdfFile).mtimeMs;
    return sourcesMtime(entry) > pdfMtime;
  } catch {
    return true;
  }
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
    // Run xelatex 3-pass with bibtex. Use semicolons (not &&) so bibtex
    // warnings don't kill the chain. Check for PDF existence, not exit code.
    execSync(
      `cd "${paperDir}" && xelatex -interaction=nonstopmode "${tex}.tex"; bibtex "${tex}" 2>/dev/null; xelatex -interaction=nonstopmode "${tex}.tex"; xelatex -interaction=nonstopmode "${tex}.tex"`,
      { stdio: "pipe", timeout: 180000 },
    );
  } catch (e) {
    // xelatex may return non-zero on warnings but still produce a PDF.
    // Only log as warning, don't fail yet.
  }
  // Check if PDF was actually produced (the real success criterion).
  const pdfPath = join(paperDir, `${tex}.pdf`);
  if (existsSync(pdfPath)) {
    console.log(`  OK    ${tex}.pdf`);
    return true;
  } else {
    console.error(`  FAIL  ${tex}.tex — no PDF produced`);
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
    "open-schools-26-arxiv": 18,
    "five-years-from-now-26-arxiv": 19,
    "calarts-callouts-papers-26-arxiv": 20,
    "handle-identity-atproto-26-arxiv": 21,
    "ucla-arts-funding-26-arxiv": 22,
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
      abstract:
        "A compact JOSS summary of Aesthetic Computer for archival and citation purposes. It distills the platform into a conventional software paper format.",
      metaKey: "joss-ac",
    },
    {
      file: "kidlisp-26-joss.pdf",
      title: "KidLisp '26",
      detail: "JOSS Summary &middot; 3pp",
      abstract:
        "A compact JOSS summary of KidLisp for archival and citation purposes. It frames the language as a small but expressive tool for generative art.",
      metaKey: "joss-kidlisp",
    },
    {
      file: "kidlisp-els-2026.pdf",
      title: "KidLisp (ELS 2026)",
      detail:
        "A Minimal Lisp for Generative Art with Social Composition &middot; ELS ACM SIGS 4pp",
      abstract:
        "An ELS conference version of KidLisp that emphasizes social composition. It positions the language as a shared practice rather than a solo scripting environment.",
      metaKey: "els-kidlisp",
    },
  ];

  // Guest papers — moved to platter readings (OCR'd text files)
  const guestPdfs = [];
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

  // Paper detail descriptions and short previews, keyed by siteName.
  const PAPER_COPY = {
    "aesthetic-computer-26-arxiv": {
      detail: "A Mobile-First Runtime for Creative Computing &middot; arXiv 5pp",
      abstract:
        "Aesthetic Computer is presented as a mobile-first creative computing runtime where the interface, publishing flow, and community feedback loop are part of the medium. The paper argues that small pieces can make software feel more social, more portable, and easier to share.",
    },
    "kidlisp-26-arxiv": {
      detail: "A Minimal Lisp for Generative Art on a Social Platform &middot; arXiv 6pp",
      abstract:
        "KidLisp is the platform's tiny Lisp for building visual and musical pieces in the browser. The paper shows how a minimal language can stay approachable while still supporting generative art and composition.",
    },
    "plorking-the-planet-26-arxiv": {
      detail: "Laptop Orchestras, PLOrk Heritage, and Aesthetic Computer &middot; arXiv",
      abstract:
        "This paper connects Aesthetic Computer to laptop orchestras and the collaborative traditions of PLOrk. It treats the browser as a place for ensemble practice, not just solo desktop programming.",
    },
    "ac-native-os-26-arxiv": {
      detail: "A Bare-Metal Creative Computing Operating System &middot; arXiv 5pp",
      abstract:
        "AC Native OS describes a bare-metal runtime for creative computing. It focuses on boot-time simplicity and the idea that the operating system itself can be a programmable art surface.",
    },
    "piece-api-26-arxiv": {
      detail: "Processing at the Core of the Piece API &middot; arXiv 7pp",
      abstract:
        "The Piece API rethinks creative software around composable pieces instead of monolithic apps. It uses Processing's lineage to connect setup(), boot(), and the act of publishing.",
    },
    "who-pays-for-creative-tools-26-arxiv": {
      detail: "Funding, Burnout, and Survival in Open-Source Creative Computing &middot; arXiv 5pp",
      abstract:
        "A short look at who supports open-source creative tools and what that labor costs. The paper connects funding, burnout, and long-term maintenance to the life of artistic software.",
    },
    "pieces-not-programs-26-arxiv": {
      detail: "The Piece as a Unit of Creative Cognition &middot; arXiv 4pp",
      abstract:
        "A piece is treated here as the basic unit of creative cognition in AC. The paper argues that smaller, shareable pieces encourage composition, remix, and publication.",
    },
    "notepat-26-arxiv": {
      detail: "From Keyboard Toy to System Front Door &middot; arXiv 5pp",
      abstract:
        "notepat.com is framed as a keyboard-first front door to the system. The paper follows the toy-like input surface as it grows into a fuller creative interface.",
    },
    "radical-computer-art-26-arxiv": {
      detail: "Goodiepalian Approaches in Aesthetic Computer &middot; arXiv 5pp",
      abstract:
        "This paper treats Goodiepalian practice as a model for radical computer art. It emphasizes play, notation, and the social life of systems over polished product design.",
    },
    "whistlegraph-26-arxiv": {
      detail: "Drawing, Singing, and the Graphic Score as Viral Form &middot; arXiv 4pp",
      abstract:
        "Whistlegraph explores drawing, singing, and score-making as forms that can spread like software. The paper links graphic notation to performance, remix, and browser-native sharing.",
    },
    "sucking-on-the-complex-26-arxiv": {
      detail: "Platform Hegemony, Critique-as-Content, and Anti-Environments &middot; arXiv 5pp",
      abstract:
        "Sucking on the Complex critiques platform hegemony and the way critique becomes content. It looks for anti-environments that stay messy, resistant, and alive.",
    },
    "dead-ends-26-arxiv": {
      detail: "Dormant Paths, Evolutionary Branches, and Abandoned Approaches &middot; arXiv 4pp",
      abstract:
        "The paper catalogs dormant branches, abandoned experiments, and paths that never became default. It treats dead ends as useful history rather than failure.",
    },
    "folk-songs-26-arxiv": {
      detail: "Oral Tradition Meets the Browser Keyboard &middot; arXiv",
      abstract:
        "Playable Folk Songs brings oral tradition into the browser keyboard. The paper asks how simple interaction can carry collective memory and repetition.",
    },
    "repo-archaeology-26-arxiv": {
      detail: 'Tracing the Evolution of AC Through Its Git History &middot; arXiv 3pp &middot; <a href="/ac-repo-archaeology">interactive timeline</a>',
      abstract:
        "Repository Archaeology traces the project through its git history. The paper shows how version control can become a narrative medium for design evolution.",
    },
    "network-audit-26-arxiv": {
      detail: "Who Uses Aesthetic Computer and What Do They Make? &middot; arXiv 4pp",
      abstract:
        "Network Audit asks who uses Aesthetic Computer and what they make with it. The paper turns usage patterns into a portrait of a community in motion.",
    },
    "kidlisp-reference-26-arxiv": {
      detail: "118 Built-ins in 12 Categories &middot; arXiv 4pp",
      abstract:
        "The KidLisp reference compresses the language into a usable field guide. It groups 118 built-ins into 12 categories for quick browsing and recall.",
    },
    "citation-diversity-audit-26": {
      detail: "Diversity and Inclusion in AC Paper Citations &middot; 4pp",
      abstract:
        "Citation Diversity Audit looks at who gets cited in the papers and where the archive is thin. The paper uses citation patterns as a proxy for inclusion and intellectual range.",
    },
    "open-schools-26-arxiv": {
      detail: "",
      abstract:
        "Get Closed Source Out of Schools makes the case that creative computing should be teachable, inspectable, and modifiable. The paper argues for open tools as infrastructure for learning.",
    },
    "five-years-from-now-26-arxiv": {
      detail: "",
      abstract:
        "Five Years from Now is a projection paper about where the project could go if current habits continue. It uses the near future to test the consequences of today's decisions.",
    },
    "calarts-callouts-papers-26-arxiv": {
      detail: "",
      abstract:
        "CalArts, Callouts, and Papers turns a local institutional context into a study of friction, attention, and production. The paper leans into psycho style to show how academic labor is staged and performed.",
    },
    "handle-identity-atproto-26-arxiv": {
      detail: "",
      abstract:
        "Handle Identity on the AT Protocol treats naming as a social and technical problem. The paper explores how handles, identity, and publishing can be tied together without losing portability.",
    },
    "ucla-arts-funding-26-arxiv": {
      detail: "",
      abstract:
        "Two Departments, One Building examines how funding and infrastructure shape creative work in shared spaces. The paper looks at administrative boundaries as part of the artistic system.",
    },
    "kidlisp-cards-26-arxiv": {
      detail: "",
      abstract:
        "KidLisp Cards condenses the language into a pocketable card format. It is meant to make the language easier to browse, teach, and carry.",
    },
    "reading-the-score-26-arxiv": {
      detail: "",
      abstract:
        "Reading the Score looks at the graphic score as an interface for interpretation and collaboration. The paper treats notation as a computational and social object.",
    },
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

  function paperCopy(key) {
    return PAPER_COPY[key] || {};
  }

  // Build paper entries HTML
  let papersHtml = "";
  for (const p of papers) {
    const copy = paperCopy(p.siteName);
    const detail = copy.detail || "";
    const abstract = copy.abstract || "";
    const hasCards = existsSync(join(SITE_DIR, `${p.siteName}-cards.pdf`));
    const createdStr = p.created ? fmtDate(p.created) : "";
    const revStr = p.revisions > 0 ? `r${p.revisions}` : "";
    const tKey = translationKey(p.dir);
    const updatedISO = p.mtime.toISOString();
    papersHtml += `
    <div class="p" data-paper-id="${tKey}"${hasCards ? "" : ` data-no-cards="1"`}${p.psycho ? ` data-psycho="1"` : ""} data-created="${p.created || ""}" data-updated="${updatedISO}">
        <div class="title"><a href="/${p.siteName}.pdf" data-base="/${p.siteName}">${p.title}</a></div>
        <div class="detail">${detail}</div>
        <div class="abstract">${abstract}</div>
        <div class="meta-row"><span class="author">@jeffrey</span>${createdStr ? `<span class="created" title="Created">${createdStr}</span>` : ""}<span class="revisions" title="Revision count">revision ${p.revisions || 1}</span><span class="updated" title="Last updated">${fmtTime(p.mtime)}</span></div>
    </div>\n`;
  }
  for (const ex of extras) {
    const createdStr = ex.created ? fmtDate(ex.created) : "";
    const revStr = ex.revisions > 0 ? `r${ex.revisions}` : "";
    const exKey = { "joss-ac": "joss-ac", "joss-kidlisp": "joss-kidlisp", "els-kidlisp": "els" }[ex.metaKey] || ex.metaKey;
    papersHtml += `
    <div class="p" data-paper-id="${exKey}">
        <div class="title"><a href="/${ex.file}">${ex.title}</a></div>
        <div class="detail">${ex.detail}</div>
        <div class="abstract">${ex.abstract}</div>
        <div class="meta-row"><span class="created" title="Created">${createdStr}</span><span class="revisions" title="Revisions">${revStr}</span><span class="updated" title="Last updated">${fmtTime(ex.mtime)}</span></div>
    </div>\n`;
  }

  // Build guest papers HTML
  let guestHtml = "";
  for (const g of guestPdfs) {
    const fp = join(SITE_DIR, g.file);
    if (existsSync(fp)) {
      guestHtml += `
    <div class="p guest" data-paper-id="${g.metaKey}" data-no-cards="1" data-created="${g.year}-01-01" data-updated="${g.year}-01-01T00:00:00.000Z">
        <div class="title"><a href="/${g.file}">${g.title}</a></div>
        <div class="detail">${g.detail}</div>
        <div class="abstract">${g.abstract}</div>
        <div class="meta-row"><span class="author">${g.author}</span><span class="created" title="Published">${g.year}</span></div>
    </div>\n`;
    }
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

  // Replace guest papers between guest markers
  const guestStart = "<!-- guest-start -->";
  const guestEnd = "<!-- guest-end -->";
  if (guestHtml && html.includes(guestStart)) {
    const gBefore = html.slice(0, html.indexOf(guestStart) + guestStart.length);
    const gAfter = html.slice(html.indexOf(guestEnd));
    html = gBefore + "\n" + guestHtml + "\n    " + gAfter;
  }

  writeFileSync(indexPath, html);
  const guestCount = guestPdfs.filter(g => existsSync(join(SITE_DIR, g.file))).length;
  console.log(`  INDEX updated with ${papers.length + extras.length} papers + ${guestCount} guest papers.`);
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
const args = process.argv.slice(2);
const force = args.includes("--force");
const positional = args.filter((a) => !a.startsWith("--"));
const [cmd, langFilter] = positional;

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
  const candidates = files.filter((f) => f.texExists);
  const toBuild = force ? candidates : candidates.filter(needsRebuild);
  const skipped = candidates.length - toBuild.length;
  const mode = force ? " (forced)" : "";
  console.log(
    `\nBuilding ${toBuild.length} paper${toBuild.length !== 1 ? "s" : ""}${filter ? ` (${LANG_NAMES[filter]})` : " (all languages)"}${mode}...`,
  );
  if (skipped > 0) console.log(`  (${skipped} up-to-date, skipped)`);
  console.log();
  const built = [];
  const failed = [];
  for (const entry of toBuild) {
    if (buildOne(entry)) built.push(entry);
    else failed.push(entry);
  }
  console.log(`\nDone: ${built.length} built, ${skipped} skipped, ${failed.length} failed.\n`);
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
  // Full pipeline: build (incremental) → deploy → update index → verify
  const mode = force ? " (forced)" : " (incremental)";
  console.log(`\n=== PUBLISH${mode}: build → deploy → update index → verify ===\n`);

  const files = findAll();
  const candidates = files.filter((f) => f.texExists);
  const toBuild = force ? candidates : candidates.filter(needsRebuild);
  const skipped = candidates.length - toBuild.length;
  console.log(`Building ${toBuild.length} papers (all languages)...`);
  if (skipped > 0) console.log(`  (${skipped} up-to-date, skipped)`);
  console.log();
  const built = [];
  const failed = [];
  for (const entry of toBuild) {
    if (buildOne(entry)) built.push(entry);
    else failed.push(entry);
  }
  console.log(`\nBuild: ${built.length} OK, ${skipped} skipped, ${failed.length} failed.\n`);

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
  node papers/cli.mjs build [lang]     Build changed PDFs (all langs, or: en, da, es, zh)
  node papers/cli.mjs build --force    Rebuild all PDFs (skip mtime check)
  node papers/cli.mjs deploy           Copy built PDFs to site directory
  node papers/cli.mjs publish          Incremental build + deploy + update index + verify
  node papers/cli.mjs publish --force  Full rebuild + deploy + update index + verify
  node papers/cli.mjs status           Show build status for all papers
  node papers/cli.mjs verify           Check all linked PDFs exist
  node papers/cli.mjs log              Show build log

Builds are incremental by default — only papers with source files newer than
their output PDF are rebuilt. Use --force to bypass.
`);
}
