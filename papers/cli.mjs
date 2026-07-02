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
const THUMBS_DIR = join(SITE_DIR, "thumbs");
const BUILDLOG = join(PAPERS_DIR, "BUILDLOG.md");
const METADATA_PATH = join(PAPERS_DIR, "metadata.json");
const LANGS = ["en", "da", "es", "zh", "ja", "ru"];
const LANG_NAMES = { en: "English", da: "Danish", es: "Spanish", zh: "Chinese", ja: "Japanese", ru: "Russian" };

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
  "arxiv-hand-and-loop": {
    base: "handloop",
    siteName: "hand-and-loop-26-arxiv",
    title: "The Hand and the Loop",
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
  "arxiv-pals": {
    base: "pals",
    siteName: "pals-mark-26-arxiv",
    title: "The Pals Mark",
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
  "arxiv-keymaps": {
    base: "keymaps",
    siteName: "keymaps-social-software-26-arxiv",
    title: "Keymaps as Social Software",
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
  "essay-may-26": {
    base: "may-26",
    siteName: "aesthetic-may-26-essay",
    title: "Aesthetic May '26",
    format: "essay",
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
  "arxiv-holden": {
    base: "holden",
    siteName: "potter-and-prompt-26-arxiv",
    title: "The Potter and the Prompt",
  },
  "arxiv-url-tradition": {
    base: "url-tradition",
    siteName: "url-tradition-26-arxiv",
    title: "The URL Tradition",
  },
  "arxiv-latency": {
    base: "latency",
    siteName: "where-the-microseconds-go-26-arxiv",
    title: "Where the Microseconds Go",
  },
  "arxiv-penrose": {
    base: "penrose",
    siteName: "diagrams-from-data-26-arxiv",
    title: "Diagrams from Data",
  },
  "arxiv-nom": {
    base: "nom",
    siteName: "nom-games-26-arxiv",
    title: "The nom Games",
  },
  "arxiv-cal": {
    base: "cal",
    siteName: "cal-aesthetical-26-arxiv",
    title: "AesthetiCal: A URL-Addressable Calendar",
  },
  "cv": {
    base: "cv",
    siteName: "jeffrey-alan-scudder-cv",
    title: "Jeffrey Alan Scudder — CV",
    hidden: true,
  },
  "arxiv-rhizome": {
    base: "rhizome",
    siteName: "rhizome-dossier-26-arxiv",
    title: "Rhizome.org — A Dossier",
  },
  "arxiv-sfpc": {
    base: "sfpc",
    siteName: "sfpc-dossier-26-arxiv",
    title: "School for Poetic Computation — A Dossier",
  },
  "arxiv-eyebeam": {
    base: "eyebeam",
    siteName: "eyebeam-dossier-26-arxiv",
    title: "Eyebeam — A Dossier",
  },
  "arxiv-recurse": {
    base: "recurse",
    siteName: "recurse-dossier-26-arxiv",
    title: "Recurse Center — A Dossier",
  },
  "arxiv-internet-archive": {
    base: "internet-archive",
    siteName: "internet-archive-dossier-26-arxiv",
    title: "Internet Archive — A Dossier",
  },
  "arxiv-mellon": {
    base: "mellon",
    siteName: "mellon-dossier-26-arxiv",
    title: "Mellon Foundation — A Dossier",
  },
  "arxiv-pioneer-works": {
    base: "pioneer-works",
    siteName: "pioneer-works-dossier-26-arxiv",
    title: "Pioneer Works — A Dossier",
  },
  "arxiv-fraserin": {
    base: "fraserin",
    siteName: "fraserin-essay-26-arxiv",
    title: "A Fraserin' Art + Tech",
  },
  "arxiv-comp-strats": {
    base: "comp-strats",
    siteName: "comp-strats-26-arxiv",
    title: "Comp Strats",
  },
  "arxiv-microvision": {
    base: "microvision",
    siteName: "microvision-dossier-26-arxiv",
    title: "MicroVision — A Dossier",
  },
  "arxiv-calarts-news": {
    base: "calarts-news",
    siteName: "whats-new-calarts-26-arxiv",
    title: "What's New CalArts!? — A Dossier",
  },
  "arxiv-new-inc": {
    base: "new-inc",
    siteName: "new-inc-dossier-26-arxiv",
    title: "NEW INC — A Dossier",
  },
  "arxiv-studio-museum": {
    base: "studio-museum",
    siteName: "studio-museum-dossier-26-arxiv",
    title: "Studio Museum in Harlem — A Dossier",
  },
  "arxiv-hathitrust": {
    base: "hathitrust",
    siteName: "hathitrust-dossier-26-arxiv",
    title: "HathiTrust — A Dossier",
  },
  "arxiv-the-kitchen": {
    base: "the-kitchen",
    siteName: "the-kitchen-dossier-26-arxiv",
    title: "The Kitchen — A Dossier",
  },
  "arxiv-machine-project": {
    base: "machine-project",
    siteName: "machine-project-dossier-26-arxiv",
    title: "Machine Project — A Dossier",
  },
  "arxiv-heavy-manners-library": {
    base: "heavy-manners-library",
    siteName: "heavy-manners-library-dossier-26-arxiv",
    title: "Heavy Manners Library — A Dossier",
  },
  "arxiv-creative-time": {
    base: "creative-time",
    siteName: "creative-time-dossier-26-arxiv",
    title: "Creative Time — A Dossier",
  },
  "arxiv-creative-capital": {
    base: "creative-capital",
    siteName: "creative-capital-dossier-26-arxiv",
    title: "Creative Capital — A Dossier",
  },
};

// Dossier swimlane — rendered in their own "dossiers;" section on the
// index, separate from the argumentative papers. Order here is the
// display order within that section.
const DOSSIER_DIRS = [
  "arxiv-rhizome",
  "arxiv-sfpc",
  "arxiv-eyebeam",
  "arxiv-recurse",
  "arxiv-internet-archive",
  "arxiv-mellon",
  "arxiv-pioneer-works",
  "arxiv-new-inc",
  "arxiv-studio-museum",
  "arxiv-hathitrust",
  "arxiv-the-kitchen",
  "arxiv-machine-project",
  "arxiv-heavy-manners-library",
  "arxiv-creative-time",
  "arxiv-creative-capital",
  "arxiv-microvision",
  "arxiv-calarts-news",
];
const dossierRank = (dir) => {
  const i = DOSSIER_DIRS.indexOf(dir);
  return i === -1 ? 99 : i;
};

// Top-level index categories. Every listed (non-hidden, non-archived)
// paper resolves to exactly one. Order here is the page order. The
// "software" lane is populated from the `extras` array (JOSS/ELS), not
// from PAPER_MAP, so its `dirs` stays empty. Lists flagged `nosort` keep
// their curated DOM order and are skipped by the client-side sort bar.
const CATEGORIES = [
  {
    key: "platform",
    title: "platform &amp; language",
    sub: "the runtime, the language, and the surfaces you actually touch.",
    dirs: [
      "arxiv-ac",
      "arxiv-kidlisp",
      "arxiv-os",
      "arxiv-api",
      "arxiv-pieces",
      "arxiv-notepat",
      "arxiv-cal",
      "arxiv-kidlisp-reference",
      "arxiv-kidlisp-cards",
      "arxiv-keymaps",
      "arxiv-url-tradition",
      "arxiv-latency",
      "arxiv-penrose",
      "arxiv-identity",
    ],
  },
  {
    key: "essays",
    title: "essays &amp; criticism",
    sub: "arguments about creative computing — its lineage, its players, and its discontents.",
    dirs: [
      "arxiv-plork",
      "arxiv-sustainability",
      "arxiv-goodiepal",
      "arxiv-whistlegraph",
      "arxiv-complex",
      "arxiv-dead-ends",
      "arxiv-folk-songs",
      "arxiv-futures",
      "arxiv-holden",
      "arxiv-fraserin",
      "arxiv-score-analysis",
      "arxiv-comp-strats",
      "arxiv-nom",
      "essay-may-26",
    ],
  },
  {
    key: "audits",
    title: "audits &amp; field studies",
    sub: "data turned back on the project itself — who uses it, what it cites, where it came from.",
    dirs: [
      "arxiv-archaeology",
      "arxiv-hand-and-loop",
      "arxiv-network-audit",
      "arxiv-diversity",
      "arxiv-open-schools",
    ],
  },
  {
    key: "dossiers",
    title: "dossiers",
    sub: "what's publicly recoverable about art-and-tech institutions and their largest funders — fact-surfacing, not argument.",
    nosort: true,
    dirs: [...DOSSIER_DIRS, "arxiv-ucla-arts"],
  },
  {
    key: "software",
    title: "software papers",
    sub: "conventional software-paper summaries, for archival and citation.",
    dirs: [], // populated from the `extras` array (JOSS / ELS)
  },
];

// Papers retired to the quiet Archive section at the very bottom of the
// page, regardless of their category. Keyed by `dir`.
const ARCHIVE_DIRS = new Set(["arxiv-calarts"]);

// dir -> category key (dossiers included; archived + software handled separately)
const CATEGORY_OF = {};
for (const c of CATEGORIES) for (const d of c.dirs) CATEGORY_OF[d] = c.key;
const categoryOf = (dir) => CATEGORY_OF[dir] || "essays";

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
        hidden: !!info.hidden,
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

// Sniff the xelatex .log next to a built PDF for the silent-failure signature:
// fontspec couldn't load a requested font, so the run fell back to `nullfont`
// and still emitted a small stub PDF (xelatex exits 0). Same shape as
// the cards-convert bug — a stub passes the mtime check forever.
function logShowsBrokenBuild(pdfFile) {
  const logFile = pdfFile.replace(/\.pdf$/, ".log");
  if (!existsSync(logFile)) return false;
  try {
    const log = readFileSync(logFile, "utf8");
    return log.includes("! Package fontspec Error") || log.includes("nullfont");
  } catch {
    return false;
  }
}

// Returns true if the paper needs rebuilding (source newer than PDF, or no PDF).
function needsRebuild(entry) {
  if (!entry.pdfExists) return true;
  try {
    const pdfMtime = statSync(entry.pdfFile).mtimeMs;
    if (sourcesMtime(entry) > pdfMtime) return true;
    if (logShowsBrokenBuild(entry.pdfFile)) return true;
    return false;
  } catch {
    return true;
  }
}

// Stamp figures/version.tex (and a bare version.tex) from git + metadata so
// a paper that does `\input{version}` always carries the commit it was cut
// from. No-op unless the paper's .tex actually inputs version. The hash gets
// a "+" suffix when the working tree is dirty, matching `git describe --dirty`
// convention — a clean release build (e.g. the oven) stamps a bare hash.
function stampVersion(entry, paperDir) {
  const tex = texName(entry.base, entry.lang);
  const texPath = join(paperDir, `${tex}.tex`);
  let src = "";
  try {
    src = readFileSync(texPath, "utf8");
  } catch {
    return;
  }
  if (!src.includes("{version}")) return; // paper opts in via \input/\InputIfFileExists{version}

  let hash = "unknown";
  let dirty = "";
  try {
    hash = execSync("git rev-parse --short HEAD", { cwd: paperDir, stdio: ["ignore", "pipe", "ignore"] })
      .toString()
      .trim();
    // Dirty flag reflects THIS paper's own directory only (gitignored
    // version.tex/history.tex/*.pdf excluded), not unrelated repo changes,
    // so a committed paper stamps a clean edition hash.
    const porcelain = execSync('git status --porcelain -- .', { cwd: paperDir, stdio: ["ignore", "pipe", "ignore"] })
      .toString()
      .trim();
    if (porcelain) dirty = "+";
  } catch {
    /* not a git checkout — leave hash as "unknown" */
  }

  const meta = loadMetadata();
  const rev = meta?.[entry.dir]?.revisions ?? "?";

  const body =
    `% auto-generated by papers/cli.mjs at build time — do not edit\n` +
    `\\newcommand{\\paperhash}{${hash}${dirty}}\n` +
    `\\newcommand{\\paperrev}{${rev}}\n`;
  try {
    writeFileSync(join(paperDir, "version.tex"), body);
  } catch {
    /* read-only tree — leave any committed version.tex in place */
  }

  // history.tex: a \paperhistory macro built from the git log of the
  // paper's own .tex (date + commit subject), for an in-PDF changelog.
  // Only written if the paper inputs {history}.
  if (src.includes("{history}")) {
    let hist = "";
    try {
      const raw = execSync(
        `git log --format=%ad%x09%s --date=short -- "${tex}.tex"`,
        { cwd: paperDir, stdio: ["ignore", "pipe", "ignore"] },
      ).toString().trim();
      const esc = (s) =>
        s.replace(/\\/g, "").replace(/([&_#%$])/g, "\\$1").replace(/—/g, "---");
      // Strip the redundant repeated "<dir>:" / "papers:" commit prefix.
      const stripPrefix = (m) =>
        m.replace(/^(papers\/[\w-]+|arxiv-[\w-]+|papers|[\w-]+):\s*/i, "");
      const rows = raw.split("\n").filter(Boolean).map((line) => {
        const tab = line.indexOf("\t");
        const date = esc(line.slice(0, tab));
        let msg = esc(stripPrefix(line.slice(tab + 1)));
        if (msg.length > 58) msg = msg.slice(0, 56) + "\\,\\ldots";
        return `{\\color{acgray}${date}}~~${msg}\\\\`;
      });
      // One entry per line, left-aligned.
      hist = rows.join("\n");
    } catch {
      /* no git log — empty history */
    }
    const histBody =
      `% auto-generated by papers/cli.mjs at build time — do not edit\n` +
      `\\newcommand{\\paperhistory}{%\n${hist}\n}\n`;
    try {
      writeFileSync(join(paperDir, "history.tex"), histBody);
    } catch {
      /* read-only tree */
    }
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

  // Stamp the version macros from git before xelatex runs.
  stampVersion(entry, paperDir);

  // Deterministic PDFs: derive SOURCE_DATE_EPOCH from the newest source mtime.
  // xelatex + xdvipdfmx honor this for /CreationDate, /ModDate, and the /ID
  // trailer, so identical input bytes produce identical output bytes — which
  // stops the oven auto-build commit loop on the 2 PDFs whose only diff
  // between runs was wall-clock metadata baked into the trailer.
  const newestMs = sourcesMtime(entry);
  const sourceDateEpoch = Number.isFinite(newestMs)
    ? Math.floor(newestMs / 1000).toString()
    : Math.floor(Date.now() / 1000).toString();

  try {
    // Run xelatex 3-pass with bibtex. Use semicolons (not &&) so bibtex
    // warnings don't kill the chain. Check for PDF existence, not exit code.
    execSync(
      `cd "${paperDir}" && xelatex -interaction=nonstopmode "${tex}.tex"; bibtex "${tex}" 2>/dev/null; xelatex -interaction=nonstopmode "${tex}.tex"; xelatex -interaction=nonstopmode "${tex}.tex"`,
      {
        stdio: "pipe",
        timeout: 180000,
        env: {
          ...process.env,
          SOURCE_DATE_EPOCH: sourceDateEpoch,
          FORCE_SOURCE_DATE: "1",
        },
      },
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

// Render page 1 of a deployed PDF as a JPEG thumbnail. Lazy: skip if the
// thumb is newer than the PDF. Returns true if a thumb exists after the call.
function genThumbnail(pdfPath, siteName) {
  if (!existsSync(pdfPath)) return false;
  mkdirSync(THUMBS_DIR, { recursive: true });
  const out = join(THUMBS_DIR, `${siteName}.jpg`);
  if (existsSync(out)) {
    const pdfM = statSync(pdfPath).mtimeMs;
    const thumbM = statSync(out).mtimeMs;
    if (thumbM >= pdfM) return true;
  }
  const tmpPrefix = join(THUMBS_DIR, `${siteName}.tmp`);
  try {
    // Cards are physically small (4x6in), so render them at a higher DPI to
    // keep the cover crisp; full pages stay at a lighter resolution.
    const dpi = /-cards\.pdf$/.test(pdfPath) ? 110 : 72;
    execSync(
      `pdftoppm -jpeg -jpegopt quality=75,progressive=y -r ${dpi} -f 1 -l 1 "${pdfPath}" "${tmpPrefix}"`,
      { stdio: "pipe", timeout: 30000 },
    );
    // pdftoppm appends -1 (or -01) for single page output
    const candidates = [`${tmpPrefix}-1.jpg`, `${tmpPrefix}-01.jpg`];
    const produced = candidates.find((p) => existsSync(p));
    if (!produced) {
      console.warn(`  THUMB miss ${siteName} (no pdftoppm output)`);
      return false;
    }
    if (existsSync(out)) execSync(`rm "${out}"`);
    execSync(`mv "${produced}" "${out}"`);
    return true;
  } catch (e) {
    console.warn(`  THUMB fail ${siteName}: ${String(e.message).slice(0, 120)}`);
    return false;
  }
}

// Square illustration tile straight from the paper's cover art — the index
// is a cover-and-title gallery, so the tile is the standalone emblem/illy,
// not the full first page. Lazy: skip if the tile is newer than the source.
function genCoverTile(coverPath, siteName) {
  if (!existsSync(coverPath)) return false;
  mkdirSync(THUMBS_DIR, { recursive: true });
  const out = join(THUMBS_DIR, `${siteName}.jpg`);
  if (existsSync(out) && statSync(out).mtimeMs >= statSync(coverPath).mtimeMs) {
    return true;
  }
  try {
    execSync(
      `magick "${coverPath}" -resize 520x520^ -gravity center -extent 520x520 -quality 84 "${out}"`,
      { stdio: "pipe", timeout: 30000 },
    );
    return true;
  } catch (e) {
    console.warn(`  TILE fail ${siteName}: ${String(e.message).slice(0, 120)}`);
    return false;
  }
}

// siteName → its paper's cover.png, from PAPER_MAP.
function coverTileSources() {
  const map = {};
  for (const [dir, info] of Object.entries(PAPER_MAP)) {
    const cover = join(PAPERS_DIR, dir, "figures", "cover.png");
    if (existsSync(cover)) map[info.siteName] = cover;
  }
  return map;
}

// Generate thumbs for every PDF in SITE_DIR (covers PAPER_MAP papers, JOSS
// extras, ELS, guest PDFs — anything published). Prefer the cover-art tile;
// fall back to the PDF first page for anything without a cover.png (JOSS, etc.).
function genAllThumbnails() {
  if (!existsSync(SITE_DIR)) return 0;
  let count = 0;
  const tiles = coverTileSources();
  const pdfs = readdirSync(SITE_DIR).filter((n) => n.endsWith(".pdf"));
  for (const name of pdfs) {
    const siteName = name.slice(0, -4);
    if (siteName.endsWith("-cards")) continue;
    const ok = tiles[siteName]
      ? genCoverTile(tiles[siteName], siteName)
      : genThumbnail(join(SITE_DIR, name), siteName);
    if (ok) count++;
  }
  return count;
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
    "keymaps-social-software-26-arxiv": 0.5,
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
    "hand-and-loop-26-arxiv": 14.5,
    "network-audit-26-arxiv": 15,
    "kidlisp-reference-26-arxiv": 16,
    "citation-diversity-audit-26": 17,
    "open-schools-26-arxiv": 18,
    "five-years-from-now-26-arxiv": 19,
    "aesthetic-may-26-essay": 19.5,
    "calarts-callouts-papers-26-arxiv": 20,
    "handle-identity-atproto-26-arxiv": 21,
    "ucla-arts-funding-26-arxiv": 22,
    "potter-and-prompt-26-arxiv": 23,
  };

  // Collect deployed English PDFs sorted by importance
  const papers = [];
  for (const e of entries.filter((e) => e.lang === "en" && e.sitePdfExists)) {
    const stat = statSync(e.sitePdf);
    const m = meta[e.dir] || {};
    const rank = IMPORTANCE[e.siteName] || 99;
    // Prefer stored updated timestamp over file mtime (deploy copies all PDFs, clobbering mtime)
    const updated = m.updated ? new Date(m.updated) : stat.mtime;
    papers.push({ ...e, mtime: updated, created: m.created || null, revisions: m.revisions || 0, rank });
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

  // Guest papers were moved to platter readings (OCR'd text files); the
  // lane is retired, so only the JOSS/ELS extras remain.
  const extras = [];
  for (const ex of extraPdfs) {
    const fp = join(SITE_DIR, ex.file);
    if (existsSync(fp)) {
      const stat = statSync(fp);
      const m = meta[ex.metaKey] || {};
      const updated = m.updated ? new Date(m.updated) : stat.mtime;
      extras.push({ ...ex, mtime: updated, created: m.created || null, revisions: m.revisions || 0 });
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
    "cal-aesthetical-26-arxiv": {
      detail: "A URL-Addressable Calendar for Aesthetic Computer &middot; arXiv 6pp",
      abstract:
        "AesthetiCal adds time to Aesthetic Computer as a piece, not a second app: month/week/day views and a DateWizard share a per-handle synced store, addressable by permahandle and interchangeable as standard iCalendar. The paper describes the shipped v1 and sketches the next phase — bookable shows, invites, and the Aestheticks ticketing tie-in.",
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
    "hand-and-loop-26-arxiv": {
      detail: "Three Eras of My Own Code: Hand-Written, GPT-Grafted, Agentic &middot; arXiv 4pp",
      abstract:
        "The Hand and the Loop reads AC's git history across three eras — hand-written, GPT-grafted, and fully agentic — and asks honestly whether the codebase is better off. It weighs 5x velocity and five new subsystems against the loss of instrument-like minimalism, and argues for keeping the loop in the leaves while a human hand carves the foundational libraries.",
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
    "nom-games-26-arxiv": {
      detail: "Muncher Arcade for Aesthetic Computer &middot; arXiv",
      abstract:
        "The nom games rebuild MECC's Word and Number Munchers as Aesthetic Computer pieces --- numbnom, engnom, mexinom, and notenom. The paper shows how one shared engine and a reusable virtual synth controller turn each new subject or language into a content table rather than a new game, with the answer key in notenom doubling as a playable instrument.",
    },
    "aesthetic-may-26-essay": {
      detail: "Essay &middot; ~5pp",
      abstract:
        "Aesthetic May '26 is a magazine-style essay companion to Five Years from Now. May is both the month and the modal verb of possibility; the essay reads the present as a field of branching maybes rather than a line of probabilities --- where the music lane may go, what jeffrey may do, what may not happen this year, and the weather acting on all of it. First entry in the essay-* lane.",
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
    "potter-and-prompt-26-arxiv": {
      detail: "John Holden's Proto-Cognitive Music Theory and Aesthetic Computer &middot; arXiv 7pp",
      abstract:
        "The Potter and the Prompt argues that AC independently converges on the core principles of John Holden's 1770 proto-cognitive music theory. It proposes AC as a computational laboratory for advancing Holden's unfinished program on grouping, attention, and the module.",
    },
    "url-tradition-26-arxiv": {
      detail: "Addressable Creative Computing from Lovelace's Footnotes to Aesthetic Computer &middot; arXiv",
      abstract:
        "The URL Tradition traces address-thinking from Lovelace's footnotes, Bush's trails, Xanadu, and the Negro Motorist Green Book through net.art, single-serving sites, the tilde, Glitch, and Aesthetic Computer's prompt-as-address-bar. It argues the URL is not a feature but a medium property that reshapes authorship, distribution, pedagogy, performance, and political claim.",
    },

    // --- Dossiers (fact-surfacing lane; rendered in the dossiers section) ---
    "rhizome-dossier-26-arxiv": {
      detail: "501(c)(3) digital-arts recipient &middot; IRS 990 pipeline &middot; arXiv",
      abstract:
        "What is publicly recoverable about Rhizome.org: governance, finances, and named grants reconstructed from IRS 990 filings and masthead snapshots. The dossier records the documentary record and stops where the facts run out.",
    },
    "sfpc-dossier-26-arxiv": {
      detail: "School for Poetic Computation &middot; LLC, public finance repo &middot; arXiv",
      abstract:
        "What is publicly recoverable about the School for Poetic Computation: an LLC that publishes its own finances to a public GitHub repository, read alongside programs and people. Fact-surfacing, not argument.",
    },
    "eyebeam-dossier-26-arxiv": {
      detail: "501(c)(3) art-and-technology recipient &middot; IRS 990 pipeline &middot; arXiv",
      abstract:
        "What is publicly recoverable about Eyebeam: financials, governance, residencies, and named grants pulled from IRS 990 XML. The dossier surfaces the record without interpreting it.",
    },
    "recurse-dossier-26-arxiv": {
      detail: "Recurse Center &middot; for-profit, recruiting-funded model &middot; arXiv",
      abstract:
        "What is publicly recoverable about the Recurse Center: a for-profit programmers' retreat funded by a recruiting model, read through founder interviews and public statements. Fact-surfacing, not argument.",
    },
    "internet-archive-dossier-26-arxiv": {
      detail: "501(c)(3) recipient &middot; IRS 990 + litigation track &middot; arXiv",
      abstract:
        "What is publicly recoverable about the Internet Archive: finances and governance from IRS 990 filings, plus a litigation track from public court records. The dossier records both and stops at the facts.",
    },
    "mellon-dossier-26-arxiv": {
      detail: "The funder flip &middot; 990-PF + grants database &middot; arXiv",
      abstract:
        "The funder side: what is publicly recoverable about the Mellon Foundation from its 990-PF and grants database — the Form 990-PF that paid for nontrivial chunks of the other dossiers. Fact-surfacing, not argument.",
    },
    "pioneer-works-dossier-26-arxiv": {
      detail: "501(c)(3) recipient &middot; founder-as-funder &middot; arXiv",
      abstract:
        "What is publicly recoverable about Pioneer Works: finances, governance, and a founder-as-funder structure reconstructed from IRS 990 filings and public record. The dossier surfaces the record without interpreting it.",
    },
    "new-inc-dossier-26-arxiv": {
      detail: "Embedded in the New Museum's 990 &middot; arXiv",
      abstract:
        "What is publicly recoverable about NEW INC, the New Museum's incubator: finances and governance read out of the parent museum's IRS 990, where the program is embedded rather than separately filed. Fact-surfacing, not argument.",
    },
    "studio-museum-dossier-26-arxiv": {
      detail: "501(c)(3) recipient &middot; capital campaign &middot; arXiv",
      abstract:
        "What is publicly recoverable about the Studio Museum in Harlem: finances, governance, and a capital campaign reconstructed from IRS 990 filings and public documents. The dossier records the documentary record.",
    },
    "hathitrust-dossier-26-arxiv": {
      detail: "UMich library service &middot; no separate 990 &middot; arXiv",
      abstract:
        "What is publicly recoverable about HathiTrust: a University of Michigan library service with no separate IRS 990, surfaced instead through host-institution disclosures and public reports. Fact-surfacing, not argument.",
    },
    "the-kitchen-dossier-26-arxiv": {
      detail: "501(c)(3) recipient &middot; a 50-plus-year arc &middot; arXiv",
      abstract:
        "What is publicly recoverable about The Kitchen: a half-century of finances and governance reconstructed from IRS 990 filings and public record across the organization's long arc. The dossier stops where the facts run out.",
    },
    "machine-project-dossier-26-arxiv": {
      detail: "501(c)(3) recipient (until 2018) &middot; Echo Park &middot; arXiv",
      abstract:
        "What is publicly recoverable about Machine Project: the Echo Park space's finances and governance from IRS 990 filings through its 2018 wind-down. Fact-surfacing, not argument.",
    },
    "heavy-manners-library-dossier-26-arxiv": {
      detail: "Small space &middot; status undisclosed &middot; arXiv",
      abstract:
        "What is publicly recoverable about Heavy Manners Library: a small space whose legal status is undisclosed, surfaced through what little public record exists. The dossier records the gaps as part of the record.",
    },
    "microvision-dossier-26-arxiv": {
      detail: "The public-company flip &middot; SEC EDGAR + 11-year archive &middot; arXiv",
      abstract:
        "The public-company side: what is publicly recoverable about MicroVision (NASDAQ: MVIS) from SEC EDGAR and an eleven-year family archive of filings. Fact-surfacing, not argument.",
    },
    "whats-new-calarts-26-arxiv": {
      detail: "CalArts &middot; a dossier in news format &middot; arXiv",
      abstract:
        "What's New CalArts!? surfaces what is publicly recoverable about CalArts in a news-format dossier: programs, finances, and governance from public documents. The dossier records the record and stops at the facts.",
    },
    "creative-time-dossier-26-arxiv": {
      detail: "501(c)(3) public-art commissioner &middot; IRS 990 pipeline &middot; arXiv",
      abstract:
        "What is publicly recoverable about Creative Time, the New York public-art nonprofit (1974–): structure, programs, people, and money from the IRS 990 series and its program archive. Scaffold revision — figures pending the data pass; fact-surfacing, not argument.",
    },
    "creative-capital-dossier-26-arxiv": {
      detail: "501(c)(3) artist-grant funder &middot; 990 + grantee record &middot; arXiv",
      abstract:
        "What is publicly recoverable about Creative Capital, the New York nonprofit (1999–) that regrants to individual artists: structure, the award, people, and money from the IRS 990 series and the public grantee record. Scaffold revision — figures pending; fact-surfacing, not argument.",
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

  // Render one paper card (shared by the papers list and dossiers section)
  function renderPaper(p) {
    const copy = paperCopy(p.siteName);
    const detail = copy.detail || "";
    const abstract = copy.abstract || "";
    const hasCards = existsSync(join(SITE_DIR, `${p.siteName}-cards.pdf`));
    const createdStr = p.created ? fmtDate(p.created) : "";
    const tKey = translationKey(p.dir);
    const updatedISO = p.mtime.toISOString();
    const thumbName = `${p.siteName}.jpg`;
    const thumbExists = existsSync(join(THUMBS_DIR, thumbName));
    const thumbHtml = thumbExists
      ? `<a class="thumb" href="/${p.siteName}.pdf" tabindex="-1" aria-hidden="true"><img src="/thumbs/${thumbName}" alt="" loading="lazy" decoding="async"></a>`
      : "";
    // Links under the illustration: cards format + any translated editions.
    const cardsTl = hasCards
      ? `<a class="tl tl-cards" href="/${p.siteName}-cards.pdf" title="Mobile-friendly cards format">cards</a>`
      : "";
    const langTl = LANGS.filter((l) => l !== "en")
      .filter((l) => existsSync(join(SITE_DIR, `${p.siteName}-${l}.pdf`)))
      .map((l) => `<a class="tl tl-lang" href="/${p.siteName}-${l}.pdf" title="${LANG_NAMES[l]}">${l.toUpperCase()}</a>`)
      .join("");
    const thumbLinks = cardsTl || langTl ? `<div class="thumb-links">${cardsTl}${langTl}</div>` : "";
    const thumbCol = thumbHtml || thumbLinks
      ? `<div class="thumb-col">${thumbHtml}${thumbLinks}</div>`
      : "";
    return `
    <div class="p" data-paper-id="${tKey}"${hasCards ? "" : ` data-no-cards="1"`}${p.psycho ? ` data-psycho="1"` : ""}${p.deprecated ? ` data-deprecated="1"` : ""} data-created="${p.created || ""}" data-updated="${updatedISO}">
        ${thumbCol}<div class="body">
        <div class="title"><a href="/${p.siteName}.pdf" data-base="/${p.siteName}">${p.title}</a></div>
        <div class="detail">${detail}</div>
        <div class="abstract">${abstract}</div>
        <div class="meta-row"><span class="author">@jeffrey</span>${createdStr ? `<span class="created" title="Created">${createdStr}</span>` : ""}<span class="revisions" title="Revision count">revision ${p.revisions || 1}</span><span class="updated" title="Last updated">${fmtTime(p.mtime)}</span></div>
        </div>
    </div>\n`;
  }

  // Render one JOSS/ELS "software paper" card from the extras array.
  function renderExtra(ex) {
    const createdStr = ex.created ? fmtDate(ex.created) : "";
    const revStr = ex.revisions > 0 ? `r${ex.revisions}` : "";
    const exKey = { "joss-ac": "joss-ac", "joss-kidlisp": "joss-kidlisp", "els-kidlisp": "els" }[ex.metaKey] || ex.metaKey;
    const exSiteName = ex.file.replace(/\.pdf$/, "");
    const exThumbExists = existsSync(join(THUMBS_DIR, `${exSiteName}.jpg`));
    const exThumbHtml = exThumbExists
      ? `<a class="thumb" href="/${ex.file}" tabindex="-1" aria-hidden="true"><img src="/thumbs/${exSiteName}.jpg" alt="" loading="lazy" decoding="async"></a>`
      : "";
    return `
    <div class="p" data-paper-id="${exKey}">
        ${exThumbHtml}<div class="body">
        <div class="title"><a href="/${ex.file}">${ex.title}</a></div>
        <div class="detail">${ex.detail}</div>
        <div class="abstract">${ex.abstract}</div>
        <div class="meta-row"><span class="created" title="Created">${createdStr}</span><span class="revisions" title="Revisions">${revStr}</span><span class="updated" title="Last updated">${fmtTime(ex.mtime)}</span></div>
        </div>
    </div>\n`;
  }

  // Wrap a category's cards in a labeled <section>. `count` is the number
  // of cards; `nosort`/extra modifiers control the client-side sort.
  function renderSection({ key, title, sub, nosort }, cardsHtml, count, archive = false) {
    if (!cardsHtml) return "";
    const cls = `cat${archive ? " cat--archive" : ""}`;
    const nosortAttr = nosort || archive ? " data-nosort" : "";
    return `
    <section class="${cls}" data-cat="${key}">
        <div class="cat-head" role="button" tabindex="0" aria-expanded="true"><span class="cat-chevron" aria-hidden="true">▾</span><span class="cat-name">${title}</span><span class="cat-semi">;</span><span class="cat-count">${count}</span></div>
        <div class="cat-sub">${sub}</div>
        <div class="cat-list"${nosortAttr}>
${cardsHtml}        </div>
    </section>\n`;
  }

  // Bucket every listed paper into exactly one category; archived papers
  // are pulled out to their own quiet section at the bottom.
  const buckets = {};
  for (const c of CATEGORIES) buckets[c.key] = [];
  const archived = [];
  for (const p of papers) {
    if (p.hidden) continue; // built + tracked, but not listed publicly
    if (ARCHIVE_DIRS.has(p.dir)) {
      archived.push({ ...p, deprecated: true, psycho: false });
      continue;
    }
    buckets[categoryOf(p.dir)].push(p);
  }
  // Dossiers keep their curated order; other lanes keep importance rank.
  buckets.dossiers.sort((a, b) => dossierRank(a.dir) - dossierRank(b.dir));

  const uncategorized = papers.filter(
    (p) => !p.hidden && !ARCHIVE_DIRS.has(p.dir) && !CATEGORY_OF[p.dir],
  );
  if (uncategorized.length) {
    console.log(
      `  WARN ${uncategorized.length} uncategorized paper(s) → essays: ${uncategorized.map((p) => p.dir).join(", ")}`,
    );
  }

  // Build the full categorized block (all sections, in page order).
  let papersHtml = "";
  for (const c of CATEGORIES) {
    if (c.key === "software") {
      const cards = extras.map(renderExtra).join("");
      papersHtml += renderSection(c, cards, extras.length);
    } else {
      const list = buckets[c.key];
      papersHtml += renderSection(c, list.map(renderPaper).join(""), list.length);
    }
  }
  if (archived.length) {
    papersHtml += renderSection(
      {
        key: "archive",
        title: "archive",
        sub: "deprecated — kept for the record, no longer actively maintained.",
      },
      archived.map(renderPaper).join(""),
      archived.length,
      true,
    );
  }

  // Read current index, replace the categorized block between markers.
  let html = readFileSync(indexPath, "utf8");
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
  const sectionCounts = CATEGORIES.map((c) =>
    c.key === "software" ? `${c.key} ${extras.length}` : `${c.key} ${buckets[c.key].length}`,
  ).join(", ");
  console.log(
    `  INDEX updated: ${papers.length + extras.length} papers across ${CATEGORIES.length} categories (${sectionCounts}${archived.length ? `, archive ${archived.length}` : ""}).`,
  );

  writeFeed(papers, extras, PAPER_COPY);
}

// Escape a string for inclusion in an XML text node or attribute.
function xmlEscape(s) {
  return String(s ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}

// Convert HTML-flavored fragments (entities, anchor tags) to plain text,
// so the string can be safely XML-escaped into a feed summary.
function htmlToText(s) {
  return String(s ?? "")
    .replace(/<a\b[^>]*>([\s\S]*?)<\/a>/gi, "$1")
    .replace(/<[^>]+>/g, "")
    .replace(/&middot;/g, "·")
    .replace(/&times;/g, "×")
    .replace(/&mdash;/g, "—")
    .replace(/&ndash;/g, "–")
    .replace(/&hellip;/g, "…")
    .replace(/&nbsp;/g, " ")
    .replace(/&amp;/g, "&")
    .replace(/&quot;/g, '"')
    .replace(/&apos;/g, "'")
    .replace(/&#(\d+);/g, (_, n) => String.fromCodePoint(parseInt(n, 10)))
    .replace(/&#x([0-9a-f]+);/gi, (_, n) => String.fromCodePoint(parseInt(n, 16)))
    .replace(/\s+/g, " ")
    .trim();
}

// Build an Atom feed of all papers and write feed.xml + rss.xml to SITE_DIR.
// Papers are sorted newest first by mtime. Both files share the same Atom
// content so that /feed.xml and /rss.xml both resolve to a valid feed.
function writeFeed(papers, extras, PAPER_COPY) {
  const FEED_TITLE = "papers · Aesthetic Computer";
  const FEED_SUBTITLE =
    "Academic papers on Aesthetic Computer, KidLisp, and creative computing.";
  const SITE_URL = "https://papers.aesthetic.computer";
  const FEED_URL = `${SITE_URL}/feed.xml`;
  const AUTHOR_NAME = "@jeffrey";
  const AUTHOR_URI = "https://prompt.ac/@jeffrey";

  const entries = [];
  for (const p of papers) {
    const copy = (PAPER_COPY || {})[p.siteName] || {};
    entries.push({
      id: `${SITE_URL}/${p.siteName}.pdf`,
      url: `${SITE_URL}/${p.siteName}.pdf`,
      title: p.title,
      detail: copy.detail || "",
      summary: copy.abstract || "",
      updated: p.mtime instanceof Date ? p.mtime : new Date(p.mtime),
      published: p.created ? new Date(`${p.created}T00:00:00Z`) : null,
      category: "arXiv",
    });
  }
  for (const ex of extras) {
    entries.push({
      id: `${SITE_URL}/${ex.file}`,
      url: `${SITE_URL}/${ex.file}`,
      title: ex.title,
      detail: ex.detail || "",
      summary: ex.abstract || "",
      updated: ex.mtime instanceof Date ? ex.mtime : new Date(ex.mtime),
      published: ex.created ? new Date(`${ex.created}T00:00:00Z`) : null,
      category: ex.metaKey && ex.metaKey.startsWith("joss") ? "JOSS" : "ELS",
    });
  }
  entries.sort((a, b) => b.updated - a.updated);

  const feedUpdated =
    entries.length > 0 ? entries[0].updated : new Date();

  let xml = `<?xml version="1.0" encoding="utf-8"?>\n`;
  xml += `<feed xmlns="http://www.w3.org/2005/Atom">\n`;
  xml += `  <title>${xmlEscape(FEED_TITLE)}</title>\n`;
  xml += `  <subtitle>${xmlEscape(FEED_SUBTITLE)}</subtitle>\n`;
  xml += `  <link rel="alternate" type="text/html" href="${SITE_URL}/"/>\n`;
  xml += `  <link rel="self" type="application/atom+xml" href="${FEED_URL}"/>\n`;
  xml += `  <id>${SITE_URL}/</id>\n`;
  xml += `  <updated>${feedUpdated.toISOString()}</updated>\n`;
  xml += `  <author>\n`;
  xml += `    <name>${xmlEscape(AUTHOR_NAME)}</name>\n`;
  xml += `    <uri>${AUTHOR_URI}</uri>\n`;
  xml += `  </author>\n`;
  xml += `  <icon>${SITE_URL}/papers-og.jpg</icon>\n`;
  xml += `  <rights>CC BY 4.0 unless noted otherwise.</rights>\n`;

  for (const e of entries) {
    const cleanDetail = htmlToText(e.detail);
    const cleanSummary = htmlToText(e.summary);
    const summary = [cleanDetail, cleanSummary].filter(Boolean).join(" — ");
    xml += `  <entry>\n`;
    xml += `    <title>${xmlEscape(htmlToText(e.title))}</title>\n`;
    xml += `    <link rel="alternate" type="application/pdf" href="${xmlEscape(e.url)}"/>\n`;
    xml += `    <id>${xmlEscape(e.id)}</id>\n`;
    xml += `    <updated>${e.updated.toISOString()}</updated>\n`;
    if (e.published) {
      xml += `    <published>${e.published.toISOString()}</published>\n`;
    }
    xml += `    <category term="${xmlEscape(e.category)}"/>\n`;
    xml += `    <author><name>${xmlEscape(AUTHOR_NAME)}</name></author>\n`;
    if (summary) {
      xml += `    <summary type="text">${xmlEscape(summary)}</summary>\n`;
    }
    xml += `  </entry>\n`;
  }
  xml += `</feed>\n`;

  writeFileSync(join(SITE_DIR, "feed.xml"), xml);
  // Mirror the Atom feed at /rss.xml so readers probing either path succeed.
  writeFileSync(join(SITE_DIR, "rss.xml"), xml);
  console.log(`  FEED updated: feed.xml + rss.xml (${entries.length} entries).`);
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
  console.log("\nGenerating thumbnails...\n");
  const thumbCount = genAllThumbnails();
  console.log(`  THUMBS ${thumbCount} JPEG previews in thumbs/.\n`);
  console.log("Done.\n");
} else if (cmd === "thumbs") {
  console.log("\nGenerating thumbnails for every deployed PDF...\n");
  const thumbCount = genAllThumbnails();
  console.log(`\nDone. ${thumbCount} thumbs in thumbs/.\n`);
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
  const now = new Date().toISOString();
  const builtDirs = new Set(built.filter((e) => e.lang === "en").map((e) => e.dir));
  for (const dir of builtDirs) {
    if (!meta[dir]) meta[dir] = { created: now.slice(0, 10), revisions: 0 };
    meta[dir].revisions = (meta[dir].revisions || 0) + 1;
    meta[dir].updated = now;
  }
  saveMetadata(meta);
  console.log(`  METADATA updated (${builtDirs.size} papers incremented).\n`);

  // Page-1 JPEG thumbnails for the index
  console.log("Generating thumbnails...\n");
  const thumbCount = genAllThumbnails();
  console.log(`  THUMBS ${thumbCount} JPEG previews in thumbs/.\n`);

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
