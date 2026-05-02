#!/usr/bin/env node
// migrate-cover.mjs — apply the new dossier cover layout (TikZ overlay,
// pals top-left, QR top-right, illustration-behind-title vignette) to all
// dossiers that still use the old "centered pals + 15em hero" cover.
//
// Idempotent: detects whether a dossier has already been migrated by
// looking for the \papersitename macro.
//
// Usage:
//   node papers/bin/migrate-cover.mjs <arxiv-slug>
//   node papers/bin/migrate-cover.mjs --all

import { readFileSync, writeFileSync, readdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");

// Map arxiv-slug → siteName (must mirror gen-qrs.mjs).
const SITE_NAMES = {
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
};

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (const a of argv) {
  if (a.startsWith("--")) flags[a.slice(2)] = true;
  else positional.push(a);
}

function findTex(dir) {
  for (const f of readdirSync(join(PAPERS_DIR, dir))) {
    if (f.endsWith(".tex")) return f;
  }
  return null;
}

function migrate(slug) {
  const siteName = SITE_NAMES[slug];
  if (!siteName) return { slug, status: "skip", reason: "no siteName" };

  const texFile = findTex(slug);
  if (!texFile) return { slug, status: "skip", reason: "no .tex" };

  const path = join(PAPERS_DIR, slug, texFile);
  let src = readFileSync(path, "utf8");

  if (src.includes("\\papersitename")) {
    return { slug, status: "skip", reason: "already migrated" };
  }

  // 1. Add \usepackage{tikz} after \usepackage{graphicx} if not already present
  if (!/\\usepackage\{tikz\}/.test(src)) {
    src = src.replace(
      /(\\usepackage\{graphicx\}\n)/,
      "$1\\usepackage{tikz}\n",
    );
  }

  // 2. Add \papersitename + \paperurl after \paperrevision
  src = src.replace(
    /(\\newcommand\{\\paperrevision\}\{[^}]+\}\n)/,
    `$1\\newcommand{\\papersitename}{${siteName}}\n` +
      `\\newcommand{\\paperurl}{https://papers.aesthetic.computer/\\papersitename.pdf}\n`,
  );

  // 3. Replace cover block — match the old block exactly
  const oldBlock =
    /\\twocolumn\[\{%\s*\n\\noindent\\hfill\\raisebox\{-1\.2em\}\[0pt\]\[0pt\]\{\\includegraphics\[height=3\.5em\]\{pals\}\}\\par\\vspace\{-2\.6em\}\s*\n\\begin\{center\}\s*\n\\includegraphics\[height=15em\]\{figures\/cover\}\\par\\vspace\{0\.6em\}\s*\n\{\\acbold\\fontsize\{22pt\}\{26pt\}\\selectfont\\color\{acdark\} ([^}]+)\}\\par\s*\n\\vspace\{0\.2em\}\s*\n\{\\aclight\\fontsize\{11pt\}\{13pt\}\\selectfont\\color\{acpink\} A Dossier\}\\par\s*\n\\vspace\{0\.3em\}\s*\n\{\\aclight\\fontsize\{9pt\}\{11pt\}\\selectfont\\color\{acgray\} ([^}]+)\}\\par\s*\n\\vspace\{0\.6em\}\s*\n\{\\normalsize\\href\{https:\/\/prompt\.ac\/@jeffrey\}\{@jeffrey\}\}\\par\s*\n\{\\small\\color\{acgray\} Aesthetic\.Computer\}\\par\s*\n\{\\small\\color\{acgray\} ORCID: \\href\{https:\/\/orcid\.org\/0009-0007-4460-4913\}\{0009-0007-4460-4913\}\}\\par\s*\n\\vspace\{0\.2em\}\s*\n\{\\small\\color\{acpurple\} \\url\{https:\/\/aesthetic\.computer\}\}\\par\s*\n\\vspace\{0\.4em\}\s*\n\{\\footnotesize\\color\{acgray\}Created \\papercreated\{\} \\,\\textbullet\\, Revision \\paperrevision\}\\par\s*\n\\vspace\{0\.6em\}\s*\n\\rule\{\\textwidth\}\{1\.5pt\}\s*\n\\vspace\{0\.5em\}\s*\n\\end\{center\}/;

  const m = src.match(oldBlock);
  if (!m) {
    return { slug, status: "fail", reason: "old cover block not found" };
  }
  const title = m[1];
  const subtitle = m[2];

  const newBlock =
    `\\twocolumn[{%\n` +
    `% Top header row: pals (left) + QR + label (right)\n` +
    `\\noindent\n` +
    `\\begin{minipage}[t]{0.5\\textwidth}\\raggedright\n` +
    `\\includegraphics[height=3em]{pals}\n` +
    `\\end{minipage}%\n` +
    `\\begin{minipage}[t]{0.5\\textwidth}\\raggedleft\n` +
    `\\begin{tabular}{@{}r@{}}\n` +
    `{\\footnotesize\\acbold\\color{acpink} \\,\\,Read this paper now!}\\\\[0.18em]\n` +
    `\\href{\\paperurl}{\\includegraphics[height=2.4cm]{figures/qr}}\n` +
    `\\end{tabular}\n` +
    `\\end{minipage}\\par\n` +
    `\\vspace{0.4em}\n` +
    `\n` +
    `% Hero illustration with title floating over the bottom vignette\n` +
    `\\begin{center}\n` +
    `\\begin{tikzpicture}\n` +
    `  \\node[inner sep=0pt] (cover) at (0,0) {\\includegraphics[width=0.86\\textwidth]{figures/cover}};\n` +
    `  \\node[anchor=south, align=center, inner sep=0pt] at ([yshift=2.4em]cover.south) {%\n` +
    `    \\begin{tabular}{c}\n` +
    `      {\\acbold\\fontsize{30pt}{34pt}\\selectfont\\color{acdark}\\strut ${title}}\\\\[0.05em]\n` +
    `      {\\aclight\\fontsize{13pt}{15pt}\\selectfont\\color{acpink}\\strut A Dossier}\\\\[0.2em]\n` +
    `      {\\aclight\\fontsize{9.5pt}{11pt}\\selectfont\\color{acgray}\\strut ${subtitle}}\\\\\n` +
    `    \\end{tabular}\n` +
    `  };\n` +
    `\\end{tikzpicture}\n` +
    `\\par\\vspace{0.4em}\n` +
    `\n` +
    `{\\normalsize\\href{https://prompt.ac/@jeffrey}{@jeffrey}}\\par\n` +
    `{\\small\\color{acgray} Aesthetic.Computer}\\par\n` +
    `{\\small\\color{acgray} ORCID: \\href{https://orcid.org/0009-0007-4460-4913}{0009-0007-4460-4913}}\\par\n` +
    `\\vspace{0.2em}\n` +
    `{\\small\\color{acpurple} \\url{https://aesthetic.computer}}\\par\n` +
    `\\vspace{0.4em}\n` +
    `{\\footnotesize\\color{acgray}Created \\papercreated{} \\,\\textbullet\\, Revision \\paperrevision}\\par\n` +
    `\\vspace{0.6em}\n` +
    `\\rule{\\textwidth}{1.5pt}\n` +
    `\\vspace{0.5em}\n` +
    `\\end{center}`;

  src = src.replace(oldBlock, newBlock);
  writeFileSync(path, src, "utf8");
  return { slug, status: "ok", title, subtitle };
}

let targets = [];
if (flags.all) {
  for (const slug of Object.keys(SITE_NAMES)) {
    if (readdirSync(PAPERS_DIR).includes(slug)) targets.push(slug);
  }
} else if (positional.length) {
  targets = positional;
} else {
  console.error("usage: migrate-cover.mjs <arxiv-slug> | --all");
  process.exit(1);
}

console.log(`▸ migrating ${targets.length} dossier(s)`);
for (const t of targets) {
  const r = migrate(t);
  if (r.status === "ok") {
    console.log(`  ✓ ${r.slug}: "${r.title}"`);
  } else {
    console.log(`  · ${r.slug}: ${r.status} (${r.reason})`);
  }
}
