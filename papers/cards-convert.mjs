#!/usr/bin/env node
// cards-convert.mjs — Convert two-column papers to cards format
// Usage: node cards-convert.mjs arxiv-notepat/notepat.tex
//        node cards-convert.mjs all          Convert all papers in PAPER_MAP
//
// Creates {base}-cards.tex from {base}.tex by:
//   1. Replacing the preamble with ac-paper-cards setup
//   2. Reformatting the title block to cards-style centered layout
//   3. Keeping all body content intact (sections auto-break via cards.sty)

import { readFileSync, writeFileSync, existsSync } from "fs";
import { join, basename, dirname } from "path";

const PAPERS_DIR = new URL(".", import.meta.url).pathname;

const PAPER_MAP = {
  "arxiv-ac": { base: "ac", title: "Aesthetic\\acdot Computer '26" },
  "arxiv-api": { base: "api", title: "From \\texttt{setup()} to \\texttt{boot()}" },
  "arxiv-archaeology": { base: "archaeology", title: "Repository Archaeology" },
  "arxiv-dead-ends": { base: "dead-ends", title: "Vestigial Features" },
  "arxiv-diversity": { base: "diversity", title: "Citation Diversity Audit" },
  "arxiv-goodiepal": { base: "goodiepal", title: "Radical Computer Art" },
  "arxiv-kidlisp": { base: "kidlisp", title: "Kid{\\color{acpurple}Lisp} '26" },
  "arxiv-kidlisp-reference": { base: "kidlisp-reference", title: "KidLisp Language Reference" },
  "arxiv-network-audit": { base: "network-audit", title: "Network Audit" },
  "arxiv-notepat": { base: "notepat", title: "notepat{\\color{acpurple}.}{\\color{acpink}com}" },
  "arxiv-os": { base: "os", title: "AC Native OS '26" },
  "arxiv-pieces": { base: "pieces", title: "Pieces Not Programs" },
  "arxiv-sustainability": { base: "sustainability", title: "Who Pays for Creative Tools?" },
  "arxiv-whistlegraph": { base: "whistlegraph", title: "Whistlegraph" },
  "arxiv-complex": { base: "complex", title: "Sucking on the Complex" },
};

function extractFromTex(content) {
  // Extract pdftitle
  const pdftitleMatch = content.match(/pdftitle\s*=\s*\{([^}]+)\}/);
  const pdftitle = pdftitleMatch ? pdftitleMatch[1] : "Untitled";

  // Extract subtitle from \aclight\fontsize line
  const subtitleMatch = content.match(
    /\\aclight\\fontsize\{[^}]+\}\{[^}]+\}\\selectfont\\color\{acpink\}\s*(.+?)\s*\}\\par/
  );
  const subtitle = subtitleMatch ? subtitleMatch[1].trim() : null;

  // Extract graphicspath
  const gpMatch = content.match(/\\graphicspath\{\{([^}]+)\}\}/);
  const graphicspath = gpMatch ? gpMatch[1] : "figures/";

  // Check for listings
  const hasListings = content.includes("\\usepackage{listings}") || content.includes("\\begin{lstlisting}");

  // Check for CJK
  const hasCJK = content.includes("\\usepackage{xeCJK}");
  const cjkFontMatch = content.match(/\\setCJKmainfont\{([^}]+)\}/);
  const cjkFont = cjkFontMatch ? cjkFontMatch[1] : "Droid Sans Fallback";

  // Check for KidLisp-specific fonts
  const hasKidlispFonts = content.includes("kidlispbold") || content.includes("kidlispfont");

  // Find body start
  const bodyStart = content.indexOf("\\begin{document}");
  const bodyEnd = content.indexOf("\\end{document}");

  if (bodyStart === -1 || bodyEnd === -1) return null;

  // Extract body content after \begin{document}
  let body = content.substring(bodyStart + "\\begin{document}".length, bodyEnd).trim();

  // Remove the existing title block (everything before the first \section)
  const firstSection = body.search(/\\section\{/);
  let titleContent = "";
  let mainBody = body;

  if (firstSection > 0) {
    titleContent = body.substring(0, firstSection).trim();
    mainBody = body.substring(firstSection).trim();
  }

  // Extract abstract from title content — only use \begin{abstract}...\end{abstract}
  let abstract = "";
  const abstractMatch = titleContent.match(
    /\\begin\{abstract\}([\s\S]*?)\\end\{abstract\}/
  );
  if (abstractMatch) {
    abstract = abstractMatch[1].trim();
  }

  return {
    pdftitle,
    subtitle,
    graphicspath,
    hasListings,
    hasCJK,
    cjkFont,
    hasKidlispFonts,
    abstract,
    mainBody,
    titleContent,
  };
}

function generateCardsTeX(dir, info, parsed) {
  const extraPackages = [];
  if (parsed.hasListings) extraPackages.push("\\usepackage{listings}");

  const cjkBlock = parsed.hasCJK
    ? `\\usepackage{xeCJK}\n\\setCJKmainfont{${parsed.cjkFont}}`
    : "";

  const kidlispFonts = parsed.hasKidlispFonts
    ? `\\newfontfamily\\kidlispbold{ywft-processing-bold}[\n  Path=../../system/public/type/webfonts/,\n  Extension=.ttf\n]\n\\newfontfamily\\kidlispfont{ywft-processing-light}[\n  Path=../../system/public/type/webfonts/,\n  Extension=.ttf\n]`
    : "";

  const title = info.title || parsed.pdftitle;
  const subtitle = parsed.subtitle || "";

  const abstractCard = parsed.abstract
    ? `% ============================================================
% ABSTRACT CARD
% ============================================================
\\clearpage
\\begin{accentcard}
\\cardtitle{Abstract}

${parsed.abstract}
\\end{accentcard}

`
    : "";

  return `% !TEX program = xelatex
% Cards format — auto-generated from ${info.base}.tex by cards-convert.mjs
\\documentclass[11pt]{article}

\\usepackage{fontspec}
\\usepackage{unicode-math}
\\setmainfont{Latin Modern Roman}
\\setsansfont{Latin Modern Sans}
\\setmonofont{Latin Modern Mono}[Scale=0.88]
${cjkBlock ? "\n" + cjkBlock : ""}

\\usepackage{graphicx}
\\graphicspath{{${parsed.graphicspath}}}
\\usepackage{booktabs}
\\usepackage{tabularx}
\\usepackage{ragged2e}
\\usepackage{microtype}
\\usepackage{natbib}
${extraPackages.join("\n")}
${kidlispFonts ? "\n" + kidlispFonts : ""}

\\makeatletter
\\def\\input@path{{../}}
\\makeatother
\\usepackage{ac-paper-cards}

\\hypersetup{
  pdftitle={${parsed.pdftitle}},
}

\\begin{document}

% ============================================================
% TITLE CARD
% ============================================================
\\thispagestyle{empty}
\\vspace*{\\fill}
\\begin{center}
\\includegraphics[height=3em]{pals}\\par\\vspace{0.6em}
{\\acbold\\fontsize{20pt}{24pt}\\selectfont\\color{acdark} ${title}}\\par
\\vspace{0.3em}
${subtitle ? `{\\aclight\\fontsize{10pt}{12pt}\\selectfont\\color{acpink} ${subtitle}}\\par\n\\vspace{0.8em}` : "\\vspace{0.5em}"}
{\\normalsize Jeffrey Alan Scudder}\\par
{\\small\\color{acgray} Aesthetic Inc.}\\par
{\\small\\color{acgray} ORCID: \\href{https://orcid.org/0009-0007-4460-4913}{0009-0007-4460-4913}}\\par
\\vspace{0.8em}
\\rule{0.6\\textwidth}{1pt}\\par
\\vspace{0.4em}
{\\small\\color{acpink}\\textbf{[ working draft --- not for citation ]}}\\par
\\vspace{0.3em}
{\\footnotesize\\color{acgray} March 2026}\\par
\\end{center}
\\vspace*{\\fill}

% ============================================================
% INDEX CARD
% ============================================================
\\cardindex

${abstractCard}% ============================================================
% BODY
% ============================================================
${parsed.mainBody}

\\end{document}
`;
}

function convertPaper(dirName) {
  const info = PAPER_MAP[dirName];
  if (!info) {
    console.error(`  Unknown paper: ${dirName}`);
    return false;
  }

  const texPath = join(PAPERS_DIR, dirName, `${info.base}.tex`);
  const outPath = join(PAPERS_DIR, dirName, `${info.base}-cards.tex`);

  if (!existsSync(texPath)) {
    console.error(`  NOT FOUND: ${texPath}`);
    return false;
  }

  const content = readFileSync(texPath, "utf8");
  const parsed = extractFromTex(content);

  if (!parsed) {
    console.error(`  PARSE FAIL: ${texPath}`);
    return false;
  }

  const cardsTeX = generateCardsTeX(dirName, info, parsed);
  writeFileSync(outPath, cardsTeX, "utf8");
  console.log(`  WROTE ${dirName}/${info.base}-cards.tex`);
  return true;
}

// --- CLI ---
const target = process.argv[2];

if (!target) {
  console.log("Usage: node cards-convert.mjs <dir-name|all>");
  console.log("  node cards-convert.mjs arxiv-notepat");
  console.log("  node cards-convert.mjs all");
  process.exit(0);
}

if (target === "all") {
  console.log("\nConverting all papers to cards format...\n");
  let ok = 0;
  for (const dir of Object.keys(PAPER_MAP)) {
    if (convertPaper(dir)) ok++;
  }
  console.log(`\nDone: ${ok}/${Object.keys(PAPER_MAP).length} converted.\n`);
} else {
  convertPaper(target);
}
