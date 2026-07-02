#!/usr/bin/env node
// cover.mjs — square podcast cover art for a reading.
//
// Echoes the essay's title page: the pink drop-shadow YWFT-Processing title on
// paper stock, the five-color AC bar, and the series identity. Rendered with
// xelatex (same engine/fonts as the essays) → PDF → PNG, so the cover and the
// document are visually the same object.
//
// Usage:
//   import { renderCover } from "./cover.mjs"
//   renderCover({ title, author, date, slug }, outDir) → { full, embed }

import { writeFileSync, mkdirSync, rmSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");           // repo root (absolute)
const FONTS = resolve(REPO, "system/public/type/webfonts") + "/";
const MONO = resolve(REPO, "bills/invoices") + "/";

const texEscape = (s) =>
  String(s).replace(/([&%$#_{}])/g, "\\$1").replace(/~/g, "\\textasciitilde{}").replace(/\^/g, "\\textasciicircum{}");

function coverTex({ title, author, date }) {
  const speaker = String(author).replace(/^@/, "");
  return String.raw`% !TEX program = xelatex
\documentclass[12pt]{article}
\usepackage[paperwidth=6in,paperheight=6in,margin=0.5in]{geometry}
\usepackage[table]{xcolor}
\usepackage{fontspec}
\usepackage{tikz}
\pagestyle{empty}

\definecolor{acpink}{HTML}{FF6B9D}
\definecolor{accyan}{HTML}{4ECDC4}
\definecolor{acyellow}{HTML}{FFE66D}
\definecolor{acmint}{HTML}{8CE99A}
\definecolor{titlepink}{RGB}{180,72,135}
\definecolor{acink}{RGB}{40,36,48}
\definecolor{acpaper}{HTML}{FFF9FC}
\definecolor{acgray}{RGB}{119,119,119}

\newfontfamily\coverlight{ywft-processing-light}[Path=${FONTS},Extension=.ttf]
\newfontfamily\coverbold{ywft-processing-bold}[Path=${FONTS},Extension=.ttf]
\newfontfamily\covermono{BerkeleyMonoVariable-Regular}[Path=${MONO},Extension=.ttf]

\newcommand{\acdot}{\textcolor{acpink}{.}}
\newcommand{\colorbar}{%
  \textcolor{acpink}{\rule{0.2\linewidth}{7pt}}%
  \textcolor{accyan}{\rule{0.2\linewidth}{7pt}}%
  \textcolor{acyellow}{\rule{0.2\linewidth}{7pt}}%
  \textcolor{acmint}{\rule{0.2\linewidth}{7pt}}%
  \textcolor{acpink}{\rule{0.2\linewidth}{7pt}}%
}

\begin{document}
\pagecolor{acpaper}\color{acink}
\centering
\vspace*{0.15in}

{\covermono\fontsize{13pt}{15pt}\selectfont\color{acgray} A\, READING}\\[0.14in]
{\covermono\fontsize{10pt}{12pt}\selectfont\color{acgray} AESTHETIC\acdot COMPUTER}\\[0.3in]

\colorbar\\[0.2in]

\vfill

\begin{tikzpicture}
  \node[acink, opacity=0.22, align=center, text width=\linewidth,
        font=\coverlight\fontsize{50pt}{56pt}\selectfont, inner sep=0pt]
        at (2.5pt,-2.5pt) {${texEscape(title)}};
  \node[titlepink, align=center, text width=\linewidth,
        font=\coverlight\fontsize{50pt}{56pt}\selectfont, inner sep=0pt]
        at (0,0) {${texEscape(title)}};
\end{tikzpicture}

\vfill

{\coverbold\fontsize{15pt}{18pt}\selectfont\color{acink} read by @${texEscape(speaker)}}\\[0.12in]
${date ? String.raw`{\covermono\fontsize{12pt}{15pt}\selectfont\color{acgray} ${texEscape(date)}}\\[0.28in]` : String.raw`\vspace{0.28in}`}

\colorbar
\vspace*{0.15in}
\end{document}
`;
}

export function renderCover(script, outDir) {
  mkdirSync(outDir, { recursive: true });
  const build = resolve(outDir, `.cover-build-${script.slug}`);
  mkdirSync(build, { recursive: true });
  const tex = resolve(build, "cover.tex");
  writeFileSync(tex, coverTex(script));

  execFileSync("xelatex", ["-interaction=nonstopmode", "-halt-on-error", "cover.tex"], {
    cwd: build, stdio: "ignore",
  });
  const pdf = resolve(build, "cover.pdf");
  if (!existsSync(pdf)) throw new Error("cover xelatex produced no pdf");

  const full = resolve(outDir, `${script.slug}-cover.png`);   // ~3000px, archival
  const embed = resolve(outDir, `${script.slug}-cover-1400.png`); // podcast art
  execFileSync("mutool", ["draw", "-o", full, "-r", "500", pdf, "1"], { stdio: "ignore" });
  execFileSync("ffmpeg", ["-y", "-i", full, "-vf", "scale=1400:1400", embed], { stdio: "ignore" });

  rmSync(build, { recursive: true, force: true });
  return { full, embed };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  // Standalone smoke test.
  const out = resolve(HERE, "..", "out");
  const { full } = renderCover(
    { slug: "test", title: "The Market on Your Name", author: "@jeffrey", date: "July 2026" },
    out,
  );
  console.log(`✓ ${full}`);
}
