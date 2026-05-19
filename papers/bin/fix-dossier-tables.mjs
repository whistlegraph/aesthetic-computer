#!/usr/bin/env node
// fix-dossier-tables.mjs — make dossier tables stop overflowing the column.
//
// The bug: many dossiers use `\begin{tabularx}{\columnwidth}{lr}` (or
// similar) with NO `X` column. tabularx needs at least one X column to
// distribute width; with none it ignores the width target and lays the
// table out at natural width, which overflows the narrow two-column
// measure and gets clipped at the page edge (the "page 4 tables cut
// off" report). Longer strings in translations make it worse.
//
// Fix: every tabularx whose colspec has no `X` is converted to a plain
// `tabular` wrapped in `\resizebox{\columnwidth}{!}{...}` so it can
// never exceed the column. tabularx blocks that DO have an X column
// already wrap correctly and are left untouched. Idempotent.
//
// Usage:
//   node papers/bin/fix-dossier-tables.mjs            (all dossiers, en)
//   node papers/bin/fix-dossier-tables.mjs arxiv-recurse
//   node papers/bin/fix-dossier-tables.mjs --dry

import { readFileSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");

const DOSSIERS = [
  "arxiv-rhizome", "arxiv-sfpc", "arxiv-eyebeam", "arxiv-recurse",
  "arxiv-internet-archive", "arxiv-mellon", "arxiv-pioneer-works",
  "arxiv-new-inc", "arxiv-studio-museum", "arxiv-hathitrust",
  "arxiv-the-kitchen", "arxiv-machine-project",
  "arxiv-heavy-manners-library", "arxiv-microvision", "arxiv-calarts-news",
];

const argv = process.argv.slice(2);
const dry = argv.includes("--dry");
const only = argv.filter((a) => !a.startsWith("--"));

const MARK = "% [fix-dossier-tables]";

// Read a brace-balanced {...} group starting at `i` (s[i] must be '{').
// Returns { body, end } where end is the index just past the closing '}'.
function readGroup(s, i) {
  if (s[i] !== "{") return null;
  let depth = 0;
  for (let j = i; j < s.length; j++) {
    if (s[j] === "{") depth++;
    else if (s[j] === "}") {
      depth--;
      if (depth === 0) return { body: s.slice(i + 1, j), end: j + 1 };
    }
  }
  return null;
}

function fixFile(path) {
  let s = readFileSync(path, "utf8");
  const orig = s;
  let out = "";
  let i = 0;
  let fixed = 0;
  let skipped = 0;

  while (i < s.length) {
    const at = s.indexOf("\\begin{tabularx}", i);
    if (at === -1) {
      out += s.slice(i);
      break;
    }
    // width group, then colspec group
    const wStart = at + "\\begin{tabularx}".length;
    const w = readGroup(s, wStart);
    if (!w) { out += s.slice(i, wStart); i = wStart; continue; }
    const c = readGroup(s, w.end);
    if (!c) { out += s.slice(i, w.end); i = w.end; continue; }

    const colspec = c.body;
    const hasX = /X/.test(colspec);
    const endTok = "\\end{tabularx}";
    const endAt = s.indexOf(endTok, c.end);
    if (endAt === -1) { out += s.slice(i, c.end); i = c.end; continue; }

    const inner = s.slice(c.end, endAt); // table body between header and \end
    const alreadyWrapped =
      s.slice(Math.max(0, at - 120), at).includes(MARK);

    if (hasX || alreadyWrapped) {
      // Leave well-formed (X-column) or already-fixed tables alone.
      out += s.slice(i, endAt + endTok.length);
      i = endAt + endTok.length;
      skipped++;
      continue;
    }

    // Convert: tabularx (no X) → tabular wrapped in a width clamp.
    out += s.slice(i, at);
    out +=
      `${MARK}\n\\resizebox{\\columnwidth}{!}{%\n` +
      `\\begin{tabular}{${colspec}}` +
      inner +
      `\\end{tabular}%\n}`;
    i = endAt + endTok.length;
    fixed++;
  }

  // Ensure \resizebox is available (graphicx). Almost always already
  // loaded for \includegraphics, but inject defensively if absent.
  if (fixed > 0 && !/\\usepackage(\[[^\]]*\])?\{graphicx\}/.test(out) &&
      !/\\usepackage(\[[^\]]*\])?\{graphics\}/.test(out)) {
    out = out.replace(
      /(\\documentclass[^\n]*\n)/,
      `$1\\usepackage{graphicx} ${MARK}\n`,
    );
  }

  const changed = out !== orig;
  if (changed && !dry) writeFileSync(path, out, "utf8");
  return { changed, fixed, skipped };
}

const targets = (only.length ? only : DOSSIERS).filter((d) =>
  existsSync(join(PAPERS_DIR, d)),
);

let totalFixed = 0;
for (const dir of targets) {
  const full = join(PAPERS_DIR, dir);
  for (const f of readdirSync(full)) {
    // English + any translation, but NOT the generated -cards.tex
    // (cards-convert already converts tabularx→tabular for cards).
    if (!f.endsWith(".tex") || f.endsWith("-cards.tex")) continue;
    const r = fixFile(join(full, f));
    if (r.fixed || r.changed) {
      totalFixed += r.fixed;
      console.log(
        `  ${r.changed ? (dry ? "would fix" : "fixed") : "ok"} ${dir}/${f} — ` +
          `${r.fixed} wrapped, ${r.skipped} left (X-col/already)`,
      );
    }
  }
}
console.log(`\n${dry ? "[dry] " : ""}${totalFixed} table(s) wrapped across ${targets.length} dossier(s).`);
