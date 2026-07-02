#!/usr/bin/env node
// essay-to-script.mjs — turn an AC essay (.tex or .md) into a clean spoken
// script: { title, author, date, paragraphs[], wordCount }.
//
// The job is subtraction. An essay carries apparatus a reader-aloud must not
// voice: footnotes (citations), section headings (visual scaffolding), URLs,
// the colophon. We strip all of it and keep the argument, split into
// paragraphs so produce.mjs can put a breath between each.
//
// Usage:
//   node bin/essay-to-script.mjs ../../papers/essay-named-markets/named-markets.tex
//   import { essayToScript } from "./essay-to-script.mjs"

import { readFileSync } from "node:fs";
import { basename } from "node:path";

// ── balanced-brace helpers ─────────────────────────────────────────────
// Remove `\name{ ... }` entirely (drops command AND its argument).
function removeCommand(s, name) {
  const re = new RegExp(`\\\\${name}\\s*\\{`, "g");
  let out = "";
  let last = 0;
  let m;
  while ((m = re.exec(s))) {
    out += s.slice(last, m.index);
    let i = m.index + m[0].length;
    let depth = 1;
    while (i < s.length && depth > 0) {
      if (s[i] === "{") depth++;
      else if (s[i] === "}") depth--;
      i++;
    }
    last = i;
    re.lastIndex = i;
  }
  out += s.slice(last);
  return out;
}

// Replace `\name{inner}` with `inner` (keeps the argument, drops the wrapper).
function unwrapCommand(s, name) {
  const re = new RegExp(`\\\\${name}\\s*\\{`, "g");
  let out = "";
  let last = 0;
  let m;
  while ((m = re.exec(s))) {
    out += s.slice(last, m.index);
    let i = m.index + m[0].length;
    let depth = 1;
    let inner = "";
    while (i < s.length && depth > 0) {
      if (s[i] === "{") { depth++; inner += s[i]; }
      else if (s[i] === "}") { depth--; if (depth > 0) inner += s[i]; }
      else inner += s[i];
      i++;
    }
    out += inner;
    last = i;
    re.lastIndex = i;
  }
  out += s.slice(last);
  return out;
}

// `\href{url}{text}` → text
function unwrapHref(s) {
  const re = /\\href\s*\{/g;
  let out = "";
  let last = 0;
  let m;
  while ((m = re.exec(s))) {
    out += s.slice(last, m.index);
    let i = m.index + m[0].length;
    // skip url group
    let depth = 1;
    while (i < s.length && depth > 0) { if (s[i] === "{") depth++; else if (s[i] === "}") depth--; i++; }
    // now text group
    if (s[i] === "{") {
      i++; depth = 1; let text = "";
      while (i < s.length && depth > 0) {
        if (s[i] === "{") { depth++; text += s[i]; }
        else if (s[i] === "}") { depth--; if (depth > 0) text += s[i]; }
        else text += s[i];
        i++;
      }
      out += text;
    }
    last = i;
    re.lastIndex = i;
  }
  out += s.slice(last);
  return out;
}

function stripLineComments(s) {
  // Remove from an unescaped % to end of line.
  return s.replace(/(^|[^\\])%.*$/gm, "$1");
}

// ── .tex parser (AC essay format) ──────────────────────────────────────
function parseTex(raw) {
  let s = stripLineComments(raw);

  const grab = (name) => {
    const m = s.match(new RegExp(`\\\\${name}\\s*\\{`));
    if (!m) return null;
    let i = m.index + m[0].length, depth = 1, inner = "";
    while (i < s.length && depth > 0) {
      if (s[i] === "{") { depth++; inner += s[i]; }
      else if (s[i] === "}") { depth--; if (depth > 0) inner += s[i]; }
      else inner += s[i];
      i++;
    }
    return inner;
  };

  let title = (grab("essaytitle") || basename("untitled")).replace(/\\\\/g, " ").replace(/\s+/g, " ").trim();
  let date = (grab("essaydate") || "").replace(/\s+/g, " ").trim();
  const authorM = raw.match(/pdfauthor=\{([^}]*)\}/);
  const author = authorM ? authorM[1].trim() : "@jeffrey";

  // Body = between \begin{document} and \end{document}
  const b = s.indexOf("\\begin{document}");
  const e = s.indexOf("\\end{document}");
  let body = b >= 0 ? s.slice(b + "\\begin{document}".length, e >= 0 ? e : undefined) : s;

  // Strip vertical-space directives (may carry a length arg like -0.5in).
  body = body.replace(/\\vspace\*?\s*\{[^}]*\}/g, "\n\n");

  // Drop footnotes (citations), colophon, and headings entirely — all via
  // balanced-brace removal so nested macros (\ac{}, \url{}) don't fool us.
  for (const cmd of [
    "footnote", "colophon", "textsuperscript", "includegraphics",
    "essaytitle", "essaydate", "thispagestyle", "pagestyle", "hypersetup",
    "section", "subsection",   // visual scaffold — a reading is continuous
  ]) {
    body = removeCommand(body, cmd);
  }
  body = body.replace(/\\acbreak\b/g, "\n\n");

  // Environments we don't voice.
  body = body.replace(/\\begin\{center\}[\s\S]*?\\end\{center\}/g, "\n\n");
  body = body.replace(/\\(begin|end)\{[^}]*\}/g, "\n\n");

  // Inline wrappers.
  body = unwrapHref(body);
  body = body.replace(/\\url\s*\{[^}]*\}/g, ""); // urls are not read
  for (const cmd of ["textit", "textbf", "emph", "textsc", "underline", "text"]) {
    body = unwrapCommand(body, cmd);
  }
  // \ac{} macro → spoken name
  body = body.replace(/\\ac\{\}/g, "Aesthetic Computer").replace(/\\ac\b/g, "Aesthetic Computer");
  body = body.replace(/\\np\{?\}?/g, "notepat").replace(/\\acos\{?\}?/g, "AC Native OS");

  // Punctuation / spacing normalization for TTS prosody.
  body = body
    .replace(/\\textcolor\s*\{[^}]*\}\s*\{([^}]*)\}/g, "$1")
    .replace(/---/g, ", ")           // em dash → comma pause
    .replace(/--/g, ", ")
    .replace(/``|''|"/g, "")         // strip quote marks (voiced oddly)
    .replace(/\\@/g, "")
    .replace(/\\,|\\enspace|\\quad|\\;|~/g, " ")
    .replace(/\$\\cdot\$|\\cdot/g, " ")
    .replace(/\\footnotesize|\\small|\\normalsize|\\par/g, " ")
    .replace(/\\[a-zA-Z]+\*?/g, "")  // any stray control words
    .replace(/[{}]/g, "")
    .replace(/[ \t]+/g, " ")
    .replace(/\s+([,.;:])/g, "$1")   // no space before punctuation
    .replace(/,\s*,/g, ",");          // collapse doubled commas

  return { title, date, author, body };
}

// ── .md parser (opinion essays) ────────────────────────────────────────
function parseMd(raw) {
  let s = raw;
  let title = "Untitled", date = "", author = "@jeffrey";
  const fm = s.match(/^---\n([\s\S]*?)\n---\n/);
  if (fm) {
    const block = fm[1];
    const t = block.match(/title:\s*["']?(.+?)["']?\s*$/m); if (t) title = t[1].trim();
    const d = block.match(/date:\s*(.+)$/m); if (d) date = d[1].trim();
    const a = block.match(/author:\s*(.+)$/m); if (a) author = a[1].trim();
    s = s.slice(fm[0].length);
  }
  let body = s
    .replace(/^#{1,6}\s+.*$/gm, "\n\n")     // headings → break
    .replace(/^\s*---\s*$/gm, "\n\n")        // hr → break
    .replace(/^\s*>\s?/gm, "")               // blockquote marker
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1") // links → text
    .replace(/\*\*([^*]+)\*\*/g, "$1")
    .replace(/\*([^*]+)\*/g, "$1")
    .replace(/`([^`]+)`/g, "$1")
    .replace(/—/g, ", ")
    .replace(/[ \t]+/g, " ");
  return { title, date, author, body };
}

export function essayToScript(path) {
  const raw = readFileSync(path, "utf8");
  const isTex = path.endsWith(".tex");
  const { title, date, author, body } = isTex ? parseTex(raw) : parseMd(raw);

  const paragraphs = body
    .split(/\n\s*\n/)
    .map((p) => p.replace(/\s+/g, " ").trim())
    .filter((p) => p.length > 1);

  const wordCount = paragraphs.join(" ").split(/\s+/).filter(Boolean).length;
  const slug = basename(path).replace(/\.[^.]+$/, "");
  return { slug, title, date, author, paragraphs, wordCount };
}

// CLI: print the script as JSON.
if (import.meta.url === `file://${process.argv[1]}`) {
  const p = process.argv[2];
  if (!p) { console.error("usage: essay-to-script.mjs <essay.tex|.md>"); process.exit(1); }
  const script = essayToScript(p);
  console.error(`✓ ${script.title} — ${script.paragraphs.length} paragraphs · ${script.wordCount} words`);
  console.log(JSON.stringify(script, null, 2));
}
