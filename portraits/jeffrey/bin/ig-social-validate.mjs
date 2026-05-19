#!/usr/bin/env node
// Stage 6.5: validation gate. The PDF shipped with "\textbackslash{}textbf"
// garbage because nothing checked it — this is that check. Hard-fails (exit
// 1) on escaping artifacts, count mismatches, structural breakage, or LaTeX
// errors; warns (exit 0) on softer quality smells. Runnable before *and*
// after the xelatex compile (it validates whatever artifacts exist).
//
// Usage:
//   node bin/ig-social-validate.mjs whistlegraph          # pre-compile
//   node bin/ig-social-validate.mjs whistlegraph --post    # + log/pdf
//
// Exit 0 = ok (warnings allowed), 1 = hard failure (do not ship/compile).

import { readFileSync, existsSync, statSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");
const account = process.argv[2];
if (!account || account.startsWith("--")) {
  console.error("usage: ig-social-validate.mjs <account> [--post]");
  process.exit(64);
}
const POST = process.argv.includes("--post");
const dir = join(REPO_ROOT, "portraits/jeffrey/social", account);
const P = (f) => join(dir, f);

const fails = [];
const warns = [];
const fail = (m) => fails.push(m);
const warn = (m) => warns.push(m);
const read = (f) => readFileSync(P(f), "utf8");
const sizeOf = (f) => (existsSync(P(f)) ? statSync(P(f)).size : 0);

// 1 ── required artifacts present + non-trivial ----------------------------
for (const [f, min] of [
  ["graph.json", 2000],
  ["REPORT.md", 1000],
  ["REPORT.tex", 1000],
  ["artworld.csv", 200],
]) {
  if (!existsSync(P(f))) fail(`missing artifact: ${f}`);
  else if (sizeOf(f) < min) fail(`${f} suspiciously small (${sizeOf(f)}B)`);
}
if (fails.length) done(); // exits; can't check further without files

// 2 ── LaTeX escaping artifacts (the exact bug that shipped) ---------------
const tex = read("REPORT.tex");
const ARTIFACTS = [
  [/\\textbackslash\{\}\s*(textbf|textit|section|subsection|item|begin|end)/,
    "double-escaped control sequence (\\textbackslash{} before a macro)"],
  [/\\textbackslash\\\{\\\}/, "escaped-then-escaped braces (\\textbackslash\\{\\})"],
  [/\\textbackslash\{\}#/, "double-escaped # in header"],
  [/\\subsection\*\{\s*\}/, "empty \\subsection*{}"],
  [/\\textbf\{\s*\}/, "empty \\textbf{}"],
  [/\\textcolor\{[a-z0-9_]+\}\{\s*\}/, "empty \\textcolor cell"],
  [/&\s*&\s*&\s*&\s*&\s*\\\\/, "fully empty table row"],
];
for (const [re, why] of ARTIFACTS) {
  const m = tex.match(re);
  if (m)
    fail(`REPORT.tex artifact — ${why}\n     e.g. …${tex
      .slice(Math.max(0, m.index - 30), m.index + 50)
      .replace(/\n/g, "⏎")}…`);
}

// 2b ── same artifact scan on the broadside, if present -------------------
if (existsSync(P("ONEPAGER.tex"))) {
  const otx = read("ONEPAGER.tex");
  for (const [re, why] of ARTIFACTS) {
    const m = otx.match(re);
    if (m)
      fail(
        `ONEPAGER.tex artifact — ${why}\n     e.g. …${otx
          .slice(Math.max(0, m.index - 30), m.index + 50)
          .replace(/\n/g, "⏎")}…`,
      );
  }
  if (!/\\begin\{document\}/.test(otx) || !/\\end\{document\}/.test(otx))
    fail("ONEPAGER.tex missing \\begin/\\end{document}");
}

// 3 ── structural balance --------------------------------------------------
const bal = (open, close, label) => {
  const o = (tex.match(new RegExp(`\\\\begin\\{${open}\\}`, "g")) || []).length;
  const c = (tex.match(new RegExp(`\\\\end\\{${close}\\}`, "g")) || []).length;
  if (o !== c) fail(`${label}: ${o} \\begin vs ${c} \\end (unbalanced)`);
  return o;
};
const nTables = bal("longtable", "longtable", "longtable");
bal("itemize", "itemize", "itemize");
if (!/\\begin\{document\}/.test(tex) || !/\\end\{document\}/.test(tex))
  fail("REPORT.tex missing \\begin/\\end{document}");
if (nTables < 3) warn(`only ${nTables} longtables (expected ≥3 categories)`);

// 4 ── count consistency across data → md/csv → tex ------------------------
const g = JSON.parse(read("graph.json"));
const artNodes = g.nodes.filter((n) => n.art_world);
const totalsAW = g.totals.art_world;
if (artNodes.length !== totalsAW)
  fail(`graph.json: totals.art_world=${totalsAW} ≠ nodes.art_world=${artNodes.length}`);
const byTypeSum = Object.entries(g.totals.by_type)
  .filter(([k]) => k !== "other")
  .reduce((s, [, v]) => s + v, 0);
if (byTypeSum !== totalsAW)
  warn(`by_type art sum=${byTypeSum} ≠ art_world=${totalsAW} (artist/other edge)`);

const csv = read("artworld.csv").trim().split("\n");
const csvRows = csv.length - 1; // minus header
if (csvRows !== artNodes.length)
  fail(`artworld.csv rows=${csvRows} ≠ art_world nodes=${artNodes.length}`);
const headerCols = csv[0].split(",").length;
const badCsv = csv
  .slice(1)
  .findIndex(
    (l) => (l.match(/,/g) || []).length + 1 < headerCols - 0, // quoted commas ok-ish
  );
// (loose check — quoted fields make exact column counting unreliable; only
//  flag a row with far too few separators)
if (
  csv
    .slice(1)
    .some((l) => (l.match(/,/g) || []).length < headerCols - 3)
)
  fail(`artworld.csv has a row with too few columns (row ~${badCsv + 2})`);

// rows rendered into the .tex should match the art-node count too
const texRowCount = (tex.match(/\\\\(?:\n|$)/g) || []).length;
if (texRowCount < artNodes.length)
  warn(
    `tex row markers (${texRowCount}) < art nodes (${artNodes.length}) — ` +
      `truncated table?`,
  );

// 5 ── data sanity / noise surfacing (warn, never fail) --------------------
const soft = artNodes.filter(
  (n) => (n.confirmed_by || []).length <= 1 && (n.confirmed_by || [])[0] !== "wikidata",
);
warn(
  `${soft.length}/${artNodes.length} art nodes are single-heuristic-only ` +
    `(potential false positives — leads, not facts)`,
);
const noLabel = artNodes.filter((n) => !n.label || n.label === n.id).length;
if (noLabel) warn(`${noLabel} art nodes have no real name/label`);

// 6 ── post-compile: xelatex log + pdf -------------------------------------
if (POST) {
  if (existsSync(P("REPORT.log"))) {
    const log = read("REPORT.log");
    if (/\n!\s/.test(log) || /Undefined control sequence/.test(log))
      fail(
        "xelatex log has a fatal error (! … / Undefined control sequence) — " +
          (log.match(/\n!.*/)?.[0] || "see REPORT.log"),
      );
    const overfull = (log.match(/Overfull \\hbox/g) || []).length;
    if (overfull > 25)
      warn(`${overfull} Overfull \\hbox (content poking into margins)`);
  } else warn("no REPORT.log to inspect (compile first for full validation)");

  if (existsSync(P("REPORT.pdf"))) {
    if (sizeOf("REPORT.pdf") < 5000) fail("REPORT.pdf is tiny — likely broken");
    // Page count from the xelatex log (no pdfinfo dependency).
    let pages = 0;
    if (existsSync(P("REPORT.log"))) {
      pages = +(read("REPORT.log").match(
        /Output written on .*\((\d+) pages?/,
      )?.[1] || 0);
    }
    if (!pages) {
      // fallback: count /Type /Page objects in the PDF bytes
      try {
        pages = (
          readFileSync(P("REPORT.pdf"), "latin1").match(
            /\/Type\s*\/Page[^s]/g,
          ) || []
        ).length;
      } catch {
        /* ignore */
      }
    }
    if (pages < 1) fail("REPORT.pdf — could not confirm ≥1 page");
    else console.error(`  pdf: ${pages} pages, ${sizeOf("REPORT.pdf")}B`);
  } else fail("--post given but REPORT.pdf does not exist");

  // The broadside has one hard rule: it MUST be exactly one page.
  if (existsSync(P("ONEPAGER.log"))) {
    const olog = read("ONEPAGER.log");
    if (/\n!\s/.test(olog) || /Undefined control sequence/.test(olog))
      fail(
        "ONEPAGER xelatex fatal — " +
          (olog.match(/\n!.*/)?.[0] || "see ONEPAGER.log"),
      );
    const op = +(olog.match(/Output written on .*\((\d+) pages?/)?.[1] || 0);
    if (op !== 1)
      fail(`ONEPAGER.pdf is ${op} pages — a broadside MUST be exactly 1`);
    else console.error(`  broadside: 1 page ✓ (${sizeOf("ONEPAGER.pdf")}B)`);
  } else if (POST) warn("no ONEPAGER.log (compile the broadside for full gate)");
}

function done() {
  for (const w of warns) console.error(`  ⚠ ${w}`);
  if (fails.length) {
    console.error(`\n✘ VALIDATION FAILED (${fails.length}):`);
    for (const f of fails) console.error(`  ✘ ${f}`);
    process.exit(1);
  }
  console.error(
    `\n✓ validation passed${
      warns.length ? ` (${warns.length} warning${warns.length > 1 ? "s" : ""})` : ""
    } — ${POST ? "tex+log+pdf" : "tex+data"} clean`,
  );
  process.exit(0);
}
done();
