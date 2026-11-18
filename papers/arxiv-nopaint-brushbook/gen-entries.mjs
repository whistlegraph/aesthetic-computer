#!/usr/bin/env node
// gen-entries.mjs — write the brushbook's generated parts.
//
//   node papers/arxiv-nopaint-brushbook/gen-entries.mjs                 from what is here
//   node papers/arxiv-nopaint-brushbook/gen-entries.mjs --from DIR      take in a new test run first
//
// brushes.json (extract-brushes.mjs) is what the game says about each brush.
// e2e-summary.json is what tests/browser/brushbook.e2e.mjs saw when it
// ran each one. This joins them into entries.tex, tables.tex and tally.tex.
//
// --from DIR reads the test's output folder (results.json and its PNGs),
// trims the results into e2e-summary.json, and makes one strip per brush in
// shots/: the painting before, the proposal on screen, the painting after
// Paint. It also makes cover.jpg from the painted squares. Needs ffmpeg.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const FFMPEG = process.env.FFMPEG || "/opt/homebrew/bin/ffmpeg";
const read = (f) => JSON.parse(readFileSync(join(HERE, f), "utf8"));
const slug = (s) => s.replace(/[^a-z0-9]+/gi, "-").replace(/^-|-$/g, "").slice(0, 40) || "x";
const esc = (s) => String(s)
  .replace(/\\/g, "\\textbackslash{}")
  .replace(/([#$%&_{}])/g, "\\$1")
  .replace(/~/g, "\\textasciitilde{}")
  .replace(/\^/g, "\\textasciicircum{}");

const { brushes, totals, painters } = read("brushes.json");

// ---- take in a test run ----------------------------------------------------
const from = process.argv.includes("--from") ? process.argv[process.argv.indexOf("--from") + 1] : null;
if (from) {
  const run = JSON.parse(readFileSync(join(from, "results.json"), "utf8"));
  const cases = [];
  mkdirSync(join(HERE, "shots"), { recursive: true });
  const ffmpeg = (...args) => execFileSync(FFMPEG, ["-loglevel", "error", "-y", ...args]);
  const painted = [];
  for (const brush of brushes) {
    const rows = run.results.filter((r) => r.brush === brush.name);
    const skipped = rows.find((r) => r.skipped);
    const watch = rows.find((r) => r.decision === "watch");
    const paint = rows.find((r) => r.decision === "paint") || watch, no = rows.find((r) => r.decision === "no");
    const pick = (r) => r && { pass: !!r.pass, said: r.said || null, opens: !!r.opens, home: !!r.home, changed: r.changed ?? null,
      error: r.error || null, errors: r.log?.errors?.length || 0 };
    const row = { brush: brush.name, start: paint?.start || null, skipped: skipped?.skipped || null, watched: !!watch, line: paint?.line || null, paint: pick(paint), no: pick(no) };
    const files = paint && [paint.beforePng, paint.shot, paint.png].map((f) => f && join(from, f));
    if (files && files.every((f) => f && existsSync(f))) {
      // before | proposal | painted, each 256 high, on white.
      row.strip = `shots/${slug(brush.name)}.jpg`;
      ffmpeg("-i", files[0], "-i", files[1], "-i", files[2], "-filter_complex",
        "[0]crop=256:256:0:0,pad=264:256:0:0:white[a];[1]scale=-2:256:flags=lanczos,pad=iw+8:256:0:0:white[b];[2]crop=256:256:0:0[c];[a][b][c]hstack=3",
        "-q:v", "3", join(HERE, row.strip));
      painted.push(files[2]);
    }
    cases.push(row);
  }
  // The cover: the painted squares, six across.
  const cols = 6, rows = Math.floor(painted.length / cols), used = painted.slice(0, cols * rows);
  if (rows) {
    const layout = used.map((_, i) => `${(i % cols) * 256}_${Math.floor(i / cols) * 256}`).join("|");
    ffmpeg(...used.flatMap((f) => ["-i", f]), "-filter_complex",
      `${used.map((_, i) => `[${i}]crop=256:256:0:0[v${i}]`).join(";")};${used.map((_, i) => `[v${i}]`).join("")}xstack=inputs=${used.length}:layout=${layout}`,
      "-q:v", "4", join(HERE, "cover.jpg"));
  }
  writeFileSync(join(HERE, "e2e-summary.json"), JSON.stringify({
    at: run.at, script: "tests/browser/brushbook.e2e.mjs",
    build: "No Paint classic (2021 Construct export plus toolchain/nopaint/patch-classic.mjs), served from a folder, network blocked",
    browser: "headless Chrome on a fleet Mac mini", view: run.view, lookMs: run.look, cover: rows ? { cols, rows } : null, cases,
  }, null, 1) + "\n");
}

const summary = existsSync(join(HERE, "e2e-summary.json")) ? read("e2e-summary.json") : { cases: [] };
const cases = Object.fromEntries(summary.cases.map((c) => [c.brush, c]));

// ---- what the test saw, in a sentence ---------------------------------------
function ranLine(brush) {
  const c = cases[brush.name];
  if (!c) return { verdict: "untested", line: "Not tested." };
  if (c.skipped) return { verdict: "untested", line: `Not run: ${c.skipped}.` };
  if (c.watched) return c.paint.pass
    ? { verdict: "works", line: `\\spl{${esc(c.line)}}. Its first steps painted with no button pressed.` }
    : { verdict: "differs", line: `\\spl{${esc(c.line)}}: it painted nothing by itself.` };
  if (!c.paint || !c.no) return { verdict: "untested", line: "Not tested." };
  const ran = c.paint?.said ? `\\spl{${esc(c.paint.said)}}` : `\\spl{${esc(brush.name)}}`;
  const said = c.line && c.line.replace(":", "") !== (c.paint?.said || brush.name) ? `Typed \\spl{${esc(c.line)}}; it ran as ${ran}` : ran;
  if (c.paint?.pass && c.no?.pass) return { verdict: "works", line: `${said}. Paint changed the painting; No left it as it was.` };
  const parts = [];
  if (!c.paint?.opens) parts.push("it did not open");
  else {
    if (!c.paint.home) parts.push("Paint did not come back to the next proposal");
    else if (!c.paint.changed) parts.push("Paint left the painting as it was");
    if (c.no && !c.no.home) parts.push("No did not come back to the next proposal");
    else if (c.no && c.no.changed) parts.push("No changed the painting");
  }
  return { verdict: "differs", line: `${said}: ${parts.join("; ") || "the run did not finish"}.` };
}
// Share of all proposals: one of the two tables at even odds, then by weight.
const share = (brush) => brush.table ? 0.5 * brush.weight / totals[brush.table] : 0;
const percent = (p) => p >= 0.0995 ? `${Math.round(p * 100)}` : (p * 100).toFixed(1).replace(/\.0$/, "");
const kind = (brush) => brush.table === "Filters" ? "filter" : brush.table === "Brushes" ? "brush" : "by name";

// ---- entries.tex -------------------------------------------------------------
const BOOK_WORDS = {
  // The game's Stamp page has no sentence. This one is the book's.
  Stamp: "Place one drawing from a painter's set. Each painter's handle names a set; Num picks the drawing.",
};
brushes.forEach((b, i) => { b.label = `br:${i + 1}`; });
let tex = "% Generated by gen-entries.mjs from brushes.json and e2e-summary.json. Do not edit by hand.\n";
let letter = "";
const tally = { works: 0, differs: 0, untested: 0 };
for (const b of brushes) {
  const first = b.name[0].toUpperCase();
  if (first !== letter) { letter = first; tex += `\n% ${letter}\n`; } // the running head carries the alphabet
  const ran = ranLine(b);
  tally[ran.verdict]++;
  const c = cases[b.name];
  const fields = [];
  for (const r of b.ranges) fields.push(`\\field{${esc(r.option)}}{${esc(r.range)}}`);
  for (const v of b.variants || []) fields.push(`\\field{on a phone}{\\spl{${esc(b.name)}: ${esc(v.options)}} ${v.ranges.map((r) => `${esc(r.option)}: ${esc(r.range)}`).join("; ")}}`);
  if (b.painters) fields.push(`\\field{art}{${b.painters.map((p) => `@${esc(p)}`).join(", ")}}`);
  fields.push(b.table
    ? `\\field{odds}{About ${percent(share(b))} in 100 proposals.}`
    : "\\field{odds}{Never proposed. It comes only when you type it.}");
  fields.push(`\\field{ran}{${ran.line}}`);
  const by = b.by ? `@${esc(b.by)}` : b.painters ? `${b.painters.length} painters` : "";
  const desc = b.desc ? esc(b.desc) : BOOK_WORDS[b.name] ? esc(BOOK_WORDS[b.name]) : "";
  const strip = c?.strip && existsSync(join(HERE, c.strip)) ? c.strip : "";
  tex += `\\brushentry{${esc(b.name)}}{${esc(b.options)}}{${kind(b)}}{${b.label}}{${by}}{%\n${desc}}{%\n${fields.join("\n")}}{${strip}}{${c?.start === "card" ? "the test card" : "a new painting"}}\n`;
}
writeFileSync(join(HERE, "entries.tex"), tex);

// ---- tables.tex --------------------------------------------------------------
let tab = "% Generated by gen-entries.mjs. Do not edit by hand.\n";
for (const [name, title] of [["Brushes", "Brushes: they add something"], ["Filters", "Filters: they change what is there"]]) {
  const rows = brushes.filter((b) => b.table === name).sort((a, b) => b.weight - a.weight || a.name.localeCompare(b.name));
  tab += `\\backhead{${title}}\n\\noindent{\\footnotesize\\begin{tabularx}{\\linewidth}{@{}L r r r@{}}\\toprule\n & weight & in 100 & page \\\\\\midrule\n`;
  for (const b of rows) tab += `\\spl{${esc(b.name)}} & ${b.weight} & ${percent(share(b))} & \\pageref{${b.label}} \\\\\n`;
  tab += `\\midrule total & ${totals[name]} & 50 & \\\\\\bottomrule\n\\end{tabularx}}\n`;
}
const named = brushes.filter((b) => !b.table);
tab += `\\backhead{By name only}\n\\noindent{\\footnotesize ${named.map((b) => `\\spl{${esc(b.name)}} (p.\\,\\pageref{${b.label}})`).join(", ")}.}\n`;
// Who made what.
const made = new Map();
for (const b of brushes) if (b.by) made.set(b.by, [...(made.get(b.by) || []), b]);
tab += "\\backhead{Who made which brush}\n\\noindent{\\footnotesize\\begin{tabularx}{\\linewidth}{@{}l L@{}}\\toprule\n";
for (const [who, list] of [...made].sort((a, b) => b[1].length - a[1].length || a[0].localeCompare(b[0])))
  tab += `@${esc(who)} & ${list.map((b) => `\\spl{${esc(b.name)}}`).join(", ")} \\\\\n`;
tab += "\\bottomrule\\end{tabularx}}\n";
writeFileSync(join(HERE, "tables.tex"), tab);

writeFileSync(join(HERE, "tally.tex"),
  `\\newcommand{\\brushcount}{${brushes.length}}\\newcommand{\\tallyworks}{${tally.works}}\\newcommand{\\tallydiffers}{${tally.differs}}` +
  `\\newcommand{\\tallyuntested}{${tally.untested}}\\newcommand{\\paintercount}{${painters.length}}` +
  `\\newcommand{\\brushtablecount}{${brushes.filter((b) => b.table === "Brushes").length}}\\newcommand{\\filtertablecount}{${brushes.filter((b) => b.table === "Filters").length}}` +
  `\\newcommand{\\testdate}{${summary.at ? new Date(summary.at).toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric", timeZone: "UTC" }) : "not yet"}}\n`);
console.log(`${brushes.length} entries: ${tally.works} work, ${tally.differs} differ, ${tally.untested} untested`);
