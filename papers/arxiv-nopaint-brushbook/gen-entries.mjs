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
      painted.push({ file: files[2], table: brush.table });
    }
    cases.push(row);
  }
  // The cover: the painted squares, six across.
  const cols = 6, rows = Math.floor(painted.length / cols), used = painted.slice(0, cols * rows);
  if (rows) {
    // Each square wears its kind's colour as a border; white gutters between.
    const edge = { Brushes: "0x00763E", Filters: "0x2860D0" };
    const layout = used.map((_, i) => `${(i % cols) * 288}_${Math.floor(i / cols) * 288}`).join("|");
    ffmpeg(...used.flatMap((p) => ["-i", p.file]), "-filter_complex",
      `${used.map((p, i) => `[${i}]crop=256:256:0:0,pad=276:276:10:10:color=${edge[p.table] || "0x40384A"},pad=288:288:6:6:color=white[v${i}]`).join(";")};${used.map((_, i) => `[v${i}]`).join("")}xstack=inputs=${used.length}:layout=${layout}:fill=white`,
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

// Share of all proposals: one of the two tables at even odds, then by weight.
const share = (brush) => brush.table ? 0.5 * brush.weight / totals[brush.table] : 0;
const percent = (p) => p >= 0.0995 ? `${Math.round(p * 100)}` : (p * 100).toFixed(1).replace(/\.0$/, "");
const kind = (brush) => brush.table === "Filters" ? "filter" : brush.table === "Brushes" ? "brush" : "by name";

// ---- entries.tex -------------------------------------------------------------
const BOOK_WORDS = {
  // The game's Stamp page has no sentence. This one is the book's.
  Stamp: "Place a drawing from a painter's set.",
};
// The colour the game's help gives each painter (HelpByLine, HelpName).
const colours = Object.fromEntries(brushes.filter((b) => b.by && b.color).map((b) => [b.by, b.color.toUpperCase()]));
const painterChip = (who) => `\\painter{${esc(who)}}{${colours[who] || "FFFFFF"}}`;
const kindKey = (b) => b.table === "Filters" ? "filter" : b.table === "Brushes" ? "brush" : "name";
const kindChip = (b) => `\\kindchip{${kindKey(b)}}{${kind(b).toUpperCase()}}`;
const widest = Math.max(...brushes.map(share));
const bar = (b) => `\\oddsbar{${kindKey(b)}}{${(2.6 * share(b) / widest).toFixed(2)}cm}`;
brushes.forEach((b, i) => { b.label = `br:${i + 1}`; });
let tex = "% Generated by gen-entries.mjs from brushes.json and e2e-summary.json. Do not edit by hand.\n";
const tally = { works: 0, differs: 0, untested: 0 };
for (const [n, b] of brushes.entries()) {
  const c = cases[b.name];
  // Left grid: what you can type after the name.
  const left = [];
  const chips = (r) => r.option.split(/,\s*/).map((o) => `\\opt{${esc(o)}}`).join(" ");
  // More than four option rows do not fit the card; then two share a row.
  if (b.ranges.length > 4) for (let i = 0; i < b.ranges.length; i += 2) {
    const [a, z] = [b.ranges[i], b.ranges[i + 1]];
    left.push(`\\grow{${chips(a)}}{${z ? `\\makebox[0.95cm][l]{${esc(a.range)}}${chips(z)}\\ ${esc(z.range)}` : esc(a.range)}}`);
  } else for (const r of b.ranges) left.push(`\\grow{${chips(r)}}{${esc(r.range)}}`);
  // Camera on a phone takes Facing where a desktop takes Number.
  for (const v of b.variants || []) for (const r of v.ranges.filter((r) => !b.ranges.some((x) => x.option === r.option)))
    left.push(`\\grow{\\lab{on a phone}}{${chips(r)}\\ ${esc(r.range)}}`);
  if (!left.length) left.push("\\grow{\\lab{options}}{None. The name is all you type.}");
  // Right grid: how often it comes, and what the test saw.
  const right = [b.table
    ? `\\grow{\\lab{odds}}{${bar(b)}\\ \\ ${percent(share(b))} in 100}`
    : "\\grow{\\lab{odds}}{Never proposed.}"];
  let verdict = "untested";
  if (!c || (!c.skipped && !c.paint)) right.push("\\grow{\\lab{test}}{Not tested.}");
  else if (c.skipped) right.push(`\\grow{\\lab{test}}{Not run: it ${esc(c.skipped)}.}`);
  else {
    const typed = c.line || b.name, ran = c.paint.said || typed;
    right.push(`\\grow{\\lab{typed}}{\\ran{${esc(typed)}}}`);
    if (ran !== typed.replace(":", "")) right.push(`\\grow{\\lab{ran as}}{\\ran{${esc(ran)}}}`);
    const seen = [];
    if (c.watched) seen.push(c.paint.pass ? "\\ok{painted by itself}" : "\\bad{painted nothing by itself}");
    else {
      seen.push(!c.paint.opens ? "\\bad{did not open}" : !c.paint.home ? "\\bad{Paint did not come back}"
        : c.paint.changed ? "\\ok{Paint changed it}" : "\\bad{Paint changed nothing}");
      if (c.no) seen.push(!c.no.home ? "\\bad{No did not come back}" : c.no.changed ? "\\bad{No changed it}" : "\\ok{No left it}");
    }
    verdict = c.paint.pass && (c.watched || c.no?.pass) ? "works" : "differs";
    right.push(`\\grow{\\lab{test}}{${seen.join(" ")}}`);
  }
  tally[verdict]++;
  const by = b.by ? painterChip(b.by) : b.painters ? `{\\sffamily\\scriptsize\\color{white}${b.painters.length} painters}` : "";
  const desc = b.desc ? esc(b.desc) : BOOK_WORDS[b.name] ? esc(BOOK_WORDS[b.name]) : "";
  const strip = c?.strip && existsSync(join(HERE, c.strip)) ? c.strip : "";
  const options = b.options ? b.options.split(" ").map((o) => `\\bandopt{${esc(o)}}`).join("\\hspace{2pt}") : "";
  const def = (name, value) => `\\def\\${name}{${value}}`;
  tex += [def("bn", esc(b.name)), def("bo", options), def("bk", kindKey(b)), def("bK", kind(b).toUpperCase()), def("bl", b.label),
    def("bp", by), def("bd", desc), def("bL", `%\n${left.join("\n")}`), def("bR", `%\n${right.join("\n")}`), def("bs", strip), def("bt", c?.start === "card" ? "the test card" : "a new painting")].join("%\n") + "%\n\\brushcard\n" + (n % 2 ? "\\clearpage\n" : "\\cardgap\n");
}
writeFileSync(join(HERE, "entries.tex"), tex);

// ---- tables.tex --------------------------------------------------------------
let tab = "% Generated by gen-entries.mjs. Do not edit by hand.\n";
for (const [name, title] of [["Brushes", "Brushes: they add something"], ["Filters", "Filters: they change what is there"]]) {
  const rows = brushes.filter((b) => b.table === name).sort((a, b) => b.weight - a.weight || a.name.localeCompare(b.name));
  tab += `\\fullwidth{\\backhead{${title}}\n\\noindent{\\footnotesize\\arrayrulecolor{acgray!40}\\renewcommand{\\arraystretch}{1.3}\\begin{tabularx}{\\linewidth}{@{}l L r r r@{}}\\hline\n & & weight & in 100 & page \\\\\\hline\n`;
  for (const b of rows) tab += `\\spl{${esc(b.name)}} & ${bar(b)} & ${b.weight} & ${percent(share(b))} & \\pageref{${b.label}} \\\\\\hline\n`;
  tab += `total & & ${totals[name]} & 50 & \\\\\\hline\n\\end{tabularx}}}\n`;
}
const named = brushes.filter((b) => !b.table);
tab += `\\fullwidth{\\backhead{By name only}\n\\noindent{\\footnotesize ${named.map((b) => `\\spl{${esc(b.name)}} p.\\,\\pageref{${b.label}}`).join("\\quad ")}}}\n`;
// Who made what.
const made = new Map();
for (const b of brushes) if (b.by) made.set(b.by, [...(made.get(b.by) || []), b]);
tab += "\\fullwidth{\\backhead{Who made which brush}\n\\noindent{\\footnotesize\\arrayrulecolor{acgray!40}\\renewcommand{\\arraystretch}{1.45}\\begin{tabularx}{\\linewidth}{@{}l L@{}}\\hline\n";
for (const [who, list] of [...made].sort((a, b) => b[1].length - a[1].length || a[0].localeCompare(b[0])))
  tab += `${painterChip(who)} & ${list.map((b) => `\\spl{${esc(b.name)}}`).join(" ")} \\\\\\hline\n`;
tab += "\\end{tabularx}}}\n";
// Whose drawings Stamp places, as the game's Stamp page lists them.
const stamp = brushes.find((b) => b.painters);
if (stamp) tab += `\\fullwidth{\\backhead{Whose drawings Stamp places}\n\\noindent{\\raggedright\\renewcommand{\\paintersize}{\\scriptsize}${stamp.painters.map(painterChip).join(" ")}\\par}}\n`;
writeFileSync(join(HERE, "tables.tex"), tab);

writeFileSync(join(HERE, "tally.tex"),
  `\\newcommand{\\firstbrush}{${esc(brushes[0].name)}}\\newcommand{\\brushcount}{${brushes.length}}\\newcommand{\\tallyworks}{${tally.works}}\\newcommand{\\tallydiffers}{${tally.differs}}` +
  `\\newcommand{\\tallyuntested}{${tally.untested}}\\newcommand{\\paintercount}{${painters.length}}` +
  `\\newcommand{\\brushtablecount}{${brushes.filter((b) => b.table === "Brushes").length}}\\newcommand{\\filtertablecount}{${brushes.filter((b) => b.table === "Filters").length}}` +
  `\\newcommand{\\testdate}{${summary.at ? new Date(summary.at).toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric", timeZone: "UTC" }) : "not yet"}}\n`);
console.log(`${brushes.length} entries: ${tally.works} work, ${tally.differs} differ, ${tally.untested} untested`);
