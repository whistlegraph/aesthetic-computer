#!/usr/bin/env node
// Stage 6: turn the assembled graph into what was actually asked for —
// line-by-line tables + a literate written summary, NOT a chart.
//
// Reads graph.json (from ig-social-edges.mjs) and emits:
//   social/<acct>/REPORT.md    literate summary + ranked tables per category
//   social/<acct>/artworld.csv exhaustive one-row-per-art-world-node table
//
// Re-runnable any time (zero network); safe to run against partial data
// while enrichment is still going.
//
// Usage: node bin/ig-social-report.mjs whistlegraph

import { readFileSync, writeFileSync, existsSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");
const account = process.argv[2];
if (!account) {
  console.error("usage: ig-social-report.mjs <account>");
  process.exit(64);
}
const dir = join(REPO_ROOT, "portraits/jeffrey/social", account);
const gPath = join(dir, "graph.json");
if (!existsSync(gPath)) {
  console.error(`no graph.json — run ig-social-edges.mjs ${account} first`);
  process.exit(1);
}
const g = JSON.parse(readFileSync(gPath, "utf8"));
const nodes = g.nodes;
const art = nodes.filter((n) => n.art_world);
const aff = g.edges.filter((e) => e.kind === "affiliation");

const reachStr = (r) =>
  r == null ? "—" : r >= 1000 ? `${Math.round(r / 1000)}k` : String(r);
const conf = (n) => (n.confirmed_by || []).join("+") || "—";
const desc = (n) =>
  (n.web_description || n.ig_category || "").replace(/\|/g, "/").slice(0, 60);
const md = (s) => String(s ?? "").replace(/\|/g, "\\|");

const order = ["museum", "institution", "gallery", "art_media", "curator", "artist"];
const LABEL = {
  museum: "Museums",
  institution: "Institutions & foundations",
  gallery: "Galleries",
  art_media: "Art media, magazines & fairs",
  curator: "Curators, dealers, critics — people who run things",
  artist: "Artists",
};
const grouped = Object.fromEntries(order.map((t) => [t, []]));
for (const n of art) if (grouped[n.type]) grouped[n.type].push(n);
for (const t of order)
  grouped[t].sort(
    (a, b) => (b.reach || 0) - (a.reach || 0) || a.id.localeCompare(b.id),
  );

const webOnly = art.filter(
  (n) => (n.confirmed_by || []).length === 1 && n.confirmed_by[0] === "wikidata",
);
const tri = art.filter((n) => (n.confirmed_by || []).length >= 3);

// ── literate summary ───────────────────────────────────────────────────────
const total = g.totals.nodes;
const pct = ((art.length / total) * 100).toFixed(1);
const counts = order
  .map((t) => `${grouped[t].length} ${t.replace("_", " ")}`)
  .join(", ");

let out = `# @${account} — the contemporary art world it follows

*Generated ${g.generated} from \`graph.json\`. ${
  g.totals.enriched
} of ${total} nodes IG-enriched; Wikidata corroboration ongoing — numbers climb on re-run.*

## The shape of it

@${account} follows **${total}** accounts. **${art.length}** of them (${pct}%)
resolve to the art world — ${counts}. The rest (${
  total - art.length
}) are personal/peripheral and deliberately left out of the tables below.

A node is counted as art-world when **at least one** of three independent
methods agrees: a name/handle heuristic, Instagram's own business category,
or a Wikidata occupation / instance-of record. **${
  tri.length
}** nodes are confirmed by all three (the spine of the map). **${
  webOnly.length
}** were caught *only* by Wikidata — eponymous galleries and plain-name
curators/artists that neither the keyword pass nor Instagram's category field
would ever surface, which is exactly the gap that pass was added to close.

The strongest signal in the data is **not** the famous museums (those are the
loud, obvious nodes) but the mid-tier: working commercial galleries, artist-run
spaces, and the individual curators/dealers/critics — and the **${
  aff.length
}** affiliation links that say who runs or works at what. That relational
layer is the actual "map"; the tables below are sorted by reach only because
it's a stable order, not because reach is the interesting variable.

`;

for (const t of order) {
  const list = grouped[t];
  if (!list.length) continue;
  out += `\n## ${LABEL[t]} — ${list.length}\n\n`;
  out += `| # | handle | name | reach | confirmed | what it is |\n`;
  out += `|--:|--------|------|------:|-----------|------------|\n`;
  list.forEach((n, i) => {
    out += `| ${i + 1} | [@${md(n.id)}](https://instagram.com/${
      n.id
    }) | ${md(n.label)} | ${reachStr(n.reach)} | ${conf(n)} | ${md(
      desc(n),
    )} |\n`;
  });
}

// who-runs-what
out += `\n## Who runs / works at what — ${aff.length} affiliation links\n\n`;
out += `Parsed from Instagram bios (\`@mention\` + role word) and Wikidata\n`;
out += `employer / member-of. This is the connective tissue of the map.\n\n`;
out += `| person / org | → | affiliated with | source |\n`;
out += `|------|--|------|--------|\n`;
for (const e of aff.slice(0, 200)) {
  const r = e.role ? `/${e.role}` : "";
  out += `| @${md(e.source)} | → | ${md(e.target)} | ${e.via}${r} |\n`;
}
if (aff.length > 200)
  out += `\n*…and ${aff.length - 200} more — full list in \`artworld.csv\`.*\n`;

// wikidata-only
out += `\n## Found only by Wikidata — ${webOnly.length}\n\n`;
out += `Art-world identity the heuristic *and* Instagram's category both\n`;
out += `missed. The reason the corroboration stage exists.\n\n`;
out += `| handle | name | wikidata says |\n|--------|------|---------------|\n`;
for (const n of webOnly
  .slice()
  .sort((a, b) => (b.reach || 0) - (a.reach || 0))
  .slice(0, 120))
  out += `| @${md(n.id)} | ${md(n.label)} | ${md(n.web_description || "")} |\n`;

// caveats
out += `\n## Confidence & known noise\n
- **Single-source nodes are softer.** A node confirmed only by the name
  heuristic (e.g. a "founder"/"studio" bio keyword) can be a false positive
  — a non-art entrepreneur, a tattoo "studio". Treat \`confirmed\` = a single
  method as a lead, not a fact; multi-method rows are solid.
- **Wikidata name collisions.** Common-name searches can grab the wrong
  entity (a historical painter instead of the living gallerist; an athlete
  sharing a name). Flagged for the disambiguation pass.
- **Reach is followers**, an attention proxy, not art-world importance — a
  600-follower curatorial project can matter more than a 1M museum feed.
- The exhaustive, sortable, machine-readable version of every art-world row
  (incl. all affiliations) is in **\`artworld.csv\`** alongside this file.
`;

writeFileSync(join(dir, "REPORT.md"), out);

// ── exhaustive CSV ─────────────────────────────────────────────────────────
const affBy = {};
for (const e of aff)
  (affBy[e.source] ||= []).push(`${e.target}${e.role ? `(${e.role})` : ""}`);
const csvEsc = (s) => {
  const v = String(s ?? "");
  return /[",\n]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v;
};
const cols = [
  "username",
  "full_name",
  "type",
  "confirmed_by",
  "reach",
  "relation",
  "ig_category",
  "wikidata",
  "web_description",
  "affiliations",
];
let csv = cols.join(",") + "\n";
for (const n of art
  .slice()
  .sort(
    (a, b) =>
      order.indexOf(a.type) - order.indexOf(b.type) ||
      (b.reach || 0) - (a.reach || 0),
  )) {
  csv +=
    [
      n.id,
      n.label,
      n.type,
      (n.confirmed_by || []).join("+"),
      n.reach ?? "",
      n.relation ?? "",
      n.ig_category ?? "",
      n.wikidata ?? "",
      n.web_description ?? "",
      (affBy[n.id] || []).join("; "),
    ]
      .map(csvEsc)
      .join(",") + "\n";
}
writeFileSync(join(dir, "artworld.csv"), csv);

// ── LaTeX → PDF (xelatex; the repo's papers TeX, no pandoc needed) ──────────
// tx() is ONLY ever applied to raw text, never to strings that already
// contain control sequences — the old code double-escaped its own \textbf
// and \# and shipped "\textbackslash{}textbf" garbage. Order matters:
// backslash first, then the rest.
const tx = (s) =>
  String(s ?? "")
    .replace(/\\/g, "\\textbackslash{}")
    .replace(/([&%$#_{}])/g, "\\$1")
    .replace(/~/g, "\\textasciitilde{}")
    .replace(/\^/g, "\\textasciicircum{}");

// Markdown prose → LaTeX. Escapes raw text FIRST, *then* re-introduces
// \textbf / headings / itemize on the already-safe text (the `**`, `##`,
// `- ` markers are punctuation tx() leaves untouched, so this is sound).
function mdToTex(src) {
  const linesOut = [];
  let inList = false;
  const flush = () => {
    if (inList) {
      linesOut.push("\\end{itemize}");
      inList = false;
    }
  };
  for (let raw of src.split("\n")) {
    raw = raw.trim();
    if (!raw || /^# /.test(raw) || /^\*.*\*$/.test(raw)) {
      flush();
      continue;
    } // skip title + the *generated* italic line
    if (/^## /.test(raw)) {
      flush();
      linesOut.push(`\\subsection*{${tx(raw.slice(3))}}`);
      continue;
    }
    const esc = (t) =>
      tx(t).replace(/\*\*(.+?)\*\*/g, "\\textbf{$1}").replace(/`(.+?)`/g, "\\texttt{$1}");
    if (/^- /.test(raw)) {
      if (!inList) {
        linesOut.push("\\begin{itemize}\\setlength\\itemsep{2pt}");
        inList = true;
      }
      linesOut.push(`\\item ${esc(raw.slice(2))}`);
      continue;
    }
    flush();
    linesOut.push(esc(raw) + "\\par");
  }
  flush();
  return linesOut.join("\n");
}

// Handle that can wrap instead of punching into the margin. Built
// char-by-char so a breakpoint can NEVER land inside a control sequence
// (the previous regex sliced its own \allowbreak into \all + owbreak →
// "Undefined control sequence"). \zwsp = \discretionary{}{}{} (bulletproof
// zero-width breakpoint, works in TU/ttfamily). seqsplit isn't installed.
function hdl(u) {
  let s = "";
  let run = 0;
  for (const ch of String(u)) {
    if (ch === "_") {
      s += "\\_\\zwsp ";
      run = 0;
    } else if (ch === "." || ch === "-") {
      s += `${ch}\\zwsp `;
      run = 0;
    } else if (/[A-Za-z0-9]/.test(ch)) {
      s += ch;
      if (++run >= 6) {
        s += "\\zwsp ";
        run = 0;
      }
    } else {
      s += tx(ch); // rare in IG handles; escape + treat as a break boundary
      run = 0;
    }
  }
  return `{\\footnotesize\\ttfamily @${s}}`;
}

// Confidence colour: 3 methods = solid, 2 = ok, 1 = soft lead, 0 = grey.
function confTex(n) {
  const c = (n.confirmed_by || []).length;
  const v = tx(conf(n));
  if (c >= 3) return `\\textcolor{c3}{\\textbf{${v}}}`;
  if (c === 2) return `\\textcolor{c2}{${v}}`;
  if (c === 1) return `\\textcolor{c1}{${v}}`;
  return `\\textcolor{c0}{${v}}`;
}

// One colour-coded longtable. `tint` keys the zebra + header bar colour
// (matches the \definecolor names in the preamble: museum/gallery/…).
const longtable = (title, count, tint, head, widths, rows) => {
  if (!rows.length) return "";
  const colspec = widths
    .map((w) =>
      w.startsWith("p{") ? `>{\\raggedright\\arraybackslash}${w}` : w,
    )
    .join("");
  const hrow =
    head.map((h) => `\\textbf{${tx(h)}}`).join(" & ") + " \\\\";
  let s = `\\par\\vspace{8pt}\\noindent`;
  s += `\\colorbox{${tint}}{\\color{white}\\textbf{ ${tx(title)} }}`;
  s += `\\hfill{\\small ${count} entries}\\par\\vspace{3pt}\n`;
  s += `{\\footnotesize\\rowcolors{2}{${tint}!8}{white}\n`;
  s += `\\begin{longtable}{${colspec}}\n\\toprule\n${hrow}\n\\midrule\n`;
  s += `\\endfirsthead\n\\toprule\n${hrow}\n\\midrule\n\\endhead\n`;
  s += `\\bottomrule\n\\endlastfoot\n`;
  s += rows.map((r) => r.join(" & ") + " \\\\").join("\n") + "\n";
  s += `\\end{longtable}}\n`;
  return s;
};

const proseTex = mdToTex(out.split("## Museums")[0]);
// Pass RAW markdown (incl. the raw "&" heading) — mdToTex escapes it.
const caveatsTex = mdToTex(
  "## Confidence & known noise\n" +
    (out.split("## Confidence & known noise")[1] || ""),
);

let tex = `\\documentclass[9pt]{extarticle}
\\usepackage[landscape,margin=1.4cm]{geometry}
\\usepackage{fontspec}
\\setmainfont{Arial}
\\usepackage{longtable,booktabs,array}
\\usepackage[table]{xcolor}
\\usepackage[hidelinks]{hyperref}
\\definecolor{museum}{HTML}{1f6feb}
\\definecolor{institution}{HTML}{7b3ff2}
\\definecolor{gallery}{HTML}{e8482c}
\\definecolor{art_media}{HTML}{0fa36b}
\\definecolor{curator}{HTML}{f2a900}
\\definecolor{artist}{HTML}{d6336c}
\\definecolor{misc}{HTML}{555555}
\\definecolor{c3}{HTML}{1a7f37}
\\definecolor{c2}{HTML}{1f6feb}
\\definecolor{c1}{HTML}{b35900}
\\definecolor{c0}{HTML}{8a8f98}
\\newcommand{\\zwsp}{\\discretionary{}{}{}}
\\setlength{\\parskip}{4pt}\\setlength{\\parindent}{0pt}
\\setlength{\\tabcolsep}{4pt}\\renewcommand{\\arraystretch}{1.12}
\\begin{document}
\\begin{center}{\\LARGE\\textbf{@${tx(account)} — the contemporary art world it follows}}\\\\[3pt]
{\\small Generated ${tx(g.generated)} \\quad·\\quad ${art.length} art-world of ${total} followed \\quad·\\quad ${aff.length} affiliation links}\\end{center}
\\vspace{6pt}
${proseTex}
\\clearpage
`;

const W = ["p{0.7cm}", "p{3.0cm}", "p{4.0cm}", "p{1.2cm}", "p{2.8cm}", "p{11.4cm}"];
const HEAD = ["#", "handle", "name", "reach", "confirmed", "what it is"];
for (const t of order) {
  const list = grouped[t];
  if (!list.length) continue;
  tex += longtable(
    LABEL[t],
    list.length,
    t,
    HEAD,
    W,
    list.map((n, i) => [
      i + 1,
      hdl(n.id),
      tx(n.label),
      reachStr(n.reach),
      confTex(n),
      tx(desc(n)),
    ]),
  );
  tex += "\\clearpage\n";
}
tex += longtable(
  "Who runs / works at what",
  aff.length,
  "curator",
  ["person / org", "affiliated with", "source"],
  ["p{5cm}", "p{13cm}", "p{4cm}"],
  aff.map((e) => [
    hdl(e.source),
    tx(e.target),
    `{\\footnotesize ${tx(`${e.via}${e.role ? " / " + e.role : ""}`)}}`,
  ]),
);
tex += "\\clearpage\n";
tex += longtable(
  "Found only by Wikidata — what the heuristic & IG both missed",
  webOnly.length,
  "artist",
  ["handle", "name", "wikidata says"],
  ["p{4.5cm}", "p{5.5cm}", "p{13cm}"],
  webOnly
    .slice()
    .sort((a, b) => (b.reach || 0) - (a.reach || 0))
    .map((n) => [hdl(n.id), tx(n.label), tx(n.web_description || "")]),
);
tex += `\\clearpage\n${caveatsTex}\n\\end{document}\n`;
writeFileSync(join(dir, "REPORT.tex"), tex);

// ── ONE-PAGE BROADSIDE (ONEPAGER.tex → ONEPAGER.pdf) ───────────────────────
// A single designed sheet: masthead, lede, the three-lens overlap panel,
// the art world by category (top names, dense multicol), who-runs-what,
// colophon. The exhaustive 529-row line-by-line stays in artworld.csv.
const H = (n) => (n.confirmed_by || []).includes("heuristic");
const I = (n) => (n.confirmed_by || []).includes("ig_category");
const Wd = (n) => (n.confirmed_by || []).includes("wikidata");
const ov = { Ho: 0, Io: 0, Wo: 0, HI: 0, HW: 0, IW: 0, HIW: 0 };
for (const n of art) {
  const h = H(n), i = I(n), w = Wd(n);
  if (h && i && w) ov.HIW++;
  else if (h && i) ov.HI++;
  else if (h && w) ov.HW++;
  else if (i && w) ov.IW++;
  else if (h) ov.Ho++;
  else if (i) ov.Io++;
  else if (w) ov.Wo++;
}
const trunc = (s, n2) => {
  s = String(s || "");
  return s.length > n2 ? s.slice(0, n2 - 1) + "…" : s;
};
const CAP = {
  museum: 99,
  institution: 99,
  art_media: 99,
  curator: 26,
  gallery: 30,
  artist: 28,
};
function nameList(t) {
  const list = grouped[t];
  const cap = CAP[t];
  const shown = list.slice(0, cap).map((n) => tx(trunc(n.label, 24)));
  let s = `\\par\\smallskip\\noindent\\colorbox{${t}}{\\color{white}\\textbf{ ${tx(
    LABEL[t],
  )} }}\\,{\\footnotesize\\textbf{${list.length}}}\\par\\smallskip\n`;
  s += `{\\scriptsize\\raggedright ${shown.join(" \\,·\\, ")}`;
  if (list.length > cap)
    s += ` \\,·\\, {\\itshape +${list.length - cap} more in artworld.csv}`;
  s += `\\par}\n`;
  return s;
}
const lede =
  `@${tx(account)} follows {\\bfseries ${total}} accounts; ` +
  `{\\bfseries ${art.length}} are art world — ${order
    .map((t) => `${grouped[t].length}~${t.replace("_", " ")}`)
    .join(", ")} — laced together by {\\bfseries ${aff.length}} ` +
  `\\textit{who-runs-what} links. Every entry is tagged by which of three ` +
  `independent lenses caught it: a name/handle heuristic, Instagram's own ` +
  `business category, and Wikidata occupation/instance records. Their ` +
  `overlap is the map's confidence:`;
const affTop = aff
  .filter((e) => e.role || e.via === "wikidata")
  .slice(0, 9)
  .map(
    (e) =>
      `{\\ttfamily\\scriptsize @${tx(e.source)}} \\,$\\rightarrow$\\, ${tx(
        trunc(e.target, 34),
      )}${e.role ? ` {\\scriptsize\\itshape (${tx(e.role)})}` : ""}`,
  )
  .join("\\\\\n");

const one = `\\documentclass[9pt]{extarticle}
\\usepackage[a4paper,margin=1.05cm]{geometry}
\\usepackage{fontspec}\\setmainfont{Arial}
\\usepackage[table]{xcolor}\\usepackage{multicol,microtype}
\\definecolor{museum}{HTML}{1f6feb}\\definecolor{institution}{HTML}{7b3ff2}
\\definecolor{gallery}{HTML}{e8482c}\\definecolor{art_media}{HTML}{0fa36b}
\\definecolor{curator}{HTML}{f2a900}\\definecolor{artist}{HTML}{d6336c}
\\definecolor{ink}{HTML}{14181f}\\definecolor{spine}{HTML}{1a7f37}
\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0pt}
\\pagestyle{empty}\\color{ink}
\\begin{document}
\\noindent\\rule{\\textwidth}{2pt}\\par\\vspace{2pt}
\\begin{center}
{\\fontsize{23}{25}\\selectfont\\textbf{THE ART WORLD OF @${tx(account)}}}\\\\[2pt]
{\\footnotesize A BROADSIDE MAP OF THE CONTEMPORARY GALLERY WORLD ONE ACCOUNT INHABITS \\,·\\, ${tx(
  g.generated,
)}}
\\end{center}
\\vspace{1pt}\\noindent\\rule{\\textwidth}{0.6pt}\\par\\vspace{5pt}
{\\small ${lede}}\\par\\vspace{6pt}

\\noindent\\begin{minipage}{\\textwidth}
\\setlength{\\fboxsep}{5pt}\\centering
\\colorbox{spine}{\\color{white}\\textbf{~THE SPINE — confirmed by all three: ${
  ov.HIW
}~}}\\quad
\\fbox{heuristic only: \\textbf{${ov.Ho}}}\\quad
\\fbox{IG-category only: \\textbf{${ov.Io}}}\\quad
\\fbox{Wikidata only: \\textbf{${ov.Wo}}}\\\\[3pt]
\\fbox{heuristic\\,$\\cap$\\,IG: \\textbf{${ov.HI}}}\\quad
\\fbox{heuristic\\,$\\cap$\\,Wikidata: \\textbf{${ov.HW}}}\\quad
\\fbox{IG\\,$\\cap$\\,Wikidata: \\textbf{${ov.IW}}}
\\end{minipage}\\par\\vspace{7pt}
\\noindent\\rule{\\textwidth}{0.6pt}\\par\\vspace{3pt}

\\begin{multicols}{2}
${order.map(nameList).join("\n")}
\\columnbreak
\\par\\smallskip\\noindent\\colorbox{curator}{\\color{white}\\textbf{ WHO RUNS / WORKS AT WHAT }}\\par\\smallskip
{\\raggedright ${affTop}\\par}
\\par\\smallskip {\\scriptsize\\itshape ${aff.length} affiliation links total — full list in artworld.csv}\\par
\\end{multicols}

\\vfill\\noindent\\rule{\\textwidth}{0.6pt}\\par\\vspace{2pt}
{\\scriptsize A node is art-world if {\\bfseries any} lens agrees; the spine (${
  ov.HIW
}) is triple-confirmed. Reach = follower count, an attention proxy, not importance. Single-lens rows are leads, not facts. Exhaustive 529-row line-by-line + every affiliation: {\\ttfamily artworld.csv}. Full detail tables: {\\ttfamily REPORT.pdf}. Pipeline: \\texttt{portraits/jeffrey/bin/ig-social-*} \\,·\\, validated by \\texttt{ig-social-validate.mjs}.}
\\end{document}
`;
writeFileSync(join(dir, "ONEPAGER.tex"), one);

console.error(
  `report: ${art.length} art-world nodes, ${aff.length} affiliations\n` +
    `→ ${join(dir, "REPORT.md")}\n→ ${join(dir, "artworld.csv")}\n` +
    `→ ${join(dir, "REPORT.tex")} (xelatex → REPORT.pdf)\n` +
    `→ ${join(dir, "ONEPAGER.tex")} (xelatex → ONEPAGER.pdf, 1-page broadside)`,
);
