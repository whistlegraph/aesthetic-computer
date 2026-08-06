import { readFile } from "node:fs/promises";
import puppeteer from "puppeteer";

const [input, output] = process.argv.slice(2);
if (!input || !output) throw new Error("usage: node render-piecefarm-report.mjs input.md output.pdf");

const escape = (value) => value.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
const inline = (value) => escape(value)
  .replace(/`([^`]+)`/g, "<code>$1</code>")
  .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>")
  .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2">$1</a>');

function markdown(source) {
  const lines = source.split(/\r?\n/);
  const out = [];
  let paragraph = [];
  let list = null;
  let code = null;
  let codeLanguage = "";
  const flushParagraph = () => {
    if (paragraph.length) out.push(`<p>${inline(paragraph.join(" "))}</p>`);
    paragraph = [];
  };
  const flushList = () => {
    if (list) out.push(`</${list}>`);
    list = null;
  };
  const openList = (kind) => {
    flushParagraph();
    if (list !== kind) { flushList(); out.push(`<${kind}>`); list = kind; }
  };
  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index];
    if (code) {
      if (line.startsWith("```")) {
        out.push(`<pre class="${codeLanguage === "mermaid" ? "diagram" : ""}"><code>${escape(code.join("\n"))}</code></pre>`);
        code = null; codeLanguage = "";
      } else code.push(line);
      continue;
    }
    if (line.startsWith("```")) {
      flushParagraph(); flushList(); code = []; codeLanguage = line.slice(3).trim(); continue;
    }
    const heading = /^(#{1,4})\s+(.+)$/.exec(line);
    if (heading) {
      flushParagraph(); flushList();
      const level = heading[1].length;
      out.push(`<h${level}>${inline(heading[2])}</h${level}>`);
      continue;
    }
    const unordered = /^-\s+(.+)$/.exec(line);
    if (unordered) { openList("ul"); out.push(`<li>${inline(unordered[1])}</li>`); continue; }
    const ordered = /^\d+\.\s+(.+)$/.exec(line);
    if (ordered) { openList("ol"); out.push(`<li>${inline(ordered[1])}</li>`); continue; }
    if (line.startsWith("|") && lines[index + 1]?.match(/^\|[-:| ]+\|$/)) {
      flushParagraph(); flushList();
      const headers = line.slice(1, -1).split("|").map((cell) => cell.trim());
      index += 1;
      const rows = [];
      while (lines[index + 1]?.startsWith("|")) {
        index += 1;
        rows.push(lines[index].slice(1, -1).split("|").map((cell) => cell.trim()));
      }
      out.push(`<table><thead><tr>${headers.map((cell) => `<th>${inline(cell)}</th>`).join("")}</tr></thead><tbody>${rows.map((row) => `<tr>${row.map((cell) => `<td>${inline(cell)}</td>`).join("")}</tr>`).join("")}</tbody></table>`);
      continue;
    }
    if (!line.trim()) { flushParagraph(); flushList(); continue; }
    paragraph.push(line.trim());
  }
  flushParagraph(); flushList();
  return out.join("\n");
}

const source = await readFile(input, "utf8");
const body = markdown(source);
const html = `<!doctype html><html><head><meta charset="utf-8"><style>
  @page { size: Letter; margin: 0.66in 0.7in 0.72in; }
  :root { --ink:#15201e; --muted:#57706a; --mint:#169c87; --gold:#d99b12; --pink:#c8499e; --paper:#f8f7f0; }
  * { box-sizing: border-box; }
  body { margin:0; background:var(--paper); color:var(--ink); font-family:-apple-system,BlinkMacSystemFont,"Helvetica Neue",Arial,sans-serif; font-size:10pt; line-height:1.39; }
  h1 { color:#07110f; font-size:31pt; line-height:1.02; letter-spacing:-1.3px; margin:0 0 13pt; padding:0 0 17pt; border-bottom:5px solid var(--gold); }
  h1 + p { color:var(--muted); font-size:12pt; line-height:1.45; margin-bottom:28pt; }
  h2 { color:#073c34; font-size:18pt; line-height:1.1; margin:21pt 0 8pt; padding-top:3pt; break-after:avoid; }
  h3 { color:#31524b; font-size:13pt; margin:15pt 0 5pt; break-after:avoid; }
  p { margin:0 0 8pt; orphans:3; widows:3; }
  ul,ol { margin:3pt 0 9pt 20pt; padding:0; }
  li { margin:0 0 3.5pt; padding-left:2pt; }
  strong { color:#073c34; }
  code { font-family:"SFMono-Regular",Menlo,Consolas,monospace; font-size:0.88em; color:#7e285f; background:#eee8df; padding:1px 3px; border-radius:3px; }
  pre { white-space:pre-wrap; break-inside:avoid; background:#091815; color:#dff4ed; border-left:5px solid var(--mint); padding:10pt 12pt; border-radius:4px; font:8.3pt/1.4 "SFMono-Regular",Menlo,monospace; margin:10pt 0 14pt; }
  pre code { color:inherit; background:none; padding:0; }
  pre.diagram { color:#dff4ed; border-left-color:var(--pink); font-size:7.4pt; }
  table { width:100%; border-collapse:collapse; margin:10pt 0 16pt; font-size:9.5pt; break-inside:avoid; }
  th { text-align:left; color:white; background:#164e43; padding:6pt 7pt; }
  td { padding:5pt 7pt; border-bottom:1px solid #c9d3ce; }
  td:last-child, th:last-child { text-align:right; }
  a { color:var(--mint); text-decoration:none; }
  h2:nth-of-type(3n+1) { border-left:4px solid var(--pink); padding-left:9pt; }
  h2:nth-of-type(3n+2) { border-left:4px solid var(--mint); padding-left:9pt; }
  h2:nth-of-type(3n) { border-left:4px solid var(--gold); padding-left:9pt; }
  .colophon { margin-top:55pt; padding:22pt 25pt; background:#091815; color:#dff4ed; border-top:7px solid var(--gold); break-inside:avoid; }
  .colophon .name { color:white; font-size:25pt; font-weight:800; letter-spacing:-0.6px; }
  .colophon .stats { color:#8ee1cf; font:10pt/1.6 "SFMono-Regular",Menlo,monospace; margin-top:8pt; }
  .colophon .next { color:#f1c855; font-size:12pt; font-weight:700; margin-top:15pt; }
</style></head><body>${body}<div class="colophon"><div class="name">PIECEFARM / SEASON ONE</div><div class="stats">57,398 ITERATIONS · 10,768 ACCEPTED · 62 GIT EDITIONS<br>143.6 FPS · ~195 MB · 12 LIVE ADDRESSES · 7 VISUAL REVIEWS</div><div class="next">STATUS: OFF &amp; PRESERVED<br>NEXT FRONTIER: HEREDITY</div></div></body></html>`;

const browser = await puppeteer.launch({
  headless: true,
  executablePath: process.env.PUPPETEER_EXECUTABLE_PATH || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
});
try {
  const page = await browser.newPage();
  await page.setContent(html, { waitUntil: "load" });
  await page.pdf({
    path: output,
    format: "Letter",
    printBackground: true,
    displayHeaderFooter: true,
    headerTemplate: '<div style="font:8px -apple-system;color:#73827e;width:100%;padding:0 0.7in;text-align:right">AESTHETIC COMPUTER / PIECEFARM</div>',
    footerTemplate: '<div style="font:8px -apple-system;color:#73827e;width:100%;padding:0 0.7in;display:flex;justify-content:space-between"><span>JULY 23–24, 2026</span><span><span class="pageNumber"></span> / <span class="totalPages"></span></span></div>',
    margin: { top: "0.7in", right: "0.7in", bottom: "0.72in", left: "0.7in" },
  });
} finally {
  await browser.close();
}
