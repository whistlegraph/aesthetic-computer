import fs from "node:fs";

const src = fs.readFileSync(process.argv[2], "utf8");
const lines = src.split("\n");

const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
function inline(s) {
  s = esc(s);
  s = s.replace(/`([^`]+)`/g, (_, c) => `<code>${c}</code>`);
  s = s.replace(/\[([^\]]+)\]\(([^)]+)\)/g, (_, t, u) => `<a href="${u}">${t}</a>`);
  s = s.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  s = s.replace(/(^|[^*])\*([^*]+)\*(?!\*)/g, "$1<em>$2</em>");
  return s;
}

let html = "", inList = null, para = [], inBq = false, bq = [];
const flushPara = () => { if (para.length) { html += `<p>${inline(para.join(" "))}</p>\n`; para = []; } };
const flushList = () => { if (inList) { html += `</${inList}>\n`; inList = null; } };
const flushBq = () => { if (inBq) { html += `<blockquote>${bq.map(l => inline(l)).join("<br>")}</blockquote>\n`; bq = []; inBq = false; } };

for (let raw of lines) {
  const line = raw.replace(/\s+$/,"");
  if (/^>\s?/.test(line)) { flushPara(); flushList(); inBq = true; bq.push(line.replace(/^>\s?/, "")); continue; }
  flushBq();
  if (line.trim() === "") { flushPara(); flushList(); continue; }
  if (/^---+$/.test(line)) { flushPara(); flushList(); html += "<hr>\n"; continue; }
  let m;
  if ((m = line.match(/^(#{1,4})\s+(.*)$/))) { flushPara(); flushList(); const lv = m[1].length; html += `<h${lv}>${inline(m[2])}</h${lv}>\n`; continue; }
  if ((m = line.match(/^\s*[-*]\s+(.*)$/))) {
    flushPara();
    if (inList !== "ul") { flushList(); html += "<ul>\n"; inList = "ul"; }
    let item = m[1];
    item = item.replace(/^\[ \]\s*/, "☐ ").replace(/^\[x\]\s*/i, "☑ ");
    html += `<li>${inline(item)}</li>\n`; continue;
  }
  if ((m = line.match(/^\s*\d+\.\s+(.*)$/))) {
    flushPara();
    if (inList !== "ol") { flushList(); html += "<ol>\n"; inList = "ol"; }
    html += `<li>${inline(m[1])}</li>\n`; continue;
  }
  para.push(line.trim());
}
flushPara(); flushList(); flushBq();

const doc = `<!DOCTYPE html><html><head><meta charset="utf-8"><style>
@page { size: letter; margin: 0.7in; }
* { box-sizing: border-box; }
body { font-family: "Charter", "Georgia", "Iowan Old Style", serif; color: #1a1c20; font-size: 10.5px; line-height: 1.5; margin: 0; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
h1 { font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; font-size: 18px; margin: 0 0 6px; letter-spacing: -0.3px; border-bottom: 3px solid #14161a; padding-bottom: 6px; }
h2 { font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; font-size: 13px; margin: 18px 0 4px; color: #14161a; border-bottom: 1px solid #d8dbe0; padding-bottom: 3px; }
h2 em { font-style: normal; font-weight: 400; color: #7a818c; font-size: 11px; }
h3 { font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; font-size: 11px; margin: 12px 0 3px; color: #2a2d33; }
p { margin: 0 0 7px; }
ul, ol { margin: 0 0 8px; padding-left: 20px; }
li { margin-bottom: 3px; }
strong { color: #14161a; }
code { font-family: "SF Mono", "Menlo", monospace; font-size: 9px; background: #eef0f3; color: #b3344f; padding: 0.5px 4px; border-radius: 3px; }
a { color: #2c4ad8; text-decoration: none; word-break: break-all; }
hr { border: none; border-top: 1px solid #e3e6ea; margin: 14px 0; }
blockquote { background: #f4f6f9; border-left: 3px solid #2c4ad8; margin: 0 0 12px; padding: 8px 12px; font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; font-size: 9.5px; color: #41454d; border-radius: 0 4px 4px 0; }
blockquote code { background: #e6e9ef; }
h2 + p, h3 + p { margin-top: 2px; }
</style></head><body>
${html}
</body></html>`;
fs.writeFileSync(process.argv[3], doc);
console.log("wrote", process.argv[3]);
