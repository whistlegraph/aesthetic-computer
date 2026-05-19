#!/usr/bin/env node
// web-to-pdf — turn a client-rendered Cargo.site article into a clean,
// typeset, application-grade PDF (selectable text, full-resolution images).
//
// Why this exists: Cargo.site pages (4nmag.com, many artist/press sites,
// fiabenitez.com, etc.) are JS-rendered shells. Printing them directly
// clips text off the page edge and the artwork lazy-loads as background
// elements that never appear in a capture. But the *initial* HTML embeds
// `window.__PRELOADED_STATE__` — the full content tree + media manifest —
// before the client app nulls it. We pull the article straight from there,
// rebuild it as semantic HTML, fetch the images from Cargo's freight CDN
// at print resolution, and let headless Chrome paginate our own clean
// print stylesheet.
//
// Usage:
//   tools/web-to-pdf.mjs <url> [--out DIR] [--name BASENAME]
//                              [--img-width PX] [--kicker "TEXT"]
//
// Example:
//   tools/web-to-pdf.mjs https://4nmag.com/fia-benitez --out ~/Desktop
//
// Output: <out>/<name>.pdf  (+ <out>/<name>-img/ with the source images)
//
// Dependencies: node + the monorepo's puppeteer-core (for Chrome only).
// No ImageMagick needed — Cargo's freight CDN resizes server-side.

import { writeFile, mkdir } from "node:fs/promises";
import { existsSync, readdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(fileURLToPath(import.meta.url), "../..");

// ---- args ----------------------------------------------------------------
const argv = process.argv.slice(2);
const url = argv.find((a) => !a.startsWith("--"));
const opt = (k, d) => {
  const i = argv.indexOf(`--${k}`);
  return i !== -1 && argv[i + 1] ? argv[i + 1] : d;
};
if (!url || argv.includes("--help")) {
  console.log(
    "usage: web-to-pdf <url> [--out DIR] [--name BASENAME] [--img-width PX] [--kicker TEXT]"
  );
  process.exit(url ? 0 : 1);
}
const outDir = resolve(opt("out", ".").replace(/^~/, homedir()));
const imgWidth = parseInt(opt("img-width", "1800"), 10);
const kickerOverride = opt("kicker", null);
const purl = new URL(url).pathname.split("/").filter(Boolean).pop() || "index";
const baseName = opt("name", null);

const UA =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36";

// ---- 1. fetch raw HTML, extract __PRELOADED_STATE__ ----------------------
const html = await (await fetch(url, { headers: { "User-Agent": UA } })).text();
const anchor = html.indexOf("window.__PRELOADED_STATE__");
if (anchor === -1) {
  console.error(
    "No __PRELOADED_STATE__ found — this tool targets client-rendered Cargo.site pages."
  );
  process.exit(2);
}
let j = html.indexOf("{", anchor);
let depth = 0;
let k = j;
for (; k < html.length; k++) {
  const c = html[k];
  if (c === "{") depth++;
  else if (c === "}" && --depth === 0) break;
}
const state = JSON.parse(html.slice(j, k + 1));

const page = Object.values(state.pages?.byId || {}).find((p) => p.purl === purl);
if (!page) {
  console.error(`No page with purl "${purl}" in preloaded state.`);
  process.exit(3);
}
const siteTitle = state.site?.website_title || "";
const name = baseName || page.title || purl;

// Freight serves heavy PNGs; recompress to JPEG with ImageMagick if present.
const magick = ["magick", "convert"].find(
  (c) => spawnSync(c, ["-version"], { stdio: "ignore" }).status === 0
);
const ext = magick ? "jpg" : "png";

// ---- 2. clean Cargo content → semantic HTML ------------------------------
let raw = page.content || "";
const figures = [];
raw = raw.replace(
  /<media-item[^>]*\bhash="(\w+)"[^>]*>([\s\S]*?)<\/media-item>/g,
  (_, hash, inner) => {
    const cap = (inner.match(/<figcaption[^>]*>([\s\S]*?)<\/figcaption>/) || [, ""])[1].trim();
    figures.push({ hash, cap });
    return `\n\n[[FIG:${figures.length - 1}]]\n\n`;
  }
);
raw = raw
  .split('<span class="menu">')[0]
  .replace(/<\/?column-set[^>]*>/g, "")
  .replace(/<\/?column-unit[^>]*>/g, "")
  .replace(/<text-icon[^>]*><\/text-icon>/g, "")
  .replace(/\sstyle="[^"]*"/g, "")
  .replace(/\sclass="[^"]*"/g, "")
  .replace(/<\/?span[^>]*>/g, "")
  .replace(/<\/?div[^>]*>/g, "")
  .replace(/<hr\s*\/?>/g, "")
  .replace(/&nbsp;/g, " ")
  .replace(/\s*<br\s*\/?>\s*/g, "\n")
  .replace(/[ \t]+/g, " ")
  // force a paragraph break before short bold labels ("KR:", "Fía Benítez:")
  .replace(/<b>\s*([^<:]{1,42}:)/g, "\n\n<b>$1")
  .replace(/\n{3,}/g, "\n\n");

const h1 = (raw.match(/<h1>([\s\S]*?)<\/h1>/) || [, ""])[1].replace(/\s+/g, " ").trim();
raw = raw.replace(/<h1>[\s\S]*?<\/h1>/, "").trim();

// Balance b/i/a within each block so styles never leak across <p> bounds
// (HTML's active-formatting reconstruction otherwise drags an unclosed
//  <i> through every following paragraph).
function balance(s) {
  const out = [];
  const stack = [];
  const re = /<(\/?)(b|i|a)\b([^>]*)>|([^<]+)/g;
  let m;
  while ((m = re.exec(s))) {
    if (m[4] !== undefined) {
      out.push(m[4]);
      continue;
    }
    const tag = m[2];
    if (m[1] !== "/") {
      out.push(`<${tag}${m[3]}>`);
      stack.push(tag);
    } else {
      const i = stack.lastIndexOf(tag);
      if (i === -1) continue;
      for (let x = stack.length - 1; x >= i; x--) out.push(`</${stack[x]}>`);
      const reopen = stack.splice(i);
      reopen.shift();
      for (const t of reopen) {
        out.push(`<${t}>`);
        stack.push(t);
      }
    }
  }
  while (stack.length) out.push(`</${stack.pop()}>`);
  return out.join("");
}

let body = "";
let intro = true;
let bylineDone = false;
for (let b of raw.split(/\n{2,}/).map((x) => x.trim()).filter(Boolean)) {
  const fig = b.match(/^\[\[FIG:(\d+)\]\]$/);
  if (fig) {
    const { hash, cap } = figures[+fig[1]];
    body += `<figure><img src="${name}-img/${hash}.${ext}" alt=""><figcaption>${balance(cap)}</figcaption></figure>\n`;
    continue;
  }
  b = b.replace(/\[\[FIG:\d+\]\]/g, "").trim();
  const flat = b.replace(/<[^>]+>/g, "").replace(/\s+/g, " ").trim();
  if (!flat) continue;
  if (!bylineDone && /^[A-Z][a-z]+ \d{1,2},? \d{4}/.test(flat)) {
    const m = flat.match(/^(.*?\d{4})\s*by\s+(.+)$/i);
    body += `<p class="byline">${
      m ? `${m[1].trim()} &nbsp;·&nbsp; By ${m[2].trim()}` : flat
    }</p>\n`;
    bylineDone = true;
    continue;
  }
  if (/:\s*$/.test(flat) === false && /^[A-Z][A-Za-z. ]+:/.test(flat)) intro = false;
  const cls = !bylineDone ? "lede" : intro ? "lede" : "copy";
  body += `<p class="${cls}">${balance(b).replace(/\s+/g, " ").trim()}</p>\n`;
}

// ---- 3. download images from the freight CDN at print resolution --------
// Freight always serves the master's format (often heavy PNG). Recompress
// to JPEG with ImageMagick when available; otherwise keep the PNG.
const imgDir = join(outDir, `${name}-img`);
await mkdir(imgDir, { recursive: true });
const mediaByHash = Object.fromEntries((page.media || []).map((m) => [m.hash, m]));
for (const { hash } of figures) {
  const m = mediaByHash[hash];
  const fname = m?.name || hash;
  const src = `https://freight.cargo.site/w/${imgWidth}/i/${hash}/${fname}`;
  const buf = Buffer.from(
    await (await fetch(src, { headers: { "User-Agent": UA } })).arrayBuffer()
  );
  const dest = join(imgDir, `${hash}.${ext}`);
  if (magick) {
    const orig = join(imgDir, `${hash}.orig`);
    await writeFile(orig, buf);
    spawnSync(magick, [orig, "-resize", `${imgWidth}x`, "-quality", "88", "-strip", dest], {
      stdio: "ignore",
    });
    spawnSync("rm", ["-f", orig]);
  } else {
    await writeFile(dest, buf);
  }
}
if (!magick)
  console.warn("note: ImageMagick not found — images kept as PNG (larger PDF).");

// ---- 4. typeset --------------------------------------------------------
const kicker = kickerOverride || (siteTitle ? `${siteTitle}` : "");
const docHtml = `<!doctype html><html><head><meta charset="utf-8"><style>
@page { size: Letter; margin: 0.95in 0.9in 0.85in; }
* { box-sizing: border-box; }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { font: 11.5pt/1.62 "Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif; color:#1a1a1a; margin:0; }
.kicker { font:600 9pt/1.35 ui-sans-serif,"Helvetica Neue",Arial,sans-serif; letter-spacing:.2em; text-transform:uppercase; color:#8a8a8a; margin:0 0 1rem; }
h1 { font:600 25pt/1.18 "Iowan Old Style",Georgia,serif; margin:0 0 1.4rem; letter-spacing:-.01em; max-width:26ch; }
h1 i { font-style:italic; }
.byline { font:600 9.5pt/1.4 ui-sans-serif,"Helvetica Neue",Arial,sans-serif; letter-spacing:.07em; text-transform:uppercase; color:#767676; margin:1.5rem 0 1.7rem; padding-bottom:1.05rem; border-bottom:1px solid #d8d8d8; }
p { margin:0 0 1.05rem; orphans:2; widows:2; }
p.lede { font-size:12.5pt; line-height:1.6; color:#2e2e2e; }
p.lede i { font-style:italic; }
p.copy b { font-weight:600; }
a { color:inherit; text-decoration:none; border-bottom:1px solid #bcbcbc; }
figure { margin:1.9rem 0 2.1rem; text-align:center; break-inside:avoid; page-break-inside:avoid; }
figure img { max-width:100%; max-height:7.4in; height:auto; border:1px solid #ececec; }
figcaption { font:italic 9.5pt/1.5 "Iowan Old Style",Georgia,serif; color:#6b6b6b; margin:.75rem auto 0; max-width:46ch; }
figcaption i { font-style:normal; }
.source { margin-top:2.4rem; padding-top:1rem; border-top:1px solid #d8d8d8; font:9pt/1.5 ui-sans-serif,"Helvetica Neue",Arial,sans-serif; color:#8a8a8a; }
</style></head><body>
${kicker ? `<p class="kicker">${kicker}</p>` : ""}
<h1>${h1 || page.title}</h1>
${body}
<p class="source">Originally published at ${url}</p>
</body></html>`;

const htmlPath = join(outDir, `${name}.html`);
await writeFile(htmlPath, docHtml);

// ---- 5. render via headless Chrome -------------------------------------
const { default: puppeteer } = await import(
  join(ROOT, "node_modules/puppeteer-core/lib/esm/puppeteer/puppeteer-core.js")
);
const chromeCandidates = [
  process.env.CHROME_PATH,
  ...[`${homedir()}/.cache/puppeteer/chrome`].flatMap((d) =>
    existsSync(d)
      ? readdirSync(d).map(
          (v) =>
            `${d}/${v}/chrome-mac-arm64/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing`
        )
      : []
  ),
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
].filter(Boolean);
const executablePath = chromeCandidates.find((p) => p && existsSync(p));
if (!executablePath) {
  console.error("No Chrome found. Set CHROME_PATH=… and retry.");
  process.exit(4);
}
const browser = await puppeteer.launch({
  executablePath,
  headless: true,
  args: ["--no-sandbox", "--force-color-profile=srgb"],
});
const tab = await browser.newPage();
await tab.goto(`file://${htmlPath}`, { waitUntil: "networkidle0" });
const pdfPath = join(outDir, `${name}.pdf`);
await tab.pdf({
  path: pdfPath,
  format: "Letter",
  printBackground: true,
  margin: { top: 0, right: 0, bottom: 0, left: 0 },
  preferCSSPageSize: true,
});
await browser.close();

console.log(`✓ ${pdfPath}`);
console.log(`  ${figures.length} images · source: ${url}`);
