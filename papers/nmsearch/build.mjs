#!/usr/bin/env node
// nmsearch build: field registry + local platter index → public/nmsearch/.
// Usage: node papers/nmsearch/build.mjs
import { readFileSync, writeFileSync, readdirSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

import { createRequire } from "node:module";
const { decode } = createRequire(new URL("../../system/package.json", import.meta.url))("he");

const here = dirname(fileURLToPath(import.meta.url));
const papers = join(here, "..");
const root = join(papers, "..");
const out = join(root, "system/public/papers.aesthetic.computer/nmsearch");

const field = JSON.parse(readFileSync(join(here, "field.json"), "utf8"));
const score = readFileSync(join(papers, "SCORE.md"), "utf8");

const strip = (s) =>
  s.replace(/\[([^\]]+)\]\([^)]+\)/g, "$1").replace(/[`*_]/g, "").replace(/\s+/g, " ").trim();

const local = [];

// Papers: parse the published index rows (title, subtitle, abstract, kind, date, PDF).
const pub = join(root, "system/public/papers.aesthetic.computer");
const index = readFileSync(join(pub, "index.html"), "utf8");
const unesc = (s) => decode(s.replace(/<[^>]+>/g, " ")).replace(/\s+/g, " ").trim();
const rowRe = /<tr data-status="([^"]*)" data-kind="([^"]*)" data-key="([^"]*)"[^>]*data-created="([^"]*)"[^>]*>([\s\S]*?)<\/tr>/g;
let m;
while ((m = rowRe.exec(index))) {
  const [, status, kind, key, created, body] = m;
  const href = body.match(/<a href="(\/[^"]+\.pdf)"/)?.[1];
  const title = body.match(/<span class="paper-title"><a [^>]*>([^<]+)<\/a>/)?.[1];
  if (!href || !title) continue;
  const dt = body.match(/<div class="dt">([\s\S]*?)<\/div>/)?.[1] || "";
  const abstract = body.match(/<div class="abstract">([\s\S]*?)<\/div>/)?.[1] || "";
  local.push({
    kind: "paper",
    title: unesc(title),
    note: unesc(dt),
    text: unesc(abstract),
    meta: `${kind} · ${status} · ${created}`,
    date: created,
    url: `https://papers.aesthetic.computer${href}`,
    id: key,
  });
}

// Sub-platters: section 8 of SCORE.md, one bullet each.
const platters = score.split("### 8. Platters")[1]?.split("\n### ")[0] || "";
for (const line of platters.split("\n")) {
  const m = line.match(/^- \[`?([^`\]]+?)`?\]\(([^)]+)\)\s*[—-]\s*(.+)$/);
  if (!m) continue;
  local.push({
    kind: "platter",
    title: m[1].replace(/\/$/, ""),
    note: strip(m[3]).slice(0, 320),
    url: m[2].startsWith("http") ? m[2] : `https://github.com/whistlegraph/aesthetic-computer/tree/main/papers/${m[2]}`,
    id: m[1].replace(/\/$/, ""),
  });
}

// Index the records offered by the public Platter, including its three generated lists.
const platter = readFileSync(join(pub, "platter.html"), "utf8");
const home = "https://papers.aesthetic.computer";
const indexedUrls = new Set(local.map((item) => item.url));
function platterEntry(section, href, title, note = "") {
  const url = new URL(decode(href), home).href;
  if (!/^https?:/.test(url) || !title || indexedUrls.has(url)) return;
  indexedUrls.add(url);
  local.push({ kind: "platter", title, note, meta: section, url, id: section });
}
const markup = platter.split("<script>")[0];
for (const match of markup.matchAll(/<div class="section" data-section="([^"]+)">([\s\S]*?)(?=<div class="section" data-section=|$)/g)) {
  const [, section, html] = match;
  for (const [, href, label] of html.matchAll(/<a class="item" href="([^"]+)"[^>]*>([\s\S]*?)<\/a>/g)) {
    platterEntry(section, href, unesc(label));
  }
  for (const [, body] of html.matchAll(/<div class="item-dual">([\s\S]*?)<\/div>/g)) {
    const title = body.match(/class="file">([\s\S]*?)<\/span>/)?.[1];
    const note = body.match(/class="label">([\s\S]*?)<\/span>/)?.[1];
    const href = body.match(/<a href="([^"]+)"/)?.[1];
    if (title && href) platterEntry(section, href, unesc(title), unesc(note || ""));
  }
}
for (const [, section, rows] of platter.matchAll(/const (reports|plans|studies) = \[([\s\S]*?)\n\s*\];/g)) {
  for (const line of rows.trim().split("\n")) {
    const [file, title] = JSON.parse(line.trim().replace(/,$/, ""));
    platterEntry(section, `https://github.com/whistlegraph/aesthetic-computer/blob/main/${section}/${file}`, title);
  }
}

// Sleuth sources ride along as "how to search" entries.
const sleuth = JSON.parse(readFileSync(join(papers, "sleuth/sources.json"), "utf8"));
for (const s of sleuth.sources) {
  local.push({
    kind: "sleuth",
    title: s.id,
    note: `${s.kind}: ${(s.mechanisms || []).join(", ")}`,
    url: "https://papers.aesthetic.computer/sleuth/",
    id: `sleuth:${s.id}`,
  });
}

// Hosted third-party PDFs on the papers host (e.g. Menkman) are field texts we mirror.
for (const f of readdirSync(pub)) {
  if (!f.endsWith(".pdf")) continue;
  if (/-26-/.test(f) || local.some((l) => l.url.endsWith("/" + f))) continue; // our own variants are already indexed
  local.push({
    kind: "pdf",
    title: f.replace(/\.pdf$/, "").replace(/-/g, " "),
    note: "hosted field text",
    url: `https://papers.aesthetic.computer/${f}`,
    id: f,
  });
}

local.push({ kind: "page", title: "mediascholar", note: "autonomous new-media paper worker, live progress", url: "https://papers.aesthetic.computer/scholar/", id: "scholar" });
local.push({ kind: "page", title: "deadlines", note: "open calls, residencies, grants and venues for the field", url: "https://papers.aesthetic.computer/deadlines/", id: "deadlines" });

function writeIndex(filename, data) {
  const path = join(out, filename);
  if (existsSync(path)) {
    const { built, ...previous } = JSON.parse(readFileSync(path, "utf8"));
    if (built && JSON.stringify(previous) === JSON.stringify(data)) return;
  }
  writeFileSync(path, JSON.stringify({ ...data, built: new Date().toISOString() }, null, 1));
}
writeIndex("field.json", field);
writeIndex("local.json", { items: local });
const tiers = field.sources.reduce((a, s) => ((a[s.tier] = (a[s.tier] || 0) + 1), a), {});
console.log(`nmsearch: ${field.sources.length} sources (${Object.entries(tiers).map(([k, v]) => `${v} ${k}`).join(", ")}), ${local.length} local items → ${out}`);
