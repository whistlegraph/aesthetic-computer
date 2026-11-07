#!/usr/bin/env node
// nmsearch build: field registry + local platter index → public/nmsearch/.
// Usage: node papers/nmsearch/build.mjs
import { readFileSync, writeFileSync, readdirSync, existsSync, statSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

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
const unesc = (s) => s.replace(/&apos;/g, "'").replace(/&quot;/g, '"').replace(/&amp;/g, "&").replace(/&middot;/g, "·").replace(/<[^>]+>/g, "");
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

writeFileSync(join(out, "field.json"), JSON.stringify({ ...field, built: new Date().toISOString() }, null, 1));
writeFileSync(join(out, "local.json"), JSON.stringify({ built: new Date().toISOString(), items: local }, null, 1));
const tiers = field.sources.reduce((a, s) => ((a[s.tier] = (a[s.tier] || 0) + 1), a), {});
console.log(`nmsearch: ${field.sources.length} sources (${Object.entries(tiers).map(([k, v]) => `${v} ${k}`).join(", ")}), ${local.length} local items → ${out}`);
