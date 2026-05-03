#!/usr/bin/env node
// scout.mjs — resolve per-slide content queries from the audience config
// into base64 data URLs that slide HTML can embed inline. Supports:
//   { glob: "<absolute or repo-relative pattern>" }                 → image
//   { glob: "...pdf", pdfPage: 1, pdfWidth: 800 }                   → first PDF page
//   { commits: "<git --grep regex>", since: "48 hours" }            → commit list strings
//   { files: "<glob>", since: "48 hours", limit: 8 }                → recent matching paths
//   { json: "<repo-relative path>" }                                → parsed JSON value
//   { screenshot: "<URL>", ... }                                    → pre-baked PNG
//                                                                     (cached at out/screenshots/<seg>-<name>.png
//                                                                      by `bin/screenshots.mjs`)
// Output: out/assets.json mapping slide-name → resolved values keyed by query name.
// Usage: node bin/scout.mjs [audience-name]

import { execFileSync, execSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync, statSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const audienceName = process.argv[2] || "fia";
const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const TMP = `${tmpdir()}/recap-pdf-${process.pid}`;
mkdirSync(TMP, { recursive: true });

function expandGlob(pattern) {
  const abs = pattern.startsWith("/") ? pattern : join(REPO, pattern);
  // Use shell glob expansion for simplicity.
  try {
    const out = execSync(`ls -1 ${abs} 2>/dev/null || true`, { encoding: "utf8" });
    return out.split("\n").map((s) => s.trim()).filter(Boolean);
  } catch { return []; }
}

function pdfPageToDataUrl(pdfPath, page = 1, width = 800) {
  const stem = `${TMP}/${basename(pdfPath, ".pdf")}-p${page}`;
  // pdftoppm uses 1-based page index; -scale-to fits the longer side.
  execFileSync("pdftoppm", [
    "-png", "-r", "150",
    "-f", String(page), "-l", String(page),
    "-scale-to", String(width),
    pdfPath, stem,
  ]);
  // pdftoppm produces "<stem>-1.png" or "<stem>-01.png" — find it.
  const candidates = expandGlob(`${stem}-*.png`);
  if (!candidates.length) throw new Error(`pdftoppm produced no output for ${pdfPath}`);
  const buf = readFileSync(candidates[0]);
  return `data:image/png;base64,${buf.toString("base64")}`;
}

function imageToDataUrl(p) {
  const buf = readFileSync(p);
  const ext = p.toLowerCase().split(".").pop();
  const mime = ext === "svg" ? "image/svg+xml" : ext === "webp" ? "image/webp" : ext === "jpg" || ext === "jpeg" ? "image/jpeg" : "image/png";
  return `data:${mime};base64,${buf.toString("base64")}`;
}

function recentCommits(grep, sinceArg, limit = 8) {
  const since = sinceArg || "48 hours ago";
  const out = execSync(
    `git -C ${REPO} log --since="${since}" -E --grep="${grep}" --pretty=format:"%h|%s" -n ${limit}`,
    { encoding: "utf8" },
  );
  return out.split("\n").filter(Boolean).map((l) => {
    const i = l.indexOf("|");
    return { hash: l.slice(0, i), subject: l.slice(i + 1) };
  });
}

function recentFiles(glob, sinceHours = 168, limit = 12) {
  // mtime within sinceHours; sorted newest first.
  const abs = glob.startsWith("/") ? glob : join(REPO, glob);
  const sinceMs = Date.now() - sinceHours * 3600 * 1000;
  const matches = expandGlob(abs)
    .map((f) => ({ f, mtime: statSync(f).mtimeMs }))
    .filter((x) => x.mtime >= sinceMs)
    .sort((a, b) => b.mtime - a.mtime)
    .slice(0, limit);
  return matches.map((x) => x.f);
}

function resolveQuery(segName, name, q) {
  if (q.screenshot) {
    const cached = `${ROOT}/out/screenshots/${segName}-${name}.png`;
    if (!expandGlob(cached).length) {
      console.warn(`  ⚠ ${name}: screenshot '${q.screenshot}' not cached at ${cached.replace(REPO + "/", "")} — run bin/screenshots.mjs first`);
      return null;
    }
    return imageToDataUrl(cached);
  }
  if (q.glob) {
    const matches = expandGlob(q.glob);
    if (!matches.length) {
      console.warn(`  ⚠ ${name}: glob "${q.glob}" matched nothing`);
      return null;
    }
    const file = matches[0];
    if (q.pdfPage) return pdfPageToDataUrl(file, q.pdfPage, q.pdfWidth || 800);
    return imageToDataUrl(file);
  }
  if (q.commits) return recentCommits(q.commits, q.since, q.limit);
  if (q.files) return recentFiles(q.files, q.sinceHours || 168, q.limit || 12);
  if (q.json) {
    const abs = q.json.startsWith("/") ? q.json : join(REPO, q.json);
    try { return JSON.parse(readFileSync(abs, "utf8")); }
    catch (e) { console.warn(`  ⚠ ${name}: json '${q.json}' read failed: ${e.message}`); return null; }
  }
  console.warn(`  ⚠ ${name}: unknown query shape`);
  return null;
}

const out = {};
for (const seg of audience.segments) {
  const slide = audience.slides[seg.name];
  if (!slide || typeof slide !== "object" || !slide.queries) continue;
  console.log(`▸ ${seg.name}`);
  out[seg.name] = {};
  for (const [name, q] of Object.entries(slide.queries)) {
    const value = resolveQuery(seg.name, name, q);
    if (value !== null) {
      out[seg.name][name] = value;
      const desc = typeof value === "string" && value.startsWith("data:")
        ? `${value.slice(0, value.indexOf(";"))} (${(value.length / 1024).toFixed(0)} KB b64)`
        : Array.isArray(value)
          ? `${value.length} items`
          : String(value).slice(0, 60);
      console.log(`  ✓ ${name}: ${desc}`);
    }
  }
}

writeFileSync(`${ROOT}/out/assets.json`, JSON.stringify(out, null, 2));
console.log(`✓ ${ROOT}/out/assets.json · ${Object.keys(out).length} slide(s)`);
