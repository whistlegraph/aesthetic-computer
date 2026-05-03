#!/usr/bin/env node
// dropbox-manifest.mjs
// Walks /Whistlegraph/ via the Dropbox HTTP API and writes a markdown manifest
// (folder counts, file counts, sizes, sample paths) to ../dropbox-manifest.md.
//
// Token is read from aesthetic-computer-vault/dropbox/auth.json (dbxcli format).
// No SDK, no npm install — uses Node 18+ built-in fetch.

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const AUTH_PATH = path.resolve(__dirname, "../../../aesthetic-computer-vault/dropbox/auth.json");
const OUT_MD = path.resolve(__dirname, "../dropbox-manifest.md");
const OUT_JSON = path.resolve(__dirname, "../dropbox-manifest.json");
const OUT_FILES = path.resolve(__dirname, "../dropbox-files.json");
const ROOT = process.argv[2] || "/Whistlegraph";

const auth = JSON.parse(fs.readFileSync(AUTH_PATH, "utf8"));
const token = auth?.[""]?.personal;
if (!token) {
  console.error("No token at auth['']['personal'] in", AUTH_PATH);
  process.exit(1);
}

async function api(endpoint, body) {
  const res = await fetch(`https://api.dropboxapi.com/2/${endpoint}`, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${token}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
  });
  if (!res.ok) {
    throw new Error(`${endpoint} ${res.status}: ${await res.text()}`);
  }
  return res.json();
}

console.error(`listing ${ROOT} recursively…`);
const entries = [];
let resp = await api("files/list_folder", {
  path: ROOT,
  recursive: true,
  include_deleted: false,
  include_media_info: false,
  include_non_downloadable_files: true,
});
entries.push(...resp.entries);
while (resp.has_more) {
  resp = await api("files/list_folder/continue", { cursor: resp.cursor });
  entries.push(...resp.entries);
  console.error(`  …${entries.length} entries so far`);
}
console.error(`done: ${entries.length} entries`);

// Bucket by top-level subfolder
const byTop = new Map();
let totalBytes = 0;
let totalFiles = 0;
for (const e of entries) {
  const rel = e.path_display.replace(ROOT + "/", "");
  const top = rel.split("/")[0] || "(root)";
  if (!byTop.has(top)) {
    byTop.set(top, { subdirs: 0, files: 0, bytes: 0, samples: [] });
  }
  const s = byTop.get(top);
  if (e[".tag"] === "folder") {
    if (rel.includes("/")) s.subdirs++;
  } else if (e[".tag"] === "file") {
    s.files++;
    s.bytes += e.size || 0;
    totalFiles++;
    totalBytes += e.size || 0;
    if (s.samples.length < 4) s.samples.push({ path: rel, size: e.size, modified: e.client_modified });
  }
}

const fmtSize = (b) => {
  if (b >= 1e9) return (b / 1e9).toFixed(2) + " GB";
  if (b >= 1e6) return (b / 1e6).toFixed(1) + " MB";
  if (b >= 1e3) return (b / 1e3).toFixed(1) + " KB";
  return (b || 0) + " B";
};

const ext = (p) => {
  const i = p.lastIndexOf(".");
  return i > 0 ? p.slice(i).toLowerCase() : "(none)";
};

const extCounts = new Map();
for (const e of entries) {
  if (e[".tag"] !== "file") continue;
  const x = ext(e.path_display);
  extCounts.set(x, (extCounts.get(x) || 0) + 1);
}

let md = `# Dropbox manifest — \`${ROOT}/\`\n\n`;
md += `Generated ${new Date().toISOString().slice(0, 19)}Z via Dropbox HTTP API.\n\n`;
md += `**Total**: ${entries.length} entries · ${totalFiles} files · ${fmtSize(totalBytes)}\n\n`;
md += `**Top-level subfolders**: ${byTop.size}\n\n`;

md += `## By subfolder\n\n`;
md += `| Subfolder | Subdirs | Files | Size | Sample files |\n`;
md += `|---|---:|---:|---:|---|\n`;
const sorted = [...byTop.entries()].sort((a, b) => b[1].bytes - a[1].bytes);
for (const [name, s] of sorted) {
  const samples = s.samples.length
    ? s.samples.map((x) => `\`${x.path}\` (${fmtSize(x.size)})`).join("<br>")
    : "—";
  md += `| **${name}** | ${s.subdirs} | ${s.files} | ${fmtSize(s.bytes)} | ${samples} |\n`;
}

md += `\n## File types\n\n`;
md += `| Extension | Count |\n|---|---:|\n`;
const sortedExt = [...extCounts.entries()].sort((a, b) => b[1] - a[1]);
for (const [x, n] of sortedExt) md += `| \`${x}\` | ${n} |\n`;

fs.writeFileSync(OUT_MD, md);
fs.writeFileSync(
  OUT_JSON,
  JSON.stringify(
    {
      generatedAt: new Date().toISOString(),
      root: ROOT,
      totalEntries: entries.length,
      totalFiles,
      totalBytes,
      byTop: Object.fromEntries(byTop),
      extCounts: Object.fromEntries(extCounts),
    },
    null,
    2,
  ),
);
// Flat per-file dump (for HTML browser)
const flatFiles = entries
  .filter((e) => e[".tag"] === "file")
  .map((e) => {
    const rel = e.path_display.replace(ROOT + "/", "");
    return {
      p: rel,
      s: e.size || 0,
      m: e.client_modified || e.server_modified || null,
    };
  });
fs.writeFileSync(OUT_FILES, JSON.stringify(flatFiles));

console.error(`wrote ${OUT_MD}`);
console.error(`wrote ${OUT_JSON}`);
console.error(`wrote ${OUT_FILES} (${flatFiles.length} files)`);
