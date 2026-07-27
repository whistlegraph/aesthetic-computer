#!/usr/bin/env node
// Build a ranked Reels queue from the public Whistlegraph index, download the
// archive assets, and audit sampled frames for TikTok/@whistlegraph watermark
// text before exposing clips in ready/.

import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  linkSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../..");
const SOURCE = resolve(ROOT, "system/public/whistlegraph.org/posts.json");
const SWIFT = resolve(HERE, "audit-watermarks.swift");
const DEFAULT_OUT = resolve(HERE, "downloads/reels-shortlist");
const REPORT = resolve(HERE, "REELS-SHORTLIST.md");
const CSV = resolve(HERE, "REELS-SHORTLIST.csv");

const args = process.argv.slice(2);
const value = (name, fallback) => {
  const prefixed = args.find((arg) => arg.startsWith(`--${name}=`));
  if (prefixed) return prefixed.slice(name.length + 3);
  const index = args.indexOf(`--${name}`);
  return index >= 0 && args[index + 1] ? args[index + 1] : fallback;
};
const LIMIT = Math.max(1, Number(value("limit", 30)) || 30);
const OUT = resolve(value("out", DEFAULT_OUT));
const REDO = args.includes("--redo");
const VISUAL_REVIEWED = args.includes("--visual-reviewed");

function run(command, commandArgs, options = {}) {
  const result = spawnSync(command, commandArgs, {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
    ...options,
  });
  if (result.status !== 0) {
    throw new Error(`${command} failed (${result.status})\n${result.stderr || result.stdout}`);
  }
  return result.stdout;
}

function compact(value) {
  const n = Number(value || 0);
  if (n >= 1_000_000_000) return `${(n / 1_000_000_000).toFixed(1)}B`;
  if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
  if (n >= 1_000) return `${(n / 1_000).toFixed(1)}K`;
  return String(n);
}

function csv(value) {
  return `"${String(value ?? "").replaceAll('"', '""')}"`;
}

mkdirSync(OUT, { recursive: true });
const videosDir = join(OUT, "downloaded");
const readyDir = join(OUT, "ready");
const reviewDir = join(OUT, "review");
mkdirSync(videosDir, { recursive: true });
mkdirSync(readyDir, { recursive: true });
mkdirSync(reviewDir, { recursive: true });

const model = JSON.parse(readFileSync(SOURCE, "utf8"));
const posts = model.posts
  .filter((post) => post.platform === "tiktok" && post.src)
  .sort((a, b) => (b.views || 0) - (a.views || 0))
  .slice(0, LIMIT);

for (let index = 0; index < posts.length; index += 1) {
  const post = posts[index];
  const target = join(videosDir, `${String(index + 1).padStart(2, "0")}-${post.id}.mp4`);
  post.localFile = target;
  if (!existsSync(target) || REDO) {
    console.log(`download ${index + 1}/${posts.length} ${post.id} (${compact(post.views)} views)`);
    run("curl", ["-fL", "--retry", "3", "--silent", "--show-error", "-o", target, post.src]);
  }
}

const auditBinary = join(OUT, "audit-watermarks");
run("swiftc", ["-O", SWIFT, "-o", auditBinary]);
const auditLines = run(auditBinary, posts.map((post) => post.localFile))
  .trim()
  .split("\n")
  .filter(Boolean)
  .map((line) => JSON.parse(line));
const byFile = new Map(auditLines.map((audit) => [audit.file, audit]));

for (const post of posts) {
  post.audit = byFile.get(post.localFile) || {
    status: "review-no-audit-result",
    matchedTerms: [],
    recognizedText: [],
    errors: ["No audit result"],
  };
  const filename = basename(post.localFile);
  rmSync(join(readyDir, filename), { force: true });
  rmSync(join(reviewDir, filename), { force: true });
  const destination = post.audit.status === "ocr-clear" ? readyDir : reviewDir;
  const destinationFile = join(destination, filename);
  try {
    linkSync(post.localFile, destinationFile);
  } catch {
    copyFileSync(post.localFile, destinationFile);
  }
}
rmSync(auditBinary, { force: true });

const generated = new Date().toISOString();
writeFileSync(join(OUT, "audit.json"), JSON.stringify({ generated, visualReviewed: VISUAL_REVIEWED, posts }, null, 2) + "\n");

const clearCount = posts.filter((post) => post.audit.status === "ocr-clear").length;
const md = [
  "# Whistlegraph Reels shortlist",
  "",
  `Generated ${generated} from the ${model.generated} Whistlegraph index (${model.count.toLocaleString()} posts).`,
  "",
  `Top ${posts.length} TikToks by views. ${clearCount} passed the seven-frame macOS Vision OCR audit and were placed in \`ready/\`; all others were isolated in \`review/\`.`,
  "",
  "> **Watermark rule:** `ocr-clear` means sampled frames contained no recognized `TikTok`, `@whistlegraph`, or `whistlegraph` text. It is a useful automated gate, not proof against a logo-only or missed watermark.",
  ...(VISUAL_REVIEWED ? [
    "",
    `> **Visual review:** On ${generated.slice(0, 10)}, five evenly spaced frames from each of the ${posts.length} clips were manually reviewed; no visible TikTok logo, TikTok text, or @whistlegraph watermark was found.`,
  ] : [
    "",
    "> **Posting gate:** Visually review every `ready/` clip before posting. Pass `--visual-reviewed` only after that review is complete.",
  ]),
  "",
  "| # | Views | Engagement | Duration | Date | Audit | TikTok | Caption |",
  "|---:|---:|---:|---:|---|---|---|---|",
  ...posts.map((post, index) => {
    const engagement = (post.likes || 0) + (post.comments || 0) + (post.reposts || 0) + (post.saves || 0);
    const caption = (post.desc || "(untitled)").replaceAll("|", "\\|").replaceAll("\n", " ");
    return `| ${index + 1} | ${compact(post.views)} | ${compact(engagement)} | ${post.duration || "?"}s | ${post.date || "?"} | ${post.audit.status} | [${post.id}](${post.url}) | ${caption} |`;
  }),
  "",
  "## Suggested first Reels batch",
  "",
  "Start with the highest-ranked `ocr-clear` clips, but avoid posting several near-identical pieces back-to-back. Alternate duet/drawing, teachable gesture, performance, outdoor game, and narrative/character clips. Use fresh Reel-native cover art and captions; do not preserve TikTok UI text even when the underlying video is clean.",
  "",
  "## Rebuild",
  "",
  "```bash",
  "node toolchain/whistlegraph/reels-shortlist.mjs --limit 30",
  "```",
  "",
];
writeFileSync(REPORT, md.join("\n"));

const csvRows = [
  ["rank", "id", "date", "views", "likes", "comments", "reposts", "saves", "duration_seconds", "audit", "matched_terms", "caption", "tiktok_url", "asset_url"],
  ...posts.map((post, index) => [
    index + 1, post.id, post.date, post.views, post.likes, post.comments,
    post.reposts, post.saves, post.duration, post.audit.status,
    post.audit.matchedTerms.join(";"), post.desc, post.url, post.src,
  ]),
];
writeFileSync(CSV, csvRows.map((row) => row.map(csv).join(",")).join("\n") + "\n");

console.log(`ready: ${clearCount}/${posts.length}`);
console.log(`report: ${REPORT}`);
console.log(`bundle: ${OUT}`);
