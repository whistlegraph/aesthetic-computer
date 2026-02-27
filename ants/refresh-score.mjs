#!/usr/bin/env node
// ants/refresh-score.mjs
// Refreshes the Front Door stats in SCORE.md with live data from /api/metrics
// and local file counts. Runs once per day (skips if already refreshed today).
//
// Usage:
//   node ants/refresh-score.mjs                       # uses production API
//   node ants/refresh-score.mjs http://localhost:8888  # uses local dev server
//   node ants/refresh-score.mjs --force                # bypass daily check
//
// Called from: .git/hooks/pre-commit (once per day on first commit)

import { readFileSync, writeFileSync, existsSync, readdirSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const SCORE_PATH = join(ROOT, "SCORE.md");
const DISKS_DIR = join(ROOT, "system/public/aesthetic.computer/disks");
const FUNCTIONS_DIR = join(ROOT, "system/netlify/functions");
const MARKER = "/tmp/ac-score-refreshed";

const args = process.argv.slice(2);
const force = args.includes("--force");
const apiArg = args.find((a) => a.startsWith("http"));
const API_BASE = apiArg || "https://aesthetic.computer";

// Once-per-day check
const today = new Date().toISOString().slice(0, 10);
if (!force && existsSync(MARKER)) {
  const markerDate = readFileSync(MARKER, "utf8").trim();
  if (markerDate === today) {
    process.exit(0); // Already refreshed today
  }
}

async function fetchMetrics() {
  try {
    const res = await fetch(`${API_BASE}/api/metrics?fresh=true`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    return await res.json();
  } catch (err) {
    process.stderr.write(`refresh-score: could not fetch metrics: ${err.message}\n`);
    return null;
  }
}

function countFiles(dir, ext) {
  try {
    return readdirSync(dir).filter((f) => f.endsWith(ext)).length;
  } catch {
    return 0;
  }
}

function formatDate() {
  return new Date().toLocaleDateString("en-US", {
    year: "numeric",
    month: "short",
    day: "numeric",
  });
}

async function main() {
  const metrics = await fetchMetrics();
  const mjsCount = countFiles(DISKS_DIR, ".mjs");
  const lispCount = countFiles(DISKS_DIR, ".lisp");
  const builtInPieces = mjsCount + lispCount;
  const apiEndpoints = countFiles(FUNCTIONS_DIR, ".mjs");

  const lines = [];
  lines.push(
    `${builtInPieces} built-in pieces (${mjsCount} JS + ${lispCount} KidLisp), ~${apiEndpoints} API endpoints.`
  );

  if (metrics) {
    const parts = [];
    if (metrics.handles) parts.push(`${metrics.handles} registered handles`);
    if (metrics.pieces) parts.push(`${metrics.pieces} user-published pieces`);
    if (metrics.paintings) parts.push(`${metrics.paintings} paintings`);
    if (metrics.kidlisp) parts.push(`${metrics.kidlisp} KidLisp programs`);
    if (metrics.chatMessages) parts.push(`${metrics.chatMessages} chat messages`);
    if (metrics.printsOrdered) parts.push(`${metrics.printsOrdered} prints ordered`);
    if (parts.length) lines.push(parts.join(", ") + ".");
  }

  lines.push(`*Last refreshed: ${formatDate()}*`);

  const statsBlock = lines.join("<br>\n");

  const score = readFileSync(SCORE_PATH, "utf8");

  const startMarker = "<!-- stats:start -->";
  const endMarker = "<!-- stats:end -->";

  if (!score.includes(startMarker) || !score.includes(endMarker)) {
    process.stderr.write(
      `refresh-score: missing stats markers in SCORE.md\n`
    );
    process.exit(1);
  }

  const before = score.slice(
    0,
    score.indexOf(startMarker) + startMarker.length
  );
  const after = score.slice(score.indexOf(endMarker));
  const updated = before + "\n" + statsBlock + "\n" + after;

  if (updated !== score) {
    writeFileSync(SCORE_PATH, updated);
    process.stderr.write(`refresh-score: ✓ updated SCORE.md\n`);
  } else {
    process.stderr.write(`refresh-score: no changes needed\n`);
  }

  // Mark today as done
  writeFileSync(MARKER, today);
}

main();
