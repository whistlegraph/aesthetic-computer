#!/usr/bin/env node

// Checks all portfolio links in jas.life/index.html and reports their status.
// Usage: node check-links.mjs

import { readFileSync } from "fs";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __dirname = dirname(fileURLToPath(import.meta.url));
const html = readFileSync(join(__dirname, "index.html"), "utf-8");

// Extract all href URLs from <a> tags (skip anchors and mailto)
const linkRegex = /<a\s+[^>]*href="([^"]+)"[^>]*>([\s\S]*?)<\/a>/gi;
const links = [];
let match;
while ((match = linkRegex.exec(html)) !== null) {
  const url = match[1];
  const text = match[2].replace(/<[^>]*>/g, "").replace(/\s+/g, " ").trim();
  // Skip fragment-only links
  if (!url.startsWith("#")) {
    links.push({ url, text });
  }
}

// Check if a link is inside an HTML comment
function isCommentedOut(url) {
  const commentRegex = /<!--[\s\S]*?-->/g;
  let m;
  while ((m = commentRegex.exec(html)) !== null) {
    if (m[0].includes(url)) return true;
  }
  return false;
}

const TIMEOUT_MS = 15000;

async function checkUrl(url) {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), TIMEOUT_MS);
  try {
    const res = await fetch(url, {
      method: "HEAD",
      signal: controller.signal,
      redirect: "follow",
      headers: {
        "User-Agent":
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) jas-link-checker/1.0",
      },
    });
    clearTimeout(timer);
    // Some servers reject HEAD, retry with GET
    if (res.status === 405 || res.status === 403) {
      const controller2 = new AbortController();
      const timer2 = setTimeout(() => controller2.abort(), TIMEOUT_MS);
      const res2 = await fetch(url, {
        method: "GET",
        signal: controller2.signal,
        redirect: "follow",
        headers: {
          "User-Agent":
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) jas-link-checker/1.0",
        },
      });
      clearTimeout(timer2);
      return { status: res2.status, ok: res2.ok };
    }
    return { status: res.status, ok: res.ok };
  } catch (err) {
    clearTimeout(timer);
    return { status: null, ok: false, error: err.message };
  }
}

console.log(`\nChecking ${links.length} links from jas.life/index.html...\n`);

const results = [];

// Check in batches of 5 to avoid overwhelming
const BATCH = 5;
for (let i = 0; i < links.length; i += BATCH) {
  const batch = links.slice(i, i + BATCH);
  const batchResults = await Promise.all(
    batch.map(async (link) => {
      const commented = isCommentedOut(link.url);
      const result = await checkUrl(link.url);
      return { ...link, ...result, commented };
    }),
  );
  results.push(...batchResults);
}

// Separate into OK, commented-out, and broken
const broken = results.filter((r) => !r.ok && !r.commented);
const commentedBroken = results.filter((r) => !r.ok && r.commented);
const ok = results.filter((r) => r.ok);

// Print all results
for (const r of results) {
  const icon = r.ok ? "\x1b[32m✓\x1b[0m" : "\x1b[31m✗\x1b[0m";
  const status = r.status || r.error || "unknown";
  const comment = r.commented ? " \x1b[33m(commented out)\x1b[0m" : "";
  console.log(`${icon} [${status}] ${r.text}${comment}`);
  console.log(`  ${r.url}\n`);
}

// Summary
console.log("─".repeat(60));
console.log(
  `\x1b[32m✓ ${ok.length} OK\x1b[0m | \x1b[31m✗ ${broken.length} BROKEN\x1b[0m | \x1b[33m${commentedBroken.length} broken but commented out\x1b[0m`,
);

if (broken.length > 0) {
  console.log(`\n\x1b[31m── BROKEN LINKS (need fixing) ──\x1b[0m\n`);
  broken.forEach((r, i) => {
    console.log(
      `  ${i + 1}. \x1b[31m✗\x1b[0m [${r.status || r.error}] ${r.text}`,
    );
    console.log(`     ${r.url}`);
  });
  console.log();
  process.exit(1);
} else {
  console.log("\nAll active links are healthy!");
  process.exit(0);
}
