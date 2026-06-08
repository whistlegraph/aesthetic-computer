#!/usr/bin/env node
// hand-meter — measure a file against the "hand" metrics from
// papers/arxiv-hand-and-loop. turns "feels bloated" into numbers, and
// shows where ceremony ends and real surface begins. see HAND.md.
//
// usage:
//   hand-meter <file>                     metrics for the working-tree file
//   hand-meter <file> <rev|date> [...]    also measure at each rev/date, in columns
//
// a date (YYYY-MM-DD) resolves to the last commit on/before it touching <file>.
//
//   hand-meter system/public/aesthetic.computer/lib/disk.mjs 2023-12-31
//   hand-meter system/.../disk.mjs 2023-12-31 2025-06-30 HEAD

import { execSync } from "child_process";
import { readFileSync } from "fs";

const [path, ...revs] = process.argv.slice(2);
if (!path) {
  console.error("usage: hand-meter <file> [rev|date ...]");
  process.exit(1);
}

const isDate = (s) => /^\d{4}-\d{2}-\d{2}$/.test(s);

// source text for a column: the working tree, a date-resolved commit, or a rev.
function source(ref) {
  if (!ref) return { label: "working", text: readFileSync(path, "utf8") };
  if (isDate(ref)) {
    const rev = git(`log -1 --until=${ref}T23:59:59 --format=%h -- "${path}"`);
    if (!rev) return { label: `${ref} (none)`, text: "" };
    return { label: `${ref} @${rev}`, text: git(`show ${rev}:"${path}"`) };
  }
  return { label: ref, text: git(`show ${ref}:"${path}"`) };
}

function git(args) {
  try {
    return execSync(`git ${args}`, { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim();
  } catch {
    return "";
  }
}

const count = (re, s) => (s.match(re) || []).length;

function measure(text) {
  const lines = text ? text.split("\n") : [];
  const loc = lines.length;
  const comment = lines.filter((l) => /^\s*(\/\/|\/\*|\*)/.test(l)).length;
  const log = lines.filter((l) => /console\.(log|warn|error|info)/.test(l));
  const emojiRe = /\p{Extended_Pictographic}/u;
  const fns = count(/\bfunction\b/g, text) + count(/=>\s*\{/g, text);
  return {
    "lines": loc,
    "comment lines": comment,
    "comment density": loc ? `${((comment / loc) * 100).toFixed(0)}%` : "—",
    "try/catch": count(/\btry\s*\{/g, text),
    "null guards (?. ?? ==null)": count(/\?\./g, text) + count(/\?\?/g, text) + count(/[!=]==\s*null/g, text) + count(/[!=]==\s*undefined/g, text),
    "banner comments (// ===)": count(/\/\/\s*=+/g, text),
    "emoji console logs": log.filter((l) => emojiRe.test(l)).length,
    "functions (approx)": fns,
    "avg fn length (loc/fn)": fns ? Math.round(loc / fns) : "—",
  };
}

const cols = [source(undefined), ...revs.map(source)];
const data = cols.map((c) => ({ ...c, m: measure(c.text) }));
const rows = Object.keys(data[0].m);

// width-aware table
const labelW = Math.max(...rows.map((r) => r.length));
const colW = data.map((c) => Math.max(c.label.length, 12));
const pad = (s, w, left = false) => (left ? String(s).padEnd(w) : String(s).padStart(w));

console.log(`\n  ${path}\n`);
console.log("  " + pad("metric", labelW, true) + "   " + data.map((c, i) => pad(c.label, colW[i])).join("   "));
console.log("  " + "─".repeat(labelW) + "   " + data.map((_, i) => "─".repeat(colW[i])).join("   "));
for (const r of rows) {
  console.log("  " + pad(r, labelW, true) + "   " + data.map((c, i) => pad(c.m[r], colW[i])).join("   "));
}
console.log("");
