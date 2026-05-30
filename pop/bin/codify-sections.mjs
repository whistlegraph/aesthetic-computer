#!/usr/bin/env node
// pop/bin/codify-sections.mjs — add lowercase letter codes (a..z) to
// every section in every released pop struct.json AND its matching AC
// piece manifest at system/public/aesthetic.computer/disks/pop/<slug>.json.
//
// Idempotent — re-running produces no diff. Run after a new track lands
// or after a struct's sections list changes.
//
// Usage:
//   node pop/bin/codify-sections.mjs
//   node pop/bin/codify-sections.mjs --dry-run
//
// Why: lets the AC player jump to a named section via prompt syntax —
//   `marimbaba c` → section index 2 (letter "c")
//   `hellsine:r`  → section index 17 (letter "r")
// The letter shows up in the side stamp + on each illy thumb so users
// can discover the codes.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");

const DRY = process.argv.includes("--dry-run");
const LETTERS = "abcdefghijklmnopqrstuvwxyz";

// (struct path, manifest path) pairs. Either or both can be missing —
// the script skips missing entries with a warning so the rest still run.
const TRACKS = [
  {
    slug:     "marimbaba",
    struct:   "pop/marimba/out/marimbaba.struct.json",
    manifest: "system/public/aesthetic.computer/disks/pop/marimbaba.json",
  },
  {
    slug:     "helpabeach",
    struct:   "pop/chillwave/out/helpabeach.struct.json",
    manifest: "system/public/aesthetic.computer/disks/pop/helpabeach.json",
  },
  {
    slug:     "trancenwaltz",
    struct:   "pop/dance/out/trancenwaltz.assets/struct.json",
    manifest: "system/public/aesthetic.computer/disks/pop/trancenwaltz.json",
  },
  {
    slug:     "trancepenta",
    struct:   null, // no struct.json yet — manifest only
    manifest: "system/public/aesthetic.computer/disks/pop/trancepenta.json",
  },
  {
    slug:     "hellsine",
    struct:   "pop/hellsine/hellsine.struct.json",
    manifest: "system/public/aesthetic.computer/disks/pop/hellsine.json",
  },
  {
    slug:     "solafiya",
    struct:   "pop/jungle/out/solafiya.struct.json",
    manifest: "system/public/aesthetic.computer/disks/pop/solafiya.json",
  },
];

function readJson(path) {
  return JSON.parse(readFileSync(path, "utf8"));
}

function writeJson(path, data) {
  // 2-space indent + trailing newline matches the existing pop struct style.
  writeFileSync(path, JSON.stringify(data, null, 2) + "\n");
}

function codifySections(sections, label) {
  if (!Array.isArray(sections)) {
    console.warn(`  ⚠ ${label}: sections is not an array, skipping`);
    return { changed: 0, total: 0 };
  }
  if (sections.length > LETTERS.length) {
    throw new Error(
      `${label}: ${sections.length} sections exceeds single-letter ceiling (${LETTERS.length}). ` +
      `Extend to two-letter codes (aa, ab, …) before running again.`
    );
  }
  let changed = 0;
  for (let i = 0; i < sections.length; i++) {
    const want = LETTERS[i];
    if (sections[i].code !== want) {
      sections[i].code = want;
      changed++;
    }
  }
  return { changed, total: sections.length };
}

function processFile(absPath, label) {
  if (!absPath) return { skipped: true };
  if (!existsSync(absPath)) {
    console.warn(`  ⚠ ${label}: not found at ${absPath.replace(REPO + "/", "")}`);
    return { skipped: true };
  }
  const data = readJson(absPath);
  const sections = data.sections;
  const { changed, total } = codifySections(sections, label);
  if (changed > 0 && !DRY) writeJson(absPath, data);
  return { changed, total, path: absPath };
}

function main() {
  console.log(DRY ? "▸ codify-sections (DRY RUN)" : "▸ codify-sections");
  let totalChanged = 0;
  let totalChecked = 0;
  for (const t of TRACKS) {
    console.log(`\n· ${t.slug}`);
    const struct   = processFile(t.struct   && resolve(REPO, t.struct),   `${t.slug} struct`);
    const manifest = processFile(t.manifest && resolve(REPO, t.manifest), `${t.slug} manifest`);

    // Sanity check: if both exist their section counts should match.
    if (!struct.skipped && !manifest.skipped && struct.total !== manifest.total) {
      console.warn(
        `  ⚠ ${t.slug}: struct has ${struct.total} sections but manifest has ${manifest.total}. ` +
        `Codes may go out of sync — reconcile and re-run.`
      );
    }
    if (!struct.skipped) {
      console.log(`  struct   · ${struct.total} sections · ${struct.changed} updated → ${LETTERS.slice(0, struct.total)}`);
      totalChecked += struct.total; totalChanged += struct.changed;
    }
    if (!manifest.skipped) {
      console.log(`  manifest · ${manifest.total} sections · ${manifest.changed} updated → ${LETTERS.slice(0, manifest.total)}`);
      totalChecked += manifest.total; totalChanged += manifest.changed;
    }
  }
  console.log(`\n✓ codified ${totalChanged}/${totalChecked} sections${DRY ? " (no writes — dry run)" : ""}`);
}

main();
