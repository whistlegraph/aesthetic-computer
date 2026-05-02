#!/usr/bin/env node
// build-dossier.mjs — multi-pass xelatex + bibtex build for an AC dossier.
//
// Each dossier uses \bibliography{references}, so resolved citations require:
//   pass 1: xelatex   (writes .aux with \citation{} entries)
//   bib  : bibtex     (resolves citations, writes .bbl)
//   pass 2: xelatex   (incorporates .bbl into bibliography)
//   pass 3: xelatex   (resolves cross-refs to bibliography numbers)
//
// Usage:
//   node papers/bin/build-dossier.mjs arxiv-rhizome
//   node papers/bin/build-dossier.mjs --all

import { execFileSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (const a of argv) {
  if (a.startsWith("--")) flags[a.slice(2)] = true;
  else positional.push(a);
}

function findBase(dir) {
  // Pick the primary .tex (skip -cards.tex variants and translation files).
  const tex = readdirSync(join(PAPERS_DIR, dir)).filter((f) => f.endsWith(".tex"));
  if (!tex.length) throw new Error(`no .tex in ${dir}`);
  const main = tex.find((f) => !/-(cards|da|es|zh|ja)\.tex$/.test(f));
  return (main || tex[0]).replace(/\.tex$/, "");
}

const DOSSIER_SLUGS = new Set([
  "arxiv-rhizome", "arxiv-sfpc", "arxiv-eyebeam", "arxiv-recurse",
  "arxiv-internet-archive", "arxiv-mellon", "arxiv-pioneer-works",
  "arxiv-new-inc", "arxiv-studio-museum", "arxiv-hathitrust",
  "arxiv-the-kitchen", "arxiv-machine-project", "arxiv-heavy-manners-library",
]);

function run(cmd, args, cwd) {
  return execFileSync(cmd, args, { cwd, stdio: "pipe", encoding: "utf8" });
}

function buildOne(dir) {
  const cwd = join(PAPERS_DIR, dir);
  const base = findBase(dir);
  const t0 = Date.now();
  try {
    run("xelatex", ["-interaction=nonstopmode", `${base}.tex`], cwd);
    run("bibtex", [base], cwd);
    run("xelatex", ["-interaction=nonstopmode", `${base}.tex`], cwd);
    run("xelatex", ["-interaction=nonstopmode", `${base}.tex`], cwd);
  } catch (e) {
    // Suppress noisy output unless build truly fails to produce a PDF
    if (!existsSync(join(cwd, `${base}.pdf`))) throw e;
  }
  const log = readFileSync(join(cwd, `${base}.log`), "utf8");
  const undef = (log.match(/Citation `([^']+)' on page \d+ undefined/g) || [])
    .map((m) => m.match(/`([^']+)'/)[1]);
  const unique = [...new Set(undef)];
  const pages = (log.match(/Output written on .+?\((\d+) pages?\)/) || [])[1];
  const dur = ((Date.now() - t0) / 1000).toFixed(1);
  return { dir, base, pages: pages || "?", undefinedCites: unique, durSec: dur };
}

let targets = [];
if (flags.all) {
  // --all means all known dossiers (not every arxiv-* dir).
  for (const d of DOSSIER_SLUGS) {
    if (readdirSync(PAPERS_DIR).includes(d)) targets.push(d);
  }
} else if (positional.length) {
  targets = positional;
} else {
  console.error("usage: build-dossier.mjs <arxiv-slug> | --all");
  process.exit(1);
}

console.log(`▸ building ${targets.length} dossier(s)`);
let totalUndef = 0;
for (const t of targets) {
  try {
    const r = buildOne(t);
    const undefStr = r.undefinedCites.length
      ? ` ⚠ ${r.undefinedCites.length} undefined: ${r.undefinedCites.slice(0, 4).join(", ")}${r.undefinedCites.length > 4 ? "…" : ""}`
      : "";
    console.log(`  ✓ ${r.dir} (${r.base}.pdf, ${r.pages}pp, ${r.durSec}s)${undefStr}`);
    totalUndef += r.undefinedCites.length;
  } catch (e) {
    console.error(`  ✗ ${t}: ${e.message.split("\n")[0]}`);
    process.exitCode = 2;
  }
}
if (totalUndef) console.log(`\n  total undefined citations: ${totalUndef}`);
