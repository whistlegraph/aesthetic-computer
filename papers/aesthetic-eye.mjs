#!/usr/bin/env node
// aesthetic-eye — render-first visual QA for paper diagrams.
//
// A TeX build proves syntax, not design. This tool prepares diagram crops for
// visual inference and enforces a current-PDF manifest whose verdict is the
// literal `design: pass|fail` for every diagram.

import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import { access, mkdir, readFile } from "node:fs/promises";
import { basename, dirname, extname, isAbsolute, join, resolve } from "node:path";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";

const exec = promisify(execFile);
const REQUIRED_CHECKS = ["tangents", "type", "balance", "spaceUse", "hierarchy", "edgeRouting"];

async function exists(path) {
  try { await access(path); return true; }
  catch { return false; }
}

async function sha256(path) {
  return createHash("sha256").update(await readFile(path)).digest("hex");
}

function resolveInputs(input, manifestArg) {
  const absolute = resolve(input);
  const paperDir = extname(absolute).toLowerCase() === ".pdf" ? dirname(absolute) : absolute;
  const manifestPath = manifestArg
    ? resolve(manifestArg)
    : join(paperDir, "aesthetic-eye.json");
  return { paperDir, manifestPath };
}

async function loadReview(input, manifestArg) {
  const { paperDir, manifestPath } = resolveInputs(input, manifestArg);
  if (!(await exists(manifestPath))) throw new Error(`missing aesthetic-eye manifest: ${manifestPath}`);
  const manifest = JSON.parse(await readFile(manifestPath, "utf8"));
  const pdfPath = isAbsolute(manifest.pdf || "")
    ? manifest.pdf
    : join(paperDir, manifest.pdf || "");
  if (!manifest.pdf || !(await exists(pdfPath))) throw new Error(`manifest PDF is missing: ${pdfPath}`);
  return { paperDir, manifestPath, manifest, pdfPath };
}

export function validateManifest(manifest, currentPdfSha256) {
  const errors = [];
  const diagrams = Array.isArray(manifest?.diagrams) ? manifest.diagrams : [];
  if (manifest?.schema !== 1) errors.push("schema must be 1");
  if (!manifest?.visualInference) errors.push("visualInference must be true");
  if (manifest?.reviewer?.kind !== "visual-inference") errors.push("reviewer.kind must be visual-inference");
  if (!manifest?.reviewedAt) errors.push("reviewedAt is required");
  if (!manifest?.pdfSha256) errors.push("pdfSha256 is required");
  else if (currentPdfSha256 && manifest.pdfSha256 !== currentPdfSha256) errors.push("review is stale: PDF hash changed");
  if (!Number.isInteger(manifest?.expectedDiagrams) || manifest.expectedDiagrams < 0) {
    errors.push("expectedDiagrams must be a non-negative integer");
  } else if (diagrams.length !== manifest.expectedDiagrams) {
    errors.push(`expected ${manifest.expectedDiagrams} diagram(s), found ${diagrams.length}`);
  }

  const ids = new Set();
  for (const [index, diagram] of diagrams.entries()) {
    const prefix = `diagram ${diagram?.id || index + 1}`;
    if (!diagram?.id) errors.push(`${prefix}: id is required`);
    else if (ids.has(diagram.id)) errors.push(`${prefix}: id must be unique`);
    else ids.add(diagram.id);
    if (!Number.isInteger(diagram?.page) || diagram.page < 1) errors.push(`${prefix}: page must be >= 1`);
    if (!Array.isArray(diagram?.crop) || diagram.crop.length !== 4 || diagram.crop.some((n) => !Number.isFinite(n) || n < 0 || n > 1)) {
      errors.push(`${prefix}: crop must be four normalized values between 0 and 1`);
    } else if (diagram.crop[0] + diagram.crop[2] > 1 || diagram.crop[1] + diagram.crop[3] > 1) {
      errors.push(`${prefix}: crop extends beyond the rendered page`);
    }
    if (!['pass', 'fail'].includes(diagram?.design)) errors.push(`${prefix}: design must be pass or fail`);
    for (const check of REQUIRED_CHECKS) {
      if (!['pass', 'fail'].includes(diagram?.checks?.[check])) errors.push(`${prefix}: checks.${check} must be pass or fail`);
    }
    const failedChecks = REQUIRED_CHECKS.filter((check) => diagram?.checks?.[check] === "fail");
    if (diagram?.design === "pass" && failedChecks.length) {
      errors.push(`${prefix}: design cannot pass while ${failedChecks.join(", ")} fail`);
    }
  }

  const allDesignPass = diagrams.length === manifest?.expectedDiagrams
    && diagrams.every((diagram) => diagram.design === "pass");
  return { pass: errors.length === 0 && allDesignPass, errors, diagrams };
}

async function check(input, manifestArg) {
  const review = await loadReview(input, manifestArg);
  const digest = await sha256(review.pdfPath);
  const verdict = validateManifest(review.manifest, digest);
  for (const diagram of verdict.diagrams) console.log(`design: ${diagram.design}  ${diagram.id}  page ${diagram.page}`);
  if (verdict.errors.length) for (const error of verdict.errors) console.error(`FAIL: ${error}`);
  if (!verdict.pass && !verdict.errors.length) console.error("FAIL: one or more diagrams have design: fail");
  console.log(`aesthetic-eye: ${verdict.pass ? "PASS" : "FAIL"}  ${basename(review.pdfPath)}`);
  if (!verdict.pass) process.exitCode = 1;
}

async function prepare(input, manifestArg, outputArg) {
  const review = await loadReview(input, manifestArg);
  const outputDir = outputArg ? resolve(outputArg) : join(review.paperDir, ".aesthetic-eye");
  await mkdir(outputDir, { recursive: true });
  const pagePrefix = join(outputDir, "page");
  await exec("pdftoppm", ["-png", "-r", "144", review.pdfPath, pagePrefix], { maxBuffer: 8 * 1024 * 1024 });

  const crops = [];
  for (const diagram of review.manifest.diagrams || []) {
    const pagePath = `${pagePrefix}-${diagram.page}.png`;
    if (!(await exists(pagePath))) throw new Error(`${diagram.id}: rendered page ${diagram.page} is missing`);
    const { stdout } = await exec("magick", ["identify", "-format", "%w %h", pagePath]);
    const [pageWidth, pageHeight] = stdout.trim().split(/\s+/).map(Number);
    const [x, y, width, height] = diagram.crop;
    const geometry = `${Math.round(pageWidth * width)}x${Math.round(pageHeight * height)}+${Math.round(pageWidth * x)}+${Math.round(pageHeight * y)}`;
    const cropPath = join(outputDir, `diagram-${diagram.id}.png`);
    await exec("magick", [pagePath, "-crop", geometry, "+repage", cropPath]);
    crops.push(cropPath);
    console.log(`${diagram.id}: ${cropPath}`);
  }

  if (crops.length) {
    const contactPath = join(outputDir, "diagrams-contact.png");
    await exec("magick", ["montage", ...crops, "-thumbnail", "1000x700", "-tile", "2x", "-geometry", "+24+24", contactPath]);
    console.log(`contact: ${contactPath}`);
  }
  console.log(`pdfSha256: ${await sha256(review.pdfPath)}`);
  console.log("Next: inspect every crop visually, record design: pass|fail plus all six checks, then run `aesthetic-eye check`.");
}

function option(args, name) {
  const index = args.indexOf(name);
  return index >= 0 ? args[index + 1] : undefined;
}

async function main() {
  const args = process.argv.slice(2);
  const command = args[0];
  const input = args[1];
  if (!['prepare', 'check'].includes(command) || !input) {
    console.error("usage: node papers/aesthetic-eye.mjs <prepare|check> <paper-dir|pdf> [--manifest path] [--out dir]");
    process.exit(2);
  }
  if (command === "prepare") await prepare(input, option(args, "--manifest"), option(args, "--out"));
  else await check(input, option(args, "--manifest"));
}

const isMain = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) main().catch((error) => { console.error(`aesthetic-eye: ${error.message}`); process.exit(1); });
