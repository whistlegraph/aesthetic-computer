#!/usr/bin/env node
// necklace.mjs — audition the rhythm theory from a terminal, before writing a track.
//
//   node pop/bin/necklace.mjs E 5 16                 analyse a Euclidean rhythm
//   node pop/bin/necklace.mjs son                    analyse a named timeline
//   node pop/bin/necklace.mjs "x..x..x."             analyse a box pattern
//   node pop/bin/necklace.mjs son --space --bpm 120  place it around a head
//   node pop/bin/necklace.mjs son rumba --morph      walk one into the other
//   node pop/bin/necklace.mjs --list                 the platter's catalogue

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import {
  rhythm, bjorklund, toBox, toIoi, analyze, dist, morphPath,
  necklaceCanonical, complement, enumerateNecklaces,
} from "../lib/necklace.mjs";
import { spatialReport } from "../lib/necklace-space.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const PLATTER = join(HERE, "..", "..", "papers", "rhythm-platter", "timelines.json");

const argv = process.argv.slice(2);
const flags = {};
const args = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; } else flags[k] = true;
  } else args.push(a);
}

const platter = JSON.parse(readFileSync(PLATTER, "utf8"));
const named = new Map(platter.named_timelines.rows.map((r) => [r.id, r]));

function resolve(tokens) {
  if (!tokens.length) return null;
  if (tokens[0].toUpperCase() === "E" && tokens.length >= 3) {
    return { label: `E(${tokens[1]},${tokens[2]})`, r: bjorklund(+tokens[1], +tokens[2]), rest: tokens.slice(3) };
  }
  const t = tokens[0];
  if (named.has(t)) { const row = named.get(t); return { label: t, r: rhythm(row.onsets, row.n), rest: tokens.slice(1) }; }
  return { label: t, r: rhythm(t), rest: tokens.slice(1) };
}

// ── the circle, drawn ───────────────────────────────────────────────────────
// Toussaint's clock diagram, at terminal resolution. Step 0 at the top.
function circle(r) {
  const x = rhythm(r);
  const R = 7, W = R * 2 + 1;
  const grid = Array.from({ length: W }, () => Array(W * 2).fill(" "));
  for (let i = 0; i < x.n; i++) {
    const a = (Math.PI * 2 * i) / x.n - Math.PI / 2;
    const row = Math.round(R + Math.sin(a) * R);
    const col = Math.round(R + Math.cos(a) * R) * 2;
    grid[row][col] = x.bits[i] ? "●" : "·";
  }
  return grid.map((row) => row.join("").trimEnd()).join("\n");
}

if (flags.list) {
  console.log("named timelines:");
  for (const row of platter.named_timelines.rows) {
    console.log(`  ${row.id.padEnd(10)} n=${String(row.n).padEnd(3)} ${row.box}  ${row.euclidean ? "EUCLIDEAN" : ""}`);
  }
  console.log(`\neuclidean catalogue: ${platter.euclidean_catalogue.count} entries, ` +
    `all cross-checked against bjorklund (${platter.euclidean_catalogue.all_verified_against_bjorklund})`);
  console.log("  source: Toussaint (2005) §4 via papers/rhythm-platter/");
  process.exit(0);
}

const first = resolve(args);
if (!first) {
  console.error("usage: necklace.mjs <E k n | name | box> [second] [--space] [--morph] [--bpm N] [--list]");
  process.exit(1);
}

// ── morph between two rhythms ───────────────────────────────────────────────
if (flags.morph) {
  const second = resolve(first.rest);
  if (!second) { console.error("--morph needs two rhythms"); process.exit(1); }
  const d = dist(first.r, second.r, { measure: "chronotonic" });
  console.log(`${first.label} → ${second.label}`);
  console.log(`chronotonic distance ${d.distance.toFixed(4)} at rotation ${d.rotation}\n`);
  const path = morphPath(first.r, second.r, { align: false });
  path.forEach((step, i) => console.log(`  ${String(i).padStart(2)}  ${toBox(step)}  (${toIoi(step).join(",")})`));
  process.exit(0);
}

// ── spatial placement ───────────────────────────────────────────────────────
if (flags.space) {
  const bpm = Number(flags.bpm ?? 120);
  const report = spatialReport(first.r, bpm, {
    elevation: flags.elevation ?? "flat",
    mirror: !!flags.mirror,
    rotation: Number(flags.rotation ?? 0),
  });
  console.log(`${first.label} spatialised at ${bpm} BPM  (n=${report.n}, k=${report.k})\n`);
  console.log("  step   azimuth      elevation   distance");
  for (const p of report.positions) {
    console.log(`  ${String(p.step).padStart(4)}   ${p.azimuthDeg.toFixed(1).padStart(7)}°   ` +
      `${(p.elevation * 180 / Math.PI).toFixed(1).padStart(7)}°   ${p.distance}`);
  }
  const b = report.balance;
  console.log(`\n  sound-field centroid: ${b.balanced ? "none — perfectly balanced" : `${b.angleDeg.toFixed(1)}° (magnitude ${b.magnitude.toFixed(3)})`}`);
  console.log(`  step spacing: ${report.minAudibleAngle.spacingDeg.toFixed(2)}°  ` +
    `(frontally resolvable: ${report.minAudibleAngle.resolvableFrontally}, laterally: ${report.minAudibleAngle.lateral})`);
  console.log(`  step duration: ${report.precedence.stepMs.toFixed(1)} ms   cycle: ${(report.entrainment.cycleMs / 1000).toFixed(2)} s`);
  console.log(`  max precession: ${report.precession.maxStepsPerPhrase} steps/phrase`);
  if (report.warnings.length) {
    console.log("\n  warnings:");
    for (const w of report.warnings) console.log(`    ! ${w}`);
  } else console.log("\n  no perceptual warnings.");
  process.exit(0);
}

// ── the default report ──────────────────────────────────────────────────────
const a = analyze(first.r);
console.log(`\n${first.label}   n=${a.n}  k=${a.k}\n`);
console.log(circle(first.r));
console.log(`\n  box          ${a.box}`);
console.log(`  onsets       [${a.onsets.join(",")}]`);
console.log(`  IOI          (${a.ioi.join(",")})`);
console.log(`  necklace     ${a.necklace}`);
console.log(`  bracelet     ${a.bracelet}`);
console.log(`  complement   ${toBox(complement(first.r))}`);
console.log(`\n  euclidean    ${a.euclidean}`);
console.log(`  evenness     ${a.evenness.toFixed(4)}   (chord-sum, Demaine et al. 2009)`);
console.log(`  evenness E   ${a.evennessIoi.toFixed(4)}   (AC-local, hypnotek.c)`);
console.log(`  vertex D     ${a.vertexDistance.toFixed(3)}    (AC-local, hypnotek.c)`);
console.log(`  IOI variance ${a.ioiVariance.toFixed(4)}`);
console.log(`  balance      ${a.balance.balanced ? "perfectly balanced (no direction)" : `${a.balance.magnitude.toFixed(4)} at ${(a.balance.angle * 180 / Math.PI).toFixed(1)}°`}`);
console.log(`\n  oddity       ${a.rhythmicOddity === null ? "n/a (odd cycle)" : a.rhythmicOddity}`);
console.log(`  off-beatness ${a.offbeatness.count}${a.offbeatness.degenerate ? "  ! degenerate: n is prime" : ""}  at [${a.offbeatness.positions.join(",")}]`);
console.log(`  deep         winograd=${a.winogradDeep}  erdos=${a.erdosDeep}`);
console.log(`  periodic     ${a.periodic}`);
console.log(`  intervals    [${a.intervalVector.join(",")}]`);
console.log(`\n  syncopation  lhl=${a.syncopation.lhl}  c-score=${a.syncopation.cScore}  note-to-beat=${a.syncopation.noteToBeat.toFixed(2)}`);
if (a.n <= 20 && a.k <= 8) {
  console.log(`\n  ${enumerateNecklaces(a.n, a.k).length} necklaces exist with these (n,k); this is ${necklaceCanonical(first.r)}`);
}
console.log("");
