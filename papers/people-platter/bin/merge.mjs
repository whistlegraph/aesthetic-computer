#!/usr/bin/env node
// Merge the three sweep markdown tables into one JSON manifest.
// Run from anywhere; paths resolve relative to this script.
//   node papers/people-platter/bin/merge.mjs
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");          // papers/people-platter/
const SWEEPS = resolve(ROOT, "sweeps");
const OUT = resolve(ROOT, "merged.json");

const TRACK_FILES = {
  A: [resolve(SWEEPS, "track-a-papers.md")],
  B: [resolve(SWEEPS, "track-b-contributors.md")],
  C: [
    resolve(SWEEPS, "track-c-plans-todos-v1.md"),
    resolve(SWEEPS, "track-c-plans-todos-v2.md"),
  ],
};

// Manual exclusion list — known non-people that bypass our regex.
const EXCLUDE = new Set([
  "Handle", "Real name", "Login", "Identity", "Name", "Variants", "Sources",
  "Aesthetic.Computer", "Oven (aesthetic.computer)",
  "@jeffrey on prompt.ac", "prompt.ac/@jeffrey", "jeffrey",
  "Jeffrey Alan Scudder", "Jeffrey Scudder", "DIGITPAIN", "@whistlegraph",
  "whistlegraph",
  "claude", "Claude", "Copilot", "copilot-swe-agent[bot]",
  "(dan / rcrdlbl)", // raw alias row; canonical name unknown
]);

// Aliases — collapse known same-person rows into a canonical name.
const ALIAS = new Map([
  ["Sam Aaron", "Samuel Aaron"],
  ["Tim Berners", "Tim Berners-Lee"],
  ["Nick Bergson", "Nick Bergson-Shilcock"],
  ["@georgica", "Georgica Pettus"],
  ["ggcajp", "Georgica Pettus"],
  ["@ida", "Ida Pruitt"],
  ["idapruitt", "Ida Pruitt"],
  ["Ida (Pruitt?)", "Ida Pruitt"],
  ["@maya", "Maya Man"],
  ["mayaman", "Maya Man"],
  ["@mxsage", "Sage Jenson"],
  ["mxsage", "Sage Jenson"],
  ["Sage Jenson", "Sage Jenson"],
  ["tinatari", "Tina Tarighian"],
  ["ttarigh", "Tina Tarighian"],
  ["mileshiroo", "Miles Peyton"],
  ["estebanuribe", "Esteban Uribe"],
  ["rcrdlbl", "rcrdlbl (dan@tlon.io)"],
  ["rackodo", "Bash Elliott"],
]);

function canonical(name) {
  return ALIAS.get(name) ?? name;
}

function parseTable(md, track) {
  // Find every line starting with `|` that has 4+ pipe-separated columns
  // Skip header and separator rows.
  const rows = [];
  for (const line of md.split("\n")) {
    if (!line.startsWith("|")) continue;
    const cells = line.split("|").slice(1, -1).map(s => s.trim());
    if (cells.length < 2) continue;
    if (cells[0] === "" || /^[- :]+$/.test(cells[0])) continue; // separator
    if (cells[0] === "Name" || cells[0] === "Identity" || cells[0] === "Login" || cells[0] === "Real name" || cells[0] === "Handle") continue;
    rows.push({ track, cells });
  }
  return rows;
}

function ingest(rowsByTrack) {
  // people: Map<canonicalName, {tracks: Set, rows: [{track, cells}]}>
  const people = new Map();
  for (const [track, rows] of Object.entries(rowsByTrack)) {
    for (const row of rows) {
      const raw = row.cells[0].replace(/\*\*/g, "").replace(/`/g, "").trim();
      if (!raw || EXCLUDE.has(raw)) continue;
      // skip handles in track A/C if they're not ours, but keep names with hyphens/etc.
      const name = canonical(raw);
      if (EXCLUDE.has(name)) continue;
      if (!people.has(name)) {
        people.set(name, { name, tracks: new Set(), rows: [] });
      }
      const p = people.get(name);
      p.tracks.add(track);
      p.rows.push({ track, cells: row.cells });
    }
  }
  return people;
}

const rowsByTrack = {};
for (const [track, files] of Object.entries(TRACK_FILES)) {
  rowsByTrack[track] = [];
  for (const file of files) {
    const md = readFileSync(file, "utf8");
    rowsByTrack[track].push(...parseTable(md, track));
  }
}

const people = ingest(rowsByTrack);

// Build output array, sorted by surname (last whitespace-token), then full name.
function surnameKey(name) {
  const cleaned = name.replace(/[^\p{L}\p{M}\s.'-]/gu, "").trim();
  const tokens = cleaned.split(/\s+/);
  return (tokens[tokens.length - 1] || "").toLowerCase();
}

const out = [...people.values()]
  .map(p => {
    // Try to extract a Role/Context column (col 3 or 4 depending on schema)
    const merged = { name: p.name, tracks: [...p.tracks].sort() };
    const sourcesSet = new Set();
    const rolesSet = new Set();
    for (const r of p.rows) {
      // Schema A: | Name | Variants | Sources | Role/Context | Quote |
      // Schema B (git committers): | Name | Email | Commits | First | Last | Notes |
      // Schema B (handles): | Handle | Files | Context |
      // Schema C: | Name | Variants | Sources | Role/Context | Quote |
      if (r.track === "A" || r.track === "C") {
        if (r.cells[2]) sourcesSet.add(r.cells[2]);
        if (r.cells[3]) rolesSet.add(r.cells[3]);
      } else {
        // B
        if (r.cells[5]) rolesSet.add(r.cells[5]); // Notes for committers
        else if (r.cells[2]) rolesSet.add(r.cells[2]); // Context for handles
        sourcesSet.add("git/AC handles");
      }
    }
    merged.sources = [...sourcesSet];
    merged.roles = [...rolesSet];
    return merged;
  })
  .sort((a, b) => {
    const sa = surnameKey(a.name);
    const sb = surnameKey(b.name);
    if (sa !== sb) return sa.localeCompare(sb);
    return a.name.localeCompare(b.name);
  });

writeFileSync(OUT, JSON.stringify(out, null, 2));

const counts = { total: out.length };
for (const t of ["A", "B", "C"]) {
  counts[t] = out.filter(p => p.tracks.includes(t)).length;
}
counts.AB = out.filter(p => p.tracks.includes("A") && p.tracks.includes("B")).length;
counts.AC = out.filter(p => p.tracks.includes("A") && p.tracks.includes("C")).length;
counts.BC = out.filter(p => p.tracks.includes("B") && p.tracks.includes("C")).length;
counts.ABC = out.filter(p => p.tracks.length === 3).length;

console.log(JSON.stringify(counts, null, 2));
