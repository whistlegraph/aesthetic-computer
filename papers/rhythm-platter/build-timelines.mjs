#!/usr/bin/env node
// build-timelines.mjs — regenerate timelines.json from the fetched sources.
//
// Reads sources/toussaint-2005-bridges.txt (produced by fetch-sources.mjs) and
// extracts the E(k,n) catalogue as STRUCTURED FACTS ONLY: k, n, onsets, IOI,
// box string, necklace representative, and culture tags. Source prose is never
// copied into the repo; each row carries a locator back into the paper instead.
//
// Every extracted box string is cross-checked against a local Bjorklund
// implementation. That check is the whole point of doing this before writing
// pop/lib: if our Bjorklund disagrees with the published catalogue anywhere,
// the library is wrong and we find out here rather than inside a track.
//
//   node papers/rhythm-platter/build-timelines.mjs

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SRC = join(HERE, "sources", "toussaint-2005-bridges.txt");

if (!existsSync(SRC)) {
  console.error(`✗ ${SRC} missing — run: node papers/rhythm-platter/fetch-sources.mjs`);
  process.exit(1);
}

// ── the reference implementation under test ────────────────────────────────
// Bjorklund's algorithm: grow k "[1]" groups and (n-k) "[0]" groups, then fold
// the smaller pile onto the larger until one pile has at most one group.
export function bjorklund(k, n) {
  if (k <= 0 || k > n) return [];
  let a = Array.from({ length: k }, () => [1]);
  let b = Array.from({ length: n - k }, () => [0]);
  while (b.length > 1) {
    const m = Math.min(a.length, b.length);
    const na = [], nb = [];
    for (let i = 0; i < m; i++) na.push(a[i].concat(b[i]));
    const rest = a.length > m ? a : b;
    for (let i = m; i < rest.length; i++) nb.push(rest[i]);
    a = na; b = nb;
  }
  return a.concat(b).flat();
}

// The necklace representative: lexicographically least rotation. Two rhythms
// are the same necklace iff their representatives are equal. Comparing raw
// strings instead is the classic bug — the published catalogue prints one
// arbitrary rotation per entry.
export const necklaceOf = (bits) => {
  let best = null;
  for (let r = 0; r < bits.length; r++) {
    const t = bits.slice(r).concat(bits.slice(0, r)).join("");
    if (best === null || t < best) best = t;
  }
  return best;
};

const toBits = (onsets, n) => { const s = Array(n).fill(0); onsets.forEach((i) => (s[i] = 1)); return s; };
const toBox = (bits) => bits.map((v) => (v ? "x" : ".")).join("");
const onsetsOf = (bits) => bits.map((v, i) => (v ? i : -1)).filter((i) => i >= 0);
const ioisOf = (o, n) => o.map((v, i) => (i + 1 < o.length ? o[i + 1] - v : n - v + o[0]));

// ── extract the catalogue ──────────────────────────────────────────────────
const raw = readFileSync(SRC, "utf8").split("\n");
const HEAD = /^E\((\d+),\s*(\d+)\)\s*=\s*\[([x.\s]+)\]\s*=\s*\((\d+)\)\s*(.*)$/;
const entries = [];
let cur = null;
for (const ln of raw) {
  const m = ln.match(HEAD);
  if (m) {
    if (cur) entries.push(cur);
    cur = { k: +m[1], n: +m[2], box: m[3].replace(/\s+/g, ""), ioi: m[4], text: m[5].trim() };
  } else if (cur) {
    if (/^E\(/.test(ln) || /^\s*$/.test(ln)) { entries.push(cur); cur = null; }
    else cur.text += " " + ln.trim();
  }
}
if (cur) entries.push(cur);

// The paper lists some E(k,n) twice (main catalogue, then the aksak taxonomy).
// Keep the longer attribution paragraph, which is the catalogue one.
const byKey = new Map();
for (const e of entries) {
  const key = `${e.k},${e.n}`;
  const prev = byKey.get(key);
  if (!prev || e.text.length > prev.text.length) byKey.set(key, e);
}

// Culture / genre tags, matched against a fixed vocabulary. Tags are facts about
// attribution; the surrounding prose stays in the paper.
const VOCAB = ["Greece", "Greek", "Bulgaria", "Bulgarian", "Macedonia", "Macedonian", "Turkey",
  "Turkish", "India", "Indian", "Persia", "Persian", "Brazil", "Brazilian", "Cuba", "Cuban",
  "Mexico", "Mexican", "Africa", "African", "Ghana", "Ashanti", "Bantu", "Mandinka", "Tuareg",
  "Nubia", "Namibia", "Rwanda", "Central African Republic", "Aka", "Pygmies", "Pygmy", "Sudan",
  "Turkestan", "Serbia", "Serbian", "Arab", "Colombia", "Trinidad", "Bali", "Indonesian",
  "flamenco", "Bossa-Nova", "Samba", "Calypso", "Cumbia", "Rumba", "Korea", "Korean", "Romania",
  "Romanian", "Nigeria", "Bohlen-Pierce", "Steve Reich", "ragtime", "jazz",
  "electronic dance music"];

let mismatches = 0;
const euclidean = [...byKey.values()]
  .sort((a, b) => a.n - b.n || a.k - b.k)
  .map((e) => {
    const printed = e.box.split("").map((c) => (c === "x" ? 1 : 0));
    const ok = printed.length === e.n && necklaceOf(printed) === necklaceOf(bjorklund(e.k, e.n));
    if (!ok) { mismatches++; console.error(`  ✗ MISMATCH ${e.k},${e.n}: printed ${e.box}`); }
    return {
      id: `E(${e.k},${e.n})`, k: e.k, n: e.n,
      onsets: onsetsOf(printed), ioi: e.ioi.split("").map(Number),
      box: e.box, necklace: necklaceOf(printed),
      euclidean: true, verified_against_bjorklund: ok,
      tags: [...new Set(VOCAB.filter((v) => e.text.includes(v)))].slice(0, 12),
      source: "toussaint-2005-bridges",
      source_locator: `§4 catalogue entry E(${e.k},${e.n})`,
    };
  });

// ── named timelines the /pop tooling needs ─────────────────────────────────
// The six distinguished 5-onset/16-pulse timelines Toussaint uses throughout the
// clave papers, plus the binary and ternary anchors. checked:false means the
// onset set is the standard published form but has not been re-verified against
// a primary source in this pass.
const NAMED = [
  ["son", [0, 3, 6, 10, 12], 16, ["Cuba"], "The 3-2 son clave. IOI (3,3,4,2,4) — not maximally even, so NOT a Euclidean rhythm."],
  ["rumba", [0, 3, 7, 10, 12], 16, ["Cuba"], "The 3-2 rumba clave: the son with its third stroke displaced by one pulse."],
  ["bossa", [0, 3, 6, 10, 13], 16, ["Brazil", "Bossa-Nova"], "The bossa-nova clave. This — not the son — is the E(5,16) necklace."],
  ["shiko", [0, 4, 6, 10, 12], 16, ["Africa"], "Bell timeline, IOI (4,2,4,2,4)."],
  ["soukous", [0, 3, 6, 10, 11], 16, ["Africa"], "Congolese timeline, IOI (3,3,4,1,5)."],
  ["gahu", [0, 3, 6, 10, 14], 16, ["Ghana"], "Ewe timeline, IOI (3,3,4,4,2)."],
  ["tresillo", [0, 3, 6], 8, ["Cuba"], "First bar of the son clave; the E(3,8) necklace."],
  ["cinquillo", [0, 2, 3, 5, 6], 8, ["Cuba"], "The E(5,8) necklace."],
  ["bembe", [0, 2, 4, 5, 7, 9, 11], 12, ["Africa", "Ghana"], "The standard 12-pulse bell; the E(7,12) necklace."],
];

const named = NAMED.map(([id, onsets, n, tags, gloss]) => {
  const bits = toBits(onsets, n);
  const nk = necklaceOf(bits);
  return {
    id, k: onsets.length, n, onsets, ioi: ioisOf(onsets, n),
    box: toBox(bits), necklace: nk,
    euclidean: nk === necklaceOf(bjorklund(onsets.length, n)),
    tags, gloss,
    source: "toussaint-2002-clave / toussaint-2013-book",
    checked: false,
  };
});

const doc = {
  generated_by: "papers/rhythm-platter/build-timelines.mjs",
  regenerate: "node papers/rhythm-platter/fetch-sources.mjs && node papers/rhythm-platter/build-timelines.mjs",
  posture: "Structured facts only. Source prose is not reproduced; each row carries a locator instead.",
  euclidean_catalogue: {
    source: "toussaint-2005-bridges",
    count: euclidean.length,
    all_verified_against_bjorklund: mismatches === 0,
    rows: euclidean,
  },
  named_timelines: { count: named.length, rows: named },
};

writeFileSync(join(HERE, "timelines.json"), JSON.stringify(doc, null, 2) + "\n");
console.log(`✓ ${euclidean.length} Euclidean + ${named.length} named → timelines.json`);
console.log(`  bjorklund cross-check: ${mismatches === 0 ? "all agree" : `${mismatches} MISMATCH`}`);
console.log(`  euclidean among named: ${named.filter((r) => r.euclidean).map((r) => r.id).join(", ")}`);
if (mismatches) process.exit(1);
