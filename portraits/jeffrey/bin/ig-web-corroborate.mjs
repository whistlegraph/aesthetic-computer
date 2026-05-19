#!/usr/bin/env node
// Stage 5 (optional, web-only — zero Instagram load): cross-reference each
// candidate against Wikidata + Wikipedia to catch art-world identity that
// NEITHER the name heuristic NOR Instagram's category field reveal — empty-
// bio accounts and plain-name handles whose curator / artist / gallerist /
// institution identity only exists in structured public knowledge bases.
//
// Why Wikidata, not Google/DDG: general-web scraping bot-challenges (HTTP
// 202) within a few queries and won't survive a 600+ batch. Wikidata is
// free, key-less, unthrottled, and *structured* — it states occupation
// (artist/curator/gallerist), instance-of (art gallery/art museum), and
// employer/member-of (the institutions a person runs or works at) directly,
// which is exactly "find art-world people and who they work with" at higher
// precision than parsing search snippets. Cached DDG hits (whitney, moca…)
// from the earlier pass are kept in webcache/ and still readable.
//
// Account-safe by construction: never touches instagram.com. Every API
// response cached to webcache/<sha1>.json so re-runs cost $0 and resume
// instantly; polite jitter + backoff for the flaky local network.
//
// Usage:
//   node bin/ig-web-corroborate.mjs whistlegraph                # all candidates
//   node bin/ig-web-corroborate.mjs whistlegraph --cap 1090     # priority slice
//   node bin/ig-web-corroborate.mjs whistlegraph --delay 0.4    # secs between live calls
//
// Reads:  social/<acct>/candidates.json  (+ profiles.jsonl if present)
// Writes: social/<acct>/web.jsonl         (one verdict per line, resumable)
//         social/<acct>/webcache/*.json   (raw API responses, gitignored)

import {
  readFileSync,
  writeFileSync,
  existsSync,
  mkdirSync,
  appendFileSync,
} from "fs";
import { join } from "path";
import { createHash } from "crypto";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");

const account = process.argv[2];
if (!account || account.startsWith("--")) {
  console.error("usage: ig-web-corroborate.mjs <account> [--cap N] [--delay S]");
  process.exit(64);
}
const arg = (k, d) => {
  const i = process.argv.indexOf(k);
  return i > -1 ? process.argv[i + 1] : d;
};
const CAP = parseInt(arg("--cap", "0"), 10); // 0 = no cap
const DELAY = Math.max(0, parseFloat(arg("--delay", "0.4"))) * 1000;

const dir = join(REPO_ROOT, "portraits/jeffrey/social", account);
const cacheDir = join(dir, "webcache");
mkdirSync(cacheDir, { recursive: true });
const outPath = join(dir, "web.jsonl");

const UA = "ac-research/1.0 (mail@aesthetic.computer) art-world-graph";

// Art-world lexicon matched against Wikidata descriptions + occupation /
// instance-of labels. Lowercased substring match — deliberately broad on the
// recall side; a wrong include is one harmless faint node, a miss loses a
// real gallery.
const ART_PERSON = [
  "artist",
  "painter",
  "sculptor",
  "photographer",
  "printmaker",
  "video art",
  "new media",
  "digital art",
  "net art",
  "installation art",
  "performance art",
  "conceptual art",
  "curator",
  "gallerist",
  "art dealer",
  "art historian",
  "art critic",
  "arts administrator",
  "museum director",
  "art collector",
  "art patron",
];
const ART_ORG = [
  "art gallery",
  "contemporary art gallery",
  "art museum",
  "modern art",
  "contemporary art",
  "kunsthalle",
  "kunstverein",
  "art centre",
  "art center",
  "arts organization",
  "arts organisation",
  "art foundation",
  "artist-run",
  "biennial",
  "biennale",
  "art space",
  "art institution",
  "art school",
  "art collective",
];
const matchAny = (s, list) => {
  const t = (s || "").toLowerCase();
  return list.filter((w) => t.includes(w));
};

const sha1 = (s) => createHash("sha1").update(s).digest("hex");

function readJsonl(p) {
  if (!existsSync(p)) return [];
  return readFileSync(p, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => {
      try {
        return JSON.parse(l);
      } catch {
        return null;
      }
    })
    .filter(Boolean);
}

// Clean-only seen-set: error rows are retried on re-run (flaky network).
function loadDone() {
  const done = new Set();
  for (const r of readJsonl(outPath))
    if (r.username && !r.error) done.add(r.username);
  return done;
}

function looksLikeName(s) {
  return !!s && s.trim().length >= 3 && /[a-zA-Z]/.test(s);
}

// One cached, retried JSON GET. Cache key is the full URL.
async function apiGet(url, { live }) {
  const cachePath = join(cacheDir, `${sha1(url)}.json`);
  if (existsSync(cachePath)) {
    try {
      return { data: JSON.parse(readFileSync(cachePath, "utf8")), cached: true };
    } catch {
      /* fall through to refetch */
    }
  }
  let lastErr;
  for (let attempt = 1; attempt <= 5; attempt++) {
    try {
      if (live.v) {
        const wait = DELAY + Math.random() * Math.max(DELAY, 600);
        await new Promise((r) => setTimeout(r, wait));
      }
      const res = await fetch(url, {
        headers: { "User-Agent": UA, "Accept-Language": "en" },
        signal: AbortSignal.timeout(20000),
      });
      live.v = true;
      if (res.status === 429 || res.status >= 500) {
        throw new Error(`http ${res.status}`);
      }
      const data = await res.json();
      writeFileSync(cachePath, JSON.stringify(data));
      return { data, cached: false };
    } catch (e) {
      lastErr = e;
      const wait = Math.min(10000 * attempt, 90000);
      console.error(`  api retry ${attempt}/5 in ${wait / 1000}s (${e.message})`);
      await new Promise((r) => setTimeout(r, wait));
    }
  }
  throw lastErr;
}

const WD = "https://www.wikidata.org/w/api.php";

async function corroborate(name, live) {
  // 1. search Wikidata for the name (returns id + a human description).
  const sUrl =
    `${WD}?action=wbsearchentities&format=json&language=en&uselang=en` +
    `&type=item&limit=5&search=${encodeURIComponent(name)}`;
  const { data: s } = await apiGet(sUrl, { live });
  const hits = s.search || [];
  if (!hits.length) {
    return { web_artworld: false, web_score: 0, note: "no wikidata entity" };
  }

  // Prefer the first hit whose description already reads art-world; else
  // take the top hit and inspect its claims.
  let chosen =
    hits.find((h) =>
      matchAny(h.description, [...ART_PERSON, ...ART_ORG]).length,
    ) || hits[0];

  const descHitsP = matchAny(chosen.description, ART_PERSON);
  const descHitsO = matchAny(chosen.description, ART_ORG);

  // 2. pull occupation (P106), instance-of (P31), employer (P108),
  //    member-of (P463) — the last two are "who they run / work at".
  const eUrl =
    `${WD}?action=wbgetentities&format=json&languages=en` +
    `&props=claims|descriptions&ids=${chosen.id}`;
  const { data: e } = await apiGet(eUrl, { live });
  const ent = e.entities?.[chosen.id] || {};
  const claims = ent.claims || {};
  const desc =
    ent.descriptions?.en?.value || chosen.description || null;

  const refQids = new Set();
  const idsOf = (prop) =>
    (claims[prop] || [])
      .map((c) => c.mainsnak?.datavalue?.value?.id)
      .filter(Boolean);
  const occ = idsOf("P106");
  const inst = idsOf("P31");
  const emp = idsOf("P108");
  const mem = idsOf("P463");
  [...occ, ...inst, ...emp, ...mem].forEach((q) => refQids.add(q));

  // 3. resolve all referenced QIDs to labels in ONE batched call.
  let labels = {};
  if (refQids.size) {
    const lUrl =
      `${WD}?action=wbgetentities&format=json&languages=en&props=labels` +
      `&ids=${[...refQids].slice(0, 50).join("|")}`;
    const { data: l } = await apiGet(lUrl, { live });
    for (const [q, v] of Object.entries(l.entities || {}))
      labels[q] = v.labels?.en?.value || null;
  }
  const lab = (q) => labels[q] || null;

  const occupations = occ.map(lab).filter(Boolean);
  const instanceOf = inst.map(lab).filter(Boolean);
  const affiliations = [...emp, ...mem]
    .map((q) => ({ org: lab(q), qid: q }))
    .filter((a) => a.org);

  const occArt = matchAny(occupations.join(" · "), ART_PERSON);
  const instArt = matchAny(instanceOf.join(" · "), ART_ORG);

  const personArt = descHitsP.length || occArt.length;
  const orgArt = descHitsO.length || instArt.length;
  const web_artworld = !!(personArt || orgArt);
  const web_score =
    (descHitsP.length + occArt.length) * 25 +
    (descHitsO.length + instArt.length) * 30 +
    Math.min(affiliations.length, 4) * 8;

  return {
    web_artworld,
    web_score,
    wikidata: chosen.id,
    description: desc,
    occupations,
    instance_of: instanceOf,
    affiliations, // employer / member-of orgs = who they run / work at
    signals: [
      ...descHitsP.map((w) => `desc:${w}`),
      ...descHitsO.map((w) => `desc:${w}`),
      ...occArt.map((w) => `occ:${w}`),
      ...instArt.map((w) => `inst:${w}`),
    ],
    sources: [`wikidata:${chosen.id}`],
  };
}

// ── main ───────────────────────────────────────────────────────────────────
const candPath = join(dir, "candidates.json");
if (!existsSync(candPath)) {
  console.error(`no candidates.json — run ig-artworld-classify.mjs ${account}`);
  process.exit(1);
}
const candidates = JSON.parse(readFileSync(candPath, "utf8")).candidates;
const profByUser = new Map(
  readJsonl(join(dir, "profiles.jsonl"))
    .filter((r) => !r.error)
    .map((p) => [p.username, p]),
);

const done = loadDone();
let queue = candidates.filter((c) => !done.has(c.username)); // priority order
if (CAP > 0) queue = queue.slice(0, CAP);

console.error(
  `web-corroborate (wikidata): ${candidates.length} candidates, ` +
    `${done.size} done, ${queue.length} this run${CAP ? ` (cap ${CAP})` : ""}`,
);

const live = { v: false }; // shared so we only pace between LIVE fetches
let n = 0;
const started = Date.now();
for (const c of queue) {
  const pr = profByUser.get(c.username);
  const name = (pr?.full_name || c.full_name || "").trim();
  let row;
  try {
    if (!looksLikeName(name)) {
      row = {
        username: c.username,
        full_name: name || null,
        web_artworld: false,
        web_score: 0,
        note: "no searchable name",
      };
    } else {
      row = {
        username: c.username,
        full_name: name,
        ...(await corroborate(name, live)),
        heuristic_type: c.type,
        ig_category: pr?.ig_category || null,
      };
    }
  } catch (err) {
    row = { username: c.username, error: String(err.message || err) };
  }
  row.fetched_at = new Date().toISOString();
  appendFileSync(outPath, JSON.stringify(row) + "\n");
  n++;
  if (n % 25 === 0 || n === queue.length) {
    const rate = n / Math.max((Date.now() - started) / 1000, 1e-6);
    const all = readJsonl(outPath);
    const hits = all.filter((r) => r.web_artworld).length;
    console.error(
      `  [${n}/${queue.length}] ${rate.toFixed(2)}/s  ` +
        `web_artworld total=${hits}  last=@${c.username}`,
    );
  }
}
console.error(`done. → ${outPath}`);
