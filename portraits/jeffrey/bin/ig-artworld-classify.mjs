#!/usr/bin/env node
// Offline (zero-network) art-world classifier + Phase-2 priority ranker.
//
// Reads the lightweight 1st-degree node lists produced by ig-social-graph.py
// and labels each node with a contemporary-art-world type from name/username
// signals + a curated seed allowlist. The point is to map the gallery world
// @whistlegraph sits in — galleries, museums, institutions, artist-run/project
// spaces, art media/fairs, and the people who run them — NOT to enumerate
// fans. The output ranking decides the order ig-social-enrich.py spends its
// scarce, ban-risky authenticated profile requests: art-world nodes first,
// generic fans never (unless --include-fans).
//
// Usage:
//   node bin/ig-artworld-classify.mjs whistlegraph
//
// Reads:  portraits/jeffrey/social/<acct>/{followers,following}.jsonl
// Writes: portraits/jeffrey/social/<acct>/candidates.json
//
// NOTE: name/username signals are a coarse first pass. The authoritative
// type comes later from Instagram's own category_name + bio (Phase 2). Here
// we only need "is this plausibly art-world, and how strongly" to prioritize.

import { readFileSync, writeFileSync, existsSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");

const account = process.argv[2];
if (!account) {
  console.error("usage: ig-artworld-classify.mjs <account>");
  process.exit(64);
}

const socialDir = join(REPO_ROOT, "portraits/jeffrey/social", account);

// ── Seed allowlist ─────────────────────────────────────────────────────────
// Accounts we already know are art-world: the ones already pulled into
// ig-archive/ for collector research, plus canonical institutions/galleries.
// These get a strong score so Phase 2 confirms them first.
const SEED_TYPES = {
  // already archived here for collector/peer research
  kadistkadist: "institution", // KADIST foundation
  moca: "museum", // MOCA Los Angeles
  printedmatter_artbookfairs: "art_media", // Printed Matter / NY+LA art book fairs
  petra_cortright: "artist",
  clairelevans: "artist",
  "julia.voloshyna.art": "artist",
  // canonical contemporary galleries / institutions (seed, extend freely)
  gagosian: "gallery",
  hauserwirth: "gallery",
  davidzwirner: "gallery",
  pacegallery: "gallery",
  perrotin: "gallery",
  thaddaeusropac: "gallery",
  whitecube: "gallery",
  sadiecoles: "gallery",
  gladstone_gallery: "gallery",
  mariangoodmangallery: "gallery",
  bortolami_gallery: "gallery",
  bridgetdonahue_nyc: "gallery",
  "47canal": "gallery",
  galerie_buchholz: "gallery",
  moma: "museum",
  whitneymuseum: "museum",
  guggenheim: "museum",
  newmuseum: "museum",
  thebroad: "museum",
  walkerartcenter: "museum",
  tate: "museum",
  serpentineuk: "institution",
  kw_institute: "institution",
  swiss_institute: "institution",
  rhizomedotorg: "institution", // Rhizome — net-art institution (relevant to AC)
  eyebeamnyc: "institution",
  pioneerworks: "institution",
  thekitchen_nyc: "institution",
  artforum: "art_media",
  artnews: "art_media",
  frieze: "art_media",
  e_flux: "art_media",
  artbasel: "art_media",
};

// ── Keyword signals (lowercased substring / word match) ────────────────────
// Ordered by specificity; first match wins the type, but ALL matches are kept
// in `signals` for auditability.
const TYPE_KEYWORDS = [
  ["museum", ["museum", "musée", "museo", "kunstmuseum", "moca", "mfa "]],
  [
    "institution",
    [
      "foundation",
      "fondation",
      "fondazione",
      "stiftung",
      "kunsthalle",
      "kunstverein",
      "kunsthaus",
      "institute of art",
      "art institute",
      "art center",
      "arts centre",
      "biennale",
      "biennial",
      "triennial",
      "documenta",
      "art council",
    ],
  ],
  [
    "gallery",
    [
      "gallery",
      "galerie",
      "galería",
      "galleria",
      "gallerie",
      " projects",
      "project space",
      "artist-run",
      "artist run",
      "art space",
      "fine art",
      "contemporary art",
      "editions",
    ],
  ],
  [
    "art_media",
    [
      "art fair",
      "art book",
      "artbook",
      "art magazine",
      "art journal",
      "art review",
      "art press",
      "publishing",
      "printed matter",
    ],
  ],
  [
    "person_role",
    [
      "curator",
      "gallerist",
      "art dealer",
      "art director",
      "director of",
      "founder of",
      "chief curator",
      "registrar",
      "art advisor",
      "art consultant",
      "art historian",
      "collector",
      "patron",
    ],
  ],
  [
    "artist",
    [
      "artist",
      "studio",
      "practice",
      "sculptor",
      "painter",
      "media art",
      "new media",
      "net art",
      "digital art",
      "kunst", // de: "art" — broad, low weight (below)
    ],
  ],
];

const WEIGHT = {
  seed: 100,
  museum: 60,
  institution: 55,
  gallery: 50,
  art_media: 45,
  person_role: 40,
  artist: 25,
  verified: 8,
  business: 5,
  in_both: 6, // mutual follow → stronger relationship signal
};

function readJsonl(path) {
  if (!existsSync(path)) return [];
  return readFileSync(path, "utf8")
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

function classify(node, lists) {
  const u = (node.username || "").toLowerCase();
  const fn = (node.full_name || "").toLowerCase();
  const hay = `${u} ${fn}`;
  const signals = [];
  let score = 0;
  let type = "other";

  if (SEED_TYPES[u]) {
    type = SEED_TYPES[u];
    score += WEIGHT.seed;
    signals.push(`seed:${type}`);
  }

  for (const [t, words] of TYPE_KEYWORDS) {
    for (const w of words) {
      if (hay.includes(w)) {
        signals.push(`kw:${t}:${w.trim()}`);
        // "kunst" alone is too broad — quarter weight.
        const add = w === "kunst" ? Math.round(WEIGHT.artist / 4) : WEIGHT[t === "person_role" ? "person_role" : t] ?? WEIGHT.artist;
        score += add;
        if (type === "other") type = t === "person_role" ? "curator" : t;
        break; // one hit per type bucket is enough
      }
    }
  }

  if (node.is_verified) {
    score += WEIGHT.verified;
    signals.push("verified");
  }
  if (node.is_business_account) {
    score += WEIGHT.business;
    signals.push("business");
  }
  if (lists.size === 2) {
    score += WEIGHT.in_both;
    signals.push("mutual");
  }

  return { type, score, signals };
}

// ── Merge the two lists, keyed by username ─────────────────────────────────
const followers = readJsonl(join(socialDir, "followers.jsonl"));
const following = readJsonl(join(socialDir, "following.jsonl"));

if (!followers.length && !following.length) {
  console.error(
    `no node lists at ${socialDir}. Run bin/ig-social-graph.py ${account} first.`,
  );
  process.exit(1);
}

const byUser = new Map();
const note = (node, which) => {
  const u = node.username;
  if (!byUser.has(u)) byUser.set(u, { node, lists: new Set() });
  byUser.get(u).lists.add(which);
};
followers.forEach((n) => note(n, "followers"));
following.forEach((n) => note(n, "following"));

const candidates = [];
for (const { node, lists } of byUser.values()) {
  const { type, score, signals } = classify(node, lists);
  candidates.push({
    username: node.username,
    full_name: node.full_name || null,
    type,
    score,
    art_world: type !== "other" && type !== "artist" ? true : score >= WEIGHT.artist + WEIGHT.verified,
    relation:
      lists.size === 2
        ? "mutual"
        : lists.has("following")
          ? "idol" // jeffrey follows them, no follow-back
          : "fan", // they follow jeffrey
    is_verified: !!node.is_verified,
    signals,
  });
}

// Art-world first, then by score — this is the Phase-2 work order.
candidates.sort(
  (a, b) =>
    Number(b.art_world) - Number(a.art_world) || b.score - a.score ||
    a.username.localeCompare(b.username),
);

const byType = {};
for (const c of candidates) byType[c.type] = (byType[c.type] || 0) + 1;

const out = {
  account,
  generated: new Date().toISOString().slice(0, 10),
  totals: {
    nodes: candidates.length,
    followers: followers.length,
    following: following.length,
    art_world: candidates.filter((c) => c.art_world).length,
    by_type: byType,
  },
  // candidates[] is pre-sorted as the enrichment work order.
  candidates,
};

const outPath = join(socialDir, "candidates.json");
writeFileSync(outPath, JSON.stringify(out, null, 2));
console.error(
  `classified ${candidates.length} nodes — ${out.totals.art_world} art-world ` +
    `(${Object.entries(byType)
      .sort((a, b) => b[1] - a[1])
      .map(([k, v]) => `${k}:${v}`)
      .join(" ")})\n→ ${outPath}`,
);
