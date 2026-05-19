#!/usr/bin/env node
// Offline (zero-network) graph assembler + data-image renderer.
//
// Joins the three social-graph artifacts into one typed graph and draws it as
// a contemporary-art-world map: galleries / museums / institutions / art-media
// / curators foregrounded and sized by reach, the people who run them
// clustered around their institution by parsed bio affiliations, and personal
// fans demoted to a faint background ring (present, never the subject).
//
// Usage:
//   node bin/ig-social-edges.mjs whistlegraph
//
// Reads:  portraits/jeffrey/social/<acct>/{candidates.json,profiles.jsonl,
//                                          profile.json}
// Writes: portraits/jeffrey/social/<acct>/graph.json   (typed nodes + edges)
//         portraits/jeffrey/social/<acct>/graph.svg    (the data image)

import { readFileSync, writeFileSync, existsSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");

const account = process.argv[2];
if (!account) {
  console.error("usage: ig-social-edges.mjs <account>");
  process.exit(64);
}
const dir = join(REPO_ROOT, "portraits/jeffrey/social", account);

// Instagram's own category strings → our art-world taxonomy. The IG category
// is authoritative when present; otherwise we fall back to the name heuristic.
const IG_CAT = {
  "art gallery": "gallery",
  "art museum": "museum",
  museum: "museum",
  "performance & event venue": "institution",
  "arts & entertainment": "art_media",
  "art": "artist",
  artist: "artist",
  "musician/band": "artist",
  "media/news company": "art_media",
  "magazine": "art_media",
  "publisher": "art_media",
  "non-profit organization": "institution",
  "education": "institution",
  "community organization": "institution",
};

const TYPE_COLOR = {
  gallery: "#e8482c", // vermilion
  museum: "#1f6feb", // blue
  institution: "#7b3ff2", // violet
  art_media: "#0fa36b", // green
  curator: "#f2a900", // amber (people who run things)
  artist: "#d6336c", // magenta
  other: "#8a8f98", // grey (fans / unknown)
};
const ART_TYPES = new Set([
  "gallery",
  "museum",
  "institution",
  "art_media",
  "curator",
]);

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
const readJson = (p, d) =>
  existsSync(p) ? JSON.parse(readFileSync(p, "utf8")) : d;

const candDoc = readJson(join(dir, "candidates.json"), null);
if (!candDoc) {
  console.error(
    `no candidates.json — run ig-artworld-classify.mjs ${account} first.`,
  );
  process.exit(1);
}
const acctProfile = readJson(join(dir, "profile.json"), { username: account });
const profiles = readJsonl(join(dir, "profiles.jsonl")).filter((r) => !r.error);
const profByUser = new Map(profiles.map((p) => [p.username, p]));
// Wikidata corroboration (stage 5) — additive: promotes/typing nodes IG's
// category field left blank, never demotes an already-classified art node.
const webByUser = new Map(
  readJsonl(join(dir, "web.jsonl"))
    .filter((r) => !r.error && r.web_artworld)
    .map((r) => [r.username, r]),
);

// Map a Wikidata verdict to our taxonomy when IG/heuristic couldn't.
function webType(w) {
  const blob = [
    ...(w.instance_of || []),
    ...(w.occupations || []),
    w.description || "",
  ]
    .join(" · ")
    .toLowerCase();
  if (/art museum|museum/.test(blob)) return "museum";
  if (/art gallery|commercial art gallery/.test(blob)) return "gallery";
  if (/magazine|publish|book fair|art fair/.test(blob)) return "art_media";
  if (/curator|gallerist|art dealer|art histor|director/.test(blob))
    return "curator";
  if (/foundation|kunsthalle|kunstverein|nonprofit|institut|bienn/.test(blob))
    return "institution";
  if (/artist|painter|sculptor|photograph|art\b/.test(blob)) return "artist";
  return "artist"; // web-confirmed art-world but unclear bucket
}

// ── Resolve every candidate into a typed node ──────────────────────────────
function resolveType(c, pr, w) {
  if (pr && pr.ig_category) {
    const t = IG_CAT[pr.ig_category.trim().toLowerCase()];
    if (t) return t;
  }
  // person with art-world affiliations in bio → curator/role person
  if (pr && (pr.affiliations || []).some((a) => a.role)) return "curator";
  if (c.type === "person_role") return "curator";
  if (c.type === "other" && w) return webType(w); // Wikidata fills the gap
  return c.type; // heuristic fallback (gallery/museum/.../artist/other)
}

const nodes = [];
const edges = [];
const center = acctProfile.username || account;

for (const c of candDoc.candidates) {
  const pr = profByUser.get(c.username);
  const w = webByUser.get(c.username);
  const type = resolveType(c, pr, w);
  const enriched = !!pr;
  const reach = pr ? pr.followers || 0 : null;
  const artWorld = ART_TYPES.has(type) || c.art_world || !!w;
  nodes.push({
    id: c.username,
    label: pr?.full_name || c.full_name || c.username,
    type,
    relation: c.relation, // mutual | idol | fan
    art_world: artWorld,
    enriched,
    reach,
    ig_category: pr?.ig_category || null,
    external_url: pr?.external_url || null,
    wikidata: w?.wikidata || null,
    web_description: w?.description || null,
    confirmed_by: [
      c.art_world && "heuristic",
      pr?.ig_category && IG_CAT[pr.ig_category.trim().toLowerCase()] &&
        "ig_category",
      w && "wikidata",
    ].filter(Boolean),
  });

  // follow edge(s) relative to the account
  if (c.relation === "mutual" || c.relation === "idol")
    edges.push({ source: center, target: c.username, kind: "follows" });
  if (c.relation === "mutual" || c.relation === "fan")
    edges.push({ source: c.username, target: center, kind: "follows" });

  // affiliation edges: this person → the institution(s) they run/work at.
  // Two sources, both kept in graph.json: IG-bio @mentions (target is an IG
  // handle, may resolve to another node) and Wikidata employer/member-of
  // (target is an org label — authoritative "who works where").
  for (const a of pr?.affiliations || []) {
    edges.push({
      source: c.username,
      target: a.handle,
      kind: "affiliation",
      via: "ig_bio",
      role: a.role || null,
    });
  }
  for (const a of w?.affiliations || []) {
    edges.push({
      source: c.username,
      target: a.org,
      kind: "affiliation",
      via: "wikidata",
      qid: a.qid || null,
    });
  }
}

// ── graph.json ─────────────────────────────────────────────────────────────
const byType = {};
for (const n of nodes) byType[n.type] = (byType[n.type] || 0) + 1;
const graph = {
  account: center,
  generated: new Date().toISOString().slice(0, 10),
  totals: {
    nodes: nodes.length,
    art_world: nodes.filter((n) => n.art_world).length,
    enriched: nodes.filter((n) => n.enriched).length,
    affiliations: edges.filter((e) => e.kind === "affiliation").length,
    by_type: byType,
  },
  center: {
    username: center,
    followers: acctProfile.followers ?? null,
    followees: acctProfile.followees ?? null,
  },
  nodes,
  edges,
};
writeFileSync(join(dir, "graph.json"), JSON.stringify(graph, null, 2));

// ── graph.svg — the data image ─────────────────────────────────────────────
const W = 1600;
const H = 1600;
const cx = W / 2;
const cy = H / 2;
const esc = (s) =>
  String(s).replace(
    /[&<>"]/g,
    (ch) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[ch],
  );

// Art-world nodes: grouped into angular sectors by type, placed on a ring
// whose radius is tiered (museums/institutions inner → galleries → media →
// curators), sized by sqrt(reach). Sorted by reach so big institutions read
// first inside each sector.
const SECTORS = ["museum", "institution", "gallery", "art_media", "curator"];
const RING = {
  museum: 230,
  institution: 300,
  gallery: 400,
  art_media: 500,
  curator: 590,
};
const sectorSpan = (2 * Math.PI) / SECTORS.length;

const art = nodes
  .filter((n) => n.art_world && SECTORS.includes(n.type))
  .sort((a, b) => (b.reach || 0) - (a.reach || 0));
const bySector = Object.fromEntries(SECTORS.map((s) => [s, []]));
for (const n of art) bySector[n.type].push(n);

const pos = new Map();
const placed = [];
SECTORS.forEach((sec, si) => {
  const list = bySector[sec];
  const a0 = si * sectorSpan - Math.PI / 2 + 0.12;
  const a1 = (si + 1) * sectorSpan - Math.PI / 2 - 0.12;
  list.forEach((n, idx) => {
    const t = list.length === 1 ? 0.5 : idx / (list.length - 1);
    const ang = a0 + t * (a1 - a0);
    // gentle radial jitter so equal-type rings don't perfectly overlap labels
    const r = RING[sec] + ((idx % 3) - 1) * 26;
    const x = cx + r * Math.cos(ang);
    const y = cy + r * Math.sin(ang);
    pos.set(n.id, { x, y });
    const rad = 5 + Math.sqrt(Math.max(n.reach || 0, 0)) / 14;
    placed.push({ n, x, y, rad: Math.min(rad, 46), ang });
  });
});

// Fans / non-art nodes: faint background ring, capped for SVG sanity.
const fans = nodes.filter((n) => !pos.has(n.id));
const FAN_CAP = 700;
const fanShown = fans.slice(0, FAN_CAP);
const fanSvg = fanShown
  .map((_, i) => {
    const ang = (i / fanShown.length) * 2 * Math.PI;
    const r = 700 + ((i * 53) % 90);
    return `<circle cx="${(cx + r * Math.cos(ang)).toFixed(1)}" cy="${(
      cy +
      r * Math.sin(ang)
    ).toFixed(1)}" r="2.1" fill="#8a8f98" opacity="0.16"/>`;
  })
  .join("");

// Affiliation edges (person → institution) drawn only when both ends are
// placed art-world nodes — these are the "who runs what" threads.
const affSvg = edges
  .filter((e) => e.kind === "affiliation" && pos.has(e.source) && pos.has(e.target))
  .map((e) => {
    const a = pos.get(e.source);
    const b = pos.get(e.target);
    return `<line x1="${a.x.toFixed(1)}" y1="${a.y.toFixed(1)}" x2="${b.x.toFixed(
      1,
    )}" y2="${b.y.toFixed(1)}" stroke="#f2a900" stroke-width="1.1" opacity="0.5"/>`;
  })
  .join("");

// Follow spokes from the center hub to art-world nodes (fan spokes omitted —
// they'd be a hairball; fans stay as the faint outer dots).
const spokeSvg = placed
  .map(
    ({ x, y, n }) =>
      `<line x1="${cx}" y1="${cy}" x2="${x.toFixed(1)}" y2="${y.toFixed(
        1,
      )}" stroke="${
        n.relation === "mutual" ? "#cdd3da" : "#e6e9ee"
      }" stroke-width="${n.relation === "mutual" ? 1.3 : 0.7}" opacity="${
        n.relation === "mutual" ? 0.55 : 0.3
      }"/>`,
  )
  .join("");

const nodeSvg = placed
  .map(({ n, x, y, rad }) => {
    const col = TYPE_COLOR[n.type] || TYPE_COLOR.other;
    const stroke = n.enriched ? "#11151a" : "#11151a55";
    const showLabel = rad >= 9 || n.relation === "mutual";
    const label = showLabel
      ? `<text x="${(x + rad + 4).toFixed(1)}" y="${(y + 4).toFixed(
          1,
        )}" font-family="Helvetica,Arial,sans-serif" font-size="${
          rad >= 18 ? 15 : 12
        }" fill="#11151a">${esc(
          (n.label || n.id).slice(0, 28),
        )}</text>`
      : "";
    return `<g><circle cx="${x.toFixed(1)}" cy="${y.toFixed(1)}" r="${rad.toFixed(
      1,
    )}" fill="${col}" fill-opacity="${
      n.enriched ? 0.92 : 0.55
    }" stroke="${stroke}" stroke-width="1.2"/>${label}</g>`;
  })
  .join("");

const legend = SECTORS.concat(["artist", "other"])
  .map((t, i) => {
    const ly = 70 + i * 30;
    const lab =
      { other: "fans / unknown", art_media: "art media · fairs", curator: "people who run them" }[
        t
      ] || t;
    return `<circle cx="60" cy="${ly}" r="9" fill="${
      TYPE_COLOR[t] || TYPE_COLOR.other
    }"/><text x="80" y="${
      ly + 5
    }" font-family="Helvetica,Arial,sans-serif" font-size="18" fill="#11151a">${esc(
      lab,
    )} (${byType[t] || 0})</text>`;
  })
  .join("");

const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">
<rect width="${W}" height="${H}" fill="#fbfbf9"/>
<text x="${cx}" y="44" text-anchor="middle" font-family="Helvetica,Arial,sans-serif" font-size="30" font-weight="700" fill="#11151a">@${esc(
  center,
)} · contemporary art world map</text>
<text x="${cx}" y="${
  H - 30
}" text-anchor="middle" font-family="Helvetica,Arial,sans-serif" font-size="15" fill="#6b7280">${
  graph.totals.art_world
} art-world nodes · ${graph.totals.enriched} enriched · ${
  graph.totals.affiliations
} affiliation links · ${fans.length} fans (faint) · ${graph.generated}</text>
${fanSvg}
${spokeSvg}
${affSvg}
${nodeSvg}
<circle cx="${cx}" cy="${cy}" r="22" fill="#11151a"/>
<text x="${cx}" y="${
  cy + 5
}" text-anchor="middle" font-family="Helvetica,Arial,sans-serif" font-size="14" font-weight="700" fill="#fff">${esc(
  center.slice(0, 5),
)}</text>
${legend}
</svg>`;
writeFileSync(join(dir, "graph.svg"), svg);

console.error(
  `graph: ${nodes.length} nodes (${graph.totals.art_world} art-world, ` +
    `${graph.totals.enriched} enriched), ${graph.totals.affiliations} ` +
    `affiliation links\n→ ${join(dir, "graph.json")}\n→ ${join(
      dir,
      "graph.svg",
    )}`,
);
