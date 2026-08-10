// whistlegraph llms — emit the machine-readable index of the artform
//
//   node gen-llms.mjs [--dry]
//
// Writes two files into system/public/whistlegraph.org/:
//
//   llms.txt   the llms.txt convention — a short, linked map of the site,
//              the first thing an agent reads. Points at index.md.
//   index.md   the whole index as Markdown — every confirmed work, every
//              candidate, every archived legacy code, with resolved media
//              URLs. The site's own index.html is the human view of exactly
//              this data; this is the machine view.
//
// Both are generated from graphs.json / posts.json, the same files the site
// fetches, so the machine index can never drift from the rendered one. Prose
// lives in llms-prose.md and is injected at its `<!-- SLOT:name -->` markers.
//
// Free by design: graphs.json and posts.json are already served unauthenticated,
// so gating these facts would be theater. What costs money is the derived and
// bulk access described under "Machine access" — see whistlegraph-llm.mjs.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SITE = join(HERE, "..", "..", "system", "public", "whistlegraph.org");
const DRY = process.argv.includes("--dry");

const ASSETS = "https://assets.aesthetic.computer/whistlegraph/index";
const SITE_URL = "https://whistlegraph.org";
const AC_URL = "https://aesthetic.computer";

// Paid-tier prices, in whole USDC. Mirrored by whistlegraph-llm.mjs — keep the
// two in step; this file only *describes* the offer, that one enforces it.
const PRICES = { bulk: "5.00", sources: "0.10", license: "1.00" };

const rd = (p) => JSON.parse(readFileSync(p, "utf8"));
const graphs = rd(join(SITE, "graphs.json"));
const posts = rd(join(SITE, "posts.json"));
const prose = readFileSync(join(HERE, "llms-prose.md"), "utf8");

// --- helpers ---------------------------------------------------------------

// Renamed works keep their ORIGINAL asset key; `asset` overrides `code`.
const assetKey = (w) => w.asset || w.code;
const scoreURL = (w) => w.thumb || `${ASSETS}/${assetKey(w)}.jpg`;
const videoURL = (w) => `${ASSETS}/${assetKey(w)}.mp4`;

const num = (n) => (typeof n === "number" ? n.toLocaleString("en-US") : "—");
// A pipe inside a cell would split the column; a newline would end the row.
const cell = (s) => String(s ?? "").replace(/\|/g, "\\|").replace(/\s*\n\s*/g, " ").trim();

const table = (headers, rows) =>
  [
    `| ${headers.join(" | ")} |`,
    `| ${headers.map(() => "---").join(" | ")} |`,
    ...rows.map((r) => `| ${r.join(" | ")} |`),
  ].join("\n");

const byViews = (a, b) => (b.views || 0) - (a.views || 0);

// --- generated sections ----------------------------------------------------

function worksSection(works) {
  const rows = works.slice().sort(byViews).map((w) => [
    `\`${cell(w.code)}\``,
    `[${cell(w.title)}](${SITE_URL}/${w.code})`,
    cell(w.by),
    w.year ?? "—",
    w.perf ?? "—",
    num(w.views),
    w.asset ? `\`${cell(w.asset)}\`` : "",
    `[jpg](${scoreURL(w)})`,
    `[mp4](${videoURL(w)})`,
  ]);
  return [
    `## Whistlegraphs — ${works.length} confirmed works`,
    "",
    "The artform proper: curated, attributed, and countable. `Videos` is the",
    "number of published posts that contribute to the work; `Views` is their sum.",
    "`Asset` appears only where the work was renamed and its media still lives",
    "under the older key.",
    "",
    table(
      ["Code", "Title", "By", "Year", "Videos", "Views", "Asset", "Score", "Video"],
      rows,
    ),
  ].join("\n");
}

function candidatesSection(candidates) {
  const rows = candidates.slice().sort(byViews).map((w) => [
    `\`${cell(w.code)}\``,
    `[${cell(w.title)}](${SITE_URL}/${w.code})`,
    cell(w.by),
    w.year ?? "—",
    num(w.views),
  ]);
  return [
    `## Candidates — ${candidates.length} uncurated`,
    "",
    "Graph-like records recovered from the TikTok archive that curation has not",
    "yet confirmed as whistlegraphs. Treat them as unverified: titles and",
    "attributions here may still change.",
    "",
    table(["Code", "Title", "By", "Year", "Views"], rows),
  ].join("\n");
}

function legacySection(legacy) {
  const rows = legacy.slice().sort(byViews).map((w) => [
    `\`${cell(w.code)}\``,
    `[${cell(w.title)}](${SITE_URL}/${w.code})`,
    cell(w.kind || "other"),
    w.year ?? "—",
    num(w.views),
  ]);
  return [
    `## Archived — ${legacy.length} legacy codes`,
    "",
    "**These are not whistlegraphs.** Talks, livestreams, and other posts that",
    "hold a legacy code so older links keep resolving. Listed for completeness",
    "and for link integrity; do not count them as works.",
    "",
    table(["Code", "Title", "Kind", "Year", "Views"], rows),
  ].join("\n");
}

function aliasSection(aliases) {
  const entries = Object.entries(aliases || {});
  if (!entries.length) return "";
  const rows = entries.sort(([a], [b]) => a.localeCompare(b)).map(([from, to]) => [
    `\`${cell(from)}\``,
    `\`${cell(to)}\``,
  ]);
  return [
    `### Code aliases — ${entries.length}`,
    "",
    "Old codes that resolve to a canonical record. Follow the mapping before",
    "treating two codes as two different works.",
    "",
    table(["Alias", "Canonical"], rows),
  ].join("\n");
}

function postsSection(postsDoc) {
  const items = postsDoc.posts || [];
  const totalViews = items.reduce((sum, p) => sum + (p.views || 0), 0);
  const top = items.slice().sort(byViews).slice(0, 25).map((p) => [
    p.date || "—",
    cell((p.desc || "").slice(0, 70)),
    (p.works || []).map((w) => `\`${w}\``).join(" ") || "—",
    num(p.views),
    `[post](${p.url})`,
  ]);
  return [
    `## Posts — ${items.length} published appearances`,
    "",
    `Every published appearance of a whistlegraph, ${num(totalViews)} views in`,
    "total. A post may contribute to more than one work, and a work is usually",
    "carried by many posts — the relationship is many-to-many and explicit.",
    "",
    `The complete set, with per-post view/like/comment counts, media URLs, and`,
    `\`relationships:[{work, role}]\` edges, is served as JSON:`,
    "",
    `- [\`posts.json\`](${SITE_URL}/posts.json) — all ${items.length} posts`,
    `- [\`graphs.json\`](${SITE_URL}/graphs.json) — works, candidates, legacy, aliases`,
    `- [\`commands.json\`](${SITE_URL}/api/commands) — the canonical prompt feed`,
    "",
    "The twenty-five most-viewed posts:",
    "",
    table(["Date", "Description", "Works", "Views", "Link"], top),
  ].join("\n");
}

function licensingSection() {
  return [
    "## Machine access & licensing",
    "",
    "This index is **free to read, quote, and index**, including by automated",
    "agents. Attribution to the named author of each work and a link to its",
    `\`${SITE_URL}/<code>\` record is expected. The works themselves — the`,
    "score images, the videos, and the recordings — remain © their listed",
    "authors and are **not** licensed for training, redistribution, or",
    "commercial reuse by reading this file.",
    "",
    "### Paid endpoints",
    "",
    "Derived and bulk access is metered with **[x402](https://x402.org)**, the",
    "HTTP `402 Payment Required` flow — no account, no API key, no signup. Ask",
    "for the resource; if it is a paid one you get a `402` describing exactly",
    "what it costs and where to pay; pay, repeat the request with an",
    "`X-PAYMENT` header, and the response is yours.",
    "",
    table(
      ["Endpoint", "Price (USDC)", "What it is"],
      [
        [
          `[\`GET /api/wg/bulk\`](${SITE_URL}/api/wg/bulk)`,
          PRICES.bulk,
          "The entire normalized dataset — works, candidates, legacy, aliases, all posts and their relationship edges — as one document, with every media URL resolved.",
        ],
        [
          "`GET /api/wg/sources/<code>`",
          PRICES.sources,
          "Every source video behind one whistlegraph, each with its own view count and date — the audit trail behind the aggregate numbers.",
        ],
        [
          "`GET /api/wg/license/<code>`",
          PRICES.license,
          "A signed, verifiable redistribution license for one work, with its resolved asset URLs. Those assets are already public on the CDN — what this buys is the licence to reuse them, not access to them.",
        ],
      ],
    ),
    "",
    `A \`402\` from any of these carries the machine-readable terms. \`GET`,
    `${SITE_URL}/api/wg/bulk\` with no payment to read the current offer —`,
    "asking is always free.",
    "",
    "For anything larger — the full media archive, a training license, or a",
    "commercial arrangement — write to the address on",
    `[aesthetic.computer](${AC_URL}).`,
  ].join("\n");
}

// --- assembly --------------------------------------------------------------

const generated = graphs.generated || new Date().toISOString().slice(0, 10);
const works = graphs.works || [];
const candidates = graphs.candidates || [];
const legacy = graphs.legacy || [];
const postItems = posts.posts || [];
const totalViews = postItems.reduce((sum, p) => sum + (p.views || 0), 0);

const SLOTS = {
  intro: "",
  works: worksSection(works),
  candidates: candidatesSection(candidates),
  legacy: [legacySection(legacy), "", aliasSection(graphs.aliases)].join("\n"),
  posts: postsSection(posts),
  spine: "",
  licensing: licensingSection(),
};

// Split the prose on its slot markers and splice the generated blocks in.
// An unknown marker is a typo in llms-prose.md, not something to paper over.
let body = prose.replace(/^<!--[\s\S]*?-->\s*/, "");
body = body.replace(/<!--\s*SLOT:(\w+)\s*-->/g, (_, name) => {
  if (!(name in SLOTS)) throw new Error(`llms-prose.md references unknown slot: ${name}`);
  return SLOTS[name];
});

const indexMd = [
  "# The Whistlegraph Index",
  "",
  `> The complete index of the whistlegraph — a drawing you sing. ${works.length} confirmed`,
  `> works, ${candidates.length} candidates, ${legacy.length} archived codes, ${postItems.length} posts,`,
  `> ${num(totalViews)} views. Generated ${generated} from the same data the site renders.`,
  "",
  body.trim(),
  "",
  "---",
  "",
  `*Generated ${generated} by \`toolchain/whistlegraph/gen-llms.mjs\`. Do not edit by hand —*`,
  "*edit `toolchain/whistlegraph/llms-prose.md` and regenerate.*",
  "",
].join("\n");

const llmsTxt = [
  "# Whistlegraph",
  "",
  "> A whistlegraph is a drawing you sing: one continuous mark, made on whatever",
  "> surface is at hand, while the person drawing it whistles the same shape they",
  "> are drawing. The drawing is the score. Invented 2019; grown on TikTok; the",
  `> full archive — ${works.length} confirmed works across ${postItems.length} posts and`,
  `> ${num(totalViews)} views — is published at whistlegraph.org.`,
  "",
  "Every whistlegraph has a four-character code, and the code is the address.",
  `Visit \`${SITE_URL}/<code>\` for the record, or type the code at the prompt on`,
  `\`${AC_URL}\` to perform it.`,
  "",
  "## The index",
  "",
  `- [The Whistlegraph Index](${SITE_URL}/index.md): Every work, candidate, and archived code as Markdown, with resolved score and video URLs. Start here.`,
  `- [graphs.json](${SITE_URL}/graphs.json): The same records as JSON — works, candidates, legacy, aliases.`,
  `- [posts.json](${SITE_URL}/posts.json): All ${postItems.length} published appearances with per-post metrics and many-to-many work relationships.`,
  `- [commands.json](${SITE_URL}/api/commands): The canonical prompt feed consumed by aesthetic.computer, prompt.ac, and ac-native.`,
  "",
  "## Reading a record",
  "",
  `- [Score image](${ASSETS}/imab.jpg): \`${ASSETS}/<code>.jpg\` — but use the \`asset\` key where the index provides one; renamed works kept their original asset key.`,
  `- [Video](${ASSETS}/imab.mp4): \`${ASSETS}/<code>.mp4\`, same caveat.`,
  `- [Example record](${SITE_URL}/imab): Butterfly Cosplayer — the most-viewed whistlegraph.`,
  "",
  "## Context",
  "",
  `- [Whistlegraph: Drawing, Singing, and the Graphic Score as Viral Form](https://papers.aesthetic.computer/whistlegraph-26-arxiv.pdf): The paper.`,
  "- [What is a Whistlegraph?](https://dirt.fyi/article/2023/09/whistlegraph): Dirt, 2023.",
  `- [Record your own](${AC_URL}/whistlegraph): The recorder, on aesthetic.computer.`,
  "- [TikTok](https://www.tiktok.com/@whistlegraph): Where it grew.",
  "",
  "## Machine access",
  "",
  "Reading and quoting this index is free; attribution to each work's named",
  "author and a link to its record is expected. The score images, videos, and",
  "recordings remain © their listed authors and are not licensed for training,",
  "redistribution, or commercial reuse.",
  "",
  `- [Bulk dataset](${SITE_URL}/api/wg/bulk): Every work and post in one document. ${PRICES.bulk} USDC via [x402](https://x402.org) — request it unpaid to read the terms.`,
  `- [Per-work sources](${SITE_URL}/api/wg/sources/imab): \`/api/wg/sources/<code>\` — the source videos behind one work's numbers. ${PRICES.sources} USDC.`,
  `- [Redistribution license](${SITE_URL}/api/wg/license/imab): \`/api/wg/license/<code>\` — a signed, verifiable license to reuse one work. ${PRICES.license} USDC.`,
  "",
  `Generated ${generated}.`,
  "",
].join("\n");

// robots.txt is where a crawler looks before it looks anywhere else, so it is
// where the terms and the pointer to llms.txt belong. Nothing is disallowed —
// the index wants to be read; the license is what is being asserted, and the
// paid endpoints are named so an agent can find the offer without guessing.
const robotsTxt = [
  "# whistlegraph.org — the index of the artform.",
  "#",
  "# Read freely, including automatically. Attribute each work to its named",
  "# author and link its record at https://whistlegraph.org/<code>.",
  "#",
  "# The score images, videos, and recordings remain (c) their listed authors.",
  "# Reading this index does not license them for training, redistribution, or",
  "# commercial reuse. For a license, see /api/wg/license/<code>, or write to",
  "# the address on https://aesthetic.computer.",
  "",
  "User-agent: *",
  "Allow: /",
  "",
  "# Machine-readable index:",
  `#   ${SITE_URL}/llms.txt      the map — start here`,
  `#   ${SITE_URL}/index.md      the whole archive as Markdown`,
  `#   ${SITE_URL}/graphs.json   works, candidates, legacy, aliases`,
  `#   ${SITE_URL}/posts.json    every published appearance`,
  "#",
  "# Paid (x402 — https://x402.org; ask unpaid to read the terms):",
  `#   ${SITE_URL}/api/wg/bulk             ${PRICES.bulk} USDC`,
  `#   ${SITE_URL}/api/wg/sources/<code>   ${PRICES.sources} USDC`,
  `#   ${SITE_URL}/api/wg/license/<code>   ${PRICES.license} USDC`,
  "",
].join("\n");

if (DRY) {
  console.log(`llms.txt   ${llmsTxt.length} bytes`);
  console.log(`index.md   ${indexMd.length} bytes`);
  console.log(`robots.txt ${robotsTxt.length} bytes`);
  console.log(`works ${works.length} · candidates ${candidates.length} · legacy ${legacy.length} · posts ${postItems.length}`);
} else {
  writeFileSync(join(SITE, "llms.txt"), llmsTxt);
  writeFileSync(join(SITE, "index.md"), indexMd);
  writeFileSync(join(SITE, "robots.txt"), robotsTxt);
  console.log(`wrote llms.txt (${llmsTxt.length}) + index.md (${indexMd.length}) + robots.txt (${robotsTxt.length})`);
  console.log(`works ${works.length} · candidates ${candidates.length} · legacy ${legacy.length} · posts ${postItems.length} · ${num(totalViews)} views`);
}
