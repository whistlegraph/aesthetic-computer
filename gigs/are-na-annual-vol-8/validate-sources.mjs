#!/usr/bin/env node
// Validate the blocks in "the-score-that-teaches-itself":
//  - flag every Wikipedia link (we want primary sources)
//  - for each block, search are.na for existing uses of the same URL
//    and of the topic name — so we can see what other people
//    are connecting to for the same idea (usually the real source)
//
// Usage:
//   ARENA_TOKEN=... node validate-sources.mjs          # all blocks
//   ARENA_TOKEN=... node validate-sources.mjs --wiki   # wiki only
//   ARENA_TOKEN=... node validate-sources.mjs --json > report.json
//
// ARENA_TOKEN is optional but greatly raises rate limits.

const SLUG = "the-score-that-teaches-itself";
const TOKEN = process.env.ARENA_TOKEN;
const API = "https://api.are.na/v2";

const args = new Set(process.argv.slice(2));
const WIKI_ONLY = args.has("--wiki");
const JSON_OUT = args.has("--json");

const headers = TOKEN
  ? { Authorization: `Bearer ${TOKEN}` }
  : {};

async function j(url) {
  const r = await fetch(url, { headers });
  if (!r.ok) throw new Error(`${r.status} ${r.statusText} — ${url}`);
  return r.json();
}

function isWiki(url) {
  return url && /wikipedia\.org/i.test(url);
}

function hostOf(url) {
  try { return new URL(url).hostname.replace(/^www\./, ""); } catch { return ""; }
}

// Pull a short topic phrase out of a title like "Treatise (Cardew) - Wikipedia"
function topicOf(title) {
  if (!title) return "";
  return title
    .replace(/\s*[-–—|]\s*Wikipedia.*$/i, "")
    .replace(/^\s+|\s+$/g, "");
}

// Search are.na for any blocks whose source matches the URL (or title)
// Returns an array of { url, host, count, sample: [{channelSlug, user}] }
async function searchSimilar(block) {
  const out = { exact: [], nearby: [] };
  const url = block.source?.url;
  const topic = topicOf(block.title) || block.generated_title || "";
  // 1) exact URL — search /search/blocks for the URL itself
  if (url) {
    try {
      const q = encodeURIComponent(url);
      const r = await j(`${API}/search/blocks?q=${q}&per=10`);
      for (const b of r.blocks || []) {
        if (b.source?.url === url) {
          out.exact.push({
            id: b.id,
            title: b.title || b.generated_title,
            url: b.source.url,
            connections: b.connections_count || 0,
          });
        }
      }
    } catch (e) { out.err = e.message; }
  }
  // 2) topic phrase — find the most-connected non-wiki blocks for the same idea
  if (topic) {
    try {
      const q = encodeURIComponent(topic);
      const r = await j(`${API}/search/blocks?q=${q}&per=20`);
      const seen = new Set();
      for (const b of r.blocks || []) {
        const bu = b.source?.url;
        if (!bu) continue;
        if (seen.has(bu)) continue;
        seen.add(bu);
        if (bu === url) continue;
        if (isWiki(bu)) continue;
        out.nearby.push({
          url: bu,
          host: hostOf(bu),
          title: b.title || b.generated_title,
          connections: b.connections_count || 0,
        });
      }
      out.nearby.sort((a, b) => b.connections - a.connections);
      out.nearby = out.nearby.slice(0, 5);
    } catch (e) { out.err = (out.err || "") + "; " + e.message; }
  }
  return out;
}

function fmtReport(rows) {
  const lines = [];
  for (const row of rows) {
    const flag = row.isWiki ? "🟡 WIKI" : "⚪ primary";
    const url = row.url || "(text)";
    const host = hostOf(url) || "—";
    lines.push(`\n#${row.position}  ${flag}  [${host}]`);
    lines.push(`  title   : ${row.title || row.content?.slice(0, 80) || "(empty)"}`);
    lines.push(`  url     : ${url}`);
    lines.push(`  desc    : ${(row.description || "").slice(0, 100)}`);
    if (row.sim) {
      if (row.sim.exact?.length) {
        lines.push(`  ↳ already on are.na: ${row.sim.exact.length} exact block(s), top connections: ${row.sim.exact.slice(0, 3).map(e => e.connections).join("/")}`);
      } else if (url) {
        lines.push(`  ↳ not on are.na (exact URL) — fresh`);
      }
      if (row.sim.nearby?.length) {
        lines.push(`  ↳ alternate sources people connect for this topic:`);
        for (const n of row.sim.nearby) {
          lines.push(`      • [${n.host}] ${n.connections}× conn — ${n.url}`);
        }
      }
    }
  }
  return lines.join("\n");
}

async function main() {
  console.error(`fetching /channels/${SLUG}/contents …`);
  const ch = await j(`${API}/channels/${SLUG}/contents?per=100&direction=desc`);
  const blocks = ch.contents || [];
  console.error(`got ${blocks.length} blocks${TOKEN ? "" : " (unauthed, rate limits apply)"}\n`);

  const rows = [];
  for (const b of blocks) {
    const url = b.source?.url;
    const row = {
      position: b.position,
      class: b.class,
      title: b.title,
      content: b.content,
      description: b.description,
      url,
      isWiki: isWiki(url),
    };
    if (WIKI_ONLY && !row.isWiki) { rows.push(row); continue; }
    if (b.class === "Link") {
      try {
        row.sim = await searchSimilar(b);
        process.stderr.write(".");
      } catch (e) {
        row.simErr = e.message;
        process.stderr.write("x");
      }
      await new Promise(r => setTimeout(r, 400)); // gentle rate limit
    }
    rows.push(row);
  }
  console.error("\n");

  if (JSON_OUT) {
    console.log(JSON.stringify({ channel: SLUG, at: new Date().toISOString(), rows }, null, 2));
  } else {
    console.log(fmtReport(rows));
    const wikiCount = rows.filter(r => r.isWiki).length;
    console.log(`\n\nsummary: ${rows.length} blocks · ${wikiCount} wikipedia link(s) flagged for swap`);
  }
}

main().catch(e => { console.error("error:", e.message); process.exit(1); });
