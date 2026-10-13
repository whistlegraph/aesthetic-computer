#!/usr/bin/env node
// choir curator, 26.10.12
// Once a day on jasellite: find fresh art news and open calls with Exa, let
// Claude Sonnet 5 pick a small handful for Fía's choir, and hand them to
// /api/choir?curate=1 (which skips repeats and retires closed calls).
//
//   EXA_API_KEY            Exa search key
//   CHOIR_EDITOR_SECRET    the curate door's shared secret (vault lith/.env)
//   CLAUDE_CODE_OAUTH_TOKEN headless `claude -p` auth (~/.config/ac/claude.env)
//   CHOIR_API              default https://aesthetic.computer/api/choir
//   CHOIR_MODEL            default claude-sonnet-5
//   DRY_RUN=1              print the picks, post nothing
//
// Rules from Fía (10-12): no Artforum links; e-flux welcome; link the real page
// and credit where we found it ("via"); keep it a curated handful, not a mirror.

import { spawn } from "node:child_process";

const API = process.env.CHOIR_API || "https://aesthetic.computer/api/choir";
const MODEL = process.env.CHOIR_MODEL || "claude-sonnet-5";
const DRY = process.env.DRY_RUN === "1";
const BLOCK = /artforum\.com/i;
const today = new Date().toISOString().slice(0, 10);
const log = (...a) => console.log(new Date().toISOString().slice(11, 19), ...a);

const NEWS_SOURCES = ["e-flux.com", "hyperallergic.com", "artnews.com", "news.artnet.com", "theartnewspaper.com", "frieze.com",
  "latimes.com", "nytimes.com", "bombmagazine.org", "contemporaryartdaily.com", "culturedmag.com", "kcet.org", "lataco.com", "artillerymag.com"];
const NEWS_QUERIES = [
  "art news today: museum appointments, exhibitions opening, artist news",
  "Los Angeles art scene news this week: gallery openings, museum news",
  "women artists news exhibition this week",
  "e-flux announcements this week",
];
const OPP_QUERIES = [
  "grants and fellowships for Los Angeles visual artists with upcoming deadlines",
  "open calls for women and non-binary artists with upcoming deadlines",
  "residencies for installation, site-specific and place-based artists, applications open",
  "grants and residencies for artist books, artists' publications, and art writing",
  "performance art and interdisciplinary residencies accepting applications",
  "convocatoria abierta artistas latinoamericanos y mexicanos 2027",
  "convocatoria residencia artística España artistas visuales",
  "opportunities listings Hyperallergic BOMB NYFA Res Artis this month",
];

// Fía (bug 6f2e3b34f7ec): "Can we add openings in LA? … Look at See Saw, Culture LA."
const OPENING_QUERIES = [
  "Los Angeles gallery openings and opening receptions this week",
  "See Saw gallery guide Los Angeles openings this weekend",
  "Culture LA art openings this week",
  "new exhibitions opening in Los Angeles galleries this month",
];
const OPENING_SCHEMA = { type: "object", required: ["isOpening", "show", "venue", "openingDate"], properties: {
  isOpening: { type: "boolean", description: "true only for a specific exhibition opening or reception at a named venue, in or near Los Angeles" },
  show: { type: "string", description: "exhibition title" }, artists: { type: "string" }, venue: { type: "string" },
  neighborhood: { type: "string" }, openingDate: { type: "string", description: "opening reception date YYYY-MM-DD" },
  openingTime: { type: "string" }, closingDate: { type: "string", description: "last day YYYY-MM-DD or empty" },
  officialUrl: { type: "string", description: "the venue's own page for this show if this page is a listing, else empty" },
} };

const OPP_SCHEMA = { type: "object", required: ["isOpportunity", "name", "deadline", "eligibility", "offers", "location"], properties: {
  isOpportunity: { type: "boolean", description: "true only for an open call, grant, residency, fellowship, prize or commission artists can apply to" },
  name: { type: "string" }, organizer: { type: "string" },
  deadline: { type: "string", description: "YYYY-MM-DD, or empty if rolling/unknown" },
  eligibility: { type: "string" }, offers: { type: "string" }, location: { type: "string" },
  officialUrl: { type: "string", description: "the organizer's own page for this call if this page is a listing that links to it, else empty" },
} };
const NEWS_SCHEMA = { type: "object", required: ["isNews", "headline", "summary"], properties: {
  isNews: { type: "boolean", description: "true for a dated news story, review or announcement about art, artists or institutions" },
  headline: { type: "string" }, summary: { type: "string", description: "one or two plain factual sentences" },
  location: { type: "string" },
} };

// Exa allows 10 requests a second; deep searches run three at a time.
async function inBatches(list, fn, size = 3) {
  const out = [];
  for (let i = 0; i < list.length; i += size) {
    out.push(...(await Promise.all(list.slice(i, i + size).map(fn))));
    await new Promise((r) => setTimeout(r, 1200));
  }
  return out;
}

// Listing sites and aggregators: fine as a "via", never as the link itself.
const LISTINGS = /(^|\.)(starbridge\.ai|bidscopeai\.com|artdeadline\.com|artjobs\.com|xhbt\.org|callfor\.org|globalsouthopportunities\.com|contest360\.in|opencalls\.monographica\.com|artconnect\.com|transartists\.org|resartis\.org|nyfa\.org|bombmagazine\.org|hyperallergic\.com\/[^/]*opportunit)/i;

// An extracted "official" link is only trusted if it actually loads.
async function loads(url) {
  if (!/^https:\/\//.test(url || "")) return false;
  try {
    const r = await fetch(url, { method: "GET", redirect: "follow", signal: AbortSignal.timeout(12000), headers: { "user-agent": "Mozilla/5.0 choir-curator" } });
    return r.ok;
  } catch { return false; }
}

async function exa(query, { schema, domains, sinceDays }) {
  const r = await fetch("https://api.exa.ai/search", {
    method: "POST", headers: { "x-api-key": process.env.EXA_API_KEY, "content-type": "application/json" },
    body: JSON.stringify({
      query, type: "deep", numResults: 10, excludeDomains: ["artforum.com"],
      ...(domains ? { includeDomains: domains } : {}),
      startPublishedDate: new Date(Date.now() - sinceDays * 86400e3).toISOString(),
      contents: { summary: { query: "Extract the details.", schema } },
    }),
  });
  const d = await r.json();
  if (!r.ok) { log("exa", r.status, query.slice(0, 40), JSON.stringify(d).slice(0, 160)); return []; }
  return (d.results || []).map((x) => { let s = {}; try { s = JSON.parse(x.summary); } catch {} return { url: x.url, title: x.title, image: x.image || null, published: x.publishedDate || null, ...s }; });
}

function claude(prompt) {
  return new Promise((resolve, reject) => {
    const p = spawn("claude", ["-p", "--model", MODEL, "--output-format", "json"], { stdio: ["pipe", "pipe", "pipe"] });
    let out = "", err = "";
    p.stdout.on("data", (b) => (out += b)); p.stderr.on("data", (b) => (err += b));
    p.on("close", (code) => {
      let text = out;
      try { const j = JSON.parse(out); if (j.is_error) return reject(Error("claude: " + (j.result || "error"))); text = j.result ?? out; } catch {}
      if (code !== 0) return reject(Error(`claude exited ${code}: ${(err || out).slice(0, 300)}`));
      resolve(String(text));
    });
    p.stdin.end(prompt);
  });
}

const host = (u) => { try { return new URL(u).hostname.replace(/^www\./, ""); } catch { return ""; } };

async function main() {
  for (const k of ["EXA_API_KEY", ...(DRY ? [] : ["CHOIR_EDITOR_SECRET"])]) if (!process.env[k]) throw Error(`missing ${k}`);
  const current = (await (await fetch(`${API}?news=1&limit=100`)).json()).posts || [];
  const existing = new Set(current.flatMap((p) => p.links || []));
  const already = current.map((p) => (p.preview?.title || p.text.split("\n")[0]).slice(0, 90));
  log("discover has", existing.size, "links");

  const news = (await inBatches(NEWS_QUERIES, (q) => exa(q, { schema: NEWS_SCHEMA, domains: NEWS_SOURCES, sinceDays: 3 }))).flat()
    .filter((x) => x.isNews && !BLOCK.test(x.url) && !existing.has(x.url));
  const opps = (await inBatches(OPP_QUERIES, (q) => exa(q, { schema: OPP_SCHEMA, sinceDays: 60 }))).flat()
    .filter((x) => x.isOpportunity && !BLOCK.test(x.url) && !existing.has(x.url) && !existing.has(x.officialUrl) && (!x.deadline || x.deadline >= today));
  const openings = (await inBatches(OPENING_QUERIES, (q) => exa(q, { schema: OPENING_SCHEMA, sinceDays: 21 }))).flat()
    .filter((x) => x.isOpening && !BLOCK.test(x.url) && !existing.has(x.url) && !existing.has(x.officialUrl) &&
      ((x.closingDate || x.openingDate || "") >= today));
  const uniq = (list) => [...new Map(list.map((x) => [x.url, x])).values()];
  const pool = [...uniq(news).map((x, i) => ({ id: `n${i}`, kind: "news", ...x })), ...uniq(opps).map((x, i) => ({ id: `o${i}`, kind: "opportunity", ...x })),
    ...uniq(openings).map((x, i) => ({ id: `s${i}`, kind: "opening", ...x }))];
  log("candidates:", pool.filter((x) => x.kind === "news").length, "news,", pool.filter((x) => x.kind === "opportunity").length, "opportunities,",
    pool.filter((x) => x.kind === "opening").length, "LA openings");
  if (!pool.length) return log("nothing new");

  const brief = pool.map((x) => ({ id: x.id, kind: x.kind, url: x.url, site: host(x.url), title: x.title, published: x.published,
    ...(x.kind === "news" ? { headline: x.headline, summary: x.summary, location: x.location } :
      x.kind === "opening" ? { show: x.show, artists: x.artists, venue: x.venue, neighborhood: x.neighborhood, openingDate: x.openingDate, openingTime: x.openingTime, closingDate: x.closingDate, officialUrl: x.officialUrl } :
      { name: x.name, organizer: x.organizer, deadline: x.deadline, eligibility: x.eligibility, offers: x.offers, location: x.location, officialUrl: x.officialUrl }) }));
  const prompt = `You curate Discover for "choir", an invite-only chat app Fía Benitez started for her artist friends: mostly women, mostly in Los Angeles. Fía is an LA installation and performance artist and writer (artist books, editing), a citizen of Mexico and Spain. Tagline: "Comments on culture".

Today is ${today}. From the CANDIDATES below, pick at most 4 news items, at most 4 opportunities, and at most 4 Los Angeles gallery openings that this group would genuinely want. Prefer: LA and California; women and non-binary artists; installation, site/place, performance, artist books and art writing; Latin America and Spain; free-to-apply calls with real money or time. Skip: anything already stale, paywalled teasers, art-market gossip, pay-to-enter juried shows, calls restricted to places this group can't apply from, and duplicates of the same story.

Rules from Fía: never include Artforum. If a candidate is a listing page (BOMB, Hyperallergic, NYFA, Res Artis, ArtDeadline, an aggregator) and it names the organizer's own page in officialUrl, use the official page as url and put the listing's name in "via". Never invent facts, dates, money or images: use only what the candidate says.

Write each title as a clean human headline (no site names or section suffixes). For news, description = one or two plain factual sentences. For opportunities, description must start "Deadline: <Mon D>." (or "Rolling.") followed by who can apply and what it offers, in two short sentences.
For openings, title = "<Show> at <Venue>"; description must start "Opens <Day Mon D>[, <time>]." then the artists and neighborhood in one short sentence; set deadline to the closing date if known, else the opening date. Prefer openings in the next 10 days, spread across different galleries.

ALREADY IN DISCOVER (never pick the same story or call again, even at a different URL):
${JSON.stringify(already)}

Reply with ONLY this JSON, no prose:
{"items":[{"id":"<candidate id>","kind":"news|opportunity|opening","url":"...","title":"...","description":"...","siteName":"<publication or organizer>","via":"<listing name or empty>","deadline":"YYYY-MM-DD or empty","why":"<a few words>"}]}

CANDIDATES:
${JSON.stringify(brief)}`;

  const reply = await claude(prompt);
  const json = reply.slice(reply.indexOf("{"), reply.lastIndexOf("}") + 1);
  let picks;
  try { picks = JSON.parse(json).items || []; } catch { throw Error("model reply was not JSON: " + reply.slice(0, 300)); }
  const byId = new Map(pool.map((x) => [x.id, x]));
  const items = [];
  for (const p of picks.filter((p) => byId.has(p.id) && !BLOCK.test(p.url || ""))) {
    const c = byId.get(p.id);
    let url = c.url;
    if (p.url && p.url === c.officialUrl && p.url !== c.url) url = (await loads(p.url)) ? p.url : c.url; // official only if real
    if (LISTINGS.test(host(url) + new URL(url).pathname)) { log("skip listing without a working official page:", url); continue; }
    if (existing.has(url)) continue;
    items.push({ url, title: p.title, description: p.description, kind: ["opportunity", "opening"].includes(p.kind) ? p.kind : "news",
      deadline: /^\d{4}-\d{2}-\d{2}$/.test(p.deadline || "") ? p.deadline : null,
      image: url === c.url ? c.image : null, siteName: p.siteName || host(url), via: p.via || (url !== c.url ? host(c.url) : null) });
  }
  for (const it of items) log({ opportunity: "📮", opening: "🖼️" }[it.kind] || "📰", it.title, "·", host(it.url), it.via ? `(via ${it.via})` : "");
  if (DRY) return log("dry run: posted nothing");
  const r = await fetch(`${API}?curate=1`, { method: "POST", headers: { "content-type": "application/json", "x-choir-editor": process.env.CHOIR_EDITOR_SECRET }, body: JSON.stringify({ items }) });
  const d = await r.json().catch(() => ({}));
  if (!r.ok) throw Error(`curate ${r.status}: ${d.error || ""}`);
  log("added", d.added?.length, "skipped", d.skipped?.length, "expired", d.expired?.length);
}

main().catch((e) => { log("✕", e.message); process.exit(1); });
