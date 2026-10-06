#!/usr/bin/env node
// ig-gun.mjs — the "Instagram gun" for Fuser ICP outreach. PROTOTYPE.
//
// Instead of starting from a studio's project page, start from a creator's or
// brand's PUBLIC Instagram, pick ONE product post we can reverse-engineer, and
// write the creator-first board brief, the client-story script and a DRAFT
// email in the shape the existing ICP lane already consumes.
//
//   node fuser-gun/ig-gun.mjs scout <handle> [--as aesthetic] [--limit 25] [--top 12] [--fixture] [--no-media]
//   node fuser-gun/ig-gun.mjs rank  <handle>                              # re-rank from media.json, no network
//   node fuser-gun/ig-gun.mjs brief <handle> [--hero <rank>] [--product "Name"] [--voice stock|jeffrey] [--rank <n>]
//
// Default source: indexed public posts through Exa's keyless MCP search.
// --source graph opts into Business Discovery with Facebook Login credentials.
// --fixture is an explicit offline demo; failed live searches never use it.
//
// Credentials: <repo>/vault/<account>/instagram.env, <PREFIX>_IG_USER_ID and
// <PREFIX>_IG_TOKEN, env first, then the file. They are never printed, never
// written into out/. This file is public-safe. Node builtins only.
//
// House rules the output must fit (iris video/docs/ICP-BRIEF.md, HOUSE-STYLE.md):
// reverse-engineer ONE real product; products, never people or children; the
// label separate from the object; never Meshy-retexture lettering; cap 5,000
// credits / floor 2,000; video at most 90 s; the voice quotes nothing verbatim
// and pads nothing. The gun ranks posts for "board-ability" with a cheap
// caption + media-type + aspect heuristic. It does NOT see pixels: a vision
// pass (a person, or a vision model) confirms the hero shows no face and a
// full, uncropped product before anything is built.

import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { basename, join, resolve } from "node:path";
import { normalizeSearchResults, searchInstagram } from "./public-search.mjs";

const here = import.meta.dirname;
const root = resolve(here, "..");
const OUT = resolve(process.env.IG_GUN_OUT || join(here, "out"));
const FIXTURES = join(here, "fixtures");

const API_VERSION = "v25.0";

// Same registry as toolchain/instagram/ig.mjs: alias → env-var prefix.
const ACCOUNTS = { aesthetic: "AESTHETIC", whistlegraph: "WHISTLEGRAPH", oskiewar: "OSKIEWAR", menuband: "MENUBAND" };

// Budget and shape constants from ICP-BRIEF (2026-09-30/10-01).
const CREDIT_CAP = 5000;
const CREDIT_FLOOR = 2000;
const VIDEO_MAX_S = 90;
// Observed 2026-10-05: Pro 2K image, Meshy v7 60k PBR, Kling pro 4 s without audio.
// These are estimates for planning; `fuser-board.mjs quote` is the live price.
const PRICE = { image: 192.9, meshy: 1653.2, kling: 617.2 };

// The worker refreshes this shortlist against the live catalog before building.
// Hirad's ordering guidance is about families, not individual model variants.
const MODEL_SELECTION = {
  catalog_url: "https://fuser.studio/models",
  order_basis: "Most used by default; Hirad, relayed by Jeffrey on 2026-10-05",
  policy: "Shortlist compatible families in most-used order, then choose by reference fidelity, required output and live price. Popularity is not a quality score; the new-models section is separate.",
  requires_catalog_refresh: true,
  record_before_generation: ["catalog check date", "observed family order", "exact variant", "selection reason", "live quote"],
  proposed: {
    image: { family: "Gemini Image", variant: "gemini-3-pro-image", reason: "Reference fidelity; no variant-level popularity claim" },
    mesh: { family: "Meshy", variant: "v7", reason: "Multi-view reconstruction" },
    video: { family: "Kling 3.0 Video", variant: "pro", reason: "Rigid-product push-in" },
  },
};
const IMAGE_INPUTS = { model: MODEL_SELECTION.proposed.image.variant, resolution: "2K", num_requests: 1, output_format: "image/png" };

// ── args ─────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; } else flags[key] = true;
  } else positional.push(a);
}
const cmd = positional.shift();
const handleArg = (positional.shift() || "").replace(/^@/, "").toLowerCase();

function die(msg) { console.error(`✗ ${msg}`); process.exit(1); }
const now = () => new Date().toISOString();
const sha256 = (buf) => createHash("sha256").update(buf).digest("hex");
const ensure = (dir) => { mkdirSync(dir, { recursive: true }); return dir; };
const writeJSON = (path, data) => writeFileSync(path, JSON.stringify(data, null, 2) + "\n");
const readJSON = (path) => JSON.parse(readFileSync(path, "utf8"));

// ── credentials (never printed) ──────────────────────────────────────
function parseEnvFile(path) {
  const out = {};
  for (const raw of readFileSync(path, "utf8").split("\n")) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    const eq = line.indexOf("=");
    if (eq < 0) continue;
    out[line.slice(0, eq).trim()] = line.slice(eq + 1).trim().replace(/^["']|["']$/g, "");
  }
  return out;
}

// Two flavours of the same API (found 2026-10-05, see README "API constraints"):
//   instagram  graph.instagram.com, "Instagram API with Instagram Login" — what every vault
//              instagram.env holds today. Its IG User node has NO business_discovery edge.
//   facebook   graph.facebook.com, "Instagram API with Facebook Login" — a Page-linked IG
//              professional account read with a Facebook Login user/page token. This is the
//              one that carries business_discovery. Creds: <PREFIX>_FB_IG_USER_ID + <PREFIX>_FB_TOKEN
//              in the same vault file (or env). Not provisioned yet on any account.
const HOSTS = { instagram: "https://graph.instagram.com", facebook: "https://graph.facebook.com" };

function loadAccount(alias, host = "instagram") {
  const prefix = ACCOUNTS[alias];
  if (!prefix) return null;
  const path = join(root, "vault", alias, "instagram.env");
  const file = existsSync(path) ? parseEnvFile(path) : {};
  const get = (k) => process.env[k] || file[k];
  const idKey = host === "facebook" ? `${prefix}_FB_IG_USER_ID` : `${prefix}_IG_USER_ID`;
  const tokenKey = host === "facebook" ? `${prefix}_FB_TOKEN` : `${prefix}_IG_TOKEN`;
  const igUserId = get(idKey);
  const token = get(tokenKey);
  if (!igUserId || !token) return null;
  return { alias, host, igUserId, token };
}

// ── Graph API ────────────────────────────────────────────────────────
const MEDIA_FIELDS = "id,caption,media_type,media_product_type,media_url,thumbnail_url,permalink,like_count,comments_count,timestamp";
const PROFILE_FIELDS = "username,name,biography,website,followers_count,media_count,profile_picture_url";

function discoveryUrl(creds, handle, limit, { children = true } = {}) {
  const media = `media.limit(${limit}){${MEDIA_FIELDS}${children ? ",children{id,media_type,media_url}" : ""}}`;
  const fields = `business_discovery.username(${handle}){${PROFILE_FIELDS},${media}}`;
  return `${HOSTS[creds.host]}/${API_VERSION}/${creds.igUserId}?fields=${encodeURIComponent(fields)}&access_token=${encodeURIComponent(creds.token)}`;
}

// Error shape mirrors ig.mjs: message, code/subcode, user title/msg, fbtrace.
// Nothing here can carry a token, so it is safe to persist in api.json.
function apiError(status, body) {
  const err = body?.error || body || {};
  return {
    status,
    message: err.message || null,
    code: err.code ?? null,
    error_subcode: err.error_subcode ?? null,
    error_user_title: err.error_user_title || null,
    error_user_msg: err.error_user_msg || null,
    fbtrace_id: err.fbtrace_id || null,
  };
}

async function callDiscovery(creds, handle, limit) {
  const attempts = [];
  for (const children of [true, false]) {
    const url = discoveryUrl(creds, handle, limit, { children });
    const response = await fetch(url, { signal: AbortSignal.timeout(30_000) });
    const body = await response.json().catch(() => ({}));
    if (response.ok && body.business_discovery) {
      attempts.push({ account: creds.alias, host: creds.host, children, ok: true, at: now() });
      return { ok: true, data: body.business_discovery, attempts };
    }
    const error = apiError(response.status, body);
    attempts.push({ account: creds.alias, host: creds.host, children, ok: false, error, at: now() });
    // Only a complaint about the children field is worth retrying without it; anything
    // else (no edge, permission, token, target not a business account) is final here.
    if (!(error.code === 100 && /children/i.test(error.message || ""))) break;
  }
  return { ok: false, attempts };
}

// ── ranking: board-ability ───────────────────────────────────────────
// Positive: a product or object we can take apart. Negative: people, faces,
// children, events. This is a caption heuristic, so it is a sort, not a verdict.
const PEOPLE = /\b(me|my|myself|i'm|im|selfie|portrait|headshot|team|founders?|staff|crew|interview|meet|wearing|wears|wore|outfit|fit check|models?|fans?|concert|tour|tonight|on stage|onstage|backstage|birthday|wedding|baby|kids?|child|children|family|friends?|dog|cat|puppy|smile|face|hair|makeup|dancing|singing|crowd|audience|party|celebrat\w*)\b/gi;
const PRODUCT = /\b(new|launch(?:ed|ing)?|now available|available now|shop|restock(?:ed)?|pre-?order|edition|colou?rways?|collection|set|puzzle|vase|lamp|mug|cup|bowl|tray|clock|candle|blanket|pillow|poster|print|vinyl|lp|record|cd|cassette|merch|hoodie|tee|t-?shirt|crewneck|tote|box|tin|bottle|jar|pack(?:aging)?|object|designed by|made (?:of|in|by)|link in bio|gift|toy|blocks?|game|notebook|pen|pencil|tape|sticker|cover|sleeve)\b/gi;
const MINORS = /\b(kids?|child|children|baby|toddler|son|daughter|niece|nephew)\b/i;

function countMatches(text, re) { re.lastIndex = 0; return (text.match(re) || []).length; }

function aspectBucket(w, h) {
  if (!w || !h) return null;
  const r = w / h;
  if (Math.abs(r - 1) < 0.06) return "1:1";
  if (Math.abs(r - 0.8) < 0.06) return "4:5";
  if (Math.abs(r - 1.91) < 0.12) return "1.91:1";
  if (r < 0.65) return "9:16";
  if (r > 1.2) return "landscape";
  return "portrait";
}

function scorePost(post, maxEngagement) {
  const caption = post.caption || "";
  const reasons = [];
  let score = 50;

  const type = post.media_type || "IMAGE";
  if (type === "IMAGE") { score += 15; reasons.push("still image +15"); }
  else if (type === "CAROUSEL_ALBUM") { score += 10; reasons.push("carousel, first frame usable +10"); }
  else if (type === "VIDEO") { score -= 20; reasons.push("video/reel needs a still, usually people −20"); }

  const people = Math.min(countMatches(caption, PEOPLE), 4);
  if (people) { score -= 12 * people; reasons.push(`people words ×${people} −${12 * people}`); }
  const product = Math.min(countMatches(caption, PRODUCT), 4);
  if (product) { score += 8 * product; reasons.push(`product words ×${product} +${8 * product}`); }
  if (MINORS.test(caption)) { score -= 60; reasons.push("mentions a minor: never a source −60"); }

  const engagement = post.like_count == null && post.comments_count == null ? null : (post.like_count || 0) + 3 * (post.comments_count || 0);
  const eng = maxEngagement ? (engagement || 0) / maxEngagement : 0;
  score += Math.round(15 * eng);
  if (eng) reasons.push(`engagement ${(eng * 100).toFixed(0)}% of top +${Math.round(15 * eng)}`);

  if (post.timestamp) {
    const days = (Date.now() - Date.parse(post.timestamp)) / 86_400_000;
    if (days <= 180) { score += 5; reasons.push("recent +5"); }
  }

  const aspect = post.aspect || aspectBucket(post.width, post.height);
  if (aspect === "1:1" || aspect === "4:5") { score += 5; reasons.push(`${aspect} product frame +5`); }
  else if (aspect === "9:16") { score -= 10; reasons.push("9:16 story/reel frame −10"); }

  if (post.fixture_kind === "person") { score -= 50; reasons.push("fixture marked as a person −50"); }
  if (post.fixture_kind === "product") { score += 10; reasons.push("fixture marked as a product +10"); }

  return { score: Math.max(0, Math.min(100, score)), engagement, reasons, aspect: aspect || null };
}

// Pull a product name out of a caption: a quoted phrase, else a Title-Case run
// of 2–4 words that is not at the start of a sentence. Flagged for confirmation.
function guessProduct(caption) {
  if (!caption) return null;
  const quoted = caption.match(/(?:“([^”]{3,40})”|"([^"\n]{3,40})")/);
  if (quoted) return (quoted[1] || quoted[2]).trim();
  const runs = caption.match(/(?<=[a-z,;:]\s)(?:[A-Z][\w'&-]+\s){1,3}[A-Z][\w'&-]+/g);
  if (runs) {
    const run = runs.find((r) => !/^(Link|Shop|Now|New|Follow|Tag|Happy)\b/.test(r));
    if (run) return run.trim();
  }
  const first = caption.split(/[.!?\n]/)[0].trim();
  return first.length > 3 && first.length <= 48 ? first : null;
}

function rankPosts(posts) {
  const maxEng = Math.max(0, ...posts.map((p) => (p.like_count || 0) + 3 * (p.comments_count || 0)));
  const ranked = posts.map((p) => {
    const s = scorePost(p, maxEng);
    return {
      id: p.id, permalink: p.permalink, media_type: p.media_type, timestamp: p.timestamp,
      like_count: p.like_count ?? null, comments_count: p.comments_count ?? null,
      engagement: s.engagement, score: s.score, aspect: s.aspect, reasons: s.reasons,
      product_guess: p.product || guessProduct(p.caption), caption_excerpt: (p.caption || "").slice(0, 140),
      file: p.file || null, fixture_kind: p.fixture_kind || null,
    };
  }).sort((a, b) => b.score - a.score || (b.engagement || 0) - (a.engagement || 0) || Date.parse(b.timestamp || 0) - Date.parse(a.timestamp || 0));
  ranked.forEach((r, i) => { r.rank = i + 1; });
  return ranked;
}

// ── image headers (dimensions without a decoder) ─────────────────────
function imageSize(buf) {
  if (buf.length > 24 && buf.readUInt32BE(0) === 0x89504e47) return { format: "png", width: buf.readUInt32BE(16), height: buf.readUInt32BE(20) };
  if (buf.length > 4 && buf[0] === 0xff && buf[1] === 0xd8) {
    let i = 2;
    while (i + 9 < buf.length) {
      if (buf[i] !== 0xff) { i++; continue; }
      const marker = buf[i + 1];
      if (marker === 0xd8 || marker === 0x01 || (marker >= 0xd0 && marker <= 0xd7)) { i += 2; continue; }
      const len = buf.readUInt16BE(i + 2);
      if (marker >= 0xc0 && marker <= 0xcf && ![0xc4, 0xc8, 0xcc].includes(marker))
        return { format: "jpeg", height: buf.readUInt16BE(i + 5), width: buf.readUInt16BE(i + 7) };
      i += 2 + len;
    }
    return { format: "jpeg" };
  }
  if (buf.length > 30 && buf.toString("ascii", 0, 4) === "RIFF" && buf.toString("ascii", 8, 12) === "WEBP") {
    const chunk = buf.toString("ascii", 12, 16);
    if (chunk === "VP8X") return { format: "webp", width: 1 + buf.readUIntLE(24, 3), height: 1 + buf.readUIntLE(27, 3) };
    if (chunk === "VP8L") { const b = buf.readUInt32LE(21); return { format: "webp", width: 1 + (b & 0x3fff), height: 1 + ((b >> 14) & 0x3fff) }; }
    if (chunk === "VP8 ") return { format: "webp", width: buf.readUInt16LE(26) & 0x3fff, height: buf.readUInt16LE(28) & 0x3fff };
  }
  return { format: "unknown" };
}

function stillUrl(post) {
  if (post.media_type === "VIDEO") return post.thumbnail_url || null;
  if (post.media_type === "CAROUSEL_ALBUM") {
    const child = (post.children?.data || post.children || []).find((c) => c.media_type === "IMAGE" && c.media_url);
    return child?.media_url || post.media_url || null;
  }
  return post.media_url || null;
}

async function downloadStills(dir, posts, top) {
  const mediaDir = ensure(join(dir, "media"));
  const byEngagement = [...posts].sort((a, b) => ((b.like_count || 0) + 3 * (b.comments_count || 0)) - ((a.like_count || 0) + 3 * (a.comments_count || 0))).slice(0, top);
  const index = [];
  for (const post of byEngagement) {
    const url = stillUrl(post);
    if (!url) { index.push({ id: post.id, permalink: post.permalink, skipped: "no still url" }); continue; }
    try {
      const res = await fetch(url, { signal: AbortSignal.timeout(60_000) });
      if (!res.ok) { index.push({ id: post.id, permalink: post.permalink, skipped: `http ${res.status}` }); continue; }
      const buf = Buffer.from(await res.arrayBuffer());
      const size = imageSize(buf);
      if (size.format === "unknown") { index.push({ id: post.id, permalink: post.permalink, skipped: "response is not a supported image" }); continue; }
      const ext = size.format === "png" ? "png" : size.format === "webp" ? "webp" : "jpg";
      const file = `${post.id}.${ext}`;
      writeFileSync(join(mediaDir, file), buf);
      post.file = `media/${file}`; post.width = size.width; post.height = size.height;
      index.push({ file: post.file, id: post.id, source_url: url.split("?")[0], permalink: post.permalink, fetched_at: now(),
        sha256: sha256(buf), width: size.width ?? null, height: size.height ?? null, bytes: buf.length,
        attribution: post.discovery ? "Search-indexed Instagram thumbnail; source and crop need visual confirmation. Reference only; rights remain with the owner." : "Public Instagram post; rights remain with the account. Reference only, never presented as our work." });
      process.stderr.write(`  ↓ ${post.id} ${size.width}×${size.height}\n`);
    } catch (e) {
      index.push({ id: post.id, permalink: post.permalink, skipped: e.message });
    }
  }
  writeJSON(join(dir, "media-index.json"), index);
  const csv = ["file,source_url,page_url,fetched_at,attribution,width,height"];
  for (const m of index) if (m.file) csv.push([m.file, m.source_url, m.permalink, m.fetched_at, `"${m.attribution}"`, m.width, m.height].join(","));
  writeFileSync(join(dir, "media.csv"), csv.join("\n") + "\n");
  return index;
}

// ── fixtures ─────────────────────────────────────────────────────────
function loadFixture(handle) {
  const path = join(FIXTURES, `${handle}.json`);
  if (!existsSync(path)) return null;
  const f = readJSON(path);
  f.profile = { ...f.profile, username: f.profile?.username || handle, fixture: true };
  f.media = (f.media || []).map((m, i) => ({ id: m.id || `fixture-${handle}-${i + 1}`, ...m, fixture: true }));
  return f;
}

// ── scout ────────────────────────────────────────────────────────────
async function scout(handle) {
  if (!handle) die("scout needs a handle");
  const dir = ensure(join(OUT, handle));
  const limit = Number(flags.limit || 25);
  const top = Number(flags.top || 12);
  if (!Number.isInteger(limit) || limit < 1 || limit > 100 || !Number.isInteger(top) || top < 1 || top > 100) die("--limit and --top must be integers from 1 to 100");
  const source = flags.fixture ? "fixture" : flags["search-results"] ? "search" : flags.source || (flags.host || flags.as ? "graph" : "exa");
  if (!["fixture", "search", "graph", "exa"].includes(source)) die("--source must be exa or graph; use --fixture for a demo");
  const api = { handle, requested_at: now(), attempts: [], source: null, note: null };

  let profile = null, media = null;
  if (source === "exa" || source === "search") {
    process.stderr.write(`→ public Instagram search @${handle} via ${source === "exa" ? "Exa" : "saved results"}\n`);
    const evidence = source === "exa" ? await searchInstagram(handle, limit, flags.query ? String(flags.query) : undefined) : readJSON(resolve(String(flags["search-results"])));
    const normalized = normalizeSearchResults(handle, evidence);
    if (!normalized.media.length) {
      writeJSON(join(dir, "search-failed.json"), { evidence, excluded: normalized.excluded });
      die(`no posts attributable to @${handle} in search evidence; see search-failed.json. Previous successful output retained; no fixture substituted.`);
    }
    writeJSON(join(dir, "search.json"), evidence);
    writeJSON(join(dir, "search-excluded.json"), normalized.excluded);
    profile = normalized.profile; media = normalized.media;
    api.source = source === "exa" ? "exa" : "public-search";
    api.note = "Indexed public posts, not a complete or current feed. Counts, post dates and exact media types are unknown. Search thumbnails need visual confirmation.";
    api.attempts.push({ provider: evidence.provider || source, query: evidence.query || null, retrieved_at: evidence.retrieved_at || now(), ok: true, returned: evidence.results.length, accepted: media.length });
  }
  if (source === "graph") {
    api.version = API_VERSION; api.edge = "business_discovery";
    const aliases = flags.as ? [String(flags.as)] : Object.keys(ACCOUNTS);
    const hosts = flags.host ? [String(flags.host)] : ["facebook"];
    if (hosts.some((host) => !HOSTS[host])) die("--host must be facebook or instagram");
    outer: for (const host of hosts) {
      for (const alias of aliases) {
        const creds = loadAccount(alias, host);
        if (!creds) { api.attempts.push({ account: alias, host, ok: false, error: { message: "not provisioned on this host" } }); continue; }
        process.stderr.write(`→ business_discovery @${handle} as @${alias} via ${HOSTS[host]}\n`);
        const r = await callDiscovery(creds, handle, limit);
        api.attempts.push(...r.attempts);
        if (r.ok) {
          const { media: m, ...p } = r.data;
          profile = p; media = m?.data || [];
          api.source = "graph"; api.account = alias; api.host = HOSTS[host];
          break outer;
        }
        const last = r.attempts.at(-1)?.error;
        process.stderr.write(`  ✗ ${last?.status} ${last?.message || ""} (code ${last?.code}/${last?.error_subcode ?? "-"})\n`);
        if (flags.as && flags.host) break outer;
      }
    }
  }

  if (source === "fixture") {
    const fixture = loadFixture(handle);
    if (!fixture) {
      writeJSON(join(dir, "api.json"), api);
      die(`no fixture at fixtures/${handle}.json`);
    }
    profile = fixture.profile; media = fixture.media;
    api.source = "fixture"; api.fixture_path = `fixtures/${handle}.json`;
    api.note = "fixture explicitly requested; no API call made";
    process.stderr.write(`→ using fixture for @${handle} (${media.length} items)\n`);
  }
  if (!profile) {
    writeJSON(join(dir, "graph-error.json"), api);
    die(`no Business Discovery result for @${handle}; use --source exa for public search. No fixture substituted.`);
  }

  // A new scout invalidates drafts built from a previous hero or data source.
  for (const path of ["brief.md", "script.txt", "email.md", "board-spec.json", "icp-queue.line", "provenance.json", "data"])
    rmSync(join(dir, path), { recursive: true, force: true });

  api.counts = { media_returned: media.length, profile_followers: profile.followers_count ?? null, profile_media_count: profile.media_count ?? null };
  writeJSON(join(dir, "profile.json"), { ...profile, fetched_at: now(), source: api.source });
  writeJSON(join(dir, "media.json"), media);

  if (!flags["no-media"] && api.source !== "fixture") await downloadStills(dir, media, top);
  else {
    writeJSON(join(dir, "media-index.json"), []);
    writeFileSync(join(dir, "media.csv"), "file,source_url,page_url,fetched_at,attribution,width,height\n");
  }
  writeJSON(join(dir, "media.json"), media); // now with file/width/height

  const ranking = rankPosts(media);
  writeJSON(join(dir, "ranking.json"), {
    handle, ranked_at: now(), source: api.source,
    note: "Caption + media-type + aspect heuristic. A vision pass (person or vision model) picks the hero: it must show no face, no minor, and the FULL uncropped product.",
    hero: ranking.find((r) => r.media_type !== "VIDEO") || ranking[0] || null,
    posts: ranking,
  });
  writeJSON(join(dir, "api.json"), api);

  console.log(`✓ @${handle} via ${api.source}${api.account ? ` (@${api.account})` : ""}: ${media.length} posts, ${profile.followers_count ?? "?"} followers, ${profile.media_count ?? "?"} media`);
  for (const r of ranking.slice(0, 5)) console.log(`  #${r.rank} ${String(r.score).padStart(3)}  ${r.media_type?.padEnd(14)} ${r.permalink || "(fixture)"}  ${r.product_guess || ""}`);
  console.log(`  → ${dir}`);
}

function rank(handle) {
  const dir = join(OUT, handle);
  if (!existsSync(join(dir, "media.json"))) die(`scout @${handle} first`);
  const media = readJSON(join(dir, "media.json"));
  const ranking = rankPosts(media);
  const prior = existsSync(join(dir, "ranking.json")) ? readJSON(join(dir, "ranking.json")) : {};
  writeJSON(join(dir, "ranking.json"), { ...prior, handle, ranked_at: now(), hero: ranking.find((r) => r.media_type !== "VIDEO") || ranking[0] || null, posts: ranking });
  for (const r of ranking.slice(0, 10)) console.log(`  #${r.rank} ${String(r.score).padStart(3)}  ${r.media_type?.padEnd(14)} ${r.permalink || "(fixture)"}  ${r.product_guess || ""}`);
}

// ── brief ────────────────────────────────────────────────────────────
// What we grow from the product, by category word. One new sibling in their
// system; the lettering and artwork stay exactly as published.
function variantFor(product, caption, hints = {}) {
  const base = categoryVariant(product, caption);
  // A fixture or a worker may override any field; the rest comes from the category.
  const picked = Object.fromEntries(["variant", "keep", "blank", "lift", "stage"].filter((k) => hints[k]).map((k) => [k, hints[k]]));
  return { ...base, ...picked };
}

function categoryVariant(product, caption) {
  const text = `${product || ""} ${caption || ""}`.toLowerCase();
  // variant: what Nano Banana Pro draws · keep: what stays exact (short, spoken) · blank: the carrier
  // lift: the "label off the bottle" line · stage: their own product-only staging to match
  if (/vinyl|\blp\b|record/.test(text)) return { variant: "a new pressing colour", keep: "your sleeve", blank: "the disc", lift: "Next, it sets the sleeve aside and keeps the plain disc.", stage: "your flat-lay, half out of the sleeve" };
  if (/puzzle/.test(text)) return { variant: "a new gradient", keep: "your box", blank: "the pieces", lift: "Next, it takes the print off the pieces.", stage: "your top-down shot" };
  if (/hoodie|tee|t-shirt|crewneck|shirt|tote/.test(text)) return { variant: "a new colourway", keep: "your print", blank: "the garment", lift: "Next, it lifts the print off the garment.", stage: "your flat-lay" };
  if (/candle|vase|lamp|mug|cup|bowl|clock|tray|toy|blocks?|robot/.test(text)) return { variant: "a new colourway", keep: "the form and every mark", blank: "the body", lift: "Next, it takes the marks off the body.", stage: "your tabletop shot" };
  if (/poster|print|cover|sleeve|book|zine/.test(text)) return { variant: "a new edition colour", keep: "your artwork", blank: "the paper", lift: "Next, it lifts the artwork off the paper.", stage: "your wall shot" };
  if (/bottle|jar|tin|box|pack/.test(text)) return { variant: "a new sibling in the range", keep: "your label and lettering", blank: "the container", lift: "Next, it takes the label off the container.", stage: "your product-only shot" };
  return { variant: "a new colourway", keep: "every mark and word", blank: "the object", lift: "Next, it takes the marks off the object.", stage: "your own product shot" };
}

function displayName(profile) {
  const n = profile.name || profile.username;
  return n.replace(/\s*\|.*$/, "").trim();
}

function spokenName(profile) {
  // The script addresses the account by its public name ("Areaware", "Olivia"),
  // first word for a person-named creator account, whole name for a brand.
  const name = displayName(profile);
  return profile.address_as || (profile.kind === "creator" ? name.split(" ")[0] : name);
}

function scriptLines({ who, product, v, voice }) {
  const opener = voice === "jeffrey"
    ? `Hey ${who}, this is @jeffrey from Fuser. I checked out your Instagram and made a variation of your ${product}.`
    : `Hey ${who}, this is Fuser. We checked out your Instagram and made a variation of your ${product}.`;
  // 14h: one short sentence per step, the model named once where it is pressed,
  // no hedges in the voice (provenance lives in the receipt). 70–100 words.
  return [
    opener,
    "It starts from your photo.",
    `Nano Banana Pro draws ${v.variant}, and keeps ${v.keep} exactly as it is.`,
    v.lift,
    "Then it makes the side and back views.",
    "Meshy turns them into a 3D model.",
    `We stage it like ${v.stage}.`,
    "And Kling gives it a slow push-in.",
    `So here's your original ${product}, and a new one grown from it!`,
    "Give it a try at fuser.studio",
  ];
}

function wordCount(lines) { return lines.join(" ").split(/\s+/).filter(Boolean).length; }

function brief(handle) {
  const dir = join(OUT, handle);
  for (const f of ["profile.json", "media.json", "ranking.json"]) if (!existsSync(join(dir, f))) die(`scout @${handle} first (missing ${f})`);
  const profile = readJSON(join(dir, "profile.json"));
  const media = readJSON(join(dir, "media.json"));
  const ranking = readJSON(join(dir, "ranking.json"));
  const api = existsSync(join(dir, "api.json")) ? readJSON(join(dir, "api.json")) : {};

  const heroRank = flags.hero ? Number(flags.hero) : null;
  const hero = heroRank ? ranking.posts.find((p) => p.rank === heroRank) : ranking.hero;
  if (!hero) die("no hero post in ranking.json");
  if (hero.media_type === "VIDEO") die(`hero #${hero.rank} is a video; pick a still with --hero <rank>`);
  if (hero.fixture_kind === "person" || MINORS.test(hero.caption_excerpt)) die(`hero #${hero.rank} reads as a person; the gun never builds from a person. Pick another with --hero <rank>.`);
  const post = media.find((m) => m.id === hero.id) || {};

  const who = spokenName(profile);
  const brand = displayName(profile);
  const product = flags.product || post.product || hero.product_guess || "hero object";
  const productConfirmed = Boolean(flags.product || post.product);
  const v = variantFor(product, post.caption, post);
  const voice = flags.voice === "jeffrey" ? "jeffrey" : "stock";
  const lines = scriptLines({ who, product, v, voice });
  // The script speaks to them ("your box"); the brief and the prompts speak about them.
  const theirs = (s) => s.replace(/^your /, "their ");
  const theObj = (s) => s.replace(/^your /, "the ");
  const cap = (s) => s.charAt(0).toUpperCase() + s.slice(1);
  const blankName = `Blank ${cap(v.blank.replace(/^the /, ""))}`;
  const newName = `New ${product}`;
  const refName = `${product} · @${profile.username}`;
  const words = wordCount(lines);
  const slug = handle.replace(/[^a-z0-9]+/g, "-");
  const rankNo = flags.rank || "000";
  const refFile = hero.file || `reference/${slug}-hero.jpg`;
  const today = now().slice(0, 10);
  const permalink = hero.permalink || "(fixture: confirm the post URL by hand)";
  const searchSource = ["exa", "public-search"].includes(api.source);
  const sourceDescription = api.source === "graph"
    ? `Business Discovery via ${api.host} ${api.version}, read as @${api.account}`
    : searchSource ? `${api.source} indexed public search; see search.json for the retrieved evidence. This is not a complete feed; engagement, post dates and media types remain unknown`
    : `hand-written fixture ${api.fixture_path || ""}; no live post evidence`;
  const sourceStatus = api.source === "graph" ? `solid: Graph API at ${profile.fetched_at || today}`
    : searchSource ? "soft: indexed public search; confirm against the post" : "soft: hand-written fixture";
  const imageStatus = hero.file ? (searchSource ? "Downloaded search thumbnail; confirm media type, full product and crop before generation." : "Downloaded reference; visual confirmation required.") : "Reference image missing; retrieve and inspect the source before generation.";

  const budget = [
    ["Variant (Nano Banana Pro)", PRICE.image], ["Lift the blank object (Nano Banana Pro)", PRICE.image],
    ["Side view (Nano Banana Pro)", PRICE.image], ["Back view (Nano Banana Pro)", PRICE.image],
    ["3D (Meshy 7 multi-image, read price live)", PRICE.meshy], ["Staged shot (Nano Banana Pro)", PRICE.image],
    ["Push-in (Kling 3.0 pro, 4 s)", PRICE.kling],
  ];
  const estimate = budget.reduce((s, [, c]) => s + c, 0);
  const retries = 2 * PRICE.image + PRICE.kling;

  const top3 = ranking.posts.slice(0, 3);

  const briefMd = `# ${brand} · ${product} — creator-first ICP brief (prototype)

Generated ${today} by \`fuser-gun/ig-gun.mjs brief ${handle}\` from the public Instagram of @${profile.username}
(${sourceDescription}).
Same shape as \`video/docs/ICP-BRIEF.md\`, read as the worker would: TOP-DOWN from ONE real post.

## Client

- Account: @${profile.username}${profile.name ? ` · ${profile.name}` : ""}${profile.kind ? ` · ${profile.kind}` : ""}
- Followers ${profile.followers_count ?? "n/a"} · media ${profile.media_count ?? "n/a"}${profile.website ? ` · ${profile.website}` : ""}
- Bio: ${(profile.biography || "").replace(/\s+/g, " ").trim() || "n/a"}
- icp-queue line: \`${rankNo}|${brand}|${slug}\`

## The source post (TOP-DOWN, 14d)

- Post: ${permalink}
- Reference status: ${imageStatus}
- Posted ${hero.timestamp || "n/a"} · ${hero.media_type || "IMAGE"} · likes ${hero.like_count ?? "n/a"} · comments ${hero.comments_count ?? "n/a"} · aspect ${hero.aspect || "n/a"}
- Board-ability ${hero.score}/100 (${hero.reasons.join("; ")})
- Caption: ${(post.caption || "").replace(/\s+/g, " ").slice(0, 300) || "n/a"}
- Reference file: \`${refFile}\` — credited on the board's first node as "${product} · @${profile.username}", URL in \`reference/provenance.json\`.
- Product: **${product}**${productConfirmed ? "" : " (guessed from the caption; confirm before building, `--product \"Name\"`)"}.

**Vision pass before anything is built (hard gate):** the hero shows no face, no minor, and the FULL uncropped
product (FULL PRODUCT FIRST). If the post crops it, add the account's own full view, or the maker's official
product shot, as a second reference. If no post of theirs shows the product clean, block with that reason;
never draw a stand-in.

## What we grow from it (reverse-engineer ONE real product)

- Original: their ${product}, as published, credited.
- Parts: ${v.keep.replace(/^your /, "their ")} (kept exact) separate from ${v.blank} (the carrier).
- New sibling: ${v.variant} of the same ${product}. Same type style, layout and palette logic. Spelled correctly. Recorded in the receipt
  as our exploration in their style, never their artwork. The voice never says so.
- Rebuild: put their artwork back onto the recoloured ${v.blank.replace(/^the /, "")} in 2D, where the lettering stays exact.
- 3D: side and back views checked against the front, into Meshy 7 multi-image. If the lettering smears, the 3D
  is the form only; say so in the receipt. Never retexture lettering.
- Staging: match ${theirs(v.stage)} (set, light, shadow), theirs beside ours in the fidelity frame.
- Film: Kling 3.0 pro, 4 s, push-in, rigid and simple.

## Node chain (board spec in \`board-spec.json\`)

| # | node | type | input | output |
|---|---|---|---|---|
| 0 | ${refName} | ImageNode | \`${refFile}\` | the original |
| 1 | ${newName} | FalGeminiImageNode (Nano Banana Pro) | 0 | ${v.variant}, artwork kept |
| 2 | ${blankName} | FalGeminiImageNode | 0 | ${v.blank} with the artwork lifted off |
| 3 | Side View · Back View | FalGeminiImageNode ×2 | 1 | views for 3D, checked against the front |
| 4 | 3D Model | FalMeshyNode v7 multi-image | 1, 3 | the form (texture ref = 1; lettering never retextured) |
| 5 | Staged Shot | FalGeminiImageNode | 1, 0 | ours in their staging |
| 6 | Push In | FalKling30VideoNode pro 4 s | 5 | the clip |

Stages: [1, 2] → [3] → [4, 5] → [6]. Cheap first, inspect, fidelity frame per node (\`evidence/fidelity-<node>.png\`).

Refresh [Fuser Models](${MODEL_SELECTION.catalog_url}) before building: its default order is most used
(Hirad, relayed by Jeffrey on 2026-10-05). Shortlist compatible families in that order, then choose
by reference fidelity, output needs and live price. Record the check date, observed order, exact
variant and reason in \`board-spec.json\`. Family order does not rank variants; new models are separate.

## Budget

Cap **${CREDIT_CAP.toLocaleString()}** credits for this client, Iris balance never below **${CREDIT_FLOOR.toLocaleString()}**.
Estimate ${estimate.toLocaleString()} (${budget.map(([n, c]) => `${n} ${c}`).join(" · ")}), plus up to ${retries} for two retries.
\`fuser-board.mjs quote\` is the price; read it before every press. This brief spends nothing.

## Acceptance

Per node: not cheaper or different than the post next to it (flat, wrong material, lost detail, altered lettering = fail).
Two fidelity failures drop the branch and the story goes on without it. What is kept (${v.keep.replace(/^your /, "their ")}) matches the post exactly.

## Video (≤ ${VIDEO_MAX_S} s, HOUSE-STYLE 14h adapted for a creator)

Opens on the finished board zoomed out. Every press filmed on its own node. Say it, show it. Compare before the CTA.
Script: \`script.txt\` (${lines.length} lines, ${words} words, ${voice === "jeffrey" ? "@jeffrey PVC" : "stock voice, so the opener says \"this is Fuser\""}; models named once at their press).
Spoken CTA "Give it a try at Fuser dot studio"; caption "Give it a try at fuser.studio", no period, no captions on the last frame.

## Other posts the gun ranked

${top3.map((p) => `${p.rank}. ${p.score}/100 · ${p.media_type} · ${p.permalink || "(fixture)"} · ${p.product_guess || ""}`).join("\n")}

## What this is not

Not verified with the account. Not a commitment. The post is reference only; rights stay with @${profile.username}.
No DM, no comment, no tag, no Instagram write of any kind. Email in \`email.md\` is a DRAFT and is not sent from here.
`;

  const emailMd = `# DRAFT — not sent

From: Jeffrey <jeffrey@fuser.studio>
To: ${profile.contact || "(business contact from their website or bio; Business Discovery exposes no email)"}
Subject: A variation of your ${product}, made in Fuser

Hi ${who},

We checked out your Instagram and made a short piece around your ${product}: your post as the starting point, then ${v.variant}, a 3D model of it, and a four-second push-in, all on one Fuser board.

The video is attached, under ${Math.min(VIDEO_MAX_S, 75)} seconds. ${v.keep.replace(/^your /, "Your ")} is untouched; the new one is our exploration in your style.

If a board like this fits how you make things, I can set one up for you to try.

Jeffrey
Fuser · fuser.studio

Attachment: ${slug}-client-story.mp4 (after render and approval). Send-readiness: needs approval (first contact, attachment).
`;

  const spec = {
    title: `${brand} · ${product}`,
    description: `Creator-first ICP board grown from ${permalink}. Reference credited on node 0. Prototype spec from fuser-gun; prompts to be tuned by the worker after the vision pass.`,
    budget: CREDIT_CAP,
    model_selection: MODEL_SELECTION,
    nodes: [
      { name: refName, type: "ImageNode", file: refFile, annotation: `${permalink} — ${searchSource ? "search-indexed candidate; visual confirmation required" : "reference; visual confirmation required"}` },
      { name: newName, type: "FalGeminiImageNode", inputs: { ...IMAGE_INPUTS, prompt: `Using this exact ${product} as the only reference, make ${v.variant}. Keep ${theObj(v.keep)} exactly as shown, every letter and mark identical. Same light, same background, same camera.`, aspect_ratio: hero.aspect === "4:5" ? "4:5" : "1:1" }, wires: { image: [refName] } },
      { name: blankName, type: "FalGeminiImageNode", inputs: { ...IMAGE_INPUTS, prompt: `Show the same ${product} with all printed artwork and lettering removed, leaving only ${v.blank}, plain. Same light, same angle.`, aspect_ratio: hero.aspect === "4:5" ? "4:5" : "1:1" }, wires: { image: [refName] } },
      { name: "Side View", type: "FalGeminiImageNode", inputs: { ...IMAGE_INPUTS, prompt: `The same object from the first image, exact side view, same proportions, same colours, plain background.`, aspect_ratio: "1:1" }, wires: { image: [newName] } },
      { name: "Back View", type: "FalGeminiImageNode", inputs: { ...IMAGE_INPUTS, prompt: `The same object from the first image, exact back view, same proportions, same colours, plain background.`, aspect_ratio: "1:1" }, wires: { image: [newName] } },
      { name: "3D Model", type: "FalMeshyNode", inputs: { version: MODEL_SELECTION.proposed.mesh.variant, target_polycount: 60000, enable_pbr: true }, wires: { image: [newName, "Side View", "Back View"], texture_image_url: [newName] } },
      { name: "Staged Shot", type: "FalGeminiImageNode", inputs: { ...IMAGE_INPUTS, prompt: `Place the object from the first image in the setting of the second image. Match light, shadow and lens. Nothing else changes.`, aspect_ratio: hero.aspect === "4:5" ? "4:5" : "1:1" }, wires: { image: [newName, refName] } },
      { name: "Push In", type: "FalKling30VideoNode", inputs: { prompt: "Locked-off product shot. The camera pushes in slowly. Nothing else moves.", model: MODEL_SELECTION.proposed.video.variant, duration: 4, generate_audio: false }, wires: { image: ["Staged Shot"] } },
    ],
    stages: [[newName, blankName], ["Side View", "Back View"], ["3D Model", "Staged Shot"], ["Push In"]],
  };

  // Dossier-shaped data/ so the existing lane's tooling reads it unchanged.
  const data = ensure(join(dir, "data"));
  const solid = sourceStatus;
  writeFileSync(join(data, "profile.csv"), ["field,value,status,source_url",
    `username,@${profile.username},${solid},https://www.instagram.com/${profile.username}/`,
    `name,"${(profile.name || "").replace(/"/g, "'")}",${solid},https://www.instagram.com/${profile.username}/`,
    `followers_count,${profile.followers_count ?? ""},${profile.followers_count != null ? solid : "missing"},https://www.instagram.com/${profile.username}/`,
    `media_count,${profile.media_count ?? ""},${profile.media_count != null ? solid : "missing"},https://www.instagram.com/${profile.username}/`,
    `website,${profile.website || ""},${profile.website ? "solid: profile field" : "missing"},https://www.instagram.com/${profile.username}/`].join("\n") + "\n");
  writeFileSync(join(data, "projects.csv"), ["project,year,medium_and_relevance,source_urls,image_file,status",
    ...ranking.posts.filter((p) => p.media_type !== "VIDEO").slice(0, 8).map((p) => `"${(p.product_guess || "post").replace(/"/g, "'")}",${(p.timestamp || "").slice(0, 4)},"Instagram ${p.media_type}; board-ability ${p.score}/100",${p.permalink || ""},${p.file || ""},${sourceStatus}`)].join("\n") + "\n");
  writeFileSync(join(data, "signals.csv"), ["signal,observed_date,source_urls,status",
    ...ranking.posts.slice(0, 8).map((p) => `"${p.caption_excerpt.replace(/"/g, "'").replace(/\n/g, " ")}",${(p.timestamp || "").slice(0, 10)},${p.permalink || ""},${sourceStatus}; engagement ${p.engagement ?? "unknown"}`)].join("\n") + "\n");
  writeFileSync(join(data, "fuser-fit.md"), briefMd);
  writeFileSync(join(data, "outreach.md"), emailMd);
  writeFileSync(join(data, "README.md"), `# Research status\n\nSource: ${sourceDescription}. Retrieved ${profile.fetched_at || today}.\n\n${imageStatus}\n\nThe product name "${product}" ${productConfirmed ? "was supplied" : "is guessed from the indexed caption"}. Ranking uses text and image dimensions; it does not inspect pixels.\n\nMissing: verified business contact, who makes what for the account, rights to the design, any prior Fuser contact. No generation or outreach performed.\n`);

  writeFileSync(join(dir, "brief.md"), briefMd);
  writeFileSync(join(dir, "script.txt"), lines.join("\n") + "\n");
  writeFileSync(join(dir, "email.md"), emailMd);
  writeJSON(join(dir, "board-spec.json"), spec);
  writeFileSync(join(dir, "icp-queue.line"), `${rankNo}|${brand}|${slug}\n`);
  writeJSON(join(dir, "provenance.json"), { reference: [{ file: hero.file || null, permalink: hero.permalink, source: api.source, discovery: post.discovery || null, credit: `${brand} (@${profile.username})`, note: imageStatus, needs_visual_confirmation: true }], generated_at: now() });

  console.log(`✓ brief for @${handle}: hero #${hero.rank} ${permalink}`);
  console.log(`  product "${product}"${productConfirmed ? "" : " (guessed; confirm)"} → ${v.variant}`);
  console.log(`  script ${lines.length} lines / ${words} words (${voice}) · estimate ${estimate} cr of cap ${CREDIT_CAP}`);
  console.log(`  → ${dir}/{brief.md,script.txt,email.md,board-spec.json,icp-queue.line,data/}`);
}

// ── main ─────────────────────────────────────────────────────────────
function help(code = 0) {
  console.log(`ig-gun.mjs — Instagram gun for Fuser ICP outreach (prototype; read-only)

usage:
  node fuser-gun/ig-gun.mjs scout <handle> [--source exa|graph] [--query "search terms"] [--limit 25] [--top 12] [--no-media]
  node fuser-gun/ig-gun.mjs scout <handle> --search-results results.json
  node fuser-gun/ig-gun.mjs scout <handle> --fixture
  node fuser-gun/ig-gun.mjs rank  <handle>
  node fuser-gun/ig-gun.mjs brief <handle> [--hero <rank>] [--product "Name"] [--voice stock|jeffrey] [--rank <n>]

scout  searches public Instagram posts through Exa (no token). Saves search evidence, post
       permalinks, available thumbnails and rankings in out/<handle>/. Search counts and dates
       remain unknown. --source graph opts into Business Discovery (--as account / --host
       facebook|instagram). --fixture explicitly uses a demo; no automatic fixture fallback.
brief  writes brief.md (ICP-BRIEF shape), script.txt (HOUSE-STYLE 14h, creator form), email.md (DRAFT),
       board-spec.json (fuser-board.mjs spec), icp-queue.line and a dossier-shaped data/.
Spends no credits, builds no board, sends nothing, writes nothing to Instagram.`);
  process.exit(code);
}

if (!cmd || flags.help || cmd === "help") help(cmd ? 0 : 1);
if (!/^[a-z0-9_][a-z0-9_.]{0,29}$/.test(handleArg) || handleArg.includes("..")) die("use an Instagram handle, not a path or URL");
try {
  if (cmd === "scout") await scout(handleArg);
  else if (cmd === "rank") rank(handleArg);
  else if (cmd === "brief") brief(handleArg);
  else help(1);
} catch (e) {
  die(e.message || String(e));
}
