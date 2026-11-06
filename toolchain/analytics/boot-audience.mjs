// boot-audience — who is loading the network: people, bots, and walls.
//
// The first-party visit tracker keeps only an `automated` flag by design (no
// UA, IP or URL), so it can say how many bot visits there were but never which
// bots. Lith's boot telemetry (the `boots` collection) does keep the user
// agent, the query string, Cloudflare's country and the signed-in handle, so
// this is where the nature of the traffic comes from. Three audiences:
//
//   humans    — a top-level page load, not automated: browser, country,
//               signed-in or anonymous, which pieces, which referrer sites
//   automated — the same load from a known bot UA, WebDriver fleet marker or
//               one of our own render query flags: by family and country
//   walls     — embedded iframes (the kidlisp.com gallery screens); counted
//               apart because one screen in a room boots thousands of times
//
// A boot exists only when boot.mjs ran, so bots that never execute JavaScript
// (curl, python, most scrapers) are invisible here. Cloudflare sees those.
// The visit tracker's automated count and this one are different instruments.
import { visitProperty, automatedVisit, visitReferrer } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { bootPiece } from "./piece-landings.mjs";

// Our own render flags, as visit-model knows them. These are fleet traffic.
export const OWN_MARKERS = Object.freeze(["social-preview", "offline-render", "jev-vs-jev", "ac-automation"]);

// Most specific first. The family is what the report shows; the raw UA is
// kept only as one truncated example per family so an unknown bot can be named.
const FAMILIES = [
  ["Googlebot", /Googlebot|Google-InspectionTool|Storebot-Google|AdsBot-Google|Mediapartners-Google/i],
  ["GoogleOther", /GoogleOther/i],
  ["Bingbot", /bingbot|BingPreview|msnbot/i],
  ["Applebot", /Applebot/i],
  ["DuckDuckBot", /DuckDuckBot|DuckDuckGo/i],
  ["YandexBot", /Yandex/i],
  ["Baiduspider", /Baiduspider/i],
  ["PetalBot", /PetalBot/i],
  ["GPTBot", /GPTBot|ChatGPT-User|OAI-SearchBot/i],
  ["ClaudeBot", /ClaudeBot|Claude-User|Claude-SearchBot|anthropic-ai/i],
  ["PerplexityBot", /PerplexityBot|Perplexity-User/i],
  ["Bytespider", /Bytespider|TikTokSpider/i],
  ["CCBot", /CCBot/i],
  ["Amazonbot", /Amazonbot/i],
  ["Meta crawler", /meta-externalagent|facebookexternalhit|FacebookBot|facebookcatalog/i],
  ["Twitterbot", /Twitterbot/i],
  ["Slackbot", /Slackbot|Slack-ImgProxy/i],
  ["Discordbot", /Discordbot/i],
  ["LinkedInBot", /LinkedInBot/i],
  ["TelegramBot", /TelegramBot/i],
  ["WhatsApp", /WhatsApp/i],
  ["Pinterest", /Pinterestbot|Pinterest\//i],
  ["Redditbot", /redditbot/i],
  ["Link unfurler", /Iframely|Embedly|Mastodon|Bluesky Cardyb|Synapse|Akkoma|Pleroma|Misskey/i],
  ["Uptime monitor", /UptimeRobot|Pingdom|StatusCake|Site24x7|Better ?Uptime|Freshping|monitor/i],
  ["SEO crawler", /AhrefsBot|SemrushBot|MJ12bot|DotBot|DataForSeoBot|Screaming Frog|SiteAuditBot|BLEXBot/i],
  ["Lighthouse", /Lighthouse|Chrome-Lighthouse|PageSpeed/i],
  ["Headless Chrome", /HeadlessChrome/i],
  ["Puppeteer/Playwright", /Puppeteer|Playwright/i],
  ["curl", /\bcurl\//i],
  ["wget", /\bWget\//i],
  ["Python client", /python-requests|python-urllib|aiohttp|httpx|Scrapy|python/i],
  ["Go client", /Go-http-client/i],
  ["Node client", /node-fetch|undici|axios/i],
  ["Java client", /Java\/|okhttp|Apache-HttpClient/i],
];

// A family name for an automated boot. Our own markers win, then named bots,
// then whatever word carried "bot", "crawler" or "spider" in the UA.
export function botFamily(userAgent = "", search = "") {
  const params = new URLSearchParams(search || "");
  const own = OWN_MARKERS.find(key => params.has(key));
  if (own) return `own: ${own}`;
  const ua = String(userAgent || "");
  for (const [name, pattern] of FAMILIES) if (pattern.test(ua)) return name;
  const generic = ua.match(/([A-Za-z0-9_.-]*(?:bot|crawler|spider|webindexer)[A-Za-z0-9_.-]*)/i);
  if (generic) return generic[1];
  if (!ua.trim()) return "empty user agent";
  return "WebDriver or unlabeled";
}

// Browser family without its version, from boot.mjs's parsed "Chrome 152".
export function browserFamily(browser, userAgent = "") {
  if (browser) return String(browser).replace(/\s+\d+$/, "");
  const ua = String(userAgent || "");
  if (/\b(iPhone|iPad)\b/.test(ua) && /AppleWebKit/.test(ua)) return "iOS WebView";
  if (/\bwv\b|; wv\)/.test(ua)) return "Android WebView";
  if (/Electron/.test(ua)) return "Electron";
  return "unknown";
}

const bump = (map, key, by = 1) => { if (key !== null && key !== undefined && key !== "") map[key] = (map[key] || 0) + by; };
const rank = (map, limit) => Object.entries(map).sort((a, b) => b[1] - a[1]).slice(0, limit).map(([key, n]) => ({ key, n }));

// Fold boots in a window into the three audiences. `properties` is the
// scope's reviewed site list; everything else is skipped and counted.
export function foldBootAudience(boots, { properties, limit = 12 } = {}) {
  const side = () => ({ boots: 0, completed: 0, byProperty: {}, byCountry: {}, byPiece: {}, byReferrer: {} });
  const humans = { ...side(), signedIn: 0, anonymous: 0, mobile: 0, byBrowser: {}, byLanguage: {} };
  const automated = { ...side(), byFamily: {}, examples: {} };
  const walls = { boots: 0, byReferrer: {}, byProperty: {}, byTimezone: {} };
  let skipped = 0, localDev = 0, packMode = 0;
  for (const { meta = {}, status, server = {} } of boots) {
    const property = visitProperty(meta.host);
    if (!property || (properties && !properties.includes(property))) { skipped++; continue; }
    if (meta.localDev) { localDev++; continue; }
    if (meta.packMode) { packMode++; continue; }
    if (meta.embedded) {
      walls.boots++;
      bump(walls.byProperty, property);
      bump(walls.byReferrer, visitReferrer(meta.referrer) || "(none)");
      bump(walls.byTimezone, meta.timezone || "(unknown)");
      continue;
    }
    const bot = automatedVisit({ userAgent: meta.userAgent, webdriver: meta.webdriver }, meta.search || "", meta.automation);
    const row = bot ? automated : humans;
    row.boots++;
    if (status === "success") row.completed++;
    bump(row.byProperty, property);
    bump(row.byCountry, server.country || "(unknown)");
    const piece = bootPiece(meta.path);
    bump(row.byPiece, piece === null ? "(private)" : piece);
    bump(row.byReferrer, visitReferrer(meta.referrer) || "(direct or unknown)");
    if (bot) {
      const family = botFamily(meta.userAgent, meta.search);
      bump(automated.byFamily, family);
      if (!automated.examples[family]) automated.examples[family] = String(meta.userAgent || "").slice(0, 160);
    } else {
      meta.user ? humans.signedIn++ : humans.anonymous++;
      if (meta.mobile) humans.mobile++;
      bump(humans.byBrowser, browserFamily(meta.browser, meta.userAgent));
      bump(humans.byLanguage, meta.language ? String(meta.language).slice(0, 12) : "(unknown)");
    }
  }
  const finish = (row, keys) => Object.fromEntries(Object.entries(row).map(([key, value]) =>
    [key, keys.includes(key) ? rank(value, limit) : value]));
  const ranked = ["byProperty", "byCountry", "byPiece", "byReferrer", "byBrowser", "byLanguage", "byFamily", "byTimezone"];
  const out = {
    humans: finish(humans, ranked),
    automated: finish(automated, ranked),
    walls: finish(walls, ranked),
    excluded: { outsideScope: skipped, localDev, packMode },
  };
  out.automated.examples = Object.fromEntries(out.automated.byFamily.map(({ key }) => [key, automated.examples[key]]));
  const total = humans.boots + automated.boots;
  out.automatedShare = total ? Math.round((automated.boots / total) * 1000) / 10 : null;
  return out;
}

// Signed-in usage per public handle from the same boots: how often each
// account loaded a page, what they landed on, and where from. Boots are page
// loads, so one person keeping a tab open all day is one boot.
export function foldHandleUsage(boots, { properties, limit = 50, pieces = 6 } = {}) {
  const rows = new Map();
  let anonymous = 0;
  for (const { meta = {}, status, createdAt } of boots) {
    const property = visitProperty(meta.host);
    if (!property || (properties && !properties.includes(property)) || meta.localDev || meta.packMode || meta.embedded) continue;
    if (automatedVisit({ userAgent: meta.userAgent, webdriver: meta.webdriver }, meta.search || "", meta.automation)) continue;
    const handle = meta.user?.handle;
    if (!handle) { anonymous++; continue; }
    const key = `@${String(handle).replace(/^@/, "")}`;
    const row = rows.get(key) || { handle: key, boots: 0, completed: 0, mobile: 0, properties: {}, pieces: {}, browsers: {}, firstSeen: null, lastSeen: null };
    row.boots++;
    if (status === "success") row.completed++;
    if (meta.mobile) row.mobile++;
    bump(row.properties, property);
    const piece = bootPiece(meta.path);
    bump(row.pieces, piece === null ? "(private)" : piece);
    bump(row.browsers, browserFamily(meta.browser, meta.userAgent));
    const at = createdAt ? new Date(createdAt) : null;
    if (at && !Number.isNaN(+at)) {
      if (!row.firstSeen || at < row.firstSeen) row.firstSeen = at;
      if (!row.lastSeen || at > row.lastSeen) row.lastSeen = at;
    }
    rows.set(key, row);
  }
  const ranked = [...rows.values()].sort((a, b) => b.boots - a.boots);
  return {
    handles: ranked.length, anonymousBoots: anonymous, truncated: ranked.length > limit,
    rows: ranked.slice(0, limit).map(row => ({ ...row,
      properties: rank(row.properties, pieces), pieces: rank(row.pieces, pieces), browsers: rank(row.browsers, 3) })),
  };
}
