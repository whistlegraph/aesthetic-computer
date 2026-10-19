// google-search — Search Console and Google Analytics for the network report.
//
// One Google Cloud service account in AC's own project (aesthetic-computer),
// the key encrypted in the vault at dark-window/gcp-service-key.json.gpg and
// decrypted into memory only, never written. It is a restricted user on each
// Search Console property and a Viewer on the Analytics property, so these
// reads are all it can do here. Read-only scopes; no SDK.
//
//   searchConsole({ site, days, dimension, limit })  clicks, impressions, CTR, position
//   sitemaps({ site })                                  submission and error state
//   analytics({ days, limit })                           GA4 sessions by source / medium / landing page
import crypto from "node:crypto";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { vaultPath } from "../vault/credentials.mjs";

const pexec = promisify(execFile);
export const KEY_FILE = "dark-window/gcp-service-key.json.gpg";
export const GA4_PROPERTY = "394537035"; // "aesthetic-computer", web stream aesthetic.computer
export const SEARCH_SITES = ["aesthetic.computer", "sotce.net", "laklok.com", "oskiewar.com", "prompt.ac",
  "whistlegraph.org", "mime.ac", "jas.life", "nopaint.art", "notepaint.art"];
const SCOPES = "https://www.googleapis.com/auth/webmasters.readonly https://www.googleapis.com/auth/analytics.readonly";

// A signed JWT assertion for the token endpoint (RFC 7523).
export function assertion(key, { now = Math.floor(Date.now() / 1000), scope = SCOPES } = {}) {
  const part = (value) => Buffer.from(JSON.stringify(value)).toString("base64url");
  const input = `${part({ alg: "RS256", typ: "JWT" })}.${part({ iss: key.client_email, scope, aud: "https://oauth2.googleapis.com/token", iat: now, exp: now + 3600 })}`;
  return `${input}.${crypto.sign("RSA-SHA256", Buffer.from(input), key.private_key).toString("base64url")}`;
}

let cached = { token: null, until: 0 };
let keyReader = null; // tests swap in a generated key instead of the vault's
export function useKeyReader(reader) { keyReader = reader; cached = { token: null, until: 0 }; }
export async function token({ fetch = globalThis.fetch, readKey = keyReader } = {}) {
  if (cached.token && cached.until > Date.now()) return cached.token;
  const key = readKey ? await readKey()
    : JSON.parse((await pexec("gpg", ["--batch", "--quiet", "--decrypt", vaultPath(KEY_FILE)], { maxBuffer: 1 << 20 })).stdout);
  const res = await fetch("https://oauth2.googleapis.com/token", { method: "POST",
    headers: { "content-type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({ grant_type: "urn:ietf:params:oauth:grant-type:jwt-bearer", assertion: assertion(key) }) });
  const body = await res.json();
  if (!res.ok || !body.access_token) throw new Error(`Google token: ${body.error_description || body.error || res.status}`);
  cached = { token: body.access_token, until: Date.now() + (body.expires_in - 120) * 1000 };
  return cached.token;
}

async function google(url, { method = "GET", body, fetch = globalThis.fetch } = {}) {
  const res = await fetch(url, { method, headers: { Authorization: `Bearer ${await token({ fetch })}`, "content-type": "application/json" },
    ...(body ? { body: JSON.stringify(body) } : {}) });
  const json = await res.json().catch(() => ({}));
  if (!res.ok) throw new Error(`${res.status} ${json.error?.message?.split(". ")[0] || res.statusText}`);
  return json;
}

const day = (offset) => new Date(Date.now() - offset * 86400_000).toISOString().slice(0, 10);
const round = (n, d = 1) => Math.round(n * 10 ** d) / 10 ** d;

// Search data lags about two days, so the window ends two days back.
export async function searchConsole({ site, days = 7, dimension = "query", limit = 25, fetch } = {}) {
  if (!SEARCH_SITES.includes(site)) throw new Error(`site must be one of ${SEARCH_SITES.join(", ")}`);
  if (!["query", "page", "country", "device", "date"].includes(dimension)) throw new Error("dimension must be query, page, country, device or date");
  if (!Number.isInteger(days) || days < 1 || days > 480) throw new Error("days must be 1..480");
  const startDate = day(days + 2), endDate = day(2), url = `https://www.googleapis.com/webmasters/v3/sites/${encodeURIComponent(`sc-domain:${site}`)}/searchAnalytics/query`;
  const [rows, totals] = await Promise.all([
    google(url, { method: "POST", fetch, body: { startDate, endDate, dimensions: [dimension], rowLimit: Math.min(limit, 1000) } }),
    google(url, { method: "POST", fetch, body: { startDate, endDate } }),
  ]);
  const shape = (r) => ({ clicks: r.clicks, impressions: r.impressions, ctr: round((r.ctr || 0) * 100, 2), position: round(r.position || 0) });
  return { site, startDate, endDate, note: "Search Console data lags about two days; ctr is a percentage.",
    totals: totals.rows?.[0] ? shape(totals.rows[0]) : { clicks: 0, impressions: 0, ctr: 0, position: 0 },
    [dimension]: (rows.rows || []).map((r) => ({ key: r.keys[0], ...shape(r) })) };
}

export async function sitemaps({ site, fetch } = {}) {
  if (!SEARCH_SITES.includes(site)) throw new Error(`site must be one of ${SEARCH_SITES.join(", ")}`);
  const body = await google(`https://www.googleapis.com/webmasters/v3/sites/${encodeURIComponent(`sc-domain:${site}`)}/sitemaps`, { fetch });
  return (body.sitemap || []).map((s) => ({ path: s.path, lastSubmitted: s.lastSubmitted, lastDownloaded: s.lastDownloaded || null,
    pending: s.isPending, errors: Number(s.errors || 0), warnings: Number(s.warnings || 0),
    submitted: (s.contents || []).reduce((n, c) => n + Number(c.submitted || 0), 0) }));
}

export async function analytics({ days = 7, limit = 15, fetch } = {}) {
  if (!Number.isInteger(days) || days < 1 || days > 90) throw new Error("days must be 1..90");
  const url = `https://analyticsdata.googleapis.com/v1beta/properties/${GA4_PROPERTY}:runReport`;
  const range = [{ startDate: `${days}daysAgo`, endDate: "today" }];
  const metrics = [{ name: "sessions" }, { name: "engagedSessions" }, { name: "activeUsers" }];
  const report = (dimensions) => google(url, { method: "POST", fetch, body: { dateRanges: range, dimensions: dimensions.map((name) => ({ name })), metrics,
    orderBys: [{ metric: { metricName: "sessions" }, desc: true }], limit } });
  const fold = (r) => (r.rows || []).map((row) => ({ key: row.dimensionValues.map((d) => d.value).join(" / "),
    sessions: Number(row.metricValues[0].value), engaged: Number(row.metricValues[1].value), users: Number(row.metricValues[2].value) }));
  const [sources, landings, totals] = await Promise.all([report(["sessionSource", "sessionMedium"]), report(["landingPage"]),
    google(url, { method: "POST", fetch, body: { dateRanges: range, metrics } })]);
  const t = totals.rows?.[0]?.metricValues || [];
  return { property: GA4_PROPERTY, days, note: "Google Analytics 4 (aesthetic.computer web stream). Users are GA's estimate, unlike AC's first-party visits.",
    totals: { sessions: Number(t[0]?.value || 0), engaged: Number(t[1]?.value || 0), users: Number(t[2]?.value || 0) },
    bySourceMedium: fold(sources), byLandingPage: fold(landings) };
}

// The network report's search section: totals and top queries for every site.
export async function searchSummary({ days = 7, perSite = 5, fetch } = {}) {
  const rows = await Promise.all(SEARCH_SITES.map(async (site) => {
    try { const r = await searchConsole({ site, days, limit: perSite, fetch }); return { site, ...r.totals, topQueries: r.query }; }
    catch (error) { return { site, unavailable: String(error.message).slice(0, 160) }; }
  }));
  return { days, note: "Search Console, ending two days back.", sites: rows.sort((a, b) => (b.impressions || 0) - (a.impressions || 0)) };
}
