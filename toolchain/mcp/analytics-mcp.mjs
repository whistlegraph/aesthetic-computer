#!/usr/bin/env node
// analytics-mcp.mjs — network visits, Braincells, direct DMG downloads, app opens and App Store downloads as tools.
//
// visits_report runs toolchain/analytics/visits-report.mjs on lith over ssh
// (see toolchain/analytics/VISITS.md for what a "visit" can and cannot mean)
// and folds its rows per property. app_downloads reads Apple's ONGOING
// "App Downloads Standard" analytics report for each app. Both are read-only.
//
// Credentials come from the vault checkout: the lith ssh key, and the App
// Store Connect key decrypted with gpg into memory (never written to disk).

import { execFile } from "node:child_process";
import crypto from "node:crypto";
import { promisify } from "node:util";
import zlib from "node:zlib";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { fetchRetry } from "./fetch-retry.mjs";
import { VISIT_ACTIONS, VISIT_DEPTHS, visitProperty, visitScopeMatch } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { fisheryOptions } from "../analytics/human-fishery.mjs";
import { recognitionReportOptions } from "../analytics/network-recognition.mjs";
import { vaultPath } from "../vault/credentials.mjs";
import { tezosRevenue } from "../analytics/tezos-revenue.mjs";
import { socialReport } from "../analytics/social-report.mjs";
import { fileURLToPath } from "node:url";

const REPO_ROOT = fileURLToPath(new URL("../..", import.meta.url));

const pexec = promisify(execFile);
const LITH = "root@lith.aesthetic.computer";
const sshKey = () => vaultPath("home/.ssh/id_rsa");

const ASC_KEY_ID = "S4TQKG6U99";
const ASC_ISSUER = "69a6de78-fa3c-47e3-e053-5b8c7c11a4d1";
const ASC_API = "https://api.appstoreconnect.apple.com";
const APPS = {
  menuband: "6767311903",
  oskiewar: "6802477398",
  nopaint: "1107427275",
  fingerquilt: "1153451161",
  softwallpaper: "1390237091",
  aestheticcomputer: "6450940883",
  aesel: "6812823093",
};
const ACTIONS = VISIT_ACTIONS;

async function humanFishery(args = {}) {
  fisheryOptions(args); // Validate before making an SSH call.
  const { minutes = 5, scope = "studio", limit = 50, startedAfter } = args;
  const quoted = "'" + JSON.stringify({ minutes, scope, limit, startedAfter }).replaceAll("'", "'\\''") + "'";
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/human-fishery-report.mjs ${quoted}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 30000, maxBuffer: 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{")));
}

async function networkRecognition(args = {}) {
  recognitionReportOptions(args);
  const quoted = "'" + JSON.stringify({ hours: args.hours ?? 24 }).replaceAll("'", "'\\''") + "'";
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/network-recognition-report.mjs ${quoted}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 30000, maxBuffer: 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{")));
}

async function journeyReport(mode, args = {}) {
  const { hours = 24, limit = mode === "features" ? 30 : 100, scope = "studio", handle, property } = args;
  if (!Number.isFinite(hours) || hours <= 0 || hours > 840 || !Number.isInteger(limit) || limit < 1 || limit > (mode === "features" ? 50 : 500) ||
      !["studio", "clients", "all"].includes(scope) || (handle !== undefined && !/^@?[a-z0-9_-]{1,64}$/i.test(handle))) throw new Error("Invalid report options");
  if (property !== undefined && !visitScopeMatch(scope).property.$in.includes(visitProperty(property))) throw new Error("Property is outside the selected scope");
  const quoted = "'" + JSON.stringify({ hours, limit, scope, handle, property }).replaceAll("'", "'\\''") + "'";
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/journey-report.mjs ${mode} ${quoted}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 30000, maxBuffer: 2 * 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{")));
}

// 🌐 Visits

async function visitsReport({ hours = 48, scope = "all", end } = {}) {
  hours = Number(hours);
  if (!Number.isFinite(hours) || hours <= 0 || hours > 35 * 24) throw new Error("hours must be 1..840");
  if (!["studio", "clients", "all"].includes(scope)) throw new Error("scope must be studio, clients or all");
  if (end !== undefined && !/^\d{4}-\d\d-\d\dT[\d:.]+Z$/.test(end)) throw new Error("end must be an ISO UTC date");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/visits-report.mjs --hours ${hours} --scope ${scope}${end ? ` --end ${end}` : ""}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  const report = JSON.parse(stdout.slice(stdout.indexOf("{"))); // database.mjs logs a line first

  const sites = {};
  for (const row of report.audience) {
    const site = sites[row._id.property] ??= { group: row.group, visits: 0, interacted: 0, engaged: 0, actionVisits: 0, depth: {}, visibleMinutes: 0, surfaces: {}, actions: {} };
    site.visits += row.visits;
    site.interacted += row.interacted;
    site.engaged += row.engaged;
    site.actionVisits += row.actionVisits || 0;
    for (const seconds of VISIT_DEPTHS) site.depth[seconds] = (site.depth[seconds] || 0) + (row[`interacted${seconds}`] || 0);
    site.visibleMinutes += row.activeSecondsLowerBound / 60;
    site.surfaces[row._id.surface] = (site.surfaces[row._id.surface] || 0) + row.visits;
    for (const a of ACTIONS) if (row[a]) site.actions[a] = (site.actions[a] || 0) + row[a];
  }
  for (const site of Object.values(sites)) site.visibleMinutes = Math.round(site.visibleMinutes);
  const automated = {};
  for (const row of report.automation) automated[row._id.property] = (automated[row._id.property] || 0) + row.visits;
  const days = {};
  for (const row of report.periods) {
    if (row._id.automated) continue;
    const day = days[row.start] ??= { visits: 0, interacted: 0, engaged: 0 };
    day.visits += row.visits; day.interacted += row.interacted; day.engaged += row.engaged;
  }
  const sum = (key) => Object.values(sites).reduce((n, s) => n + s[key], 0);
  return {
    window: { start: report.start, end: report.end, scope, earliestRetainedVisit: report.earliestRetainedVisit },
    unit: report.unit,
    totals: { visits: sum("visits"), interacted: sum("interacted"), engaged: sum("engaged"),
      actionVisits: sum("actionVisits"),
      depth: Object.fromEntries(VISIT_DEPTHS.map(seconds => [seconds, Object.values(sites).reduce((n, site) => n + site.depth[seconds], 0)])),
      automated: Object.values(automated).reduce((n, v) => n + v, 0) },
    sites: Object.fromEntries(Object.entries(sites).sort((a, b) => b[1].visits - a[1].visits)),
    automatedBySite: automated,
    perDay: days,
  };
}

// 🧠 Braincells

// Circulation and value, spend by model and handle, purchases and AC's
// OpenRouter balance, folded on lith by toolchain/analytics/braincells-report.mjs.
async function braincellsReport({ hours = 24, limit = 25 } = {}) {
  hours = Number(hours); limit = Number(limit);
  if (!Number.isFinite(hours) || hours <= 0 || hours > 840) throw new Error("hours must be 1..840");
  if (!Number.isInteger(limit) || limit < 1 || limit > 200) throw new Error("limit must be 1..200");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/braincells-report.mjs --hours ${hours} --limit ${limit}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 8 * 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{")));
}

// 🛰️ The whole network at once: traffic per site, where it came from, and the
// Braincell economy. This is the report to run when someone asks how the
// Aesthetic network is doing. Each part fails on its own without sinking the rest.
const checkedHours = (hours, max = 840) => {
  hours = Number(hours);
  if (!Number.isFinite(hours) || hours <= 0 || hours > max) throw new Error(`hours must be 1..${max}`);
  return hours;
};

async function networkReport({ hours = 24, scope = "all" } = {}) {
  const part = promise => promise.then(value => value, error => ({ unavailable: String(error.message || error) }));
  const [visits, referrers, pieces, braincells, tezos, social] = await Promise.all([
    part(visitsReport({ hours, scope })),
    part(journeyReport("referrers", { hours, scope, limit: 25 })),
    part(journeyReport("pieces", { hours, scope, limit: 25 })),
    part(braincellsReport({ hours, limit: 15 })),
    part(tezosRevenue({ hours: checkedHours(hours) })),
    part(socialReport({ root: REPO_ROOT, hours: checkedHours(hours, 2160) })),
  ]);
  return { window: visits.window ?? { hours, scope }, visits, referrers, pieces, braincells, tezos, social };
}

// ⬇️ Direct downloads

// The Slab and Aesel DMGs go through lith's /api/download counter. Rows keep
// only a keyed hash of the address, so leaving the fleet out means asking
// lith for this machine's own hash and passing it (and any others) along.
async function directDownloads({ days = 30, exclude = [], excludeSelf = true } = {}) {
  days = Number(days);
  if (!Number.isFinite(days) || days <= 0 || days > 3650) throw new Error("days must be 1..3650");
  const hashes = [...exclude];
  if (excludeSelf) hashes.push((await (await fetchRetry("https://aesthetic.computer/api/download?whoami=1")).json()).hash);
  if (hashes.some((h) => !/^[a-f0-9]{16}$/.test(h))) throw new Error("exclude takes 16-hex hashes from /api/download?whoami=1");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/downloads-report.mjs --days ${days}${hashes.length ? ` --exclude ${hashes.join(",")}` : ""}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  const report = JSON.parse(stdout.slice(stdout.indexOf("{")));
  const apps = {};
  for (const row of report.rows) {
    const app = apps[row._id.app] ??= { downloads: 0, places: 0, self: 0, automated: 0, byVersion: {}, countries: {}, platforms: {} };
    if (row._id.automated) { app.automated += row.downloads; continue; }
    if (row._id.self) { app.self += row.downloads; continue; }
    app.downloads += row.downloads;
    app.places += row.places;
    app.byVersion[row._id.version] = (app.byVersion[row._id.version] || 0) + row.downloads;
    for (const [k, v] of Object.entries(row.countries)) app.countries[k] = (app.countries[k] || 0) + v;
    for (const [k, v] of Object.entries(row.platforms)) app.platforms[k] = (app.platforms[k] || 0) + v;
  }
  return { window: { start: report.start, end: report.end, earliestDownload: report.earliestDownload },
    note: "downloads/places leave out automated and excluded (self) traffic; places = distinct address hashes per version, summed", apps };
}

// 🚀 App opens

// Native apps post a launch to lith's /api/app-open with a random install id
// that lives only as long as the app is installed. Rows fold per app and day.
async function appOpens({ days = 7 } = {}) {
  days = Number(days);
  if (!Number.isFinite(days) || days <= 0 || days > 35) throw new Error("days must be 1..35");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/opens-report.mjs --days ${days}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  const report = JSON.parse(stdout.slice(stdout.indexOf("{")));
  const apps = {};
  for (const row of report.rows) {
    const app = apps[row._id.app] ??= { opens: 0, fresh: 0, daily: {}, platforms: {}, versions: {} };
    app.opens += row.opens;
    app.fresh += row.fresh;
    app.daily[row._id.day] = { active: row.active, opens: row.opens, fresh: row.fresh };
    for (const [k, v] of Object.entries(row.platforms)) app.platforms[k] = (app.platforms[k] || 0) + v;
    for (const [k, v] of Object.entries(row.versions)) app.versions[k] = (app.versions[k] || 0) + v;
  }
  return { since: report.start, note: report.unit, apps };
}

async function iosUsage({ days = 7 } = {}) {
  days = Number(days);
  if (!Number.isInteger(days) || days < 1 || days > 35) throw new Error("days must be 1..35");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/ios-usage-report.mjs --days ${days}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{")));
}

// 📅 Daily

// lith folds each finished day of visits, direct downloads and app opens into
// `metrics-daily`; Apple's numbers join here, by the same day.
async function dailyMetrics({ days = 14, appStore = true } = {}) {
  days = Number(days);
  if (!Number.isFinite(days) || days <= 0 || days > 3650) throw new Error("days must be 1..3650");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/daily-report.mjs --days ${days}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  const report = JSON.parse(stdout.slice(stdout.indexOf("{")));
  const out = Object.fromEntries(report.rows.map(({ _id, generatedAt, day, ...row }) => [_id, row]));
  if (appStore) {
    const apple = await appDownloads({ days: Math.min(days, 60) });
    for (const [app, data] of Object.entries(apple.apps)) {
      for (const [day, kinds] of Object.entries(data.daily || {})) {
        ((out[day] ??= {}).appStore ??= {})[app] = kinds;
      }
    }
  }
  return { since: report.start, note: "Days are UTC. visits = non-automated page visits; downloads leave out automated only (fleet self-downloads included); opens.active = installs opened that day; appStore lags ~1 day.",
    days: Object.fromEntries(Object.entries(out).sort()) };
}

// 📱 App Store

let ascKey;
async function ascToken() {
  ascKey ??= (await pexec("gpg", ["--batch", "--quiet", "--decrypt", vaultPath(`apple/AuthKey_${ASC_KEY_ID}.p8.gpg`)])).stdout;
  const b64 = (o) => Buffer.from(JSON.stringify(o)).toString("base64url");
  const now = Math.floor(Date.now() / 1000);
  const input = `${b64({ alg: "ES256", kid: ASC_KEY_ID, typ: "JWT" })}.${b64({ iss: ASC_ISSUER, iat: now, exp: now + 1200, aud: "appstoreconnect-v1" })}`;
  const sig = crypto.sign("sha256", Buffer.from(input), { key: ascKey, dsaEncoding: "ieee-p1363" });
  return `${input}.${sig.toString("base64url")}`;
}

async function asc(path) {
  const res = await fetchRetry(`${ASC_API}${path}`, { headers: { Authorization: `Bearer ${await ascToken()}` } });
  const body = await res.json();
  if (!res.ok) throw new Error(`${res.status} ${JSON.stringify(body.errors ?? body)}`);
  return body;
}

// Instances restate earlier dates, so for each row date only the instance
// with the latest processingDate counts. Days with no downloads have no rows.
async function appDownloads({ days = 7, apps = Object.keys(APPS) } = {}) {
  days = Number(days);
  if (!Number.isFinite(days) || days < 1 || days > 60) throw new Error("days must be 1..60");
  const since = new Date(Date.now() - (days + 1) * 86400000).toISOString().slice(0, 10);
  const out = {};
  for (const name of apps) {
    const id = APPS[name];
    if (!id) throw new Error(`unknown app ${name}; known: ${Object.keys(APPS).join(", ")}`);
    const requests = (await asc(`/v1/apps/${id}/analyticsReportRequests`)).data
      .filter((r) => r.attributes.accessType === "ONGOING" && !r.attributes.stoppedDueToInactivity);
    if (!requests.length) { out[name] = { note: "no ONGOING analytics report request" }; continue; }
    const byDate = {}; // date -> { processed, counts }
    for (const request of requests) {
      const reports = await asc(`/v1/analyticsReportRequests/${request.id}/reports?filter[name]=${encodeURIComponent("App Downloads Standard")}`);
      for (const report of reports.data) {
        const instances = await asc(`/v1/analyticsReports/${report.id}/instances?filter[granularity]=DAILY&limit=200`);
        for (const instance of instances.data) {
          const processed = instance.attributes.processingDate;
          if (processed < since) continue;
          const segments = await asc(`/v1/analyticsReportInstances/${instance.id}/segments`);
          const counts = {};
          for (const segment of segments.data) {
            const gz = Buffer.from(await (await fetchRetry(segment.attributes.url)).arrayBuffer());
            const [head, ...lines] = zlib.gunzipSync(gz).toString().trim().split("\n");
            const cols = head.split("\t");
            for (const line of lines) {
              const row = Object.fromEntries(line.split("\t").map((v, k) => [cols[k], v]));
              const day = counts[row.Date] ??= {};
              const key = `${row["Download Type"]} (${row.Device ?? row.Platform ?? "?"})`;
              day[key] = (day[key] || 0) + Number(row.Counts || 0);
            }
          }
          for (const [date, tally] of Object.entries(counts)) {
            if (date < since) continue;
            if (!byDate[date] || byDate[date].processed < processed) byDate[date] = { processed, tally };
          }
        }
      }
    }
    const dates = Object.keys(byDate).sort();
    const firstTime = dates.reduce((n, d) => n + Object.entries(byDate[d].tally)
      .filter(([k]) => k.startsWith("First-time")).reduce((m, [, v]) => m + v, 0), 0);
    out[name] = { firstTimeDownloads: firstTime, daily: Object.fromEntries(dates.map((d) => [d, byDate[d].tally])) };
  }
  return { since, note: "Apple's data lags about a day; dates absent from `daily` had no downloads.", apps: out };
}

// 🔌 MCP

const TOOLS = [
  {
    name: "network_recognition",
    description: "Private aggregate AC-session recognition on Sotce: eligible visits checked, server-verified existing AC sessions, unknown results and bridge completion by UTC day. No handles, account IDs, unique-user counts or anonymous fishery joins. Unknown includes browser storage restrictions and must not be interpreted as logged out. Coverage begins only after deployment.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
    } },
  },
  ...["account_activity", "network_referrers", "feature_usage", "piece_landings"].map(name => ({
    name,
    description: name === "piece_landings"
      ? "Which pieces people land on, anonymous visitors included: page loads per property and piece from lith's boot telemetry, split anonymous / signed-in, completed loads and top referrer sites per piece. Parameters are stripped; private paths are excluded. Counts landings, not in-tab moves between pieces. Optional property filter."
      : name === "feature_usage"
      ? "Laer Klokken/laklok feature-use counts by authenticated account: ranked controls, per-account counts, UTC daily activity and supported features with no recorded use, plus visibleMinutes (signed-in minutes open in a visible tab, per account and per UTC day, from the presence rollout onward). Covers canvas and HTML clients from the feature rollout onward. Zero is not proof of non-use. Optional public handle/property filter; up to 35 days, at most 50 accounts. No chat content, page contents or link destinations."
      : name === "account_activity"
      ? "Follow server-verified authenticated accounts through public AC/Sotce activity: public handle, runtime session alias, piece opens and action milestones. Optional handle filter. Only records from the account-activity rollout onward; no inferred identities or retroactive joins to anonymous fish. Login does not prove human activity. Private SSH-backed read."
      : "Referral sites across the Aesthetic network: first-party visits plus separately labeled historical AC boot referrals. Domain only, no full referrer URLs. Null is direct-or-unavailable. Excludes known automation; visits and boots are separate instruments and must not be summed.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
      limit: { type: "integer", minimum: 1, maximum: name === "feature_usage" ? 50 : 500, description: name === "feature_usage" ? "Maximum accounts; default 30" : "Maximum rows; default 100" },
      scope: { type: "string", enum: ["studio", "clients", "all"], description: "Default studio" },
      property: { type: "string", description: "Optional reviewed site, e.g. sotce.net; must belong to selected scope" },
      ...(!["network_referrers", "piece_landings"].includes(name) ? { handle: { type: "string", description: "Optional public handle, with or without @" } } : {}),
    } },
  })),
  {
    name: "network_report",
    description: "The Aesthetic network report in one call: first-party visits per site (including shop.aesthetic.computer and notepaint.art), referral sites, landing pieces (anonymous and signed-in), the Braincell economy (circulation and dollar value, spend by model, handle and product, purchases, OpenRouter balance and usage), Tezos revenue (objkt sales, royalties, Keeps fees, buyers) and social accounts (Instagram reels with insights, Bluesky, X). Use this whenever asked about Aesthetic network traffic. Parts that fail report themselves as unavailable.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
      scope: { type: "string", enum: ["studio", "clients", "all"], description: "Visit scope; default all" },
    } },
  },
  {
    name: "tezos_revenue",
    description: "Tez that reached AC's Tezos wallets (aesthetic.tez, keeps.tez, kidlisp treasury) and the Keeps contract in the window, from public TzKT data: total in XTZ and USD, by source (objkt sales and royalties, teia, Keeps mint fees, direct payments including Braincell tez checkouts), by wallet, top senders and top buyers. Moves between AC's own wallets are excluded.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
    } },
  },
  {
    name: "social_report",
    description: "AC's social accounts: every Instagram account's followers and its posts in the window with likes, comments and Reel insights (views, reach, saves, shares), Bluesky @aesthetic.computer followers and recent posts with engagement, and X follower/post counts. Read-only, official APIs.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 2160, description: "Lookback hours for posts; default 24" },
    } },
  },
  {
    name: "braincells_report",
    description: "The Braincell economy: Braincells in circulation and their value at pack price ($5 per million), debt and in-flight holds, top holders; spend in the window by model, handle and product (per-request rows for Aesel/Whistlegraph hosted inference from the ledger rollout, No Paint moves, Whistlegraph speech), purchased versus free-allowance totals, estimated provider cost; Stripe, Apple and Tezos purchases; AC's OpenRouter balance and key usage. Names public handles. Read-only.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
      limit: { type: "integer", minimum: 1, maximum: 200, description: "Rows per ranking; default 25" },
    } },
  },
  {
    name: "human_fishery",
    description: "AC Human Fishery: watch recent likely-human activity through Silo's existing MongoDB firehose on Lith. Returns temporary fish names for non-automated visits with interaction, public property, broad surface, visible-time depth and action flags. Read-only snapshots; repeat after at least 15 seconds for changes. Does not identify people, link separate visits or infer cross-site journeys. lastReportedAt is the last changed snapshot, not proof someone is still online.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      minutes: { type: "number", minimum: 1, maximum: 60, description: "Look back this many minutes for reported activity; default 5" },
      scope: { type: "string", enum: ["studio", "clients", "all"], description: "Default studio" },
      limit: { type: "integer", minimum: 1, maximum: 200, description: "Maximum fish returned; default 50" },
      startedAfter: { type: "string", description: "Optional ISO date: only visits arriving after this time, useful for watching the first arrival after a deploy" },
    } },
  },
  {
    name: "ios_usage",
    description: "AC iOS 1.2+ foreground opens, unique active installs, first-observed installs, returning installs, active seconds, successful loads and canvas engagement. App Store downloads remain separate. Older builds have no coverage; no user/account identities are returned.",
    inputSchema: { type: "object", properties: { days: { type: "integer", description: "UTC days including today (default 7, max 35)" } } },
  },
  {
    name: "visits_report",
    description: "First-party page visits across AC web properties (from lith's network-visits collector), folded per site: visits, interacted, engaged (10s+), visible minutes, actions, automated traffic. Page visits, not unique people. Retention is 35 days; collection began 2026-09-23.",
    inputSchema: {
      type: "object",
      properties: {
        hours: { type: "number", description: "Window length in hours (default 48, max 840)" },
        scope: { type: "string", enum: ["studio", "clients", "all"], description: "Default all" },
        end: { type: "string", description: "Optional ISO UTC end, e.g. 2026-09-26T20:00:00Z" },
      },
    },
  },
  {
    name: "direct_downloads",
    description: "Direct DMG downloads of Slab and Aesel counted by lith's /api/download redirect (counting began 2026-09-28): per app downloads, distinct places, versions, countries, platforms. Leaves out bots and, by default, this machine's own network.",
    inputSchema: {
      type: "object",
      properties: {
        days: { type: "number", description: "How many days back (default 30)" },
        exclude: { type: "array", items: { type: "string" }, description: "More address hashes to leave out (from /api/download?whoami=1 on other fleet machines)" },
        excludeSelf: { type: "boolean", description: "Leave out this machine's own network (default true)" },
      },
    },
  },
  {
    name: "daily_metrics",
    description: "One row per UTC day across everything we measure: web visits per site, direct DMG downloads, native app opens (active installs, opens, fresh installs) from lith's metrics-daily rollup, plus Apple's App Store downloads by type. Kept indefinitely (counts only).",
    inputSchema: {
      type: "object",
      properties: {
        days: { type: "number", description: "How many days back (default 14)" },
        appStore: { type: "boolean", description: "Join Apple's App Store numbers (default true)" },
      },
    },
  },
  {
    name: "app_opens",
    description: "Native app launches counted by lith's /api/app-open (counting began 2026-09-28): per app and day, active installs, opens and fresh installs (first launch, which includes reinstalls). Install ids are random and die with the app.",
    inputSchema: {
      type: "object",
      properties: { days: { type: "number", description: "How many days back (default 7, max 35)" } },
    },
  },
  {
    name: "app_downloads",
    description: "App Store downloads per app from Apple's App Downloads Standard analytics report: first-time downloads plus a daily breakdown by download type and device.",
    inputSchema: {
      type: "object",
      properties: {
        days: { type: "number", description: "How many days back (default 7, max 60)" },
        apps: { type: "array", items: { type: "string", enum: Object.keys(APPS) }, description: "Default all" },
      },
    },
  },
];

async function callTool(name, args = {}) {
  const result = name === "visits_report" ? await visitsReport(args)
    : name === "braincells_report" ? await braincellsReport(args)
    : name === "network_report" ? await networkReport(args)
    : name === "tezos_revenue" ? await tezosRevenue({ hours: checkedHours(args.hours ?? 24) })
    : name === "social_report" ? await socialReport({ root: REPO_ROOT, hours: checkedHours(args.hours ?? 24, 2160) })
    : name === "account_activity" ? await journeyReport("accounts", args)
    : name === "feature_usage" ? await journeyReport("features", args)
    : name === "network_referrers" ? await journeyReport("referrers", args)
    : name === "piece_landings" ? await journeyReport("pieces", args)
    : name === "human_fishery" ? await humanFishery(args)
    : name === "network_recognition" ? await networkRecognition(args)
    : name === "direct_downloads" ? await directDownloads(args)
    : name === "daily_metrics" ? await dailyMetrics(args)
    : name === "app_opens" ? await appOpens(args)
    : name === "ios_usage" ? await iosUsage(args)
    : name === "app_downloads" ? await appDownloads(args)
    : (() => { throw new Error(`unknown tool ${name}`); })();
  return [{ type: "text", text: JSON.stringify(result, null, 2) }];
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: params?.protocolVersion || "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "analytics-mcp", version: "1.0.0" },
          },
        };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "📈 analytics-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📈 analytics-mcp started (visits_report, direct_downloads, app_downloads)" });
