#!/usr/bin/env node
// analytics-mcp.mjs — network visits, direct DMG downloads and App Store downloads as tools.
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
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import zlib from "node:zlib";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const pexec = promisify(execFile);
const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");
const VAULT = join(ROOT, "aesthetic-computer-vault");
const LITH = "root@lith.aesthetic.computer";
const SSH_KEY = join(VAULT, "home/.ssh/id_rsa");

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
const ACTIONS = ["download_clicked", "media_started", "link_followed", "canvas_interacted",
  "round_started", "round_completed", "match_completed", "mime_interact", "mime_scroll_feed", "mime_original_open"];

// 🌐 Visits

async function visitsReport({ hours = 48, scope = "all", end } = {}) {
  hours = Number(hours);
  if (!Number.isFinite(hours) || hours <= 0 || hours > 35 * 24) throw new Error("hours must be 1..840");
  if (!["studio", "clients", "all"].includes(scope)) throw new Error("scope must be studio, clients or all");
  if (end !== undefined && !/^\d{4}-\d\d-\d\dT[\d:.]+Z$/.test(end)) throw new Error("end must be an ISO UTC date");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/visits-report.mjs --hours ${hours} --scope ${scope}${end ? ` --end ${end}` : ""}`;
  const { stdout } = await pexec("ssh", ["-i", SSH_KEY, "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  const report = JSON.parse(stdout.slice(stdout.indexOf("{"))); // database.mjs logs a line first

  const sites = {};
  for (const row of report.audience) {
    const site = sites[row._id.property] ??= { group: row.group, visits: 0, interacted: 0, engaged: 0, visibleMinutes: 0, surfaces: {}, actions: {} };
    site.visits += row.visits;
    site.interacted += row.interacted;
    site.engaged += row.engaged;
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
      automated: Object.values(automated).reduce((n, v) => n + v, 0) },
    sites: Object.fromEntries(Object.entries(sites).sort((a, b) => b[1].visits - a[1].visits)),
    automatedBySite: automated,
    perDay: days,
  };
}

// ⬇️ Direct downloads

// The Slab and Aesel DMGs go through lith's /api/download counter. Rows keep
// only a keyed hash of the address, so leaving the fleet out means asking
// lith for this machine's own hash and passing it (and any others) along.
async function directDownloads({ days = 30, exclude = [], excludeSelf = true } = {}) {
  days = Number(days);
  if (!Number.isFinite(days) || days <= 0 || days > 3650) throw new Error("days must be 1..3650");
  const hashes = [...exclude];
  if (excludeSelf) hashes.push((await (await fetch("https://aesthetic.computer/api/download?whoami=1")).json()).hash);
  if (hashes.some((h) => !/^[a-f0-9]{16}$/.test(h))) throw new Error("exclude takes 16-hex hashes from /api/download?whoami=1");
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/downloads-report.mjs --days ${days}${hashes.length ? ` --exclude ${hashes.join(",")}` : ""}`;
  const { stdout } = await pexec("ssh", ["-i", SSH_KEY, "-o", "ConnectTimeout=10", LITH, remote],
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

// 📱 App Store

let ascKey;
async function ascToken() {
  ascKey ??= (await pexec("gpg", ["--batch", "--quiet", "--decrypt", join(VAULT, `apple/AuthKey_${ASC_KEY_ID}.p8.gpg`)])).stdout;
  const b64 = (o) => Buffer.from(JSON.stringify(o)).toString("base64url");
  const now = Math.floor(Date.now() / 1000);
  const input = `${b64({ alg: "ES256", kid: ASC_KEY_ID, typ: "JWT" })}.${b64({ iss: ASC_ISSUER, iat: now, exp: now + 1200, aud: "appstoreconnect-v1" })}`;
  const sig = crypto.sign("sha256", Buffer.from(input), { key: ascKey, dsaEncoding: "ieee-p1363" });
  return `${input}.${sig.toString("base64url")}`;
}

async function asc(path) {
  const res = await fetch(`${ASC_API}${path}`, { headers: { Authorization: `Bearer ${await ascToken()}` } });
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
            const gz = Buffer.from(await (await fetch(segment.attributes.url)).arrayBuffer());
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
    : name === "direct_downloads" ? await directDownloads(args)
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
