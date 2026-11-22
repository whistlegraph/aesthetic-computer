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
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { fetchRetry } from "./fetch-retry.mjs";
import { VISIT_ACTIONS, VISIT_DEPTHS, visitProperty, visitScopeMatch } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { fisheryOptions } from "../analytics/human-fishery.mjs";
import { recognitionReportOptions } from "../analytics/network-recognition.mjs";
import { vaultPath } from "../vault/credentials.mjs";
import { tezosRevenue } from "../analytics/tezos-revenue.mjs";
import { socialReport } from "../analytics/social-report.mjs";
import { nopaintReportOptions, nopaintSummary } from "../analytics/nopaint.mjs";
import { searchConsole, sitemaps, analytics as googleAnalytics, searchSummary, SEARCH_SITES } from "../analytics/google-search.mjs";
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
    const site = sites[row._id.property] ??= { group: row.group, visits: 0, interacted: 0, engaged: 0, actionVisits: 0, depth: {}, visibleMinutes: 0, surfaces: {}, actions: {}, actionsBySurface: {} };
    site.visits += row.visits;
    site.interacted += row.interacted;
    site.engaged += row.engaged;
    site.actionVisits += row.actionVisits || 0;
    for (const seconds of VISIT_DEPTHS) site.depth[seconds] = (site.depth[seconds] || 0) + (row[`interacted${seconds}`] || 0);
    site.visibleMinutes += row.activeSecondsLowerBound / 60;
    site.surfaces[row._id.surface] = (site.surfaces[row._id.surface] || 0) + row.visits;
    for (const a of ACTIONS) if (row[a]) {
      site.actions[a] = (site.actions[a] || 0) + row[a];
      const here = site.actionsBySurface[row._id.surface] ??= {};
      here[a] = (here[a] || 0) + row[a];
    }
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
    aesel: report.aesel || [],
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

// 👣 Who came into rax (the world under the prompt) and how they moved, read on
// lith from the server's own walk log by rax/walks-cli.mjs.
async function raxWalks({ hours = 24, visit, points = 600 } = {}) {
  hours = Number(hours); points = Number(points);
  if (!Number.isFinite(hours) || hours <= 0 || hours > 840) throw new Error("hours must be 1..840");
  if (!Number.isInteger(points) || points < 2 || points > 5000) throw new Error("points must be 2..5000");
  if (visit !== undefined && !/^[a-f0-9]{8}$/.test(visit)) throw new Error("visit must be an 8-character visit id");
  const remote = `cd /opt/ac && node rax/walks-cli.mjs --json ${visit ? `--visit ${visit} --points ${points}` : `--hours ${hours}`}`;
  const { stdout } = await pexec("ssh", ["-i", sshKey(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 32 * 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{")));
}

// 🎨 No Paint

// Classic Construct saves, teacher notes, the sticker split and the notepaint
// funnel, folded on lith by toolchain/analytics/nopaint-report.mjs.
async function nopaintReport({ hours = 24, end } = {}) {
  hours = Number(hours);
  if (end !== undefined && !/^\d{4}-\d\d-\d\dT[\d:.]+Z$/.test(end)) throw new Error("end must be an ISO UTC date");
  nopaintReportOptions({ hours, end }); // Validate before making an SSH call.
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/nopaint-report.mjs --hours ${hours}${end ? ` --end ${end}` : ""}`;
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

// The report is as slow as its slowest part, so each part gets a deadline and
// comes back `unavailable` past it. `timings` (seconds) names the slow one.
// Healthy lith parts take 4 to 8 s when they all run at once, so the deadline
// sits well above that: it is for a stalled part, not a slow one.
const PART_DEADLINE_MS = 15_000;

// The parts by name. The jasellite timer builds the first set: lith, public
// APIs, and the social accounts whose env files are copied there. The second
// set decrypts vault files with the GPG key, which stays on the Macs.
const SNAPSHOT_PARTS = ["visits", "referrers", "pieces", "audience", "handles", "braincells", "nopaint", "tezos", "iosUsage", "social"];
const VAULT_PARTS = ["search", "googleAnalytics", "appStore", "review"];

// Apple's numbers lag a day and arrive sparse, so the app parts read at
// least a week regardless of the visit window.
const reportAppDays = hours => Math.min(Math.max(Math.ceil(hours / 24), 7), 35);

function reportPart(name, { hours, scope }) {
  const days = Math.max(1, Math.ceil(hours / 24));
  switch (name) {
    case "visits": return visitsReport({ hours, scope });
    case "referrers": return journeyReport("referrers", { hours, scope, limit: 25 });
    case "pieces": return journeyReport("pieces", { hours, scope, limit: 25 });
    case "audience": return journeyReport("audience", { hours, scope, limit: 12 });
    case "handles": return journeyReport("handles", { hours, scope, limit: 25 });
    case "braincells": return braincellsReport({ hours, limit: 15 });
    case "nopaint": return nopaintReport({ hours }).then(nopaintSummary);
    case "tezos": return tezosRevenue({ hours });
    case "iosUsage": return iosUsage({ days: reportAppDays(hours) });
    case "social": return socialReport({ root: REPO_ROOT, hours: checkedHours(hours, 2160) });
    case "search": return searchSummary({ days: Math.max(days, 7) });
    case "googleAnalytics": return googleAnalytics({ days: Math.min(days, 90) });
    case "appStore": return appDownloads({ days: reportAppDays(hours) });
    case "review": return appReview();
  }
}

// The Google and review-queue parts take 0.5 to 6 s depending on how busy this
// Mac is, and change slowly. Beside a snapshot they are kept on disk: an
// answer under fifteen minutes old is used as is, an older one (up to twelve
// hours) is still returned at once while a new one is fetched behind it, and
// the report's `kept` says when each was built. App Store downloads keep
// themselves for three hours.
const PART_KEEP_MS = 15 * 60_000;
const PART_STALE_MS = 12 * 3600_000;
const partRefreshes = new Map(); // name -> the fetch in flight
async function keptPart(name, build) {
  const file = join(homedir(), ".cache", "ac-analytics", `report-${name}.json`);
  const refresh = () => {
    if (!partRefreshes.has(name)) partRefreshes.set(name, (async () => {
      const value = await build();
      const at = Date.now();
      try {
        await mkdir(dirname(file), { recursive: true });
        await writeFile(`${file}.${process.pid}.tmp`, JSON.stringify({ at, value }));
        await rename(`${file}.${process.pid}.tmp`, file);
      } catch (error) { console.error(`${name} not kept: ${error.message}`); }
      return { value };
    })().finally(() => partRefreshes.delete(name)));
    return partRefreshes.get(name);
  };
  let kept;
  try { kept = JSON.parse(await readFile(file, "utf8")); } catch {}
  const age = kept ? Date.now() - kept.at : Infinity;
  if (!(age >= 0 && age < PART_STALE_MS)) return refresh();
  if (age >= PART_KEEP_MS) refresh().catch(() => {}); // the kept answer stands if this fails
  return { value: kept.value, at: kept.at };
}

// Run the named parts at once. Each gets the deadline and a timing in seconds.
// A part past its deadline keeps running, so with `keep` its answer is still
// on disk for the next call.
async function runParts(names, { hours = 24, scope = "all" } = {}, { keep = false } = {}) {
  hours = checkedHours(hours);
  const values = {}, timings = {}, kept = {};
  await Promise.all(names.map(async name => {
    const began = Date.now();
    let timer;
    const late = new Promise(resolve => {
      timer = setTimeout(() => resolve({ unavailable: `no answer within ${PART_DEADLINE_MS / 1000} s` }), PART_DEADLINE_MS);
    });
    const failed = error => ({ unavailable: String(error.message || error) });
    const build = async () => reportPart(name, { hours, scope });
    const work = (keep && name !== "appStore" ? keptPart(name, build) : build().then(value => ({ value })))
      .then(answer => answer, error => ({ value: failed(error) }));
    const answer = await Promise.race([work, late.then(value => ({ value }))]);
    values[name] = answer.value;
    if (answer.at) kept[name] = new Date(answer.at).toISOString();
    clearTimeout(timer);
    timings[name] = Math.round((Date.now() - began) / 100) / 10;
  }));
  return { values, timings, kept };
}

function assembleReport({ values: v, timings, kept = {} }, { hours = 24, scope = "all" } = {}) {
  return { window: v.visits.window ?? { hours, scope }, timings, ...(Object.keys(kept).length ? { kept } : {}), visits: v.visits, referrers: v.referrers, pieces: v.pieces, audience: v.audience,
    handles: v.handles, braincells: v.braincells, nopaint: v.nopaint, tezos: v.tezos, social: v.social, search: v.search, googleAnalytics: v.googleAnalytics,
    apps: { days: reportAppDays(Number(hours)), review: v.review, appStore: appStoreTotals(v.appStore), iosUsage: v.iosUsage.unavailable ? v.iosUsage : iosTotals(v.iosUsage) } };
}

const mergeParts = (...sets) => Object.fromEntries(["values", "timings", "kept"].map(key => [key, Object.assign({}, ...sets.map(set => set[key]))]));

// 📸 A timer on jasellite builds most parts of the default report (24
// hours, every scope) every ten minutes: `node toolchain/mcp/analytics-mcp.mjs
// --snapshot`, set up by toolchain/analytics/install-snapshot-timer.sh. Asking
// for that report here reads them in a single ssh hop while this Mac builds
// the vault parts itself. Anything else, a stale or missing snapshot, or
// `fresh`, builds every part live.
const SNAPSHOT_FILE = ".local/share/ac/analytics/network-report.json"; // under $HOME
const SNAPSHOT_HOST = process.env.AC_ANALYTICS_SNAPSHOT_HOST ?? "jasellite"; // "" turns it off
const SNAPSHOT_MAX_AGE_MS = 25 * 60_000; // two missed runs, then go live

async function networkSnapshot() {
  const { stdout } = await pexec("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=4", SNAPSHOT_HOST, `cat ${SNAPSHOT_FILE}`],
    { timeout: 6000, maxBuffer: 8 * 1024 * 1024 });
  const { at, values, timings } = JSON.parse(stdout);
  const age = Date.now() - at;
  if (!(age >= 0 && age < SNAPSHOT_MAX_AGE_MS)) throw new Error(`snapshot is ${Math.round(age / 60_000)} min old`);
  if (!SNAPSHOT_PARTS.every(name => values?.[name])) throw new Error("snapshot lacks parts this version expects");
  return { at, values, timings };
}

async function networkReport({ hours = 24, scope = "all", fresh = false } = {}) {
  const options = { hours, scope };
  if (fresh || !SNAPSHOT_HOST || Number(hours) !== 24 || scope !== "all") return assembleReport(await runParts([...SNAPSHOT_PARTS, ...VAULT_PARTS], options), options);
  const [kept, vault] = await Promise.all([networkSnapshot().catch(error => error), runParts(VAULT_PARTS, options, { keep: true })]);
  if (!(kept instanceof Error)) return { snapshotAt: new Date(kept.at).toISOString(), ...assembleReport(mergeParts(kept, vault), options) };
  const reason = String(kept.message || kept).trim().split("\n").pop();
  return { snapshotMissed: reason, ...assembleReport(mergeParts(await runParts(SNAPSHOT_PARTS, options), vault), options) };
}

// What the timer runs. Most parts unavailable means this host cannot reach
// lith; keeping the last good snapshot is better than that.
async function writeSnapshot() {
  const { values, timings } = await runParts(SNAPSHOT_PARTS, { hours: 24, scope: "all" });
  const failed = SNAPSHOT_PARTS.filter(name => values[name]?.unavailable);
  if (failed.length > SNAPSHOT_PARTS.length / 2) throw new Error(`${failed.length} of ${SNAPSHOT_PARTS.length} parts unavailable; snapshot not written`);
  const file = join(homedir(), SNAPSHOT_FILE);
  await mkdir(dirname(file), { recursive: true });
  await writeFile(`${file}.tmp`, JSON.stringify({ at: Date.now(), values, timings }));
  await rename(`${file}.tmp`, file);
  return `${file}: ${SNAPSHOT_PARTS.length - failed.length} of ${SNAPSHOT_PARTS.length} parts, slowest ${Math.max(...Object.values(timings))} s${failed.length ? `, unavailable: ${failed.join(", ")}` : ""}`;
}

// The review queue: every app on the account with its newest version per
// platform. "queue" holds the ones Apple has not finished with.
const REVIEW_PENDING = new Set(["WAITING_FOR_REVIEW", "IN_REVIEW", "PENDING_DEVELOPER_RELEASE", "REJECTED", "METADATA_REJECTED", "INVALID_BINARY"]);
async function appReview() {
  const { data: apps } = await asc("/v1/apps?fields[apps]=name&limit=50");
  const rows = await Promise.all(apps.map(async (app) => {
    const { data } = await asc(`/v1/apps/${app.id}/appStoreVersions?limit=4&fields[appStoreVersions]=platform,versionString,appStoreState,createdDate`);
    const newest = {};
    for (const v of data) newest[v.attributes.platform] ??= { platform: v.attributes.platform, version: v.attributes.versionString, state: v.attributes.appStoreState };
    return { app: app.attributes.name, versions: Object.values(newest) };
  }));
  const queue = rows.flatMap((r) => r.versions.filter((v) => REVIEW_PENDING.has(v.state)).map((v) => ({ app: r.app, ...v })));
  return { queue, apps: rows };
}

// Totals only; the daily breakdown stays in app_downloads.
function appStoreTotals(report) {
  if (report.unavailable) return report;
  const apps = {};
  for (const [name, app] of Object.entries(report.apps ?? {})) {
    if (!app.daily) { apps[name] = app; continue; }
    const byType = {};
    for (const day of Object.values(app.daily)) for (const [type, n] of Object.entries(day)) byType[type] = (byType[type] ?? 0) + n;
    apps[name] = { firstTimeDownloads: app.firstTimeDownloads, byType };
  }
  return { since: report.since, note: report.note, apps };
}

function iosTotals(report) {
  const apps = {};
  for (const [name, { daily, ...totals }] of Object.entries(report.apps ?? {})) apps[name] = totals;
  return { start: report.start, end: report.end, note: report.note, apps };
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
  // Hold the promise, not the key: parallel callers share one decrypt.
  ascKey ??= pexec("gpg", ["--batch", "--quiet", "--decrypt", vaultPath(`apple/AuthKey_${ASC_KEY_ID}.p8.gpg`)])
    .then(({ stdout }) => stdout, (error) => { ascKey = undefined; throw error; });
  const key = await ascKey;
  const b64 = (o) => Buffer.from(JSON.stringify(o)).toString("base64url");
  const now = Math.floor(Date.now() / 1000);
  const input = `${b64({ alg: "ES256", kid: ASC_KEY_ID, typ: "JWT" })}.${b64({ iss: ASC_ISSUER, iat: now, exp: now + 1200, aud: "appstoreconnect-v1" })}`;
  const sig = crypto.sign("sha256", Buffer.from(input), { key, dsaEncoding: "ieee-p1363" });
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
async function appDownloadsFor(name, since) {
  const requests = (await asc(`/v1/apps/${APPS[name]}/analyticsReportRequests`)).data
    .filter((r) => r.attributes.accessType === "ONGOING" && !r.attributes.stoppedDueToInactivity);
  if (!requests.length) return { note: "no ONGOING analytics report request" };
  const reports = (await Promise.all(requests.map((request) =>
    asc(`/v1/analyticsReportRequests/${request.id}/reports?filter[name]=${encodeURIComponent("App Downloads Standard")}`)))).flatMap((r) => r.data);
  const instances = (await Promise.all(reports.map((report) =>
    asc(`/v1/analyticsReports/${report.id}/instances?filter[granularity]=DAILY&limit=200`)))).flatMap((r) => r.data)
    .filter((instance) => instance.attributes.processingDate >= since);
  const tallies = await Promise.all(instances.map(async (instance) => {
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
    return { processed: instance.attributes.processingDate, counts };
  }));
  const byDate = {}; // date -> { processed, tally }
  for (const { processed, counts } of tallies) for (const [date, tally] of Object.entries(counts)) {
    if (date < since) continue;
    if (!byDate[date] || byDate[date].processed < processed) byDate[date] = { processed, tally };
  }
  const dates = Object.keys(byDate).sort();
  const firstTime = dates.reduce((n, d) => n + Object.entries(byDate[d].tally)
    .filter(([k]) => k.startsWith("First-time")).reduce((m, [, v]) => m + v, 0), 0);
  return { firstTimeDownloads: firstTime, daily: Object.fromEntries(dates.map((d) => [d, byDate[d].tally])) };
}

// Apple restates these numbers about once a day, and the walk above is the
// slowest part of network_report. Answers are kept on disk for three hours, so
// they outlive the hub resting this server; `fresh` skips the kept answer.
const APP_CACHE = join(homedir(), ".cache", "ac-analytics", "app-downloads.json");
const APP_CACHE_MS = 3 * 3600_000;
const appFlights = new Map(); // key -> the fetch in flight, shared by callers

const appCacheRead = async () => { try { return JSON.parse(await readFile(APP_CACHE, "utf8")); } catch { return {}; } };

async function appDownloads({ days = 7, apps = Object.keys(APPS), fresh = false } = {}) {
  days = Number(days);
  if (!Number.isFinite(days) || days < 1 || days > 60) throw new Error("days must be 1..60");
  for (const name of apps) if (!APPS[name]) throw new Error(`unknown app ${name}; known: ${Object.keys(APPS).join(", ")}`);
  const key = `${days}:${apps.join(",")}`;
  const kept = fresh ? null : (await appCacheRead())[key];
  if (kept && Date.now() - kept.at < APP_CACHE_MS) return { ...kept.report, cachedAt: new Date(kept.at).toISOString() };
  if (!appFlights.has(key)) appFlights.set(key, (async () => {
    const since = new Date(Date.now() - (days + 1) * 86400000).toISOString().slice(0, 10);
    const rows = await Promise.all(apps.map((name) => appDownloadsFor(name, since)));
    const report = { since, note: "Apple's data lags about a day; dates absent from `daily` had no downloads.",
      apps: Object.fromEntries(apps.map((name, i) => [name, rows[i]])) };
    try {
      const now = Date.now();
      const store = Object.fromEntries(Object.entries(await appCacheRead()).filter(([, entry]) => now - entry.at < APP_CACHE_MS));
      store[key] = { at: now, report };
      await mkdir(dirname(APP_CACHE), { recursive: true });
      await writeFile(`${APP_CACHE}.${process.pid}.tmp`, JSON.stringify(store));
      await rename(`${APP_CACHE}.${process.pid}.tmp`, APP_CACHE);
    } catch (error) { console.error(`app-downloads cache not written: ${error.message}`); }
    return report;
  })().finally(() => appFlights.delete(key)));
  return appFlights.get(key);
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
  ...["account_activity", "network_referrers", "feature_usage", "piece_landings", "mime_journeys", "network_audience", "handle_usage"].map(name => ({
    name,
    description: name === "network_audience"
      ? "Who is loading the network, from lith's boot telemetry: humans (browser, country, language, signed-in vs anonymous, pieces, referrer sites), automated (bot family such as Googlebot, GPTBot, Headless Chrome or our own render flags, with one example user agent per family, plus country, pieces and referrers) and walls (embedded iframes like the kidlisp.com gallery screens). Only bots that ran JavaScript appear; the visit tracker's automated count is a separate instrument. Use when asked where bots come from or what they are."
      : name === "handle_usage"
      ? "Signed-in usage per public handle: page loads (boots) carrying each handle with completed loads, pieces landed on, properties, browsers, first and last seen, plus server-verified account-activity events, sessions, pieces and actions. Bots, walls and local dev excluded. Login is not proof of a human. Private SSH-backed read."
      : name === "mime_journeys"
      ? "Compare signed-in AC accounts opening MIME from the prompt (swipe vs wheel/key/history), direct MIME visits, and Laer Klokken opens/minutes. Includes overlap, public handles and per-content foreground time. Private SSH report; MIME coverage starts at its tracking rollout, no anonymous-history joins. Optional handle filter."
      : name === "piece_landings"
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
      ...(!["network_referrers", "piece_landings", "network_audience", "handle_usage"].includes(name) ? { handle: { type: "string", description: "Optional public handle, with or without @" } } : {}),
    } },
  })),
  {
    name: "network_report",
    description: "The Aesthetic network report in one call: first-party visits per site (including shop.aesthetic.computer and notepaint.art), referral sites, landing pieces (anonymous and signed-in), the audience split (humans by browser, country and language vs automated by bot family, country and piece, vs embedded walls), signed-in usage per handle (boots, pieces and account-activity events), the Braincell economy (circulation and dollar value, spend by model, handle and product, purchases, OpenRouter balance and usage), No Paint (classic game saves per UTC day, teacher notes, the notepaint/AC sticker split and notepaint funnel), Tezos revenue (objkt sales, royalties, Keeps fees, buyers), social accounts (Instagram reels with insights, Bluesky, X), Google search (clicks, impressions and top queries per domain), Google Analytics sessions by source, and the apps (App Store review queue, App Store downloads per app, AC iOS active installs). Use this whenever asked about Aesthetic network traffic. Parts that fail report themselves as unavailable.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
      scope: { type: "string", enum: ["studio", "clients", "all"], description: "Visit scope; default all" },
      fresh: { type: "boolean", description: "Build every part now. The default report (24 hours, all) takes its visit, audience, Braincell, No Paint, Tezos, iOS and social parts from a snapshot at most 25 minutes old (snapshotAt says when it was built); the Google and review-queue parts may be kept answers, dated in `kept`" },
    } },
  },
  {
    name: "nopaint_report",
    description: "No Paint (nopaint.art) in one read: classic saves from the 2021 Construct game's Done button (total, per UTC day, first/last savedAt, codes reserved and never saved; counts only, never codes or images; these saves are invisible to the visit collector), teacher notes (count, school and language per note, form opened/sent; no names, emails or messages), the 50/50 sticker split (notepaint vs aesthetic.computer: shown, opened, arrivals, engaged, made), the notepaint funnel for arrivals from No Paint, and the Braincells funnel on notepaint. Page visits, not unique people. Read-only.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
      end: { type: "string", description: "Optional ISO UTC end, e.g. 2026-11-04T12:00:00Z" },
    } },
  },
  {
    name: "search_console",
    description: `Google Search Console for one AC domain (${SEARCH_SITES.join(", ")}): total clicks, impressions, CTR and average position plus the top rows by query, page, country, device or date; optionally the sitemaps and their crawl state. Data ends about two days back. Read-only service account.`,
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, required: ["site"], properties: {
      site: { type: "string", enum: SEARCH_SITES },
      days: { type: "integer", minimum: 1, maximum: 480, description: "Default 7" },
      dimension: { type: "string", enum: ["query", "page", "country", "device", "date"], description: "Default query" },
      limit: { type: "integer", minimum: 1, maximum: 1000, description: "Rows; default 25" },
      sitemaps: { type: "boolean", description: "Also list submitted sitemaps (default false)" },
    } },
  },
  {
    name: "google_analytics",
    description: "Google Analytics 4 for aesthetic.computer (property 401592606, stream G-B4TLVYKXVF): sessions, engaged sessions and GA's user estimate, by source/medium and by landing page. GA counts differ from AC's first-party visits; compare, never add.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      days: { type: "integer", minimum: 1, maximum: 90, description: "Default 7" },
      limit: { type: "integer", minimum: 1, maximum: 100, description: "Rows per breakdown; default 15" },
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
    name: "rax_walks",
    description: "Who has been in rax, the 3D world under the prompt (formerly dool), from the server's own walk log on lith: one row per visit with room, public handle or guest name, when they entered, how long they stayed and how far they walked. Pass a visit id for that visit's path (t, x, y, z, yaw), thinned to `points` samples from the recorded 60 a second. A visit is one connection, not one person; guests only watch, so only handles have a path. Recording began 2026-11-18 and keeps 35 days. Read-only.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      hours: { type: "number", exclusiveMinimum: 0, maximum: 840, description: "Lookback hours; default 24" },
      visit: { type: "string", pattern: "^[a-f0-9]{8}$", description: "A visit id from the listing: return its path" },
      points: { type: "integer", minimum: 2, maximum: 5000, description: "Most path samples to return; default 600" },
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
    description: "AC Human Fishery: recent anonymous visits with interaction, plus a separate MIME watch. mime.events includes verified signed-in handles, prompt swipe/direct entry, per-piece foreground milliseconds and returns; deduplicate by event. mime.directPageArrivals includes non-automated mime.ac page arrivals even before interaction. Anonymous fish and account events are separate, overlapping instruments: never join them or add their counts. Anonymous prompt swipes are not covered. Read-only; repeat after at least 15 seconds. Last report/exposure is not proof someone is still online.",
    annotations: { readOnlyHint: true, destructiveHint: false },
    inputSchema: { type: "object", additionalProperties: false, properties: {
      minutes: { type: "number", minimum: 1, maximum: 60, description: "Look back this many minutes for reported activity; default 5" },
      scope: { type: "string", enum: ["studio", "clients", "all"], description: "Default studio" },
      limit: { type: "integer", minimum: 1, maximum: 200, description: "Maximum rows per fish, MIME event and MIME arrival list; default 50" },
      startedAfter: { type: "string", description: "Optional ISO date: anonymous arrivals and MIME account events after this time" },
    } },
  },
  {
    name: "ios_usage",
    description: "AC iOS 1.2+ foreground opens, unique active installs, first-observed installs, returning installs, active seconds, successful loads and canvas engagement. App Store downloads remain separate. Older builds have no coverage; no user/account identities are returned.",
    inputSchema: { type: "object", properties: { days: { type: "integer", description: "UTC days including today (default 7, max 35)" } } },
  },
  {
    name: "visits_report",
    description: "First-party page visits across AC web properties (from lith's network-visits collector), folded per site: visits, interacted, engaged (10s+), visible minutes, actions (also split by landing surface in actionsBySurface; media starts split into audio_started / video_started, with camera or call previews counted apart as live_preview_started), automated traffic. Page visits, not unique people. Retention is 35 days; collection began 2026-09-23.",
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
        fresh: { type: "boolean", description: "Ask Apple again instead of using an answer kept from the last three hours (see cachedAt)" },
      },
    },
  },
];

async function callTool(name, args = {}) {
  const result = name === "visits_report" ? await visitsReport(args)
    : name === "braincells_report" ? await braincellsReport(args)
    : name === "rax_walks" ? await raxWalks(args)
    : name === "nopaint_report" ? await nopaintReport(args)
    : name === "network_report" ? await networkReport(args)
    : name === "search_console" ? { ...(await searchConsole(args)), ...(args.sitemaps ? { sitemaps: await sitemaps({ site: args.site }) } : {}) }
    : name === "google_analytics" ? await googleAnalytics(args)
    : name === "tezos_revenue" ? await tezosRevenue({ hours: checkedHours(args.hours ?? 24) })
    : name === "social_report" ? await socialReport({ root: REPO_ROOT, hours: checkedHours(args.hours ?? 24, 2160) })
    : name === "mime_journeys" ? await journeyReport("mime", args)
    : name === "account_activity" ? await journeyReport("accounts", args)
    : name === "feature_usage" ? await journeyReport("features", args)
    : name === "network_referrers" ? await journeyReport("referrers", args)
    : name === "piece_landings" ? await journeyReport("pieces", args)
    : name === "network_audience" ? await journeyReport("audience", args)
    : name === "handle_usage" ? await journeyReport("handles", args)
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
if (process.argv.includes("--snapshot")) {
  try { console.log(await writeSnapshot()); process.exit(0); }
  catch (error) { console.error(String(error.message || error)); process.exit(1); }
} else if (port) serveHttp({ handleMessage, port, banner: "📈 analytics-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📈 analytics-mcp started (visits_report, direct_downloads, app_downloads)" });
