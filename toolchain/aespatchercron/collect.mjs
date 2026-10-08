import { readFile } from "node:fs/promises";
import { git, hash, run } from "./io.mjs";
import { visitScopeMatch, visitSurface, VISIT_PROPERTIES, SURFACES } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { collectDiagnostics, journalNames, errorNames, functionConsumers } from "./diagnostics.mjs";
import { userReports } from "./reports.mjs";
import { emptySignals, mergeSignals, validateSignals } from "./signals.mjs";
import { collectPostHog } from "./posthog.mjs";

export async function catalog(repo) {
  const prefix = "system/public/aesthetic.computer/disks/";
  return (await git(repo, "ls-tree", "-r", "--name-only", "HEAD", "--", prefix)).split("\n")
    .filter(p => /^[-a-z0-9]+\.mjs$/.test(p.slice(prefix.length)))
    .map(p => p.slice(prefix.length, -4)).filter(slug => visitSurface(`/${slug}`) !== null);
}

export function collectorOptions({ hours = 24, minimum = 3, limit = 100, chatLimit = 200, logLimit = 2000, routes, end = new Date().toISOString(), properties, hosts }) {
  if (!Number.isInteger(hours) || hours < 1 || hours > 168 || !Number.isInteger(minimum) || minimum < 3 || minimum > 100 ||
      !Number.isInteger(limit) || limit < 1 || limit > 500 || !Number.isFinite(Date.parse(end)) ||
      !Number.isInteger(chatLimit) || chatLimit < 1 || chatLimit > 500 || !Number.isInteger(logLimit) || logLimit < 1 || logLimit > 5000 ||
      !Array.isArray(routes) || !routes.length || routes.some(s => !/^[a-z0-9-]+$/.test(s))) throw new Error("Invalid collection bounds");
  return { hours, minimum, limit, chatLimit, logLimit, routes, end, properties, hosts };
}

// This function travels over stdin and runs inside Lith. Only grouped rows return.
export async function collectDatabase(db, opts) {
  const { routes, properties, hosts, minimum, limit } = opts;
  const end = new Date(opts.end), start = new Date(+end - opts.hours * 3600000);
  const window = { $gte: start, $lt: end };
  const output = { format: "aespatcher.signals.v1", start: start.toISOString(), end: end.toISOString(), minimum,
    runs: [], transitions: [], visits: [], unavailable: [], truncated: [] };
  async function rows(name, collection, pipeline) {
    try {
      const data = await db.collection(collection).aggregate([...pipeline, { $limit: limit + 1 }], { maxTimeMS: 20000, allowDiskUse: false }).toArray();
      if (data.length > limit) output.truncated.push(name);
      return data.slice(0, limit);
    } catch { output.unavailable.push(name); return []; }
  }
  const prod = { "meta.host": { $in: hosts }, "meta.localDev": { $ne: true }, "meta.embedded": { $ne: true }, "meta.packMode": { $ne: true },
    "meta.userAgent": { $not: /bot|crawler|spider|headless|lighthouse|curl|python|wget|monitor/i } };
  output.runs = await rows("runs", "piece-runs", [
    { $match: { createdAt: window, ...prod, slug: { $in: routes } } },
    { $group: { _id: "$slug", runs: { $sum: 1 }, errors: { $sum: { $cond: [{ $or: [
      { $eq: ["$status", "error"] }, { $ne: [{ $ifNull: ["$error", null] }, null] },
    ] }, 1, 0] } } } },
    { $match: { runs: { $gte: minimum } } }, { $sort: { errors: -1, runs: -1, _id: 1 } },
    { $project: { _id: 0, route: "$_id", runs: 1, errors: 1 } },
  ]);
  // Keep private/unknown routes in the window as null so filtering never invents a direct edge.
  for (const source of ["piece-runs", "account-activity"]) {
    const piece = source === "piece-runs";
    const events = await rows(source, source, [
      { $match: piece ? { createdAt: window, ...prod, bootId: { $type: "string", $ne: "" } }
        : { at: window, tenant: "aesthetic", property: "aesthetic.computer", action: "piece_opened", session: { $type: "string", $ne: "" } } },
      { $project: { _id: 1, at: piece ? "$createdAt" : "$at", sequence: piece ? "$createdAt" : "$sequence",
        partition: piece ? "$bootId" : { user: "$user", session: "$session" },
        route: { $cond: [{ $in: [piece ? "$slug" : "$piece", routes] }, piece ? "$slug" : "$piece", null] } } },
      { $setWindowFields: { partitionBy: "$partition", sortBy: { sequence: 1, at: 1, _id: 1 }, output: {
        previous: { $shift: { output: "$route", by: -1, default: null } },
        previousAt: { $shift: { output: "$at", by: -1, default: null } },
      } } },
      { $match: { $expr: { $and: [{ $ne: ["$route", null] }, { $ne: ["$previous", null] },
        { $gte: [{ $subtract: ["$at", "$previousAt"] }, 0] }, { $lte: [{ $subtract: ["$at", "$previousAt"] }, 1800000] }] } } },
      { $group: { _id: { from: "$previous", to: "$route" }, count: { $sum: 1 } } },
      { $match: { count: { $gte: minimum } } }, { $sort: { count: -1, "_id.from": 1, "_id.to": 1 } },
      { $project: { _id: 0, from: "$_id.from", to: "$_id.to", count: 1, source: { $literal: source } } },
    ]);
    output.transitions.push(...events);
  }
  output.visits = await rows("visits", "network-visits", [
    { $match: { startedAt: window, automated: false, property: { $in: properties }, surface: { $in: ["home", "play", "gallery", "read", "support", "other"] } } },
    { $group: { _id: { property: "$property", surface: "$surface" }, visits: { $sum: 1 },
      interacted: { $sum: { $cond: ["$interacted", 1, 0] } }, engaged: { $sum: { $cond: ["$engaged", 1, 0] } } } },
    { $match: { visits: { $gte: minimum } } }, { $sort: { visits: -1 } },
    { $project: { _id: 0, property: "$_id.property", surface: "$_id.surface", visits: 1, interacted: 1, engaged: 1 } },
  ]);
  return output;
}

export function validateReport(report, routes) {
  const keys = (value, names) => value && typeof value === "object" && !Array.isArray(value) && Object.keys(value).every(k => names.includes(k)) && names.every(k => Object.hasOwn(value, k));
  const count = n => Number.isSafeInteger(n) && n >= 0;
  const sources = ["runs", "piece-runs", "account-activity", "visits"];
  const route = s => routes.includes(s);
  if (!keys(report, ["format", "start", "end", "minimum", "runs", "transitions", "visits", "unavailable", "truncated"]) ||
      report.format !== "aespatcher.signals.v1" || !Number.isFinite(Date.parse(report.start)) || !Number.isFinite(Date.parse(report.end)) || Date.parse(report.end) <= Date.parse(report.start) ||
      !count(report.minimum) || report.minimum < 3 || ["runs", "transitions", "visits", "unavailable", "truncated"].some(k => !Array.isArray(report[k]) || report[k].length > 1000) ||
      report.unavailable.some(s => !sources.includes(s)) || report.truncated.some(s => !sources.includes(s)) ||
      report.runs.some(r => !keys(r, ["route", "runs", "errors"]) || !route(r.route) || !count(r.runs) || !count(r.errors) || r.errors > r.runs || r.runs < report.minimum) ||
      report.transitions.some(r => !keys(r, ["from", "to", "count", "source"]) || !route(r.from) || !route(r.to) || !count(r.count) || r.count < report.minimum || !["piece-runs", "account-activity"].includes(r.source)) ||
      report.visits.some(r => !keys(r, ["property", "surface", "visits", "interacted", "engaged"]) || !visitScopeMatch("studio").property.$in.includes(r.property) || !SURFACES.includes(r.surface) ||
        !count(r.visits) || !count(r.interacted) || !count(r.engaged) || r.visits < report.minimum || r.interacted > r.visits || r.engaged > r.interacted)) throw new Error("Rejected non-minimized signal report");
  return report;
}

export async function collect(repo, config) {
  const routes = await catalog(repo);
  const opts = collectorOptions({ ...config.collection, routes, properties: visitScopeMatch("studio").property.$in,
    hosts: ["aesthetic.computer", ...VISIT_PROPERTIES["aesthetic.computer"]] });
  const remote = config.lith;
  if (!/^[a-zA-Z0-9_.@-]+$/.test(remote.host) || !/^\/[a-zA-Z0-9_./-]+$/.test(remote.root)) throw new Error("Invalid Lith transport");
  opts.consumers = await functionConsumers(repo, routes);
  const source = `import { createHash } from "node:crypto"; import { execFile } from "node:child_process"; import { promisify } from "node:util"; const execFileAsync = promisify(execFile); console.log = () => {};\nconst { connect, closePool } = await import(${JSON.stringify(remote.root + "/system/backend/database.mjs")});\ntry { const { db } = await connect(); const opts = ${JSON.stringify(opts)}; const report = await (${collectDatabase.toString()})(db, opts); const signals = await (${collectDiagnostics.toString()})(db, opts, ${journalNames.toString()}, ${errorNames.toString()}, ${userReports.toString()}); process.stdout.write(JSON.stringify({ report, signals })); } catch { process.stderr.write("Collection unavailable\\n"); process.exitCode = 1; } finally { await closePool(); }`;
  const cmd = ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", ...(remote.identity ? ["-i", remote.identity] : []), remote.host,
    `cd '${remote.root}/system' && AC_DB_CLOSE=1 node --env-file=.env --input-type=module`];
  let parsed;
  try {
    const result = await run(cmd, { input: source, timeout: 180000 });
    if (result.code !== 0) throw new Error("Lith unavailable");
    parsed = JSON.parse(result.stdout);
    validateReport(parsed.report, routes); validateSignals(parsed.signals, routes);
  } catch {
    const signals = emptySignals();
    signals.coverage = ["boots", "chat-clock", "chat-system", "lith-journal", "lith-errors"].map(source => ({ source, status: "unavailable", reason: "Lith transport or report validation failed; raw diagnostics withheld." }));
    parsed = { signals, report: { format: "aespatcher.signals.v1", start: new Date(Date.parse(opts.end) - opts.hours * 3600000).toISOString(), end: opts.end, minimum: opts.minimum,
      runs: [], transitions: [], visits: [], unavailable: ["runs", "piece-runs", "account-activity", "visits"], truncated: [] } };
  }
  const signals = validateSignals(mergeSignals(parsed.signals, await collectPostHog(repo, config)), routes);
  const modules = {};
  for (const file of ["collect.mjs", "diagnostics.mjs", "reports.mjs", "signals.mjs", "posthog.mjs"])
    modules[file] = hash(await readFile(new URL(file, import.meta.url)));
  return { report: parsed.report, signals, provenance: { revision: await git(repo, "rev-parse", "HEAD"),
    collector: hash(JSON.stringify(modules)), modules, remotePayload: hash(source), at: new Date().toISOString() } };
}
