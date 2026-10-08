import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { git } from "./io.mjs";
import { eligiblePiece } from "./config.mjs";
import { userReports } from "./reports.mjs";
const execFileAsync = promisify(execFile);

export async function functionConsumers(repo, routes) {
  const names = (await git(repo, "ls-tree", "-r", "--name-only", "HEAD", "--", "system/netlify/functions/")).split("\n")
    .map(path => /^system\/netlify\/functions\/([a-z0-9-]+)\.(?:mjs|js)$/.exec(path)?.[1]).filter(name => name &&
      !["reports", "stories", "client-media", "get-builds", "register-build", "update-build", "verify-builds-password"].includes(name));
  const consumers = Object.fromEntries(names.map(name => [name, []]));
  const source = await git(repo, "grep", "-n", "-E", "/api/|/\\.netlify/functions/", "HEAD", "--", "system/public/aesthetic.computer/disks/*.mjs");
  for (const line of source.split("\n")) {
    const route = /^HEAD:system\/public\/aesthetic\.computer\/disks\/([a-z0-9-]+)\.mjs:/.exec(line)?.[1];
    if (!route || !routes.includes(route) || !eligiblePiece(route)) continue;
    for (const match of line.matchAll(/\/(?:api|\.netlify\/functions)\/([a-z0-9-]+)/g)) {
      if (consumers[match[1]] && !consumers[match[1]].includes(route)) consumers[match[1]].push(route);
    }
  }
  return consumers;
}

// Executed on Lith. Personal columns and raw logs never enter its result.
export async function collectDiagnostics(db, opts, readJournal, readErrors, classify = userReports) {
  const output = { format: "aespatcher.supplement.v1", coverage: [], leads: [], privateReports: [], metrics: [] };
  const end = new Date(opts.end), start = new Date(+end - opts.hours * 3600000), window = { $gte: start, $lt: end };
  const available = (source, scanned, truncated = false) => output.coverage.push({ source, status: "available", scanned, truncated });
  const unavailable = source => output.coverage.push({ source, status: "unavailable", reason: "Source could not be read within its bound; no zero-activity claim." });
  try {
    const rows = await db.collection("boots").aggregate([
      { $match: { createdAt: window, "meta.host": { $in: opts.hosts }, "meta.localDev": { $ne: true }, "meta.embedded": { $ne: true }, "meta.packMode": { $ne: true },
        "meta.userAgent": { $not: /bot|crawler|spider|headless|lighthouse|curl|python|wget|monitor/i } } },
      { $project: { route: { $let: { vars: { found: { $regexFind: { input: { $ifNull: ["$meta.path", ""] }, regex: "^/([a-z0-9-]+)(?:$|[:~/])" } } },
        in: { $ifNull: [{ $arrayElemAt: ["$$found.captures", 0] }, null] } } },
        failed: { $or: [{ $eq: ["$status", "error"] }, { $ne: [{ $ifNull: ["$error", null] }, null] }, { $in: ["error", { $ifNull: ["$events.level", []] }] }] } } },
      { $group: { _id: { $cond: [{ $in: ["$route", opts.routes] }, "$route", null] }, boots: { $sum: 1 }, errors: { $sum: { $cond: ["$failed", 1, 0] } } } },
      { $sort: { errors: -1, boots: -1 } }, { $limit: opts.limit + 1 },
    ], { maxTimeMS: 20000, allowDiskUse: false }).toArray();
    available("boots", rows.slice(0, opts.limit).reduce((n, r) => n + r.boots, 0), rows.length > opts.limit);
    for (const row of rows.slice(0, opts.limit)) {
      if (row.boots < opts.minimum) continue;
      output.metrics.push({ source: "boots", metric: "boots", target: row._id, count: row.boots });
      output.metrics.push({ source: "boots", metric: "error-bearing-boots", target: row._id, count: row.errors });
      if (row.errors >= opts.minimum) output.leads.push({ source: "boots", kind: "boot-error", route: row._id, count: row.errors, evidenceRefs: [],
        summary: row._id ? `${row.errors} boots into ${row._id} retained or logged errors; reproduce before locating the cause.` : "Error-bearing boots lack an eligible public piece path; manual runtime review required." });
    }
  } catch { unavailable("boots"); }
  for (const source of ["chat-clock", "chat-system"]) {
    try {
      const rows = await db.collection(source).aggregate([
        { $match: { when: window, deleted: { $ne: true }, text: { $type: "string" } } },
        { $sort: { when: -1 } },
        { $lookup: { from: `${source}-mutes`, localField: "user", foreignField: "user", as: "mute", pipeline: [{ $limit: 1 }, { $project: { _id: 1 } }] } },
        { $match: { "mute.0": { $exists: false } } }, { $limit: opts.chatLimit + 1 }, { $project: { _id: 0, when: 1, text: 1 } },
      ], { maxTimeMS: 10000, allowDiskUse: false }).toArray();
      const reports = classify(rows.slice(0, opts.chatLimit), source, opts.routes);
      available(source, Math.min(rows.length, opts.chatLimit), rows.length > opts.chatLimit);
      output.leads.push(...reports.leads); output.privateReports.push(...reports.privateReports);
      output.metrics.push({ source, metric: "matched-public-reports", target: null, count: reports.privateReports.length });
    } catch { unavailable(source); }
  }
  for (const source of ["lith-journal", "lith-errors"]) {
    try {
      const result = source === "lith-journal" ? await readJournal(start, end, opts.logLimit) : await readErrors(start, end, opts.logLimit);
      const counts = new Map();
      for (const name of result.names) if (Object.hasOwn(opts.consumers, name)) counts.set(name, (counts.get(name) || 0) + 1);
      available(source, result.scanned, result.truncated);
      for (const [name, count] of counts) {
        if (count < opts.minimum) continue;
        output.metrics.push({ source, metric: "function-errors", target: name, count });
        const consumers = opts.consumers[name];
        const route = consumers.length === 1 ? consumers[0] : null;
        output.leads.push({ source, kind: "backend-error", route, count, evidenceRefs: [], summary: route
          ? `${name} recorded ${count} backend errors; ${route} references that endpoint in source. Reproduce client behavior; backend changes require manual review.`
          : `${name} recorded ${count} backend errors without one unambiguous leaf consumer; manual backend review required.` });
      }
    } catch { unavailable(source); }
  }
  return output;
}

export async function journalNames(start, end, limit) {
  const { stdout } = await execFileAsync("journalctl", ["--unit=lith", `--since=${start.toISOString()}`, `--until=${end.toISOString()}`, "--output=json", "--no-pager", `--lines=${limit + 1}`], { timeout: 15000, maxBuffer: 4 * 1024 * 1024 });
  const lines = stdout.split("\n").filter(Boolean), names = [];
  for (const line of lines.slice(-limit)) {
    const row = JSON.parse(line), message = typeof row.MESSAGE === "string" ? row.MESSAGE : "";
    const name = /^fn\/([a-z0-9-]+) (?:background |stream )?error:/.exec(message)?.[1];
    if (name) names.push(name);
  }
  return { names, scanned: Math.min(lines.length, limit), truncated: lines.length > limit };
}

export async function errorNames(start, end, limit) {
  const response = await fetch(`http://127.0.0.1:8888/lith/errors?limit=${Math.min(limit, 500)}`, { signal: AbortSignal.timeout(10000) });
  if (!response.ok) throw new Error("Lith errors unavailable");
  const chunks = []; let bytes = 0;
  for await (const chunk of response.body) {
    bytes += chunk.byteLength;
    if (bytes > 2 * 1024 * 1024) throw new Error("Lith errors exceed read bound");
    chunks.push(chunk);
  }
  const result = JSON.parse(Buffer.concat(chunks).toString("utf8"));
  if (!Array.isArray(result.errors)) throw new Error("Unexpected Lith errors response");
  const rows = result.errors.filter(row => Date.parse(row.time) >= +start && Date.parse(row.time) < +end);
  return { names: rows.map(row => row.fn), scanned: result.errors.length, truncated: result.total >= Math.min(limit, 500) };
}
