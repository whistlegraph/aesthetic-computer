#!/usr/bin/env node
// Run on lith: node --env-file=system/.env system/backend/mail-events-cli.mjs
import { pathToFileURL } from "node:url";

export function options(args) {
  const out = { since: 1440, limit: 100, failures: false, json: false };
  for (let i = 0; i < args.length; i++) {
    const key = args[i].replace(/^--/, "");
    if (["json", "failures", "help"].includes(key)) out[key] = true;
    else if (["since", "limit", "trace", "letter"].includes(key)) out[key] = args[++i];
    else throw new Error("Unknown option; use --help");
  }
  out.since = Number(out.since); out.limit = Number(out.limit);
  if (!Number.isFinite(out.since) || out.since <= 0 || out.since > 43200) throw new Error("--since must be 1–43200 minutes");
  if (!Number.isInteger(out.limit) || out.limit < 1 || out.limit > 1000) throw new Error("--limit must be 1–1000");
  if (out.trace !== undefined && !/^[a-f\d]{8}(?:-[a-f\d]{4}){3}-[a-f\d]{12}$/i.test(out.trace)) throw new Error("Invalid --trace UUID");
  if (out.letter !== undefined && !/^[a-f\d]{24}$/i.test(out.letter)) throw new Error("Invalid --letter ID");
  return out;
}

export function queryFor(opts, now = Date.now()) {
  return {
    when: { $gte: new Date(now - opts.since * 60000) },
    ...(opts.trace ? { trace: opts.trace } : {}),
    ...(opts.letter ? { letterId: opts.letter } : {}),
    ...(opts.failures ? { $or: [
      { event: { $in: ["failed", "rejected", "deferred", "push_failed", "relay_fallback", "relays_failed", "smtp_error"] } },
      { status: { $gte: 400 } }, { failed: { $gt: 0 } },
    ] } : {}),
  };
}

export async function inspectMailEvents(database, opts) {
  const collection = database.db.collection("mail-events");
  const query = queryFor(opts);
  const [summary, rows] = await Promise.all([
    collection.aggregate([
      { $match: query },
      { $group: { _id: { transport: "$transport", event: "$event", reason: "$reason", status: "$status" }, count: { $sum: 1 }, latest: { $max: "$when" } } },
      { $sort: { count: -1 } },
    ], { maxTimeMS: 5000 }).toArray(),
    collection.find(query, { projection: { _id: 0 }, maxTimeMS: 5000 }).sort({ when: -1 }).limit(opts.limit).toArray(),
  ]);
  return { sinceMinutes: opts.since, retainedDays: 30, summary, events: rows.reverse() };
}

async function main() {
  const opts = options(process.argv.slice(2));
  if (opts.help) {
    console.log("mail-events-cli [--since minutes (default 1440)] [--limit 1–1000] [--failures] [--trace UUID] [--letter ID] [--json]\nSummary counts cover the full selected window; event rows are limited. Times are UTC. Retention: 30 days.");
    return;
  }
  const { connect, closePool } = await import("./database.mjs");
  // Keep machine-readable stdout free of database startup chatter.
  const saved = console.log;
  console.log = (...args) => console.error(...args);
  try {
    const database = await connect();
    const result = await inspectMailEvents(database, opts);
    if (opts.json) saved(JSON.stringify(result));
    else {
      saved(`Last ${opts.since} minutes; ${result.events.length} latest events shown (30-day retention)`);
      for (const row of result.summary) saved(`${row.count}  ${Object.values(row._id).filter((v) => v != null).join(" / ")}`);
      for (const { when, transport, event, trace, ...fields } of result.events) saved(`${when.toISOString()}  ${transport || "-"} ${event}  ${trace || "-"}  ${JSON.stringify(fields)}`);
    }
  } finally { await closePool(); console.log = saved; }
}
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch(() => { console.error("Mail event inspection failed; check options and database access."); process.exitCode = 1; });
}
