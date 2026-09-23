#!/usr/bin/env node
// Run on Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/visits-report.mjs --hours 72
import { connect, closePool } from "../../system/backend/database.mjs";
import { VISIT_COLLECTION, RETENTION_DAYS, visitReportPipeline, visitScopeMatch, visitGroup } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
const args = process.argv.slice(2);
const value = name => args[args.indexOf(name) + 1];
const hours = args.includes("--hours") ? Number(value("--hours")) : 72;
const end = args.includes("--end") ? new Date(value("--end")) : new Date();
if (!Number.isFinite(hours) || hours <= 0 || hours > RETENTION_DAYS * 24 || !Number.isFinite(+end))
  throw new Error(`Use --hours 1..${RETENTION_DAYS * 24} and optional --end ISO-date`);
const scope = args.includes("--scope") ? value("--scope") : "studio";
const scopeMatch = visitScopeMatch(scope);
const start = new Date(+end - hours * 3600000);
const { db } = await connect();
try {
  const collection = db.collection(VISIT_COLLECTION);
  const rows = await collection.aggregate(visitReportPipeline(start, end, false, scope), { maxTimeMS: 20000 }).toArray();
  const periods = await collection.aggregate(visitReportPipeline(start, end, true, scope), { maxTimeMS: 20000 }).toArray();
  const first = await collection.find(scopeMatch, { projection: { startedAt: 1 } }).sort({ startedAt: 1 }).limit(1).next();
  console.log(JSON.stringify({ format: "ac.network-visits.v1", scope, start, end,
    earliestRetainedVisit: first?.startedAt || null, retentionDays: RETENTION_DAYS,
    unit: "page visits, not unique people",
    audience: rows.filter(row => !row._id.automated).map(row => ({ ...row, group: visitGroup(row._id.property), unknownAudience: row.visits - row.interacted })),
    automation: rows.filter(row => row._id.automated).map(row => ({ ...row, group: visitGroup(row._id.property) })),
    periods: periods.map(row => ({ ...row, group: visitGroup(row._id.property),
      start: new Date(+start + row._id.period * 86400000),
      end: new Date(Math.min(+end, +start + (row._id.period + 1) * 86400000)) })),
  }, null, 2));
} finally { await closePool(); }
