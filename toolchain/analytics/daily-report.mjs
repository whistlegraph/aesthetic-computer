#!/usr/bin/env node
// Run on Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/daily-report.mjs --days 14
// Reads the per-day rollups written by system/backend/metrics-daily.mjs.
import { connect, closePool } from "../../system/backend/database.mjs";
const args = process.argv.slice(2);
const days = args.includes("--days") ? Number(args[args.indexOf("--days") + 1]) : 14;
if (!Number.isFinite(days) || days <= 0 || days > 3650) throw new Error("Use --days 1..3650");
const start = new Date(Date.now() - days * 86400000).toISOString().slice(0, 10);
const { db } = await connect();
try {
  const rows = await db.collection("metrics-daily").find({ _id: { $gte: start } }).sort({ _id: 1 }).toArray();
  console.log(JSON.stringify({ format: "ac.metrics-daily.v1", start, rows }, null, 2));
} finally { await closePool(); }
