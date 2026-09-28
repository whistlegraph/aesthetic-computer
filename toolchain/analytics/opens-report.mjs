#!/usr/bin/env node
// Run on Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/opens-report.mjs --days 7
// Native app launches counted by /api/app-open (system/netlify/functions/app-open.mjs).
// Per app and day: active installs (rows), opens, and fresh installs.
import { connect, closePool } from "../../system/backend/database.mjs";
const args = process.argv.slice(2);
const value = name => args[args.indexOf(name) + 1];
const days = args.includes("--days") ? Number(value("--days")) : 7;
if (!Number.isFinite(days) || days <= 0 || days > 35) throw new Error("Use --days 1..35");
const start = new Date(Date.now() - days * 86400000).toISOString().slice(0, 10);
const { db } = await connect();
try {
  const rows = await db.collection("app-opens").aggregate([
    { $match: { day: { $gte: start } } },
    { $group: {
      _id: { app: "$app", day: "$day" },
      active: { $sum: 1 }, opens: { $sum: "$opens" }, fresh: { $sum: { $cond: ["$fresh", 1, 0] } },
      platforms: { $push: "$platform" }, versions: { $push: "$version" }, countries: { $push: "$country" },
    } },
    { $sort: { "_id.app": 1, "_id.day": 1 } },
  ], { maxTimeMS: 20000 }).toArray();
  const tally = list => list.reduce((out, key) => ({ ...out, [key ?? "?"]: (out[key ?? "?"] || 0) + 1 }), {});
  console.log(JSON.stringify({ format: "ac.app-opens.v1", start, end: new Date(),
    unit: "active = installs that opened that day; fresh = first launch of a new install (includes reinstalls)",
    rows: rows.map(({ platforms, versions, countries, ...row }) => ({ ...row,
      platforms: tally(platforms), versions: tally(versions), countries: tally(countries) })),
  }, null, 2));
} finally { await closePool(); }
