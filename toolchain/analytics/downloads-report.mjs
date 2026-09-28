#!/usr/bin/env node
// Run on Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/downloads-report.mjs --days 30 --exclude <hash>,<hash>
// Direct DMG downloads counted by /api/download (system/netlify/functions/download.mjs).
import { connect, closePool } from "../../system/backend/database.mjs";
const args = process.argv.slice(2);
const value = name => args[args.indexOf(name) + 1];
const days = args.includes("--days") ? Number(value("--days")) : 30;
if (!Number.isFinite(days) || days <= 0 || days > 3650) throw new Error("Use --days 1..3650");
const exclude = args.includes("--exclude") ? value("--exclude").split(",").filter(Boolean) : [];
const start = new Date(Date.now() - days * 86400000);
const { db } = await connect();
try {
  const rows = await db.collection("downloads").aggregate([
    { $match: { at: { $gte: start } } },
    { $group: {
      _id: { app: "$app", version: "$version", automated: "$automated", self: { $in: ["$hash", exclude] } },
      downloads: { $sum: 1 }, places: { $addToSet: "$hash" },
      countries: { $push: "$country" }, platforms: { $push: "$platform" },
      first: { $min: "$at" }, last: { $max: "$at" },
    } },
  ], { maxTimeMS: 20000 }).toArray();
  const first = await db.collection("downloads").find({}, { projection: { at: 1 } }).sort({ at: 1 }).limit(1).next();
  const tally = list => list.reduce((out, key) => ({ ...out, [key ?? "?"]: (out[key ?? "?"] || 0) + 1 }), {});
  console.log(JSON.stringify({ format: "ac.downloads.v1", start, end: new Date(),
    earliestDownload: first?.at || null, excluded: exclude.length,
    rows: rows.map(({ places, countries, platforms, ...row }) => ({ ...row,
      places: places.length, countries: tally(countries), platforms: tally(platforms) })),
  }, null, 2));
} finally { await closePool(); }
