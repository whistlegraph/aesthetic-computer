#!/usr/bin/env node
// On Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/ios-usage-report.mjs --days 7
import { connect, closePool } from "../../system/backend/database.mjs";
import { nativeUsageReport } from "../../system/backend/native-usage.mjs";
const args = process.argv.slice(2), i = args.indexOf("--days");
const days = i < 0 ? 7 : Number(args[i + 1]);
if (!Number.isInteger(days) || days < 1 || days > 35) throw new Error("Use --days 1..35");
const end = new Date(), start = new Date(end.toISOString().slice(0, 10));
start.setUTCDate(start.getUTCDate() - days + 1);
try { const { db } = await connect(); console.log(JSON.stringify(await nativeUsageReport(db, start, end), null, 2)); }
finally { await closePool(); }
