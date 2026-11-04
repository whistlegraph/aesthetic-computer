#!/usr/bin/env node
// Read-only: run from /opt/ac/system with node --env-file=.env.
// No Paint: classic saves, teacher notes, the sticker split and the notepaint
// funnel. The report itself lives in nopaint.mjs (shared with the analytics MCP).
import { connect, closePool } from "../../system/backend/database.mjs";
import { nopaintReport, nopaintReportOptions } from "./nopaint.mjs";

const args = process.argv.slice(2);
const value = name => args[args.indexOf(name) + 1];
const options = nopaintReportOptions({ hours: args.includes("--hours") ? value("--hours") : 24,
  end: args.includes("--end") ? value("--end") : undefined });
const { db } = await connect();
try { console.log(JSON.stringify(await nopaintReport(db, options), null, 2)); }
finally { await closePool(); }
