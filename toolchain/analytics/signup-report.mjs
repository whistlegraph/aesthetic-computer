#!/usr/bin/env node
// On Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/signup-report.mjs --days 35
import { connect } from "../../system/backend/database.mjs";
import { readSignupAccounts, signupReport } from "../../system/backend/signup-report.mjs";

const args = process.argv.slice(2);
let days = 35, end = new Date();
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--days") days = Number(args[++i]);
  else if (args[i] === "--end") end = new Date(args[++i]);
  else throw new Error("Usage: signup-report.mjs [--days 1..365] [--end ISO-date]");
}
if (!Number.isFinite(days) || days < 1 || days > 365 || !Number.isFinite(+end)) throw new Error("Invalid report window");
const start = new Date(+end - days * 86400000);
try {
  const accounts = await readSignupAccounts({ start, end });
  const database = await connect();
  try { console.log(JSON.stringify(await signupReport({ db: database.db, accounts, start, end }), null, 2)); }
  finally { await database.disconnect(); }
  process.exit(0);
} catch (error) { console.error(error.message); process.exit(1); }
