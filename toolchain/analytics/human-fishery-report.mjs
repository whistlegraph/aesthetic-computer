#!/usr/bin/env node
// On Lith: node --env-file=.env ../toolchain/analytics/human-fishery-report.mjs '{"minutes":5}'
import { connect, closePool } from "../../system/backend/database.mjs";
import { fisheryOptions, fisherySnapshot, fisheryPipeline } from "./human-fishery.mjs";

const options = fisheryOptions(JSON.parse(process.argv[2] || "{}"));
const { db } = await connect();
try {
  const rows = await db.collection("_firehose").aggregate(fisheryPipeline(options), { maxTimeMS: 10000 }).toArray();
  console.log(JSON.stringify(fisherySnapshot(rows, options)));
} finally { await closePool(); }
