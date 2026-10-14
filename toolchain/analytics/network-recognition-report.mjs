#!/usr/bin/env node
// Private SSH/MCP report. No public endpoint exposes recognition results.
import { connect, closePool } from "../../system/backend/database.mjs";
import { networkRecognitionReport, recognitionReportOptions } from "./network-recognition.mjs";

const options = JSON.parse(process.argv[2] || "{}");
recognitionReportOptions(options);
const { db } = await connect();
try { console.log(JSON.stringify(await networkRecognitionReport(db, options))); }
finally { await closePool(); }
