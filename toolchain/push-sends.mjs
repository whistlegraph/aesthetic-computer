#!/usr/bin/env node
// push-sends, 26.11.24
// What went out. Reads the Mongo `push-sends` ledger that shared/push.mjs
// writes on every send (topic broadcast, a person's devices, an app-registry
// target) and totals a window: sends, devices reached, by route and channel.
// The ledger holds no titles or bodies, so neither does this report.
//
//   node toolchain/push-sends.mjs        # the last 24 hours
//   node toolchain/push-sends.mjs 168    # the last week
//
// Companion to toolchain/push-subscribers.mjs (who is listening).

import { MongoClient } from "mongodb";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

function loadEnv(file) {
  const full = path.join(ROOT, file);
  if (!fs.existsSync(full)) return;
  for (const line of fs.readFileSync(full, "utf8").split("\n")) {
    const match = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)\s*=\s*(.*)$/);
    if (!match) continue;
    let value = match[2].trim();
    const quoted = value.startsWith('"') && value.endsWith('"');
    const ticked = value.startsWith("'") && value.endsWith("'");
    if (quoted || ticked) value = value.slice(1, -1);
    process.env[match[1]] ??= value;
  }
}

loadEnv("vault/.devcontainer/envs/devcontainer.env");

const { MONGODB_CONNECTION_STRING: uri, MONGODB_NAME: name } = process.env;
if (!uri) {
  console.error("🔴 MONGODB_CONNECTION_STRING missing — is the vault mounted?");
  process.exit(1);
}

const hours = Number(process.argv[2] || 24);
if (!(hours > 0)) {
  console.error("Usage: node toolchain/push-sends.mjs [hours]");
  process.exit(1);
}
const since = new Date(Date.now() - hours * 3600 * 1000);
const client = new MongoClient(uri, { serverSelectionTimeoutMS: 15000 });

const route = (row) =>
  row.via === "topic" ? `topic ${row.topic}`
    : row.via === "target" ? `app ${row.app || "any"}${row.topic ? ` / ${row.topic}` : ""}`
      : `person${row.kind ? ` (${row.kind})` : ""}`;

try {
  await client.connect();
  const rows = await client.db(name).collection("push-sends")
    .find({ at: { $gte: since } }).sort({ at: 1 }).toArray();

  const total = { sends: 0, attempted: 0, succeeded: 0, failed: 0, pruned: 0, unreached: 0 };
  const channels = {};
  const routes = new Map();
  const people = new Set();
  for (const row of rows) {
    total.sends++;
    for (const k of ["attempted", "succeeded", "failed", "pruned"]) total[k] += row[k] || 0;
    if (!row.attempted) total.unreached++;
    for (const [k, n] of Object.entries(row.channels || {})) channels[k] = (channels[k] || 0) + n;
    if (row.user) people.add(row.user);
    const key = route(row);
    const r = routes.get(key) || { sends: 0, succeeded: 0, failed: 0 };
    r.sends++; r.succeeded += row.succeeded || 0; r.failed += row.failed || 0;
    routes.set(key, r);
  }

  console.log(`🔔 Push sends, last ${hours} h (since ${since.toISOString()})\n`);
  if (!rows.length) {
    console.log("  None recorded. (The ledger only counts sends after it was deployed.)");
  } else {
    console.log(`  sends ${total.sends}   devices delivered ${total.succeeded} / ${total.attempted}   failed ${total.failed}   pruned ${total.pruned}`);
    console.log(`  sends that reached no device ${total.unreached}   distinct recipients ${people.size}`);
    console.log(`  channels ${Object.entries(channels).map(([k, n]) => `${k} ${n}`).join("   ") || "—"}`);
    console.log(`  first ${rows[0].at.toISOString()}   last ${rows.at(-1).at.toISOString()}\n`);
    const width = Math.max(...[...routes.keys()].map((k) => k.length));
    for (const [key, r] of [...routes].sort((a, b) => b[1].sends - a[1].sends)) {
      console.log(`  ${key.padEnd(width)}  ${String(r.sends).padStart(5)} sends  ${String(r.succeeded).padStart(5)} delivered  ${String(r.failed).padStart(4)} failed`);
    }
  }
} finally {
  await client.close();
}
