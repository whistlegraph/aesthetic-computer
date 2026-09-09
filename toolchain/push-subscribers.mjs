#!/usr/bin/env node
// push-subscribers, 26.09.09
// Who is listening. Counts the live rows in the Mongo `push-tokens`
// collection — the ones shared/push.mjs will actually fan out to — grouped
// by topic, with devices and distinct people counted separately (one person
// with a laptop and a phone is two devices, one subscriber).
//
//   node toolchain/push-subscribers.mjs              # every topic
//   node toolchain/push-subscribers.mjs sotce-pages  # one topic, with devices
//
// Legacy FCM-era rows (no `kind`) are ignored here exactly as the sender
// ignores them, so this count is what a broadcast would really reach.

import { MongoClient } from "mongodb";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const LIVE = { kind: { $in: ["webpush", "apns"] } };

// The connection string lives in the vault, not the environment, on a
// workstation — read it the way the dev container would.
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

const only = process.argv[2];
const client = new MongoClient(uri, { serverSelectionTimeoutMS: 15000 });

try {
  await client.connect();
  const devices = client.db(name).collection("push-tokens");

  const rows = await devices
    .aggregate([
      { $match: LIVE },
      { $unwind: "$topics" },
      ...(only ? [{ $match: { topics: only } }] : []),
      {
        $group: {
          _id: "$topics",
          devices: { $sum: 1 },
          people: { $addToSet: "$user" },
        },
      },
      { $project: { devices: 1, people: { $size: "$people" } } },
      { $sort: { devices: -1 } },
    ])
    .toArray();

  const total = await devices.countDocuments(LIVE);
  console.log(`🔔 ${total} live device${total === 1 ? "" : "s"} registered\n`);

  if (!rows.length) {
    console.log(only ? `no devices on "${only}"` : "no topics subscribed");
  } else {
    console.log("topic            devices  subscribers");
    for (const row of rows) {
      console.log(
        `${row._id.padEnd(16)} ${String(row.devices).padStart(7)}  ${String(row.people).padStart(11)}`,
      );
    }
  }

  // One topic named: name its devices too, so a small list stays legible.
  if (only) {
    const list = await devices
      .find({ ...LIVE, topics: only })
      .project({ _id: 0, label: 1, platform: 1, kind: 1, updatedAt: 1 })
      .sort({ updatedAt: -1 })
      .toArray();
    console.log("");
    for (const device of list) {
      const when = device.updatedAt
        ? new Date(device.updatedAt).toISOString().slice(0, 10)
        : "?";
      console.log(
        `  ${device.kind}/${device.platform}  ${device.label || "(unlabeled)"}  · ${when}`,
      );
    }
  }
} finally {
  await client.close();
}
