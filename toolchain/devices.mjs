#!/usr/bin/env node
// devices, 2026.10.08
// The AC network device registry (Mongo `app-devices`, shared/app-devices.mjs):
// which person runs which build on which device, and who can be notified.
//
//   node toolchain/devices.mjs                        # every app: devices, people, builds
//   node toolchain/devices.mjs whistlegraph           # one app, newest devices first
//   node toolchain/devices.mjs whistlegraph @dreamdeal # one person's devices
//   node toolchain/devices.mjs whistlegraph --build 114
import { MongoClient } from "mongodb";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { APPS } from "../shared/app-registry.mjs";
import { APP_DEVICES } from "../shared/app-devices.mjs";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
// The connection string lives in the vault on a workstation, as in push-subscribers.mjs.
for (const file of ["vault/.devcontainer/envs/devcontainer.env"]) {
  const full = path.join(ROOT, file);
  if (!fs.existsSync(full)) continue;
  for (const line of fs.readFileSync(full, "utf8").split("\n")) {
    const match = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)\s*=\s*(.*)$/);
    if (match) process.env[match[1]] ??= match[2].trim().replace(/^(["'])(.*)\1$/, "$2");
  }
}
const { MONGODB_CONNECTION_STRING: uri, MONGODB_NAME: name } = process.env;
if (!uri) { console.error("🔴 MONGODB_CONNECTION_STRING missing — is the vault mounted?"); process.exit(1); }

const args = process.argv.slice(2);
const app = args.find(a => Object.hasOwn(APPS, a));
const handle = args.find(a => a.startsWith("@"))?.slice(1);
const build = args.includes("--build") ? args[args.indexOf("--build") + 1] : undefined;
const ago = date => {
  if (!date) return "—";
  const minutes = Math.round((Date.now() - new Date(date)) / 60000);
  return minutes < 60 ? `${minutes}m` : minutes < 2880 ? `${Math.round(minutes / 60)}h` : `${Math.round(minutes / 1440)}d`;
};

const client = new MongoClient(uri);
try {
  await client.connect();
  const devices = client.db(name).collection(APP_DEVICES);
  if (!app) {
    const rows = await devices.aggregate([
      { $group: { _id: "$app", devices: { $sum: 1 }, people: { $addToSet: "$user" },
        pushable: { $sum: { $cond: [{ $ifNull: ["$push", false] }, 1, 0] } }, builds: { $addToSet: "$build" } } },
      { $sort: { devices: -1 } },
    ]).toArray();
    if (!rows.length) console.log("No devices registered yet.");
    for (const r of rows) console.log(`${r._id.padEnd(18)} ${String(r.devices).padStart(4)} devices  ${String(r.people.filter(Boolean).length).padStart(4)} people  ${String(r.pushable).padStart(4)} pushable  builds ${r.builds.filter(Boolean).sort((a, b) => b - a).join(",")}`);
  } else {
    const filter = { app, ...(handle ? { handle } : {}), ...(build ? { build: String(build) } : {}) };
    const rows = await devices.find(filter).sort({ lastSeenAt: -1 }).limit(200).toArray();
    if (!rows.length) console.log("No matching devices.");
    for (const d of rows) console.log([
      (d.handle ? "@" + d.handle : "(signed out)").padEnd(18), `build ${d.build || "?"}`.padEnd(10),
      `${d.model || "?"} ${d.platform} ${d.os || ""}`.padEnd(28), `opened ${ago(d.lastOpenAt)} ago`.padEnd(16),
      `${d.opens || 0} opens`.padEnd(10), d.push ? `push:${d.push.kind}` : "no push", d.deviceId,
    ].join("  "));
  }
} finally {
  await client.close();
}
