#!/usr/bin/env node
// notify, 2026.10.08
// Send an AC network notification through the device registry
// (shared/push.mjs sendToTarget → Mongo `app-devices`).
//
//   node toolchain/notify.mjs whistlegraph @dreamdeal "Title" "Body"
//   node toolchain/notify.mjs whistlegraph --topic testers "Title" "Body"
//   node toolchain/notify.mjs whistlegraph --device <deviceId> "Title" "Body"
//   … --url /wgZuhus   open this path when tapped
//   … --dry            list who would receive it, send nothing
import { MongoClient } from "mongodb";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { APPS } from "../shared/app-registry.mjs";
import { APP_DEVICES, targetFilter } from "../shared/app-devices.mjs";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
// Mongo from the devcontainer env; APNs and VAPID keys from lith's env.
for (const file of ["vault/.devcontainer/envs/devcontainer.env", "vault/lith/.env"]) {
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
const take = flag => { const i = args.indexOf(flag); if (i < 0) return undefined; const [, v] = args.splice(i, 2); return v; };
const dry = args.includes("--dry") && !!args.splice(args.indexOf("--dry"), 1);
const topic = take("--topic"), deviceId = take("--device"), url = take("--url");
const app = args.shift();
if (!Object.hasOwn(APPS, app ?? "")) { console.error(`Usage: notify <${Object.keys(APPS).join("|")}> (@handle | --topic t | --device id) "Title" "Body"`); process.exit(1); }
const handle = args[0]?.startsWith("@") ? args.shift().slice(1) : undefined;
const [title, body = ""] = args;
if (!title || [handle, topic, deviceId].filter(Boolean).length !== 1) { console.error("Give exactly one of @handle, --topic or --device, then a title."); process.exit(1); }

const client = new MongoClient(uri);
try {
  await client.connect();
  const db = client.db(name);
  let target = topic ? { app, topic } : deviceId ? { app, deviceId } : null;
  if (handle) {
    const row = await db.collection(APP_DEVICES).findOne({ app, handle });
    if (!row?.user) { console.error(`No ${app} device signed in as @${handle}.`); process.exit(1); }
    target = { app, user: row.user };
  }
  const rows = await db.collection(APP_DEVICES).find({ ...targetFilter(target), push: { $exists: true } })
    .project({ handle: 1, deviceId: 1, model: 1, build: 1, "push.kind": 1, "push.env": 1 }).toArray();
  console.log(`${rows.length} reachable device(s):`);
  for (const r of rows) console.log(`  ${r.handle ? "@" + r.handle : "(signed out)"}  ${r.model || "?"}  build ${r.build || "?"}  ${r.push.kind}/${r.push.env || "web"}  ${r.deviceId}`);
  if (dry || !rows.length) process.exit(0);
  const { sendToTarget } = await import("../shared/push.mjs");
  const started = performance.now();
  const summary = await sendToTarget(db, target, { title, body, data: url ? { url } : undefined });
  console.log(JSON.stringify({ ...summary, ms: Math.round(performance.now() - started) }));
} finally {
  await client.close();
}
