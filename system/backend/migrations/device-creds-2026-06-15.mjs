// device-creds-2026-06-15.mjs
//
// One-off migration: move per-user OS provisioning secrets (Claude OAuth
// token + GitHub PAT) OUT of the `@handles` documents (and the vestigial
// `device-tokens` collection) into the dedicated `device-creds` collection
// keyed by Auth0 sub. After this runs, the secrets no longer ride along on
// the handle docs that get $lookup-joined into public content.
//
// Usage (from anywhere inside the repo):
//   node system/backend/migrations/device-creds-2026-06-15.mjs           # dry run
//   node system/backend/migrations/device-creds-2026-06-15.mjs --apply   # write
//
// Idempotent: re-running after --apply finds nothing left to move.

import { MongoClient } from "mongodb";
import { readFileSync } from "fs";
import { fileURLToPath } from "url";
import path from "path";

const APPLY = process.argv.includes("--apply");

// Load Mongo creds from the devcontainer env (same source the dev stack uses).
const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, "../../..");
const envPath = path.join(
  repoRoot,
  "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env",
);
const env = {};
for (const line of readFileSync(envPath, "utf8").split("\n")) {
  const m = line.match(/^([A-Z0-9_]+)\s*=\s*(.*)$/);
  if (m) env[m[1]] = m[2].replace(/^["']|["']$/g, "");
}
const uri = env.MONGODB_CONNECTION_STRING;
const dbName = env.MONGODB_NAME;
if (!uri || !dbName) {
  console.error("Missing MONGODB_CONNECTION_STRING / MONGODB_NAME");
  process.exit(1);
}

const client = new MongoClient(uri);
await client.connect();
const db = client.db(dbName);
const creds = db.collection("device-creds");

console.log(`\n=== device-creds migration (${APPLY ? "APPLY" : "DRY RUN"}) ===\n`);

let movedFromHandles = 0;
let movedFromDeviceTokens = 0;

// --- Source A: @handles.{claudeCodeToken,githubPat} (the live store) ---
const handleDocs = await db
  .collection("@handles")
  .find({
    $or: [
      { claudeCodeToken: { $exists: true } },
      { githubPat: { $exists: true } },
    ],
  })
  .project({
    handle: 1,
    claudeCodeToken: 1,
    claudeCodeTokenUpdated: 1,
    githubPat: 1,
    githubPatUpdated: 1,
  })
  .toArray();

for (const h of handleDocs) {
  const set = {};
  if (h.claudeCodeToken) {
    set.claudeToken = h.claudeCodeToken;
    set.claudeTokenUpdated = h.claudeCodeTokenUpdated || new Date();
  }
  if (h.githubPat) {
    set.githubPat = h.githubPat;
    set.githubPatUpdated = h.githubPatUpdated || new Date();
  }
  if (Object.keys(set).length === 0) continue;
  console.log(
    `  @${h.handle} (${h._id}) → device-creds: ${Object.keys(set)
      .filter((k) => !k.endsWith("Updated"))
      .join(", ")}`,
  );
  movedFromHandles++;
  if (APPLY) {
    await creds.updateOne({ _id: h._id }, { $set: set }, { upsert: true });
    await db.collection("@handles").updateOne(
      { _id: h._id },
      {
        $unset: {
          claudeCodeToken: "",
          claudeCodeTokenUpdated: "",
          githubPat: "",
          githubPatUpdated: "",
        },
      },
    );
  }
}

// --- Source B: vestigial device-tokens collection (fill only gaps) ---
const tokenDocs = await db
  .collection("device-tokens")
  .find({
    $or: [
      { claudeToken: { $exists: true } },
      { githubPat: { $exists: true } },
    ],
  })
  .toArray()
  .catch(() => []);

for (const t of tokenDocs) {
  const existing = await creds.findOne({ _id: t._id });
  const set = {};
  if (t.claudeToken && !existing?.claudeToken) {
    set.claudeToken = t.claudeToken;
    set.claudeTokenUpdated = t.claudeTokenUpdated || new Date();
  }
  if (t.githubPat && !existing?.githubPat) {
    set.githubPat = t.githubPat;
    set.githubPatUpdated = t.githubPatUpdated || new Date();
  }
  if (Object.keys(set).length === 0) continue;
  console.log(`  device-tokens ${t._id} → device-creds (gap fill)`);
  movedFromDeviceTokens++;
  if (APPLY) {
    await creds.updateOne({ _id: t._id }, { $set: set }, { upsert: true });
  }
}

const total = await creds.countDocuments();
console.log(
  `\nFrom @handles: ${movedFromHandles} | from device-tokens: ${movedFromDeviceTokens}`,
);
console.log(`device-creds now holds ${total} doc(s).`);
if (!APPLY) console.log("\n(dry run — nothing written. Re-run with --apply.)");

await client.close();
