// One-off migration: convert kidlisp_hash_unique from a plain unique index to a
// PARTIAL unique index ({ hash: $type string }) so rows lacking a real hash
// don't all collide on { hash: null }. Idempotent. Reads creds from a .env file.
//
// Usage: node tezos/migrate-kidlisp-hash-index.mjs [path/to/.env]

import { readFileSync } from "fs";
import { MongoClient } from "mongodb";

const envPath =
  process.argv[2] || "aesthetic-computer-vault/lith/.env";

// Minimal .env parser (handles KEY=value, ignores comments/blank lines).
for (const line of readFileSync(envPath, "utf8").split("\n")) {
  const m = line.match(/^\s*([A-Z0-9_]+)\s*=\s*(.*)\s*$/);
  if (!m) continue;
  let val = m[2];
  if (
    (val.startsWith('"') && val.endsWith('"')) ||
    (val.startsWith("'") && val.endsWith("'"))
  ) {
    val = val.slice(1, -1);
  }
  if (!(m[1] in process.env)) process.env[m[1]] = val;
}

const uri = process.env.MONGODB_CONNECTION_STRING;
const dbName = process.env.MONGODB_NAME;
if (!uri || !dbName) {
  console.error("Missing MONGODB_CONNECTION_STRING / MONGODB_NAME in", envPath);
  process.exit(1);
}

const client = new MongoClient(uri);
await client.connect();
const col = client.db(dbName).collection("kidlisp");

const nullHashCount = await col.countDocuments({
  $or: [{ hash: null }, { hash: { $exists: false } }],
});
console.log(`📊 Docs with null/missing hash: ${nullHashCount}`);

const before = await col.indexes();
const existing = before.find((i) => i.name === "kidlisp_hash_unique");
console.log("🔎 Current kidlisp_hash_unique:", JSON.stringify(existing));

const isPartial = existing?.partialFilterExpression;
if (existing && !isPartial) {
  console.log("♻️  Dropping plain-unique kidlisp_hash_unique…");
  await col.dropIndex("kidlisp_hash_unique");
}

if (!existing || !isPartial) {
  console.log("🧱 Creating partial unique index…");
  await col.createIndex(
    { hash: 1 },
    {
      unique: true,
      background: true,
      name: "kidlisp_hash_unique",
      partialFilterExpression: { hash: { $type: "string" } },
    },
  );
  console.log("✅ Partial unique index created.");
} else {
  console.log("✅ Index already partial — nothing to do.");
}

const after = await col.indexes();
console.log(
  "🔎 After:",
  JSON.stringify(after.find((i) => i.name === "kidlisp_hash_unique")),
);

await client.close();
console.log("🏁 Done.");
