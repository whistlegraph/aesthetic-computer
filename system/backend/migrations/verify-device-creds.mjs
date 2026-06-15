// Verify the device-creds migration: secrets present in device-creds, gone from @handles.
import { MongoClient } from "mongodb";
import { readFileSync } from "fs";
import { fileURLToPath } from "url";
import path from "path";

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, "../../..");
const env = {};
for (const line of readFileSync(
  path.join(repoRoot, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env"),
  "utf8",
).split("\n")) {
  const m = line.match(/^([A-Z0-9_]+)\s*=\s*(.*)$/);
  if (m) env[m[1]] = m[2].replace(/^["']|["']$/g, "");
}
const client = new MongoClient(env.MONGODB_CONNECTION_STRING);
await client.connect();
const db = client.db(env.MONGODB_NAME);
const sub = "auth0|63effeeb2a7d55f8098d62f9"; // @jeffrey

const h = await db.collection("@handles").findOne({ _id: sub });
console.log("@handles doc fields now:", Object.keys(h).sort().join(", "));
console.log("  still has claudeCodeToken?", "claudeCodeToken" in h);
console.log("  still has githubPat?      ", "githubPat" in h);

const c = await db.collection("device-creds").findOne({ _id: sub });
console.log("\ndevice-creds doc fields:", Object.keys(c).sort().join(", "));
console.log("  claudeToken present?", !!c.claudeToken, "(len", c.claudeToken?.length, ")");
console.log("  githubPat present?  ", !!c.githubPat, "(len", c.githubPat?.length, ")");

// Confirm NO @handles doc anywhere still carries a secret.
const leftover = await db.collection("@handles").countDocuments({
  $or: [{ claudeCodeToken: { $exists: true } }, { githubPat: { $exists: true } }],
});
console.log("\n@handles docs still carrying secrets:", leftover);

await client.close();
