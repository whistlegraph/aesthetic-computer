#!/usr/bin/env node
// Rotate profile-stream secret, store in vault env, and upsert MongoDB secrets.

import crypto from "crypto";
import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { MongoClient } from "mongodb";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT = path.resolve(__dirname, "..");
const DEFAULT_VAULT_ENV = path.resolve(
  ROOT,
  "aesthetic-computer-vault/session-server/.env",
);

function parseArgs(argv) {
  const out = {
    bytes: 32,
    dryRun: false,
    noVaultWrite: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const next = argv[i + 1];

    if (arg === "--vault-env" && next) {
      out.vaultEnvPath = path.resolve(process.cwd(), next);
      i += 1;
      continue;
    }
    if (arg === "--secret" && next) {
      out.secret = next;
      i += 1;
      continue;
    }
    if (arg === "--bytes" && next) {
      out.bytes = Number(next) || 32;
      i += 1;
      continue;
    }
    if (arg === "--mongodb-uri" && next) {
      out.mongoUri = next;
      i += 1;
      continue;
    }
    if (arg === "--db-name" && next) {
      out.dbName = next;
      i += 1;
      continue;
    }
    if (arg === "--dry-run") {
      out.dryRun = true;
      continue;
    }
    if (arg === "--no-vault-write") {
      out.noVaultWrite = true;
      continue;
    }
    if (arg === "--help" || arg === "-h") {
      out.help = true;
      continue;
    }
  }

  return out;
}

function helpText() {
  return [
    "rotate-profile-stream-secret.mjs",
    "",
    "Rotates profile-stream secret and writes it to:",
    "1) MongoDB secrets collection (_id: profile-stream)",
    "2) aesthetic-computer-vault/session-server/.env",
    "",
    "Usage:",
    "  node utilities/rotate-profile-stream-secret.mjs [options]",
    "",
    "Options:",
    "  --vault-env <path>     Override vault env path",
    "  --mongodb-uri <uri>    Override MongoDB connection string",
    "  --db-name <name>       Override MongoDB database name",
    "  --secret <value>       Use explicit secret value (skip generation)",
    "  --bytes <n>            Random byte length (default: 32)",
    "  --no-vault-write       Do not update vault .env",
    "  --dry-run              Print actions without writing",
  ].join("\n");
}

function parseEnvText(text) {
  const out = {};
  const lines = `${text || ""}`.split(/\r?\n/);
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith("#")) continue;
    const match = trimmed.match(/^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (!match) continue;
    const key = match[1];
    let value = match[2] || "";
    value = value.replace(/^["']|["']$/g, "");
    out[key] = value;
  }
  return out;
}

function quoteEnvValue(value) {
  return `"${`${value}`.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
}

function upsertEnvVar(text, key, value) {
  const lines = `${text || ""}`.split(/\r?\n/);
  const index = lines.findIndex((line) =>
    new RegExp(`^\\s*${key}\\s*=`).test(line),
  );
  const rendered = `${key}=${quoteEnvValue(value)}`;
  if (index >= 0) {
    lines[index] = rendered;
  } else {
    if (lines.length > 0 && lines[lines.length - 1] !== "") lines.push("");
    lines.push(rendered);
  }
  if (lines[lines.length - 1] !== "") lines.push("");
  return lines.join("\n");
}

function mask(value) {
  if (!value) return "(empty)";
  const text = `${value}`;
  if (text.length <= 8) return "*".repeat(text.length);
  return `${text.slice(0, 4)}...${text.slice(-4)}`;
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  if (args.help) {
    console.log(helpText());
    return;
  }

  const vaultEnvPath = args.vaultEnvPath || DEFAULT_VAULT_ENV;
  const vaultText = await fs.readFile(vaultEnvPath, "utf8");
  const vaultEnv = parseEnvText(vaultText);

  const mongoUri =
    args.mongoUri ||
    process.env.MONGODB_CONNECTION_STRING ||
    vaultEnv.MONGODB_CONNECTION_STRING;
  const dbName = args.dbName || process.env.MONGODB_NAME || vaultEnv.MONGODB_NAME;

  if (!mongoUri) {
    throw new Error("Missing MongoDB connection string.");
  }
  if (!dbName) {
    throw new Error("Missing MongoDB database name.");
  }

  const secret =
    args.secret ||
    crypto.randomBytes(Math.max(16, args.bytes || 32)).toString("hex");
  const now = new Date();

  console.log("🔐 Profile stream secret rotation");
  console.log(`   vault env: ${vaultEnvPath}`);
  console.log(`   secret: ${mask(secret)}`);
  if (args.dryRun) {
    console.log("   mode: dry-run");
  }

  if (!args.noVaultWrite) {
    const updatedVaultText = upsertEnvVar(vaultText, "PROFILE_STREAM_SECRET", secret);
    if (!args.dryRun) {
      await fs.writeFile(vaultEnvPath, updatedVaultText, "utf8");
    }
    console.log(`✅ Vault updated (${args.dryRun ? "skipped write" : "written"})`);
  } else {
    console.log("⏭️  Vault update skipped (--no-vault-write)");
  }

  if (!args.dryRun) {
    const client = new MongoClient(mongoUri, {
      serverSelectionTimeoutMS: 10000,
      connectTimeoutMS: 10000,
    });
    try {
      await client.connect();
      const db = client.db(dbName);
      await db.collection("secrets").updateOne(
        { _id: "profile-stream" },
        {
          $set: {
            secret,
            updatedAt: now,
            updatedBy: process.env.USER || process.env.LOGNAME || "unknown",
          },
          $setOnInsert: {
            createdAt: now,
          },
        },
        { upsert: true },
      );

      const verify = await db
        .collection("secrets")
        .findOne({ _id: "profile-stream" }, { projection: { secret: 1 } });
      if (!verify || `${verify.secret || ""}` !== secret) {
        throw new Error("Secret verification failed after MongoDB write.");
      }
    } finally {
      await client.close();
    }
  }

  console.log(`✅ MongoDB secrets/profile-stream updated (${args.dryRun ? "dry-run" : "written"})`);
}

main().catch((err) => {
  console.error("❌ rotate-profile-stream-secret failed:", err?.message || err);
  process.exitCode = 1;
});
