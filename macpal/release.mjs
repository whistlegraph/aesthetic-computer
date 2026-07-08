#!/usr/bin/env node
// release.mjs — publish a MacPal release for over-the-air self-update.
//
// Every installed pal (Fía's star, the neo/blueberry badges) polls
//   https://releases.aesthetic.computer/macpal/latest.json
// hourly via UpdatePlugin.swift and swaps itself for anything newer — so
// shipping a build is just:
//
//   node macpal/release.mjs              # package.sh (sign+notarize) → upload
//   node macpal/release.mjs --no-package # reuse an existing dist/MacPal.zip
//   node macpal/release.mjs --dry-run    # show what would upload
//
// The version comes from Resources/Info.plist (bump CFBundleShortVersionString
// before releasing). The zip lands at macpal/MacPal-<version>.zip (immutable),
// the manifest at macpal/latest.json (no-cache), both public-read on the
// releases-aesthetic-computer Space — the same bucket the desktop app uses.
//
// Credentials: SPACES_KEY / SPACES_SECRET / SPACES_ENDPOINT from the vault
// silo/.env (same file ac-electron's publish-release.mjs reads).

import fs from "fs";
import path from "path";
import { createHash } from "crypto";
import { execSync } from "child_process";
import { fileURLToPath } from "url";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const noPackage = args.includes("--no-package");

// ── creds (vault silo/.env, or the environment) ────────────────────────────
for (const envPath of [
  path.join(__dirname, "..", "..", "aesthetic-computer-vault", "silo", ".env"),
  path.join(__dirname, "..", "silo", ".env"),
]) {
  if (!fs.existsSync(envPath)) continue;
  for (const line of fs.readFileSync(envPath, "utf8").split("\n")) {
    const m = line.match(/^([A-Z_]+)=(.*)$/);
    if (m && !process.env[m[1]]) process.env[m[1]] = m[2].replace(/^["']|["']$/g, "");
  }
  break;
}

// ── version from Info.plist ─────────────────────────────────────────────────
const plist = fs.readFileSync(path.join(__dirname, "Resources", "Info.plist"), "utf8");
const version = plist.match(
  /CFBundleShortVersionString<\/key>\s*<string>([^<]+)<\/string>/,
)?.[1];
if (!version) {
  console.error("✗ couldn't read CFBundleShortVersionString from Resources/Info.plist");
  process.exit(1);
}

// ── package (sign + notarize) unless told not to ────────────────────────────
const zipPath = path.join(__dirname, "dist", "MacPal.zip");
if (!noPackage) {
  console.log(`› packaging v${version} (sign + notarize — a few minutes)`);
  execSync("./package.sh", { cwd: __dirname, stdio: "inherit" });
} else if (!fs.existsSync(zipPath)) {
  console.error("✗ --no-package but dist/MacPal.zip doesn't exist");
  process.exit(1);
}

const zip = fs.readFileSync(zipPath);
const sha256 = createHash("sha256").update(zip).digest("hex");

const BUCKET = "releases-aesthetic-computer";
const key = `macpal/MacPal-${version}.zip`;
const url = `https://releases.aesthetic.computer/${key}`;
const manifest = {
  version,
  url,
  sha256,
  size: zip.length,
  at: new Date().toISOString(),
};

console.log(`› MacPal v${version} — ${(zip.length / 1024).toFixed(0)} KB, sha256 ${sha256.slice(0, 12)}…`);
if (dryRun) {
  console.log("  [dry-run]", JSON.stringify(manifest, null, 2));
  process.exit(0);
}

const s3 = new S3Client({
  endpoint: process.env.SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com",
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.SPACES_KEY || "",
    secretAccessKey: process.env.SPACES_SECRET || "",
  },
  requestChecksumCalculation: "WHEN_REQUIRED",
  responseChecksumValidation: "WHEN_REQUIRED",
});

console.log(`› uploading ${key}`);
await s3.send(new PutObjectCommand({
  Bucket: BUCKET,
  Key: key,
  Body: zip,
  ContentType: "application/zip",
  CacheControl: "public, max-age=31536000", // versioned name → immutable
  ACL: "public-read",
}));

console.log("› uploading macpal/latest.json");
await s3.send(new PutObjectCommand({
  Bucket: BUCKET,
  Key: "macpal/latest.json",
  Body: JSON.stringify(manifest, null, 2),
  ContentType: "application/json",
  CacheControl: "no-cache, no-store, must-revalidate",
  ACL: "public-read",
}));

console.log(`✓ published — pals pick it up within the hour`);
console.log(`  ${url}`);
console.log(`  https://releases.aesthetic.computer/macpal/latest.json`);
