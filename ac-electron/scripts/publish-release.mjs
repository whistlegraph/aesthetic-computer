#!/usr/bin/env node
// Publish Electron release to DigitalOcean Spaces + register with silo.
//
// Usage:
//   node scripts/publish-release.mjs [--notes "Release notes"] [--dry-run]
//
// Requires env vars (from silo/.env or environment):
//   SPACES_KEY, SPACES_SECRET, SPACES_ENDPOINT
//   SILO_URL (default: https://silo.aesthetic.computer)
//   PUBLISH_SECRET (shared secret for silo admin API)

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";
import { Upload } from "@aws-sdk/lib-storage";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const distDir = path.join(__dirname, "..", "dist");
const pkgPath = path.join(__dirname, "..", "package.json");

// Load silo/.env if available
const siloEnvPath = path.join(__dirname, "..", "..", "silo", ".env");
if (fs.existsSync(siloEnvPath)) {
  for (const line of fs.readFileSync(siloEnvPath, "utf8").split("\n")) {
    const match = line.match(/^([A-Z_]+)=(.*)$/);
    if (match && !process.env[match[1]]) {
      process.env[match[1]] = match[2].replace(/^["']|["']$/g, "");
    }
  }
}

const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf8"));
const version = pkg.version;

const BUCKET = "releases-aesthetic-computer";
const PREFIX = "desktop/";
const BASE_URL = "https://releases.aesthetic.computer/desktop";
const SILO_URL = process.env.SILO_URL || "https://silo.aesthetic.computer";
const PUBLISH_SECRET = process.env.PUBLISH_SECRET || "";

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const notesIdx = args.indexOf("--notes");
const releaseNotes = notesIdx >= 0 ? args[notesIdx + 1] || "" : "";

const s3 = new S3Client({
  endpoint: process.env.SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com",
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.SPACES_KEY || "",
    secretAccessKey: process.env.SPACES_SECRET || "",
  },
  forcePathStyle: false,
  // DO Spaces' S3 impl still expects the legacy XML Checksum behavior;
  // AWS SDK v3 otherwise sends checksum headers that Spaces rejects with
  // InvalidArgument 400 during multipart.
  requestChecksumCalculation: "WHEN_REQUIRED",
  responseChecksumValidation: "WHEN_REQUIRED",
});

// Files to upload: manifests + binaries matching current version
const manifestFiles = ["latest-linux.yml", "latest-mac.yml", "latest.yml"];
const binaryPatterns = [".AppImage", ".dmg", ".zip", ".exe", ".deb", ".rpm", ".blockmap"];

function collectFiles() {
  if (!fs.existsSync(distDir)) {
    console.error("No dist/ directory found. Run a build first.");
    process.exit(1);
  }

  const files = [];
  for (const name of fs.readdirSync(distDir)) {
    const fullPath = path.join(distDir, name);
    if (!fs.statSync(fullPath).isFile()) continue;

    // Include manifests
    if (manifestFiles.includes(name)) {
      files.push({ name, path: fullPath, isManifest: true });
      continue;
    }

    // Include binaries for this version
    if (binaryPatterns.some((ext) => name.endsWith(ext)) && name.includes(version)) {
      files.push({ name, path: fullPath, isManifest: false });
    }
  }

  return files;
}

function parseYml(content) {
  // Simple YAML parser for electron-builder manifest format
  const result = {};
  for (const line of content.split("\n")) {
    const match = line.match(/^(\w+):\s*(.+)$/);
    if (match) {
      let val = match[2].trim().replace(/^['"]|['"]$/g, "");
      if (!isNaN(val) && val !== "") val = Number(val);
      result[match[1]] = val;
    }
  }
  return result;
}

function platformFromManifest(name) {
  if (name === "latest-linux.yml") return "linux";
  if (name === "latest-mac.yml") return "mac";
  if (name === "latest.yml") return "win";
  return null;
}

async function uploadFile(file) {
  const key = PREFIX + file.name;
  const size = fs.statSync(file.path).size;
  const contentType = file.isManifest
    ? "text/yaml"
    : "application/octet-stream";
  const cacheControl = file.isManifest
    ? "no-cache, no-store, must-revalidate"
    : "public, max-age=31536000";

  console.log(`  Uploading ${file.name} (${(size / 1024 / 1024).toFixed(1)} MB) → s3://${BUCKET}/${key}`);

  if (dryRun) return;

  // Use multipart for anything over 8 MB — DO Spaces frequently times out
  // single-part PutObject on large files (e.g. the 200 MB DMG). DO Spaces
  // rejects the ACL header on CreateMultipartUpload — we set ACL after.
  if (size > 8 * 1024 * 1024) {
    const upload = new Upload({
      client: s3,
      params: {
        Bucket: BUCKET,
        Key: key,
        Body: fs.createReadStream(file.path),
        ContentType: contentType,
        CacheControl: cacheControl,
      },
      queueSize: 4,
      partSize: 16 * 1024 * 1024,
      leavePartsOnError: false,
    });
    upload.on("httpUploadProgress", (p) => {
      if (p.total) {
        const pct = ((p.loaded / p.total) * 100).toFixed(0);
        process.stdout.write(`\r    ${pct}% (${(p.loaded / 1024 / 1024).toFixed(1)}/${(p.total / 1024 / 1024).toFixed(1)} MB)`);
      }
    });
    await upload.done();
    process.stdout.write("\n");
    // Apply public-read ACL as a follow-up (DO Spaces requires this path
    // when the initial multipart request can't carry ACL).
    const { PutObjectAclCommand } = await import("@aws-sdk/client-s3");
    await s3.send(new PutObjectAclCommand({
      Bucket: BUCKET,
      Key: key,
      ACL: "public-read",
    }));
    return;
  }

  await s3.send(
    new PutObjectCommand({
      Bucket: BUCKET,
      Key: key,
      Body: fs.readFileSync(file.path),
      ContentType: contentType,
      CacheControl: cacheControl,
      ACL: "public-read",
    }),
  );
}

async function registerWithSilo(platforms) {
  const url = `${SILO_URL}/api/desktop/register`;
  const body = { version, platforms, releaseNotes };

  console.log(`\n  Registering v${version} with silo...`);
  if (dryRun) {
    console.log("  [dry-run] Would POST:", JSON.stringify(body, null, 2));
    return;
  }

  const resp = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "X-Publish-Secret": PUBLISH_SECRET,
    },
    body: JSON.stringify(body),
  });

  if (!resp.ok) {
    const err = await resp.text();
    throw new Error(`Silo register failed (${resp.status}): ${err}`);
  }

  const data = await resp.json();
  console.log(`  Registered: ${JSON.stringify(data)}`);
}

async function main() {
  console.log(`\nPublishing Aesthetic Computer Desktop v${version}`);
  console.log(`  Bucket: ${BUCKET}`);
  console.log(`  Silo: ${SILO_URL}`);
  if (dryRun) console.log("  [DRY RUN - no actual uploads]");

  const files = collectFiles();
  if (files.length === 0) {
    console.error("\nNo files to upload. Build first with: npm run build:linux");
    process.exit(1);
  }

  console.log(`\nFound ${files.length} files to upload:`);
  for (const f of files) console.log(`  ${f.name} (${f.isManifest ? "manifest" : "binary"})`);

  // Upload all files
  console.log("\nUploading to DigitalOcean Spaces...");
  for (const file of files) {
    await uploadFile(file);
  }

  // Parse manifests to build platform metadata
  const platforms = {};
  for (const file of files.filter((f) => f.isManifest)) {
    const platform = platformFromManifest(file.name);
    if (!platform) continue;
    const content = fs.readFileSync(file.path, "utf8");
    const parsed = parseYml(content);
    if (parsed.path) {
      // Get size from actual file on disk (yml may not have top-level size)
      const binaryPath = path.join(distDir, parsed.path);
      const size = parsed.size || (fs.existsSync(binaryPath) ? fs.statSync(binaryPath).size : 0);
      platforms[platform] = {
        filename: parsed.path,
        size,
        sha512: parsed.sha512 || "",
        url: `${BASE_URL}/${parsed.path}`,
      };
    }
  }

  if (Object.keys(platforms).length === 0) {
    console.warn("\nNo manifest files found - skipping silo registration.");
    console.log("Upload the latest-*.yml files to register.");
    return;
  }

  // Register with silo
  await registerWithSilo(platforms);

  console.log(`\nDone! v${version} published.`);
  console.log(`  Download: ${BASE_URL}/`);
  console.log(`  Silo API: ${SILO_URL}/desktop/latest`);
}

main().catch((err) => {
  console.error("\nPublish failed:", err.name, "-", err.message);
  if (err.$metadata) console.error("  http:", err.$metadata.httpStatusCode);
  if (err.Code) console.error("  code:", err.Code);
  if (err.cause) console.error("  cause:", err.cause?.message || err.cause);
  if (err.stack) console.error(err.stack);
  process.exit(1);
});
