#!/usr/bin/env node
// ac-m4l/build-notepat.mjs
//
// Canonical build script for the notepat.com Max for Live device.
// Produces a versioned offline-chunked amxd + a latest.json manifest.
//
// Layout:
//   system/public/m4l/notepat.com.amxd              ← always-current alias
//   system/public/m4l/notepat.com/<git-hash>.amxd   ← immutable versioned
//   system/public/m4l/notepat.com/latest.json       ← manifest
//
// The manifest carries: piece git hash, SHA-256, filesize, build time,
// and the download permalink (https://notepat.com/amxd). The piece's
// env-info bridge in bios.mjs reads this to decide whether the user's
// installed amxd is out of date.
//
// Usage:
//   node ac-m4l/build-notepat.mjs            # build to repo paths
//   node ac-m4l/build-notepat.mjs --desktop  # also drop on ~/Desktop
//   node ac-m4l/build-notepat.mjs --log      # stream progress events

import { promises as fs } from "node:fs";
import path from "node:path";
import os from "node:os";
import { createHash } from "node:crypto";
import { execSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(__dirname, "..");
const AC_SOURCE_DIR = path.join(REPO_ROOT, "system/public/aesthetic.computer");
const M4L_DIR = path.join(REPO_ROOT, "system/public/m4l");
const DEVICE_DIR = path.join(M4L_DIR, "notepat.com");

// Pass AC_SOURCE_DIR through so the bundler finds disks/ + lib/.
process.env.AC_SOURCE_DIR = process.env.AC_SOURCE_DIR || AC_SOURCE_DIR;

const args = process.argv.slice(2);
const WITH_DESKTOP = args.includes("--desktop");
const VERBOSE = args.includes("--log");
const IF_STALE = args.includes("--if-stale");
const SYNC_SPACES = args.includes("--sync-spaces");

// DO Spaces bucket where the versioned amxds live permanently (same
// bucket as the rest of AC's CDN assets). Each deploy uploads the new
// hash-qualified amxd here so lith's local /m4l/ mirror is just the
// latest two — S3 is the long-term archive.
const SPACES_BUCKET = "assets-aesthetic-computer";
const SPACES_ENDPOINT = "https://sfo3.digitaloceanspaces.com";
const SPACES_PREFIX = "m4l/notepat.com";

// Files that actually influence the amxd binary. Anything outside this
// set (other pieces, docs, infra, other lith routes) shouldn't trigger
// a rebuild when we're called with --if-stale.
const INPUT_PATHS = [
  "system/public/aesthetic.computer/disks/notepat-remote.mjs",
  "system/public/aesthetic.computer/bios.mjs",
  "system/public/aesthetic.computer/lib/",
  "oven/bundler.mjs",
];

function gitHash() {
  try {
    return execSync("git rev-parse HEAD", { cwd: REPO_ROOT }).toString().trim();
  } catch {
    return "unversioned";
  }
}

function gitDirty() {
  // Only tracked-file modifications count as "dirty" for versioning —
  // stray untracked files on the server (like deploy.fish's
  // system/public/.commit-ref) shouldn't make every deploy's version
  // look like a dev build.
  try {
    execSync("git diff --quiet HEAD --", { cwd: REPO_ROOT, stdio: "pipe" });
    return false;
  } catch {
    return true;
  }
}

async function readExistingManifest() {
  const manifestPath = path.join(DEVICE_DIR, "latest.json");
  try {
    return JSON.parse(await fs.readFile(manifestPath, "utf8"));
  } catch {
    return null;
  }
}

function amxdInputsChangedSince(lastBuiltCommit) {
  try {
    const diff = execSync(
      `git diff --name-only ${lastBuiltCommit} HEAD -- ${INPUT_PATHS.map((p) => `'${p}'`).join(" ")}`,
      { cwd: REPO_ROOT, stdio: "pipe" },
    ).toString().trim();
    return diff ? diff.split("\n") : [];
  } catch {
    // git diff failed (invalid commit, etc.) — treat as needing rebuild.
    return null;
  }
}

// Uncommitted input changes — captures both unstaged and staged edits
// under INPUT_PATHS. `git status --porcelain` is the right primitive
// here; with pathspecs it narrows to just the files we care about.
function amxdInputsUncommitted() {
  try {
    const out = execSync(
      `git status --porcelain -- ${INPUT_PATHS.map((p) => `'${p}'`).join(" ")}`,
      { cwd: REPO_ROOT, stdio: "pipe" },
    ).toString().trim();
    if (!out) return [];
    // Each line is "XY filename" — strip the status columns.
    return out.split("\n").map((l) => l.slice(3));
  } catch {
    return [];
  }
}

async function main() {
  const bundlerPath = path.join(REPO_ROOT, "oven/bundler.mjs");

  const hash = gitHash();
  const dirty = gitDirty();
  const version = dirty ? `${hash}-dirty` : hash;

  // --if-stale short-circuits when no amxd input has changed since the
  // last successful build. Cheaper than always rebuilding on deploys
  // that only touch unrelated files (docs, other pieces, infra).
  if (IF_STALE) {
    const prev = await readExistingManifest();
    if (prev?.piece_git) {
      const committed = amxdInputsChangedSince(prev.piece_git) || [];
      const uncommitted = dirty ? amxdInputsUncommitted() : [];
      const combined = [...new Set([...committed, ...uncommitted])];
      if (combined.length === 0) {
        console.log(
          `✓ skip — no amxd-input changes since ${prev.piece_git.slice(0, 9)}`,
        );
        return;
      }
      console.log(`→ rebuild needed — ${combined.length} input(s) changed:`);
      for (const f of combined) console.log(`    · ${f}`);
    } else {
      console.log("→ initial build — no prior manifest");
    }
  }

  const { createM4DBundle } = await import(bundlerPath);

  const onProgress = VERBOSE
    ? (p) => console.log(`[${p.stage}] ${p.message}`)
    : () => {};

  console.log(`→ Building notepat.com.amxd @ ${version}…`);
  const { binary, filename, sizeKB } = await createM4DBundle(
    "notepat-remote",
    true,
    onProgress,
  );

  // Compute SHA-256 for the manifest.
  const sha256 = createHash("sha256").update(binary).digest("hex");
  const sizeBytes = binary.length;

  await fs.mkdir(DEVICE_DIR, { recursive: true });

  // Versioned file — immutable, named by git hash.
  const versionedName = `${version}.amxd`;
  const versionedPath = path.join(DEVICE_DIR, versionedName);
  await fs.writeFile(versionedPath, binary);

  // Current alias at the root — download link target.
  const aliasPath = path.join(M4L_DIR, "notepat.com.amxd");
  await fs.writeFile(aliasPath, binary);

  // Manifest.
  const manifest = {
    name: "notepat.com",
    version,
    piece_git: hash,
    dirty,
    built: new Date().toISOString(),
    amxd: {
      filename: "notepat.com.amxd",
      versionedPath: `/m4l/notepat.com/${versionedName}`,
      aliasPath: "/m4l/notepat.com.amxd",
      permalink: "https://notepat.com/amxd",
      sizeBytes,
      sha256,
    },
  };
  const manifestPath = path.join(DEVICE_DIR, "latest.json");
  await fs.writeFile(manifestPath, JSON.stringify(manifest, null, 2) + "\n");

  console.log(`  ✓ ${versionedName} (${sizeKB} KB, sha256 ${sha256.slice(0, 12)}…)`);
  console.log(`  ✓ ${path.relative(REPO_ROOT, aliasPath)} (alias)`);
  console.log(`  ✓ ${path.relative(REPO_ROOT, manifestPath)}`);

  if (WITH_DESKTOP) {
    const desktopPath = path.join(os.homedir(), "Desktop", filename);
    await fs.writeFile(desktopPath, binary);
    console.log(`  ✓ ${desktopPath} (Desktop)`);
  }

  if (SYNC_SPACES) {
    await syncToSpaces({ binary, versionedName, manifest });
  }
}

// Upload the versioned amxd + latest.json to DO Spaces so each build
// has a durable permalink outside lith. Mirrors the ac-os OTA pattern:
// versioned artifacts are immutable; `latest.json` is the rolling
// pointer the piece's env-info fetch reads to detect stale installs.
async function syncToSpaces({ binary, versionedName, manifest }) {
  const accessKeyId =
    process.env.DO_SPACES_KEY || process.env.AWS_ACCESS_KEY_ID;
  const secretAccessKey =
    process.env.DO_SPACES_SECRET || process.env.AWS_SECRET_ACCESS_KEY;
  if (!accessKeyId || !secretAccessKey) {
    console.warn(
      "  ⚠  --sync-spaces set but no credentials (DO_SPACES_KEY/SECRET or AWS_ACCESS_KEY_ID/SECRET); skipping upload",
    );
    return;
  }
  // Resolve @aws-sdk/client-s3 out of oven's node_modules — that's
  // where the dep already lives (oven depends on it for existing
  // S3 pipelines) and it saves root-level install duplication.
  const ovenRequire = createRequire(path.join(REPO_ROOT, "oven/package.json"));
  const { S3Client, PutObjectCommand } = ovenRequire("@aws-sdk/client-s3");
  const s3 = new S3Client({
    endpoint: SPACES_ENDPOINT,
    region: "sfo3",
    credentials: { accessKeyId, secretAccessKey },
  });
  const uploads = [
    {
      key: `${SPACES_PREFIX}/${versionedName}`,
      body: binary,
      contentType: "application/octet-stream",
    },
    {
      key: `${SPACES_PREFIX}/latest.json`,
      body: Buffer.from(JSON.stringify(manifest, null, 2) + "\n"),
      contentType: "application/json",
    },
    {
      key: `${SPACES_PREFIX}.amxd`, // alias at m4l/notepat.com.amxd
      body: binary,
      contentType: "application/octet-stream",
    },
  ];
  for (const u of uploads) {
    await s3.send(
      new PutObjectCommand({
        Bucket: SPACES_BUCKET,
        Key: u.key,
        Body: u.body,
        ACL: "public-read",
        ContentType: u.contentType,
        CacheControl: u.key.endsWith("latest.json") ? "no-cache" : "public,max-age=31536000,immutable",
      }),
    );
    console.log(`  ☁  s3://${SPACES_BUCKET}/${u.key}`);
  }
}

main().catch((err) => {
  console.error("✗ Build failed:", err);
  process.exit(1);
});
