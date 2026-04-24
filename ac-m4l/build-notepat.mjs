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

async function main() {
  const bundlerPath = path.join(REPO_ROOT, "oven/bundler.mjs");
  const { createM4DBundle } = await import(bundlerPath);

  const hash = gitHash();
  const dirty = gitDirty();
  const version = dirty ? `${hash}-dirty` : hash;

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
}

main().catch((err) => {
  console.error("✗ Build failed:", err);
  process.exit(1);
});
