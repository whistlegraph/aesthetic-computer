// os-builder.mjs — FedAC OS image assembly for the /os endpoint
//
// Downloads a pre-baked base image from CDN, injects a piece bundle into
// the FEDAC-PIECE ext4 partition via debugfs, and streams the result.
//
// Requires: e2fsprogs (debugfs) installed on the server.

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { execSync, exec } from "child_process";
import { randomUUID } from "crypto";
import { createHash } from "crypto";
import { createBundle, createJSPieceBundle } from "./bundler.mjs";

// ─── Configuration ──────────────────────────────────────────────────

const CACHE_DIR = process.env.OS_CACHE_DIR || "/opt/oven/cache";
const TEMP_DIR = process.env.OS_TEMP_DIR || "/tmp";
const BASE_IMAGE_URL =
  process.env.FEDAC_BASE_IMAGE_URL ||
  "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/fedac-base-latest.img";
const MANIFEST_URL =
  process.env.FEDAC_MANIFEST_URL ||
  "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/fedac-base-manifest.json";

// Concurrency limit — each build copies ~3GB, so cap parallel builds.
const MAX_CONCURRENT_BUILDS = 2;
let activeBuildCount = 0;
const recentBuilds = [];
const MAX_RECENT = 20;

// ─── Manifest ───────────────────────────────────────────────────────

let cachedManifest = null;

async function fetchManifest(onProgress) {
  if (cachedManifest) return cachedManifest;
  onProgress?.({ stage: "manifest", message: "Fetching base image manifest..." });
  const res = await fetch(MANIFEST_URL);
  if (!res.ok) throw new Error(`Manifest fetch failed: ${res.status}`);
  cachedManifest = await res.json();
  onProgress?.({ stage: "manifest", message: `Base image v${cachedManifest.version}, Fedora ${cachedManifest.fedora}` });
  return cachedManifest;
}

// Invalidate manifest cache (call after base image update).
export function invalidateManifest() {
  cachedManifest = null;
}

// ─── Base Image Cache ───────────────────────────────────────────────

async function ensureBaseImage(onProgress) {
  await fs.mkdir(CACHE_DIR, { recursive: true });
  const manifest = await fetchManifest(onProgress);
  const basePath = path.join(CACHE_DIR, "fedac-base.img");

  // Check if cached image matches manifest SHA256
  try {
    const stat = await fs.stat(basePath);
    if (stat.size === manifest.totalSize) {
      // Quick size check passes — trust it (full SHA256 is slow for 3GB)
      onProgress?.({ stage: "base", message: "Base image cached and ready" });
      return { basePath, manifest };
    }
  } catch {
    // File doesn't exist
  }

  // Download base image
  onProgress?.({ stage: "base", message: "Downloading base image from CDN..." });
  const res = await fetch(BASE_IMAGE_URL);
  if (!res.ok) throw new Error(`Base image download failed: ${res.status}`);

  const tmpPath = basePath + ".downloading";
  const writer = fsSync.createWriteStream(tmpPath);
  const reader = res.body.getReader();
  let downloaded = 0;
  const total = manifest.totalSize || parseInt(res.headers.get("content-length")) || 0;

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    writer.write(value);
    downloaded += value.length;
    if (total > 0 && downloaded % (50 * 1024 * 1024) < value.length) {
      const pct = Math.round((downloaded / total) * 100);
      onProgress?.({ stage: "base", message: `Downloading base image... ${pct}%` });
    }
  }
  writer.end();
  await new Promise((resolve, reject) => {
    writer.on("finish", resolve);
    writer.on("error", reject);
  });

  // Atomic rename
  await fs.rename(tmpPath, basePath);
  onProgress?.({ stage: "base", message: "Base image downloaded and cached" });
  return { basePath, manifest };
}

// ─── Piece Injection ────────────────────────────────────────────────

function extractPartition(imagePath, manifest, tempPartPath) {
  // Extract the FEDAC-PIECE partition from the disk image using dd
  const { piecePartitionOffset, piecePartitionSize } = manifest;
  execSync(
    `dd if="${imagePath}" of="${tempPartPath}" bs=512 skip=${Math.floor(piecePartitionOffset / 512)} count=${Math.floor(piecePartitionSize / 512)} 2>/dev/null`,
    { stdio: "pipe" }
  );
}

function injectPieceIntoPartition(tempPartPath, pieceHtml) {
  // Write piece.html into the ext4 partition using debugfs (no mount needed)
  const tmpPiecePath = tempPartPath + ".piece.html";
  fsSync.writeFileSync(tmpPiecePath, pieceHtml);

  try {
    // Remove existing placeholder
    try {
      execSync(`debugfs -w -R "rm piece.html" "${tempPartPath}" 2>/dev/null`, { stdio: "pipe" });
    } catch {
      // File may not exist yet, that's fine
    }
    // Write the new piece
    execSync(`debugfs -w -R "write ${tmpPiecePath} piece.html" "${tempPartPath}"`, { stdio: "pipe" });
  } finally {
    try { fsSync.unlinkSync(tmpPiecePath); } catch {}
  }
}

function writePartitionBack(imagePath, manifest, tempPartPath) {
  // Write modified partition back into the image at the same offset
  const { piecePartitionOffset, piecePartitionSize } = manifest;
  execSync(
    `dd if="${tempPartPath}" of="${imagePath}" bs=512 seek=${Math.floor(piecePartitionOffset / 512)} count=${Math.floor(piecePartitionSize / 512)} conv=notrunc 2>/dev/null`,
    { stdio: "pipe" }
  );
}

// ─── Main Build Flow ────────────────────────────────────────────────

export async function streamOSImage(res, target, isJSPiece, density, onProgress) {
  if (activeBuildCount >= MAX_CONCURRENT_BUILDS) {
    throw new Error(`Server busy: ${activeBuildCount}/${MAX_CONCURRENT_BUILDS} OS builds in progress. Try again shortly.`);
  }

  activeBuildCount++;
  const buildId = randomUUID().slice(0, 8);
  const startTime = Date.now();
  const tempImagePath = path.join(TEMP_DIR, `fedac-os-${buildId}.img`);
  const tempPartPath = path.join(TEMP_DIR, `fedac-part-${buildId}.img`);

  try {
    // 1. Ensure base image is cached
    const { basePath, manifest } = await ensureBaseImage(onProgress);

    if (!manifest.piecePartitionOffset || manifest.piecePartitionOffset === 0) {
      throw new Error("Base image manifest missing piecePartitionOffset — rebuild base image with --base-image flag");
    }

    // 2. Build piece bundle
    onProgress?.({ stage: "bundle", message: `Bundling ${target}...` });
    const bundleResult = isJSPiece
      ? await createJSPieceBundle(target, (p) => onProgress?.({ stage: "bundle", message: p.message }), true, density)
      : await createBundle(target, (p) => onProgress?.({ stage: "bundle", message: p.message }), true, density);
    const pieceHtml = bundleResult.html;
    onProgress?.({ stage: "bundle", message: `Bundle ready: ${bundleResult.sizeKB}KB` });

    // 3. Copy base image to temp
    onProgress?.({ stage: "assemble", message: "Copying base image..." });
    await fs.copyFile(basePath, tempImagePath);

    // 4. Extract FEDAC-PIECE partition
    onProgress?.({ stage: "assemble", message: "Extracting piece partition..." });
    extractPartition(tempImagePath, manifest, tempPartPath);

    // 5. Inject piece.html via debugfs
    onProgress?.({ stage: "inject", message: "Injecting piece into partition..." });
    injectPieceIntoPartition(tempPartPath, pieceHtml);

    // 6. Write modified partition back
    onProgress?.({ stage: "inject", message: "Writing partition back..." });
    writePartitionBack(tempImagePath, manifest, tempPartPath);

    // 7. Stream to client (if res provided)
    if (res) {
      const stat = await fs.stat(tempImagePath);
      const filename = `${target}-os.img`;
      res.set({
        "Content-Type": "application/octet-stream",
        "Content-Disposition": `attachment; filename="${filename}"`,
        "Content-Length": stat.size,
        "Cache-Control": "no-cache",
      });

      onProgress?.({ stage: "stream", message: `Streaming ${Math.round(stat.size / 1024 / 1024)}MB image...` });

      const readStream = fsSync.createReadStream(tempImagePath);
      await new Promise((resolve, reject) => {
        readStream.pipe(res);
        readStream.on("end", resolve);
        readStream.on("error", reject);
        res.on("close", () => {
          readStream.destroy();
          resolve();
        });
      });
    }

    const elapsed = Date.now() - startTime;
    onProgress?.({ stage: "done", message: `OS image ready in ${Math.round(elapsed / 1000)}s` });

    // Track recent builds
    recentBuilds.unshift({
      buildId,
      target,
      isJSPiece,
      density,
      elapsed,
      time: new Date().toISOString(),
    });
    if (recentBuilds.length > MAX_RECENT) recentBuilds.pop();

    return { buildId, elapsed, filename: `${target}-os.img` };
  } finally {
    activeBuildCount--;
    // Clean up temp files
    try { await fs.unlink(tempImagePath); } catch {}
    try { await fs.unlink(tempPartPath); } catch {}
  }
}

// ─── Status ─────────────────────────────────────────────────────────

export function getOSBuildStatus() {
  return {
    activeBuildCount,
    maxConcurrent: MAX_CONCURRENT_BUILDS,
    recentBuilds: recentBuilds.slice(0, 10),
    baseImageUrl: BASE_IMAGE_URL,
    manifestUrl: MANIFEST_URL,
  };
}

export { ensureBaseImage };
