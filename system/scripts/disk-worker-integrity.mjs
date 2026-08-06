import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import path from "node:path";

const WORKER_FILENAME = /^disk\.worker\.[a-f0-9]{12}\.mjs$/;
const SHA256 = /^[a-f0-9]{64}$/;

function validateSources(sources) {
  if (!Array.isArray(sources) || sources.length === 0) {
    throw new Error("worker manifest has no source list");
  }
  const sorted = [...new Set(sources)].sort();
  if (sorted.length !== sources.length || sorted.some((source, i) => source !== sources[i])) {
    throw new Error("worker manifest sources must be unique and sorted");
  }
  for (const source of sorted) {
    if (
      typeof source !== "string" ||
      path.isAbsolute(source) ||
      source.split(/[\\/]/).includes("..")
    ) {
      throw new Error(`invalid worker source path: ${source}`);
    }
  }
  return sorted;
}

export async function computeSourceSha256(systemDir, sources) {
  const hash = createHash("sha256");
  hash.update("ac-disk-worker-sources-v1\0");
  for (const source of validateSources(sources)) {
    const bytes = await readFile(path.join(systemDir, source));
    hash.update(source);
    hash.update("\0");
    hash.update(String(bytes.length));
    hash.update("\0");
    hash.update(bytes);
    hash.update("\0");
  }
  return hash.digest("hex");
}

export async function verifyDiskWorker(systemDir) {
  const libDir = path.join(systemDir, "public/aesthetic.computer/lib");
  const manifest = JSON.parse(
    await readFile(path.join(libDir, "disk-worker-manifest.json"), "utf8"),
  );
  if (!WORKER_FILENAME.test(manifest.filename || "")) {
    throw new Error("invalid worker filename");
  }
  if (!SHA256.test(manifest.sha256 || "") || !SHA256.test(manifest.sourceSha256 || "")) {
    throw new Error("invalid worker manifest hash");
  }

  const worker = await readFile(path.join(libDir, manifest.filename));
  const workerSha256 = createHash("sha256").update(worker).digest("hex");
  if (worker.length !== manifest.bytes || workerSha256 !== manifest.sha256) {
    throw new Error("worker artifact does not match its manifest");
  }

  const sourceSha256 = await computeSourceSha256(systemDir, manifest.sources);
  if (sourceSha256 !== manifest.sourceSha256) {
    throw new Error("worker bundle is stale; run npm run build:disk-worker");
  }

  return { ...manifest, sourceCount: manifest.sources.length };
}
