// memory/remote.mjs
// Optional remote queue + flush adapter. Disabled by default.

import { existsSync } from "fs";
import { mkdir, readFile, writeFile, appendFile } from "fs/promises";

function asBool(value, fallback = false) {
  if (value == null || value === "") return fallback;
  return /^(1|true|yes|on)$/i.test(String(value));
}

export function getRemoteConfig() {
  const explicitEnable = asBool(process.env.AGENT_MEMORY_REMOTE_ENABLED, false);
  const endpoint = process.env.AGENT_MEMORY_REMOTE_URL || "";

  return {
    enabled: explicitEnable && !!endpoint,
    endpoint,
    token: process.env.AGENT_MEMORY_REMOTE_TOKEN || "",
    batchSize: Number(process.env.AGENT_MEMORY_REMOTE_BATCH_SIZE || 50),
  };
}

async function ensureQueueDir(queueDir) {
  await mkdir(queueDir, { recursive: true });
}

export async function enqueueRemoteRecord(queueFilePath, queueDir, record) {
  const remote = getRemoteConfig();
  if (!remote.enabled) {
    return { queued: false, reason: "remote-disabled" };
  }

  await ensureQueueDir(queueDir);
  await appendFile(queueFilePath, `${JSON.stringify(record)}\n`, "utf8");
  return { queued: true };
}

export async function flushRemoteQueue(queueFilePath) {
  const remote = getRemoteConfig();

  if (!remote.enabled) {
    return { flushed: 0, failed: 0, pending: 0, reason: "remote-disabled" };
  }

  if (!existsSync(queueFilePath)) {
    return { flushed: 0, failed: 0, pending: 0 };
  }

  const data = await readFile(queueFilePath, "utf8");
  const lines = data
    .split("\n")
    .map((line) => line.trim())
    .filter(Boolean);

  if (lines.length === 0) {
    return { flushed: 0, failed: 0, pending: 0 };
  }

  let flushed = 0;
  let failed = 0;
  const keep = [];

  const limit = Math.max(1, remote.batchSize);
  const run = lines.slice(0, limit);
  const remaining = lines.slice(limit);

  for (const line of run) {
    try {
      const payload = JSON.parse(line);
      const headers = {
        "content-type": "application/json",
      };
      if (remote.token) {
        headers.authorization = `Bearer ${remote.token}`;
      }

      const response = await fetch(remote.endpoint, {
        method: "POST",
        headers,
        body: JSON.stringify(payload),
      });

      if (!response.ok) {
        failed += 1;
        keep.push(line);
      } else {
        flushed += 1;
      }
    } catch {
      failed += 1;
      keep.push(line);
    }
  }

  const finalLines = [...keep, ...remaining];
  const finalData = finalLines.length ? `${finalLines.join("\n")}\n` : "";
  await writeFile(queueFilePath, finalData, "utf8");

  return {
    flushed,
    failed,
    pending: finalLines.length,
  };
}
