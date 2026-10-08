import { readdir, lstat } from "node:fs/promises";
import { createReadStream } from "node:fs";
import { createGunzip } from "node:zlib";

// Runs on Lith. Raw requests, headers, paths and addresses never leave this function.
export async function collectAccessLogs(scope, directory = "/var/log/caddy") {
  const start = Date.parse(scope.start), end = Date.parse(scope.end);
  const names = (await readdir(directory)).filter(name => /^access(?:-[0-9T:.a-z-]+)?\.log(?:\.gz)?$/.test(name));
  const files = [];
  for (const name of names) {
    const info = await lstat(`${directory}/${name}`);
    if (!info.isFile() || info.isSymbolicLink()) continue;
    files.push({ name, modified: +info.mtime });
  }
  files.sort((a, b) => b.modified - a.modified);
  if (!files.length) throw new Error("No access log files");
  const buckets = new Map(), maxBytes = 256 * 1024 * 1024, deadline = Date.now() + 45000;
  let bytes = 0, scanned = 0, earliest = Infinity, truncated = false, malformed = false;
  for (const file of files.slice(0, 4)) {
    const input = createReadStream(`${directory}/${file.name}`), stream = file.name.endsWith(".gz") ? input.pipe(createGunzip()) : input;
    // Forward a rotated-file read failure to the decompressor as well.
    if (stream !== input) input.on("error", error => stream.destroy(error));
    let rest = "";
    try {
      for await (const chunk of stream) {
        bytes += chunk.length;
        if (bytes > maxBytes || Date.now() > deadline) { truncated = true; break; }
        const lines = (rest + chunk.toString("utf8")).split("\n"); rest = lines.pop();
        if (rest.length > 1024 * 1024) throw new Error("Oversized access row");
        for (const line of lines) {
          if (!line) continue;
          let row;
          try { row = JSON.parse(line); } catch { malformed = true; continue; }
          const at = row.ts * 1000;
          if (!Number.isFinite(at)) { malformed = true; continue; }
          earliest = Math.min(earliest, at);
          if (at < start || at >= end) continue;
          const host = typeof row.request?.host === "string" ? row.request.host.toLowerCase().replace(/:\d+$/, "") : "";
          if (!scope.hosts.includes(host)) continue;
          if (!Number.isInteger(row.status) || row.status < 100 || row.status > 599) { malformed = true; continue; }
          const hour = new Date(Math.floor(at / 3600000) * 3600000).toISOString(), key = `${host}:${hour}`;
          const bucket = buckets.get(key) || { host, hour, requests: 0, errors5xx: 0 };
          bucket.requests++; bucket.errors5xx += Number(row.status >= 500); scanned++;
          buckets.set(key, bucket);
        }
      }
      if (rest.trim()) malformed = true;
    } finally { input.destroy(); stream.destroy(); }
    if (truncated || earliest <= start) break;
  }
  // Retention, a read cap, or malformed rows make absence of origin errors inconclusive.
  return { status: "available", rows: [...buckets.values()], scanned, truncated: truncated || malformed || earliest > start };
}
