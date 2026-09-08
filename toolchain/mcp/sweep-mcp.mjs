#!/usr/bin/env node
// sweep-mcp.mjs — disk sweeping for the fleet Macs as a first-class tool.
//
// Wraps toolchain/macos/cleaner.sh and the Spaces offload lanes so an agent
// can inventory, clean, offload, and prune without shelling out to
// deletion-shaped commands. Every lane is a fixed source→destination pair
// declared below; there are no arbitrary-path operations. Pruning refuses to
// delete anything the remote has not been verified to hold.

import { execFile } from "node:child_process";
import { readdir, stat } from "node:fs/promises";
import { homedir, hostname } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const pexec = promisify(execFile);
const HOME = homedir();
const HOST = hostname().split(".")[0].replace(/-\d+$/, "") || "unknown";
const CLEANER = join(HOME, ".local/bin/cleaner");
const ENDPOINT = "https://sfo3.digitaloceanspaces.com";
const AWS_BASE = ["--endpoint-url", ENDPOINT, "--region", "sfo3"];

// The offload lanes. Each is a fixed local directory mirrored to a fixed
// private Spaces prefix; prunable lanes may delete local files only after
// this process has counted the remote and found it whole.
const LANES = {
  "codex-sessions": {
    local: join(HOME, ".codex/sessions"),
    remote: `s3://shelf-sync/${HOST}-system/codex-sessions`,
    prunable: true,
    match: (name) => name.endsWith(".jsonl"),
  },
  "codex-images": {
    local: join(HOME, ".codex/generated_images"),
    remote: `s3://shelf-sync/${HOST}-system/codex-images`,
    prunable: true,
    match: () => true,
  },
  shelf: {
    local: join(HOME, "Documents/Shelf"),
    remote: `s3://shelf-sync/${HOST}`,
    prunable: false, // Shelf policy: curate manually, never machine-prune.
    match: () => true,
  },
};

async function aws(args, timeout = 900_000) {
  const { stdout } = await pexec("aws", [...args, ...AWS_BASE], { timeout, maxBuffer: 64 * 1024 * 1024 });
  return stdout;
}

async function walk(dir, out = []) {
  for (const entry of await readdir(dir, { withFileTypes: true }).catch(() => [])) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) await walk(path, out);
    else if (entry.isFile()) out.push(path);
  }
  return out;
}

async function remoteCount(lane) {
  const listing = await aws(["s3", "ls", "--recursive", `${lane.remote}/`]).catch(() => "");
  return listing.split("\n").filter((line) => line.trim()).length;
}

async function cleaner(flags) {
  const { stdout } = await pexec("bash", [CLEANER, ...flags], { timeout: 900_000, maxBuffer: 8 * 1024 * 1024 });
  return stdout;
}

const text = (value) => [{ type: "text", text: typeof value === "string" ? value : JSON.stringify(value, null, 2) }];

async function callTool(name, args = {}) {
  switch (name) {
    case "sweep_report": {
      const out = await cleaner([]);
      return text(out);
    }
    case "sweep_apply": {
      const flags = ["--apply"];
      if (args.remoteBacked) flags.push("--remote-backed");
      if (args.thinSnapshots) flags.push("--thin-snapshots");
      const out = await cleaner(flags);
      const tail = out.split("\n").filter((l) => /reclaimed|skip:|Removing|Avail|Capacity|\/System\/Volumes\/Data/.test(l));
      return text(tail.join("\n") || out.slice(-1500));
    }
    case "sweep_offload": {
      const lane = LANES[args.lane];
      if (!lane) throw new Error(`unknown lane: ${args.lane} (${Object.keys(LANES).join(", ")})`);
      await aws(["s3", "sync", lane.local, lane.remote, "--acl", "private", "--only-show-errors", "--exclude", ".DS_Store"]);
      const [remote, local] = [await remoteCount(lane), (await walk(lane.local)).length];
      return text({ lane: args.lane, uploadedTo: lane.remote, remoteObjects: remote, localFiles: local, verified: remote >= local });
    }
    case "sweep_prune": {
      const lane = LANES[args.lane];
      if (!lane) throw new Error(`unknown lane: ${args.lane} (${Object.keys(LANES).join(", ")})`);
      if (!lane.prunable) throw new Error(`lane ${args.lane} is not prunable by policy`);
      const olderThanDays = Number(args.olderThanDays ?? 30);
      if (!(olderThanDays >= 7)) throw new Error("olderThanDays must be at least 7");
      const [remote, files] = [await remoteCount(lane), await walk(lane.local)];
      if (remote < files.length) {
        throw new Error(`refusing to prune: remote holds ${remote} objects but ${files.length} exist locally — run sweep_offload first`);
      }
      const cutoff = Date.now() - olderThanDays * 86_400_000;
      let freed = 0;
      const victims = [];
      for (const path of files) {
        if (!lane.match(path.split("/").pop())) continue;
        const info = await stat(path);
        if (info.mtimeMs < cutoff) { victims.push(path); freed += info.size; }
      }
      if (!args.confirm) {
        return text({ lane: args.lane, dryRun: true, wouldDelete: victims.length, wouldFreeMiB: Math.round(freed / 1048576), note: "pass confirm: true to delete" });
      }
      const { unlink } = await import("node:fs/promises");
      for (const path of victims) await unlink(path);
      return text({ lane: args.lane, deleted: victims.length, freedMiB: Math.round(freed / 1048576), remoteObjects: remote });
    }
    default:
      throw new Error(`unknown tool: ${name}`);
  }
}

const TOOLS = [
  {
    name: "sweep_report",
    description: "Run the fleet disk cleaner in report-only mode: every cache, workspace, and protected surface with sizes and policies, plus current disk usage. Read-only.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "sweep_apply",
    description: "Run cleaner --apply: clear known-regenerable caches (skips apps in use). remoteBacked additionally prunes Spaces-verified media (pop out/, assets mirror) — it fails closed on anything unverified. Returns the reclaim summary.",
    inputSchema: {
      type: "object",
      properties: {
        remoteBacked: { type: "boolean", description: "Also prune remote-backed AC media after Spaces verification (default false)" },
        thinSnapshots: { type: "boolean", description: "Thin local APFS snapshots (default false)" },
      },
    },
  },
  {
    name: "sweep_offload",
    description: "Mirror a fixed offload lane up to the private shelf-sync Spaces bucket and verify counts. Lanes: codex-sessions (~/.codex/sessions), codex-images (~/.codex/generated_images), shelf (~/Documents/Shelf). Upload only — never deletes.",
    inputSchema: {
      type: "object",
      properties: { lane: { type: "string", enum: ["codex-sessions", "codex-images", "shelf"] } },
      required: ["lane"],
    },
  },
  {
    name: "sweep_prune",
    description: "Delete local files older than N days from a prunable offload lane (codex-sessions, codex-images) — ONLY after verifying the remote holds at least as many objects as exist locally; refuses otherwise. Dry-run by default; pass confirm: true to delete. The shelf lane is never prunable.",
    inputSchema: {
      type: "object",
      properties: {
        lane: { type: "string", enum: ["codex-sessions", "codex-images"] },
        olderThanDays: { type: "number", description: "Age floor in days (default 30, minimum 7)" },
        confirm: { type: "boolean", description: "Actually delete (default false = dry run)" },
      },
      required: ["lane"],
    },
  },
];

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: params?.protocolVersion || "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "sweep-mcp", version: "1.0.0" },
          },
        };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "🧹 sweep-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🧹 sweep-mcp started (sweep_report, sweep_apply, sweep_offload, sweep_prune)" });
