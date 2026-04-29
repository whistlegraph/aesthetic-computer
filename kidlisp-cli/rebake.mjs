#!/usr/bin/env node
// rebake.mjs — Admin-driven piece media rebake (no Tezos required).
//
// Usage: node kidlisp-cli/rebake.mjs $2un
//        node kidlisp-cli/rebake.mjs $2un --api https://localhost:8888
//
// Calls /api/admin-rebake (admin only) to refresh the oven-baked
// artifact + thumbnail for a piece — works for both minted tokens
// and unminted pieces. Polls /api/keep-status until the job ends
// and prints the resulting IPFS URIs.
//
// Use this when an oven thumbnail or bundle came back broken
// (e.g. the all-black 528-byte WebP from a Puppeteer flake) and
// you want a fresh pin without walking the keep-mint UI.
//
// For minted-token full regen (rebake + on-chain edit_metadata),
// use regenerate-media.mjs instead.

import fs from "fs";
import path from "path";
import os from "os";
import { execFileSync } from "child_process";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const AC_LOGIN = path.join(__dirname, "..", "tezos", "ac-login.mjs");

const args = process.argv.slice(2);
function opt(name, fallback) {
  const i = args.indexOf(`--${name}`);
  return i >= 0 && args[i + 1] ? args[i + 1] : fallback;
}
const API_BASE = opt("api", "https://aesthetic.computer").replace(/\/$/, "");

const flagsWithValues = new Set(["api"]);
const pieces = [];
for (let i = 0; i < args.length; i++) {
  const a = args[i];
  if (a.startsWith("--")) {
    if (flagsWithValues.has(a.slice(2))) i++;
    continue;
  }
  pieces.push(a);
}

if (pieces.length === 0) {
  console.log(`
  rebake — Admin rebake of a KidLisp piece's oven media (artifact + thumbnail)

  Usage:
    node kidlisp-cli/rebake.mjs \\$2un
    node kidlisp-cli/rebake.mjs \\$2un --api https://localhost:8888

  Auth: requires an admin AC login (~/.ac-token, refreshed by ac-login).
  `);
  process.exit(0);
}

if (API_BASE.includes("localhost")) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// ── Auth ──
const authPath = path.join(os.homedir(), ".ac-token");
function readToken() {
  if (!fs.existsSync(authPath)) return null;
  try {
    const data = JSON.parse(fs.readFileSync(authPath, "utf8"));
    if (data.expires_at && Date.now() > data.expires_at) return null;
    return data;
  } catch {
    return null;
  }
}

let auth = readToken();
if (!auth) {
  console.log("\n🔐 Not logged in — launching ac-login...\n");
  try {
    execFileSync("node", [AC_LOGIN], { stdio: "inherit" });
  } catch {
    console.error("❌ Login failed.");
    process.exit(1);
  }
  auth = readToken();
  if (!auth) {
    console.error("❌ Still no valid token.");
    process.exit(1);
  }
}

const TOKEN = auth.access_token;
const WHO = auth.user?.handle ? `@${auth.user.handle}` : auth.user?.email || "unknown";

const POLL_INTERVAL_MS = 2500;
const POLL_TIMEOUT_MS = 5 * 60 * 1000;

async function rebakeOne(pieceArg) {
  const piece = pieceArg.replace(/^\$/, "");
  console.log(`\n${"─".repeat(60)}\n🎨 $${piece}  (admin: ${WHO})`);

  const startRes = await fetch(`${API_BASE}/api/admin-rebake`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${TOKEN}`,
    },
    body: JSON.stringify({ piece: `$${piece}` }),
  });

  const text = await startRes.text();
  let started;
  try { started = JSON.parse(text); } catch { started = { error: text.slice(0, 300) }; }

  if (!startRes.ok) {
    console.error(`❌ admin-rebake HTTP ${startRes.status}: ${started.error || text}`);
    return { piece, success: false };
  }

  const jobId = started.jobId;
  if (!jobId) {
    console.error(`❌ no jobId in response: ${JSON.stringify(started).slice(0, 200)}`);
    return { piece, success: false };
  }
  console.log(`  job ${jobId}  →  polling ${API_BASE}/api/keep-status`);

  const start = Date.now();
  let lastLine = "";
  while (Date.now() - start < POLL_TIMEOUT_MS) {
    await new Promise((r) => setTimeout(r, POLL_INTERVAL_MS));
    let job;
    try {
      const r = await fetch(`${API_BASE}/api/keep-status?jobId=${encodeURIComponent(jobId)}`);
      if (!r.ok) continue;
      job = await r.json();
    } catch { continue; }

    const elapsed = ((Date.now() - start) / 1000).toFixed(0);
    const line = `[${elapsed}s] ${job.status}: ${job.stage || "?"} — ${job.message || job.stageMessage || ""}`;
    if (line !== lastLine) {
      process.stdout.write(`\r  ${line.padEnd(78)}`);
      lastLine = line;
    }

    if (job.status === "ready") {
      process.stdout.write("\n");
      console.log(`  ✅ ready`);
      if (job.artifactUri) console.log(`     artifact:  ${job.artifactUri}`);
      if (job.thumbnailUri) console.log(`     thumbnail: ${job.thumbnailUri}`);
      return { piece, success: true, ...job };
    }
    if (job.status === "failed") {
      process.stdout.write("\n");
      console.error(`  ❌ failed at ${job.errorStage || job.stage}: ${job.error || "unknown"}`);
      return { piece, success: false };
    }
  }

  process.stdout.write("\n");
  console.error(`  ⏱  timed out after ${POLL_TIMEOUT_MS / 1000}s (job may still be running)`);
  return { piece, success: false };
}

let failed = 0;
for (const p of pieces) {
  const r = await rebakeOne(p);
  if (!r.success) failed++;
}
process.exit(failed > 0 ? 1 : 0);
