#!/usr/bin/env node
// Submit one latest-build bot fight to the remote Replay Oven and download the
// gated artifacts. Publication is intentionally a separate local command.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const OVEN = (process.env.OVEN_URL || "https://oven.aesthetic.computer").replace(/\/$/, "");
const argv = process.argv.slice(2);
const value = (name, fallback) => {
  const at = argv.indexOf(`--${name}`);
  return at >= 0 ? argv[at + 1] : fallback;
};
const day = value("day", new Date().toISOString().slice(0, 10));
const index = Number(value("index", "0"));
const ref = value("ref", "origin/main");
const theme = value("theme", "light");
const outRoot = resolve(value("out", "tmp/oskiewar-reels/queue"));

function adminKey() {
  if (process.env.OS_BUILD_ADMIN_KEY?.trim()) return process.env.OS_BUILD_ADMIN_KEY.trim();
  const plain = join(homedir(), "aesthetic-computer-vault", "oven", "os-build-admin-key.txt");
  if (existsSync(plain)) return readFileSync(plain, "utf8").trim();
  if (existsSync(`${plain}.gpg`)) {
    try {
      return execFileSync("gpg", ["--pinentry-mode", "loopback", "-d", `${plain}.gpg`],
        { encoding: "utf8", stdio: ["inherit", "pipe", "ignore"] }).trim();
    } catch {}
  }
  throw new Error("Oven admin key is unavailable");
}

const key = adminKey();
async function ask(path, init = {}) {
  const response = await fetch(`${OVEN}${path}`, { ...init,
    headers: { authorization: `Bearer ${key}`, ...(init.headers || {}) } });
  if (!response.ok) {
    const body = await response.json().catch(() => ({}));
    throw new Error(body.error || `Oven returned HTTP ${response.status}`);
  }
  return response;
}

const started = await ask("/oskiewar-reel", { method: "POST",
  headers: { "content-type": "application/json" },
  body: JSON.stringify({ day, index, ref, theme }) });
let job = await started.json();
console.log(`🥊 Oven job ${job.id} · ${day} #${index} · ${ref}`);

let last = "";
while (!["success", "failed", "cancelled"].includes(job.status)) {
  await new Promise((done) => setTimeout(done, 2000));
  job = await (await ask(`/oskiewar-reel/${job.id}`)).json();
  const line = `${job.stage} ${job.percent}%`;
  if (line !== last) { console.log(`   ${line}`); last = line; }
}
if (job.status !== "success") throw new Error(job.error || `Oven job ${job.status}`);

const dir = join(outRoot, job.reelId);
mkdirSync(dir, { recursive: true });
for (const [name, file] of Object.entries({ reel: "reel.mp4", cover: "cover.jpg",
  thumbnail: "thumbnail-10-percent.jpg", sidecar: "reel.json" })) {
  const response = await ask(`/oskiewar-reel/${job.id}/${name}`);
  writeFileSync(join(dir, file), Buffer.from(await response.arrayBuffer()));
}
const record = JSON.parse(readFileSync(join(dir, "reel.json"), "utf8"));
if (record.sourceCommit !== job.resolvedRef || !record.meta?.ok ||
    !record.sync?.ok || !record.motion?.ok)
  throw new Error("downloaded Reel failed commit or quality verification");
console.log(`✓ ${record.id} · ${record.motion.sourceFps} fixed-step source fps · ${record.sourceCommit.slice(0, 9)}`);
console.log(dir);
