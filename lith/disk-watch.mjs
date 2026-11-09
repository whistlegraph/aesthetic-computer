#!/usr/bin/env node
// Disk watch: every 15 minutes (lith/disk-watch.timer) lith reads how full
// each AC server's root disk is and emails mail@aesthetic.computer when one
// is filling up. On 2026-10-20 silo's disk filled with a backup and took
// MongoDB, and with it all of AC, down; nothing said so until it was over.
//
// lith reads its own disk directly. The others answer over SSH with a key
// that can only run df (authorized_keys: restrict,command="df -Pk /"), made
// and placed by lith/install-disk-watch.fish.
//
// Email when a disk gets worse, again every REMIND hours while it stays
// bad, once when it recovers, and when a server stops answering twice in a
// row. State lives in /var/lib/ac-disk-watch/state.json.
//
// lith's disk has a 95% limit. From TRIM_AT up the watch frees space on lith
// itself (old deploy rollback snapshots, the journal, the npm cache), and
// deploys refuse to start at LIMIT (slab/bin/ac-deploy,
// toolchain/private-products-deploy.fish). On 2026-11-09 lith reached 92%
// because every product deploy left a 155 MB snapshot in /tmp: 201 of them.
// `node lith/disk-watch.mjs --dry` prints the readings and what it would
// free, and changes and sends nothing.

import { execFile } from "node:child_process";
import { promisify } from "node:util";
import fs from "node:fs/promises";
import path from "node:path";
import { hostname } from "node:os";
import { fileURLToPath } from "node:url";

const run = promisify(execFile);
const sh = (script) => run("bash", ["-c", script], { timeout: 300_000 });
const here = path.dirname(fileURLToPath(import.meta.url));
const TO = "mail@aesthetic.computer";
const KEY = "/root/.ssh/disk_watch";
const STATE = "/var/lib/ac-disk-watch/state.json";
const dry = process.argv.includes("--dry");

export const SERVERS = [
  { name: "lith", local: true },
  { name: "silo", host: "silo.aesthetic.computer" },
  { name: "oven", host: "oven.aesthetic.computer" },
  { name: "session-server", host: "session-server.aesthetic.computer" },
];

const GB = 1024 * 1024; // df -k reports kilobytes
export const LEVELS = ["ok", "warning", "critical"];
const REMIND = { warning: 24, critical: 2 }; // hours between repeat emails
export const LIMIT = 95; // lith's disk never goes past this
export const TRIM_AT = 90; // lith frees space from here, before it reaches LIMIT
const STAGES = "/tmp/ac-products.*"; // one rollback snapshot per product deploy
const HOUR = 3600_000;

// "warning" at 85% full or under 3 GB free; "critical" at 93% or under 1.5 GB.
export function level({ percent, freeKb }) {
  if (percent >= 93 || freeKb < 1.5 * GB) return "critical";
  if (percent >= 85 || freeKb < 3 * GB) return "warning";
  return "ok";
}

// The last line of `df -Pk /`: filesystem, size, used, available, use%, mount.
export function parseDf(text) {
  const fields = text.trim().split("\n").at(-1)?.trim().split(/\s+/) || [];
  const [, sizeKb, usedKb, freeKb, percent] = fields.map((f, i) => (i ? parseInt(f, 10) : f));
  if (![sizeKb, usedKb, freeKb, percent].every(Number.isFinite)) throw Error(`unreadable df output: ${text.slice(0, 120)}`);
  return { sizeKb, usedKb, freeKb, percent };
}

async function read(server) {
  if (server.local) return parseDf((await run("df", ["-Pk", "/"])).stdout);
  const { stdout } = await run("ssh", ["-i", KEY, "-o", "BatchMode=yes", "-o", "ConnectTimeout=10",
    "-o", "StrictHostKeyChecking=accept-new", `root@${server.host}`, "df"], { timeout: 30_000 });
  return parseDf(stdout);
}

// Which deploy snapshots to remove, newest kept. `stages` is [{ path, mtime }].
// One under an hour old is never removed: a deploy may still be using it.
export function stagesToRemove(stages, keep, now = Date.now()) {
  return [...stages].sort((a, b) => b.mtime - a.mtime).slice(keep)
    .filter((s) => now - s.mtime >= HOUR).map((s) => s.path);
}

async function stages() {
  const { stdout } = await sh(`for d in ${STAGES}; do [ -d "$d" ] && echo "$(stat -c %Y "$d") $d"; done; true`);
  return stdout.trim().split("\n").filter(Boolean).map((line) => {
    const [mtime, p] = line.split(" ");
    return { path: p, mtime: Number(mtime) * 1000 };
  });
}

// Free lith's disk until it is under TRIM_AT, cheapest loss first. Each step
// is safe to lose: snapshots past the newest few, journal past 200 MB, the
// npm download cache. Returns what it did, in words.
async function trim(reading) {
  const done = [];
  const steps = [
    ["older deploy snapshots, newest 3 kept", async () => removeStages(3)],
    ["journal vacuumed to 200 MB", () => sh("journalctl --vacuum-size=200M")],
    ["npm cache cleaned", () => sh("npm cache clean --force")],
    ["all deploy snapshots but the newest", async () => removeStages(1)],
  ];
  for (const [label, step] of steps) {
    if (reading.percent < TRIM_AT) break;
    const before = reading.freeKb;
    const note = await step();
    reading = await read({ local: true });
    const freed = reading.freeKb - before;
    if (freed > 0.05 * GB) done.push(`${label}${note ? ` (${note})` : ""}: freed ${gb(freed)}`);
  }
  return { reading, done };
}

async function removeStages(keep) {
  const gone = stagesToRemove(await stages(), keep);
  if (gone.length) await run("rm", ["-rf", "--", ...gone]);
  return gone.length ? `${gone.length} removed` : "";
}

const gb = (kb) => `${(kb / GB).toFixed(1)} GB`;
const describe = (name, d) => `${name}: ${d.percent}% full, ${gb(d.freeKb)} free of ${gb(d.sizeKb)}`;

// Decide what to say about one server, given its reading and remembered state.
export function judge(previous = {}, reading, now = Date.now()) {
  if (reading.error) {
    const misses = (previous.misses || 0) + 1;
    const say = misses === 2 ? "unreachable" : null;
    return { next: { ...previous, misses }, say };
  }
  const was = previous.level || "ok", now_ = level(reading);
  const next = { level: now_, misses: 0, mailedAt: previous.mailedAt, percent: reading.percent };
  let say = null;
  if (LEVELS.indexOf(now_) > LEVELS.indexOf(was)) say = now_;
  else if (now_ !== "ok" && (!previous.mailedAt || now - previous.mailedAt >= REMIND[now_] * 3600_000)) say = now_;
  else if (now_ === "ok" && was !== "ok") say = "recovered";
  if (say) next.mailedAt = say === "recovered" ? undefined : now;
  return { next, say };
}

async function main() {
  const state = JSON.parse(await fs.readFile(STATE, "utf8").catch(() => "{}"));
  const readings = await Promise.all(SERVERS.map(async (s) => {
    try { return [s.name, await read(s)]; } catch (error) { return [s.name, { error: error.message.split("\n")[0] }]; }
  }));
  let trimmed = [];
  const lith = readings.find(([name]) => name === "lith");
  if (!lith[1].error && lith[1].percent >= TRIM_AT) {
    if (dry) {
      console.log(`lith is ${lith[1].percent}% full; would remove ${stagesToRemove(await stages(), 3).length} deploy snapshots, then vacuum the journal and npm cache if still at ${TRIM_AT}%`);
    } else {
      try { ({ reading: lith[1], done: trimmed } = await trim(lith[1])); }
      catch (error) { trimmed = [`trimming failed: ${error.message.split("\n")[0]}`]; }
      for (const line of trimmed) console.log(`trimmed lith: ${line}`);
    }
  }
  const news = [];
  for (const [name, reading] of readings) {
    const { next, say } = judge(state[name], reading);
    state[name] = next;
    if (say) news.push({ name, reading, say });
  }
  const table = readings.map(([name, r]) => r.error ? `${name}: unreachable (${r.error})` : describe(name, r)).join("\n");
  if (dry) { console.log(table); console.log(news.map((n) => `would email: ${n.name} ${n.say}`).join("\n") || "nothing to email"); return; }
  await fs.mkdir(path.dirname(STATE), { recursive: true });
  await fs.writeFile(STATE, JSON.stringify(state, null, 2));
  if (!news.length && !trimmed.length) return;
  const worst = news.find((n) => n.say === "critical") || news.find((n) => n.say === "warning") || news[0];
  const headline = news.map((n) => n.say === "recovered" ? `${n.name} recovered`
    : n.say === "unreachable" ? `${n.name} not answering` : `${n.name} disk ${n.reading.percent}%`).join(", ")
    || `lith trimmed to ${lith[1].percent}%`;
  const icon = { critical: "🛑", warning: "⚠️", unreachable: "❓", recovered: "✅" }[worst?.say] || "🧹";
  const trimText = trimmed.length ? `lith passed ${TRIM_AT}% and was trimmed (its limit is ${LIMIT}%):\n${trimmed.join("\n")}\n\n` : "";
  const { email } = await import(path.join(here, "../system/backend/email.mjs"));
  const sent = await email({
    to: TO,
    subject: `${icon} AC disks: ${headline}`,
    text: `${trimText}${news.map((n) => n.reading.error ? `${n.name} has not answered twice in a row: ${n.reading.error}` : `${n.say.toUpperCase()} ${describe(n.name, n.reading)}`).join("\n")}\n\nAll servers right now:\n${table}\n\nWarning at 85% or under 3 GB free; critical at 93% or under 1.5 GB. lith trims itself from ${TRIM_AT}% and deploys refuse at ${LIMIT}%.\nThis is lith/disk-watch.mjs on ${hostname()}; it repeats every 24 h while a disk is at warning and every 2 h at critical.\n`,
  });
  if (!sent) process.exitCode = 1;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) await main();
