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
// `node lith/disk-watch.mjs --dry` prints the readings and sends nothing.

import { execFile } from "node:child_process";
import { promisify } from "node:util";
import fs from "node:fs/promises";
import path from "node:path";
import { hostname } from "node:os";
import { fileURLToPath } from "node:url";

const run = promisify(execFile);
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
  if (!news.length) return;
  const worst = news.find((n) => n.say === "critical") || news.find((n) => n.say === "warning") || news[0];
  const headline = news.map((n) => n.say === "recovered" ? `${n.name} recovered`
    : n.say === "unreachable" ? `${n.name} not answering` : `${n.name} disk ${n.reading.percent}%`).join(", ");
  const icon = { critical: "🛑", warning: "⚠️", unreachable: "❓", recovered: "✅" }[worst.say];
  const { email } = await import(path.join(here, "../system/backend/email.mjs"));
  const sent = await email({
    to: TO,
    subject: `${icon} AC disks: ${headline}`,
    text: `${news.map((n) => n.reading.error ? `${n.name} has not answered twice in a row: ${n.reading.error}` : `${n.say.toUpperCase()} ${describe(n.name, n.reading)}`).join("\n")}\n\nAll servers right now:\n${table}\n\nWarning at 85% or under 3 GB free; critical at 93% or under 1.5 GB.\nThis is lith/disk-watch.mjs on ${hostname()}; it repeats every 24 h while a disk is at warning and every 2 h at critical.\n`,
  });
  if (!sent) process.exitCode = 1;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) await main();
