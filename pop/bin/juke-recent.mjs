#!/usr/bin/env node
// juke-recent — send the /pop tracks you rendered lately to your Juke cloud,
// so the `juke` piece lists them under your handle (its `mine` tab).
//
//   node pop/bin/juke-recent.mjs            upload what changed in the last 14 days
//   node pop/bin/juke-recent.mjs --days 3   a shorter window
//   node pop/bin/juke-recent.mjs --max 12   newest N only (default 24)
//   node pop/bin/juke-recent.mjs --dry      say what would go, send nothing
//   node pop/bin/juke-recent.mjs --quiet    for the post-commit hook: never fails
//
// A track is an mp3 sitting directly in a lane's out/ directory (takes/ and
// stems below it stay home). out/ is not in git, so this is the path a render
// takes to the web. A re-render replaces the cloud copy of the same name.
// Uploads stay private: only your signed-in handle can list or play them. Sign in with
// `node juke-wizard/bin/juke-cloud.mjs login` (~/.ac-token).

import { promises as fs } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const POP = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const ENDPOINT = `${(process.env.AC_API_ORIGIN || "https://aesthetic.computer").replace(/\/$/, "")}/api/juke-cloud`;
const MAX_BYTES = 60 * 1024 * 1024;
const SKIP = new Set(["node_modules", ".git", "references", "takes", "stems", "cache"]);

const args = process.argv.slice(2);
const flag = (name) => args.includes(`--${name}`);
const option = (name, fallback) => { const i = args.indexOf(`--${name}`); return i >= 0 ? Number(args[i + 1]) || fallback : fallback; };
const quiet = flag("quiet"), dry = flag("dry"), days = option("days", 14), max = option("max", 24);
const say = (...line) => console.log(...line);

// Every mp3 directly inside a directory named out/, anywhere under pop/.
async function finals(dir, found = []) {
  let entries;
  try { entries = await fs.readdir(dir, { withFileTypes: true }); } catch { return found; }
  for (const entry of entries) {
    if (!entry.isDirectory() || SKIP.has(entry.name)) continue;
    const path = join(dir, entry.name);
    if (entry.name !== "out") { await finals(path, found); continue; }
    for (const file of await fs.readdir(path, { withFileTypes: true })) {
      if (!file.isFile() || !file.name.toLowerCase().endsWith(".mp3")) continue;
      const stat = await fs.stat(join(path, file.name));
      found.push({ path: join(path, file.name), name: file.name, bytes: stat.size, mtime: stat.mtimeMs });
    }
  }
  return found;
}

async function token() {
  let session;
  try { session = JSON.parse(await fs.readFile(join(homedir(), ".ac-token"), "utf8")); }
  catch { throw new Error("not signed in (node juke-wizard/bin/juke-cloud.mjs login)"); }
  if (!session.access_token || (session.expires_at && session.expires_at <= Date.now()))
    throw new Error("login expired (node juke-wizard/bin/juke-cloud.mjs login)");
  return session.access_token;
}

async function api(bearer, method, input) {
  const response = await fetch(ENDPOINT, {
    method,
    headers: { Authorization: `Bearer ${bearer}`, ...(input ? { "Content-Type": "application/json" } : {}) },
    body: input ? JSON.stringify(input) : undefined,
  });
  const output = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(output.error || `cloud request failed (${response.status})`);
  return output;
}

async function main() {
  const since = Date.now() - days * 86400000;
  const recent = (await finals(POP))
    .filter((t) => t.mtime >= since && t.bytes > 0 && t.bytes <= MAX_BYTES)
    .sort((a, b) => b.mtime - a.mtime);
  // The same name in two lanes: the newer render speaks for it.
  const tracks = [...new Map(recent.reverse().map((t) => [t.name, t])).values()].reverse().slice(0, max);
  if (!tracks.length) return say(`juke-recent: nothing rendered in the last ${days} days`);

  const bearer = await token();
  const cloud = new Map();
  for (const track of (await api(bearer, "GET")).tracks || []) if (!cloud.has(track.name)) cloud.set(track.name, track);

  let sent = 0;
  for (const track of tracks) {
    const there = cloud.get(track.name);
    const lane = basename(dirname(dirname(track.path)));
    // Same size and no older than the render: already up.
    if (there && there.bytes === track.bytes && Date.parse(there.updatedAt) >= track.mtime - 1000) continue;
    if (dry) { say(`would send ${lane}/${track.name}${there ? " (replaces the cloud copy)" : ""}`); continue; }
    const prepared = await api(bearer, "POST", { action: "upload", filename: track.name, bytes: track.bytes });
    const put = await fetch(prepared.uploadURL, { method: "PUT", headers: prepared.headers, body: await fs.readFile(track.path) });
    if (!put.ok) throw new Error(`upload failed for ${track.name} (${put.status})`);
    if (there) await api(bearer, "POST", { action: "delete", key: there.key });
    sent++;
    say(`sent ${lane}/${track.name} (${(track.bytes / 1048576).toFixed(1)} MB)`);
  }
  say(`juke-recent: ${sent} sent, ${tracks.length - sent} ${dry ? "checked" : "already up"} — https://aesthetic.computer/juke~mine`);
}

main().catch((error) => {
  console.error(`juke-recent: ${error.message}`);
  process.exitCode = quiet ? 0 : 1;
});
