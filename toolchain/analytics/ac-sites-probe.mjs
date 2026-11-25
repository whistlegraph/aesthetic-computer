#!/usr/bin/env node
// AC sites probe: the other rooms. Each machine named in
// ~/.config/blueberry-wallpaper/sites.json runs ac-local-probe.mjs on its own
// LAN, piped over ssh so it needs no checkout, and its map comes back here
// for the wallpaper to draw beside this room's.
//
//   sites.json   [{ "machine": "jastow" }, { "machine": "a-laptop", "place": "home" }]
//
// A site that cannot be reached keeps its last map, marked with its age:
// the cache (sites-cache.json, beside the feed) is the map we keep.
// Machine names and what their rooms hold stay on this Mac, never in the repo.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const CONF = process.env.AC_SITES || join(homedir(), ".config/blueberry-wallpaper/sites.json");
const CACHE = process.env.AC_SITES_CACHE || join(homedir(), ".local/share/blueberry-wallpaper/sites-cache.json");
const EVERY_MS = 60000;
const TIMEOUT_MS = 45000;

const readJson = (path, fallback) => { try { return JSON.parse(readFileSync(path, "utf8")); } catch { return fallback; } };

function writeCache(cache) {
  try {
    mkdirSync(dirname(CACHE), { recursive: true });
    const tmp = `${CACHE}.${process.pid}.tmp`;
    writeFileSync(tmp, JSON.stringify(cache));
    renameSync(tmp, CACHE);
  } catch {}
}

// The probe's source goes in on stdin; the snapshot comes out on stdout.
function probeOver(machine) {
  return new Promise(resolve => {
    if (!/^[A-Za-z0-9._-]+$/.test(machine)) return resolve({ error: "bad name" });
    const ssh = spawn("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", machine, "node --input-type=module - --print"],
      { stdio: ["pipe", "pipe", "pipe"] });
    let out = "", err = "";
    const timer = setTimeout(() => ssh.kill("SIGKILL"), TIMEOUT_MS);
    ssh.stdout.on("data", d => (out += d));
    ssh.stderr.on("data", d => (err += d));
    ssh.on("error", e => (err += e.message));
    ssh.on("close", code => {
      clearTimeout(timer);
      try { resolve({ local: JSON.parse(out) }); }
      catch { resolve({ error: (err.trim().split("\n").pop() || `exit ${code}`).slice(0, 80) }); }
    });
    ssh.stdin.end(readFileSync(join(HERE, "ac-local-probe.mjs")));
  });
}

/** Every site, freshly probed where it answers and from the cache where it does not. */
export async function probeSites() {
  const sites = readJson(CONF, []);
  const cache = readJson(CACHE, {});
  const now = Date.now() / 1000;
  const results = await Promise.all(sites.map(async site => {
    const r = await probeOver(site.machine);
    if (r.local) cache[site.machine] = { at: now, local: r.local };
    const kept = cache[site.machine];
    return { machine: site.machine, place: site.place || "", at: kept?.at || 0, stale: !r.local, error: r.error || "", local: kept?.local || null };
  }));
  writeCache(cache);
  return results;
}

/** Keeps `state.sites` current in the background, so a slow ssh never holds a tick. */
export async function watchSites(state) {
  for (;;) {
    try { state.sites = await probeSites(); } catch {}
    await new Promise(r => setTimeout(r, EVERY_MS));
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  console.log(JSON.stringify(await probeSites(), null, 2));
}
