#!/usr/bin/env node
// backfill-takes.mjs — upload takes already on disk to /api/menuband-takes.
//
// Finds every `<name>.mp3` that sits next to a `<name>-stems/` folder (the
// pair Menu Band drops on the Desktop) under the folders given, and backs it
// up under your handle. The take's name is its takeId, so running this again
// skips what's already there.
//
//   node slab/menuband/bin/backfill-takes.mjs ~/Documents/Shelf            # dry run
//   node slab/menuband/bin/backfill-takes.mjs ~/Documents/Shelf --upload
//
// Auth is ~/.ac-token (run `ac-login` if it has expired). MENUBAND_TAKES_API
// overrides the endpoint, e.g. http://localhost:8888/api/menuband-takes.

import { readFile, readdir, stat } from "node:fs/promises";
import { homedir, hostname } from "node:os";
import { join, resolve } from "node:path";

const API = process.env.MENUBAND_TAKES_API || "https://aesthetic.computer/api/menuband-takes";
const STEMS = ["tones.wav", "percussion.wav", "voice.wav", "notes.mid", "mix.json"];
const args = process.argv.slice(2);
const upload = args.includes("--upload");
const roots = args.filter((a) => !a.startsWith("--")).map((a) => resolve(a));
const machine = (process.env.MENUBAND_MACHINE || hostname()).replace(/\.local$/, "").toLowerCase();

if (roots.length === 0) {
  console.error("usage: backfill-takes.mjs <folder>... [--upload]");
  process.exit(1);
}

// Every mp3 with a sibling `-stems` folder, found up to a few levels down.
async function findTakes(dir, depth = 4, found = new Map()) {
  let entries;
  try { entries = await readdir(dir, { withFileTypes: true }); } catch { return found; }
  const names = new Set(entries.map((e) => e.name));
  for (const entry of entries) {
    const path = join(dir, entry.name);
    if (entry.isFile() && entry.name.endsWith(".mp3")) {
      const name = entry.name.slice(0, -4);
      if (names.has(`${name}-stems`) && !found.has(name)) found.set(name, { name, mp3: path, stems: join(dir, `${name}-stems`) });
    } else if (entry.isDirectory() && depth > 0 && !entry.name.endsWith("-stems")) {
      await findTakes(path, depth - 1, found);
    }
  }
  return found;
}

async function describe(take) {
  const files = [{ name: "mix.mp3", path: take.mp3 }];
  for (const stem of STEMS) {
    const path = join(take.stems, stem);
    try { if ((await stat(path)).isFile()) files.push({ name: stem, path }); } catch {}
  }
  for (const file of files) file.bytes = (await stat(file.path)).size;
  let mix = {};
  try { mix = JSON.parse(await readFile(join(take.stems, "mix.json"), "utf8")); } catch {}
  return {
    takeId: take.name,
    recordedAt: (await stat(take.mp3)).mtime.toISOString(),
    machine,
    duration: Number(mix.duration) || 0,
    bpm: mix.bpm ?? null,
    program: mix.program ?? null,
    files,
  };
}

async function call(token, body) {
  const res = await fetch(API, {
    method: "POST",
    headers: { Authorization: `Bearer ${token}`, "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  const data = await res.json().catch(() => ({}));
  if (!res.ok) throw new Error(`${res.status} ${data.error || ""} ${data.missing ? data.missing.join(",") : ""}`.trim());
  return data;
}

async function send(token, take) {
  const begun = await call(token, {
    action: "begin",
    ...take,
    files: take.files.map(({ name, bytes }) => ({ name, bytes })),
  });
  if (begun.committed) return `${begun.code} already backed up`;
  for (const up of begun.uploads) {
    const file = take.files.find((f) => f.name === up.name);
    const res = await fetch(up.url, { method: up.method, headers: up.headers, body: await readFile(file.path) });
    if (!res.ok) throw new Error(`PUT ${up.name}: ${res.status} ${(await res.text()).slice(0, 200)}`);
  }
  await call(token, { action: "commit", code: begun.code });
  return `${begun.code} uploaded`;
}

const found = new Map();
for (const root of roots) await findTakes(root, 4, found);
const takes = [...found.values()].sort((a, b) => a.name.localeCompare(b.name));
console.log(`${takes.length} take${takes.length === 1 ? "" : "s"} on ${machine}${upload ? "" : " (dry run, add --upload)"}`);

let token;
if (upload) {
  const session = JSON.parse(await readFile(join(homedir(), ".ac-token"), "utf8"));
  if (session.expires_at && session.expires_at < Date.now()) {
    console.error("~/.ac-token has expired; run ac-login first");
    process.exit(1);
  }
  token = session.access_token;
}

let failed = 0;
for (const take of takes) {
  const info = await describe(take);
  const mb = (info.files.reduce((n, f) => n + f.bytes, 0) / 1e6).toFixed(1);
  const line = `${info.takeId}  ${info.duration.toFixed(1)}s  ${info.files.length} files  ${mb} MB`;
  if (!upload) { console.log(line); continue; }
  try { console.log(`${line}  → ${await send(token, info)}`); }
  catch (error) { failed += 1; console.log(`${line}  ✗ ${error.message}`); }
}
if (failed) process.exit(1);
