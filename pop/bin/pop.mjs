#!/usr/bin/env node
// pop.mjs — the unified Pop build system. ONE driver for every song.
//
// The pop/ tree grew ~15 C-engine songs, each with its own ad-hoc entry point
// (run-c.mjs / render-c.mjs / render.sh / bake.mjs + build.sh). This ties them
// together: every song declares a tiny `<name>.song.json` manifest next to its
// code, and this CLI discovers + drives them all the same way.
//
// A manifest (engine "c") looks like:
//   {
//     "name": "fluttabap360",
//     "score": "out/fluttabap360.score.txt",   // {score} placeholder target
//     "bake":  "node bin/render-X.mjs --bake {score} --no-open",   // compose → score
//     "build": "bash c/build.sh",                                  // compile the C engine
//     "render":"node c/run-c.mjs {score} --out out/X.mp3 --wav",   // C → master mp3/wav
//     "loop":  "node c/run-c.mjs {score} --out out/X-loop.mp3 --loop --wav",  // optional
//     "outputs": { "album": "out/X.mp3", "loop": "out/X-loop.mp3" }
//   }
// Commands run with cwd = the manifest's directory; {score} expands to `score`.
//
// Usage:
//   pop list                       list every discovered song
//   pop render <name> [--loop] [--play]   bake → build → render (the full pipe)
//   pop build  <name>              just (re)compile the engine
//   pop bake   <name>              just (re)bake the score
//   pop render-all [--loop]        render every song (rebuild the whole catalog)
//   pop where  <name>              print the manifest path + outputs

import { readdirSync, statSync, readFileSync } from "node:fs";
import { join, dirname, resolve, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const POP = resolve(dirname(fileURLToPath(import.meta.url)), "..");   // pop/

// ── discover every <name>.song.json (one or two levels under pop/) ──────────
function discover() {
  const songs = [];
  const scan = (dir, depth) => {
    let entries;
    try { entries = readdirSync(dir); } catch { return; }
    for (const e of entries) {
      const p = join(dir, e);
      let st; try { st = statSync(p); } catch { continue; }
      if (st.isDirectory()) {
        if (depth > 0 && e !== "node_modules" && e !== "out" && !e.startsWith(".")) scan(p, depth - 1);
      } else if (e.endsWith(".song.json")) {
        try {
          const m = JSON.parse(readFileSync(p, "utf8"));
          m.__dir = dir; m.__manifest = p;
          songs.push(m);
        } catch (err) { console.warn(`  ! bad manifest ${relative(POP, p)}: ${err.message}`); }
      }
    }
  };
  scan(POP, 2);
  return songs.sort((a, b) => a.name.localeCompare(b.name));
}

const fill = (cmd, m) => cmd.replaceAll("{score}", m.score ?? "");

function run(cmd, cwd, label) {
  if (!cmd) return true;
  if (label) console.log(`  ▸ ${label}`);
  const r = spawnSync(cmd, { cwd, stdio: "inherit", shell: true });
  if (r.status !== 0) { console.error(`  ✗ failed: ${cmd}`); return false; }
  return true;
}

function find(songs, name) {
  const m = songs.find((s) => s.name === name);
  if (!m) { console.error(`✗ no song "${name}". Try: pop list`); process.exit(1); }
  return m;
}

// engine binary is stale if its source is newer (best-effort, c/ convention)
function buildIfStale(m) {
  if (!m.build) return true;
  return run(fill(m.build, m), m.__dir, "build (compile engine)");
}

function renderSong(m, { loop, play } = {}) {
  console.log(`\n● ${m.name}${m.title ? `  — ${m.title}` : ""}`);
  if (!buildIfStale(m)) return false;
  if (!run(fill(m.bake, m), m.__dir, "bake (compose → score)")) return false;
  if (!run(fill(m.render, m), m.__dir, "render (C → master)")) return false;
  if (loop && m.loop) run(fill(m.loop, m), m.__dir, "render loop");
  const album = m.outputs?.album;
  if (album) {
    const abs = resolve(m.__dir, album);
    console.log(`  ✓ ${relative(process.cwd(), abs)}`);
    if (play) spawnSync("open", ["-a", "QuickTime Player", abs]);
  }
  return true;
}

// ── CLI ─────────────────────────────────────────────────────────────────────
const [cmd, ...rest] = process.argv.slice(2);
const flags = new Set(rest.filter((a) => a.startsWith("--")));
const args = rest.filter((a) => !a.startsWith("--"));
const songs = discover();

switch (cmd) {
  case undefined:
  case "list": {
    console.log(`Pop catalog — ${songs.length} song${songs.length === 1 ? "" : "s"}:`);
    for (const m of songs) {
      const where = relative(POP, m.__dir);
      console.log(`  ${m.name.padEnd(20)} ${(m.engine ?? "?").padEnd(4)} ${where}`);
    }
    if (!songs.length) console.log("  (none yet — add a <name>.song.json next to a song's code)");
    break;
  }
  case "where": {
    const m = find(songs, args[0]);
    console.log(`manifest: ${m.__manifest}`);
    console.log(`dir:      ${m.__dir}`);
    for (const [k, v] of Object.entries(m.outputs ?? {})) console.log(`  ${k}: ${resolve(m.__dir, v)}`);
    break;
  }
  case "build": run(fill(find(songs, args[0]).build, find(songs, args[0])), find(songs, args[0]).__dir, "build"); break;
  case "bake": { const m = find(songs, args[0]); run(fill(m.bake, m), m.__dir, "bake"); break; }
  case "render": {
    const m = find(songs, args[0]);
    process.exit(renderSong(m, { loop: flags.has("--loop"), play: flags.has("--play") }) ? 0 : 1);
  }
  case "render-all": {
    let ok = 0;
    for (const m of songs) if (renderSong(m, { loop: flags.has("--loop") })) ok++;
    console.log(`\n✓ rendered ${ok}/${songs.length} songs`);
    process.exit(ok === songs.length ? 0 : 1);
  }
  default:
    console.error(`unknown command "${cmd}". commands: list | render <name> | build <name> | bake <name> | render-all | where <name>`);
    process.exit(1);
}
