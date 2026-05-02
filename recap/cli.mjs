#!/usr/bin/env node
// recap/cli.mjs — `ac-24` CLI for the aesthetic 24 recap pipeline.
//
// The minimum-viable scaffold mirrors `papers/cli.mjs`: a single
// versioned entrypoint that owns the surface area used by the oven's
// recap-builder.mjs and (later) by anyone running the pipeline locally.
// For now it's a thin wrapper around the existing `bin/*` scripts +
// `pipeline.fish` — every step's caching lives inside its own script
// (tts.mjs, transcribe.mjs, align.mjs, jeffrey-photos.mjs, etc.) and
// will keep getting tightened. See feedback memory:
// `feedback_recap_ac24_cli.md` for the full vision.
//
// Subcommands:
//   ac24 build <audience>  — run the full pipeline for an audience
//   ac24 version           — print CLI + pipeline version
//   ac24 cache             — show per-episode cache state (stub)
//
// Usage from the repo:
//   node recap/cli.mjs build jeffrey-73h-2026-05-02
//
// On the oven, recap-builder.mjs can switch from `fish pipeline.fish`
// to `node cli.mjs build <audience>` — same behavior, versioned shell.

import { readFileSync, existsSync, statSync } from "node:fs";
import { spawn } from "node:child_process";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = HERE; // recap/

const PKG = (() => {
  try { return JSON.parse(readFileSync(`${ROOT}/package.json`, "utf8")); }
  catch { return { name: "ac-24", version: "0.0.0" }; }
})();

const argv = process.argv.slice(2);
const cmd = argv[0];

// ── help ──────────────────────────────────────────────────────────────
function usage() {
  console.log(`ac-24 ${PKG.version}  — aesthetic 24 recap pipeline\n`);
  console.log(`Subcommands:`);
  console.log(`  build <audience>     run the full pipeline for an audience`);
  console.log(`  build <audience> --skip-tts    reuse existing recap.mp3 (now redundant — tts.mjs auto-skips on hash match)`);
  console.log(`  cache <audience>     show per-step cache state for an audience`);
  console.log(`  version              print version`);
  console.log(`  help                 show this`);
}

// ── build ─────────────────────────────────────────────────────────────
async function build(audience, extraArgs) {
  if (!audience) { console.error("✗ build requires an audience name"); process.exit(2); }
  if (!/^[\w.-]+$/.test(audience)) { console.error(`✗ invalid audience name: ${audience}`); process.exit(2); }
  const audiencePath = `${ROOT}/audience/${audience}.mjs`;
  if (!existsSync(audiencePath)) { console.error(`✗ audience config not found: ${audiencePath}`); process.exit(2); }

  // pipeline.fish carries the source of truth for ordering today; this
  // is a pass-through. When the cache layer lands here, this function
  // will iterate the steps directly with hashed skip/run decisions.
  return await new Promise((resolveProm) => {
    const proc = spawn("fish", ["./pipeline.fish", audience, ...extraArgs], {
      cwd: ROOT,
      stdio: "inherit",
      env: process.env,
    });
    proc.on("close", (code) => {
      process.exitCode = code;
      resolveProm();
    });
    proc.on("error", (err) => {
      console.error(`✗ failed to spawn pipeline.fish: ${err.message}`);
      process.exitCode = 1;
      resolveProm();
    });
  });
}

// ── cache (read-only inspector) ───────────────────────────────────────
function cacheStatus(audience) {
  const out = `${ROOT}/out`;
  const fmt = (p) => {
    if (!existsSync(p)) return "MISSING";
    const stat = statSync(p);
    return `${(stat.size / 1024).toFixed(0)} KB · ${stat.mtime.toISOString()}`;
  };
  const items = [
    [`tts (recap.mp3)`,         `${out}/recap.mp3`,       `${out}/recap.mp3.hash`],
    [`transcribe (words.json)`, `${out}/words.json`,      `${out}/words.json.hash`],
    [`align (segments.json)`,   `${out}/segments.json`,   `${out}/segments.json.hash`],
    [`subs.json`,               `${out}/subs.json`,       null],
    [`subtitle-track.txt`,      `${out}/subtitle-track.txt`, null],
    [`waltz.mp3`,               `${out}/waltz.mp3`,       null],
    [`recap.mp4`,               `${out}/recap.mp4`,       null],
  ];
  console.log(`ac-24 cache · ${audience || "(no audience filter)"}`);
  for (const [label, file, hashFile] of items) {
    const hash = hashFile && existsSync(hashFile)
      ? readFileSync(hashFile, "utf8").trim().slice(0, 16)
      : null;
    console.log(`  ${label.padEnd(26)} ${fmt(file)}${hash ? ` · hash ${hash}` : ""}`);
  }
  console.log(``);
  console.log(`  jeffrey-photos: ${(() => {
    try { return require("node:fs").readdirSync(`${out}/jeffrey-photos`).length + " files"; }
    catch { return "MISSING"; }
  })()}`);
}

// ── dispatch ──────────────────────────────────────────────────────────
const positional = argv.filter((a) => !a.startsWith("--"));
const flags = argv.filter((a) => a.startsWith("--"));

switch (cmd) {
  case "build":
    await build(positional[1], flags);
    break;
  case "cache":
    cacheStatus(positional[1] || "");
    break;
  case "version":
    console.log(`ac-24 ${PKG.version}`);
    break;
  case "help":
  case "--help":
  case "-h":
  case undefined:
    usage();
    break;
  default:
    console.error(`✗ unknown subcommand: ${cmd}`);
    usage();
    process.exit(2);
}
