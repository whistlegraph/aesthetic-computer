#!/usr/bin/env node
// Resolve every track of a physical release to a Red Book master
// (44.1 kHz, 16-bit, stereo) under pop/physical/masters/.
//
// Three ways in, per pop/physical/masters.json: an archived master from
// the shelf-sync Space or the AC CDN, a deterministic re-render from the
// lane, or — when a render's vocal stems are lost — a decode of the
// released 320 k mp3, which the manifest marks lossy so the disc never
// pretends otherwise.

import { createHash } from "node:crypto";
import { execFileSync, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../../..");
const argv = process.argv.slice(2);
const value = (name, fallback) => {
  const index = argv.indexOf(`--${name}`);
  return index === -1 ? fallback : argv[index + 1];
};
const force = argv.includes("--force");
const only = value("only");

const program = JSON.parse(readFileSync(resolve(value("program", resolve(HERE, "../pixsies-so-far.json"))), "utf8"));
const recipes = JSON.parse(readFileSync(resolve(HERE, "../masters.json"), "utf8"));
const out = resolve(value("out", resolve(HERE, "../masters")));
const scratch = resolve(out, ".src");
mkdirSync(scratch, { recursive: true });

const run = (command, args, options = {}) => {
  const result = spawnSync(command, args, { cwd: ROOT, stdio: "inherit", ...options });
  if (result.status !== 0) throw new Error(`${command} exited ${result.status}`);
};

const probe = (path, entries) => execFileSync("ffprobe", [
  "-v", "error", "-show_entries", entries, "-of", "default=nw=1:nk=1", path,
], { encoding: "utf8" }).trim().split("\n");

const seconds = (path) => Number(probe(path, "format=duration")[0]);
const digest = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");
const clock = (s) => `${Math.floor(Math.round(s) / 60)}:${String(Math.round(s) % 60).padStart(2, "0")}`;

// ffmpeg's ebur128 reports integrated loudness and true peak on stderr.
function loudness(path) {
  const { stderr } = spawnSync("ffmpeg", [
    "-hide_banner", "-nostats", "-i", path, "-af", "ebur128=peak=true", "-f", "null", "-",
  ], { encoding: "utf8" });
  const summary = stderr.slice(stderr.lastIndexOf("Integrated loudness"));
  const lufs = summary.match(/I:\s*(-?[\d.]+) LUFS/);
  const peak = summary.match(/Peak:\s*(-?[\d.]+) dBFS/);
  return { lufs: lufs ? Number(lufs[1]) : null, truePeakDb: peak ? Number(peak[1]) : null };
}

function fetchArchive(recipe, path) {
  if (!force && existsSync(path)) return;
  run("aws", ["--endpoint-url", recipes.endpoint, "s3", "cp", `${recipes.space}/${recipe.key}`, path, "--quiet"]);
}

async function fetchUrl(url, path) {
  if (!force && existsSync(path)) return;
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${response.status} downloading ${url}`);
  writeFileSync(path, Buffer.from(await response.arrayBuffer()));
}

function render(recipe, slug) {
  const fill = (text) => text.replace(/\{scratch\}/g, scratch).replace(/\{home\}/g, homedir());
  const produces = resolve(ROOT, fill(recipe.produces));
  if (!force && existsSync(produces)) return produces;
  const [command, ...args] = recipe.command.map(fill);
  process.stdout.write(`  rendering ${slug} — this is the lane's own engine, not a copy\n`);
  run(command, args);
  if (!existsSync(produces)) throw new Error(`${slug} render did not produce ${produces}`);
  return produces;
}

// Everything converges here: Red Book rate and depth, with the lane's
// documented master chain applied first when the render is pre-master.
function redbook(source, target, chainSpec) {
  const filters = [chainSpec, "aresample=44100:dither_method=triangular"]
    .filter(Boolean)
    .join(",");
  run("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", source,
    "-af", filters, "-ac", "2", "-c:a", "pcm_s16le", target], { stdio: "inherit" });
}

const entries = [];
for (const [index, track] of program.tracks.entries()) {
  const recipe = recipes.tracks[track.slug];
  if (!recipe) throw new Error(`No master recipe for ${track.slug}`);
  if (only && only !== track.slug) continue;
  const number = String(index + 1).padStart(2, "0");
  const target = resolve(out, `${number}-${track.slug}.wav`);
  const referencePath = resolve(scratch, `.${track.slug}.reference.mp3`);
  await fetchUrl(track.audio, referencePath);
  const reference = seconds(referencePath);

  let source;
  let lossy = false;
  if (recipe.from === "archive") {
    source = resolve(scratch, `${track.slug}.archive.wav`);
    fetchArchive(recipe, source);
  } else if (recipe.from === "cdn") {
    source = resolve(scratch, `${track.slug}.cdn.wav`);
    await fetchUrl(recipe.url, source);
  } else if (recipe.from === "render") {
    source = render(recipe, track.slug);
  } else if (recipe.from === "released-mp3") {
    source = referencePath;
    lossy = true;
  } else {
    throw new Error(`Unknown source "${recipe.from}" for ${track.slug}`);
  }

  if (force || !existsSync(target)) redbook(source, target, recipe.master);
  const duration = seconds(target);
  const drift = duration - reference;
  if (Math.abs(drift) > 0.5) {
    throw new Error(`${track.slug} master is ${clock(duration)}, the release is ${clock(reference)} (${drift.toFixed(3)}s apart)`);
  }
  const level = loudness(target);
  entries.push({
    position: index + 1,
    slug: track.slug,
    title: track.title,
    file: `${number}-${track.slug}.wav`,
    from: recipe.from,
    lossy,
    why: recipe.why,
    durationSeconds: duration,
    releaseDriftSeconds: Number(drift.toFixed(3)),
    lufs: level.lufs,
    truePeakDb: level.truePeakDb,
    bytes: statSync(target).size,
    sha256: digest(target),
  });
  console.log(`${number} ${track.slug.padEnd(18)} ${clock(duration)}  ${recipe.from.padEnd(13)} ${String(level.lufs).padStart(6)} LUFS  ${String(level.truePeakDb).padStart(6)} dBTP${lossy ? "  (lossy)" : ""}`);
}

if (!only) {
  writeFileSync(resolve(out, "manifest.json"), `${JSON.stringify({
    schemaVersion: 1,
    builtAt: new Date().toISOString(),
    program: program.title,
    format: { sampleRate: 44100, bitDepth: 16, channels: 2 },
    lossless: entries.filter((entry) => !entry.lossy).length,
    tracks: entries,
  }, null, 2)}\n`);
  const lossy = entries.filter((entry) => entry.lossy).map((entry) => entry.slug);
  console.log(`\n${out}\n${entries.length} masters · ${entries.length - lossy.length} lossless${lossy.length ? ` · from the released mp3: ${lossy.join(", ")}` : ""}`);
}
