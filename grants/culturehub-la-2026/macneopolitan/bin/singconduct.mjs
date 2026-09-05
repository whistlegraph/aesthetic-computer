#!/usr/bin/env node
// singconduct.mjs — a sing-aware .mbscore conductor for the MacNeoPolitan
// lane. Voices with a `lyrics` key are SUNG in realtime through Menu Band's
// `…menuband.say` route: one scheduled say-post per note, syllable from the
// lyrics, pitch = 2^((midi - singBase)/12) clamped to AVSpeech's 0.5–2.0.
// Voices without lyrics fire as normal play payloads. Everything locks to
// one shared startEpoch, same as conduct.mjs (whose poster this reuses).
// This prototypes the schema a future Menu Band sing-engine adoption would
// read natively from playScoreFile.
//
//   node bin/singconduct.mjs scores/mary-had-a-little-lamb.mbscore [host]
//
// Default host is local. Menu Band must be running on the target.

import { existsSync, readFileSync } from "node:fs";
import { hostname } from "node:os";
import { resolve } from "node:path";
import { spawn, spawnSync } from "node:child_process";

const PLAY = "computer.aestheticcomputer.menuband.play";
const SAY = "computer.aestheticcomputer.menuband.say";

// Same poster as slab/menuband/bin/conduct.mjs — build on demand.
const POSTER_SRC = `import Foundation
let env = ProcessInfo.processInfo.environment
var ui: [String: String] = [:]
if let kv = env["MB_KV"] {
  for pair in kv.split(separator: ";") {
    let p = pair.split(separator: "=", maxSplits: 1)
    if p.count == 2 { ui[String(p[0])] = String(p[1]) }
  }
}
DistributedNotificationCenter.default().postNotificationName(
  NSNotification.Name(env["MB_NAME"]!), object: nil,
  userInfo: ui.isEmpty ? nil : ui, deliverImmediately: true)
RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.06))`;

const MY = new Set([hostname(), hostname().replace(/\.local$/, ""), "local",
  "localhost", "self"].map((s) => s.toLowerCase()));
const isLocal = (h) => MY.has(String(h).toLowerCase());

function ensurePoster(host) {
  const build = "cat > /tmp/mbpost.swift && swiftc -O /tmp/mbpost.swift -o /tmp/mbpost";
  const have = (isLocal(host)
    ? spawnSync("sh", ["-c", "test -x /tmp/mbpost"])
    : spawnSync("ssh", ["-o", "ConnectTimeout=10", host, "test -x /tmp/mbpost"])
  ).status === 0;
  if (have) return true;
  const r = isLocal(host)
    ? spawnSync("sh", ["-c", build], { input: POSTER_SRC, encoding: "utf8" })
    : spawnSync("ssh", ["-o", "ConnectTimeout=15", host, build], { input: POSTER_SRC, encoding: "utf8" });
  return r.status === 0;
}

function post(host, hook, kv) {
  return new Promise((res) => {
    const p = isLocal(host)
      ? spawn("/tmp/mbpost", [], { env: { ...process.env, MB_NAME: hook, MB_KV: kv }, stdio: "ignore" })
      : spawn("ssh", ["-o", "ConnectTimeout=10", host, `MB_NAME=${hook} MB_KV='${kv}' /tmp/mbpost`], { stdio: "ignore" });
    p.on("close", (c) => res(c === 0));
    p.on("error", () => res(false));
  });
}

// ---- main ----
const args = process.argv.slice(2);
const vox = args.includes("--vox");
const [scorePath, hostArg] = args.filter((a) => !a.startsWith("--"));
if (!scorePath) {
  console.log("usage: node bin/singconduct.mjs <score.mbscore> [host] [--vox]");
  console.log("  --vox: pre-render the sung line (WORLD stretch+pitch via");
  console.log("  bin/vox.py, cached next to the score) and play it at the");
  console.log("  downbeat instead of firing per-syllable say posts.");
  process.exit(1);
}
const host = hostArg || "local";
const score = JSON.parse(readFileSync(resolve(scorePath), "utf8"));
const bpm = score.bpm || 120;
const beat = 60 / bpm;
const lead = score.lead ?? 3.0;
const epoch = Date.now() / 1000 + Math.max(0.5, lead);

if (!ensurePoster(host)) {
  console.error(`✗ could not build /tmp/mbpost on ${host}`);
  process.exit(1);
}

console.log(`♪ ${score.title} — ${bpm} bpm, downbeat in ${lead}s on ${host}`);
const posts = [];
let voiceIdx = -1;
for (const voice of score.voices || []) {
  voiceIdx++;
  if (voice.lyrics && vox) {
    // Voxxed mode: render (or reuse) the sung line and play it at the
    // downbeat. afplay has no clock, so spawn ~120ms early to cover its
    // startup; the vocal bypasses Menu Band's FX chain for now — feeding
    // it through the engine is the sing-adoption step.
    if (!isLocal(host)) {
      console.log(`  ⚠ ${voice.name}: --vox is local-only for now, skipping`);
      continue;
    }
    const singVoice = voice.singVoice || "Fred";
    const wav = resolve(scorePath).replace(
      /\.mbscore$/, `.vox${voiceIdx}-${singVoice.toLowerCase()}.wav`);
    if (!existsSync(wav)) {
      console.log(`  ⌛ ${voice.name}: rendering vox line (first run)…`);
      const py = resolve(process.env.HOME, "aesthetic-computer/pop/.venv/bin/python");
      const r = spawnSync(py, [
        resolve(scorePath, "..", "..", "bin", "vox.py"),
        resolve(scorePath), String(voiceIdx), wav,
      ], { encoding: "utf8" });
      if (r.status !== 0) {
        console.error(`  ✗ vox render failed:\n${r.stderr}`);
        continue;
      }
    }
    const waitMs = Math.max(0, epoch * 1000 - Date.now() - 120);
    setTimeout(() => spawn("afplay", [wav], { stdio: "ignore" }), waitMs);
    console.log(`  ✎ ${voice.name}: voxxed line at the downbeat (${wav.split("/").pop()})`);
    continue;
  }
  if (voice.lyrics) {
    const base = Number(voice.singBase) || 60;
    const singVoice = voice.singVoice || "Fred";
    const syllables = String(voice.lyrics).trim().split(/\s+/);
    let beats = 0;
    let s = 0;
    for (const token of String(voice.notes).split(",")) {
      const [tok, len] = token.split(":");
      const dur = parseFloat(len) || 1;
      if (tok !== "r") {
        const midi = parseFloat(tok);
        const syl = syllables[s++] ?? "la";
        const pitch = Math.min(2, Math.max(0.5, 2 ** ((midi - base) / 12)));
        const at = (epoch + beats * beat).toFixed(3);
        posts.push(post(host, SAY,
          `text=${syl};voice=${singVoice};pitch=${pitch.toFixed(3)};startEpoch=${at}`));
      }
      beats += dur;
    }
    console.log(`  ✎ ${voice.name}: ${s} sung syllables over ${beats} beats`);
  } else {
    const kv = [`bpm=${bpm}`, `startEpoch=${epoch.toFixed(3)}`];
    for (const [k, v] of Object.entries(voice)) if (k !== "name") kv.push(`${k}=${v}`);
    if (score.title) kv.push(`title=${score.title.replace(/[;'=]/g, " ").trim()}`);
    posts.push(post(host, PLAY, kv.join(";")));
    console.log(`  ♫ ${voice.name}: play payload`);
  }
}
const oks = await Promise.all(posts);
console.log(oks.every(Boolean) ? `  ✓ ${oks.length} posts sent` : "  ⚠ some posts failed");
