#!/usr/bin/env node
// conduct.mjs — play a saved Menu Band fleet score across N computers.
//
// A score (`scores/*.mbscore.json`) is a durable, re-runnable composition:
// it declares how many computers it is composed for and one voice per
// computer. This runner assigns each voice to a host, deploys the tiny
// `/tmp/mbpost` poster where needed, and fires every voice at ONE shared
// `startEpoch` so the machines lock to the same downbeat (NTP-synced clocks;
// no LAN or Multipeer needed — it posts directly on each host).
//
//   node bin/conduct.mjs --list                       # list saved scores
//   node bin/conduct.mjs prelude-in-c                 # show requirements
//   node bin/conduct.mjs prelude-in-c blueberry neo   # perform it
//
// A host token is run locally when it matches this machine's hostname (or is
// `local`/`localhost`/`self`); otherwise it is reached over ssh.

import { readdirSync, readFileSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join, resolve } from "node:path";
import { hostname } from "node:os";
import { spawn, spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SCORES_DIR = resolve(HERE, "..", "scores");
const HOOK = "computer.aestheticcomputer.menuband.play";

// The poster (mirrors slab/menuband LLM guide): reads MB_NAME + MB_KV env and
// posts one DistributedNotification. Deployed + compiled on demand per host.
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

const MY_NAMES = new Set(
  [hostname(), hostname().replace(/\.local$/, ""), "local", "localhost", "self", ""]
    .map((s) => s.toLowerCase()),
);
const isLocal = (host) => MY_NAMES.has(String(host).toLowerCase());

function sh(cmd, args) {
  return spawnSync(cmd, args, { encoding: "utf8" });
}

// Sum the beats of a voice's longest track so we can report a duration.
function durationSec(voice, bpm) {
  const trackBeats = (s) =>
    String(s || "")
      .split(",")
      .reduce((a, tok) => a + (parseFloat(tok.split(":")[1]) || 0), 0);
  const beats = Math.max(
    ...["notes", "notes2", "notes3", "notes4"].map((k) => trackBeats(voice[k])),
    0,
  );
  return (beats / bpm) * 60;
}

function loadScore(nameOrPath) {
  let p = nameOrPath;
  if (!p.includes("/") && !p.endsWith(".mbscore") && !p.endsWith(".json")) p = join(SCORES_DIR, `${p}.mbscore`);
  else p = resolve(p);
  if (!existsSync(p)) {
    console.error(`✗ no score at ${p}`);
    process.exit(1);
  }
  const score = JSON.parse(readFileSync(p, "utf8"));
  score.machines ??= score.voices.length;
  return { score, path: p };
}

function listScores() {
  if (!existsSync(SCORES_DIR)) return console.log("(no scores dir yet)");
  const files = readdirSync(SCORES_DIR).filter((f) => f.endsWith(".mbscore"));
  if (!files.length) return console.log("(no scores yet)");
  console.log("Saved Menu Band scores:\n");
  for (const f of files) {
    const s = JSON.parse(readFileSync(join(SCORES_DIR, f), "utf8"));
    const dur = Math.max(...s.voices.map((v) => durationSec(v, s.bpm || 120)));
    const id = f.replace(/\.mbscore$/, "");
    console.log(`  ${id}`);
    console.log(`    ${s.title} — ${s.composer || "?"}`);
    console.log(`    composed for ${s.machines || s.voices.length} computers · ${s.bpm} bpm · ~${dur.toFixed(0)}s`);
    console.log(`    voices: ${s.voices.map((v) => v.name).join(", ")}\n`);
  }
}

// Ensure /tmp/mbpost exists+executable on a host, building it if missing.
function ensurePoster(host) {
  const build = "cat > /tmp/mbpost.swift && swiftc -O /tmp/mbpost.swift -o /tmp/mbpost";
  const run = (cmd, args) =>
    spawnSync(cmd, args, { input: POSTER_SRC, encoding: "utf8" });
  const already =
    (isLocal(host)
      ? sh("sh", ["-c", "test -x /tmp/mbpost"])
      : sh("ssh", ["-o", "ConnectTimeout=10", host, "test -x /tmp/mbpost"])
    ).status === 0;
  if (already) return true;
  process.stdout.write(`    building poster on ${host}… `);
  const r = isLocal(host)
    ? run("sh", ["-c", build])
    : run("ssh", ["-o", "ConnectTimeout=15", host, build]);
  const ok = r.status === 0;
  console.log(ok ? "ok" : `FAILED\n${r.stderr || ""}`);
  return ok;
}

function kvFor(voice, bpm, epoch) {
  const kv = [`bpm=${bpm}`];
  for (const [k, val] of Object.entries(voice)) {
    if (k === "name") continue;
    kv.push(`${k}=${val}`);
  }
  kv.push(`startEpoch=${epoch}`);
  return kv.join(";");
}

const SAY = "computer.aestheticcomputer.menuband.say";
// Distinct AVSpeech voices so each machine sounds like its own character.
const SAY_VOICES = ["Samantha", "Daniel", "Karen", "Fred", "Alex", "Victoria"];
// Friendly name for greetings + assignment (first label of the host).
const shortName = (h) => (isLocal(h) ? hostname() : h).split(".")[0];

// Post a payload (play or say) to a host; resolves to true on a clean send.
function post(host, hook, kv) {
  return new Promise((res) => {
    const p = isLocal(host)
      ? spawn("/tmp/mbpost", [], { env: { ...process.env, MB_NAME: hook, MB_KV: kv }, stdio: "ignore" })
      : spawn("ssh", ["-o", "ConnectTimeout=10", host, `MB_NAME=${hook} MB_KV='${kv}' /tmp/mbpost`], { stdio: "ignore" });
    p.on("close", (code) => res(code === 0));
    p.on("error", () => res(false));
  });
}

const firePlay = (host, voice, bpm, epoch) => post(host, HOOK, kvFor(voice, bpm, epoch));
const fireSay = (host, text, voiceName, epoch) =>
  post(host, SAY, `text=${text};voice=${voiceName};startEpoch=${epoch}`);

// ---- main ----
const argv = process.argv.slice(2);
if (!argv.length || argv[0] === "--list" || argv[0] === "list") {
  listScores();
  process.exit(0);
}

const { score } = loadScore(argv[0]);
const flags = argv.slice(1).filter((a) => a.startsWith("--"));
const hosts = argv.slice(1).filter((a) => !a.startsWith("--"));
const need = score.machines;
const talk = !flags.includes("--quiet"); // machines greet + sign off unless silenced

console.log(`\n♪ ${score.title}`);
console.log(`  ${score.composer || ""}`);
if (score.description) console.log(`  ${score.description}`);
const dur = Math.max(...score.voices.map((v) => durationSec(v, score.bpm)));
console.log(`  composed for ${need} computers · ${score.bpm} bpm · ~${dur.toFixed(0)}s · ${score.voices.length} voices`);

if (hosts.length < need) {
  console.log(`\n  This score needs ${need} computers. You gave ${hosts.length}.`);
  console.log(`  Perform it with:  node bin/conduct.mjs ${argv[0]} host1 ${need > 1 ? "host2 …" : ""}`.trimEnd());
  console.log(`  (a host is run locally if it matches this machine's name, else over ssh)\n`);
  process.exit(hosts.length ? 1 : 0);
}

const roster = hosts.slice(0, need);
console.log(`\n  assignment:`);
roster.forEach((h, i) =>
  console.log(`    ${score.voices[i].name.padEnd(16)} → ${shortName(h)}${isLocal(h) ? " (local)" : ` (${h})`}`),
);

console.log(`\n  deploying posters…`);
const ready = roster.map((h) => ensurePoster(h));

// Estimated spoken length (AVSpeech default ≈ 13 chars/sec) so lines are
// sequenced around how long each actually takes — no overlap, no dead air.
const sayDur = (t) => t.length / 13 + 0.7;
const SPEAK_GAP = 0.55; // breath between two speakers
const PRE_DOWNBEAT = 1.2; // beat of silence after the last greeting

const greetText = (i) => {
  const others = roster.filter((_, j) => j !== i).map(shortName).join(" and ") || "everyone";
  return `Hi ${others}, this is ${shortName(roster[i])}. I have the ${score.voices[i].name}.`;
};
const byeText = (i) => (i === 0 ? `That was fun. Goodbye!` : `Bye from ${shortName(roster[i])}.`);

// Build the whole timeline up front, in wall-clock epochs.
const now = Date.now() / 1000;
const greetAt = new Array(roster.length);
let cursor = now + 0.6;
if (talk)
  for (let i = 0; i < roster.length; i++) {
    greetAt[i] = cursor;
    cursor += sayDur(greetText(i)) + SPEAK_GAP;
  }
const downbeat = (talk ? cursor + PRE_DOWNBEAT : now + (score.lead ?? 3.0));
const byeAt = new Array(roster.length);
let bcur = downbeat + dur + 1.2;
if (talk)
  for (let i = 0; i < roster.length; i++) {
    byeAt[i] = bcur;
    bcur += sayDur(byeText(i)) + SPEAK_GAP;
  }

async function run() {
  const sends = []; // { host, kind, ok }

  if (talk) {
    console.log(`\n  greetings…`);
    for (let i = 0; i < roster.length; i++)
      if (ready[i])
        sends.push({ host: roster[i], kind: "greeting", ok: await fireSay(roster[i], greetText(i), SAY_VOICES[i % SAY_VOICES.length], greetAt[i].toFixed(3)) });
  }

  console.log(`\n  downbeat at epoch ${downbeat.toFixed(3)} (in ${(downbeat - Date.now() / 1000).toFixed(1)}s)…`);
  for (let i = 0; i < roster.length; i++)
    sends.push({ host: roster[i], kind: "music", ok: ready[i] ? await firePlay(roster[i], score.voices[i], score.bpm, downbeat.toFixed(3)) : false });

  if (talk) {
    for (let i = 0; i < roster.length; i++)
      if (ready[i])
        sends.push({ host: roster[i], kind: "farewell", ok: await fireSay(roster[i], byeText(i), SAY_VOICES[i % SAY_VOICES.length], byeAt[i].toFixed(3)) });
  }

  // Honest reporting: which voices actually accepted their cues.
  const played = roster.filter((_, i) => sends.some((s) => s.host === roster[i] && s.kind === "music" && s.ok));
  const failed = sends.filter((s) => !s.ok);
  console.log("");
  if (failed.length === 0) {
    console.log(`  ✓ ${score.title} — ${played.length}/${need} computers cued. (~${dur.toFixed(0)}s${talk ? " + intro/outro" : ""})\n`);
  } else {
    for (const f of failed) console.log(`  ✗ ${shortName(f.host)} — ${f.kind} did not send (host unreachable / poster missing).`);
    console.log(`  ⚠ ${played.length}/${need} computers cued — see failures above.\n`);
    process.exitCode = 1;
  }
}
run();
