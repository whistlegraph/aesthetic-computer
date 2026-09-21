#!/usr/bin/env node
// trio.mjs — conduct one MacNeoPolitan movement across the band.
//
//   node bin/trio.mjs scores/trio-i-birth.mbscore neo blueberry blush
//   node bin/trio.mjs scores/trio-i-birth.mbscore neo blueberry --reduce
//   node bin/trio.mjs --setlist scores/setlist.json neo blueberry blush
//
// One host per voice, in order. A host is local when it matches this
// machine's LocalHostName; otherwise it is reached over ssh. Everything
// locks to one shared downbeat, corrected for each host's measured clock
// skew (min-RTT sampling over a persistent ssh pipe, as perform.mjs does).
//
// What a voice may carry beyond the ordinary play keys:
//   lyrics / singVoice — the voice SINGS, live: at cue time the member's
//     own speech synthesizer speaks the line into live/hosts/macos/livesing,
//     which lifts it onto the notes under the member's voice profile
//     (members/<name>/voice.json: lock, vibrato) and sounds it on the
//     downbeat — ask→sound ≈ 0.6 s, so the lead covers it. The lane's rule
//     holds: a member always renders its own text on its own body.
//     `--vox` falls back to the older pre-rendered bin/vox.py wav.
//     lyrics: one token per note; syllables of a word joined by "-".
//   sayVoice — the cast voice used for this member's spoken lines.
//
// What a score may carry beyond voices:
//   intro / outro — [{ voice: i, text }] spoken in sequence by that member
//     (its sayVoice) before the downbeat / after the last note.
//
// --reduce: with fewer hosts than voices, fold each missing instrumental
//   voice onto voice 0 as a parallel track (notes2/3/4) so a rehearsal can
//   happen before the third body arrives. Missing SUNG voices are skipped
//   with a warning — nobody else may sing a member's line.
// --via menuband (default) | livesing: sung parts are posted to the member's
//   Menu Band as a play payload carrying lyrics — Menu Band speaks, sings and
//   sounds the line in its own engine and lights the keys — or fired at the
//   standalone live/hosts/macos/livesing.
// --vox: pre-render sung lines with bin/vox.py (voxplay) instead of live.
// --sim: the simulator — every voice on THIS machine, from one Menu Band:
//   voice i posts with `sim=i/n`, so Menu Band gives it its own singer node
//   (panned across the stage), a tile-sized face and caption at the bottom
//   right of the screen, and every member sings at once. No hosts needed.
// --quiet: skip intro and outro.
// --dry: prepare everything (posters, skews, vox renders), schedule nothing.

import { existsSync, readFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { hostname } from "node:os";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn, spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const PLAY = "computer.aestheticcomputer.menuband.play";
const SAY = "computer.aestheticcomputer.menuband.say";
const REMOTE_DIR = "/tmp/mnp";
const REMOTE_PY = "~/aesthetic-computer/pop/.venv/bin/python";
const LOCAL_PY = resolve(process.env.HOME, "aesthetic-computer/pop/.venv/bin/python");
const VOX_PREROLL = "0.5"; // baked into every render by bin/vox.py and livesing
const LIVE_DIR = resolve(HERE, "..", "..", "..", "..", "live");   // the singer core + macOS host
const LIVESING = "~/aesthetic-computer/live/hosts/macos/livesing";

// ---- who am I -------------------------------------------------------------
const localName = (() => {
  const r = spawnSync("scutil", ["--get", "LocalHostName"], { encoding: "utf8" });
  return (r.stdout || "").trim() || hostname().split(".")[0];
})();
const MY = new Set([localName, hostname(), hostname().split(".")[0], "local",
  "localhost", "self"].map((s) => s.toLowerCase()));
const isLocal = (h) => MY.has(String(h).toLowerCase());
const shortName = (h) => (isLocal(h) ? localName : String(h).split(".")[0]);

// ---- tiny swift helpers, built on demand per host --------------------------
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

const VOXPLAY_SRC = `import AVFoundation
let a = CommandLine.arguments
let url = URL(fileURLWithPath: a[1])
let epoch = Double(a[2])!
let preroll = a.count > 3 ? (Double(a[3]) ?? 0) : 0
let player = try! AVAudioPlayer(contentsOf: url)
player.prepareToPlay()
let delay = (epoch - preroll) - Date().timeIntervalSince1970
if delay <= 0 { player.play() }
else { player.play(atTime: player.deviceCurrentTime + delay) }
RunLoop.main.run(until: Date(timeIntervalSinceNow: max(0, delay) + player.duration + 0.5))`;

// Run a shell snippet on a host (bash -s so fish login shells are fine).
function shOn(host, script, input) {
  return isLocal(host)
    ? spawnSync("bash", ["-s"], { input: script + (input ? `\n${input}` : ""), encoding: "utf8" })
    : spawnSync("ssh", ["-o", "ConnectTimeout=15", host, "bash -s"],
        { input: script + (input ? `\n${input}` : ""), encoding: "utf8" });
}

function ensureTool(host, name, src) {
  const have = shOn(host, `test -x /tmp/${name}`).status === 0;
  if (have) return true;
  process.stdout.write(`    building ${name} on ${shortName(host)}… `);
  const r = shOn(host, `cat > /tmp/${name}.swift <<'SWIFT_EOF'\n${src}\nSWIFT_EOF\nswiftc -O /tmp/${name}.swift -o /tmp/${name}`);
  console.log(r.status === 0 ? "ok" : `FAILED\n${r.stderr}`);
  return r.status === 0;
}

function post(host, hook, kv) {
  if (show) { console.log(`\n  [${shortName(host)}] ${hook.split(".").pop()}\n    ${kv.split(";").map((x) => x.length > 160 ? x.slice(0, 157) + "…" : x).join("\n    ")}`); return Promise.resolve(true); }
  return new Promise((res) => {
    const p = isLocal(host)
      ? spawn("/tmp/mbpost", [], { env: { ...process.env, MB_NAME: hook, MB_KV: kv }, stdio: "ignore" })
      : spawn("ssh", ["-o", "ConnectTimeout=10", host, `MB_NAME=${hook} MB_KV='${kv.replace(/'/g, "'\\''")}' /tmp/mbpost`], { stdio: "ignore" });   // lyrics carry apostrophes
    p.on("close", (c) => res(c === 0));
    p.on("error", () => res(false));
  });
}

// ---- clock skew (host clock minus ours), min-RTT over a persistent pipe ----
function measureSkew(host) {
  return new Promise((done) => {
    if (isLocal(host)) return done(0);
    const probe = spawn("ssh", [host,
      'python3 -u -c "import sys,time\nfor line in sys.stdin: print(time.time(), flush=True)"']);
    const samples = [];
    let t0 = 0;
    const ping = () => { t0 = Date.now() / 1000; probe.stdin.write("x\n"); };
    const finish = () => {
      try { probe.kill(); } catch {}
      if (!samples.length) return done(0);
      samples.sort((a, b) => a.rtt - b.rtt);
      done(samples[0].off);
    };
    probe.stdout.on("data", (d) => {
      const t1 = Date.now() / 1000;
      const remote = Number(String(d).trim().split("\n").at(-1));
      samples.push({ rtt: t1 - t0, off: remote - (t0 + t1) / 2 });
      samples.length >= 15 ? finish() : ping();
    });
    probe.on("error", () => done(0));
    setTimeout(finish, 15000);
    setTimeout(ping, 400);
  });
}

// ---- the sung line: rendered on the member's own body ----------------------
function voxCacheName(scorePath, voiceIdx, voice) {
  const hash = createHash("sha1")
    .update(JSON.stringify([voice.notes, voice.lyrics, voice.singVoice, voice.singVibratoHz, score.bpm]))
    .digest("hex").slice(0, 8);
  const singVoice = (voice.singVoice || "Fred").toLowerCase().replace(/[^a-z0-9]/g, "");
  return `${basename(scorePath, ".mbscore")}.vox${voiceIdx}-${singVoice}-${hash}.wav`;
}

function renderVox(host, scorePath, voiceIdx, voice) {
  const wavName = voxCacheName(scorePath, voiceIdx, voice);
  const scoreJson = readFileSync(scorePath, "utf8");
  const voxPy = readFileSync(resolve(HERE, "vox.py"), "utf8");
  const remoteScore = `${REMOTE_DIR}/${basename(scorePath)}`;
  const remoteWav = `${REMOTE_DIR}/${wavName}`;
  const py = isLocal(host) ? LOCAL_PY : REMOTE_PY;
  // Ship the score + renderer, then render only if the cache misses.
  const script = [
    `mkdir -p ${REMOTE_DIR}`,
    `cat > ${remoteScore} <<'SCORE_EOF'\n${scoreJson}\nSCORE_EOF`,
    `cat > ${REMOTE_DIR}/vox.py <<'VOX_EOF'\n${voxPy}\nVOX_EOF`,
    `if [ -f ${remoteWav} ]; then echo cached; else ${py} ${REMOTE_DIR}/vox.py ${remoteScore} ${voiceIdx} ${remoteWav} >/dev/null && echo rendered; fi`,
  ].join("\n");
  const r = shOn(host, script);
  const status = (r.stdout || "").trim().split("\n").at(-1);
  if (r.status !== 0 || !["cached", "rendered"].includes(status)) {
    console.log(`    ✗ ${voice.name}: vox render failed on ${shortName(host)}\n${r.stderr}`);
    return null;
  }
  console.log(`    ${voice.name}: sung line ${status} on ${shortName(host)} (${wavName})`);
  return remoteWav;
}

function scheduleVox(host, wav, epoch) {
  const cmd = `/tmp/voxplay ${wav} ${epoch} ${VOX_PREROLL} >/dev/null 2>&1 &`;
  const p = isLocal(host)
    ? spawn("bash", ["-c", cmd], { detached: true, stdio: "ignore" })
    : spawn("ssh", ["-o", "ConnectTimeout=10", host, cmd], { detached: true, stdio: "ignore" });
  p.unref();
  return true;
}

// ---- the live singer: built on the member, fed the profile ------------------
function ensureLivesing(host) {
  const have = shOn(host, `test -x ${LIVESING}`).status === 0;
  if (have) return true;
  process.stdout.write(`    building livesing on ${shortName(host)}… `);
  if (!isLocal(host)) {
    const r = spawnSync("rsync", ["-aq", "--exclude", "*.o", "--exclude", "libsinger.a", "--exclude", "singer",
      "--exclude", "hosts/macos/livesing", LIVE_DIR + "/", `${host}:aesthetic-computer/live/`], { encoding: "utf8" });
    if (r.status !== 0) { console.log(`FAILED (rsync)\n${r.stderr}`); return false; }
  }
  const b = shOn(host, `cd ~/aesthetic-computer/live && hosts/macos/build.sh`);
  console.log(b.status === 0 ? "ok" : `FAILED\n${b.stderr}`);
  return b.status === 0;
}

// The speech cache (bin/stems.mjs): a member whose profile names a recorded
// speech provider has, per lyric, a stem.wav + stem.meta.json. Ship both to
// the host and livesing sings the remembered speech — offline, still live.
function shipStem(host, memberName, lyrics) {
  const prof = JSON.parse(readFileSync(resolve(HERE, "..", "members", memberName, "voice.json"), "utf8"));
  const speech = prof.aesthetivox?.speech;
  if (!speech?.provider) return null;
  const text = bareLyrics(lyrics).split(/\s+/).map((t) => t.split("-").join("")).join(" ");
  const hash = createHash("sha1").update(`${speech.provider}·${speech.voice || ""}·${text}`).digest("hex").slice(0, 12);
  const dir = resolve(HERE, "..", "members", memberName, "speech", hash);
  if (!existsSync(resolve(dir, "stem.wav")) || !existsSync(resolve(dir, "stem.meta.json"))) {
    console.log(`    ⚠ ${memberName}: no cached speech for this lyric (run bin/stems.mjs) — falling back to its synthesizer`);
    return null;
  }
  const remote = `${REMOTE_DIR}/speech-${memberName}-${hash}`;
  if (isLocal(host)) return { stem: resolve(dir, "stem.wav"), meta: resolve(dir, "stem.meta.json") };
  const r = spawnSync("sh", ["-c", `ssh -o ConnectTimeout=10 ${host} 'mkdir -p ${remote}' && scp -q ${resolve(dir, "stem.wav")} ${resolve(dir, "stem.meta.json")} ${host}:${remote}/`], { encoding: "utf8" });
  return r.status === 0 ? { stem: `${remote}/stem.wav`, meta: `${remote}/stem.meta.json` } : null;
}

function shipProfile(host, memberName) {
  const p = resolve(HERE, "..", "members", memberName, "voice.json");
  if (!existsSync(p)) return null;
  const remote = `${REMOTE_DIR}/${memberName}-voice.json`;
  const r = shOn(host, `mkdir -p ${REMOTE_DIR} && cat > ${remote} <<'PROFILE_EOF'\n${readFileSync(p, "utf8")}\nPROFILE_EOF`);
  return r.status === 0 ? remote : null;
}

// Fire the live singer on the member: it speaks, analyzes, sings, and sounds
// the line at `epoch` — all in the moment, on its own hardware.
function scheduleLive(host, p, epoch) {
  const q = (v) => `'${String(v).replace(/'/g, "'\\''")}'`;
  const args = [`--lyrics ${q(bareLyrics(p.voice.lyrics))}`, `--notes ${q(p.voice.notes)}`, `--bpm ${bpm}`,
    `--voice ${q(p.voice.singVoice || "Fred")}`, `--epoch ${epoch}`,
    `--out ${REMOTE_DIR}/${basename(scorePath, ".mbscore")}-v${p.idx}-live.wav`];
  if (p.profile) args.push(`--profile ${p.profile}`);
  if (p.speech) args.push(`--stem ${p.speech.stem}`, `--meta ${p.speech.meta}`);
  const cmd = `mkdir -p ${REMOTE_DIR}; ${LIVESING} ${args.join(" ")} >/tmp/mnp/livesing-v${p.idx}.log 2>&1 &`;
  const sp = isLocal(host)
    ? spawn("bash", ["-c", cmd], { detached: true, stdio: "ignore" })
    : spawn("ssh", ["-o", "ConnectTimeout=10", host, cmd], { detached: true, stdio: "ignore" });
  sp.unref();
  return true;
}

// ---- the score --------------------------------------------------------------
const argv = process.argv.slice(2);
const flags = argv.filter((a) => a.startsWith("--"));
const words = argv.filter((a) => !a.startsWith("--"));
const reduce = flags.includes("--reduce");
const sim = flags.includes("--sim");
const useVox = flags.includes("--vox");
const via = (flags.find((f) => f.startsWith("--via")) || "").split("=")[1] || (words.includes("livesing") ? "livesing" : "menuband");
const quiet = flags.includes("--quiet");
const dry = flags.includes("--dry");
const show = flags.includes("--show");   // print every payload instead of posting it
const setlistMode = flags.includes("--setlist");

const [scoreArg, ...givenHosts] = words;
const hosts = sim ? [localName] : givenHosts;   // the simulator plays the whole band here
// "/" tokens in lyrics are caption line breaks (compose.mjs puts one at every
// phrase); Menu Band reads them, the older paths (livesing, vox, stems) don't.
const bareLyrics = (l) => String(l).trim().split(/\s+/).filter((t) => t !== "/").join(" ");
if (!scoreArg || !hosts.length) {
  console.log("usage: node bin/trio.mjs <score.mbscore> <host1> [host2 host3] [--reduce] [--quiet] [--dry]");
  console.log("       node bin/trio.mjs <score.mbscore> --sim            (the whole band on this machine)");
  console.log("       node bin/trio.mjs --setlist <setlist.json> <host1> [host2 host3] [--reduce]");
  process.exit(1);
}

if (setlistMode) {
  // A setlist is { "movements": ["scores/a.mbscore", …], "gap": 4.0 }:
  // each movement is conducted in turn by a fresh trio.mjs, waiting for the
  // previous one to finish sounding plus `gap` seconds of hall.
  const list = JSON.parse(readFileSync(resolve(scoreArg), "utf8"));
  const gap = list.gap ?? 4.0;
  const pass = flags.filter((f) => f !== "--setlist");
  console.log(`\n▶ setlist: ${list.title || basename(scoreArg)} — ${list.movements.length} movements\n`);
  for (const mv of list.movements) {
    const mvPath = resolve(dirname(resolve(scoreArg)), mv);
    const r = spawnSync("node", [fileURLToPath(import.meta.url), mvPath, ...hosts, ...pass],
      { stdio: ["ignore", "pipe", "inherit"], encoding: "utf8" });
    process.stdout.write(r.stdout);
    const m = r.stdout.match(/ends at epoch ([\d.]+)/);
    if (!m) { console.log("  ✗ movement did not report an end epoch — stopping setlist"); process.exit(1); }
    const wait = Number(m[1]) + gap - Date.now() / 1000;
    if (wait > 0) await new Promise((res) => setTimeout(res, wait * 1000));
  }
  process.exit(0);
}

const scorePath = resolve(scoreArg);
const score = JSON.parse(readFileSync(scorePath, "utf8"));
const bpm = score.bpm || 120;
const beat = 60 / bpm;
const voices = score.voices || [];
const need = score.machines ?? voices.length;
const beatsOf = (s) => String(s || "").split(",").reduce((a, t) => a + (parseFloat(t.split(":")[1]) || 0), 0);
const voiceBeats = (v) => Math.max(...["notes", "notes2", "notes3", "notes4"].map((k) => beatsOf(v[k])));
const durSec = Math.max(...voices.map(voiceBeats)) * beat;

if (hosts.length < need && !reduce && !sim) {
  console.log(`\n  This movement needs ${need} computers. You gave ${hosts.length}.`);
  console.log(`  Rehearse it anyway with --reduce (missing instrumental parts fold onto ${shortName(hosts[0])}).\n`);
  process.exit(1);
}

// Assignment: voice i → host i. With --reduce, the unhosted voices fold.
const roster = sim ? voices.map(() => hosts[0]) : hosts.slice(0, Math.min(need, hosts.length));
const parts = roster.map((h, i) => ({ host: h, voice: { ...voices[i] }, idx: i, sung: !!voices[i].lyrics,
  sim: sim ? `${i}/${voices.length}` : null }));
const skipped = [];
if (reduce) {
  let track = 2;
  for (let j = roster.length; j < voices.length; j++) {
    const v = voices[j];
    if (v.lyrics) { skipped.push(v.name); continue; }
    if (track > 4) { skipped.push(v.name); continue; }
    parts[0].voice[`notes${track}`] = v.notes;
    if (v.velocity != null) parts[0].voice[`velocity${track}`] = v.velocity;
    parts[0].folded = [...(parts[0].folded || []), v.name];
    track++;
  }
}

console.log(`\n♪ ${score.title} — ${bpm} bpm, ~${durSec.toFixed(0)}s`);
console.log(`  assignment:`);
for (const p of parts)
  console.log(`    ${p.voice.name.padEnd(22)} → ${shortName(p.host)}${isLocal(p.host) ? " (local)" : ""}${p.sim ? `  [sim tile ${p.idx + 1}/${parts.length}]` : ""}${p.sung ? "  ♫ sung" : ""}${p.folded ? `  + folded: ${p.folded.join(", ")}` : ""}`);
for (const s of skipped) console.log(`    ${s.padEnd(22)} → (no body yet — skipped; a member sings only its own line)`);

// ---- prepare: tools, skews, renders ----------------------------------------
console.log(`\n  preparing…`);
const ready = parts.map((p) => ensureTool(p.host, "mbpost", POSTER_SRC));
for (const p of parts) if (p.sung && ready[p.idx]) {
  if (useVox) {
    p.voxOk = ensureTool(p.host, "voxplay", VOXPLAY_SRC);
    if (p.voxOk) p.wav = renderVox(p.host, scorePath, p.idx, p.voice);
  } else if (via === "menuband") {
    const member = String(p.voice.name).split(/\s|·/)[0];
    p.member = member;
    p.speech = shipStem(p.host, member, p.voice.lyrics);
    p.liveOk = true;
    const nNotes = String(p.voice.notes).split(",").filter((t) => !t.startsWith("r:")).length;
    const nSyl = bareLyrics(p.voice.lyrics).split(/\s+/).reduce((a, t) => a + t.split("-").length, 0);
    console.log(`    ${p.voice.name}: sung by Menu Band — ${nNotes} notes / ${nSyl} syllables${nNotes !== nSyl ? "  ⚠ mismatch" : ""}${p.speech ? " · remembered speech" : ""}`);
  } else {
    p.liveOk = ensureLivesing(p.host);
    const member = String(p.voice.name).split(/\s|·/)[0];
    p.profile = shipProfile(p.host, member);
    p.speech = shipStem(p.host, member, p.voice.lyrics);
    const nNotes = String(p.voice.notes).split(",").filter((t) => !t.startsWith("r:")).length;
    const nSyl = bareLyrics(p.voice.lyrics).split(/\s+/).reduce((a, t) => a + t.split("-").length, 0);
    console.log(`    ${p.voice.name}: live — ${nNotes} notes / ${nSyl} syllables${nNotes !== nSyl ? "  ⚠ mismatch" : ""}${p.profile ? ` · profile ${member}` : "  ⚠ no profile"}${p.speech ? " · remembered speech (jeffrey)" : ""}`);
  }
}

console.log(`  measuring clock skew…`);
const skews = {};
for (const p of parts) {
  skews[p.host] = await measureSkew(p.host);
  console.log(`    ${shortName(p.host)}: ${isLocal(p.host) ? "conductor (0.0ms)" : (skews[p.host] * 1000).toFixed(1) + "ms"}`);
}
const skewed = (host, epoch) => (epoch + (skews[host] ?? 0)).toFixed(3);

// ---- timeline ---------------------------------------------------------------
const sayDur = (t) => t.length / 13 + 0.7; // AVSpeech ≈ 13 chars/s
const SPEAK_GAP = 0.6;
const PRE_DOWNBEAT = 1.5;
const sayVoiceOf = (i) => voices[i]?.sayVoice || voices[i]?.singVoice || "Fred";
const spoken = (lines) => (lines || []).filter((l) => parts[l.voice] && !skipped.includes(voices[l.voice]?.name));

const intro = quiet ? [] : spoken(score.intro);
const outro = quiet ? [] : spoken(score.outro);
const now = Date.now() / 1000;
let cursor = now + 2.0;
const introAt = intro.map((l) => { const at = cursor; cursor += sayDur(l.text) + SPEAK_GAP; return at; });
// Live singing needs the lead to cover ssh spawn + speech + analysis (~1.1 s
// measured, 0.7 s headroom at 3.0 s): hold at least 4.5 s when any part sings live.
const anyLive = parts.some((p) => p.sung) && !useVox;
const downbeat = intro.length ? cursor + PRE_DOWNBEAT : now + Math.max(anyLive ? 4.5 : 2.0, score.lead ?? 3.0);
let ocur = downbeat + durSec + 1.5;
const outroAt = outro.map((l) => { const at = ocur; ocur += sayDur(l.text) + SPEAK_GAP; return at; });
const endsAt = outro.length ? ocur : downbeat + durSec;

for (let i = 0; i < intro.length; i++)
  console.log(`  say v${intro[i].voice} @${introAt[i].toFixed(3)} ~${sayDur(intro[i].text).toFixed(2)} "${intro[i].text}"`);
for (let i = 0; i < outro.length; i++)
  console.log(`  say v${outro[i].voice} @${outroAt[i].toFixed(3)} ~${sayDur(outro[i].text).toFixed(2)} "${outro[i].text}"`);

if (dry && !show) { console.log(`\n  --dry: prepared, nothing scheduled.\n`); process.exit(0); }

// ---- fire -------------------------------------------------------------------
const sends = [];
const sayOn = (host, text, voiceName, at) =>
  post(host, SAY, `text=${text.replace(/[;'=]/g, " ")};voice=${voiceName};startEpoch=${at}`);

for (let i = 0; i < intro.length; i++) {
  const l = intro[i];
  sends.push({ kind: "intro", host: parts[l.voice].host, ok: await sayOn(parts[l.voice].host, l.text, sayVoiceOf(l.voice), skewed(parts[l.voice].host, introAt[i])) });
}

console.log(`\n  downbeat at epoch ${downbeat.toFixed(3)} (in ${(downbeat - Date.now() / 1000).toFixed(1)}s)…`);
for (const p of parts) {
  if (!ready[p.idx]) { sends.push({ kind: "music", host: p.host, ok: false }); continue; }
  if (p.sung && !useVox && via === "menuband") {
    // One payload: the sung line (lyrics + notes, track 0 lights keys only),
    // the whistle on notes2.., and the member's profile constraints.
    const vj = p.member ? JSON.parse(readFileSync(resolve(HERE, "..", "members", p.member, "voice.json"), "utf8")) : {};
    const prof = vj.aesthetivox || {};
    const kv = [`bpm=${bpm}`, `startEpoch=${skewed(p.host, downbeat)}`, `program=${p.voice.program ?? 78}`,
      `notes=${p.voice.notes}`, `lyrics=${String(p.voice.lyrics).replace(/[;=]/g, " ")}`,
      `singVoice=${p.voice.singVoice || prof.base_voice || "Fred"}`,
      `singVibratoHz=${p.voice.singVibratoHz ?? prof.sing?.vibrato_hz ?? 5}`,
      `singVibCents=${prof.sing?.vibrato_depth_cents ?? 18}`, `singLock=${prof.sing?.harmony_lock ?? 0.875}`,
      `singF0Floor=${prof.f0_floor ?? 55}`];
    if (p.speech) kv.push(`stemPath=${p.speech.stem}`, `wordsPath=${p.speech.meta}`);
    if (vj.color) kv.push(`captionColor=${vj.color}`);   // the member's color on its caption banner
    if (p.member && !flags.includes("--no-face")) kv.push(`face=${p.member}`);   // its cartoon face, mouth on the onsets
    for (const k of ["notes2", "notes3", "notes4", "velocity2", "velocity3", "velocity4"]) if (p.voice[k] != null) kv.push(`${k}=${p.voice[k]}`);
    if (p.voice.double) {
      const up = Number(p.voice.doubleTranspose) || 0;
      const doubled = String(p.voice.notes).split(",").map((t) => { const [tok, d] = t.split(":"); return /^\d+$/.test(tok) ? `${Number(tok) + up}:${d}` : t; }).join(",");
      const slot = ["notes2", "notes3", "notes4"].find((k) => !p.voice[k]);
      if (slot) kv.push(`${slot}=${doubled}`, `velocity${slot.slice(5)}=${p.voice.doubleVelocity ?? 48}`);
    }
    if (score.title) kv.push(`title=${score.title.replace(/[;'=]/g, " ").trim()}`);
    if (p.sim) kv.push(`sim=${p.sim}`);
    sends.push({ kind: "menuband-sing", host: p.host, ok: await post(p.host, PLAY, kv.join(";")) });
    continue;
  }
  if (p.sung) {
    const at = skewed(p.host, downbeat);
    const ok = useVox ? (p.wav ? scheduleVox(p.host, p.wav, at) : false)
                      : (p.liveOk ? scheduleLive(p.host, p, at) : false);
    sends.push({ kind: useVox ? "sung" : "live", host: p.host, ok });
    // A sung voice may also carry instrumental tracks (notes2..4): the
    // member sings AND its whistle plays — one play payload alongside.
    if (p.voice.notes2) {
      const kv = [`bpm=${bpm}`, `startEpoch=${skewed(p.host, downbeat)}`, `program=${p.voice.program ?? 78}`,
        `notes=${p.voice.notes2}`, `velocity=${p.voice.velocity2 ?? 56}`];
      if (p.voice.notes3) kv.push(`notes2=${p.voice.notes3}`, `velocity2=${p.voice.velocity3 ?? 56}`);
      if (p.voice.notes4) kv.push(`notes3=${p.voice.notes4}`, `velocity3=${p.voice.velocity4 ?? 56}`);
      if (score.title) kv.push(`title=${score.title.replace(/[;'=]/g, " ").trim()}`);
      if (p.sim) kv.push(`sim=${p.sim}`);
      sends.push({ kind: "tracks", host: p.host, ok: await post(p.host, PLAY, kv.join(";")) });
    }
    // A sung voice may also carry a whistle double on its own machine.
    if (p.voice.double) {
      const up = Number(p.voice.doubleTranspose) || 0;
      const doubled = String(p.voice.notes).split(",")
        .map((t) => { const [tok, d] = t.split(":"); return /^\d+$/.test(tok) ? `${Number(tok) + up}:${d}` : t; })
        .join(",");
      const kv = [`bpm=${bpm}`, `startEpoch=${skewed(p.host, downbeat)}`, `program=${p.voice.doubleProgram ?? 78}`,
        `velocity=${p.voice.doubleVelocity ?? 56}`, `notes=${doubled}`];
      if (p.sim) kv.push(`sim=${p.sim}`);
      sends.push({ kind: "double", host: p.host, ok: await post(p.host, PLAY, kv.join(";")) });
    }
    continue;
  }
  const kv = [`bpm=${bpm}`, `startEpoch=${skewed(p.host, downbeat)}`];
  for (const [k, v] of Object.entries(p.voice))
    if (!["name", "lyrics", "singVoice", "singBase", "singVibratoHz", "sayVoice", "double", "doubleProgram", "doubleVelocity", "doubleTranspose"].includes(k))
      kv.push(`${k}=${v}`);
  if (score.title) kv.push(`title=${score.title.replace(/[;'=]/g, " ").trim()}`);
  if (p.sim) kv.push(`sim=${p.sim}`);
  sends.push({ kind: "music", host: p.host, ok: await post(p.host, PLAY, kv.join(";")) });
}

for (let i = 0; i < outro.length; i++) {
  const l = outro[i];
  sends.push({ kind: "outro", host: parts[l.voice].host, ok: await sayOn(parts[l.voice].host, l.text, sayVoiceOf(l.voice), skewed(parts[l.voice].host, outroAt[i])) });
}

const failed = sends.filter((s) => !s.ok);
console.log("");
if (!failed.length) console.log(`  ✓ ${sends.length} cues sent to ${parts.length} computer${parts.length > 1 ? "s" : ""}.`);
else for (const f of failed) console.log(`  ✗ ${shortName(f.host)} — ${f.kind} did not send.`);
console.log(`  ends at epoch ${endsAt.toFixed(3)}\n`);
process.exitCode = failed.length ? 1 : 0;
