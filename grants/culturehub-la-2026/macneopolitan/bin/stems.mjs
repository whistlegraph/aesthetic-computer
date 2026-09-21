#!/usr/bin/env node
// stems.mjs — remember the speech, sing it live. For every sung voice in the
// scores whose member's profile names a recorded speech provider (neo →
// jeffrey's own cloned voice), speak the lyric ONCE through the say endpoint
// with exact per-character timestamps, then run the spinging metadata layer
// (whisper identity check, witness timing, phonemes, measured vowel nuclei)
// and cache everything under members/<name>/speech/<hash>/. At showtime the
// conductor hands livesing the cached stem + meta: no network, still live.
//
//   node bin/stems.mjs            # build/refresh the cache for all scores
//   node bin/stems.mjs --force    # re-speak even when cached
//
// Cache key = sha1(provider · voice · lyric text). Re-run after compose.mjs.

import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..", "..");
const SAY = resolve(REPO, "pop/bin/say.mjs");
const WORDMETA = resolve(REPO, "spinging/lib/wordmeta.mjs");
const force = process.argv.includes("--force");

const profiles = {};
for (const m of readdirSync(resolve(LANE, "members"))) {
  const p = resolve(LANE, "members", m, "voice.json");
  if (existsSync(p)) profiles[m] = JSON.parse(readFileSync(p, "utf8"));
}

// The lyric grammar (one token per note, "-" joins syllables) → plain speech.
const plain = (lyrics) => lyrics.split(/\s+/).map((t) => t.split("-").join("")).join(" ");

let built = 0, cached = 0, skipped = 0;
for (const f of readdirSync(resolve(LANE, "scores")).filter((f) => f.endsWith(".mbscore"))) {
  const score = JSON.parse(readFileSync(resolve(LANE, "scores", f), "utf8"));
  for (const v of score.voices || []) {
    if (!v.lyrics) continue;
    const member = String(v.name).split(/\s|·/)[0];
    const speech = profiles[member]?.aesthetivox?.speech;
    if (!speech?.provider) { skipped++; continue; }
    const text = plain(v.lyrics);
    const hash = createHash("sha1").update(`${speech.provider}·${speech.voice || ""}·${text}`).digest("hex").slice(0, 12);
    const dir = resolve(LANE, "members", member, "speech", hash);
    mkdirSync(dir, { recursive: true });
    const wav = resolve(dir, "stem.wav"), meta = resolve(dir, "stem.meta.json");
    if (existsSync(wav) && existsSync(meta) && !force) { cached++; continue; }
    console.log(`\n♪ ${member} · ${f} · ${v.name}\n  "${text}"`);
    writeFileSync(resolve(dir, "text.txt"), text + "\n");
    // 1. speak, with exact timestamps
    const mp3 = resolve(dir, "stem.mp3");
    let r = spawnSync("node", [SAY, resolve(dir, "text.txt"), "--provider", speech.provider, "--voice", speech.voice || "neutral:0", "--out", mp3, "--timestamps", ...(force ? ["--force"] : [])], { encoding: "utf8" });
    if (r.status !== 0) { console.log(`  ✗ say failed\n${r.stderr}`); continue; }
    // 2. wav for the engine
    r = spawnSync("ffmpeg", ["-v", "error", "-y", "-i", mp3, "-ac", "1", "-ar", "22050", wav], { encoding: "utf8" });
    if (r.status !== 0) { console.log(`  ✗ ffmpeg failed\n${r.stderr}`); continue; }
    // 3. the metadata layer, gated on exact word identity
    r = spawnSync("node", [WORDMETA, wav, "--lyrics", v.lyrics, "--witness", `${mp3}.alignment.json`, "--out", meta], { encoding: "utf8" });
    process.stdout.write(r.stdout.split("\n").filter((l) => /placed|witness|nuclei|⚠|✗/.test(l)).map((l) => "  " + l.trim()).join("\n") + "\n");
    if (r.status !== 0) { console.log(`  ✗ wordmeta refused (${r.status}) — stem kept for inspection, not used`); continue; }
    writeFileSync(resolve(dir, "lyrics.txt"), v.lyrics + "\n");
    built++;
  }
}
console.log(`\n${built} built · ${cached} cached · ${skipped} voices without a speech provider (they speak with their own synthesizer)`);
