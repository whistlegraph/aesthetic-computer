#!/usr/bin/env node
// master-audio.mjs — a whistlegraph clip's phone audio through the /pop master.
//
//   node master-audio.mjs <clip.mp4> <out.wav>
//
// The archive clips are TikTok's 32 kbps AAC of three people singing round a
// table: pens scratch, the room hums, the voices sit back. Cleanup is
// ffmpeg's (high-pass, a light spectral denoise so the pens recede without
// the voices going watery); the character is acdsp's 1176 and the EQ
// graph's intents (mud and boxiness out, presence in, sibilance held); the
// boundary is a two-pass loudnorm to -12 LUFS, -1 dBTP — level with the
// reels around it; the sources themselves run hot and clip.

import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { chain, compressor, eq, processWav } from "../../pop/lib/master.mjs";

const [src, out] = process.argv.slice(2);
if (!src || !out) {
  console.error("usage: node master-audio.mjs <clip.mp4> <out.wav>");
  process.exit(1);
}

const ff = (...args) => {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", ...args], { encoding: "utf8" });
  if (r.status !== 0) throw new Error(r.stderr.slice(-800));
  return r.stderr;
};

const tmp = mkdtempSync(join(tmpdir(), "wg-master-"));
const clean = join(tmp, "clean.wav");
const voiced = join(tmp, "voiced.wav");

ff("-v", "error", "-i", src, "-vn", "-ar", "48000", "-ac", "2",
   "-af", "highpass=f=85,afftdn=nr=10:nf=-42:tn=1", clean);

const vocalBus = chain(
  eq("rumble"),
  eq("mud", -2.5),
  eq("boxy", -1.5),
  compressor("1176", { ratio: 4, in: -4, out: +3, attack: 5, release: 4, iron: 0.4 }),
  eq("presence", +3),
  eq("sibilance", -1.5),
  eq("air", +1),
);
const r = processWav(clean, voiced, vocalBus, { bits: 24, quiet: true });
if (!r.ok) throw new Error(`acdsp: ${r.stderr}`);

const target = "I=-12:TP=-1:LRA=9";
const probe = ff("-i", voiced, "-af", `loudnorm=${target}:print_format=json`, "-f", "null", "-");
const json = probe.slice(probe.lastIndexOf("{"));
const m = JSON.parse(json.slice(0, json.indexOf("}") + 1));
ff("-v", "error", "-i", voiced, "-af",
   `deesser=i=0.3,loudnorm=${target}:measured_I=${m.input_i}:measured_TP=${m.input_tp}` +
   `:measured_LRA=${m.input_lra}:measured_thresh=${m.input_thresh}:offset=${m.target_offset}:linear=true`,
   "-ar", "48000", "-c:a", "pcm_s24le", out);

rmSync(tmp, { recursive: true, force: true });
console.log(`✓ ${out}  (in ${m.input_i} LUFS → -12)`);
