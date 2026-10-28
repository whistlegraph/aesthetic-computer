#!/usr/bin/env node
// master-audio.mjs — a whistlegraph clip's phone audio through the /pop master.
//
//   node master-audio.mjs <clip.mp4> <out.wav> [--stems <dir>] [--lift 2.5]
//
// The archive clips are TikTok's 32 kbps AAC of three people singing round a
// table over a beat; the voices sit back. stems.py splits vocals from the
// beat (cached in --stems, default beside <out>), and only the vocals are
// worked: a high-pass and a light spectral denoise so the pens recede, then
// acdsp's 1176 and the EQ graph's intents (mud and boxiness out, presence
// in, sibilance held). The beat goes back underneath untouched — denoising
// the full mix had flattened it — with the vocals --lift dB above it. The
// boundary is a two-pass loudnorm to -12 LUFS, -1 dBTP — level with the
// reels around it; the sources themselves run hot and clip.

import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { fileURLToPath } from "node:url";
import { chain, compressor, eq, processWav } from "../../pop/lib/master.mjs";

const argv = process.argv.slice(2);
const flag = (name, fallback) => (argv.includes(name) ? argv[argv.indexOf(name) + 1] : fallback);
const [src, out] = argv;
if (!src || !out || src.startsWith("--")) {
  console.error("usage: node master-audio.mjs <clip.mp4> <out.wav> [--stems <dir>] [--lift 2.5]");
  process.exit(1);
}
const stems = flag("--stems", join(dirname(out), "stems"));
const lift = Number(flag("--lift", 2.5));

const ff = (...args) => {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", ...args], { encoding: "utf8" });
  if (r.status !== 0) throw new Error(r.stderr.slice(-800));
  return r.stderr;
};

if (!existsSync(join(stems, "vocals.wav"))) {
  const py = join(homedir(), ".local/pipx/venvs/demucs/bin/python");
  const here = dirname(fileURLToPath(import.meta.url));
  const r = spawnSync(py, [join(here, "stems.py"), src, stems], { stdio: "inherit" });
  if (r.status !== 0) throw new Error("stems.py failed");
}

const tmp = mkdtempSync(join(tmpdir(), "wg-master-"));
const clean = join(tmp, "clean.wav");
const voiced = join(tmp, "voiced.wav");
const mixed = join(tmp, "mixed.wav");

ff("-v", "error", "-i", join(stems, "vocals.wav"), "-ar", "48000", "-ac", "2",
   "-af", "highpass=f=85,afftdn=nr=8:nf=-45", clean);

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

ff("-v", "error", "-i", voiced, "-i", join(stems, "beat.wav"), "-filter_complex",
   `[0:a]deesser=i=0.3,volume=${lift}dB[v];[1:a]aresample=48000[b];` +
   "[v][b]amix=inputs=2:normalize=0[m]", "-map", "[m]", "-c:a", "pcm_f32le", mixed);

const target = "I=-12:TP=-1:LRA=9";
const probe = ff("-i", mixed, "-af", `loudnorm=${target}:print_format=json`, "-f", "null", "-");
const json = probe.slice(probe.lastIndexOf("{"));
const m = JSON.parse(json.slice(0, json.indexOf("}") + 1));
ff("-v", "error", "-i", mixed, "-af",
   `loudnorm=${target}:measured_I=${m.input_i}:measured_TP=${m.input_tp}` +
   `:measured_LRA=${m.input_lra}:measured_thresh=${m.input_thresh}:offset=${m.target_offset}:linear=true`,
   "-ar", "48000", "-c:a", "pcm_s24le", out);

rmSync(tmp, { recursive: true, force: true });
console.log(`✓ ${out}  (in ${m.input_i} LUFS → -12)`);
