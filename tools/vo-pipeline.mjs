#!/usr/bin/env node
// vo-pipeline.mjs — voiceover + baked-in-subtitle pipeline
//
// Reads a script file (YAML-ish or a small JSON) of timestamped narration
// segments, generates TTS via macOS `say`, assembles an SRT, mixes the VO
// onto the source video's audio with automatic ducking, and burns the
// subtitles into the image using libass via ffmpeg.
//
// Usage:
//   node tools/vo-pipeline.mjs <script.md> --video <input.mp4> --out <output.mp4>
//
// Optional flags:
//   --voice <name>      macOS `say` voice (default: Daniel — en_GB)
//   --rate <wpm>        Speech rate (default: 170)
//   --duck <db>         How much to attenuate source audio under VO (default: -18)
//   --style <srt-style> libass style override (ASS section)
//   --caption-only      Skip TTS — only burn subtitles
//   --narrate-only      Skip subtitle burn — only mix VO audio
//   --keep-scratch      Leave intermediate wavs/srts in /tmp for debugging
//
// Script format (markdown with timestamps):
//
//   # title: AC Native Demo
//   # voice: Daniel
//   # rate: 170
//
//   [0:00] What if your computer were a musical instrument?
//   [0:03] Not had a music app — but was an instrument.
//   [0:06] This is Aesthetic Computer, booting from USB.
//   [0:12] (notepat)
//   [0:12] notepat. Every key is a pitch. Every chord is a polyphonic voice
//          through ALSA at 192 kilohertz.
//   [0:20] --silence--
//
// Lines beginning with ( ) are stage directions and are included as
// subtitle text in parentheses but not spoken. Lines with --silence-- or
// empty body suppress both. Timestamp format is [M:SS] or [M:SS.mmm].

import { execFileSync, execSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync, existsSync, rmSync } from "node:fs";
import { join, basename, dirname, resolve } from "node:path";
import { parseArgs } from "node:util";
import { tmpdir } from "node:os";

// ─── ARG PARSING ─────────────────────────────────────────────────────────
const { values: args, positionals } = parseArgs({
  options: {
    video: { type: "string" },
    out: { type: "string" },
    voice: { type: "string", default: "Daniel" },
    rate: { type: "string", default: "170" },
    duck: { type: "string", default: "-18" },
    "caption-only": { type: "boolean", default: false },
    "narrate-only": { type: "boolean", default: false },
    "keep-scratch": { type: "boolean", default: false },
    help: { type: "boolean", default: false, short: "h" },
  },
  allowPositionals: true,
});

if (args.help || positionals.length === 0) {
  console.log(readFileSync(new URL(import.meta.url)).toString().split("\n")
    .filter(l => l.startsWith("//")).slice(0, 40).join("\n").replace(/^\/\/\s?/gm, ""));
  process.exit(0);
}

const SCRIPT_PATH = resolve(positionals[0]);
const VIDEO_PATH = args.video ? resolve(args.video) : null;
const OUT_PATH = args.out ? resolve(args.out)
  : VIDEO_PATH?.replace(/(\.[^.]+)$/, "-narrated$1")
  ?? "out-narrated.mp4";

if (!VIDEO_PATH) {
  console.error("× --video is required");
  process.exit(2);
}
if (!existsSync(SCRIPT_PATH)) {
  console.error(`× script not found: ${SCRIPT_PATH}`);
  process.exit(2);
}
if (!existsSync(VIDEO_PATH)) {
  console.error(`× video not found: ${VIDEO_PATH}`);
  process.exit(2);
}

const SCRATCH = join(tmpdir(), `vo-pipeline-${Date.now()}`);
mkdirSync(SCRATCH, { recursive: true });

// ─── SCRIPT PARSING ──────────────────────────────────────────────────────
function parseTime(t) {
  // "M:SS" or "M:SS.mmm" → seconds (float)
  const [m, rest] = t.split(":");
  return Number(m) * 60 + Number(rest);
}
function fmtSrtTime(sec) {
  // "HH:MM:SS,mmm"
  const ms = Math.floor((sec % 1) * 1000);
  const s = Math.floor(sec);
  const hh = String(Math.floor(s / 3600)).padStart(2, "0");
  const mm = String(Math.floor((s % 3600) / 60)).padStart(2, "0");
  const ss = String(s % 60).padStart(2, "0");
  return `${hh}:${mm}:${ss},${String(ms).padStart(3, "0")}`;
}

const scriptRaw = readFileSync(SCRIPT_PATH, "utf8");
const meta = { title: basename(SCRIPT_PATH), voice: args.voice, rate: Number(args.rate) };
const segments = [];

for (const rawLine of scriptRaw.split("\n")) {
  const line = rawLine.replace(/\r$/, "");
  if (!line.trim()) continue;

  // Front-matter comments
  const fmMatch = line.match(/^#\s*(title|voice|rate):\s*(.+)$/i);
  if (fmMatch) {
    const key = fmMatch[1].toLowerCase();
    meta[key] = key === "rate" ? Number(fmMatch[2]) : fmMatch[2].trim();
    continue;
  }
  if (line.startsWith("#")) continue; // comment

  const tsMatch = line.match(/^\s*\[(\d+:\d+(?:\.\d+)?)\]\s*(.*)$/);
  if (!tsMatch) {
    // Continuation line → append to last segment
    if (segments.length > 0 && line.trim().length > 0 && !line.startsWith("//")) {
      segments[segments.length - 1].text += " " + line.trim();
    }
    continue;
  }
  const t = parseTime(tsMatch[1]);
  const body = tsMatch[2].trim();
  const silent = body === "" || body === "--silence--";
  const stageOnly = body.startsWith("(") && body.endsWith(")");
  segments.push({ t, text: body, silent, stageOnly });
}

// Close segments by giving each a "next timestamp" hint for SRT end times.
for (let i = 0; i < segments.length; i++) {
  segments[i].nextT = segments[i + 1]?.t ?? segments[i].t + 3;
}

console.log(`→ parsed ${segments.length} segments, voice=${meta.voice}, rate=${meta.rate} wpm`);

// ─── STEP 1: GENERATE TTS (macOS `say`) ──────────────────────────────────
const voPath = join(SCRATCH, "vo.wav");
const chunkWavs = [];

if (!args["caption-only"]) {
  for (let i = 0; i < segments.length; i++) {
    const seg = segments[i];
    if (seg.silent || seg.stageOnly) continue;

    const aiffPath = join(SCRATCH, `seg-${String(i).padStart(3, "0")}.aiff`);
    const wavPath = join(SCRATCH, `seg-${String(i).padStart(3, "0")}.wav`);
    try {
      execFileSync("say",
        ["-v", meta.voice, "-r", String(meta.rate), "-o", aiffPath, seg.text],
        { stdio: "inherit" }
      );
      execFileSync("ffmpeg", ["-y", "-i", aiffPath, "-ar", "48000", "-ac", "2", wavPath],
        { stdio: ["ignore", "ignore", "ignore"] });
      const duration = Number(execSync(
        `ffprobe -v error -show_entries format=duration -of default=nokey=1:noprint_wrappers=1 "${wavPath}"`
      ).toString().trim());
      seg.ttsDuration = duration;
      seg.ttsPath = wavPath;
      chunkWavs.push({ at: seg.t, path: wavPath, duration });
    } catch (e) {
      console.error(`× TTS failed for segment ${i}: ${e.message}`);
      process.exit(3);
    }
  }

  // Probe the video duration
  const vidDuration = Number(execSync(
    `ffprobe -v error -show_entries format=duration -of default=nokey=1:noprint_wrappers=1 "${VIDEO_PATH}"`
  ).toString().trim());

  // Build a single VO track the length of the video: start with silence,
  // pad each chunk to its timestamp, overlay.
  // Easier: render a VO track with adelay on each, amix them.
  const inputs = chunkWavs.flatMap(c => ["-i", c.path]);
  const filters = chunkWavs.map((c, i) =>
    `[${i}:a]adelay=${Math.round(c.at * 1000)}|${Math.round(c.at * 1000)}[a${i}]`
  ).join(";");
  const amix = chunkWavs.length > 0
    ? `${chunkWavs.map((_, i) => `[a${i}]`).join("")}amix=inputs=${chunkWavs.length}:normalize=0[vo]`
    : `anullsrc=r=48000:cl=stereo[vo]`;
  const duration = Math.ceil(vidDuration + 1);

  if (chunkWavs.length > 0) {
    execFileSync("ffmpeg", [
      "-y",
      ...inputs,
      "-filter_complex", `${filters};${amix}`,
      "-map", "[vo]",
      "-t", String(duration),
      "-ar", "48000",
      "-ac", "2",
      voPath
    ], { stdio: ["ignore", "ignore", "inherit"] });
    console.log(`→ VO track built: ${voPath}`);
  }
}

// ─── STEP 2: GENERATE SRT ────────────────────────────────────────────────
const srtPath = join(SCRATCH, "captions.srt");
{
  const lines = [];
  let n = 1;
  for (const seg of segments) {
    if (seg.silent) continue;
    const text = seg.stageOnly ? seg.text : seg.text; // keep parenthetical stage dirs visible
    // End timestamp: either next segment start, or this + tts duration, or +3s
    const end = seg.nextT ?? (seg.t + (seg.ttsDuration ?? 3));
    lines.push(
      `${n++}`,
      `${fmtSrtTime(seg.t)} --> ${fmtSrtTime(Math.max(seg.t + 0.5, end - 0.1))}`,
      text,
      ""
    );
  }
  writeFileSync(srtPath, lines.join("\n"));
  console.log(`→ SRT written: ${srtPath} (${n - 1} cues)`);
}

// ─── STEP 3: MIX + BURN ──────────────────────────────────────────────────
// - If we have a VO, amix it with the source audio (ducked).
// - If subtitle burn is requested, pipe through the subtitles filter.

const args3 = ["-y", "-i", VIDEO_PATH];
if (!args["caption-only"] && existsSync(voPath)) args3.push("-i", voPath);

// Video filter: burn subtitles if not narrate-only
const vfChain = [];
if (!args["narrate-only"]) {
  // Inline ASS styling (sepia-ish to match AC palette).
  // FontSize is in ASS "play height" units — ~18 reads cleanly at 1280×800.
  const style = args.style ?? "FontName=Helvetica Neue,FontSize=16," +
    "PrimaryColour=&H00e4d8bc,OutlineColour=&H001c1812,BackColour=&Hc01c1812," +
    "BorderStyle=3,Outline=1,Shadow=0,Alignment=2,MarginV=28";
  vfChain.push(`subtitles='${srtPath.replace(/'/g, "\\'")}':force_style='${style}'`);
}
if (vfChain.length) args3.push("-vf", vfChain.join(","));

// Audio mix
if (!args["caption-only"] && existsSync(voPath)) {
  const duckDb = Number(args.duck);
  const af = `[0:a]volume=${Math.pow(10, duckDb/20).toFixed(3)}[bg];` +
    `[bg][1:a]amix=inputs=2:duration=first:normalize=0[aout]`;
  args3.push(
    "-filter_complex", af,
    "-map", "0:v", "-map", "[aout]"
  );
} else {
  args3.push("-map", "0:v", "-map", "0:a?");
}

args3.push(
  "-c:v", "libx264", "-preset", "medium", "-crf", "20", "-pix_fmt", "yuv420p",
  "-movflags", "+faststart",
  "-c:a", "aac", "-b:a", "160k",
  OUT_PATH
);

console.log(`→ encoding ${OUT_PATH}`);
execFileSync("ffmpeg", args3, { stdio: "inherit" });

// ─── CLEANUP ─────────────────────────────────────────────────────────────
if (!args["keep-scratch"]) {
  rmSync(SCRATCH, { recursive: true, force: true });
} else {
  console.log(`→ scratch kept: ${SCRATCH}`);
}

console.log(`✓ done: ${OUT_PATH}`);
