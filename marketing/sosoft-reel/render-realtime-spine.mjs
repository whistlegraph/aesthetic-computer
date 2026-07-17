#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const raw = (name) => {
  const downloads = `/Users/jas/Downloads/${name}`;
  const working = `/Users/jas/Documents/Shelf/scores-for-social-software/raw-working/${name}`;
  const archived = `/Users/jas/.Trash/Scores-for-Social-Software-Raw-2026-07-16/${name}`;
  if (existsSync(downloads)) return downloads;
  if (existsSync(working)) return working;
  return archived;
};
const MASTER = raw("IMG_4889.MOV");
const BIO = raw("Biophonia_SoSoft_Fuser_V2.mp4");
const CHELLY = raw("ChellyJin_SoSoftPresentation.mp4");
const output = resolve(OUT, "unboxing-spine-realtime.mp4");

const narration = readFileSync(resolve(ROOT, "narration.txt"), "utf8").trim();
const alignment = JSON.parse(readFileSync(resolve(OUT, "narration-alignment.json"), "utf8")).alignment;
const phrases = ["My contribution", "Æther Cavendish", "Chelly Jin", "Jordan Silver", "Em Lugo", "Darlyn Phan", "Thomas Noya", "Banyi Huang", "Alexander Espinosa", "Mavyn Vu", "Casey Reas"];
const starts = phrases.map((p) => alignment.character_start_times_seconds[narration.indexOf(p)]);
const total = alignment.character_end_times_seconds.at(-1);
const sectionDur = (i) => (i + 1 < starts.length ? starts[i + 1] : total) - starts[i];
const heldSection = (at, dur, holdAt, label) => {
  const hold = Math.min(3, dur * 0.28);
  return [
    { src: 0, at, dur: dur - hold, label },
    { src: 0, at: holdAt, dur: hold, label: `${label}-closeup`, freeze: true },
  ];
};

// Destination durations come from the exact Jeffrey alignment. Every clip plays
// at 1×. We cut among real moments instead of uniformly accelerating 11:28.
const introB = Math.max(0, starts[0] - 8);
const clips = [
  { src: 0, at: 0.000, dur: 8.000, label: "cover" },
  { src: 0, at: 88.000, dur: introB, label: "title-card" },
  ...heldSection(132.000, sectionDur(0), 140.000, "notepat"),
  // Start once Æther's folded black packet and silver seal are legible.
  { src: 0, at: 212.000, dur: sectionDur(1), label: "vigil-score" },
  // Chelly and Thomas have canonical moving-image masters: show the work,
  // rather than another photograph of a page describing it.
  { src: 2, at: 120.000, dur: sectionDur(2), label: "software-as-choreography" },
  ...heldSection(295.000, sectionDur(3), 300.000, "sonic-architecture"),
  { src: 0, at: 635.000, dur: sectionDur(4), label: "cues-for-losing-direction" },
  ...heldSection(350.000, sectionDur(5), 356.000, "line-piece-1"),
  { src: 1, at: 110.000, dur: sectionDur(6), label: "biophonia" },
  ...heldSection(432.000, sectionDur(7), 438.000, "cosmographic-score"),
  ...heldSection(468.000, sectionDur(8), 475.000, "music-for-world-computers"),
  ...heldSection(500.000, sectionDur(9), 506.000, "radio-is-an-altar"),
  { src: 0, at: 88.000, dur: sectionDur(10), label: "closing-title-card" },
];

const filters = [];
const mainCount = clips.filter((c) => c.src === 0).length;
filters.push(`[0:v]split=${mainCount}${clips.filter((c) => c.src === 0).map((_, i) => `[m${i}]`).join("")}`);
let mi = 0;
const labels = [];
for (let i = 0; i < clips.length; i++) {
  const c = clips[i];
  const input = c.src === 0 ? `m${mi++}` : `${c.src}:v`;
  const out = `c${i}`;
  const end = c.at + c.dur;
  // Roll the source highlights into a soft shoulder so the white cotton gloves
  // retain their weave and folds. Deepen only cyan/blue regions for the UCLA
  // folder; neutral paper and skin should not inherit that density change.
  const mainGrade = "curves=all='0/0 0.20/0.025 0.45/0.31 0.70/0.64 0.85/0.77 1/0.88',huesaturation=colors=c+b:saturation=0.14:intensity=-0.18:strength=60:lightness=1,selectivecolor=correction_method=relative:cyans='0.10 0.08 -0.08 0.16':blues='0.16 0.12 -0.10 0.18':whites='0 0 0 0.06',eq=saturation=1.05,colorbalance=rs=0.004:bs=-0.006:pl=1,unsharp=5:5:0.70:3:3:0.18";
  const workGrade = "curves=all='0/0 0.14/0.05 0.46/0.40 0.82/0.84 1/0.96',eq=saturation=1.04,colorbalance=rs=0.004:bs=-0.006:pl=1,unsharp=5:5:0.55:3:3:0.12";
  const framing = c.src === 0
    ? `scale=1080:1920:flags=lanczos,${mainGrade}`
    : c.src === 1
      ? `scale=1080:1920:force_original_aspect_ratio=increase:flags=lanczos,crop=1080:1920,${workGrade}`
      : `scale=1080:-2:flags=lanczos,pad=1080:1920:0:(oh-ih)/2:color=black,${workGrade}`;
  const temporal = c.freeze
    ? `trim=start=${c.at}:end=${c.at + 0.08},setpts=PTS-STARTPTS,${framing},fps=30,tpad=stop_mode=clone:stop_duration=${c.dur},trim=duration=${c.dur}`
    : `trim=start=${c.at}:end=${end},setpts=PTS-STARTPTS,${framing},fps=30`;
  filters.push(`[${input}]${temporal},format=yuv420p[${out}]`);
  labels.push(`[${out}]`);
}
filters.push(`${labels.join("")}concat=n=${clips.length}:v=1:a=0[outv]`);

console.log(`render real-time spine · ${clips.length} clips · ${clips.reduce((n, c) => n + c.dur, 0).toFixed(3)}s`);
const result = spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "warning", "-hwaccel", "videotoolbox",
  "-i", MASTER, "-i", BIO, "-i", CHELLY, "-filter_complex", filters.join(";"), "-map", "[outv]", "-an", "-r", "30",
  "-c:v", "libx264", "-preset", "fast", "-crf", "16", "-maxrate", "20M", "-bufsize", "40M",
  "-pix_fmt", "yuv420p", "-movflags", "+faststart", output], { stdio: "inherit" });
if (result.status !== 0) process.exit(result.status ?? 1);
console.log(output);
