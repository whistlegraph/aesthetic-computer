#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const spine = resolve(OUT, "unboxing-spine-2m.mp4");
const audio = resolve(OUT, "narration.mp3");
const output = resolve(OUT, "scores-for-social-software-review-01.mp4");
const project = JSON.parse(readFileSync(resolve(ROOT, "index.json"), "utf8"));
const alignment = JSON.parse(readFileSync(resolve(OUT, "narration-alignment.json"), "utf8")).alignment;
if (!existsSync(spine) || !existsSync(audio)) throw new Error("render spine and narration first");

const probe = (path) => Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", path], { encoding: "utf8" }).stdout.trim());
const spineDuration = probe(spine);
const audioDuration = probe(audio);
const ratio = audioDuration / spineDuration;
const narration = readFileSync(resolve(ROOT, "narration.txt"), "utf8").trim();
const starts = ["My contribution", "Æther Cavendish", "Chelly Jin", "Jordan Silver", "Em Lugo", "Darlyn Phan", "Thomas Noya", "Banyi Huang", "Alexander Espinosa", "Mavyn Vu", "Casey Reas"]
  .map((phrase) => alignment.character_start_times_seconds[narration.indexOf(phrase)]);
const slug = (item) => String(item.order).padStart(2, "0") + "-" + item.title
  .normalize("NFKD").replace(/[^a-zA-Z0-9]+/g, "-").replace(/^-|-$/g, "").toLowerCase();

const args = ["-y", "-hide_banner", "-loglevel", "warning", "-i", spine];
for (const item of project.items) args.push("-loop", "1", "-framerate", "30", "-i", resolve(OUT, "overlays", `${slug(item)}.png`));
args.push("-i", audio);

const filters = [`[0:v]setpts=${ratio.toFixed(8)}*PTS[base]`];
let prev = "base";
for (let i = 0; i < project.items.length; i++) {
  const next = `v${i + 1}`;
  // Keep the still present through its spoken visual description, with a small
  // breath between cards. This is a review cut; alpha fades land after selects.
  filters.push(`[${prev}][${i + 1}:v]overlay=0:0:enable='between(t,${starts[i].toFixed(3)},${(starts[i + 1] - 0.35).toFixed(3)})'[${next}]`);
  prev = next;
}
args.push("-filter_complex", filters.join(";"), "-map", `[${prev}]`, "-map", `${project.items.length + 1}:a`,
  "-t", String(audioDuration), "-r", "30", "-c:v", "h264_videotoolbox", "-b:v", "10M", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-af", "loudnorm=I=-14:TP=-1.5:LRA=11", "-movflags", "+faststart", output);
console.log(`render ${audioDuration.toFixed(2)}s · spine × ${ratio.toFixed(4)} · ${project.items.length} overlays`);
const result = spawnSync("ffmpeg", args, { stdio: "inherit" });
if (result.status !== 0) process.exit(result.status ?? 1);
console.log(output);
