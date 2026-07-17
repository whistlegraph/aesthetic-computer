#!/usr/bin/env node
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const ROOT = dirname(fileURLToPath(import.meta.url));
const VIDEO = resolve(ROOT, "out/scores-for-social-software-captioned-08.mp4");
const OUT = "/Users/jas/Desktop/Scores-for-Social-Software-Loops";
const narration = readFileSync(resolve(ROOT, "narration.txt"), "utf8").trim();
const alignment = JSON.parse(readFileSync(resolve(ROOT, "out/narration-alignment.json"), "utf8")).alignment;
const at = (phrase) => alignment.character_start_times_seconds[narration.indexOf(phrase)];

const loops = [
  { slug: "00-intro", title: "Intro", start: 0.5, duration: 4 },
  { slug: "01-jeffrey-alan-scudder-notepat", title: "Jeffrey Alan Scudder — Notepat", start: at("My contribution") + 0.3, duration: 4 },
  { slug: "02-aether-cavendish-vigil-score", title: "Æther Cavendish — Vigil Score", start: at("Æther Cavendish") + 0.3, duration: 4 },
  { slug: "03-chelly-jin-software-as-a-choreography", title: "Chelly Jin — Software as a Choreography", start: at("Chelly Jin") + 0.3, duration: 4 },
  { slug: "04-jordan-silver-sonic-architecture", title: "Jordan Silver — Sonic Architecture", start: at("Jordan Silver") + 0.3, duration: 4 },
  { slug: "05-em-lugo-cues-for-losing-direction", title: "Em Lugo — Cues for Losing Direction", start: at("Em Lugo") + 0.3, duration: 4 },
  { slug: "06-darlyn-phan-line-piece-1", title: "Darlyn Phan — Line Piece 1", start: at("Darlyn Phan") + 0.3, duration: 4 },
  { slug: "07-thomas-noya-biophonia", title: "Thomas Noya — Biophonía", start: at("Thomas Noya") + 0.3, duration: 4 },
  { slug: "08-banyi-huang-cosmographic-score", title: "Banyi Huang — A Cosmographic Score", start: at("Banyi Huang") + 0.3, duration: 4 },
  { slug: "09-alexander-espinosa-music-for-world-computers", title: "Alexander Espinosa — Music for World Computers", start: at("Alexander Espinosa") + 0.3, duration: 4 },
  { slug: "10-mavyn-vu-radio-is-an-altar", title: "Mavyn Vu — The Radio Is an Altar: Portal", start: at("Mavyn Vu") + 0.3, duration: 4 },
  { slug: "11-lauren-lee-mccarthy-casey-reas-auto-tune", title: "Lauren Lee McCarthy and Casey Reas — Auto Tune", start: at("Casey Reas") + 0.3, duration: 4 },
];

mkdirSync(OUT, { recursive: true });
const run = (args) => {
  const r = spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", ...args], { stdio: "inherit" });
  if (r.status !== 0) process.exit(r.status ?? 1);
};
const filter = "fps=10,scale=432:-2:flags=lanczos";
for (const item of loops) {
  const base = resolve(OUT, item.slug);
  process.stdout.write(`${item.slug}\n`);
  run(["-ss", String(item.start), "-t", String(item.duration), "-i", VIDEO, "-vf", `${filter},split[a][b];[a]palettegen=max_colors=128:stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=3:diff_mode=rectangle`, "-loop", "0", `${base}.gif`]);
  const webp = spawnSync("magick", [`${base}.gif`, "-coalesce", "-quality", "72", "-define", "webp:method=5", "-loop", "0", `${base}.webp`], { stdio: "inherit" });
  if (webp.status !== 0) process.exit(webp.status ?? 1);
}

// A brisk overview samples every contribution in publication order.
const sampleStarts = loops.slice(1).map((item) => item.start + 0.6);
const inputs = sampleStarts.flatMap((start) => ["-ss", String(start), "-t", "0.7", "-i", VIDEO]);
const labels = sampleStarts.map((_, i) => `[${i}:v]${filter},setpts=PTS-STARTPTS[v${i}]`).join(";");
const concat = sampleStarts.map((_, i) => `[v${i}]`).join("");
const overviewMp4 = resolve(OUT, "12-overview-source.mp4");
run([...inputs, "-filter_complex", `${labels};${concat}concat=n=${sampleStarts.length}:v=1:a=0[out]`, "-map", "[out]", "-an", "-c:v", "libx264", "-crf", "20", "-pix_fmt", "yuv420p", overviewMp4]);
run(["-i", overviewMp4, "-vf", "split[a][b];[a]palettegen=max_colors=128:stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=3:diff_mode=rectangle", "-loop", "0", resolve(OUT, "12-overview.gif")]);
{
  const webp = spawnSync("magick", [resolve(OUT, "12-overview.gif"), "-coalesce", "-quality", "72", "-define", "webp:method=5", "-loop", "0", resolve(OUT, "12-overview.webp")], { stdio: "inherit" });
  if (webp.status !== 0) process.exit(webp.status ?? 1);
}

writeFileSync(resolve(OUT, "README.txt"), [
  "Scores for Social Software — looping media exports",
  "",
  "Each numbered contribution is supplied as GIF and animated WebP.",
  "Dimensions: 432 × 768. Frame rate: 10 fps. Artist loops: 4 seconds.",
  "The final overview samples all contributions in publication order.",
  "",
  ...loops.map((item) => `${item.slug}: ${item.title}`),
  "12-overview: all contributions",
  "",
].join("\n"));
console.log(OUT);
