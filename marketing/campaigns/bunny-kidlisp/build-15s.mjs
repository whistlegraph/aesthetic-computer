// build-15s.mjs — 15s felt-bunny video: hold shot-A (5s) → morph A→B (5s) →
// hold shot-B (5s). seg2 bridges (starts at A, ends at B) so the three clips
// read continuous. Silent-ish; @jeffrey adds audio at post. → ~/Desktop.
import { generateShot } from "../../../pop/lib/fal-seedance.mjs";
import { spawnSync } from "node:child_process";
import { writeFileSync, existsSync } from "node:fs";
import { homedir } from "node:os";

const HERE = new URL(".", import.meta.url).pathname;
const A = `${HERE}gens/shot-a.png`;
const B = `${HERE}gens/shot-b.png`;
const OUT = `${homedir()}/Desktop/bunny-kidlisp-felt-15s.mp4`;
const segs = [`${HERE}gens/seg1.mp4`, `${HERE}gens/seg2.mp4`, `${HERE}gens/seg3.mp4`];

const common = { duration: "5", ratio: "9:16", resolution: "720p", tier: "fast", log: console.log };

const jobs = [
  { ...common, image: A, outPath: segs[0], label: "hold-A",
    prompt: "stop-motion needle-felt diorama gently alive: the wool bunnies' long felt ears twitch and tip, one leans a little closer, soft breathing wobble; the colorful KidLisp pattern on the little laptop screen softly glows and shimmers. very gentle, locked camera, cozy." },
  { ...common, image: A, endImage: B, outPath: segs[1], label: "morph-A-B",
    prompt: "smooth handmade stop-motion transition: the felt bunnies shift and resettle into a new arrangement while the laptop screen morphs from one colorful KidLisp pattern into another; continuous, gentle, tender, locked camera." },
  { ...common, image: B, outPath: segs[2], label: "hold-B",
    prompt: "stop-motion needle-felt diorama gently alive: the wool bunnies settle in and watch, ears twitching, tiny breathing wobble; the new colorful KidLisp pattern on the laptop screen softly glows and shimmers. gentle, locked camera, cozy." },
];

for (const j of jobs) {
  console.log(`▸ ${j.label}…`);
  const r = await generateShot(j);
  if (!r.ok) { console.error(`✗ ${j.label}: ${r.error}`); process.exit(1); }
  console.log(`  ✓ ${j.label} seed=${r.seed} ${(r.bytes / 1e6).toFixed(1)}MB`);
}

const list = `${HERE}gens/concat-15s.txt`;
writeFileSync(list, segs.map((s) => `file '${s}'`).join("\n") + "\n");
const enc = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0", "-i", list,
  "-c:v", "libx264", "-preset", "faster", "-crf", "18", "-pix_fmt", "yuv420p", "-an",
  "-movflags", "+faststart", OUT,
], { stdio: "inherit" });
if (enc.status !== 0 || !existsSync(OUT)) { console.error("✗ concat failed"); process.exit(1); }
console.log(`✓ ${OUT}`);
