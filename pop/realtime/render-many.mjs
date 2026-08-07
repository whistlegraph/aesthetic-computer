#!/usr/bin/env node
// Expand every elected-candidate genome into a 32-measure C-engine track.

import { mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const ENGINE = resolve(HERE, "c/measure-engine");
const GENOMES = resolve(ROOT, "out/composition-beds/genomes.json");
const OUT = resolve(ROOT, "out/realtime-tracks");
const BARS = 32;
const SCALES = {
  minor: [0,2,3,5,7,8,10], dorian: [0,2,3,5,7,9,10], major: [0,2,4,5,7,9,11],
  phrygian: [0,1,3,5,7,8,10], mixolydian: [0,2,4,5,7,9,10],
  lydian: [0,2,4,6,7,9,11], harmonicMinor: [0,2,3,5,7,8,11],
};
const PATTERNS = {
  arch: [0,2,4,6,4,3,2,0,2,4,7,6,4,2,1,0], answer: [4,2,0,3,2,1,0,-1,0,2,5,4,3,1,0,0],
  wide: [0,4,1,6,4,7,3,2,0,5,2,8,6,3,1,0], orbit: [0,2,1,4,2,5,3,1,0,3,2,6,4,2,1,0],
  fall: [7,6,4,3,2,1,0,2,5,4,3,1,2,0,-1,0], beacon: [0,0,4,3,0,5,4,2,0,7,6,4,2,1,0,0],
};
const CITIES = [
  ["los-angeles",34.0522,-118.2437], ["copenhagen",55.6761,12.5683], ["tokyo",35.6762,139.6503],
  ["nairobi",-1.2921,36.8219], ["reykjavik",64.1466,-21.9426], ["buenos-aires",-34.6037,-58.3816],
  ["new-york",40.7128,-74.0060], ["seoul",37.5665,126.9780], ["lagos",6.5244,3.3792],
  ["sao-paulo",-23.5505,-46.6333], ["auckland",-36.8509,174.7645], ["svalbard",78.2232,15.6469],
];
mkdirSync(OUT, { recursive: true });
if (!readFileSync) process.exit(1);
const genomes = JSON.parse(readFileSync(GENOMES, "utf8"));
const globalSeed = Math.floor(Date.now() / 3_600_000);

async function weather([city, lat, lon]) {
  try {
    const url = `https://api.open-meteo.com/v1/forecast?latitude=${lat}&longitude=${lon}&current=weather_code,wind_speed_10m,precipitation,cloud_cover&timezone=auto`;
    const data = await fetch(url, { signal: AbortSignal.timeout(4000) }).then((response) => response.json());
    return { city, cloud: data.current?.cloud_cover ?? 50, wind: data.current?.wind_speed_10m ?? 5,
      rain: data.current?.precipitation ?? 0, code: data.current?.weather_code ?? 0 };
  } catch { return { city, cloud: 50, wind: 5, rain: 0, code: 0 }; }
}
const contexts = await Promise.all(CITIES.map(weather));
const rhythmSlots = (name) => name === "halves" ? [0,8] : name === "quarters" ? [0,4,8,12]
  : name === "syncopated" ? [0,6,10,14] : name === "three" ? [0,6,12]
  : name === "gallop" ? [0,3,6,10,13] : [0,11];
const degreeMidi = (scale, degree, octave = 0) => 64 + scale[((degree % 7) + 7) % 7] + Math.floor(degree / 7) * 12 + octave * 12;
const hex = (value) => (value >>> 0).toString(16);

function plans(genome, context, trackIndex) {
  const scale = SCALES[genome.scale], pattern = PATTERNS[genome.melody], slots = rhythmSlots(genome.rhythm);
  const tempo = genome.bpm * (1 + Math.min(0.04, context.wind / 1000));
  const density = context.cloud > 75 ? 0.58 : context.cloud < 20 ? 1 : 0.8;
  const lines = [];
  for (let bar = 0; bar < BARS; bar++) {
    const chord = genome.chords[Math.floor(bar / 2) % genome.chords.length];
    const section = bar < 4 ? "intro" : bar < 12 ? "statement" : bar < 16 ? "breath" : bar < 24 ? "develop" : bar < 30 ? "return" : "outro";
    const sectionDensity = section === "intro" ? 0.55 : section === "breath" ? 0.35 : section === "return" ? 1 : section === "outro" ? 0.45 : 0.8;
    const octave = section === "intro" ? -1 : section === "return" ? 1 : 0;
    const notes = [];
    for (let i = 0; i < slots.length; i++) {
      const chance = ((bar * 41 + i * 73 + globalSeed + trackIndex * 17) % 100) / 100;
      if (chance > density * sectionDensity) continue;
      let degree = pattern[(bar * slots.length + i) % pattern.length];
      if (section === "develop" && bar % 4 >= 2) degree += 2;
      const duration = genome.rhythm === "halves" ? 6 : genome.rhythm === "sparse" ? 8 : 2;
      notes.push(`${slots[i]},${degreeMidi(scale, degree, octave)},${duration},${86 + i * 5}`);
      if (section === "return" && i % 2 === 0) notes.push(`${slots[i]},${degreeMidi(scale, degree, octave - 1)},${duration},62`);
    }
    const pulse = context.rain > 0 ? "aaaa" : genome.rhythm === "gallop" ? "a4a4" : "8888";
    const contextHash = hex(globalSeed * 2654435761 + bar * 2246822519 + context.code + trackIndex);
    lines.push(`P|${bar}|${tempo.toFixed(2)}|${chord.notes.join(",")}|${notes.join(";")}|${pulse}|${contextHash}`);
  }
  return { text: lines.join("\n") + "\n", tempo };
}

const made = [];
for (let index = 0; index < genomes.length; index++) {
  const genome = genomes[index], context = contexts[index], stem = `${genome.id}-${genome.name.replaceAll(" ", "-")}-${context.city}`;
  const wav = resolve(OUT, `${stem}.wav`), mp3 = resolve(OUT, `${stem}.mp3`);
  const score = plans(genome, context, index);
  const c = spawnSync(ENGINE, ["--render", wav, "--bars", String(BARS)], { input: score.text, encoding: "utf8" });
  if (c.status !== 0) throw new Error(c.stderr || `C render failed: ${stem}`);
  const ff = spawnSync("ffmpeg", ["-hide_banner","-loglevel","error","-y","-i",wav,
    "-codec:a","libmp3lame","-b:a","256k","-metadata",`title=${genome.name} · ${context.city}`,mp3]);
  unlinkSync(wav);
  if (ff.status !== 0) throw new Error(`encode failed: ${stem}`);
  const duration = Number(spawnSync("ffprobe", ["-v","error","-show_entries","format=duration","-of","csv=p=0",mp3], { encoding:"utf8" }).stdout.trim());
  made.push({ file: mp3, duration, genome: genome.id, name: genome.name, bpm: +score.tempo.toFixed(2), ...context });
  console.log(`✓ ${genome.id} ${genome.name} · ${context.city} · ${duration.toFixed(1)}s`);
}

const concat = resolve(OUT, "concat.txt");
writeFileSync(concat, made.map((track) => `file '${track.file.replaceAll("'", "'\\''")}'`).join("\n") + "\n");
const reel = resolve(OUT, "realtime-tracks-audition.mp3");
const joined = spawnSync("ffmpeg", ["-hide_banner","-loglevel","error","-y","-f","concat","-safe","0","-i",concat,"-c","copy",reel]);
unlinkSync(concat);
if (joined.status !== 0) throw new Error("audition join failed");
let at = 0;
const stamp = (seconds) => `${Math.floor(seconds / 60)}:${String(Math.floor(seconds % 60)).padStart(2,"0")}`;
writeFileSync(resolve(OUT, "tracks.json"), JSON.stringify(made, null, 2));
writeFileSync(resolve(OUT, "README.txt"), made.map((track) => {
  const line = `${stamp(at)}  ${track.genome}  ${track.name} · ${track.city} · ${track.bpm} BPM · cloud ${track.cloud}% · wind ${track.wind} km/h`;
  at += track.duration;
  return line;
}).join("\n") + "\n");
console.log(`✓ ${made.length} C-engine tracks + reel → ${OUT}`);
