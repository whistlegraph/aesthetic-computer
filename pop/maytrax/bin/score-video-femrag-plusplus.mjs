#!/usr/bin/env node
// Scrolling performance score, using the audio renderer's literal events.
// Restore the events/struct JSON with pop/bin/remote-workspace.mjs if needed.
// node pop/maytrax/bin/score-video-femrag-plusplus.mjs [--frame 34] [--open]
import { readFileSync, writeFileSync, mkdirSync, renameSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { createCanvas } from "canvas";

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, "../../..");
const dir = resolve(here, "../out");
const slug = "femrag-plusplus";
const arg = (flag, fallback) => {
  const i = process.argv.indexOf(flag);
  return i < 0 ? fallback : process.argv[i + 1];
};
const audio = resolve(arg("--audio", `${dir}/${slug}-release.mp3`));
const output = resolve(arg("--out", `${dir}/${slug}-score.mp4`));
const score = JSON.parse(readFileSync(`${dir}/${slug}.events.json`, "utf8"));
const { sections } = JSON.parse(readFileSync(`${dir}/${slug}.struct.json`, "utf8"));
const env = { ...process.env, PATH: `${root}/toolchain/shims:${process.env.PATH}` };
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "csv=p=0", audio], { env, encoding: "utf8" });
if (probe.status !== 0) throw new Error(probe.stderr || "Audio probe failed");
if (Math.abs(Number(probe.stdout) - score.seconds) > .1) {
  throw new Error("Audio duration does not match the score");
}
const W = 1280, H = 720, FPS = 24;
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
const LEFT = 176, RIGHT = 1240, TOP = 139, HEAD = 470;
const BAR = 240 / score.bpm, SCALE = (RIGHT - LEFT) / (4 * BAR);
const lanes = [
  { name: "BELLS", keys: ["bell", "rbell"], h: 172, color: "#ffce70", range: [36, 112] },
  { name: "BASS", keys: ["sub", "throat"], h: 96, color: "#72e0c0", range: [19, 59] },
  { name: "KICK", keys: ["boom"], h: 44, color: "#ff7c92" },
  { name: "SNARE", keys: ["snare"], h: 44, color: "#ab9aff" },
  { name: "HATS", keys: ["hat"], h: 38, color: "#7fcfff" },
  { name: "DONK", keys: ["donk"], h: 38, color: "#ffa66f" },
  { name: "RISER", keys: ["riser"], h: 30, color: "#a7b7c8" },
  { name: "VOICE", keys: ["voice"], h: 30, color: "#f8efe5" },
];
let y = TOP;
for (const lane of lanes) {
  lane.y = y; y += lane.h;
  lane.events = score.events.filter(e => lane.keys.includes(e.i) && e.t >= 0)
    .map(e => ({ ...e, duration: e.dur ?? e.length ?? ({ boom: .16, snare: .10, hat: e.open ? .14 : .035, donk: .12 }[e.i] ?? .1) }))
    .sort((a, b) => a.t - b.t);
}
const BOTTOM = y;
const clock = t => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;
const text = (s, x, y, size, color = "#f4eee6", align = "left") => {
  ctx.fillStyle = color; ctx.font = `${size}px Menlo, monospace`;
  ctx.textAlign = align; ctx.fillText(s, x, y);
};
const pitchY = (lane, midi) => lane.y + lane.h - 12
  - (midi - lane.range[0]) / (lane.range[1] - lane.range[0]) * (lane.h - 24);

function draw(now) {
  ctx.fillStyle = "#11151c"; ctx.fillRect(0, 0, W, H);
  text("Femrag++", 40, 65, 46);
  text("Aesthetic Dot Computer", 42, 98, 19, "#a1adbc");
  const section = sections.find(s => now >= s.startSec && now < s.endSec) ?? sections.at(-1);
  text(section.name.toUpperCase().replaceAll("-", " "), RIGHT, 60, 28, "#ffce70", "right");
  text(`${score.bpm} BPM   ${clock(now)} / ${clock(score.seconds)}`, RIGHT, 97, 20, "#a1adbc", "right");
  const xAt = t => HEAD + (t - now) * SCALE;
  const start = now - (HEAD - LEFT) / SCALE;
  const end = now + (RIGHT - HEAD) / SCALE;
  for (const [i, lane] of lanes.entries()) {
    ctx.fillStyle = i % 2 ? "#171d27" : "#141a23";
    ctx.fillRect(LEFT, lane.y, RIGHT - LEFT, lane.h - 1);
    text(lane.name, 40, lane.y + lane.h / 2 + 6, 19, lane.color);
    if (lane.range) {
      for (let midi = Math.ceil(lane.range[0] / 12) * 12; midi < lane.range[1]; midi += 12) {
        const yy = pitchY(lane, midi);
        ctx.fillStyle = "#27303b"; ctx.fillRect(LEFT, yy, RIGHT - LEFT, 1);
        text(`C${midi / 12 - 1}`, LEFT + 6, yy - 3, 11, "#697688");
      }
    }
  }
  for (let beat = Math.max(0, Math.ceil(start / (BAR / 4))); beat * BAR / 4 <= end; beat++) {
    const xx = xAt(beat * BAR / 4);
    ctx.fillStyle = beat % 4 ? "#232d39" : "#3a4656";
    ctx.fillRect(xx, TOP, 1, BOTTOM - TOP);
    if (beat % 4 === 0) text(String(beat / 4 + 1), xx + 5, TOP - 10, 14, "#a1adbc");
  }
  ctx.save(); ctx.beginPath(); ctx.rect(LEFT, TOP, RIGHT - LEFT, BOTTOM - TOP); ctx.clip();
  for (const lane of lanes) {
    for (const e of lane.events) {
      if (e.t > end) break;
      if (e.t + e.duration < start) continue;
      const active = now >= e.t && now < e.t + e.duration;
      const color = e.i === "throat" ? "#d99eff" : lane.color;
      const xx = xAt(e.t), ww = Math.max(5, e.duration * SCALE);
      const yy = lane.range ? pitchY(lane, e.midi) : lane.y + lane.h / 2;
      const hh = lane.range ? 5 : Math.max(5, Math.min(lane.h - 12, 9 + (e.gain ?? .1) * 65));
      ctx.globalAlpha = active ? 1 : e.t < now ? .35 : .78;
      ctx.fillStyle = active ? "#ffffff" : color;
      if (e.i === "rbell") {
        ctx.beginPath(); ctx.moveTo(xx, yy); ctx.lineTo(xx + ww, yy - 4);
        ctx.lineTo(xx + ww, yy + 4); ctx.closePath(); ctx.fill();
      } else ctx.fillRect(xx, yy - hh / 2, ww, hh);
      if (active) {
        ctx.globalAlpha = 1; ctx.fillStyle = color;
        ctx.beginPath(); ctx.arc(HEAD, yy, lane.range ? 5 : 7, 0, Math.PI * 2); ctx.fill();
      }
    }
  }
  ctx.restore();
  ctx.fillStyle = "#f4eee6"; ctx.fillRect(HEAD, TOP - 6, 2, BOTTOM - TOP + 6);
  // Full arrangement strip: the current section and cursor remain visible.
  for (const s of sections) {
    const xx = LEFT + s.startSec / score.seconds * (RIGHT - LEFT);
    const ww = (s.endSec - s.startSec) / score.seconds * (RIGHT - LEFT);
    ctx.fillStyle = s === section ? "#ffce70" : s.endSec <= now ? "#566273" : "#303b49";
    ctx.fillRect(xx, 670, ww - 3, 12);
  }
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(LEFT + now / score.seconds * (RIGHT - LEFT), 665, 2, 22);
  text(`BAR ${Math.floor(now / BAR) + 1}`, 40, 684, 17, "#a1adbc");
}

mkdirSync(dirname(output), { recursive: true });
if (process.argv.includes("--frame")) {
  const t = Number(arg("--frame", 34));
  if (!Number.isFinite(t) || t < 0 || t >= score.seconds) throw new Error("Invalid frame time");
  draw(t);
  const path = output.replace(/\.mp4$/, `-frame-${t}.png`);
  writeFileSync(path, canvas.toBuffer("image/png")); console.log(path);
} else {
  const temp = output.replace(/\.mp4$/, ".partial.mp4");
  const encoder = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pixel_format", "bgra", "-video_size", `${W}x${H}`,
    "-framerate", String(FPS), "-i", "pipe:0", "-i", audio,
    "-map", "0:v:0", "-map", "1:a:0", "-c:v", "libx264", "-preset", "fast",
    "-crf", "19", "-threads", "2", "-filter_threads", "1", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "256k", "-t", String(score.seconds),
    "-movflags", "+faststart", "-metadata", "title=Femrag++ — score",
    "-metadata", "artist=Aesthetic Dot Computer", temp], { env, stdio: ["pipe", "ignore", "inherit"] });
  const finished = once(encoder, "close");
  // Catch process/pipe errors during the frame loop, including an early exit.
  let failure;
  encoder.on("error", e => { failure = e; });
  encoder.stdin.on("error", e => { failure = e; });
  finished.catch(e => { failure = e; });
  const frames = Math.ceil(score.seconds * FPS);
  for (let f = 0; f < frames; f++) {
    if (failure || encoder.exitCode !== null) throw failure ?? new Error("Encoder exited early");
    draw(f / FPS);
    if (!encoder.stdin.write(canvas.toBuffer("raw"))) await once(encoder.stdin, "drain");
    if (f % (FPS * 10) === 0) console.log(`${Math.round(f / frames * 100)}%`);
  }
  encoder.stdin.end();
  const [code] = await finished;
  if (code !== 0) throw new Error(`ffmpeg exited ${code}`);
  renameSync(temp, output); console.log(output);
  if (process.argv.includes("--open")) spawnSync("open", [output]);
}
