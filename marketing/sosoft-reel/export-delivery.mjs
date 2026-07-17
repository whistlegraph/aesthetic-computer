#!/usr/bin/env node
import { createHash } from "node:crypto";
import { copyFileSync, mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const DELIVERY = resolve(ROOT, "delivery");
const SOURCE = resolve(OUT, "scores-for-social-software-captioned-08.mp4");
const MASTER = resolve(DELIVERY, "scores-for-social-software-master-vertical.mp4");
const REVIEW = resolve(DELIVERY, "scores-for-social-software-review-vertical.mp4");
const POSTER = resolve(DELIVERY, "scores-for-social-software-poster.jpg");
const SRT = resolve(DELIVERY, "scores-for-social-software-captions.srt");

mkdirSync(DELIVERY, { recursive: true });
copyFileSync(SOURCE, MASTER);

const words = JSON.parse(readFileSync(resolve(OUT, "words.json"), "utf8"));
const phrases = [];
for (let i = 0; i < words.length;) {
  const group = [];
  while (i < words.length && group.length < 7) {
    group.push(words[i++]);
    if (/[.!?]$/.test(group.at(-1).text)) break;
  }
  phrases.push(group);
}
const stamp = (ms) => {
  const n = Math.max(0, Math.round(ms));
  const h = Math.floor(n / 3600000);
  const m = Math.floor((n % 3600000) / 60000);
  const s = Math.floor((n % 60000) / 1000);
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}:${String(s).padStart(2, "0")},${String(n % 1000).padStart(3, "0")}`;
};
writeFileSync(SRT, phrases.map((p, i) => `${i + 1}\n${stamp(p[0].fromMs)} --> ${stamp(p.at(-1).toMs)}\n${p.map((w) => w.text).join(" ")}\n`).join("\n"));

const run = (args) => {
  const r = spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", ...args], { stdio: "inherit" });
  if (r.status !== 0) process.exit(r.status ?? 1);
};
run(["-i", MASTER, "-c:v", "libx264", "-preset", "medium", "-crf", "25", "-maxrate", "5M", "-bufsize", "10M", "-c:a", "aac", "-b:a", "128k", "-movflags", "+faststart", REVIEW]);
run(["-ss", "1.5", "-i", MASTER, "-frames:v", "1", "-q:v", "2", POSTER]);

const files = [MASTER, REVIEW, POSTER, SRT];
const manifest = files.map((path) => {
  const data = readFileSync(path);
  return { file: path.split("/").at(-1), bytes: statSync(path).size, sha256: createHash("sha256").update(data).digest("hex") };
});
writeFileSync(resolve(DELIVERY, "manifest.json"), `${JSON.stringify({ generatedAt: new Date().toISOString(), files: manifest }, null, 2)}\n`);
console.log(DELIVERY);
