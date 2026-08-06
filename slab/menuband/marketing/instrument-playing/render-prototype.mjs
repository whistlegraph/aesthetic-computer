#!/usr/bin/env node
// Render a small, honest Menu Band playing film from the app's real keymap UI.
// Scope is deliberately narrow: one piano voice, QWERTY playing, no drums.

import { execFileSync } from "node:child_process";
import { mkdirSync, rmSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, "../..");
const repo = resolve(root, "../..");
const bin = resolve(root, ".build/debug/MenuBand");
const reelBackground = resolve(root, "screenshots/reel-background.png");
const stampReel = resolve(repo, "marketing/av-reels/bin/stamp-reel.mjs");
const out = resolve(here, "out");
const framesDir = resolve(out, "frames");
const fps = 15;
const sampleRate = 48_000;
const duration = 11;
const commandTaps = [[0.70, 0.16], [0.98, 0.16]];
const escapeTap = [9.35, 0.30];
const voicePath = resolve(here, "jeffrey-double-tap-command.mp3");
const escapeVoicePath = resolve(here, "jeffrey-press-escape.mp3");
const captionPath = resolve(here, "captions.srt");

if (!existsSync(bin)) {
  console.error(`Build Menu Band first: cd ${root} && swift build`);
  process.exit(1);
}

rmSync(out, { recursive: true, force: true });
mkdirSync(framesDir, { recursive: true });

// One ascending C-major scale. The generous pace makes this a legible lesson,
// not a performance montage: watch one key, hear one note, move right.
const notes = [
  [2.35, 0.48, 60], // C
  [3.10, 0.48, 62], // D
  [3.85, 0.48, 64], // E
  [4.60, 0.48, 65], // F
  [5.35, 0.48, 67], // G
  [6.10, 0.48, 69], // A
  [6.85, 0.48, 71], // B
  [7.60, 1.10, 72], // C
];

const totalFrames = Math.round(duration * fps);
const sequence = Array.from({ length: totalFrames }, (_, i) => {
  const t = i / fps;
  const held = notes.filter(([at, len]) => t >= at && t < at + len)
    .map(([, , midi]) => midi);
  const played = notes.filter(([at]) => t >= at).map(([, , midi]) => midi);
  const energy = notes.reduce((sum, [at, len]) => {
    if (t < at || t >= at + len + 0.7) return sum;
    const age = t - at;
    return sum + (age < len ? 0.72 : 0.72 * Math.exp(-(age - len) * 5));
  }, 0);
  const levels = Array.from({ length: 90 }, (_, x) => {
    const phase = t * 10 + x * 0.29;
    return Math.min(1, energy * (0.12 + 0.12 * Math.abs(Math.sin(phase))));
  });
  const command = commandTaps.some(([at, len]) => t >= at && t < at + len);
  const escape = t >= escapeTap[0] && t < escapeTap[0] + escapeTap[1];
  return { notes: held, played, command, escape, levels, cursor: (t / duration) % 1, program: 0 };
});
// Render each distinct state through the shipping menu-bar and QWERTY views,
// then assemble the timed frame sequence from those real product surfaces.
const statesDir = resolve(out, "states");
mkdirSync(statesDir, { recursive: true });
const openingCaptionPath = resolve(statesDir, "caption-opening.png");
const closingCaptionPath = resolve(statesDir, "caption-closing.png");
for (const [caption, path] of [
  ["Double tap Command for Menu Band.", openingCaptionPath],
  ["Press Escape to get back to your Mac.", closingCaptionPath],
]) execFileSync("magick", ["-background", "none", "-fill", "white",
  "-stroke", "#241b31", "-strokewidth", "3", "-font", "/System/Library/Fonts/Helvetica.ttc",
  "-pointsize", "46", `label:${caption}`, "-bordercolor", "none", "-border", "24x14", path]);
const statePath = new Map();
const stateKey = (frame) => frame.command ? "command" : frame.escape ? "escape"
  : `p-${frame.played.join("-") || "none"}-h-${frame.notes.join("-") || "none"}`;
const states = [...new Map(sequence.map((frame) => [stateKey(frame), {
  key: stateKey(frame), notes: frame.notes, played: frame.played,
  keys: frame.command ? [55] : frame.escape ? [53] : [],
  red: frame.command || frame.escape,
}])).values()];
for (const state of states) {
  const { key } = state;
  if (statePath.has(key)) continue;
  const menuPath = resolve(statesDir, `${key}-menubar.png`);
  const qwertyPath = resolve(statesDir, `${key}-qwerty.png`);
  const staffPath = resolve(statesDir, `${key}-staff.png`);
  execFileSync(bin, ["--render-menubar", "--notes", state.notes.join(","),
    "--out", menuPath, "--scale", "4", "--light", "--key-accent",
    "--program", "0", "--no-settings"], { stdio: "inherit" });
  execFileSync(bin, ["--render-qwerty", "--notes", state.notes.join(","),
    "--keys", state.keys.join(","), "--out", qwertyPath, "--scale", "2"],
    { stdio: "inherit" });
  execFileSync(bin, ["--render-scale-staff", "--notes", state.played.join(","),
    "--out", staffPath, "--scale", "2"], { stdio: "inherit" });
  statePath.set(key, { menuPath, qwertyPath, staffPath, red: state.red });
}
for (let i = 0; i < sequence.length; i++) {
  const t = i / fps;
  const state = statePath.get(stateKey(sequence[i]));
  const staffY = 678 + Math.round(8 * Math.sin(t * 2.0));
  const menuY = 925 + Math.round(6 * Math.sin(t * 2.35 + 1.4));
  const qwertyY = 1125 + Math.round(9 * Math.sin(t * 1.75 + 2.2));
  const args = [reelBackground, "-resize", "1080x1920^", "-gravity", "center",
    "-extent", "1080x1920"];
  if (state.red) args.push("-fill", "#ff1f2d", "-colorize", "18");
  args.push(
    "(", state.staffPath, "-resize", "900x", ")", "-gravity", "north", "-geometry", `+0+${staffY}`, "-composite",
    "(", state.menuPath, "-resize", "920x", ")", "-gravity", "north", "-geometry", `+0+${menuY}`, "-composite",
    "(", state.qwertyPath, "-resize", "840x", ")", "-gravity", "north", "-geometry", `+0+${qwertyY}`, "-composite");
  const caption = t < 2.15 ? openingCaptionPath
    : t >= 8.7 && t < 10.65 ? closingCaptionPath : null;
  if (caption) args.push(caption, "-gravity", "north", "-geometry", "+0+1540", "-composite");
  args.push(resolve(framesDir, `lesson-${String(i).padStart(4, "0")}.png`));
  execFileSync("magick", args, { stdio: "ignore" });
}

// Lightweight piano-like synthesis: struck harmonic partials with a quick
// attack and exponential decay. It is deterministic and aligned to the UI.
const samples = new Float32Array(Math.ceil(duration * sampleRate));
for (const [at, len, midi] of notes) {
  const start = Math.round(at * sampleRate);
  const end = Math.min(samples.length, Math.round((at + len + 1.8) * sampleRate));
  const f = 440 * 2 ** ((midi - 69) / 12);
  for (let i = start; i < end; i++) {
    const t = (i - start) / sampleRate;
    const attack = Math.min(1, t / 0.008);
    const release = t > len ? Math.exp(-(t - len) * 4.8) : 1;
    const body = Math.exp(-t * 1.35) * release;
    const s = Math.sin(2 * Math.PI * f * t)
      + 0.42 * Math.sin(2 * Math.PI * f * 2.01 * t)
      + 0.17 * Math.sin(2 * Math.PI * f * 3.98 * t);
    samples[i] += 0.13 * attack * body * s;
  }
}
// Two short tactile cues accompany the two Command-key flashes.
for (const [at] of commandTaps) {
  const start = Math.round(at * sampleRate);
  const end = Math.min(samples.length, start + Math.round(0.12 * sampleRate));
  for (let i = start; i < end; i++) {
    const t = (i - start) / sampleRate;
    samples[i] += 0.16 * Math.sin(2 * Math.PI * 880 * t) * Math.exp(-t * 28);
  }
}
{
  const start = Math.round(escapeTap[0] * sampleRate);
  const end = Math.min(samples.length, start + Math.round(0.15 * sampleRate));
  for (let i = start; i < end; i++) {
    const t = (i - start) / sampleRate;
    samples[i] += 0.16 * Math.sin(2 * Math.PI * 660 * t) * Math.exp(-t * 24);
  }
}

const wav = Buffer.alloc(44 + samples.length * 2);
const channels = 1, bits = 16, byteRate = sampleRate * channels * bits / 8;
wav.write("RIFF", 0); wav.writeUInt32LE(wav.length - 8, 4); wav.write("WAVE", 8);
wav.write("fmt ", 12); wav.writeUInt32LE(16, 16); wav.writeUInt16LE(1, 20);
wav.writeUInt16LE(channels, 22); wav.writeUInt32LE(sampleRate, 24);
wav.writeUInt32LE(byteRate, 28); wav.writeUInt16LE(channels * bits / 8, 32);
wav.writeUInt16LE(bits, 34); wav.write("data", 36); wav.writeUInt32LE(samples.length * 2, 40);
for (let i = 0; i < samples.length; i++) {
  const v = Math.max(-1, Math.min(1, samples[i]));
  wav.writeInt16LE(Math.round(v * 32767), 44 + i * 2);
}
const audioPath = resolve(out, "performance.wav");
writeFileSync(audioPath, wav);

const baseVideoPath = resolve(out, "base-menu-band-scale.mp4");
execFileSync("ffmpeg", ["-v", "error", "-y", "-framerate", String(fps),
  "-i", resolve(framesDir, "lesson-%04d.png"), "-i", audioPath, "-i", voicePath,
  "-i", escapeVoicePath,
  "-filter_complex", "[1:a]volume=0.82[synth];[2:a]volume=1.0[open];[3:a]adelay=8700|8700,volume=1.0[close];[synth][open][close]amix=inputs=3:duration=longest:normalize=0[a]",
  "-map", "0:v", "-map", "[a]", "-vf", "format=yuv420p",
  "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-c:a", "aac", "-b:a", "192k",
  "-t", String(duration), "-movflags", "+faststart", baseVideoPath], { stdio: "inherit" });

// Finish through the same 9:16 AC reel chrome as the other Menu Band films:
// audio-reactive Pals at both edges and climbing MenuBand.app title columns.
const videoPath = resolve(out, "menu-band-command-scale-instagram-reel-v9.mp4");
execFileSync(process.execPath, [stampReel, baseVideoPath,
  "--title", "MenuBand.app", "--tint", "128,96,164", "--no-fingers",
  "--motion-rate", "0.28", "--intro-pulse", "0", "--stamp-gap", "4",
  "--fps", "30", "--out", videoPath], { stdio: "inherit" });

console.log(videoPath);
