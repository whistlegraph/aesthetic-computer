#!/usr/bin/env node
// gen-vo.mjs — ElevenLabs "jeffrey" voiceover for the Menu Band promo reel.
//
// Synthesizes one short line per reel section through the AC /api/say
// endpoint (provider "jeffrey", stability >= 0.5 to keep his identity),
// then assembles a single vo.mp3 timed to the 34.4s reel: each line is
// delayed to its section's t0 with adelay, then amix'd onto a silent bed.
//
//   node pop/menuband/bin/gen-vo.mjs          # synth + assemble
//   node pop/menuband/bin/gen-vo.mjs --play    # play the assembled vo.mp3
//
// Outputs:
//   out/vo/line-<id>.mp3   per-line audio
//   out/vo.mp3             timeline-placed mix (34.4s)
//   out/vo.timings.json    [{id, t0, dur, text}]
//
// Does NOT touch menuband-reel.mp4 — the mux is the main session's call.

import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { mkdir, writeFile, readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const execFileP = promisify(execFile);

const HERE = path.dirname(fileURLToPath(import.meta.url));
const MB = path.resolve(HERE, ".."); // pop/menuband
const OUT = path.join(MB, "out");
const VO_DIR = path.join(OUT, "vo");
const BOARD = path.join(MB, "board.json");
const NOTES = path.join(OUT, "menuband-waltz.notes.json");

const SAY_URL = "https://aesthetic.computer/api/say";
const STABILITY = 0.55; // >= 0.5 keeps jeffrey's identity (per project memory)

// ── The VO script ─────────────────────────────────────────────────────
// One line per section, lowercase, plain, a little wry — jeffrey's voice,
// not ad-copy. 1-open is a breath (no line). Each line is short enough to
// land inside its section at a natural pace.
//
//   1-menu   1.2–14.4 (13.2s) the hero; room for two sentences
//   2-about  14.4–22.7 (8.3s) one short line
//   3-keymap 22.7–31.0 (8.3s) one short line
//   4-end    31.0–34.4 (3.4s) the sign-off
const SCRIPT = {
  "1-menu": "wanna start a band? play music straight from your macos menu bar.",
  "2-about": "it's a tiny download, and it speaks your language.",
  "3-keymap": "it goes fullscreen too — a whole keymap, a gamepad mode, and a mic sampler.",
  "4-end": "menu band. now on the mac app store.",
};

async function ffprobeDuration(file) {
  const { stdout } = await execFileP("ffprobe", [
    "-v", "error",
    "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1",
    file,
  ]);
  return parseFloat(stdout.trim());
}

async function synthLine(text, outFile) {
  // POST to /api/say. Text travels in `body.from` (the say.js contract).
  // Response is a 302 → CDN mp3 (or 200 base64 if caching failed).
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Origin: "https://aesthetic.computer",
    },
    body: JSON.stringify({ from: text, provider: "jeffrey", stability: STABILITY }),
    redirect: "follow", // fetch follows the 302 to the CDN automatically
  });

  const ctype = res.headers.get("content-type") || "";
  if (!res.ok) {
    const body = await res.text();
    throw new Error(`/api/say ${res.status} ${res.statusText}\n  ctype: ${ctype}\n  body: ${body.slice(0, 500)}`);
  }

  let buf;
  if (ctype.includes("application/json")) {
    // Unexpected for our request shape, but handle base64 / url fallbacks.
    const json = await res.json();
    if (json.audio) buf = Buffer.from(json.audio, "base64");
    else if (json.url) buf = Buffer.from(await (await fetch(json.url)).arrayBuffer());
    else throw new Error(`/api/say returned JSON without audio:\n  ${JSON.stringify(json).slice(0, 500)}`);
  } else {
    buf = Buffer.from(await res.arrayBuffer());
  }

  if (!buf || buf.length < 256) {
    throw new Error(`/api/say returned tiny/empty body (${buf?.length ?? 0} bytes), ctype=${ctype}`);
  }
  await writeFile(outFile, buf);
  return ffprobeDuration(outFile);
}

async function main() {
  const playOnly = process.argv.includes("--play");
  const voMp3 = path.join(OUT, "vo.mp3");

  if (playOnly) {
    if (!existsSync(voMp3)) {
      console.error(`No ${voMp3} yet — run without --play first.`);
      process.exit(1);
    }
    console.log(`▶ playing ${voMp3}`);
    try {
      await execFileP("open", ["-a", "QuickTime Player", voMp3]);
    } catch {
      await execFileP("afplay", [voMp3]);
    }
    return;
  }

  await mkdir(VO_DIR, { recursive: true });

  const board = JSON.parse(await readFile(BOARD, "utf8"));
  let totalDur = 34.4131;
  try {
    const notes = JSON.parse(await readFile(NOTES, "utf8"));
    if (notes.durationSec) totalDur = notes.durationSec;
  } catch { /* fall back to default reel length */ }

  // Build the work list from the board, in section order.
  const shots = [...board.shots].sort((a, b) => a.t0 - b.t0);

  const timings = [];
  const lineFiles = []; // { id, t0, file }

  console.log(`Synthesizing jeffrey VO (provider=jeffrey, stability=${STABILITY})\n`);

  for (const shot of shots) {
    const text = SCRIPT[shot.id];
    if (!text) {
      console.log(`  ${shot.id.padEnd(8)} (no line — silence)`);
      continue;
    }
    const file = path.join(VO_DIR, `line-${shot.id}.mp3`);
    const dur = await synthLine(text, file);
    const sectionLen = shot.t1 - shot.t0;
    const fit = dur <= sectionLen ? "ok" : `⚠ over by ${(dur - sectionLen).toFixed(2)}s`;
    console.log(`  ${shot.id.padEnd(8)} t0=${shot.t0.toFixed(1)}s  dur=${dur.toFixed(2)}s / ${sectionLen.toFixed(1)}s  ${fit}`);
    console.log(`           "${text}"`);
    timings.push({ id: shot.id, t0: shot.t0, dur: Number(dur.toFixed(3)), text });
    lineFiles.push({ id: shot.id, t0: shot.t0, file });
  }

  // ── Assemble: silent bed of totalDur, each line adelay'd to its t0,
  //    then amix everything together. ──────────────────────────────────
  const inputs = [];
  for (const lf of lineFiles) inputs.push("-i", lf.file);

  // anullsrc bed as the last input so the mix length is fixed at totalDur.
  const bedIdx = lineFiles.length;
  const filterParts = [];
  const mixLabels = [];
  lineFiles.forEach((lf, i) => {
    const ms = Math.round(lf.t0 * 1000);
    filterParts.push(`[${i}:a]adelay=${ms}|${ms},apad[a${i}]`);
    mixLabels.push(`[a${i}]`);
  });
  // Bed: stereo silence for the full reel duration.
  const bedLabel = `[bed]`;
  filterParts.push(`[${bedIdx}:a]atrim=0:${totalDur.toFixed(3)},asetpts=PTS-STARTPTS${bedLabel}`);
  mixLabels.push(bedLabel);

  const filter =
    filterParts.join(";") +
    ";" +
    mixLabels.join("") +
    `amix=inputs=${mixLabels.length}:duration=longest:normalize=0,` +
    `atrim=0:${totalDur.toFixed(3)},asetpts=PTS-STARTPTS[out]`;

  const args = [
    "-y",
    ...inputs,
    "-f", "lavfi", "-t", totalDur.toFixed(3), "-i", "anullsrc=channel_layout=stereo:sample_rate=44100",
    "-filter_complex", filter,
    "-map", "[out]",
    "-c:a", "libmp3lame", "-q:a", "2",
    voMp3,
  ];

  await execFileP("ffmpeg", args, { maxBuffer: 32 * 1024 * 1024 });

  const voDur = await ffprobeDuration(voMp3);
  await writeFile(path.join(OUT, "vo.timings.json"), JSON.stringify(timings, null, 2) + "\n");

  console.log(`\n✅ ${path.relative(MB, voMp3)}  total=${voDur.toFixed(2)}s (reel ${totalDur.toFixed(2)}s)`);
  console.log(`   ${path.relative(MB, path.join(OUT, "vo.timings.json"))}`);
  console.log(`   ${lineFiles.length} per-line files in ${path.relative(MB, VO_DIR)}/`);
  console.log(`\n   listen:  node pop/menuband/bin/gen-vo.mjs --play`);
}

main().catch((err) => {
  console.error("\n❌ gen-vo failed:\n" + (err?.stack || err?.message || String(err)));
  process.exit(1);
});
