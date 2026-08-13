// compose — lay narration onto the recorded clip at MEASURED offsets, and emit
// captions. Distilled from captutor/lib/compose.mjs; the amix/loudnorm shape
// and its rationale are inherited wholesale:
//
//   - adelay pins each beat's voice to where the beat truly began.
//   - amix normalize=0 is load-bearing: the default divides by input count,
//     which would leave an N-beat tutorial 1/N as loud. Beats never overlap,
//     so summing straight is exactly right.
//   - loudnorm brings ElevenLabs' conservative levels to a spoken-word target.
//
// Captions here are a VTT sidecar plus an embedded mov_text track, same as
// captutor: sidecars work in <track>, the embedded copy survives QuickTime.

import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";

const FFMPEG = process.env.FFMPEG || "ffmpeg";
const FFPROBE = process.env.FFPROBE || "ffprobe";

const stamp = (sec) => {
  const s = Math.max(0, sec);
  const h = Math.floor(s / 3600);
  const m = Math.floor((s % 3600) / 60);
  const ss = (s % 60).toFixed(3).padStart(6, "0");
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}:${ss}`;
};

/// One cue per beat, timed from its measured offset and true spoken length.
export function writeVTT(beats, path) {
  const body = beats
    .map((b, i) =>
      `${i + 1}\n${stamp(b.offsetSec)} --> ${stamp(b.offsetSec + b.durationSec)}\n${b.say}\n`)
    .join("\n");
  writeFileSync(path, `WEBVTT\n\n${body}`);
  return beats.length;
}

export function narrationFilter(beats) {
  const chains = beats.map((b, i) => {
    const ms = Math.round(b.offsetSec * 1000);
    return `[${i + 1}:a]adelay=${ms}|${ms}[d${i}]`;
  });
  const mixIn = beats.map((_, i) => `[d${i}]`).join("");
  return (
    `${chains.join(";")};${mixIn}` +
    `amix=inputs=${beats.length}:normalize=0:dropout_transition=0,` +
    `loudnorm=I=-16:LRA=7:TP=-1.5,aresample=48000[a]`
  );
}

export function mux({ clip, beats, out, vtt }) {
  const args = ["-y", "-loglevel", "error", "-i", clip];
  for (const b of beats) args.push("-i", b.mp3);
  if (vtt) args.push("-i", vtt);

  args.push(
    "-filter_complex", narrationFilter(beats),
    "-map", "0:v", "-map", "[a]",
    "-c:v", "copy",
    "-c:a", "aac", "-b:a", "192k");

  if (vtt) {
    args.push(
      "-map", `${beats.length + 1}:s`,
      "-c:s", "mov_text",
      "-metadata:s:s:0", "language=eng");
  }

  args.push("-movflags", "+faststart", out);
  execFileSync(FFMPEG, args, { stdio: ["ignore", "inherit", "inherit"] });
  return out;
}

export function probe(path) {
  const raw = execFileSync(FFPROBE, [
    "-v", "error", "-select_streams", "v:0",
    "-show_entries", "stream=width,height:format=duration",
    "-of", "json", path,
  ], { encoding: "utf8" });
  const info = JSON.parse(raw);
  return {
    width: info.streams?.[0]?.width,
    height: info.streams?.[0]?.height,
    duration: info.format?.duration,
  };
}
