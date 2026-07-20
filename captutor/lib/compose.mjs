// compose — lay the narration onto the recorded clip and emit captions.
//
// Audio is placed at MEASURED offsets, not planned ones. The runner timestamps
// each beat as it actually happens against the recorder's own clock (`reel`
// reports `since`, the wall-clock moment the stream began), so a beat that
// overran its narration — an AI generation that took 40s instead of the 6s we
// scripted — cannot drift the rest of the video. Every later beat is pinned to
// where it truly landed. This is why nothing here needs to "re-sync".
//
// Captions are a VTT sidecar, not burned pixels. Two reasons: fuser's
// <VideoDocs> already renders <track kind="captions" /> (empty until now), and
// burning text needs an ffmpeg with libass — which the stock Homebrew build
// does NOT have. Sidecar captions are also selectable, searchable, and
// translatable. `--burn` can come later for platforms that demand baked text.

import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";

const FFMPEG = process.env.FFMPEG || "ffmpeg";

const stamp = (sec) => {
  const s = Math.max(0, sec);
  const h = Math.floor(s / 3600);
  const m = Math.floor((s % 3600) / 60);
  const ss = (s % 60).toFixed(3).padStart(6, "0");
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}:${ss}`;
};

/// Group a beat's words into readable caption cues.
///
/// One cue per beat would sit on screen too long to read comfortably; one cue
/// per word flickers. We break on sentence punctuation, then on a long pause
/// (people naturally chunk where the speaker breathed), then on a word ceiling.
function cues(beat, offsetSec, { maxWords = 7, pauseMs = 380 } = {}) {
  const out = [];
  let cur = [];
  const flush = () => {
    if (!cur.length) return;
    out.push({
      from: offsetSec + cur[0].fromMs / 1000,
      to: offsetSec + cur[cur.length - 1].toMs / 1000,
      text: cur.map((w) => w.text).join(" "),
    });
    cur = [];
  };
  for (const [i, w] of beat.words.entries()) {
    cur.push(w);
    const next = beat.words[i + 1];
    const endsSentence = /[.!?]$/.test(w.text);
    const bigPause = next && next.fromMs - w.toMs >= pauseMs;
    if (endsSentence || bigPause || cur.length >= maxWords) flush();
  }
  flush();
  return out;
}

export function writeVTT(beats, path) {
  const all = beats.flatMap((b) => cues(b, b.offsetSec));
  const body = all
    .map((c, i) => `${i + 1}\n${stamp(c.from)} --> ${stamp(c.to)}\n${c.text}\n`)
    .join("\n");
  writeFileSync(path, `WEBVTT\n\n${body}`);
  return all.length;
}

/// Mux: video from the reel clip (stream-copied — it is already h264 and
/// re-encoding would only soften the text), narration mixed in at each beat's
/// measured offset.
///
/// amix with normalize=0 is load-bearing. The default normalizes by input count,
/// so an N-beat tutorial would come out roughly 1/N as loud — quiet in a way
/// that reads as a broken render rather than a mixing choice. The beats never
/// overlap, so summing them straight is exactly right.
export function mux({ clip, beats, out, vtt }) {
  const args = ["-y", "-i", clip];
  for (const b of beats) args.push("-i", b.mp3);
  // Embed the captions as a real subtitle track (tx3g/mov_text), not just a
  // sidecar file. A sidecar only helps in the browser, where <VideoDocs> can
  // point a <track> at it — open the same mp4 in QuickTime and the captions
  // simply are not there. Embedding means the file carries them everywhere.
  if (vtt) args.push("-i", vtt);

  const chains = beats.map((b, i) => {
    const ms = Math.round(b.offsetSec * 1000);
    return `[${i + 1}:a]adelay=${ms}|${ms}[d${i}]`;
  });
  const mixIn = beats.map((_, i) => `[d${i}]`).join("");
  const filter =
    `${chains.join(";")};${mixIn}amix=inputs=${beats.length}:normalize=0:dropout_transition=0[a]`;

  args.push(
    "-filter_complex", filter,
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

  execFileSync(FFMPEG, args, { stdio: ["ignore", "ignore", "pipe"] });
  return out;
}

export function probe(path) {
  const raw = execFileSync("ffprobe", [
    "-v", "error",
    "-show_entries", "format=duration,size",
    "-show_entries", "stream=codec_name,width,height",
    "-of", "json", path,
  ], { encoding: "utf8" });
  return JSON.parse(raw);
}
