// core-stream.mjs — a video as a stream of frames, looping, for the terminal.
//
// ffmpeg decodes the film over and over, already scaled down to what
// half-block cells can show, and hands over one frame each time it is asked.
// Memory stays a few frames however long the film.
// It needs ffmpeg on the machine; without it a video plays in an outside app.
import { execFile, spawn } from "node:child_process";
import { existsSync } from "node:fs";

const PLACES = ["/opt/homebrew/bin/ffmpeg", "/usr/local/bin/ffmpeg", "/usr/bin/ffmpeg"];
let found;
/** The ffmpeg to use, or "" when there is none. Asked once. */
export function ffmpegPath() {
  return found ??= new Promise((resolve) => {
    const known = PLACES.find((place) => existsSync(place));
    if (known) return resolve(known);
    execFile("ffmpeg", ["-version"], { timeout: 4000 }, (error) => resolve(error ? "" : "ffmpeg"));
  });
}

/** The picture size ffmpeg reports for a file, from the line it prints about the video stream. */
export function videoSize(report) {
  const match = /Video:.*?\b(\d{2,5})x(\d{2,5})\b/.exec(String(report || ""));
  return match ? { width: Number(match[1]), height: Number(match[2]) } : null;
}
/** A size no wider than `maxWidth`, same shape, both sides even. */
export function fitSize({ width, height }, maxWidth = 256) {
  const scale = Math.min(1, maxWidth / width), even = (n) => Math.max(2, Math.round(n / 2) * 2);
  return { width: even(width * scale), height: even(height * scale) };
}

const probe = (ffmpeg, file) => new Promise((resolve) => {
  // ffmpeg with no output exits non-zero and still describes the input on stderr.
  execFile(ffmpeg, ["-hide_banner", "-i", file], { timeout: 8000 }, (error, stdout, stderr) => resolve(videoSize(stderr)));
});

/** Start looping `file`. Resolves to { width, height, fps, next(), stop() }. */
export async function openStream(file, { maxWidth = 256, fps = 20, onEnd = () => {} } = {}) {
  const ffmpeg = await ffmpegPath();
  if (!ffmpeg) throw new Error("ffmpeg is not installed, so a video cannot play in the terminal.");
  const size = await probe(ffmpeg, file);
  if (!size) throw new Error("ffmpeg found no picture in that file.");
  const { width, height } = fitSize(size, maxWidth), bytes = width * height * 4;
  // No -re: ffmpeg decodes as fast as it is read, and it is read one frame
  // at a time, by next(), so the host's clock paces the film the way it paces
  // a core. ffmpeg waits on the full pipe in between and costs nothing.
  const child = spawn(ffmpeg, ["-v", "error", "-stream_loop", "-1", "-i", file, "-an",
    "-vf", `fps=${fps},scale=${width}:${height}`, "-f", "rawvideo", "-pix_fmt", "rgba", "pipe:1"], { stdio: ["ignore", "pipe", "ignore"] });
  child.stdout.pause();
  let stopped = false;
  child.on("error", () => {});
  child.on("exit", () => { if (!stopped) onEnd(); });
  // The pipe is closed as well as the process killed: a paused pipe with frames left in it would hold Node open.
  const stop = () => { stopped = true; child.stdout.destroy(); child.kill("SIGKILL"); };
  process.once("exit", stop);
  /** The next frame, or null when ffmpeg has not finished one yet. Call it once a frame time. */
  const next = () => child.stdout.read(bytes);
  return { width, height, fps, next, stop: () => { process.off("exit", stop); stop(); } };
}
