// Stage 3 — the words, and only the words.
//
// Nothing is drawn on the video. The reel is the match as the game rendered
// it at 1080x1920: no plate, no bands, no blurred backdrop, no scaling. An
// earlier version composed a square capture into a letterbox with burned-in
// type; @jeffrey asked for uncut gameplay at the native shape instead, and he
// is right — the game already lays itself out for the viewport it is handed,
// so anything composed on top is a second, worse layout fighting the first.
//
// What survives here is the part that was never pixels: the caption, the
// hashtags, the cover frame Meta asks for, a review thumbnail, and the spec
// check that decides whether the file is publishable at all.

import { spawnSync } from "node:child_process";
import { existsSync, writeFileSync } from "node:fs";

export const reelSize = { width: 1080, height: 1920 };

const durationOf = (video) => {
  const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries",
    "format=duration", "-of", "csv=p=0", video], { encoding: "utf8" });
  return Number(probe.stdout?.trim()) || 0;
};

// Meta crops any cover to the middle 9:16, so a frame of a 9:16 reel is
// already the right shape. Where to take it is the harder question: a fixed
// offset used to land inside the opening countdown, so every cover was two
// motionless figures. Sampling by fraction puts it in the middle of the
// fight, which is the only part worth showing.
export function cover(reel, out, at = 0.45) {
  const stamp = Math.max(0.5, durationOf(reel) * at);
  const ffmpeg = spawnSync("ffmpeg", ["-y", "-ss", String(stamp), "-i", reel,
    "-frames:v", "1", "-q:v", "2", out], { encoding: "utf8" });
  if (ffmpeg.status !== 0) throw new Error(ffmpeg.stderr?.slice(-500) || "cover failed");
  return out;
}

// Slidecop's own test, run rather than asserted: shrink a frame to a tenth and
// look at it. There is no burned-in type left to fail, so what this checks now
// is whether the *fight* still reads at thumbnail size — which is what decides
// whether anyone stops scrolling.
export function thumbnail(reel, out, at = 0.45) {
  const stamp = Math.max(0.5, durationOf(reel) * at);
  spawnSync("ffmpeg", ["-y", "-ss", String(stamp), "-i", reel, "-frames:v", "1",
    "-vf", `scale=${Math.round(reelSize.width / 10)}:-1`, "-q:v", "2", out],
    { encoding: "utf8" });
  return existsSync(out) ? out : null;
}

// What Meta's spec table asks about, answered from the file itself.
export function inspect(reel) {
  const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries",
    "format=duration,size:stream=codec_type,codec_name,width,height,r_frame_rate," +
    "bit_rate,sample_rate,channels", "-of", "json", reel], { encoding: "utf8" });
  const data = JSON.parse(probe.stdout || "{}");
  const video = (data.streams || []).find((s) => s.codec_type === "video") || {};
  const audio = (data.streams || []).find((s) => s.codec_type === "audio") || {};
  const seconds = Number(data.format?.duration || 0);
  const megabytes = Number(data.format?.size || 0) / 1e6;
  const [num, den] = String(video.r_frame_rate || "0/1").split("/").map(Number);
  const fps = den ? num / den : 0;
  const aspect = video.width && video.height ? video.width / video.height : 0;
  const checks = {
    container: { ok: /\.mp4$/.test(reel), value: "mp4" },
    videoCodec: { ok: ["h264", "hevc"].includes(video.codec_name), value: video.codec_name },
    audioCodec: { ok: audio.codec_name === "aac", value: audio.codec_name },
    sampleRate: { ok: Number(audio.sample_rate) <= 48000, value: audio.sample_rate },
    channels: { ok: [1, 2].includes(Number(audio.channels)), value: audio.channels },
    frameRate: { ok: fps >= 23 && fps <= 60, value: fps.toFixed(2) },
    columns: { ok: Number(video.width) <= 1920, value: video.width },
    // 9:16 exactly, because the capture is 9:16 — a drift here means
    // something scaled the video, which is the thing this stage must not do.
    aspect: { ok: Math.abs(aspect - 9 / 16) < 0.001, value: aspect.toFixed(4) },
    duration: { ok: seconds >= 3 && seconds <= 900, value: seconds.toFixed(2) },
    fileSize: { ok: megabytes <= 300, value: `${megabytes.toFixed(1)} MB` },
    // Meta asks for 128 kbps. A track far under it is not a codec problem —
    // it is a silent match, which is a reel nobody should post.
    audioBitrate: { ok: Number(audio.bit_rate) >= 64000,
      value: `${Math.round(Number(audio.bit_rate || 0) / 1000)} kbps` },
  };
  return { width: video.width, height: video.height, seconds, megabytes, fps,
    checks, ok: Object.values(checks).every((check) => check.ok) };
}

export function writeSidecar(path, payload) {
  writeFileSync(path, JSON.stringify(payload, null, 2) + "\n");
  return path;
}
