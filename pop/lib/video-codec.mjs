// video-codec.mjs — the H.264 encoder args every reel/preview encode shares.
//
// Default is libx264 (every Mac in the fleet). A box with an NVIDIA card sets
// AC_VIDEO_ENCODER=nvenc (jastow's reel farm) and gets h264_nvenc instead,
// tuned so `crf` means roughly the same quality on both paths.

export function videoEncoder() {
  return process.env.AC_VIDEO_ENCODER === "nvenc" ? "nvenc" : "x264";
}

// crf: libx264 constant-rate factor; nvenc's constant-quality `cq` tracks it
// about one step higher for the same look. preset: libx264 speed preset.
export function h264Args({ crf = 20, preset = "faster" } = {}) {
  if (videoEncoder() === "nvenc") {
    return ["-c:v", "h264_nvenc", "-preset", "p7", "-tune", "hq", "-rc", "vbr",
      // Capped: Instagram re-encodes everything, so bits past ~8–10 Mbps are
      // thrown away (uncapped cq ran ~11 Mbps on a clock reel).
      "-cq", String(crf + 1), "-b:v", "8M", "-maxrate", "12M", "-bufsize", "16M",
      "-spatial-aq", "1", "-temporal-aq", "1", "-bf", "3", "-g", "120",
      "-profile:v", "high"];
  }
  return ["-c:v", "libx264", "-preset", preset, "-crf", String(crf), "-threads", "0"];
}
