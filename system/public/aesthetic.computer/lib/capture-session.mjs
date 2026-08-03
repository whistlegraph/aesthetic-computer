export function shouldResumeCaptureRecorder(state, freshSession = false) {
  return state === "paused" && !freshSession;
}

const COMPACT_VIDEO_MIMES = [
  "video/mp4;codecs=avc1.42E01E,mp4a.40.2",
  "video/mp4;codecs=avc1.42E01E",
  "video/mp4",
];

export function chooseCompactVideoMime(isTypeSupported) {
  if (typeof isTypeSupported !== "function") return null;
  return COMPACT_VIDEO_MIMES.find((mime) => isTypeSupported(mime)) || null;
}

export function getCompactVideoSize(
  sourceWidth,
  sourceHeight,
  pixelRatio = 1,
  maxDimension = 1440,
) {
  const width = Math.max(2, Number(sourceWidth) || 2);
  const height = Math.max(2, Number(sourceHeight) || 2);
  const ratio = Math.max(1, Number(pixelRatio) || 1);
  const limit = Math.max(2, Number(maxDimension) || 1440);
  const scale = Math.min(ratio, limit / Math.max(width, height));
  const even = (value) => Math.max(2, Math.round(value / 2) * 2);
  return { width: even(width * scale), height: even(height * scale) };
}

export function measureCaptureAVSync(videoStartMs, audioStartMs, toleranceMs = 80) {
  if (!Number.isFinite(videoStartMs) || !Number.isFinite(audioStartMs)) {
    return null;
  }
  const offsetMs = audioStartMs - videoStartMs;
  return {
    videoStartMs,
    audioStartMs,
    offsetMs,
    toleranceMs,
    aligned: Math.abs(offsetMs) <= toleranceMs,
  };
}
