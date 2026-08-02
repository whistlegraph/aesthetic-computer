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
