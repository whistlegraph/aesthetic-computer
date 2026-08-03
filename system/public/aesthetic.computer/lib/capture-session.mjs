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

export function getCameraCaptureSize(
  requestedWidth,
  requestedHeight,
  pixelRatio = 1,
  minLongEdge = 1280,
  maxLongEdge = 1920,
) {
  const width = Math.max(2, Number(requestedWidth) || 2);
  const height = Math.max(2, Number(requestedHeight) || 2);
  const longEdge = Math.max(width, height);
  const desiredScale = Math.max(
    1,
    Number(pixelRatio) || 1,
    Math.max(2, Number(minLongEdge) || 1280) / longEdge,
  );
  const scale = Math.min(
    desiredScale,
    Math.max(2, Number(maxLongEdge) || 1920) / longEdge,
  );
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
