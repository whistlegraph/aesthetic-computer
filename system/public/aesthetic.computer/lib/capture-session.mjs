export function shouldResumeCaptureRecorder(state, freshSession = false) {
  return state === "paused" && !freshSession;
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
