export function normalizeTapeAudio(
  channels,
  {
    targetRms = 10 ** (-18 / 20),
    peakCeiling = 10 ** (-1 / 20),
    maxBoost = 6,
    silenceRms = 0.001,
  } = {},
) {
  const samples = (channels || []).filter((channel) => channel?.length);
  let count = 0;
  let sumSquares = 0;
  let peak = 0;

  for (const channel of samples) {
    for (let i = 0; i < channel.length; i += 1) {
      const sample = Number.isFinite(channel[i]) ? channel[i] : 0;
      sumSquares += sample * sample;
      peak = Math.max(peak, Math.abs(sample));
      count += 1;
    }
  }

  const rms = count ? Math.sqrt(sumSquares / count) : 0;
  if (!count || rms < silenceRms || peak === 0) {
    return { gain: 1, rms, peak, normalized: false };
  }

  const gain = Math.min(maxBoost, targetRms / rms, peakCeiling / peak);
  for (const channel of samples) {
    for (let i = 0; i < channel.length; i += 1) {
      channel[i] = Math.max(-peakCeiling, Math.min(peakCeiling, channel[i] * gain));
    }
  }

  return { gain, rms, peak, normalized: Math.abs(gain - 1) > 0.001 };
}

export function attachSoundtrackToFrames(frames, totalSamples) {
  if (!Array.isArray(frames) || frames.length === 0 || totalSamples <= 0) {
    return frames;
  }

  const firstAt = frames[0][0];
  const lastAt = frames[frames.length - 1][0];
  const priorAt = frames.length > 1 ? frames[frames.length - 2][0] : firstAt - 1;
  const endAt = lastAt + Math.max(1, lastAt - priorAt);
  const span = Math.max(1, endAt - firstAt);
  const starts = frames.map((frame) =>
    Math.max(
      0,
      Math.min(totalSamples, Math.round(((frame[0] - firstAt) / span) * totalSamples)),
    ),
  );

  for (let i = 0; i < frames.length; i += 1) {
    frames[i][3] = {
      audioStart: starts[i],
      audioEnd: i + 1 < starts.length ? starts[i + 1] : totalSamples,
    };
  }
  return frames;
}

export function frameIndexForSoundtrackProgress(frames, progress) {
  if (!Array.isArray(frames) || frames.length === 0) return -1;
  const totalSamples = frames[frames.length - 1]?.[3]?.audioEnd;
  if (!Number.isFinite(totalSamples) || totalSamples <= 0) return -1;

  const sample = Math.max(0, Math.min(totalSamples - 1, progress * totalSamples));
  let low = 0;
  let high = frames.length - 1;
  while (low <= high) {
    const mid = (low + high) >> 1;
    const track = frames[mid]?.[3];
    if (!track) return -1;
    if (sample < track.audioStart) high = mid - 1;
    else if (sample >= track.audioEnd) low = mid + 1;
    else return mid;
  }
  return Math.max(0, Math.min(frames.length - 1, low));
}
