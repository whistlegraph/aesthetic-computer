// The sync verifier — the feedback loop the ear kept winning against.
//
// The render stage stamps every sound the game asks for onto the reel's own
// clock (render.mjs collects `oskiewar:signal` events against the first kept
// frame). This stage measures what actually landed in the encoded file and
// compares: audio onsets found by RMS rise, matched to the nearest expected
// signal. The result is arithmetic — median skew, worst skew, how much of the
// reel is silent, and the longest silent stretch with the signals that should
// have been sounding inside it. A reel that "sounds early" stops being an
// argument and becomes a number.

import { spawnSync } from "node:child_process";

// RMS level per 50ms window, from the encoded reel's audio stream.
// `asetnsamples` first, because astats' reset counts FRAMES, not samples —
// without repacking, one "window" quietly became most of a minute and the
// whole report measured mush.
function rmsWindows(reel) {
  const probe = spawnSync("ffmpeg", ["-hide_banner", "-i", reel, "-map", "0:a",
    "-af", "asetnsamples=n=2400,astats=metadata=1:reset=1," +
    "ametadata=print:key=lavfi.astats.Overall.RMS_level",
    "-f", "null", "-"], { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
  const lines = (probe.stderr || "").split("\n");
  const windows = [];
  for (let i = 0; i < lines.length - 1; i++) {
    const t = lines[i].match(/pts_time:([\d.]+)/);
    const v = lines[i + 1].match(/RMS_level=(-?[\d.]+|-inf)/);
    if (t && v) windows.push({ t: Number(t[1]),
      db: v[1] === "-inf" ? -100 : Number(v[1]) });
  }
  return windows;
}

// An onset is a rise of 8dB over the local floor into audible territory.
function onsets(windows) {
  const found = [];
  for (let i = 1; i < windows.length; i++) {
    const prev = windows[i - 1].db, here = windows[i].db;
    if (here > -45 && here - prev >= 8 &&
        (!found.length || windows[i].t - found.at(-1) > 0.12))
      found.push(windows[i].t);
  }
  return found;
}

// Silent stretches — where the reel goes quiet and for how long.
function silences(windows, floor = -55) {
  const spans = [];
  let start = null;
  for (const { t, db } of windows) {
    if (db < floor) { if (start === null) start = t; }
    else if (start !== null) {
      if (t - start >= 0.75) spans.push({ from: +start.toFixed(2), to: +t.toFixed(2) });
      start = null;
    }
  }
  const last = windows.at(-1)?.t ?? 0;
  if (start !== null && last - start >= 0.75)
    spans.push({ from: +start.toFixed(2), to: +last.toFixed(2) });
  return spans;
}

export function verifySync(reel, signals) {
  const windows = rmsWindows(reel);
  if (!windows.length) return { ok: false, why: "no audio stream to measure" };
  const heard = onsets(windows);
  const quiet = silences(windows);
  const duration = windows.at(-1).t;

  // Match each measured onset to the nearest expected signal. Skew is
  // heard-minus-expected: positive means the sound landed late in the file,
  // negative early. Signals cluster (one hit fires kick+snare+bell), so many
  // signals per onset is normal — the onset takes the closest.
  const deltas = [];
  for (const at of heard) {
    let best = null;
    for (const { t } of signals) {
      const d = at - t;
      if (best === null || Math.abs(d) < Math.abs(best)) best = d;
    }
    if (best !== null && Math.abs(best) <= 1) deltas.push(+best.toFixed(3));
  }
  deltas.sort((a, b) => a - b);
  const median = deltas.length ? deltas[Math.floor(deltas.length / 2)] : null;
  const worst = deltas.length
    ? deltas.reduce((a, b) => (Math.abs(b) > Math.abs(a) ? b : a), 0) : null;

  // Signals that should be audible inside each silent span — the killcam
  // symptom: dramatic frames whose sounds played seconds earlier, live.
  const unvoiced = quiet.map((span) => ({ ...span,
    signals: signals.filter(({ t }) => t >= span.from && t <= span.to)
      .map(({ event, t }) => `${event}@${t.toFixed(1)}`) }))
    .filter((span) => span.signals.length || span.to - span.from >= 1.5);

  const audible = windows.filter(({ db }) => db > -55).length / windows.length;

  return {
    duration: +duration.toFixed(2),
    expectedSignals: signals.length,
    heardOnsets: heard.length,
    matchedOnsets: deltas.length,
    medianSkew: median,
    worstSkew: worst,
    audibleFraction: +audible.toFixed(2),
    silences: unvoiced,
    // The gate: onsets land within 150ms of a signal at the median, nothing
    // drifts past 400ms, and no silent stretch runs 2.5s or longer.
    ok: median !== null && Math.abs(median) <= 0.15 &&
      Math.abs(worst ?? 0) <= 0.4 &&
      !quiet.some((span) => span.to - span.from >= 2.5),
  };
}
