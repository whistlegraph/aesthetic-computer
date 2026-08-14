// The sync verifier — the feedback loop the ear kept winning against.
//
// The render stage stamps every sound the game asks for onto the reel's own
// clock — the demo's ticks, which is what the shipped picture is cut on. This
// stage measures what actually landed in the encoded file and compares: audio
// onsets found by RMS rise, against the score. The result is arithmetic —
// offset, coverage, median and worst skew, how much of the reel is silent, and
// the longest silent stretch with the signals that should have been sounding
// inside it. A reel that "sounds early" stops being an argument and becomes a
// number.
//
// This is only worth anything because the signals arrive on the same clock the
// picture is cut on (see `demoOriginMs` in render.mjs). Stamp them on the
// audio's own clock and every measurement below compares the sound to itself.

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

// The score at the detector's own resolution. Signals fire in chords — one
// connected punch stamps punch + partdamage + bodyhit on the same tick — and
// `onsets` cannot resolve two rises inside 0.12s, so counting raw signals
// against onsets compares a number to a number that can never reach it.
// Collapsing them the same way the detector does gives an honest denominator.
function chords(signals, gap = 0.12) {
  const kept = [];
  for (const t of signals.map((signal) => signal.t).sort((a, b) => a - b))
    if (!kept.length || t - kept.at(-1) > gap) kept.push(t);
  return kept;
}

// The one lag that best explains every onset at once. Matching each onset to
// its nearest signal is blind to a constant offset — slide the whole track a
// third of a second and each onset simply re-pairs with the neighbouring
// chord, which is exactly how 277ms of skew measured as 8ms and shipped.
// Sliding the whole score against the whole measurement cannot be fooled that
// way. Ties go to the lag nearest zero, so a rhythm too regular to localise is
// called aligned rather than accused.
function lag(score, heard, reach = 0.6, step = 0.005, window = 0.06) {
  let best = { at: 0, hits: 0 };
  for (let at = -reach; at <= reach + 1e-9; at += step) {
    const hits = score.filter((t) =>
      heard.some((onset) => Math.abs(onset - t - at) <= window)).length;
    if (hits > best.hits ||
        (hits === best.hits && Math.abs(at) < Math.abs(best.at)))
      best = { at: +at.toFixed(3), hits };
  }
  return best.at;
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

  // Two questions, kept apart so each failure has one name. `offset` asks
  // whether the sound is in the right place; `coverage` — measured at that
  // offset, so a merely-shifted reel is not also accused of being mute —
  // asks whether it is there at all. Nothing used to ask the second one, and
  // a reel could pass on a handful of onsets while the fight played silent.
  const score = chords(signals);
  const offset = score.length && heard.length ? lag(score, heard) : 0;
  const voiced = score.filter((t) =>
    heard.some((at) => Math.abs(at - t - offset) <= 0.15)).length;
  const coverage = score.length ? +(voiced / score.length).toFixed(2) : 1;

  return {
    duration: +duration.toFixed(2),
    expectedSignals: signals.length,
    signalChords: score.length,
    voicedChords: voiced,
    coverage,
    offset,
    heardOnsets: heard.length,
    matchedOnsets: deltas.length,
    medianSkew: median,
    worstSkew: worst,
    audibleFraction: +audible.toFixed(2),
    silences: unvoiced,
    // The gate: the whole track sits within 80ms of the picture, per-onset
    // jitter stays inside 150ms at the median and 400ms at worst, three
    // quarters of the score is voiced, and no silent stretch runs 2.5s.
    //
    // 80ms on offset: audio ahead of picture is the harsh direction and stops
    // being detectable around 45ms, unacceptable around 90ms (ITU-R BT.1359).
    // The mux is now tick-exact arithmetic, so anything past 5 frames is a
    // broken seam, not tolerance.
    //
    // 0.75 on coverage: healthy reels measure 0.82–0.91 (2026-08-13 s1, s2),
    // and the missing sixth is not a defect — a hit landing on a neighbour
    // that is still ringing cannot produce a fresh 8dB rise, so some masking
    // is the detector's own floor. 0.75 sits below that floor with room for a
    // busier round, and still fails hard on anything structural: a dead audio
    // tee, a muted mix, or a reel whose sound covers only its opening all land
    // far underneath. Failing is cheap — the reel is held for a human.
    ok: median !== null && Math.abs(offset) <= 0.08 &&
      Math.abs(median) <= 0.15 && Math.abs(worst ?? 0) <= 0.4 &&
      coverage >= 0.75 &&
      !quiet.some((span) => span.to - span.from >= 2.5),
  };
}
