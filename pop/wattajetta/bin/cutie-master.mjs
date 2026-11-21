// cutie-master.mjs — the wattajetta cutie club master, one chain for both
// engines (bin/render-wattajetta-cutie.mjs and c/run-cutie.mjs).
//
// A pop master by the house gate (pop/MASTERING.md): −10 ±1 LUFS, ≤ −2 dBTP,
// bass mono under 120 Hz, harmonics over the sine kick and bass so they are
// still there on a phone. Profiled against Fcukers' "Only One" (2026-11-20):
// that record is mono in the lows, 6 dB brighter at 2–6 k and 10 dB at 6–16 k
// than the first cutie master was.
//
//   lows   LR4 split at 120 Hz, the low band folded to mono and driven through
//          tanh at unity small-signal gain: the kick's peak comes down (it
//          stood 17 dB over the body) and the bass under it does not move
//          (not oversampled: ffmpeg's oversampled clip delays the band, and
//          against the undelayed band above it that cancelled 5 dB under 60 Hz
//          and 3.5 dB at 60–120 Hz, measured 2026-11-20; under 120 Hz there is
//          nothing for the oversampling to protect)
//   teeth  that band's harmonics, 180–1100 Hz, mixed back under
//   tone   presence and air lifts (the voices are sines; they have room for it)
//   width  the form's stereo story: a side gain over the band above 120 Hz,
//          drawn between the breakpoints the renderer writes
//   glue   3:1 compressor, fast enough to take the bell and clap transients
//          so the limiter rests (idle half the time, 1.8 dB at the 90th centile)
//   level  one static gain into a 4× oversampled limiter at −2.3 dBFS

export const MASTER = {
  lowSplit: 120,
  lowDrive: 8,        // dB into the tanh and back out again
  teeth: 1.2,         // linear mix of the low band's harmonics (0.7 before the low band's delay was fixed)
  lowShelf: -3,       // dB under 100 Hz (−1.5 before; with the lows whole again the phone proxy lost 5.8 dB)
  presence: 3,        // dB at 3.4 kHz
  air: 2,             // dB shelf over 9 kHz (4.5 read as too airy, 2026-11-20)
  glue: "threshold=-23dB:ratio=3:attack=4:release=110:makeup=1:knee=8",
  gain: 12,           // dB, the one static gain
  ceiling: -2.3,      // dBFS at the limiter; lands under −2 dBTP after decode
};

// [[seconds, gain], …] → an ffmpeg expression in t, linear between the points
function curve(points) {
  let e = String(points.at(-1)[1]);
  for (let i = points.length - 2; i >= 0; i--) {
    const [t0, w0] = points[i], [t1, w1] = points[i + 1];
    if (t1 > t0) e = `if(lt(t,${t1.toFixed(3)}),${w0}+(${w1 - w0})*(t-${t0.toFixed(3)})/${(t1 - t0).toFixed(3)},${e})`;
  }
  return e;
}

// ffmpeg -filter_complex for input 0 → [out]. fade is [start, seconds] or null;
// width is the renderer's [[seconds, side gain], …] or null for none.
export function masterGraph(sr, fade, M = MASTER, width = null) {
  const W = width?.length > 1 ? curve(width) : null;
  const mid = "0.5*(val(0)+val(1))", side = "0.5*(val(0)-val(1))";
  const mono = "pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1";
  return [
    `[0:a]highpass=f=28,acrossover=split=${M.lowSplit}:order=4th[lo]${W ? "[hi0]" : "[hi]"}`,
    ...(W ? [`[hi0]aeval=exprs='${mid}+(${W})*${side}|${mid}-(${W})*${side}':c=same[hi]`] : []),
    `[lo]${mono},volume=${M.lowDrive}dB,asoftclip=type=tanh,volume=${-M.lowDrive}dB,asplit=2[low][drv]`,
    `[drv]highpass=f=180,highpass=f=180,lowpass=f=1100,volume=${M.teeth}[teeth]`,
    `[low][hi][teeth]amix=inputs=3:normalize=0,` + [
      `lowshelf=f=100:g=${M.lowShelf}`,
      `equalizer=f=3400:t=o:w=1.6:g=${M.presence}`,
      `highshelf=f=9000:g=${M.air}`,
      `acompressor=${M.glue}`,
      `volume=${M.gain}dB`,
      `aresample=${sr * 4}`,
      `alimiter=limit=${M.ceiling}dB:attack=5:release=90:asc=1:level=disabled`,
      `aresample=${sr}`,
      ...(fade ? [`afade=t=out:st=${fade[0].toFixed(2)}:d=${fade[1]}`] : []),
    ].join(",") + "[out]",
  ].join(";");
}

export const COVER = new URL("../cover/wattajetta-cutie-cover-3000.jpg", import.meta.url).pathname;
