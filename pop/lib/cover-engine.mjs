// cover-engine.mjs — shared cover-video effects, extracted so the
// trancenwaltz (pop/dance/bin/cover-video.mjs) and undabeach
// (pop/chillwave/bin/preview-score.mjs) renderers stop forking.
//
// Phase 1 of reports/pop-pipeline-audit.md: the verlet-string playhead
// + rotating-disc + the string→illustration WARP. A new effect added
// here lands in BOTH songs.
//
// Usage:
//   import { makeVerletString } from "../../lib/cover-engine.mjs";
//   const vs = makeVerletString(ctx, { W, H, playheadX, duration });
//   vs.pluck(yPix, amount, sign, rgb); vs.step(); vs.warp(); vs.draw();
//   vs.withRotation(vs.trackTheta(audioT), () => { …lanes…; vs.draw(); });

import { createCanvas } from "canvas";

export function hexToRgb(hex) {
  const h = String(hex).replace("#", "");
  return [parseInt(h.slice(0, 2), 16) || 0,
          parseInt(h.slice(2, 4), 16) || 0,
          parseInt(h.slice(4, 6), 16) || 0];
}

// A pinned vertical segmented string the track plucks as events cross.
// The track group (lane waveforms + string) rotates as one disc a
// plain 360° about canvas centre across the song, so the string is
// extended PAST the canvas diagonal — at any rotation it still cuts
// clear across frame with its round caps off-screen.
export function makeVerletString(ctx, { W, H, playheadX, duration, segments = 72 }) {
  const NS_N = segments;
  const ROT_CX = Math.round(W / 2);
  const ROT_CY = Math.round(H / 2);
  const ROT_DIAG = Math.hypot(W, H);
  const NS_Y0 = ROT_CY - ROT_DIAG / 2 - 40;
  const NS_Y1 = ROT_CY + ROT_DIAG / 2 + 40;

  const nsY = new Float32Array(NS_N);
  const nsX = new Float32Array(NS_N);
  const nsPX = new Float32Array(NS_N);
  const nsHot = new Float32Array(NS_N);
  for (let i = 0; i < NS_N; i++) {
    nsY[i] = NS_Y0 + (NS_Y1 - NS_Y0) * (i / (NS_N - 1));
    nsX[i] = playheadX; nsPX[i] = playheadX;
  }
  const nsCR = new Float32Array(NS_N);
  const nsCG = new Float32Array(NS_N);
  const nsCB = new Float32Array(NS_N);
  nsCR.fill(255); nsCG.fill(232); nsCB.fill(150); // default nylon cream

  function trackTheta(at) {
    return Math.PI * 2 * Math.max(0, Math.min(1, at / Math.max(0.001, duration)));
  }
  function withRotation(theta, fn) {
    ctx.save();
    ctx.translate(ROT_CX, ROT_CY);
    ctx.rotate(theta);
    ctx.translate(-ROT_CX, -ROT_CY);
    fn();
    ctx.restore();
  }
  function pluck(yPix, amount, sign, rgb) {
    let idx = Math.round(((yPix - NS_Y0) / (NS_Y1 - NS_Y0)) * (NS_N - 1));
    idx = Math.max(2, Math.min(NS_N - 3, idx));
    nsX[idx]     += sign * amount;
    nsX[idx - 1] += sign * amount * 0.55;
    nsX[idx + 1] += sign * amount * 0.55;
    nsHot[idx] = 1;
    nsHot[idx - 1] = Math.max(nsHot[idx - 1], 0.7);
    nsHot[idx + 1] = Math.max(nsHot[idx + 1], 0.7);
    if (rgb) {
      for (const j of [idx - 1, idx, idx + 1]) {
        nsCR[j] = rgb[0]; nsCG[j] = rgb[1]; nsCB[j] = rgb[2];
      }
    }
  }
  function step() {
    for (let i = 1; i < NS_N - 1; i++) {
      const v = (nsX[i] - nsPX[i]) * 0.90;
      nsPX[i] = nsX[i];
      nsX[i] = nsX[i] + v + (playheadX - nsX[i]) * 0.020;
    }
    for (let pass = 0; pass < 2; pass++) {
      for (let i = 1; i < NS_N - 1; i++) {
        nsX[i] = (nsX[i] * 2 + nsX[i - 1] + nsX[i + 1]) * 0.25;
      }
    }
    nsX[0] = nsX[NS_N - 1] = playheadX;
    for (let i = 0; i < NS_N; i++) nsHot[i] *= 0.90;
  }
  function needleXAt(yPix) {
    const f = Math.max(0, Math.min(1, (yPix - NS_Y0) / (NS_Y1 - NS_Y0))) * (NS_N - 1);
    const i = Math.floor(f), frac = f - i;
    if (i >= NS_N - 1) return nsX[NS_N - 1];
    return nsX[i] + (nsX[i + 1] - nsX[i]) * frac;
  }

  // ── string → ILLUSTRATION warp ──────────────────────────────────
  // A vertical strip around the playhead is re-drawn in row-bands,
  // each sheared horizontally to follow the string's local deflection
  // (sin² window → no seam at the strip edges), so the picture reads
  // as physically tugged by the string. Skipped when ~straight.
  const WARP_HALF = 190, WARP_W = WARP_HALF * 2;
  const WARP_STEP = 6, WARP_BANDS = 7, WARP_STRENGTH = 0.85;
  const WARP_BAND_W = WARP_W / WARP_BANDS;
  const warpWin = new Float64Array(WARP_BANDS);
  for (let b = 0; b < WARP_BANDS; b++) {
    warpWin[b] = Math.sin(Math.PI * ((b + 0.5) / WARP_BANDS)) ** 2;
  }
  const warpCanvas = createCanvas(WARP_W, H);
  const warpCtx = warpCanvas.getContext("2d");
  function warp() {
    let maxDev = 0;
    for (let i = 1; i < NS_N - 1; i++) {
      const d = Math.abs(nsX[i] - playheadX);
      if (d > maxDev) maxDev = d;
    }
    if (maxDev < 1.5) return; // string straight → nothing to warp
    const x0 = Math.round(playheadX - WARP_HALF);
    warpCtx.clearRect(0, 0, WARP_W, H);
    warpCtx.drawImage(ctx.canvas, x0, 0, WARP_W, H, 0, 0, WARP_W, H);
    for (let y = 0; y < H; y += WARP_STEP) {
      const dx = (needleXAt(y) - playheadX) * WARP_STRENGTH;
      for (let b = 0; b < WARP_BANDS; b++) {
        const sx = b * WARP_BAND_W;
        const shift = dx * warpWin[b];
        ctx.drawImage(
          warpCanvas, sx, y, WARP_BAND_W + 1, WARP_STEP,
          x0 + sx + shift, y, WARP_BAND_W + 1, WARP_STEP,
        );
      }
    }
  }

  function draw() {
    ctx.save();
    ctx.lineCap = "round";
    ctx.lineJoin = "round";
    ctx.strokeStyle = "rgba(0,0,0,0.55)";
    ctx.lineWidth = 5;
    ctx.beginPath();
    ctx.moveTo(nsX[0] + 3, nsY[0] + 4);
    for (let i = 1; i < NS_N; i++) ctx.lineTo(nsX[i] + 3, nsY[i] + 4);
    ctx.stroke();
    for (let i = 1; i < NS_N; i++) {
      const h = Math.max(nsHot[i], nsHot[i - 1]);
      const dev = Math.abs(((nsX[i] + nsX[i - 1]) / 2) - playheadX);
      const act = Math.min(1, Math.max(h, dev / 26));
      const x0 = nsX[i - 1], y0 = nsY[i - 1], x1 = nsX[i], y1 = nsY[i];
      ctx.strokeStyle = `rgba(60,46,32,${(0.10 + act * 0.40).toFixed(3)})`;
      ctx.lineWidth = 4 + act * 4;
      ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1); ctx.stroke();
      const cr = (nsCR[i] + nsCR[i - 1]) / 2;
      const cg = (nsCG[i] + nsCG[i - 1]) / 2;
      const cb = (nsCB[i] + nsCB[i - 1]) / 2;
      const wb = 0.16 * act;
      const br = Math.round(cr + (255 - cr) * wb);
      const bg = Math.round(cg + (255 - cg) * wb);
      const bbl = Math.round(cb + (255 - cb) * wb);
      ctx.strokeStyle = `rgba(${br},${bg},${bbl},${(0.13 + act * 0.80).toFixed(3)})`;
      ctx.lineWidth = 2.2 + act * 3.4;
      ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1); ctx.stroke();
      const specA = act * 0.85;
      if (specA > 0.02) {
        ctx.strokeStyle = `rgba(255,255,246,${specA.toFixed(3)})`;
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.moveTo(x0 - 1.2, y0); ctx.lineTo(x1 - 1.2, y1);
        ctx.stroke();
      }
    }
    ctx.restore();
  }

  // ── string-vibe SCENE displacement (the undabeach effect) ───────
  // The whole illustration is shoved along the disc's rotation angle
  // by the string's PEAK deflection, plus a slight tilt from the
  // AVERAGE deflection and a tiny scale-punch — so the picture
  // visibly lurches/tilts with the verlet vibe as the disc turns.
  function deflection() {
    let devSum = 0, devPeak = 0;
    for (let i = 1; i < NS_N - 1; i++) {
      const d = nsX[i] - playheadX;
      devSum += d;
      if (Math.abs(d) > Math.abs(devPeak)) devPeak = d;
    }
    return { devPeak, devAvg: devSum / (NS_N - 2) };
  }
  // String-bend → scene PAN + ZOOM BUMP (no rotation). Returns an
  // offset the caller folds straight into its drawX/drawY/zoom, so
  // the illustration AND its backlight transmission-mask AND the
  // lanes all stay registered (a ctx-transform wrapper broke the
  // stained-glass mask). A signed spring genuinely follows the verlet
  // swing (a low-pass mean cancels an oscillating signal to zero).
  let sm = 0;
  function sceneOffset() {
    const { devPeak } = deflection();
    const tgt = Math.max(-70, Math.min(70, devPeak));
    sm += (tgt - sm) * 0.45;                  // follows the bend, lightly eased
    const u = Math.abs(sm) / 70;
    return {
      dx: sm * 2.0,                            // horizontal pan/bump
      dy: sm * 0.7,                            // a little vertical too
      zoomMul: 1 + u * 0.045,                  // gentle zoom BUMP, no rotate
    };
  }

  return { trackTheta, withRotation, pluck, step, needleXAt, warp, draw,
           deflection, sceneOffset, NS_Y0, NS_Y1, ROT_DIAG };
}
