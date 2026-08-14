#!/usr/bin/env node
// maytrax/bin/beat-align.mjs — find each clip's visual downbeats and slide
// the clip so they land on the track's real ones.
//
// A Seedance take has its own internal rhythm: landings, direction changes,
// paw strikes. Those are IMPACTS — frames where inter-frame motion spikes.
// The generated clip runs longer than its section (ceil(exact)+crossfade),
// so there is SLACK: a choice of where inside the take the section's window
// sits. This measures per-frame motion for every picked take, then chooses
// the head-offset whose impacts line up best with the kicks inside that
// section — read from the renderer's own events.json, never detected.
//
//   node pop/maytrax/bin/beat-align.mjs            # analyze + write offsets
//   node pop/maytrax/bin/beat-align.mjs --chart <video>   # energy chart PNG
//
// The offsets land in out/reel/motion/offsets.json; the assembly step in
// pop/lib/motion-pipeline.mjs honours them on cut-shots.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createCanvas } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const SLUG = "femrag-plusplus";
const MOTION_DIR = `${OUT}/reel/motion`;
const FPS = 24;
const FROM = 26.67;                    // reel slice offset into the track
const XF = 60 / 144 / 2;               // the assembly's eighth-note crossfade

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

// Per-frame motion: decode small + gray, mean |Δ| between frames. The
// POSITIVE derivative of that curve is the impact signal — energy arriving.
function motionCurve(path) {
  const AW = 160, AH = 90;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", path, "-f", "rawvideo", "-pix_fmt", "gray", "-s", `${AW}x${AH}`,
    "-r", String(FPS), "pipe:1"],
    { maxBuffer: 1 << 30 });
  if (r.status !== 0 || !r.stdout?.length) return null;
  const raw = r.stdout, fb = AW * AH, n = Math.floor(raw.length / fb);
  const curve = new Float32Array(n);
  for (let f = 1; f < n; f++) {
    let acc = 0;
    const a = f * fb, b = (f - 1) * fb;
    for (let p = 0; p < fb; p += 2) acc += Math.abs(raw[a + p] - raw[b + p]);
    curve[f] = acc / (fb / 2) / 255;
  }
  return curve;
}
// Full colour analysis: per-frame mean luma + dominant hue (circular mean
// of the frame's chroma). These are the two arcs @jeffrey wants the edit
// to map musically — overall light/dark, and changes in dominant colour.
function colorCurve(path) {
  const AW = 96, AH = 54;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", path, "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${AW}x${AH}`,
    "-r", String(FPS), "pipe:1"], { maxBuffer: 1 << 30 });
  if (r.status !== 0 || !r.stdout?.length) return null;
  const raw = r.stdout, fb = AW * AH * 3, n = Math.floor(raw.length / fb);
  const luma = new Float32Array(n), hueX = new Float32Array(n), hueY = new Float32Array(n), sat = new Float32Array(n);
  for (let f = 0; f < n; f++) {
    let lx = 0, hx = 0, hy = 0, sa = 0;
    const a = f * fb;
    for (let p = 0; p < fb; p += 9) {           // every 3rd pixel
      const R = raw[a + p], G = raw[a + p + 1], B = raw[a + p + 2];
      lx += (R + G + B) / 3;
      // opponent axes → chroma vector; magnitude weights the hue mean
      const ox = R - (G + B) / 2, oy = (G - B) * .866;
      hx += ox; hy += oy; sa += Math.hypot(ox, oy);
    }
    const m = fb / 9;
    luma[f] = lx / m / 255;
    hueX[f] = hx / m; hueY[f] = hy / m; sat[f] = sa / m / 255;
  }
  return { luma, hueX, hueY, sat, n };
}
const hueOf = (c, f) => Math.atan2(c.hueY[f], c.hueX[f]);
const hueDist = (a, b) => { let d = Math.abs(a - b) % (Math.PI * 2); return d > Math.PI ? Math.PI * 2 - d : d; };

// Mean luminance per frame — the darkness/brightness arc of a take.
function lumaCurve(path) {
  const AW = 160, AH = 90;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", path, "-f", "rawvideo", "-pix_fmt", "gray", "-s", `${AW}x${AH}`,
    "-r", String(FPS), "pipe:1"], { maxBuffer: 1 << 30 });
  if (r.status !== 0 || !r.stdout?.length) return null;
  const raw = r.stdout, fb = AW * AH, n = Math.floor(raw.length / fb);
  const curve = new Float32Array(n);
  for (let f = 0; f < n; f++) {
    let acc = 0;
    const a = f * fb;
    for (let p = 0; p < fb; p += 4) acc += raw[a + p];
    curve[f] = acc / (fb / 4) / 255;
  }
  return curve;
}
const impacts = (curve) => {
  const d = new Float32Array(curve.length);
  for (let i = 1; i < curve.length; i++) d[i] = Math.max(0, curve[i] - curve[i - 1]);
  return d;
};

const struct = JSON.parse(readFileSync(`${OUT}/reel/${SLUG}-reel.struct.json`, "utf8"));
const feed = JSON.parse(readFileSync(`${OUT}/${SLUG}.events.json`, "utf8"));
const shots = JSON.parse(readFileSync(`${MOTION_DIR}/shots.json`, "utf8"));
const takes = existsSync(`${MOTION_DIR}/takes.json`)
  ? JSON.parse(readFileSync(`${MOTION_DIR}/takes.json`, "utf8")) : {};

const clipSeconds = (p) => Number(spawnSync("ffprobe", ["-v", "error",
  "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", p],
  { encoding: "utf8" }).stdout?.trim()) || 0;

// ── chart mode — motion energy of a finished cut vs the score ────────────
if (flags.chart) {
  const video = flags.chart === true ? `${OUT}/${SLUG}-shakeout-reel-final.mp4` : flags.chart;
  const curve = motionCurve(video);
  if (!curve) { console.error("✗ could not read " + video); process.exit(1); }
  const N = curve.length;
  // the score's energy at each frame: decaying envelope over all hits
  const DECAY = { boom: .2, snare: .16, donk: .13, sub: .3, hat: .07, bell: .09, riser: .5 };
  const music = new Float32Array(N);
  for (const e of feed.events) {
    const d = DECAY[e.i]; if (!d) continue;
    const f0 = Math.max(0, Math.floor((e.t - FROM) * FPS));
    const f1 = Math.min(N, Math.ceil((e.t - FROM + d * 3) * FPS));
    for (let f = f0; f < f1; f++) {
      music[f] += Math.exp(-Math.max(0, f / FPS + FROM - e.t) / d) * (e.gain ?? .1);
    }
  }
  const kicks = feed.events.filter((e) => e.i === "boom")
    .map((e) => e.t - FROM).filter((t) => t >= 0 && t <= N / FPS);

  // Two stacked panels sharing one x — never a dual axis. Palette
  // validated: #d97706 (music) / #2563eb (motion) on white.
  const CW = 1400, CH = 900, PAD = 56, PH = 210, GAP = 56;
  const c = createCanvas(CW, CH), x = c.getContext("2d");
  x.fillStyle = "#ffffff"; x.fillRect(0, 0, CW, CH);
  const plotW = CW - PAD * 2;
  const tx = (f) => PAD + (f / N) * plotW;
  const luma = lumaCurve(video);
  const panels = [
    { y0: PAD, data: music, col: "#d97706", label: "music energy (events.json)" },
    { y0: PAD + PH + GAP, data: curve, col: "#2563eb", label: "frame motion (mean |Δ|)" },
    { y0: PAD + (PH + GAP) * 2, data: luma, col: "#7c3aed", label: "frame brightness (mean luma) — the lights arc" },
  ];
  x.font = "13px sans-serif";
  for (const p of panels) {
    const max = Math.max(...p.data) || 1;
    // section bands + boundaries (recessive)
    for (const s of struct.sections) {
      x.fillStyle = "rgba(0,0,0,.03)";
      if (struct.sections.indexOf(s) % 2) x.fillRect(tx(s.startSec * FPS), p.y0, (s.endSec - s.startSec) * FPS / N * plotW, PH);
      x.fillStyle = "#6b7280";
      x.fillText(s.name, tx(s.startSec * FPS) + 4, p.y0 + 14);
    }
    // kick downbeat ticks
    x.strokeStyle = "rgba(0,0,0,.16)";
    for (const k of kicks) {
      x.beginPath(); x.moveTo(tx(k * FPS), p.y0 + PH - 26); x.lineTo(tx(k * FPS), p.y0 + PH); x.stroke();
    }
    // the curve
    x.strokeStyle = p.col; x.lineWidth = 2; x.beginPath();
    for (let f = 0; f < N; f++) {
      const yy = p.y0 + PH - (p.data[f] / max) * (PH - 24);
      f ? x.lineTo(tx(f), yy) : x.moveTo(tx(f), yy);
    }
    x.stroke();
    x.fillStyle = "#111827";
    x.fillText(p.label, PAD, p.y0 - 8);
    x.lineWidth = 1;
  }
  // shared x axis: seconds
  x.fillStyle = "#6b7280";
  for (let sec = 0; sec <= N / FPS; sec += 5) {
    x.fillText(`${sec}s`, tx(sec * FPS) - 8, CH - 18);
  }
  x.fillStyle = "#111827"; x.font = "15px sans-serif";
  x.fillText("SHAKEOUT reel — where the energy is (ticks = kicks from the score)", PAD, 24);
  const chartPath = `${MOTION_DIR}/energy-chart.png`;
  writeFileSync(chartPath, c.toBuffer("image/png"));
  console.log(`✓ ${chartPath}`);
  if (flags.open) spawnSync("open", [chartPath]);
  process.exit(0);
}


// ── remix mode — bar-locked recut of the whole loud half ─────────────────
// The takes are a LIBRARY. The narrative half (find → the ignition) keeps
// its story cut, with the ignition boundary solve; everything after the
// blast is REMIXED on the bar grid: a hard cut every bar, each bar's clip
// chosen so (1) its brightness matches the bar's musical energy, (2) its
// dominant COLOUR changes from the previous bar — colour shifts land on
// bar lines, (3) its motion impacts sit on the bar's kicks, and (4) reuse
// is penalized so the edit keeps moving. Fast, musical, deterministic.
if (flags.remix) {
  const BAR = 60 / 144 * 4, BEAT = 60 / 144;
  const takeFiles = {};
  for (const s of shots) {
    const pth = `${MOTION_DIR}/${SLUG}-reel-shot-${s.i}-${s.name}.mp4`;
    if (existsSync(pth)) takeFiles[s.name] = pth;
  }
  console.log(`▸ remix: analysing ${Object.keys(takeFiles).length} takes …`);
  const lib = {};
  for (const [name, pth] of Object.entries(takeFiles)) {
    const c = colorCurve(pth);
    if (c) { lib[name] = { path: pth, c, len: c.n / FPS }; console.log(`  · ${name} ${(c.n / FPS).toFixed(1)}s`); }
  }
  // music energy per frame across the reel
  const DECAY2 = { boom: .2, snare: .16, donk: .13, sub: .3, hat: .07, bell: .09, riser: .5 };
  const NREEL = Math.ceil((struct.totalSec) * FPS);
  const music = new Float32Array(NREEL);
  for (const e of feed.events) {
    const d = DECAY2[e.i]; if (!d) continue;
    const f0 = Math.max(0, Math.floor((e.t - FROM) * FPS));
    const f1 = Math.min(NREEL, Math.ceil((e.t - FROM + d * 3) * FPS));
    for (let f = f0; f < f1; f++) music[f] += Math.exp(-Math.max(0, f / FPS + FROM - e.t) / d) * (e.gain ?? .1);
  }

  // narrative head: find / thread / coil(shortened) / ignite(full take)
  const ignSec = struct.sections.find((x) => x.name === "ignite");
  const coilSec = struct.sections.find((x) => x.name === "coil");
  const ign = lib.ignite;
  let tIgn = 0;
  { let best = 0; for (let f = 1; f < Math.min(ign.c.n, 3 * FPS); f++) {
      const d = ign.c.luma[f] - ign.c.luma[f - 1];
      if (d > best) { best = d; tIgn = f / FPS; } } }
  const segs = [];
  const secOf = (n) => struct.sections.find((x) => x.name === n);
  for (const n of ["find", "thread"]) {
    const sc = secOf(n);
    segs.push({ src: lib[n].path, ss: 0, dur: sc.endSec - sc.startSec, why: n });
  }
  const coilDur = (coilSec.endSec - coilSec.startSec) - tIgn;
  segs.push({ src: lib.coil.path, ss: 0, dur: coilDur, why: "coil" });
  const ignDur = Math.min(ign.len, ignSec.endSec - (coilSec.startSec + coilDur) + 0);
  segs.push({ src: ign.path, ss: 0, dur: ign.len, why: `ignite (blast ON the drop @${tIgn.toFixed(2)}s)` });
  let cursor = coilSec.startSec + coilDur + ign.len;

  // remix tail: bar grid to the end
  const remixPool = ["hammer", "orbit", "ignite", "coil", "thread", "find"].filter((n) => lib[n]);
  const usage = Object.fromEntries(remixPool.map((n) => [n, []]));
  const END = struct.totalSec;
  // bar-mean energy normalized over the remix region → target luma
  const barsList = [];
  for (let b = cursor; b < END - .01; b += BAR) barsList.push([b, Math.min(END, b + BAR)]);
  // final bar = the oomph beat: cut it as its own segment later
  const barEnergy = barsList.map(([a, b]) => {
    let e = 0, k = 0;
    for (let f = Math.floor(a * FPS); f < Math.min(NREEL, Math.ceil(b * FPS)); f++) { e += music[f]; k++; }
    return k ? e / k : 0;
  });
  const eMin = Math.min(...barEnergy), eMax = Math.max(...barEnergy);
  let prevHue = hueOf(ign.c, ign.c.n - 1), prevName = "ignite";
  barsList.forEach(([a, b], bi) => {
    const dur = b - a;
    const target = (barEnergy[bi] - eMin) / (eMax - eMin + 1e-6);   // 0..1
    // kicks inside this bar, bar-local
    const kicks = feed.events.filter((e) => e.i === "boom" && e.t - FROM >= a && e.t - FROM < b)
      .map((e) => e.t - FROM - a);
    let best = null;
    for (const name of remixPool) {
      const L = lib[name];
      const maxStart = L.len - dur - .05;
      if (maxStart < 0) continue;
      for (let ss = 0; ss <= maxStart; ss += BEAT / 2) {
        const f0 = Math.floor(ss * FPS), f1 = Math.min(L.c.n, Math.ceil((ss + dur) * FPS));
        let lu = 0, k = 0; for (let f = f0; f < f1; f++) { lu += L.c.luma[f]; k++; }
        lu /= Math.max(1, k);
        const hue = hueOf(L.c, Math.min(L.c.n - 1, Math.floor((ss + dur / 2) * FPS)));
        let imp = 0;
        for (const kt of kicks) {
          const f = f0 + Math.round(kt * FPS);
          const d = (L.c.luma[f] ?? 0) - (L.c.luma[f - 1] ?? 0);
          imp += Math.max(0, d);
        }
        const reuse = usage[name].reduce((acc, [ua, ub]) => acc + Math.max(0, Math.min(ub, ss + dur) - Math.max(ua, ss)), 0);
        const score =
          -Math.abs(lu - (0.12 + 0.55 * target)) * 3      // brightness ↔ energy
          + hueDist(hue, prevHue) * .55                    // colour change on the bar line
          + imp * 8                                        // impacts on kicks
          - reuse * .6                                     // keep it moving
          + (name === prevName ? -.35 : 0);                // prefer a switch
        if (!best || score > best.score) best = { score, name, ss, hue };
      }
    }
    if (!best) return;
    usage[best.name].push([best.ss, best.ss + dur]);
    segs.push({ src: lib[best.name].path, ss: best.ss, dur, why: `bar ${bi} ← ${best.name}@${best.ss.toFixed(2)} (E ${target.toFixed(2)})` });
    prevHue = best.hue; prevName = best.name;
    cursor = b;
  });
  for (const g of segs) console.log(`  ✂ ${g.dur.toFixed(2)}s  ${g.why}`);
  const total = segs.reduce((a, g) => a + g.dur, 0);
  console.log(`  Σ picture ${total.toFixed(2)}s vs audio ${struct.totalSec.toFixed(2)}s`);

  // render segments + hard-concat + mux (hard cuts ARE the fast edits)
  const segPaths = [];
  segs.forEach((g, i) => {
    const o = `${MOTION_DIR}/rmx-${String(i).padStart(2, "0")}.mp4`;
    const r = spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
      "-ss", g.ss.toFixed(3), "-i", g.src, "-t", g.dur.toFixed(3),
      "-vf", "scale=720:1280,fps=24", "-an",
      "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p", o]);
    if (r.status !== 0) { console.error(`✗ seg ${i} failed`); process.exit(1); }
    segPaths.push(o);
  });
  const listPath = `${MOTION_DIR}/rmx-concat.txt`;
  writeFileSync(listPath, segPaths.map((t) => `file '${t}'`).join("\n") + "\n");
  const outPath = `${OUT}/${SLUG}-shakeout-reel.mp4`;
  const mux = spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
    "-f", "concat", "-safe", "0", "-i", listPath, "-i", `${OUT}/reel/${SLUG}-reel.mp3`,
    "-map", "0:v", "-map", "1:a", "-c:v", "copy", "-c:a", "aac", "-b:a", "256k",
    "-shortest", "-movflags", "+faststart", outPath]);
  if (mux.status !== 0) { console.error("✗ remix mux failed"); process.exit(1); }
  console.log(`✓ ${outPath} (remix cut)`);
  process.exit(0);
}

// ── align mode — pick each cut-shot's head offset inside its slack ───────
const offsets = {};
for (const s of shots) {
  const sec = struct.sections.find((x) => x.name === s.name);
  if (!sec) continue;
  const shotPath = `${MOTION_DIR}/${SLUG}-reel-shot-${s.i}-${s.name}.mp4`;
  const picked = takes[s.name] ? resolve(MOTION_DIR, takes[s.name]) : shotPath;
  if (!existsSync(picked)) { console.log(`  ○ ${s.name}: no take`); continue; }
  if (s.endImage) { console.log(`  ○ ${s.name}: morph — keeps its arrival`); continue; }
  const clipLen = clipSeconds(picked);
  const need = (sec.endSec - sec.startSec) + XF;
  const slack = clipLen - need;
  if (slack < 1 / FPS) { console.log(`  ○ ${s.name}: no slack (${slack.toFixed(2)}s)`); continue; }
  const curve = motionCurve(picked);
  if (!curve) { console.log(`  ○ ${s.name}: unreadable`); continue; }
  const imp = impacts(curve);
  // the section's kicks, in section-local time, weighted by their gain —
  // downbeats (bar starts) hit harder in the score, so they weigh more.
  const kicks = feed.events.filter((e) => e.i === "boom" &&
    e.t >= FROM + sec.startSec && e.t < FROM + sec.endSec)
    .map((e) => ({ t: e.t - FROM - sec.startSec, g: e.gain ?? .15 }));
  if (!kicks.length) { console.log(`  ○ ${s.name}: no kicks in section`); continue; }
  let best = 0, bestScore = -1;
  const steps = Math.floor(slack * FPS);
  for (let o = 0; o <= steps; o++) {
    const off = o / FPS;
    let score = 0;
    for (const k of kicks) {
      const f = Math.round((k.t + off) * FPS);
      // a little tolerance: the impact may land a frame either side
      const v = Math.max(imp[f] ?? 0, imp[f - 1] ?? 0, imp[f + 1] ?? 0);
      score += v * k.g;
    }
    if (score > bestScore) { bestScore = score; best = off; }
  }
  offsets[s.name] = best;
  console.log(`  ✓ ${s.name}: offset ${best.toFixed(3)}s of ${slack.toFixed(2)}s slack (${kicks.length} kicks)`);
}
// ── the ignition constraint — brightness alignment, @jeffrey 2026-08-13 ──
// "when he jumps up it should be like when he lights up, not in the dark."
// The ignite take turns its lights on T_ign seconds into the clip, but the
// struct starts the clip AT the drop — so the blast lands T_ign late. The
// cut is editorial: pull the coil→ignite boundary EARLIER by T_ign so his
// dark launch plays over the end of the buildup and the frame BLASTS
// BRIGHT exactly on the drop kick. The lost time cascades forward into
// `run`, which is a Ken Burns fallback and can be any length.
{
  const ignPath = `${MOTION_DIR}/${SLUG}-reel-shot-3-ignite.mp4`;
  const ignSec = struct.sections.find((x) => x.name === "ignite");
  const coilSec = struct.sections.find((x) => x.name === "coil");
  const hamSec = struct.sections.find((x) => x.name === "hammer");
  const runSec = struct.sections.find((x) => x.name === "run");
  if (existsSync(ignPath) && ignSec && coilSec && hamSec && runSec) {
    const luma = lumaCurve(ignPath);
    // the ignition = the biggest brightness JUMP in the first 3 seconds
    let tIgn = 0, best = 0;
    for (let f = 1; f < Math.min(luma.length, 3 * FPS); f++) {
      const d = luma[f] - luma[f - 1];
      if (d > best) { best = d; tIgn = f / FPS; }
    }
    console.log(`  ☀ ignite: lights-on at ${tIgn.toFixed(2)}s into the take (jump ${best.toFixed(3)})`);
    if (tIgn > 2 / FPS) {
      const ignLen = clipSeconds(ignPath);
      const hamPath = `${MOTION_DIR}/${SLUG}-reel-shot-4-hammer.mp4`;
      const hamLen = existsSync(hamPath) ? clipSeconds(hamPath) : 0;
      // coil gives up tIgn; ignite and hammer run their full takes; run
      // (Ken Burns) absorbs whatever is left up to its fixed end.
      const coilExact = (coilSec.endSec - coilSec.startSec) - tIgn;
      const ignExact = ignLen - XF;
      const hamExact = hamLen ? hamLen - XF : (hamSec.endSec - hamSec.startSec);
      const runEnd = runSec.endSec;
      const runStart = coilSec.startSec + coilExact + ignExact + hamExact;
      const runExact = runEnd - runStart;
      if (coilExact > 1 && runExact > 1) {
        offsets["coil"] = { off: offsets["coil"] ?? 0, exact: coilExact };
        offsets["ignite"] = { off: 0, exact: ignExact };
        offsets["hammer"] = { off: 0, exact: hamExact };
        offsets["run"] = { off: 0, exact: runExact };
        console.log(`  ☀ boundaries: coil ${coilExact.toFixed(2)}s → ignite ${ignExact.toFixed(2)}s (blast ON the drop) → hammer ${hamExact.toFixed(2)}s → run ${runExact.toFixed(2)}s`);
      } else console.log(`  ☀ cascade infeasible (coil ${coilExact.toFixed(2)}s / run ${runExact.toFixed(2)}s) — skipped`);
    } else console.log("  ☀ ignition already at the head — no boundary change");
  }
}
writeFileSync(`${MOTION_DIR}/offsets.json`, JSON.stringify(offsets, null, 2));
console.log(`✓ ${MOTION_DIR}/offsets.json — re-run --assemble to apply`);
