#!/usr/bin/env node
// momboba/bin/assemble-listener.mjs — build the 10-minute BACKGROUND PLATE
// for the listener video from the 9 Seedance movement clips.
//
// Each Seedance clip is only ~5 s (the-dream 12 s). Each MOVEMENT, though,
// plays for up to ~3 minutes. The old build ping-ponged (forward+reversed) and
// stream-looped — but the reverse play read as lame (jas, 2026-06-15). So now
// each movement is cut from DIFFERENT CROPPINGS of its one clip:
//   1. split the movement into ~clip-length CROP SHOTS (≈ one forward play of
//      the clip each — no reverse, no mid-shot loop pop; if a shot outruns the
//      clip the last frame freezes while the crop keeps drifting, so it never
//      goes static),
//   2. each crop shot picks a distinct framing from MOVES (zoom + a slow
//      ken-burns pan across a different region of the felt) — establishing
//      wide, push into a corner, rise from the bottom, slow detail, etc.,
//   3. HARD-CUT between croppings AND between movements — the scene changes
//      exactly when the music's movement changes (cuts > morphs), and every
//      cut lands on a fresh angle so the looping clip never reads as a loop.
//      Output is a SILENT 1920×1080 ~600 s plate; chrome-listener.mjs then
//      draws the score + chrome over it and muxes the real audio.
//
// Picks: respects out/motion/takes.json (audition board) like the pipeline,
// falling back to the shot file, then the newest archived take.
//
// Usage:
//   node pop/momboba/bin/assemble-listener.mjs            # → out/listener-plate.mp4
//   node pop/momboba/bin/assemble-listener.mjs --out X.mp4

import { readFileSync, writeFileSync, existsSync, readdirSync, mkdirSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const OUT = `${LANE}/out`;
const MOTION = `${OUT}/motion`;
const rel = (p) => p.replace(REPO + "/", "");

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf(`--${k}`); return i >= 0 && args[i + 1] ? args[i + 1] : d; };
const PLATE = resolve(process.cwd(), opt("out", `${OUT}/listener-plate.mp4`));
const W = 1920, H = 1080, FPS = 30;

// movement order ↔ motion-shot order (1:1, same index as gen-motion-listener).
const SHOTS = ["arrival", "boba-sip", "galleries", "waterlily-awe",
  "first-sheep", "asleep-flock", "the-dream", "morning", "resolve"];

// total runtime comes from the score data (authoritative timing); --dur caps it
// for short examples. --xfade sets the crossfade-between-scenes seconds.
const S = JSON.parse(readFileSync(`${OUT}/momabobasheep.scorodeon.json`, "utf8"));
const TOTAL = opt("dur") ? parseFloat(opt("dur")) : S.dur;   // 600 (or a short example)
const XF = parseFloat(opt("xfade", "0.8"));          // crossfade between scenes (0 = hard cut)
const TRIM = parseFloat(opt("trim", "0.6"));         // drop each clip's jumpy tail (no end-loop snap)
const SLOMO = parseFloat(opt("slomo", "1.35"));      // slow the shots (>1 = slower, dreamier)
const SHOTFPS = parseFloat(opt("shotfps", "12"));    // decimate shot motion → stop-motion stutter

const takesPath = `${MOTION}/takes.json`;
const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};

function clipFor(i, name) {
  if (takes[name]) { const p = resolve(MOTION, takes[name]); if (existsSync(p)) return p; }
  const shot = `${MOTION}/momabobasheep-listener-shot-${i}-${name}.mp4`;
  if (existsSync(shot)) return shot;
  const arch = `${MOTION}/archive`;
  const base = `momabobasheep-listener-shot-${i}-${name}.v`;
  const vs = (existsSync(arch) ? readdirSync(arch) : [])
    .filter((f) => f.startsWith(base))
    .sort((a, b) => Number(b.match(/\.v(\d+)\.mp4$/)?.[1] ?? 0) - Number(a.match(/\.v(\d+)\.mp4$/)?.[1] ?? 0));
  return vs.length ? `${arch}/${vs[0]}` : null;
}

const run = (a) => {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...a],
    { stdio: ["ignore", "ignore", "pipe"] });
  if (r.status !== 0) { console.error(`✗ ffmpeg:\n${r.stderr?.toString().slice(-500)}`); process.exit(1); }
};

const TMP = `${MOTION}/plate-tmp`;
mkdirSync(TMP, { recursive: true });

// crop framings — each shot rides a different region of the felt at a slow
// drift (z = zoom; f* = start pan anchor 0..1, t* = end anchor; 0=left/top).
// Cycled with a per-movement offset so adjacent movements never open the same.
const MOVES = [
  { z: 1.08, fx: 0.50, fy: 0.52, tx: 0.50, ty: 0.44 }, // establishing, tiny rise
  { z: 1.34, fx: 0.22, fy: 0.30, tx: 0.36, ty: 0.42 }, // drift into upper-left
  { z: 1.50, fx: 0.74, fy: 0.58, tx: 0.60, ty: 0.50 }, // ease in from the right
  { z: 1.26, fx: 0.50, fy: 0.76, tx: 0.50, ty: 0.56 }, // rise from bottom-center
  { z: 1.62, fx: 0.46, fy: 0.42, tx: 0.54, ty: 0.48 }, // slow detail push
  { z: 1.30, fx: 0.72, fy: 0.22, tx: 0.54, ty: 0.36 }, // come down from top-right
  { z: 1.44, fx: 0.28, fy: 0.64, tx: 0.42, ty: 0.50 }, // up out of lower-left
];

// one forward play of the clip ≈ one crop shot (no reverse, no loop pop).
function clipDur(p) {
  const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", p], { encoding: "utf8" });
  const d = Number(r.stdout?.trim());
  return Number.isFinite(d) && d > 0 ? d : 5;
}

// gather clips. --clips <comma-list> (ShotWizard driver) wins; else scan the
// movement-order shot files. Each carries its real duration.
const clips = [];
const clipsFlag = opt("clips", null);
if (clipsFlag) {
  for (const p of clipsFlag.split(",").map((s) => s.trim()).filter(Boolean)) {
    if (!existsSync(p)) { console.error(`✗ clip missing: ${rel(p)}`); continue; }
    clips.push({ name: p.split("/").pop().replace(/\.mp4$/, ""), path: p, dur: clipDur(p) });
  }
} else {
  for (let i = 0; i < SHOTS.length; i++) {
    const p = clipFor(i, SHOTS[i]);
    if (p) clips.push({ name: SHOTS[i], path: p, dur: clipDur(p) });
  }
}
if (!clips.length) { console.error("✗ no clips found"); process.exit(1); }

// CYCLE through the clips for the whole track (jas, 2026-06-16): each segment
// is one play of the NEXT clip in the cycle — so a clip never repeats back to
// back — under a fresh crop each time it comes round (MOVES advances on its own
// stride; gcd(MOVES, clips)=1 → every clip sees every crop before any repeat).
// No string/track/warp — just the clips, cropped, cycling.
let placed = 0, ci = 0, si = 0;                          // placed = EFFECTIVE output dur
const segs = [], durs = [];
while (placed < TOTAL - 0.05) {
  const clip = clips[ci % clips.length]; ci++;
  const mv = MOVES[si % MOVES.length]; si++;
  const first = segs.length === 0;
  const usable = Math.max(1.0, clip.dur - TRIM);          // drop the jumpy tail
  let shotDur = usable * SLOMO;                            // then slow it down
  const contrib = first ? shotDur : shotDur - XF;         // effective added length
  if (placed + contrib > TOTAL) shotDur = TOTAL - placed + (first ? 0 : XF);
  if (shotDur < XF + 0.3) break;
  const SW = Math.round(W * mv.z), SH = Math.round(H * mv.z);
  const rx = SW - W, ry = SH - H;                          // pan range at this zoom
  const D = shotDur.toFixed(3);
  const xE = `${rx}*(${mv.fx}+(${mv.tx}-${mv.fx})*t/${D})`;
  const yE = `${ry}*(${mv.fy}+(${mv.ty}-${mv.fy})*t/${D})`;
  const seg = `${TMP}/seg-${String(segs.length).padStart(3, "0")}-${clip.name}.mp4`;
  // trim the jumpy tail, SLOW the motion (setpts), drop to FPS, freeze-pad if
  // the shot still outruns it, then pan a 1080p crop window across.
  run(["-i", clip.path, "-t", D, "-vf",
    `trim=0:${usable.toFixed(3)},setpts=PTS*${SLOMO},fps=${SHOTFPS},` +
    `tpad=stop_mode=clone:stop_duration=60,` +
    `scale=${SW}:${SH}:force_original_aspect_ratio=increase,crop=${SW}:${SH},setsar=1,` +
    `crop=${W}:${H}:x=${xE}:y=${yE}`,
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p", "-r", String(FPS), "-an", seg]);
  segs.push(seg); durs.push(shotDur);
  placed += first ? shotDur : shotDur - XF;
}
console.log(`  ${segs.length} crop-shots cycling ${clips.length} clips → ${placed.toFixed(1)}s (xfade ${XF}s)`);

if (XF > 0 && segs.length > 1) {
  // chain an xfade dissolve between every consecutive crop-shot (smooth
  // crossfade between scenes). offset accumulates as the output grows.
  const inputs = segs.flatMap((s) => ["-i", s]);
  const filter = [];
  let prev = "[0:v]", outDur = durs[0];
  for (let i = 1; i < segs.length; i++) {
    const off = Math.max(0, outDur - XF);
    filter.push(`${prev}[${i}:v]xfade=transition=fade:duration=${XF}:offset=${off.toFixed(3)}[v${i}]`);
    prev = `[v${i}]`;
    outDur = outDur + durs[i] - XF;
  }
  console.log(`  crossfading ${segs.length} segments …`);
  run([...inputs, "-filter_complex", filter.join(";"), "-map", prev,
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p", "-r", String(FPS),
    "-movflags", "+faststart", PLATE]);
} else {
  const listPath = `${TMP}/concat.txt`;
  writeFileSync(listPath, segs.map((s) => `file '${s}'`).join("\n") + "\n");
  console.log(`  concatenating ${segs.length} segments (hard cuts) …`);
  run(["-f", "concat", "-safe", "0", "-i", listPath, "-c", "copy", "-movflags", "+faststart", PLATE]);
}

rmSync(TMP, { recursive: true, force: true });
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", PLATE], { encoding: "utf8" });
console.log(`✓ ${rel(PLATE)} · ${Number(probe.stdout?.trim()).toFixed(1)}s · ${W}x${H}@${FPS} silent`);
