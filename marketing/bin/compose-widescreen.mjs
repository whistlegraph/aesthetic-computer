#!/usr/bin/env node
// compose-widescreen.mjs — GENERIC 1920x1080 compositor for narrated
// application videos. Drives the whole picture from a campaign config:
// per-segment illustrations (with Ken Burns), top-left titles + slide-number
// superscripts, a top-right bouncing wordmark, the karaoke word-train caption
// layer, and a narration + ambient-bed audio mix.
//
// Prereq:  node recap/cli.mjs build <cfg.audience>   (writes recap/out/…)
// Usage:   node marketing/bin/compose-widescreen.mjs <campaign-dir>
//
// All text is pre-rendered to PNG via ImageMagick (YWFT renders correctly there,
// unlike node-canvas) and overlaid timed by ffmpeg.
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { execSync, spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const FONT = `${process.env.HOME}/Library/Fonts/ywft-processing-bold.ttf`;
const W = 1920, H = 1080, FPS = 30;

const campArg = process.argv[2];
if (!campArg) { console.error("usage: compose-widescreen.mjs <campaign-dir>"); process.exit(2); }
const campDir = resolve(process.cwd(), campArg);
const { default: cfg } = await import(pathToFileURL(`${campDir}/campaign.mjs`).href);

const TMP = `/tmp/wsvideo-${cfg.name}`; mkdirSync(TMP, { recursive: true });
const NARR = `${REPO}/recap/out/recap.mp3`;
const BED = cfg.bed;
const OUT = cfg.out;
const CREAM = cfg.palette?.cream || "#fcf7c5";
const GREEN = cfg.palette?.green || "#3dff88";
const SHADOW = cfg.palette?.shadow || "#000000";
const TITLES = cfg.titles || {};

const segs = JSON.parse(readFileSync(`${REPO}/recap/out/segments.json`, "utf8"));
const DURATION = Math.max(...segs.map((s) => s.endSec));
const EXTRACTS = cfg.extracts || {};               // paper-extract scenes
const hasIllo = (s) => existsSync(`${campDir}/${s.name}/gens/native.png`) || existsSync(`${campDir}/${s.name}/gens/pencil.png`);
const hasBg = (s) => hasIllo(s) || EXTRACTS[s.name];
const imgSegs = segs.filter(hasBg);                // segments with a background
const lastIx = imgSegs.length - 1;                 // last bg holds to the end (covers tail)

// --preview T0 DUR → render just a short window (with all chrome) for fast feedback
const pvI = process.argv.indexOf("--preview");
const PREVIEW = pvI >= 0 ? { t0: Number(process.argv[pvI + 1]), dur: Number(process.argv[pvI + 2]) } : null;

// --contact → one composite frame per scene, montaged into a contact sheet (a
// fast "read the whole video" before committing to a full render).
if (process.argv.includes("--contact")) {
  const sheetDir = `${TMP}/contact`; mkdirSync(sheetDir, { recursive: true });
  const self = fileURLToPath(import.meta.url);
  // spoken narration per segment (from the word timings, text-fixes applied)
  const words = JSON.parse(readFileSync(`${REPO}/recap/out/words.json`, "utf8"));
  const FIXc = (cfg.textFixes || []).map(([p, r, f]) => [new RegExp(p, f || ""), r]);
  const mmss = (t) => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;
  const spokenFor = (s, segEnd) => FIXc.reduce((a, [re, r]) => a.replace(re, r),
    words.filter((w) => (w.fromMs / 1000) >= s.startSec - 0.05 && (w.fromMs / 1000) < segEnd - 0.05)
      .map((w) => w.text).join(" ").replace(/\s+([,.;:!?])/g, "$1").replace(/^["“]/, "")).trim();
  const tiles = [];
  imgSegs.forEach((s, k) => {
    const segEnd = (k === imgSegs.length - 1) ? DURATION : s.endSec;
    const tMid = Math.min(segEnd - 0.2, Math.max(s.startSec + 0.6, (s.startSec + segEnd) / 2));
    console.log(`contact ${k + 1}/${imgSegs.length}: ${s.name} @ ${tMid.toFixed(1)}s`);
    const r = spawnSync("node", [self, campArg, "--preview", tMid.toFixed(2), "0.12"], { stdio: "inherit" });
    if (r.status !== 0) { console.error("✗ contact frame failed", s.name); return; }
    const raw = `${sheetDir}/raw_${String(k).padStart(2, "0")}.png`;
    execSync(`ffmpeg -y -i "${OUT.replace(/\.mp4$/, "-preview.mp4")}" -frames:v 1 "${raw}" 2>/dev/null`);
    // caption text (header + the spoken line) → write to file to avoid escaping
    const header = `${k + 1}.  ${TITLES[s.name] || s.name}   ·   ${mmss(s.startSec)}`;
    const capFile = `${sheetDir}/cap_${k}.txt`;
    writeFileSync(capFile, `${header}\n${spokenFor(s, segEnd) || "—"}`);
    // resize frame to tile width, render wrapped caption below, stack
    const tile = `${sheetDir}/tile_${String(k).padStart(2, "0")}.png`;
    execSync(`magick "${raw}" -resize 760x ` +
      `\\( -background "#0b0f0d" -fill "#e8ffe8" -font "${FONT}" -pointsize 23 -size 720x caption:@"${capFile}" -splice 20x14 -background "#0b0f0d" -gravity West \\) ` +
      `-background "#0b0f0d" -append "${tile}"`, { stdio: "ignore" });
    tiles.push(tile);
  });
  const sheet = OUT.replace(/\.mp4$/, "-contact.png");
  execSync(`magick montage ${tiles.map((t) => `"${t}"`).join(" ")} -tile 2x -geometry +12+12 -background "#0a0a0a" "${sheet}"`);
  console.log(`✓ contact sheet → ${sheet}`);
  execSync(`open "${sheet}"`);
  process.exit(0);
}

// Ken Burns motion: per-segment zoom + pan + subtle wobble (pop-style, varied).
// KW/KH sized just above peak zoom (1.13) so zoompan never upscales — the only
// scale is one sharp lanczos pass from the source.
const KW = 2208, KH = 1242;
const MOVES = [
  { z0: 1.00, z1: 1.10, dx: 70, dy: -34 },   // push in, drift up-right
  { z0: 1.10, z1: 1.00, dx: -66, dy: 26 },   // pull out, drift down-left
  { z0: 1.00, z1: 1.08, dx: -58, dy: 36 },   // push in, drift down-left
  { z0: 1.07, z1: 1.13, dx: 48, dy: 44 },    // slow push, diagonal down-right
];
function kenBurns(k, frames, phase = 0) {
  const m = MOVES[k % MOVES.length], f = Math.max(1, frames), ph = `(on+${phase})`;
  const z = `(${m.z0}+(${m.z1}-${m.z0})*${ph}/${f})`;
  const wob = `5*sin(2*PI*0.11*${ph}/${FPS})`;
  const x = `iw/2-(iw/zoom/2)+(${m.dx})*${ph}/${f}+${wob}`;
  const y = `ih/2-(ih/zoom/2)+(${m.dy})*${ph}/${f}+${wob}`;
  return `scale=${KW}:${KH}:force_original_aspect_ratio=increase:flags=lanczos,crop=${KW}:${KH},` +
    `zoompan=z='${z}':x='${x}':y='${y}':d=1:s=${W}x${H}:fps=${FPS},setsar=1,format=yuv420p`;
}

// ── ImageMagick text/dot PNG helpers (hard pop shadow) ───────────────
function textPng(out, text, { size, fill, width, oy = 6, align = "Center" }) {
  const ox = Math.round(oy * 0.85);
  const src = width
    ? ["-size", `${width}x`, "-background", "none", "-fill", fill, "-font", FONT, "-pointsize", String(size), "-gravity", align, `caption:${text}`]
    : ["-background", "none", "-fill", fill, "-font", FONT, "-pointsize", String(size), `label:${text}`];
  const r = spawnSync("magick", [...src, "(", "+clone", "-background", SHADOW, "-shadow", `100x0+${ox}+${oy}`, ")",
    "+swap", "-background", "none", "-layers", "merge", "+repage", out], { encoding: "utf8" });
  if (r.status !== 0) { console.error("magick failed:", text, r.stderr); process.exit(1); }
  const [w, h] = execSync(`magick identify -format "%w %h" ${out}`).toString().trim().split(" ").map(Number);
  return { w, h };
}
function dotPng(out, side) {   // small green square "period" with the pop shadow
  spawnSync("magick", ["-size", `${side + 8}x${side + 8}`, "xc:none", "-fill", GREEN, "-draw", `rectangle 0,0 ${side - 1},${side - 1}`,
    "(", "+clone", "-background", SHADOW, "-shadow", "100x0+3+4", ")", "+swap", "-background", "none", "-layers", "merge", "+repage", out], { encoding: "utf8" });
}
const idim = (p) => execSync(`magick identify -format "%w %h" ${p}`).toString().trim().split(" ").map(Number);

// ── 1) karaoke word-train caption layer (windowed under --preview) ───
console.log(`[${cfg.name}] rendering word-train caption layer…`);
{
  const capArgs = [`${REPO}/marketing/bin/captions-train.mjs`, campDir];
  if (PREVIEW) capArgs.push("--start", String(PREVIEW.t0), "--frames", String(Math.round(PREVIEW.dur * FPS)));
  const r = spawnSync("node", capArgs, { stdio: "inherit" });
  if (r.status !== 0) { console.error("✗ captions-train failed"); process.exit(1); }
}
const CAPTIONS_MOV = `${TMP}/captions.mov`;

// ── 2) title / number / wordmark PNGs ────────────────────────────────
console.log("rendering text pngs…");
const COLORS = cfg.colors || {};
const titleDims = {};
// titles + slide-number superscripts in the per-slide accent color (same as captions)
for (const s of segs) if (TITLES[s.name]) titleDims[s.name] = textPng(`${TMP}/title_${s.name}.png`, TITLES[s.name], { size: 84, fill: COLORS[s.name] || CREAM, oy: 7 });
// superscript number in a contrasting color (cream) against the accent title
segs.forEach((s, k) => { if (TITLES[s.name]) textPng(`${TMP}/num_${s.name}.png`, String(k + 1), { size: 38, fill: cfg.numColor || CREAM, oy: 4 }); });

// right-side vertical wordmark label: [Aesthetic] · bouncing green "." · [Computer]
const wmParts = cfg.wordmark || [];
let haveSide = false, sideEls = [], dotEl = null;
if (wmParts.length >= 2) {
  textPng(`${TMP}/wm_a.png`, wmParts[0], { size: 36, fill: CREAM, oy: 4 });
  textPng(`${TMP}/wm_c.png`, wmParts[1], { size: 36, fill: CREAM, oy: 4 });
  execSync(`magick ${TMP}/wm_a.png -rotate 90 ${TMP}/wm_a_s.png`);
  execSync(`magick ${TMP}/wm_c.png -rotate 90 ${TMP}/wm_c_s.png`);
  dotPng(`${TMP}/wm_dot.png`, 9);
  const [aw, ah] = idim(`${TMP}/wm_a_s.png`), [cw] = idim(`${TMP}/wm_c_s.png`), [dw, dh] = idim(`${TMP}/wm_dot.png`);
  const colW = Math.max(aw, cw, dw), rightX = W - colW - 30, topY = 60, gap = 5;
  const dotY = topY + ah + gap, cY = dotY + dh + gap;
  sideEls = [
    { png: `${TMP}/wm_a_s.png`, x: Math.round(rightX + (colW - aw) / 2), y: topY },
    { png: `${TMP}/wm_c_s.png`, x: Math.round(rightX + (colW - cw) / 2), y: Math.round(cY) },
  ];
  dotEl = { png: `${TMP}/wm_dot.png`, x: Math.round(rightX + (colW - dw) / 2), y: Math.round(dotY), amp: 11, f: 0.3 };
  haveSide = true;
}
// push the side-label overlays (words faint, dot bounces left) → [vout]
function sideOverlay(inputs, fc, cur, idx) {
  if (!haveSide) { fc.push(`[${cur}]copy[vout]`); return idx; }
  for (const el of sideEls) {
    inputs.push(`-i ${el.png}`);
    fc.push(`[${idx}:v]format=rgba,colorchannelmixer=aa=0.72[w${idx}]`);
    fc.push(`[${cur}][w${idx}]overlay=${el.x}:${el.y}[t${idx}]`); cur = `t${idx}`; idx++;
  }
  inputs.push(`-i ${dotEl.png}`);
  fc.push(`[${cur}][${idx}:v]overlay='${dotEl.x}-${dotEl.amp}*abs(sin(2*PI*${dotEl.f}*t))':${dotEl.y}[vout]`); idx++;
  return idx;
}

const TX = 70, TY = 94;             // title anchor (nudged down for a nicer safe zone)

// ── 2.5) PREVIEW: render only a short window (local timeline) for fast feedback ─
if (PREVIEW) {
  const { t0, dur } = PREVIEW, t1 = t0 + dur;
  const inputs = [], fc = [];
  const ov = imgSegs.map((s, k) => ({ s, k, segEnd: (k === lastIx ? DURATION : s.endSec) }))
    .filter(({ s, segEnd }) => segEnd > t0 && s.startSec < t1);
  ov.forEach(({ s, k, segEnd }, j) => {
    const subStart = Math.max(t0, s.startSec), subEnd = Math.min(t1, segEnd), subDur = subEnd - subStart;
    if (EXTRACTS[s.name]) {
      const ex = EXTRACTS[s.name], clip = `${TMP}/extract_${s.name}.mp4`;
      if (!existsSync(clip)) spawnSync("node", [`${REPO}/marketing/bin/paper-extract-scene.mjs`, "--pdf", resolve(REPO, ex.pdf), "--quote", ex.quote, "--out", clip, "--start", String(s.startSec), "--dur", (segEnd - s.startSec).toFixed(3), ...(ex.cite ? ["--cite", ex.cite] : [])], { stdio: "inherit" });
      inputs.push(`-ss ${(subStart - s.startSec).toFixed(3)} -t ${subDur.toFixed(3)} -i ${clip}`);
      fc.push(`[${j}:v]fps=${FPS},setsar=1,format=yuv420p[p${j}]`);
    } else {
      const img = existsSync(`${campDir}/${s.name}/gens/native.png`) ? `${campDir}/${s.name}/gens/native.png` : `${campDir}/${s.name}/gens/pencil.png`;
      inputs.push(`-framerate ${FPS} -loop 1 -t ${subDur.toFixed(3)} -i ${img}`);
      fc.push(`[${j}:v]${kenBurns(k, Math.round((segEnd - s.startSec) * FPS), Math.round((subStart - s.startSec) * FPS))}[p${j}]`);
    }
  });
  fc.push(ov.map((_, j) => `[p${j}]`).join("") + `concat=n=${ov.length}:v=1:a=0[base]`);
  let idx = ov.length, cur = "base";
  ov.forEach(({ s, segEnd }) => {
    if (!TITLES[s.name] || EXTRACTS[s.name]) return;
    const win = `between(t,${Math.max(0, s.startSec - t0).toFixed(2)},${Math.min(dur, segEnd - t0).toFixed(2)})`;
    inputs.push(`-i ${TMP}/title_${s.name}.png`);
    fc.push(`[${cur}][${idx}:v]overlay=${TX}:${TY}:enable='${win}'[t${idx}]`); cur = `t${idx}`; idx++;
    inputs.push(`-i ${TMP}/num_${s.name}.png`);
    fc.push(`[${cur}][${idx}:v]overlay=${TX + titleDims[s.name].w + 8}:${TY - 10}:enable='${win}'[t${idx}]`); cur = `t${idx}`; idx++;
  });
  inputs.push(`-i ${CAPTIONS_MOV}`);
  fc.push(`[${cur}][${idx}:v]overlay=0:0[t${idx}]`); cur = `t${idx}`; idx++;
  idx = sideOverlay(inputs, fc, cur, idx);
  inputs.push(`-ss ${t0} -t ${dur} -i ${NARR}`); const aN = idx++;
  if (BED && existsSync(BED)) { inputs.push(`-ss ${t0} -t ${dur} -i ${BED}`); const aB = idx++; fc.push(`[${aN}:a]volume=1.0[an]`); fc.push(`[${aB}:a]volume=0.32[ab]`); fc.push(`[an][ab]amix=inputs=2:duration=longest:dropout_transition=0[aout]`); }
  else fc.push(`[${aN}:a]volume=1.0[aout]`);
  const pOut = OUT.replace(/\.mp4$/, "-preview.mp4");
  const fcPath = `${TMP}/filter_preview.txt`; writeFileSync(fcPath, fc.join(";\n"));
  const cmd = `ffmpeg -y ${inputs.join(" ")} -filter_complex_script ${fcPath} -map "[vout]" -map "[aout]" -r ${FPS} -t ${dur.toFixed(2)} -c:v libx264 -pix_fmt yuv420p -crf 12 -preset medium -c:a aac -b:a 192k "${pOut}"`;
  writeFileSync(`${TMP}/cmd_preview.sh`, cmd);
  console.log(`PREVIEW [${t0}..${t1}s] → ${pOut}`);
  const rr = spawnSync("bash", ["-c", cmd], { stdio: "inherit" });
  console.log(rr.status === 0 ? `✓ PREVIEW ${pOut}` : `✗ ffmpeg exit ${rr.status}`);
  process.exit(rr.status === 0 ? 0 : 1);
}

// ── 3) ffmpeg graph ──────────────────────────────────────────────────
console.log("assembling ffmpeg graph…");
const inputs = [], fc = [];
imgSegs.forEach((s, k) => {
  const end = (k === lastIx) ? DURATION : s.endSec;
  const dur = end - s.startSec;
  // paper-extract scene: render the PDF-highlight clip and use it as the bg
  if (EXTRACTS[s.name]) {
    const ex = EXTRACTS[s.name];
    const clip = `${TMP}/extract_${s.name}.mp4`;
    const r = spawnSync("node", [`${REPO}/marketing/bin/paper-extract-scene.mjs`,
      "--pdf", resolve(REPO, ex.pdf), "--quote", ex.quote, "--out", clip,
      "--start", String(s.startSec), "--dur", dur.toFixed(3), ...(ex.cite ? ["--cite", ex.cite] : [])],
      { stdio: "inherit" });
    if (r.status !== 0) { console.error("✗ paper-extract failed"); process.exit(1); }
    inputs.push(`-i ${clip}`);
    fc.push(`[${k}:v]fps=${FPS},setsar=1,format=yuv420p[p${k}]`);   // normalize for concat
    return;
  }
  const img = existsSync(`${campDir}/${s.name}/gens/native.png`) ? `${campDir}/${s.name}/gens/native.png` : `${campDir}/${s.name}/gens/pencil.png`;
  inputs.push(`-framerate ${FPS} -loop 1 -t ${dur.toFixed(3)} -i ${img}`);
  fc.push(`[${k}:v]${kenBurns(k, Math.round(dur * FPS))}[p${k}]`);
});
fc.push(imgSegs.map((_, k) => `[p${k}]`).join("") + `concat=n=${imgSegs.length}:v=1:a=0[base]`);

let idx = imgSegs.length, cur = "base";
imgSegs.forEach((s, k) => {
  if (!TITLES[s.name] || EXTRACTS[s.name]) return;   // extract scenes carry their own citation
  const end = (k === lastIx) ? DURATION : s.endSec;
  const win = `between(t,${s.startSec.toFixed(2)},${end.toFixed(2)})`;
  inputs.push(`-i ${TMP}/title_${s.name}.png`);
  fc.push(`[${cur}][${idx}:v]overlay=${TX}:${TY}:enable='${win}'[t${idx}]`); cur = `t${idx}`; idx++;
  const nx = TX + titleDims[s.name].w + 8;
  inputs.push(`-i ${TMP}/num_${s.name}.png`);
  fc.push(`[${cur}][${idx}:v]overlay=${nx}:${TY - 10}:enable='${win}'[t${idx}]`); cur = `t${idx}`; idx++;
});
// word-train caption layer (full-frame transparent overlay)
inputs.push(`-i ${CAPTIONS_MOV}`);
fc.push(`[${cur}][${idx}:v]overlay=0:0[t${idx}]`); cur = `t${idx}`; idx++;
// right-side vertical wordmark label with bouncing dot
idx = sideOverlay(inputs, fc, cur, idx);

// audio: narration + ambient bed (quiet)
inputs.push(`-i ${NARR}`); const aNarr = idx++;
const hasBed = BED && existsSync(BED);
if (hasBed) { inputs.push(`-i ${BED}`); const aBed = idx++;
  fc.push(`[${aNarr}:a]volume=1.0[an]`); fc.push(`[${aBed}:a]volume=0.32[ab]`);
  fc.push(`[an][ab]amix=inputs=2:duration=longest:dropout_transition=0[aout]`);
} else { fc.push(`[${aNarr}:a]volume=1.0[aout]`); }

const fcPath = `${TMP}/filter.txt`;
writeFileSync(fcPath, fc.join(";\n"));
const cmd = `ffmpeg -y ${inputs.join(" ")} -filter_complex_script ${fcPath} ` +
  `-map "[vout]" -map "[aout]" -r ${FPS} -t ${DURATION.toFixed(2)} ` +
  `-c:v libx264 -pix_fmt yuv420p -crf 12 -preset slow -c:a aac -b:a 192k "${OUT}"`;
writeFileSync(`${TMP}/cmd.sh`, cmd);
console.log(`inputs: ${inputs.length}, overlays: ${idx} · running ffmpeg → ${OUT}`);
const r = spawnSync("bash", ["-c", cmd], { stdio: "inherit" });
console.log(r.status === 0 ? `✓ DONE ${OUT}` : `✗ ffmpeg exit ${r.status}`);

// ── opening title card: render the branded card → a short push-in clip → concat ahead ─
if (r.status === 0 && cfg.titleCard) {
  console.log("rendering animated opener…");
  const opener = `${TMP}/opener.mp4`;
  const ro = spawnSync("node", [`${REPO}/marketing/bin/opener-anim.mjs`, campArg], { stdio: "inherit" });
  if (ro.status === 0 && existsSync(opener)) {
    // re-encode the join (concat filter) → clean, continuous timestamps so every
    // player handles the boundary (stream-copy concat stalls QuickTime at the cut).
    const final = `${TMP}/final.mp4`;
    const rc = spawnSync("bash", ["-c",
      `ffmpeg -y -i "${opener}" -i "${OUT}" -filter_complex "[0:v][0:a][1:v][1:a]concat=n=2:v=1:a=1[v][a]" ` +
      `-map "[v]" -map "[a]" -r ${FPS} -c:v libx264 -pix_fmt yuv420p -crf 12 -preset medium -c:a aac -b:a 192k -movflags +faststart "${final}"`],
      { stdio: "inherit" });
    if (rc.status === 0) { execSync(`mv "${final}" "${OUT}"`); console.log(`✓ opener prepended → ${OUT}`); }
    else console.error("✗ opener concat failed");
  }
}
process.exit(r.status === 0 ? 0 : 1);
