#!/usr/bin/env node
// serp-comp.mjs — widescreen (1920x1080) compositor for the Serpentine video.
// No libass/drawtext in ffmpeg -> all text is pre-rendered PNG (ImageMagick,
// YWFT Processing, sharp /pop-style -shadow) and overlaid timed by ffmpeg.
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { execSync, spawnSync } from "node:child_process";

const REPO = "/Users/jas/aesthetic-computer";
const CAMP = `${REPO}/marketing/campaigns/serpentine-video`;
const OUTDIR = "/tmp/serp-comp"; mkdirSync(OUTDIR, { recursive: true });
const FONT = `${process.env.HOME}/Library/Fonts/ywft-processing-bold.ttf`;
const NARR = `${REPO}/recap/out/recap.mp3`;
const BED = `${process.env.HOME}/Documents/Shelf/serpentine-ambient-bed.mp3`;
const W = 1920, H = 1080, FPS = 30;
const CREAM = "#fcf7c5", GREEN = "#3dff88", SHADOW = "#000000";

const segs = JSON.parse(readFileSync(`${REPO}/recap/out/segments.json`, "utf8"));
const subsRaw = JSON.parse(readFileSync(`${REPO}/recap/out/subs.json`, "utf8"));
const DURATION = Math.max(...segs.map(s => s.endSec));

const TITLES = {
  "01_title": "A Computer of Your Own", "02_problem": "The Condition",
  "03_os_layer": "The Need", "04_creative_os": "The Creative OS",
  "05_commons": "The Commons", "06_llms": "What LLMs Change",
  "07_poised": "Why Aesthetic.Computer", "08_plan": "The Six Months",
  "09_outro": "Art x Convergence",
};
const FIXES = [[/aesthetic\.?\s?computer/gi,"Aesthetic.Computer"],[/twenty[ -]?twenty[ -]?six/gi,"2026"],[/Scutter/g,"Scudder"],[/Allen/g,"Alan"],[/[Kk]id ?[Ll]isp/g,"KidLisp"],[/[Tt]hink ?[Pp]ad/g,"ThinkPad"],[/\bAC ?OS\b/gi,"AC Native OS"]];
const fix = t => FIXES.reduce((s,[a,b])=>s.replace(a,b), t);

// ── ImageMagick text PNG with sharp /pop shadow (-shadow 100x0+ox+oy) ──
function textPng(out, text, { size, fill, width, oy = 6, align = "Center" }) {
  const ox = Math.round(oy * 0.85);
  const src = width
    ? ["-size", `${width}x`, "-background", "none", "-fill", fill, "-font", FONT,
       "-pointsize", String(size), "-gravity", align, `caption:${text}`]
    : ["-background", "none", "-fill", fill, "-font", FONT, "-pointsize", String(size),
       `label:${text}`];
  // render text, then composite a hard black shadow behind it
  const args = [...src,
    "(", "+clone", "-background", SHADOW, "-shadow", `100x0+${ox}+${oy}`, ")",
    "+swap", "-background", "none", "-layers", "merge", "+repage", out];
  const r = spawnSync("magick", args, { encoding: "utf8" });
  if (r.status !== 0) { console.error("magick failed:", text, r.stderr); process.exit(1); }
  // return rendered size
  const dim = execSync(`magick identify -format "%w %h" ${out}`).toString().trim().split(" ").map(Number);
  return { w: dim[0], h: dim[1] };
}
function dotPng(out, side) {
  // small green SQUARE (pixel-period feel) with the sharp /pop shadow
  spawnSync("magick", ["-size", `${side + 8}x${side + 8}`, "xc:none", "-fill", GREEN,
    "-draw", `rectangle 0,0 ${side - 1},${side - 1}`,
    "(", "+clone", "-background", SHADOW, "-shadow", "100x0+3+4", ")",
    "+swap", "-background", "none", "-layers", "merge", "+repage", out], { encoding: "utf8" });
}

console.log("rendering text pngs…");
// titles (top-left)
for (const s of segs) { if (TITLES[s.name]) textPng(`${OUTDIR}/title_${s.name}.png`, TITLES[s.name], { size: 84, fill: CREAM, oy: 7 }); }
// captions (bottom center, wrapped)
const caps = subsRaw.map((c, i) => {
  const txt = fix(c.text);
  textPng(`${OUTDIR}/cap_${i}.png`, txt, { size: 58, fill: CREAM, width: 1500, oy: 6, align: "Center" });
  return { i, start: c.startSec, end: c.endSec };
});
// wordmark parts + dot
const wmSize = 44;
const aDim = textPng(`${OUTDIR}/wm_a.png`, "Aesthetic", { size: wmSize, fill: CREAM, oy: 4 });
const cDim = textPng(`${OUTDIR}/wm_c.png`, "Computer", { size: wmSize, fill: CREAM, oy: 4 });
const dotSide = Math.round(wmSize * 0.22);
dotPng(`${OUTDIR}/wm_dot.png`, dotSide);
const dotImgW = parseInt(execSync(`magick identify -format "%w" ${OUTDIR}/wm_dot.png`).toString().trim(), 10);
// layout wordmark top-right: [Aesthetic][gap][dot][gap][Computer], snug
const gap = Math.round(wmSize * 0.07);
const wmTotal = aDim.w + gap + dotImgW + gap + cDim.w;
const wmX = W - 68 - wmTotal;
const wmY = 60;
const aX = wmX, dotX = aX + aDim.w + gap, cX = dotX + dotImgW + gap;
// dot sits low like a period; bounces upward
const dotRestY = wmY + Math.round(wmSize * 0.66);
const bounceAmp = 18, bounceF = 1.3;

// ── build ffmpeg ──
console.log("assembling ffmpeg graph…");
const inputs = [];
const fc = [];
// 9 pencil stills, each looped for its segment duration
segs.filter(s => TITLES[s.name] || s.name === "09_outro").forEach(() => {});
const imgSegs = segs.filter(s => s.name !== "10_end"); // 9 segments
imgSegs.forEach((s, k) => {
  const dur = (s.name === "09_outro") ? (DURATION - s.startSec) : (s.endSec - s.startSec);
  const nativeImg = `${CAMP}/${s.name}/gens/native.png`;
  const img = existsSync(nativeImg) ? nativeImg : `${CAMP}/${s.name}/gens/pencil.png`;
  inputs.push(`-loop 1 -t ${dur.toFixed(3)} -i ${img}`);
  fc.push(`[${k}:v]scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H},setsar=1,fps=${FPS}[p${k}]`);
});
fc.push(imgSegs.map((_, k) => `[p${k}]`).join("") + `concat=n=${imgSegs.length}:v=1:a=0[base]`);

let idx = imgSegs.length;           // next input index
let cur = "base";
// title overlays
const titleInputIdx = {};
imgSegs.forEach((s) => {
  if (!TITLES[s.name]) return;
  inputs.push(`-i ${OUTDIR}/title_${s.name}.png`);
  const end = (s.name === "09_outro") ? DURATION : s.endSec;
  fc.push(`[${cur}][${idx}:v]overlay=68:56:enable='between(t,${s.startSec.toFixed(2)},${end.toFixed(2)})'[t${idx}]`);
  cur = `t${idx}`; idx++;
});
// caption overlays
caps.forEach((c) => {
  inputs.push(`-i ${OUTDIR}/cap_${c.i}.png`);
  fc.push(`[${cur}][${idx}:v]overlay=(W-w)/2:H-h-86:enable='between(t,${c.start.toFixed(2)},${c.end.toFixed(2)})'[t${idx}]`);
  cur = `t${idx}`; idx++;
});
// per-slide reference letters (A, B, C…) under the caption, for feedback
imgSegs.forEach((s, k) => {
  const letter = String.fromCharCode(65 + k);
  const lp = `${OUTDIR}/letter_${k}.png`;
  textPng(lp, letter, { size: 48, fill: GREEN, oy: 5 });
  inputs.push(`-i ${lp}`);
  const end = (s.name === "09_outro") ? DURATION : s.endSec;
  fc.push(`[${cur}][${idx}:v]overlay=(W-w)/2:H-h-26:enable='between(t,${s.startSec.toFixed(2)},${end.toFixed(2)})'[t${idx}]`);
  cur = `t${idx}`; idx++;
});
// wordmark Aesthetic
inputs.push(`-i ${OUTDIR}/wm_a.png`);
fc.push(`[${cur}][${idx}:v]overlay=${aX}:${wmY}[t${idx}]`); cur = `t${idx}`; idx++;
// wordmark Computer
inputs.push(`-i ${OUTDIR}/wm_c.png`);
fc.push(`[${cur}][${idx}:v]overlay=${cX}:${wmY}[t${idx}]`); cur = `t${idx}`; idx++;
// bouncing dot
inputs.push(`-i ${OUTDIR}/wm_dot.png`);
fc.push(`[${cur}][${idx}:v]overlay=${dotX}:'${dotRestY}-${bounceAmp}*abs(sin(2*PI*${bounceF}*t))'[vout]`); idx++;

// audio: narration + ambient bed (bed quiet), match narration length
inputs.push(`-i ${NARR}`); const aNarr = idx; idx++;
inputs.push(`-i ${BED}`); const aBed = idx; idx++;
fc.push(`[${aNarr}:a]volume=1.0[an]`);
fc.push(`[${aBed}:a]volume=0.32[ab]`);
fc.push(`[an][ab]amix=inputs=2:duration=longest:dropout_transition=0[aout]`);

const fcPath = `${OUTDIR}/filter.txt`;
writeFileSync(fcPath, fc.join(";\n"));
const OUT = `${process.env.HOME}/Documents/Shelf/serpentine-fae-widescreen.mp4`;
const cmd = `ffmpeg -y ${inputs.join(" ")} -filter_complex_script ${fcPath} ` +
  `-map "[vout]" -map "[aout]" -r ${FPS} -t ${DURATION.toFixed(2)} ` +
  `-c:v libx264 -pix_fmt yuv420p -crf 18 -preset medium -c:a aac -b:a 192k "${OUT}"`;
writeFileSync(`${OUTDIR}/cmd.sh`, cmd);
console.log(`inputs: ${inputs.length}, overlays: ${idx}`);
console.log("running ffmpeg… ->", OUT);
const r = spawnSync("bash", ["-c", cmd], { stdio: "inherit" });
console.log(r.status === 0 ? `✓ DONE ${OUT}` : `✗ ffmpeg exit ${r.status}`);
