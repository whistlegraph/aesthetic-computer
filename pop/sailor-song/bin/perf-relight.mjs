#!/usr/bin/env node
// perf-relight.mjs — her room, lit by the record. (v98: "map the lights in her room and use those as the things to
// illuminate; bump via layer separation to create a vibe; virtually turn the lights off; a natural lit thing based on
// track info.") Over perf-video.mjs's base (no strip), the frame is split by vision-matte.swift's matte (Apple Vision
// subject lifting, full res, time-smoothed) into her + the guitar (left close to natural) and the room behind, and the
// room is relit from the receipt: ambient falls and rises with the sections (verses dim, choruses up, the break nearly
// off, the bridge climbing, the finale full, the release dark with her alone lit), tinted by the current chord (G#m
// amber, B violet, Emaj7 between; slow glides); the ceiling lamp blooms on kicks (bigger on downbeats); the fairy
// lights along the ceiling glow with the pads and twinkle on hats and clicks; the window glows with bells, gongs and
// rises and lets go slowly. Gain maps multiply the room, blooms add to it, the matte composites her back on top.
//
// The lyric is a singalong (v103: "bouncing ball style"): the sung line sits bottom-middle in the slab caption
// lettering (Comic Sans MS Bold bubble glyphs from glyph-atlas.py), white fill, black outline, sung words white and
// unsung grey, the hard drop shadow coloured by the mix — the chord's hue, as bright as the section, pulsing on kicks,
// flashing on bells — and a ball hops word to word, landing on each exactly at its onset. --vhs adds a VHS pass on
// the encode (chroma shift, soft luma, grain, scanlines, a rolling tracking band, a vignette); jeffrey ditched it (v103).
//
//   node pop/sailor-song/bin/perf-relight.mjs --audio out/sailor-song-v103.mp3 [--base out/sailor-song-v103-perf.mp4] [--vhs]
//     → out/<stem>-relight.mp4
//   … --only 20,42,88.7 --png DIR   → just those record seconds, as PNGs (the look, before the whole render)
//   … --from 36 --to 48 --out X.mp4  → just that stretch of the record, as a clip (a preview)
//   … --lyric-only --small --from 36 --to 48 → the words and ball alone over the picture at 960×540, fast — the timing loop
//   … --no-lyric                      → the room and her, no words
//   … --fia                           → Fia's Cut: the sung nouns get eyecons over their words in the captions (src/eyecons, bin/eyecons.py) → <stem>-relight-fia.mp4
import { readFileSync, writeFileSync, existsSync, openSync, readSync, closeSync, unlinkSync, mkdtempSync, readdirSync, statSync } from "node:fs";
import { spawn, execFileSync, spawnSync } from "node:child_process";
import { dirname, resolve, basename, join } from "node:path";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url)), LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out"), SR = 48000, PY = resolve(LANE, "../.venv/bin/python");
const arg = (k, d = null) => { const i = process.argv.indexOf(`--${k}`); return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith("--") ? process.argv[i + 1] : (process.argv.includes(`--${k}`) ? true : d); };
const AUDIO = resolve(arg("audio")); const stem = basename(AUDIO).replace(/\.(mp3|wav|flac)$/, "");
const R = JSON.parse(readFileSync(resolve(OUT, `${stem}.events.json`), "utf8")), T0 = R.startSec ?? 0;
// the base: this version's, else the newest *-perf.mp4 whose receipt shares this record's clock (startSec and sections —
// the picture is the same film; only the mix moved), so a new mix needs no new base
const sameClock = (a, b) => a.startSec === b.startSec && JSON.stringify(a.sections) === JSON.stringify(b.sections);
const PERFS = readdirSync(OUT).filter((f) => /^sailor-song-v\d+-perf\.mp4$/.test(f)).map((f) => resolve(OUT, f)).sort((a, b) => statSync(b).mtimeMs - statSync(a).mtimeMs);
const BASE = resolve(arg("base") || (existsSync(resolve(OUT, `${stem}-perf.mp4`)) ? resolve(OUT, `${stem}-perf.mp4`)
  : PERFS.find((f) => { const r = f.replace(/-perf\.mp4$/, ".events.json"); try { return sameClock(JSON.parse(readFileSync(r, "utf8")), R); } catch { return false; } })
    || (PERFS[0] && (console.error(`⚠ no base's receipt shares this record's clock (a receipt pruned from out/?) — taking the newest: ${basename(PERFS[0])}`), PERFS[0])) || resolve(OUT, `${stem}-perf.mp4`)));
if (!existsSync(BASE)) { console.error(`✗ base video missing: ${BASE} (render perf-video.mjs without --strip first)`); process.exit(1); }
const probe = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height,r_frame_rate,nb_frames", "-of", "csv=p=0", BASE]).toString().trim().split(",");
const DELIVER = Number(arg("deliver", 0)), LYRIC_ONLY = !!arg("lyric-only"), NO_LYRIC = !!arg("no-lyric"), SMALL = !!arg("small"), BALL = !!arg("ball");   // --ball: the bouncing ball (jeffrey lost it, v103)
const FIA = !!arg("fia");                                                   // Fia's Cut: the eyecons (a separate deliverable; off by default, the main render untouched)
// --vertical: the reel. A 9:16 window of the take (304×540 of the 960×540, centred on her at x 370) composited at
// 1080×1920 (--small: 540×960), the lyric a size smaller and placed between her face and the guitar (v103).
const VERTICAL = !!arg("vertical"), VIEW = VERTICAL ? { x: 218, y: 0, w: 304, h: 540 } : { x: 0, y: 0, w: 960, h: 540 };
// speed (v103: "why is it so slow?"): the take is 960×540, so compositing at 1080 only costs — --small works at 540 and
// --deliver 1080 upscales (lanczos + CAS) inside the encoder; --jobs N cuts the record into N stretches rendered in
// parallel (video only) and concatenates them, muxing the audio once.
const JOBS = Number(arg("jobs", 0)), NO_AUDIO = !!arg("no-audio"), X264 = !!arg("x264");   // the Mac's VideoToolbox encodes by default (libx264 at 1080p60 was the clock); --x264 for the software path   // --no-lyric: the room and her, no words
const W = VERTICAL ? (SMALL ? 540 : 1080) : SMALL ? 960 : +probe[0], H = VERTICAL ? (SMALL ? 960 : 1920) : SMALL ? 540 : +probe[1], FPS = eval(probe[2]), NF = +probe[3] || 0;
const FROM = Number(arg("from", 0)), TO = arg("to") ? Number(arg("to")) : null;
if (JOBS > 1) {                                                             // the conductor: N of this script, then one concat
  const dur = TO ?? Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", BASE]).toString());
  const out = resolve(arg("out") || resolve(OUT, `${stem}-relight${VERTICAL ? "-reel" : ""}${FIA ? "-fia" : ""}.mp4`)), tmp = mkdtempSync(join(tmpdir(), "relight-jobs-")), step = Math.ceil((dur - FROM) / JOBS);
  const skip = ["--jobs", "--out", "--from", "--to"], pass = process.argv.slice(2).filter((a, i, A) => !skip.includes(a) && !skip.includes(A[i - 1]));
  const kids = [], segs = [];
  for (let k = 0; k < JOBS; k++) { const a = FROM + k * step, b = Math.min(dur, a + step); if (a >= dur) break; const seg = join(tmp, `seg-${k}.mp4`); segs.push(seg);
    kids.push(new Promise((res, rej) => { const c = spawn(process.execPath, [fileURLToPath(import.meta.url), ...pass, "--from", String(a), "--to", String(b), "--no-audio", "--out", seg], { stdio: ["ignore", k ? "ignore" : "inherit", "inherit"] }); c.on("close", (code) => code ? rej(new Error(`job ${k} exited ${code}`)) : res()); })); }
  await Promise.all(kids);
  const list = join(tmp, "list.txt"); writeFileSync(list, segs.map((f) => `file '${f}'`).join("\n") + "\n");
  const mux = spawnSync("ffmpeg", ["-v", "error", "-y", "-f", "concat", "-safe", "0", "-i", list, ...(FROM ? ["-ss", String(FROM)] : []), ...(TO ? ["-to", String(TO)] : []), "-i", AUDIO, "-map", "0:v", "-map", "1:a", "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-movflags", "+faststart", "-shortest", out], { stdio: "inherit" });
  for (const f of segs) try { unlinkSync(f); } catch {} try { unlinkSync(list); } catch {}
  console.log(`${mux.status ? "✗" : "✓"} ${out}  (${JOBS} jobs)`); process.exit(mux.status || 0);
}
const outPath = resolve(arg("out") || resolve(OUT, `${stem}-${LYRIC_ONLY ? "lyric" : "relight"}${VERTICAL ? "-reel" : ""}${FIA ? "-fia" : ""}${TO ? `-${FROM}-${TO}` : ""}.mp4`));
const sx = W / VIEW.w, sy = H / VIEW.h, VX = VIEW.x, VY = VIEW.y;              // take space → frame: (x - VX) * sx, (y - VY) * sy
// the OUTPUT space (v103: "the lyrics are burned in in a way that they aren't crisp — sharpened after bake?"): the room is
// composited at the base's size, upscaled here (bilinear), and the lyric is drawn AFTER that at full delivery size from
// a finer atlas, so the words never pass through the encoder's upscale + sharpen
const OH = DELIVER && DELIVER !== H ? DELIVER : H, OW = Math.round(W * OH / H), osx = OW / VIEW.w, osy = OH / VIEW.h, OB = OH !== H ? Buffer.alloc(OW * OH * 3) : null;
let ob = null;                                                                   // the buffer the lyric draws on and the encoder gets

// ── the take's clock: record time → reg time (+startSec) → take time, through the timemap, like perf-video.mjs ──
const pairs = readFileSync(resolve(LANE, "src/vox/reg/timemap.txt"), "utf8").trim().split("\n").map((l) => l.trim().split(/\s+/).map(Number)).filter((p) => p.length === 2);
const takeOf = (reg) => { const x = reg * SR; let i = 1; while (i < pairs.length - 1 && pairs[i][1] < x) i++;
  const [s0, d0] = pairs[i - 1], [s1, d1] = pairs[i]; const f = d1 === d0 ? 0 : (x - d0) / (d1 - d0); return (s0 + (s1 - s0) * f) / SR; };

// ── the matte (vision-matte.swift → src/matte-take.mp4, gray in the luma, take clock, native size) ──
//    Decoded by ffmpeg into a FIFO and read synchronously: the take index per record frame never decreases, so the
//    stream is walked forward, skipping the take's dropped frames and holding its held ones. The soft edge Vision
//    leaves is pulled a little inward (smoothstep 0.25→0.85) so no bright fringe of wall rides along her hair.
const MATTE = resolve(LANE, "src/matte-take.mp4");
if (!existsSync(MATTE) && !LYRIC_ONLY) { console.error(`✗ ${MATTE} missing — see bin/vision-matte.swift`); process.exit(1); }
const MFPS = eval(execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=r_frame_rate", "-of", "csv=p=0", MATTE]).toString().trim());
let matteAt = () => {}; const M = new Float32Array(W * H); let MFD = null, mdec = null, FIFO = null;
if (!LYRIC_ONLY) {
const [MW, MH] = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height", "-of", "csv=p=0", MATTE]).toString().trim().split(",").map(Number);
FIFO = join(mkdtempSync(join(tmpdir(), "relight-")), "matte.gray"); execFileSync("mkfifo", [FIFO]);
mdec = spawn("ffmpeg", ["-v", "error", "-i", MATTE, "-f", "rawvideo", "-pix_fmt", "gray", "-y", FIFO], { stdio: ["ignore", "ignore", "inherit"] });
MFD = openSync(FIFO, "r"); const mraw = Buffer.alloc(MW * MH); let mIdx = -1, mEnd = false, mLast = -2;
const readMatte = () => { let got = 0; while (got < mraw.length) { const n = readSync(MFD, mraw, got, mraw.length - got, null); if (n <= 0) { mEnd = true; return false; } got += n; } return true; };
const shape = (v) => { const t = Math.min(1, Math.max(0, (v / 255 - 0.25) / 0.6)); return t * t * (3 - 2 * t); };
matteAt = (take) => { const k = Math.round(take * MFPS);
  while (mIdx < k && !mEnd) { if (!readMatte()) break; mIdx++; }
  if (mIdx === mLast) return; mLast = mIdx;                                // a held take frame: the matte is already up
  if (MW === W && MH === H && !VERTICAL) { for (let i = 0; i < W * H; i++) M[i] = shape(mraw[i]); return; }
  const fx = (MW / 960) * VIEW.w / W, fy = (MH / 540) * VIEW.h / H, ox = VX * MW / 960, oy = VY * MH / 540;   // bilinear, through the viewport
  for (let y = 0; y < H; y++) { const v = Math.min(MH - 1.001, Math.max(0, oy + (y + 0.5) * fy - 0.5)), v0 = v | 0, fv = v - v0, r0 = v0 * MW, r1 = (v0 + 1) * MW;
    for (let x = 0; x < W; x++) { const u = Math.min(MW - 1.001, Math.max(0, ox + (x + 0.5) * fx - 0.5)), u0 = u | 0, fu = u - u0;
      M[y * W + x] = shape((mraw[r0 + u0] * (1 - fu) + mraw[r0 + u0 + 1] * fu) * (1 - fv) + (mraw[r1 + u0] * (1 - fu) + mraw[r1 + u0 + 1] * fu) * fv); } } };

}
// ── two repairs on the matte, per frame (v103: "bottom left of guitar mask is weird, top of center light masks weird
//    too — probably glare"): the window's glare blows the guitar's lower bout to white and Vision lets it go, so the
//    guitar's body and neck from guitar-track.py are unioned in (soft 8 px edge); and under the lamp the bright glass
//    gets pulled into her hair, so inside the lamp's disc only dark pixels stay foreground (her hair is dark, glass is not).
const TRACK = (() => { try { return JSON.parse(readFileSync(resolve(LANE, "src/guitar-track.json"), "utf8")); } catch { return null; } })();
const TFPS = TRACK && TRACK.length > 1 ? (TRACK.length - 1) / TRACK.at(-1).t : 30;
const repairMatte = (take) => { if (LYRIC_ONLY) return;
  if (TRACK) { const k = TRACK[Math.min(TRACK.length - 1, Math.max(0, Math.round(take * TFPS)))], hx = (k.hole[0] - VX) * sx, hy = (k.hole[1] - VY) * sy, ex = (k.head[0] - VX) * sx, ey = (k.head[1] - VY) * sy;
    const L = Math.hypot(ex - hx, ey - hy), ux = (ex - hx) / L, uy = (ey - hy) / L, nx = uy, ny = -ux, soft = 8 * sx;
    const cx = hx + ux * 15 * sx - nx * 25 * sy, cy = hy + uy * 15 * sx - ny * 25 * sy, A = 200 * sx, B = 124 * sy;        // the body: an ellipse along the neck
    const x0 = Math.max(0, cx - A - soft) | 0, x1 = Math.min(W - 1, cx + A + soft) | 0, y0 = Math.max(0, cy - A - soft) | 0, y1 = Math.min(H - 1, cy + A + soft) | 0;
    for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) { const dx = x - cx, dy = y - cy, a = dx * ux + dy * uy, b = dx * nx + dy * ny, r = Math.hypot(a / A, b / B);
      if (r < 1 + soft / B) { const m = Math.min(1, (1 + soft / B - r) * B / soft); const o = y * W + x; if (m > M[o]) M[o] = m; } }
    const s0 = 140 * sx, s1 = L * 1.12, hw0 = 34 * sy, hw1 = 30 * sy;                                                     // the neck + headstock: a tapered band
    const bx0 = Math.max(0, Math.min(hx + ux * s0, hx + ux * s1) - 60) | 0, bx1 = Math.min(W - 1, Math.max(hx + ux * s0, hx + ux * s1) + 60) | 0;
    const by0 = Math.max(0, Math.min(hy + uy * s0, hy + uy * s1) - 60) | 0, by1 = Math.min(H - 1, Math.max(hy + uy * s0, hy + uy * s1) + 60) | 0;
    for (let y = by0; y <= by1; y++) for (let x = bx0; x <= bx1; x++) { const dx = x - hx, dy = y - hy, a = dx * ux + dy * uy, b = Math.abs(dx * nx + dy * ny); if (a < s0 || a > s1) continue;
      const hw = hw0 + (hw1 - hw0) * (a - s0) / (s1 - s0) + (a > L * 0.95 ? 12 * sy : 0), m = Math.min(1, (hw + soft - b) / soft); if (m > 0) { const o = y * W + x; if (m > M[o]) M[o] = m; } } }
  const Lp = LIGHTS.lamp, lx = (Lp.x - VX) * sx, ly = (Lp.y - VY) * sy, lr = Lp.r * 1.15 * sx;
  for (let y = Math.max(0, ly - lr) | 0; y <= Math.min(H - 1, ly + lr); y++) for (let x = Math.max(0, lx - lr) | 0; x <= Math.min(W - 1, lx + lr); x++) { if (Math.hypot(x - lx, y - ly) > lr) continue;
    const o = y * W + x, p = o * 3, lum = (fb[p] * 0.299 + fb[p + 1] * 0.587 + fb[p + 2] * 0.114) / 255, keep = 1 - Math.min(1, Math.max(0, (lum - 0.42) / 0.18)); M[o] *= keep; } };

// a 3-tap blur each way on the matte (v103: "edge aliasing on the right side of her"): the composite's edge stays soft
const M2 = new Float32Array(W * H);
const softenMatte = () => { if (LYRIC_ONLY) return;
  for (let y = 0; y < H; y++) { const r = y * W; M2[r] = M[r]; M2[r + W - 1] = M[r + W - 1]; for (let x = 1; x < W - 1; x++) M2[r + x] = (M[r + x - 1] + 2 * M[r + x] + M[r + x + 1]) * 0.25; }
  for (let x = 0; x < W; x++) { M[x] = M2[x]; M[(H - 1) * W + x] = M2[(H - 1) * W + x]; } for (let y = 1; y < H - 1; y++) for (let x = 0; x < W; x++) { const o = y * W + x; M[o] = (M2[o - W] + 2 * M2[o] + M2[o + W]) * 0.25; } };

// ── the single's cover grade (bin/cover.py v8, as perf-video --cover had it), applied HERE so it can arrive with the
//    arrangement (v103: "fade in all the overlay elements so it starts like the straight video she sent, and the colour
//    grading gets more into our world as the instruments queue up"): a natural cubic S-curve through the cover's
//    points, saturation 1.22, contrast 1.06, and the reds pushed deeper (the shirt) — mixed in by `arrive` ──
const CURVE = (() => { const X = [0, 0.1, 0.5, 0.9, 1], Y = [0, 0.06, 0.5, 0.95, 1], n = X.length, h = [], a = [], l = [1], mu = [0], z = [0], c = new Array(n).fill(0), b = [], d = [];
  for (let i = 0; i < n - 1; i++) h[i] = X[i + 1] - X[i]; for (let i = 1; i < n - 1; i++) a[i] = 3 / h[i] * (Y[i + 1] - Y[i]) - 3 / h[i - 1] * (Y[i] - Y[i - 1]);
  for (let i = 1; i < n - 1; i++) { l[i] = 2 * (X[i + 1] - X[i - 1]) - h[i - 1] * mu[i - 1]; mu[i] = h[i] / l[i]; z[i] = (a[i] - h[i - 1] * z[i - 1]) / l[i]; }
  for (let j = n - 2; j >= 0; j--) { c[j] = z[j] - mu[j] * c[j + 1]; b[j] = (Y[j + 1] - Y[j]) / h[j] - h[j] * (c[j + 1] + 2 * c[j]) / 3; d[j] = (c[j + 1] - c[j]) / (3 * h[j]); }
  const lut = new Uint8ClampedArray(256); for (let v = 0; v < 256; v++) { const x = v / 255; let j = 0; while (j < n - 2 && x > X[j + 1]) j++; const t = x - X[j]; lut[v] = Math.round(255 * (Y[j] + b[j] * t + c[j] * t * t + d[j] * t * t * t)); } return lut; })();
const coverGrade = (a) => { if (a <= 0.002) return; const ia = 1 - a;
  for (let p = 0; p < W * H * 3; p += 3) { let r = fb[p], g = fb[p + 1], b = fb[p + 2];
    const mx = Math.max(g, b), red = r > mx ? (r - mx) / 255 : 0;                           // the reds, by how red they are
    r = Math.min(255, r * (1 + 0.14 * red)); g *= 1 - 0.06 * red; b *= 1 - 0.1 * red;
    r = CURVE[r | 0]; g = CURVE[g | 0]; b = CURVE[b | 0];
    const l = 0.299 * r + 0.587 * g + 0.114 * b; r = l + (r - l) * 1.22; g = l + (g - l) * 1.22; b = l + (b - l) * 1.22;
    r = (r - 128) * 1.06 + 128; g = (g - 128) * 1.06 + 128; b = (b - 128) * 1.06 + 128;
    fb[p] = Math.max(0, Math.min(255, fb[p] * ia + r * a)); fb[p + 1] = Math.max(0, Math.min(255, fb[p + 1] * ia + g * a)); fb[p + 2] = Math.max(0, Math.min(255, fb[p + 2] * ia + b * a)); } };

// ── the stage light on her (v103: "front lighting on her face, depth-modelled, bump-mapped"): depth-map.py's clip
//    (src/depth-take.mp4, half res, closer = brighter) streamed like the matte; normals from the depth gradient,
//    a key from the upper left front and a rim from the right, Lambert-shaded, applied inside the matte only —
//    and only where the depth is continuous (the silhouette's depth cliff would otherwise draw a dark outline) —
//    plus a soft frontal spot on her face. All of it arrives with the arrangement. Absent the clip, no stage light. ──
const DEPTH = resolve(LANE, "src/depth-take.mp4");
const STAGE = !LYRIC_ONLY && !!arg("stage") && existsSync(DEPTH) && (() => { try { return execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=nb_frames", "-of", "csv=p=0", DEPTH], { stdio: ["ignore", "pipe", "ignore"] }).toString().trim() !== ""; } catch { return false; } })();   // a clip still being written is not a clip
let depthAt = () => {}; const DN = STAGE ? new Float32Array(W * H) : null;         // the shading gain per pixel, 1 = untouched
if (STAGE) { const [DW, DH] = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height", "-of", "csv=p=0", DEPTH]).toString().trim().split(",").map(Number);
  const DFIFO = join(mkdtempSync(join(tmpdir(), "relight-depth-")), "depth.gray"); execFileSync("mkfifo", [DFIFO]);
  const ddec = spawn("ffmpeg", ["-v", "error", "-i", DEPTH, "-f", "rawvideo", "-pix_fmt", "gray", "-y", DFIFO], { stdio: ["ignore", "ignore", "inherit"] });
  const DFD = openSync(DFIFO, "r"), draw = Buffer.alloc(DW * DH), dq = new Float32Array(DW * DH), gq = new Float32Array(DW * DH); let dIdx = -1, dEnd = false, dLast = -2;
  const readDepth = () => { let got = 0; while (got < draw.length) { const n = readSync(DFD, draw, got, draw.length - got, null); if (n <= 0) { dEnd = true; return false; } got += n; } return true; };
  const Lk = [-0.35, -0.55, 0.75], Lr = [0.7, -0.2, 0.6]; for (const L of [Lk, Lr]) { const n = Math.hypot(...L); L[0] /= n; L[1] /= n; L[2] /= n; }
  const RELIEF = 90 * (DW / 960), K = 0.28, CLIFF = 0.06;             // v103: toned down ("too extreme")                            // depth units per pixel of slope; the key's strength; a gradient past this is a silhouette, not a face
  depthAt = (take) => { const k = Math.round(take * MFPS); while (dIdx < k && !dEnd) { if (!readDepth()) break; dIdx++; } if (dIdx === dLast) return; dLast = dIdx;
    for (let i = 0; i < DW * DH; i++) dq[i] = draw[i] / 255;
    // the shading at depth resolution: Sobel normals, two lights, the cliff test
    for (let y = 1; y < DH - 1; y++) for (let x = 1; x < DW - 1; x++) { const o = y * DW + x;
      const gx = (dq[o - DW + 1] + 2 * dq[o + 1] + dq[o + DW + 1] - dq[o - DW - 1] - 2 * dq[o - 1] - dq[o + DW - 1]) / 8, gy = (dq[o + DW - 1] + 2 * dq[o + DW] + dq[o + DW + 1] - dq[o - DW - 1] - 2 * dq[o - DW] - dq[o - DW + 1]) / 8;
      const cliff = Math.min(1, Math.max(0, (Math.hypot(gx, gy) - CLIFF * 0.5) / (CLIFF * 0.5)));                 // 1 at a depth cliff
      const nx = -gx * RELIEF, ny = -gy * RELIEF, nn = Math.hypot(nx, ny, 1), sk = Math.max(0, (nx * Lk[0] + ny * Lk[1] + Lk[2]) / nn), sr = Math.max(0, (nx * Lr[0] + ny * Lr[1] + Lr[2]) / nn);
      gq[o] = 1 + (K * (sk - Lk[2]) + 0.25 * K * (sr - Lr[2])) * (1 - cliff); }
    for (let x = 0; x < DW; x++) { gq[x] = 1; gq[(DH - 1) * DW + x] = 1; } for (let y = 0; y < DH; y++) { gq[y * DW] = 1; gq[y * DW + DW - 1] = 1; }
    // up to the frame through the viewport, bilinear
    const fx = (DW / 960) * VIEW.w / W, fy = (DH / 540) * VIEW.h / H, ox = VX * DW / 960, oy = VY * DH / 540;
    for (let y = 0; y < H; y++) { const v = Math.min(DH - 1.001, Math.max(0, oy + (y + 0.5) * fy - 0.5)), v0 = v | 0, fv = v - v0, r0 = v0 * DW, r1 = (v0 + 1) * DW;
      for (let x = 0; x < W; x++) { const u = Math.min(DW - 1.001, Math.max(0, ox + (x + 0.5) * fx - 0.5)), u0 = u | 0, fu = u - u0;
        DN[y * W + x] = (gq[r0 + u0] * (1 - fu) + gq[r0 + u0 + 1] * fu) * (1 - fv) + (gq[r1 + u0] * (1 - fu) + gq[r1 + u0 + 1] * fu) * fv; } } };
  process.on("exit", () => { try { closeSync(DFD); ddec.kill(); unlinkSync(DFIFO); } catch {} }); }
// the face spot: a soft disc of light where her head is (the matte's topmost mass), front-on
let faceX = W * 0.4, faceY = H * 0.3;
const findFace = () => { let sx_ = 0, sy_ = 0, n = 0; const step = 4 * sx | 0 || 1;
  for (let y = 0; y < H * 0.6; y += step) for (let x = 0; x < W; x += step) { const m = M[y * W + x]; if (m > 0.5) { const wgt = 1 - y / (H * 0.6); sx_ += x * wgt; sy_ += y * wgt; n += wgt; } }
  if (n > 0) { faceX += (sx_ / n - faceX) * 0.1; faceY += (sy_ / n + 0.1 * H - faceY) * 0.1; } };

// ── the lights (room-lights.py), as static fields over the frame ──
const LIGHTS = JSON.parse(readFileSync(resolve(LANE, "src/room-lights.json"), "utf8"));
const LAMP = new Float32Array(W * H), WINDOW = new Float32Array(W * H);
{ const L = LIGHTS.lamp, lx = (L.x - VX) * sx, ly = (L.y - VY) * sy, s2 = 2 * (250 * sx) ** 2, Wn = { x0: LIGHTS.window.x0 - VX, x1: LIGHTS.window.x1 - VX, y0: LIGHTS.window.y0 - VY, y1: LIGHTS.window.y1 - VY }, soft = 70 * sx;
  const edge = (d) => Math.min(1, Math.max(0, d / soft));
  for (let y = 0; y < H; y++) for (let x = 0; x < W; x++) { const o = y * W + x; LAMP[o] = Math.exp(-((x - lx) ** 2 + (y - ly) ** 2) / s2);
    const inx = edge(x - Wn.x0 * sx + soft * 0.5) * edge(Wn.x1 * sx - x + soft * 0.5), iny = edge(y - Wn.y0 * sy + soft * 0.5) * edge(Wn.y1 * sy - y + soft * 0.5); WINDOW[o] = inx * iny; } }
const FAIRY = LIGHTS.fairy.map(([x, y], i) => ({ x: (x - VX) * sx, y: (y - VY) * sy, i }));

// ── the score: sections → energy, bars → chord tint, events → the lights ──
const sections = (R.sections || []).slice().sort((a, b) => a.start - b.start), bars = (R.bars || []).slice().sort((a, b) => a.t - b.t);
const END = sections.at(-1)?.end ?? Infinity;                             // after the outro the record lets go: the release
const ENERGY = { intro: 0.3, verse1: 0.42, chorus1: 0.78, verse2: 0.38, chorus2: 0.95, break: 0.08, bridge: [0.28, 0.72], outro: 1.0, release: 0.04 };
const energyAt = (t) => { const s = sections.find((s) => t >= s.start && t < s.end); if (!s) return t >= END ? ENERGY.release : ENERGY.intro;
  const e = ENERGY[s.name] ?? 0.5; return Array.isArray(e) ? e[0] + (e[1] - e[0]) * (t - s.start) / (s.end - s.start) : e; };
const TINT = { "G#m": [1.0, 0.84, 0.62], "B": [0.78, 0.76, 1.0], "Emaj7": [0.96, 0.9, 0.8] };               // the room
const HUE = { "G#m": [255, 160, 50], "B": [140, 100, 255], "Emaj7": [240, 200, 110] };                      // the lyric's shadow
const chordAt = (t) => { let c = null; for (const b of bars) { if (t >= b.t) c = b; else break; } return c; };
const vmax = {}; for (const e of R.events || []) vmax[e.voice] = Math.max(vmax[e.voice] || 0, e.gain ?? 0.5);
const pick = (...voices) => (R.events || []).filter((e) => voices.includes(e.voice)).map((e) => ({ t: e.t, dur: e.dur || 0.1, g: (e.gain ?? 0.5) / (vmax[e.voice] || 1) })).sort((a, b) => a.t - b.t);
const KICKS = pick("kick", "wub-kick", "rev-kick"), HATS = pick("hat", "click", "rim"), PADS = pick("pad", "pad-hit", "high", "vib"), BELLS = pick("bell", "gong", "rise", "impact"), ARPS = pick("arp", "ostinato", "hook");
const downbeat = (t) => bars.some((b) => Math.abs(b.t - t) < 0.04);
for (const k of KICKS) { k.down = downbeat(k.t); k.w = k.down ? 1.6 : 1; }
// an envelope over a list: the sum of each event's attack-less decay since it hit (dec seconds to 1/e), windowed
const env = (ev, now, dec, hold = 0) => { let s = 0; for (let i = ev.cur || 0; i < ev.length && ev[i].t <= now; i++) { const dt = now - ev[i].t; if (dt > hold + dec * 5) { if (i === (ev.cur || 0)) ev.cur = i + 1; continue; }
  s += ev[i].g * (ev[i].w ?? 1) * (dt < hold ? 1 : Math.exp(-(dt - hold) / dec)); } return s; };
const sounding = (ev, now) => { let s = 0; for (let i = ev.cur || 0; i < ev.length && ev[i].t <= now; i++) { if (ev[i].t + ev[i].dur + 1 < now) { if (i === (ev.cur || 0)) ev.cur = i + 1; continue; } if (now < ev[i].t + ev[i].dur) s += ev[i].g; } return s; };
const fnv = (str) => { let h = 2166136261; for (const c of Buffer.from(str, "utf8")) h = Math.imul(h ^ c, 16777619) >>> 0; return h; };
for (const h of HATS) { const r = fnv(String(h.t)); h.pts = [0, 1, 2, 3, 4, 5, 6, 7].map((i) => (r >>> (i * 4)) % FAIRY.length); }

// ── lyrics: words-record.json (master clock) grouped into lyrics-sung.txt's lines; lettered from the Comic Sans atlas ──
const WORDS_FILE = ["src/words-video.json", "src/words-record.json"].map((f) => resolve(LANE, f)).find(existsSync);   // lyric-judge.py's corrected copy first
const WORDS = (() => { try { return JSON.parse(readFileSync(WORDS_FILE, "utf8")); } catch { return []; } })().map((w) => ({ text: w.text, a: w.fromMs / 1000 + T0, b: w.toMs / 1000 + T0,
  syl: (w.tokens && w.tokens.length && w.tokens.map((t) => t.text).join("") === w.text ? w.tokens : [{ text: w.text, fromMs: w.fromMs, toMs: w.toMs }]).map((t) => ({ text: t.text, a: t.fromMs / 1000 + T0, b: t.toMs / 1000 + T0 })) }));
const APX = osx >= 1.5 ? 144 : 72, ATLAS = resolve(LANE, `src/glyph-atlas/comic-${APX}`);
if (!existsSync(`${ATLAS}.json`)) execFileSync(PY, [resolve(HERE, "glyph-atlas.py"), "--px", String(APX), "--stroke", String(APX / 24)], { stdio: "inherit" });
const GA = JSON.parse(readFileSync(`${ATLAS}.json`, "utf8")), GFILL = readFileSync(`${ATLAS}.fill.raw`), GOUTER = readFileSync(`${ATLAS}.outer.raw`);
const WHITE = [250, 250, 250], GREY = [135, 135, 135], BLACK = [12, 12, 12], SPACE = GA.glyphs[" "].adv;
const norm = (t) => t.toLowerCase().replace(/[^a-z0-9']/g, "");
const LINES = []; { let k = 0; const lyr = (() => { try { return readFileSync(resolve(LANE, "src/lyrics-sung.txt"), "utf8"); } catch { return ""; } })().split("\n").map((l) => l.trim()).filter((l) => l && !l.startsWith("["));
  for (const line of lyr) { const toks = line.split(/\s+/), got = []; let kk = k;
    for (const tok of toks) { const n = norm(tok); if (!n) continue; let j = kk; while (j < WORDS.length && j < kk + 6 && norm(WORDS[j].text) !== n) j++;
      if (j < WORDS.length && j < kk + 6) { got.push({ tok, w: WORDS[j] }); kk = j + 1; } else got.push({ tok, w: null }); }
    const timed = got.filter((g) => g.w); if (timed.length) { LINES.push({ words: got, a: timed[0].w.a, b: timed.at(-1).w.b }); k = kk; } } }
for (const L of LINES) for (const [wi, g] of L.words.entries()) { const syl = g.w && g.w.text === g.tok ? g.w.syl : null; let si = 0, sk = 0;
  g.gl = [...g.tok].map((ch, i) => { const gg = GA.glyphs[ch] || GA.glyphs["?"], h = fnv(`${L.a}:${wi}:${i}:${ch}`);
    if (syl) { while (si < syl.length - 1 && sk >= [...syl[si].text].length) { si++; sk = 0; } }
    // MacPal jitter, deterministic per glyph: no x, a hair of y, at most a degree of tilt, and a slow sway of its own
    return { g: gg, jy: (((h >>> 8) % 5) - 2) * GA.px / 60, rot: (((h >>> 16) % 5) - 2) * 0.5 * Math.PI / 180, per: 1.6 + ((h >>> 4) % 9) / 10, ph: (h % 100) / 50, si, sk: sk++, sn: syl ? [...syl[si].text].length : 1 }; });
  g.adv = g.gl.reduce((a, x) => a + x.g.adv, 0);
  // the syllables' landing spots along the word (atlas units from the word's left edge), for the ball
  g.syl = (syl || [{ a: g.w?.a, b: g.w?.b }]).map((t, i) => { let x0 = 0, wsum = 0; for (const q of g.gl) { if (q.si < i) x0 += q.g.adv; else if (q.si === i) wsum += q.g.adv; } return { a: t.a, b: t.b, off: x0 + wsum / 2 }; }); }
// the lines become CHUNKS of at most five words (v103: "shorter, 5 words max, more lines"), split after commas where
// they fall, else evenly. Each chunk is one centred row at a fixed size (smaller only if it would not fit).
const CHUNKS = []; for (const L of LINES) { const n = L.words.length, parts = Math.ceil(n / 5); let cuts = [];
  if (parts > 1) { const marks = L.words.map((g, i) => i).filter((i) => i < n - 1 && /[,?;:]["']?$/.test(L.words[i].tok));
    if (marks.length >= parts - 1 && marks.every((m, k) => (k ? m - marks[k - 1] : m + 1) <= 5) && n - 1 - marks.at(-1) <= 5) cuts = marks.slice(0, parts - 1).map((m) => m + 1);
    else cuts = Array.from({ length: parts - 1 }, (_, k) => Math.round((k + 1) * n / parts)); }
  let prev = 0; for (const c of [...cuts, n]) { const words = L.words.slice(prev, c), timed = words.filter((g) => g.w); prev = c;
    if (timed.length) CHUNKS.push({ words, a: timed[0].w.a, b: timed.at(-1).w.b }); } }
const SC_MAX = (VERTICAL ? 0.19 : 0.4) * osx * (72 / GA.px) /* the finer atlas draws the same size */, ROW = VERTICAL ? OH * 0.555 : OH * 0.88, GAP = SPACE * 1.7, GRAV = 1400 * osy, SLIDE = 0.22 * OW;   // v103: smaller, higher, words further apart; the reel a size smaller, between her face and the guitar
for (const C of CHUNKS) { const tw = C.words.reduce((a, g) => a + g.adv, 0) + GAP * (C.words.length - 1);
  C.sc = Math.min(SC_MAX, (OW * 0.86) / tw); let pen = (OW - tw * C.sc) / 2; for (const g of C.words) { g.x = pen; g.base = ROW; pen += (g.adv + GAP) * C.sc; } }
// the switch between chunks (v103: "the prior opacities off to the left, the next shifts in to centre and opacities in"):
// one 0.35 s move shared by both, placed in the gap after the last word, or just before the next when there is no gap
CHUNKS.forEach((C, i) => { const P = CHUNKS[i - 1], N = CHUNKS[i + 1];
  // the next line comes in as soon as this one's last word is done (v103: "as soon as that extended long ends, swap — not
  // de-highlight and wait"); only across a long instrumental gap (> 4 s) does it hold off until 1.5 s before its first word
  // in sequence, never overlapping: the old line is gone in 0.15 s, the new one arrives over the next 0.2 s
  // and never before the last word has finished (v103: "some words don't fully complete"): when lines run together the
  // out and in squeeze into whatever gap there is, the new line finishing its fade-in a touch after its first word if it must
  // the dead zone (v103: "readability vs timing"): a finished line holds 0.35 s before it slides, when the gap allows; a
  // shorter gap shares itself out (hold 40%, out 25%, in the rest); under 0.2 s it is a hard cut
  if (N) { const gap = N.a - C.b; if (gap > 4) { C.outA = N.a - 1.5; C.outB = C.outA + 0.15; } else if (gap >= 0.9) { C.outA = C.b + 0.35; C.outB = C.outA + 0.15; } else if (gap >= 0.2) { C.outA = C.b + gap * 0.4; C.outB = C.outA + gap * 0.25; } else { C.outA = C.b; C.outB = C.b + 0.06; } }
  else { C.outA = C.b + 0.1; C.outB = C.b + 0.5; }
  C.inA = P ? P.outB : C.a - 0.6; C.inB = P ? (C.a - P.outB < 0.2 ? P.outB + 0.06 : Math.min(C.a, P.outB + 0.2)) : C.a - 0.25; });
const easeIO = (f) => f * f * (3 - 2 * f);
// a chunk's alpha and x-shift now: in from the right, out to the left
const chunkState = (C, now) => { if (now < C.inA || now > C.outB) return null;
  if (now < C.inB) { const e = easeIO((now - C.inA) / (C.inB - C.inA)); return { al: e, dx: (1 - e) * SLIDE }; }
  if (now > C.outA) { const e = easeIO((now - C.outA) / (C.outB - C.outA)); return { al: 1 - e, dx: -e * SLIDE }; }
  return { al: 1, dx: 0 }; };
// the ball's whole path: one landing per syllable, plus extra bounces on the spot through a held syllable (chromatic),
// so the ball is ONE continuous trajectory — arcs between consecutive landings, nothing else
const LANDS = []; for (const C of CHUNKS) for (const g of C.words) if (g.w) for (const t of g.syl) LANDS.push({ t: t.a, b: t.b, x: g.x + t.off * C.sc, C, chroma: false });
LANDS.sort((a, b) => a.t - b.t);
for (let i = LANDS.length - 1; i >= 0; i--) { const L = LANDS[i], next = LANDS[i + 1]?.t ?? L.b + 0.6, until = Math.min(L.b, next - 0.3) - L.t;
  if (until > 0.85) { const k = Math.floor(until / 0.42), extra = []; for (let q = 1; q <= k; q++) extra.push({ t: L.t + q * until / (k + 1), b: L.b, x: L.x, C: L.C, chroma: true }); LANDS.splice(i + 1, 0, ...extra); } }
const DWELL = 0.08;

// ── Fia's Cut (--fia): the eyecons. (Fia, via jeffrey: "a version with eyecons like a cutout of Anne Hathaway's face and
//    other floating identifiers … wiggling up like emoji or illustrations like clipart, to represent the nouns"; then the
//    recast: "placed near the words in captions … more like supersets / added on to captions … down near bottom … to bring
//    more symbolic meaning there.") Each concrete noun in the lyric has a sticker in src/eyecons/<name>.png (bin/eyecons.py:
//    Noto emoji and a Commons photo of Anne Hathaway, white cutout border, soft shadow). The sticker ANNOTATES the caption:
//    it sits just above its own word — centred on the word, its bottom a quarter em above the glyph tops, never on the
//    letters — about 1.5 cap heights tall (the Hathaway cutout a touch more); a noun of several words (Anne Hathaway, run
//    away, sit it out) centres over its span. When the word is sung it springs up from behind the letters (8 % overshoot,
//    a wiggle that settles, a faint 2 Hz wobble after), then stays as long as its line does and slides and fades out WITH
//    the line. Two annotated words close enough to collide shrink together to fit. Drawn inside the lyric block, under the
//    letters, after the words are laid out. The face shield is still honoured (moot down on the caption row).
const EYE_MAP = [[/^saw$/, "eyes"], [/^anne$/, "hathaway", { size: 1.3, span: 2 }], [/^pen$/, "pen"], [/^coughed$/, "cough"], [/^knees$/, "knees"], [/^baby$/, "baby"],
  [/^kiss$/, "kiss"], [/^mouth$/, "mouth"], [/^love$/, "heart"], [/^sailor$/, "sailboat"], [/^taste$/, "tongue"], [/^flavor$/, "icecream"], [/^god$/, "pray"], [/^savior$/, "halo"],
  [/^mom$/, "mom"], [/^worried$/, "worried"], [/^sleep$/, "sleep"], [/^wait$/, "hourglass"], [/^sting$/, "bee"], [/^bleeding$/, "blood"], [/^run$/, "runner", { span: 2 }],
  [/^walls$/, "bricks"], [/^house$/, "house"], [/^cat$/, "cat"], [/^mouse$/, "mouse"], [/^forever$/, "infinity"], [/^sit$/, "chair", { span: 3 }]];
const EYE_DIR = resolve(LANE, "src/eyecons"), EYE_IMG = {}, CUES = [];
const EYE = { cap: 1.5, gap: 0.25, rise: 0.5, vis: 352 / 512 };            // the picture's height in cap heights; the gap above the glyph tops in em; the spring's length in s; the picture's share of the sticker canvas (bin/eyecons.py)
if (FIA) {
  // the stickers, decoded by ffmpeg to rgba and premultiplied (so the bilinear blit never pulls dark fringe out of the transparent pixels),
  // with a chain of half-size mips (box-filtered) so a sticker drawn at a tenth of its size is sampled, not skipped
  const mip = (im) => { const w = im.w >> 1, h = im.h >> 1, px_ = Buffer.alloc(w * h * 4), S = im.px, R = im.w * 4;
    for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) { const o = (y * w + x) * 4, i = (2 * y * im.w + 2 * x) * 4; for (let c = 0; c < 4; c++) px_[o + c] = (S[i + c] + S[i + 4 + c] + S[i + R + c] + S[i + R + 4 + c] + 2) >> 2; } return { w, h, px: px_ }; };
  for (const name of new Set(EYE_MAP.map((e) => e[1]))) { const f = resolve(EYE_DIR, `${name}.png`); if (!existsSync(f)) { console.error(`⚠ no eyecon for ${name} (${f}) — run bin/eyecons.py`); continue; }
    const [w, h] = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height", "-of", "csv=p=0", f]).toString().trim().split(",").map(Number);
    const px_ = execFileSync("ffmpeg", ["-v", "error", "-i", f, "-f", "rawvideo", "-pix_fmt", "rgba", "-"], { maxBuffer: 1 << 26 });
    for (let o = 0; o < px_.length; o += 4) { const a = px_[o + 3] / 255; px_[o] *= a; px_[o + 1] *= a; px_[o + 2] *= a; }
    let L = EYE_IMG[name] = { w, h, px: px_ }; while (L.w >= 64) { L.next = mip(L); L = L.next; } }
  // the cues: one per sung noun; a noun of several words holds its span
  for (let i = 0; i < WORDS.length; i++) { const n = norm(WORDS[i].text), e = EYE_MAP.find(([re]) => re.test(n)); if (!e || !EYE_IMG[e[1]]) continue;
    const o = e[2] || {}, span = o.span || 1, ws = WORDS.slice(i, i + span);
    CUES.push({ name: e[1], a: WORDS[i].a, b: ws.at(-1).b, size: o.size || 1, ws: new Set(ws), h: fnv(`eye:${e[1]}:${i}`) }); i += span - 1; }
  // each chunk's annotations: the cue, and where it sits over the chunk's words of its span (the chunk's own x, before the slide)
  for (const C of CHUNKS) { C.eyes = []; for (const cue of CUES) { const mine = C.words.filter((g) => g.w && cue.ws.has(g.w)); if (!mine.length) continue;
    const x0 = Math.min(...mine.map((g) => g.x)), x1 = Math.max(...mine.map((g) => g.x + g.adv * C.sc)); C.eyes.push({ cue, x: (x0 + x1) / 2 }); } C.eyes.sort((p, q) => p.x - q.x); }
  console.log(`  Fia's Cut: ${CUES.length} eyecon cues from ${Object.keys(EYE_IMG).length} stickers, on ${CHUNKS.filter((C) => C.eyes.length).length} caption rows`); }
const easeOutBack = (p, c1 = 1.4) => { const c3 = c1 + 1; return 1 + c3 * (p - 1) ** 3 + c1 * (p - 1) ** 2; };   // c1 1.4 → ~8 % overshoot
const pickMip = (im, px_) => { let L = im; while (L.next && L.next.w >= px_) L = L.next; return L; };                // the smallest level still at least the drawn size
// a premultiplied rgba sticker blitted onto the output buffer, scaled and tilted about its centre (bilinear)
const spriteO = (im, cx, cy, sc, rot, al) => { const w = im.w, h = im.h, P = im.px, cs = Math.cos(rot), sn = Math.sin(rot), rad = Math.hypot(w, h) * sc / 2;
  const X0 = Math.max(0, Math.floor(cx - rad)), X1 = Math.min(OW - 1, Math.ceil(cx + rad)), Y0 = Math.max(0, Math.floor(cy - rad)), Y1 = Math.min(OH - 1, Math.ceil(cy + rad));
  for (let Y = Y0; Y <= Y1; Y++) for (let X = X0; X <= X1; X++) { const dx = X + 0.5 - cx, dy = Y + 0.5 - cy, u = (dx * cs + dy * sn) / sc + w / 2 - 0.5, v = (-dx * sn + dy * cs) / sc + h / 2 - 0.5;
    if (u < 0 || v < 0 || u >= w - 1 || v >= h - 1) continue; const u0 = u | 0, v0 = v | 0, fu = u - u0, fv = v - v0, o = (v0 * w + u0) * 4, q = o + w * 4;
    const a = ((P[o + 3] * (1 - fu) + P[o + 7] * fu) * (1 - fv) + (P[q + 3] * (1 - fu) + P[q + 7] * fu) * fv) / 255 * al; if (a < 0.004) continue;
    const r = ((P[o] * (1 - fu) + P[o + 4] * fu) * (1 - fv) + (P[q] * (1 - fu) + P[q + 4] * fu) * fv) * al, g = ((P[o + 1] * (1 - fu) + P[o + 5] * fu) * (1 - fv) + (P[q + 1] * (1 - fu) + P[q + 5] * fu) * fv) * al, b = ((P[o + 2] * (1 - fu) + P[o + 6] * fu) * (1 - fv) + (P[q + 2] * (1 - fu) + P[q + 6] * fu) * fv) * al;   // premultiplied: the colour already carries the alpha
    const k = (Y * OW + X) * 3; ob[k] = Math.min(255, ob[k] * (1 - a) + r); ob[k + 1] = Math.min(255, ob[k + 1] * (1 - a) + g); ob[k + 2] = Math.min(255, ob[k + 2] * (1 - a) + b); } };
// one caption row's eyecons now, riding the row's slide and alpha (st): the spring up from behind the letters, the rest above
// the word, the collision shrink; the row's letters are drawn after, so they stay on top
const drawEyeconsOn = (C, st, now) => { if (!C.eyes || !C.eyes.length) return; const sc = C.sc, em = GA.px * sc, cap = GA.ascent * 0.72 * sc, top = ROW - GA.ascent * sc;
  const FCX = faceX * OW / W, FCY = faceY * OH / H, FR = 0.2 * OH;
  const live = []; for (const e of C.eyes) if (now >= e.cue.a) live.push({ e, canvas: EYE.cap * cap * e.cue.size / EYE.vis, x: e.x + st.dx });
  let k = 1; for (let i = 1; i < live.length; i++) { const p = live[i - 1], q = live[i], need = (p.canvas + q.canvas) / 2 * EYE.vis + 0.15 * em, have = q.x - p.x; if (have < need) k = Math.min(k, have / need); }   // neighbours that would touch shrink together
  k = Math.max(0.5, k);
  for (const { e, canvas: c0, x } of live) { const im = EYE_IMG[e.cue.name], canvas = c0 * k, u = now - e.cue.a, ph = (e.cue.h % 1000) / 1000 * Math.PI * 2;
    const p = Math.min(1, u / EYE.rise), f = easeOutBack(p), settle = 1 - p;
    const restY = top - EYE.gap * em - canvas * (EYE.vis / 2 + 6 / 512), startY = ROW - (GA.ascent - GA.H / 2) * sc;   // the picture's bottom a quarter em over the glyph tops (the canvas centres its picture 6/512 up); from behind the letters' middle
    const y = startY + (restY - startY) * f, xx = x + settle * 0.1 * canvas * Math.sin(2 * Math.PI * 4.5 * u + ph);
    const rot = settle * 0.17 * Math.sin(2 * Math.PI * 4.5 * u + ph) + 0.035 * Math.sin(2 * Math.PI * 2 * u + ph), al = st.al * Math.min(1, u / 0.1);   // ~10° wiggle on the way up, 2° wobble at 2 Hz after
    if (Math.hypot(xx - FCX, y - FCY) < FR + canvas / 2) continue;                                                       // the shield: never over her face (moot on the caption row, kept)
    const L = pickMip(im, canvas); spriteO(L, xx, y, (0.7 + 0.3 * f) * canvas / L.w, rot, al); } };                   // the scale is the LEVEL's

// ── drawing on the rgb24 frame buffer ──
let fb = null;
const px = (x, y, r, g, b, a = 1) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3;
  fb[o] = Math.min(255, fb[o] * (1 - a) + r * a); fb[o + 1] = Math.min(255, fb[o + 1] * (1 - a) + g * a); fb[o + 2] = Math.min(255, fb[o + 2] * (1 - a) + b * a); };
const add = (x, y, r, g, b, a) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3, k = a * (1 - M[y * W + x]);   // the room only
  fb[o] = Math.min(255, fb[o] + r * k); fb[o + 1] = Math.min(255, fb[o + 1] + g * k); fb[o + 2] = Math.min(255, fb[o + 2] + b * k); };
const glow = (x, y, rad, r, g, b, a) => { const R2 = rad * rad; for (let j = -rad; j <= rad; j++) for (let i = -rad; i <= rad; i++) { const d2 = i * i + j * j; if (d2 > R2) continue; add(x + i, y + j, r, g, b, a * (1 - d2 / R2) ** 2); } };
// the same, on the output buffer (the lyric layer)
const pxO = (x, y, r, g, b, a = 1) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= OW || y >= OH) return; const o = (y * OW + x) * 3;
  ob[o] = Math.min(255, ob[o] * (1 - a) + r * a); ob[o + 1] = Math.min(255, ob[o + 1] * (1 - a) + g * a); ob[o + 2] = Math.min(255, ob[o + 2] * (1 - a) + b * a); };
const blobO = (cx, cy, rx, ry, r, g, b, a = 1) => { for (let y = Math.floor(cy - ry - 1); y <= cy + ry + 1; y++) for (let x = Math.floor(cx - rx - 1); x <= cx + rx + 1; x++) {
  const d = Math.hypot((x + 0.5 - cx) / rx, (y + 0.5 - cy) / ry) * Math.min(rx, ry); const k = Math.min(1, Math.max(0, Math.min(rx, ry) - d + 0.75)); if (k > 0) pxO(x, y, r, g, b, a * k); } };
const glyphO = (layer, g, cx, cy, sc, rot, r, gg, b, a) => { const w = g.w, h = GA.H, cs = Math.cos(rot), sn = Math.sin(rot), rad = Math.hypot(w, h) * sc / 2;
  for (let Y = Math.floor(cy - rad); Y <= cy + rad; Y++) for (let X = Math.floor(cx - rad); X <= cx + rad; X++) {
    const dx = X - cx, dy = Y - cy, u = (dx * cs + dy * sn) / sc + w / 2, v = (-dx * sn + dy * cs) / sc + h / 2;
    if (u < 0 || v < 0 || u >= w - 1 || v >= h - 1) continue; const u0 = u | 0, v0 = v | 0, fu = u - u0, fv = v - v0, o = v0 * GA.W + g.x + u0;
    const c = (layer[o] * (1 - fu) + layer[o + 1] * fu) * (1 - fv) + (layer[o + GA.W] * (1 - fu) + layer[o + GA.W + 1] * fu) * fv;
    if (c > 2) pxO(X, Y, r, gg, b, a * c / 255); } };
// the composite, up to the output size (bilinear), with two things done to the picture on the way up — both are
// per-pixel SOURCE OFFSETS inside the same bilinear pass, so a frame with neither costs nothing extra:
//
// the kiss glitch (v103 zoom-bumped it; v120, jeffrey: "we don't need so much of a zoom glitch, maybe just a bit of
// color separation would do"): no zoom, no jump — each grain of the ki-ki-ki (the receipt's stutter events) SPLITS the
// colour, the red channel pulled a few px one way and the blue the other (hashed direction per grain, 2–6 px at 1080),
// held until the next grain, a slightly bigger split landing on the downbeat and easing out over 0.4 s; chorus 2's
// downbeat gets a smaller one
const STUT = (R.events || []).filter((e) => e.voice === "stutter").map((e) => e.t - T0).sort((a, b) => a - b);
const BUMPS = [{ t: 38.444, steps: STUT.filter((t) => t > 38.0 && t < 38.444), amt: 1 }, { t: 84.898, steps: [84.898 - 0.12, 84.898 - 0.06], amt: 0.55 }];   // only the grains inside the stop (jeffrey: the early ones "unnecessary")
const splitOf = (t, amt) => { const h = fnv(`split:${t.toFixed(3)}`), px = (2 + 4 * ((h >>> 4) % 100) / 100) * amt, th = (((h >>> 12) % 100) / 100 - 0.5) * 1.4 + (((h >>> 20) & 1) ? Math.PI : 0);   // mostly sideways, either way, a little tilt
  return { dx: px * Math.cos(th), dy: px * Math.sin(th) }; };
let SPLIT = { dx: 0, dy: 0 };                                                 // the red channel's offset in 1080 px; blue gets the opposite
const splitAt = (rn) => { SPLIT = { dx: 0, dy: 0 }; for (const b of BUMPS) { const d = rn - b.t;
  if (d < 0) { let last = null; for (const t of b.steps) if (rn >= t) last = t; if (last != null) SPLIT = splitOf(last, b.amt); }
  else if (d < 0.4) { const f = d / 0.4, k = (1 - f) * (1 - f), s = splitOf(b.t, b.amt), m = Math.hypot(s.dx, s.dy), big = 6 * b.amt * k / m; SPLIT = { dx: s.dx * big, dy: s.dy * big }; } } };   // the downbeat: the top of the grains' range, in its own hashed direction
// the arrangement's dance (v103: "start formal, black and white, less shaky; as the ornament comes in, increase the shake
// and the colour, so the captions surprise the way the arrangement does"): 0 through the intro and verse 1, a first step
// when the kick lands, half at chorus 1, full from chorus 2, resting in the break, back up through the bridge; each step
// a 2 s ramp. The captions, the lights and now the twangs' ripple all ride it.
const DANCE = [[-99, 0], [2.1, 0], [10.8, 0.15], [23.6, 0.2], [38.4, 0.55], [69.1, 0.35], [84.9, 1], [115.3, 0.3], [124.8, 0.7], [139.9, 1]];
const danceAt = (rn) => { let dance = 0; for (let i = 1; i < DANCE.length; i++) { const [t0, d0] = DANCE[i - 1], [t1, d1] = DANCE[i]; if (rn >= t0 && rn < t1) { dance = d0 + (d1 - d0) * Math.min(1, (rn - t0) / 2.0); break; } if (rn >= t1) dance = d1; } return dance; };
// the twangs (v120, jeffrey: "I wonder if the guitar parts could somehow warble the edges of the video — add a bit of
// motion blur / displacement / ripple — that'd be really nice for the twangs specifically"; then "attached to every
// twang", "only come in post chorus, not in the intro", "get more extreme like the other effects"): every strum she
// plays — bin/strum-onsets.py's src/strums.json, onsets off the guitar stem (the receipt's gtr events are one a bar;
// they are the fallback) — sends a ripple in from the FRAME EDGES: a sine along the distance to each edge (wavelength
// ~50 px, travelling inward at ~300 px/s), weighted (1 - d/B)² over the outer 17 % of the frame and zero inside it (her
// face is never touched), decaying over ~350 ms. Its amplitude is the dance × the strum's strength: nothing before
// chorus 1, ~3–5 px there, the full 10 px by chorus 2 and the finale. The four edges each contribute along their own
// normal, summed, so the corners stay continuous — one 1-D table per axis per frame. While it rings, a light motion
// blur near the edges only: a share of the previous output frame, by the same edge weight.
const STRUMS_FILE = resolve(LANE, "src/strums.json");
const STRUMS = existsSync(STRUMS_FILE) ? JSON.parse(readFileSync(STRUMS_FILE, "utf8")).map((o) => ({ t: o.t, g: o.gain })).sort((a, b) => a.t - b.t) : pick("gtr");
const RIP_FROM = 38.4;                                                        // record s: chorus 1; before it the strums leave the picture alone
const RIP = { amp: 10, band: 0.17, lambda: 50, speed: 300, tau: 0.15, len: 0.45, blur: 0.2 }, RING = [];   // px at 1080 (× dance × strength), the band's share of the frame, px, px/s, s, s, the ghost's share
const RX = new Float32Array(OW), RY = new Float32Array(OH), BX = new Float32Array(OW), BY = new Float32Array(OH);   // the displacement and the edge weight, per column / per row
let ripOn = 0, PREV = null, prevFi = -2;
// the held 'long's (v120, jeffrey: "get really cool on the held 'long' rainbow times"): while the lyric runs its rainbow
// and the room steps hue to the beat, the PICTURE goes too — slow waves rolling in from the edges at full amplitude,
// the whole frame swimming outward from her face (a radial wave, a breath per beat; the one place she may move), a
// chromatic split that breathes with the beat in a slowly turning direction, and feedback trails that bloom outward
// (the last frame pulled back toward her and kept). In over 0.4 s from where the rainbow starts, out within 0.5 s of
// the word's end (the second ends at 122.0; "And we can run" follows).
const LONGS = WORDS.filter((w) => /^long\W*$/i.test(w.text) && w.b - w.a > 1.2);
const LONG = { k: 0, cx: 0, cy: 0, a: 0, w: 0, ph: 0, trail: 0, zoom: 1 };
const longAt = (now, beatF) => { let k = 0; for (const w of LONGS) { const a = w.a + 0.25; if (now < a || now > w.b + 0.5) continue; k = Math.max(k, now < w.b ? Math.min(1, (now - a) / 0.4) : 1 - (now - w.b) / 0.5); }
  k = Math.max(0, k); k = k * k * (3 - 2 * k); LONG.k = k; if (!k) return; const S = OH / 1080;
  LONG.cx = faceX * OW / W; LONG.cy = faceY * OH / H;                                                                  // the swim's centre: her face, in output px
  const breath = 0.6 + 0.4 * Math.cos(Math.PI * beatF) ** 2;                                                           // fullest on the beat, easing through it
  LONG.a = 7 * S * k * breath; LONG.w = 2 * Math.PI / (230 * S); LONG.ph = -LONG.w * 90 * S * now;                   // the radial wave: 7 px, λ 230 px, outward at 90 px/s
  LONG.trail = 0.5 * k; LONG.zoom = 1 - 0.012 * k; };                                                                 // the feedback: half the last frame, pulled 1.2 % toward her each frame
const rippleAt = (now) => { RING.length = 0; const S = OH / 1080, B = RIP.band * Math.min(OW, OH), rn = now - T0;
  for (let i = STRUMS.cur || 0; i < STRUMS.length && STRUMS[i].t <= now; i++) { const dt = now - STRUMS[i].t; if (dt > RIP.len) { if (i === (STRUMS.cur || 0)) STRUMS.cur = i + 1; continue; } if (rn >= RIP_FROM) RING.push({ dt, g: STRUMS[i].g }); }
  ripOn = 0; const waves = [];
  if (RING.length) { const dance = danceAt(rn), w2 = 2 * Math.PI / (RIP.lambda * S);
    for (const r of RING) { const e = Math.exp(-r.dt / RIP.tau) * Math.min(1, (RIP.len - r.dt) / 0.05) * dance; ripOn = Math.max(ripOn, e); waves.push({ a: RIP.amp * S * (0.35 + 0.65 * r.g) * e, w: w2, ph: -w2 * RIP.speed * S * r.dt }); } }
  if (LONG.k > 0) { const w2 = 2 * Math.PI / (110 * S); ripOn = Math.max(ripOn, LONG.k); waves.push({ a: RIP.amp * S * LONG.k, w: w2, ph: -w2 * 90 * S * now }); }   // the long: a slow wave, full size, rolling in
  if (!waves.length) return;
  const f = (d) => { if (d >= B) return 0; const w = (1 - d / B) ** 2; let s = 0; for (const q of waves) s += q.a * Math.sin(q.w * d + q.ph); return s * w; }, wgt = (d) => d >= B ? 0 : (1 - d / B) ** 2;
  for (let x = 0; x < OW; x++) { const dL = x + 0.5, dR = OW - 0.5 - x; RX[x] = f(dL) - f(dR); BX[x] = Math.min(1, wgt(dL) + wgt(dR)); }
  for (let y = 0; y < OH; y++) { const dT = y + 0.5, dB = OH - 0.5 - y; RY[y] = f(dT) - f(dB); BY[y] = Math.min(1, wgt(dT) + wgt(dB)); } };
const ZB = Buffer.alloc(W * H * 3);
const upscale = (fi) => { const rip = ripOn > 0.002, lng = LONG.k > 0.002, spl = Math.abs(SPLIT.dx) + Math.abs(SPLIT.dy) > 0.05; if (!OB && !rip && !spl && !lng) { ob = fb; return; } ob = OB || ZB;
  const fx = W / OW, fy = H / OH, S = OH / 1080, sdx = SPLIT.dx * S * fx, sdy = SPLIT.dy * S * fy;         // the split, in base px
  const held = PREV && prevFi === fi - 1, blur = rip && held ? RIP.blur * ripOn : 0, trail = lng && held ? LONG.trail : 0;   // ghosts only from a frame that was kept
  const bl = (c, u, v) => { const u0 = u | 0, v0 = v | 0, fu = u - u0, fv = v - v0, a = (v0 * W + u0) * 3 + c, b = a + W * 3; return (fb[a] * (1 - fu) + fb[a + 3] * fu) * (1 - fv) + (fb[b] * (1 - fu) + fb[b + 3] * fu) * fv; };
  const cu = (u) => Math.min(W - 1.001, Math.max(0, u)), cv = (v) => Math.min(H - 1.001, Math.max(0, v));
  const LCX = LONG.cx, LCY = LONG.cy, LA = LONG.a, LW = LONG.w, LPH = LONG.ph, LZ = LONG.zoom;
  // the face shield (v120, jeffrey: "make sure there is no glitch on her face"): inside a disc on the matte's head nothing
  // moves, splits, blurs or trails — every offset and ghost is scaled by (1 - shield), a 60 px smoothstep at the rim
  const FCX = faceX * OW / W, FCY = faceY * OH / H, FR = 0.2 * OH, FS = 60 * S, FE = FR + FS;
  for (let y = 0; y < OH; y++) { const vy = y + 0.5, q = y * OW * 3, ry = rip ? RY[y] : 0, by = rip ? BY[y] : 0;
    const ady = Math.abs(vy - FCY);
    for (let x = 0; x < OW; x++) { const o = q + x * 3; let keep = 1; const adx = Math.abs(x + 0.5 - FCX);
      if (ady < FE && adx < FE) { const d = Math.sqrt(adx * adx + ady * ady); if (d <= FR) keep = 0; else if (d < FE) { const t = (d - FR) / FS; keep = t * t * (3 - 2 * t); } }
      let X = x + 0.5 + (rip ? RX[x] * keep : 0), Y = vy + ry * keep;
      if (lng && keep) { const dx = X - LCX, dy = Y - LCY, r = Math.sqrt(dx * dx + dy * dy) + 1e-3, sw = LA * keep * Math.sin(LW * r + LPH); X += dx / r * sw; Y += dy / r * sw; }
      const u = cu(X * fx - 0.5), v = cv(Y * fy - 0.5);
      if (spl) { const kx = sdx * keep, ky = sdy * keep; ob[o] = bl(0, cu(u + kx), cv(v + ky)); ob[o + 1] = bl(1, u, v); ob[o + 2] = bl(2, cu(u - kx), cv(v - ky)); }
      else { const u0 = u | 0, v0 = v | 0, fu = u - u0, fv = v - v0, a = (v0 * W + u0) * 3, b = a + W * 3;
        for (let c = 0; c < 3; c++) ob[o + c] = (fb[a + c] * (1 - fu) + fb[a + 3 + c] * fu) * (1 - fv) + (fb[b + c] * (1 - fu) + fb[b + 3 + c] * fu) * fv; }
      if (trail && keep) { const px_ = Math.min(OW - 1.001, Math.max(0, LCX + (x + 0.5 - LCX) * LZ - 0.5)), py_ = Math.min(OH - 1.001, Math.max(0, LCY + (y + 0.5 - LCY) * LZ - 0.5)), u0 = px_ | 0, v0 = py_ | 0, fu = px_ - u0, fv = py_ - v0, a = (v0 * OW + u0) * 3, b = a + OW * 3;
        for (let c = 0; c < 3; c++) { const g = (PREV[a + c] * (1 - fu) + PREV[a + 3 + c] * fu) * (1 - fv) + (PREV[b + c] * (1 - fu) + PREV[b + 3 + c] * fu) * fv; ob[o + c] += (g - ob[o + c]) * trail * keep; } }
      else if (blur && keep) { const k = blur * keep * Math.max(by, BX[x]); if (k > 0.002) { ob[o] += (PREV[o] - ob[o]) * k; ob[o + 1] += (PREV[o + 1] - ob[o + 1]) * k; ob[o + 2] += (PREV[o + 2] - ob[o + 2]) * k; } } } }
  if (rip || lng) { if (!PREV) PREV = Buffer.alloc(OW * OH * 3); ob.copy(PREV); prevFi = fi; } };
// a wise sharpen on the delivered picture, before the words (v103: "a post smart sharpen over all the video so it's all
// crisp"): unsharp on luma only (no colour fringing), the amount gated by local contrast — flat skin, wall and bokeh get
// almost none, edges and texture get the full dose — and a clamp so no halo overshoots its neighbours by more than a step
const SHARP = { amount: 0.9, radius: 1, lo: 6, hi: 40, clamp: 28 }, SB = Buffer.alloc(0);
let LUM = null, BLR = null;
const sharpen = () => { if (ob === fb) return;                                       // (never on the raw composite when nothing was rescaled)
  const n = OW * OH; if (!LUM || LUM.length !== n) { LUM = new Float32Array(n); BLR = new Float32Array(n); }
  for (let i = 0, p = 0; i < n; i++, p += 3) LUM[i] = 0.299 * ob[p] + 0.587 * ob[p + 1] + 0.114 * ob[p + 2];
  for (let y = 0; y < OH; y++) { const r = y * OW; BLR[r] = LUM[r]; BLR[r + OW - 1] = LUM[r + OW - 1]; for (let x = 1; x < OW - 1; x++) BLR[r + x] = (LUM[r + x - 1] + 2 * LUM[r + x] + LUM[r + x + 1]) * 0.25; }
  for (let x = 0; x < OW; x++) { LUM[x] = BLR[x]; LUM[(OH - 1) * OW + x] = BLR[(OH - 1) * OW + x]; }
  for (let y = 1; y < OH - 1; y++) for (let x = 0; x < OW; x++) { const o = y * OW + x; LUM[o] = (BLR[o - OW] + 2 * BLR[o] + BLR[o + OW]) * 0.25; }   // LUM is now the blur
  for (let y = 1; y < OH - 1; y++) for (let x = 1; x < OW - 1; x++) { const o = y * OW + x, p = o * 3, l = 0.299 * ob[p] + 0.587 * ob[p + 1] + 0.114 * ob[p + 2], d = l - LUM[o];
    const ad = Math.abs(d); if (ad < 0.5) continue;
    const gate = Math.min(1, Math.max(0, (ad * 4 - SHARP.lo) / (SHARP.hi - SHARP.lo)));                  // local contrast: how much detail is really here
    let add = d * SHARP.amount * gate; if (add > SHARP.clamp) add = SHARP.clamp; else if (add < -SHARP.clamp) add = -SHARP.clamp;
    if (add === 0) continue; const k = (l + add) / (l + 1e-3);
    ob[p] = Math.max(0, Math.min(255, ob[p] * k)); ob[p + 1] = Math.max(0, Math.min(255, ob[p + 1] * k)); ob[p + 2] = Math.max(0, Math.min(255, ob[p + 2] * k)); } };
// a filled ellipse with a 1.5 px antialiased edge, over everything (the ball)
const blob = (cx, cy, rx, ry, r, g, b, a = 1) => { for (let y = Math.floor(cy - ry - 1); y <= cy + ry + 1; y++) for (let x = Math.floor(cx - rx - 1); x <= cx + rx + 1; x++) {
  const d = Math.hypot((x + 0.5 - cx) / rx, (y + 0.5 - cy) / ry) * Math.min(rx, ry); const k = Math.min(1, Math.max(0, Math.min(rx, ry) - d + 0.75)); if (k > 0) px(x, y, r, g, b, a * k); } };
// a glyph from the atlas, blitted scaled and tilted (bilinear), one coverage layer in one colour
const glyph = (layer, g, cx, cy, sc, rot, r, gg, b, a) => { const w = g.w, h = GA.H, cs = Math.cos(rot), sn = Math.sin(rot), rad = Math.hypot(w, h) * sc / 2;
  for (let Y = Math.floor(cy - rad); Y <= cy + rad; Y++) for (let X = Math.floor(cx - rad); X <= cx + rad; X++) {
    const dx = X - cx, dy = Y - cy, u = (dx * cs + dy * sn) / sc + w / 2, v = (-dx * sn + dy * cs) / sc + h / 2;
    if (u < 0 || v < 0 || u >= w - 1 || v >= h - 1) continue; const u0 = u | 0, v0 = v | 0, fu = u - u0, fv = v - v0, o = v0 * GA.W + g.x + u0;
    const c = (layer[o] * (1 - fu) + layer[o + 1] * fu) * (1 - fv) + (layer[o + GA.W] * (1 - fu) + layer[o + GA.W + 1] * fu) * fv;
    if (c > 2) px(X, Y, r, gg, b, a * c / 255); } };

let tint = [1, 1, 1], hue = [240, 200, 110], amb = ENERGY.intro;        // glide state
// the room's state, per frame: ambient eased toward the section's energy, tint and hue eased toward the chord's
function glide(now) { const E = energyAt(now); amb += (E - amb) * Math.min(1, (1 / FPS) / 1.6);
  const c = chordAt(now), want = (c && TINT[c.chord]) || [0.92, 0.88, 0.86], wantHue = (c && HUE[c.chord]) || [240, 200, 110];
  for (let i = 0; i < 3; i++) { tint[i] += (want[i] - tint[i]) * Math.min(1, (1 / FPS) / 0.7); hue[i] += (wantHue[i] - hue[i]) * Math.min(1, (1 / FPS) / 0.7); } return c; }
const CAM_MOVE = 178.24;                                                  // take seconds: she picks the camera up; the room's lights are nowhere after that
function drawFrame(fi) {
  const now = T0 + FROM + fi / FPS, take = takeOf(now); matteAt(take); repairMatte(take); softenMatte(); if (STAGE) depthAt(take); findFace(); const c = glide(now);
  // the relight arrives with the arrangement (v103: "the opening should look naturally lit too"): nothing through the intro,
  // rising from the kick's entrance (10.8 s) to full at chorus 1 (38.4 s); and a 2 s dissolve back to the plain picture
  // before she reaches for the camera
  const arrive = Math.min(1, Math.max(0, (now - T0 - 10.8) / (38.4 - 10.8))), arriveS = arrive * arrive * (3 - 2 * arrive);
  // the dance (v103: "start formal, black and white, less shaky; as the ornament comes in, increase the shake and the
  // colour, so the captions surprise the way the arrangement does"): 0 through the intro and verse 1, a first step
  // when the kick lands, half at chorus 1, full from chorus 2, resting in the break, back up through the bridge
  const rn = now - T0, dance = danceAt(rn);
  const lightsOn = arriveS * (1 - Math.min(1, Math.max(0, (take - (CAM_MOVE - 2.3)) / 2.0)));
  const toBlack = 0;                                                                                  // no dip to black: jeffrey likes her moving the camera; the effects just dissolve off first
  const room = 1 + (0.72 + 0.28 * amb - 1) * lightsOn;                    // v103: a simple grade — the room breathes with the sections, never goes out; gone by the end
  const lampBloom = Math.min(0.5, env(KICKS, now, 0.08, 0.03)) * lightsOn, lampOn = (0.12 * amb + 0.2 * lampBloom) * lightsOn;      // a burst, not a whiteout
  const bellRaw = env(BELLS, now, 1.4, 0.1), windowGlow = Math.min(0.15, 0.08 * bellRaw) * lightsOn, windowOn = (0.15 + 0.1 * amb) * lightsOn;
  const padLevel = Math.min(1, sounding(PADS, now) * 0.6) * lightsOn;
  // a held 'long' (the rainbow word): the room's lights flicker through the same colours (v103)
  const arpEnv = Math.min(1, env(ARPS, now, 0.8) * 0.35), longW = WORDS.find((w) => /^long\W*$/i.test(w.text) && w.b - w.a > 1.2 && now >= w.a + 0.25 && now < w.b);
  const longOn = longW && arpEnv > 0.05 ? Math.min(1, arpEnv * 3) * lightsOn * 0.6 : 0, flickHz = 6 + 10 * arpEnv;   // the word keeps its blink; the room steps TO THE BEAT, subtly (jeffrey)
  const beatOf = (() => { const b = chordAt(now); if (!b) return { k: 0, f: 0 }; const per = b.dur / 4, k = Math.floor((now - b.t) / per), f = ((now - b.t) % per) / per; return { k: b.n * 4 + k, f }; })();
  const roomHue = (i) => wheel(((beatOf.k + i) % 12) / 12), roomPulse = 0.75 + 0.25 * (1 - beatOf.f) * (1 - beatOf.f);   // a new colour each beat, a soft pulse on the beat
  // every bulb its own (jeffrey: "variable in brightness, flicker with the lyric, not ALL lit"): a hashed brightness 0.25–1,
  // and each bulb follows the word's blink in its own phase, so at any instant a third of the string is up, the rest low
  const bulb = (i) => { const h = fnv(`bulb:${i}`), bright = 0.25 + 0.75 * ((h >>> 8) % 100) / 100, ph = ((h >>> 16) % 100) / 100;
    const blink = 0.5 + 0.5 * Math.sin(2 * Math.PI * (now * flickHz / 2 + ph)); return bright * (0.35 + 0.65 * Math.pow(blink, 3)); };
  const wheel = (ph) => { const hh = ((ph % 1) + 1) % 1, i6 = (hh * 6) | 0, fr = hh * 6 - i6, Q = 255 * (1 - 0.9 * fr), T = 255 * (1 - 0.9 * (1 - fr)), lo = 25; return [[255, T, lo], [Q, 255, lo], [lo, 255, T], [lo, Q, 255], [T, lo, 255], [255, lo, Q]][i6]; };
  if (!LYRIC_ONLY) { coverGrade(arriveS);
  // 1. the room: multiplicative gain (ambient × tint, lifted by the lamp's pool and the window), her kept close to natural
  const T = tint.map((t) => 1 + (0.55 + 0.45 * t - 1) * lightsOn);          // the chord's tint at half strength, gone by the end
  const fgGain = 0.92 + 0.08 * room, fgTint = T.map((t) => 0.8 + 0.2 * t);   // she stays nearly natural, just a little of the room's colour
  const spotR2 = (0.2 * W) ** 2, spotK = STAGE ? 0.1 * lightsOn : 0, stageK = lightsOn;    // the face spot and the bump key arrive with the rest
  for (let o = 0, p = 0, y = 0, x = 0; o < W * H; o++, p += 3, x = ++x === W ? (y++, 0) : x) { const m = M[o], lamp = LAMP[o], win = WINDOW[o];
    const g = room + lampOn * lamp + windowOn * win * (1 - room) + windowGlow * win * 0.6;
    const dx = x - faceX, dy = y - faceY, spot = 1 + spotK * Math.max(0, 1 - (dx * dx + dy * dy) / spotR2), fg = fgGain * spot * (STAGE ? 1 + (DN[o] - 1) * stageK : 1);
    const gr = (g * T[0] + lampBloom * lamp * 0.1) * (1 - m) + fg * fgTint[0] * m, gg = (g * T[1] + lampBloom * lamp * 0.08) * (1 - m) + fg * fgTint[1] * m, gb = (g * T[2] + lampBloom * lamp * 0.06) * (1 - m) + fg * fgTint[2] * m;
    fb[p] = Math.min(255, fb[p] * gr); fb[p + 1] = Math.min(255, fb[p + 1] * gg); fb[p + 2] = Math.min(255, fb[p + 2] * gb); }
  // 2. the lamp's bloom and the window's glow, added to the room
  // the ceiling flicker: a radial halo on the wall and ceiling BEHIND HER HEAD (jeffrey: "radial, behind her head, so it's
  // more related"), its colour the beat's, pulsing with it; the room only (the matte keeps it off her)
  const kickHalo = Math.min(1, env(KICKS, now, 0.12, 0.02)) * (0.04 + 0.26 * dance) * lightsOn;         // the kick pumps it: a whisper at the start, hard by the choruses (jeffrey)
  if (longOn > 0 || kickHalo > 0.01) { const c = longOn > 0 ? roomHue(6) : hue.map((h) => h * 0.9 + 25), k = Math.max(0.22 * longOn * roomPulse, kickHalo), R0 = (0.26 + 0.08 * kickHalo) * W, R2 = R0 * R0, hx = faceX, hy = faceY - 0.06 * H;
    const y0 = Math.max(0, hy - R0) | 0, y1 = Math.min(H - 1, hy + R0) | 0, x0 = Math.max(0, hx - R0) | 0, x1 = Math.min(W - 1, hx + R0) | 0;
    for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) { const d2 = (x - hx) ** 2 + (y - hy) ** 2; if (d2 > R2) continue; const o = y * W + x, a = k * (1 - d2 / R2) ** 2 * (1 - M[o]), p = o * 3;
      fb[p] = Math.min(255, fb[p] + c[0] * a); fb[p + 1] = Math.min(255, fb[p + 1] + c[1] * a); fb[p + 2] = Math.min(255, fb[p + 2] + c[2] * a); } }
  if (lampBloom > 0.02) { const L = LIGHTS.lamp, lx = (L.x - VX) * sx, ly = (L.y - VY) * sy, R0 = Math.round(L.r * 1.6 * sx); glow(lx, ly, R0, 255, 228, 180, 0.18 * lampBloom); glow(lx, ly, Math.round(R0 * 2.4), 255, 220, 170, 0.04 * lampBloom); }
  if (windowGlow > 0.02) { const Wn = LIGHTS.window; for (let y = Math.max(0, (Wn.y0 - VY) * sy); y < Math.min(H, (Wn.y1 - VY) * sy); y += 2) for (let x = Math.max(0, (Wn.x0 - VX) * sx); x < Math.min(W, (Wn.x1 - VX) * sx); x += 2) { const k = WINDOW[(y | 0) * W + (x | 0)] * 0.12 * windowGlow; add(x, y, 225, 238, 255, k); add(x + 1, y, 225, 238, 255, k); add(x, y + 1, 225, 238, 255, k); add(x + 1, y + 1, 225, 238, 255, k); } }
  // 3. the fairy lights: a bead of light each, glowing with the pads, a handful flashing on each hat
  const flash = new Float32Array(FAIRY.length); for (let i = HATS.cur || 0; i < HATS.length && HATS[i].t <= now; i++) { const dt = now - HATS[i].t; if (dt < 0.35) for (const p of HATS[i].pts) flash[p] = Math.max(flash[p], HATS[i].g * Math.exp(-dt / 0.07)); }
  if (lightsOn > 0) for (const f of FAIRY) { const base = (0.08 + 0.3 * padLevel + 0.25 * amb) * lightsOn, fl = flash[f.i] * 1.2 * lightsOn;
    let cr = 170, cg = 150, cb = 255; if (longOn > 0) { const c = roomHue((f.i / 6) | 0), k = longOn * roomPulse * bulb(f.i); cr += (c[0] - cr) * k; cg += (c[1] - cg) * k; cb += (c[2] - cb) * k; glow(f.x, f.y, Math.round(11 * sx), c[0], c[1], c[2], 0.6 * k); glow(f.x, f.y, Math.round(4 * sx), c[0], c[1], c[2], 0.5 * k); }   // a bit more colour shooting off each bulb
    glow(f.x, f.y, Math.round(7 * sx), cr, cg, cb, 0.5 * base); glow(f.x, f.y, 2, 235, 230, 255, 0.7 * base);
    if (fl > 0.02) { glow(f.x, f.y, Math.round(12 * sx), 210, 200, 255, 0.6 * fl); glow(f.x, f.y, 3, 255, 255, 255, 0.9 * fl); } }
  }
  // 4. the singalong: the chunk being sung (and the one leaving); letters fill in syllable by syllable, the sung
  //    syllable pops, the sung word a touch larger, a held syllable kerns outward and sways; the ball rides its one path.
  splitAt(now - T0); longAt(now, beatOf.f); if (LONG.k > 0) { const th = now * 0.7, m = 4 * LONG.k * (0.4 + 0.6 * Math.cos(Math.PI * beatOf.f) ** 2); SPLIT = { dx: SPLIT.dx + m * Math.cos(th), dy: SPLIT.dy + m * Math.sin(th) }; }   // the long's split: up to 4 px, breathing with the beat, turning
  rippleAt(now); upscale(fi); sharpen();                                       // the room is done at the base size, upscaled (the twangs' ripple, the kiss's split, the long's swim), SHARPENED, then the words go on
  if (!NO_LYRIC) { const k = Math.min(1, 0.3 + 0.7 * amb) * (1 + 0.5 * lampBloom), fl = Math.min(1, bellRaw) * 120, SH = hue.map((h) => Math.min(255, (h * k + fl) * dance)), off = 0.07 * GA.px * (CHUNKS[0]?.sc ?? SC_MAX);   // the shadow tight under the glyph (v103: "too far from the captions")
    // the "longs" (v103: "when arpeggiating should blink colors rapidly — psychic effects"): a held syllable's letters run
    // the hue wheel, each letter a step behind the last, blinking at 6 Hz, 16 Hz with the arp under it
    const arp = Math.min(1, env(ARPS, now, 0.8) * 0.35), psyHz = 6 + 10 * arp;   // an envelope (0.8 s decay): the rainbow rides through the thinner arpeggio in the break
    const psychic = (i, wild) => { const hh = (now * psyHz / 6 + i * 0.13) % 1, i6 = (hh * 6) | 0, fr = hh * 6 - i6, Q = 255 * (1 - 0.9 * fr), T = 255 * (1 - 0.9 * (1 - fr)), lo = 25;
      const c = [[255, T, lo], [Q, 255, lo], [lo, 255, T], [lo, Q, 255], [T, lo, 255], [255, lo, Q]][i6], blink = 0.55 + 0.45 * (Math.sin(2 * Math.PI * now * psyHz + i * 0.9) > 0 ? 1 : 0.35);
      return c.map((v) => Math.round((250 * (1 - wild) + v * blink * wild))); };
    const easeIn = CHUNKS.length ? Math.min(1, Math.max(0, (now - CHUNKS[0].inA) / 2.5)) : 1;          // the captions ease in over their first line
    const states = new Map(); for (const C of CHUNKS) { const st = chunkState(C, now); if (st) states.set(C, { al: st.al * easeIn, dx: st.dx }); }
    for (const [C, st] of states) { const al = st.al, sc = C.sc, em = GA.px * sc;
      if (FIA) drawEyeconsOn(C, st, now);                                                 // Fia's Cut: the row's eyecons, under its letters
      // the words stay put (v103: "the other words need to stay put"); a held word kerns out about its own centre, by the dance
      const wildOf = (w) => w.w ? dance * Math.max(0, ...w.w.syl.map((t) => now >= t.a && now < t.b && t.b - t.a > 0.6 && now - t.a > 0.25 ? Math.abs(Math.sin(Math.PI * (now - t.a - 0.25) * 3.2)) : 0)) : 0;
      for (const [wi, w] of C.words.entries()) { const timed = !!w.w, wordOn = timed && now >= w.w.a, cur = wordOn && now < w.w.b + 0.1;
        const prevOn = timed ? false : (() => { const p = C.words[wi - 1]; return p && p.w ? now >= p.w.b : wordOn; })();
        const wildW = wildOf(w), kern = wildW * 0.05 * GA.px; let pen = w.x + st.dx - kern * sc * (w.gl.length - 1) / 2;
        for (const q of w.gl) { const gg = q.g, t = timed ? w.w.syl[q.si] : null;
          const lit = t ? t.a + Math.min(0.6, (t.b - t.a) * 0.8) * q.sk / q.sn : 1e9, on = timed ? now >= lit : prevOn;   // the last letter lit by 80% of the syllable
          const held = t && now >= t.a && now < t.b && t.b - t.a > 0.6 && now - t.a > 0.25, wild = held ? dance * Math.abs(Math.sin(Math.PI * (now - t.a - 0.25) * 3.2)) : 0;
          const sylPop = t && now >= t.a ? 1 + 0.14 * dance * Math.max(0, 1 - (now - t.a) / 0.15) : 1, chPop = on && t ? 1 + (0.04 + 0.08 * dance) * Math.max(0, 1 - (now - lit) / 0.1) : 1;
          const s2 = sc * sylPop * chPop * (cur ? 1 + (0.06 + 0.05 * lampBloom) * dance : 1) * (1 + 0.12 * wild), sway = 0.6 * dance * Math.sin(2 * Math.PI * now / q.per + q.ph);
          const lat = wild * em * 0.05 * Math.sin(now * 23 + q.ph * 5), cx = pen + gg.adv * sc / 2 + lat, cy = ROW - (GA.ascent - GA.H / 2) * s2 + (q.jy * dance + sway) * sc - wild * em * 0.07 * Math.sin(now * 40 + q.ph * 7), gx = cx + (gg.w / 2 + gg.dx - gg.adv / 2) * s2, rot = q.rot * dance * (1 + 2.5 * wild);
          const isLong = /^long\W*$/i.test(w.tok) && w.w && w.w.b - w.w.a > 1.2, fill = on ? (isLong && cur && arp > 0.05 ? psychic(w.gl.indexOf(q), Math.min(1, arp * 3)) : WHITE) : GREY;   // the rainbow: only the long 'long's (held > 1.2 s), under the arpeggio
          glyphO(GOUTER, gg, gx + off, cy + off, s2, rot, ...SH, al); glyphO(GOUTER, gg, gx, cy, s2, rot, ...BLACK, al); glyphO(GFILL, gg, gx, cy, s2, rot, ...fill, al);
          pen += gg.adv * sc + kern * sc; } } }
    // the ball: between landing j and j+1 it sits DWELL then flies one gravity arc, each end riding its own chunk's slide
    if (BALL && LANDS.length && now >= CHUNKS[0].inA && now < LANDS.at(-1).b + 0.5) {
      let j = -1; for (let i = 0; i < LANDS.length; i++) { if (LANDS[i].t <= now) j = i; else break; }
      const sc0 = (j < 0 ? LANDS[0] : LANDS[j]).C.sc, cap = GA.ascent * 0.72 * sc0, br = Math.max(6, cap * 0.42), top = ROW - cap - 0.3 * GA.px * sc0 - br;
      const sx_ = (L) => L.x + (states.get(L.C)?.dx ?? (now < L.C.inA ? SLIDE : -SLIDE));
      let bx, by, sqx = 1, sqy = 1, al = 1, chroma = false;
      if (j < 0) { const f = Math.min(1, Math.max(0, (now - CHUNKS[0].inA) / Math.max(0.2, LANDS[0].t - CHUNKS[0].inA))); bx = sx_(LANDS[0]); by = top - (1 - f * f) * cap * 3; al = Math.min(1, f * 2); }   // the drop-in
      else { const g = LANDS[j], n = LANDS[j + 1], dt = now - g.t; bx = sx_(g); by = top; chroma = g.chroma;
        if (n) { const gap = n.t - g.t, dwell = Math.min(DWELL, gap * 0.25), T = gap - dwell, f = Math.min(1, Math.max(0, (dt - dwell) / T));
          const apex = Math.min(OH * 0.45, Math.max(cap * 0.5, GRAV * T * T / 8)); bx = sx_(g) + (sx_(n) - sx_(g)) * f; by = top - 4 * f * (1 - f) * apex; chroma = g.chroma || n.chroma; }
        else al = Math.max(0, 1 - (now - g.b) / 0.5);
        if (dt < 0.09) { const s = 1 - dt / 0.09; sqx = 1 + 0.3 * s; sqy = 1 - 0.25 * s; } }   // the squash: a shape, not a move
      let ballRGB = WHITE; if (chroma) { const hh = (now * 1.5) % 1, i6 = (hh * 6) | 0, fr = hh * 6 - i6, Q = 255 * (1 - 0.85 * fr), T = 255 * (1 - 0.85 * (1 - fr)), lo = 255 * 0.15;
        ballRGB = [[255, T, lo], [Q, 255, lo], [lo, 255, T], [lo, Q, 255], [T, lo, 255], [255, lo, Q]][i6].map(Math.round); }
      if (al > 0) { blobO(bx + off, by + off, br * sqx, br * sqy, ...SH, al); blobO(bx, by, br * sqx + 1.6, br * sqy + 1.6, ...BLACK, al); blobO(bx, by, br * sqx, br * sqy, ...ballRGB, al); } } }
  if (toBlack > 0) { const k = 1 - toBlack; for (let i = 0; i < ob.length; i++) ob[i] *= k; }   // the end: to black
}

// ── the pipes; the VHS lives on the encoder's input ──
const VHS_CHAIN = ["format=yuv444p", "chromashift=cbh=3:crh=-2", "gblur=sigma=0.9:sigmaV=0.01", "noise=c0s=9:c0f=t+u:c1s=4:c1f=t+u:c2s=4:c2f=t+u",
  "drawgrid=w=iw:h=3:t=1:c=black@0.09", "drawbox=y='mod(t*23\\,ih+80)-40':w=iw:h=22:c=white@0.045:t=fill", "vignette=angle=PI/8", "eq=saturation=1.08:contrast=1.02", "format=yuv420p"].join(",");
const UP_CHAIN = "";                                                        // the upscale happens in here now, before the words
const VF = [UP_CHAIN, arg("vhs") ? VHS_CHAIN : ""].filter(Boolean); const VHS = VF.length ? ["-vf", VF.join(",")] : [];
const frameBytes = W * H * 3;
const ONLY = arg("only") ? String(arg("only")).split(",").map((t) => Math.round((Number(t) - FROM) * FPS)) : null, PNG = arg("png") ? resolve(arg("png")) : OUT;
const dec = spawn("ffmpeg", ["-v", "error", ...(FROM ? ["-ss", String(FROM)] : []), ...(TO ? ["-to", String(TO)] : []), "-i", BASE, "-vf", VERTICAL ? `crop=iw*${VIEW.w / 960}:ih:iw*${VIEW.x / 960}:0,scale=${W}:${H}:flags=lanczos` : SMALL ? "scale=960:540" : "null", "-f", "rawvideo", "-pix_fmt", "rgb24", "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = ONLY ? null : spawn("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${OW}x${OH}`, "-r", String(FPS), "-i", "-", ...(NO_AUDIO ? [] : [...(FROM ? ["-ss", String(FROM)] : []), ...(TO ? ["-to", String(TO)] : []), "-i", AUDIO, "-map", "0:v", "-map", "1:a"]), ...VHS,
  ...(X264 || LYRIC_ONLY ? ["-c:v", "libx264", "-crf", LYRIC_ONLY ? "20" : "15", "-preset", LYRIC_ONLY ? "veryfast" : "medium"] : ["-c:v", "h264_videotoolbox", "-b:v", OH >= 1080 ? "16M" : "8M", "-profile:v", "high", "-allow_sw", "1"]), "-pix_fmt", "yuv420p", ...(NO_AUDIO ? [] : ["-c:a", "aac", "-b:a", "256k", "-shortest"]), "-movflags", "+faststart", outPath], { stdio: ["pipe", "inherit", "inherit"] });
let pending = Buffer.alloc(0), fi = 0;
dec.stdout.on("data", (chunk) => { pending = pending.length ? Buffer.concat([pending, chunk]) : chunk;
  while (pending.length >= frameBytes) { fb = Buffer.from(pending.subarray(0, frameBytes)); pending = pending.subarray(frameBytes);
    if (ONLY) { if (ONLY.includes(fi)) { drawFrame(fi); const f = resolve(PNG, `${stem}-${(FROM + fi / FPS).toFixed(1)}s.png`);
        execFileSync("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${OW}x${OH}`, "-i", "-", ...VHS, "-frames:v", "1", f], { input: ob }); console.log(`  ${f}`); }
      else { matteAt(takeOf(T0 + FROM + fi / FPS)); glide(T0 + FROM + fi / FPS); } fi++; if (fi > Math.max(...ONLY)) { dec.kill(); process.exit(0); } continue; }
    drawFrame(fi++);
    if (!enc.stdin.write(ob)) { dec.stdout.pause(); enc.stdin.once("drain", () => dec.stdout.resume()); }
    if (fi % 300 === 0) process.stdout.write(`\r  ${NF ? Math.round(100 * fi / NF) + "%" : fi}`); } });
dec.stdout.on("end", () => enc && enc.stdin.end());
enc && enc.on("close", (code) => { try { if (MFD != null) { closeSync(MFD); mdec.kill(); unlinkSync(FIFO); } } catch {}
  console.log(`\r${code ? "✗" : "✓"} ${outPath}  (${fi} frames; ${CHUNKS.length} chunks of ${LINES.length} lines, ${FAIRY.length} fairy lights, ${KICKS.length} kicks${VHS.length ? ", vhs" : ""})`); process.exit(code || 0); });
