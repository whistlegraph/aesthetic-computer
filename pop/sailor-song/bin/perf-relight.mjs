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
const BASE = resolve(arg("base") || (existsSync(resolve(OUT, `${stem}-perf.mp4`)) ? resolve(OUT, `${stem}-perf.mp4`)
  : readdirSync(OUT).filter((f) => /^sailor-song-v\d+-perf\.mp4$/.test(f)).map((f) => resolve(OUT, f)).sort((a, b) => statSync(b).mtimeMs - statSync(a).mtimeMs)
    .find((f) => { const r = f.replace(/-perf\.mp4$/, ".events.json"); try { return sameClock(JSON.parse(readFileSync(r, "utf8")), R); } catch { return false; } }) || resolve(OUT, `${stem}-perf.mp4`)));
if (!existsSync(BASE)) { console.error(`✗ base video missing: ${BASE} (render perf-video.mjs without --strip first)`); process.exit(1); }
const probe = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height,r_frame_rate,nb_frames", "-of", "csv=p=0", BASE]).toString().trim().split(",");
const LYRIC_ONLY = !!arg("lyric-only"), NO_LYRIC = !!arg("no-lyric"), SMALL = !!arg("small"), BALL = !!arg("ball");   // --ball: the bouncing ball (jeffrey lost it, v103)
// speed (v103: "why is it so slow?"): the take is 960×540, so compositing at 1080 only costs — --small works at 540 and
// --deliver 1080 upscales (lanczos + CAS) inside the encoder; --jobs N cuts the record into N stretches rendered in
// parallel (video only) and concatenates them, muxing the audio once.
const DELIVER = Number(arg("deliver", 0)), JOBS = Number(arg("jobs", 0)), NO_AUDIO = !!arg("no-audio"), X264 = !!arg("x264");   // the Mac's VideoToolbox encodes by default (libx264 at 1080p60 was the clock); --x264 for the software path   // --no-lyric: the room and her, no words
const W = SMALL ? 960 : +probe[0], H = SMALL ? 540 : +probe[1], FPS = eval(probe[2]), NF = +probe[3] || 0;
const FROM = Number(arg("from", 0)), TO = arg("to") ? Number(arg("to")) : null;
if (JOBS > 1) {                                                             // the conductor: N of this script, then one concat
  const dur = TO ?? Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", BASE]).toString());
  const out = resolve(arg("out") || resolve(OUT, `${stem}-relight.mp4`)), tmp = mkdtempSync(join(tmpdir(), "relight-jobs-")), step = Math.ceil((dur - FROM) / JOBS);
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
const outPath = resolve(arg("out") || resolve(OUT, `${stem}-${LYRIC_ONLY ? "lyric" : "relight"}${TO ? `-${FROM}-${TO}` : ""}.mp4`));
const sx = W / 960, sy = H / 540;

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
  if (MW === W && MH === H) { for (let i = 0; i < W * H; i++) M[i] = shape(mraw[i]); return; }
  const fx = MW / W, fy = MH / H;                                            // bilinear up (the 60 fps / 1080p deliveries)
  for (let y = 0; y < H; y++) { const v = Math.min(MH - 1.001, Math.max(0, (y + 0.5) * fy - 0.5)), v0 = v | 0, fv = v - v0, r0 = v0 * MW, r1 = (v0 + 1) * MW;
    for (let x = 0; x < W; x++) { const u = Math.min(MW - 1.001, Math.max(0, (x + 0.5) * fx - 0.5)), u0 = u | 0, fu = u - u0;
      M[y * W + x] = shape((mraw[r0 + u0] * (1 - fu) + mraw[r0 + u0 + 1] * fu) * (1 - fv) + (mraw[r1 + u0] * (1 - fu) + mraw[r1 + u0 + 1] * fu) * fv); } } };

}
// ── two repairs on the matte, per frame (v103: "bottom left of guitar mask is weird, top of center light masks weird
//    too — probably glare"): the window's glare blows the guitar's lower bout to white and Vision lets it go, so the
//    guitar's body and neck from guitar-track.py are unioned in (soft 8 px edge); and under the lamp the bright glass
//    gets pulled into her hair, so inside the lamp's disc only dark pixels stay foreground (her hair is dark, glass is not).
const TRACK = (() => { try { return JSON.parse(readFileSync(resolve(LANE, "src/guitar-track.json"), "utf8")); } catch { return null; } })();
const TFPS = TRACK && TRACK.length > 1 ? (TRACK.length - 1) / TRACK.at(-1).t : 30;
const repairMatte = (take) => { if (LYRIC_ONLY) return;
  if (TRACK) { const k = TRACK[Math.min(TRACK.length - 1, Math.max(0, Math.round(take * TFPS)))], hx = k.hole[0] * sx, hy = k.hole[1] * sy, ex = k.head[0] * sx, ey = k.head[1] * sy;
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
  const Lp = LIGHTS.lamp, lx = Lp.x * sx, ly = Lp.y * sy, lr = Lp.r * 1.15 * sx;
  for (let y = Math.max(0, ly - lr) | 0; y <= Math.min(H - 1, ly + lr); y++) for (let x = Math.max(0, lx - lr) | 0; x <= Math.min(W - 1, lx + lr); x++) { if (Math.hypot(x - lx, y - ly) > lr) continue;
    const o = y * W + x, p = o * 3, lum = (fb[p] * 0.299 + fb[p + 1] * 0.587 + fb[p + 2] * 0.114) / 255, keep = 1 - Math.min(1, Math.max(0, (lum - 0.42) / 0.18)); M[o] *= keep; } };

// ── the lights (room-lights.py), as static fields over the frame ──
const LIGHTS = JSON.parse(readFileSync(resolve(LANE, "src/room-lights.json"), "utf8"));
const LAMP = new Float32Array(W * H), WINDOW = new Float32Array(W * H);
{ const L = LIGHTS.lamp, lx = L.x * sx, ly = L.y * sy, s2 = 2 * (250 * sx) ** 2, Wn = LIGHTS.window, soft = 70 * sx;
  const edge = (d) => Math.min(1, Math.max(0, d / soft));
  for (let y = 0; y < H; y++) for (let x = 0; x < W; x++) { const o = y * W + x; LAMP[o] = Math.exp(-((x - lx) ** 2 + (y - ly) ** 2) / s2);
    const inx = edge(x - Wn.x0 * sx + soft * 0.5) * edge(Wn.x1 * sx - x + soft * 0.5), iny = edge(y - Wn.y0 * sy + soft * 0.5) * edge(Wn.y1 * sy - y + soft * 0.5); WINDOW[o] = inx * iny; } }
const FAIRY = LIGHTS.fairy.map(([x, y], i) => ({ x: x * sx, y: y * sy, i }));

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
const KICKS = pick("kick", "wub-kick", "rev-kick"), HATS = pick("hat", "click", "rim"), PADS = pick("pad", "pad-hit", "high", "vib"), BELLS = pick("bell", "gong", "rise", "impact");
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
const ATLAS = resolve(LANE, "src/glyph-atlas/comic-72");
if (!existsSync(`${ATLAS}.json`)) execFileSync(PY, [resolve(HERE, "glyph-atlas.py")], { stdio: "inherit" });
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
const SC_MAX = 0.5 * sx, ROW = H * 0.925, GRAV = 1400 * sy, SLIDE = 0.22 * W;   // px/s² — the ball's gravity; how far a line slides
for (const C of CHUNKS) { const tw = C.words.reduce((a, g) => a + g.adv, 0) + SPACE * (C.words.length - 1);
  C.sc = Math.min(SC_MAX, (W * 0.86) / tw); let pen = (W - tw * C.sc) / 2; for (const g of C.words) { g.x = pen; g.base = ROW; pen += (g.adv + SPACE) * C.sc; } }
// the switch between chunks (v103: "the prior opacities off to the left, the next shifts in to centre and opacities in"):
// one 0.35 s move shared by both, placed in the gap after the last word, or just before the next when there is no gap
CHUNKS.forEach((C, i) => { const P = CHUNKS[i - 1], N = CHUNKS[i + 1];
  if (N) { const t0 = Math.max(C.b, N.a - 0.45); C.outA = N.a - t0 < 0.15 ? N.a - 0.3 : t0; C.outB = Math.min(N.a, C.outA + 0.35); } else { C.outA = C.b + 0.1; C.outB = C.b + 0.5; }
  C.inA = P ? P.outA : C.a - 0.6; C.inB = P ? P.outB : C.a - 0.25; });
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

// ── drawing on the rgb24 frame buffer ──
let fb = null;
const px = (x, y, r, g, b, a = 1) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3;
  fb[o] = Math.min(255, fb[o] * (1 - a) + r * a); fb[o + 1] = Math.min(255, fb[o + 1] * (1 - a) + g * a); fb[o + 2] = Math.min(255, fb[o + 2] * (1 - a) + b * a); };
const add = (x, y, r, g, b, a) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3, k = a * (1 - M[y * W + x]);   // the room only
  fb[o] = Math.min(255, fb[o] + r * k); fb[o + 1] = Math.min(255, fb[o + 1] + g * k); fb[o + 2] = Math.min(255, fb[o + 2] + b * k); };
const glow = (x, y, rad, r, g, b, a) => { const R2 = rad * rad; for (let j = -rad; j <= rad; j++) for (let i = -rad; i <= rad; i++) { const d2 = i * i + j * j; if (d2 > R2) continue; add(x + i, y + j, r, g, b, a * (1 - d2 / R2) ** 2); } };
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
  const now = T0 + FROM + fi / FPS, take = takeOf(now); matteAt(take); repairMatte(take); const c = glide(now);
  const lightsOn = 1 - Math.min(1, Math.max(0, (take - (CAM_MOVE - 0.74)) / 0.6));
  const room = 0.72 + 0.28 * amb;                                        // v103: a simple grade — the room breathes with the sections, never goes out
  const lampBloom = Math.min(0.5, env(KICKS, now, 0.08, 0.03)) * lightsOn, lampOn = (0.12 * amb + 0.2 * lampBloom) * lightsOn;      // a burst, not a whiteout
  const bellRaw = env(BELLS, now, 1.4, 0.1), windowGlow = Math.min(0.15, 0.08 * bellRaw) * lightsOn, windowOn = (0.15 + 0.1 * amb) * lightsOn;
  const padLevel = Math.min(1, sounding(PADS, now) * 0.6) * lightsOn;
  if (!LYRIC_ONLY) {
  // 1. the room: multiplicative gain (ambient × tint, lifted by the lamp's pool and the window), her kept close to natural
  const T = tint.map((t) => 0.55 + 0.45 * t);                              // the chord's tint at half strength
  const fgGain = 0.92 + 0.08 * room, fgTint = T.map((t) => 0.8 + 0.2 * t);   // she stays nearly natural, just a little of the room's colour
  for (let o = 0, p = 0; o < W * H; o++, p += 3) { const m = M[o], lamp = LAMP[o], win = WINDOW[o];
    const g = room + lampOn * lamp + windowOn * win * (1 - room) + windowGlow * win * 0.6;
    const gr = (g * T[0] + lampBloom * lamp * 0.1) * (1 - m) + fgGain * fgTint[0] * m, gg = (g * T[1] + lampBloom * lamp * 0.08) * (1 - m) + fgGain * fgTint[1] * m, gb = (g * T[2] + lampBloom * lamp * 0.06) * (1 - m) + fgGain * fgTint[2] * m;
    fb[p] = Math.min(255, fb[p] * gr); fb[p + 1] = Math.min(255, fb[p + 1] * gg); fb[p + 2] = Math.min(255, fb[p + 2] * gb); }
  // 2. the lamp's bloom and the window's glow, added to the room
  if (lampBloom > 0.02) { const L = LIGHTS.lamp, lx = L.x * sx, ly = L.y * sy, R0 = Math.round(L.r * 1.6 * sx); glow(lx, ly, R0, 255, 228, 180, 0.18 * lampBloom); glow(lx, ly, Math.round(R0 * 2.4), 255, 220, 170, 0.04 * lampBloom); }
  if (windowGlow > 0.02) { const Wn = LIGHTS.window; for (let y = Wn.y0 * sy; y < Wn.y1 * sy; y += 2) for (let x = Wn.x0 * sx; x < Wn.x1 * sx; x += 2) { const k = WINDOW[(y | 0) * W + (x | 0)] * 0.12 * windowGlow; add(x, y, 225, 238, 255, k); add(x + 1, y, 225, 238, 255, k); add(x, y + 1, 225, 238, 255, k); add(x + 1, y + 1, 225, 238, 255, k); } }
  // 3. the fairy lights: a bead of light each, glowing with the pads, a handful flashing on each hat
  const flash = new Float32Array(FAIRY.length); for (let i = HATS.cur || 0; i < HATS.length && HATS[i].t <= now; i++) { const dt = now - HATS[i].t; if (dt < 0.35) for (const p of HATS[i].pts) flash[p] = Math.max(flash[p], HATS[i].g * Math.exp(-dt / 0.07)); }
  if (lightsOn > 0) for (const f of FAIRY) { const base = (0.08 + 0.3 * padLevel + 0.25 * amb) * lightsOn, fl = flash[f.i] * 1.2 * lightsOn;
    glow(f.x, f.y, Math.round(7 * sx), 170, 150, 255, 0.5 * base); glow(f.x, f.y, 2, 235, 230, 255, 0.7 * base);
    if (fl > 0.02) { glow(f.x, f.y, Math.round(12 * sx), 210, 200, 255, 0.6 * fl); glow(f.x, f.y, 3, 255, 255, 255, 0.9 * fl); } }
  }
  // 4. the singalong: the chunk being sung (and the one leaving); letters fill in syllable by syllable, the sung
  //    syllable pops, the sung word a touch larger, a held syllable kerns outward and sways; the ball rides its one path.
  if (!NO_LYRIC) { const k = Math.min(1, 0.3 + 0.7 * amb) * (1 + 0.5 * lampBloom), fl = Math.min(1, bellRaw) * 120, SH = hue.map((h) => Math.min(255, h * k + fl)), off = 5 * sx;
    const states = new Map(); for (const C of CHUNKS) { const st = chunkState(C, now); if (st) states.set(C, st); }
    for (const [C, st] of states) { const al = st.al, sc = C.sc, em = GA.px * sc;
      for (const w of C.words) { const timed = !!w.w, wordOn = timed && now >= w.w.a, cur = wordOn && now < w.w.b + 0.1;
        const prevOn = timed ? false : (() => { const p = C.words[C.words.indexOf(w) - 1]; return p && p.w ? now >= p.w.b : wordOn; })();
        const wildW = timed ? Math.max(0, ...w.w.syl.map((t) => now >= t.a && now < t.b && t.b - t.a > 0.6 && now - t.a > 0.25 ? Math.abs(Math.sin(Math.PI * (now - t.a - 0.25) * 3.2)) : 0)) : 0;
        const kern = wildW * 0.09 * GA.px; let pen = w.x + st.dx - kern * sc * (w.gl.length - 1) / 2;
        for (const q of w.gl) { const gg = q.g, t = timed ? w.w.syl[q.si] : null;
          const lit = t ? t.a + (t.b - t.a) * q.sk / q.sn : 1e9, on = timed ? now >= lit : prevOn;
          const held = t && now >= t.a && now < t.b && t.b - t.a > 0.6 && now - t.a > 0.25, wild = held ? Math.abs(Math.sin(Math.PI * (now - t.a - 0.25) * 3.2)) : 0;
          const sylPop = t && now >= t.a ? 1 + 0.14 * Math.max(0, 1 - (now - t.a) / 0.15) : 1, chPop = on && t ? 1 + 0.12 * Math.max(0, 1 - (now - lit) / 0.1) : 1;
          const s2 = sc * sylPop * chPop * (cur ? 1.06 + 0.05 * lampBloom : 1) * (1 + 0.12 * wild), sway = 0.6 * Math.sin(2 * Math.PI * now / q.per + q.ph);
          const lat = wild * em * 0.05 * Math.sin(now * 23 + q.ph * 5), cx = pen + gg.adv * sc / 2 + lat, cy = ROW - (GA.ascent - GA.H / 2) * s2 + (q.jy + sway) * sc - wild * em * 0.07 * Math.sin(now * 40 + q.ph * 7), gx = cx + (gg.w / 2 + gg.dx - gg.adv / 2) * s2, rot = q.rot * (1 + 2.5 * wild);
          glyph(GOUTER, gg, gx + off, cy + off, s2, rot, ...SH, al); glyph(GOUTER, gg, gx, cy, s2, rot, ...BLACK, al); glyph(GFILL, gg, gx, cy, s2, rot, ...(on ? WHITE : GREY), al);
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
          const apex = Math.min(H * 0.45, Math.max(cap * 0.5, GRAV * T * T / 8)); bx = sx_(g) + (sx_(n) - sx_(g)) * f; by = top - 4 * f * (1 - f) * apex; chroma = g.chroma || n.chroma; }
        else al = Math.max(0, 1 - (now - g.b) / 0.5);
        if (dt < 0.09) { const s = 1 - dt / 0.09; sqx = 1 + 0.3 * s; sqy = 1 - 0.25 * s; } }   // the squash: a shape, not a move
      let ballRGB = WHITE; if (chroma) { const hh = (now * 1.5) % 1, i6 = (hh * 6) | 0, fr = hh * 6 - i6, Q = 255 * (1 - 0.85 * fr), T = 255 * (1 - 0.85 * (1 - fr)), lo = 255 * 0.15;
        ballRGB = [[255, T, lo], [Q, 255, lo], [lo, 255, T], [lo, Q, 255], [T, lo, 255], [255, lo, Q]][i6].map(Math.round); }
      if (al > 0) { blob(bx + off, by + off, br * sqx, br * sqy, ...SH, al); blob(bx, by, br * sqx + 1.6, br * sqy + 1.6, ...BLACK, al); blob(bx, by, br * sqx, br * sqy, ...ballRGB, al); } } }
}

// ── the pipes; the VHS lives on the encoder's input ──
const VHS_CHAIN = ["format=yuv444p", "chromashift=cbh=3:crh=-2", "gblur=sigma=0.9:sigmaV=0.01", "noise=c0s=9:c0f=t+u:c1s=4:c1f=t+u:c2s=4:c2f=t+u",
  "drawgrid=w=iw:h=3:t=1:c=black@0.09", "drawbox=y='mod(t*23\\,ih+80)-40':w=iw:h=22:c=white@0.045:t=fill", "vignette=angle=PI/8", "eq=saturation=1.08:contrast=1.02", "format=yuv420p"].join(",");
const UP_CHAIN = DELIVER && DELIVER !== H ? `scale=-2:${DELIVER}:flags=lanczos,cas=0.4` : "";
const VF = [UP_CHAIN, arg("vhs") ? VHS_CHAIN : ""].filter(Boolean); const VHS = VF.length ? ["-vf", VF.join(",")] : [];
const frameBytes = W * H * 3;
const ONLY = arg("only") ? String(arg("only")).split(",").map((t) => Math.round((Number(t) - FROM) * FPS)) : null, PNG = arg("png") ? resolve(arg("png")) : OUT;
const dec = spawn("ffmpeg", ["-v", "error", ...(FROM ? ["-ss", String(FROM)] : []), ...(TO ? ["-to", String(TO)] : []), "-i", BASE, ...(SMALL ? ["-vf", "scale=960:540"] : []), "-f", "rawvideo", "-pix_fmt", "rgb24", "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = ONLY ? null : spawn("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", ...(NO_AUDIO ? [] : [...(FROM ? ["-ss", String(FROM)] : []), ...(TO ? ["-to", String(TO)] : []), "-i", AUDIO, "-map", "0:v", "-map", "1:a"]), ...VHS,
  ...(X264 || LYRIC_ONLY ? ["-c:v", "libx264", "-crf", LYRIC_ONLY ? "20" : "15", "-preset", LYRIC_ONLY ? "veryfast" : "medium"] : ["-c:v", "h264_videotoolbox", "-b:v", (DELIVER || H) >= 1080 ? "16M" : "8M", "-profile:v", "high", "-allow_sw", "1"]), "-pix_fmt", "yuv420p", ...(NO_AUDIO ? [] : ["-c:a", "aac", "-b:a", "256k", "-shortest"]), "-movflags", "+faststart", outPath], { stdio: ["pipe", "inherit", "inherit"] });
let pending = Buffer.alloc(0), fi = 0;
dec.stdout.on("data", (chunk) => { pending = pending.length ? Buffer.concat([pending, chunk]) : chunk;
  while (pending.length >= frameBytes) { fb = Buffer.from(pending.subarray(0, frameBytes)); pending = pending.subarray(frameBytes);
    if (ONLY) { if (ONLY.includes(fi)) { drawFrame(fi); const f = resolve(PNG, `${stem}-${(FROM + fi / FPS).toFixed(1)}s.png`);
        execFileSync("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-i", "-", ...VHS, "-frames:v", "1", f], { input: fb }); console.log(`  ${f}`); }
      else { matteAt(takeOf(T0 + FROM + fi / FPS)); glide(T0 + FROM + fi / FPS); } fi++; if (fi > Math.max(...ONLY)) { dec.kill(); process.exit(0); } continue; }
    drawFrame(fi++);
    if (!enc.stdin.write(fb)) { dec.stdout.pause(); enc.stdin.once("drain", () => dec.stdout.resume()); }
    if (fi % 300 === 0) process.stdout.write(`\r  ${NF ? Math.round(100 * fi / NF) + "%" : fi}`); } });
dec.stdout.on("end", () => enc && enc.stdin.end());
enc && enc.on("close", (code) => { try { if (MFD != null) { closeSync(MFD); mdec.kill(); unlinkSync(FIFO); } } catch {}
  console.log(`\r${code ? "✗" : "✓"} ${outPath}  (${fi} frames; ${CHUNKS.length} chunks of ${LINES.length} lines, ${FAIRY.length} fairy lights, ${KICKS.length} kicks${VHS.length ? ", vhs" : ""})`); process.exit(code || 0); });
