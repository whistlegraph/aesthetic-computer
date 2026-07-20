// reel.mjs — a talking head becomes a vertical reel.
//
// Takes any 16:9 clip of a person speaking and returns a 1080×1920 reel: the
// camera follows their face, their words appear as they say them, and an
// instrumental bed sits underneath, ducked so it never fights the voice.
//
//   node marketing/talking-head/bin/reel.mjs ~/Desktop/YC.mov \
//     --out marketing/talking-head/out/yc-reel.mp4 --kit felt
//
//   --kit felt|brush|wood|eight0eight|pulse   bed instrumentation (jingle.mjs)
//   --bedgain 0.42        how loud the bed sits under the voice
//   --accent "#3dff88"    karaoke fill color
//   --zoom 1.0            1.0 = full-height crop, the sharpest framing
//   --sync auto|<ms>      A/V offset correction; see below
//   --title Name          text climbing the side stamps
//   --no-stamps           drop the pals chrome
//   --no-bed              speech only
//   --no-grade            skip the color polish
//   --seconds N           render only the first N seconds (for quick looks)
//
// LIP SYNC. Consumer captures bake in an A/V offset, and this one does: the YC
// clip runs its picture about 100ms behind its sound, which reads as dubbing the
// moment you put captions under it. `--sync auto` measures the offset by
// cross-correlating mouth motion against the audio envelope (see lib/av-sync.mjs)
// and holds the sound back to meet the lips. Pass a number of ms to override, or
// 0 to leave the footage alone.
//
// The correction is applied to the AUDIO, not the video, and that is deliberate:
// video can only be shifted in whole frames (33ms at a time here), so correcting
// on that side can never land closer than half a frame — measure 100ms and you
// must choose between 3 frames and 4. Audio shifts by samples. So the picture is
// left exactly as shot and the sound is delayed onto it, which also keeps every
// frame of the clip instead of eating its head.
//
// Three constraints from the house drove the rest, and they're worth knowing
// before you edit this:
//
//   · This ffmpeg has no drawtext and no libass. Text cannot be burned in by
//     ffmpeg at all. Glyphs are baked to PNGs by ImageMagick and composited
//     per-frame onto a canvas — the same detour captions-train.mjs and
//     stamp-reel.mjs take, and the reason preview-shared.mjs exists.
//   · The crop is done on the canvas, not by ffmpeg's `crop` filter, because we
//     want a smoothed sub-pixel pan (see reframe.mjs) and a filter expression
//     can't hold state between frames.
//   · Reels bring their own furniture — no progress bar, no timecode. That rule
//     is in kidlisp-reels/SCORE.md and av-reels/README.md both.

import { spawn, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, rmSync } from "node:fs";
import { basename, dirname, join, resolve } from "node:path";
import { createCanvas, createImageData } from "canvas";

import { faceTrack, buildReframer } from "../../lib/reframe.mjs";
import { measureAvOffset } from "../../lib/av-sync.mjs";
import { wordsFromWhisper } from "../../lib/words.mjs";
import { makeSideStamps, makeChromaGlyphs, dayglow } from "../../lib/side-stamps.mjs";
import {
  magickRenderText, spawnFFmpegEncode, decodeAudioMono, computeRmsEnvelope,
} from "../../../pop/lib/preview-shared.mjs";
import { renderBed, renderSineBed } from "../../podcast/bin/jingle.mjs";

const W = 1080;
const H = 1920;
const FPS = 30;

// Plain Arial Bold, per @jeffrey — the geometric YWFT face is AC's, but captions
// on a person's face want to disappear into legibility, not perform. The .ttc
// collections resolve to their Regular weight, so we ask for the bold file.
const CAP_FONT = [
  "/System/Library/Fonts/Supplemental/Arial Bold.ttf",
  "/System/Library/Fonts/HelveticaNeue.ttc",
  "/System/Library/Fonts/Helvetica.ttc",
].find(existsSync) || "Helvetica";

// Type is baked large and drawn small — a 120px glyph scaled down to 56 stays
// crisp, where a 56px glyph scaled up would not.
//
// Captions here sit on a moving person in a patterned shirt, so they carry both
// an outline and a hard drop shadow. Either alone loses against the footage: the
// shadow dies on dark cloth, the outline dies on the black mic. Together the
// glyph always has an edge somewhere.
const CAP = {
  renderPx: 150,
  drawPx: 78, // big — the captions ARE the composition now, sitting over the face
  // Wide, since they no longer dodge the shirt patch — they sit up on the face,
  // between the side-stamp columns.
  maxLineW: 800,
  wordGap: 20,
  bandY: 0.46, // up on the face, near center — over him, where the eye already is
  lead: 0.12, // let a phrase arrive a beat before its first word lands
  tail: 0.30, // and linger a beat after its last one
  cream: "#fcf7c5",
  shadow: "#000000",
  shadowSpec: "100x0+6+7", // hard offset, no blur — captions-train's look
  stroke: "#000000",
  strokeWidth: 6,
};

// A gentle polish, not a rescue: the source measures near-neutral (a mild warm
// cast, sane brightness), so this only firms up the contrast and lifts the
// saturation enough to survive a phone screen.
const GRADE = "eq=contrast=1.07:saturation=1.12:brightness=0.012";

// Sharpening happens AFTER the canvas has blown 405×720 up to 1080×1920, because
// sharpening before an upscale just gives you bigger, softer halos. Denoise runs
// first: a 2018 webcam is grainy, and an unsharp mask with no denoise in front of
// it is a grain amplifier.
//
// `cas` is contrast-adaptive — it finds edges and leaves flat areas alone, which
// is exactly what upscaled footage needs and exactly what unsharp gets wrong
// (unsharp haloes a cheek as eagerly as it sharpens an eye). It does the lifting;
// unsharp only adds a little bite behind it.
//
// None of this invents detail. There are 405×720 real pixels here and no filter
// makes more — for actual new detail you need a learned upscaler (see --fal).
const CRISP = "hqdn3d=1.5:1.2:6:6,cas=strength=0.65,unsharp=5:5:0.45:5:5:0.0";

// The tape starts mid-word — there is no runway before "My", the recording began
// on the M. Nothing downstream can restore what was never captured, so instead we
// give the ear a moment to arrive: hold the first frame, let the bed come up, and
// fade the voice in so its clipped attack reads as a start rather than a splice.
let LEAD_FRAMES = 15; // exactly 0.5s at 30fps — keep this in FRAMES; 0 in song mode
const VOICE_FADE = 0.09;

// He is quiet, close-mic'd, and in a hard room. In order: lose the rumble, lose
// the hiss, tame the S's the presence boost is about to expose, scoop the boxy
// 250Hz the wall put there, lift the 3kHz where consonants live, add a little
// air, then even out the level so the quiet half of a sentence lands as hard as
// the loud half.
const VOICE_CHAIN = [
  "highpass=f=85",
  "afftdn=nf=-25",
  "deesser=i=0.35",
  "equalizer=f=250:t=q:w=1.2:g=-3",
  "equalizer=f=3200:t=q:w=1.4:g=3.5",
  "treble=g=2.5:f=8500",
  "acompressor=threshold=-20dB:ratio=3:attack=8:release=200:makeup=2",
  "loudnorm=I=-16:TP=-1.5:LRA=9",
].join(",");

// ── args ────────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flag = (name, dflt = null) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[i + 1] : dflt;
};
const has = (name) => argv.includes(`--${name}`);

const SRC = resolve(argv.find((a) => !a.startsWith("--")) || "");
if (!SRC || !existsSync(SRC)) {
  console.error("usage: reel.mjs <video> [--out path] [--audio-from orig.mov] [--kit felt] [--sync auto|ms]");
  process.exit(2);
}

// Video and audio can come from different files. That's the whole point of the
// fal upscale path: Topaz gives back a sharper PICTURE but re-encodes the sound,
// so `--audio-from` keeps the picture from the upscaled clip and the audio (and
// the clip's real 2018 date) from the untouched original. Default: same file.
const AUDIO_SRC = resolve(flag("audio-from", SRC));
if (!existsSync(AUDIO_SRC)) {
  console.error(`--audio-from not found: ${AUDIO_SRC}`);
  process.exit(2);
}

const slug = basename(flag("audio-from", SRC)).replace(/\.[^.]+$/, "").toLowerCase();
const OUT_DIR = join(dirname(new URL(import.meta.url).pathname), "..", "out");
const OUT = resolve(flag("out", join(OUT_DIR, `${slug}-reel.mp4`)));
const WORK = join(OUT_DIR, `.work-${slug}`);
const KIT = flag("kit", "felt");
const BED_GAIN = parseFloat(flag("bedgain", "0.42"));
const ACCENT = flag("accent", "#3dff88");
const ZOOM = parseFloat(flag("zoom", "1.0"));
const STAMP_TITLE = flag("title", "prompt.AC");
const STAMP_SIZE = parseFloat(flag("stamp-size", "145"));
const STAMP_TITLE_PX = parseFloat(flag("stamp-title-px", "72"));
const STAMP_TITLE_SCALE = parseFloat(flag("stamp-title-scale", "0.62"));
const STAMP_CHROMA = parseFloat(flag("stamp-chroma", "3"));
const SYNC_ARG = flag("sync", "auto");
const LIMIT = flag("seconds") ? parseFloat(flag("seconds")) : null;
const USE_BED = !has("no-bed");
const USE_STAMPS = !has("no-stamps");
const USE_GRADE = !has("no-grade");

// A learned upscaler (Topaz) rebuilds the background and hair beautifully but
// waxes the skin — it reads real edges as detail and smooth skin as noise to
// remove, so the face comes back soft while everything around it sharpens. When
// the video is upscaled and the original is on hand (--audio-from), we blend the
// ORIGINAL's face — real pores and grain — back over just the face region,
// feathered, keeping the upscale everywhere else. `--face 0` disables it.
const FACE_RESTORE = SRC !== AUDIO_SRC && !flag("song") && parseFloat(flag("face", "1.0")) > 0;
const FACE_STRENGTH = Math.min(1, Math.max(0, parseFloat(flag("face", "1.0"))));

// SONG MODE. When the picture is a finished piece (a warped sing-along) and the
// audio is a finished mix, none of the spoken-take machinery applies — no voice
// conditioning, no bed, no A/V-sync guess, no held lead-in. `--song <mix.wav>`
// uses the audio as-is and `--words <sung.json>` times the captions to it. The
// crop, karaoke captions, stamps and timer are all still wanted, so only the
// audio front-end is bypassed.
const SONG = flag("song") ? resolve(flag("song")) : null;
const WORDS_FILE = flag("words") ? resolve(flag("words")) : null;
if (SONG) LEAD_FRAMES = 0; // the warp already timed the picture; no runway needed

mkdirSync(WORK, { recursive: true });
mkdirSync(dirname(OUT), { recursive: true });

const sh = (cmd, args) => {
  const r = spawnSync(cmd, args, { stdio: ["ignore", "pipe", "pipe"], encoding: "utf8" });
  if (r.status !== 0) throw new Error(`${cmd} failed: ${r.stderr?.slice(0, 400)}`);
  return r.stdout;
};

// ── 1. face track ───────────────────────────────────────────────────────────
console.log("· tracking face (Vision)");
const track = faceTrack(SRC, { fps: 30, cachePath: join(WORK, "track.json") });
console.log(`  ${track.samples.length} samples, ${
  ((track.samples.length / Math.round(track.duration * 30)) * 100).toFixed(0)}% detection`);

// ── 2. measure the A/V offset ───────────────────────────────────────────────
// Do this before anything else consumes the timeline: everything downstream —
// the captions, the crop, the mix — assumes picture and sound already agree.
let SYNC_MS = 0;
if (SONG) {
  // the song's audio and its warped picture already share a clock
} else if (SYNC_ARG === "auto") {
  console.log("· measuring A/V offset (mouth motion × audio envelope)");
  const { offsetMs, confidence } = await measureAvOffset(SRC, { track });
  if (confidence < 1.15) {
    console.log(`  weak correlation (${confidence.toFixed(2)}) — leaving sync alone`);
  } else if (offsetMs > 0) {
    SYNC_MS = offsetMs;
    console.log(`  picture is ${offsetMs}ms late (confidence ${confidence.toFixed(2)}) — holding sound back to meet it`);
  } else {
    console.log(`  sound is ${-offsetMs}ms late (confidence ${confidence.toFixed(2)}) — leaving alone`);
  }
} else {
  SYNC_MS = Math.max(0, parseFloat(SYNC_ARG));
  if (SYNC_MS) console.log(`· delaying sound ${SYNC_MS}ms (manual)`);
}

// Trust the frame COUNT, not the container's duration. They disagree — this clip
// holds 1773 frames (59.10s at 30fps) but declares itself 59.12s long — and if
// you cut the audio to the declared duration you end up with 20ms of sound after
// the last picture, which a player shows as a black flash at the end.
const nbFrames = parseInt(
  sh("ffprobe", ["-v", "error", "-select_streams", "v",
    "-show_entries", "stream=nb_frames", "-of", "csv=p=0", SRC]).trim(),
  10,
);
const FULL = Number.isFinite(nbFrames) && nbFrames > 0 ? nbFrames / FPS : track.duration;
const DUR = LIMIT ? Math.min(LIMIT, FULL) : FULL;

// ── 3. transcribe ───────────────────────────────────────────────────────────
// whisper at token granularity (-ml 1); words.mjs stitches the pieces back up.
const VOICE = join(WORK, "voice.wav");
const MONO = join(WORK, "mono16k.wav");
const WHISPER_JSON = join(WORK, "whisper.json");
const MODEL = resolve("recap/models/ggml-large-v3-turbo.bin");

let words;
if (WORDS_FILE) {
  // Caption timings supplied (the sung word alignment) — accept either a bare
  // word array or an alignment doc with a `.words` array.
  const doc = JSON.parse(readFileSync(WORDS_FILE, "utf8"));
  const arr = Array.isArray(doc) ? doc : doc.words;
  words = arr.map((w) => ({ text: w.text, fromMs: w.fromMs ?? w.from, toMs: w.toMs ?? w.to }));
  console.log(`· captions from ${basename(WORDS_FILE)} — ${words.length} words`);
} else {
  // The voice is cleaned and levelled before anything else touches it. A 2018
  // webcam recording is quiet, uneven, and full of room tone, and the bed's duck
  // (below) keys off this signal — feed it the raw audio and the compressor
  // triggers on the hiss, pinning the bed down for the whole minute so you never
  // hear it.
  console.log("· extracting + conditioning voice");
  sh("ffmpeg", ["-v", "error", "-y", "-i", AUDIO_SRC,
    "-af", VOICE_CHAIN,
    "-ac", "2", "-ar", "44100", "-c:a", "pcm_s16le", VOICE]);
  sh("ffmpeg", ["-v", "error", "-y", "-i", AUDIO_SRC, "-ac", "1", "-ar", "16000", MONO]);

  if (!existsSync(WHISPER_JSON)) {
    console.log("· transcribing (whisper)");
    sh("whisper-cli", [
      "-m", MODEL, "-f", MONO, "-oj", "-ojf", "-ml", "1", "-l", "en",
      "-of", WHISPER_JSON.replace(/\.json$/, ""),
    ]);
  }
  words = wordsFromWhisper(WHISPER_JSON);
  console.log(`  ${words.length} words`);
}

const at = buildReframer(track, { w: W, h: H, fps: FPS, zoom: ZOOM });

// ── 4. bed + mix ────────────────────────────────────────────────────────────
// The voice is pushed later by two separate amounts, and it matters that they're
// distinct: LEAD is an editorial runway (a held frame, so the ear can arrive
// before the clipped first word), while SYNC is the measured A/V error. `adelay`
// moves the voice by whole samples, so the lips can be met exactly rather than to
// the nearest frame — video can only shift in whole frames, audio cannot.
//
// LEAD is derived from LEAD_FRAMES, never the other way round. The held picture
// can only ever be an integer number of frames, so the sound must be delayed by
// exactly that many — deriving one from a rounded copy of the other is how a
// silent 33ms of drift gets in.
const LEAD = LEAD_FRAMES / FPS;
const voiceStart = LEAD + SYNC_MS / 1000;
const delayMs = Math.round(voiceStart * 1000);
const TOTAL = LEAD + DUR;

// Fade the truncated attack in, so it reads as a start rather than a splice.
const cond = `adelay=${delayMs}|${delayMs},afade=t=in:st=${voiceStart.toFixed(3)}:d=${VOICE_FADE}`;

const AUDIO = SONG || join(WORK, "final.wav");
if (SONG) {
  // The audio is finished — use it as-is, and the captions already carry the
  // right times (LEAD is 0 here, so the shift below is a no-op).
  console.log(`· song audio: ${basename(SONG)}`);
} else if (USE_BED) {
  const BED = join(WORK, "bed.wav");
  if (KIT === "sine") {
    console.log("· scoring bed (sine pad)");
    renderSineBed(TOTAL + 1, BED);
  } else {
    console.log(`· scoring bed (${KIT} @ 72bpm)`);
    renderBed(TOTAL + 1, BED, { kit: KIT });
  }

  console.log("· mixing (sidechain duck)");
  // threshold 0.05 (≈ -26dB), not produce.mjs's 0.02: now that the voice is
  // normalized its silences are genuinely silent, and a higher threshold means
  // only real speech pushes the bed down. The bed swells back in every gap —
  // including the lead-in, where it plays alone and covers the runway.
  // Master to -14 LUFS last — phones play the reel, not a studio monitor.
  // asplit is NOT optional. The voice is needed twice — once as the key that
  // tells the compressor when to duck, and once as the thing you actually hear —
  // and a filter label in ffmpeg feeds exactly ONE consumer. Naming [vx] in two
  // places does not fan it out and does not error; it silently produces a mix
  // with the voice in it TWICE, once delayed and once not. That is invisible when
  // the delay is 95ms (it just sounds a bit smeared) and unmissable at 595ms,
  // where the words arrive half a second before the lips.
  sh("ffmpeg", [
    "-v", "error", "-y", "-i", VOICE, "-i", BED,
    "-filter_complex",
    `[0:a]${cond},apad,asplit=2[vx][vkey];` +
      `[1:a]volume=${BED_GAIN},aphaser=type=t:speed=0.25:decay=0.4[bedfx];` +
      `[bedfx][vkey]sidechaincompress=threshold=0.05:ratio=6:attack=6:release=380[bd];` +
      `[vx][bd]amix=inputs=2:duration=longest:normalize=0[mix];` +
      `[mix]atrim=0:${TOTAL},loudnorm=I=-14:TP=-1:LRA=11[out]`,
    "-map", "[out]", "-ac", "2", "-c:a", "pcm_s16le", AUDIO,
  ]);
} else {
  sh("ffmpeg", ["-v", "error", "-y", "-i", VOICE,
    "-af", `${cond},atrim=0:${TOTAL}`, "-ac", "2", "-c:a", "pcm_s16le", AUDIO]);
}

// The captions ride the voice, so they move with it.
for (const w of words) {
  w.fromMs += delayMs;
  w.toMs += delayMs;
}

// ── 5. bake the glyphs ──────────────────────────────────────────────────────
// Two PNGs per word: cream, and accent. The karaoke fill is the accent one
// revealed left-to-right by a clip rect, so both must register pixel-exactly.
console.log(`· baking glyphs (${basename(CAP_FONT)})`);
const GLYPHS = join(WORK, "glyphs");
mkdirSync(GLYPHS, { recursive: true });

const scale = CAP.drawPx / CAP.renderPx;
for (const [i, w] of words.entries()) {
  const safe = String(i).padStart(3, "0");
  // Identical geometry for both, so the accent registers pixel-exactly over the
  // cream when the fill sweeps across it — same stroke, same shadow, same size.
  const shared = {
    ptSize: CAP.renderPx,
    font: CAP_FONT,
    shadow: CAP.shadow,
    shadowSpec: CAP.shadowSpec,
    stroke: CAP.stroke,
    strokeWidth: CAP.strokeWidth,
  };
  w.imgCream = await magickRenderText(w.text, {
    ...shared, fill: CAP.cream, outPath: join(GLYPHS, `${safe}-c.png`),
  });
  w.imgAccent = await magickRenderText(w.text, {
    ...shared, fill: ACCENT, outPath: join(GLYPHS, `${safe}-a.png`),
  });
  w.dw = w.imgCream.width * scale;
  w.dh = w.imgCream.height * scale;
  // Whisper read the *audio*, which is the thing we trust; the sound was held
  // back to meet the lips, so the words move with it.
  w.from = w.fromMs / 1000;
  w.to = w.toMs / 1000;
}

// ── the clock ───────────────────────────────────────────────────────────────
// Not a playback timecode — the clip's own wall clock, running. It starts at the
// moment the tape was made and ticks forward in real time as the reel plays, so
// the seconds ticking past are the seconds he actually sat there in 2018.
//
// It rides the side stamps: instead of a fixed word climbing the margins, the
// live readout does, in YWFT with the same bounce and chroma fringe as the pals.
// Built from a glyph ATLAS — a running clock passes through some sixty distinct
// strings, which would be hundreds of magick calls, but is only ever made of
// thirteen characters. From the ORIGINAL — the upscale re-stamps today's date.
const created = sh("ffprobe", ["-v", "error", "-show_entries", "format_tags=creation_time",
  "-of", "csv=p=0", AUDIO_SRC]).trim();
// UTC, as the container recorded it. Reading it in local time would restamp an
// eight-year-old tape with wherever the rendering machine happens to be.
const t0 = created ? new Date(created) : null;

// Clock time is SOURCE time — it should read the same second whether or not the
// reel opens on a held frame, so the caller passes srcT, not reel t.
const clockText = (srcT) => {
  const d = new Date(t0.getTime() + srcT * 1000);
  const p = (n) => String(n).padStart(2, "0");
  return `${d.getUTCFullYear()}.${p(d.getUTCMonth() + 1)}.${p(d.getUTCDate())} ` +
    `${p(d.getUTCHours())}:${p(d.getUTCMinutes())}:${p(d.getUTCSeconds())}`;
};
if (t0) console.log(`  clock starts ${clockText(0)}`);

// ── 6. group into phrases ───────────────────────────────────────────────────
// A phrase is as many words as fit on one line. It appears when its first word
// is spoken and leaves when its last one ends, so the eye never tracks a line
// that has already gone quiet.
const phrases = [];
let cur = [];
let curW = 0;
for (const w of words) {
  const add = w.dw + (cur.length ? CAP.wordGap : 0);
  if (cur.length && curW + add > CAP.maxLineW) {
    phrases.push(cur);
    cur = [];
    curW = 0;
  }
  cur.push(w);
  curW += w.dw + (cur.length > 1 ? CAP.wordGap : 0);
}
if (cur.length) phrases.push(cur);

for (const p of phrases) {
  p.from = p[0].from;
  p.to = p[p.length - 1].to;
  p.width = p.reduce((s, w, i) => s + w.dw + (i ? CAP.wordGap : 0), 0);
}
console.log(`  ${phrases.length} phrases`);

// The phrase on screen is the last one to have *started*, never merely the first
// one that matches. Consecutive phrases overlap — one is still fading out as the
// next leads in — and matching first-wins would let the dying phrase hold the
// screen while the incoming line never appeared at all.
const phraseAt = (t) => {
  let best = null;
  for (const p of phrases) {
    if (t < p.from - CAP.lead) break; // phrases are in time order
    best = p;
  }
  if (!best || t > best.to + CAP.tail) return null;
  return best;
};

// ── 7. render ───────────────────────────────────────────────────────────────
const SRC_FRAMES = Math.round(DUR * FPS);
const FRAMES = LEAD_FRAMES + SRC_FRAMES;
const SRC_W = track.srcW;
const SRC_H = track.srcH;
const FRAME_BYTES = SRC_W * SRC_H * 4;

// The stamps' glow rides the finished mix, so they pulse with his voice rather
// than on a timer.
const { audio, sr } = decodeAudioMono(AUDIO);
const envelope = computeRmsEnvelope(audio, sr, FPS, TOTAL);
const envAt = (f) => envelope[Math.min(envelope.length - 1, Math.max(0, f))] || 0;

// The side stamps carry the /pop look: the title word climbs the columns beside
// the pals, exactly as every other AC reel. The clock is separate furniture, up
// in the corner (below) — the two don't share a home.
const stamps = await makeSideStamps({
  w: W, h: H, fps: FPS, frames: FRAMES,
  assetsDir: join(WORK, "stamps"),
  title: STAMP_TITLE,
  stampSize: STAMP_SIZE,
  titlePx: STAMP_TITLE_PX,
  charScale: STAMP_TITLE_SCALE,
  chromaPx: STAMP_CHROMA,
});

// The clock's own glyph atlas, drawn top-left each frame (see the loop).
const clockAtlas = t0
  ? await makeChromaGlyphs({ charset: "0123456789.: ", ptSize: 128, assetsDir: join(WORK, "clock") })
  : null;
const CLOCK_SCALE = 0.42; // a little timer, not a headline
const CLOCK_KERN = 1;
const CLOCK_MARGIN = 40; // gap from the right and bottom edges
const CLOCK_BASELINE = H - 70; // vertical center of the timer, near the bottom

console.log(`· rendering ${FRAMES} frames → ${basename(OUT)}`);

// The picture is played exactly as shot — the sync correction was spent on the
// audio, where it could be exact.
const dec = spawn("ffmpeg", [
  "-v", "error", "-i", SRC,
  "-f", "rawvideo", "-pix_fmt", "rgba",
  "-vf", USE_GRADE ? `fps=${FPS},${GRADE}` : `fps=${FPS}`,
  "-t", String(DUR), "-",
], { stdio: ["ignore", "pipe", "inherit"] });

// The second decoder — the original, for its real face texture. Same grade so
// the blended skin matches the base's color, same fps so frame N lines up with
// frame N of the upscale (Topaz preserved the 1:1 frame timing).
let ORIG_W = 0;
let ORIG_H = 0;
let ORIG_BYTES = 0;
let decO = null;
if (FACE_RESTORE) {
  [ORIG_W, ORIG_H] = sh("ffprobe", ["-v", "error", "-select_streams", "v",
    "-show_entries", "stream=width,height", "-of", "csv=p=0", AUDIO_SRC])
    .trim().split(",").map(Number);
  ORIG_BYTES = ORIG_W * ORIG_H * 4;
  decO = spawn("ffmpeg", [
    "-v", "error", "-i", AUDIO_SRC,
    "-f", "rawvideo", "-pix_fmt", "rgba",
    "-vf", USE_GRADE ? `fps=${FPS},${GRADE}` : `fps=${FPS}`,
    "-t", String(DUR), "-",
  ], { stdio: ["ignore", "pipe", "inherit"] });
  console.log(`  face-restore: original ${ORIG_W}×${ORIG_H} → face @ ${FACE_STRENGTH}`);
}

// Not preview-shared's spawnFFmpegEncode — that one takes no filters, and the
// sharpening has to happen here, on the far side of the upscale.
const enc = spawn("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", // node-canvas raw is native byte order
  "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-i", AUDIO,
  ...(USE_GRADE ? ["-vf", CRISP] : []),
  "-c:v", "libx264", "-preset", "slow", "-crf", "16",
  "-c:a", "aac", "-b:a", "192k",
  "-pix_fmt", "yuv420p", "-shortest", "-movflags", "+faststart",
  OUT,
], { stdio: ["pipe", "inherit", "inherit"] });

const src = createCanvas(SRC_W, SRC_H);
const sctx = src.getContext("2d");
const dst = createCanvas(W, H);
const ctx = dst.getContext("2d");
ctx.imageSmoothingEnabled = true;
ctx.imageSmoothingQuality = "high"; // it's a 2.67× upscale; it needs the help

// Face-restore scratch: the original frame, and an offscreen where the original
// face is feathered before it's laid over the base.
const srcO = FACE_RESTORE ? createCanvas(ORIG_W, ORIG_H) : null;
const sctxO = srcO ? srcO.getContext("2d") : null;
const faceCv = FACE_RESTORE ? createCanvas(W, H) : null;
const faceCtx = faceCv ? faceCv.getContext("2d") : null;
if (faceCtx) {
  faceCtx.imageSmoothingEnabled = true;
  faceCtx.imageSmoothingQuality = "high";
}
const O2U = FACE_RESTORE ? ORIG_W / SRC_W : 1; // upscaled coords → original coords

// The face box (upscaled coords) for a source time, smoothed against jitter and
// grown past Vision's tight crop to take in forehead and jaw — the parts that
// went soft, not just the features.
const faceSamples = track.samples;
const faceBox = (st) => {
  let s = faceSamples[0];
  for (const c of faceSamples) { if (c.t > st) break; s = c; }
  const gx = s.w * 0.28;
  const gy = s.h * 0.22;
  return { x: s.x - gx, y: s.y - gy, w: s.w + gx * 2, h: s.h + gy * 2 };
};

let frame = 0;

// A 16×16 thumbnail of the cropped frame, which is plenty to ask "what color is
// this shot." We take the hue of the most *colorful* pixels rather than the mean:
// average a face, a blue shirt and a green wall together and you get mud, and mud
// has no hue to push.
const SAMP = 16;
const samp = createCanvas(SAMP, SAMP);
const sampCtx = samp.getContext("2d");
let hueR = 0;
let hueG = 0;
let hueB = 0; // EMA'd, or the tint strobes frame to frame

const sampleTint = () => {
  const d = sampCtx.getImageData(0, 0, SAMP, SAMP).data;
  let wr = 0;
  let wg = 0;
  let wb = 0;
  let wsum = 0;
  for (let i = 0; i < d.length; i += 4) {
    const r = d[i];
    const g = d[i + 1];
    const b = d[i + 2];
    const sat = Math.max(r, g, b) - Math.min(r, g, b); // colorfulness = weight
    wr += r * sat;
    wg += g * sat;
    wb += b * sat;
    wsum += sat;
  }
  if (wsum < 1) return null;
  const k = 0.06; // slow — the chrome drifts through the shot's palette
  hueR += (wr / wsum - hueR) * k;
  hueG += (wg / wsum - hueG) * k;
  hueB += (wb / wsum - hueB) * k;
  const seen = dayglow([hueR, hueG, hueB]);
  const palette = [[255, 30, 180], [30, 255, 215], [255, 225, 25], [115, 70, 255]];
  const pos = (frame / FPS / 4.5) % palette.length;
  const a = palette[Math.floor(pos)];
  const b = palette[(Math.floor(pos) + 1) % palette.length];
  const f = pos - Math.floor(pos);
  const cycle = a.map((c, i) => c * (1 - f) + b[i] * f);
  const core = seen.map((c, i) => Math.round(c * 0.58 + cycle[i] * 0.42));
  return { core, hi: cycle.map((c) => Math.min(255, Math.round(c + 55))) };
};

const write = (buf) =>
  new Promise((res) => (enc.stdin.write(buf) ? res() : enc.stdin.once("drain", res)));

// `frame` is reel time. The footage is DUR long but the reel is LEAD longer, so
// anything that reads the source — the camera, the pixels — asks in SOURCE time,
// while everything laid on top (captions, stamps, clock) asks in reel time. The
// sound was delayed by exactly LEAD_FRAMES too, which is what keeps the lips on
// the words across the join.
const drawFrame = async (rgba, rgbaO) => {
  const t = frame / FPS;
  const srcT = Math.max(0, t - LEAD);

  sctx.putImageData(createImageData(new Uint8ClampedArray(rgba), SRC_W, SRC_H), 0, 0);

  const { sx, sy, sw, sh: cropH } = at(srcT);
  ctx.drawImage(src, sx, sy, sw, cropH, 0, 0, W, H);

  // Face restore: lay the original's real skin over the waxed upscale, but only
  // inside the face and only where it's needed. The blend fades from strong at
  // the center to nothing at the box edge (a radial mask), so there is no seam
  // between restored skin and upscaled jaw/hair.
  if (FACE_RESTORE && rgbaO) {
    sctxO.putImageData(createImageData(new Uint8ClampedArray(rgbaO), ORIG_W, ORIG_H), 0, 0);
    const fb = faceBox(srcT);
    // face box (upscaled coords) → output coords via the same crop mapping
    const ox = ((fb.x - sx) / sw) * W;
    const oy = ((fb.y - sy) / cropH) * H;
    const ow = (fb.w / sw) * W;
    const oh = (fb.h / cropH) * H;

    if (ow > 2 && oh > 2) {
      faceCtx.clearRect(0, 0, W, H);
      // the original's matching region, upscaled to sit exactly over the base
      faceCtx.drawImage(
        srcO, fb.x * O2U, fb.y * O2U, fb.w * O2U, fb.h * O2U,
        ox, oy, ow, oh,
      );
      // feather to transparent at the edges
      const cx = ox + ow / 2;
      const cy = oy + oh / 2;
      // A big full-strength core, feathering only near the very edge — so most of
      // the face is real texture and only the rim crossfades into the upscale.
      const grad = faceCtx.createRadialGradient(cx, cy, Math.min(ow, oh) * 0.44, cx, cy, Math.max(ow, oh) * 0.68);
      grad.addColorStop(0, "rgba(0,0,0,1)");
      grad.addColorStop(1, "rgba(0,0,0,0)");
      faceCtx.globalCompositeOperation = "destination-in";
      faceCtx.fillStyle = grad;
      faceCtx.fillRect(ox - ow, oy - oh, ow * 3, oh * 3);
      faceCtx.globalCompositeOperation = "source-over";

      ctx.globalAlpha = FACE_STRENGTH;
      ctx.drawImage(faceCv, 0, 0);
      ctx.globalAlpha = 1;
    }
  }

  sampCtx.drawImage(src, sx, sy, sw, cropH, 0, 0, SAMP, SAMP);
  const tint = sampleTint();

  const p = phraseAt(t);
  if (p) {
    // Fade in over the lead, out over the tail — a hard cut on a caption reads
    // as a glitch at 30fps.
    const fade = Math.min(1, (t - (p.from - CAP.lead)) / 0.18, (p.to + CAP.tail - t) / 0.28);
    let x = (W - p.width) / 2;
    const y = H * CAP.bandY;

    for (const w of p) {
      const spoken = Math.min(1, Math.max(0, (t - w.from) / Math.max(0.001, w.to - w.from)));
      // Every word in the phrase stays legible; the accent sweep — not opacity —
      // is what tells you where the voice is. Dimming the not-yet-spoken words
      // enough to be visible on a flat cover leaves them invisible on skin.
      ctx.globalAlpha = Math.max(0, fade) * (spoken > 0 ? 1 : 0.86);
      ctx.drawImage(w.imgCream, x, y, w.dw, w.dh);

      if (spoken > 0) {
        ctx.save();
        ctx.beginPath();
        ctx.rect(x, y, w.dw * spoken, w.dh); // the fill sweeps with the voice
        ctx.clip();
        ctx.drawImage(w.imgAccent, x, y, w.dw, w.dh);
        ctx.restore();
      }
      x += w.dw + CAP.wordGap;
    }
    ctx.globalAlpha = 1;
  }

  if (USE_STAMPS) stamps.draw(ctx, t, envAt(frame), tint);

  // A little timer, bottom-right, ticking in real 2018 seconds — SOURCE time, so
  // it holds on the opening frame and starts when the footage does. Right-aligned
  // (measured, then laid out backward from the right margin) so its right edge
  // stays pinned as the digits change width. Same chroma fringe as the stamps.
  if (clockAtlas) {
    const text = clockText(srcT);
    const env = envAt(frame);
    const col = tint?.core || [235, 240, 250];
    const colHi = tint?.hi || [255, 255, 255];
    const width = clockAtlas.measure(text, CLOCK_SCALE, CLOCK_KERN);
    let x = W - CLOCK_MARGIN - width;
    for (const [i, ch] of [...text].entries()) {
      const g = clockAtlas.glyphs.get(ch);
      if (!g) {
        x += clockAtlas.spaceW * CLOCK_SCALE + CLOCK_KERN;
        continue;
      }
      const dw = g.width * CLOCK_SCALE;
      const dh = g.height * CLOCK_SCALE;
      const ph = t * 1.6 - i * 0.16;
      const bobY = 2 * Math.sin(ph * 2);
      const swell = 1 + 0.05 * Math.sin(ph * 2 + 0.6) * (0.5 + env);
      ctx.save();
      ctx.translate(x + dw / 2, CLOCK_BASELINE + bobY);
      ctx.scale(swell, swell);
      stamps.drawChroma(ctx, stamps.tintGlyph, g, g, -dw / 2, -dh / 2, dw, dh, env, col, colHi);
      ctx.restore();
      x += dw + CLOCK_KERN;
    }
  }

  await write(dst.toBuffer("raw"));
  frame += 1;
  if (frame % 150 === 0) process.stdout.write(`\r  ${frame}/${FRAMES}`);
};

// Pull one fixed-size frame at a time from a decoder's byte stream. With two
// decoders (upscale + original) they have to advance in lockstep, so an on-demand
// reader is cleaner than accumulating each stream's chunks independently.
const makeReader = (stream, bytes) => {
  const it = stream[Symbol.asyncIterator]();
  let buf = Buffer.alloc(0);
  let done = false;
  return async () => {
    while (buf.length < bytes && !done) {
      const { value, done: d } = await it.next();
      if (d) { done = true; break; }
      buf = buf.length ? Buffer.concat([buf, value]) : value;
    }
    if (buf.length < bytes) return null;
    const f = buf.subarray(0, bytes);
    buf = buf.subarray(bytes);
    return f;
  };
};

const nextUp = makeReader(dec.stdout, FRAME_BYTES);
const nextOrig = decO ? makeReader(decO.stdout, ORIG_BYTES) : async () => null;

for (let sf = 0; sf < SRC_FRAMES; sf += 1) {
  const up = await nextUp();
  if (!up) break;
  const orig = await nextOrig();

  // Open on a held frame — the runway the recording never gave us. Both decoders
  // are held on their first frame together so the face restore stays aligned.
  if (sf === 0) {
    for (let i = 0; i < LEAD_FRAMES; i += 1) await drawFrame(up, orig);
  }
  await drawFrame(up, orig);
}

enc.stdin.end();
await new Promise((res) => enc.on("close", res));
process.stdout.write(`\r  ${frame}/${FRAMES}\n`);

rmSync(GLYPHS, { recursive: true, force: true });
console.log(`✓ ${OUT}`);
