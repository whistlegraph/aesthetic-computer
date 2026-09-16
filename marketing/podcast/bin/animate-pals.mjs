#!/usr/bin/env node
// animate-pals.mjs — turn still "pals" logo tiles into seamless LOOPING
// turnaround clips with Seedance 2.0, then encode each clip as an animated
// WebP (hero) and an animated PNG (APNG, for hosts that only take PNG).
//
// The loop trick: the same still is passed as both the start frame (image)
// and the end frame (endImage), so the last frame snaps back to the first
// and the object returns to its starting pose. The encoder drops that
// duplicated last frame so the loop has no hitch.
//
// Usage:
//   node bin/animate-pals.mjs <slug...>            one or more pals (e.g. nat-jade chrome)
//   node bin/animate-pals.mjs --all                every pal in system/backend/logo.mjs
//   node bin/animate-pals.mjs --missing            every pal without a local .mp4 yet
//   node bin/animate-pals.mjs --tray nat|cf|ig|av|materials
//
// Flags:
//   --tier fast|standard   Seedance tier (default standard: nicer facets/light)
//   --duration N           seconds per clip, 4–15 (default 5; Seedance rejects anything under 4)
//   --cheap                shorthand for --tier fast --duration 4 (≈ $0.97/pal vs $1.51)
//   --motion <style>       turntable | pulse | morph | auto (default auto: pulse for glowing
//                          materials, else turntable; morph is opt-in — it melts the mark)
//   --concurrency N        parallel fal jobs (default 3)
//   --encode-only          skip fal, just (re)encode webp+apng from existing .mp4s
//   --force                regenerate even if the .mp4 already exists
//   --dry                  print the plan + cost estimate and exit
//
// Outputs: out/pals/turnarounds/<slug>.{mp4,webp,apng}
// Stills are read from out/pals/<slug>.png, or fetched from the pals CDN.
// Then: node bin/publish-pals-turnaround.mjs <slug...>|--all

import { generateShot, RATE_PER_SEC } from "../../../pop/lib/fal.mjs";
import { logoSlugs } from "../../../system/backend/logo.mjs";
import { existsSync, mkdirSync, writeFileSync, readdirSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const STILLS = resolve(HERE, "..", "out", "pals");
const OUT = resolve(STILLS, "turnarounds");
mkdirSync(OUT, { recursive: true });

const PALS_CDN = "https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com";
const FPS = 24;

const argv = process.argv.slice(2);
const flag = (k, d = null) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : d; };
const has = (k) => argv.includes(`--${k}`);
const CHEAP = has("cheap");
const TIER = flag("tier", CHEAP ? "fast" : "standard");
const DURATION = Math.min(15, Math.max(4, Number(flag("duration", CHEAP ? 4 : 5))));
const MOTION = flag("motion", "auto");
const CONCURRENCY = Number(flag("concurrency", 3));
const ENCODE_ONLY = has("encode-only");
const FORCE = has("force");
const DRY = has("dry");

const allSlugs = logoSlugs.map((f) => f.replace(/^pals-/, "").replace(/\.png$/, ""));
const trayFilter = flag("tray");
let slugs = argv.filter((a) => !a.startsWith("--") && !argv.includes(`--${a}`) && allSlugs.includes(a));
if (has("all")) slugs = allSlugs;
if (trayFilter) {
  slugs = allSlugs.filter((s) => (trayFilter === "materials" ? !/^(nat|cf|ig|av)-/.test(s) : s.startsWith(`${trayFilter}-`)));
}
if (has("missing")) slugs = allSlugs.filter((s) => !existsSync(resolve(OUT, `${s}.mp4`)));
if (!slugs.length) {
  console.error("no pals selected — pass slugs, --all, --missing, or --tray <nat|cf|ig|av|materials>");
  console.error(`known: ${allSlugs.join(" ")}`);
  process.exit(1);
}

// ── motion prompts — every style returns to the exact start pose so the
// start=end frame trick loops. The look lives in the still; these are MOTION only.
const LOCK = "The flat background stays perfectly still and unchanged. Locked, centered camera with NO camera movement, no zoom, no pan. Crisp, high detail, even studio light, seamless loopable, no text, no extra objects, the object never leaves frame.";
const MOTIONS = {
  turntable:
    "product turntable: the sculpted logo object rotates smoothly and continuously through one full 360-degree spin on an invisible turntable and returns to the exact starting angle, a seamless loop. As it turns its surface catches soft studio light with gentle moving glints and highlights. Only the object rotates in place. " + LOCK,
  // morph: Seedance treats "jiggle like jelly" as licence to melt the mark into a
  // blob mid-loop (felt / balloon / bubblegum all did). Fun, but it loses the
  // logo, so it is opt-in only.
  morph:
    "the soft squishy logo object does one slow full 360-degree turn on an invisible turntable while gently bouncing and jiggling like jelly — a soft squash as it lands, a gentle stretch as it lifts — and settles back to the exact starting pose and angle, a seamless loop. Playful, bouncy, tactile. Only the object moves. " + LOCK,
  pulse:
    "the glowing logo object does one slow full 360-degree turn on an invisible turntable while its light breathes: the glow swells brighter then softens twice per turn, with soft bloom and faint flicker, returning to the exact starting brightness and angle, a seamless loop. Only the object moves and glows. " + LOCK,
};
function motionFor(slug) {
  if (MOTION !== "auto") return MOTION;
  if (/neon|holo|electric|spectrum|crystal|amethyst/.test(slug)) return "pulse";
  return "turntable";
}
function ensureStill(slug) {
  const p = resolve(STILLS, `${slug}.png`);
  if (existsSync(p)) return p;
  mkdirSync(STILLS, { recursive: true });
  const url = `${PALS_CDN}/pals-${slug}.png`;
  console.log(`  ⬇ ${slug}: fetching still ${url}`);
  execFileSync("curl", ["-sfL", "-o", p, url]);
  if (!existsSync(p)) throw new Error(`could not fetch still for ${slug}`);
  return p;
}

// ── encoders ────────────────────────────────────────────────────────────
function frameCount(mp4) {
  const out = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-count_frames",
    "-show_entries", "stream=nb_read_frames", "-of", "csv=p=0", mp4]).toString().trim();
  return Number(out) || Math.round(DURATION * FPS);
}
export function encodeTurnaround(slug) {
  const mp4 = resolve(OUT, `${slug}.mp4`);
  const webp = resolve(OUT, `${slug}.webp`);
  const apng = resolve(OUT, `${slug}.apng`);
  const keep = frameCount(mp4) - 1; // drop the duplicated last frame → clean loop
  const base = `trim=end_frame=${keep},setpts=PTS-STARTPTS`;

  // WebP: 720px @ 24fps lossy — this ffmpeg has no libwebp, so frames → img2webp.
  const frames = resolve(tmpdir(), `pals-frames-${slug}-${process.pid}`);
  rmSync(frames, { recursive: true, force: true });
  mkdirSync(frames, { recursive: true });
  execFileSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", mp4,
    "-vf", `${base},fps=${FPS},scale=720:-1:flags=lanczos`, resolve(frames, "%04d.png")]);
  const pngs = readdirSync(frames).filter((f) => f.endsWith(".png")).sort().map((f) => resolve(frames, f));
  execFileSync("img2webp", ["-loop", "0", "-d", String(Math.round(1000 / FPS)), "-lossy", "-q", "72", "-m", "4", ...pngs, "-o", webp]);
  rmSync(frames, { recursive: true, force: true });

  // APNG: 400px @ 12fps, 256-colour palette, mixed prediction — PNG frames are
  // lossless so this is the knob that keeps it near 2–3MB instead of 8MB+.
  execFileSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", mp4,
    "-vf", `${base},fps=12,scale=400:-1:flags=lanczos,split[a][b];[a]palettegen=max_colors=256:stats_mode=diff[p];[b][p]paletteuse=dither=sierra2_4a`,
    "-plays", "0", "-pred", "mixed", "-f", "apng", apng]);
  return { webp, apng };
}

// ── plan ─────────────────────────────────────────────────────────────────
const jobs = slugs.map((slug) => ({
  slug,
  motion: motionFor(slug),
  mp4: resolve(OUT, `${slug}.mp4`),
  skipFal: ENCODE_ONLY || (!FORCE && existsSync(resolve(OUT, `${slug}.mp4`))),
}));
const toGenerate = jobs.filter((j) => !j.skipFal);
const cost = toGenerate.length * DURATION * RATE_PER_SEC[TIER];
console.log(`pals turnarounds: ${jobs.length} selected · ${toGenerate.length} to generate on fal (${TIER}) ≈ $${cost.toFixed(2)}`);
for (const j of jobs) console.log(`  ${j.skipFal ? "· encode " : "★ fal    "} ${j.slug}  [${j.motion}]`);
if (DRY) process.exit(0);
if (toGenerate.length) writeFileSync(resolve(OUT, `.last-batch.json`), JSON.stringify({ at: new Date().toISOString(), tier: TIER, slugs: toGenerate.map((j) => j.slug) }, null, 2));

// ── run ─────────────────────────────────────────────────────────────────
const results = [];
let cursor = 0;
async function worker() {
  while (cursor < jobs.length) {
    const job = jobs[cursor++];
    const t0 = Date.now();
    try {
      if (!job.skipFal) {
        const still = ensureStill(job.slug);
        const r = await generateShot({
          image: still, endImage: still,
          prompt: MOTIONS[job.motion],
          duration: String(DURATION), ratio: "1:1", resolution: "720p", tier: TIER,
          outPath: job.mp4, label: `pals-turn-${job.slug}`,
        });
        if (!r.ok && !existsSync(job.mp4)) throw new Error(r.error || "fal failed");
      }
      const enc = encodeTurnaround(job.slug);
      results.push({ slug: job.slug, ok: true, motion: job.motion, ...enc, secs: ((Date.now() - t0) / 1000) | 0 });
      console.log(`  ✓ ${job.slug} (${job.motion}) ${results.at(-1).secs}s`);
    } catch (err) {
      results.push({ slug: job.slug, ok: false, error: err.message });
      console.log(`  ✗ ${job.slug}: ${err.message}`);
    }
  }
}
await Promise.all(Array.from({ length: Math.min(CONCURRENCY, jobs.length) }, worker));

const ok = results.filter((r) => r.ok);
console.log(`\ndone: ${ok.length}/${results.length} ok → ${OUT}`);
if (ok.length) console.log(`publish: node bin/publish-pals-turnaround.mjs ${ok.map((r) => r.slug).join(" ")}`);
process.exit(ok.length === results.length ? 0 : 1);
