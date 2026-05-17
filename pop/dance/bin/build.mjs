#!/usr/bin/env node
// build.mjs — versioned build orchestrator for the dance pop tracks.
//
// Each invocation picks the NEXT build number for a given track and
// writes ALL produced artifacts into an isolated build directory:
//
//   ~/Desktop/builds/<name>/b<NNN>/
//     <name>.mp3                — rendered audio
//     <name>.assets/struct.json — composition struct (always paired)
//     <name>.assets/...         — lane previews + title chars
//     <name>-cover.mp4          — rendered cover video
//     manifest.json             — what params were used (CLI + paths + git sha)
//
// Build numbers are zero-padded 3-digit (b001, b002, …) and auto-
// increment per track. Nothing is overwritten — every build is a
// distinct directory on disk. The renderer scripts (trance.mjs +
// cover-video.mjs) are invoked with --out pointing into the build dir
// so they write directly to the right place.
//
// Usage:
//   node pop/dance/bin/build.mjs trancenwaltz
//   node pop/dance/bin/build.mjs trancenwaltzi --note "war progression v8"
//   node pop/dance/bin/build.mjs trancenwaltz --audio-only
//   node pop/dance/bin/build.mjs trancenwaltz --video-only --reuse-audio b007
//
// Flags:
//   --note "..."        free-form label saved into manifest.json
//   --audio-only        skip the video render (just produce the MP3 + struct)
//   --video-only        skip the audio render — uses the audio from --reuse-audio
//   --reuse-audio bNNN  point the video render at a prior build's audio
//   --no-prelude        skip the head-down/laptops-closed prelude overlay
//
// Per-track CONFIG below defines: audio-renderer flags (meter, mode),
// section illustration set, prelude path, hide-lanes, BPM, title.

import { execFileSync, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, writeFileSync, copyFileSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { homedir } from "node:os";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const BUILDS_ROOT = `${homedir()}/Desktop/builds`;

const argv = process.argv.slice(2);
if (argv.length === 0 || argv[0].startsWith("--")) {
  console.error("usage: build.mjs <track-name> [flags]");
  console.error("       <track-name> = trancenwaltz | trancenwaltzi");
  process.exit(1);
}
const NAME = argv.shift();
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}

// ── per-track configuration ──────────────────────────────────────────
const SECDIR_ROOT = `${homedir()}/Documents/Working Desktop/gens`;
function illusMap(secDir, variant = "v8") {
  const pairs = [];
  for (const sec of ["intro", "break1", "build1", "drop1", "break2", "build2", "drop2", "outro"]) {
    const p = join(secDir, sec, "gens", `${variant}.png`);
    if (existsSync(p)) pairs.push(`${sec}=${p}`);
  }
  return pairs.join(",");
}
// Each video format declares its own canvas size, the variant suffix
// of the section illustrations to use (square gets a 1:1 native set,
// vertical gets the 2:3 portrait set), and the prelude image at the
// matching aspect. The first entry is the default cover; later entries
// suffix the output filename (e.g. <name>-cover-vertical.mp4).
const CONFIGS = {
  trancenwaltz: {
    title: "trancenwaltz",
    bpm: "137.143",
    // Sung-vox stem is pre-rendered via pop/dance/bin/vocal.mjs and
    // saved to ~/Desktop/trance-sung-vox.mp3. trance.mjs mixes it in
    // via --vocal-stem when present.
    audio: [
      "--meter", "3",
      "--vocal-stem", `${homedir()}/Desktop/trance-sung-vox.mp3`,
    ],
    sectionsDir: `${SECDIR_ROOT}/trancenwaltz-sections`,
    hideLanes: null,
    formats: [
      {
        suffix: null, // default cover — no suffix → <name>-cover.mp4
        size: "1500x1500",
        variant: "v10", // 1024x1024 native square illustrations (diverse pixie casting)
        prelude: `${SECDIR_ROOT}/trancenwaltz-sections/intro-prelude/gens/v10.png`,
      },
      {
        suffix: "vertical",
        size: "1080x1920",
        variant: "v8", // 1024x1536 native portrait illustrations
        prelude: `${SECDIR_ROOT}/trancenwaltz-sections/intro-prelude/gens/v1.png`,
      },
    ],
  },
  trancenwaltzi: {
    title: "trancenwaltzi",
    bpm: "137.143",
    audio: ["--mode", "chill", "--meter", "3"],
    sectionsDir: `${SECDIR_ROOT}/trancenwaltzi-sections`,
    hideLanes: "piano",
    formats: [
      {
        suffix: null,
        size: "1500x1500",
        variant: "v9",
        prelude: null, // chill mix has no greeting → no prelude swap
      },
    ],
  },
};
const CFG = CONFIGS[NAME];
if (!CFG) { console.error(`unknown track '${NAME}'. options: ${Object.keys(CONFIGS).join(", ")}`); process.exit(1); }

// ── pick next build number ───────────────────────────────────────────
const trackRoot = `${BUILDS_ROOT}/${NAME}`;
mkdirSync(trackRoot, { recursive: true });
const existingBuilds = readdirSync(trackRoot)
  .filter((d) => /^b\d{3}$/.test(d))
  .map((d) => parseInt(d.slice(1), 10))
  .sort((a, b) => a - b);
const nextNum = (existingBuilds[existingBuilds.length - 1] ?? 0) + 1;
const buildId = `b${String(nextNum).padStart(3, "0")}`;
const buildDir = `${trackRoot}/${buildId}`;
mkdirSync(buildDir, { recursive: true });

const audioOut = `${buildDir}/${NAME}.mp3`;
const videoOut = `${buildDir}/${NAME}-cover.mp4`;

console.log(`▸ build ${NAME}/${buildId} → ${buildDir}`);

// ── git sha for the manifest ─────────────────────────────────────────
let gitSha = null;
try {
  gitSha = execFileSync("git", ["rev-parse", "--short", "HEAD"], { cwd: REPO, encoding: "utf8" }).trim();
} catch {}

// ── audio render ──────────────────────────────────────────────────────
let audioCmd = null;
if (!flags["video-only"]) {
  audioCmd = ["node", `${REPO}/recap/bin/trance.mjs`, ...CFG.audio, "--out", audioOut];
  console.log(`▸ audio · ${audioCmd.join(" ")}`);
  const r = spawnSync(audioCmd[0], audioCmd.slice(1), { stdio: "inherit" });
  if (r.status !== 0) { console.error("✗ audio render failed"); process.exit(r.status || 1); }
} else if (flags["reuse-audio"]) {
  const srcDir = `${trackRoot}/${flags["reuse-audio"]}`;
  if (!existsSync(`${srcDir}/${NAME}.mp3`)) {
    console.error(`✗ --reuse-audio ${flags["reuse-audio"]} not found at ${srcDir}/${NAME}.mp3`);
    process.exit(1);
  }
  // Copy the audio + struct into this build dir so the video render
  // sees them locally and the build dir is self-contained.
  copyFileSync(`${srcDir}/${NAME}.mp3`, audioOut);
  mkdirSync(`${buildDir}/${NAME}.assets`, { recursive: true });
  copyFileSync(`${srcDir}/${NAME}.assets/struct.json`, `${buildDir}/${NAME}.assets/struct.json`);
  console.log(`▸ audio · REUSED from ${flags["reuse-audio"]}`);
} else {
  console.error("✗ --video-only requires --reuse-audio <bNNN>");
  process.exit(1);
}

// ── video render(s) — one per declared format, each with its own
//                     variant (square vs portrait illustrations) ────
const videoCmds = [];
const extraOutputs = {};
if (!flags["audio-only"]) {
  for (const fmt of CFG.formats || []) {
    const ILLUS_MAP = illusMap(CFG.sectionsDir, fmt.variant);
    const introIllus = `${CFG.sectionsDir}/intro/gens/${fmt.variant}.png`;
    if (!existsSync(introIllus)) {
      console.error(`✗ missing intro illustration for variant ${fmt.variant}: ${introIllus}`);
      process.exit(1);
    }
    const out = fmt.suffix
      ? videoOut.replace(/\.mp4$/, `-${fmt.suffix}.mp4`)
      : videoOut;
    if (fmt.suffix) extraOutputs[fmt.suffix] = out;
    const cmd = [
      "node", `${REPO}/pop/dance/bin/cover-video.mjs`,
      "--track", audioOut,
      "--illustration", introIllus,
      "--illustrations", ILLUS_MAP,
      "--title", CFG.title,
      "--bpm", CFG.bpm,
      "--size", fmt.size,
      "--out", out,
    ];
    if (fmt.prelude && !flags["no-prelude"] && existsSync(fmt.prelude)) {
      cmd.push("--prelude", fmt.prelude);
    }
    if (CFG.hideLanes) {
      cmd.push("--hide-lanes", CFG.hideLanes);
    }
    videoCmds.push({ kind: fmt.suffix || "square", out, cmd, fmt });
  }
  for (const job of videoCmds) {
    console.log(`▸ video [${job.kind} · ${job.fmt.size} · variant=${job.fmt.variant}] · ${job.cmd.join(" ")}`);
    const r = spawnSync(job.cmd[0], job.cmd.slice(1), { stdio: "inherit" });
    if (r.status !== 0) { console.error(`✗ video [${job.kind}] render failed`); process.exit(r.status || 1); }
  }
} else {
  console.log(`▸ video · SKIPPED (--audio-only)`);
}

// ── manifest ──────────────────────────────────────────────────────────
const manifest = {
  name: NAME,
  buildId,
  buildNum: nextNum,
  builtAt: new Date().toISOString(),
  gitSha,
  note: flags.note || null,
  config: CFG,
  formats: (CFG.formats || []).map((f) => ({ ...f, prelude: f.prelude && !flags["no-prelude"] ? f.prelude : null })),
  audioCmd,
  videoCmds: videoCmds.map((j) => ({ kind: j.kind, out: j.out, cmd: j.cmd, ok: existsSync(j.out) })),
  reuseAudio: flags["reuse-audio"] || null,
  outputs: {
    audio: existsSync(audioOut) ? audioOut : null,
    video: existsSync(videoOut) ? videoOut : null,
    struct: existsSync(`${buildDir}/${NAME}.assets/struct.json`) ? `${buildDir}/${NAME}.assets/struct.json` : null,
    ...Object.fromEntries(Object.entries(extraOutputs).map(([k, v]) => [`video_${k}`, existsSync(v) ? v : null])),
  },
};
writeFileSync(`${buildDir}/manifest.json`, JSON.stringify(manifest, null, 2));

console.log(`✓ ${NAME}/${buildId} complete`);
if (manifest.outputs.audio) console.log(`  audio · ${manifest.outputs.audio}`);
if (manifest.outputs.video) console.log(`  video · ${manifest.outputs.video}`);
