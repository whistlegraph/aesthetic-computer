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
// Builds live under Working Desktop (the auto-cleaned ~/Desktop has
// erased mid-render outputs in the past — see [[feedback_desktop_autocleaned]]).
const BUILDS_ROOT = `${homedir()}/Documents/Working Desktop/builds`;

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
        variant: "v15", // 1024x1536 portrait — degradation arc: apple peeks through scrap, pals only on backsides, green war-guilt drones, whistlegraph→robotic-nightmare morph
        prelude: `${SECDIR_ROOT}/trancenwaltz-sections/intro-prelude/gens/v15.png`,
      },
      {
        // 16:9 YouTube visualizer — the full cover-video chrome
        // (title / multi-lane piano-roll / karaoke / progress)
        // reframed landscape. Uses v10b (1536×1024 native landscape,
        // regenerated from cover-prompt-landscape.txt) so the
        // top/bottom crop is gentle and the canonical
        // whistlegraph-butterfly lid scrap lands correctly.
        // Upload via toolchain/youtube/yt.mjs.
        suffix: "youtube",
        size: "1920x1080",
        variant: "v10b",
        prelude: `${SECDIR_ROOT}/trancenwaltz-sections/intro-prelude/gens/v10.png`,
      },
      {
        // Spotify Canvas: chrome-free, SILENT, 9:16, rapid cycle through
        // every section illy → seamless 6 s loop. Rendered by
        // canvas-loop.mjs (NOT cover-video.mjs — no chrome at all).
        suffix: "canvas",
        size: "1080x1920",
        variant: "v15",
        canvas: true,
        dur: 6,
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
        variant: "v16", // photographic calm Trader Joe's shop w/ pixie crew
        prelude: null, // chill mix has no greeting → no prelude swap
      },
      {
        // IG-story cut — midnight-unravelling portrait set (v40p):
        // overtly-unsettling foggy 3am Trader Joe's, walls of reality
        // tearing, crew uncanny, jeffrey calm-blank. Latest 7-section
        // composition (build2 dropped). Built from
        // gens/trancenwaltzi-sections/<sec>/gens/v40p.png.
        suffix: "vertical",
        size: "1080x1920",
        variant: "v40p",
        prelude: null, // chill mix has no greeting → no prelude swap
      },
      {
        // Same Canvas model — staged for when trancenwaltzi releases.
        suffix: "canvas",
        size: "1080x1920",
        variant: "v25p", // latest happy-arc portrait set
        canvas: true,
        dur: 6,
      },
    ],
  },
  trancepenta: {
    title: "trancepenta",
    bpm: "126",
    // trancepenta's audio is the 4-stage v13 master (engine --hell 13
    // --meter 5 dorian --gallop --beat-in 30 → scratch-mix →
    // place-penta-vocal → finalize-penta-vocal). build.mjs can't
    // express that as one trance.mjs call, so a `bakeScript` runs the
    // deterministic pop/dance/bin/bake-trancepenta.sh, which writes
    // the MASTER + .mp3 + the paired preBright .assets/struct.json into
    // the SHARED twi-out dir. `audioSrc`/`structSrc` say where to pull
    // the finished master + struct from.
    audio: ["--mode", "chill", "--meter", "5"], // (descriptive; bakeScript drives the real render)
    bakeScript: `${REPO}/pop/dance/bin/bake-trancepenta.sh`,
    audioSrc: `${homedir()}/Documents/Working Desktop/twi-out/trancepenta-MASTER.wav`,
    structSrc: `${homedir()}/Documents/Working Desktop/twi-out/trancepenta-MASTER-preBright.wav.assets/struct.json`,
    sectionsDir: `${SECDIR_ROOT}/trancepenta-sections`,
    hideLanes: null, // dense chill mix — show every populated lane
    noLyrics: true,  // DistroKid/IG rule — NO baked lyric/greeting text
    // DISMAL palette — greyer / lower-chroma than trancenwaltzi's
    // bright defaults. Mirrors the per-section diegetic colours the
    // illy prompts bake in (drained slate-blue → cyan-grey → sick
    // teal → cold cyan-blue → bruised violet → acid grey-green → cold
    // steel-blue → ashen). Passed to cover-video via --section-tints.
    sectionTints: {
      intro:  "rgba(78,96,116,0.20)",   // drained slate-blue
      break1: "rgba(86,118,128,0.20)",  // cold cyan-grey
      build1: "rgba(70,108,108,0.20)",  // dim sick teal
      drop1:  "rgba(72,104,140,0.20)",  // cold cyan-blue (the flare)
      break2: "rgba(96,90,118,0.20)",   // bruised violet-grey
      build2: "rgba(96,108,92,0.20)",   // acid grey-green
      drop2:  "rgba(80,98,128,0.20)",   // cold steel-blue (final flood)
      outro:  "rgba(92,100,112,0.20)",  // ashen grey-blue
    },
    formats: [
      {
        // Vertical 2:3 portrait set — the after-hours Trader Joe's /
        // jeffrey-teaching-young-Zuck / pixsies-watching set (v2, with
        // per-section dismal tint + PALS lid back-glow + screen-spill
        // on faces). v1 was the dismal foggy-harbour first pass, kept
        // on disk as .v1 backup. Primary deliverable = full vertical +
        // a 30s IG-story cut.
        suffix: "vertical",
        size: "1080x1920",
        variant: "v2",
        prelude: null, // chill mix has no greeting → no prelude swap
      },
      {
        // 16:9 YouTube visualizer — same model as the trancenwaltz
        // youtube format. v10b = native-landscape (1536×1024) illys
        // regenerated from cover-prompt-landscape.txt; ZOOM_DAMP on
        // landscape halves the ken-burns amplitude in cover-video.mjs.
        // Upload via toolchain/youtube/yt.mjs.
        suffix: "youtube",
        size: "1920x1080",
        variant: "v10b",
        prelude: null,
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
if (!flags["video-only"] && CFG.bakeScript) {
  // Multi-stage bake (trancepenta): run the deterministic bake script,
  // then copy its finished MASTER + paired struct into the build dir.
  audioCmd = ["bash", CFG.bakeScript];
  if (flags["reuse-bake"] && existsSync(CFG.audioSrc) && existsSync(CFG.structSrc)) {
    console.log(`▸ audio · REUSE-BAKE (skip ${CFG.bakeScript}; using cached ${CFG.audioSrc})`);
  } else {
    console.log(`▸ audio · bake · ${audioCmd.join(" ")}`);
    const r = spawnSync(audioCmd[0], audioCmd.slice(1), { stdio: "inherit" });
    if (r.status !== 0) { console.error("✗ audio bake failed"); process.exit(r.status || 1); }
  }
  if (!existsSync(CFG.audioSrc)) { console.error(`✗ bake produced no master at ${CFG.audioSrc}`); process.exit(1); }
  // cover-video probes the mp3; copy the master (as .mp3 container is
  // fine — cover-video only ffprobes duration + decodes via ffmpeg).
  spawnSync("ffmpeg", ["-y", "-i", CFG.audioSrc, "-b:a", "320k", audioOut], { stdio: "ignore" });
  mkdirSync(`${buildDir}/${NAME}.assets`, { recursive: true });
  copyFileSync(CFG.structSrc, `${buildDir}/${NAME}.assets/struct.json`);
  console.log(`▸ audio · baked master + struct → ${buildDir}`);
} else if (!flags["video-only"]) {
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
    // --only <suffix> cuts just one format (e.g. --only vertical) so a
    // single-cut doesn't re-render the other formats. "square" matches
    // the default no-suffix format.
    if (flags.only && (fmt.suffix || "square") !== flags.only) {
      console.log(`▸ video [${fmt.suffix || "square"}] · SKIPPED (--only ${flags.only})`);
      continue;
    }
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
    let cmd;
    if (fmt.canvas) {
      // Spotify Canvas — its own renderer, no audio/chrome at all.
      cmd = [
        "node", `${REPO}/pop/dance/bin/canvas-loop.mjs`,
        "--illustrations", ILLUS_MAP,
        "--size", fmt.size,
        "--dur", String(fmt.dur || 6),
        "--out", out,
      ];
    } else {
      cmd = [
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
      // Multi-stage tracks ship their struct alongside the audio in
      // the build dir — point cover-video at it explicitly (its
      // <track>.assets/struct.json default also resolves, but be
      // explicit so a reused/baked struct is unambiguous).
      if (CFG.bakeScript) {
        cmd.push("--struct", `${buildDir}/${NAME}.assets/struct.json`);
      }
      // DISMAL per-section palette override (greyer than the bright
      // cover-video defaults).
      if (CFG.sectionTints) {
        cmd.push("--section-tints", JSON.stringify(CFG.sectionTints));
      }
      // NO baked lyric / greeting text (DistroKid + IG). cover-video
      // only loads karaoke when the lyrics file exists; point it at a
      // guaranteed-absent path so the text layer is fully skipped.
      if (CFG.noLyrics) {
        cmd.push("--lyrics", `${buildDir}/.NO_LYRICS_${NAME}.json`);
      }
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
