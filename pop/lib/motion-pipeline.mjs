// pop/lib/motion-pipeline.mjs — the shared illy→motion workflow for
// /pop tracks. A lane driver (e.g. marimba/bin/gen-motion-marimbaba.mjs)
// supplies a SHOTS table + lane config; this module supplies the whole
// CLI: pricing, generation, take versioning, pick-based assembly.
//
// The iterative money-safe loop:
//   1. generate            — Seedance shots per section (cached; --force
//                            re-rolls ARCHIVE the old take, never delete)
//   2. audition            — pop/bin/audition-motion.mjs serves a takes
//                            board; picks land in out/motion/takes.json
//   3. --assemble          — trims the PICKED take of each section to its
//                            exact struct length, concats, muxes the track
//   …repeat 1–3 until the cut is right. Takes are never lost; a re-roll
//   is the only thing that costs money.
//
// Conventions (overridable via cfg):
//   panels   <lane>/out/<slug>-yt-sec-<i>-<name>.png
//   struct   <lane>/out/<slug>.struct.json  (sections[].name/startSec/endSec)
//   audio    <lane>/out/<slug>.mp3
//   shots    <lane>/out/motion/<slug>-shot-<i>-<name>.mp4 (+ archive/*.vN.mp4)
//   takes    <lane>/out/motion/takes.json   { sectionName: "relative/file.mp4" }
//   final    <lane>/out/<slug>-motion-yt.mp4 (previous cuts archived too)
//
// SHOTS table entry (per section name):
//   { motion: "…prompt…", morphTo: "sectionName" | undefined }
// morphTo uses the partner panel as the Seedance end-frame. Cuts are the
// default — morph ONLY same-camera escalations (see pop-motion-pipeline
// memory: cross-scene morphs invent doubled figures).

import { readFileSync, writeFileSync, existsSync, mkdirSync, renameSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { generateShot, RATE_PER_SEC } from "./fal.mjs";

export function parseFlags(argv = process.argv) {
  const flags = {};
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
    else { flags[a.slice(2)] = next; i++; }
  }
  return flags;
}

// Build the per-section shot list from struct + SHOTS table.
export function shotList(cfg) {
  const struct = JSON.parse(readFileSync(cfg.structPath, "utf8"));
  const panel = cfg.panelFor; // (name, index) => png path
  return struct.sections.map((s, i) => {
    const shot = cfg.shots[s.name];
    if (!shot) throw new Error(`SHOTS table missing section "${s.name}"`);
    const exact = s.endSec - s.startSec;
    const morphIdx = shot.morphTo ? struct.sections.findIndex((x) => x.name === shot.morphTo) : -1;
    return {
      i, name: s.name, exact,
      dur: Math.min(15, Math.max(4, Math.ceil(exact))),
      image: panel(s.name, i),
      endImage: shot.morphTo ? panel(shot.morphTo, morphIdx) : null,
      prompt: `${shot.motion}\n\n${cfg.mediumMotion || ""}`.trim(),
      out: `${cfg.motionDir}/${cfg.slug}-shot-${i}-${s.name}.mp4`,
    };
  });
}

function makeArchiver(cfg) {
  const dir = `${cfg.motionDir}/archive`;
  mkdirSync(dir, { recursive: true });
  return function archive(path) {
    if (!existsSync(path)) return;
    const base = path.split("/").pop().replace(/\.mp4$/, "");
    const n = readdirSync(dir).filter((f) => f.startsWith(base + ".v")).length + 1;
    const dest = `${dir}/${base}.v${n}.mp4`;
    renameSync(path, dest);
    console.log(`  ⌂ archived previous take → ${dest}`);
  };
}

// The full CLI. cfg: { slug, laneDir, structPath, panelFor, shots,
// mediumMotion, audio, finalOut, ratio?, resolution? }
export async function runMotionCli(cfg, flags = parseFlags()) {
  cfg.motionDir = cfg.motionDir || `${cfg.laneDir}/out/motion`;
  mkdirSync(cfg.motionDir, { recursive: true });
  const archive = makeArchiver(cfg);
  const TIER = flags.tier || "fast";
  const ONLY = flags.only ? String(flags.only).split(",") : null;
  const shots = shotList(cfg);

  for (const s of shots) {
    for (const p of [s.image, s.endImage].filter(Boolean)) {
      if (!existsSync(p)) { console.error(`✗ panel missing: ${p}`); process.exit(1); }
    }
  }

  const rate = RATE_PER_SEC[TIER];
  const billed = shots.reduce((a, s) => a + s.dur, 0);
  console.log(`▸ ${cfg.slug} motion pass · ${shots.length} shots · ${billed}s billed · ~$${(billed * rate).toFixed(2)} @ ${TIER} 720p`);

  // shots.json — the inputs manifest (prompt, panels, durations) so
  // ClipWizard and other tools can show exactly what each shot was
  // made from. Rewritten on every invocation; always current.
  writeFileSync(`${cfg.motionDir}/shots.json`, JSON.stringify(shots.map((s) => ({
    i: s.i, name: s.name, exact: s.exact, dur: s.dur,
    image: s.image, endImage: s.endImage, prompt: s.prompt,
    tier: TIER, ratePerSec: rate,
  })), null, 2));

  if (flags["dry-run"]) {
    for (const s of shots) {
      console.log(`\n── shot ${s.i} ${s.name} · ${s.dur}s (exact ${s.exact.toFixed(2)}s) ${s.endImage ? "MORPH" : "cut"}`);
      console.log(s.prompt);
    }
    return;
  }

  if (!flags.assemble) {
    const todo = shots.filter((s) =>
      (ONLY ? ONLY.includes(s.name) : true) && (flags.force || !existsSync(s.out)));
    // --prompt "<full text>" overrides the SHOTS-table prompt for a
    // single-shot re-roll (ClipWizard's editable re-roll). Per-run
    // experiment only — promote keepers into the SHOTS table (ask
    // Claude) so the manifest stays truthful.
    if (flags.prompt) {
      if (!ONLY || todo.length !== 1) {
        console.error("✗ --prompt requires --only with exactly one section");
        process.exit(1);
      }
      todo[0].prompt = String(flags.prompt);
      console.log(`  ✎ prompt override for ${todo[0].name}`);
    }
    if (!todo.length) {
      console.log("nothing to generate — --force re-rolls (old take archived), --assemble builds the cut");
      return;
    }
    console.log(`  generating ${todo.length} shot(s) concurrently …`);
    for (const s of todo) archive(s.out);
    const results = await Promise.all(todo.map((s) =>
      generateShot({
        image: s.image, endImage: s.endImage, prompt: s.prompt,
        duration: s.dur, ratio: cfg.ratio || "16:9",
        resolution: cfg.resolution || "720p", tier: TIER,
        outPath: s.out, label: `${s.i}-${s.name}`,
      }).then((r) => ({ s, r }))
    ));
    let failed = 0;
    for (const { s, r } of results) {
      if (r.ok) console.log(`✓ shot ${s.i} ${s.name} · seed ${r.seed} · ${(r.bytes / 1e6).toFixed(1)} MB`);
      else { failed++; console.error(`✗ shot ${s.i} ${s.name}: ${r.error}`); }
    }
    if (failed) { console.error(`✗ ${failed} shot(s) failed — rerun to retry (cached shots skip)`); process.exit(1); }
    console.log(`✓ all shots done — audition or --assemble`);
    return;
  }

  // ── assemble: picked takes → exact trims → concat → mux ────────────
  const takesPath = `${cfg.motionDir}/takes.json`;
  const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};
  const AUDIO = flags.audio ? resolve(process.cwd(), String(flags.audio)) : cfg.audio;
  const FINAL = flags.out ? resolve(process.cwd(), String(flags.out)) : cfg.finalOut;

  console.log("  trimming picked takes to exact section lengths …");
  const clipSeconds = (p) => {
    const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
      "-of", "default=noprint_wrappers=1:nokey=1", p], { encoding: "utf8" });
    return Number(r.stdout?.trim()) || 0;
  };
  const trimmed = [];
  // Newest archived take for a section — the fallback when a re-roll
  // archived the current file and then failed (so s.out is gone).
  const newestArchived = (s) => {
    const dir = `${cfg.motionDir}/archive`;
    const base = `${cfg.slug}-shot-${s.i}-${s.name}.v`;
    const versions = (existsSync(dir) ? readdirSync(dir) : [])
      .filter((f) => f.startsWith(base))
      .sort((a, b) => Number(b.match(/\.v(\d+)\.mp4$/)?.[1] ?? 0) - Number(a.match(/\.v(\d+)\.mp4$/)?.[1] ?? 0));
    return versions.length ? `${dir}/${versions[0]}` : null;
  };
  for (const s of shots) {
    let picked = takes[s.name] ? resolve(cfg.motionDir, takes[s.name]) : s.out;
    if (picked !== s.out) console.log(`  ☑ ${s.name}: ${takes[s.name]}`);
    if (!existsSync(picked)) {
      const fallback = newestArchived(s);
      if (fallback) {
        console.log(`  ↩ ${s.name}: current take missing — using newest archived`);
        picked = fallback;
      }
    }
    const t = `${cfg.motionDir}/trim-${s.i}-${s.name}.mp4`;
    let res;
    if (existsSync(picked)) {
      // Generated clips run ceil(exact) seconds; the excess must go.
      // MORPH shots arrive at their end-frame in the FINAL frames, so
      // they trim from the HEAD (keep the arrival, it kisses the next
      // section's opening panel). Cut-shots trim the tail as before.
      const excess = s.endImage ? Math.max(0, clipSeconds(picked) - s.exact) : 0;
      if (excess > 0.01) console.log(`  ↪ ${s.name}: morph — trimming ${excess.toFixed(2)}s from head`);
      res = spawnSync("ffmpeg", [
        "-y", "-i", picked,
        ...(excess > 0.01 ? ["-ss", excess.toFixed(3)] : []),
        "-t", s.exact.toFixed(3),
        "-vf", "scale=1280:720,fps=24", "-an",
        "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
        t,
      ], { stdio: ["ignore", "ignore", "pipe"] });
    } else {
      // No take at all → Ken Burns the panel so the film still completes.
      const frames = Math.round(s.exact * 24);
      console.log(`  ⚠ ${s.name}: no take — Ken Burns fallback (${frames}f)`);
      res = spawnSync("ffmpeg", [
        "-y", "-loop", "1", "-i", s.image,
        "-vf", `scale=7680:-2,zoompan=z='1+0.10*on/${frames}':x='iw/2-(iw/zoom)/2':y='ih/2-(ih/zoom)/2':d=${frames}:s=1280x720:fps=24`,
        "-frames:v", String(frames), "-an",
        "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
        t,
      ], { stdio: ["ignore", "ignore", "pipe"] });
    }
    if (res.status !== 0) { console.error(`✗ trim failed ${s.name}: ${res.stderr.toString().slice(-300)}`); process.exit(1); }
    trimmed.push(t);
  }

  const listPath = `${cfg.motionDir}/concat.txt`;
  writeFileSync(listPath, trimmed.map((t) => `file '${t}'`).join("\n") + "\n");
  archive(FINAL);
  console.log("  concatenating + muxing audio …");
  const mux = spawnSync("ffmpeg", [
    "-y", "-f", "concat", "-safe", "0", "-i", listPath, "-i", AUDIO,
    "-map", "0:v", "-map", "1:a",
    "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest",
    "-movflags", "+faststart", FINAL,
  ], { stdio: ["ignore", "ignore", "pipe"] });
  if (mux.status !== 0) { console.error(`✗ mux failed: ${mux.stderr.toString().slice(-400)}`); process.exit(1); }
  console.log(`✓ ${FINAL}`);
}
