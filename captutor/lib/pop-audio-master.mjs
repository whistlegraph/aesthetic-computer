// Captutor's spoken-word mastering lane. The character stage is /pop's acdsp
// engine; ffmpeg owns extraction, delivery normalization, and the final AAC.

import { execFileSync, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync, mkdtempSync, readFileSync, renameSync, rmSync, writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));

function popMasterCandidates() {
  return [
    process.env.CAPTUTOR_POP_MASTER_MODULE,
    resolve(HERE, "../../pop/lib/master.mjs"),
    resolve(HERE, "../vendor/pop/lib/master.mjs"),
  ].filter(Boolean);
}

async function loadPopMaster() {
  const modulePath = popMasterCandidates().find(existsSync);
  if (!modulePath) {
    throw new Error(
      "/pop mastering runtime missing; reinstall Captutor or set CAPTUTOR_POP_MASTER_MODULE",
    );
  }
  const runtime = await import(pathToFileURL(modulePath).href);
  if (!runtime.acdspAvailable()) {
    throw new Error("/pop acdsp is not built; reinstall Captutor or build its Pop runtime");
  }
  return runtime;
}

function loudness(path, target) {
  const measured = spawnSync("ffmpeg", [
    "-hide_banner", "-nostats", "-i", path,
    "-af", `loudnorm=I=${target.integrated}:TP=${target.truePeak}:LRA=${target.lra}:print_format=json`,
    "-f", "null", "-",
  ], { encoding:"utf8", stdio:["ignore", "ignore", "pipe"] });
  const stderr = measured.stderr || "";
  const matches = [...stderr.matchAll(/\{\s*"input_i"[\s\S]*?\}/g)];
  if (!matches.length) throw new Error(`could not measure loudness for ${path}`);
  return JSON.parse(matches.at(-1)[0]);
}

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

export async function masterPopAudio({
  input,
  out,
  receipt = out.replace(/\.mp4$/i, ".audio-master.json"),
  preset = "vocalLead",
  presetOptions = { in_db:-4, out_db:0, iron:0.35 },
  target = { integrated:-16, truePeak:-1.5, lra:7 },
}) {
  input = resolve(input);
  out = resolve(out);
  receipt = resolve(receipt);
  if (!existsSync(input) || input === out) {
    throw new Error("Pop mastering needs distinct existing input and output paths");
  }

  const { presets, processWav } = await loadPopMaster();
  if (typeof presets[preset] !== "function") throw new Error(`unknown /pop preset: ${preset}`);
  const work = mkdtempSync(`${tmpdir()}/captutor-pop-master-`);
  const extracted = resolve(work, "source.wav");
  const characterized = resolve(work, "characterized.wav");

  try {
    const before = loudness(input, target);
    execFileSync("ffmpeg", [
      "-y", "-hide_banner", "-loglevel", "error", "-i", input,
      "-map", "0:a:0", "-ar", "48000", "-ac", "2", "-c:a", "pcm_f32le", extracted,
    ], { stdio:"inherit" });

    const chain = presets[preset](presetOptions);
    const processed = processWav(extracted, characterized, chain, { float:true });
    if (!processed.ok) throw new Error(processed.stderr || "/pop mastering failed");
    const measured = loudness(characterized, target);
    const normalize = [
      `loudnorm=I=${target.integrated}:TP=${target.truePeak}:LRA=${target.lra}`,
      `measured_I=${measured.input_i}:measured_LRA=${measured.input_lra}`,
      `measured_TP=${measured.input_tp}:measured_thresh=${measured.input_thresh}`,
      `offset=${measured.target_offset}:linear=true`,
    ].join(":") + ",aresample=48000";

    execFileSync("ffmpeg", [
      "-y", "-hide_banner", "-loglevel", "error",
      "-i", input, "-i", characterized,
      "-map", "0:v:0", "-map", "1:a:0", "-map", "0:s?",
      "-map_metadata", "0", "-map_chapters", "0",
      "-c:v", "copy", "-c:s", "copy",
      "-af", normalize, "-c:a", "aac", "-b:a", "192k", "-ar", "48000", "-ac", "2",
      "-movflags", "+faststart", out,
    ], { stdio:"inherit" });

    const after = loudness(out, target);
    const integrated = Number(after.input_i);
    const truePeak = Number(after.input_tp);
    if (Math.abs(integrated - target.integrated) > 0.6 || truePeak > -1.0) {
      throw new Error(`master outside boundary: ${integrated} LUFS / ${truePeak} dBTP`);
    }
    const result = {
      schema:"captutor-pop-master/v1",
      createdAt:new Date().toISOString(),
      source:basename(input),
      output:basename(out),
      engine:"/pop acdsp",
      preset,
      chain,
      target,
      before:{
        integratedLufs:Number(before.input_i),
        truePeakDbtp:Number(before.input_tp),
        lra:Number(before.input_lra),
      },
      after:{ integratedLufs:integrated, truePeakDbtp:truePeak, lra:Number(after.input_lra) },
      sha256:sha256(out),
    };
    writeFileSync(receipt, `${JSON.stringify(result, null, 2)}\n`);
    return { ...result, receipt };
  } finally {
    rmSync(work, { recursive:true, force:true });
  }
}

// Replace a generated delivery atomically enough for the render pipeline: the
// unmastered encode remains beside it until /pop has produced and verified the
// replacement. A failed pass restores the original file.
export async function masterPopDelivery(video, options = {}) {
  const raw = video.replace(/\.mp4$/i, ".unmastered.mp4");
  const receipt = options.receipt || video.replace(/\.mp4$/i, ".audio-master.json");
  rmSync(raw, { force:true });
  renameSync(video, raw);
  try {
    const result = await masterPopAudio({
      input:raw,
      out:video,
      receipt,
      preset:options.preset,
      presetOptions:options.presetOptions,
      target:options.target,
    });
    rmSync(raw, { force:true });
    return result;
  } catch (error) {
    rmSync(video, { force:true });
    renameSync(raw, video);
    throw error;
  }
}
