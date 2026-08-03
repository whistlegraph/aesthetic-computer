#!/usr/bin/env node

// Rebuild narration, captions, and delivery chrome from an accepted take.
// Product pixels come from the existing bake-time clip; the browser is never
// driven and no Fuser generation runs.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { narrate } from "../lib/narrate.mjs";
import { mux, writeVTT } from "../lib/compose.mjs";
import { masterPopDelivery } from "../lib/pop-audio-master.mjs";

const HERE = resolve(fileURLToPath(new URL("..", import.meta.url)));
const value = (flag, fallback = null) => {
  const i = process.argv.indexOf(flag);
  return i === -1 ? fallback : process.argv[i + 1];
};
const slug = value("--screenplay");
const locale = value("--locale", "en");
const format = value("--format", "docs");
if (!slug) throw new Error("usage: recompose-existing --screenplay <slug> [--locale en] [--format docs]");

const screenplayPath = join(HERE, "screenplays", `${slug}.mjs`);
const sp = (await import(pathToFileURL(screenplayPath).href)).default;
const workDir = join(HERE, "out", `${slug}.${locale}.${format}`);
const priorStoryboardPath = join(workDir, "storyboard.json");
const priorStoryboard = existsSync(priorStoryboardPath)
  ? JSON.parse(readFileSync(priorStoryboardPath, "utf8"))
  : null;
const priorWidth = Number(priorStoryboard?.media?.width);
const priorHeight = Number(priorStoryboard?.media?.height);
if (priorWidth === 2560 || priorHeight === 2560) {
  process.env.CAPTUTOR_STAGE_MODE = "1";
  if (priorHeight > priorWidth) process.env.CAPTUTOR_VERTICAL_MODE = "1";
}
// deliver.mjs reads the Stage geometry contract at module initialization, so
// load it only after the accepted storyboard has restored that environment.
const { deliver } = await import("../lib/deliver.mjs");
const oldCuesPath = join(workDir, "cues.json");
const clip = ["clip-bake-time.mp4", "clip.mp4"]
  .map((name) => join(workDir, name)).find(existsSync);
if (!clip || !existsSync(oldCuesPath)) throw new Error(`no accepted take in ${workDir}`);

const say = (value) => typeof value === "string" ? value : value?.[locale];
const spoken = sp.beats.map((beat) => ({ ...beat, say:say(beat.say) }));
if (spoken.some((beat) => !beat.say)) throw new Error(`missing ${locale} narration`);
const voices = await narrate(spoken, {
  voice:sp.voice,
  dir:join(HERE, "out", "voice", `${sp.slug}.${locale}`),
});
const oldCues = JSON.parse(readFileSync(oldCuesPath, "utf8"));
if (oldCues.length !== voices.length) throw new Error("beat count changed; record a new take");
const cues = voices.map((beat, index) => ({
  ...beat,
  index,
  offsetSec:Number(oldCues[index].offsetSec),
  sourceOffsetSec:oldCues[index].sourceOffsetSec ?? null,
}));
const chapters = Array.isArray(sp.chapters) ? sp.chapters.map((chapter) => ({
  ...chapter,
  startSec:Number.isInteger(Number(chapter.beatIndex)) && cues[Number(chapter.beatIndex)]
    ? cues[Number(chapter.beatIndex)].offsetSec
    : chapter.startSec,
})) : null;
for (let index = 0; index < cues.length - 1; index += 1) {
  const available = cues[index + 1].offsetSec - cues[index].offsetSec;
  if (cues[index].durationSec > available + 0.08) {
    throw new Error(`revised beat ${index + 1} overlaps the next visual beat by ${(cues[index].durationSec - available).toFixed(2)}s`);
  }
}

const vtt = join(workDir, `${slug}.vtt`);
const composed = join(workDir, `${slug}.mp4`);
const output = join(workDir, `${slug}.${format}.mp4`);
writeVTT(cues, vtt);
writeFileSync(oldCuesPath, `${JSON.stringify(cues, null, 2)}\n`);
mux({ clip, beats:cues, out:composed, vtt });
deliver({
  clip:composed, cues, format, out:output, workDir, locale,
  title:say(sp.title),
  brandChrome:sp.brandChrome || null,
  chapters,
  terminalCard:null,
});
if (sp.audioMaster) await masterPopDelivery(output, sp.audioMaster);
console.log(JSON.stringify({ output, vtt, cues:oldCuesPath }, null, 2));
