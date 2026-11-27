#!/usr/bin/env node
// fetch-snacks.mjs — CC0 chip crunches and drink sounds into assets/snacks/, named by what they are, for
// gunrunmicro. A fixed list (no search, no credentials), so a re-run gets the same files and skips the ones
// already there. Writes assets/snacks/SOURCES.json.
//
// One source: Freesound items whose API record says Creative Commons 0 (commercial use, no attribution
// required). These are the public preview MP3s: the original needs an OAuth login.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const OUT = resolve(dirname(fileURLToPath(import.meta.url)), "../assets/snacks");
const UA = "sound-fetch/1.0";

// [file, id, user id (in the preview URL), username, description]
const fs = (file, id, uid, author, description) => ({
  file, description, author, licence: "CC0 1.0",
  source: `https://freesound.org/people/${encodeURIComponent(author)}/sounds/${id}/`,
  download: `https://cdn.freesound.org/previews/${Math.floor(id / 1000)}/${id}_${uid}-hq.mp3`,
});

const SOUNDS = [
  // chips: bites and chewing, mouth open
  fs("crunch-bite-01.mp3", 316926, 4972784, "sammy214", "Biting potato chips"),
  fs("crunch-bite-02.mp3", 844811, 8175347, "SammygoodTunes", "One crunch"),
  fs("crunch-bite-03.mp3", 528962, 11720698, "steenish", "One crunch"),
  fs("crunch-chew-01.mp3", 334209, 4290188, "WasabiWielder", "Eating crisps"),
  fs("crunch-chew-02.mp3", 573577, 12467243, "vibritherabjit123", "Short crisps-eating noise"),
  // other things bitten, snapped and broken
  fs("crunch-crispy-01.mp3", 718593, 11865776, "JoMungus", "A crispy bite"),
  fs("crunch-apple-01.mp3", 822335, 17759724, "kirikkola", "Apple crunch, eating"),
  fs("crunch-apple-02.mp3", 250106, 2772007, "phatcorns", "Apple bite"),
  fs("crunch-carrot-01.mp3", 106392, 367313, "j1987", "Carrot bitten"),
  fs("crunch-snap-01.mp3", 176543, 3277771, "deleted_user_3277771", "One crunch"),
  fs("crunch-celery-01.mp3", 388019, 7280678, "saturdaysoundguy", "Celery snapped"),
  fs("crunch-bone-01.mp3", 630590, 8207611, "Vinni_R", "Bone-break effect"),
  fs("crunch-popcorn-01.mp3", 641145, 13071295, "sussyyousuckidiot", "Popcorn eaten"),
  fs("crunch-tortilla-01.mp3", 548624, 10570070, "Beenjamone", "Eating a chip"),
  fs("crunch-bag-01.mp3", 334208, 4290188, "WasabiWielder", "Crisp bag rustling"),
  // cans and bottles opening
  fs("can-open-01.mp3", 240218, 1440889, "Alexsani", "Pop can opening"),
  fs("can-open-02.mp3", 830245, 9616576, "IENBA", "Soda can opening"),
  fs("can-open-03.mp3", 856868, 15176424, "Robo9418", "Soda can opening"),
  fs("can-fizz-01.mp3", 411218, 6023964, "AndersHolm", "Can opened, fizz after it"),
  fs("can-crack-01.mp3", 347857, 1046667, "Dredile", "Cracking a beer or soda can"),
  fs("cork-pop-01.mp3", 757257, 10938187, "HenKonen", "Wine cork pulled"),
  // pouring, ice
  fs("pour-soda-01.mp3", 619922, 13663560, "ebj2231", "Soda poured into a glass"),
  fs("pour-drink-01.mp3", 636151, 13907202, "nataliegonzalez19", "A drink poured"),
  fs("ice-drop-01.mp3", 837694, 15375955, "Valerie-Vivegnis", "Ice cube falling in a glass"),
  fs("ice-drop-02.mp3", 837693, 15375955, "Valerie-Vivegnis", "Ice cube falling in a glass, take 2"),
  fs("ice-clink-01.mp3", 436967, 3160531, "creeeeak", "Ice in a glass"),
  // drinking
  fs("gulp-01.mp3", 400374, 7283708, "sabbyt2", "One gulp"),
  fs("gulp-02.mp3", 693249, 12711963, "UnderASpell", "One swallow"),
  fs("gulp-03.mp3", 829847, 16563371, "haus.the.child", "Swallow, gulp"),
  fs("gulp-04.mp3", 534336, 11867884, "Defaultv", "Drinking, a gulp"),
  fs("slurp-can-01.mp3", 817785, 15179699, "Daftrio", "Slurp from a can"),
  fs("slurp-straw-01.mp3", 344593, 6125852, "kmyers1316", "Slurping through a straw"),
];

function probe(path) {
  try {
    const j = JSON.parse(execFileSync("ffprobe", ["-v", "error", "-show_entries",
      "format=duration:stream=sample_rate,channels,codec_name", "-of", "json", path], { encoding: "utf8" }));
    const s = j.streams[0];
    return { seconds: +(+j.format.duration).toFixed(2), sampleRate: +s.sample_rate, channels: s.channels, codec: s.codec_name };
  } catch { return null; }
}

mkdirSync(OUT, { recursive: true });
let failed = 0, total = 0;
const manifest = [];
for (const s of SOUNDS) {
  const dest = resolve(OUT, s.file);
  if (!existsSync(dest)) {
    try {
      const res = await fetch(s.download, { headers: { "User-Agent": UA } });
      if (!res.ok) throw new Error(`http ${res.status}`);
      writeFileSync(dest + ".part", Buffer.from(await res.arrayBuffer()));
      renameSync(dest + ".part", dest);
    } catch (e) { failed++; console.error(`✗ ${s.file}: ${e.message}`); continue; }
  }
  const audio = probe(dest);
  if (!audio) { failed++; console.error(`✗ ${s.file}: does not decode`); continue; }
  const bytes = statSync(dest).size;
  total += bytes;
  manifest.push({ ...s, bytes, ...audio });
  console.log(`✓ ${s.file}  ${audio.seconds}s  ${audio.sampleRate} Hz`);
}
writeFileSync(resolve(OUT, "SOURCES.json"), JSON.stringify({
  note: "Every file is CC0 1.0 on its Freesound page: commercial use, no attribution required. The files are preview MP3s of the originals.",
  licenceUrl: "https://creativecommons.org/publicdomain/zero/1.0/",
  sounds: manifest,
}, null, 2) + "\n");
console.log(`${manifest.length}/${SOUNDS.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
if (failed) process.exitCode = 1;
