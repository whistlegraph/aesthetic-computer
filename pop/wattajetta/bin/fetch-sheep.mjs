#!/usr/bin/env node
// fetch-sheep.mjs — CC0 sheep sounds into assets/sheep/, named by what they
// are. A fixed list (no search, no credentials), so a re-run gets the same
// files and skips the ones already there. Writes assets/sheep/SOURCES.json.
//
// Three sources, all CC0 1.0 (commercial use, no attribution required):
//   BSB — BigSoundBank (Joseph Sardin): original FLACs; each page says
//         "CC0 (public domain)".
//   WC  — Wikimedia Commons: original WAV, file page licence CC0.
//   FS  — Freesound items whose API record says Creative Commons 0. These
//         are the public preview MP3s: the original needs an OAuth login.
// Nothing here is taken from a film, TV show, game or video site.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/sheep");
const UA = "sound-fetch/1.0";
const CC0 = "CC0 1.0";

// BigSoundBank: [file, id, slug, description]
const bsb = (file, id, slug, description) => ({
  file, description, author: "Joseph Sardin (BigSoundBank)", licence: CC0,
  source: `https://bigsoundbank.com/${slug}-s${id}.html`,
  download: `https://bigsoundbank.com/UPLOAD/flac/${id}.flac`,
});
// Freesound: [file, id, user id (in the preview URL), username, description]
const fs = (file, id, uid, author, description) => ({
  file, description, author, licence: CC0,
  source: `https://freesound.org/people/${encodeURIComponent(author)}/sounds/${id}/`,
  download: `https://cdn.freesound.org/previews/${Math.floor(id / 1000)}/${id}_${uid}-hq.mp3`,
});

const SHED = "Sheep bleat under a metal shelter, slight reverb, close";
const NORWAY = "Norwegian ewe, one short bleat, close (RØDE VideoMic)";

const SOUNDS = [
  // single adult bleats
  fs("sheep-bleat-01.mp3", 803460, 15895934, "TheKingOfGeeks360", "Sheep bleat, close, phone microphone"),
  fs("sheep-bleat-02.mp3", 710296, 201532, "michaelperfect", `${NORWAY}; low`),
  fs("sheep-bleat-03.mp3", 710298, 201532, "michaelperfect", NORWAY),
  fs("sheep-bleat-04.mp3", 710299, 201532, "michaelperfect", NORWAY),
  fs("sheep-bleat-05.mp3", 710300, 201532, "michaelperfect", NORWAY),
  fs("sheep-bleat-06.mp3", 677221, 14166079, "satoristudios3", "East Friesian-cross dairy ewe baaing for snacks, close"),
  fs("sheep-bleat-07.mp3", 669872, 8392567, "SergioJbs", "Sheep bleat, one 'baa', close (Mexico)"),
  bsb("sheep-bleat-08.flac", 2345, "sheep-3", SHED),
  bsb("sheep-bleat-09.flac", 2347, "sheep-5", SHED),
  bsb("sheep-bleat-10.flac", 2348, "sheep-6", SHED),
  bsb("sheep-bleat-11.flac", 2349, "sheep-7", SHED),
  { file: "sheep-bleat-12.wav", author: "Gasybeaugosse2020", licence: CC0,
    description: "Sheep bleat in Antefasy, Madagascar; original WAV via Wikimedia Commons",
    source: "https://commons.wikimedia.org/wiki/File:Sheep_in_Antefasy.wav",
    download: "https://upload.wikimedia.org/wikipedia/commons/a/ac/Sheep_in_Antefasy.wav" },
  fs("ewe-bleat-01.mp3", 692900, 14166079, "satoristudios3", "Shetland ewe, a deeper baa than her flock-mates, close"),
  // long, sustained bleats (takes with several)
  fs("sheep-bleat-long-01.mp3", 644830, 9159316, "Breviceps", "Loud sheep 'bah', long and sustained, very clean"),
  fs("sheep-bleat-long-02.mp3", 210511, 770707, "Yuval", "Sheep bleating in a field, close (Schoeps CMIT 5u); several bleats"),
  fs("sheep-bleat-long-03.mp3", 428103, 2061858, "Garuda1982", "Sheep bleats, shotgun mic (Rode NTG4+); several long high bleats"),
  fs("sheep-bleat-long-04.mp3", 383144, 676927, "zachrau", "Sheep bleating on a farm, some distant, some very close"),
  fs("sheep-bleat-long-05.mp3", 362283, 676927, "zachrau", "Sheep bleating on a farm"),
  fs("sheep-bleat-long-06.mp3", 471162, 3248005, "Sheyvan", "A few sheep bleating, low voices (AKG C1000S)"),
  // lambs
  fs("lamb-bleat-01.mp3", 488573, 2243933, "pesticideX", "Hungry lamb wanting milk, long sustained bleats (Levin, New Zealand)"),
  fs("lamb-bleat-02.mp3", 266689, 2454586, "EduFigueres", "Baby lamb calling its mother, city farm, The Hague"),
  fs("lamb-bleat-03.mp3", 266688, 2454586, "EduFigueres", "Baby lamb calling its mother, take 2, city farm, The Hague"),
  fs("lamb-bleat-04.mp3", 692898, 14166079, "satoristudios3", "Twin lambs (Shetland x East Friesian) baaing"),
  fs("lamb-bleat-05.mp3", 182509, 854782, "swiftoid", "Lamb bleating, then its grumpy mother answers; faint birds"),
  // rams
  fs("ram-bleat-01.mp3", 521856, 11431915, "D.jones", "Three rams (uploader: 'sheeps sound or rams'); constant background noise"),
  fs("ram-bleat-02.mp3", 784193, 2479206, "stinky_ox", "Two rams 'talking' in a stable on a rainy night; low grunting vocalisations, rain, a car"),
  // flocks
  fs("sheep-flock-01.mp3", 386448, 1661766, "felix.blume", "Flock bleating in a prairie, many bleats very close to the mic, MS stereo (Tierra del Fuego, Argentina)"),
  fs("sheep-flock-02.mp3", 670847, 11519060, "bruno.auzet", "Dozens of ewes bleating for food in a sheepfold, straw, bells, birds (France)"),
  fs("sheep-flock-03.mp3", 609160, 938246, "nomadas", "A couple of hundred ewes and lambs bleating in a shed, walking through (Burgos, Spain)"),
  // not a sheep
  fs("sheep-bleat-imitation-01.mp3", 840342, 18292866, "Imp_sounds", "IMITATION: a person mimicking a sheep's bleat, not a real sheep"),
];

const MAX_BYTES = 40e6; // the whole folder stays under this

function probe(path) {
  try {
    const j = JSON.parse(execFileSync("ffprobe", ["-v", "error", "-show_entries",
      "format=duration:stream=sample_rate,channels,codec_name", "-of", "json", path], { encoding: "utf8" }));
    const s = j.streams[0];
    return { seconds: +(+j.format.duration).toFixed(2), sampleRate: +s.sample_rate, channels: s.channels, codec: s.codec_name };
  } catch { return null; }
}

mkdirSync(OUT, { recursive: true });
let total = 0, failed = 0;
const manifest = [];
async function download(url) {
  let err;
  for (let n = 0; n < 4; n++) {
    try {
      const res = await fetch(url, { headers: { "User-Agent": UA }, signal: AbortSignal.timeout(90e3) });
      if (!res.ok) throw new Error(`http ${res.status}`);
      return Buffer.from(await res.arrayBuffer());
    } catch (e) {
      err = e;
      // Wikimedia sometimes refuses Node's fetch but serves curl.
      try { return execFileSync("curl", ["-sfL", "--max-time", "90", "--max-filesize", "30000000", "-A", UA, url], { maxBuffer: 40e6 }); }
      catch { await new Promise((r) => setTimeout(r, 1500 * (n + 1))); }
    }
  }
  throw err;
}

for (const s of SOUNDS) {
  const dest = resolve(OUT, s.file);
  if (!existsSync(dest)) {
    try {
      const buf = await download(s.download);
      if (total + buf.length > MAX_BYTES) throw new Error("over the size budget");
      writeFileSync(dest + ".part", buf);
      renameSync(dest + ".part", dest);
    } catch (e) { failed++; console.error(`✗ ${s.file}: ${e.message}`); continue; }
  }
  const bytes = statSync(dest).size;
  const audio = probe(dest);
  if (!audio) { failed++; console.error(`✗ ${s.file}: does not decode`); continue; }
  total += bytes;
  manifest.push({ ...s, bytes, ...audio });
  console.log(`✓ ${s.file}  ${audio.seconds}s  ${audio.sampleRate} Hz`);
}
writeFileSync(resolve(OUT, "SOURCES.json"), JSON.stringify({
  note: "Every file is CC0 1.0 on its source page: commercial use, no attribution required. Freesound files are preview MP3s of the originals; BigSoundBank and Wikimedia Commons files are the originals.",
  licenceUrl: "https://creativecommons.org/publicdomain/zero/1.0/",
  sounds: manifest,
}, null, 2) + "\n");
console.log(`${manifest.length}/${SOUNDS.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
if (failed) process.exitCode = 1;
