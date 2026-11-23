#!/usr/bin/env node
// fetch-springs-mice.mjs — CC0 spring sounds into assets/springs/ and mouse /
// rat sounds into assets/mice/, named by what they are. A fixed list (no
// search, no credentials), so a re-run gets the same files and skips the ones
// already there. Writes a SOURCES.json in each folder.
//
// Two sources, both CC0 1.0 (commercial use, no attribution required):
//   BSB — BigSoundBank (Joseph Sardin): original FLAC or AIFF; each page says
//         "CC0 (public domain)".
//   FS  — Freesound items whose API record says Creative Commons 0. These
//         are the public preview MP3s: the original needs an OAuth login.
// Nothing here is taken from a film, TV show, game or video site. Each mouse
// description says whether it is a real animal or an imitation.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ASSETS = resolve(HERE, "../assets");
const UA = "sound-fetch/1.0";
const CC0 = "CC0 1.0";

// BigSoundBank: id is the zero-padded 4-digit number in the page URL.
const bsb = (file, id, slug, ext, description) => ({
  file, description, author: "Joseph Sardin (BigSoundBank)", licence: CC0,
  source: `https://bigsoundbank.com/${slug}-s${id}.html`,
  download: `https://bigsoundbank.com/UPLOAD/${ext}/${id}.${ext}`,
});
// Freesound: [file, id, user id (in the preview URL), username, description]
const fs = (file, id, uid, author, description) => ({
  file, description, author, licence: CC0,
  source: `https://freesound.org/people/${encodeURIComponent(author)}/sounds/${id}/`,
  download: `https://cdn.freesound.org/previews/${Math.floor(id / 1000)}/${id}_${uid}-hq.mp3`,
});

const SPRINGS = [
  // single boings from small coil springs
  fs("spring-boing-01.mp3", 472478, 723384, "denalwa", "Single boing from a battery-compartment spring"),
  fs("spring-boing-02.mp3", 472477, 723384, "denalwa", "Single boing from a battery-compartment spring, take 2"),
  fs("spring-boing-03.mp3", 376513, 3728489, "cribbler", "Lamp spring plucked, percussive boing (96 kHz original)"),
  fs("spring-boing-04.mp3", 868612, 17675683, "CAT-FOX_ALEX", "Steel spring on a camera clamp plucked, de-noised"),
  fs("spring-boing-05.mp3", 868611, 17675683, "CAT-FOX_ALEX", "Steel spring on a camera clamp plucked, de-noised, take 2"),
  // door-stop springs
  fs("spring-doorstop-01.mp3", 829617, 17997942, "deleted_user_17997942", "Dense spring door stop flicked and released, Zoom H6 X-Y"),
  fs("spring-doorstop-02.mp3", 829616, 17997942, "deleted_user_17997942", "Dense spring door stop flicked and released, take 2"),
  fs("spring-doorstop-03.mp3", 361374, 6628656, "Agentjoshh", "Spring door stop boing"),
  fs("spring-doorstop-04.mp3", 344741, 3296183, "XenosNS", "Metal spring door stop on a wall, flicked repeatedly (many takes in one file)"),
  fs("spring-doorstop-05.mp3", 125739, 296888, "alienistcog", "Bathroom door stop spring twanged, several takes"),
  fs("spring-doorstop-rattle-01.mp3", 865177, 15176424, "Robo9418", "Wall door stop pulled and let go, rattle; faint handling noise at 4.7 s"),
  // slinkies
  fs("spring-slinky-01.mp3", 593141, 13371625, "21100375", "Plastic slinky collapsing onto itself, Zoom H6"),
  fs("spring-slinky-02.mp3", 32598, 283287, "FranticAerostat", "Plastic slinky toy, short"),
  // coil springs stretched, dropped, twanged
  fs("spring-coil-stretch-01.mp3", 245819, 4451798, "blukotek", "Steel coil spring pulled and released"),
  fs("spring-coil-stretch-02.mp3", 10560, 186, "batchku", "Metal springs on a calabash shell (David Bithell's 'Sun Ra' instrument) pulled, loud; recorded by Ali Momeni"),
  bsb("spring-coil-stretch-03.aiff", "0206", "large-spring", "aiff", "Eight noises from a large spring under tension"),
  fs("spring-drop-01.mp3", 542694, 10965995, "BonginkosiMakhubu190225", "Spiral spring dropped, Zoom H6"),
  fs("spring-twang-01.mp3", 511332, 562829, "onehugeeye", "Metal spring landing on wood, ceramic and cardboard, several twangs"),
  fs("spring-screendoor-01.mp3", 124705, 296888, "alienistcog", "Old rusty screen-door spring struck, plucked and rattled"),
  // spring reverb tank
  fs("spring-reverb-crash-01.mp3", 194507, 9064, "andreas", "Fostex 3180 spring reverb opened up and its springs hit directly"),
  fs("spring-reverb-drip-01.mp3", 11222, 29612, "jnr hacksaw", "A drip processed with spring reverb"),
  // jaw harp (a plucked steel reed: the classic 'boing')
  bsb("spring-jawharp-01.flac", "2277", "boing-cartoon-1", "flac", "Jaw harp (guimbarde) playing a cartoon bounce"),
  bsb("spring-jawharp-02.flac", "2281", "boing-cartoon-5", "flac", "Jaw harp (guimbarde) playing a cartoon bounce, take 5"),
  fs("spring-jawharp-03.mp3", 540788, 11537497, "magnuswaker", "Jaw harp boing ('oh-ey'), 96 kHz original"),
];

const MICE = [
  // real mice
  bsb("mouse-squeak-01.aiff", "0459", "mouse-screams", "aiff", "REAL mouse, caught by a cat, squeaking when touched before release"),
  fs("mouse-squeak-02.mp3", 463789, 7805928, "shyguy014", "REAL mouse (or young rat) caught in a trap, squeaking; de-noised and amplified, Zoom H4n"),
  fs("mouse-squeak-03.mp3", 727114, 15781963, "berboberbo", "REAL wild mice squeaking under a floor, Sontronics STC-3X"),
  fs("mouse-chirps-01.mp3", 796878, 15895934, "TheKingOfGeeks360", "REAL pet house mouse chattering, close"),
  bsb("dormouse-squeak-01.flac", "1073", "garden-dormouse", "flac", "REAL wild garden dormouse alarm cries, in a garden"),
  // real rats
  fs("rat-squeak-01.mp3", 143125, 2580450, "Zabuhailo", "REAL pet rat squeaking, mono, 96 kHz original"),
  fs("rat-squeak-02.mp3", 134791, 2460522, "Sycoraxi", "REAL pet fancy rats squeaking and screaming; some noise"),
  fs("rat-squeak-03.mp3", 217767, 950925, "Reitanna", "REAL pet rats squeaking over food; low quality"),
  fs("rat-squeak-04.mp3", 728086, 1889150, "macohibs", "REAL distressed rat squeaking"),
  fs("rat-squeak-05.mp3", 397651, 7554526, "SieuAmThanh", "REAL angry rat, Zoom H1"),
  fs("rat-squeak-babies-01.mp3", 841571, 15895934, "TheKingOfGeeks360", "REAL baby brown rats squeaking while nursing, close"),
  fs("rat-brux-01.mp3", 219140, 950925, "Reitanna", "REAL pet rat bruxing (grinding teeth, contented)"),
  // gnawing, nibbling, scratching
  bsb("rat-nibble-01.aiff", "0185", "rat-nibbling", "aiff", "REAL pet rat nibbling cereal"),
  fs("rat-gnaw-01.mp3", 143250, 2580450, "Zabuhailo", "REAL pet rat gnawing a rusk, 96 kHz original"),
  fs("mouse-gnaw-01.mp3", 215764, 3406193, "vumseplutten1709", "REAL mouse gnawing wood at night"),
  fs("hamster-scratch-01.mp3", 541229, 6383813, "karinalarasart", "HAMSTER, not a mouse: pet Syrian hamster scratching paper bedding into a nest, iPhone 8"),
  fs("degu-scurry-01.mp3", 420087, 7249762, "14GPanskaZakopcanik_Jonas", "DEGU, not a mouse: pet degu running on a floating floor"),
  // imitation
  fs("mouse-squeak-imitation-01.mp3", 188043, 3403708, "AntumDeluge", "IMITATION: a person sucking air past teeth and lip, modified; not a real mouse"),
];

const MAX_BYTES = 25e6; // each folder stays under this

function probe(path) {
  try {
    const j = JSON.parse(execFileSync("ffprobe", ["-v", "error", "-show_entries",
      "format=duration:stream=sample_rate,channels,codec_name", "-of", "json", path], { encoding: "utf8" }));
    const s = j.streams[0];
    return { seconds: +(+j.format.duration).toFixed(2), sampleRate: +s.sample_rate, channels: s.channels, codec: s.codec_name };
  } catch { return null; }
}

async function download(url) {
  let err;
  for (let n = 0; n < 4; n++) {
    try {
      const res = await fetch(url, { headers: { "User-Agent": UA } });
      if (!res.ok) throw new Error(`http ${res.status}`);
      return Buffer.from(await res.arrayBuffer());
    } catch (e) {
      err = e;
      try { return execFileSync("curl", ["-sfL", "--max-filesize", "30000000", "-A", UA, url], { maxBuffer: 40e6 }); }
      catch { await new Promise((r) => setTimeout(r, 1500 * (n + 1))); }
    }
  }
  throw err;
}

let failed = 0;
for (const [folder, sounds] of [["springs", SPRINGS], ["mice", MICE]]) {
  const OUT = resolve(ASSETS, folder);
  mkdirSync(OUT, { recursive: true });
  let total = 0;
  const manifest = [];
  for (const s of sounds) {
    const dest = resolve(OUT, s.file);
    if (!existsSync(dest)) {
      try {
        const buf = await download(s.download);
        if (total + buf.length > MAX_BYTES) throw new Error("over the size budget");
        writeFileSync(dest + ".part", buf);
        renameSync(dest + ".part", dest);
      } catch (e) { failed++; console.error(`✗ ${folder}/${s.file}: ${e.message}`); continue; }
    }
    const bytes = statSync(dest).size;
    const audio = probe(dest);
    if (!audio) { failed++; console.error(`✗ ${folder}/${s.file}: does not decode`); continue; }
    total += bytes;
    manifest.push({ ...s, bytes, ...audio });
    console.log(`✓ ${folder}/${s.file}  ${audio.seconds}s  ${audio.sampleRate} Hz`);
  }
  writeFileSync(resolve(OUT, "SOURCES.json"), JSON.stringify({
    note: "Every file is CC0 1.0 on its source page: commercial use, no attribution required. Freesound files are preview MP3s of the originals; BigSoundBank files are the originals.",
    licenceUrl: "https://creativecommons.org/publicdomain/zero/1.0/",
    sounds: manifest,
  }, null, 2) + "\n");
  console.log(`${folder}: ${manifest.length}/${sounds.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
}
if (failed) process.exitCode = 1;
