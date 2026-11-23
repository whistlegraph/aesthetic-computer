#!/usr/bin/env node
// fetch-donkeys.mjs — CC0 donkey sounds into assets/donkeys/, named by what
// they are. A fixed list (no search, no credentials), so a re-run gets the
// same files and skips the ones already there. Writes assets/donkeys/SOURCES.json.
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
const OUT = resolve(HERE, "../assets/donkeys");
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

const SOUNDS = [
  // single brays
  bsb("donkey-bray-01.flac", 1549, "donkey-braying-1", "Donkey bray; a horse's breath at the end"),
  bsb("donkey-bray-02.flac", 1550, "donkey-braying-2", "Donkey bray"),
  bsb("donkey-bray-03.flac", 1551, "donkey-braying-3", "Donkey bray"),
  fs("donkey-bray-04.mp3", 705300, 1661766, "felix.blume", "Donkey braying and screaming loud, close MS recording, distant village behind (Mexico, 2009)"),
  fs("donkey-bray-05.mp3", 133761, 1661766, "felix.blume", "Donkey braying in a field near La Preciosita, Puebla, Mexico"),
  fs("donkey-bray-06.mp3", 787564, 15895934, "TheKingOfGeeks360", "Donkey braying in a trailer, close"),
  fs("donkey-bray-07.mp3", 831960, 15895934, "TheKingOfGeeks360", "Donkey bray, medium distance, phone microphone"),
  fs("donkey-bray-squeaky-01.mp3", 859477, 15895934, "TheKingOfGeeks360", "Donkey, squeaky high-pitched bray, phone microphone"),
  // long brays
  { file: "donkey-bray-long-01.wav", author: "Felix Blume", licence: CC0,
    description: "Donkey (Balthazar) braying in his enclosure, south of France, MS stereo; original WAV via Wikimedia Commons",
    source: "https://commons.wikimedia.org/wiki/File:157763_felix-blume_a-donkey-is-braying-in-his-enclosure-in-south-of-france.wav",
    download: "https://upload.wikimedia.org/wikipedia/commons/2/25/157763_felix-blume_a-donkey-is-braying-in-his-enclosure-in-south-of-france.wav",
    // Wikimedia throttles generic user agents hard; the same recording's Freesound preview stands in.
    fallback: fs("donkey-bray-long-01.mp3", 157763, 1661766, "felix.blume",
      "Donkey (Balthazar) braying in his enclosure, south of France, MS stereo (preview; the original WAV is on Wikimedia Commons)") },
  fs("donkey-bray-long-02.mp3", 152938, 1661766, "felix.blume", "Donkey braying in a High Atlas field, Morocco; a dog answers"),
  fs("donkey-bray-long-03.mp3", 152907, 1661766, "felix.blume", "Donkey braying in a field near the market, Assem Souk, Morocco"),
  fs("donkey-bray-long-04.mp3", 170483, 1661766, "felix.blume", "Donkey (Balthazar) struggling to bray in the early morning, south of France"),
  fs("donkey-bray-mountain-01.mp3", 759892, 14889307, "Sacha.Julien", "Donkey braying among sheep and guard dogs, Mercantour mountains, double mid-side stereo"),
  fs("donkey-bray-imitation-01.mp3", 560692, 10936067, "StarNinjas37", "IMITATION: a person's mouth-made donkey bray, not a real donkey"),
  // more than one donkey
  bsb("donkey-herd-01.flac", 1548, "donkeys-braying", "Two donkeys braying at the same time"),
  fs("donkey-herd-02.mp3", 842289, 15895934, "TheKingOfGeeks360", "Two donkeys braying in a trailer, close, phone microphone"),
  fs("donkey-herd-03.mp3", 170588, 1661766, "felix.blume", "Several donkeys braying in a village near Mount Ararat, Anatolia"),
  fs("donkey-herd-04.mp3", 246406, 1661766, "felix.blume", "Donkeys left at the weekly market answering each other, Assem Souk, Morocco"),
  // snorts, breath, chewing
  fs("donkey-snort-01.mp3", 856327, 15895934, "TheKingOfGeeks360", "Donkey snort"),
  fs("donkey-breath-01.mp3", 170567, 1661766, "felix.blume", "Donkey (Balthazar) breathing hard and wheezing in the early morning, south of France"),
  fs("donkey-chew-01.mp3", 182505, 854782, "swiftoid", "Donkey munching grass, close, high noise floor"),
  // mules (donkey x horse)
  fs("mule-bray-01.mp3", 856360, 15895934, "TheKingOfGeeks360", "MULE, not a donkey: neigh that ends in a short bray"),
  fs("mule-bray-02.mp3", 868304, 15895934, "TheKingOfGeeks360", "MULE, not a donkey: neigh and short bray, take 2"),
  // hooves
  fs("donkey-hooves-01.mp3", 378549, 6246233, "rdpipo7777", "'pasos burro' (donkey steps), SM58; the page does not say whether it is a real donkey or foley"),
  fs("horse-hooves-gravel-01.mp3", 233345, 737995, "Ornery", "HORSES, not donkeys: walking on a gravel road, Palo Alto hills (stand-in for donkey hooves on gravel)"),
];

const MAX_BYTES = 60e6; // the whole folder stays under this

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
      const res = await fetch(url, { headers: { "User-Agent": UA } });
      if (!res.ok) throw new Error(`http ${res.status}`);
      return Buffer.from(await res.arrayBuffer());
    } catch (e) {
      err = e;
      // Wikimedia sometimes refuses Node's fetch but serves curl.
      try { return execFileSync("curl", ["-sfL", "--max-filesize", "30000000", "-A", UA, url], { maxBuffer: 40e6 }); }
      catch { await new Promise((r) => setTimeout(r, 1500 * (n + 1))); }
    }
  }
  throw err;
}

for (const item of SOUNDS) {
  // An item may name a fallback (same recording, other host); whichever is on disk counts.
  const { fallback, ...primary } = item;
  const choices = fallback ? [primary, fallback] : [primary];
  let s = choices.find((c) => existsSync(resolve(OUT, c.file)));
  if (!s) {
    let lastErr;
    for (const c of choices) {
      try {
        const buf = await download(c.download);
        if (total + buf.length > MAX_BYTES) throw new Error("over the size budget");
        const dest = resolve(OUT, c.file);
        writeFileSync(dest + ".part", buf);
        renameSync(dest + ".part", dest);
        s = c;
        break;
      } catch (e) { lastErr = e; console.error(`  ${c.file}: ${e.message}${c === primary && fallback ? ", trying fallback" : ""}`); }
    }
    if (!s) { failed++; console.error(`✗ ${item.file}: ${lastErr.message}`); continue; }
  }
  const dest = resolve(OUT, s.file);
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
