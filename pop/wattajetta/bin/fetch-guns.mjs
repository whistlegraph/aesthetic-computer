#!/usr/bin/env node
// fetch-guns.mjs — CC0 weapon sounds into assets/guns/, named by weapon class.
// A fixed list (no search, no credentials), so a re-run gets the same files
// and skips the ones already there. Writes assets/guns/SOURCES.json.
//
// Two sources, both CC0 1.0 (commercial use, no attribution required):
//   FFL — The Free Firearm Sound Library (Ben Jaszczak, Brian Nelson, Kevin
//         Heras, Matthew Nanney; Kickstarter 2014), original WAVs from the
//         GitHub mirror. Licence statement:
//         https://opengameart.org/content/the-free-firearm-sound-library
//   FS  — Freesound items whose page says Creative Commons 0. These are the
//         public preview MP3s: the original WAV/FLAC needs an OAuth login.
// Nothing here is taken from a game.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/guns");
const UA = "sound-fetch/1.0";
const CC0 = "CC0 1.0";

const FFL_RAW = "https://raw.githubusercontent.com/buddingmonkey/FreeFirearmsSFXLibrary/main/";
const FFL_PAGE = "https://opengameart.org/content/the-free-firearm-sound-library";
const FFL_AUTHOR = "Ben Jaszczak, Brian Nelson, Kevin Heras, Matthew Nanney (The Free Firearm Sound Library)";
const ffl = (file, path, description) => ({
  file, description, author: FFL_AUTHOR, licence: CC0,
  source: FFL_PAGE, download: FFL_RAW + path.split("/").map(encodeURIComponent).join("/"),
});
// Freesound: [file, id, user id (in the preview URL), username, description]
const fs = (file, id, uid, author, description) => ({
  file, description, author, licence: CC0,
  source: `https://freesound.org/people/${encodeURIComponent(author)}/sounds/${id}/`,
  download: `https://cdn.freesound.org/previews/${Math.floor(id / 1000)}/${id}_${uid}-hq.mp3`,
});

const SOUNDS = [
  // 9 mm pistol
  ffl("pistol-9mm-shot-01.wav", "Prepared SFX/Walther PPQ/X_39P.wav", "Walther PPQ 9mm, single shots, near, stereo"),
  fs("pistol-9mm-shot-02.mp3", 828786, 15072041, "areniporgen", "Glock 19X 9mm, one close shot outdoors, phone microphone"),
  ffl("pistol-9mm-mag-insert-01.wav", "Master Tracks/Walther PPQ/X_2.wav", "Walther PPQ, magazine inserted"),
  ffl("pistol-9mm-mag-detach-01.wav", "Master Tracks/Walther PPQ/X_3.wav", "Walther PPQ, magazine detached"),
  fs("pistol-9mm-mag-insert-02.mp3", 171209, 1769769, "brianhanson2nd", "Smith & Wesson 9mm, magazine inserted"),
  ffl("pistol-9mm-slide-drop-01.wav", "Master Tracks/Walther PPQ/X_15.wav", "Walther PPQ, slide dropped"),
  fs("pistol-9mm-slide-rack-01.mp3", 442560, 3207383, "nikkolaus", "Sig Sauer P229, slide racked back"),
  ffl("pistol-9mm-dry-fire-01.wav", "Master Tracks/Walther PPQ/X_18.wav", "Walther PPQ, dry fire"),
  fs("pistol-9mm-dry-fire-02.mp3", 842748, 71257, "qubodup", "Glock 17, dry trigger pull"),
  // heavy pistol
  fs("pistol-heavy-shot-01.mp3", 231243, 1934171, "AugustSandberg", "Desert Eagle .50, shots at an indoor range"),
  fs("pistol-heavy-shot-02.mp3", 151071, 2732486, "vabadus", "Desert Eagle .50AE, one shot, moderate echo"),
  ffl("pistol-heavy-shot-03.wav", "Prepared SFX/1911/A_42P.wav", "1911 .45, single shots, near, stereo"),
  fs("pistol-heavy-slide-release-01.mp3", 151070, 2732486, "vabadus", "Desert Eagle .50AE, slide released"),
  ffl("pistol-heavy-slide-rack-01.wav", "Master Tracks/1911/A_24.wav", "1911 .45, full slide rack"),
  // SMG (MP5 class)
  fs("smg-mp5-burst-01.mp3", 201667, 3749344, "Franki-01234", "MP5 burst (blanks) in a doorway, street acoustics"),
  fs("smg-mp5-burst-02.mp3", 201672, 3749344, "Franki-01234", "MP5 burst (blanks) heard from down the block"),
  ffl("smg-9mm-shot-01.wav", "Prepared SFX/Carl Gustav M45/G_31P.wav", "Carl Gustav M45 9mm SMG, single shots, near, stereo"),
  ffl("smg-9mm-burst-01.wav", "Prepared SFX/Carl Gustav M45/G_33P.wav", "Carl Gustav M45 9mm SMG, short bursts, near, stereo"),
  ffl("smg-9mm-bolt-lock-01.wav", "Master Tracks/Carl Gustav M45_Swedish K/G_10.wav", "Carl Gustav M45, bolt locked back"),
  // micro-SMG (Uzi class) — no CC0 Uzi fire found; the PPSh is the fast stand-in
  ffl("smg-micro-burst-01.wav", "Prepared SFX/PPSh/P_34P.wav", "PPSh 7.62x25 SMG, long burst, near, stereo (stand-in for Uzi fire)"),
  fs("smg-micro-reload-01.mp3", 628693, 3609395, "KeshaFilm", "Uzi reload"),
  // AK-47
  ffl("ak47-shot-01.wav", "Prepared SFX/AK-47/C_28P.wav", "AK-47 7.62x39, single shots, near, stereo"),
  ffl("ak47-burst-01.wav", "Prepared SFX/AK-47/C_29P.wav", "AK-47, short bursts, near, stereo"),
  ffl("ak47-burst-02.wav", "Prepared SFX/AK-47/C_27P.wav", "AK-47, long burst, near, stereo"),
  fs("ak47-burst-03.mp3", 201670, 3749344, "Franki-01234", "AK-47 burst (blanks) fired into a street"),
  fs("ak47-burst-04.mp3", 520935, 773642, "ninebilly", "AK-47 burst, uploader's own recording"),
  ffl("ak47-mag-insert-01.wav", "Master Tracks/AK-47/C_10.wav", "AK-47, magazine inserted"),
  ffl("ak47-mag-detach-01.wav", "Master Tracks/AK-47/C_11.wav", "AK-47, magazine detached"),
  ffl("ak47-bolt-rack-01.wav", "Master Tracks/AK-47/C_2.wav", "AK-47, charging handle full cycle"),
  // M4-type carbine
  ffl("m4-shot-01.wav", "Prepared SFX/AR-15/D_32P.wav", "AR-15 / M4 5.56, single shots, near, stereo"),
  ffl("m4-shot-02.wav", "Prepared SFX/AR-15/D_24P.wav", "AR-15 / M4 5.56, single shots, mid distance, stereo"),
  fs("m4-shot-03.mp3", 162396, 71257, "qubodup", "M16 single shot, from a US Navy training video (public domain)"),
  fs("m4-burst-01.mp3", 201668, 3749344, "Franki-01234", "M16 burst (blanks) in a street"),
  ffl("m4-mag-insert-01.wav", "Master Tracks/AR-15/D_4.wav", "AR-15, loaded magazine inserted"),
  ffl("m4-bolt-rack-01.wav", "Master Tracks/AR-15/D_14.wav", "AR-15, charging handle full rack"),
  fs("mg-burst-01.mp3", 482122, 71257, "qubodup", "Machine gun burst, from a US military video (public domain)"),
  // shotguns
  ffl("shotgun-pump-shot-01.wav", "Prepared SFX/Nova/O_21P.wav", "Benelli Nova 12 gauge pump, single shots, near, stereo"),
  fs("shotgun-pump-shot-rack-01.mp3", 159710, 2886479, "AnthonyChan0", "Mossberg 500A, one shot then pump, woodland"),
  ffl("shotgun-pump-rack-01.wav", "Master Tracks/Nova/O_6.wav", "Benelli Nova, full pump cycle, loaded"),
  fs("shotgun-pump-rack-02.mp3", 449614, 9348290, "dasBUTCHER84", "Winchester SXP 20 gauge, pump rack"),
  fs("shotgun-pump-rack-03.mp3", 449612, 9348290, "dasBUTCHER84", "Winchester SXP 20 gauge, pump rack"),
  fs("shotgun-auto-shot-01.mp3", 865991, 19132311, "db465", "AA-12 auto shotgun shot — synthesized by a physics DSP program, not a recording"),
  // sniper rifle
  ffl("sniper-shot-01.wav", "Prepared SFX/Tikka/W_29P.wav", "Tikka T3 .30-06 bolt action, single shots, near, stereo"),
  ffl("sniper-shot-tail-01.wav", "Prepared SFX/Tikka/W_24P.wav", "Tikka T3 .30-06, single shots, mid distance with tail, stereo"),
  fs("sniper-shot-tail-02.mp3", 182790, 71257, "qubodup", "Sniper rifle shots, from a US government video (public domain)"),
  fs("sniper-shot-tail-03.mp3", 815476, 71257, "qubodup", "Sniper shot with echo, from a DVIDS video (public domain)"),
  fs("sniper-50cal-shot-01.mp3", 855595, 71257, "qubodup", "Barrett M82A1 single shot, from a DVIDS video (public domain)"),
  ffl("sniper-bolt-rack-01.wav", "Master Tracks/Tikka/W_16.wav", "Tikka T3, bolt full cycle, loaded"),
  fs("rifle-dry-fire-01.mp3", 725402, 7157894, "serøutōnin--deprivəd", "Ruger 10/22, dry fire click"),
  // rockets and explosions
  fs("rpg-launch-01.mp3", 67541, 71257, "qubodup", "BGM-71 TOW missile launch, from a US military video (stand-in for an RPG)"),
  fs("rpg-launch-02.mp3", 675581, 2524442, "craigsmith", "Bazooka shot, from a vintage film-effects tape library"),
  fs("grenade-explosion-01.mp3", 67469, 71257, "qubodup", "M67 fragmentation grenade, from a US military video (public domain)"),
  fs("grenade-explosion-02.mp3", 609587, 1038806, "unfa", "Grenade explosion, designed from a firecracker recording and a synth"),
  // casings, impacts, ricochets, whiz-bys, distance
  fs("casing-drop-01.mp3", 773859, 15468302, "MrGungus", ".223 casing dropped on a concrete floor"),
  fs("casing-drop-02.mp3", 778014, 8927049, "BlondPanda", "9mm casing dropped, metallic roll"),
  fs("casing-shotgun-shell-drop-01.mp3", 773860, 15468302, "MrGungus", "12 gauge shell dropped on a concrete floor"),
  fs("ricochet-01.mp3", 523403, 8956746, "C-V", ".22 rifle shot with a real ricochet, desert"),
  fs("ricochet-02.mp3", 30932, 15696, "aust_paul", "Four simulated ricochets off a hard surface"),
  fs("bullet-impact-ground-01.mp3", 789388, 15956618, "modusmogulus", "Bullet-impact-like hit on the ground (made with a whip)"),
  fs("bullet-impact-metal-01.mp3", 351371, 4603244, "wilhellboy", "Bullet ping on metal (made with pots and pans)"),
  fs("whiz-by-01.mp3", 789222, 15956618, "modusmogulus", "Subsonic fly-by whiz (recorded from a whip)"),
  fs("whiz-by-02.mp3", 505238, 150886, "Daleonfire", "Whiz-by, synthesized"),
  fs("distant-shot-01.mp3", 541164, 9662992, "Resaural", "A distant gunshot at night, with tail"),
];

const MAX_BYTES = 150e6; // the whole folder stays under this

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
for (const s of SOUNDS) {
  const dest = resolve(OUT, s.file);
  if (!existsSync(dest)) {
    try {
      let buf, err;
      for (let n = 0; n < 4 && !buf; n++) { // the GitHub mirror drops large files now and then
        try {
          const res = await fetch(s.download, { headers: { "User-Agent": UA } });
          if (!res.ok) throw new Error(`http ${res.status}`);
          buf = Buffer.from(await res.arrayBuffer());
        } catch (e) { err = e; }
      }
      if (!buf) throw err;
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
  note: "Every file is CC0 1.0 on its source page: commercial use, no attribution required. Freesound files are preview MP3s of the originals.",
  licenceUrl: "https://creativecommons.org/publicdomain/zero/1.0/",
  sounds: manifest,
}, null, 2) + "\n");
console.log(`${manifest.length}/${SOUNDS.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
if (failed) process.exitCode = 1;
