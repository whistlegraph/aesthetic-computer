#!/usr/bin/env node
// fetch-samples.mjs — the band's sampled instruments, from proper places.
//
//   drums  Virtuosity Drums (Versilian Studios + Karoryfer Samples), CC0
//          github.com/sfzinstruments/virtuosity_drums — a real kit, multi-mic
//   bass   Growlybass (Karoryfer Samples), CC0 — a Squier Jazz Bass, direct
//          github.com/sfzinstruments/karoryfer.growlybass
//   modem  Freesound 454650 "MODEM_2.aif" by G_M_D_THREE, CC0 — already in
//          the vault's freesound cache (see pop/samples/modem/MODEM.md)
//
// Only the hits this track plays are fetched (about 40 MB of a 1.5 GB pair of
// libraries), into out/samples/ — gitignored like every pop out/. Re-runnable.
import { mkdirSync, existsSync, writeFileSync, statSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
export const SAMPLES = resolve(HERE, "../out/samples");
const RAW = "https://raw.githubusercontent.com/sfzinstruments";
const rr = (n) => Array.from({ length: n }, (_, i) => i + 1);

// piece → the files per mic. Velocity layers are the kit's own; we take the loud end.
export const KIT = {
  kick:   { mics: ["kickmic", "oh", "room"], files: [3, 4].flatMap((v) => rr(4).map((r) => `kick/MIC_kick_snon_vl${v}_rr${r}.flac`)) },
  snare:  { mics: ["snaremic", "oh", "room"], files: [26, 28, 30, 32, 34, 36].map((v) => `snare/MIC_snare_center_vl${v}.flac`) },
  ghost:  { mics: ["snaremic", "oh"], files: [8, 10, 12, 14].map((v) => `snare/MIC_snare_center_vl${v}.flac`) },
  rim:    { mics: ["snaremic", "oh"], files: [10, 12, 14, 16].map((v) => `snare/MIC_snare_crossstick_vl${v}.flac`) },
  hat:    { mics: ["oh"], files: [3, 4].flatMap((v) => rr(4).map((r) => `hh/MIC_hh_closed_vl${v}_rr${r}.flac`)) },
  ride:   { mics: ["oh", "room"], files: [2, 3].flatMap((v) => rr(4).map((r) => `ride/MIC_ride_ride_vl${v}_rr${r}.flac`)) },
  crash:  { mics: ["oh", "room"], files: rr(4).map((r) => `crash/MIC_crash_crash_vl3_rr${r}.flac`) },
  htom:   { mics: ["oh", "room"], files: [12, 14, 16].map((v) => `htom/MIC_htom_center_vl${v}.flac`) },
  ltom:   { mics: ["oh", "room"], files: [12, 14, 16].map((v) => `ltom/MIC_ltom_center_vl${v}.flac`) },
};
export const BASS_NOTES = ["db2", "e2", "gb2", "a2", "c3"];
export const BASS = BASS_NOTES.flatMap((n) => [`${n}_ff_rr1`, `${n}_ff_rr2`, `${n}_f_rr1`].map((f) => `sustain/${f}.wav`));
export const kitPath = (mic, file) => resolve(SAMPLES, "virtuosity_drums", mic, file.replaceAll("MIC", mic));
export const bassPath = (file) => resolve(SAMPLES, "growlybass", file);

if (import.meta.url === `file://${process.argv[1]}`) {
  const jobs = [];
  for (const { mics, files } of Object.values(KIT)) for (const mic of mics) for (const f of files)
    jobs.push([`${RAW}/virtuosity_drums/master/Samples/${mic}/${f.replaceAll("MIC", mic)}`, kitPath(mic, f)]);
  for (const f of BASS) jobs.push([`${RAW}/karoryfer.growlybass/master/${f}`, bassPath(f)]);
  let got = 0, failed = 0;
  for (const [url, path] of jobs) {
    if (existsSync(path) && statSync(path).size > 2000) continue;
    mkdirSync(dirname(path), { recursive: true });
    const r = spawnSync("curl", ["-sfL", "--retry", "2", "-o", path, url]);
    if (r.status === 0) got++; else { failed++; console.log(`FAILED ${url}`); }
  }
  writeFileSync(resolve(SAMPLES, "SOURCES.txt"), [
    "virtuosity_drums — Versilian Studios + Karoryfer Samples — CC0 1.0 — https://github.com/sfzinstruments/virtuosity_drums",
    "growlybass — Karoryfer Samples — CC0 1.0 — https://github.com/sfzinstruments/karoryfer.growlybass",
    "modem — Freesound 454650 MODEM_2.aif by G_M_D_THREE — CC0 1.0 — https://freesound.org/s/454650/",
  ].join("\n") + "\n");
  console.log(`${jobs.length} files in the kit, ${got} fetched now, ${failed} failed`);
}
