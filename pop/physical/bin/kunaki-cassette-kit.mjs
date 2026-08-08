#!/usr/bin/env node
// Build a Kunaki cassette kit from the same program the disc uses.
//
// A tape is two continuous sides, so the gaps the CD plant inserts have
// to be printed into the audio here. Side A takes the first --split
// tracks; the rest go to side B. Each side must stay under 40 minutes.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { CASSETTE_SPECS } from "../../../marketing/podcast/lib/kunaki.mjs";
import { ACCENT, INK, PALETTE, TEXT_PALETTE, clock, coverTiles, digest, download, duration, esc, mosaic, proof, render } from "../lib/kit.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const argv = process.argv.slice(2);
const value = (name, fallback) => {
  const index = argv.indexOf(`--${name}`);
  return index === -1 ? fallback : argv[index + 1];
};
const force = argv.includes("--force");

const program = JSON.parse(readFileSync(resolve(value("program", resolve(HERE, "../pixsies-so-far.json"))), "utf8"));
const mastersDir = resolve(value("masters", resolve(HERE, "../masters")));
const masters = JSON.parse(readFileSync(resolve(mastersDir, "manifest.json"), "utf8"));
const out = resolve(value("out", resolve(HERE, "../out/pixsies-so-far-kunaki-cassette")));
const coverDir = resolve(out, ".covers");
const split = Number(value("split", 5));
const gap = CASSETTE_SPECS.audio.gapSeconds ?? 2;
mkdirSync(coverDir, { recursive: true });

const tracks = masters.tracks.map((master, index) => ({
  ...master,
  side: index < split ? "A" : "B",
  cover: program.tracks[index].cover,
}));

const coverPaths = [];
for (const track of tracks) {
  const path = resolve(coverDir, `${String(track.position).padStart(2, "0")}-${track.slug}.jpg`);
  await download(track.cover, path, force);
  coverPaths.push(path);
}

// One ffmpeg pass per side: every master in, silence between, one WAV out.
function buildSide(letter) {
  const members = tracks.filter((track) => track.side === letter);
  if (!members.length) throw new Error(`Side ${letter} has no tracks`);
  const path = resolve(out, `side-${letter.toLowerCase()}.wav`);
  const inputs = members.flatMap((track) => ["-i", resolve(mastersDir, track.file)]);
  const pads = members.map((track, index) => (index === members.length - 1
    ? `[${index}:a]aresample=44100[a${index}]`
    : `[${index}:a]aresample=44100,apad=pad_dur=${gap}[a${index}]`)).join(";");
  const concat = `${members.map((_, index) => `[a${index}]`).join("")}concat=n=${members.length}:v=0:a=1[out]`;
  if (force || !existsSync(path)) {
    execFileSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...inputs,
      "-filter_complex", `${pads};${concat}`, "-map", "[out]",
      "-ac", "2", "-c:a", "pcm_s16le", path], { stdio: "inherit" });
  }
  const seconds = duration(path);
  if (seconds > CASSETTE_SPECS.audio.maxMinutesPerSide * 60) {
    throw new Error(`Side ${letter} runs ${clock(seconds)}; a side holds ${CASSETTE_SPECS.audio.maxMinutesPerSide} minutes`);
  }
  return { letter, file: `side-${letter.toLowerCase()}.wav`, durationSeconds: seconds, sha256: digest(path), tracks: members };
}

const sides = ["A", "B"].map(buildSide);

const title = esc(program.title);
const artist = esc(program.artist);
const tiles = coverTiles(coverPaths, coverDir, force);

const jCard = render(out, "j-card", CASSETTE_SPECS.artwork.jCard, `${mosaic(tiles, CASSETTE_SPECS.artwork.jCard.width, CASSETTE_SPECS.artwork.jCard.height)}
  <rect x="96" y="404" width="1008" height="300" rx="8" fill="#fff9fc" opacity=".94"/>
  <text x="600" y="530" text-anchor="middle" font-family="monospace" font-size="74" font-weight="700" fill="${INK}">${title}</text>
  <text x="600" y="612" text-anchor="middle" font-family="monospace" font-size="32" fill="${ACCENT}">${artist}</text>
  <text x="600" y="668" text-anchor="middle" font-family="monospace" font-size="24" fill="#777">aesthetic.computer/pop · ${esc(program.asOf)}</text>`);

// The label is the only part with room for a side's running order.
const labels = sides.map((side) => {
  const rows = side.tracks.map((track, index) => {
    const y = 190 + index * 58;
    return `<text x="70" y="${y}" font-family="monospace" font-size="30" fill="${INK}"><tspan fill="${TEXT_PALETTE[index % TEXT_PALETTE.length]}">${String(track.position).padStart(2, "0")}</tspan><tspan dx="20">${esc(track.title)}</tspan></text><text x="992" y="${y}" text-anchor="end" font-family="monospace" font-size="24" fill="#777">${clock(track.durationSeconds)}</text>`;
  }).join("");
  return render(out, `label-${side.letter.toLowerCase()}`, CASSETTE_SPECS.artwork.labelA, `
    <rect width="266" height="22" fill="${PALETTE[0]}"/><rect x="266" width="266" height="22" fill="${PALETTE[1]}"/><rect x="532" width="266" height="22" fill="${PALETTE[2]}"/><rect x="798" width="264" height="22" fill="${PALETTE[3]}"/>
    <text x="70" y="104" font-family="monospace" font-size="44" font-weight="700" fill="${INK}">${title}</text>
    <text x="992" y="104" text-anchor="end" font-family="monospace" font-size="44" font-weight="700" fill="${ACCENT}">SIDE ${side.letter}</text>
    <text x="70" y="146" font-family="monospace" font-size="24" fill="#777">${artist} · ${clock(side.durationSeconds)}</text>
    ${rows}`);
});

const production = {
  schemaVersion: 1,
  state: "prepared",
  preparedAt: new Date().toISOString(),
  title: program.title,
  artist: program.artist,
  asOf: program.asOf,
  sourceAudio: `Red Book masters from bin/masters.mjs (${masters.lossless} of ${masters.tracks.length} lossless).`,
  program: {
    trackCount: tracks.length,
    gapSeconds: gap,
    sides: sides.map((side) => ({
      letter: side.letter,
      file: side.file,
      durationSeconds: side.durationSeconds,
      displayDuration: clock(side.durationSeconds),
      sha256: side.sha256,
      tracks: side.tracks.map((track) => ({ position: track.position, title: track.title, durationSeconds: track.durationSeconds, lossy: track.lossy })),
    })),
  },
  artwork: { jCard, labelA: labels[0], labelB: labels[1], proof: proof(out, [jCard, ...labels], 1) },
  vendor: {
    name: "kunaki",
    productType: "cassette",
    productId: null,
    productCreation: "browser-upload",
    fulfillment: "http-api",
    publishedUnitPriceUsd: CASSETTE_SPECS.priceUsd,
  },
  specs: CASSETTE_SPECS,
};
writeFileSync(resolve(out, "manifest.json"), `${JSON.stringify(production, null, 2)}\n`);
writeFileSync(resolve(out, "README.md"), `# ${program.title} — cassette\n\nUpload \`side-a.wav\` and \`side-b.wav\` as the two sides, then \`j-card.jpg\` for the case insert and \`label-a.jpg\` / \`label-b.jpg\` for the shell. The gaps between tracks are already printed into each side.\n\nRecord the 10-character product id in \`manifest.json\` under \`vendor.productId\`, then quote and order with \`bin/kunaki-order.mjs --kit ${out.replace(process.env.HOME || "", "~")}\`.\n`);

console.log(`${out}\n${sides.map((side) => `side ${side.letter} · ${side.tracks.length} tracks · ${clock(side.durationSeconds)}`).join("\n")}`);
