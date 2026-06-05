#!/usr/bin/env node
// title-card.mjs — compose the branded title card: a custom colored-pencil
// "Silly Symphonies" frame illustration (gen) + YWFT/AC typography laid into its
// empty centre cartouche. Outputs /tmp/wsvideo-<name>/title_card.png.
//
// Usage: node marketing/bin/title-card.mjs <campaign-dir>
import { mkdirSync, existsSync } from "node:fs";
import { execSync } from "node:child_process";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

const FONT = `${process.env.HOME}/Library/Fonts/ywft-processing-bold.ttf`;
const W = 1920, H = 1080;

const campDir = resolve(process.cwd(), process.argv[2] || ".");
const { default: cfg } = await import(pathToFileURL(`${campDir}/campaign.mjs`).href);
const tc = cfg.titleCard || {};
const TMP = `/tmp/wsvideo-${cfg.name}`; mkdirSync(TMP, { recursive: true });
const OUT = `${TMP}/title_card.png`;

const gen = tc.gen ? resolve(campDir, tc.gen) : `${campDir}/title-card/gens/v1.png`;
if (!existsSync(gen)) { console.error(`✗ no title-card illustration at ${gen}`); process.exit(1); }

const BG = tc.bg || "#0a1a13";                 // deep green letterbox (matches the frame ground)
const titleParts = tc.title || cfg.wordmark || ["Aesthetic", "Computer"];
const sub = tc.subtitle || "";
const INK = tc.ink || "#0e2c20";               // dark ink for the cream cartouche
const DOT = tc.dot || "#1c9b5e";               // brand-green accent dot
const AMBER = tc.subInk || "#7a5512";
const cx = tc.cx ?? Math.round(W / 2), titleY = tc.titleY ?? 322;
const titlePx = tc.titlePx ?? 92, subPx = tc.subPx ?? 46;

const idim = (p) => execSync(`magick identify -format "%w %h" "${p}"`).toString().trim().split(" ").map(Number);
function textPng(out, text, px, fill) {
  execSync(`magick -background none -fill "${fill}" -font "${FONT}" -pointsize ${px} label:"${text}" "${out}"`);
  return idim(out);
}

// base: gen scaled to fill height, centred on the letterbox ground
execSync(`magick -size ${W}x${H} xc:'${BG}' \\( "${gen}" -resize x${H} \\) -gravity center -composite "${TMP}/tc_base.png"`);

// title: two words STACKED (fits the oval cartouche), small green brand dot
// between them, subtitle below — all centred.
const [pa, pb] = titleParts;
const [waw, wah] = textPng(`${TMP}/tc_a.png`, pa, titlePx, INK);
const [wbw, wbh] = textPng(`${TMP}/tc_b.png`, pb, titlePx, INK);
const dotSide = Math.round(titlePx * 0.15);
execSync(`magick -size ${dotSide}x${dotSide} xc:'${DOT}' "${TMP}/tc_dot.png"`);
const lg = Math.round(titlePx * 0.16);
const y1 = titleY, dy = y1 + wah + lg, y2 = dy + dotSide + lg;
const cmds = [`magick "${TMP}/tc_base.png"`,
  `\\( "${TMP}/tc_a.png" \\) -gravity NorthWest -geometry +${cx - Math.round(waw / 2)}+${y1} -composite`,
  `\\( "${TMP}/tc_dot.png" \\) -gravity NorthWest -geometry +${cx - Math.round(dotSide / 2)}+${dy} -composite`,
  `\\( "${TMP}/tc_b.png" \\) -gravity NorthWest -geometry +${cx - Math.round(wbw / 2)}+${y2} -composite`];

// subtitle (centred, below the stacked title)
if (sub) {
  const [sw] = textPng(`${TMP}/tc_sub.png`, sub, subPx, AMBER);
  const sy = y2 + wbh + Math.round(lg * 2.2);
  cmds.push(`\\( "${TMP}/tc_sub.png" \\) -gravity NorthWest -geometry +${cx - Math.round(sw / 2)}+${sy} -composite`);
}
cmds.push(`"${OUT}"`);
execSync(cmds.join(" "));
console.log(`✓ title card → ${OUT}`);
execSync(`open -a Preview "${OUT}"`);
