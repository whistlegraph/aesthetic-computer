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

// title: two words on ONE line (the "aesthetic.computer" wordmark), small green
// brand dot between them, subtitle below — all centred. Auto-scaled down so the
// whole line fits inside the oval cartouche.
const [pa, pb] = titleParts;
const maxW = Math.round(W * 0.74);
let px = titlePx, waw, wah, wbw, wbh, dotSide, hg, totalW;
for (let pass = 0; pass < 6; pass++) {
  [waw, wah] = textPng(`${TMP}/tc_a.png`, pa, px, INK);
  [wbw, wbh] = textPng(`${TMP}/tc_b.png`, pb, px, INK);
  dotSide = Math.round(px * 0.16);
  hg = Math.round(px * 0.30);                       // horizontal gap around the dot
  totalW = waw + hg + dotSide + hg + wbw;
  if (totalW <= maxW) break;
  px = Math.round(px * maxW / totalW * 0.99);       // shrink to fit, retry
}
execSync(`magick -size ${dotSide}x${dotSide} xc:'${DOT}' "${TMP}/tc_dot.png"`);
const lineH = Math.max(wah, wbh);
const y1 = titleY;
const xa = cx - Math.round(totalW / 2);
const xdot = xa + waw + hg;
const xb = xdot + dotSide + hg;
const yDot = y1 + Math.round(lineH / 2 - dotSide / 2);
const cmds = [`magick "${TMP}/tc_base.png"`,
  `\\( "${TMP}/tc_a.png" \\) -gravity NorthWest -geometry +${xa}+${y1} -composite`,
  `\\( "${TMP}/tc_dot.png" \\) -gravity NorthWest -geometry +${xdot}+${yDot} -composite`,
  `\\( "${TMP}/tc_b.png" \\) -gravity NorthWest -geometry +${xb}+${y1} -composite`];

// subtitle (centred, below the single-line title)
if (sub) {
  const [sw] = textPng(`${TMP}/tc_sub.png`, sub, subPx, AMBER);
  const sy = y1 + lineH + Math.round(px * 0.36);
  cmds.push(`\\( "${TMP}/tc_sub.png" \\) -gravity NorthWest -geometry +${cx - Math.round(sw / 2)}+${sy} -composite`);
}
cmds.push(`"${OUT}"`);
execSync(cmds.join(" "));
console.log(`✓ title card → ${OUT}`);
if (process.argv.includes("--open")) execSync(`open -a Preview "${OUT}"`);
