#!/usr/bin/env node
// stage-map.mjs, 2026.11.19
// Draws a stage of No Paint classic: the frame the game showed, and over it
// every placed thing as a numbered box, with a list beside it that names each
// one (type, UID, layer, place and size in layout pixels, its words).
//
// The stage is what the bridge's "stage" op answers (classic-bridge.js) and
// the frame is a screenshot taken at the same moment; tests/browser/
// classic-stage.mjs makes both for each screen of the game.
//
//   node toolchain/nopaint/stage-map.mjs stage.json frame.png out.svg [--hidden] [--layer=UI]
//
// The SVG stands alone (the frame is inside it). Solid boxes are shown things,
// dashed ones are there but not visible (drawn only with --hidden).

import { readFileSync, writeFileSync } from "node:fs";

const [stagePath, framePath, outPath, ...flags] = process.argv.slice(2);
if (!stagePath || !framePath || !outPath) { console.error("usage: stage-map.mjs stage.json frame.png out.svg [--hidden] [--layer=NAME]"); process.exit(2); }
const stage = JSON.parse(readFileSync(stagePath, "utf8")), frame = readFileSync(framePath);
const hidden = flags.includes("--hidden"), only = flags.find((flag) => flag.startsWith("--layer="))?.slice(8);
const W = frame.readUInt32BE(16), H = frame.readUInt32BE(20); // a PNG says its size here
const ZOOM = Number(flags.find((flag) => flag.startsWith("--zoom="))?.slice(7)) || 2;
const esc = (text) => String(text).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const plain = (text) => String(text).replace(/\[[^\]]*\]/g, "").replace(/\s+/g, " ").trim();
const HUES = ["#e6194b", "#3cb44b", "#4363d8", "#f58231", "#911eb4", "#008080", "#9a6324", "#800000", "#000075", "#808000", "#f032e6", "#469990"];

// Things too big to be worth a box (backdrops, buffers the size of the screen) go in the list only.
const things = stage.things.filter((thing) => (hidden || thing.shown) && (!only || thing.layer === only));
const layers = [...new Set(things.map((thing) => thing.layer))];
const rows = things.map((thing, i) => {
  const [[x0, y0], [x1, y1]] = thing.css, w = x1 - x0, h = y1 - y0;
  const onPage = x1 > 0 && y1 > 0 && x0 < W && y0 < H;
  return { ...thing, n: i + 1, x0, y0, w, h, hue: HUES[layers.indexOf(thing.layer) % HUES.length], boxed: onPage && w * h < W * H * 0.6 };
});
const LIST = 560, LINE = 15, height = Math.max(H * ZOOM, 40 + rows.length * LINE + layers.length * 4);
const boxes = rows.filter((row) => row.boxed).map((row) => {
  const x = row.x0 * ZOOM, y = row.y0 * ZOOM, w = Math.max(2, row.w * ZOOM), h = Math.max(2, row.h * ZOOM);
  return `<rect x="${x.toFixed(1)}" y="${y.toFixed(1)}" width="${w.toFixed(1)}" height="${h.toFixed(1)}" fill="none" stroke="${row.hue}" stroke-width="1.5"${row.shown ? "" : ' stroke-dasharray="4 3"'}/>`
    + `<rect x="${x.toFixed(1)}" y="${(y - 1).toFixed(1)}" width="${String(row.n).length * 7 + 6}" height="13" fill="${row.hue}"/>`
    + `<text x="${(x + 3).toFixed(1)}" y="${(y + 9).toFixed(1)}" font-size="11" fill="#fff">${row.n}</text>`;
}).join("\n");
const list = rows.map((row, i) => {
  const words = row.text !== undefined ? ` “${esc(plain(row.text).slice(0, 26))}”` : row.animation !== undefined ? ` ${esc(row.animation)}:${row.frame}` : "";
  const tint = row.tint && row.tint.some((v) => Math.abs(v - 1) > 0.01) ? ` tint ${row.tint.map((v) => v.toFixed(1)).join(",")}` : "";
  const line = `${String(row.n).padStart(3)} ${row.type} #${row.uid} ${row.layer}  ${Math.round(row.box[0])},${Math.round(row.box[1])} ${Math.round(row.box[2] - row.box[0])}×${Math.round(row.box[3] - row.box[1])}${row.opacity < 1 ? ` α${row.opacity.toFixed(2)}` : ""}${tint}${row.boxed ? "" : " (no box)"}${words}`;
  return `<text x="${W * ZOOM + 12}" y="${36 + i * LINE}" font-size="11" fill="${row.shown ? row.hue : "#999"}" xml:space="preserve">${esc(line)}</text>`;
}).join("\n");
writeFileSync(outPath, `<svg xmlns="http://www.w3.org/2000/svg" width="${W * ZOOM + LIST}" height="${height}" font-family="Menlo, monospace">
<rect width="100%" height="100%" fill="#fff"/>
<image href="data:image/png;base64,${frame.toString("base64")}" width="${W * ZOOM}" height="${H * ZOOM}" style="image-rendering:pixelated"/>
${boxes}
<text x="${W * ZOOM + 12}" y="16" font-size="12" font-weight="bold">${esc(stage.layout)} · ${stage.size.join("×")} layout px · ${rows.length} things${hidden ? " (hidden too)" : " shown"}</text>
${list}
</svg>
`);
console.log(`${outPath}: ${stage.layout}, ${rows.length} things, ${rows.filter((row) => row.boxed).length} boxed`);
