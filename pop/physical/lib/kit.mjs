// Shared spine for the physical kits: cover mosaics, exact-size artwork,
// and the small measurements both the disc and the tape need.

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { basename, resolve } from "node:path";

export const PALETTE = ["#ff6b9d", "#4ecdc4", "#ffe66d", "#8ce99a"];
// The bright palette is for bars and fills. Set on paper it loses the
// yellow and green entirely, so type gets these darker cousins.
export const TEXT_PALETTE = ["#e0447c", "#1f9d94", "#b07d00", "#3f9a55"];
export const PAPER = "#fff9fc";
export const INK = "#282430";
export const ACCENT = "#b44887";

export const esc = (text) => String(text).replace(/[&<>"']/g, (character) => ({
  "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&apos;",
}[character]));

export const clock = (seconds) => {
  const rounded = Math.round(seconds);
  return `${Math.floor(rounded / 60)}:${String(rounded % 60).padStart(2, "0")}`;
};

export const digest = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");

export const duration = (path) => Number(execFileSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", path,
], { encoding: "utf8" }).trim());

export async function download(url, path, force = false) {
  if (!force && existsSync(path)) return;
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${response.status} downloading ${url}`);
  writeFileSync(path, Buffer.from(await response.arrayBuffer()));
}

// Cover thumbnails are embedded as data URIs, so they stay small enough
// that libxml never meets a single line it won't parse.
export function coverTiles(paths, cacheDir, force = false) {
  return paths.map((path, index) => {
    const preview = resolve(cacheDir, `.mosaic-${String(index + 1).padStart(2, "0")}.jpg`);
    if (force || !existsSync(preview)) {
      execFileSync("magick", [path, "-resize", "640x640^", "-gravity", "center",
        "-extent", "640x640", "-quality", "88", preview]);
    }
    return `data:image/jpeg;base64,${readFileSync(preview).toString("base64")}`;
  });
}

export function mosaic(tiles, width, height, opacity = 1) {
  const columns = 3;
  const cellWidth = width / columns;
  const cellHeight = height / Math.ceil(tiles.length / columns);
  return tiles.map((tile, index) => {
    const x = (index % columns) * cellWidth;
    const y = Math.floor(index / columns) * cellHeight;
    return `<svg x="${x}" y="${y}" width="${cellWidth + 1}" height="${cellHeight + 1}" overflow="hidden"><image href="${tile}" width="100%" height="100%" preserveAspectRatio="xMidYMid slice" opacity="${opacity}"/></svg>`;
  }).join("");
}

export function svg(width, height, body) {
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"><rect width="100%" height="100%" fill="${PAPER}"/>${body}</svg>`;
}

// One sheet of every printed part, so a build can be looked at rather than
// trusted. Assembled from padded thumbnails because magick montage wants a
// font this machine does not carry.
export function proof(out, files, columns = 2) {
  const path = resolve(out, "proof.jpg");
  const cells = files.map((file, index) => {
    const cell = resolve(out, `.proof-${index}.png`);
    execFileSync("magick", [resolve(out, file), "-resize", "700x700",
      "-background", "#2b2836", "-gravity", "center", "-extent", "740x740", cell]);
    return cell;
  });
  const rows = [];
  for (let index = 0; index < cells.length; index += columns) {
    const row = resolve(out, `.proof-row-${rows.length}.png`);
    execFileSync("magick", [...cells.slice(index, index + columns), "+append", row]);
    rows.push(row);
  }
  execFileSync("magick", [...rows, "-append", "-quality", "92", path]);
  for (const temporary of [...cells, ...rows]) rmSync(temporary, { force: true });
  return basename(path);
}

// Kunaki wants JPEG at 300 DPI in the exact pixel size of the part.
export function render(out, name, spec, body, dpi = 300) {
  const sourcePath = resolve(out, `.${name}.svg`);
  const pngPath = resolve(out, `.${name}.png`);
  const jpgPath = resolve(out, `${name}.jpg`);
  writeFileSync(sourcePath, svg(spec.width, spec.height, body));
  execFileSync("rsvg-convert", ["--width", String(spec.width), "--height", String(spec.height), "--output", pngPath, sourcePath]);
  execFileSync("magick", [pngPath, "-units", "PixelsPerInch", "-density", String(dpi), "-quality", "96", jpgPath]);
  return basename(jpgPath);
}
