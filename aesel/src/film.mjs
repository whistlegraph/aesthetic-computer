// film — a short film from a score a model can write.
//
// A language model cannot emit video. It can write a first frame as SVG and
// say what moves: "the sun's cy goes from 420 to 120 between 0 and 6 seconds".
// This file plays that: it works out every frame, has macOS draw them (sips),
// and has ffmpeg bind them into an .mp4, with a soundtrack when the film
// carries a wav score. Nothing the model wrote is run as code.
//
//   { "seconds": 8, "fps": 12, "width": 640, "height": 480,
//     "svg": "<svg …>…</svg>",                       the first frame
//     "tracks": [{ "id": "sun", "attr": "cy", "from": 420, "to": 120, "start": 0, "end": 6, "ease": "inout" }],
//     "score": { "bpm": 96, "notes": […] } }          optional
import { spawn, spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { renderScore, scoreProblems } from "./wav-score.mjs";

export const MAX_SECONDS = 12, MAX_FPS = 15, MAX_TRACKS = 60, MAX_SIDE = 960;
const EASES = { linear: (t) => t, in: (t) => t * t, out: (t) => t * (2 - t), inout: (t) => (t < 0.5 ? 2 * t * t : 1 - 2 * (1 - t) * (1 - t)) };
const NUMBER = /-?\d*\.?\d+(?:e-?\d+)?/gi;
const HEX = /^#([0-9a-f]{6})$/i;

/** What is wrong with a film, as lines the model can act on. */
export function filmProblems(film) {
  const problems = [];
  if (!film || typeof film !== "object") return ["The film must be a JSON object."];
  if (!/^\s*<svg[\s>][\s\S]*<\/svg>\s*$/.test(String(film.svg || ""))) problems.push("`svg` must be one complete <svg>…</svg> document: the first frame.");
  if (!(film.seconds > 0 && film.seconds <= MAX_SECONDS)) problems.push(`\`seconds\` must be between 1 and ${MAX_SECONDS}.`);
  if (film.fps !== undefined && !(film.fps >= 1 && film.fps <= MAX_FPS)) problems.push(`\`fps\` must be between 1 and ${MAX_FPS}.`);
  for (const side of ["width", "height"]) if (!(film[side] >= 64 && film[side] <= MAX_SIDE)) problems.push(`\`${side}\` must be between 64 and ${MAX_SIDE}.`);
  const tracks = Array.isArray(film.tracks) ? film.tracks : [];
  if (!tracks.length) problems.push("`tracks` is empty: nothing would move.");
  if (tracks.length > MAX_TRACKS) problems.push(`At most ${MAX_TRACKS} tracks.`);
  for (const [index, track] of tracks.entries()) {
    const where = `tracks[${index}]`;
    if (!track?.id || !new RegExp(`\\bid="${escape(track.id)}"`).test(film.svg || "")) { problems.push(`${where}: no element in the svg has id="${track?.id}".`); continue; }
    if (!track.attr) problems.push(`${where}: name the attribute in \`attr\`.`);
    if (!Array.isArray(track.steps) && (track.from === undefined || track.to === undefined)) problems.push(`${where}: give \`from\` and \`to\`, or \`steps\`.`);
    if (!(track.end > track.start)) problems.push(`${where}: \`end\` must come after \`start\`.`);
    if (problems.length > 6) break;
  }
  if (film.score !== undefined) problems.push(...scoreProblems(film.score).map((problem) => `score: ${problem}`));
  return problems.slice(0, 8);
}

const escape = (text) => String(text).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
const round = (n) => Math.round(n * 100) / 100;

// Between two values: numbers, #rrggbb colours, or strings whose numbers move.
function between(from, to, t) {
  if (typeof from === "number" && typeof to === "number") return round(from + (to - from) * t);
  const a = String(from), b = String(to);
  const ha = HEX.exec(a), hb = HEX.exec(b);
  if (ha && hb) return "#" + [0, 2, 4].map((i) => Math.round(parseInt(ha[1].slice(i, i + 2), 16) * (1 - t) + parseInt(hb[1].slice(i, i + 2), 16) * t).toString(16).padStart(2, "0")).join("");
  const na = a.match(NUMBER) || [], nb = b.match(NUMBER) || [];
  if (na.length && na.length === nb.length && a.replace(NUMBER, "#") === b.replace(NUMBER, "#")) {
    let i = 0;
    return a.replace(NUMBER, () => round(Number(na[i]) + (Number(nb[i]) - Number(na[i++])) * t));
  }
  return t < 1 ? a : b;
}

/** A track's value at a time in seconds. */
export function valueAt(track, seconds) {
  const span = Math.max(1e-6, track.end - track.start);
  const t = Math.max(0, Math.min(1, (seconds - track.start) / span));
  if (Array.isArray(track.steps)) return track.steps[Math.min(track.steps.length - 1, Math.floor(t * track.steps.length))];
  return between(track.from, track.to, (EASES[track.ease] || EASES.inout)(t));
}

const attribute = (text) => String(text).replace(/&/g, "&amp;").replace(/"/g, "&quot;").replace(/</g, "&lt;");
const content = (text) => String(text).replace(/&/g, "&amp;").replace(/</g, "&lt;");

/** The film's frame at a time: the svg with every track's value written in. */
export function frameAt(film, seconds) {
  let svg = film.svg;
  for (const track of film.tracks || []) {
    const value = valueAt(track, seconds);
    const open = new RegExp(`<([a-zA-Z][\\w:-]*)((?:\\s[^<>]*?)?\\sid="${escape(track.id)}"[^<>]*?)(/?)>`);
    const found = open.exec(svg);
    if (!found) continue;
    const [whole, tag, attrs, selfClosing] = found;
    if (track.attr === "text") {
      const close = svg.indexOf(`</${tag}>`, found.index);
      if (close > 0 && !selfClosing) svg = svg.slice(0, found.index + whole.length) + content(value) + svg.slice(close);
      continue;
    }
    const pair = new RegExp(`(\\s)${escape(track.attr)}="[^"]*"`);
    const next = pair.test(attrs) ? attrs.replace(pair, `$1${track.attr}="${attribute(value)}"`) : `${attrs} ${track.attr}="${attribute(value)}"`;
    svg = svg.slice(0, found.index) + `<${tag}${next}${selfClosing}>` + svg.slice(found.index + whole.length);
  }
  return svg;
}

// The root says how big it is drawn, and stands on white paper.
function sized(svg, width, height) {
  return svg.replace(/<svg\b([^>]*)>/, (whole, attrs) =>
    `<svg${attrs.replace(/\s(width|height)="[^"]*"/g, "")} width="${width}" height="${height}"><rect width="100%" height="100%" fill="#ffffff"/>`);
}

const run = (command, args) => new Promise((resolve, reject) => {
  const child = spawn(command, args, { stdio: ["ignore", "ignore", "pipe"] });
  let said = "";
  child.stderr.on("data", (data) => { said = (said + data).slice(-600); });
  child.once("error", reject);
  child.once("exit", (status) => (status === 0 ? resolve() : reject(new Error(`${command} failed: ${said.trim().split("\n").at(-1) || status}`))));
});

/** Render a film to .mp4 bytes. Needs macOS (sips draws the frames) and ffmpeg. */
export async function renderFilm(film, { onstep = () => {} } = {}) {
  if (process.platform !== "darwin") throw new Error("a film is drawn by macOS for now");
  if (spawnSync("which", ["ffmpeg"]).status !== 0) throw new Error("a film needs ffmpeg to bind its frames: brew install ffmpeg");
  const fps = Math.round(film.fps || 12), count = Math.max(1, Math.round(film.seconds * fps));
  const scratch = mkdtempSync(path.join(tmpdir(), "film-"));
  try {
    const names = [];
    for (let i = 0; i < count; i++) {
      const name = `f${String(i).padStart(4, "0")}`;
      writeFileSync(path.join(scratch, `${name}.svg`), sized(frameAt(film, i / fps), film.width, film.height));
      names.push(name);
    }
    onstep(`drawing ${count} frames`);
    await run("sips", ["-s", "format", "png", ...names.map((name) => path.join(scratch, `${name}.svg`)), "--out", scratch]);
    const sound = film.score ? path.join(scratch, "sound.wav") : "";
    if (sound) writeFileSync(sound, renderScore(film.score));
    onstep("binding the film");
    const out = path.join(scratch, "film.mp4");
    await run("ffmpeg", ["-y", "-loglevel", "error", "-framerate", String(fps), "-i", path.join(scratch, "f%04d.png"), ...(sound ? ["-i", sound] : []),
      "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2", "-c:v", "libx264", "-pix_fmt", "yuv420p", "-r", String(fps),
      ...(sound ? ["-c:a", "aac", "-af", "apad"] : []), "-t", String(film.seconds), "-movflags", "+faststart", out]);
    return readFileSync(out);
  } finally { rmSync(scratch, { recursive: true, force: true }); }
}
