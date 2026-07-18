#!/usr/bin/env node
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { basename, resolve } from "node:path";
import { execFileSync } from "node:child_process";
import { CASSETTE_SPECS } from "../lib/kunaki.mjs";

const args = Object.fromEntries(process.argv.slice(2).map((arg, i, all) => arg.startsWith("--") ? [arg.slice(2), all[i + 1]?.startsWith("--") ? true : all[i + 1]] : null).filter(Boolean));
const slug = args.slug || "physical-mail";
const title = args.title || "A Record in the Mail";
const subtitle = args.subtitle || "Aesthetic Computer · read by @jeffrey";
const cover = resolve(args.cover || `marketing/podcast/out/${slug}-cover.png`);
const audio = resolve(args.audio || `marketing/podcast/out/${slug}.mp3`);
const out = resolve(args.out || `marketing/podcast/out/${slug}-cassette`);
if (!existsSync(cover)) throw new Error(`Missing cover: ${cover}`);
if (!existsSync(audio)) throw new Error(`Missing audio: ${audio}`);
mkdirSync(out, { recursive: true });

const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&apos;" }[c]));
const svg = (width, height, side, extra = "") => `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"><rect width="100%" height="100%" fill="#fff9fc"/><rect y="0" width="20%" height="28" fill="#ff6b9d"/><rect x="20%" width="20%" height="28" fill="#4ecdc4"/><rect x="40%" width="20%" height="28" fill="#ffe66d"/><rect x="60%" width="20%" height="28" fill="#8ce99a"/><rect x="80%" width="20%" height="28" fill="#ff6b9d"/><text x="54" y="105" font-family="monospace" font-size="25" fill="#777">AESTHETIC.COMPUTER · CASSETTE ${side}</text><text x="54" y="190" font-family="sans-serif" font-size="54" font-weight="700" fill="#b44887">${esc(title)}</text><text x="54" y="242" font-family="monospace" font-size="24" fill="#282430">${esc(subtitle)}</text>${extra}<rect x="32" y="32" width="${width - 64}" height="${height - 64}" fill="none" stroke="#ff6b9d" stroke-width="3" stroke-dasharray="12 10" opacity=".35"/></svg>`;

const render = (name, spec, side, extra = "") => {
  const svgPath = resolve(out, `.${name}.svg`);
  const pngPath = resolve(out, `.${name}.png`);
  const jpgPath = resolve(out, `${name}.jpg`);
  writeFileSync(svgPath, svg(spec.width, spec.height, side, extra));
  execFileSync("rsvg-convert", ["--width", String(spec.width), "--height", String(spec.height), "--output", pngPath, svgPath]);
  execFileSync("magick", [pngPath, "-units", "PixelsPerInch", "-density", "300", "-quality", "95", jpgPath]);
  return jpgPath;
};

const jCard = resolve(out, "j-card.jpg");
execFileSync("magick", [cover, "-resize", "1200x1110^", "-gravity", "center", "-extent", "1200x1110", "-units", "PixelsPerInch", "-density", "300", "-quality", "95", jCard]);
const labelA = render("label-a", CASSETTE_SPECS.artwork.labelA, "SIDE A", `<text x="54" y="310" font-family="monospace" font-size="22" fill="#777">THE ESSAY + LISTENER MAIL INVITATION</text>`);
const labelB = render("label-b", CASSETTE_SPECS.artwork.labelB, "SIDE B", `<text x="54" y="310" font-family="monospace" font-size="22" fill="#777">LETTERS · mail@aesthetic.computer</text>`);

const duration = Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", audio], { encoding: "utf8" }).trim());
if (duration > CASSETTE_SPECS.audio.maxMinutesPerSide * 60) throw new Error(`Audio is ${(duration / 60).toFixed(1)} minutes; maximum is 40 minutes per side`);
const sideA = resolve(out, "side-a.wav");
execFileSync("ffmpeg", ["-y", "-i", audio, "-ar", "44100", "-ac", "2", sideA], { stdio: "ignore" });
const manifest = { vendor: "kunaki", productType: "cassette", title, sourceAudio: basename(audio), durationSeconds: duration, files: { sideA: basename(sideA), jCard: basename(jCard), labelA: basename(labelA), labelB: basename(labelB) }, specs: CASSETTE_SPECS, note: "Kunaki product creation remains a manual browser upload; the API begins after a product ID exists." };
writeFileSync(resolve(out, "manifest.json"), JSON.stringify(manifest, null, 2) + "\n");
writeFileSync(resolve(out, "README.md"), `# ${title} — Kunaki cassette kit\n\nUpload \`side-a.wav\` to side A. Leave side B silent until its program is chosen. Upload \`j-card.jpg\`, \`label-a.jpg\`, and \`label-b.jpg\` in Kunaki's browser product creator. After publishing, record the 10-character product ID in \`manifest.json\`; fulfillment can then use \`bin/kunaki.mjs\`.\n\nArtwork: JPEG, 300 DPI, no bleed. J-card 1200×1110; labels 1062×496. Keep important text clear of edges.\n`);
console.log(out);
