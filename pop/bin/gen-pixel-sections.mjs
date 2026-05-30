#!/usr/bin/env node
// gen-pixel-sections.mjs — deterministic pixel-art pass over pop panels.
//
// Down-samples each per-section illy PNG to a small canvas (default
// 192×288) and re-maps it to a 24-colour AC-native palette with
// Floyd–Steinberg dither. Outputs land beside the source as
// `<slug>-p-sec-<NN>-<id>.pixel.png` and are mirrored into
// `system/public/assets/pop/<slug>/sec-<N>.pixel.png` so the asset
// pipeline (`npm run pop:assets:up`) can sync them to DigitalOcean.
//
// Why deterministic (not AI re-gen): the photo-real panels are locked,
// on-model, and ship continuity (jeffrey's gear, butterfly, PALS arc).
// ImageMagick is already a repo dep, runs offline, costs nothing, and
// the output is byte-reproducible from the same source + same palette.
//
// Usage:
//   node pop/bin/gen-pixel-sections.mjs --lane hellsine --slug hellsine
//   node pop/bin/gen-pixel-sections.mjs --lane marimba --slug marimbaba --force
//   node pop/bin/gen-pixel-sections.mjs --lane hellsine --slug hellsine --only 0,1,2
//   node pop/bin/gen-pixel-sections.mjs --lane hellsine --slug hellsine --write-manifest

import { existsSync, mkdirSync, readdirSync, statSync, copyFileSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");
const REPO = resolve(POP, "..");

// ── arg parsing ──────────────────────────────────────────────────────
const args = {};
{
  const a = process.argv.slice(2);
  for (let i = 0; i < a.length; i++) {
    const k = a[i];
    if (!k.startsWith("--")) continue;
    const key = k.slice(2);
    const v = (a[i + 1] && !a[i + 1].startsWith("--")) ? a[++i] : "true";
    args[key] = v;
  }
}
const LANE = args.lane;
const SLUG = args.slug;
const FORCE = args.force === "true";
const WRITE_MANIFEST = args["write-manifest"] === "true";
const ONLY = args.only ? new Set(args.only.split(",").map(s => s.trim())) : null;
const SIZE = args.size || "192x288";
const PALETTE = args.palette
  ? resolve(args.palette)
  : `${POP}/lib/pixel-art/ac24.png`;

if (!LANE || !SLUG) {
  console.error("usage: node pop/bin/gen-pixel-sections.mjs --lane <name> --slug <name> [--force] [--only 0,1] [--palette <path>] [--size 192x288] [--write-manifest]");
  process.exit(1);
}

const OUT = `${POP}/${LANE}/out`;
const MIRROR = `${REPO}/system/public/assets/pop/${SLUG}`;

if (!existsSync(OUT)) {
  console.error(`✗ no out dir for lane '${LANE}': ${OUT}`);
  process.exit(1);
}

// ── 24-colour AC-native palette ──────────────────────────────────────
// Mixes: AC UI cream/coral/sage, hellsine lava arc, PALS rainbow, and
// 8 neutrals. Logic per slot is documented inline so the palette can be
// retuned by editing AC24_HEX in one place.
const AC24_HEX = [
  // AC UI cream / coral / sage (4)
  "#fff8e8", // cream — AC default page bg
  "#ffd6c2", // coral pink — AC accent
  "#ff7a5a", // hot coral — AC button hover
  "#9ac2a8", // sage — AC chip
  // hellsine lava arc — warm fire (5)
  "#ffb24a", // sulphur-yellow / amber kindling
  "#ff8a3c", // lava orange (TITLE_PALETTE mid)
  "#ff6a1f", // deep lava (TITLE_PALETTE low)
  "#dc4834", // red-alarm flare (statement-a)
  "#7a2a18", // obsidian-ember dark warm
  // hellsine cool / cosmic (4)
  "#3c4876", // dusk indigo (overture-a)
  "#7452a8", // electric violet (bridge-a)
  "#4c8cc8", // cyan plunge (bridge-c)
  "#60a8c4", // pale aqua (bridge-d)
  // PALS rainbow (7)
  "#22e0ff", // PALS cyan
  "#ff3cb8", // PALS magenta / hot-pink
  "#b8ff3c", // PALS lime
  "#ffc83c", // PALS gold
  "#ff7a1f", // PALS electric-orange
  "#6a1fff", // PALS deep-violet
  "#1f1fff", // PALS electric-blue
  // 8 neutrals — black → near-white plus 3 warm darks (8 total slots,
  // but we only have 4 left after 16 chroma; pack the warm darks
  // through the chroma allocations above and keep these grey-only)
  "#000000", // pure black
  "#3a3a3e", // dark grey (warm-leaning)
  "#8a8a8e", // mid grey
  "#e2e2dc", // near-white (cream-leaning)
];
if (AC24_HEX.length !== 24) {
  console.error(`✗ palette must be 24 colours, got ${AC24_HEX.length}`);
  process.exit(1);
}

function generatePalette() {
  mkdirSync(dirname(PALETTE), { recursive: true });
  // Build a 24x1 indexed PNG by drawing one 1px column per colour.
  // `magick -size 24x1 xc:none` then `-draw "fill #xxx point X,0"` 24x.
  const drawCmds = AC24_HEX.flatMap((hex, i) =>
    ["-fill", hex, "-draw", `point ${i},0`]
  );
  const argv = [
    "-size", "24x1", "xc:none",
    ...drawCmds,
    "PNG8:" + PALETTE,
  ];
  const r = spawnSync("magick", argv, { stdio: ["ignore", "pipe", "inherit"] });
  if (r.status !== 0) {
    console.error("✗ magick palette generation failed");
    process.exit(1);
  }
  console.log(`▸ wrote palette → ${PALETTE.replace(REPO + "/", "")}  (24 colours)`);
}

if (!existsSync(PALETTE) || FORCE) {
  generatePalette();
} else {
  console.log(`▸ using existing palette ${PALETTE.replace(REPO + "/", "")}`);
}

// ── source discovery ────────────────────────────────────────────────
// Match both `-sec-0-` (single-digit, marimba/dance) and `-sec-00-`
// (two-digit padded, hellsine) using a single regex.
const SEC_RE = new RegExp(`^${SLUG}-p-sec-(\\d{1,2})-([^.]+?)\\.png$`);
const allNames = readdirSync(OUT);
const sources = [];
for (const n of allNames) {
  const m = n.match(SEC_RE);
  if (!m) continue;
  const idx = parseInt(m[1], 10);
  const id = m[2];
  if (ONLY && !ONLY.has(String(idx))) continue;
  sources.push({ name: n, idx, id, pad: m[1].length });
}
sources.sort((a, b) => a.idx - b.idx);

if (sources.length === 0) {
  console.error(`✗ no panels matched ${SLUG}-p-sec-NN-id.png in ${OUT}`);
  process.exit(1);
}

console.log(`▸ ${sources.length} source panel(s) found in ${OUT.replace(REPO + "/", "")}`);
mkdirSync(MIRROR, { recursive: true });

// ── optional progress heartbeat ─────────────────────────────────────
let progress = null;
try {
  progress = await import("../lib/render-progress.mjs");
  progress.begin({ type: "illy", label: `${SLUG} pixel` });
} catch { /* optional */ }

// ── per-panel pass ──────────────────────────────────────────────────
const t0 = Date.now();
let made = 0, skipped = 0, mirrored = 0;
const sizes = [];

for (let i = 0; i < sources.length; i++) {
  const { name, idx, id } = sources[i];
  const src = `${OUT}/${name}`;
  // Output name preserves the original two-digit (or one-digit) form
  // so it sorts the same way as the source.
  const padded = String(idx).padStart(sources[0].pad, "0");
  const outName = `${SLUG}-p-sec-${padded}-${id}.pixel.png`;
  const out = `${OUT}/${outName}`;
  const mirrorOut = `${MIRROR}/sec-${idx}.pixel.png`;

  const srcStat = statSync(src);
  const upToDate =
    !FORCE &&
    existsSync(out) && existsSync(mirrorOut) &&
    statSync(out).mtimeMs >= srcStat.mtimeMs &&
    statSync(mirrorOut).mtimeMs >= srcStat.mtimeMs;

  if (upToDate) {
    skipped++;
    sizes.push(statSync(out).size);
    console.log(`  · sec-${idx} ${id}  (cached)`);
  } else {
    // Down-sample → Floyd–Steinberg → remap to 24-colour palette.
    // -filter Box: blocky downsample, retains pixel-art look at low res
    // PNG8:    : forces indexed-colour PNG, the smallest encoding
    const argv = [
      src,
      "-filter", "Box",
      "-resize", `${SIZE}!`,
      "-dither", "FloydSteinberg",
      "-remap", PALETTE,
      "PNG8:" + out,
    ];
    const r = spawnSync("magick", argv, { stdio: ["ignore", "pipe", "inherit"] });
    if (r.status !== 0) {
      console.error(`✗ magick failed on ${name}`);
      process.exit(1);
    }
    copyFileSync(out, mirrorOut);
    made++;
    const kb = statSync(out).size;
    sizes.push(kb);
    console.log(`  + sec-${idx} ${id}  (${(kb / 1024).toFixed(1)} KB)`);
  }
  if (existsSync(mirrorOut)) mirrored++;
  if (progress) progress.update(((i + 1) / sources.length) * 100, { done: i + 1, total: sources.length });
}

const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
const avgKb = sizes.length ? (sizes.reduce((s, x) => s + x, 0) / sizes.length / 1024).toFixed(1) : "0";
console.log(`✓ ${made} generated, ${skipped} cached, ${mirrored} mirrored  ·  avg ${avgKb} KB  ·  ${elapsed}s`);

// ── optional manifest variant ───────────────────────────────────────
if (WRITE_MANIFEST) {
  const manifest = `${REPO}/system/public/aesthetic.computer/disks/pop/${SLUG}.json`;
  if (!existsSync(manifest)) {
    console.log(`▸ manifest skipped: ${manifest.replace(REPO + "/", "")} does not exist`);
  } else {
    const json = readFileSync(manifest, "utf8");
    // Regex-replace any /assets/pop/<slug>/sec-N.<ext> illy field to .pixel.png
    const replaced = json.replace(
      new RegExp(`(/assets/pop/${SLUG}/sec-\\d+)\\.(jpg|jpeg|png)`, "g"),
      "$1.pixel.png"
    );
    const variant = manifest.replace(/\.json$/, ".pixel.json");
    writeFileSync(variant, replaced);
    console.log(`▸ wrote manifest variant → ${variant.replace(REPO + "/", "")}`);
  }
}

if (progress) progress.end();
