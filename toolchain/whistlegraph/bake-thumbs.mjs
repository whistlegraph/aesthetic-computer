// whistlegraph.org index thumbnails — bake tighter, clearer 4:5 crops
//
//   node bake-thumbs.mjs                # download → back up → bake → upload
//   node bake-thumbs.mjs --bake-only    # everything except the upload
//   node bake-thumbs.mjs --restore      # copy index-original/ back over index/
//
// The site's tiny 64×80 thumbs load straight TikTok frames (320×568) from
// /whistlegraph/index/<asset||code>.jpg, and the drawn graph usually sits
// small and muddy somewhere in the lower half — CSS papers over it with
// object-position. This bakes the crop instead: sharp's attention strategy
// picks a 256×320 window (a 1.25× zoom into the frame — attention won a
// side-by-side against entropy and a hand-rolled edge-energy profile on
// eight diverse codes), upscaled to 320×400 (4:5), with a gentle contrast
// stretch, a touch of saturation, and a mild sharpen. Tuned by eye on
// contact sheets; stronger settings went neon on chalk and grass.
//
// True originals are backed up server-side to /whistlegraph/index-original/
// BEFORE any overwrite, and never re-backed-up — so a re-run can't clobber
// a real original with an already-baked image, and --restore undoes the
// whole pass in one command. Only .jpg files named by graphs.json works are
// touched; the .mp4s and the posts/ subfolder are left alone, as are the
// curated ten (they carry a slug and serve their art from /whistlegraph/
// <slug>/, not from index/).

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import sharp from "sharp";

const HERE = dirname(fileURLToPath(import.meta.url));
const GRAPHS = join(HERE, "..", "..", "system", "public", "whistlegraph.org", "graphs.json");
const ORIG_DIR = join(HERE, "downloads", "index-thumbs-original");
const BAKED_DIR = join(HERE, "downloads", "index-thumbs-baked");

const ENDPOINT = "https://sfo3.digitaloceanspaces.com";
const BUCKET = "s3://assets-aesthetic-computer";
const INDEX = "whistlegraph/index";
const BACKUP = "whistlegraph/index-original";

// Bake geometry + grade — the numbers the contact sheets settled on.
const OUT_W = 320, OUT_H = 400; // 4:5
const CROP_W = 256, CROP_H = 320; // attention window: 1.25× zoom into 320×568
const QUALITY = 85;

const args = process.argv.slice(2);
const RESTORE = args.includes("--restore");
const BAKE_ONLY = args.includes("--bake-only");

const aws = (...a) =>
  execFileSync("aws", [...a, "--endpoint-url", ENDPOINT], { encoding: "utf8" });

// Bucket listing → Set of bare names ("soda", not "soda.jpg") — one listing
// call per prefix instead of a 404 per code.
function listJpgs(prefix) {
  let out = "";
  try {
    out = aws("s3", "ls", `${BUCKET}/${prefix}/`);
  } catch (err) {
    // An empty prefix exits 1 with no output — that's a real (empty) answer,
    // not a failure. Anything with stderr is a genuine error.
    if (err.stderr?.trim() || err.stdout?.trim()) throw err;
  }
  return new Set(
    out
      .split("\n")
      .map((line) => line.trim().split(/\s+/).pop())
      .filter((name) => name?.endsWith(".jpg"))
      .map((name) => name.slice(0, -4)),
  );
}

// Every work's asset||code — the curated ten (slug) and the audio-only
// works (noGlyph) have no index/ thumb to bake.
const { works } = JSON.parse(readFileSync(GRAPHS, "utf8"));
const targets = [
  ...new Set(works.filter((w) => !w.slug && !w.noGlyph).map((w) => w.asset || w.code)),
];

const inIndex = listJpgs(INDEX);
const present = targets.filter((t) => inIndex.has(t));
const skipped = targets.filter((t) => !inIndex.has(t));
console.log(`${targets.length} targets — ${present.length} in bucket, ${skipped.length} skipped`);
if (skipped.length) console.log(`  skipped (no bucket jpg): ${skipped.join(", ")}`);

if (RESTORE) {
  // Reversal: server-side copy of every backed-up original over index/.
  const backedUp = listJpgs(BACKUP);
  let restored = 0;
  for (const name of present) {
    if (!backedUp.has(name)) continue;
    aws("s3", "cp", `${BUCKET}/${BACKUP}/${name}.jpg`, `${BUCKET}/${INDEX}/${name}.jpg`, "--acl", "public-read");
    restored++;
  }
  console.log(`restored ${restored} originals from ${BACKUP}/`);
  process.exit(0);
}

mkdirSync(ORIG_DIR, { recursive: true });
mkdirSync(BAKED_DIR, { recursive: true });

// 1. Download any original we don't already hold locally. The local copy
// doubles as a second backup tier and feeds the bake.
let downloaded = 0;
for (const name of present) {
  const local = join(ORIG_DIR, `${name}.jpg`);
  if (existsSync(local)) continue;
  aws("s3", "cp", `${BUCKET}/${INDEX}/${name}.jpg`, local, "--quiet");
  downloaded++;
}
console.log(`downloaded ${downloaded} originals (${present.length - downloaded} already local)`);

// 2. Back up to the bucket — server-side copy, and ONLY for names not
// already under index-original/: once the live jpg has been overwritten
// with a bake, re-copying it would destroy the true original forever.
const backedUp = listJpgs(BACKUP);
let backups = 0;
for (const name of present) {
  if (backedUp.has(name)) continue;
  aws("s3", "cp", `${BUCKET}/${INDEX}/${name}.jpg`, `${BUCKET}/${BACKUP}/${name}.jpg`);
  backups++;
}
console.log(`backed up ${backups} originals to ${BACKUP}/ (${backedUp.size} already there)`);

// 3. Bake.
let baked = 0;
for (const name of present) {
  await sharp(join(ORIG_DIR, `${name}.jpg`))
    .resize(CROP_W, CROP_H, { fit: "cover", position: sharp.strategy.attention })
    .resize(OUT_W, OUT_H)
    .normalise({ lower: 1, upper: 99 }) // percentile clamp: whiteboards keep their whites
    .modulate({ saturation: 1.15 })
    .sharpen({ sigma: 0.8 })
    .jpeg({ quality: QUALITY })
    .toFile(join(BAKED_DIR, `${name}.jpg`));
  baked++;
}
console.log(`baked ${baked} thumbs to ${BAKED_DIR}`);

if (BAKE_ONLY) {
  console.log("--bake-only: stopping before upload");
  process.exit(0);
}

// 4. Upload over the live names. Backups are verified above, so this is
// the only destructive step and it is already reversible.
let uploaded = 0;
for (const name of present) {
  aws("s3", "cp", join(BAKED_DIR, `${name}.jpg`), `${BUCKET}/${INDEX}/${name}.jpg`, "--acl", "public-read", "--quiet");
  uploaded++;
}
console.log(`uploaded ${uploaded} baked thumbs over ${INDEX}/`);
console.log("note: assets.aesthetic.computer sits behind Cloudflare + DO CDN with max-age=3600 — edges refresh within the hour.");
