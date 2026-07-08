// whistlegraph glyphs — harvest the final frame of every cached video
//
//   node glyphs.mjs             # all cached mp4s not yet harvested
//   node glyphs.mjs --id <id>   # just one
//
// The last frame of a whistlegraph video is (usually) the finished
// drawing — the glyph. This walks downloads/video/*.mp4 (the cache that
// lyrics.mjs builds) and writes a 320px-wide jpg per video into
// downloads/glyphs/<id>.jpg, seeking 0.4s before the end. Some videos
// end on a title card or fade; those get fixed by hand with an earlier
// seek (see the site's glyph recipe — mommy-wow/puzzle/slinky-dog used
// -sseof -2). Resumable: existing jpgs are skipped.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const VIDEO = join(HERE, "downloads", "video");
const GLYPHS = join(HERE, "downloads", "glyphs");

mkdirSync(GLYPHS, { recursive: true });

const only = (() => {
  const i = process.argv.indexOf("--id");
  return i >= 0 ? process.argv[i + 1] : null;
})();

let mp4s = readdirSync(VIDEO).filter((f) => f.endsWith(".mp4"));
if (only) mp4s = mp4s.filter((f) => f.startsWith(only));

let made = 0;
let skipped = 0;
let failed = 0;
for (const file of mp4s) {
  const id = file.replace(/\.mp4$/, "");
  const jpg = join(GLYPHS, `${id}.jpg`);
  if (existsSync(jpg)) {
    skipped += 1;
    continue;
  }
  try {
    execFileSync(
      "ffmpeg",
      ["-y", "-sseof", "-0.4", "-i", join(VIDEO, file), "-frames:v", "1",
        "-update", "1", "-vf", "scale=320:-2", "-q:v", "4", jpg],
      { stdio: ["ignore", "ignore", "pipe"] },
    );
    made += 1;
  } catch {
    failed += 1;
    console.log(`glyph failed: ${id}`);
  }
}
console.log(`${made} glyphs harvested, ${skipped} already done, ${failed} failed → ${GLYPHS}`);
