#!/usr/bin/env node
// Download the selected takes from REELS-BACKLOG.md into a flat, AirDrop-ready
// folder. Filenames sort by title and carry the code, date, views, and post id,
// so the folder stays readable on a phone with no manifest open.
//
//   node toolchain/whistlegraph/reels-fetch.mjs ~/Desktop/"Whistlegraph Reels"

import { readFileSync, writeFileSync, mkdirSync, existsSync, statSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));
const POSTS = resolve(here, "../../system/public/whistlegraph.org/posts.json");

// [post id, title, code, note]. Order follows REELS-BACKLOG.md: exact takes
// Alex named first, then the provisional top-viewed takes.
const PICKS = [
  ["7144610877633776938", "Blown Up Blue Balloon", "bubb", "Alex: “pretty good one for the reposting backlog”"],
  ["7143830552666246443", "Blown Up Blue Balloon", "bubb", "Alternate take"],
  ["7213456526663568683", "Double Heart Balloon", "ballo", "Alex: “This one's advanced”"],
  ["7214261543750405418", "Double Heart Balloon", "ballo", "Alternate take"],
  ["6925092114304961797", "Scared of Stairs", "sos", "Alex: “We gotta try posting this classic” — the top [sos] video"],
  ["6897724978821680389", "Battle Between Smiley Faces", "smil", "Alex named this exact deconstructed take"],
  ["7072997790468640043", "Circumference Divided By Diameter", "pi", "Sole take; one of the Shorts-bought videos"],
  ["7044242994622598446", "Friction Pad Erase", "frpd", "Sole take; the old Pilot Pen ad"],
  ["6945376059697270022", "Circle Line Triangle Hand", "clth", "Sole take"],
  ["7054371039035297070", "Bum Doo Doo", "bmdo", "Sole take"],
  ["7160197103426325802", "That Isn't Me", "znme", "Sole take; carries the “Us Kidnapping You” plot"],
  ["6930792379267992838", "The Effort Behind A Whistlegraph", "talk", "Talk post; Alex: “This is a good video.”"],
  ["7138355225550654762", "Headache Splitting", "head", "Top of four takes — pick unconfirmed"],
  ["7152762372862594346", "I'm a Ghost", "ghst", "Top of eleven takes — pick unconfirmed"],
  ["7199367653008428334", "Hey There, Apple", "appl", "Top of twenty-three takes — pick unconfirmed"],
  ["6990264500985302277", "Mommy Wow", "w0w", "Top take — Alex wants “the second take with Adam's beat”, confirm which"],
  ["7005122213166730502", "Mommy Wow", "w0w", "Second by views — likely the Adam's-beat take, confirm"],
  ["6893660463901461765", "One Plus Two Equals Three", "1p2", "Top of three — Alex: “try a few of the Artsy Math posts”"],
  ["6928955080364281093", "What's Inside Your Heart?", "wiyh", "Top of thirty-three — retry with comments enabled"],
  ["6801685115446627590", "Time To Grow", "grow", "Top of seventeen — Alex prefers the yellow-paper take, confirm this is it"],
  ["6912600718113393926", "Divided By Zero", "div0", "Sole take"],
  ["6945129772385062149", "Draw A Triangle Bug", "trib", "Sole take"],
  ["6908626648812276997", "Switch Switch Switch", "swch", "Top of three — pick unconfirmed"],
  ["6936780382176070918", "Fingers in the Paint", "fitp", "Top of three — pick unconfirmed"],
  ["6894914332245708037", "Camille and Alex", "cmal", "Top of five merged takes — pick unconfirmed"],
  ["6912777559335849221", "Blow My Pen", "bpen", "Top of three — pick unconfirmed"],
];

const out = process.argv[2];
if (!out) {
  console.error("usage: reels-fetch.mjs <outdir>");
  process.exit(1);
}
mkdirSync(out, { recursive: true });

const raw = JSON.parse(readFileSync(POSTS, "utf8"));
const posts = Array.isArray(raw) ? raw : raw.posts ?? Object.values(raw).find(Array.isArray);
const byId = new Map(posts.map((post) => [post.id, post]));

const views = (n) =>
  n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : n >= 1e3 ? `${Math.round(n / 1e3)}K` : String(n);
const safe = (s) => s.replace(/[/:]/g, "-").replaceAll('"', "");

const manifest = [];
for (const [id, title, code, note] of PICKS) {
  const post = byId.get(id);
  if (!post) {
    console.error(`missing post ${id} (${title})`);
    continue;
  }
  const file = `${safe(title)} [${code}] ${post.date} ${views(post.views)} ${id}.mp4`;
  const dest = `${out}/${file}`;
  if (!existsSync(dest)) {
    try {
      execFileSync("curl", ["-sfL", "--max-time", "120", "-o", dest, post.src]);
    } catch {
      console.error(`failed ${id} ${post.src}`);
      continue;
    }
  }
  const bytes = statSync(dest).size;
  manifest.push({
    id, title, code, note, file, bytes,
    date: post.date, views: post.views, duration: post.duration,
    caption: post.desc, tiktok: post.url,
  });
  console.log(`${(bytes / 1e6).toFixed(1)}MB  ${file}`);
}

writeFileSync(`${out}/manifest.json`, JSON.stringify(manifest, null, 2));
const total = manifest.reduce((sum, m) => sum + m.bytes, 0);
console.log(`\n${manifest.length} files, ${(total / 1e6).toFixed(0)}MB`);
