// whistlegraph comments — the back-of-house comment room's intake and sorter
//
//   node comments.mjs add <file|dir>... [--code imab] [--post 69…] [--note "…"]
//                                       [--captured 2021-08-04] [--status keeper]
//   node comments.mjs list [--status unsorted] [--code imab]
//   node comments.mjs set <id>... [--status keeper|filed|unsorted|ditch]
//                                 [--code imab] [--post 69…] [--note "…"]
//                                 [--captured 2021-08-04]
//   node comments.mjs prune       # delete everything marked ditch, images too
//
// Alex's TikTok comment screenshots from over the years land here to get
// sorted (requested 2026-09-03). Images are content-hashed into
// system/public/whistlegraph.org/comments/img/<id>.<ext> so the same
// screenshot can never be archived twice, and the manifest at
// comments/comments.json is the single source of truth the /comments room
// renders. Statuses: unsorted → keeper (Alex might use it) or filed
// (sorted under a work code, archival) or ditch (prune removes it).

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import {
  existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync,
  writeFileSync,
} from "node:fs";
import { basename, dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOM = join(HERE, "..", "..", "system", "public", "whistlegraph.org", "comments");
const IMG = join(ROOM, "img");
const MANIFEST = join(ROOM, "comments.json");
const STATUSES = ["unsorted", "keeper", "filed", "ditch"];
const IMAGE_EXTS = new Set([".png", ".jpg", ".jpeg", ".webp", ".gif", ".heic", ".heif"]);

function readManifest() {
  if (!existsSync(MANIFEST)) {
    return {
      about: "Back-of-house archive of TikTok comment screenshots — sorted with toolchain/whistlegraph/comments.mjs, rendered at whistlegraph.org/comments.",
      updated: null,
      comments: [],
    };
  }
  return JSON.parse(readFileSync(MANIFEST, "utf8"));
}

function writeManifest(manifest) {
  manifest.updated = new Date().toISOString();
  mkdirSync(ROOM, { recursive: true });
  writeFileSync(MANIFEST, JSON.stringify(manifest, null, 2) + "\n");
}

// --flag value pairs from argv; everything else is a positional.
function parseArgs(argv) {
  const flags = {};
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    if (argv[i].startsWith("--")) flags[argv[i].slice(2)] = argv[++i] ?? "";
    else positional.push(argv[i]);
  }
  return { flags, positional };
}

function checkFlags(flags) {
  if (flags.status && !STATUSES.includes(flags.status)) {
    console.error(`status must be one of: ${STATUSES.join(", ")}`);
    process.exit(1);
  }
  if (flags.code && !/^[a-z0-9]{1,30}$/.test(flags.code)) {
    console.error("code must be a bare lowercase work code, e.g. imab");
    process.exit(1);
  }
  if (flags.post && !/^\d+$/.test(flags.post)) {
    console.error("post must be a numeric TikTok post id");
    process.exit(1);
  }
  if (flags.captured && !/^\d{4}(-\d{2}(-\d{2})?)?$/.test(flags.captured)) {
    console.error("captured must be YYYY, YYYY-MM, or YYYY-MM-DD");
    process.exit(1);
  }
}

function sips(args) {
  return execFileSync("sips", args, { stdio: ["ignore", "pipe", "ignore"] }).toString();
}

function dimensions(path) {
  try {
    const out = sips(["-g", "pixelWidth", "-g", "pixelHeight", path]);
    const w = Number(out.match(/pixelWidth: (\d+)/)?.[1]);
    const h = Number(out.match(/pixelHeight: (\d+)/)?.[1]);
    if (w && h) return { w, h };
  } catch {}
  return {};
}

function collectImages(paths) {
  const files = [];
  for (const given of paths) {
    const path = resolve(given);
    if (!existsSync(path)) {
      console.error(`not found: ${given}`);
      continue;
    }
    if (statSync(path).isDirectory()) {
      for (const name of readdirSync(path).sort()) {
        if (IMAGE_EXTS.has(extname(name).toLowerCase())) files.push(join(path, name));
      }
    } else if (IMAGE_EXTS.has(extname(path).toLowerCase())) {
      files.push(path);
    } else {
      console.error(`not an image: ${given}`);
    }
  }
  return files;
}

function add(positional, flags) {
  const files = collectImages(positional);
  if (!files.length) {
    console.error("nothing to add — pass screenshot files or a folder of them");
    process.exit(1);
  }
  const manifest = readManifest();
  const known = new Set(manifest.comments.map((c) => c.id));
  mkdirSync(IMG, { recursive: true });
  let added = 0;
  let dupes = 0;
  for (const file of files) {
    let source = file;
    let ext = extname(file).toLowerCase();
    if (ext === ".heic" || ext === ".heif") {
      // iPhone captures arrive HEIC; browsers can't show those, so bake a jpg.
      const jpg = join(IMG, `.convert-${basename(file)}.jpg`);
      try {
        sips(["-s", "format", "jpeg", file, "--out", jpg]);
        source = jpg;
        ext = ".jpg";
      } catch {
        console.error(`heic conversion failed (sips): ${basename(file)}`);
        continue;
      }
    }
    const bytes = readFileSync(source);
    const id = createHash("sha1").update(bytes).digest("hex").slice(0, 8);
    if (ext === ".jpeg") ext = ".jpg";
    const img = `img/${id}${ext}`;
    if (known.has(id)) {
      dupes += 1;
    } else {
      writeFileSync(join(ROOM, img), bytes);
      const entry = {
        id,
        img,
        ...dimensions(join(ROOM, img)),
        status: flags.status || "unsorted",
        added: new Date().toISOString().slice(0, 10),
      };
      if (flags.code) entry.code = flags.code;
      if (flags.post) entry.post = flags.post;
      if (flags.captured) entry.captured = flags.captured;
      if (flags.note) entry.note = flags.note;
      manifest.comments.push(entry);
      known.add(id);
      added += 1;
      console.log(`${id}  ${basename(file)}`);
    }
    if (source !== file) rmSync(source, { force: true });
  }
  if (added) writeManifest(manifest);
  console.log(`${added} archived, ${dupes} already in the room → ${MANIFEST}`);
}

function list(flags) {
  const manifest = readManifest();
  let rows = manifest.comments;
  if (flags.status) rows = rows.filter((c) => c.status === flags.status);
  if (flags.code) rows = rows.filter((c) => c.code === flags.code);
  for (const c of rows) {
    const code = c.code ? `[${c.code}]` : "";
    const bits = [c.id, c.status.padEnd(8), code.padEnd(8), c.captured || "", c.note || ""];
    console.log(bits.join("  ").trimEnd());
  }
  const counts = {};
  for (const c of manifest.comments) counts[c.status] = (counts[c.status] || 0) + 1;
  const tally = STATUSES.filter((s) => counts[s]).map((s) => `${counts[s]} ${s}`).join(", ");
  console.log(`${rows.length} shown of ${manifest.comments.length}${tally ? ` (${tally})` : ""}`);
}

function set(positional, flags) {
  if (!positional.length) {
    console.error("pass one or more ids from `comments.mjs list`");
    process.exit(1);
  }
  const fields = ["status", "code", "post", "captured", "note"];
  if (!fields.some((f) => f in flags)) {
    console.error(`pass at least one of: ${fields.map((f) => `--${f}`).join(" ")}`);
    process.exit(1);
  }
  const manifest = readManifest();
  const byId = new Map(manifest.comments.map((c) => [c.id, c]));
  let changed = 0;
  for (const id of positional) {
    const entry = byId.get(id);
    if (!entry) {
      console.error(`no such comment: ${id}`);
      continue;
    }
    for (const field of fields) {
      if (!(field in flags)) continue;
      if (flags[field] === "") delete entry[field];
      else entry[field] = flags[field];
    }
    if (!entry.status) entry.status = "unsorted";
    // Filing under a code without saying so is the common gesture; honor it.
    if (flags.code && !("status" in flags) && entry.status === "unsorted") {
      entry.status = "filed";
    }
    changed += 1;
  }
  if (changed) writeManifest(manifest);
  console.log(`${changed} updated`);
}

function prune() {
  const manifest = readManifest();
  const keep = [];
  let dropped = 0;
  for (const c of manifest.comments) {
    if (c.status === "ditch") {
      rmSync(join(ROOM, c.img), { force: true });
      dropped += 1;
    } else {
      keep.push(c);
    }
  }
  manifest.comments = keep;
  if (dropped) writeManifest(manifest);
  console.log(`${dropped} ditched, ${keep.length} remain`);
}

const [command, ...rest] = process.argv.slice(2);
const { flags, positional } = parseArgs(rest);
checkFlags(flags);
if (command === "add") add(positional, flags);
else if (command === "list") list(flags);
else if (command === "set") set(positional, flags);
else if (command === "prune") prune();
else {
  console.log("usage: node comments.mjs add|list|set|prune  (see header comment)");
  process.exit(command ? 1 : 0);
}
