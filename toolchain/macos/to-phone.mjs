#!/usr/bin/env node
// to-phone.mjs — push files from this Mac to @jeffrey's iPhone with zero taps,
// riding the shared iCloud account (both devices on the same Apple ID).
//
// Two rails, picked automatically per file:
//   • photos & videos → imported into Photos → iCloud Photos syncs them to the
//     iPhone camera roll (ready to post to Instagram, etc.)
//   • everything else → copied into iCloud Drive → shows up in the Files app
//
// No AirDrop, no accept prompt, no drag. Sync lands in a few seconds once
// iCloud pushes it.
//
// Usage:
//   node toolchain/macos/to-phone.mjs <file...>
//   node toolchain/macos/to-phone.mjs --album "pals" *.png     # into a Photos album
//   node toolchain/macos/to-phone.mjs --drive report.pdf a.zip # force iCloud Drive
//   node toolchain/macos/to-phone.mjs --folder pals doc.pdf    # Drive subfolder name
//
// Flags:
//   --photos        force every file through Photos (camera roll)
//   --drive         force every file into iCloud Drive (Files app)
//   --album NAME    Photos: import into this album (created if missing)
//   --folder NAME   iCloud Drive: subfolder to drop into (default "to-phone")
//
// macOS only. Needs iCloud Photos on (for the camera-roll rail) and iCloud
// Drive on (for the Files rail) — both already enabled on this host.

import { existsSync, mkdirSync, copyFileSync, statSync } from "node:fs";
import { resolve, basename, extname, join } from "node:path";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

if (process.platform !== "darwin") {
  console.error("✗ to-phone.mjs is macOS-only (needs Photos + iCloud).");
  process.exit(1);
}

// ── args ──────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flag = (k) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : null; };
const has = (k) => argv.includes(`--${k}`);
const FORCE_PHOTOS = has("photos");
const FORCE_DRIVE = has("drive");
const ALBUM = flag("album");
const FOLDER = flag("folder") || "to-phone";

// drop the flag values + flag names, keep bare paths
const consumed = new Set();
for (const k of ["album", "folder"]) { const i = argv.indexOf(`--${k}`); if (i >= 0) { consumed.add(i); consumed.add(i + 1); } }
const files = argv
  .map((a, i) => ({ a, i }))
  .filter(({ a, i }) => !a.startsWith("--") && !consumed.has(i))
  .map(({ a }) => resolve(a))
  .filter((p) => { if (!existsSync(p)) { console.warn(`  · skip (not found): ${p}`); return false; } return true; });

if (!files.length) { console.error("✗ no files. Usage: to-phone <file...> [--album NAME] [--drive]"); process.exit(1); }

const MEDIA = new Set([".png", ".jpg", ".jpeg", ".heic", ".heif", ".gif", ".tif", ".tiff", ".webp",
  ".mov", ".mp4", ".m4v", ".hevc"]);
const isMedia = (p) => MEDIA.has(extname(p).toLowerCase());

// route each file
const toPhotos = [], toDrive = [];
for (const p of files) {
  if (FORCE_DRIVE) toDrive.push(p);
  else if (FORCE_PHOTOS || isMedia(p)) toPhotos.push(p);
  else toDrive.push(p);
}

// ── Photos rail: import → iCloud Photos → iPhone camera roll ────────────
function importToPhotos(paths, album) {
  const list = paths.map((p) => `POSIX file ${JSON.stringify(p)}`).join(", ");
  const albumBlock = album
    ? `set tgt to missing value
       repeat with al in albums
         if name of al is ${JSON.stringify(album)} then set tgt to al
       end repeat
       if tgt is missing value then set tgt to (make new album named ${JSON.stringify(album)})
       import theItems into tgt with skip check duplicates`
    : `import theItems with skip check duplicates`;
  const osa = `tell application "Photos"
      set theItems to {${list}}
      ${albumBlock}
    end tell`;
  const r = spawnSync("osascript", ["-e", osa], { encoding: "utf8" });
  if (r.status !== 0) throw new Error((r.stderr || "osascript failed").trim());
  return r.stdout.trim();
}

// ── iCloud Drive rail: copy into CloudDocs → iPhone Files app ───────────
function copyToDrive(paths, folder) {
  const base = join(homedir(), "Library", "Mobile Documents", "com~apple~CloudDocs");
  if (!existsSync(base)) throw new Error("iCloud Drive not found (is iCloud Drive on?)");
  const dest = join(base, folder);
  mkdirSync(dest, { recursive: true });
  for (const p of paths) copyFileSync(p, join(dest, basename(p)));
  return dest;
}

// ── run ────────────────────────────────────────────────────────────────
console.log(`\n📲 to-phone → ${files.length} file(s)\n`);
let ok = 0;

if (toPhotos.length) {
  const bytes = toPhotos.reduce((n, p) => n + statSync(p).size, 0);
  process.stdout.write(`  → Photos (camera roll)${ALBUM ? ` · album "${ALBUM}"` : ""}: ${toPhotos.length} item(s), ${(bytes / 1e6).toFixed(1)} MB …`);
  try { importToPhotos(toPhotos, ALBUM); process.stdout.write(" ✓\n"); ok += toPhotos.length; }
  catch (e) { process.stdout.write(" ✗\n"); console.error(`    ${e.message}`); }
}

if (toDrive.length) {
  process.stdout.write(`  → iCloud Drive (Files app) · /${FOLDER}: ${toDrive.length} file(s) …`);
  try { const d = copyToDrive(toDrive, FOLDER); process.stdout.write(" ✓\n"); ok += toDrive.length; console.log(`    ${d}`); }
  catch (e) { process.stdout.write(" ✗\n"); console.error(`    ${e.message}`); }
}

console.log(`\n✓ ${ok}/${files.length} handed to iCloud — they land on the iPhone in a few seconds.`);
if (toPhotos.length) console.log(`  Photos → open the Photos app (Recents)${ALBUM ? ` or the "${ALBUM}" album` : ""} on the phone.`);
if (toDrive.length) console.log(`  Files → Files app ▸ iCloud Drive ▸ ${FOLDER}.`);
