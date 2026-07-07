#!/usr/bin/env node
// art.mjs — send a MacPal star's glyph art through the wire, and read it back.
//
// The star polls /api/macpal-art?to=<recipient> every ~45s; this sets the poses
// it downloads and hot-swaps. Admin-only for writes — uses @jeffrey's token.
// Companion to affirm.mjs (which carries the caption).
//
//   node macpal/art.mjs face.svg                      # → pose "glyph" (base), to=fia
//   node macpal/art.mjs --pose wave wave.svg          # add/replace a named pose
//   node macpal/art.mjs --pose glyph a.svg --pose glyph-2 b.svg   # several at once
//   node macpal/art.mjs --dir Resources/              # send every star-*.svg in a dir
//   node macpal/art.mjs --get                         # obtain poses (names + rev)
//   node macpal/art.mjs --get --out /tmp/star         # …and write each SVG to disk
//   node macpal/art.mjs --get --from device           # what the star reports rendering
//   node macpal/art.mjs --delete wave                 # remove a pose
//   node macpal/art.mjs --clear                       # wipe → star falls back to bundle art
//
// Poses: every name but "sing" is an idle-cycle frame (drawn in name order —
// glyph, glyph-2, glyph-3, …); "sing" is the open-mouth pose. A file named
// star-glyph-2.svg maps to pose "glyph-2", star-glyph-sing.svg to "sing".
//
// Auth: reads ~/.ac-token (run `node tezos/ac-login.mjs` once to create it).

import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const args = process.argv.slice(2);
const has = (n) => args.includes(n);
function flag(n) {
  const i = args.indexOf(n);
  return i !== -1 ? args[i + 1] : undefined;
}

const to = flag("--to") || "fia";
const local = has("--local");
const host = flag("--host") || (local ? "https://localhost:8888" : "https://aesthetic.computer");
const from = flag("--from"); // "device" reads the star's reported state

// Map a filename to its pose name: star-glyph-2.svg → glyph-2, …-sing → sing.
function poseFromFile(file) {
  const base = path.basename(file).replace(/\.svg$/i, "").replace(/^star-/, "");
  return base === "glyph-sing" ? "sing" : base;
}
const readSVG = (file) => fs.readFileSync(file, "utf8");

// ── assemble the poses to send (if any) ────────────────────────────────────
const poses = {};
// Repeated `--pose NAME FILE` pairs.
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--pose") {
    const name = args[i + 1];
    const file = args[i + 2];
    if (!name || !file) { console.error("✗ --pose needs NAME and FILE"); process.exit(1); }
    poses[name] = readSVG(file);
    i += 2;
  }
}
// A whole directory of star-*.svg.
if (has("--dir")) {
  const dir = flag("--dir");
  for (const f of fs.readdirSync(dir).filter((f) => /\.svg$/i.test(f))) {
    poses[poseFromFile(f)] = readSVG(path.join(dir, f));
  }
}
// A bare positional SVG file → the base "glyph" pose.
const valueFlags = ["--to", "--host", "--from", "--pose", "--dir", "--out", "--delete"];
const positionals = args.filter((a, i) => !a.startsWith("--") && !valueFlags.includes(args[i - 1]) && args[i - 2] !== "--pose");
if (positionals.length) {
  if (positionals.length > 1) { console.error("✗ pass one bare file (the base glyph), or use --pose NAME FILE / --dir"); process.exit(1); }
  poses["glyph"] = readSVG(positionals[0]);
}

const isRead = has("--get") || has("--list");
const isWrite = Object.keys(poses).length > 0 || has("--delete") || has("--clear");

if (!isRead && !isWrite) {
  console.error('usage: node macpal/art.mjs <file.svg> | --pose NAME FILE | --dir D | --get | --delete NAME | --clear  [--to fia]');
  process.exit(1);
}

// ── read path: obtain the poses (no auth needed) ───────────────────────────
if (isRead && !isWrite) {
  const wantBodies = has("--get") && !has("--list");
  const url = new URL(`${host}/api/macpal-art`);
  url.searchParams.set("to", to);
  if (from === "device") url.searchParams.set("from", "device");
  if (!wantBodies) url.searchParams.set("names", "1");
  if (local) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
  const res = await fetch(url);
  const out = await res.json().catch(() => ({}));
  if (!res.ok) { console.error(`✗ ${res.status} — ${out.message || "failed"}`); process.exit(1); }
  console.log(`${to} (${out.from}) — rev ${out.rev}${out.at ? `, set ${out.at}` : ""}`);
  console.log(`  poses: ${out.names?.length ? out.names.join(", ") : "(none — using bundle art)"}`);
  const outDir = flag("--out");
  if (wantBodies && outDir && out.poses) {
    fs.mkdirSync(outDir, { recursive: true });
    for (const [name, svg] of Object.entries(out.poses)) {
      const p = path.join(outDir, `${name}.svg`);
      fs.writeFileSync(p, svg);
      console.log(`  ↓ ${p}`);
    }
  }
  process.exit(0);
}

// ── write path: send poses / delete / clear (admin) ────────────────────────
const tokenPath = path.join(os.homedir(), ".ac-token");
let token;
try {
  token = JSON.parse(fs.readFileSync(tokenPath, "utf8")).access_token;
} catch {
  console.error("✗ No ~/.ac-token found. Log in first:  node tezos/ac-login.mjs");
  process.exit(1);
}
if (!token) { console.error("✗ ~/.ac-token has no access_token — re-run: node tezos/ac-login.mjs"); process.exit(1); }

let payload;
if (has("--clear")) {
  payload = { to, clear: true };
} else {
  const patch = { ...poses };
  if (has("--delete")) patch[flag("--delete")] = null;
  payload = { to, poses: patch };
}

if (local) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
const res = await fetch(`${host}/api/macpal-art`, {
  method: "POST",
  headers: { Authorization: `Bearer ${token}`, "Content-Type": "application/json" },
  body: JSON.stringify(payload),
});
const out = await res.json().catch(() => ({}));
if (!res.ok) {
  console.error(`✗ ${res.status} — ${out.message || "failed"}`);
  if (res.status === 401) console.error("  token expired? re-run: node tezos/ac-login.mjs");
  process.exit(1);
}
if (out.unchanged) {
  console.log(`· ${to} unchanged (rev ${out.rev})${out.rejected ? ` — ${out.rejected} rejected` : ""}  poses: ${out.names.join(", ") || "(none)"}`);
} else {
  console.log(`✓ ${to} ← rev ${out.rev}  poses: ${out.names.join(", ") || "(none — bundle art)"}`);
}
