#!/usr/bin/env node
// affirm.mjs — push an affirmation to a MacPal star.
//
// The star (macpal, default profile) polls /api/macpal-status?to=<recipient>
// every ~45s; this sets that message. Admin-only — uses @jeffrey's login token.
//
//   node macpal/affirm.mjs "proud of you 💛"            # → to=fia, production
//   node macpal/affirm.mjs "great work today ✨" --to fia
//   node macpal/affirm.mjs "testing" --local            # → https://localhost:8888
//   node macpal/affirm.mjs --clear --to fia              # blank the caption
//
// The chime that plays when the message changes is programmable — a named macOS
// system sound (Glass, Tink, Pop, Bottle, Hero, Submarine, Ping…) at --volume 0–1:
//
//   node macpal/affirm.mjs "ta-da ✨" --sound Hero --volume 0.5
//
// Playlists rotate server-side (survives this machine sleeping) — each quoted
// argument is one message, cycling every --every seconds (default 120):
//
//   node macpal/affirm.mjs --playlist "it's good to be calm!" "i love you!" --every 60
//
// Auth: reads ~/.ac-token (run `node tezos/ac-login.mjs` once to create it).

import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const args = process.argv.slice(2);
function flag(name) {
  const i = args.indexOf(name);
  return i !== -1 ? args[i + 1] : undefined;
}
const has = (name) => args.includes(name);

const to = flag("--to") || "fia";
const local = has("--local");
const playlist = has("--playlist");
const every = Number(flag("--every")) || 120;
const sound = flag("--sound");
const volume = flag("--volume");
const host = flag("--host") || (local ? "https://localhost:8888" : "https://aesthetic.computer");
const valueFlags = ["--to", "--host", "--every", "--sound", "--volume"];
const words = args.filter((a, i) => !a.startsWith("--") && !valueFlags.includes(args[i - 1]));
// One message is every non-flag argument joined (so quotes are optional-ish);
// a --playlist keeps each quoted argument as its own message.
const text = has("--clear") ? "" : words.join(" ");

if (playlist && words.length < 2) {
  console.error('usage: node macpal/affirm.mjs --playlist "msg a" "msg b" [...] [--every 120] [--to fia]');
  process.exit(1);
}
if (!playlist && !has("--clear") && !text) {
  console.error('usage: node macpal/affirm.mjs "your message" [--to fia] [--local] [--clear]');
  process.exit(1);
}

const tokenPath = path.join(os.homedir(), ".ac-token");
let token;
try {
  token = JSON.parse(fs.readFileSync(tokenPath, "utf8")).access_token;
} catch {
  console.error(`✗ No ~/.ac-token found. Log in first:  node tezos/ac-login.mjs`);
  process.exit(1);
}
if (!token) {
  console.error("✗ ~/.ac-token has no access_token — re-run: node tezos/ac-login.mjs");
  process.exit(1);
}

// The dev site serves a self-signed cert on localhost; trust it only there.
if (local) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

const payload = playlist ? { to, playlist: words, every } : { to, text };
if (sound !== undefined) payload.sound = sound;
if (volume !== undefined) payload.volume = Number(volume);

const res = await fetch(`${host}/api/macpal-status`, {
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
const chime = out.sound ? `  🔔 ${out.sound}${out.volume != null ? ` @${out.volume}` : ""}` : "";
if (out.playlist) {
  console.log(`✓ ${to} ← playlist [${out.playlist.map((m) => `"${m}"`).join(" → ")}] every ${out.every}s  (seq ${out.seq})${chime}`);
} else {
  console.log(`✓ ${to} ← "${out.text}"  (seq ${out.seq})${chime}`);
}
