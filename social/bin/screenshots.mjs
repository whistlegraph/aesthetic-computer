#!/usr/bin/env node
// Screenshots, posted on their anniversaries.
//
// The collection is 90 dated photographs of Aesthetic Computer being written
// (2021-12-05 to 2024-03-13). Each one goes out on the calendar day it was
// taken, so the schedule is the work's own dates and nothing is decided by hand.
//
//   node social/bin/screenshots.mjs plan [days]   what comes up in the next N days (default 45)
//   node social/bin/screenshots.mjs today [date]  prepare the post(s) for today, or for YYYY-MM-DD
//   node social/bin/screenshots.mjs show <id>     prepare one token's post regardless of date
//   node social/bin/screenshots.mjs done <id>     record that a token's post went out
//
// `today` and `show` write a post-ready JPEG (under Bluesky's 1 MB limit) to a
// temp folder and print its path with the Bluesky text, the X text and the alt
// text. This script posts nothing: hand those to the x MCP (bluesky_post,
// x_post), which wants a human yes.

import { readFileSync, writeFileSync, existsSync, mkdirSync, statSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

const DIR = join(dirname(fileURLToPath(import.meta.url)), "../screenshots");
const CONTRACT = "0x651c6dC799864004A9Df503db6A570c4c1CeC743";
const tokens = JSON.parse(readFileSync(join(DIR, "catalog.json"), "utf8"));
const postedFile = join(DIR, "posted.json");
const posted = existsSync(postedFile) ? JSON.parse(readFileSync(postedFile, "utf8")) : {};

// "May 4, 2023 at 08:59 PM" → Date (local wall time; only month and day matter here).
const taken = (t) => new Date(t.title.replace(" at ", " "));
const monthDay = (d) => `${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
const link = (t) => `https://opensea.io/item/ethereum/${CONTRACT.toLowerCase()}/${t.id}`;

// Days from `from` until the token's next anniversary (0 = today).
function daysUntil(t, from) {
  const d = taken(t);
  const next = new Date(from.getFullYear(), d.getMonth(), d.getDate());
  const start = new Date(from.getFullYear(), from.getMonth(), from.getDate());
  if (next < start) next.setFullYear(next.getFullYear() + 1);
  return Math.round((next - start) / 864e5);
}

function texts(t) {
  const body = `${t.title}\n${t.place}\n\nScreenshots ${t.id}/90`;
  return {
    bluesky: body, // the work, no price
    x: `${body}\n${link(t)}`, // the token page carries the current price
    alt: `A photograph of a laptop screen showing Aesthetic Computer being written, with the room around it. ${t.place}.`,
  };
}

// Fetch the original and shrink it until it fits Bluesky's 1 MB image limit.
function image(t) {
  const out = join(tmpdir(), "ac-screenshots"); mkdirSync(out, { recursive: true });
  const src = join(out, `${t.id}.src`), jpg = join(out, `${t.id}.jpg`);
  if (!existsSync(src)) execFileSync("curl", ["-sL", "--max-time", "120", "-o", src, t.image]);
  for (const width of [2000, 1600, 1200]) {
    execFileSync("ffmpeg", ["-loglevel", "error", "-y", "-i", src, "-vf", `scale='min(${width},iw)':-2`, "-q:v", "3", jpg]);
    if (statSync(jpg).size < 950_000) break;
  }
  return jpg;
}

function prepare(t) {
  const x = texts(t);
  console.log(`\n#${t.id}${posted[t.id] ? `  (already posted ${posted[t.id]})` : ""}`);
  console.log(`image:   ${image(t)}`);
  console.log(`alt:     ${x.alt}`);
  console.log(`\n-- bluesky --\n${x.bluesky}\n\n-- x --\n${x.x}\n`);
}

const [cmd, arg] = process.argv.slice(2);
const now = arg && /^\d{4}-\d{2}-\d{2}$/.test(arg) ? new Date(arg + "T12:00:00") : new Date();

if (cmd === "plan") {
  const days = Number(arg) || 45;
  const rows = tokens.map((t) => ({ t, in: daysUntil(t, now) })).filter((r) => r.in <= days).sort((a, b) => a.in - b.in);
  for (const r of rows) {
    const when = new Date(now.getFullYear(), now.getMonth(), now.getDate() + r.in);
    console.log(`${when.toISOString().slice(0, 10)}  #${String(r.t.id).padEnd(3)} ${r.t.title} — ${r.t.place}${posted[r.t.id] ? "  [posted]" : ""}`);
  }
  console.log(`\n${rows.length} posts in the next ${days} days.`);
} else if (cmd === "today") {
  const due = tokens.filter((t) => monthDay(taken(t)) === monthDay(now));
  if (!due.length) console.log(`Nothing was taken on ${monthDay(now)}. Next:`), console.log(
    tokens.map((t) => ({ t, in: daysUntil(t, now) })).sort((a, b) => a.in - b.in).slice(0, 3)
      .map((r) => `  in ${r.in} days: #${r.t.id} ${r.t.title} — ${r.t.place}`).join("\n"));
  for (const t of due) prepare(t);
} else if (cmd === "show") {
  const t = tokens.find((x) => x.id === Number(arg));
  if (!t) { console.error(`No token ${arg}`); process.exit(1); }
  prepare(t);
} else if (cmd === "done") {
  if (!tokens.some((x) => x.id === Number(arg))) { console.error(`No token ${arg}`); process.exit(1); }
  posted[Number(arg)] = new Date().toISOString().slice(0, 10);
  writeFileSync(postedFile, JSON.stringify(posted, null, 2) + "\n");
  console.log(`Recorded #${arg} as posted.`);
} else {
  console.log("usage: screenshots.mjs plan [days] | today [YYYY-MM-DD] | show <id> | done <id>");
}
