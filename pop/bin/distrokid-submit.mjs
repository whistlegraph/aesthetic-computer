#!/usr/bin/env node
// pop/bin/distrokid-submit.mjs
//
// Drives the DistroKid web upload form from a release.json — fills every
// field, uploads the audio + cover art — then STOPS at the review screen.
// You eyeball it and click the final release button yourself. A release
// is hard to unwind, so the human stays on the publish trigger.
//
// DistroKid has no public upload API; the only path is the web form.
// This drives your *installed Google Chrome* via Playwright, reusing a
// persistent profile so you only log in once.
//
// ── Usage ────────────────────────────────────────────────────────────
//   node pop/bin/distrokid-submit.mjs <distrokid-folder>
//   node pop/bin/distrokid-submit.mjs <folder> --dry-run   # validate only, no browser
//   node pop/bin/distrokid-submit.mjs <folder> --step      # pause after each section
//   node pop/bin/distrokid-submit.mjs <folder> --connect 9222
//        # attach to a Chrome already running with --remote-debugging-port=9222
//
// <folder> must contain:
//   release.json   — the metadata (schema below)
//   <audioFile>    — the WAV master named in release.json
//   <coverFile>    — the 3000² cover named in release.json
//
// First run: a Chrome window opens at distrokid.com. If you are not
// logged in, log in manually — the profile at ~/.distrokid-profile keeps
// the session for every run after.
//
// ── release.json ─────────────────────────────────────────────────────
//   {
//     "title": "marimbaba",
//     "artist": "Aesthetic Dot Computer",
//     "albumTitle": "marimbaba",
//     "label": "Aesthetic Dot Computer",
//     "primaryGenre": "Children's Music",
//     "secondaryGenre": "",
//     "songwriters": ["Jeffrey Scudder"],   // real legal names, "First Last"
//     "instrumental": true,
//     "explicit": false,
//     "year": 2026,
//     "previouslyReleased": false,
//     "releaseDate": "asap",                // "asap" or "YYYY-MM-DD"
//     "audioFile": "marimbaba-MASTER.wav",
//     "coverFile": "marimbaba-cover-3000.jpg"
//   }
//
// ── Calibration note ─────────────────────────────────────────────────
// DistroKid changes its form. The locators below are accessibility-first
// (visible label / role text) so they survive cosmetic redesigns, but a
// structural change will still break a step. A broken step logs a clear
// ⚠ and — under --step — drops into the Playwright Inspector (page.pause)
// so the selector can be fixed live. Treat this as maintenance tooling.

import { readFileSync, existsSync } from "node:fs";
import { resolve, join } from "node:path";
import { homedir } from "node:os";
import { chromium } from "playwright";

// ── args ─────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flag = (n) => argv.includes(n);
const flagVal = (n) => { const i = argv.indexOf(n); return i >= 0 ? argv[i + 1] : null; };
const DRY = flag("--dry-run");
const STEP = flag("--step");
const CONNECT_PORT = flagVal("--connect");

const expandHome = (p) =>
  p?.startsWith("~/") ? join(homedir(), p.slice(2)) : p;

const folderArg = argv.find((a) => !a.startsWith("--") && a !== CONNECT_PORT);
if (!folderArg) {
  console.error("usage: distrokid-submit.mjs <distrokid-folder> [--dry-run] [--step] [--connect PORT]");
  process.exit(1);
}
const FOLDER = resolve(process.cwd(), expandHome(folderArg));

// ── load + validate release.json ─────────────────────────────────────
const relPath = join(FOLDER, "release.json");
if (!existsSync(relPath)) {
  console.error(`✗ no release.json in ${FOLDER}`);
  process.exit(1);
}
const rel = JSON.parse(readFileSync(relPath, "utf8"));

const REQUIRED = ["title", "artist", "primaryGenre", "audioFile", "coverFile"];
const missing = REQUIRED.filter((k) => !rel[k]);
if (missing.length) {
  console.error(`✗ release.json missing: ${missing.join(", ")}`);
  process.exit(1);
}
const audioPath = join(FOLDER, rel.audioFile);
const coverPath = join(FOLDER, rel.coverFile);
for (const [label, p] of [["audio", audioPath], ["cover", coverPath]]) {
  if (!existsSync(p)) { console.error(`✗ ${label} file not found: ${p}`); process.exit(1); }
}

console.log(`▸ DistroKid submit · ${rel.title} — ${rel.artist}`);
console.log(`  album      ${rel.albumTitle || rel.title}`);
console.log(`  genre      ${rel.primaryGenre}${rel.secondaryGenre ? ` / ${rel.secondaryGenre}` : ""}`);
console.log(`  songwriter ${(rel.songwriters || []).join(", ") || "(none set)"}`);
console.log(`  flags      instrumental=${!!rel.instrumental} explicit=${!!rel.explicit}`);
console.log(`  release    ${rel.releaseDate || "asap"}`);
console.log(`  audio      ${rel.audioFile}`);
console.log(`  cover      ${rel.coverFile}`);

if (DRY) {
  console.log("✓ dry-run — release.json valid, files present. No browser launched.");
  process.exit(0);
}

// ── helpers ──────────────────────────────────────────────────────────
let page;

// Run one labelled form action; never crash the whole flow on a miss.
async function step(desc, fn) {
  try {
    await fn();
    console.log(`  ✓ ${desc}`);
  } catch (e) {
    console.warn(`  ⚠ ${desc} — ${String(e.message).split("\n")[0]}`);
    if (STEP) {
      console.log("  ⏸  Inspector open — fix the locator, Resume to continue.");
      await page.pause();
    }
  }
}

async function sectionBreak(name) {
  console.log(`── ${name} ──`);
  if (STEP) {
    console.log("  ⏸  --step: review this section, Resume in the Inspector to continue.");
    await page.pause();
  }
}

// ── browser ──────────────────────────────────────────────────────────
const PROFILE_DIR = join(homedir(), ".distrokid-profile");
let ctx;

if (CONNECT_PORT) {
  console.log(`▸ attaching to Chrome on CDP :${CONNECT_PORT}`);
  const browser = await chromium.connectOverCDP(`http://localhost:${CONNECT_PORT}`);
  ctx = browser.contexts()[0] || (await browser.newContext());
  page = ctx.pages()[0] || (await ctx.newPage());
} else {
  console.log(`▸ launching Chrome (profile: ${PROFILE_DIR})`);
  ctx = await chromium.launchPersistentContext(PROFILE_DIR, {
    channel: "chrome",
    headless: false,
    viewport: null,
    args: ["--start-maximized"],
  });
  page = ctx.pages()[0] || (await ctx.newPage());
}

// ── login gate ───────────────────────────────────────────────────────
await page.goto("https://distrokid.com/dashboard/", { waitUntil: "domcontentloaded" });
if (/signin|\/login|account\/login/i.test(page.url())) {
  console.log("⏸  Not logged in. Log into DistroKid in the Chrome window.");
  console.log("    Waiting for the dashboard… (the profile keeps this session for next time)");
  await page.waitForURL(/distrokid\.com\/(dashboard|mymusic|new)/i, { timeout: 5 * 60_000 });
}
console.log("✓ logged in");

// ── upload flow ──────────────────────────────────────────────────────
// The DistroKid upload form is one long page reached at /new. Locators
// are accessibility-first (getByLabel / getByRole / getByText) keyed off
// DistroKid's visible copy — see the calibration note in the header.
await page.goto("https://distrokid.com/new/", { waitUntil: "domcontentloaded" });
await page.waitForLoadState("networkidle").catch(() => {});

await sectionBreak("release basics");

await step("release type = single", async () => {
  await page.getByText(/^1$|one song|single/i).first().click({ timeout: 8000 });
});

await step("artist / band name", async () => {
  const a = page.getByLabel(/artist|band name/i).first();
  await a.fill(rel.artist);
});

await step("album / release title", async () => {
  await page.getByLabel(/album.*title|title of (your )?(album|release)/i)
    .first().fill(rel.albumTitle || rel.title);
});

await step("record label", async () => {
  if (rel.label)
    await page.getByLabel(/record label/i).first().fill(rel.label);
});

await step("primary genre", async () => {
  await page.getByLabel(/primary genre|^genre/i).first()
    .selectOption({ label: rel.primaryGenre });
});

await step("secondary genre", async () => {
  if (rel.secondaryGenre)
    await page.getByLabel(/secondary genre/i).first()
      .selectOption({ label: rel.secondaryGenre });
});

await sectionBreak("artwork");

await step("upload cover art", async () => {
  const input = page.locator('input[type="file"]').filter({ hasNot: page.locator("[data-audio]") }).first();
  await input.setInputFiles(coverPath);
});

await sectionBreak("the song");

await step("song title", async () => {
  await page.getByLabel(/song title|title of (your )?song/i).first().fill(rel.title);
});

await step("songwriter real name(s)", async () => {
  const names = rel.songwriters || [];
  for (let i = 0; i < names.length; i++) {
    if (i > 0) {
      await page.getByRole("button", { name: /add (another )?songwriter/i }).click().catch(() => {});
    }
    const parts = names[i].trim().split(/\s+/);
    const first = parts.shift();
    const last = parts.join(" ") || first;
    await page.getByLabel(/songwriter.*first|first name/i).nth(i).fill(first);
    await page.getByLabel(/songwriter.*last|last name/i).nth(i).fill(last);
  }
});

await step("instrumental flag", async () => {
  const box = page.getByLabel(/instrumental/i).first();
  if (rel.instrumental) await box.check();
  else await box.uncheck();
});

await step("explicit-lyrics flag", async () => {
  const want = rel.explicit ? /explicit/i : /clean|no|not explicit/i;
  await page.getByLabel(want).first().check().catch(async () => {
    await page.getByText(want).first().click();
  });
});

await step("upload audio file", async () => {
  // the audio input is usually the last/file input that accepts wav
  const inputs = page.locator('input[type="file"]');
  const n = await inputs.count();
  await inputs.nth(Math.max(0, n - 1)).setInputFiles(audioPath);
});

await sectionBreak("release date + stores");

await step("release date", async () => {
  if (rel.releaseDate && rel.releaseDate !== "asap") {
    await page.getByLabel(/release date/i).first().fill(rel.releaseDate);
  }
  // "asap" → leave DistroKid's earliest-possible default untouched
});

// stores are all-checked by DistroKid by default — left as-is on purpose.

// ── stop at review ───────────────────────────────────────────────────
console.log("");
console.log("✅ Fields filled. STOPPING before the final submit.");
console.log("   Scroll up: verify every field + the artwork + audio preview,");
console.log("   tick DistroKid's agreement boxes, then click the release button");
console.log("   yourself. The Chrome window stays open — this script will not");
console.log("   submit and will not close the browser.");
console.log("");
console.log("   When you have submitted (or want to bail), press Ctrl-C here.");

// Hold the process open so the browser stays up for the user.
await new Promise(() => {});
