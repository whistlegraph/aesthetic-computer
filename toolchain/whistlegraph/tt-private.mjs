#!/usr/bin/env node
// Select public TikTok posts through iPhone Mirroring, ready for one physical
// confirmation on the iPhone: Next → Only you → Update.
//
// TikTok's post checkboxes, tabs, and scrolling accept mirrored pointer input.
// Its custom audience radio sheet does not, so this runner deliberately stops
// at Next (N) instead of claiming a change that TikTok did not accept.
//
//   node tt-private.mjs --dry
//   node tt-private.mjs --max=1   # select one additional post
//   node tt-private.mjs --all     # tap, scroll, repeat until the list ends
//   node tt-private.mjs --assist  # you scroll; the runner selects each screen

import { execFileSync, spawnSync } from "child_process";
import { join } from "path";

const args = process.argv.slice(2);
const has = (name) => args.includes(name);
const value = (name) => {
  const arg = args.find((item) => item.startsWith(`${name}=`));
  return arg ? arg.slice(name.length + 1) : null;
};

const DRY = has("--dry");
const ALL = has("--all");
const ASSIST = has("--assist");
const maxArg = value("--max");
const LIMIT = ALL || ASSIST ? Infinity : maxArg ? Number.parseInt(maxArg, 10) : 0;
const CIRCLE_X = 0.895;
const CIRCLE_DY = -0.015;
const ROW_LO = 0.28;
const ROW_HI = 0.885;
const SCROLL_TICKS = value("--scroll") ? Number.parseInt(value("--scroll"), 10) : -5;
const MAX_SCROLLS = 300;

if (!DRY && LIMIT === 0) {
  console.error("Choose --max=N or --all. No taps were sent.");
  process.exit(2);
}
if (!DRY && (LIMIT < 1 || Number.isNaN(LIMIT))) {
  console.error("--max must be a positive integer");
  process.exit(2);
}

const HERE = new URL(".", import.meta.url).pathname;
const TAP = join(HERE, "..", "macos", "iphone-tap", "iphone-tap");
const SHOT = "/tmp/tt-private-select.png";

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
function cli(...commandArgs) {
  try {
    return JSON.parse(execFileSync(TAP, commandArgs, { encoding: "utf8" }));
  } catch (error) {
    try { return JSON.parse(error.stdout?.toString() || ""); }
    catch { return { error: error.message }; }
  }
}

function read() {
  const shot = cli("shot", SHOT);
  if (shot.error) return { error: shot.error };
  const ocr = cli("ocr", SHOT);
  if (ocr.error) return { error: ocr.error };
  return {
    width: ocr.w,
    height: ocr.h,
    lines: (ocr.lines || []).map((line) => ({
      ...line,
      cx: line.x + line.w / 2,
      cy: line.y + line.h / 2,
    })),
  };
}

const onManage = (lines) => lines.some((line) => /manage post visibility/i.test(line.text));
const publicRows = (lines) => lines
  .filter((line) => /^Ever.one\s*[\u2022\u00b7]/i.test(line.text.trim()) && line.cy > ROW_LO && line.cy < ROW_HI)
  .sort((a, b) => a.cy - b.cy);
const nextCount = (lines) => {
  const next = lines.find((line) => /^next\s*\(\d+\)/i.test(line.text.trim()));
  return next ? Number.parseInt(next.text.match(/\((\d+)\)/)[1], 10) : 0;
};
const screenKey = (rows) => rows.map((row) => `${row.text.trim()}@${row.cy.toFixed(3)}`).join("|");

function selectedAt(screen, row) {
  const size = 50;
  const cx = Math.round(CIRCLE_X * screen.width);
  const cy = Math.round((row.cy + CIRCLE_DY) * screen.height);
  const x = Math.max(0, cx - size / 2);
  const y = Math.max(0, cy - size / 2);
  const result = spawnSync("magick", [
    SHOT, "-crop", `${size}x${size}+${x}+${y}`, "-colorspace", "sRGB",
    "-format", "%[fx:mean.r],%[fx:mean.g],%[fx:mean.b]", "info:",
  ], { encoding: "utf8" });
  if (result.status !== 0) return false;
  const [r, g, b] = result.stdout.trim().split(",").map(Number);
  return Number.isFinite(r) && r - g > 0.12 && r - b > 0.08;
}

function tapRows(rows) {
  const sequence = ["tap-sequence"];
  for (const row of rows) {
    sequence.push(String(CIRCLE_X), String(Math.max(0.30, row.cy + CIRCLE_DY)), "320");
  }
  return cli(...sequence);
}

let screen = read();
if (screen.error) {
  console.error(`Read failed: ${screen.error}`);
  process.exit(1);
}
if (!onManage(screen.lines)) {
  console.error('Open TikTok’s "Manage post visibility" screen first.');
  process.exit(1);
}

const initialCount = nextCount(screen.lines);
const initialRows = publicRows(screen.lines);
const initiallyUnselected = initialRows.filter((row) => !selectedAt(screen, row));
if (DRY) {
  console.log(`Next (${initialCount}); ${initialRows.length} public rows visible, ${initiallyUnselected.length} unselected. No taps sent.`);
  process.exit(0);
}

if (ASSIST) {
  console.log(`Assist live at Next (${initialCount}). Scroll after each visible set turns pink; tap Next at the bottom.`);
  let added = 0;
  let lastKey = "";
  let stableReads = 0;
  for (;;) {
    screen = read();
    if (screen.error) { console.error(`Read failed: ${screen.error}`); process.exit(1); }
    if (screen.lines.some((line) => /update selected posts/i.test(line.text))) {
      console.log(`Audience sheet reached at Next (${nextCount(screen.lines) || initialCount + added}). Use the physical iPhone: Only you → Update.`);
      break;
    }
    if (!onManage(screen.lines)) { console.error("Left Manage post visibility."); process.exit(1); }

    const rows = publicRows(screen.lines);
    const key = screenKey(rows);
    stableReads = key === lastKey ? stableReads + 1 : 0;
    lastKey = key;
    if (stableReads < 1) { await sleep(220); continue; }

    const unselected = rows.filter((row) => !selectedAt(screen, row));
    if (unselected.length > 0) {
      const before = nextCount(screen.lines);
      const result = tapRows(unselected);
      if (result.error) { console.error(`Tap sequence failed: ${result.error}`); process.exit(1); }
      await sleep(450);
      const verified = read();
      if (verified.error) { console.error(`Verification failed: ${verified.error}`); process.exit(1); }
      const after = nextCount(verified.lines);
      if (after !== before + unselected.length) {
        console.error(`Expected Next (${before + unselected.length}), saw Next (${after}). Stopping before any retry.`);
        process.exit(1);
      }
      added += unselected.length;
      console.log(`Next (${after}) — scroll`);
      lastKey = "";
      stableReads = 0;
    }
    await sleep(300);
  }
  process.exit(0);
}

console.log(`Starting at Next (${initialCount}); selecting ${LIMIT === Infinity ? "to the end" : `up to ${LIMIT} more`}.`);
let added = 0;
let scrolls = 0;
let unchanged = 0;
let previousKey = "";
let halted = "";

while (added < LIMIT && scrolls < MAX_SCROLLS) {
  const rows = publicRows(screen.lines);
  const unselected = rows.filter((row) => !selectedAt(screen, row));
  const take = unselected.slice(0, LIMIT === Infinity ? undefined : LIMIT - added);

  if (take.length > 0) {
    const before = nextCount(screen.lines);
    const result = tapRows(take);
    if (result.error) { halted = `tap sequence: ${result.error}`; break; }
    await sleep(500);
    screen = read();
    if (screen.error) { halted = `read after taps: ${screen.error}`; break; }
    const after = nextCount(screen.lines);
    if (after !== before + take.length) {
      halted = `expected Next (${before + take.length}), saw Next (${after})`;
      break;
    }
    added += take.length;
    console.log(`selected ${after} total`);
    if (added >= LIMIT) break;
  }

  const key = screenKey(publicRows(screen.lines));
  unchanged = key === previousKey ? unchanged + 1 : 0;
  if (unchanged >= 2) break;
  previousKey = key;

  const result = cli("scroll", String(SCROLL_TICKS));
  if (result.error) { halted = `scroll: ${result.error}`; break; }
  scrolls += 1;
  await sleep(700);
  screen = read();
  if (screen.error) { halted = `read after scroll: ${screen.error}`; break; }
  if (!onManage(screen.lines)) { halted = "left Manage post visibility"; break; }
}

const selected = nextCount(screen.lines);
console.log(`Ready at Next (${selected}) after ${scrolls} scrolls; added ${added}.${halted ? ` HALTED: ${halted}` : ""}`);
if (!halted) {
  console.log("On the iPhone, tap Next → Only you → Update. Then lock it to reconnect Mirroring for verification.");
}
if (halted) process.exitCode = 1;
