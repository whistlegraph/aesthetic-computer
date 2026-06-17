#!/usr/bin/env node
// ig-archive-reels.mjs
//
// Archive @whistlegraph REELS by driving the real IG app via iPhone Mirroring.
// Companion to ig-archive-mirror.mjs (which does grid posts). Reels live in the
// full-screen reel PLAYER, where the flow is different and more dangerous:
//
//   ⋯ [0.91,0.84]  →  "Manage"  →  "Archive"  →  confirm dialog "Archive"
//      → "Archived" toast, auto-advances to the next reel.
//
// SAFETY: the reel right-rail has a REPOST button (~cy 0.62–0.72) that BROADCASTS
// to followers. This script ONLY ever taps the ⋯ at the exact coord [0.91,0.84]
// and menu items located by OCR — it NEVER taps elsewhere on the rail and NEVER
// swipes near it. Account-guarded: acts only when "whistlegra" is on screen.
//
//   node bin/ig-archive-reels.mjs --live [--limit=N] [--min-delay --max-delay]
// Start ON a @whistlegraph reel in the reel player. Default dry-run.

import { existsSync, appendFileSync, mkdirSync } from "fs";
import { join, dirname } from "path";
import { execFileSync } from "child_process";

const args = process.argv.slice(2);
const has = (n) => args.includes(n);
const val = (n) => { const a = args.find((x) => x.startsWith(n + "=")); return a ? a.slice(n.length + 1) : null; };

const ACCOUNT = val("--account") || "whistlegraph";
const ACCT_PREFIX = ACCOUNT.toLowerCase().slice(0, 10);
const LIVE = has("--live");
const LIMIT = val("--limit") ? parseInt(val("--limit"), 10) : Infinity;
const MIN_DELAY = val("--min-delay") ? parseFloat(val("--min-delay")) : 1;
const MAX_DELAY = val("--max-delay") ? parseFloat(val("--max-delay")) : 2;

const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");
const TAP = join(REPO, "toolchain/macos/iphone-tap/iphone-tap");
const archivedPath = join(REPO, "portraits/jeffrey/curated", `${ACCOUNT}-reels-archived.jsonl`);
const SHOT = "/tmp/ig-reel-shot.png";

// The ⋯ on the reel player. Its Y SHIFTS between reels (caption length, product
// tags move the rail), so we try a few candidate Ys at the rail x and detect
// which menu opened. We ONLY tap at x≈0.91 on these specific Ys — never the
// repost zone (~0.62–0.72) — so we can't broadcast.
const MORE_X = 0.91;
const MORE_YS = [0.84, 0.88, 0.80, 0.86];

if (!existsSync(TAP)) { console.error("✗ iphone-tap not built"); process.exit(1); }

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const jitter = () => (MIN_DELAY + Math.random() * (MAX_DELAY - MIN_DELAY)) * 1000;

function tapcli(...a) {
  try { return JSON.parse(execFileSync(TAP, a, { encoding: "utf8" })); }
  catch (e) { const o = e.stdout?.toString() || ""; try { return JSON.parse(o); } catch { return { error: e.message }; } }
}
const shot = () => tapcli("shot", SHOT);
const ocr = () => tapcli("ocr", SHOT);
async function tap(fx, fy) { const r = tapcli("tap", String(fx), String(fy)); await sleep(300); return r; }
function read() { const s = shot(); if (s.error) return { error: s.error }; const o = ocr(); if (o.error) return { error: o.error }; return { lines: o.lines || [] }; }

// Locate a menu item by text → center fraction, or null. `short` requires a
// short line (so a caption containing the word doesn't match a menu row).
function findItem(lines, re, { short = false } = {}) {
  const hit = lines.find((l) => re.test(l.text) && (!short || l.text.trim().length <= 16));
  return hit ? [hit.x + hit.w / 2, hit.y + hit.h / 2] : null;
}
const hasText = (lines, re) => lines.some((l) => re.test(l.text));
// Account guard: handle appears (OCR-tolerant prefix) somewhere on screen.
const onAccount = (lines) => lines.some((l) => l.text.toLowerCase().includes(ACCT_PREFIX));
// Reel/media-detail markers (so we don't act on random screens). Broad on
// purpose — some reels show "Add comment"/no view-count instead of the usual
// markers. The real safety is downstream: openOptions + Manage→Archive only
// succeed on an owned reel; anything else skips. Account guard also holds.
const onReel = (lines) =>
  hasText(lines, /views|plays|inspired on edits|original audio|· audio|add comment|view insights/i);

function record(row) {
  mkdirSync(dirname(archivedPath), { recursive: true });
  appendFileSync(archivedPath, JSON.stringify({ ...row, at: new Date().toISOString() }) + "\n");
}

// Dismiss any open bottom-sheet/dialog SAFELY — tap the dimmed area ABOVE it
// (never swipe near the rail). cy=0.25 is reliably in the dimmed zone.
async function safeDismiss() { await tap(0.5, 0.25); await sleep(600); }

// The ⋯ options menu (Save / Remix / Manage). NOT the share sheet.
const isOptionsMenu = (lines) => hasText(lines, /\bmanage\b/i) ||
  (hasText(lines, /\bremix\b/i) && hasText(lines, /\bsave\b/i));
const isShareSheet = (lines) => hasText(lines, /add to story|copy link/i);

// Open the ⋯ options menu, trying candidate Ys until the options menu (not the
// share sheet) appears. Returns true if open. Dismisses any wrong sheet first.
async function openOptions() {
  for (const y of MORE_YS) {
    await tap(MORE_X, y);
    await sleep(700);
    let r = read();
    if (r.error) continue;
    if (isOptionsMenu(r.lines)) return true;
    if (isShareSheet(r.lines)) { await safeDismiss(); continue; } // wrong icon → retry lower/other
    // some other state (e.g. nothing opened) — try next Y
  }
  return false;
}

// Archive the reel currently on screen. Returns status.
async function archiveCurrentReel() {
  let r = read();
  if (r.error) return { status: "fail", reason: r.error };
  // Patient checks — a freshly auto-advanced reel may still be loading, so the
  // handle/markers haven't rendered yet. Re-read a few times before halting.
  for (let p = 0; p < 4 && (r.error || !onReel(r.lines) || !onAccount(r.lines)); p++) {
    await sleep(700);
    r = read();
  }
  if (r.error || !onReel(r.lines)) return { status: "halt", reason: "not_on_reel_player" };
  if (!onAccount(r.lines)) return { status: "halt", reason: "account_not_visible" };

  if (!LIVE) return { status: "dry" };

  // 1) open the ⋯ options menu (tries candidate Ys; handles the shifting rail)
  if (!(await openOptions())) { await safeDismiss(); return { status: "skip", reason: "couldnt_open_options" }; }
  r = read();
  // 2) "Manage" (in the options sheet). A collab reel @whistlegraph doesn't own
  // shows Report/etc. with NO Manage → can't archive → SKIP (advance), not halt.
  const manage = findItem(r.lines, /^manage$|manage$/i, { short: true });
  if (!manage) { await safeDismiss(); return { status: "skip", reason: "collab_or_unowned" }; }
  await tap(...manage);
  await sleep(700);
  // 3) "Archive" in the Manage submenu (short line, not a caption with "archive")
  r = read();
  let arch = findItem(r.lines, /\barchive\b/i, { short: true });
  if (!arch) { await sleep(500); r = read(); arch = findItem(r.lines, /\barchive\b/i, { short: true }); }
  if (!arch) { await safeDismiss(); return { status: "halt", reason: "no_archive_item" }; }
  await tap(...arch);
  await sleep(900);
  // 4) confirmation dialog → tap the "Archive" button (NOT "Cancel")
  r = read();
  const confirm = findItem(r.lines, /^archive$/i, { short: true });
  if (!confirm) { await safeDismiss(); return { status: "fail", reason: "no_confirm_dialog" }; }
  await tap(...confirm);
  await sleep(1500);
  // 5) verify: dialog gone (poll a little for the toast/advance)
  for (let p = 0; p < 5; p++) {
    r = read();
    if (!r.error && !hasText(r.lines, /archive reel\?/i)) {
      if (hasText(r.lines, /archived/i)) return { status: "done", via: "toast" };
      return { status: "done", via: "dialog-gone" };
    }
    await sleep(600);
  }
  return { status: "fail", reason: "confirm_not_verified" };
}

console.log("══ ig-archive-reels ══");
console.log(`  account: @${ACCOUNT}   mode: ${LIVE ? "LIVE" : "DRY-RUN"}   limit: ${LIMIT === Infinity ? "none" : LIMIT}`);
console.log(`  pace: ${MIN_DELAY}–${MAX_DELAY}s/reel`);
console.log("  Start ON a @whistlegraph reel. ⋯→Manage→Archive→confirm. Repost-safe (only taps ⋯ + OCR'd items).\n");

let archived = 0, skipped = 0, halted = "";
for (let i = 0; archived < (LIMIT === Infinity ? Infinity : LIMIT); i++) {
  process.stdout.write(`[#${i + 1}] `);
  const res = await archiveCurrentReel();
  if (res.status === "done") {
    archived++;
    console.log(`✓ archived (${res.via})`);
    record({ index: i });
    await sleep(jitter()); // archive auto-advances to the next reel
  } else if (res.status === "dry") {
    console.log("(dry — would archive)"); break;
  } else if (res.status === "skip") {
    // A collab/unowned reel blocks the walk and the player won't advance
    // programmatically → halt and ask @jeffrey to swipe past it by hand.
    skipped++;
    halted = `collab/unowned reel (${res.reason}) — SWIPE to the next reel, then re-run`;
    console.log(`✋ ${halted}`); break;
  } else if (res.status === "halt") {
    halted = res.reason; console.log(`⛔ ${res.reason}`); break;
  } else {
    halted = res.reason; console.log(`✗ ${res.reason}`); break;
  }
}
console.log(`\n── done. archived ${archived}.${halted ? "  HALTED: " + halted : ""} ──`);
console.log(`log: ${archivedPath}`);
