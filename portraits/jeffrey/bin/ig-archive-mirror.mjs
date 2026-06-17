#!/usr/bin/env node
// ig-archive-mirror.mjs
//
// Archive @whistlegraph grid posts by driving the REAL Instagram app on the
// REAL iPhone through macOS "iPhone Mirroring" — the only path that changes
// grid visibility AND can't be flagged as a bot, because it literally is the
// app on the device. (IG's web/API and private-API archiving were both dead
// ends — see memory: whistlegraph-grid-pruning.)
//
// Eyes + hands come from the iphone-tap CLI (toolchain/macos/iphone-tap):
//   shot → PNG of the mirror window,  ocr → text+boxes,  tap FX FY → a tap.
// Targeting is HYBRID: on-device OCR first (fast/free), Claude vision as a
// fallback when OCR can't confidently find the control.
//
// Per post the loop walks the profile feed:
//   shot+read → classify (reel/keep? → skip) → tap "⋯" → find "Archive" →
//   tap → verify → log → pace → next.
//
// ── Usage ────────────────────────────────────────────────────────────
//   node bin/ig-archive-mirror.mjs                 # DRY-RUN (reads + plans, no taps that mutate)
//   node bin/ig-archive-mirror.mjs --live          # actually archive
//   node bin/ig-archive-mirror.mjs --live --limit=1   # one post (the 5pm smoke test)
//   node bin/ig-archive-mirror.mjs --calibrate     # interactive: dump shot+ocr so you can tune COORDS
//
// Resume is automatic (skips shortcodes already in *-archived.jsonl). Always
// preserves reels, the protected oldest post, and any --keep shortcode.

import { readFileSync, existsSync, appendFileSync, mkdirSync, writeFileSync } from "fs";
import { join, dirname } from "path";
import { execFileSync } from "child_process";

// ── args ─────────────────────────────────────────────────────────────
const args = process.argv.slice(2);
const has = (n) => args.includes(n);
const val = (n) => {
  const a = args.find((x) => x.startsWith(n + "="));
  return a ? a.slice(n.length + 1) : null;
};

const ACCOUNT = val("--account") || "whistlegraph";
const LIVE = has("--live");
const CALIBRATE = has("--calibrate");
const LIMIT = val("--limit") ? parseInt(val("--limit"), 10) : Infinity;
const MIN_DELAY = val("--min-delay") ? parseFloat(val("--min-delay")) : 4; // seconds
const MAX_DELAY = val("--max-delay") ? parseFloat(val("--max-delay")) : 9;
const KEEP = val("--keep"); // extra shortcode to never archive
const VISION_MODEL = val("--model") || "gpt-4o-mini"; // OpenAI vision
const NO_VISION = has("--no-vision"); // OCR-only

// ── paths ────────────────────────────────────────────────────────────
const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");
const TAP = join(REPO, "toolchain/macos/iphone-tap/iphone-tap");
const CURATED = join(REPO, "portraits/jeffrey/curated");
const indexPath = join(CURATED, `${ACCOUNT}-grid.json`);
const archivedPath = join(CURATED, `${ACCOUNT}-archived.jsonl`);
const failedPath = join(CURATED, `${ACCOUNT}-archive-mirror-failed.jsonl`);
const SHOT = "/tmp/ig-mirror-shot.png";

// ── coords, calibrated live 2026-06-15 against the real app ────────────
// Controls are LOCATED BY OCR every time — the ⋯ menu's contents and their
// positions shift per post type (a reel's sheet ≠ a photo's; an already-removed
// post shows "Pin to main grid" where "Remove from grid" was). These are only
// fallbacks / anchors. Run --calibrate to re-tune if Instagram's layout moves.
const COORDS = {
  firstPost: [0.17, 0.63], // top-left grid thumbnail when scrolled to the very top
  back: [0.07, 0.124], // back arrow (top-left, on the "Posts" title row) → grid. x=0.07 not 0.05 (edge misses)
  profileTab: [0.90, 0.965], // profile avatar in the bottom nav — return-to-grid fallback
};

// The ⋯ glyph isn't OCR-readable, but it sits on the SAME ROW as the author
// name, hard right. So we find the author row by OCR (account name, left-
// aligned) and tap [0.93, thatRowY]. Robust across the audio-row / no-audio
// layouts that shift the ⋯ up and down.
const MORE_X = 0.93;
const MORE_Y_FALLBACK = 0.185;

// Per-post action (confirmed with @jeffrey 2026-06-15):
//   reel/video → "Remove from grid"  (stays in the Reels tab, just off the grid)
//   photo/carousel → "Archive"       (hidden from everyone, reversible)
// Overridable: --archive-all or --remove-all force one action for every post.
const FORCE = has("--archive-all") ? "archive" : has("--remove-all") ? "remove" : null;

// ── preflight ─────────────────────────────────────────────────────────
if (!existsSync(TAP)) {
  console.error(`✗ iphone-tap not built. Run: toolchain/macos/iphone-tap/build.sh`);
  process.exit(1);
}
if (!existsSync(indexPath)) {
  console.error(`✗ missing ${indexPath} — run bin/ig-grid-index.mjs first`);
  process.exit(1);
}
const index = JSON.parse(readFileSync(indexPath, "utf8"));
const keepShortcode = KEEP || index.first_post.shortcode;

const done = new Set();
if (existsSync(archivedPath)) {
  for (const l of readFileSync(archivedPath, "utf8").split("\n").filter(Boolean)) {
    try { done.add(JSON.parse(l).shortcode); } catch {}
  }
}

// Every grid post is in scope now (reels get "Remove from grid", photos get
// "Archive"). The queue is a planning count; the live walk reads each post's
// type on screen. Keep the protected oldest post and any --keep.
const queue = index.posts.filter(
  (p) => p.shortcode !== keepShortcode && !done.has(p.shortcode),
);
const videoCount = index.posts.filter((p) => p.is_video).length;
const photoCount = index.posts.length - videoCount;

// OpenAI key: env first, else the vault devcontainer env (same source the
// recap scripts use) so the run works without an explicit export.
function resolveOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = join(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  try {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  } catch {}
  return "";
}
const VISION_KEY = resolveOpenAIKey();

const actionLabel = FORCE === "archive" ? "ALL → Archive"
  : FORCE === "remove" ? "ALL → Remove from grid"
  : "reels → Remove from grid · photos → Archive";

console.log("══ ig-grid-prune (mirror) ══");
console.log(`  account:        @${ACCOUNT}`);
console.log(`  total grid:     ${index.posts.length} (${photoCount} photo/carousel, ${videoCount} reel/video)`);
console.log(`  action:         ${actionLabel}`);
console.log(`  keep visible:   ${keepShortcode} (${index.first_post.date})`);
console.log(`  already done:   ${done.size}`);
console.log(`  to process:     ${queue.length}`);
console.log(`  pace:           ${MIN_DELAY}–${MAX_DELAY}s/post (jittered)`);
console.log(`  targeting:      OCR${NO_VISION ? " only" : ` + ${VISION_MODEL} fallback`}`);
console.log(`  vision key:     ${VISION_KEY ? "present" : "MISSING (OCR-only)"}`);
console.log(`  mode:           ${CALIBRATE ? "CALIBRATE" : LIVE ? "LIVE (will modify the grid)" : "DRY-RUN"}`);
console.log(`  limit:          ${LIMIT === Infinity ? "none" : LIMIT}`);
console.log("");

// ── primitives over iphone-tap ─────────────────────────────────────────
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const jitter = () => (MIN_DELAY + Math.random() * (MAX_DELAY - MIN_DELAY)) * 1000;

function tapcli(...a) {
  try {
    return JSON.parse(execFileSync(TAP, a, { encoding: "utf8" }));
  } catch (e) {
    const out = e.stdout?.toString() || "";
    try { return JSON.parse(out); } catch { return { error: e.message }; }
  }
}

function frame() { return tapcli("frame"); }
function shot() { return tapcli("shot", SHOT); }
function ocr() { return tapcli("ocr", SHOT); }
async function tap(fx, fy) {
  const r = tapcli("tap", String(fx), String(fy));
  await sleep(280);
  return r;
}

// Read the current screen once: returns { lines:[{text,x,y,w,h,conf}] } or null.
function read() {
  const s = shot();
  if (s.error) return { error: s.error };
  const o = ocr();
  if (o.error) return { error: o.error };
  // Detect the dreaded all-black Continuity capture (OCR finds nothing AND the
  // file is tiny/uniform) so the caller can switch to the QuickTime read path.
  return { lines: o.lines, blank: o.lines.length === 0 };
}

// Find a control by visible text. Returns center fraction [fx,fy] or null.
function findByText(lines, matcher) {
  const hit = lines.find((l) => matcher(l.text));
  if (!hit) return null;
  return [hit.x + hit.w / 2, hit.y + hit.h / 2];
}

// LLM vision fallback: ask OpenAI where a control is, normalized [0,1].
async function askVision(question) {
  if (NO_VISION || !VISION_KEY) return null;
  const b64 = readFileSync(SHOT).toString("base64");
  const body = {
    model: VISION_MODEL,
    max_tokens: 200,
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
          `This is a screenshot of an iPhone running Instagram. ${question}\n` +
          `Reply ONLY with compact JSON: {"found":true,"x":0.0,"y":0.0} where x,y are ` +
          `the target's center as fractions (0..1) from the TOP-LEFT, or {"found":false} ` +
          `if it isn't visible.` },
        { type: "image_url", image_url: { url: `data:image/png;base64,${b64}` } },
      ],
    }],
  };
  try {
    const res = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: {
        "content-type": "application/json",
        authorization: `Bearer ${VISION_KEY}`,
      },
      body: JSON.stringify(body),
    });
    const j = await res.json();
    const text = j.choices?.[0]?.message?.content || "";
    const m = text.match(/\{[^}]*\}/);
    if (!m) return null;
    const parsed = JSON.parse(m[0]);
    return parsed.found ? [parsed.x, parsed.y] : null;
  } catch (e) {
    console.log(`   ↳ vision error: ${e.message}`);
    return null;
  }
}

// Hybrid locate: OCR first, vision second.
async function locate(lines, { ocrMatch, visionAsk }) {
  if (ocrMatch) {
    const p = findByText(lines, ocrMatch);
    if (p) return { at: p, via: "ocr" };
  }
  if (visionAsk) {
    const p = await askVision(visionAsk);
    if (p) return { at: p, via: "vision" };
  }
  return null;
}

// Locate the ⋯ button. Its glyph isn't OCR-readable, but it sits on a post's
// AUTHOR ROW, hard right. The trap: on the scrollable feed page, CAPTIONS also
// start with the handle ("whistlegraph <caption…>") — so we must pick the SHORT
// line (just the handle, optionally + a verified badge), NOT a long caption.
// Search the whole left column (y up to 0.85) since the author row can sit
// mid-screen between two stacked posts.
function locateMore(lines) {
  const acct = ACCOUNT.toLowerCase();
  const leftCol = (l) => l.x + l.w / 2 < 0.45 && l.y + l.h / 2 > 0.05 && l.y + l.h / 2 < 0.85;
  // Author row: a SHORT line starting with the handle (excludes long captions).
  let row = lines
    .filter((l) => leftCol(l) && l.text.trim().toLowerCase().startsWith(acct) &&
      l.text.trim().length <= acct.length + 4)
    .sort((a, b) => a.y - b.y)[0];
  // Collab posts read "X and whistlegraph" — a short-ish contains line.
  if (!row) row = lines
    .filter((l) => leftCol(l) && l.text.toLowerCase().includes(acct) && l.text.trim().length <= 32)
    .sort((a, b) => a.y - b.y)[0];
  const y = row ? row.y + row.h / 2 : MORE_Y_FALLBACK;
  return { at: [MORE_X, y], via: row ? "author-row" : "anchor" };
}

// The ⋯ sheet has NO "Cancel" — dismiss it by swiping it down off-screen.
async function dismissSheet() {
  tapcli("swipe", "0.5", "0.15", "0.5", "0.95", "--duration", "250");
  await sleep(600);
}

// Is the ⋯ action sheet open? Match DISTINCTIVE menu-only PHRASES — multi-word
// strings that never appear in a caption. (We deliberately do NOT use bare
// "archive" here: post captions can contain the word "archive", which would
// look like an open menu — that caused false "sheet still open" halts. OCR also
// prepends row icons as glyphs, so substring-match, not exact-line.)
const SHEET_ITEMS = /(remove from grid|pin to main grid|add to grid|hide (like count|likes|share count)|turn off (commenting|downloading|reuse)|adjust preview)/i;
const sheetIsOpen = (lines) => lines.some((l) => SHEET_ITEMS.test(l.text.trim()));
// The Archive ROW is a SHORT line ("Archive", maybe an icon glyph) — distinct
// from a caption that merely contains the word "archive".
const isArchiveItem = (text) => /\barchive\b/i.test(text) && text.trim().length <= 14;

// ── logging ────────────────────────────────────────────────────────────
function record(path, row) {
  mkdirSync(dirname(path), { recursive: true });
  appendFileSync(path, JSON.stringify({ ...row, at: new Date().toISOString() }) + "\n");
}
const recordDone = (extra) => record(archivedPath, { via: "mirror", ...extra });
const recordFail = (reason, extra = {}) => record(failedPath, { reason, ...extra });

// ── calibrate mode: dump a labelled read so coords can be tuned ─────────
if (CALIBRATE) {
  const f = frame();
  if (f.error) { console.log(`frame: ${f.error}`); process.exit(1); }
  console.log(`window frame: ${JSON.stringify(f)}`);
  const r = read();
  if (r.error) { console.log(`read: ${r.error}`); process.exit(1); }
  if (r.blank) {
    console.log("⚠ capture is BLANK (Continuity privacy). Use the QuickTime read path:");
    console.log("  open a QuickTime 'Movie Recording' of the phone and re-run with");
    console.log("  --read-window calibration (see README).");
  }
  console.log(`saved shot → ${SHOT}`);
  console.log(`OCR found ${r.lines?.length || 0} text lines:`);
  for (const l of r.lines || []) {
    console.log(`  "${l.text}"  center≈ [${(l.x + l.w / 2).toFixed(3)}, ${(l.y + l.h / 2).toFixed(3)}]`);
  }
  writeFileSync("/tmp/ig-mirror-ocr.json", JSON.stringify(r, null, 2));
  console.log("full OCR → /tmp/ig-mirror-ocr.json");
  console.log("\nopen the shot to eyeball coords:  open " + SHOT);
  process.exit(0);
}

// ── act on the post currently open on screen ───────────────────────────
// SAFETY: are we on @ACCOUNT's OWN content? Profile/post headers show the
// account name CENTERED at the very top. If it's a different account, refuse to
// act (Archive/Remove are owner-only anyway, but this stops wasted churn + the
// wrong-account scare). Used to HALT the run if we ever drift off-account.
// "whistlegraph" appearing anywhere in the TOP HEADER BAND (cy<0.18) means it's
// whistlegraph's own post/grid (the header shows the profile owner there). Robust
// to OCR merging the handle into an adjacent caption ("…it'd bwhistlegraph") and
// to collab name-order ("X and whistlegraph"). A DIFFERENT account's page shows
// THAT name in the band, not whistlegraph — so this still halts on drift.
// Match a distinctive PREFIX of the handle in the header band — tolerant of OCR
// errors in the tail ("whistlegraph" → "whistlegranh", ph→nh) and of glued
// neighbours ("bwhistlegraph"). The prefix is long enough not to false-match
// other accounts. (Full match also accepted.)
const ACCT_LC = ACCOUNT.toLowerCase();
const ACCT_PREFIX = ACCT_LC.slice(0, Math.min(ACCT_LC.length, 10)); // "whistlegra"
const onAccount = (lines) =>
  lines.some((l) => l.y + l.h / 2 < 0.18 &&
    (l.text.toLowerCase().includes(ACCT_LC) || l.text.toLowerCase().includes(ACCT_PREFIX)));

// Are we on the profile GRID? "followers"/"following" when scrolled up, OR the
// Reels+Tagged tab pair (visible even scrolled down). The grid also has a
// left-aligned "Posts" TAB — which must NOT be confused with a post's header.
const onGridLines = (lines) =>
  // The stats row has BOTH "followers" and "following"; requiring both avoids
  // false-positives from a caption/comment that merely contains "following".
  (lines.some((l) => /\bfollowers\b/i.test(l.text)) &&
   lines.some((l) => /\bfollowing\b/i.test(l.text))) ||
  // OCR often prefixes the tab icons ("D Reels", "9 Tagged") — match loosely.
  (lines.some((l) => /\breels\b/i.test(l.text)) &&
   lines.some((l) => /\btagged\b/i.test(l.text)));

// Are we on a post-detail CARD? Its header reads "Posts" CENTERED at the very
// top (cx≈0.5, cy<0.17) — distinct from the grid's left-aligned "Posts" tab.
// A full-screen reel PLAYER has no such header → treated as fell-out → re-enter
// via the grid, where reels open as cards we can act on.
// A CENTERED "Posts" title at the very top is the definitive post-detail marker
// (the grid's "Posts" is a LEFT-aligned tab at cx≈0.17). This is independent of
// grid-word noise — a post caption can contain "followers"/"following"/"reels",
// which must NOT flip it back to "grid".
const onPost = (lines) =>
  lines.some((l) => /^posts$/i.test(l.text.trim()) &&
    Math.abs(l.x + l.w / 2 - 0.5) < 0.12 && l.y + l.h / 2 < 0.17);

// A cheap fingerprint of the post on screen — the longest text line in the lower
// half (caption / "Liked by …"). Used to tell whether an action advanced us to a
// new post (archive auto-advances; remove/skip may leave the same post showing).
function postSig(lines) {
  const c = (lines || [])
    .filter((l) => l.y + l.h / 2 > 0.62 && l.text.trim().length > 6)
    .sort((a, b) => b.text.length - a.text.length);
  return c[0]?.text.trim() || "";
}

// Advance to the next post within the Posts detail feed (swipe up = next).
async function advanceToNextPost() {
  tapcli("swipe", "0.5", "0.72", "0.5", "0.30", "--duration", "240");
  await sleep(750);
}

// Two-tier (confirmed 2026-06-15): reel → "Remove from grid", photo → "Archive".
// Controls are OCR-located fresh (the sheet's contents + positions vary per
// post). Returns { status: "done"|"dry"|"skip"|"fail", action, type, via }.
async function actOnCurrent() {
  let r = read();
  if (r.error) return { status: "fail", reason: r.error };
  if (r.blank) return { status: "fail", reason: "blank_capture_use_quicktime" };
  // Real login/challenge wall? Use word boundaries + specific phrases so post
  // CAPTIONS don't false-trigger (e.g. "b[log in]to" matched bare /log in/).
  // A real wall also has NO post header, so only flag it when NOT on a post.
  if (!onPost(r.lines) && r.lines.some((l) =>
    /(\blog in\b|\bsign up\b|couldn'?t refresh feed|confirm it'?s you|try again later|we restrict)/i.test(l.text)))
    return { status: "fail", reason: "auth_or_challenge_wall" };

  // Open the ⋯ sheet. The glyph isn't OCR-readable and its Y varies by layout
  // (author row, location line) and scroll position, so TRY candidate Ys at the
  // right edge until the sheet opens — primary is the located author row (the
  // SHORT handle line). Header taps that miss are harmless.
  const tryOpenMenu = async () => {
    const cands = [];
    const lm = locateMore(r.lines).at[1];
    for (const y of [lm, 0.19, 0.143, 0.232]) if (!cands.includes(y)) cands.push(y);
    for (const y of cands) {
      await tap(MORE_X, y);
      await sleep(450);
      r = read();
      if (!r.error && sheetIsOpen(r.lines)) return true;
    }
    return false;
  };
  let opened = await tryOpenMenu();
  if (!opened) {
    // The post is probably sitting scrolled so the ⋯/author row is above the
    // viewport. Gently scroll UP a touch to reveal the header, then retry once.
    // (Small + deliberate — not blind hunting.)
    tapcli("scroll", "6"); await sleep(120); tapcli("scroll", "6"); await sleep(350);
    r = read();
    if (!r.error) opened = await tryOpenMenu();
  }
  if (!opened) {
    // Genuinely no ⋯ menu (e.g. a collab post @whistlegraph doesn't own).
    return { status: "skip", reason: "no_menu" };
  }

  // Decide from what the sheet offers. "Pin to main grid" is the pin option,
  // present on normal on-grid posts — NOT an off-grid signal.
  let hasRemove = r.lines.some((l) => /remove from grid/i.test(l.text));
  let hasArchive = r.lines.some((l) => isArchiveItem(l.text));
  // The sheet animates open top-down; a fast read can catch "Pin to main grid"
  // (sheet-open passes) before "Archive"/"Remove" render → a false "off-grid"
  // skip. If neither is seen yet, re-read once after it settles.
  if (!hasRemove && !hasArchive) {
    await sleep(450);
    r = read();
    if (!r.error) {
      hasRemove = r.lines.some((l) => /remove from grid/i.test(l.text));
      hasArchive = r.lines.some((l) => isArchiveItem(l.text));
    }
  }
  const type = hasRemove ? "reel" : "photo"; // remove-from-grid ⟺ video/reel

  let effAction = null;
  if (FORCE === "remove") effAction = hasRemove ? "remove" : null;
  else if (FORCE === "archive") effAction = hasArchive ? "archive" : null;
  else if (hasRemove) effAction = "remove";  // reel/video → remove (keep in Reels tab)
  else if (hasArchive) effAction = "archive"; // photo → archive

  if (!effAction) {
    // no Remove and no Archive = already off the grid / already archived.
    await dismissSheet();
    return { status: "skip", reason: "already_off_grid", type };
  }

  if (!LIVE) {
    await dismissSheet();
    console.log(`${type} → would ${effAction}`);
    return { status: "dry", type };
  }

  const itemMatch = effAction === "remove"
    ? (t) => /remove from grid/i.test(t)
    : (t) => isArchiveItem(t); // SHORT "Archive" row, not a caption containing "archive"
  const itemAsk = effAction === "remove"
    ? "Where is the 'Remove from grid' item in this sheet?"
    : "Where is the 'Archive' item in this sheet?";
  const item = await locate(r.lines, { ocrMatch: (t) => itemMatch(t.trim()), visionAsk: itemAsk });
  if (!item) {
    await dismissSheet();
    return { status: "fail", reason: `${effAction}_item_not_found`, type };
  }
  await tap(...item.at);
  await sleep(500);

  // verify — poll for the sheet to close (close animation + reload can take a
  // moment; reads first so a fast close returns immediately). Up to ~3s.
  let sheetGone = false;
  for (let p = 0; p < 6 && !sheetGone; p++) {
    r = read();
    if (!r.error && !sheetIsOpen(r.lines)) { sheetGone = true; break; }
    await sleep(500);
  }
  if (effAction === "remove") {
    const toast = r.lines.some((l) => /removed from (the )?main grid/i.test(l.text));
    if (toast) return { status: "done", action: effAction, type, via: item.via };
    // toast can fade fast; sheet-gone is a soft confirm
    return sheetGone
      ? { status: "done", action: effAction, type, via: item.via, soft: true }
      : { status: "fail", reason: "remove_not_confirmed", type };
  }
  // archive: success = the sheet (and usually the post) is gone
  return sheetGone
    ? { status: "done", action: effAction, type, via: item.via }
    : { status: "fail", reason: "sheet_still_open_after_archive", type };
}

// ── feed walk (the fast path) ───────────────────────────────────────────
// @jeffrey's tip 2026-06-15: after Archive the Posts detail view AUTO-ADVANCES
// to the prior post — so we never go back to the grid. Open ONE post, then
// ⋯ → act → (auto-advance) → repeat. Remove/skip/dry leave the same post, so a
// signature check swipes to the next one. Eliminates the back+scroll+reopen that
// dominated the old per-post time.
const scrollToTop = async () => { for (let k = 0; k < 4; k++) { tapcli("scroll", "12"); await sleep(90); } await sleep(300); };
const onGrid = onGridLines;

// Surprise interstitial? IG pops an "Edits" App-Store promo card (recurs on app
// open). It has no X — dismiss by tapping ABOVE the card. Detect by its text.
const dismissPromoIfAny = async (lines) => {
  if (lines?.some((l) => /level up|edits|app store/i.test(l.text))) {
    await tap(0.5, 0.15); // tap outside (above the card)
    await sleep(700);
    return true;
  }
  return false;
};

console.log(`Sight-gated walk: acts ONLY when the top reads "Posts / ${ACCOUNT}".`);
console.log("Rides the Archive auto-advance. NO blind swiping — halts the moment it can't confirm.\n");

let archivedN = 0, removed = 0, skipped = 0, halted = "";
const effLimit = (LIMIT === Infinity && !LIVE) ? 5 : LIMIT; // bounded dry-run preview

for (let i = 0; archivedN + removed + skipped < effLimit; i++) {
  const label = `#${i + 1}`;
  let r = read();
  if (!r.error && r.lines && (await dismissPromoIfAny(r.lines))) r = read();

  // GATE 1: must be on a @ACCOUNT post detail ("Posts / <account>" at top).
  // Auto-advance can briefly flash a loading/reel frame, so WAIT a few re-reads
  // for it to settle on a valid post before deciding (wait-and-see, not swiping).
  if (r.error || !r.lines || !onPost(r.lines) || !onAccount(r.lines)) {
    for (let p = 0; p < 4 && (r.error || !r.lines || !(onPost(r.lines) && onAccount(r.lines))); p++) {
      await sleep(700);
      r = read();
      if (!r.error && r.lines) await dismissPromoIfAny(r.lines) && (r = read());
    }
  }
  if (r.error || !r.lines || !onPost(r.lines) || !onAccount(r.lines)) {
    // The only allowed recovery is DELIBERATE: if on @ACCOUNT's own grid, open
    // the top post. Anything else (wrong account, reel player, modal, lost
    // feed) → HALT. Never blind-swipe to hunt for a post.
    if (!r.error && r.lines && onGrid(r.lines) && onAccount(r.lines)) {
      console.log(`[${label}] on @${ACCOUNT} grid → opening top post`);
      await scrollToTop();
      await tap(...COORDS.firstPost);
      await sleep(1000);
      continue;
    }
    halted = `top is not "Posts / ${ACCOUNT}" (wrong account / off-page) — not swiping`;
    break;
  }

  const sigBefore = postSig(r.lines);
  process.stdout.write(`[${label}] `);

  // actOnCurrent opens ⋯ (confirms the menu opened) and acts (confirms the sheet
  // is gone) — every tap is gated on seeing the expected control/state.
  const res = await actOnCurrent();
  if (res.status === "done") {
    res.action === "remove" ? removed++ : archivedN++;
    console.log(`✓ ${res.type} → ${res.action}${res.soft ? " (soft)" : ""}`);
    recordDone({ action: res.action, type: res.type, index: i });

    // GATE 2: confirm we're ready for the next post. An ARCHIVE hides the post,
    // so the field ALWAYS auto-advances — just confirm we land back on a valid
    // @ACCOUNT post (no signature check; two posts can share a caption line and
    // collide, which was causing false halts). A REMOVE leaves the post on
    // screen, so it needs a deliberate advance + a real signature change.
    const validPost = (lines) => onPost(lines) && onAccount(lines);
    const movedPost = (lines) => validPost(lines) && (!sigBefore || postSig(lines) !== sigBefore);
    let onNew = false;
    if (res.action === "archive") {
      for (let p = 0; p < 7 && !onNew; p++) {        // read-first: instant advance returns at once
        const r1 = read();
        if (!r1.error && r1.lines && validPost(r1.lines)) { onNew = true; break; }
        await sleep(450);
      }
    } else { // remove: post stays → wait for natural move, else one deliberate advance
      for (let p = 0; p < 4 && !onNew; p++) {
        await sleep(450);
        const r1 = read();
        if (!r1.error && r1.lines && movedPost(r1.lines)) onNew = true;
      }
      if (!onNew) {
        await advanceToNextPost();
        for (let p = 0; p < 3 && !onNew; p++) {
          await sleep(450);
          const r2 = read();
          if (!r2.error && r2.lines && movedPost(r2.lines)) onNew = true;
        }
      }
    }
    if (!onNew) { halted = `couldn't confirm advance to the next @${ACCOUNT} post`; break; }
  } else if (res.status === "skip") {
    skipped++;
    console.log(`• skip (${res.reason})`);
    halted = `non-actionable post (${res.reason}) — stopping rather than swiping`;
    break;
  } else if (res.status === "dry") {
    console.log("(dry)"); break;
  } else {
    console.log(`✗ ${res.reason}`);
    recordFail(res.reason, { index: i });
    halted = res.reason;
    break;
  }
  await sleep(jitter());
}

console.log(`\n── done. archived ${archivedN}, removed ${removed}, skipped ${skipped}.${halted ? "  HALTED: " + halted : ""} ──`);
console.log(`log: ${archivedPath}`);
