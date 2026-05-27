#!/usr/bin/env node
// ig-grid-archive.mjs
//
// Drives Instagram's web UI via Playwright to archive ("private") every grid
// post in our curated/<account>-grid.json index, newest→oldest. Reversible —
// archived posts are kept by IG; ig-grid-restore.mjs puts them back.
//
// Uses a persistent Chrome profile so you log in ONCE manually and never
// again. Account-safe by design: no fresh-login attempts (which is what
// soft-locked @whistlegraph before — see jeffrey-platter memory).
//
// ── Usage ────────────────────────────────────────────────────────────
//   node bin/ig-grid-archive.mjs                       # @whistlegraph, dry-run
//   node bin/ig-grid-archive.mjs --live                # actually click Archive
//   node bin/ig-grid-archive.mjs --live --limit=5      # archive 5 posts then stop
//   node bin/ig-grid-archive.mjs --live --min-delay=90 --max-delay=240
//   node bin/ig-grid-archive.mjs --account=whistlegraph --keep=sDmBX0ir5u
//
// Always preserves the chronologically-first grid post (or whatever --keep
// is set to). Resume is automatic — re-running skips shortcodes already in
// curated/<account>-archived.jsonl.

import { readFileSync, existsSync, appendFileSync, mkdirSync } from "fs";
import { homedir } from "os";
import { join, dirname } from "path";
import { chromium } from "playwright";

// ── args ─────────────────────────────────────────────────────────────
const args = process.argv.slice(2);
function flag(name) {
  return args.includes(name);
}
function flagVal(name) {
  const a = args.find((x) => x.startsWith(name + "="));
  return a ? a.slice(name.length + 1) : null;
}

const ACCOUNT = flagVal("--account") || "whistlegraph";
const LIVE = flag("--live");
const LIMIT = flagVal("--limit") ? parseInt(flagVal("--limit"), 10) : Infinity;
const MIN_DELAY = flagVal("--min-delay") ? parseInt(flagVal("--min-delay"), 10) : 60;
const MAX_DELAY = flagVal("--max-delay") ? parseInt(flagVal("--max-delay"), 10) : 180;
const KEEP = flagVal("--keep"); // shortcode to never archive
const CONNECT_PORT = flagVal("--connect");
const HEADFUL = !flag("--headless");
const COOKIES_ARG = flagVal("--cookies");

// ── paths ────────────────────────────────────────────────────────────
const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");
const indexPath = join(REPO, "portraits/jeffrey/curated", `${ACCOUNT}-grid.json`);
const archivedPath = join(REPO, "portraits/jeffrey/curated", `${ACCOUNT}-archived.jsonl`);
const failedPath = join(REPO, "portraits/jeffrey/curated", `${ACCOUNT}-archive-failed.jsonl`);
const PROFILE_DIR = join(homedir(), ".ac-instagram-profile");
const COOKIES_PATH =
  COOKIES_ARG || join(REPO, "portraits/jeffrey/sessions", `${ACCOUNT}.cookies.json`);

// ── load index ───────────────────────────────────────────────────────
if (!existsSync(indexPath)) {
  console.error(`missing ${indexPath} — run bin/ig-grid-index.mjs first`);
  process.exit(1);
}
const index = JSON.parse(readFileSync(indexPath, "utf8"));
const keepShortcode = KEEP || index.first_post.shortcode;

// ── resume set ───────────────────────────────────────────────────────
const done = new Set();
if (existsSync(archivedPath)) {
  for (const l of readFileSync(archivedPath, "utf8").split("\n").filter(Boolean)) {
    try {
      done.add(JSON.parse(l).shortcode);
    } catch {}
  }
}

const queue = index.posts.filter(
  (p) => p.shortcode !== keepShortcode && !done.has(p.shortcode),
);

console.log("══ ig-grid-archive ══");
console.log(`  account:        @${ACCOUNT}`);
console.log(`  total grid:     ${index.posts.length}`);
console.log(`  keep visible:   ${keepShortcode} (${index.first_post.date})`);
console.log(`  already done:   ${done.size}`);
console.log(`  to archive:     ${queue.length}`);
console.log(`  delay range:    ${MIN_DELAY}–${MAX_DELAY}s`);
console.log(`  mode:           ${LIVE ? "LIVE (will click Archive)" : "DRY-RUN (no clicks)"}`);
console.log(`  limit:          ${LIMIT === Infinity ? "none" : LIMIT}`);
console.log("");

if (queue.length === 0) {
  console.log("nothing to do.");
  process.exit(0);
}

// ── helpers ──────────────────────────────────────────────────────────
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const jitter = () => MIN_DELAY + Math.random() * (MAX_DELAY - MIN_DELAY);

function recordDone(post, extra = {}) {
  mkdirSync(dirname(archivedPath), { recursive: true });
  appendFileSync(
    archivedPath,
    JSON.stringify({
      shortcode: post.shortcode,
      url: post.url,
      taken_at: post.taken_at,
      date: post.date,
      archived_at: new Date().toISOString(),
      ...extra,
    }) + "\n",
  );
}

function recordFail(post, reason) {
  mkdirSync(dirname(failedPath), { recursive: true });
  appendFileSync(
    failedPath,
    JSON.stringify({
      shortcode: post.shortcode,
      url: post.url,
      date: post.date,
      failed_at: new Date().toISOString(),
      reason,
    }) + "\n",
  );
}

// ── browser ──────────────────────────────────────────────────────────
let ctx, page;
if (CONNECT_PORT) {
  console.log(`▸ attaching to Chrome on CDP :${CONNECT_PORT}`);
  const browser = await chromium.connectOverCDP(`http://localhost:${CONNECT_PORT}`);
  ctx = browser.contexts()[0] || (await browser.newContext());
  page = ctx.pages()[0] || (await ctx.newPage());
} else {
  console.log(`▸ launching Chrome (profile: ${PROFILE_DIR})`);
  ctx = await chromium.launchPersistentContext(PROFILE_DIR, {
    channel: "chrome",
    headless: !HEADFUL,
    viewport: null,
    args: ["--start-maximized"],
  });
  page = ctx.pages()[0] || (await ctx.newPage());
}

// ── cookie injection ─────────────────────────────────────────────────
// Import instaloader-session cookies if available. This makes the profile
// reusable across runs and skips a manual login.
if (existsSync(COOKIES_PATH)) {
  const cookies = JSON.parse(readFileSync(COOKIES_PATH, "utf8"));
  await ctx.addCookies(cookies);
  console.log(`✓ injected ${cookies.length} cookies from ${COOKIES_PATH}`);
} else {
  console.log(`(no cookies file at ${COOKIES_PATH} — relying on profile)`);
}

// ── login gate ───────────────────────────────────────────────────────
await page.goto("https://www.instagram.com/", { waitUntil: "domcontentloaded" });
await sleep(2000);

if (/accounts\/login/.test(page.url())) {
  console.log(`⏸  Not logged in. Log into IG as @${ACCOUNT} in the Chrome window.`);
  console.log(`   This profile (${PROFILE_DIR}) keeps the session for next time.`);
  await page.waitForURL((u) => !/accounts\/login/.test(String(u)), {
    timeout: 10 * 60_000,
  });
}

// Session check: a "Log in" button (or "Sign up" CTA in the bottom-of-page
// banner) only renders for unauthenticated users. IG's logged-in nav is all
// SVG/aria-label only, so we use the negative signal exclusively.
const loggedOutMarker = await page
  .locator(
    'button:has-text("Log in"), a:has-text("Log in"), a:has-text("Sign up")',
  )
  .first()
  .isVisible({ timeout: 3000 })
  .catch(() => false);

if (/accounts\/login/.test(page.url()) || loggedOutMarker) {
  console.log("⛔ session check failed — IG shows logged-out UI.");
  console.log("   re-export cookies after a fresh instaloader login:");
  console.log(
    `   bin/ig-import-cookies.py chrome ${ACCOUNT} && bin/ig-export-cookies.py --account=${ACCOUNT}`,
  );
  process.exit(1);
}
console.log("✓ session active");

// ── archive loop ─────────────────────────────────────────────────────
let archived = 0;
let consecutiveFails = 0;

function bail(reason) {
  console.log(`\n⛔ ${reason}`);
  console.log(`   archived ${archived}, ${queue.length - archived} remain.`);
  console.log(`   re-run to resume.`);
  process.exit(1);
}

for (const post of queue) {
  if (archived >= LIMIT) {
    console.log(`\n✓ hit --limit=${LIMIT}, stopping.`);
    break;
  }

  const idx = archived + 1;
  console.log(
    `[${idx}/${Math.min(queue.length, LIMIT)}] ${post.date}  ${post.shortcode}  ♥ ${post.likes}  💬 ${post.comments}  ${post.is_video ? "▶" : " "}`,
  );

  try {
    await page.goto(post.url, { waitUntil: "domcontentloaded", timeout: 30_000 });
    await sleep(2000 + Math.random() * 2000);

    // Defensive: any login redirect = stop immediately.
    if (/accounts\/login|challenge|checkpoint/i.test(page.url())) {
      bail(`redirected to ${page.url()} — session lost or checkpointed`);
    }

    // Detect "Sorry, this page isn't available" (already archived / deleted).
    const notFound = await page
      .locator("text=/Sorry, this page isn't available|Page Not Found/i")
      .first()
      .isVisible()
      .catch(() => false);
    if (notFound) {
      console.log("   ↳ page not available — recording as done.");
      recordDone(post, { note: "page_not_available" });
      archived += 1;
      consecutiveFails = 0;
      continue;
    }

    if (!LIVE) {
      console.log("   ↳ dry-run, no click.");
      archived += 1;
      consecutiveFails = 0;
      const wait = jitter();
      console.log(`   sleep ${wait.toFixed(1)}s`);
      await sleep(wait * 1000);
      continue;
    }

    // Open "..." menu — the kebab on the post.
    // IG's "More options" button is the SVG with aria-label="More options".
    const moreBtn = page
      .getByRole("button", { name: /more options/i })
      .first();
    await moreBtn.waitFor({ state: "visible", timeout: 15_000 });
    await moreBtn.click();
    await sleep(800 + Math.random() * 500);

    // Click "Archive" in the modal. Falls back to text match if role-button
    // lookup misses (IG sometimes renders these as div+role=button).
    let archiveItem = page
      .getByRole("button", { name: /^archive$/i })
      .first();
    if (!(await archiveItem.isVisible().catch(() => false))) {
      archiveItem = page.locator("div[role=button]", { hasText: /^Archive$/ }).first();
    }
    await archiveItem.waitFor({ state: "visible", timeout: 10_000 });
    await archiveItem.click();
    await sleep(1500 + Math.random() * 1000);

    // After archiving, IG typically pops a toast and the post URL becomes
    // unavailable to the public; the menu modal closes. We don't strictly
    // need to verify — but log a sanity check.
    const stillThere = await page
      .getByRole("button", { name: /more options/i })
      .first()
      .isVisible()
      .catch(() => false);
    if (stillThere) {
      // Menu likely re-opened on a navigation, or click missed. Treat as fail.
      recordFail(post, "more_options_still_visible_after_archive_click");
      consecutiveFails += 1;
    } else {
      recordDone(post);
      archived += 1;
      consecutiveFails = 0;
    }
  } catch (err) {
    console.log(`   ↳ error: ${err.message}`);
    recordFail(post, err.message);
    consecutiveFails += 1;
  }

  if (consecutiveFails >= 3) {
    bail("3 consecutive failures — likely UI changed or rate-limited.");
  }

  if (archived < LIMIT && archived < queue.length) {
    const wait = jitter();
    console.log(`   sleep ${wait.toFixed(1)}s`);
    await sleep(wait * 1000);
  }
}

console.log(`\n✓ done. archived ${archived} post(s).`);
await ctx.close().catch(() => {});
