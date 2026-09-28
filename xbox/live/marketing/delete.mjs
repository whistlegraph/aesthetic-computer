// Delete oskiewar reels without a visible browser.
//
//   node xbox/live/marketing/delete.mjs --login                  # once: log in as @oskiewar
//   node xbox/live/marketing/delete.mjs <id|code|permalink>...   # dry run: opens each ⋯ menu
//   node xbox/live/marketing/delete.mjs <...> --confirm          # deletes, then verifies
//   node xbox/live/marketing/delete.mjs <...> --confirm --record # and writes deletedAt
//   --engine lightpanda|chrome   (default: lightpanda, falling back to headless Chrome)
//
// Meta's Graph API has no deletion endpoint, so this drives Instagram's own web
// page: the post's ⋯ menu → Delete → "Delete post?" → Delete, the same clicks a
// person makes. It never calls a private endpoint and never touches the mobile
// API (see the header of publish.mjs for why that road stays closed).
//
// The session lives in a Chrome profile of its own, so the everyday browser and
// whichever account it is logged into are never switched. `--login` is the only
// step that shows a window, because only a person may type the password.
// Lightpanda has no profile directory, so it is handed that profile's cookies
// over CDP for the length of one run.
//
// Only a post whose menu offers Delete is touched: a menu offering Report means
// the session is not the owner, and the run stops there. A deletion counts only
// once the account's media listing (official API) no longer has it.

import { spawn } from "node:child_process";
import { existsSync, writeFileSync } from "node:fs";
import { createServer } from "node:net";
import { homedir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { chromium } from "playwright";

import { apiVersion, host, ledgerPath, readLedger } from "./publish.mjs";

export const profileDir = process.env.OSKIEWAR_IG_BROWSER_PROFILE ||
  join(homedir(), ".local/share/oskiewar-ig-browser");
const lightpandaBin = process.env.LIGHTPANDA_BIN ||
  join(homedir(), ".cache/lightpanda-node/lightpanda");
const IG = "https://www.instagram.com";
const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// A post can be named by its API media id, its shortcode, or its permalink.
// The page needs the shortcode, which for a media id only the ledger knows.
export function shortcodeFor(ref, ledger = readLedger()) {
  const fromUrl = /instagram\.com\/(?:p|reels?)\/([\w-]+)/.exec(ref);
  if (fromUrl) return fromUrl[1];
  if (/^\d+$/.test(ref)) {
    const post = ledger.posts.find((row) => String(row.mediaId) === ref);
    const code = post?.permalink && shortcodeFor(post.permalink, ledger);
    if (!code) throw new Error(`${ref}: no permalink in the ledger — run reel.mjs --insights`);
    return code;
  }
  if (/^[\w-]{6,}$/.test(ref)) return ref;
  throw new Error(`${ref}: not a media id, shortcode, or permalink`);
}

// ── Engines ─────────────────────────────────────────────────────────────────

const chromeProfile = (headless) => chromium.launchPersistentContext(profileDir, {
  channel: "chrome", headless, viewport: { width: 1280, height: 900 },
});

async function freePort() {
  const server = createServer().listen(0, "127.0.0.1");
  await new Promise((resolve) => server.once("listening", resolve));
  const { port } = server.address();
  await new Promise((resolve) => server.close(resolve));
  return port;
}

async function openChrome() {
  const context = await chromeProfile(true);
  return { name: "chrome", page: await context.newPage(), close: () => context.close() };
}

async function openLightpanda() {
  if (!existsSync(lightpandaBin)) throw new Error(`no Lightpanda binary at ${lightpandaBin}`);
  // Read the session out of the dedicated profile, then close it: Chrome holds
  // a lock on the profile for as long as it is open.
  const donor = await chromeProfile(true);
  const cookies = await donor.cookies(IG);
  await donor.close();

  const port = await freePort();
  const child = spawn(lightpandaBin, ["serve", "--host", "127.0.0.1", "--port", String(port)],
    { stdio: "ignore" });
  const kill = () => child.kill();
  try {
    let browser;
    for (let tries = 0; !browser; tries++) {
      try { browser = await chromium.connectOverCDP(`http://127.0.0.1:${port}`); }
      catch (error) { if (tries > 20) throw error; await wait(150); }
    }
    const context = browser.contexts()[0] || await browser.newContext();
    const page = await context.newPage();
    // Lightpanda answers Storage.setCookies (Playwright's addCookies) with
    // BrowserContextNotLoaded, but takes Network.setCookie one at a time.
    const cdp = await context.newCDPSession(page);
    for (const { name, value, domain, path, expires, httpOnly, secure, sameSite } of cookies)
      await cdp.send("Network.setCookie", {
        name, value, domain, path, httpOnly, secure, sameSite,
        ...(expires > 0 ? { expires } : {}),
      });
    return {
      name: "lightpanda", page,
      close: async () => { await browser.close().catch(() => {}); kill(); },
    };
  } catch (error) { kill(); throw error; }
}

const engines = { lightpanda: openLightpanda, chrome: openChrome };

// ── The clicks ──────────────────────────────────────────────────────────────

// Clicks go through the DOM rather than the mouse: Lightpanda has no layout to
// aim at, and Instagram's buttons answer a DOM click the same way.
const clickButton = (page, scope, text) => page.evaluate(([scope, text]) => {
  const root = scope === "dialog"
    ? [...document.querySelectorAll("[role=dialog]")].pop() : document;
  const button = root && [...root.querySelectorAll("button,[role=button]")]
    .find((el) => el.innerText.trim() === text);
  button?.click();
  return !!button;
}, [scope, text]);

const dialogText = (page) => page.evaluate(() =>
  [...document.querySelectorAll("[role=dialog]")].pop()?.innerText || "");

async function until(page, test, timeout = 20000) {
  for (const end = Date.now() + timeout; Date.now() < end; await wait(250))
    if (await page.evaluate(test)) return true;
  return false;
}

// Returns the stage reached. Past "confirmed" the post may already be gone, so
// a failure there must be verified rather than retried on another engine.
export async function deleteOne(page, code, { confirm, log = console.log }) {
  let stage = "open";
  try {
    await page.goto(`${IG}/p/${code}/`, { waitUntil: "load", timeout: 30000 });
    const ready = await until(page, () =>
      !!document.querySelector('svg[aria-label="More options"]') ||
      /isn't available|page may have been removed/i.test(document.body?.innerText || ""));
    if (!ready) throw new Error("the post page never finished loading");
    if (!await page.evaluate(() => !!document.querySelector('svg[aria-label="More options"]')))
      return { code, stage: "gone", note: "page says the post is not available" };

    stage = "menu";
    await page.evaluate(() => {
      const svg = document.querySelector('svg[aria-label="More options"]');
      (svg.closest('[role=button],button') || svg.parentElement).click();
    });
    await until(page, () => document.querySelector("[role=dialog]"), 8000);
    const menu = await dialogText(page);
    if (!/^Delete$/m.test(menu)) {
      const why = /^Report$/m.test(menu) ? "logged in, but not as the owner"
        : /^Log in$/m.test(menu) ? "not logged in — run --login"
        : "no Delete in the ⋯ menu";
      throw Object.assign(new Error(`${why} (menu: ${menu.split("\n").join(" · ")})`), { fatal: true });
    }
    if (!confirm) {
      await page.keyboard.press("Escape").catch(() => {});
      return { code, stage: "would-delete" };
    }

    stage = "delete";
    await clickButton(page, "dialog", "Delete");
    await until(page, () => /Delete post\?/.test(
      [...document.querySelectorAll("[role=dialog]")].pop()?.innerText || ""), 8000);
    if (!/Delete post\?/.test(await dialogText(page))) throw new Error("no confirmation dialog");
    if (!await clickButton(page, "dialog", "Delete")) throw new Error("no Delete in the confirmation");
    stage = "confirmed";
    // Instagram leaves the post page once the delete lands.
    await until(page, (path) => !location.pathname.startsWith(path), 15000)
      .catch(() => {});
    log(`🗑  ${code}`);
    return { code, stage };
  } catch (error) {
    error.stage = stage;
    throw error;
  }
}

// ── Verification against the official API ───────────────────────────────────

export async function liveShortcodes(igUserId, token) {
  const codes = new Set();
  let url = `${host()}/${apiVersion()}/${igUserId}/media?fields=permalink&limit=100` +
    `&access_token=${token}`;
  while (url) {
    const body = await (await fetch(url)).json();
    if (body.error) throw new Error(body.error.message);
    for (const media of body.data || []) {
      const code = /\/(?:p|reels?)\/([\w-]+)/.exec(media.permalink || "")?.[1];
      if (code) codes.add(code);
    }
    url = body.paging?.next;
  }
  return codes;
}

export function recordGone(ledger, codes, { at = new Date().toISOString(), reason }) {
  let recorded = 0;
  for (const post of ledger.posts) {
    const code = post.permalink && /\/(?:p|reels?)\/([\w-]+)/.exec(post.permalink)?.[1];
    if (!code || !codes.includes(code) || post.deletedAt) continue;
    post.deletedAt = at;
    post.deletedReason = reason;
    recorded++;
  }
  return recorded;
}

// ── CLI ─────────────────────────────────────────────────────────────────────

async function login() {
  const context = await chromeProfile(false);
  const page = context.pages()[0] || await context.newPage();
  await page.goto(`${IG}/accounts/login/`);
  console.log(`Log in as @oskiewar in the window (profile: ${profileDir}). Waiting…`);
  for (const end = Date.now() + 10 * 60 * 1000; Date.now() < end; await wait(1000)) {
    const cookies = await context.cookies(IG).catch(() => []);
    if (cookies.some((cookie) => cookie.name === "sessionid")) {
      await wait(3000); // let Instagram finish writing the rest of the session
      console.log("✓ session saved");
      return context.close();
    }
  }
  await context.close();
  throw new Error("no login within 10 minutes");
}

export async function run(args = process.argv.slice(2)) {
  if (args.includes("--login")) return login();

  const flag = (name) => args.includes(name);
  const valued = new Set(["--engine", "--reason"]);
  const value = (name) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : null; };
  const refs = args.filter((arg, i) => !arg.startsWith("--") && !valued.has(args[i - 1]));
  if (!refs.length) throw new Error("name at least one post (media id, shortcode, or permalink)");
  if (!existsSync(profileDir)) throw new Error("no session yet — run with --login first");

  const ledger = readLedger();
  const codes = refs.map((ref) => shortcodeFor(ref, ledger));
  const confirm = flag("--confirm");
  const order = value("--engine") ? [value("--engine")] : ["lightpanda", "chrome"];

  const results = [];
  let pending = [...codes];
  for (const name of order) {
    if (!pending.length) break;
    let engine;
    try { engine = await engines[name](); }
    catch (error) { console.log(`⚠️  ${name} would not start: ${error.message}`); continue; }
    console.log(`▸ ${engine.name} · ${pending.length} post(s) · ${confirm ? "DELETING" : "dry run"}`);
    const retry = [];
    try {
      for (const code of pending) {
        const started = Date.now();
        try {
          const result = await deleteOne(engine.page, code, { confirm });
          results.push({ ...result, engine: engine.name, ms: Date.now() - started });
          if (result.stage === "would-delete") console.log(`· ${code} · owner menu offers Delete`);
          if (result.stage === "gone") console.log(`· ${code} · already gone`);
        } catch (error) {
          console.log(`⚠️  ${code} · ${engine.name} · ${error.stage}: ${error.message}`);
          if (error.fatal) throw error;
          if (error.stage === "confirmed") results.push({ code, stage: "confirmed", engine: engine.name });
          else retry.push(code);
        }
        await wait(1500); // person-paced, never a burst
      }
    } finally { await engine.close(); }
    pending = retry;
  }
  if (pending.length) console.log(`✗ not reached on any engine: ${pending.join(", ")}`);

  const { OSKIEWAR_IG_USER_ID: igUserId, OSKIEWAR_IG_TOKEN: token } = process.env;
  const clicked = results.filter((r) => r.stage === "confirmed").map((r) => r.code);
  if (!confirm || !clicked.length) return results;
  if (!igUserId || !token) {
    console.log("No OSKIEWAR_IG_TOKEN: deletions are unverified and nothing is recorded.");
    return results;
  }
  await wait(3000);
  const live = await liveShortcodes(igUserId, token);
  const gone = clicked.filter((code) => !live.has(code));
  for (const code of clicked)
    console.log(`${live.has(code) ? "✗ still listed" : "✓ gone"} · ${code}`);
  if (flag("--record") && gone.length) {
    const n = recordGone(ledger, gone, {
      reason: value("--reason") || "deleted via delete.mjs; confirmed absent from the media listing",
    });
    writeFileSync(ledgerPath, JSON.stringify(ledger, null, 2) + "\n");
    console.log(`recorded ${n} in the ledger`);
  }
  return results;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1])
  run().catch((error) => { console.error(`✗ ${error.message}`); process.exit(1); });
