// signup-password-journey.test, 2026.10.08
// The fallback door (`?login=password`): Universal Login with a password.
// End-to-end proof that a stranger can join: a fresh browser (no cookies,
// no ~/.ac-token) types `signup` at the prompt, creates an Auth0 account as
// mail+signuptest<stamp>@aesthetic.computer, gets back to AC, receives the
// verification letter, opens its link, picks a handle, and then the handle
// API answers for the new account. Afterwards the account is put through
// delete-erase-and-forget-me (locked now, purged after the grace period).
//
//   npm run test:signup:e2e:password               # headless, production
//   AC_HEADED=1 npm run test:signup:e2e:password   # watch it
//   AC_SIGNUP_KEEP=1 ...                    # leave the test account alive
//
// The letter is read off jasellite, where mail@aesthetic.computer's maildir
// lives (toolchain/macos/SCORE.md, "Mail lives on jasellite"): mbsync ac-mail,
// mu index, mu find. Gmail delivers plus-addresses to mail@, so every run gets
// its own address and its own handle without touching anyone else's inbox.
//
// The browser counts as automated (navigator.webdriver), so the signup funnel
// files these attempts under automated and the human numbers stay clean.
//
// Puppeteer needs a Chrome; with none downloaded, point it at the installed
// one: PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome".

import { randomBytes } from "node:crypto";
import { ACSession, CONFIG, scenario, report } from "./ac-harness.mjs";
import { waitForLetter } from "./jasellite-mail.mjs";

const KEEP = process.env.AC_SIGNUP_KEEP === "1";

const stamp = Date.now().toString(36);
const email = `mail+signuptest${stamp}@aesthetic.computer`;
const handle = `sut${stamp}`; // ≤ 16 characters, letters and numbers
const password = `Ac-${randomBytes(12).toString("base64url")}!9`;
const base = new URL(CONFIG.baseURL);

// ─── the browser ─────────────────────────────────────────────────────────
const ac = await ACSession.open(); // a fresh profile: the incognito case
const page = ac.page;
const stages = [];
page.on("request", (request) => {
  if (!request.url().endsWith("/api/signup-track")) return;
  try {
    const event = JSON.parse(request.postData());
    for (const stage of event.stages) if (!stages.includes(stage)) stages.push(stage);
  } catch {}
});
async function reached(stage, timeout = 30_000) {
  const deadline = Date.now() + timeout;
  while (!stages.includes(stage)) {
    if (Date.now() > deadline) throw new Error(`signup never reached "${stage}" (got ${stages.join(" → ") || "nothing"})`);
    await ac.wait(250);
  }
}
const dialogText = () => page.evaluate(() => document.querySelector("dialog.ac-signup")?.innerText || "");
const clickDialogButton = (label) => page.evaluate((label) => {
  const button = [...document.querySelectorAll("dialog.ac-signup button")].find((b) => b.textContent.trim() === label);
  button?.click();
  return !!button;
}, label);

// Each step depends on the one before, so the first failure skips the rest.
let broken = false;
const step = (name, fn) => scenario(name, async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  try { expect(true, await fn()); } catch (error) { broken = true; throw error; }
});

let sub = null, ticket = null;
const t0 = Date.now();
const elapsed = () => `${((Date.now() - t0) / 1000).toFixed(1)}s`;
console.log(`\n🧪 signup journey · ${email} · @${handle}\n`);

await step("prompt `signup` opens Auth0's create-account form", async () => {
  await ac.boot("prompt?login=password");
  await ac.focusPrompt();
  await ac.type("signup");
  await ac.press("Enter");
  await page.waitForSelector("#email", { timeout: 30_000 });
  const host = new URL(page.url()).host;
  if (!/\/u\/signup/.test(page.url())) throw new Error(`landed on ${page.url()} instead of the signup screen`);
  await reached("started", 5000);
  await ac.shot("signup-auth0-form");
  return `${host}/u/signup at ${elapsed()}`;
});

await step("a new email + password gets back to AC and asks for verification", async () => {
  await page.type("#email", email, { delay: 15 });
  await page.type("#password", password, { delay: 15 });
  await Promise.all([
    page.waitForFunction((host) => location.host === host, { timeout: 45_000 }, base.host),
    page.click("button[type=submit]"),
  ]).catch(async (error) => {
    const why = await page.evaluate(() =>
      [...document.querySelectorAll("[id*=error],.ulp-input-error-message,[class*=error]")].map((e) => e.innerText).filter(Boolean).join(" | "));
    throw new Error(why ? `Auth0 refused: ${why}` : error.message);
  });
  await reached("auth_returned");
  await reached("verification_shown");
  await page.waitForFunction(() => document.querySelector("dialog.ac-signup")?.open, { timeout: 15_000 });
  const text = await dialogText();
  if (!/Check your email/i.test(text)) throw new Error(`verification dialog says: ${text.slice(0, 120)}`);
  sub = (await page.evaluate(async () => (await window.auth0Client?.getUser())?.sub)) || null;
  await ac.shot("signup-check-email");
  return `${sub || "?"} at ${elapsed()}`;
});

await step("the verification letter arrives and its link verifies", async () => {
  ticket = await waitForLetter(email, /https:\/\/[^\s"'<>]+email-verification\?ticket=[^\s"'<>]+/);
  const tab = await ac.browser.newPage();
  // Auth0 verifies the ticket and redirects to AC with the outcome in the
  // query: ?success=true&message=Your email was verified…
  const outcomes = [];
  tab.on("framenavigated", (frame) => { if (frame === tab.mainFrame()) outcomes.push(frame.url()); });
  await tab.goto(ticket, { waitUntil: "domcontentloaded", timeout: 45_000 });
  await ac.wait(2000);
  await tab.close();
  const landed = outcomes.map((u) => new URL(u)).find((u) => u.searchParams.has("success"));
  if (landed?.searchParams.get("success") !== "true")
    throw new Error(`verification did not succeed: ${landed?.searchParams.get("message") || outcomes.at(-1)}`);
  return `letter at ${elapsed()} · "${landed.searchParams.get("message")}"`;
});

await step("“I’ve verified my email” leads to the handle form", async () => {
  await page.bringToFront();
  if (!(await clickDialogButton("I’ve verified my email"))) throw new Error("no verified button in the dialog");
  await reached("handle_shown", 20_000);
  await page.waitForSelector("#ac-signup-handle", { visible: true, timeout: 10_000 });
  await ac.shot("signup-handle-form");
  return `at ${elapsed()}`;
});

await step(`choosing @${handle} completes the signup`, async () => {
  await page.type("#ac-signup-handle", handle, { delay: 20 });
  await page.evaluate(() => document.querySelector("dialog.ac-signup .primary")?.click());
  await reached("completed", 30_000);
  await ac.wait(1500);
  await ac.shot("signup-complete");
  return `at ${elapsed()}`;
});

await step("the handle API knows the new account", async () => {
  if (!sub) throw new Error("no Auth0 sub was read from the page");
  const res = await fetch(`${CONFIG.baseURL}/handle?for=${encodeURIComponent(sub)}`);
  const json = await res.json().catch(() => ({}));
  if (json.handle !== handle) throw new Error(`/handle?for= said ${res.status} ${JSON.stringify(json)}`);
  return `@${json.handle}`;
});

// Cleanup runs whenever an account exists, even after a failed step.
if (!KEEP && sub) {
  broken = false;
  await step("delete-erase-and-forget-me locks the test account", async () => {
    const token = await page.evaluate(() => window.auth0Client?.getTokenSilently());
    if (!token) throw new Error("no token to delete with — account left alive");
    const res = await fetch(`${CONFIG.baseURL}/api/delete-erase-and-forget-me`, {
      method: "POST", headers: { Authorization: `Bearer ${token}` },
    });
    const json = await res.json().catch(() => ({}));
    if (res.status !== 200) throw new Error(`${res.status} ${JSON.stringify(json)}`);
    return `purge after ${json.purgeAfter || "?"}`;
  });
}

await ac.browser.close();
console.log(`\n  stages  ${stages.join(" → ")}`);
console.log(`  account ${email} · @${handle}${KEEP ? " (kept)" : ""}\n`);
process.exit(report());
