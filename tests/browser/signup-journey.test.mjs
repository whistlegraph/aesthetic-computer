// signup-journey.test, 2026.10.08
// End-to-end proof of the in-page door (lib/signup-flow.mjs): a fresh browser
// types `signup` at the prompt, picks a @handle (held by /api/handle-hold),
// gives mail+signuptest<stamp>@aesthetic.computer, types the six digits Auth0
// mails, and lands signed in with that handle — without ever leaving
// aesthetic.computer. Then it logs out and back in by code (the returning-user
// path), and finally puts the account through delete-erase-and-forget-me.
//
//   npm run test:signup:e2e                       # headless, production
//   AC_HEADED=1 npm run test:signup:e2e           # watch it
//   AC_SIGNUP_OVERLAY=1 npm run test:signup:e2e   # production, but with this
//       checkout's boot/bios/signup files swapped in and /api/handle-hold
//       answered locally — proves the client before it ships
//   AC_SIGNUP_KEEP=1 ...                          # leave the test account alive
//
// The code is read off jasellite (./jasellite-mail.mjs). The browser counts as
// automated, so the funnel files these attempts under automated.
//
// Puppeteer needs a Chrome; with none downloaded, point it at the installed
// one: PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome".

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { ACSession, CONFIG, scenario, report } from "./ac-harness.mjs";
import { waitForLetter } from "./jasellite-mail.mjs";

const KEEP = process.env.AC_SIGNUP_KEEP === "1";
const OVERLAY = process.env.AC_SIGNUP_OVERLAY === "1";
const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");

const stamp = Date.now().toString(36);
const email = `mail+signuptest${stamp}@aesthetic.computer`;
const handle = `sut${stamp}`; // ≤ 16 characters, letters and numbers
const CODE = /verification code is:\s*(\d{6})/;

// ─── the browser ─────────────────────────────────────────────────────────
const ac = await ACSession.open(); // a fresh profile: the incognito case
const page = ac.page;

if (OVERLAY) {
  // Serve this checkout's copies of the files the door lives in, and stand in
  // for the hold endpoint that isn't deployed yet. Auth0, the mailed code and
  // /handle stay real.
  const local = {
    "/aesthetic.computer/boot.mjs": "system/public/aesthetic.computer/boot.mjs",
    "/aesthetic.computer/bios.mjs": "system/public/aesthetic.computer/bios.mjs",
    "/aesthetic.computer/lib/signup-flow.mjs": "system/public/aesthetic.computer/lib/signup-flow.mjs",
    "/aesthetic.computer/lib/signup-model.mjs": "system/public/aesthetic.computer/lib/signup-model.mjs",
    "/aesthetic.computer/lib/auth0-otp.mjs": "system/public/aesthetic.computer/lib/auth0-otp.mjs",
  };
  await page.setBypassServiceWorker(true);
  await page.setRequestInterception(true);
  page.on("request", (request) => {
    const url = new URL(request.url());
    if (url.host !== new URL(CONFIG.baseURL).host) return request.continue();
    if (local[url.pathname]) {
      return request.respond({ status: 200, contentType: "text/javascript; charset=utf-8",
        headers: { "cache-control": "no-store" }, body: readFileSync(join(ROOT, local[url.pathname]), "utf8") });
    }
    if (url.pathname === "/api/handle-hold") {
      const body = request.method() === "POST" ? { held: true, until: new Date(Date.now() + 600000).toISOString() } : { status: "free" };
      return request.respond({ status: 200, contentType: "application/json", body: JSON.stringify(body) });
    }
    request.continue();
  });
}

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
const dialog = () => page.evaluate(() => {
  const d = document.querySelector("dialog.ac-signup");
  return d?.open ? { title: d.querySelector("h1")?.textContent || "", text: d.innerText, status: d.querySelector("[role=status]")?.textContent || "" } : null;
});
async function dialogTitled(pattern, timeout = 20_000) {
  const deadline = Date.now() + timeout;
  let last;
  while (Date.now() < deadline) {
    last = await dialog().catch(() => null);
    if (last && pattern.test(last.title)) return last;
    await ac.wait(200);
  }
  throw new Error(`expected a dialog titled ${pattern}, saw ${last ? `"${last.title}" (${last.status})` : "none"}`);
}
async function typeInto(selector, text) {
  await page.waitForSelector(selector, { visible: true, timeout: 10_000 });
  await page.click(selector, { clickCount: 3 });
  await page.type(selector, text, { delay: 25 });
}
const recent = [];
page.on("console", (m) => { recent.push(m.text().slice(0, 140)); if (recent.length > 40) recent.shift(); });
const otpToken = () => page.evaluate(() => { try { return JSON.parse(localStorage.getItem("ac-otp-session"))?.access || null; } catch { return null; } });
let token = null;
const signedInAs = () => page.evaluate(() => window.acUSER?.sub || null);
// The prompt can swallow keys typed while it is still settling, so give a
// command a few tries before deciding the door didn't open.
async function command(text, title) {
  for (let tries = 0; tries < 3; tries++) {
    // A stalled boot ignores typing, and a half-typed command can't be cleared
    // with Backspace (on an empty prompt it navigates), so retries reload.
    if (tries) await ac.boot("prompt");
    // bios sets `preloaded` once a real piece (not the blank init disk) boots.
    await page.waitForFunction(() => window.preloaded === true, { timeout: 60_000 }).catch(() => {});
    await ac.wait(1200);
    await ac.focusPrompt();
    await ac.type(text);
    await ac.wait(300);
    await ac.press("Enter");
    try { return await dialogTitled(title, 8000); } catch {}
  }
  await ac.shot(`signup-code-${text}-missing`);
  console.log("  last console:", recent.slice(-6).join(" ‖ "));
  return dialogTitled(title, 1);
}

// Each step depends on the one before, so the first failure skips the rest.
let broken = false;
const step = (name, fn) => scenario(name, async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  try { expect(true, await fn()); } catch (error) { broken = true; throw error; }
});

let sub = null, codeAsked = 0;
const t0 = Date.now();
const elapsed = () => `${((Date.now() - t0) / 1000).toFixed(1)}s`;
console.log(`\n🧪 signup journey (email code${OVERLAY ? ", local overlay" : ""}) · ${email} · @${handle}\n`);

await step("prompt `signup` opens the handle step in the page", async () => {
  await ac.boot("prompt");
  await command("signup", /Pick your @handle/);
  if (new URL(page.url()).host !== new URL(CONFIG.baseURL).host) throw new Error(`left the site for ${page.url()}`);
  await reached("started", 5000);
  await ac.shot("signup-code-handle");
  return `in-page at ${elapsed()}`;
});

await step(`@${handle} checks free and is held`, async () => {
  await typeInto("#ac-signup-handle", handle);
  await page.waitForFunction(() => /is free/.test(document.querySelector("#ac-signup-check")?.textContent || ""), { timeout: 10_000 });
  await page.click("dialog.ac-signup .primary");
  await dialogTitled(/Where should we send a code/);
  await reached("handle_held", 5000);
  const hold = await page.evaluate(() => document.querySelector("dialog.ac-signup .hold")?.textContent);
  return hold || "held";
});

await step("the email step sends a six-digit code", async () => {
  await typeInto("#ac-signup-email", email);
  codeAsked = Date.now();
  await page.click("dialog.ac-signup .primary");
  const view = await dialogTitled(/Enter the code/, 20_000);
  await reached("code_sent", 5000);
  await ac.shot("signup-code-enter");
  return view.text.split("\n").find((l) => l.includes("Sent to")) || "sent";
});

await step("typing the mailed code signs in and claims the handle", async () => {
  const code = await waitForLetter(email, CODE, { since: codeAsked });
  await typeInto("#ac-signup-code", code); // six digits submit on their own
  await reached("verified", 30_000);
  await reached("completed", 30_000);
  await page.waitForFunction(() => !document.querySelector("dialog.ac-signup")?.open, { timeout: 20_000 });
  await page.waitForFunction(() => window.acUSER?.sub, { timeout: 30_000 });
  sub = await signedInAs();
  token = await otpToken();
  await ac.wait(1500);
  await ac.shot("signup-code-done");
  return `signed in as ${sub} at ${elapsed()}`;
});

await step("the handle API knows the new account", async () => {
  const res = await fetch(`${CONFIG.baseURL}/handle?for=${encodeURIComponent(sub)}`);
  const json = await res.json().catch(() => ({}));
  if (json.handle !== handle) throw new Error(`/handle?for= said ${res.status} ${JSON.stringify(json)}`);
  return `@${json.handle}`;
});

await step("logging out and back in by code returns the same account", async () => {
  // What the logout command does for a code session (bios.mjs).
  await page.evaluate(() => { localStorage.removeItem("ac-otp-session"); localStorage.removeItem("session-aesthetic"); });
  await ac.boot("prompt");
  if (await signedInAs()) throw new Error("still signed in after forgetting the session");
  await command("login", /Log in/);
  await typeInto("#ac-signup-email", email);
  codeAsked = Date.now();
  await page.click("dialog.ac-signup .primary");
  await dialogTitled(/Enter the code/, 20_000);
  const code = await waitForLetter(email, CODE, { since: codeAsked });
  await typeInto("#ac-signup-code", code);
  await page.waitForFunction(() => window.acUSER?.sub, { timeout: 45_000 });
  const again = await signedInAs();
  token = (await otpToken()) || token;
  if (again !== sub) throw new Error(`came back as ${again}, not ${sub}`);
  return `${again} at ${elapsed()}`;
});

// Cleanup runs whenever an account exists, even after a failed step.
if (!KEEP && sub) {
  broken = false;
  await step("delete-erase-and-forget-me locks the test account", async () => {
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
