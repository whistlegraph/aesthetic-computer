// signup-journey.test, 2026.10.08
// End-to-end proof of joining at the prompt (prompt.mjs `runJoin` →
// signup-flow.mjs `step`): a fresh browser types `signup`, then
// `handle @sut<stamp>` (held by /api/handle-hold), then `email …` and the
// six-digit `code …` Auth0 mails — every answer pre-fills the next command —
// and lands signed in with that handle, without a dialog or a redirect. Then
// it types `logout`, comes back with `login` → `email` → `code` as the same
// account, and finally puts the account through delete-erase-and-forget-me.
//
//   npm run test:signup:e2e                       # headless, production
//   AC_HEADED=1 npm run test:signup:e2e           # watch it
//   AC_SIGNUP_OVERLAY=1 npm run test:signup:e2e   # production, with this
//       checkout's boot/bios/prompt/signup files swapped in — proves the client
//       before it ships. AC_SIGNUP_WORKER_DIR=<dir with disk-worker-manifest.json
//       and its bundle> serves a freshly built disk worker too.
//   AC_SIGNUP_KEEP=1 ...                          # leave the test account alive
//
// The prompt draws into a canvas, so the test reads its text off the hidden
// #software-keyboard-input that bios mirrors it into. Codes are read off
// jasellite (./jasellite-mail.mjs). The browser counts as automated.
//
// Puppeteer needs a Chrome; with none downloaded, point it at the installed
// one: PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome".

import { existsSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { ACSession, CONFIG, scenario, report } from "./ac-harness.mjs";
import { waitForLetter } from "./jasellite-mail.mjs";

const KEEP = process.env.AC_SIGNUP_KEEP === "1";
const OVERLAY = process.env.AC_SIGNUP_OVERLAY === "1";
const WORKER_DIR = process.env.AC_SIGNUP_WORKER_DIR;
const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");

const stamp = Date.now().toString(36);
const email = `mail+signuptest${stamp}@aesthetic.computer`;
const handle = `sut${stamp}`; // ≤ 16 characters, letters and numbers
const CODE = /verification code is:\s*(\d{6})/;

const ac = await ACSession.open(); // a fresh profile: the incognito case
const page = ac.page;

if (OVERLAY) {
  const A = "system/public/aesthetic.computer";
  const local = Object.fromEntries(["boot.mjs", "bios.mjs", "disks/prompt.mjs", "lib/signup-flow.mjs",
    "lib/signup-model.mjs", "lib/auth0-otp.mjs"].map((f) => [`/aesthetic.computer/${f}`, join(ROOT, A, f)]));
  let workerBundle = null;
  if (WORKER_DIR) {
    const manifest = JSON.parse(readFileSync(join(WORKER_DIR, "disk-worker-manifest.json"), "utf8"));
    local["/aesthetic.computer/lib/disk-worker-manifest.json"] = join(WORKER_DIR, "disk-worker-manifest.json");
    workerBundle = join(WORKER_DIR, manifest.filename);
  }
  await page.setBypassServiceWorker(true);
  await page.setRequestInterception(true);
  page.on("request", (request) => {
    const url = new URL(request.url());
    let file = url.host === new URL(CONFIG.baseURL).host && local[url.pathname];
    // The page may name the worker bundle without reading the manifest.
    if (!file && WORKER_DIR && /\/lib\/disk\.worker\.[0-9a-f]+\.mjs$/.test(url.pathname)) file = workerBundle;
    if (!file || !existsSync(file)) return request.continue();
    request.respond({ status: 200, headers: { "cache-control": "no-store" },
      contentType: file.endsWith(".json") ? "application/json" : "text/javascript; charset=utf-8",
      body: readFileSync(file, "utf8") });
  });
}

const stages = [];
page.on("request", (request) => {
  if (!request.url().endsWith("/api/signup-track")) return;
  try { for (const s of JSON.parse(request.postData()).stages) if (!stages.includes(s)) stages.push(s); } catch {}
});
async function reached(stage, timeout = 30_000) {
  const deadline = Date.now() + timeout;
  while (!stages.includes(stage)) {
    if (Date.now() > deadline) throw new Error(`never reached "${stage}" (got ${stages.join(" → ") || "nothing"})`);
    await ac.wait(250);
  }
}
const promptText = () => page.evaluate(() => document.getElementById("software-keyboard-input")?.value ?? null);
async function promptIs(text, timeout = 30_000) {
  const deadline = Date.now() + timeout;
  let last;
  while (Date.now() < deadline) {
    last = await promptText().catch(() => null);
    if (last === text) return;
    await ac.wait(200);
  }
  throw new Error(`prompt never read "${text}" (last "${last}")`);
}
const ready = () => page.waitForFunction(() => window.preloaded === true, { timeout: 60_000 }).catch(() => {});
const signedInAs = () => page.evaluate(() => window.acUSER?.sub || null);
const otpToken = () => page.evaluate(() => { try { return JSON.parse(localStorage.getItem("ac-otp-session"))?.access || null; } catch { return null; } });

// Type a whole command at a fresh prompt. A stalled boot ignores keys, and
// Backspace on an empty prompt navigates, so retries reload instead.
async function command(text, expectPrompt) {
  for (let tries = 0; tries < 3; tries++) {
    if (tries) await ac.boot("prompt");
    await ready();
    await ac.wait(1200);
    await ac.focusPrompt();
    await ac.type(text);
    await ac.wait(300);
    await ac.press("Enter");
    try { return await promptIs(expectPrompt, 10_000); } catch {}
  }
  return promptIs(expectPrompt, 1);
}
// Answer a pre-filled command: the cursor sits at its end, so just type on.
async function answer(text) {
  await ac.type(text);
  await ac.wait(300);
  await ac.press("Enter");
}

let broken = false;
const step = (name, fn) => scenario(name, async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  try { expect(true, await fn()); } catch (error) { broken = true; throw error; }
});

let sub = null, token = null, asked = 0;
const t0 = Date.now();
const elapsed = () => `${((Date.now() - t0) / 1000).toFixed(1)}s`;
console.log(`\n🧪 signup journey (prompt${OVERLAY ? ", local overlay" : ""}) · ${email} · @${handle}\n`);

await step("`signup` pre-fills `handle @` at the prompt", async () => {
  await ac.boot("prompt");
  await command("signup", "handle @");
  await reached("started", 5000);
  if (await page.$("dialog.ac-signup[open]")) throw new Error("a dialog opened");
  await ac.shot("join-1-handle");
  return `at ${elapsed()}`;
});

await step(`\`handle @${handle}\` is held and pre-fills \`email \``, async () => {
  await answer(handle);
  await reached("handle_held", 15_000);
  await promptIs("email ");
  await ac.shot("join-2-email");
  return `held at ${elapsed()}`;
});

await step("`email …` sends a code and pre-fills `code `", async () => {
  asked = Date.now();
  await answer(email);
  await reached("code_sent", 20_000);
  await promptIs("code ");
  await ac.shot("join-3-code");
  return `at ${elapsed()}`;
});

await step("`code …` signs in and claims the handle", async () => {
  await answer(await waitForLetter(email, CODE, { since: asked }));
  await reached("verified", 30_000);
  await reached("completed", 30_000);
  await page.waitForFunction(() => window.acUSER?.sub, { timeout: 60_000 });
  sub = await signedInAs();
  token = await otpToken();
  await ac.wait(1500);
  await ac.shot("join-4-signed-in");
  return `${sub} at ${elapsed()}`;
});

await step("the handle API knows the new account", async () => {
  const json = await (await fetch(`${CONFIG.baseURL}/handle?for=${encodeURIComponent(sub)}`)).json().catch(() => ({}));
  if (json.handle !== handle) throw new Error(`/handle?for= said ${JSON.stringify(json)}`);
  return `@${json.handle}`;
});

await step("`logout` signs out", async () => {
  for (let tries = 0; tries < 3; tries++) {
    await ac.boot("prompt");
    await ready();
    await ac.wait(1500);
    await ac.focusPrompt();
    await answer("logout");
    const out = await page.waitForFunction(() => !localStorage.getItem("ac-otp-session"), { timeout: 10_000 }).then(() => true, () => false);
    if (out) break;
  }
  // logout reloads the page; poll across the reload until no one is signed in.
  const deadline = Date.now() + 45_000;
  while ((await signedInAs().catch(() => "reloading")) && Date.now() < deadline) await ac.wait(500);
  await ready();
  if (await signedInAs()) throw new Error("still signed in");
  await ac.shot("join-5-logged-out");
  return "signed out";
});

await step("`login` → `email` → `code` returns the same account", async () => {
  await command("login", "email ");
  asked = Date.now();
  await answer(email);
  await promptIs("code ", 20_000);
  await answer(await waitForLetter(email, CODE, { since: asked }));
  await page.waitForFunction(() => window.acUSER?.sub, { timeout: 60_000 });
  const again = await signedInAs();
  token = (await otpToken()) || token;
  if (again !== sub) throw new Error(`came back as ${again}, not ${sub}`);
  await ac.shot("join-6-returned");
  return `${again} at ${elapsed()}`;
});

if (!KEEP && sub) {
  broken = false;
  await step("delete-erase-and-forget-me locks the test account", async () => {
    if (!token) throw new Error("no token to delete with — account left alive");
    const res = await fetch(`${CONFIG.baseURL}/api/delete-erase-and-forget-me`, { method: "POST", headers: { Authorization: `Bearer ${token}` } });
    const json = await res.json().catch(() => ({}));
    if (res.status !== 200) throw new Error(`${res.status} ${JSON.stringify(json)}`);
    return `purge after ${json.purgeAfter || "?"}`;
  });
}

await ac.browser.close();
console.log(`\n  stages  ${stages.join(" → ")}`);
console.log(`  account ${email} · @${handle}${KEEP ? " (kept)" : ""}\n`);
process.exit(report());
