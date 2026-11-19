// onboarding-journey.test, 2026.11.18
// The door a stranger meets today, start to finish, on film. Since 2026-10-20
// every signup goes through the hosted page (boot.mjs, `acLOGIN`), so this is
// the default path with no flags: a fresh browser types `signup` at the prompt,
// creates an account as mail+signuptest<stamp>@aesthetic.computer, comes back
// to the pixel `join` piece, opens the link in the verification letter, claims
// a throwaway @handle (sut<stamp>), passes the monthly-support offer and lands
// in Laklok signed in. A reload proves the session holds, and then the account is put
// through delete-erase-and-forget-me.
//
//   npm run test:onboarding:e2e                 # headless, production
//   AC_HEADED=1 npm run test:onboarding:e2e     # watch it
//   AC_FILM=0 ...                               # no video
//   AC_SIGNUP_KEEP=1 ...                        # leave the test account alive
//
// The film is cut from one clip per tab (prompt → hosted page → join, the
// letter's tab, then join → Laklok → prompt), skips the wait for the letter,
// and is encoded by ffmpeg into
// __screens__/onboarding-journey/onboarding-journey.mp4. Screenshots of each
// milestone land beside it.
//
// The letter is read off jasellite (./jasellite-mail.mjs). The browser counts
// as automated, so the funnel files these attempts under automated.
//
// Puppeteer needs a Chrome; with none downloaded, point it at the installed
// one: PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome".

import { randomBytes } from "node:crypto";
import { execFile } from "node:child_process";
import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { promisify } from "node:util";
import { ACSession, CONFIG, scenario, report } from "./ac-harness.mjs";
import { waitForLetter } from "./jasellite-mail.mjs";

const KEEP = process.env.AC_SIGNUP_KEEP === "1";
const FILM = process.env.AC_FILM !== "0";
const FFMPEG = process.env.AC_FFMPEG || "ffmpeg";
const NAME = "onboarding-journey";
const OUT = join(CONFIG.shotDir, NAME);

const stamp = Date.now().toString(36);
const email = `mail+signuptest${stamp}@aesthetic.computer`;
const handle = `sut${stamp}`; // throwaway; ≤ 16 characters, letters and numbers
const password = `Ac-${randomBytes(12).toString("base64url")}!9`;
const base = new URL(CONFIG.baseURL);

// ─── the browser ─────────────────────────────────────────────────────────
rmSync(join(OUT, "reel"), { recursive: true, force: true });
mkdirSync(join(OUT, "reel"), { recursive: true });
const ac = await ACSession.open(); // a fresh profile: the incognito case
const page = ac.page;
const shot = (name) => ac.shot(`${NAME}/${name}`);
// The harness turns on acDEBUG for the prompt's test hook, which this journey
// doesn't read. Debug also points Laklok's chat at a local dev port, so the
// newcomer's first room would sit on "Connecting…". A stranger has no flag.
await page.evaluateOnNewDocument(() => { window.acDEBUG = false; });
// Laklok's conversation runs in the piece's worker, out of reach of the
// page's network events, so read what the chat library says: which server it
// asked for, and whether it has been failing to reach it.
const chat = { host: null, failures: 0 };
page.on("console", (message) => {
  const text = message.text();
  const asked = text.match(/Chat connect: chat-clock → (\S+)/);
  if (asked) { chat.host = asked[1]; chat.failures = 0; }
  else if (chat.host && /connection failed, retrying/.test(text)) chat.failures++;
});

// ─── the film ────────────────────────────────────────────────────────────
// Chrome's screencast hands over a JPEG whenever the tab repaints. Frames are
// kept on disk with their timestamps and encoded once, at the end, so filming
// costs the journey nothing. (Puppeteer's own recorder encodes as it goes and
// its stop() can stall for minutes on a small machine.)
const REEL = join(OUT, "reel");
const frames = []; // { file, at } in seconds, or { cut: true } between clips
let camera = null, filming = FILM;
async function cut() {
  const session = camera;
  camera = null;
  if (!session) return;
  await session.send("Page.stopScreencast").catch(() => {});
  await session.detach().catch(() => {});
  frames.push({ cut: true });
}
async function roll(target) {
  if (!filming) return;
  await cut();
  try {
    const session = await target.createCDPSession();
    session.on("Page.screencastFrame", ({ data, metadata, sessionId }) => {
      const file = join(REEL, `${String(frames.length).padStart(6, "0")}.jpg`);
      writeFileSync(file, Buffer.from(data, "base64"));
      frames.push({ file, at: metadata.timestamp });
      session.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
    });
    await session.send("Page.startScreencast", { format: "jpeg", quality: 80, everyNthFrame: 2 });
    camera = session;
  } catch (error) {
    filming = false;
    console.warn(`  ⚠️  not filming: ${error.message}`);
  }
}
async function develop() {
  await cut();
  const shots = frames.filter((frame) => frame.file);
  if (!shots.length) return null;
  // Each frame holds until the next one; a clip's last frame holds a second.
  const lines = [];
  frames.forEach((frame, i) => {
    if (!frame.file) return;
    const next = frames[i + 1];
    lines.push(`file '${frame.file}'`, `duration ${(next?.file ? Math.max(0.01, next.at - frame.at) : 1).toFixed(3)}`);
  });
  lines.push(`file '${shots.at(-1).file}'`); // the concat demuxer drops a final duration without it
  const list = join(REEL, "frames.txt"), film = join(OUT, `${NAME}.mp4`);
  writeFileSync(list, lines.join("\n") + "\n");
  await promisify(execFile)(FFMPEG, ["-y", "-loglevel", "error", "-f", "concat", "-safe", "0", "-i", list,
    "-vf", "fps=30,scale=trunc(iw/2)*2:trunc(ih/2)*2,format=yuv420p", "-c:v", "libx264", "-preset", "veryfast",
    "-movflags", "+faststart", film], { timeout: 600_000 });
  rmSync(REEL, { recursive: true, force: true });
  return film;
}

// ─── the funnel and the dialog ───────────────────────────────────────────
const stages = [];
page.on("request", (request) => {
  if (!request.url().endsWith("/api/signup-track")) return;
  try {
    const event = JSON.parse(request.postData());
    for (const stage of event.stages) if (!stages.includes(stage)) stages.push(stage);
  } catch {}
});
// Every address the main tab visits, for the summary and for failures.
const trail = [];
page.on("framenavigated", (frame) => {
  if (frame !== page.mainFrame()) return;
  const url = new URL(frame.url()), place = url.host === base.host ? url.pathname : url.host + url.pathname;
  if (trail.at(-1) !== place) trail.push(place);
});
async function reached(stage, timeout = 30_000) {
  const deadline = Date.now() + timeout;
  while (!stages.includes(stage)) {
    if (Date.now() > deadline) throw new Error(`signup never reached "${stage}" (got ${stages.join(" → ") || "nothing"})`);
    await ac.wait(250);
  }
}
// The join piece draws the form; the dialog underneath still holds its words
// and its real inputs and buttons (lib/signup-canvas.mjs).
const dialog = () => page.evaluate(() => {
  const d = document.querySelector("dialog.ac-signup");
  return d?.open ? { title: d.querySelector("h1")?.textContent || "", status: d.querySelector("[role=status]")?.textContent || "",
    check: d.querySelector(".check")?.textContent || "" } : null;
}).catch(() => null);
async function dialogTitled(pattern, timeout = 20_000) {
  const deadline = Date.now() + timeout;
  let last;
  while (Date.now() < deadline) {
    last = await dialog();
    if (last && pattern.test(last.title)) return last;
    await ac.wait(200);
  }
  throw new Error(`expected a dialog titled ${pattern}, saw ${last ? `"${last.title}" (${last.status})` : "none"}`);
}
const press = (label) => page.evaluate((label) => {
  const button = [...document.querySelectorAll("dialog.ac-signup button")].find((b) => b.textContent.trim() === label);
  button?.click();
  return !!button;
}, label);
const auth0Complaint = () => page.evaluate(() =>
  [...document.querySelectorAll("[id*=error],.ulp-input-error-message,[class*=error]")].map((e) => e.innerText).filter(Boolean).join(" | ")).catch(() => "");
const signedIn = () => page.evaluate(() => ({ sub: window.acUSER?.sub || null, handle: window.acUSER?.handle || null })).catch(() => ({}));

// Each step depends on the one before, so the first failure skips the rest.
let broken = false;
const step = (name, fn) => scenario(name, async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  try { expect(true, await fn()); } catch (error) {
    broken = true;
    await shot(`failed-${stages.at(-1) || "start"}`).catch(() => {});
    throw error;
  }
});

// The dialog is invisible; the join piece is what a person sees. Each form
// notes which piece was on screen, and one scenario judges them after the claim.
const drawn = [];
const noteScreen = async (form) => drawn.push({ form, path: await page.evaluate(() => location.pathname).catch(() => "?") });

let sub = null, hosted = false; // hosted: the handle was picked on the sign-in page
const t0 = Date.now();
const elapsed = () => `${((Date.now() - t0) / 1000).toFixed(1)}s`;
console.log(`\n🧪 onboarding journey · ${email} · @${handle}\n`);

await step("prompt `signup` leaves for the hosted create-account page", async () => {
  await ac.boot("prompt");
  await roll(page);
  await page.waitForFunction(() => window.preloaded === true, { timeout: 60_000 }).catch(() => {});
  await ac.wait(1200);
  await shot("01-prompt");
  await ac.focusPrompt();
  await ac.type("signup");
  await ac.wait(300);
  await ac.press("Enter");
  await page.waitForSelector("#email", { timeout: 30_000 });
  if (!/\/u\/signup/.test(page.url())) throw new Error(`landed on ${page.url()} instead of the signup screen`);
  await reached("started", 5000);
  await shot("02-hosted-signup");
  return `${new URL(page.url()).host}/u/signup at ${elapsed()}`;
});

await step("a new email + password comes back to AC and asks for verification", async () => {
  await page.type("#email", email, { delay: 15 });
  // Identifier-first: the address on one screen, the password on the next.
  if (!(await page.$("#password"))) {
    await Promise.all([
      page.waitForSelector("#password", { timeout: 30_000 }),
      page.click("button[type=submit]"),
    ]).catch(async (error) => {
      const why = await auth0Complaint();
      throw new Error(why ? `Auth0 refused the address: ${why}` : `no password screen after the address: ${error.message}`);
    });
  }
  await page.type("#password", password, { delay: 15 });
  await shot("03-hosted-password");
  await Promise.all([
    page.waitForFunction((host) => location.host === host || /\/u\/custom-prompt\//.test(location.pathname), { timeout: 45_000 }, base.host),
    page.click("button[type=submit]"),
  ]).catch(async (error) => {
    const why = await auth0Complaint();
    throw new Error(why ? `Auth0 refused: ${why}` : error.message);
  });
  // The hosted page asks for the handle itself once ask-handle is bound
  // (backend/auth0-forms). A taken name is refused in AC's words, there.
  if (new URL(page.url()).host !== base.host) {
    hosted = true;
    const field = "input[name=handle]";
    await page.waitForSelector(field, { visible: true, timeout: 20_000 });
    await ac.wait(800);
    await shot("03b-hosted-handle");
    await page.type(field, "jeffrey", { delay: 40 });
    await page.keyboard.press("Enter");
    await page.waitForFunction(() => /@jeffrey\s+is taken/.test(document.body.innerText), { timeout: 20_000 })
      .catch(() => { throw new Error("the hosted form did not refuse @jeffrey"); });
    await ac.wait(1200);
    await shot("03c-hosted-handle-taken");
    await page.click(field, { clickCount: 3 });
    await page.keyboard.press("Backspace");
    await page.type(field, handle, { delay: 40 });
    await ac.wait(500);
    await shot("03d-hosted-handle-free");
    await Promise.all([
      page.waitForFunction((host) => location.host === host, { timeout: 45_000 }, base.host),
      page.keyboard.press("Enter"),
    ]).catch(async () => { throw new Error(`the hosted form kept @${handle}: ${(await page.evaluate(() => document.body.innerText)).replace(/\s+/g, " ").slice(0, 160)}`); });
  }
  await reached("auth_returned");
  await reached("verification_shown");
  await dialogTitled(/Check your email/);
  sub = (await page.evaluate(async () => (await window.auth0Client?.getUser())?.sub)) || null;
  await ac.wait(2000); // let the join piece draw
  await noteScreen("Check your email");
  await shot("04-check-your-email");
  return `${sub || "?"}${hosted ? `, @${handle} picked on the hosted page,` : ""} on ${new URL(page.url()).pathname} at ${elapsed()}`;
});

await step("the verification letter arrives and its link verifies", async () => {
  await cut(); // the wait for the letter is not worth filming
  const ticket = await waitForLetter(email, /https:\/\/[^\s"'<>]+email-verification\?ticket=[^\s"'<>]+/);
  const tab = await ac.browser.newPage();
  await tab.setViewport(page.viewport());
  await roll(tab);
  // Auth0 verifies the ticket and redirects to AC with the outcome in the
  // query: ?success=true&message=Your email was verified…
  const outcomes = [];
  tab.on("framenavigated", (frame) => { if (frame === tab.mainFrame()) outcomes.push(frame.url()); });
  await tab.goto(ticket, { waitUntil: "domcontentloaded", timeout: 45_000 });
  await ac.wait(3000);
  await tab.screenshot({ path: join(OUT, "05-verified-tab.png") });
  await cut();
  await tab.close();
  const landed = outcomes.map((u) => new URL(u)).find((u) => u.searchParams.has("success"));
  if (landed?.searchParams.get("success") !== "true")
    throw new Error(`verification did not succeed: ${landed?.searchParams.get("message") || outcomes.at(-1)}`);
  return `letter at ${elapsed()} · "${landed.searchParams.get("message")}"`;
});

await step("back on the first tab, the verified account moves on", async () => {
  await page.bringToFront();
  await roll(page);
  await ac.wait(800);
  // The page checks every fifteen seconds while it is visible, so a slow
  // letter can find the handle form already open and the button gone.
  const pressed = await press("I’ve verified my email");
  if (hosted) {
    // Already named: verifying is all that was left, and AC moves on.
    await reached("completed", 30_000);
    return `${pressed ? "pressed “I’ve verified my email”" : "the page noticed by itself"}; no handle form, at ${elapsed()}`;
  }
  if (!pressed && !stages.includes("handle_shown")) throw new Error("no verified button in the dialog, and no handle form either");
  await reached("handle_shown", 20_000);
  await dialogTitled(/Choose your @handle/);
  await page.waitForSelector("#ac-signup-handle", { timeout: 10_000 });
  return `${pressed ? "pressed “I’ve verified my email”" : "the page noticed by itself"} at ${elapsed()}`;
});

await step(`@${handle} checks free and is claimed`, async () => {
  if (hosted) return "already claimed on the hosted page";
  await page.type("#ac-signup-handle", handle, { delay: 40 });
  await page.waitForFunction(() => /is free/.test(document.querySelector("#ac-signup-check")?.textContent || ""), { timeout: 10_000 });
  await ac.wait(600);
  await noteScreen("Choose your @handle");
  await shot("06-handle-free");
  if (!(await press("Create handle"))) throw new Error("no Create handle button in the dialog");
  await reached("completed", 30_000);
  return `at ${elapsed()}`;
});

await scenario("the join piece draws each form", async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  for (const { form, path } of drawn)
    expect(path === "/join", path === "/join" ? `“${form}” drawn by join` : `“${form}” is open, but the screen shows ${path} and the form is invisible`);
});

// The offer is optional by design: a slow /api/give-status skips it
// (signup-flow.mjs, handleCreated) and the newcomer goes straight on. When it
// does open, the join piece has to be the one on screen, because the dialog
// itself is invisible (lib/signup-canvas.mjs). This check doesn't stop the
// journey: Continue still works on a dialog nobody can see.
let offered = false;
await scenario("the support offer is on screen when it opens", async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  const deadline = Date.now() + 15_000;
  let path = "";
  while (Date.now() < deadline && !offered) {
    path = await page.evaluate(() => location.pathname).catch(() => "");
    if (/^You’re @/.test((await dialog())?.title || "")) offered = true;
    else if (path === "/laklok") break;
    else await ac.wait(200);
  }
  if (!offered) return expect(path === "/laklok", path === "/laklok" ? "no offer; straight on to Laklok" : `no offer and no Laklok; the tab is on ${path}`);
  await ac.wait(1500);
  path = await page.evaluate(() => location.pathname);
  await shot("07-support-offer");
  expect(path === "/join", path === "/join" ? `“You’re @${handle}” drawn by join at ${elapsed()}`
    : `“You’re @${handle}” is open, but the screen shows ${path} and the offer is invisible`);
});

// Not a link in the chain either: an account stranded short of Laklok still
// has a handle and a session, and the steps after this one check those.
await scenario("the newcomer lands in Laklok, signed in", async (expect) => {
  if (broken) return expect(false, "skipped — an earlier step failed");
  if (offered && !(await press("Continue"))) {
    const path = await page.evaluate(() => location.pathname).catch(() => "?");
    await shot("08-stranded");
    return expect(false, `the support offer closed by itself and left the newcomer on ${path}`);
  }
  await page.waitForFunction(() => location.pathname === "/laklok", { timeout: 20_000 });
  await page.waitForFunction(() => window.acUSER?.sub, { timeout: 45_000 });
  const who = await signedIn();
  expect(who.sub === sub, `/laklok as ${who.sub} at ${elapsed()}`);
  const deadline = Date.now() + 20_000;
  while (!chat.host && Date.now() < deadline) await ac.wait(250);
  await ac.wait(6000); // time to connect, or to fail and retry
  await shot("08-laklok");
  const live = chat.host === "chat-clock.aesthetic.computer" && chat.failures === 0;
  expect(live, live ? `the conversation reached ${chat.host}` : `the conversation asked ${chat.host || "no server"} and failed ${chat.failures} time(s)`);
});

await step("the handle API knows the new account", async () => {
  const res = await fetch(`${CONFIG.baseURL}/handle?for=${encodeURIComponent(sub)}`);
  const json = await res.json().catch(() => ({}));
  if (json.handle !== handle) throw new Error(`/handle?for= said ${res.status} ${JSON.stringify(json)}`);
  return `@${json.handle}`;
});

await step("a reload of the prompt is still signed in, with the handle", async () => {
  await ac.boot("prompt");
  await page.waitForFunction((handle) => window.acUSER?.handle?.replace(/^@/, "") === handle, { timeout: 45_000 }, handle)
    .catch(async () => { throw new Error(`the prompt came back as ${JSON.stringify(await signedIn())}`); });
  await ac.wait(2500);
  await shot("09-prompt-signed-in");
  return `@${handle} at ${elapsed()}`;
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

// Chrome goes first: a headless tab still painting starves the encode.
await cut();
await ac.browser.close();
const film = await develop().catch((error) => { console.warn(`  ⚠️  film not developed: ${error.stderr || error.message}`); return null; });
console.log(`\n  stages  ${stages.join(" → ")}`);
console.log(`  trail   ${trail.join(" → ")}`);
console.log(`  account ${email} · @${handle}${KEEP ? " (kept)" : ""}`);
if (film) console.log(`  🎞️  ${film}`);
console.log("");
process.exit(report());
