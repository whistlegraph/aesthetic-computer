// login — keep Iris signed in to Fuser, with no human in the room.
//
//   await ensureSignedIn(cdp, { email: "iris@fuser.studio" })
//
// Runs in `setup`, OFF CAMERA, before every take. It is a no-op in the normal
// case (already signed in) and a full email-OTP login when it is not — so
// "a video is requested" now implies "make sure we are logged in first", which
// is the only version of this that survives being left alone.
//
// WHY THIS HAS TO EXIST. Fuser's session cookie is not written to disk: panda's
// Chrome `Cookies` DB is months stale, so the session lives only in the running
// browser's memory. Quit Chrome — a crash, a reboot, a `pkill` from
// prep-chrome — and Iris is logged out. Before this, a person had to walk over
// and type a code out of her inbox, which is not a thing an unattended renderer
// can do at 4am.
//
// THE ROUTE IS EMAIL OTP, not Google. `emailOTP()` is enabled server-side
// (services/auth/auth.ts) and the login dialog exposes it: email → Continue →
// a six-digit code is mailed → type it → in. Google's OAuth screen is a
// third-party page with its own bot defences; the OTP path is Fuser's own front
// door, and Iris owns the mailbox it mails.
//
// SELECTORS ARE LOCALE-PROOF ON PURPOSE. This runs before the screenplay's
// setLocale, so the dialog may still be in whatever language the last take used.
// Nothing here matches on words — the two inputs are found by their placeholders
// (`hello@fuser.studio` and `123456`, both hardcoded in LoginDialog.tsx and
// never translated), and the submit button is found by where it sits (the last
// button in the dialog), not by whether it reads "Continue" or "继续".
//
// The mailbox half is lib/otp-mail.py — read its header for why a UIDNEXT mark,
// and not a timestamp, is what proves a code is ours.

import { execFile } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const run = promisify(execFile);
const HERE = dirname(fileURLToPath(import.meta.url));
const OTP_MAIL = join(HERE, "otp-mail.py");
const PYTHON = process.env.CAPTUTOR_PYTHON || "/usr/bin/python3";

export const WORKSPACE = "https://app.fuser.studio/w/me";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// ── selectors ──────────────────────────────────────────────────────────────
// (See the header: placeholders, not labels — they are the same in every locale.)
const EMAIL_FIELD = 'input[placeholder^="hello@"]';
const OTP_FIELD = 'input[placeholder="123456"]';
const SIGNED_IN = '[aria-label="Create blank project"]';

// The gradient submit button. It is "Continue", then "Login", then in Chinese
// something else again — but it is always the last visible button in the dialog,
// and in the OTP step it is the only one (the Google button unmounts).
const SUBMIT = `js=(() => {
  const bs = [...document.querySelectorAll('button')]
    .filter((b) => b.offsetParent !== null && b.getBoundingClientRect().width > 0);
  return bs[bs.length - 1] || null;
})()`;

const SUBMIT_ENABLED = `(() => {
  const bs = [...document.querySelectorAll('button')]
    .filter((b) => b.offsetParent !== null && b.getBoundingClientRect().width > 0);
  const b = bs[bs.length - 1];
  return b && !b.disabled && b.getAttribute('aria-disabled') !== 'true';
})()`;

/// Where the login dialog has landed. One eval, no guessing.
const PAGE_STATE = `(() => {
  if (document.querySelector(${JSON.stringify(SIGNED_IN)})) return "in";
  if (document.querySelector(${JSON.stringify(OTP_FIELD)})) return "otp";
  if (document.querySelector(${JSON.stringify(EMAIL_FIELD)})) return "email";
  const txt = document.body ? document.body.innerText : "";
  // Turnstile expires if the dialog is left sitting, and the dialog then shows
  // an "Oops!" that no amount of typing gets you out of. It is a RELOAD state,
  // not a failure — this is exactly how we found panda: parked on
  // "We were unable to verify your humanity" with nobody there to press Refresh.
  if (/Oops!|verify your humanity|Try again|Refresh/i.test(txt)) return "stuck";
  if (/Login to Fuser|Sign in|Sign up to Fuser/i.test(txt)) return "email";
  return "unknown";
})()`;

/// The app password, from panda's ~/.config/captutor/iris.json (chmod 600) or
/// the environment. Never from the repo: this directory is inside the vault, and
/// a secret in a file that gets rsynced to two office minis is a secret twice.
export function irisCredentials() {
  const path = process.env.CAPTUTOR_IRIS_CONFIG
    || join(homedir(), ".config", "captutor", "iris.json");
  const file = existsSync(path) ? JSON.parse(readFileSync(path, "utf8")) : {};
  const appPassword = process.env.CAPTUTOR_IRIS_APP_PASSWORD || file.appPassword;
  if (!appPassword) {
    throw new Error(
      `no Gmail app password for Iris.\n` +
      `  put it in ${path} (chmod 600):\n` +
      `    { "email": "iris@fuser.studio", "appPassword": "…" }\n` +
      `  or set CAPTUTOR_IRIS_APP_PASSWORD.\n` +
      `  the source of truth is vault/fuser/iris-credentials.md`);
  }
  const email = file.email || "iris@fuser.studio";
  return {
    email,
    appPassword,                                    // never logged, never printed
    imapHost: file.imapHost || "imap.gmail.com",
    imapPort: String(file.imapPort || 993),
    imapUser: file.imapUser || email,
  };
}

/// Talk to the mailbox. The password goes in through the ENVIRONMENT, not argv:
/// argv is readable by every process on the machine (`ps`), and panda is shared.
async function mail(creds, args, { timeoutMs = 180000 } = {}) {
  const { stdout } = await run(PYTHON, [OTP_MAIL, ...args], {
    timeout: timeoutMs,
    encoding: "utf8",
    env: {
      ...process.env,
      IMAP_HOST: creds.imapHost,
      IMAP_PORT: creds.imapPort,
      IMAP_USER: creds.imapUser,
      IMAP_PASSWORD: creds.appPassword,
    },
  });
  return JSON.parse(stdout.trim().split("\n").pop());
}

/// A trusted click, with no drawn cursor.
///
/// cursor.mjs's clickOn glides a visible pointer across the screen because the
/// viewer has to see where the click landed. Nobody is watching this one — it
/// happens before `reel` rolls — so it skips the choreography and just presses.
async function tap(cdp, selector) {
  const { x, y } = await cdp.center(selector);
  await cdp.mouse("mouseMoved", x, y);
  await cdp.mouse("mousePressed", x, y);
  await cdp.mouse("mouseReleased", x, y);
  await sleep(120);
}

async function fill(cdp, selector, text) {
  await tap(cdp, selector);
  await cdp.type(text);
}

/// Is the workspace on screen? Cheap, and the answer in the normal case.
async function signedIn(cdp) {
  return !!(await cdp.eval(`!!document.querySelector(${JSON.stringify(SIGNED_IN)})`));
}

/// Wait for the app to commit to a state. A React app that is still mounting is
/// neither signed in nor signed out, and treating that moment as "logged out"
/// would mail Iris a code every single run.
async function settle(cdp, { timeoutMs = 15000 } = {}) {
  const deadline = Date.now() + timeoutMs;
  let last = "unknown";
  while (Date.now() < deadline) {
    last = await cdp.eval(PAGE_STATE).catch(() => "unknown");
    if (last !== "unknown") return last;
    await sleep(300);
  }
  return last;
}

/**
 * Make sure the filming browser is signed in as Iris. Returns
 * `{ signedIn: true, already: bool }`, or throws with what it saw.
 *
 * Safe to call before every take: when the session is alive this costs one eval.
 */
export async function ensureSignedIn(cdp, {
  email,
  timeoutMs = 240000,
  workspace = WORKSPACE,
  log = (m) => console.log(`  ${m}`),
} = {}) {
  if (await signedIn(cdp)) return { signedIn: true, already: true };

  const creds = irisCredentials();
  const who = email || creds.email;

  // Two attempts. The first can lose to an expired Turnstile — the widget dies
  // quietly on a page that has been open for hours, and the only cure is a fresh
  // load. A second attempt on a reloaded page is not a retry-until-it-works loop;
  // it is the one failure mode we know the shape of.
  for (let attempt = 1; attempt <= 2; attempt++) {
    // Always start from a fresh page: it clears a stuck "Oops!", and Turnstile
    // begins solving the moment the dialog mounts.
    await cdp.nav(workspace);
    let state = await settle(cdp);
    if (state === "in") return { signedIn: true, already: true };

    if (state === "stuck" || state === "unknown") {
      log(`login dialog was ${state} — reloading`);
      await cdp.nav(workspace);
      state = await settle(cdp);
      if (state === "in") return { signedIn: true, already: true };
    }

    log(`signing in as ${who} (attempt ${attempt}/2)`);

    if (state === "email") {
      await cdp.waitFor(`document.querySelector(${JSON.stringify(EMAIL_FIELD)})`);
      await fill(cdp, EMAIL_FIELD, who);

      // Turnstile gates the button, not the field: `isDisabled` includes
      // `!turnstileToken`. So the wait below is really "wait for Cloudflare to
      // decide we are human" — it is usually a couple of seconds, and if it
      // never comes the dialog will say so and we reload.
      try {
        await cdp.waitFor(SUBMIT_ENABLED, { timeoutMs: 45000 });
      } catch {
        log("Turnstile never released the button — retrying on a fresh page");
        continue;
      }

      // MARK THE MAILBOX FIRST. Everything after this instant is ours; anything
      // already in there is somebody else's code, or an old one of ours.
      const after = await mail(creds, ["mark"], { timeoutMs: 60000 });

      await tap(cdp, SUBMIT);

      // The OTP field appearing IS the receipt that the mail was sent — the
      // dialog only swaps to it when `account.login` resolved true.
      try {
        await cdp.waitFor(
          `document.querySelector(${JSON.stringify(OTP_FIELD)})`, { timeoutMs: 30000 });
      } catch {
        const txt = await cdp.eval("document.body.innerText").catch(() => "");
        log(`Continue did not open the code field — page says: ${JSON.stringify(txt.slice(0, 160))}`);
        continue;
      }
      log("code requested — watching the mailbox");

      const hit = await mail(creds, [
        "wait", "--after", JSON.stringify(after), "--wait", "150",
      ], { timeoutMs: 170000 });
      if (hit.error) throw new Error(`OTP mail: ${hit.error}`);
      log(`code arrived in ${hit.box} — ${JSON.stringify(hit.subject.replace(/\d{6}/, "······"))}`);

      await fill(cdp, OTP_FIELD, hit.code);
      await cdp.waitFor(SUBMIT_ENABLED, { timeoutMs: 10000 });
      await tap(cdp, SUBMIT);
    } else if (state === "otp") {
      // A half-finished dialog from an earlier run. We have no code for it
      // (the one that was mailed is gone), so start the whole thing over.
      log("dialog was waiting on a code we never got — starting over");
      await cdp.nav(workspace);
      continue;
    }

    // handleTokenSubmit reloads the page on success, so the workspace mounting
    // is the proof — not the dialog closing.
    try {
      await cdp.waitFor(
        `document.querySelector(${JSON.stringify(SIGNED_IN)})`,
        { timeoutMs: Math.min(60000, timeoutMs) });
      log(`signed in as ${who}`);
      return { signedIn: true, already: false };
    } catch {
      const txt = await cdp.eval("document.body.innerText").catch(() => "");
      if (/incorrect|Failed to verify/i.test(txt)) {
        log("Fuser rejected the code — asking for a new one");
        continue;   // a stale code slipped through, or it expired mid-type
      }
      throw new Error(`login did not land on the workspace. page says: ${txt.slice(0, 200)}`);
    }
  }

  throw new Error("could not sign in to Fuser after 2 attempts");
}
