import test from "node:test";
import assert from "node:assert/strict";
import { readFile, mkdir } from "node:fs/promises";
import { chromium } from "playwright";

async function fixture(options = {}) {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  const context = await browser.newContext({ viewport: options.viewport || { width: 1280, height: 900 } });
  if (options.optOut) await context.addInitScript(() => Object.defineProperty(navigator, "globalPrivacyControl", { value: true }));
  const state = { verified: false, handle: null, taken: true, statusFailure: false, requests: [], events: [], resends: 0 };
  await context.route("**/*", async route => {
    const request = route.request(), url = new URL(request.url());
    if (url.pathname === "/style.css") return route.fulfill({ contentType: "text/css", body: await readFile(new URL("../public/aesthetic.computer/style.css", import.meta.url), "utf8") });
    if (url.pathname === "/api/signup-track") {
      state.events.push(JSON.parse(request.postData()));
      return route.fulfill({ status: 204 });
    }
    if (url.pathname === "/api/signup-status") {
      state.requests.push(url.pathname);
      assert.equal(request.headers().authorization, "Bearer fixture-token");
      if (state.statusFailure) return route.fulfill({ status: 503, json: { message: "unavailable" } });
      if (request.method() === "POST") { state.resends++; return route.fulfill({ json: { sent: true } }); }
      return route.fulfill({ json: { verified: state.verified, handle: state.handle } });
    }
    if (url.pathname === "/handle" && request.method() === "POST") {
      state.requests.push(JSON.parse(request.postData()));
      if (state.taken) return route.fulfill({ status: 500, json: { message: "taken" } });
      state.handle = JSON.parse(request.postData()).handle;
      return route.fulfill({ json: { handle: state.handle } });
    }
    if (url.pathname.endsWith(".mjs")) {
      const filename = url.pathname.split("/").at(-1);
      assert.ok(["signup-flow.mjs", "signup-model.mjs", "visit-model.mjs", "text.mjs"].includes(filename));
      return route.fulfill({ contentType: "text/javascript", body: await readFile(new URL(`../public/aesthetic.computer/lib/${filename}`, import.meta.url), "utf8") });
    }
    return route.fulfill({ contentType: "text/html", body: `<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1"><link rel="stylesheet" href="/style.css"><style>body{background:#cdc9a3;font:18px system-ui;margin:32px}button{font:inherit;padding:12px}</style><main><h1>AC test piece</h1><button id="join">I'm new</button></main><script type="module">
      import {createSignupFlow} from '/aesthetic.computer/lib/signup-flow.mjs';
      window.acSignup = createSignupFlow(window, document);
      window.auth = {getTokenSilently: async () => 'fixture-token'};
      window.addEventListener('ac:handle-created', () => sessionStorage.setItem('fixture:handle-created', 'yes'));
      document.querySelector('#join').onclick=()=>acSignup.start('signup');
      window.ready = true;
    </script>` });
  });
  const page = await context.newPage();
  await page.goto("https://aesthetic.computer/laer-klokken");
  await page.waitForFunction(() => window.ready);
  return { browser, context, page, state };
}

test("signup survives the redirect, verifies email, handles a taken name and returns to the original piece", async () => {
  const f = await fixture();
  try {
    const { page, state } = f;
    await page.evaluate(() => { acSignup.remember(location.href); history.pushState({}, '', '/prompt'); });
    await page.getByRole("button", { name: "I'm new" }).click();
    await page.goto("https://aesthetic.computer/?code=private-code&state=private-state");
    await page.waitForFunction(() => window.ready);
    await page.evaluate(async () => { acSignup.track('auth_returned'); await acSignup.resume(auth, { email: 'private@example.com' }); });
    await page.getByRole("heading", { name: "Check your email" }).waitFor();
    await page.getByRole("button", { name: "Resend email" }).click();
    await page.getByRole("status").filter({ hasText: "Verification email sent" }).waitFor();
    assert.equal(state.resends, 1);
    assert.equal(await page.getByRole("button", { name: "Resend email" }).isDisabled(), true);
    state.verified = true;
    await page.getByRole("button", { name: "I’ve verified my email" }).click();
    await page.getByRole("heading", { name: "Choose your @handle" }).waitFor();
    await page.getByLabel("Handle", { exact: true }).fill("@new.pal");
    await page.getByRole("button", { name: "Create handle", exact: true }).click();
    await page.getByRole("status").filter({ hasText: "That handle is taken" }).waitFor();
    state.taken = false;
    await page.getByLabel("Handle", { exact: true }).fill("new.pal2");
    if (process.env.AC_SIGNUP_EVIDENCE_DIR) {
      await mkdir(process.env.AC_SIGNUP_EVIDENCE_DIR, { recursive: true });
      await page.screenshot({ path: `${process.env.AC_SIGNUP_EVIDENCE_DIR}/handle-desktop.png` });
      await page.setViewportSize({ width: 390, height: 844 });
      await page.screenshot({ path: `${process.env.AC_SIGNUP_EVIDENCE_DIR}/handle-mobile.png` });
    }
    await page.getByRole("button", { name: "Create handle", exact: true }).click();
    await page.waitForURL("https://aesthetic.computer/laer-klokken");
    assert.equal(await page.evaluate(() => sessionStorage.getItem('ac:signup:v1')), null);
    assert.equal(await page.evaluate(() => sessionStorage.getItem('fixture:handle-created')), 'yes');
    const completed = state.events.find(row => row.stages.includes("completed"));
    assert.ok(completed);
    for (const stage of ["started", "auth_returned", "verification_shown", "verification_resent", "verified", "handle_shown", "handle_failed", "completed"])
      assert.ok(completed.stages.includes(stage), stage);
    assert.equal(new Set(state.events.map(row => row.id)).size, 1);
    assert.doesNotMatch(JSON.stringify(state.events), /private|new\.pal|fixture-token/);
    assert.ok(state.events.every(row => row.automated));
  } finally { await f.browser.close(); }
});

test("privacy opt-out retains navigation without sending funnel events; status failures are recoverable", async () => {
  const f = await fixture({ optOut: true });
  try {
    const { page, state } = f;
    await page.getByRole("button", { name: "I'm new" }).click();
    state.statusFailure = true;
    await page.evaluate(() => acSignup.resume(auth, {}));
    await page.getByRole("heading", { name: "Finish signing up" }).waitFor();
    state.statusFailure = false; state.verified = true;
    await page.getByRole("button", { name: "Try again" }).click();
    await page.getByRole("heading", { name: "Choose your @handle" }).waitFor();
    assert.equal(state.events.length, 0);
    assert.equal(await page.evaluate(() => JSON.parse(sessionStorage.getItem('ac:signup:v1')).id), null);
    await page.getByLabel("Handle", { exact: true }).fill("bad name");
    await page.getByRole("button", { name: "Create handle", exact: true }).click();
    assert.ok(!state.requests.some(row => typeof row === "object"));
    await page.getByRole("button", { name: "Close", exact: true }).click();
    assert.equal(await page.getByRole("dialog").count(), 0);
  } finally { await f.browser.close(); }
});

test("ordinary and expired sessions do not open onboarding, and cancellation stops verification polling", async () => {
  const f = await fixture();
  try {
    const { page, state } = f;
    await page.evaluate(() => acSignup.resume(auth, {}));
    assert.equal(state.requests.length, 0);
    await page.evaluate(() => sessionStorage.setItem('ac:signup:v1', JSON.stringify({at: Date.now()-86400001, mode:'signup',stages:[]})));
    await page.evaluate(() => acSignup.resume(auth, {}));
    assert.equal(state.requests.length, 0);
    await page.clock.install();
    await page.getByRole("button", { name: "I'm new" }).click();
    await page.evaluate(() => acSignup.resume(auth, {}));
    await page.getByRole("heading", { name: "Check your email" }).waitFor();
    await page.getByRole("button", { name: "Close", exact: true }).click();
    const count = state.requests.length;
    await page.clock.runFor(31000);
    assert.equal(state.requests.length, count);
  } finally { await f.browser.close(); }
});

test("closing while account status is in flight does not reopen onboarding", async () => {
  const f = await fixture();
  try {
    const { page } = f;
    await page.getByRole("button", { name: "I'm new" }).click();
    await page.evaluate(async () => {
      let release;
      const delayedAuth = {getTokenSilently: () => new Promise(resolve => { release = resolve; })};
      const pending = acSignup.resume(delayedAuth, {});
      acSignup.close();
      release('fixture-token');
      await pending;
    });
    assert.equal(await page.getByRole("dialog").count(), 0);
  } finally { await f.browser.close(); }
});
