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
    const authHeaders = { "Access-Control-Allow-Origin": "https://aesthetic.computer",
      "Access-Control-Allow-Credentials": "true", "Access-Control-Allow-Headers": "content-type",
      "Access-Control-Allow-Methods": "POST, OPTIONS" };
    if (request.method() === "OPTIONS") return route.fulfill({ status: 204, headers: authHeaders });
    if (url.pathname === "/style.css") return route.fulfill({ contentType: "text/css", body: await readFile(new URL("../public/aesthetic.computer/style.css", import.meta.url), "utf8") });
    if (url.pathname === "/api/signup-track") {
      state.events.push(JSON.parse(request.postData()));
      return route.fulfill({ status: 204 });
    }
    if (url.pathname === "/api/give-status") {
      assert.equal(request.headers().authorization, "Bearer fixture-token");
      return route.fulfill(options.supportFailure ? { status: 503, json: {} }
        : { json: { monthlySupporter: options.supporter === true } });
    }
    if (url.pathname === "/api/signup-status") {
      state.requests.push(url.pathname);
      assert.equal(request.headers().authorization, "Bearer fixture-token");
      if (state.statusFailure) return route.fulfill({ status: 503, json: { message: "unavailable" } });
      if (request.method() === "POST") { state.resends++; return route.fulfill({ json: { sent: true } }); }
      return route.fulfill({ json: { verified: state.verified, handle: state.handle } });
    }
    if (url.pathname === "/api/handle-hold") return route.fulfill({ json: request.method() === "POST"
      ? { held: true, until: new Date(Date.now() + 600000).toISOString() } : { status: "free" } });
    if (url.pathname === "/passwordless/start") return route.fulfill({ headers: authHeaders, json: { sent: true } });
    if (url.pathname === "/co/authenticate") {
      if (JSON.parse(request.postData()).otp !== "123456")
        return route.fulfill({ status: 400, headers: authHeaders, json: { error: "access_denied", error_description: "Wrong email or verification code." } });
      return route.fulfill({ headers: authHeaders, json: { login_ticket: "fixture-ticket" } });
    }
    if (url.pathname === "/authorize") return route.fulfill({ contentType: "text/html", body:
      `<script>parent.postMessage({type:"authorization_response",response:{code:"fixture-code",state:${JSON.stringify(url.searchParams.get("state"))}}},"https://aesthetic.computer")</script>` });
    if (url.pathname === "/oauth/token") return route.fulfill({ headers: authHeaders, json: {
      access_token: "fixture-token", expires_in: 3600,
      id_token: `fixture.${Buffer.from(JSON.stringify({ sub: "auth0|fixture", email: "new@example.test" })).toString("base64url")}.fixture`,
    } });
    if (url.pathname === "/handle" && request.method() === "GET") return route.fulfill({ json: { handle: state.handle } });
    if (url.pathname === "/handle" && request.method() === "POST") {
      state.requests.push(JSON.parse(request.postData()));
      if (state.taken) return route.fulfill({ status: 500, json: { message: "taken" } });
      state.handle = JSON.parse(request.postData()).handle;
      return route.fulfill({ json: { handle: state.handle } });
    }
    if (url.pathname.endsWith(".mjs")) {
      const filename = url.pathname.split("/").at(-1);
      assert.ok(["signup-flow.mjs", "signup-model.mjs", "visit-model.mjs", "text.mjs", "auth0-otp.mjs"].includes(filename));
      return route.fulfill({ contentType: "text/javascript", body: await readFile(new URL(`../public/aesthetic.computer/lib/${filename}`, import.meta.url), "utf8") });
    }
    return route.fulfill({ contentType: "text/html", body: `<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1"><link rel="stylesheet" href="/style.css"><style>body{background:#cdc9a3;font:18px system-ui;margin:32px}button{font:inherit;padding:12px}</style><main><h1>AC test piece</h1><button id="join">I'm new</button></main><script type="module">
      import {createSignupFlow} from '/aesthetic.computer/lib/signup-flow.mjs';
      window.acSignup = createSignupFlow(window, document, {clientId: 'fixture-client'});
      window.auth = {getTokenSilently: async () => 'fixture-token'};
      window.addEventListener('ac:handle-created', () => sessionStorage.setItem('fixture:handle-created', 'yes'));
      document.querySelector('#join').onclick=()=>acSignup.${options.codeFlow ? "open" : "start"}('signup');
      window.ready = true;
    </script>` });
  });
  const page = await context.newPage();
  await page.goto("https://aesthetic.computer" + (options.path || "/laer-klokken"));
  await page.waitForFunction(() => window.ready);
  return { browser, context, page, state };
}

test("email-code signup reaches Laklok from home, keeps a starting piece, and recovers from a wrong code", async () => {
  for (const [path, destination, viewport] of [
    ["/", "/laklok", { width: 390, height: 844 }],
    ["/laklok", "/laklok", { width: 1280, height: 900 }],
    ["/notepat", "/notepat", { width: 390, height: 844 }],
  ]) {
    const f = await fixture({ codeFlow: true, path, viewport });
    try {
      const { page, state } = f;
      state.taken = false;
      await page.getByRole("button", { name: "I'm new" }).click();
      await page.getByRole("heading", { name: "Pick your @handle" }).waitFor();
      assert.match(await page.locator("dialog .description").textContent(), /Laklok/);
      await page.getByLabel("Handle", { exact: true }).fill("new.pal");
      await page.getByRole("button", { name: "Continue", exact: true }).click();
      await page.getByLabel("Email", { exact: true }).fill("new@example.test");
      await page.getByRole("button", { name: "Send code", exact: true }).click();
      if (path === "/") {
        await page.getByLabel("Code", { exact: true }).fill("000000");
        await page.getByRole("status").filter({ hasText: /that code doesn.t match/ }).waitFor();
        assert.equal(new URL(page.url()).pathname, "/");
      }
      await page.getByLabel("Code", { exact: true }).fill("123456");
      await page.getByRole("heading", { name: "You’re @new.pal", exact: true }).waitFor();
      assert.equal(await page.locator("dialog .description").textContent(), "Monthly support unlocks handle colors across AC.");
      await page.getByRole("button", { name: "Continue", exact: true }).click();
      await page.waitForURL(`https://aesthetic.computer${destination}`);
      await page.waitForFunction(() => sessionStorage.getItem('fixture:handle-created') === 'yes');
      assert.equal(state.handle, "new.pal");
      assert.equal(await page.evaluate(() => sessionStorage.getItem('ac:signup:v1')), null);
      assert.ok(state.events.some(event => event.stages.includes("completed")));
      assert.equal(await page.getByRole("dialog").count(), 0);
    } finally { await f.browser.close(); }
  }
});

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
    await page.getByRole("heading", { name: "You’re @new.pal2", exact: true }).waitFor();
    if (process.env.AC_SIGNUP_EVIDENCE_DIR) {
      await page.screenshot({ path: `${process.env.AC_SIGNUP_EVIDENCE_DIR}/support-mobile.png` });
      await page.setViewportSize({ width: 1280, height: 900 });
      await page.screenshot({ path: `${process.env.AC_SIGNUP_EVIDENCE_DIR}/support-desktop.png` });
    }
    await page.getByRole("button", { name: "Continue", exact: true }).click();
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

test("an account without a handle is asked once a day, sees names checked live, and closing stays on the piece", async () => {
  const f = await fixture();
  try {
    const { page, state } = f;
    state.verified = true; state.taken = false;
    assert.equal(await page.evaluate(() => acSignup.shouldAsk()), true);
    await page.evaluate(() => acSignup.resume(auth, {}, true));
    await page.getByRole("heading", { name: "Choose your @handle" }).waitFor();
    await page.getByLabel("Handle", { exact: true }).fill("new.pal");
    await page.locator("#ac-signup-check").filter({ hasText: "@new.pal is free." }).waitFor();
    await page.getByRole("button", { name: "Close", exact: true }).click();
    assert.equal(await page.getByRole("dialog").count(), 0);
    assert.equal(new URL(page.url()).pathname, "/laer-klokken");
    assert.equal(await page.evaluate(() => acSignup.shouldAsk()), false);
    assert.equal(await page.evaluate(() => sessionStorage.getItem("ac:signup:v1")), null);
    for (let i = 0; i < 40 && !state.events.some(row => row.stages.includes("handle_dismissed")); i++) await new Promise(r => setTimeout(r, 50));
    const dismissed = state.events.find(row => row.stages.includes("handle_dismissed"));
    assert.ok(dismissed);
    assert.equal(dismissed.mode, "handle");
    assert.ok(dismissed.stages.includes("handle_shown"));
    // Asked again a day later, the name is claimed and the support step follows as after any signup.
    await page.evaluate(() => localStorage.removeItem("ac:handle:asked"));
    assert.equal(await page.evaluate(() => acSignup.shouldAsk()), true);
    await page.evaluate(() => acSignup.resume(auth, {}, true));
    await page.getByLabel("Handle", { exact: true }).fill("new.pal");
    await page.getByRole("button", { name: "Create handle", exact: true }).click();
    await page.getByRole("heading", { name: "You’re @new.pal", exact: true }).waitFor();
    assert.equal(state.handle, "new.pal");
    await page.getByRole("button", { name: "Continue", exact: true }).click();
    await page.waitForURL("https://aesthetic.computer/laer-klokken");
    await page.waitForFunction(() => window.ready && sessionStorage.getItem("ac:signup:v1") === null);
    // An account that already has a handle is left alone: no dialog, no navigation.
    await page.evaluate(() => localStorage.removeItem("ac:handle:asked"));
    await page.evaluate(() => acSignup.resume(auth, {}, true));
    await page.waitForFunction(() => sessionStorage.getItem("ac:signup:v1") === null);
    assert.equal(await page.getByRole("dialog").count(), 0);
    assert.equal(new URL(page.url()).pathname, "/laer-klokken");
    // A failed status check on an ordinary session is silent.
    state.handle = null; state.statusFailure = true;
    await page.evaluate(() => acSignup.resume(auth, {}, true));
    await page.waitForFunction(() => sessionStorage.getItem("ac:signup:v1") === null);
    assert.equal(await page.getByRole("dialog").count(), 0);
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

test("support is optional after signup, skips existing supporters and failures, and respects privacy", async () => {
  for (const options of [{ supporter: true }, { supportFailure: true }, { action: "color" }, { action: "escape", optOut: true }, { action: "close" }]) {
    const f = await fixture(options);
    try {
      f.state.verified = true; f.state.taken = false;
      const { page, state } = f;
      await page.getByRole("button", { name: "I'm new" }).click();
      await page.evaluate(() => acSignup.resume(auth, {}));
      await page.getByLabel("Handle", { exact: true }).fill("new.pal");
      // Make the return navigation observable even when signup began on the destination.
      await page.evaluate(() => history.replaceState({}, "", "/prompt"));
      await page.getByRole("button", { name: "Create handle", exact: true }).click();
      if (options.action) {
        await page.getByRole("heading", { name: "You’re @new.pal", exact: true }).waitFor();
        if (options.action === "color") await page.getByRole("button", { name: "Color my handle" }).click();
        else if (options.action === "escape") await page.keyboard.press("Escape");
        else await page.getByRole("button", { name: "Close", exact: true }).click();
      }
      await page.waitForURL(`https://aesthetic.computer${options.action === "color" ? "/handle?source=signup" : "/laer-klokken"}`);
      await page.waitForFunction(() => sessionStorage.getItem("ac:signup:v1") === null);
      assert.equal(await page.evaluate(() => sessionStorage.getItem("ac:signup:v1")), null);
      if (options.optOut) assert.equal(state.events.length, 0);
      else {
        assert.ok(state.events.some(row => row.stages.includes("completed")));
        assert.equal(state.events.some(row => row.stages.includes("support_shown")), !!options.action);
        assert.equal(state.events.some(row => row.stages.includes("support_clicked")), options.action === "color");
      }
    } finally { await f.browser.close(); }
  }
});
