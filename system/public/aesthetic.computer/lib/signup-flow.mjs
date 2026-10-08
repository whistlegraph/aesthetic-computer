import { SIGNUP_TTL_MS, SIGNUP_ERRORS, signupReturnPath, signupSource, signupID } from "./signup-model.mjs";
import { automatedVisit, visitReferrer } from "./visit-model.mjs";
import { validateHandle } from "./text.mjs";

const KEY = "ac:signup:v1";

// Authentication, verification and a normal handle form; content stays local.
// The collector receives only a per-attempt UUID and allowlisted milestones.
//
// Two doors. `open()` is the in-page one: pick a @handle (held for ten minutes
// by /api/handle-hold), then an emailed six-digit code through auth0-otp.mjs —
// no redirect, no password, and none of the hosted page's Turnstile check,
// which loops forever on some networks. The resulting tokens become a hosted
// `session-aesthetic`, the same shape an embedding host hands boot. `redirect`
// is the old Universal Login door; the in-page one falls back to it whenever
// auth0-otp reports a tenant fault rather than a person's mistake.
export function createSignupFlow(win, doc, { clientId, redirect, social, socials = [] } = {}) {
  let attempt, previousPiece = signupReturnPath(win.location.href, win.location.origin);
  let dialog, timer, auth, checking = false, generation = 0, otp, otpModule, signedIn = null;
  const disabled = () => win.navigator.doNotTrack === "1" || win.doNotTrack === "1" ||
    win.navigator.globalPrivacyControl === true || win.acVisitTrackingDisabled === true;
  const supported = () => win === win.top && !win.acPACK_MODE;
  const persist = () => { try { win.sessionStorage.setItem(KEY, JSON.stringify(attempt)); } catch {} };
  const read = () => {
    if (!attempt) {
      try { attempt = JSON.parse(win.sessionStorage.getItem(KEY)); } catch {}
    }
    if (!attempt || !Number.isFinite(attempt.at) || attempt.at > Date.now() || Date.now() - attempt.at > SIGNUP_TTL_MS ||
        !["signup", "login", "handle"].includes(attempt.mode) || !Array.isArray(attempt.stages)) {
      attempt = null;
      try { win.sessionStorage.removeItem(KEY); } catch {}
    }
    return attempt;
  };
  const track = (stage, error = null) => {
    if (!read()) return;
    if (!attempt.stages.includes(stage)) attempt.stages.push(stage);
    persist();
    if (disabled() || !signupID(attempt.id)) return;
    const body = { version: 1, id: attempt.id, mode: attempt.mode, source: attempt.source,
      stages: attempt.stages, automated: automatedVisit(win.navigator, win.location.search, win.acAutomation === true),
      error: SIGNUP_ERRORS.includes(error) ? error : null, referrerHost: attempt.referrerHost };
    // Every snapshot contains prior milestones, so a dropped redirect-time
    // request or an out-of-order retry does not erase the funnel.
    try { Promise.resolve(win.fetch("/api/signup-track", {
      method: "POST", headers: { "Content-Type": "text/plain" }, body: JSON.stringify(body),
      credentials: "omit", referrerPolicy: "no-referrer", keepalive: true,
    })).catch(() => {}); } catch {}
  };
  function start(mode = "signup") {
    if (!supported()) return;
    close();
    const returnTo = signupReturnPath(win.location.href, win.location.origin) || previousPiece;
    attempt = { at: Date.now(), id: disabled() ? null : win.crypto.randomUUID(),
      mode: ["signup", "login", "handle"].includes(mode) ? mode : "login",
      source: signupSource(returnTo || win.location.pathname), stages: [],
      referrerHost: visitReferrer(doc.referrer), returnTo };
    track("started");
  }
  function close() {
    generation++;
    win.clearInterval(timer);
    timer = undefined;
    dialog?.remove(); dialog = null;
  }
  function clear() {
    attempt = null;
    try { win.sessionStorage.removeItem(KEY); } catch {}
  }
  function complete(fallback = "/chat") {
    const destination = signupReturnPath(read()?.returnTo, win.location.origin) || fallback;
    track("completed");
    clear(); close();
    win.location.assign(destination);
  }
  async function request(path, options = {}) {
    const token = signedIn ? signedIn.access : await auth.getTokenSilently();
    const response = await win.fetch(path, { ...options,
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
      signal: AbortSignal.timeout(15000), cache: "no-store" });
    const body = await response.json();
    if (!response.ok) throw Object.assign(new Error("Request failed"), { reason: body.message || "network" });
    return body;
  }
  function frame(title, description) {
    close();
    dialog = doc.createElement("dialog");
    dialog.dataset.acNoTrack = "";
    dialog.setAttribute("aria-labelledby", "ac-signup-title");
    dialog.innerHTML = `<style>
      .ac-signup form,.ac-signup input,.ac-signup button{pointer-events:auto}.ac-signup input{user-select:text;-webkit-user-select:text}
      .ac-signup{box-sizing:border-box;width:min(420px,calc(100% - 32px));max-height:calc(100dvh - 32px);overflow:auto;border:2px solid #ff71bf;border-radius:12px;padding:28px;background:#171321;color:#fff;font:17px/1.5 system-ui,sans-serif;box-shadow:0 12px 60px #0009}
      .ac-signup::backdrop{background:#0c0719bd}.ac-signup h1{font-size:28px;line-height:1.15;margin:0 28px 16px 0}.ac-signup p{margin:0 0 20px;color:#dfd6e9}.ac-signup label{display:block;margin:0 0 6px}.ac-signup input{box-sizing:border-box;width:100%;background:#292235;color:white;border:2px solid #a79aae;border-radius:6px;font:inherit;padding:12px;margin:0 0 16px}.ac-signup button{font:inherit;border:0;border-radius:6px;padding:12px 16px;cursor:pointer}.ac-signup button:focus-visible,.ac-signup input:focus-visible{outline:3px solid #ffafdb;outline-offset:3px}.ac-signup .primary{width:100%;background:#ff71bf;color:#211026;font-weight:650}.ac-signup .secondary{background:transparent;color:#e0d6ec;margin-top:10px}.ac-signup .dismiss{position:absolute;right:12px;top:8px;background:transparent;color:white;font-size:24px;padding:0 8px}.ac-signup button:disabled{opacity:.55;cursor:wait}.ac-signup [role=status]{display:block;min-height:1.5em;color:#ffe19c;margin:12px 0 0;font-size:15px}
      .ac-signup .links{display:flex;flex-wrap:wrap;justify-content:space-between;gap:4px 16px;margin-top:6px}.ac-signup .links button{background:transparent;color:#e0d6ec;padding:8px 0;text-decoration:underline;text-underline-offset:3px;text-decoration-color:#6d6080}.ac-signup .links button:disabled{text-decoration:none;color:#9b90a8;cursor:default}
      .ac-signup .at{display:flex;align-items:center;gap:0;background:#292235;border:2px solid #a79aae;border-radius:6px;margin:0 0 6px}.ac-signup .at:focus-within{outline:3px solid #ffafdb;outline-offset:3px}.ac-signup .at span{padding-left:12px;color:#ff71bf;font-weight:650}.ac-signup .at input{border:0;margin:0;background:transparent;outline:none}.ac-signup .at input:focus-visible{outline:none}
      .ac-signup .check{min-height:1.5em;margin:0 0 14px;font-size:15px;color:#9b90a8}.ac-signup .check.free{color:#8ee6a8}.ac-signup .check.no{color:#ff8f9c}.ac-signup .hold{font-size:14px;color:#ffe19c;margin:-8px 0 16px}
      .ac-signup .or{display:flex;align-items:center;gap:10px;color:#9b90a8;font-size:14px;margin:18px 0 12px}.ac-signup .or::before,.ac-signup .or::after{content:"";flex:1;border-top:1px solid #3d3450}
      .ac-signup .providers{display:grid;grid-template-columns:repeat(auto-fit,minmax(120px,1fr));gap:10px}.ac-signup .providers button{background:transparent;color:#fff;border:2px solid #4a4058}
      .ac-signup input.code{font:600 28px/1 ui-monospace,Menlo,monospace;letter-spacing:.45em;text-align:center;padding:14px 0 14px .45em}
    </style><button class="dismiss" aria-label="Close">×</button><h1 id="ac-signup-title"></h1><p class="description"></p><div class="content"></div><span role="status" aria-live="polite"></span>`;
    dialog.className = "ac-signup";
    dialog.querySelector("h1").textContent = title;
    dialog.querySelector(".description").textContent = description;
    dialog.querySelector(".dismiss").onclick = close;
    dialog.addEventListener("cancel", event => { event.preventDefault(); close(); });
    // Keep typing in the dialog out of the canvas prompt's keyboard handlers.
    for (const type of ["keydown", "keyup", "keypress", "pointerdown", "pointerup", "pointermove", "mousedown", "mouseup", "click", "touchstart", "touchend", "touchmove", "wheel"])
      dialog.addEventListener(type, event => event.stopPropagation());
    doc.body.append(dialog); dialog.showModal();
    return dialog;
  }
  function handleForm() {
    track("verified"); track("handle_shown");
    const view = frame("Choose your @handle", "Your name for sharing art and joining chat.");
    view.querySelector(".content").innerHTML = `<form><label for="ac-signup-handle">Handle</label><input id="ac-signup-handle" name="handle" autocomplete="nickname" autocapitalize="none" spellcheck="false" maxlength="17" required aria-describedby="ac-signup-hint"><p id="ac-signup-hint">1–16 letters or numbers. Dots and underscores can go between them.</p><button class="primary" type="submit">Create handle</button></form>`;
    const input = view.querySelector("input"), button = view.querySelector(".primary"), status = view.querySelector("[role=status]"), mine = generation;
    input.addEventListener("input", () => { status.textContent = ""; });
    input.focus();
    view.querySelector("form").onsubmit = async event => {
      event.preventDefault();
      if (button.disabled) return;
      const handle = input.value.trim().replace(/^@/, "");
      if (validateHandle(handle) !== "valid") { status.textContent = "Use an available name with 1–16 letters or numbers, dots or underscores."; track("handle_failed", "invalid"); return; }
      button.disabled = true; status.textContent = "Saving…";
      try {
        const result = await request("/handle", { method: "POST", body: JSON.stringify({ handle }) });
        if (mine !== generation) return;
        if (!result.handle) throw new Error("Missing handle");
        win.dispatchEvent(new win.Event("ac:handle-created"));
        complete();
      } catch (error) {
        if (mine !== generation) return;
        const reason = SIGNUP_ERRORS.includes(error.reason) ? error.reason : "network";
        track("handle_failed", reason);
        status.textContent = reason === "taken" ? "That handle is taken. Try another name." :
          reason === "unverified" ? "Verify your email, then try again." : "Couldn’t save your handle. Please try again.";
        button.disabled = false; input.focus();
      }
    };
  }
  function verification() {
    track("verification_shown");
    const view = frame("Check your email", "Open the verification link, then return here to choose your handle.");
    view.querySelector(".content").innerHTML = `<button class="primary" type="button">I’ve verified my email</button><button class="secondary" type="button">Resend email</button>`;
    const status = view.querySelector("[role=status]"), mine = generation;
    const check = async (manual = false) => {
      if (checking || mine !== generation || doc.visibilityState === "hidden") return;
      checking = true;
      try {
        const current = await request("/api/signup-status");
        if (mine !== generation) return;
        if (current.verified) {
          await auth.getTokenSilently({ cacheMode: "off" });
          if (mine !== generation) return;
          if (current.handle) complete(); else handleForm();
        } else if (manual) status.textContent = "Not verified yet. Open the link in your email, then try again.";
      } catch { if (manual && mine === generation) status.textContent = "Couldn’t check right now. Please try again."; }
      finally { checking = false; }
    };
    view.querySelector(".primary").onclick = () => check(true);
    view.querySelector(".secondary").onclick = async event => {
      const button = event.currentTarget; button.disabled = true;
      try {
        await request("/api/signup-status", { method: "POST", body: JSON.stringify({ action: "resend" }) });
        if (mine !== generation) return;
        track("verification_resent"); status.textContent = "Verification email sent. Check your inbox and spam folder.";
      } catch { status.textContent = "Couldn’t resend yet. Please wait a minute and try again."; }
      win.setTimeout(() => { button.disabled = false; }, 60000);
    };
    timer = win.setInterval(() => check(), 15000);
  }
  async function resume(client, user, force = false) {
    if (!supported() || (!read() && !force)) return;
    if (!read()) start("handle");
    auth = client;
    if (!auth || !user) return;
    const mine = ++generation;
    try {
      const status = await request("/api/signup-status");
      if (mine !== generation) return;
      if (status.handle && status.verified) { complete("/prompt"); return; }
      if (status.verified) handleForm(); else verification();
    } catch {
      if (mine !== generation) return;
      const view = frame("Finish signing up", "Your account is signed in. We couldn’t check the next step yet.");
      view.querySelector(".content").innerHTML = `<button class="primary">Try again</button>`;
      view.querySelector(".primary").onclick = () => resume(client, user);
    }
  }
  // 🚪 The in-page door

  const holdMinutes = () => Math.max(0, Math.ceil(((read()?.holdUntil || 0) - Date.now()) / 60000));
  const escape = (text) => String(text).replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);

  async function door() {
    if (!otp) {
      otpModule = await import("./auth0-otp.mjs");
      otp = otpModule.otpSignIn({ clientId });
    }
    return otp;
  }

  // Leave for the hosted page: the tenant, not the person, refused the code door.
  function fallback(mode = read()?.mode) {
    track("fallback");
    close();
    redirect?.(mode === "signup" ? "signup" : "login");
  }

  function open(mode = "signup") {
    if (!supported() || !clientId) return fallback(mode);
    start(mode === "signup" ? "signup" : "login");
    attempt.hold = win.crypto.randomUUID(); // holds need an id even when tracking is off
    persist();
    if (attempt.mode === "signup") handleStep(); else emailStep();
  }

  // 1. The @handle, checked as it is typed and held before anything else is asked.
  function handleStep({ message = "", claimNow = false } = {}) {
    const view = frame(claimNow ? "Choose your @handle" : "Pick your @handle",
      "It’s how you show up in chat and on everything you make.");
    view.querySelector(".content").innerHTML = `<form novalidate><label for="ac-signup-handle">Handle</label>
      <div class="at"><span aria-hidden="true">@</span><input id="ac-signup-handle" name="handle" autocomplete="nickname" autocapitalize="none" spellcheck="false" maxlength="17" required aria-describedby="ac-signup-check"></div>
      <p class="check" id="ac-signup-check" aria-live="polite">1–16 letters or numbers. Dots and underscores can go between them.</p>
      <button class="primary" type="submit">Continue</button>
      ${claimNow ? "" : `<div class="links"><button type="button" data-login>I already have an account</button></div>`}</form>`;
    const input = view.querySelector("input"), check = view.querySelector(".check"), status = view.querySelector("[role=status]");
    const button = view.querySelector(".primary"), mine = generation;
    if (read()?.handle) input.value = attempt.handle;
    status.textContent = message;
    let pause, asked = 0;
    const say = (text, tone = "") => { check.textContent = text; check.className = `check ${tone}`; };
    const look = async () => {
      const handle = input.value.trim().replace(/^@/, "");
      if (!handle) return say("1–16 letters or numbers. Dots and underscores can go between them.");
      if (validateHandle(handle) !== "valid") return say("Letters and numbers, with dots or underscores only between them.", "no");
      const ask = ++asked;
      say("Checking…");
      try {
        const res = await win.fetch(`/api/handle-hold?handle=${encodeURIComponent(handle)}&attempt=${read()?.hold || ""}`, { cache: "no-store" });
        const body = await res.json();
        if (ask !== asked || mine !== generation) return;
        if (body.status === "free" || body.status === "yours") say(`@${handle} is free.`, "free");
        else if (body.status === "invalid") say(body.reason === "naughty" ? "Try a different name." : "That name won’t work as a handle.", "no");
        else say(`@${handle} is taken. Try another.`, "no");
      } catch { if (ask === asked) say(""); }
    };
    input.addEventListener("input", () => { status.textContent = ""; win.clearTimeout(pause); pause = win.setTimeout(look, 280); });
    view.querySelector("[data-login]")?.addEventListener("click", () => { attempt.mode = "login"; persist(); emailStep(); });
    if (input.value) look();
    input.focus();
    view.querySelector("form").onsubmit = async (event) => {
      event.preventDefault();
      if (button.disabled) return;
      const handle = input.value.trim().replace(/^@/, "");
      if (validateHandle(handle) !== "valid") { status.textContent = "Use 1–16 letters or numbers, dots or underscores."; track("handle_failed", "invalid"); return; }
      button.disabled = true; status.textContent = "Saving…";
      if (claimNow) return claim(handle);
      try {
        const res = await win.fetch("/api/handle-hold", { method: "POST", cache: "no-store",
          headers: { "Content-Type": "application/json" }, body: JSON.stringify({ handle, attempt: read().hold }) });
        const body = await res.json().catch(() => ({}));
        if (mine !== generation) return;
        if (!res.ok) {
          const taken = body.status === "taken" || body.status === "held";
          track("handle_failed", taken ? "taken" : body.status === "invalid" ? "invalid" : "network");
          status.textContent = taken ? `@${handle} is taken. Try another.` : body.status === "invalid" ? "That name won’t work as a handle." : "Couldn’t check that name. Please try again.";
          button.disabled = false; input.focus(); return;
        }
        attempt.handle = handle; attempt.holdUntil = Date.parse(body.until) || Date.now() + 600000; persist();
        track("handle_held");
        emailStep();
      } catch {
        if (mine !== generation) return;
        status.textContent = "Couldn’t check that name. Please try again."; button.disabled = false;
      }
    };
  }

  // 2. Where to send the code.
  function emailStep(prefill = "") {
    const signup = read()?.mode === "signup";
    const view = frame(signup ? "Where should we send a code?" : "Log in",
      signup ? "We’ll email you six digits. No password." : "We’ll email you a six-digit code.");
    view.querySelector(".content").innerHTML = `${signup && attempt.handle ? `<p class="hold">@${escape(attempt.handle)} is yours for ${holdMinutes()} minutes.</p>` : ""}
      <form novalidate><label for="ac-signup-email">Email</label>
      <input id="ac-signup-email" name="email" type="email" autocomplete="email" inputmode="email" autocapitalize="none" spellcheck="false" required>
      <button class="primary" type="submit">Send code</button>
      ${socials.length && social ? `<div class="or">or</div><div class="providers">${socials.map((p) => `<button type="button" data-provider="${escape(p.connection)}">${escape(p.label)}</button>`).join("")}</div>` : ""}
      <div class="links"><button type="button" data-back>${signup ? "Change handle" : "New here? Pick a handle"}</button><button type="button" data-password>Use a password instead</button></div></form>`;
    const input = view.querySelector("input"), status = view.querySelector("[role=status]"), button = view.querySelector(".primary"), mine = generation;
    input.value = prefill;
    input.focus();
    view.querySelector("[data-back]").onclick = () => { attempt.mode = "signup"; persist(); handleStep(); };
    view.querySelector("[data-password]").onclick = () => fallback();
    for (const button of view.querySelectorAll("[data-provider]")) button.onclick = () => viaProvider(button);
    view.querySelector("form").onsubmit = async (event) => {
      event.preventDefault();
      if (button.disabled) return;
      button.disabled = true; status.textContent = "Sending…";
      try {
        const gate = await door();
        const email = await gate.sendCode(input.value);
        if (mine !== generation) return;
        track("code_sent");
        codeStep(email);
      } catch (error) {
        if (mine !== generation) return;
        if (otpModule?.tenantFaults?.includes(error.code)) return fallback();
        status.textContent = error.message || "Couldn’t send a code. Please try again.";
        button.disabled = false; input.focus();
      }
    };
  }

  // 3. The six digits. Spending them proves the address, so there is no letter
  // with a link and no "I’ve verified" button in this door.
  function codeStep(email) {
    const view = frame("Enter the code", `Sent to ${email}. It can take a minute; check spam too.`);
    view.querySelector(".content").innerHTML = `<form novalidate><label for="ac-signup-code">Code</label>
      <input id="ac-signup-code" class="code" name="code" inputmode="numeric" autocomplete="one-time-code" pattern="[0-9]*" maxlength="6" required>
      <button class="primary" type="submit">Continue</button>
      <div class="links"><button type="button" data-resend disabled>Resend code</button><button type="button" data-back>Wrong email?</button></div></form>`;
    const input = view.querySelector("input"), status = view.querySelector("[role=status]"), button = view.querySelector(".primary");
    const resend = view.querySelector("[data-resend]"), mine = generation;
    const form = view.querySelector("form");
    input.focus();
    win.setTimeout(() => { if (mine === generation) resend.disabled = false; }, 30000);
    resend.onclick = async () => {
      resend.disabled = true;
      try { await (await door()).sendCode(email); track("code_sent"); status.textContent = "New code sent."; }
      catch (error) { status.textContent = error.message || "Couldn’t resend yet."; }
      win.setTimeout(() => { if (mine === generation) resend.disabled = false; }, 30000);
    };
    view.querySelector("[data-back]").onclick = () => emailStep(email);
    input.addEventListener("input", () => {
      input.value = input.value.replace(/\D/g, "").slice(0, 6);
      status.textContent = "";
      if (input.value.length === 6) form.requestSubmit();
    });
    form.onsubmit = async (event) => {
      event.preventDefault();
      if (button.disabled || input.value.length !== 6) return;
      button.disabled = true; status.textContent = "Checking…";
      try {
        const session = await (await door()).verify(email, input.value);
        if (mine !== generation) return;
        signedIn = session;
        adopt(session);
        track("verified");
        await signedInNext();
      } catch (error) {
        if (mine !== generation) return;
        if (otpModule?.tenantFaults?.includes(error.code)) return fallback();
        track("code_failed", "code");
        status.textContent = error.message || "That code didn’t work.";
        button.disabled = false; input.value = ""; input.focus();
      }
    };
  }

  // A provider's own sign-in (Google, Apple, …) in a popup. Its email arrives
  // verified, so it lands exactly where a spent code does. The session lives
  // in auth0-spa-js's cache, which boot reads on the next load.
  async function viaProvider(button) {
    const connection = button.dataset.provider, label = button.textContent;
    const status = dialog?.querySelector("[role=status]"), mine = generation;
    button.disabled = true;
    if (status) status.textContent = `Opening ${label}…`;
    track("social_started");
    try {
      const client = await social(connection);
      if (!client || mine !== generation) return; // the redirect took over
      signedIn = null; otp?.forget();
      auth = client;
      const user = await client.getUser();
      if (!user?.sub) throw new Error("no user");
      track("verified");
      await signedInNext(user.sub);
    } catch (error) {
      if (mine !== generation) return;
      button.disabled = false;
      const off = /connection|not enabled|disabled/i.test(`${error?.error_description || ""} ${error?.message || ""}`);
      if (status) status.textContent = /cancel|closed/i.test(error?.message || error?.error || "") ? "" :
        off ? `${label} sign-in isn’t switched on yet.` : `Couldn’t sign in with ${label}. Please try again.`;
    }
  }

  // Boot reads this on the next load exactly as it reads a session handed over
  // by an embedding host; auth0-otp keeps the refresh token beside it.
  function adopt(session) {
    const encoded = win.btoa(JSON.stringify({ accessToken: session.access, account: { id: session.sub, label: session.email } }));
    try { win.localStorage.setItem("session-aesthetic", encoded); } catch {}
  }

  // Signed in: a returning account (its handle came through the linking
  // Action) goes straight back; a newcomer claims the handle they held.
  async function signedInNext(sub = signedIn?.sub) {
    const mine = generation;
    let existing = null;
    try {
      const res = await win.fetch(`/handle?for=${encodeURIComponent(sub)}`, { cache: "no-store" });
      if (res.ok) existing = (await res.json()).handle || null;
    } catch {}
    if (mine !== generation) return;
    if (existing) return complete("/prompt");
    if (read()?.handle) return claim(attempt.handle);
    if (read()?.mode === "login") return noHandleYet();
    track("handle_shown");
    handleStep({ claimNow: true });
  }

  // Logging in by code found no handle. Either this email is new here, or its
  // account predates codes and Auth0 hasn't linked the two — then the password
  // door still reaches the old account, and a second handle would be wrong.
  function noHandleYet() {
    const view = frame("No handle on this email yet", "If you already have an account, log in with your password to reach it. New here? Pick a handle.");
    view.querySelector(".content").innerHTML = `<button class="primary" type="button" data-new>Pick a handle</button><button class="secondary" type="button" data-password>Log in with my password</button>`;
    view.querySelector("[data-new]").onclick = () => { track("handle_shown"); handleStep({ claimNow: true }); };
    view.querySelector("[data-password]").onclick = () => {
      signedIn = null; otp?.forget();
      try { win.localStorage.removeItem("session-aesthetic"); } catch {}
      fallback("login");
    };
  }

  async function claim(handle) {
    const mine = generation;
    try {
      const result = await request("/handle", { method: "POST", body: JSON.stringify({ handle, hold: read()?.hold }) });
      if (mine !== generation) return;
      if (!result.handle) throw new Error("Missing handle");
      win.dispatchEvent(new win.Event("ac:handle-created"));
      complete();
    } catch (error) {
      if (mine !== generation) return;
      const reason = SIGNUP_ERRORS.includes(error.reason) ? error.reason : "network";
      track("handle_failed", reason);
      if (attempt) attempt.handle = null;
      handleStep({ claimNow: true, message: reason === "taken" ? `@${handle} was taken while you were away. Pick another.` : "Couldn’t save your handle. Please try again." });
    }
  }

  return {
    start, track, resume, complete, pending: () => !!read(), close, open,
    remember(path) { const safe = signupReturnPath(path, win.location.origin); if (safe) previousPiece = safe; },
    failed() {
      track("auth_failed", "auth");
      const view = frame("Sign-in didn’t finish", "Please try again. Your starting piece is saved in this tab.");
      view.querySelector(".content").innerHTML = `<button class="primary">Try again</button>`;
      view.querySelector(".primary").onclick = () => {
        const mode = read()?.mode === "signup" ? "signup" : undefined;
        previousPiece = signupReturnPath(read()?.returnTo, win.location.origin) || previousPiece;
        close(); win.acLOGIN?.(mode);
      };
    },
  };
}
