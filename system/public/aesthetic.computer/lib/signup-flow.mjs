import { SIGNUP_TTL_MS, SIGNUP_ERRORS, signupReturnPath, signupSource, signupID } from "./signup-model.mjs";
import { automatedVisit, visitReferrer } from "./visit-model.mjs";
import { validateHandle } from "./text.mjs";

const KEY = "ac:signup:v1";

// Authentication, verification and a normal handle form; content stays local.
// The collector receives only a per-attempt UUID and allowlisted milestones.
export function createSignupFlow(win, doc) {
  let attempt, previousPiece = signupReturnPath(win.location.href, win.location.origin);
  let dialog, timer, auth, checking = false, generation = 0;
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
    const token = await auth.getTokenSilently();
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
  return {
    start, track, resume, complete, pending: () => !!read(), close,
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
