// device-pair-login.mjs — On-device browser login for AC identity switching.
// Opened by the native OS: /api/device-pair-login?code=XXXXXX
// Handles Auth0 login flow, then claims the device-pair code with user's
// handle, AC token, and device tokens (Claude + GitHub).

export async function handler(event) {
  const params = new URLSearchParams(event.rawQuery || event.rawQueryString ||
    event.queryStringParameters || String(event.rawUrl || "").split("?")[1] || "");
  const requestedCode = params.get("code") || "";
  const code = /^[A-Z0-9]{6}$/i.test(requestedCode) ? requestedCode.toUpperCase() : "";

  const html = `<!DOCTYPE html>
<html lang="en"><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Sign in · Aesthetic Computer</title>
<meta name="theme-color" content="#14101d">
<style>
@font-face{font-family:AC;src:url('/type/webfonts/ywft-processing-regular.woff2') format('woff2');font-display:swap}
*{margin:0;padding:0;box-sizing:border-box}
html{color-scheme:dark}
@font-face{font-family:Berkeley;src:url('/type/webfonts/BerkeleyMonoVariable-Regular.woff2') format('woff2');font-display:swap}
:root{--bg:#14101d;--text:#eee5ff;--dim:#bdb0d0;--pink:#ff6b9d;--green:#b9fa70;--box:#251c35;--code:#d4b6ff}
body.light-mode{--bg:#f5f2f8;--text:#20152d;--dim:#655572;--pink:#b43b7d;--green:#396f20;--box:#e8dfef;--code:#633488;color-scheme:light}
body{font-family:Berkeley,Menlo,monospace;background:var(--bg);color:var(--text);
  min-height:100vh;min-height:100svh;display:grid;place-items:center;
  padding:max(28px,env(safe-area-inset-top)) max(24px,env(safe-area-inset-right)) max(28px,env(safe-area-inset-bottom)) max(24px,env(safe-area-inset-left));
  font-size:16px;line-height:1.5;-webkit-text-size-adjust:100%}
.card{width:100%;max-width:440px}
.identity{font-family:AC,monospace;display:inline-block;color:var(--green);text-decoration:none;font-size:22px;margin-bottom:52px}
h1{font-family:AC,monospace;color:var(--pink);font-size:clamp(40px,10vw,56px);line-height:1.05;font-weight:400;margin-bottom:24px}
.code{font-family:AC,monospace;color:var(--code);background:var(--box);border-left:4px solid var(--green);
  font-size:clamp(32px,9vw,48px);line-height:1.2;letter-spacing:.1em;
  padding:22px 18px;margin:24px 0 28px;white-space:nowrap;font-variant-numeric:tabular-nums}
p{margin:0 0 20px;color:var(--dim);overflow-wrap:anywhere}
.btn{display:block;width:100%;min-height:58px;padding:14px 20px;background:#b9fa70;
  color:#14101d;text-decoration:none;font:inherit;line-height:1.25;text-align:left;
  margin-top:28px;border:0;border-radius:0;cursor:pointer;touch-action:manipulation}
.btn:hover{background:var(--pink);color:#14101d}
.btn:active{background:#a2df59}
a:focus-visible,.btn:focus-visible{outline:3px solid #eee5ff;outline-offset:6px}
.btn:disabled{background:#514362;color:#d3c5e3;cursor:wait}
.ok{color:var(--green);font-size:18px}
.handle{white-space:nowrap}
.err{color:var(--pink)}
[hidden]{display:none!important}
#status{min-height:1.5em;margin-top:22px;overflow-wrap:anywhere}
label{display:block;margin-bottom:8px}
input{width:100%;min-height:52px;padding:12px;font:inherit;font-size:16px;border:1px solid var(--dim);border-radius:0;background:var(--box);color:var(--text)}
input:focus-visible{outline:2px solid var(--pink);outline-offset:2px}
#otp{letter-spacing:.22em;font-size:24px}
.secondary{margin-top:12px;background:transparent;color:var(--text);border:1px solid var(--dim)}
.btn{margin-top:16px}
.identity:hover{color:var(--pink)}
@media(max-height:560px){.identity{margin-bottom:24px}body{place-items:start center}}
</style>
</head><body>
<main class="card" aria-labelledby="title">
  <a class="identity" href="https://aesthetic.computer">aesthetic.computer</a>
  <h1 id="title">Sign in</h1>
  <div class="code" aria-label="Device pairing code">${code || "------"}</div>
  <form id="email-form">
    <label for="email">Email</label>
    <input id="email" name="email" type="email" autocomplete="email" autocapitalize="none" spellcheck="false" required>
    <button class="btn" id="login-btn" type="submit">Send email code →</button>
  </form>
  <form id="code-form" hidden>
    <p id="email-destination"></p>
    <label for="otp">Email code</label>
    <input id="otp" name="code" type="text" inputmode="numeric" autocomplete="one-time-code" pattern="[0-9]{6}" maxlength="6" required>
    <button class="btn" id="verify-btn" type="submit">Sign in →</button>
    <button class="btn secondary" id="change-email" type="button">Send another code</button>
  </form>
  <div id="status" role="status" aria-live="polite"></div>
  <button class="btn secondary" id="popup-btn" type="button" hidden>Sign in in a new window →</button>
  <button class="btn secondary" id="retry-pair" type="button" hidden>Try connecting again</button>
</main>

<script src="/aesthetic.computer/dep/auth0-spa-js.production.js"></script>
<script type="module">
import { otpSignIn } from "/aesthetic.computer/lib/auth0-otp.mjs";
let DEVICE_CODE = ${JSON.stringify(code)};
const query = new URLSearchParams(location.search);
const returning = query.has("state") && query.has("code");
try {
  if (returning) DEVICE_CODE = sessionStorage.getItem("ac-device-pair-code") || "";
  if (!/^[A-Z0-9]{6}$/.test(DEVICE_CODE)) DEVICE_CODE = "";
  if (DEVICE_CODE) sessionStorage.setItem("ac-device-pair-code", DEVICE_CODE);
} catch (_) {}
const el = id => document.getElementById(id);
document.querySelector(".code").textContent = DEVICE_CODE || "------";
const CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
const otp = otpSignIn({clientId:CLIENT_ID,redirectUri:window.location.origin});
let auth0Client = null, email = "", busy = false, paired = false, claimToken = "";
const theme = window.matchMedia?.("(prefers-color-scheme: light)");
function applyTheme() { document.body.classList.toggle("light-mode", !!theme?.matches); }
applyTheme();theme?.addEventListener?.("change",applyTheme);
function status(message, error = false) {
  el("status").textContent = message;
  el("status").className = error ? "err" : "";
}
async function colorHandle(node, handle) {
  const name = String(handle).replace(/^@/, "");
  node.textContent = "@" + name;
  if (!/^[a-z0-9_-]{1,64}$/i.test(name)) return;
  try {
    const response = await fetch("/api/oskiewar-leaderboard?handles=" + encodeURIComponent(name.toLowerCase()));
    if (!response.ok) return;
    const standings = await response.json();
    const colors = standings.players?.find(player => player.handle === name.toLowerCase())?.colors;
    const characters = [...("@" + name)];
    if (!Array.isArray(colors) || colors.length !== characters.length ||
        !colors.every(c => c && [c.r,c.g,c.b].every(v => Number.isFinite(v) && v >= 0 && v <= 255))) return;
    const letters = characters.map((character, index) => {
      const letter = document.createElement("span"), c = colors[index];
      letter.textContent = character;
      letter.style.color = "rgb(" + [c.r,c.g,c.b].join(",") + ")";
      return letter;
    });
    node.replaceChildren(...letters);
  } catch (_) {} // Color lookup must never turn successful login into an error.
}
function lock(value) {
  busy = value;
  for (const id of ["login-btn","verify-btn","popup-btn","change-email","retry-pair"])
    el(id).disabled = value;
}
function sdk() {
  if (!auth0Client) auth0Client = new auth0.Auth0Client({
    domain:"https://hi.aesthetic.computer",clientId:CLIENT_ID,
    cacheLocation:"localstorage",useRefreshTokens:true,
    authorizationParams:{redirect_uri:window.location.origin}
  });
  return auth0Client;
}
function authError(error) {
  status(error?.hint || "Sign-in could not finish. Please try again.", true);
  if (error?.tenant) el("popup-btn").hidden = false;
}
async function claimDevice(token) {
  if (paired || !DEVICE_CODE || !token) return;
  claimToken = token;el("retry-pair").hidden = true;status("Connecting your account…");
  try {
    const response = await fetch("/api/device-pair", {
      method:"POST",headers:{"Content-Type":"application/json","Authorization":"Bearer " + token},
      body:JSON.stringify({action:"claim",code:DEVICE_CODE})
    });
    const data = await response.json();
    if (!response.ok || !data.handle) {
      const message = response.status === 404 || response.status === 410
        ? "This screen code expired. Scan the current code on your device."
        : "Could not connect this account. Try again.";
      status(message,true);el("retry-pair").hidden = response.status === 404 || response.status === 410;
      return;
    }
    paired = true;
    el("email-form").hidden = el("code-form").hidden = el("popup-btn").hidden = true;
    const handle = document.createElement("span");
    handle.className = "handle";
    el("status").replaceChildren("Paired as ", handle, ". " + (data.kind === "browser"
      ? "Return to the game. You're signed in."
      : "Return to your device to continue."));
    void colorHandle(handle, data.handle);
    el("status").className = "ok";
  } catch (_) {
    status("Could not reach the device service. Try again.",true);el("retry-pair").hidden = false;
  }
}
el("email-form").addEventListener("submit", async event => {
  event.preventDefault();if (busy || paired || !DEVICE_CODE) return;
  const address = el("email").value.trim();
  if (!/^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$/.test(address)) {
    status("Enter your email address.",true);el("email").focus();return;
  }
  lock(true);status("Sending your email code…");
  try {
    email = await otp.sendCode(address);
    el("email-form").hidden = true;el("code-form").hidden = false;
    el("email-destination").textContent = "Enter the six digits sent to " + email + ".";
    el("otp").value = "";status("");el("otp").focus();
  } catch (error) {authError(error);} finally {lock(false);}
});
el("code-form").addEventListener("submit", async event => {
  event.preventDefault();if (busy || paired || !DEVICE_CODE) return;
  const code = el("otp").value.replace(/\\s/g, "");
  if (!/^\\d{6}$/.test(code)) {status("Enter the six-digit email code.",true);el("otp").focus();return;}
  lock(true);status("Checking your code…");
  try {
    const session = await otp.verify(email,code);
    if (!session?.access) throw new Error("Missing session");
    await claimDevice(session.access);
  } catch (error) {authError(error);el("otp").focus();} finally {lock(false);}
});
el("change-email").addEventListener("click", () => {
  if (busy || paired) return;
  el("code-form").hidden = true;el("email-form").hidden = false;
  status("");el("email").focus();
});
el("popup-btn").addEventListener("click", async () => {
  if (busy || paired || !DEVICE_CODE) return;
  lock(true);status("Complete sign-in in the new window.");
  try {
    // Called directly from the click: the SDK opens its popup before awaiting.
    const client = sdk();
    await client.loginWithPopup({authorizationParams:{redirect_uri:window.location.origin}});
    await claimDevice(await client.getTokenSilently());
  } catch (_) {status("Sign-in window could not finish. Please try again.",true);}
  finally {lock(false);}
});
el("retry-pair").addEventListener("click", async () => {
  if (busy || paired || !claimToken) return;
  lock(true);try {await claimDevice(claimToken);} finally {lock(false);}
});
async function init() {
  let locked = false;
  if (!DEVICE_CODE) {
    el("email-form").hidden = true;status("Scan a sign-in code on your device to continue.",true);return;
  }
  try {
    const token = await otp.token();
    if (token) {
      if (busy || paired) return;
      locked = true;lock(true);await claimDevice(token);return;
    }
    const client = sdk();
    if (returning) {
      await client.handleRedirectCallback();
      history.replaceState({},"","/api/device-pair-login?code=" + DEVICE_CODE);
    }
    if (await client.isAuthenticated()) {
      if (busy || paired) return;
      locked = true;lock(true);await claimDevice(await client.getTokenSilently());
    }
  } catch (_) {
    // Cached sessions may expire; the native email form remains available.
    status("Sign in with your email to continue.");
  } finally {if (locked) lock(false);}
}
init();
</script>
</body></html>`;

  return {
    statusCode: 200,
    headers: {
      "Content-Type": "text/html",
      "Access-Control-Allow-Origin": "*",
      "Cache-Control": "no-store",
    },
    body: html,
  };
}
