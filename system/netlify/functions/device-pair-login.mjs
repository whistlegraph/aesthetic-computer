// device-pair-login.mjs — On-device browser login for AC identity switching.
// Opened by the native OS: /api/device-pair-login?code=XXXXXX
// Handles Auth0 login flow, then claims the device-pair code with user's
// handle, AC token, and device tokens (Claude + GitHub).

import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  const params = new URLSearchParams(event.rawQuery || "");
  const code = params.get("code") || "";
  const baseUrl = process.env.URL || "https://aesthetic.computer";

  const html = `<!DOCTYPE html>
<html><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>AC Device Login</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:-apple-system,system-ui,sans-serif;background:#0a0a1a;color:#eee;
  min-height:100vh;display:flex;align-items:center;justify-content:center;padding:20px}
.card{background:#12122a;border:1px solid #333;border-radius:16px;padding:32px;
  max-width:420px;width:100%;text-align:center}
h1{font-size:1.5em;margin-bottom:8px;color:#c8b6ff}
.code{font-family:monospace;font-size:2.2em;font-weight:bold;letter-spacing:4px;
  color:#fff;background:#1e1e40;padding:16px;border-radius:8px;margin:16px 0}
p{margin:10px 0;line-height:1.5;color:#999}
.btn{display:inline-block;padding:14px 36px;background:linear-gradient(135deg,#667eea,#764ba2);
  color:#fff;text-decoration:none;border-radius:8px;font-weight:bold;font-size:1.1em;
  margin:16px 0;border:none;cursor:pointer}
.btn:hover{opacity:0.9}
.btn:disabled{opacity:0.4;cursor:wait}
.ok{color:#4ade80;font-size:1.2em}
.err{color:#f87171}
.spin{display:inline-block;animation:spin 1s linear infinite}
@keyframes spin{to{transform:rotate(360deg)}}
#status{min-height:60px}
</style>
</head><body>
<div class="card">
  <h1>AC Device Login</h1>
  <div class="code">${code || "------"}</div>
  <div id="status">
    <p>Sign in to switch this device's identity.</p>
    <button class="btn" id="login-btn" onclick="doLogin()">Sign In</button>
  </div>
</div>

<script src="/aesthetic.computer/dep/auth0-spa-js.production.js"></script>
<script>
const DEVICE_CODE = ${JSON.stringify(code)};
const BASE = ${JSON.stringify(baseUrl)};
let auth0Client = null;

async function init() {
  try {
    auth0Client = await auth0.createAuth0Client({
      domain: "https://hi.aesthetic.computer",
      clientId: "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt",
      cacheLocation: "localstorage",
      useRefreshTokens: true,
      authorizationParams: { redirect_uri: window.location.origin + "/api/device-pair-login?code=" + DEVICE_CODE },
    });

    // Check if returning from Auth0 redirect
    if (location.search.includes("state=") && location.search.includes("code=")) {
      await auth0Client.handleRedirectCallback();
      // Clean URL
      history.replaceState({}, "", "/api/device-pair-login?code=" + DEVICE_CODE);
    }

    const loggedIn = await auth0Client.isAuthenticated();
    if (loggedIn) {
      await claimDevice();
    }
  } catch (err) {
    console.error("Auth init error:", err);
    status("Sign-in system loading... click Sign In to continue.", false);
  }
}

function status(msg, isError) {
  document.getElementById("status").innerHTML =
    '<p class="' + (isError ? "err" : "") + '">' + msg + '</p>';
}

async function doLogin() {
  const btn = document.getElementById("login-btn");
  if (btn) btn.disabled = true;
  status('<span class="spin">&#9696;</span> Redirecting to sign in...', false);

  if (!auth0Client) {
    // Auth0 not loaded yet — try initializing
    try {
      auth0Client = await auth0.createAuth0Client({
        domain: "https://hi.aesthetic.computer",
        clientId: "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt",
        cacheLocation: "localstorage",
        useRefreshTokens: true,
        authorizationParams: { redirect_uri: window.location.origin + "/api/device-pair-login?code=" + DEVICE_CODE },
      });
    } catch (err) {
      status("Failed to load auth: " + err.message, true);
      return;
    }
  }

  try {
    await auth0Client.loginWithRedirect();
  } catch (err) {
    status("Login failed: " + err.message, true);
  }
}

async function claimDevice() {
  status('<span class="spin">&#9696;</span> Pairing device...', false);

  try {
    const token = await auth0Client.getTokenSilently();

    // Claim the device-pair code with our AC auth token
    const res = await fetch(BASE + "/.netlify/functions/device-pair", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": "Bearer " + token,
      },
      body: JSON.stringify({ action: "claim", code: DEVICE_CODE }),
    });

    const data = await res.json();

    if (res.ok && data.handle) {
      document.getElementById("status").innerHTML =
        '<p class="ok">Paired as @' + data.handle + '!</p>' +
        '<p>This window will close. Your device will reboot into the new identity.</p>';
      // Auto-close after a moment (Firefox kiosk)
      setTimeout(() => { try { window.close(); } catch(_) {} }, 4000);
    } else {
      status("Pairing failed: " + (data.message || "unknown error"), true);
    }
  } catch (err) {
    status("Error: " + err.message, true);
  }
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
