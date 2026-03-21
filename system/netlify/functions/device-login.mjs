// device-login.mjs — Phone-side device auth page
// User scans QR code → lands here → logs in via Claude OAuth → token sent to device
//
// URL: /api/device-login?code=WOLF-3847
// Redirects to Claude OAuth with state=code, callback returns here with tokens.

import crypto from "crypto";

const CLAUDE_OAUTH_URL = "https://claude.ai/oauth/authorize";
const CLAUDE_CLIENT_ID = "9d1c250a-e61b-44d9-88ed-5944d1962f5e";
const SCOPES = "org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload";

export async function handler(event) {
  const params = new URLSearchParams(event.rawQuery || "");
  const code = params.get("code");
  const oauthCode = params.get("oauth_code");
  const error = params.get("error");

  const baseUrl = process.env.URL || "https://aesthetic.computer";
  const callbackUrl = `${baseUrl}/api/device-login`;

  // Step 3: OAuth callback — exchange code and approve device
  if (oauthCode && code) {
    return await handleOAuthCallback(oauthCode, code, callbackUrl, baseUrl);
  }

  // Error from OAuth
  if (error) {
    return servePage("Auth Failed", `<p class="err">Error: ${error}</p><p>Close this page and try again on the device.</p>`);
  }

  // Step 1: Show device code confirmation page with "Login with Claude" button
  if (!code) {
    return servePage("Missing Code", `<p class="err">No device code provided.</p><p>Scan the QR code on your device to start.</p>`);
  }

  // Verify code exists and is pending
  try {
    const checkRes = await fetch(`${baseUrl}/api/device-auth?action=poll&code=${code}`);
    const checkData = await checkRes.json();
    if (checkData.error) {
      return servePage("Invalid Code", `<p class="err">Code <strong>${code}</strong> not found or expired.</p><p>Try again on the device.</p>`);
    }
    if (checkData.status === "approved") {
      return servePage("Already Done", `<p class="ok">This device is already logged in!</p>`);
    }
  } catch (e) {
    // Continue anyway — let the user try
  }

  // Generate PKCE challenge for Claude OAuth
  const { codeVerifier, codeChallenge } = generatePKCE();

  // Store verifier in a short-lived cookie (needed for callback)
  const state = `${code}:${codeVerifier}`;
  const stateB64 = Buffer.from(state).toString("base64url");

  const oauthUrl = `${CLAUDE_OAUTH_URL}?` + new URLSearchParams({
    client_id: CLAUDE_CLIENT_ID,
    response_type: "code",
    redirect_uri: `${callbackUrl}`,
    scope: SCOPES,
    code_challenge: codeChallenge,
    code_challenge_method: "S256",
    state: stateB64,
  }).toString();

  return servePage("Device Login", `
    <p>Logging in device:</p>
    <div class="code">${code}</div>
    <p>This will connect your Claude account to your AC Native device.</p>
    <a href="${oauthUrl}" class="btn">Login with Claude</a>
    <p class="dim">Your device is waiting...</p>
  `);
}

async function handleOAuthCallback(oauthCode, stateB64, callbackUrl, baseUrl) {
  // Decode state = "DEVICE_CODE:CODE_VERIFIER"
  let deviceCode, codeVerifier;
  try {
    // state comes back as a query param — might be the raw base64url from our redirect
    const raw = Buffer.from(stateB64, "base64url").toString();
    const colonIdx = raw.indexOf(":");
    deviceCode = raw.substring(0, colonIdx);
    codeVerifier = raw.substring(colonIdx + 1);
  } catch {
    return servePage("Error", `<p class="err">Invalid state parameter.</p>`);
  }

  // Exchange OAuth code for tokens using PKCE
  try {
    const tokenRes = await fetch("https://claude.ai/oauth/token", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        grant_type: "authorization_code",
        client_id: CLAUDE_CLIENT_ID,
        code: oauthCode,
        redirect_uri: callbackUrl,
        code_verifier: codeVerifier,
      }),
    });

    if (!tokenRes.ok) {
      const errText = await tokenRes.text();
      console.error("Token exchange failed:", errText);
      return servePage("Token Error", `<p class="err">Failed to exchange authorization code.</p><p>Try again on the device.</p>`);
    }

    const tokens = await tokenRes.json();

    // Build credentials in Claude Code format
    const credentials = {
      claudeAiOauth: {
        accessToken: tokens.access_token,
        refreshToken: tokens.refresh_token,
        expiresAt: Date.now() + (tokens.expires_in || 3600) * 1000,
        scopes: SCOPES.split(" "),
      },
    };

    // Approve the device code
    const approveRes = await fetch(`${baseUrl}/api/device-auth`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        action: "approve",
        code: deviceCode,
        credentials,
      }),
    });

    if (!approveRes.ok) {
      const err = await approveRes.text();
      console.error("Approve failed:", err);
      return servePage("Error", `<p class="err">Failed to send token to device.</p>`);
    }

    return servePage("Success!", `
      <p class="ok">Your device is now logged in!</p>
      <div class="code">${deviceCode}</div>
      <p>You can close this page. The device will pick up the login automatically.</p>
    `);

  } catch (err) {
    console.error("OAuth callback error:", err);
    return servePage("Error", `<p class="err">Authentication failed: ${err.message}</p>`);
  }
}

// PKCE helper (Node.js crypto)
function generatePKCE() {
  const codeVerifier = crypto.randomBytes(32).toString("base64url");
  const codeChallenge = crypto.createHash("sha256").update(codeVerifier).digest("base64url");
  return { codeVerifier, codeChallenge };
}

function servePage(title, bodyContent) {
  const html = `<!DOCTYPE html>
<html><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>${title} — AC Native</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:-apple-system,system-ui,sans-serif;background:#111;color:#eee;
  min-height:100vh;display:flex;align-items:center;justify-content:center;padding:20px}
.card{background:#1a1a2e;border-radius:16px;padding:32px;max-width:400px;width:100%;text-align:center}
h1{font-size:1.4em;margin-bottom:16px;color:#c8b6ff}
p{margin:12px 0;line-height:1.5;color:#aaa}
.code{font-family:monospace;font-size:2em;font-weight:bold;letter-spacing:4px;
  color:#fff;background:#2a2a4e;padding:16px;border-radius:8px;margin:16px 0}
.btn{display:inline-block;padding:14px 32px;background:linear-gradient(135deg,#667eea,#764ba2);
  color:#fff;text-decoration:none;border-radius:8px;font-weight:bold;font-size:1.1em;margin:16px 0}
.btn:hover{opacity:0.9}
.ok{color:#4ade80}
.err{color:#f87171}
.dim{color:#666;font-size:0.85em}
</style>
</head><body><div class="card"><h1>${title}</h1>${bodyContent}</div></body></html>`;
  return {
    statusCode: 200,
    headers: { "Content-Type": "text/html", "Access-Control-Allow-Origin": "*" },
    body: html,
  };
}
