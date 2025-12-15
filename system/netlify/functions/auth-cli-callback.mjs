// auth-cli-callback.mjs, 25.01.11
// OAuth callback endpoint for CLI authentication
// Receives authorization code and returns tokens via simple HTML page

import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  const params = new URLSearchParams(event.rawQuery || "");
  const code = params.get("code");
  const state = params.get("state");
  const error = params.get("error");
  const errorDescription = params.get("error_description");

  // Handle OAuth errors
  if (error) {
    return respond(400, null, `
      <!DOCTYPE html>
      <html>
      <head>
        <title>Auth Failed - Aesthetic Computer</title>
        <style>
          body { font-family: monospace; max-width: 600px; margin: 50px auto; padding: 20px; }
          .error { color: red; }
          code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; }
        </style>
      </head>
      <body>
        <h1>❌ Authentication Failed</h1>
        <p class="error"><strong>Error:</strong> ${error}</p>
        <p><strong>Description:</strong> ${errorDescription || "Unknown error"}</p>
        <hr>
        <p>You can close this window and try again in your terminal.</p>
      </body>
      </html>
    `, { "content-type": "text/html" });
  }

  // Validate required parameters
  if (!code || !state) {
    return respond(400, null, `
      <!DOCTYPE html>
      <html>
      <head>
        <title>Invalid Request - Aesthetic Computer</title>
        <style>
          body { font-family: monospace; max-width: 600px; margin: 50px auto; padding: 20px; }
        </style>
      </head>
      <body>
        <h1>⚠️ Invalid Request</h1>
        <p>Missing required parameters (code or state).</p>
        <p>This endpoint should only be accessed via OAuth redirect.</p>
      </body>
      </html>
    `, { "content-type": "text/html" });
  }

  // Exchange code for tokens
  const AUTH0_DOMAIN = process.env.AUTH0_DOMAIN;
  const AUTH0_CLIENT_ID = process.env.AUTH0_CLIENT_ID;
  const AUTH0_CLIENT_SECRET = process.env.AUTH0_CLIENT_SECRET;
  const REDIRECT_URI = `https://aesthetic.computer/api/auth/cli-callback`;

  try {
    const tokenResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        grant_type: "authorization_code",
        client_id: AUTH0_CLIENT_ID,
        client_secret: AUTH0_CLIENT_SECRET,
        code: code,
        redirect_uri: REDIRECT_URI,
      }),
    });

    if (!tokenResponse.ok) {
      const errorData = await tokenResponse.text();
      console.error("Token exchange failed:", errorData);
      
      return respond(500, null, `
        <!DOCTYPE html>
        <html>
        <head>
          <title>Token Exchange Failed - Aesthetic Computer</title>
          <style>
            body { font-family: monospace; max-width: 600px; margin: 50px auto; padding: 20px; }
            .error { color: red; }
          </style>
        </head>
        <body>
          <h1>❌ Token Exchange Failed</h1>
          <p class="error">Failed to exchange authorization code for tokens.</p>
          <p>Please try again or check the server logs.</p>
        </body>
        </html>
      `, { "content-type": "text/html" });
    }

    const tokens = await tokenResponse.json();

    // Get user info
    const userResponse = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: { Authorization: `Bearer ${tokens.access_token}` },
    });

    const user = userResponse.ok ? await userResponse.json() : {};

    // Return HTML page with tokens embedded (CLI will parse this)
    return respond(200, null, `
      <!DOCTYPE html>
      <html>
      <head>
        <title>Authentication Success - Aesthetic Computer</title>
        <style>
          body { 
            font-family: monospace; 
            max-width: 600px; 
            margin: 50px auto; 
            padding: 20px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
          }
          .success { 
            background: rgba(255,255,255,0.1);
            padding: 20px;
            border-radius: 8px;
            backdrop-filter: blur(10px);
          }
          h1 { margin-top: 0; }
          code { 
            background: rgba(0,0,0,0.3); 
            padding: 2px 6px; 
            border-radius: 3px;
            display: inline-block;
          }
          .token-data {
            display: none;
          }
        </style>
      </head>
      <body>
        <div class="success">
          <h1>✅ Authentication Successful</h1>
          <p><strong>Welcome!</strong> ${user.email || user.name || "User"}</p>
          <p>You are now logged in to <strong>Aesthetic Computer</strong>.</p>
          <hr style="border-color: rgba(255,255,255,0.3);">
          <p>You can now <strong>close this window</strong> and return to your terminal.</p>
          <p style="font-size: 0.9em; opacity: 0.8;">Your CLI tool has received the authentication tokens.</p>
        </div>
        
        <!-- Token data for CLI parsing (hidden from user) -->
        <div class="token-data">
          <pre id="token-payload">${JSON.stringify({ tokens, user, state }, null, 2)}</pre>
        </div>
        
        <script>
          // Optionally send token data to parent window if CLI opens in iframe
          if (window.opener) {
            try {
              window.opener.postMessage({ 
                type: 'ac-auth-success',
                data: ${JSON.stringify({ tokens, user, state })}
              }, '*');
            } catch (e) {
              console.error('Failed to post message to opener:', e);
            }
          }
        </script>
      </body>
      </html>
    `, { "content-type": "text/html" });

  } catch (err) {
    console.error("Callback error:", err);
    return respond(500, null, `
      <!DOCTYPE html>
      <html>
      <head>
        <title>Server Error - Aesthetic Computer</title>
        <style>
          body { font-family: monospace; max-width: 600px; margin: 50px auto; padding: 20px; }
          .error { color: red; }
        </style>
      </head>
      <body>
        <h1>❌ Server Error</h1>
        <p class="error">An unexpected error occurred during authentication.</p>
        <p>Please try again or contact support.</p>
      </body>
      </html>
    `, { "content-type": "text/html" });
  }
}
