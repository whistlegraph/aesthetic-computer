#!/usr/bin/env node
/**
 * ac-login - CLI authentication for Aesthetic Computer
 * 
 * Uses OAuth 2.0 Authorization Code Flow with PKCE + local callback server
 * Opens browser → user logs in → callback returns to localhost → token saved
 * 
 * Usage: 
 *   node ac-login.mjs         - Login (opens browser)
 *   node ac-login.mjs status  - Check login status
 *   node ac-login.mjs logout  - Clear stored token
 *   node ac-login.mjs token   - Print access token (for scripts)
 */

import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { exec } from 'child_process';
import http from 'http';
import crypto from 'crypto';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Config - matches vscode-extension and kidlisp.com
const AUTH0_DOMAIN = 'hi.aesthetic.computer';
const AUTH0_CLIENT_ID = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
const CALLBACK_PORT = 44233;  // Matches Auth0 allowed callback URL
const REDIRECT_URI = `http://localhost:${CALLBACK_PORT}/callback`;
const TOKEN_FILE = join(process.env.HOME, '.ac-token');

// PKCE helpers
function base64URLEncode(buffer) {
  return buffer.toString('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
}

function generateCodeVerifier() {
  return base64URLEncode(crypto.randomBytes(32));
}

function generateCodeChallenge(verifier) {
  const hash = crypto.createHash('sha256').update(verifier).digest();
  return base64URLEncode(hash);
}

// Open browser (devcontainer-aware)
function openBrowser(url) {
  const browserCmd = process.env.BROWSER;
  if (browserCmd) {
    exec(`${browserCmd} "${url}"`, (err) => {
      if (err) console.error('Failed to open browser:', err.message);
    });
  } else {
    const cmd = process.platform === 'darwin' ? 'open' :
                process.platform === 'win32' ? 'start' : 'xdg-open';
    exec(`${cmd} "${url}"`, (err) => {
      if (err) console.error('Failed to open browser:', err.message);
    });
  }
}

// Main login flow
async function login() {
  console.log('╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔐 Aesthetic Computer CLI Login                             ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  const codeVerifier = generateCodeVerifier();
  const codeChallenge = generateCodeChallenge(codeVerifier);
  const state = crypto.randomBytes(16).toString('hex');
  
  try {
    const { tokens, user } = await startLocalCallbackServer(state, codeVerifier, codeChallenge);
    
    const displayName = user.handle ? `@${user.handle}` : user.email || user.name;
    console.log('\n✅ Authentication successful!\n');
    console.log(`👤 User: ${displayName}`);
    console.log(`💾 Token stored in: ${TOKEN_FILE}`);
    console.log('\nYou can now use authenticated API calls.\n');
    
    return tokens;
  } catch (err) {
    console.error('\n❌ Login failed:', err.message);
    process.exit(1);
  }
}

// Start local callback server
async function startLocalCallbackServer(state, codeVerifier, codeChallenge) {
  return new Promise((resolve, reject) => {
    const server = http.createServer(async (req, res) => {
      const url = new URL(req.url, `http://localhost:${CALLBACK_PORT}`);
      
      if (!url.pathname.includes('callback')) {
        res.writeHead(404);
        res.end('Not found');
        return;
      }
      
      const code = url.searchParams.get('code');
      const error = url.searchParams.get('error');
      
      if (error) {
        res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
        res.end(`<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Login Failed · Aesthetic Computer</title>
  <style>
    @media (prefers-color-scheme: dark) {
      body { background: rgb(50, 30, 60); color: rgb(220, 30, 100); }
      code { color: pink; }
      mark { background-color: maroon; color: pink; }
    }
    @media (prefers-color-scheme: light) {
      body { background: rgb(255, 240, 245); color: rgb(180, 20, 80); }
      code { color: rgb(150, 50, 100); }
      mark { background-color: rgb(255, 200, 220); color: rgb(150, 20, 60); }
    }
    body {
      font-family: monospace;
      display: flex;
      width: 100vw;
      height: 100vh;
      margin: 0;
    }
    #wrapper { margin: auto; text-align: center; padding: 2em; }
    h1 { font-family: sans-serif; font-weight: normal; }
    code { font-size: 120%; opacity: 0.75; }
  </style>
</head>
<body>
  <div id="wrapper">
    <h1>❌ Authentication Failed</h1>
    <p><mark>${error}</mark></p>
    <p>${url.searchParams.get('error_description') || ''}</p>
    <p><code>You may close this page.</code></p>
  </div>
</body>
</html>`);
        server.close();
        reject(new Error(error));
        return;
      }
      
      if (!code) {
        res.writeHead(400);
        res.end('Missing authorization code');
        return;
      }
      
      try {
        const tokenResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            grant_type: 'authorization_code',
            client_id: AUTH0_CLIENT_ID,
            code_verifier: codeVerifier,
            code: code,
            redirect_uri: REDIRECT_URI,
          }),
        });
        
        if (!tokenResponse.ok) {
          const errText = await tokenResponse.text();
          throw new Error(`Token exchange failed: ${errText}`);
        }
        
        const tokens = await tokenResponse.json();
        
        const userResponse = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
          headers: { 'Authorization': `Bearer ${tokens.access_token}` },
        });
        
        const user = await userResponse.json();
        
        // Fetch AC handle
        let acHandle = null;
        try {
          const handleRes = await fetch(
            `https://aesthetic.computer/user?from=${encodeURIComponent(user.email)}&withHandle=true`
          );
          const handleData = await handleRes.json();
          if (handleData.handle) acHandle = handleData.handle;
        } catch (e) {}
        
        await fs.writeFile(TOKEN_FILE, JSON.stringify({
          access_token: tokens.access_token,
          refresh_token: tokens.refresh_token,
          id_token: tokens.id_token,
          expires_at: Date.now() + (tokens.expires_in * 1000),
          user: { email: user.email, name: user.name, sub: user.sub, picture: user.picture, handle: acHandle },
        }, null, 2));
        
        const displayName = acHandle ? `@${acHandle}` : user.email || user.name;
        res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
        res.end(`<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Logged In · Aesthetic Computer</title>
  <style>
    @media (prefers-color-scheme: dark) {
      body { background: rgb(50, 30, 60); color: rgb(220, 30, 100); }
      code { color: pink; }
      strong { color: white; }
    }
    @media (prefers-color-scheme: light) {
      body { background: rgb(255, 240, 245); color: rgb(180, 20, 80); }
      code { color: rgb(150, 50, 100); }
      strong { color: rgb(100, 20, 60); }
    }
    body {
      font-family: monospace;
      display: flex;
      width: 100vw;
      height: 100vh;
      margin: 0;
    }
    #wrapper { margin: auto; text-align: center; padding: 2em; }
    h1 { font-family: sans-serif; font-weight: normal; }
    code { font-size: 120%; opacity: 0.75; }
  </style>
</head>
<body>
  <div id="wrapper">
    <h1>✅ Login Successful</h1>
    <p>Welcome, <strong>${displayName}</strong>!</p>
    <p><code>You may close this page.</code></p>
  </div>
  <script>setTimeout(() => window.close(), 3000)</script>
</body>
</html>`);
        
        server.close();
        resolve({ tokens, user: { ...user, handle: acHandle } });
      } catch (err) {
        res.writeHead(500, { 'Content-Type': 'text/plain' });
        res.end(`Error: ${err.message}`);
        server.close();
        reject(err);
      }
    });
    
    server.listen(CALLBACK_PORT, () => {
      const authUrl = new URL(`https://${AUTH0_DOMAIN}/authorize`);
      authUrl.searchParams.set('response_type', 'code');
      authUrl.searchParams.set('client_id', AUTH0_CLIENT_ID);
      authUrl.searchParams.set('redirect_uri', REDIRECT_URI);
      authUrl.searchParams.set('scope', 'openid profile email offline_access');
      authUrl.searchParams.set('state', state);
      authUrl.searchParams.set('code_challenge', codeChallenge);
      authUrl.searchParams.set('code_challenge_method', 'S256');
      
      console.log('🌐 Opening browser for authentication...\n');
      console.log('   If your browser doesn\'t open, visit:');
      console.log(`   ${authUrl.toString().slice(0, 80)}...\n`);
      
      openBrowser(authUrl.toString());
      console.log('⏳ Waiting for authentication...\n');
    });
    
    server.on('error', (err) => {
      if (err.code === 'EADDRINUSE') {
        reject(new Error(`Port ${CALLBACK_PORT} is already in use.`));
      } else {
        reject(err);
      }
    });
    
    setTimeout(() => { server.close(); reject(new Error('Login timeout')); }, 5 * 60 * 1000);
  });
}

async function checkAuth() {
  try {
    const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
    const tokens = JSON.parse(tokenData);
    const expired = tokens.expires_at && Date.now() > tokens.expires_at;
    const expiresIn = Math.floor((tokens.expires_at - Date.now()) / 1000 / 60);
    
    console.log('╔══════════════════════════════════════════════════════════════╗');
    console.log('║  🔐 AC Login Status                                          ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    if (expired) {
      console.log('⚠️  Token expired - run `ac-login` to refresh\n');
    } else {
      console.log('✅ Logged in\n');
    }
    
    const displayName = tokens.user?.handle 
      ? `@${tokens.user.handle}` 
      : tokens.user?.email || tokens.user?.name || 'unknown';
    
    console.log(`👤 User: ${displayName}`);
    console.log(`📧 Email: ${tokens.user?.email || 'n/a'}`);
    console.log(`⏰ Expires: ${expired ? 'EXPIRED' : `in ${expiresIn} minutes`}`);
    console.log(`📁 Token: ${TOKEN_FILE}\n`);
    
    return expired ? null : tokens;
  } catch (err) {
    console.log('❌ Not logged in\n');
    console.log('   Run `ac-login` to authenticate.\n');
    return null;
  }
}

export async function getToken() {
  try {
    const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
    const tokens = JSON.parse(tokenData);
    if (tokens.expires_at && Date.now() > tokens.expires_at) return null;
    return tokens;
  } catch (err) { return null; }
}

(async () => {
  const command = process.argv[2];
  
  if (command === 'logout') {
    try { await fs.unlink(TOKEN_FILE); console.log('✅ Logged out\n'); }
    catch { console.log('ℹ️  Already logged out\n'); }
    return;
  }
  
  if (command === 'status') { await checkAuth(); return; }
  
  if (command === 'token') {
    try {
      const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
      const tokens = JSON.parse(tokenData);
      if (tokens.expires_at && Date.now() > tokens.expires_at) {
        console.error('Token expired'); process.exit(1);
      }
      console.log(tokens.access_token);
    } catch { console.error('Not logged in'); process.exit(1); }
    return;
  }
  
  if (command === 'help' || command === '-h' || command === '--help') {
    console.log(`
🔐 ac-login - Aesthetic Computer CLI Authentication

Usage:
  ac-login          Login (opens browser)
  ac-login status   Check login status  
  ac-login logout   Clear stored token
  ac-login token    Print access token (for scripts)
  ac-login help     Show this help
`);
    return;
  }
  
  await login();
})();
