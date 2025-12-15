#!/usr/bin/env node
/**
 * ac-login - CLI authentication for Aesthetic Computer
 * 
 * Uses OAuth 2.0 Authorization Code Flow with hosted callback
 * Opens browser → user logs in → callback endpoint returns token → CLI polls for it
 * 
 * Usage: 
 *   node ac-login.mjs         - Login
 *   node ac-login.mjs status  - Check login status
 *   node ac-login.mjs logout  - Clear stored token
 *   node ac-login.mjs token   - Show current token
 */

import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { exec } from 'child_process';
import http from 'http';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Config
const AUTH0_DOMAIN = 'aesthetic.us.auth0.com';
const AUTH0_CLIENT_ID = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
const CALLBACK_PORT = 44233;
const REDIRECT_URI = `http://localhost:${CALLBACK_PORT}/callback`;
const TOKEN_FILE = join(process.env.HOME, '.ac-token');

// Generate random state
function generateState() {
  return Math.random().toString(36).substring(2, 15) + 
         Math.random().toString(36).substring(2, 15);
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

// Parse HTML response to extract JSON token data
function extractTokenFromHTML(html) {
  const match = html.match(/<pre id="token-payload">([\s\S]*?)<\/pre>/);
  if (match) {
    try {
      return JSON.parse(match[1]);
    } catch (e) {
      return null;
    }
  }
  return null;
}

// Main login flow
async function login() {
  console.log('╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔐 Aesthetic Computer CLI Login                            ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  const state = generateState();
  
  try {
    const { tokens, user } = await startLocalCallbackServer(state);
    
    console.log('\n✅ Authentication successful!\n');
    console.log(`👤 User: ${user.email || user.name || user.sub}`);
    console.log(`💾 Token stored in: ${TOKEN_FILE}`);
    console.log('\nYou can now use authenticated API calls.');
    
    return tokens;
  } catch (err) {
    console.error('\n❌ Login failed:', err.message);
    process.exit(1);
  }
}

// Start local callback server
async function startLocalCallbackServer(state) {
  return new Promise((resolve, reject) => {
    const server = http.createServer(async (req, res) => {
      const url = new URL(req.url, `http://localhost:${CALLBACK_PORT}`);
      
      const code = url.searchParams.get('code');
      const returnedState = url.searchParams.get('state');
      const error = url.searchParams.get('error');
      
      // Ignore requests without auth params
      if (!code && !error) {
        res.writeHead(404);
        res.end('Not found');
        return;
      }
      
      if (error) {
        res.writeHead(200, { 'Content-Type': 'text/html' });
        res.end(`
          <html>
            <body style="font-family: monospace; padding: 40px;">
              <h1>❌ Authentication Failed</h1>
              <p>Error: ${error}</p>
              <p>${url.searchParams.get('error_description') || ''}</p>
              <p>You can close this window.</p>
            </body>
          </html>
        `);
        server.close();
        reject(new Error(error));
        return;
      }
      
      try {
        // Exchange code for tokens
        const tokenResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            grant_type: 'authorization_code',
            client_id: AUTH0_CLIENT_ID,
            code: code,
            redirect_uri: REDIRECT_URI,
          }),
        });
        
        if (!tokenResponse.ok) {
          throw new Error(`Token exchange failed: ${await tokenResponse.text()}`);
        }
        
        const tokens = await tokenResponse.json();
        
        // Get user info
        const userResponse = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
          headers: { 'Authorization': `Bearer ${tokens.access_token}` },
        });
        
        const user = await userResponse.json();
        
        // Save tokens
        await fs.writeFile(TOKEN_FILE, JSON.stringify({
          ...tokens,
          user,
          expires_at: Date.now() + (tokens.expires_in * 1000),
        }, null, 2));
        
        // Success page
        res.writeHead(200, { 'Content-Type': 'text/html' });
        res.end(`
          <html>
            <body style="font-family: monospace; padding: 50px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;">
              <h1>✅ Authentication Successful!</h1>
              <p>Welcome, <strong>${user.email || user.name}</strong>!</p>
              <p>You can now close this window and return to your terminal.</p>
            </body>
          </html>
        `);
        
        server.close();
        resolve({ tokens, user });
      } catch (err) {
        res.writeHead(500, { 'Content-Type': 'text/plain' });
        res.end(`Error: ${err.message}`);
        server.close();
        reject(err);
      }
    });
    
    server.listen(CALLBACK_PORT, () => {
      console.log(`🌐 Starting callback server on http://localhost:${CALLBACK_PORT}...\n`);
      
      // Build and open Auth0 URL
      const authUrl = new URL(`https://${AUTH0_DOMAIN}/authorize`);
      authUrl.searchParams.set('response_type', 'code');
      authUrl.searchParams.set('client_id', AUTH0_CLIENT_ID);
      authUrl.searchParams.set('redirect_uri', REDIRECT_URI);
      authUrl.searchParams.set('scope', 'openid profile email');
      authUrl.searchParams.set('state', state);
      
      console.log('📂 Opening browser for authentication...\n');
      console.log('   If your browser doesn\'t open, visit:');
      console.log(`   ${authUrl.toString()}\n`);
      
      openBrowser(authUrl.toString());
      
      console.log('⏳ Waiting for authentication...\n');
    });
    
    server.on('error', (err) => {
      if (err.code === 'EADDRINUSE') {
        reject(new Error(`Port ${CALLBACK_PORT} is already in use`));
      } else {
        reject(err);
      }
    });
  });
}

// Check if already logged in
async function checkAuth() {
  try {
    const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
    const tokens = JSON.parse(tokenData);
    
    if (tokens.expires_at && Date.now() > tokens.expires_at) {
      console.log('⚠️  Token expired');
      return null;
    }
    
    console.log('✅ Logged in');
    console.log(`👤 User: ${tokens.user?.email || tokens.user?.name || tokens.user?.sub || 'Unknown'}`);
    console.log(`🕐 Token expires: ${new Date(tokens.expires_at).toLocaleString()}`);
    
    return tokens;
  } catch (err) {
    return null;
  }
}

// Main
(async () => {
  const command = process.argv[2];
  
  if (command === 'logout') {
    try {
      await fs.unlink(TOKEN_FILE);
      console.log('✅ Logged out successfully');
    } catch (err) {
      console.log('Already logged out');
    }
    return;
  }
  
  if (command === 'status') {
    const tokens = await checkAuth();
    if (!tokens) {
      console.log('❌ Not logged in');
      console.log('Run: node ac-login.mjs');
      process.exit(1);
    }
    return;
  }
  
  if (command === 'token') {
    try {
      const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
      const tokens = JSON.parse(tokenData);
      console.log('Access Token:', tokens.access_token);
      console.log('\nFull token data:');
      console.log(JSON.stringify(tokens, null, 2));
    } catch (err) {
      console.log('❌ Not logged in');
      console.log('Run: node ac-login.mjs');
      process.exit(1);
    }
    return;
  }
  
  // Default: login
  await startLocalCallbackServer(generateState());
})();
