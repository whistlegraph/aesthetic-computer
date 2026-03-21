#!/usr/bin/env node
/**
 * ac-login - CLI authentication for Aesthetic Computer
 * 
 * Opens browser for Auth0 login, captures token, stores locally
 * Usage: node ac-login.mjs
 */

import http from 'http';
import { exec } from 'child_process';
import { promises as fs } from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import os from 'os';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Auth0 config (from boot.mjs)
const AUTH0_DOMAIN = 'aesthetic.us.auth0.com';
const AUTH0_CLIENT_ID = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
const CALLBACK_PORT = 44233; // Random high port
const REDIRECT_URI = 'http://localhost:8888'; // Use localhost:8888 (already whitelisted)
const TOKEN_FILE = path.join(os.homedir(), '.ac-token');

// Generate random state for CSRF protection
function generateState() {
  return Math.random().toString(36).substring(2, 15) + 
         Math.random().toString(36).substring(2, 15);
}

// Open browser (devcontainer-aware)
function openBrowser(url) {
  // Use $BROWSER if available (devcontainer forwards to host)
  const browserCmd = process.env.BROWSER || 
                     (process.platform === 'darwin' ? 'open' :
                      process.platform === 'win32' ? 'start' :
                      'xdg-open');
  
  exec(`${browserCmd} "${url}"`, (err) => {
    if (err) {
      console.error('   ⚠️  Could not auto-open browser:', err.message);
    }
  });
}

// Start local callback server
async function startCallbackServer() {
  return new Promise((resolve, reject) => {
    const server = http.createServer(async (req, res) => {
      const url = new URL(req.url, `http://localhost:${CALLBACK_PORT}`);
      
      // Handle any path - Auth0 redirects to REDIRECT_URI with query params
      const code = url.searchParams.get('code');
      const state = url.searchParams.get('state');
      const error = url.searchParams.get('error');
      const errorDesc = url.searchParams.get('error_description');
      
      // Only process if we have code or error params (ignore other requests)
      if (!code && !error) {
        res.writeHead(404);
        res.end('Not found');
        return;
      }
      
      if (error) {
          res.writeHead(200, { 'Content-Type': 'text/html' });
          res.end(`
            <html>
              <body style="font-family: monospace; padding: 40px; background: #0a0a0f; color: #e0e0e8;">
                <h1>❌ Authentication Failed</h1>
                <p>Error: ${error}</p>
                <p>${errorDesc || ''}</p>
                <p>You can close this window.</p>
              </body>
            </html>
          `);
          server.close();
          reject(new Error(error));
          return;
        }
        
        if (code) {
          console.log('✅ Authorization code received');
          
          // Exchange code for token
          try {
            const tokenResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({
                grant_type: 'authorization_code',
                client_id: AUTH0_CLIENT_ID,
                code,
                redirect_uri: REDIRECT_URI,
              }),
            });
            
            if (!tokenResponse.ok) {
              const errorData = await tokenResponse.text();
              throw new Error(`Token exchange failed: ${errorData}`);
            }
            
            const tokens = await tokenResponse.json();
            
            // Save token to file
            await fs.writeFile(TOKEN_FILE, JSON.stringify(tokens, null, 2), 'utf8');
            console.log(`💾 Token saved to ${TOKEN_FILE}`);
            
            // Get user info
            const userInfo = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
              headers: { Authorization: `Bearer ${tokens.access_token}` },
            });
            
            const user = await userInfo.json();
            
            res.writeHead(200, { 'Content-Type': 'text/html' });
            res.end(`
              <html>
                <body style="font-family: monospace; padding: 40px; background: #0a0a0f; color: #e0e0e8;">
                  <h1>✅ Login Successful!</h1>
                  <p>Logged in as: <strong>${user.email || user.sub}</strong></p>
                  <p>You can close this window and return to your terminal.</p>
                </body>
              </html>
            `);
            
            server.close();
            resolve({ tokens, user });
          } catch (err) {
            console.error('❌ Token exchange failed:', err.message);
            res.writeHead(500, { 'Content-Type': 'text/html' });
            res.end(`
              <html>
                <body style="font-family: monospace; padding: 40px; background: #0a0a0f; color: #e0e0e8;">
                  <h1>❌ Token Exchange Failed</h1>
                  <p>${err.message}</p>
                  <p>You can close this window.</p>
                </body>
              </html>
            `);
            server.close();
            reject(err);
          }
        }
      // End of request processing - close all cases
    });
    
    server.listen(CALLBACK_PORT, () => {
      console.log(`🌐 Callback server listening on http://localhost:${CALLBACK_PORT}`);
    });
    
    server.on('error', reject);
  });
}

// Main login flow
async function login() {
  console.log('╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔐 Aesthetic Computer CLI Login                            ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  const state = generateState();
  
  // Build Auth0 authorization URL
  const authUrl = new URL(`https://${AUTH0_DOMAIN}/authorize`);
  authUrl.searchParams.set('response_type', 'code');
  authUrl.searchParams.set('client_id', AUTH0_CLIENT_ID);
  authUrl.searchParams.set('redirect_uri', REDIRECT_URI);
  authUrl.searchParams.set('scope', 'openid profile email');
  authUrl.searchParams.set('state', state);
  
  console.log('📂 Opening browser for authentication...\n');
  console.log('   If your browser doesn\'t open automatically, visit:');
  console.log(`   ${authUrl.toString()}\n`);
  
  openBrowser(authUrl.toString());
  
  try {
    const { tokens, user } = await startCallbackServer();
    
    console.log('\n✅ Successfully authenticated!');
    console.log(`👤 User: ${user.email || user.sub}`);
    console.log(`💾 Token stored in: ${TOKEN_FILE}`);
    console.log('\nYou can now use authenticated API calls.');
    
    return tokens;
  } catch (err) {
    console.error('\n❌ Login failed:', err.message);
    process.exit(1);
  }
}

// Check if already logged in
async function checkAuth() {
  try {
    const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
    const tokens = JSON.parse(tokenData);
    
    // Try to get user info with existing token
    const userInfo = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: { Authorization: `Bearer ${tokens.access_token}` },
    });
    
    if (userInfo.ok) {
      const user = await userInfo.json();
      console.log('✅ Already logged in');
      console.log(`👤 User: ${user.email || user.sub}`);
      return tokens;
    } else {
      throw new Error('Token expired or invalid');
    }
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
      console.log(tokens.access_token);
    } catch (err) {
      console.error('❌ No token found. Run: node ac-login.mjs');
      process.exit(1);
    }
    return;
  }
  
  // Default: login
  const existing = await checkAuth();
  if (existing) {
    const answer = await new Promise(resolve => {
      process.stdout.write('Already logged in. Re-authenticate? (y/N): ');
      process.stdin.once('data', data => resolve(data.toString().trim()));
    });
    
    if (answer.toLowerCase() !== 'y') {
      console.log('Keeping existing session');
      process.exit(0);
    }
  }
  
  await login();
})();
