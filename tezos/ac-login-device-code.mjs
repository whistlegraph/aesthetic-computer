#!/usr/bin/env node
/**
 * ac-login - CLI authentication for Aesthetic Computer
 * 
 * Uses OAuth 2.0 Device Code Flow (no localhost needed!)
 * User visits URL on any device, enters code, CLI gets token
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

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Config
const AUTH0_DOMAIN = 'aesthetic.us.auth0.com';
const AUTH0_CLIENT_ID = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
const TOKEN_FILE = join(process.env.HOME, '.ac-token');
const POLL_INTERVAL = 5000; // 5 seconds

// Open browser (devcontainer-aware)
function openBrowser(url) {
  const browserCmd = process.env.BROWSER;
  if (browserCmd) {
    // In devcontainer with BROWSER env var
    exec(`${browserCmd} "${url}"`, (err) => {
      if (err) console.error('Failed to open browser:', err.message);
    });
  } else {
    // Standard environment
    const cmd = process.platform === 'darwin' ? 'open' :
                process.platform === 'win32' ? 'start' : 'xdg-open';
    exec(`${cmd} "${url}"`, (err) => {
      if (err) console.error('Failed to open browser:', err.message);
    });
  }
}

// Sleep helper
const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));

// Main login flow using Device Code Flow
async function login() {
  console.log('╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔐 Aesthetic Computer CLI Login                            ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  try {
    // Step 1: Request device code
    console.log('📱 Requesting device code...\n');
    
    const deviceResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/device/code`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        client_id: AUTH0_CLIENT_ID,
        scope: 'openid profile email offline_access',
        audience: 'https://aesthetic.us.auth0.com/api/v2/',
      }),
    });
    
    if (!deviceResponse.ok) {
      const error = await deviceResponse.text();
      throw new Error(`Failed to get device code: ${error}`);
    }
    
    const deviceData = await deviceResponse.json();
    
    // Step 2: Display instructions to user
    console.log('┌────────────────────────────────────────────────────────────┐');
    console.log('│  Please complete authentication in your browser:          │');
    console.log('├────────────────────────────────────────────────────────────┤');
    console.log(`│  1. Visit: ${deviceData.verification_uri_complete || deviceData.verification_uri}`.padEnd(61) + '│');
    console.log('│                                                            │');
    console.log(`│  2. Enter code: \x1b[1m${deviceData.user_code}\x1b[0m`.padEnd(71) + '│');
    console.log('└────────────────────────────────────────────────────────────┘\n');
    
    // Auto-open browser if complete URI is provided
    if (deviceData.verification_uri_complete) {
      console.log('🌐 Opening browser...\n');
      openBrowser(deviceData.verification_uri_complete);
    }
    
    console.log(`⏳ Waiting for authentication (expires in ${Math.floor(deviceData.expires_in / 60)} minutes)...`);
    console.log('   Press Ctrl+C to cancel\n');
    
    // Step 3: Poll for tokens
    const startTime = Date.now();
    const expiresAt = startTime + (deviceData.expires_in * 1000);
    const interval = deviceData.interval ? deviceData.interval * 1000 : POLL_INTERVAL;
    
    let dots = 0;
    while (Date.now() < expiresAt) {
      const tokenResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: new URLSearchParams({
          grant_type: 'urn:ietf:params:oauth:grant-type:device_code',
          device_code: deviceData.device_code,
          client_id: AUTH0_CLIENT_ID,
        }),
      });
      
      const tokenData = await tokenResponse.json();
      
      if (tokenResponse.ok) {
        // Success! Got tokens
        console.log('\n\n✅ Authentication successful!\n');
        
        // Get user info
        const userResponse = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
          headers: { 'Authorization': `Bearer ${tokenData.access_token}` },
        });
        
        const user = await userResponse.json();
        
        // Save tokens
        await fs.writeFile(TOKEN_FILE, JSON.stringify({
          ...tokenData,
          user,
          expires_at: Date.now() + (tokenData.expires_in * 1000),
        }, null, 2));
        
        console.log(`👤 User: ${user.email || user.name || user.sub}`);
        console.log(`💾 Token stored in: ${TOKEN_FILE}`);
        console.log('\nYou can now use authenticated API calls.');
        
        return tokenData;
      }
      
      // Handle errors
      if (tokenData.error === 'authorization_pending') {
        // Still waiting for user to authorize
        process.stdout.write('.');
        dots++;
        if (dots % 10 === 0) process.stdout.write(` ${Math.floor((Date.now() - startTime) / 1000)}s\n`);
        await sleep(interval);
        continue;
      }
      
      if (tokenData.error === 'slow_down') {
        // We're polling too fast, increase interval
        await sleep(interval + 5000);
        continue;
      }
      
      if (tokenData.error === 'expired_token') {
        throw new Error('Device code expired. Please try again.');
      }
      
      if (tokenData.error === 'access_denied') {
        throw new Error('Access denied. You cancelled the authorization.');
      }
      
      // Unknown error
      throw new Error(`Authentication failed: ${tokenData.error} - ${tokenData.error_description || 'Unknown error'}`);
    }
    
    throw new Error('Authentication timeout. Please try again.');
    
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
  await login();
})();
