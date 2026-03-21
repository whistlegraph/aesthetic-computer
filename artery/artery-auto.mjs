#!/usr/bin/env node
/**
 * 🩸 Artery Auto - Automatically selects CDP or Electron adapter
 * 
 * Import this instead of artery.mjs to get the right adapter for your environment:
 * - In VS Code with CDP: uses artery.mjs (CDP-based)
 * - In Electron app: uses artery-electron.mjs (file-based IPC)
 */

import fs from 'fs';
import http from 'http';

const BRIDGE_DIR = '/workspaces/aesthetic-computer/.electron-bridge';

/**
 * Test if CDP is available at localhost:9333 or other common ports
 */
async function testCDP() {
  const candidates = [
    { host: 'localhost', port: 9333 },
    { host: 'localhost', port: 9222 },
    { host: 'host.docker.internal', port: 9333 },
    { host: 'host.docker.internal', port: 9222 },
  ];
  
  for (const { host, port } of candidates) {
    try {
      const works = await new Promise((resolve) => {
        const req = http.get({
          hostname: host,
          port,
          path: '/json',
          timeout: 1000,
          headers: { 'Host': 'localhost' }
        }, (res) => {
          let data = '';
          res.on('data', (chunk) => data += chunk);
          res.on('end', () => resolve(data.length > 10));
        });
        req.on('error', () => resolve(false));
        req.on('timeout', () => { req.destroy(); resolve(false); });
      });
      if (works) return true;
    } catch (e) {
      // Continue
    }
  }
  return false;
}

/**
 * Check if Electron bridge is available
 */
function isElectronBridge() {
  return fs.existsSync(BRIDGE_DIR) || process.env.AC_ELECTRON === 'true';
}

/**
 * Get the appropriate Artery class
 */
export async function getArtery() {
  // First check if CDP is available
  const cdpAvailable = await testCDP();
  
  if (cdpAvailable) {
    console.log('🩸 Using CDP adapter (VS Code)');
    const { default: Artery } = await import('./artery.mjs');
    return Artery;
  }
  
  // Check if Electron bridge exists
  if (isElectronBridge()) {
    console.log('🩸 Using Electron adapter');
    const { default: ArteryElectron } = await import('./artery-electron.mjs');
    return ArteryElectron;
  }
  
  // No adapter available - create bridge directory to enable Electron mode
  console.log('🩸 Creating Electron bridge directory...');
  fs.mkdirSync(BRIDGE_DIR, { recursive: true });
  const { default: ArteryElectron } = await import('./artery-electron.mjs');
  return ArteryElectron;
}

/**
 * Create and connect an Artery instance
 */
export async function createArtery() {
  const ArteryClass = await getArtery();
  const artery = new ArteryClass();
  return artery;
}

// Default export for drop-in replacement
export default { getArtery, createArtery };
