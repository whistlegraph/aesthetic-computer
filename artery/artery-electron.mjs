#!/usr/bin/env node
/**
 * 🩸 Artery Electron Adapter
 * 
 * Provides CDP-compatible interface for running Artery tests in the Electron app.
 * Uses Electron's IPC bridge instead of Chrome DevTools Protocol.
 * 
 * Communication happens via file-based IPC since we're in a devcontainer
 * that's connected to Electron via PTY (not direct IPC).
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Colors
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const RED_BG = '\x1b[41m';
const PINK = '\x1b[95m';
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const CYAN = '\x1b[96m';
const GREEN = '\x1b[92m';
const YELLOW = '\x1b[93m';

const redLog = (msg) => console.log(`${RED_BG}${PINK}${msg}${RESET}`);
const brightLog = (msg) => console.log(`${RED_BG}${PINK}${BOLD}${msg}${RESET}`);
const darkLog = (msg) => console.log(`${PURPLE_BG}${WHITE}${msg}${RESET}`);

// Bridge directory - shared between container and Electron via mounted workspace
const BRIDGE_DIR = '/workspaces/aesthetic-computer/.electron-bridge';
const CMD_FILE = `${BRIDGE_DIR}/command.json`;
const RESULT_FILE = `${BRIDGE_DIR}/result.json`;

function ensureBridgeDir() {
  if (!fs.existsSync(BRIDGE_DIR)) {
    fs.mkdirSync(BRIDGE_DIR, { recursive: true });
  }
}

// File-based IPC for container <-> Electron communication
async function sendCommand(cmd, params = {}, timeout = 10000) {
  ensureBridgeDir();
  
  const request = {
    id: Date.now() + Math.random(),
    cmd,
    params,
    timestamp: new Date().toISOString()
  };
  
  // Clear any old result
  if (fs.existsSync(RESULT_FILE)) {
    fs.unlinkSync(RESULT_FILE);
  }
  
  // Write command
  fs.writeFileSync(CMD_FILE, JSON.stringify(request, null, 2));
  
  // Wait for result
  const startTime = Date.now();
  while (Date.now() - startTime < timeout) {
    if (fs.existsSync(RESULT_FILE)) {
      try {
        const content = fs.readFileSync(RESULT_FILE, 'utf8');
        const result = JSON.parse(content);
        if (result.id === request.id) {
          fs.unlinkSync(RESULT_FILE);
          if (result.error) {
            throw new Error(result.error);
          }
          return result.data;
        }
      } catch (e) {
        if (e.code !== 'ENOENT') {
          // Parse error or other issue - keep waiting briefly
          await new Promise(r => setTimeout(r, 20));
        }
      }
    }
    await new Promise(r => setTimeout(r, 50));
  }
  
  throw new Error(`Timeout waiting for ${cmd} response`);
}

/**
 * ArteryElectron - CDP-compatible interface for Electron app
 */
class ArteryElectron {
  constructor() {
    this.connected = false;
    this.consoleCallback = null;
    this.messageId = 0;
  }

  async connect() {
    redLog('🩸 Connecting to Electron webview...');
    
    try {
      const state = await this.getState();
      if (state) {
        this.connected = true;
        brightLog('🩸 Connected to Electron!');
        return true;
      }
      throw new Error('No state returned');
    } catch (e) {
      throw new Error(`Failed to connect: ${e.message}`);
    }
  }

  async getState() {
    return sendCommand('get-state');
  }

  async jump(piece) {
    brightLog(`🩸 Jump: ${piece}`);
    return sendCommand('navigate', { piece });
  }

  async getCurrentPiece() {
    const state = await this.getState();
    return state?.piece || null;
  }

  // CDP-compatible send method
  async send(method, params = {}) {
    if (method === 'Runtime.evaluate') {
      const result = await this.eval(params.expression);
      return { result: { value: result } };
    }
    
    if (method === 'Runtime.enable') {
      return {};
    }
    
    if (method === 'Input.dispatchKeyEvent') {
      return this.dispatchKeyEvent(params);
    }
    
    console.warn(`[ArteryElectron] Unhandled method: ${method}`);
    return {};
  }

  async eval(expression) {
    const result = await sendCommand('eval', { code: expression });
    return result?.result;
  }

  async dispatchKeyEvent(params) {
    const { type, key, code, keyCode, windowsVirtualKeyCode, shiftKey, ctrlKey, altKey, metaKey } = params;
    
    const eventInit = JSON.stringify({
      key: key || '',
      code: code || '',
      keyCode: keyCode || windowsVirtualKeyCode || 0,
      which: keyCode || windowsVirtualKeyCode || 0,
      shiftKey: shiftKey || false,
      ctrlKey: ctrlKey || false,
      altKey: altKey || false,
      metaKey: metaKey || false,
      bubbles: true,
      cancelable: true
    });
    
    const eventType = type === 'keyDown' ? 'keydown' : type === 'keyUp' ? 'keyup' : type;
    
    await this.eval(`
      (function() {
        const event = new KeyboardEvent('${eventType}', ${eventInit});
        window.dispatchEvent(event);
      })()
    `);
    
    return {};
  }

  async enableConsole(callback) {
    this.consoleCallback = callback;
    return {};
  }

  async activateAudio() {
    brightLog('🩸 Activating audio...');
    return this.eval(`
      (async function() {
        const canvas = document.querySelector('canvas');
        if (canvas) {
          canvas.click();
          return 'Audio activated';
        }
        document.body.click();
        return 'Audio activated via body';
      })()
    `);
  }

  async reload() {
    return sendCommand('reload');
  }

  async screenshot() {
    return sendCommand('screenshot');
  }

  close() {
    this.connected = false;
    darkLog('🩸 Closed');
  }

  on(event, handler) {
    if (event === 'Runtime.consoleAPICalled') {
      this.consoleCallback = handler;
    }
  }

  off(event) {
    if (event === 'Runtime.consoleAPICalled') {
      this.consoleCallback = null;
    }
  }

  // Static methods - panel control not needed in Electron
  static async openPanelStandalone() {
    brightLog('🩸 Electron webview ready');
    return true;
  }

  static async closePanelStandalone() {
    darkLog('🩸 Electron webview stays open');
    return true;
  }
}

/**
 * Check if Electron bridge is available
 */
export function isElectronBridgeAvailable() {
  return fs.existsSync(BRIDGE_DIR) || process.env.AC_ELECTRON === 'true';
}

export default ArteryElectron;

// CLI test
if (import.meta.url === `file://${process.argv[1]}`) {
  const cmd = process.argv[2];
  
  console.log(`${CYAN}🩸 Artery Electron Bridge${RESET}\n`);
  
  if (cmd === 'test') {
    (async () => {
      try {
        const artery = new ArteryElectron();
        await artery.connect();
        
        const state = await artery.getState();
        console.log(`${GREEN}State:${RESET}`, state);
        
        artery.close();
        console.log(`\n${GREEN}✓ Test complete${RESET}`);
      } catch (e) {
        console.error(`${YELLOW}Error:${RESET}`, e.message);
        process.exit(1);
      }
    })();
  } else {
    console.log('Usage: node artery-electron.mjs test');
  }
}
