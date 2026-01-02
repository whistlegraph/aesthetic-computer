// 🌐 External Chrome Manager
// Opens and controls Chrome on the host machine via SSH + CDP
// Supports extensions (Temple wallet, etc.) which VS Code Simple Browser can't load

import http from 'http';
import WebSocket from 'ws';
import { exec } from 'child_process';
import { promisify } from 'util';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const execAsync = promisify(exec);
const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Chrome profile with extensions pre-installed
const CHROME_PROFILE_NAME = 'AestheticDev';

export class ExternalChrome {
  constructor(options = {}) {
    this.sshHost = options.sshHost || 'jas@host.docker.internal';
    this.cdpPort = options.cdpPort || 9222;
    this.verbose = options.verbose || false;
    this.ws = null;
    this.msgId = 1;
  }

  log(...args) {
    if (this.verbose) console.log('[chrome]', ...args);
  }

  // Execute command on Mac via SSH
  async ssh(cmd) {
    try {
      const { stdout, stderr } = await execAsync(`ssh -o ConnectTimeout=5 ${this.sshHost} "${cmd}"`, {
        timeout: 30000,
      });
      return stdout.trim();
    } catch (e) {
      if (e.stderr?.includes('Permission denied')) {
        // SSH works but command failed
        return '';
      }
      throw e;
    }
  }

  // Check if Chrome is running with debugging
  async isChromeDebugging() {
    try {
      const result = await this.ssh(`curl -s --connect-timeout 2 http://localhost:${this.cdpPort}/json`);
      return result.length > 10;
    } catch {
      return false;
    }
  }

  // Start Chrome with remote debugging and our profile
  async startChrome() {
    this.log('Starting Chrome with debugging...');
    
    // First quit existing Chrome
    await this.ssh(`osascript -e 'quit app "Google Chrome"' 2>/dev/null || true`);
    await new Promise(r => setTimeout(r, 1000));
    
    // Start Chrome with debugging and profile
    const chromeCmd = `/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome --remote-debugging-port=${this.cdpPort} --profile-directory="${CHROME_PROFILE_NAME}" &`;
    await this.ssh(`nohup ${chromeCmd} > /dev/null 2>&1 &`);
    
    // Wait for Chrome to be ready
    for (let i = 0; i < 10; i++) {
      await new Promise(r => setTimeout(r, 500));
      if (await this.isChromeDebugging()) {
        this.log('Chrome ready');
        return true;
      }
    }
    
    throw new Error('Chrome failed to start with debugging');
  }

  // Open URL in Chrome
  async openUrl(url) {
    // Check if Chrome is running with debugging
    if (!await this.isChromeDebugging()) {
      await this.startChrome();
    }
    
    // Open URL
    await this.ssh(`open -a "Google Chrome" "${url}"`);
    this.log('Opened:', url);
  }

  // Get CDP targets via SSH tunnel
  async getTargets() {
    const result = await this.ssh(`curl -s http://localhost:${this.cdpPort}/json`);
    return JSON.parse(result);
  }

  // Find page by URL pattern
  async findPage(urlPattern) {
    const targets = await this.getTargets();
    return targets.find(t => t.type === 'page' && t.url?.includes(urlPattern));
  }

  // Connect to a page via WebSocket
  async connect(urlPattern) {
    const page = await this.findPage(urlPattern);
    if (!page) {
      throw new Error(`Page not found: ${urlPattern}`);
    }
    
    this.log('Connecting to:', page.title || page.url);
    
    // Create SSH tunnel for WebSocket
    // The wsDebuggerUrl is localhost:9222 on Mac, we tunnel through SSH
    const wsUrl = `ws://host.docker.internal:${this.cdpPort}/devtools/page/${page.id}`;
    
    this.ws = new WebSocket(wsUrl, { headers: { 'Host': 'localhost' } });
    
    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('WS timeout')), 10000);
      this.ws.on('open', () => { clearTimeout(timeout); resolve(); });
      this.ws.on('error', err => { clearTimeout(timeout); reject(err); });
    });
    
    this.log('Connected');
    return this.ws;
  }

  // Send CDP command
  async send(method, params = {}) {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      throw new Error('Not connected');
    }
    
    return new Promise((resolve, reject) => {
      const id = this.msgId++;
      const timeout = setTimeout(() => reject(new Error('Timeout: ' + method)), 30000);
      
      const handler = data => {
        try {
          const msg = JSON.parse(data);
          if (msg.id === id) {
            clearTimeout(timeout);
            this.ws.off('message', handler);
            msg.error ? reject(new Error(msg.error.message)) : resolve(msg.result);
          }
        } catch {}
      };
      
      this.ws.on('message', handler);
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  // Evaluate JS in page
  async eval(urlPattern, expression) {
    if (!this.ws) {
      await this.connect(urlPattern);
    }
    
    const result = await this.send('Runtime.evaluate', {
      expression,
      returnByValue: true,
    });
    
    if (result.exceptionDetails) {
      throw new Error(result.exceptionDetails.text || 'Eval error');
    }
    
    return result.result?.value;
  }

  // Reload page
  async reloadPage(urlPattern) {
    if (!this.ws) {
      await this.connect(urlPattern);
    }
    await this.send('Page.reload');
  }

  // Watch console output
  async watchConsole(urlPattern) {
    if (!this.ws) {
      await this.connect(urlPattern);
    }
    
    await this.send('Runtime.enable');
    await this.send('Console.enable');
    await this.send('Log.enable');
    
    console.log('📡 Watching console... (Ctrl+C to stop)\n');
    console.log('─'.repeat(60) + '\n');
    
    this.ws.on('message', data => {
      try {
        const msg = JSON.parse(data);
        
        if (msg.method === 'Runtime.consoleAPICalled') {
          const { type, args } = msg.params;
          const values = args.map(a => {
            if (a.type === 'string') return a.value;
            if (a.type === 'number') return a.value;
            if (a.type === 'boolean') return a.value;
            if (a.type === 'undefined') return 'undefined';
            if (a.type === 'object' && a.preview) {
              return JSON.stringify(a.preview.properties?.reduce((o, p) => {
                o[p.name] = p.value;
                return o;
              }, {}) || a.preview);
            }
            return a.description || a.type;
          });
          
          const icon = { log: '📝', warn: '⚠️', error: '❌', info: 'ℹ️', debug: '🐛' }[type] || '📝';
          console.log(`${icon} [${type}]`, values.join(' '));
        }
        
        if (msg.method === 'Runtime.exceptionThrown') {
          const { exceptionDetails } = msg.params;
          console.log('💥 [exception]', exceptionDetails.text || exceptionDetails.exception?.description);
        }
        
      } catch {}
    });
    
    // Keep alive
    await new Promise(() => {});
  }

  // Disconnect
  disconnect() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }
}

export default ExternalChrome;
