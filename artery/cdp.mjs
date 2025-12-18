// cdp.mjs - Robust CDP connection manager for VS Code integration
// Handles discovery, caching, and reconnection of Chrome DevTools Protocol targets

import WebSocket from 'ws';
import fetch from 'node-fetch';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const CACHE_FILE = path.join(__dirname, '.cdp-cache.json');
const CDP_PORT = 9333; // VS Code forwarded port

export class CDP {
  constructor(options = {}) {
    this.port = options.port || CDP_PORT;
    this.targetUrl = options.targetUrl || 'https://localhost:8888';
    this.ws = null;
    this.msgId = 1;
    this.verbose = options.verbose || false;
  }

  log(msg) {
    if (this.verbose) console.log(`[CDP] ${msg}`);
  }

  // Find all available CDP targets
  async listTargets() {
    try {
      const response = await fetch(`http://localhost:${this.port}/json`);
      if (!response.ok) {
        throw new Error(`CDP port ${this.port} not accessible`);
      }
      return await response.json();
    } catch (err) {
      throw new Error(`Failed to list CDP targets: ${err.message}`);
    }
  }

  // Find aesthetic.computer pages
  async findAestheticPages() {
    const targets = await this.listTargets();
    return targets.filter(t => 
      t.url && (
        t.url.includes('localhost:8888') ||
        t.url.includes('aesthetic.computer')
      )
    );
  }

  // Find a specific page by URL pattern
  async findPage(urlPattern) {
    const targets = await this.listTargets();
    
    if (typeof urlPattern === 'string') {
      return targets.find(t => t.url === urlPattern) ||
             targets.find(t => t.url && t.url.includes(urlPattern));
    }
    
    if (urlPattern instanceof RegExp) {
      return targets.find(t => t.url && urlPattern.test(t.url));
    }
    
    throw new Error('urlPattern must be string or RegExp');
  }

  // Cache target info for faster reconnection
  async cacheTarget(target) {
    const cache = {
      id: target.id,
      url: target.url,
      webSocketDebuggerUrl: target.webSocketDebuggerUrl,
      timestamp: Date.now()
    };
    await fs.writeFile(CACHE_FILE, JSON.stringify(cache, null, 2));
    this.log(`Cached target: ${target.url}`);
  }

  // Load cached target and verify it's still valid
  async loadCachedTarget() {
    try {
      const data = await fs.readFile(CACHE_FILE, 'utf8');
      const cache = JSON.parse(data);
      
      // Check if cache is recent (< 5 minutes old)
      if (Date.now() - cache.timestamp > 5 * 60 * 1000) {
        this.log('Cache expired');
        return null;
      }
      
      // Verify target still exists
      const targets = await this.listTargets();
      const target = targets.find(t => t.id === cache.id);
      
      if (target) {
        this.log(`Using cached target: ${target.url}`);
        return target;
      }
      
      this.log('Cached target no longer exists');
      return null;
    } catch (err) {
      return null;
    }
  }

  // Connect to a CDP target
  async connect(target = null) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.log('Already connected');
      return;
    }

    // Try cache first if no specific target provided
    if (!target) {
      target = await this.loadCachedTarget();
    }

    // If no cached target, find one
    if (!target) {
      target = await this.findPage(this.targetUrl);
      
      if (!target) {
        // List all aesthetic pages to help user
        const pages = await this.findAestheticPages();
        if (pages.length > 0) {
          throw new Error(
            `Page "${this.targetUrl}" not found. Available pages:\n` +
            pages.map(p => `  - ${p.url}`).join('\n')
          );
        }
        throw new Error(
          `No aesthetic.computer pages found. ` +
          `Make sure the app is open in VS Code's Simple Browser.`
        );
      }
      
      // Cache the discovered target
      await this.cacheTarget(target);
    }

    this.log(`Connecting to: ${target.url}`);
    this.ws = new WebSocket(target.webSocketDebuggerUrl);

    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Connection timeout'));
      }, 10000);

      this.ws.on('open', () => {
        clearTimeout(timeout);
        this.log('Connected');
        resolve();
      });

      this.ws.on('error', (err) => {
        clearTimeout(timeout);
        reject(err);
      });
    });
  }

  // Send CDP command
  async send(method, params = {}) {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      throw new Error('Not connected. Call connect() first.');
    }

    return new Promise((resolve, reject) => {
      const id = this.msgId++;
      const timeout = setTimeout(() => {
        reject(new Error(`Timeout waiting for ${method}`));
      }, 30000);

      const handler = (data) => {
        try {
          const msg = JSON.parse(data);
          if (msg.id === id) {
            clearTimeout(timeout);
            this.ws.off('message', handler);
            
            if (msg.error) {
              reject(new Error(`CDP error: ${msg.error.message}`));
            } else {
              resolve(msg.result);
            }
          }
        } catch (err) {
          // Ignore parse errors from unrelated messages
        }
      };

      this.ws.on('message', handler);
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  // Evaluate JavaScript in the page
  async eval(expression, options = {}) {
    const result = await this.send('Runtime.evaluate', {
      expression,
      returnByValue: options.returnByValue !== false,
      awaitPromise: options.awaitPromise || false,
      ...options
    });

    if (result.exceptionDetails) {
      throw new Error(
        `Evaluation failed: ${result.result?.description || 'Unknown error'}`
      );
    }

    return result.result?.value;
  }

  // Close connection
  close() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
      this.log('Disconnected');
    }
  }

  // Get current page info
  async getPageInfo() {
    const result = await this.eval(`({
      url: location.href,
      title: document.title,
      readyState: document.readyState,
      hasACBios: typeof window.bios !== 'undefined'
    })`);
    return result;
  }

  // Wait for page to be ready
  async waitForReady(timeout = 10000) {
    const start = Date.now();
    while (Date.now() - start < timeout) {
      try {
        const info = await this.getPageInfo();
        if (info.readyState === 'complete' && info.hasACBios) {
          this.log('Page ready');
          return info;
        }
      } catch (err) {
        // Ignore errors while waiting
      }
      await new Promise(r => setTimeout(r, 100));
    }
    throw new Error('Timeout waiting for page to be ready');
  }

  // Check if AC panel is open in VS Code
  async isPanelOpen() {
    const pages = await this.findAestheticPages();
    return pages.length > 0;
  }

  // Open the AC panel in VS Code if not already open
  async ensurePanelOpen() {
    const isOpen = await this.isPanelOpen();
    
    if (isOpen) {
      this.log('Panel already open');
      return true;
    }

    this.log('Opening AC panel...');
    
    // Find VS Code workbench target
    const targets = await this.listTargets();
    const workbench = targets.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );

    if (!workbench) {
      throw new Error('Could not find VS Code workbench');
    }

    // Connect to workbench
    const ws = new WebSocket(workbench.webSocketDebuggerUrl);
    await new Promise((resolve, reject) => {
      ws.on('open', resolve);
      ws.on('error', reject);
      setTimeout(() => reject(new Error('Timeout')), 5000);
    });

    // Toggle panel using Enter key
    const toggleCode = `
      (function() {
        const headers = Array.from(document.querySelectorAll('.pane-header'));
        const header = headers.find(h => h.textContent?.includes('Aesthetic Computer'));
        if (header) {
          const button = header.querySelector('.codicon-chevron-right, .codicon-chevron-down');
          if (button) {
            button.click();
            return 'toggled';
          }
        }
        return 'not-found';
      })()
    `;

    const result = await new Promise((resolve) => {
      let resolved = false;
      ws.on('message', (data) => {
        const msg = JSON.parse(data);
        if (msg.id === 9999 && !resolved) {
          resolved = true;
          resolve(msg.result?.result?.value);
        }
      });
      ws.send(JSON.stringify({
        id: 9999,
        method: 'Runtime.evaluate',
        params: { expression: toggleCode, returnByValue: true }
      }));
      setTimeout(() => {
        if (!resolved) {
          resolved = true;
          resolve('timeout');
        }
      }, 3000);
    });

    ws.close();
    
    // Wait for panel to open
    await new Promise(r => setTimeout(r, 500));
    
    const nowOpen = await this.isPanelOpen();
    if (nowOpen) {
      this.log('Panel opened successfully');
      return true;
    }

    this.log('Panel toggle result: ' + result);
    return false;
  }
}

// Helper: Create and auto-connect CDP instance with panel check
export async function createCDP(options = {}) {
  const cdp = new CDP(options);
  
  // Check if panel is open, if not try to open it
  if (options.ensurePanel !== false) {
    try {
      await cdp.ensurePanelOpen();
    } catch (err) {
      if (cdp.verbose) {
        console.log(`[CDP] Could not ensure panel: ${err.message}`);
      }
    }
  }
  
  await cdp.connect();
  return cdp;
}

// Helper: Execute a script with auto-cleanup
export async function withCDP(fn, options = {}) {
  const cdp = await createCDP(options);
  try {
    return await fn(cdp);
  } finally {
    cdp.close();
  }
}

export default CDP;
