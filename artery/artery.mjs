#!/usr/bin/env node
import WebSocket from 'ws';
import http from 'http';

// Colors
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const RED_BG = '\x1b[41m';
const PINK = '\x1b[95m';
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';

const redLog = (msg) => console.log(`${RED_BG}${PINK}${msg}${RESET}`);
const brightLog = (msg) => console.log(`${RED_BG}${PINK}${BOLD}${msg}${RESET}`);
const darkLog = (msg) => console.log(`${RED_BG}${PINK}${msg}${RESET}`);
const consoleLog = (msg) => console.log(`${PURPLE_BG}${WHITE}💉${RESET} ${msg}`);

// Auto-detect CDP host based on environment
function getCDPHost() {
  // Not in a container - use localhost
  if (process.env.REMOTE_CONTAINERS !== 'true' && process.env.CODESPACES !== 'true') {
    return 'localhost';
  }
  
  // In a container - need to reach the host
  // On Mac/Windows Docker Desktop, host.docker.internal works
  // On Linux, we need HOST_IP env var or to detect the gateway
  
  // First check if HOST_IP is set (common in devcontainer setups)
  if (process.env.HOST_IP) {
    return process.env.HOST_IP;
  }
  
  // Try host.docker.internal (works on Docker Desktop for Mac/Windows)
  // This will be validated at connection time
  return 'host.docker.internal';
}

// Try multiple hosts to find CDP - returns first working one
// Returns { host, port } object
async function findWorkingCDPHost() {
  const candidates = [];
  
  // Not in a container - only try localhost with common CDP ports
  if (process.env.REMOTE_CONTAINERS !== 'true' && process.env.CODESPACES !== 'true') {
    // Try multiple ports in case 9222 is taken
    for (const port of [9333, 9222]) {
      try {
        const works = await new Promise((resolve) => {
          const req = http.get({
            hostname: 'localhost',
            port: port,
            path: '/json',
            timeout: 1000,
          }, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => resolve(data.length > 0));
          });
          req.on('error', () => resolve(false));
          req.on('timeout', () => { req.destroy(); resolve(false); });
        });
        if (works) return { host: 'localhost', port };
      } catch (e) {}
    }
    return { host: 'localhost', port: 9333 };
  }
  
  // In a container - try multiple host:port combinations
  // Order matters: try most reliable options first
  candidates.push({ host: 'host.docker.internal', port: 9333 }); // Windows with new port
  candidates.push({ host: 'host.docker.internal', port: 9222 }); // Mac/Windows Docker Desktop
  candidates.push({ host: 'localhost', port: 9333 }); // SSH tunnel with new port
  candidates.push({ host: 'localhost', port: 9222 }); // SSH tunnel or VS Code forward
  candidates.push({ host: '172.17.0.1', port: 9224 }); // Docker bridge + socat (Linux)
  candidates.push({ host: '172.17.0.1', port: 9223 }); // Docker bridge + socat alt
  candidates.push({ host: '172.17.0.1', port: 9222 }); // Docker bridge direct
  if (process.env.HOST_IP) {
    candidates.push({ host: process.env.HOST_IP, port: 9333 });
    candidates.push({ host: process.env.HOST_IP, port: 9223 });
    candidates.push({ host: process.env.HOST_IP, port: 9222 });
  }
  
  for (const { host, port } of candidates) {
    try {
      const works = await new Promise((resolve) => {
        const req = http.get({
          hostname: host,
          port: port,
          path: '/json',
          timeout: 1000,
          headers: { 'Host': 'localhost' }
        }, (res) => {
          let data = '';
          res.on('data', (chunk) => data += chunk);
          res.on('end', () => resolve(data.length > 0));
        });
        req.on('error', () => resolve(false));
        req.on('timeout', () => { req.destroy(); resolve(false); });
      });
      if (works) {
        return { host, port };
      }
    } catch (e) {
      // Continue to next candidate
    }
  }
  
  // Return the first candidate as fallback (will fail with better error)
  return candidates[0] || { host: 'localhost', port: 9222 };
}

const CDP_HOST = getCDPHost();

class Artery {
  constructor() {
    this.ws = null;
    this.messageId = 0;
    this.pendingRequests = new Map();
    this.eventHandlers = new Map();
    this.debuggerUrl = null;
    this.reconnectInterval = null;
    this.shouldReconnect = false;
    this.consoleCallback = null; // Store console callback for reconnects
  }

  async findAestheticTarget() {
    // Try to find a working CDP host dynamically
    const { host: cdpHost, port: cdpPort } = await findWorkingCDPHost();
    // Store for getAllTargets and other methods
    this.cdpHost = cdpHost;
    this.cdpPort = cdpPort;
    
    const targets = await this.getAllTargets();
    
    // Look for AC iframe - either localhost or aesthetic.computer
    const acTarget = targets.find(t => 
      t.type === 'iframe' && 
      (t.url?.includes('localhost:8888') || t.url?.includes('aesthetic.computer'))
    );
    
    if (!acTarget) throw new Error('AC not found');
    
    redLog('🩸 Found AC');
    // Fix WebSocket URL based on environment - only replace if localhost is in URL
    this.debuggerUrl = acTarget.webSocketDebuggerUrl;
    this.cdpHost = cdpHost; // Store for later use
    this.cdpPort = cdpPort;
    if (this.debuggerUrl.includes('localhost')) {
      // Handle both localhost:PORT and localhost (without port)
      this.debuggerUrl = this.debuggerUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
    }
    return acTarget;
  }
  
  // Get all CDP targets (uses dynamic host/port from findAestheticTarget)
  async getAllTargets() {
    // Use stored CDP host/port or detect dynamically
    const cdpHost = this.cdpHost || (await findWorkingCDPHost()).host;
    const cdpPort = this.cdpPort || (await findWorkingCDPHost()).port;
    
    return new Promise((resolve, reject) => {
      http.get({
        hostname: cdpHost,
        port: cdpPort,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', (chunk) => data += chunk);
        res.on('end', () => {
          try { resolve(JSON.parse(data)); }
          catch (e) { reject(new Error('Parse failed')); }
        });
      }).on('error', reject);
    });
  }
  
  // List all AC frames (main + split players)
  async listACFrames() {
    const targets = await this.getAllTargets();
    const acFrames = targets.filter(t => 
      t.type === 'iframe' && 
      (t.url?.includes('localhost:8888') || t.url?.includes('aesthetic.computer'))
    );
    
    return acFrames.map(t => {
      const url = new URL(t.url);
      const player = url.searchParams.get('player');
      const piece = url.pathname.replace('/', '') || 'prompt';
      return {
        id: t.id,
        url: t.url,
        piece,
        player: player ? parseInt(player) : 0, // 0 = main, 1 = top, 2 = bottom
        label: player ? `Player ${player} (${piece})` : `Main (${piece})`,
        webSocketDebuggerUrl: t.webSocketDebuggerUrl
      };
    });
  }
  
  // Connect to a specific player frame (1 = top, 2 = bottom, 0 = main)
  async connectToPlayer(playerNum) {
    const frames = await this.listACFrames();
    const target = frames.find(f => f.player === playerNum);
    
    if (!target) {
      throw new Error(`Player ${playerNum} not found. Available: ${frames.map(f => f.label).join(', ')}`);
    }
    
    brightLog(`🩸 Connecting to ${target.label}`);
    this.debuggerUrl = target.webSocketDebuggerUrl;
    if (CDP_HOST !== 'localhost') {
      this.debuggerUrl = this.debuggerUrl
        .replace('localhost', `${CDP_HOST}:9222`)
        .replace(':9222:9222', ':9222');
    }
    this.currentPlayer = playerNum;
    return this.connect();
  }

  async connect(enableReconnect = false) {
    this.shouldReconnect = enableReconnect;
    if (!this.debuggerUrl) await this.findAestheticTarget();

    return new Promise((resolve, reject) => {
      redLog('🩸 Connecting...');
      this.ws = new WebSocket(this.debuggerUrl);
      
      this.ws.on('open', () => {
        brightLog('🩸 Connected!');
        if (this.reconnectInterval) {
          clearInterval(this.reconnectInterval);
          this.reconnectInterval = null;
        }
        resolve();
      });
      
      this.ws.on('message', (data) => this.handleMessage(JSON.parse(data.toString())));
      this.ws.on('error', (err) => {
        if (!this.shouldReconnect) redLog(`💔 ${err.message}`);
        reject(err);
      });
      
      this.ws.on('close', () => {
        if (this.shouldReconnect) {
          darkLog('🩸 Reconnecting...');
          this.attemptReconnect();
        } else {
          darkLog('🩸 Closed');
        }
      });
    });
  }

  async attemptReconnect() {
    if (!this.shouldReconnect || this.reconnectInterval) return;
    darkLog('🩸 Retry every 2s...');
    this.reconnectInterval = setInterval(async () => {
      try {
        await this.findAestheticTarget();
        await this.connect(true);
        await this.enableConsole();
        brightLog('🩸 Reconnected!');
      } catch (e) {}
    }, 2000);
  }

  handleMessage(msg) {
    if (msg.id !== undefined) {
      const pending = this.pendingRequests.get(msg.id);
      if (pending) {
        msg.error ? pending.reject(new Error(msg.error.message)) : pending.resolve(msg.result);
        this.pendingRequests.delete(msg.id);
      }
    }
    if (msg.method) {
      (this.eventHandlers.get(msg.method) || []).forEach(h => h(msg.params));
    }
  }

  async send(method, params = {}) {
    return new Promise((resolve, reject) => {
      const id = ++this.messageId;
      this.pendingRequests.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error('Timeout'));
        }
      }, 30000);
    });
  }

  on(eventMethod, handler) {
    if (!this.eventHandlers.has(eventMethod)) this.eventHandlers.set(eventMethod, []);
    this.eventHandlers.get(eventMethod).push(handler);
  }

  // Clear handlers for an event method
  off(eventMethod) {
    this.eventHandlers.delete(eventMethod);
  }

  // Enable console log tracking from the browser
  async enableConsole(callback) {
    // Store callback for use on reconnects
    if (callback !== undefined) this.consoleCallback = callback;
    const cb = this.consoleCallback;
    
    await this.send('Runtime.enable');
    // Clear any existing console handlers to avoid duplicates
    this.off('Runtime.consoleAPICalled');
    this.on('Runtime.consoleAPICalled', (params) => {
      const { type, args } = params;
      const message = args.map(arg => {
        if (arg.type === 'string') return arg.value;
        if (arg.type === 'number') return arg.value;
        if (arg.type === 'boolean') return arg.value;
        if (arg.type === 'undefined') return 'undefined';
        if (arg.type === 'object' && arg.preview) {
          return JSON.stringify(arg.preview.properties?.reduce((obj, p) => {
            obj[p.name] = p.value;
            return obj;
          }, {}) || arg.preview);
        }
        return arg.description || arg.value || `[${arg.type}]`;
      }).join(' ');
      
      if (cb) {
        cb(type, message);
      } else {
        // Default: print to console with prefix
        const prefix = type === 'error' ? '❌' : type === 'warn' ? '⚠️' : '📝';
        consoleLog(`${prefix} ${message}`);
      }
    });
  }

  async eval(expr) {
    const result = await this.send('Runtime.evaluate', {
      expression: expr,
      returnByValue: true,
      awaitPromise: true
    });
    if (result.exceptionDetails) throw new Error(`JS error: ${result.exceptionDetails.text}`);
    return result.result.value;
  }

  async jump(piece) {
    brightLog(`🩸 Jump: ${piece}`);
    return await this.eval(`window.location.href = 'https://localhost:8888/${piece}'`);
  }

  async getCurrentPiece() {
    const href = await this.eval('window.location.href');
    const match = href.match(/localhost:8888\/(.+)/);
    return match ? match[1] : null;
  }

  async openPanel() {
    brightLog('🩸 Opening AC panel...');
    // Use stored CDP host/port from findAestheticTarget, or find dynamically
    const cdpHost = this.cdpHost || (await findWorkingCDPHost()).host;
    const cdpPort = this.cdpPort || (await findWorkingCDPHost()).port;
    
    // We need to execute the VS Code command in the main workbench context
    // First, get all targets
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: cdpHost,
        port: cdpPort,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => resolve(JSON.parse(data)));
      });
      req.on('error', reject);
      req.end();
    });
    
    // Find the main workbench target (type: 'page', url contains 'workbench.html')
    const workbenchTarget = targetsJson.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );
    
    if (!workbenchTarget) {
      darkLog('💔 Could not find VS Code workbench target');
      return;
    }
    
    // Connect to workbench and execute command - URL might already have correct host
    let wsUrl = workbenchTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      // Handle both localhost:PORT and localhost (without port)
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
    }
    const ws = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      ws.on('open', () => resolve());
      ws.on('error', reject);
    });
    
    // Execute the command
    const cmdId = Math.floor(Math.random() * 1000000);
    ws.send(JSON.stringify({
      id: cmdId,
      method: 'Runtime.evaluate',
      params: {
        expression: `vscode.commands.executeCommand('aestheticComputer.openWindow')`,
        awaitPromise: true
      }
    }));
    
    await new Promise(resolve => setTimeout(resolve, 1500)); // Wait for panel to open
    ws.close();
    
    brightLog('🩸 AC panel opened');
  }

  async activateAudio() {
    brightLog('🩸 Activating audio context...');
    // Click the canvas to activate audio
    await this.click(100, 100);
    await new Promise(r => setTimeout(r, 300));
  }

  async type(text) {
    brightLog(`🩸 Typing: ${text}`);
    for (const char of text) {
      await this.send('Input.dispatchKeyEvent', {
        type: 'keyDown',
        text: char
      });
      await this.send('Input.dispatchKeyEvent', {
        type: 'keyUp',
        text: char
      });
    }
  }

  async pressKey(key) {
    brightLog(`🩸 Pressing: ${key}`);
    const keyMap = {
      'escape': 'Escape',
      'esc': 'Escape',
      'enter': 'Enter',
      'tab': 'Tab',
      'backspace': 'Backspace',
      'delete': 'Delete',
      'arrowup': 'ArrowUp',
      'arrowdown': 'ArrowDown',
      'arrowleft': 'ArrowLeft',
      'arrowright': 'ArrowRight',
      'space': ' '
    };
    
    const keyCode = keyMap[key.toLowerCase()] || key;
    
    await this.send('Input.dispatchKeyEvent', {
      type: 'keyDown',
      key: keyCode
    });
    await this.send('Input.dispatchKeyEvent', {
      type: 'keyUp',
      key: keyCode
    });
  }

  async click(x, y) {
    brightLog(`🩸 Clicking at: ${x}, ${y}`);
    await this.send('Input.dispatchMouseEvent', {
      type: 'mousePressed',
      x,
      y,
      button: 'left',
      clickCount: 1
    });
    await this.send('Input.dispatchMouseEvent', {
      type: 'mouseReleased',
      x,
      y,
      button: 'left',
      clickCount: 1
    });
  }

  async moveMouse(x, y) {
    await this.send('Input.dispatchMouseEvent', {
      type: 'mouseMoved',
      x,
      y
    });
  }

  close() {
    this.shouldReconnect = false;
    if (this.reconnectInterval) {
      clearInterval(this.reconnectInterval);
      this.reconnectInterval = null;
    }
    if (this.ws) this.ws.close();
  }
  
  // Static method to open panel without needing AC connection
  static async openPanelStandalone() {
    brightLog('🩸 Opening AC panel...');
    // Find working CDP host dynamically
    const { host: cdpHost, port: cdpPort } = await findWorkingCDPHost();
    
    // Get all targets
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: cdpHost,
        port: cdpPort,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => resolve(JSON.parse(data)));
      });
      req.on('error', reject);
      req.end();
    });
    
    // Find the main workbench target
    const workbenchTarget = targetsJson.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );
    
    if (!workbenchTarget) {
      throw new Error('Could not find VS Code workbench target');
    }
    
    // Connect to workbench - URL might already have correct host from socat
    let wsUrl = workbenchTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      // Handle both localhost:PORT and localhost (without port)
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
    }
    const ws = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      ws.on('open', () => resolve());
      ws.on('error', reject);
      setTimeout(() => reject(new Error('Timeout connecting to workbench')), 5000);
    });
    
    // Wait for response to get element coordinates
    let resolveResponse;
    const responsePromise = new Promise(resolve => {
      resolveResponse = resolve;
    });
    
    ws.on('message', (data) => {
      const msg = JSON.parse(data);
      if (msg.id === 1000) {
        resolveResponse(msg);
      }
    });
    
    // Check panel state and toggle if needed using Enter key (which works!)
    ws.send(JSON.stringify({
      id: 1000,
      method: 'Runtime.evaluate',
      params: {
        expression: `
          (function() {
            const header = document.querySelector('.pane-header[aria-label*="Aesthetic Computer"]');
            if (!header) return { found: false };
            
            const isExpanded = header.getAttribute('aria-expanded') === 'true';
            
            if (!isExpanded) {
              // Focus and press Enter to expand
              header.focus();
              const enterEvent = new KeyboardEvent('keydown', {
                key: 'Enter',
                code: 'Enter',
                keyCode: 13,
                which: 13,
                bubbles: true,
                cancelable: true
              });
              header.dispatchEvent(enterEvent);
              
              const keyupEvent = new KeyboardEvent('keyup', {
                key: 'Enter',
                code: 'Enter',
                keyCode: 13,
                which: 13,
                bubbles: true,
                cancelable: true
              });
              header.dispatchEvent(keyupEvent);
              
              return { found: true, toggled: true };
            } else {
              return { found: true, isExpanded: true };
            }
          })()
        `,
        returnByValue: true
      }
    }));
    
    const response = await responsePromise;
    
    if (response.result && response.result.result && response.result.result.value && response.result.result.value.found) {
      const { isExpanded, toggled } = response.result.result.value;
      
      if (toggled) {
        await new Promise(resolve => setTimeout(resolve, 300)); // Wait for panel to open
        brightLog('🩸 AC panel opened');
      } else if (isExpanded) {
        darkLog(`🩸 Panel already open`);
      }
    } else {
      darkLog('💔 Could not find Aesthetic Computer panel');
    }
    
    ws.close();
  }
  
  // Static method to close the panel
  static async closePanelStandalone() {
    brightLog('🩸 Closing AC panel...');
    const { host: cdpHost, port: cdpPort } = await findWorkingCDPHost();
    
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: cdpHost,
        port: cdpPort,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => resolve(JSON.parse(data)));
      });
      req.on('error', reject);
      req.end();
    });
    
    const workbenchTarget = targetsJson.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );
    
    if (!workbenchTarget) {
      throw new Error('Could not find VS Code workbench target');
    }
    
    let wsUrl = workbenchTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      // Handle both localhost:PORT and localhost (without port)
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
    }
    const ws = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      ws.on('open', () => resolve());
      ws.on('error', reject);
      setTimeout(() => reject(new Error('Timeout')), 5000);
    });
    
    let resolveResponse;
    const responsePromise = new Promise(resolve => {
      resolveResponse = resolve;
    });
    
    ws.on('message', (data) => {
      const msg = JSON.parse(data);
      if (msg.id === 1000) {
        resolveResponse(msg);
      }
    });
    
    // Find panel and toggle if expanded using Enter key
    ws.send(JSON.stringify({
      id: 1000,
      method: 'Runtime.evaluate',
      params: {
        expression: `
          (function() {
            const header = document.querySelector('.pane-header[aria-label*="Aesthetic Computer"]');
            if (!header) return { found: false };
            
            const isExpanded = header.getAttribute('aria-expanded') === 'true';
            if (isExpanded) {
              // Focus and press Enter to collapse
              header.focus();
              const enterEvent = new KeyboardEvent('keydown', {
                key: 'Enter',
                code: 'Enter',
                keyCode: 13,
                which: 13,
                bubbles: true,
                cancelable: true
              });
              header.dispatchEvent(enterEvent);
              header.dispatchEvent(new KeyboardEvent('keyup', {
                key: 'Enter', code: 'Enter', keyCode: 13, which: 13, bubbles: true, cancelable: true
              }));
              return { found: true, closed: true };
            } else {
              return { found: true, alreadyClosed: true };
            }
          })()
        `,
        returnByValue: true
      }
    }));
    
    const response = await responsePromise;
    
    if (response.result?.result?.value?.found) {
      const { closed, alreadyClosed } = response.result.result.value;
      
      if (closed) {
        await new Promise(resolve => setTimeout(resolve, 300));
        brightLog('🩸 AC panel closed');
      } else if (alreadyClosed) {
        darkLog('🩸 Panel already closed');
      }
    }
    
    ws.close();
  }
  
  // Static method to open KidLisp.com window in VS Code
  static async openKidLispWindow() {
    brightLog('🌈 Opening KidLisp.com window...');
    const { host: cdpHost, port: cdpPort } = await findWorkingCDPHost();
    
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: cdpHost,
        port: cdpPort,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => resolve(JSON.parse(data)));
      });
      req.on('error', reject);
      req.end();
    });
    
    const workbenchTarget = targetsJson.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );
    
    if (!workbenchTarget) {
      throw new Error('Could not find VS Code workbench target');
    }
    
    let wsUrl = workbenchTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
    }
    const ws = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      ws.on('open', () => resolve());
      ws.on('error', reject);
      setTimeout(() => reject(new Error('Timeout connecting to workbench')), 5000);
    });
    
    // Execute the KidLisp window command
    ws.send(JSON.stringify({
      id: 3000,
      method: 'Runtime.evaluate',
      params: {
        expression: `vscode.commands.executeCommand('aestheticComputer.openKidLispWindow')`,
        awaitPromise: true
      }
    }));
    
    await new Promise(resolve => setTimeout(resolve, 1500)); // Wait for window to open
    ws.close();
    
    brightLog('🌈 KidLisp.com window opened');
  }
  
  // Toggle local development mode (localhost:8888 vs aesthetic.computer)
  static async toggleLocalDevelopment() {
    brightLog('🩸 Toggling local development mode...');
    const { host: cdpHost, port: cdpPort } = await findWorkingCDPHost();
    
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: cdpHost,
        port: cdpPort,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => resolve(JSON.parse(data)));
      });
      req.on('error', reject);
      req.end();
    });
    
    const workbenchTarget = targetsJson.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );
    
    if (!workbenchTarget) {
      throw new Error('Could not find VS Code workbench target');
    }
    
    let wsUrl = workbenchTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      // Handle both localhost:PORT and localhost (without port)
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
    }
    const ws = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      ws.on('open', () => resolve());
      ws.on('error', reject);
      setTimeout(() => reject(new Error('Timeout')), 5000);
    });
    
    // Execute the toggle command via VS Code's command palette
    ws.send(JSON.stringify({
      id: 2000,
      method: 'Runtime.evaluate',
      params: {
        expression: `
          (async function() {
            // Execute VS Code command to toggle local development
            await vscode.commands.executeCommand('aestheticComputer.localServer');
            return { success: true };
          })()
        `,
        awaitPromise: true,
        returnByValue: true
      }
    }));
    
    await new Promise(resolve => setTimeout(resolve, 1000));
    brightLog('🩸 Local development mode toggled');
    
    ws.close();
  }
  
  // ═══════════════════════════════════════════════════════════════════════════
  // 🧠 Emacs Integration - Execute elisp via emacsclient
  // ═══════════════════════════════════════════════════════════════════════════
  
  /**
   * Execute Emacs Lisp code via emacsclient
   * @param {string} code - The elisp code to execute
   * @returns {Promise<string>} - The result from Emacs
   */
  static async execEmacs(code) {
    const { spawn } = await import('child_process');
    const emacsclient = process.env.EMACSCLIENT || '/usr/sbin/emacsclient';
    
    return new Promise((resolve, reject) => {
      const proc = spawn(emacsclient, ['--eval', code]);
      let stdout = '';
      let stderr = '';
      
      proc.stdout.on('data', (data) => { stdout += data.toString(); });
      proc.stderr.on('data', (data) => { stderr += data.toString(); });
      
      proc.on('close', (exitCode) => {
        if (exitCode === 0) {
          resolve(stdout.trim());
        } else {
          reject(new Error(stderr.trim() || `emacsclient exited with code ${exitCode}`));
        }
      });
      
      proc.on('error', (err) => {
        reject(new Error(`Failed to start emacsclient: ${err.message}`));
      });
    });
  }
  
  /**
   * Get list of Emacs buffers
   * @returns {Promise<string[]>} - Array of buffer names
   */
  static async emacsListBuffers() {
    const result = await Artery.execEmacs(`
      (mapcar #'buffer-name (buffer-list))
    `);
    // Parse the lisp list format: (#<buffer name> ...)
    // The output is like: ("buffer1" "buffer2" ...)
    const match = result.match(/\("([^"]+)"(?:\s+"([^"]+)")*\)/);
    if (match) {
      return result.slice(1, -1).split('" "').map(s => s.replace(/^"|"$/g, ''));
    }
    return result.split('\n').filter(Boolean);
  }
  
  /**
   * Switch to a buffer in Emacs
   * @param {string} bufferName - Name of the buffer to switch to
   */
  static async emacsSwitchBuffer(bufferName) {
    return Artery.execEmacs(`(switch-to-buffer "${bufferName}")`);
  }
  
  /**
   * Open a file in Emacs
   * @param {string} filePath - Path to the file to open
   */
  static async emacsOpenFile(filePath) {
    return Artery.execEmacs(`(find-file "${filePath}")`);
  }
  
  /**
   * Get current buffer content (first N characters)
   * @param {string} bufferName - Buffer name
   * @param {number} maxChars - Maximum characters to return (default 1000)
   */
  static async emacsGetBufferContent(bufferName, maxChars = 1000) {
    return Artery.execEmacs(`
      (with-current-buffer "${bufferName}"
        (buffer-substring-no-properties 
          (point-min) 
          (min (point-max) (+ (point-min) ${maxChars}))))
    `);
  }
  
  /**
   * Insert text at cursor position in Emacs
   * @param {string} text - Text to insert
   */
  static async emacsInsert(text) {
    // Escape quotes and backslashes in the text
    const escaped = text.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    return Artery.execEmacs(`(insert "${escaped}")`);
  }
  
  /**
   * Get Emacs version and check if server is running
   */
  static async emacsVersion() {
    return Artery.execEmacs('(emacs-version)');
  }
}

async function main() {
  const command = process.argv[2];
  const args = process.argv.slice(3);
  
  // Panel command doesn't need AC connection
  if (command === 'panel') {
    try {
      await Artery.openPanelStandalone();
      process.exit(0);
    } catch (error) {
      redLog(`💔 ${error.message}`);
      process.exit(1);
    }
  }
  
  // Emacs commands - don't need AC connection
  if (command === 'emacs') {
    try {
      const elisp = args.join(' ');
      if (!elisp) {
        // Show Emacs version if no args
        const version = await Artery.emacsVersion();
        brightLog('🧠 Emacs connected');
        console.log(version);
      } else {
        // Execute the elisp
        brightLog(`🧠 Executing: ${elisp}`);
        const result = await Artery.execEmacs(elisp);
        console.log(result);
      }
      process.exit(0);
    } catch (error) {
      redLog(`💔 ${error.message}`);
      process.exit(1);
    }
  }
  
  if (command === 'emacs-buffers') {
    try {
      const result = await Artery.execEmacs('(mapcar #\'buffer-name (buffer-list))');
      brightLog('🧠 Emacs Buffers:');
      // Parse and display buffers
      const buffers = result.slice(1, -1).split(' ').map(s => s.replace(/^"|"$/g, ''));
      buffers.forEach(b => console.log(`  ${b}`));
      process.exit(0);
    } catch (error) {
      redLog(`💔 ${error.message}`);
      process.exit(1);
    }
  }
  
  // Toggle local development mode
  if (command === 'toggle-local') {
    try {
      await Artery.toggleLocalDevelopment();
      process.exit(0);
    } catch (error) {
      redLog(`💔 ${error.message}`);
      process.exit(1);
    }
  }
  
  // Performance monitoring command
  if (command === 'perf') {
    const client = new Artery();
    try {
      await client.connect(true);
      await client.enableConsole();
      
      const duration = parseInt(args[0]) || 5; // Default 5 seconds
      brightLog(`🩸 WEBGPU PERFORMANCE MONITOR (${duration}s)`);
      console.log('═'.repeat(50));
      
      const samples = [];
      const startTime = Date.now();
      
      // Sample performance every 100ms
      const interval = setInterval(async () => {
        try {
          const stats = await client.eval(`
            (function() {
              // Try to get WebGPU stats from the renderer
              const webgpu = window.WebGPU;
              if (webgpu && typeof webgpu.getPerfStats === 'function') {
                return webgpu.getPerfStats();
              }
              return null;
            })()
          `);
          
          if (stats) {
            samples.push(stats);
            const bar = '█'.repeat(Math.min(50, Math.floor(stats.fps / 2)));
            const msBar = stats.frameDelta < 16.67 ? '🟢' : stats.frameDelta < 33.33 ? '🟡' : '🔴';
            console.log(`${msBar} FPS: ${String(stats.fps).padStart(3)} | MS: ${stats.frameDelta.toFixed(1).padStart(5)} | DC: ${String(stats.drawCalls).padStart(4)} | ${bar}`);
          }
        } catch (e) {
          // Ignore eval errors during sampling
        }
      }, 100);
      
      // Stop after duration
      setTimeout(() => {
        clearInterval(interval);
        
        console.log('═'.repeat(50));
        
        if (samples.length > 0) {
          const avgFps = samples.reduce((a, s) => a + s.fps, 0) / samples.length;
          const avgMs = samples.reduce((a, s) => a + s.frameDelta, 0) / samples.length;
          const minFps = Math.min(...samples.map(s => s.fps));
          const maxFps = Math.max(...samples.map(s => s.fps));
          const avgDc = samples.reduce((a, s) => a + s.drawCalls, 0) / samples.length;
          
          brightLog('📊 SUMMARY');
          console.log(`   Samples: ${samples.length}`);
          console.log(`   Avg FPS: ${avgFps.toFixed(1)}`);
          console.log(`   Min FPS: ${minFps}`);
          console.log(`   Max FPS: ${maxFps}`);
          console.log(`   Avg MS:  ${avgMs.toFixed(2)}`);
          console.log(`   Avg DC:  ${avgDc.toFixed(0)}`);
        } else {
          darkLog('💔 No WebGPU stats available. Make sure:');
          console.log('   1. WebGPU mode is enabled (api.webgpu.enabled = true)');
          console.log('   2. Perf overlay is on (api.webgpu.perf(true))');
        }
        
        console.log('═'.repeat(50));
        client.close();
        process.exit(0);
      }, duration * 1000);
      
      return; // Keep process alive
    } catch (error) {
      redLog(`💔 ${error.message}`);
      process.exit(1);
    }
  }
  
  const client = new Artery();
  
  try {
    const isRepl = command === 'repl';
    // Handle frames command before connecting (needs raw target listing)
    if (command === 'frames') {
      const frames = await client.listACFrames();
      if (frames.length === 0) {
        redLog('💔 No AC frames found');
        process.exit(1);
      }
      brightLog('🩸 AC Frames:');
      for (const frame of frames) {
        const prefix = frame.player === 0 ? '  📺' : `  🎮`;
        console.log(`${prefix} ${frame.label}`);
        console.log(`     ${PINK}${frame.url}${RESET}`);
      }
      process.exit(0);
    }
    
    // Handle player-specific connection
    if (command === 'player') {
      const playerNum = parseInt(args[0]);
      if (isNaN(playerNum) || playerNum < 0 || playerNum > 2) {
        redLog('💔 Usage: artery player <0|1|2>');
        console.log('   0 = Main frame');
        console.log('   1 = Player 1 (top split)');
        console.log('   2 = Player 2 (bottom split)');
        process.exit(1);
      }
      await client.connectToPlayer(playerNum);
      await client.enableConsole();
      brightLog(`🩸 Connected to Player ${playerNum}, streaming console...`);
      // Keep alive for console streaming
      return;
    }
    
    await client.connect(isRepl);
    if (isRepl) await client.enableConsole();
    
    switch (command) {
      case 'jump':
        if (!args[0]) {
          redLog('💔 Missing piece');
          process.exit(1);
        }
        await client.jump(args[0]);
        brightLog(`🩸 Jumped!`);
        break;
        
      case 'current':
        const current = await client.getCurrentPiece();
        brightLog(`🩸 Current: ${current || '(none)'}`);
        break;
        
      case 'eval':
        const expr = args.join(' ');
        if (!expr) {
          redLog('💔 Missing expression');
          process.exit(1);
        }
        const result = await client.eval(expr);
        brightLog('🩸 Result:');
        console.log(result);
        break;
        
      case 'type':
        const text = args.join(' ');
        if (!text) {
          redLog('💔 Missing text');
          process.exit(1);
        }
        await client.type(text);
        brightLog('🩸 Typed!');
        break;
        
      case 'key':
        if (!args[0]) {
          redLog('💔 Missing key');
          process.exit(1);
        }
        await client.pressKey(args[0]);
        brightLog('🩸 Pressed!');
        break;
        
      case 'click':
        const x = parseInt(args[0]);
        const y = parseInt(args[1]);
        if (isNaN(x) || isNaN(y)) {
          redLog('💔 Usage: artery click <x> <y>');
          process.exit(1);
        }
        await client.click(x, y);
        brightLog('🩸 Clicked!');
        break;
        
      case 'audio':
        await client.activateAudio();
        brightLog('🩸 Audio activated!');
        break;
        
      case 'hiphop':
        // Close the client we just opened (hiphop manages its own)
        client.close();
        // Dynamically import and run the hiphop test with remaining args
        const { main: runHiphop } = await import('./test-hiphop.mjs');
        // Override process.argv for the test to parse
        process.argv = ['node', 'test-hiphop.mjs', ...args];
        await runHiphop();
        return;
      
      case 'trapwaltz':
        // Close the client we just opened (trapwaltz manages its own)
        client.close();
        const { main: runTrapwaltz } = await import('./test-trapwaltz.mjs');
        process.argv = ['node', 'test-trapwaltz.mjs', ...args];
        await runTrapwaltz();
        return;
      
      case '1v1':
      case 'split':
        // Close the client we just opened (1v1 manages its own)
        client.close();
        const { main: run1v1 } = await import('./test-1v1-split.mjs');
        process.argv = ['node', 'test-1v1-split.mjs', ...args];
        await run1v1();
        return;
        
      case 'repl':
        // Clear screen and show header
        console.clear();
        console.log('═'.repeat(70));
        brightLog('🩸 ARTERY REPL - Live connection to Aesthetic Computer');
        console.log('═'.repeat(70));
        console.log('');
        console.log('  Commands:');
        console.log(`    ${PINK}.jump <piece>${RESET}  - Navigate to piece`);
        console.log(`    ${PINK}.current${RESET}       - Show current piece`);
        console.log(`    ${PINK}.type <text>${RESET}   - Type text into AC`);
        console.log(`    ${PINK}.key <key>${RESET}     - Press key (escape, enter, tab, etc)`);
        console.log(`    ${PINK}.click <x> <y>${RESET} - Click at coordinates`);
        console.log(`    ${PINK}.audio${RESET}         - Activate audio context`);
        console.log(`    ${PINK}.panel${RESET}         - Open/focus AC panel`);
        console.log(`    ${PINK}.exit${RESET}          - Close connection`);
        console.log('');
        console.log('  Type any JavaScript to execute in AC context');
        console.log('');
        console.log('─'.repeat(70));
        console.log(`${PURPLE_BG}${WHITE} AC CONSOLE OUTPUT ${RESET}`);
        console.log('─'.repeat(70));
        console.log('');
        
        const readline = await import('readline');
        const rl = readline.createInterface({
          input: process.stdin,
          output: process.stdout,
          prompt: `\n${RED_BG}${PINK}🩸 ${RESET} `
        });
        
        rl.prompt();
        
        rl.on('line', async (line) => {
          const input = line.trim();
          
          if (input === '.exit') {
            console.log('');
            console.log('─'.repeat(70));
            darkLog('🩸 Connection closed');
            console.log('═'.repeat(70));
            rl.close();
            client.close();
            process.exit(0);
          } else if (input.startsWith('.jump ')) {
            const p = input.substring(6).trim();
            console.log('');
            console.log('─'.repeat(70));
            await client.jump(p);
            brightLog(`🩸 Navigated to: ${p}`);
            console.log('─'.repeat(70));
          } else if (input === '.current') {
            const c = await client.getCurrentPiece();
            console.log('');
            console.log('─'.repeat(70));
            brightLog(`🩸 Current piece: ${c || '(none)'}`);
            console.log('─'.repeat(70));
          } else if (input.startsWith('.type ')) {
            const text = input.substring(6);
            console.log('');
            console.log('─'.repeat(70));
            await client.type(text);
            console.log('─'.repeat(70));
          } else if (input.startsWith('.key ')) {
            const key = input.substring(5).trim();
            console.log('');
            console.log('─'.repeat(70));
            await client.pressKey(key);
            console.log('─'.repeat(70));
          } else if (input.startsWith('.click ')) {
            const parts = input.substring(7).split(' ');
            const x = parseInt(parts[0]);
            const y = parseInt(parts[1]);
            if (isNaN(x) || isNaN(y)) {
              redLog('💔 Usage: .click <x> <y>');
            } else {
              console.log('');
              console.log('─'.repeat(70));
              await client.click(x, y);
              console.log('─'.repeat(70));
            }
          } else if (input === '.audio') {
            console.log('');
            console.log('─'.repeat(70));
            await client.activateAudio();
            console.log('─'.repeat(70));
          } else if (input === '.panel') {
            console.log('');
            console.log('─'.repeat(70));
            await client.openPanel();
            console.log('─'.repeat(70));
          } else if (input) {
            try {
              console.log('');
              console.log('─'.repeat(70));
              console.log(`${PINK}↓ Result${RESET}`);
              const r = await client.eval(input);
              console.log(r);
              console.log('─'.repeat(70));
            } catch (e) {
              console.log('');
              console.log('─'.repeat(70));
              redLog(`💔 Error: ${e.message}`);
              console.log('─'.repeat(70));
            }
          }
          
          rl.prompt();
        });
        
        rl.on('close', () => {
          client.close();
          process.exit(0);
        });
        break;
        
      default:
        brightLog('🩸 ARTERY');
        console.log('');
        console.log('artery jump <piece>    - Navigate to piece');
        console.log('artery current         - Show current piece');
        console.log('artery eval <expr>     - Evaluate JavaScript in AC');
        console.log('artery type <text>     - Type text into AC');
        console.log('artery key <key>       - Press key');
        console.log('artery click <x> <y>   - Click at coordinates');
        console.log('artery audio           - Activate audio context');
        console.log('artery panel           - Open AC sidebar panel');
        console.log('artery perf [seconds]  - Monitor WebGPU performance');
        console.log('artery repl            - Interactive REPL mode');
        console.log('');
        console.log('🧠 Emacs:');
        console.log('artery emacs           - Show Emacs version');
        console.log('artery emacs <elisp>   - Execute elisp in Emacs');
        console.log('artery emacs-buffers   - List Emacs buffers');
        console.log('');
        console.log('🎮 Split/Multiplayer:');
        console.log('artery frames          - List all AC frames (main + players)');
        console.log('artery player <0|1|2>  - Connect to specific player frame');
        console.log('                         0=main, 1=player1 (top), 2=player2 (bottom)');
        console.log('');
        console.log('🎵 Tests:');
        console.log('artery hiphop [opts]   - Hip-hop beat generator test');
        console.log('artery trapwaltz [opts] - Trap waltz generator (3/4 + trap)');
        console.log('artery 1v1 [p1] [p2]   - Test split view for 1v1 dueling');
    }
    
    setTimeout(() => {
      client.close();
      process.exit(0);
    }, 1000);
    
  } catch (error) {
    redLog(`💔 ${error.message}`);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export default Artery;
