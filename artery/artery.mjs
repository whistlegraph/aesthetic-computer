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
  // Check if we're in a dev container (Docker)
  if (process.env.REMOTE_CONTAINERS === 'true' || process.env.CODESPACES === 'true') {
    return 'host.docker.internal';
  }
  // Otherwise use localhost (native Windows/Mac/Linux)
  return 'localhost';
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
    const targets = await new Promise((resolve, reject) => {
      http.get({
        hostname: CDP_HOST,
        port: 9222,
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
    
    // Look for AC iframe - either localhost or aesthetic.computer
    const acTarget = targets.find(t => 
      t.type === 'iframe' && 
      (t.url?.includes('localhost:8888') || t.url?.includes('aesthetic.computer'))
    );
    
    if (!acTarget) throw new Error('AC not found');
    
    redLog('🩸 Found AC');
    // Fix WebSocket URL based on environment
    this.debuggerUrl = acTarget.webSocketDebuggerUrl;
    if (CDP_HOST !== 'localhost') {
      this.debuggerUrl = this.debuggerUrl
        .replace('localhost', `${CDP_HOST}:9222`)
        .replace(':9222:9222', ':9222');
    }
    return acTarget;
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
    // We need to execute the VS Code command in the main workbench context
    // First, get all targets
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: CDP_HOST,
        port: 9222,
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
    
    // Connect to workbench and execute command
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', `${CDP_HOST}:9222`);
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
    // Get all targets
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: CDP_HOST,
        port: 9222,
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
    
    // Connect to workbench
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', `${CDP_HOST}:9222`);
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
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: CDP_HOST,
        port: 9222,
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
    
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', `${CDP_HOST}:9222`);
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
  
  // Toggle local development mode (localhost:8888 vs aesthetic.computer)
  static async toggleLocalDevelopment() {
    brightLog('🩸 Toggling local development mode...');
    
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: CDP_HOST,
        port: 9222,
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
    
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', `${CDP_HOST}:9222`);
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
