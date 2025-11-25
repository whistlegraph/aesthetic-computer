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

class Artery {
  constructor() {
    this.ws = null;
    this.messageId = 0;
    this.pendingRequests = new Map();
    this.eventHandlers = new Map();
    this.debuggerUrl = null;
    this.reconnectInterval = null;
    this.shouldReconnect = false;
  }

  async findAestheticTarget() {
    const targets = await new Promise((resolve, reject) => {
      http.get({
        hostname: 'host.docker.internal',
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
    this.debuggerUrl = acTarget.webSocketDebuggerUrl
      .replace('localhost', 'host.docker.internal:9222')
      .replace(':9222:9222', ':9222');
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
        hostname: 'host.docker.internal',
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
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', 'host.docker.internal:9222');
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

  async enableConsole() {
    await this.send('Runtime.enable');
    await this.send('Console.enable');
    this.on('Console.messageAdded', (params) => {
      const timestamp = new Date().toLocaleTimeString('en-US', { hour12: false });
      consoleLog(`[${timestamp}] ${params.message.text}`);
    });
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
        hostname: 'host.docker.internal',
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
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', 'host.docker.internal:9222');
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
    
    // Check panel state and click if needed
    ws.send(JSON.stringify({
      id: 1000,
      method: 'Runtime.evaluate',
      params: {
        expression: `
          (function() {
            const selectors = [
              '.pane-header[aria-label*="Aesthetic Computer"]',
              '[aria-label*="Aesthetic Computer"]',
              '[role="button"][aria-label*="Aesthetic"]'
            ];
            
            for (const selector of selectors) {
              const el = document.querySelector(selector);
              if (el) {
                const rect = el.getBoundingClientRect();
                // Check if panel is expanded by looking at aria-expanded
                const isExpanded = el.getAttribute('aria-expanded') === 'true';
                return {
                  found: true,
                  isExpanded: isExpanded,
                  selector: selector,
                  x: Math.floor(rect.left + rect.width / 2),
                  y: Math.floor(rect.top + rect.height / 2)
                };
              }
            }
            
            return { found: false };
          })()
        `,
        returnByValue: true
      }
    }));
    
    const response = await responsePromise;
    
    if (response.result && response.result.result && response.result.result.value && response.result.result.value.found) {
      const { x, y, selector, isExpanded } = response.result.result.value;
      
      if (isExpanded) {
        darkLog(`🩸 Panel already open`);
      } else {
        darkLog(`🩸 Found panel at (${x}, ${y}), opening...`);
        
        // Click to open
        ws.send(JSON.stringify({
          id: 1001,
          method: 'Input.dispatchMouseEvent',
          params: {
            type: 'mousePressed',
            x, y,
            button: 'left',
            clickCount: 1
          }
        }));
        
        await new Promise(resolve => setTimeout(resolve, 50));
        
        ws.send(JSON.stringify({
          id: 1002,
          method: 'Input.dispatchMouseEvent',
          params: {
            type: 'mouseReleased',
            x, y,
            button: 'left',
            clickCount: 1
          }
        }));
        
        await new Promise(resolve => setTimeout(resolve, 1000)); // Wait for panel to open
        brightLog('🩸 AC sidebar panel opened');
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
        hostname: 'host.docker.internal',
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
    
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', 'host.docker.internal:9222');
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
    
    // Find panel and click if expanded
    ws.send(JSON.stringify({
      id: 1000,
      method: 'Runtime.evaluate',
      params: {
        expression: `
          (function() {
            const el = document.querySelector('.pane-header[aria-label*="Aesthetic Computer"]');
            if (el) {
              const isExpanded = el.getAttribute('aria-expanded') === 'true';
              const rect = el.getBoundingClientRect();
              return {
                found: true,
                isExpanded: isExpanded,
                x: Math.floor(rect.left + rect.width / 2),
                y: Math.floor(rect.top + rect.height / 2)
              };
            }
            return { found: false };
          })()
        `,
        returnByValue: true
      }
    }));
    
    const response = await responsePromise;
    
    if (response.result?.result?.value?.found) {
      const { x, y, isExpanded } = response.result.result.value;
      
      if (isExpanded) {
        // Click to close
        ws.send(JSON.stringify({
          id: 1001,
          method: 'Input.dispatchMouseEvent',
          params: { type: 'mousePressed', x, y, button: 'left', clickCount: 1 }
        }));
        
        await new Promise(resolve => setTimeout(resolve, 50));
        
        ws.send(JSON.stringify({
          id: 1002,
          method: 'Input.dispatchMouseEvent',
          params: { type: 'mouseReleased', x, y, button: 'left', clickCount: 1 }
        }));
        
        await new Promise(resolve => setTimeout(resolve, 500));
        brightLog('🩸 AC panel closed');
      } else {
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
        hostname: 'host.docker.internal',
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
    
    const wsUrl = workbenchTarget.webSocketDebuggerUrl.replace('localhost', 'host.docker.internal:9222');
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
        console.log('artery jump <piece>');
        console.log('artery current');
        console.log('artery eval <expr>');
        console.log('artery type <text>');
        console.log('artery key <key>');
        console.log('artery click <x> <y>');
        console.log('artery audio');
        console.log('artery panel');
        console.log('artery repl');
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
