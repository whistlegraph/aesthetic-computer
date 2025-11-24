#!/usr/bin/env node
/**
 * 🦎 Gecko - WebSocket Debugger Bridge
 * 
 * A real-time communication channel between LLMs and the Aesthetic Computer
 * workbench running in VS Code. Enables AI to see, inspect, and control
 * the live runtime through Chrome DevTools Protocol over WebSocket.
 */

import WebSocket from 'ws';

class Gecko {
  constructor() {
    this.ws = null;
    this.messageId = 0;
    this.pendingRequests = new Map();
    this.eventHandlers = new Map();
    this.debuggerUrl = null;
  }

  /**
   * Discover the Aesthetic Computer webview target
   */
  async findAestheticTarget() {
    const response = await fetch('http://host.docker.internal:9222/json', {
      headers: { 'Host': 'localhost' }
    });
    
    const targets = await response.json();
    
    // Find the iframe with localhost:8888 (Aesthetic Computer)
    const acTarget = targets.find(t => 
      t.type === 'iframe' && 
      t.url && 
      t.url.includes('localhost:8888')
    );
    
    if (!acTarget) {
      throw new Error('Aesthetic Computer target not found. Make sure AC is running in VS Code.');
    }
    
    console.log(`📍 Found Aesthetic Computer: ${acTarget.title}`);
    console.log(`   URL: ${acTarget.url}`);
    console.log(`   ID: ${acTarget.id}`);
    
    this.debuggerUrl = acTarget.webSocketDebuggerUrl.replace('localhost', 'host.docker.internal');
    return acTarget;
  }

  /**
   * Connect to the WebSocket debugger
   */
  async connect() {
    if (!this.debuggerUrl) {
      await this.findAestheticTarget();
    }

    return new Promise((resolve, reject) => {
      console.log(`🔌 Connecting to: ${this.debuggerUrl}`);
      
      this.ws = new WebSocket(this.debuggerUrl);
      
      this.ws.on('open', () => {
        console.log('✅ WebSocket connected!');
        resolve();
      });
      
      this.ws.on('message', (data) => {
        const message = JSON.parse(data.toString());
        this.handleMessage(message);
      });
      
      this.ws.on('error', (error) => {
        console.error('❌ WebSocket error:', error);
        reject(error);
      });
      
      this.ws.on('close', () => {
        console.log('🔌 WebSocket closed');
      });
    });
  }

  /**
   * Handle incoming messages
   */
  handleMessage(message) {
    // Response to a request
    if (message.id !== undefined) {
      const pending = this.pendingRequests.get(message.id);
      if (pending) {
        if (message.error) {
          pending.reject(new Error(message.error.message));
        } else {
          pending.resolve(message.result);
        }
        this.pendingRequests.delete(message.id);
      }
    }
    
    // Event notification
    if (message.method) {
      const handlers = this.eventHandlers.get(message.method) || [];
      handlers.forEach(handler => handler(message.params));
    }
  }

  /**
   * Send a CDP command
   */
  async send(method, params = {}) {
    return new Promise((resolve, reject) => {
      const id = ++this.messageId;
      
      this.pendingRequests.set(id, { resolve, reject });
      
      const message = { id, method, params };
      this.ws.send(JSON.stringify(message));
      
      // Timeout after 30 seconds
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error('Request timeout'));
        }
      }, 30000);
    });
  }

  /**
   * Subscribe to CDP events
   */
  on(eventMethod, handler) {
    if (!this.eventHandlers.has(eventMethod)) {
      this.eventHandlers.set(eventMethod, []);
    }
    this.eventHandlers.get(eventMethod).push(handler);
  }

  /**
   * Evaluate JavaScript in the Aesthetic Computer context
   */
  async eval(expression) {
    const result = await this.send('Runtime.evaluate', {
      expression,
      returnByValue: true,
      awaitPromise: true
    });
    
    if (result.exceptionDetails) {
      throw new Error(`JavaScript error: ${result.exceptionDetails.text}`);
    }
    
    return result.result.value;
  }

  /**
   * Navigate to a different Aesthetic Computer piece
   */
  async jump(piece) {
    console.log(`🎯 Jumping to: ${piece}`);
    return await this.eval(`window.location.href = 'https://localhost:8888/${piece}'`);
  }

  /**
   * Get current location
   */
  async getCurrentPiece() {
    const href = await this.eval('window.location.href');
    const match = href.match(/localhost:8888\/(.+)/);
    return match ? match[1] : null;
  }

  /**
   * Get console logs
   */
  async enableConsole() {
    await this.send('Runtime.enable');
    await this.send('Console.enable');
    
    this.on('Console.messageAdded', (params) => {
      const msg = params.message;
      console.log(`[AC Console] ${msg.level}: ${msg.text}`);
    });
  }

  /**
   * Execute a function in the Aesthetic Computer context
   */
  async callFunction(functionDeclaration, ...args) {
    const argsJson = JSON.stringify(args);
    return await this.eval(`(${functionDeclaration})(...${argsJson})`);
  }

  /**
   * Close the connection
   */
  close() {
    if (this.ws) {
      this.ws.close();
    }
  }
}

// CLI Interface
async function main() {
  const client = new Gecko();
  
  try {
    await client.connect();
    
    // Enable console logging
    await client.enableConsole();
    
    const args = process.argv.slice(2);
    const command = args[0];
    
    switch (command) {
      case 'jump':
        const piece = args[1];
        if (!piece) {
          console.error('Usage: ac-cdp-client.mjs jump <piece>');
          process.exit(1);
        }
        await client.jump(piece);
        console.log(`✅ Jumped to: ${piece}`);
        break;
        
      case 'current':
        const current = await client.getCurrentPiece();
        console.log(`📍 Current piece: ${current}`);
        break;
        
      case 'eval':
        const expression = args.slice(1).join(' ');
        if (!expression) {
          console.error('Usage: ac-cdp-client.mjs eval <expression>');
          process.exit(1);
        }
        const result = await client.eval(expression);
        console.log('Result:', result);
        break;
        
      case 'repl':
        console.log('🎨 Aesthetic Computer REPL - type JavaScript to execute');
        console.log('   Special commands: .jump <piece>, .current, .exit');
        
        const readline = await import('readline');
        const rl = readline.createInterface({
          input: process.stdin,
          output: process.stdout,
          prompt: 'ac> '
        });
        
        rl.prompt();
        
        rl.on('line', async (line) => {
          const input = line.trim();
          
          if (input === '.exit') {
            rl.close();
            client.close();
            process.exit(0);
          } else if (input.startsWith('.jump ')) {
            const p = input.substring(6).trim();
            await client.jump(p);
            console.log(`✅ Jumped to: ${p}`);
          } else if (input === '.current') {
            const curr = await client.getCurrentPiece();
            console.log(`📍 Current piece: ${curr}`);
          } else if (input) {
            try {
              const res = await client.eval(input);
              console.log(res);
            } catch (e) {
              console.error('Error:', e.message);
            }
          }
          
          rl.prompt();
        });
        
        rl.on('close', () => {
          client.close();
          process.exit(0);
        });
        
        // Keep alive
        return;
        
      default:
        console.log('🦎 Gecko - Aesthetic Computer Debugger');
        console.log('   A nimble climber that sees into VS Code');
        console.log('');
        console.log('Usage:');
        console.log('  gecko jump <piece>     - Navigate to a piece');
        console.log('  gecko current          - Show current piece');
        console.log('  gecko eval <expr>      - Evaluate JavaScript');
        console.log('  gecko repl             - Interactive REPL');
        console.log('');
        console.log('Examples:');
        console.log('  gecko jump prompt');
        console.log('  gecko eval "document.title"');
        console.log('  gecko repl');
    }
    
    // Close after command completes (except for REPL)
    setTimeout(() => {
      client.close();
      process.exit(0);
    }, 1000);
    
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export default Gecko;
