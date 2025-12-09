#!/usr/bin/env node
/**
 * 🎹 DAW Debug - CDP bridge for M4L jweb~ debugging
 * 
 * Connects to the Chrome DevTools Protocol exposed by Max's jweb~ 
 * and forwards console logs to artery-tui or stdout.
 * 
 * Usage:
 *   node daw-debug.mjs           # Connect and show console logs
 *   node daw-debug.mjs --json    # Output as JSON (for piping)
 *   node daw-debug.mjs --eval "code"  # Evaluate JS in jweb~
 */

import WebSocket from 'ws';
import http from 'http';

const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const FG_RED = '\x1b[31m';
const FG_GREEN = '\x1b[32m';
const FG_YELLOW = '\x1b[33m';
const FG_BLUE = '\x1b[34m';
const FG_MAGENTA = '\x1b[35m';
const FG_CYAN = '\x1b[36m';
const FG_GRAY = '\x1b[90m';

// Default port for jweb~ CDP (can be overridden)
const DAW_CDP_PORT = parseInt(process.env.DAW_CDP_PORT || '9229');

// Check if we're in a container
const IN_CONTAINER = process.env.REMOTE_CONTAINERS === 'true' || 
                     process.env.CODESPACES === 'true' ||
                     process.env.container === 'true';

// Try multiple hosts
const CDP_HOSTS = IN_CONTAINER 
  ? ['host.docker.internal', '172.17.0.1', 'localhost']
  : ['localhost', '127.0.0.1'];

class DAWDebugger {
  constructor(options = {}) {
    this.ws = null;
    this.msgId = 1;
    this.pending = new Map();
    this.host = null;
    this.port = DAW_CDP_PORT;
    this.connected = false;
    this.targetInfo = null;
    this.jsonOutput = options.json || false;
    this.onLog = options.onLog || this.defaultLogHandler.bind(this);
    this.onConnect = options.onConnect || (() => {});
    this.onDisconnect = options.onDisconnect || (() => {});
  }

  log(msg) {
    if (!this.jsonOutput) {
      console.log(msg);
    }
  }

  defaultLogHandler(entry) {
    const { type, text, timestamp, source } = entry;
    const time = new Date(timestamp).toLocaleTimeString('en-US', { 
      hour12: false, 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      fractionalSecondDigits: 3 
    });
    
    if (this.jsonOutput) {
      console.log(JSON.stringify(entry));
      return;
    }

    let color = FG_GRAY;
    let prefix = '  ';
    switch (type) {
      case 'error':
        color = FG_RED;
        prefix = '❌';
        break;
      case 'warning':
        color = FG_YELLOW;
        prefix = '⚠️ ';
        break;
      case 'info':
        color = FG_CYAN;
        prefix = 'ℹ️ ';
        break;
      case 'log':
        color = FG_GREEN;
        prefix = '📝';
        break;
      case 'debug':
        color = FG_MAGENTA;
        prefix = '🔍';
        break;
    }

    const sourceTag = source ? `${FG_GRAY}[${source}]${RESET} ` : '';
    console.log(`${DIM}${time}${RESET} ${prefix} ${sourceTag}${color}${text}${RESET}`);
  }

  async findCDPHost() {
    for (const host of CDP_HOSTS) {
      try {
        const targets = await this.fetchTargets(host, this.port);
        if (targets && targets.length > 0) {
          return { host, targets };
        }
      } catch (e) {
        // Try next host
      }
    }
    return null;
  }

  fetchTargets(host, port) {
    return new Promise((resolve, reject) => {
      const req = http.get({
        hostname: host,
        port: port,
        path: '/json',
        timeout: 2000,
        headers: { 'Host': 'localhost' } // Required for CDP
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => {
          try {
            resolve(JSON.parse(data));
          } catch (e) {
            reject(e);
          }
        });
      });
      req.on('error', reject);
      req.on('timeout', () => { req.destroy(); reject(new Error('Timeout')); });
    });
  }

  async connect() {
    this.log(`${FG_CYAN}🎹 DAW Debug - Searching for jweb~ on port ${this.port}...${RESET}`);
    
    const result = await this.findCDPHost();
    if (!result) {
      this.log(`${FG_RED}❌ No jweb~ CDP found on port ${this.port}${RESET}`);
      this.log(`${FG_YELLOW}   Make sure Ableton is running with an AC M4L device loaded${RESET}`);
      return false;
    }

    const { host, targets } = result;
    this.host = host;
    
    // Find the page target (should be metronome or other AC piece)
    const pageTarget = targets.find(t => t.type === 'page');
    if (!pageTarget) {
      this.log(`${FG_RED}❌ No page target found in CDP${RESET}`);
      return false;
    }

    this.targetInfo = pageTarget;
    this.log(`${FG_GREEN}✓ Found: ${pageTarget.title}${RESET}`);
    this.log(`${FG_GRAY}  URL: ${pageTarget.url}${RESET}`);
    
    // Build WebSocket URL
    let wsUrl = pageTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost') && host !== 'localhost') {
      // Fix host in WebSocket URL for container access
      wsUrl = wsUrl.replace(/ws:\/\/localhost/, `ws://${host}:${this.port}`);
    }
    
    this.log(`${FG_GRAY}  Connecting to: ${wsUrl}${RESET}`);
    
    return new Promise((resolve, reject) => {
      this.ws = new WebSocket(wsUrl);
      
      this.ws.on('open', async () => {
        this.connected = true;
        this.log(`${FG_GREEN}✓ Connected to jweb~ debugger${RESET}\n`);
        
        // Enable console and runtime
        await this.send('Runtime.enable');
        await this.send('Console.enable');
        await this.send('Log.enable');
        
        this.onConnect(this.targetInfo);
        resolve(true);
      });
      
      this.ws.on('message', (data) => {
        const msg = JSON.parse(data.toString());
        
        // Handle pending responses
        if (msg.id && this.pending.has(msg.id)) {
          this.pending.get(msg.id)(msg);
          this.pending.delete(msg.id);
          return;
        }
        
        // Handle events
        this.handleEvent(msg);
      });
      
      this.ws.on('close', () => {
        this.connected = false;
        this.log(`${FG_YELLOW}⚠️  Disconnected from jweb~${RESET}`);
        this.onDisconnect();
      });
      
      this.ws.on('error', (err) => {
        this.log(`${FG_RED}❌ WebSocket error: ${err.message}${RESET}`);
        reject(err);
      });
      
      setTimeout(() => reject(new Error('Connection timeout')), 5000);
    });
  }

  handleEvent(msg) {
    if (!msg.method) return;
    
    switch (msg.method) {
      case 'Console.messageAdded': {
        const m = msg.params.message;
        this.onLog({
          type: m.level,
          text: m.text,
          timestamp: Date.now(),
          source: m.source,
          url: m.url,
          line: m.line
        });
        break;
      }
      
      case 'Runtime.consoleAPICalled': {
        const { type, args, timestamp } = msg.params;
        const text = args.map(a => {
          if (a.type === 'string') return a.value;
          if (a.type === 'number') return a.value;
          if (a.type === 'boolean') return a.value;
          if (a.type === 'undefined') return 'undefined';
          if (a.type === 'object' && a.preview) {
            return JSON.stringify(a.preview.properties?.reduce((acc, p) => {
              acc[p.name] = p.value;
              return acc;
            }, {}) || a.description);
          }
          return a.description || a.value || `[${a.type}]`;
        }).join(' ');
        
        this.onLog({
          type,
          text,
          timestamp: timestamp / 1000, // Convert from microseconds
          source: 'console'
        });
        break;
      }
      
      case 'Runtime.exceptionThrown': {
        const { exceptionDetails } = msg.params;
        this.onLog({
          type: 'error',
          text: exceptionDetails.text + (exceptionDetails.exception?.description || ''),
          timestamp: Date.now(),
          source: 'exception',
          url: exceptionDetails.url,
          line: exceptionDetails.lineNumber
        });
        break;
      }
      
      case 'Log.entryAdded': {
        const { entry } = msg.params;
        this.onLog({
          type: entry.level,
          text: entry.text,
          timestamp: entry.timestamp,
          source: entry.source,
          url: entry.url
        });
        break;
      }
    }
  }

  send(method, params = {}) {
    return new Promise((resolve, reject) => {
      if (!this.ws || !this.connected) {
        reject(new Error('Not connected'));
        return;
      }
      
      const id = this.msgId++;
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reject(new Error('Timeout'));
      }, 10000);
      
      this.pending.set(id, (msg) => {
        clearTimeout(timer);
        if (msg.error) {
          reject(new Error(msg.error.message));
        } else {
          resolve(msg.result);
        }
      });
      
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  async evaluate(expression) {
    const result = await this.send('Runtime.evaluate', {
      expression,
      returnByValue: true,
      awaitPromise: true
    });
    return result.result?.value;
  }

  async getDAWState() {
    return this.evaluate(`
      (function() {
        if (typeof $commonApi !== 'undefined' && $commonApi.sound) {
          const daw = $commonApi.sound.daw;
          return daw ? {
            bpm: daw.bpm,
            playing: daw.playing,
            time: daw.time,
            sampleRate: daw.sampleRate
          } : null;
        }
        return null;
      })()
    `);
  }

  disconnect() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.connected = false;
  }
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  const jsonOutput = args.includes('--json');
  const evalCode = args.includes('--eval') ? args[args.indexOf('--eval') + 1] : null;
  const stateCheck = args.includes('--state');
  
  const debugger_ = new DAWDebugger({ json: jsonOutput });
  
  try {
    await debugger_.connect();
    
    if (evalCode) {
      // One-shot eval
      const result = await debugger_.evaluate(evalCode);
      console.log(jsonOutput ? JSON.stringify(result) : result);
      debugger_.disconnect();
      process.exit(0);
    } else if (stateCheck) {
      // One-shot state check
      const state = await debugger_.getDAWState();
      console.log(jsonOutput ? JSON.stringify(state) : state);
      debugger_.disconnect();
      process.exit(0);
    } else {
      // Stream console logs
      console.log(`${FG_CYAN}📡 Streaming console logs from jweb~... (Ctrl+C to stop)${RESET}\n`);
      
      // Keep process alive
      process.on('SIGINT', () => {
        debugger_.disconnect();
        process.exit(0);
      });
    }
  } catch (err) {
    console.error(`${FG_RED}Error: ${err.message}${RESET}`);
    process.exit(1);
  }
}

export default DAWDebugger;
