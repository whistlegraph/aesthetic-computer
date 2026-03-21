#!/usr/bin/env node
// 🧪 KidLisp.com Test Suite
// Tests for the kidlisp.com editor via CDP (Chrome DevTools Protocol)
// Run: node artery/test-kidlisp.mjs [test-name]

import WebSocket from 'ws';
import http from 'http';

// Colors for output
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const GREEN = '\x1b[32m';
const RED = '\x1b[31m';
const YELLOW = '\x1b[33m';
const CYAN = '\x1b[36m';
const MAGENTA = '\x1b[35m';
const DIM = '\x1b[2m';

const pass = (msg) => console.log(`${GREEN}✓${RESET} ${msg}`);
const fail = (msg) => console.log(`${RED}✗${RESET} ${msg}`);
const info = (msg) => console.log(`${CYAN}ℹ${RESET} ${msg}`);
const warn = (msg) => console.log(`${YELLOW}⚠${RESET} ${msg}`);
const heading = (msg) => console.log(`\n${MAGENTA}${BOLD}═══ ${msg} ═══${RESET}\n`);
const subheading = (msg) => console.log(`${CYAN}─── ${msg} ───${RESET}`);

// Find working CDP host
async function findWorkingCDPHost() {
  const candidates = [];
  const inContainer = process.env.REMOTE_CONTAINERS === 'true' || process.env.CODESPACES === 'true';
  
  if (!inContainer) {
    for (const port of [9333, 9222]) {
      candidates.push({ host: 'localhost', port });
    }
  } else {
    candidates.push({ host: 'host.docker.internal', port: 9333 });
    candidates.push({ host: 'host.docker.internal', port: 9222 });
    candidates.push({ host: '172.17.0.1', port: 9224 });
    candidates.push({ host: '172.17.0.1', port: 9223 });
    candidates.push({ host: '172.17.0.1', port: 9222 });
    candidates.push({ host: 'localhost', port: 9333 });
    candidates.push({ host: 'localhost', port: 9222 });
  }
  
  for (const { host, port } of candidates) {
    try {
      const works = await new Promise((resolve) => {
        const req = http.get({
          hostname: host, port, path: '/json', timeout: 1000,
          headers: { 'Host': 'localhost' }
        }, (res) => {
          let data = '';
          res.on('data', (chunk) => data += chunk);
          res.on('end', () => resolve(data.length > 0));
        });
        req.on('error', () => resolve(false));
        req.on('timeout', () => { req.destroy(); resolve(false); });
      });
      if (works) return { host, port };
    } catch (e) {}
  }
  return candidates[0] || { host: 'localhost', port: 9333 };
}

// KidLisp Test Harness - connects directly to kidlisp.com iframe via CDP
class KidLispTestHarness {
  constructor() {
    this.ws = null;
    this.workbenchWs = null;
    this.cdpHost = null;
    this.cdpPort = null;
    this.messageId = 1000;
    this.pendingResponses = new Map();
    this.testResults = [];
  }
  
  // Connect to the kidlisp.com iframe directly via CDP
  async connect() {
    const { host, port } = await findWorkingCDPHost();
    this.cdpHost = host;
    this.cdpPort = port;
    
    info(`Connecting to CDP at ${host}:${port}...`);
    
    const targetsJson = await new Promise((resolve, reject) => {
      const req = http.get({
        hostname: host, port, path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => resolve(JSON.parse(data)));
      });
      req.on('error', reject);
    });
    
    // Find the kidlisp.com iframe target
    const kidlispTarget = targetsJson.find(t => 
      t.type === 'iframe' && t.url && t.url.includes('kidlisp.com')
    );
    
    if (!kidlispTarget) {
      // Try to open it via workbench
      warn('KidLisp window not found, attempting to open it...');
      await this.openKidLispViaWorkbench(targetsJson);
      
      // Re-fetch targets
      const newTargets = await new Promise((resolve, reject) => {
        const req = http.get({
          hostname: host, port, path: '/json',
          headers: { 'Host': 'localhost' }
        }, (res) => {
          let data = '';
          res.on('data', chunk => data += chunk);
          res.on('end', () => resolve(JSON.parse(data)));
        });
        req.on('error', reject);
      });
      
      const newKidlispTarget = newTargets.find(t => 
        t.type === 'iframe' && t.url && t.url.includes('kidlisp.com')
      );
      
      if (!newKidlispTarget) {
        throw new Error('Could not find or open KidLisp.com window');
      }
      
      return this.connectToTarget(newKidlispTarget);
    }
    
    return this.connectToTarget(kidlispTarget);
  }
  
  async connectToTarget(target) {
    let wsUrl = target.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${this.cdpHost}:${this.cdpPort}`);
    }
    
    this.ws = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      this.ws.on('open', resolve);
      this.ws.on('error', reject);
      setTimeout(() => reject(new Error('Timeout connecting to KidLisp')), 5000);
    });
    
    this.ws.on('message', (data) => {
      const msg = JSON.parse(data);
      if (msg.id && this.pendingResponses.has(msg.id)) {
        this.pendingResponses.get(msg.id)(msg);
        this.pendingResponses.delete(msg.id);
      }
    });
    
    pass(`Connected to KidLisp.com: ${target.url.substring(0, 60)}...`);
  }
  
  async openKidLispViaWorkbench(targets) {
    const workbenchTarget = targets.find(t => 
      t.type === 'page' && t.url && t.url.includes('workbench.html')
    );
    
    if (!workbenchTarget) {
      throw new Error('Could not find VS Code workbench');
    }
    
    let wsUrl = workbenchTarget.webSocketDebuggerUrl;
    if (wsUrl.includes('localhost')) {
      wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${this.cdpHost}:${this.cdpPort}`);
    }
    
    this.workbenchWs = new WebSocket(wsUrl);
    
    await new Promise((resolve, reject) => {
      this.workbenchWs.on('open', resolve);
      this.workbenchWs.on('error', reject);
      setTimeout(() => reject(new Error('Timeout')), 5000);
    });
    
    // Execute the command to open KidLisp window
    await new Promise((resolve) => {
      const id = 9999;
      this.workbenchWs.on('message', (data) => {
        const msg = JSON.parse(data);
        if (msg.id === id) resolve();
      });
      this.workbenchWs.send(JSON.stringify({
        id,
        method: 'Runtime.evaluate',
        params: {
          expression: `vscode.commands.executeCommand('aestheticComputer.openKidLispWindow')`,
          awaitPromise: true
        }
      }));
    });
    
    // Wait for window to load
    await this.sleep(2000);
    this.workbenchWs.close();
  }
  
  // Evaluate expression in kidlisp.com context
  async evaluate(expression, timeout = 10000) {
    const id = this.messageId++;
    
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pendingResponses.delete(id);
        reject(new Error(`Timeout evaluating expression`));
      }, timeout);
      
      this.pendingResponses.set(id, (msg) => {
        clearTimeout(timer);
        if (msg.result?.exceptionDetails) {
          reject(new Error(msg.result.exceptionDetails.exception?.description || 'Evaluation error'));
        } else {
          resolve(msg.result?.result?.value);
        }
      });
      
      this.ws.send(JSON.stringify({
        id,
        method: 'Runtime.evaluate',
        params: { expression, returnByValue: true, awaitPromise: true }
      }));
    });
  }
  
  // Get Monaco editor instance
  async getEditor() {
    return this.evaluate(`monaco.editor.getEditors()[0]`);
  }
  
  // Set code in the editor
  async setCode(code) {
    const escaped = JSON.stringify(code);
    return this.evaluate(`
      (function() {
        const editor = monaco.editor.getEditors()[0];
        if (!editor) return { success: false, error: 'No editor' };
        editor.setValue(${escaped});
        return { success: true, length: ${escaped}.length };
      })()
    `);
  }
  
  // Get current code from editor
  async getCode() {
    return this.evaluate(`monaco.editor.getEditors()[0]?.getValue() || ''`);
  }
  
  // Click Play button
  async play() {
    return this.evaluate(`
      (function() {
        const btn = document.getElementById('send-button');
        if (!btn) return { success: false, error: 'No play button' };
        if (btn.disabled) return { success: false, error: 'Button disabled' };
        btn.click();
        return { success: true };
      })()
    `);
  }
  
  // Click Stop button
  async stop() {
    return this.evaluate(`
      (function() {
        const btn = document.getElementById('stop-button');
        if (!btn) return { success: false, error: 'No stop button' };
        btn.click();
        return { success: true };
      })()
    `);
  }
  
  // Clear editor
  async clear() {
    return this.evaluate(`
      (function() {
        const editor = monaco.editor.getEditors()[0];
        if (!editor) return { success: false, error: 'No editor' };
        editor.setValue('');
        return { success: true };
      })()
    `);
  }
  
  // Get playback state
  async getPlaybackState() {
    return this.evaluate(`
      (function() {
        const playBtn = document.getElementById('send-button');
        const stopBtn = document.getElementById('stop-button');
        return {
          isPlaying: playBtn?.disabled === true,
          canStop: stopBtn && !stopBtn.disabled,
          pieceReady: typeof pieceReady !== 'undefined' ? pieceReady : null
        };
      })()
    `);
  }
  
  // Get console entries
  async getConsoleEntries() {
    return this.evaluate(`
      (function() {
        const container = document.getElementById('console-output');
        if (!container) return [];
        return Array.from(container.querySelectorAll('.console-entry')).map(el => ({
          text: el.textContent,
          level: el.classList.contains('error') ? 'error' : 
                 el.classList.contains('warn') ? 'warn' : 'log'
        }));
      })()
    `);
  }
  
  // Clear console
  async clearConsole() {
    return this.evaluate(`
      (function() {
        const container = document.getElementById('console-output');
        if (container) container.innerHTML = '';
        return { success: true };
      })()
    `);
  }
  
  // Close all tabs
  async closeAllTabs() {
    return this.evaluate(`
      (function() {
        const tabs = document.querySelectorAll('.tab-close');
        const count = tabs.length;
        tabs.forEach(btn => btn.click());
        return { closedTabs: count };
      })()
    `);
  }
  
  // Get open tabs
  async getTabs() {
    return this.evaluate(`
      (function() {
        const tabs = Array.from(document.querySelectorAll('.tab'));
        return tabs.map(tab => ({
          name: tab.querySelector('.tab-name')?.textContent || 'unnamed',
          active: tab.classList.contains('active')
        }));
      })()
    `);
  }
  
  // Toggle theme
  async toggleTheme() {
    return this.evaluate(`
      (function() {
        const btn = document.getElementById('theme-toggle');
        if (btn) { btn.click(); return { success: true }; }
        return { success: false };
      })()
    `);
  }
  
  // Get current theme
  async getTheme() {
    return this.evaluate(`document.documentElement.getAttribute('data-theme') || 'auto'`);
  }
  
  // Switch language
  async switchLanguage(lang) {
    return this.evaluate(`
      (function() {
        const option = document.querySelector('.lang-option[data-lang="${lang}"]');
        if (option) { option.click(); return { success: true, lang: '${lang}' }; }
        return { success: false, error: 'Language not found' };
      })()
    `);
  }
  
  // Get UI state
  async getUIState() {
    return this.evaluate(`({
      theme: document.documentElement.getAttribute('data-theme') || 'auto',
      lang: document.documentElement.lang || 'en',
      isPlaying: document.getElementById('send-button')?.disabled === true,
      codeLength: monaco.editor.getEditors()[0]?.getValue()?.length || 0
    })`);
  }
  
  // Wait for piece to be ready
  async waitForReady(timeout = 10000) {
    const start = Date.now();
    while (Date.now() - start < timeout) {
      const state = await this.getPlaybackState();
      if (state.pieceReady) return true;
      await this.sleep(200);
    }
    return false;
  }
  
  // Helper: sleep
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
  
  // Close connection
  close() {
    if (this.ws) { this.ws.close(); this.ws = null; }
    if (this.workbenchWs) { this.workbenchWs.close(); this.workbenchWs = null; }
  }
  
  // Run a single test
  async runTest(name, fn) {
    subheading(`Test: ${name}`);
    const start = Date.now();
    try {
      await fn();
      const duration = Date.now() - start;
      pass(`${name} (${duration}ms)`);
      this.testResults.push({ name, passed: true, duration });
    } catch (error) {
      const duration = Date.now() - start;
      fail(`${name}: ${error.message}`);
      this.testResults.push({ name, passed: false, duration, error: error.message });
    }
  }
  
  // Print test summary
  printSummary() {
    heading('Test Summary');
    const passed = this.testResults.filter(t => t.passed).length;
    const failed = this.testResults.filter(t => !t.passed).length;
    const total = this.testResults.length;
    
    console.log(`${GREEN}Passed:${RESET} ${passed}`);
    console.log(`${RED}Failed:${RESET} ${failed}`);
    console.log(`${CYAN}Total:${RESET} ${total}`);
    
    if (failed > 0) {
      console.log(`\n${RED}Failed tests:${RESET}`);
      this.testResults.filter(t => !t.passed).forEach(t => {
        console.log(`  ${RED}✗${RESET} ${t.name}: ${t.error}`);
      });
    }
    
    return failed === 0;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// Test Suites
// ═══════════════════════════════════════════════════════════════════════════

// Basic connectivity and window tests
async function testBasicConnectivity(harness) {
  await harness.runTest('CDP connection established', async () => {
    if (!harness.ws) throw new Error('Not connected');
  });
  
  await harness.runTest('Monaco editor available', async () => {
    const editorCount = await harness.evaluate(`monaco.editor.getEditors().length`);
    if (!editorCount || editorCount < 1) throw new Error('No Monaco editor found');
    info(`Found ${editorCount} Monaco editor instance(s)`);
  });
  
  await harness.runTest('Get UI state', async () => {
    const state = await harness.getUIState();
    info(`UI State: theme=${state.theme}, isPlaying=${state.isPlaying}, code length=${state.codeLength}`);
  });
}

// Editor interaction tests
async function testEditorInteractions(harness) {
  await harness.runTest('Set editor code - simple', async () => {
    const result = await harness.setCode('(ink red)\n(box)');
    if (!result?.success) throw new Error(result?.error || 'Failed to set code');
    await harness.sleep(300);
  });
  
  await harness.runTest('Get editor code', async () => {
    const code = await harness.getCode();
    if (!code.includes('ink red')) throw new Error('Code not found');
    info(`Current code: ${code.substring(0, 50)}...`);
  });
  
  await harness.runTest('Set editor code - complex pattern', async () => {
    const code = `(each x 10
  (each y 10
    (ink (? red blue green yellow))
    (box (* x 20) (* y 20) 15)))`;
    const result = await harness.setCode(code);
    if (!result?.success) throw new Error(result?.error || 'Failed to set code');
    await harness.sleep(300);
  });
  
  await harness.runTest('Clear editor', async () => {
    const result = await harness.clear();
    if (!result?.success) throw new Error(result?.error || 'Failed to clear');
    await harness.sleep(200);
    const code = await harness.getCode();
    if (code.length > 0) throw new Error('Editor not cleared');
  });
}

// Playback control tests
async function testPlaybackControls(harness) {
  await harness.runTest('Play code', async () => {
    await harness.setCode('(ink lime)\n(box 50 50 100)');
    await harness.sleep(200);
    const result = await harness.play();
    if (!result?.success) throw new Error(result?.error || 'Failed to play');
    await harness.sleep(500);
  });
  
  await harness.runTest('Check playback state', async () => {
    const state = await harness.getPlaybackState();
    info(`Playback state: isPlaying=${state.isPlaying}, canStop=${state.canStop}`);
  });
  
  await harness.runTest('Stop playback', async () => {
    const result = await harness.stop();
    if (!result?.success) throw new Error(result?.error || 'Failed to stop');
    await harness.sleep(200);
  });
  
  await harness.runTest('Play animated code', async () => {
    const animCode = `(every 0.5s
  (wipe (? black navy maroon))
  (ink white)
  (box 50 50 (rand 20 100)))`;
    await harness.setCode(animCode);
    await harness.sleep(200);
    await harness.play();
    await harness.sleep(1500); // Let it animate
    await harness.stop();
  });
}

// UI interaction tests
async function testUIInteractions(harness) {
  await harness.runTest('Get current theme', async () => {
    const theme = await harness.getTheme();
    info(`Current theme: ${theme}`);
  });
  
  await harness.runTest('Toggle theme', async () => {
    const before = await harness.getTheme();
    await harness.toggleTheme();
    await harness.sleep(300);
    const after = await harness.getTheme();
    info(`Theme: ${before} -> ${after}`);
    // Toggle back
    await harness.toggleTheme();
    await harness.sleep(200);
  });
  
  await harness.runTest('Switch language to Spanish', async () => {
    const result = await harness.switchLanguage('es');
    info(`Language switch result: ${JSON.stringify(result)}`);
    await harness.sleep(300);
  });
  
  await harness.runTest('Switch language back to English', async () => {
    const result = await harness.switchLanguage('en');
    info(`Language switch result: ${JSON.stringify(result)}`);
    await harness.sleep(300);
  });
}

// Console tests
async function testConsole(harness) {
  await harness.runTest('Clear console', async () => {
    await harness.clearConsole();
    await harness.sleep(200);
  });
  
  await harness.runTest('Run code that produces console output', async () => {
    await harness.setCode('(print "Hello from KidLisp test!")');
    await harness.sleep(200);
    await harness.play();
    await harness.sleep(800);
  });
  
  await harness.runTest('Get console entries', async () => {
    const entries = await harness.getConsoleEntries();
    info(`Console has ${entries?.length || 0} entries`);
    if (entries && entries.length > 0) {
      entries.slice(0, 3).forEach(e => info(`  - [${e.level}] ${e.text?.substring(0, 50)}`));
    }
    await harness.stop();
  });
}

// KidLisp code examples test
async function testKidLispExamples(harness) {
  const examples = [
    {
      name: 'Checkerboard',
      code: `(each x 8
  (each y 8
    (ink (if (= (% (+ x y) 2) 0) white black))
    (box (* x 40) (* y 40) 40)))`
    },
    {
      name: 'Rainbow circles',
      code: `(every 0.1s
  (ink (? red orange yellow green blue purple))
  (circle (rand 320) (rand 240) (rand 10 50)))`
    },
    {
      name: 'Gradient',
      code: `(each i 256
  (ink (rgb i 0 (- 255 i)))
  (line i 0 i 240))`
    },
    {
      name: 'Noise pattern',
      code: `(each x 64
  (each y 48
    (ink (gray (noise (* x 0.1) (* y 0.1))))
    (box (* x 5) (* y 5) 5)))`
    },
    {
      name: 'Spiral',
      code: `(wipe black)
(each i 360
  (ink (hsl i 100 50))
  (circle 
    (+ 160 (* (/ i 4) (cos (radians i))))
    (+ 120 (* (/ i 4) (sin (radians i))))
    3))`
    }
  ];
  
  for (const example of examples) {
    await harness.runTest(`Example: ${example.name}`, async () => {
      await harness.setCode(example.code);
      await harness.sleep(200);
      await harness.play();
      await harness.sleep(1000);
      await harness.stop();
      await harness.sleep(200);
    });
  }
}

// Error handling tests
async function testErrorHandling(harness) {
  await harness.runTest('Syntax error handling', async () => {
    await harness.setCode('(ink red'); // Missing closing paren
    await harness.sleep(200);
    await harness.play();
    await harness.sleep(500);
    // Should show error in console
    const entries = await harness.getConsoleEntries();
    const hasError = entries?.some(e => e.level === 'error');
    info(`Console has error: ${hasError}`);
    await harness.stop();
  });
  
  await harness.runTest('Unknown function handling', async () => {
    await harness.setCode('(unknownfunc 123)');
    await harness.sleep(200);
    await harness.play();
    await harness.sleep(500);
    await harness.stop();
  });
  
  await harness.runTest('Recover from error', async () => {
    await harness.setCode('(ink blue)\n(box 50 50 100)');
    await harness.sleep(200);
    await harness.play();
    await harness.sleep(500);
    await harness.stop();
  });
}

// ═══════════════════════════════════════════════════════════════════════════
// Main
// ═══════════════════════════════════════════════════════════════════════════

async function main() {
  const args = process.argv.slice(2);
  const specificTest = args[0];
  
  heading('KidLisp.com Test Suite');
  info(`Started at ${new Date().toISOString()}`);
  
  const harness = new KidLispTestHarness();
  
  try {
    await harness.connect();
    
    const testSuites = {
      'basic': testBasicConnectivity,
      'editor': testEditorInteractions,
      'playback': testPlaybackControls,
      'ui': testUIInteractions,
      'console': testConsole,
      'examples': testKidLispExamples,
      'errors': testErrorHandling
    };
    
    if (specificTest && testSuites[specificTest]) {
      // Run specific test suite
      heading(`Running: ${specificTest}`);
      await testSuites[specificTest](harness);
    } else if (specificTest === 'all' || !specificTest) {
      // Run all test suites
      for (const [name, suite] of Object.entries(testSuites)) {
        heading(`Suite: ${name}`);
        await suite(harness);
      }
    } else {
      console.log(`Unknown test suite: ${specificTest}`);
      console.log(`Available suites: ${Object.keys(testSuites).join(', ')}, all`);
      process.exit(1);
    }
    
    const success = harness.printSummary();
    process.exit(success ? 0 : 1);
    
  } catch (error) {
    fail(`Fatal error: ${error.message}`);
    console.error(error.stack);
    process.exit(1);
  } finally {
    harness.close();
  }
}

main();
