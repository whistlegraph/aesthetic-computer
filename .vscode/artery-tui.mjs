#!/usr/bin/env node
/**
 * 🩸 Artery TUI - Interactive Terminal UI for Aesthetic Computer
 * A curses-style interface for controlling AC, running tests, and monitoring.
 */

import Artery from './artery.mjs';
import http from 'http';
import https from 'https';
import fs from 'fs';
import path from 'path';
import readline from 'readline';

// ANSI escape codes
const ESC = '\x1b';
const CSI = `${ESC}[`;

// Colors
const RESET = `${CSI}0m`;
const BOLD = `${CSI}1m`;
const DIM = `${CSI}2m`;
const ITALIC = `${CSI}3m`;
const UNDERLINE = `${CSI}4m`;
const BLINK = `${CSI}5m`;
const REVERSE = `${CSI}7m`;

// Foreground colors
const FG_BLACK = `${CSI}30m`;
const FG_RED = `${CSI}31m`;
const FG_GREEN = `${CSI}32m`;
const FG_YELLOW = `${CSI}33m`;
const FG_BLUE = `${CSI}34m`;
const FG_MAGENTA = `${CSI}35m`;
const FG_CYAN = `${CSI}36m`;
const FG_WHITE = `${CSI}37m`;
const FG_BRIGHT_RED = `${CSI}91m`;
const FG_BRIGHT_GREEN = `${CSI}92m`;
const FG_BRIGHT_YELLOW = `${CSI}93m`;
const FG_BRIGHT_MAGENTA = `${CSI}95m`;
const FG_BRIGHT_CYAN = `${CSI}96m`;

// Background colors
const BG_BLACK = `${CSI}40m`;
const BG_RED = `${CSI}41m`;
const BG_GREEN = `${CSI}42m`;
const BG_MAGENTA = `${CSI}45m`;
const BG_CYAN = `${CSI}46m`;
const BG_WHITE = `${CSI}47m`;
const BG_BRIGHT_BLACK = `${CSI}100m`;

// Cursor control
const CURSOR_HIDE = `${CSI}?25l`;
const CURSOR_SHOW = `${CSI}?25h`;
const CURSOR_HOME = `${CSI}H`;
const CLEAR_SCREEN = `${CSI}2J`;
const CLEAR_LINE = `${CSI}2K`;

// Move cursor
const moveTo = (row, col) => `${CSI}${row};${col}H`;

// Box drawing characters
const BOX = {
  topLeft: '╔',
  topRight: '╗',
  bottomLeft: '╚',
  bottomRight: '╝',
  horizontal: '═',
  vertical: '║',
  teeRight: '╠',
  teeLeft: '╣',
  teeDown: '╦',
  teeUp: '╩',
  cross: '╬',
  // Light variants
  lightH: '─',
  lightV: '│',
  lightTL: '┌',
  lightTR: '┐',
  lightBL: '└',
  lightBR: '┘',
};

// CDP Host detection (same as artery.mjs)
function getCDPHost() {
  if (process.env.REMOTE_CONTAINERS === 'true' || process.env.CODESPACES === 'true') {
    return 'host.docker.internal';
  }
  return 'localhost';
}

const CDP_HOST = getCDPHost();

class ArteryTUI {
  constructor() {
    this.client = null;
    this.connected = false;
    this.currentPiece = null;
    this.selectedIndex = 0;
    this.mode = 'menu'; // 'menu', 'repl', 'pieces', 'piece-input', 'tests', 'test-params', 'logs'
    this.logs = [];
    this.allPieces = []; // Loaded from disks directory
    this.filteredPieces = []; // Filtered by search
    this.maxLogs = 100;
    this.width = process.stdout.columns || 80;
    this.height = process.stdout.rows || 24;
    this.inputBuffer = '';
    this.statusMessage = '';
    this.statusTimeout = null;
    this.unreadLogs = 0;
    this.logRenderTimeout = null;
    
    // Server status
    this.serverStatus = { local: null, production: null }; // null = unknown, true = up, false = down
    this.serverMode = null; // 'local' or 'production' based on what AC is connected to
    
    // Test running state (defers live updates)
    this.testRunning = false;
    this.pendingRender = false;
    
    // Screen margins
    this.marginX = 2; // Left/right margin
    this.marginY = 1; // Top/bottom margin
    
    // Menu items
    this.menuItems = [
      { key: 'p', label: 'Open Panel', desc: 'Open AC sidebar in VS Code', action: () => this.openPanel() },
      { key: 'j', label: 'Jump to Piece', desc: 'Navigate to a piece', action: () => this.enterPieceMode() },
      { key: 'c', label: 'Current Piece', desc: 'Show current piece', action: () => this.showCurrent() },
      { key: 'r', label: 'REPL Mode', desc: 'Interactive JavaScript console', action: () => this.enterReplMode() },
      { key: 't', label: 'Run Tests', desc: 'Execute test suite', action: () => this.runTests() },
      { key: 'l', label: 'View Logs', desc: 'AC console output', action: () => this.enterLogsMode() },
      { key: 'a', label: 'Activate Audio', desc: 'Click to enable audio', action: () => this.activateAudio() },
      { key: 'w', label: 'WebGPU Perf', desc: 'Monitor WebGPU performance', action: () => this.webgpuPerf() },
      { key: 'x', label: 'Reconnect', desc: 'Reconnect to AC', action: () => this.reconnect() },
      { key: 'q', label: 'Quit', desc: 'Exit Artery TUI', action: () => this.quit() },
    ];
    
    // Load pieces from disk directory
    this.loadPieces();
  }
  
  loadPieces() {
    try {
      const disksPath = path.join(process.cwd(), 'system/public/aesthetic.computer/disks');
      const files = fs.readdirSync(disksPath);
      this.allPieces = files
        .filter(f => f.endsWith('.mjs') && !f.startsWith('.') && !f.includes('test'))
        .map(f => f.replace('.mjs', ''))
        .sort();
      this.filteredPieces = [...this.allPieces];
    } catch (e) {
      // Fallback to common pieces if directory read fails
      this.allPieces = [
        'prompt', 'notepat', 'wand', 'botce', 'painting', 'whistlegraph',
        'bleep', 'melody', 'tracker', 'metronome', 'microphone',
        'plot', 'line', 'rect', 'oval', 'starfield', 'toss'
      ];
      this.filteredPieces = [...this.allPieces];
    }
  }

  async start() {
    // Set up terminal
    process.stdin.setRawMode(true);
    process.stdin.resume();
    process.stdin.setEncoding('utf8');
    
    // Handle resize
    process.stdout.on('resize', () => {
      this.width = process.stdout.columns || 80;
      this.height = process.stdout.rows || 24;
      this.render();
    });
    
    // Handle input
    process.stdin.on('data', (key) => this.handleInput(key));
    
    // Initial render
    this.write(CURSOR_HIDE);
    this.write(CLEAR_SCREEN);
    
    // Try to connect
    await this.connect();
    
    // Main render
    this.render();
  }

  write(str) {
    process.stdout.write(str);
  }

  async connect() {
    this.setStatus('Checking servers...', 'info');
    this.render();
    
    // Check server availability in parallel
    await this.checkServers();
    
    this.setStatus('Connecting to AC...', 'info');
    this.render();
    
    try {
      // First check if CDP is available
      const targets = await this.getTargets();
      const acTarget = targets.find(t => 
        t.type === 'iframe' && 
        (t.url?.includes('localhost:8888') || t.url?.includes('aesthetic.computer'))
      );
      
      if (!acTarget) {
        this.connected = false;
        // Provide helpful status based on server availability
        if (this.serverStatus.local === false && this.serverStatus.production === false) {
          this.setStatus('No servers available - start local dev or check network', 'error');
        } else if (this.serverStatus.local === false && this.serverStatus.production === true) {
          this.setStatus('AC not found - press [p] to open panel (production ready)', 'warn');
        } else if (this.serverStatus.local === true) {
          this.setStatus('AC not found - press [p] to open panel (local ready)', 'warn');
        } else {
          this.setStatus('AC not found - press [p] to open panel', 'warn');
        }
        return;
      }
      
      // Determine which mode we're in based on the URL
      if (acTarget.url?.includes('localhost:8888')) {
        this.serverMode = 'local';
      } else if (acTarget.url?.includes('aesthetic.computer')) {
        this.serverMode = 'production';
      }
      
      this.client = new Artery();
      await this.client.connect();
      this.connected = true;
      
      // Get current piece
      try {
        this.currentPiece = await this.client.getCurrentPiece();
      } catch (e) {
        this.currentPiece = null;
      }
      
      // Enable console logging - we set up our own handler instead of using enableConsole()
      // because enableConsole() has its own print statements
      await this.client.send('Runtime.enable');
      await this.client.send('Console.enable');
      
      // Also listen for Runtime.consoleAPICalled for more complete coverage
      this.client.on('Console.messageAdded', (params) => {
        this.addLog(params.message.text, params.message.level || 'log');
      });
      
      this.client.on('Runtime.consoleAPICalled', (params) => {
        const text = params.args.map(arg => {
          if (arg.value !== undefined) return String(arg.value);
          if (arg.description) return arg.description;
          return JSON.stringify(arg);
        }).join(' ');
        this.addLog(text, params.type || 'log');
      });
      
      const modeLabel = this.serverMode === 'local' ? '💻 Local' : '🌐 Production';
      this.setStatus(`Connected to AC! (${modeLabel})`, 'success');
    } catch (e) {
      this.connected = false;
      this.setStatus(`Connection failed: ${e.message}`, 'error');
    }
  }
  
  async checkServers() {
    // Check both servers in parallel
    const [localStatus, prodStatus] = await Promise.all([
      this.checkServer('https://localhost:8888', false),  // false = allow self-signed certs
      this.checkServer('https://aesthetic.computer', true) // true = require valid certs
    ]);
    
    this.serverStatus.local = localStatus;
    this.serverStatus.production = prodStatus;
  }
  
  async checkServer(url, requireValidCert = true) {
    return new Promise((resolve) => {
      const timeout = setTimeout(() => resolve(false), 3000);
      
      const urlObj = new URL(url);
      const options = {
        hostname: urlObj.hostname,
        port: urlObj.port || 443,
        path: '/',
        method: 'HEAD',
        rejectUnauthorized: requireValidCert, // false allows self-signed certs
        timeout: 3000
      };
      
      const req = https.request(options, (res) => {
        clearTimeout(timeout);
        resolve(res.statusCode < 500);
      });
      
      req.on('error', () => {
        clearTimeout(timeout);
        resolve(false);
      });
      
      req.on('timeout', () => {
        clearTimeout(timeout);
        req.destroy();
        resolve(false);
      });
      
      req.end();
    });
  }

  async getTargets() {
    return new Promise((resolve, reject) => {
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
  }

  addLog(text, level = 'log') {
    const timestamp = new Date().toLocaleTimeString('en-US', { hour12: false });
    this.logs.unshift({ timestamp, text, level });
    if (this.logs.length > this.maxLogs) {
      this.logs.pop();
    }
    
    // Track unread logs when not in logs mode
    if (this.mode !== 'logs' && this.mode !== 'repl') {
      this.unreadLogs = (this.unreadLogs || 0) + 1;
    }
    
    // Throttled render for all modes to show log activity
    // But defer render if a test is currently running (to avoid flickering)
    if (!this.logRenderTimeout) {
      this.logRenderTimeout = setTimeout(() => {
        this.logRenderTimeout = null;
        if (this.testRunning) {
          this.pendingRender = true;
        } else {
          this.render();
        }
      }, 100); // Throttle to 10fps max
    }
  }

  setStatus(msg, type = 'info') {
    this.statusMessage = msg;
    this.statusType = type;
    if (this.statusTimeout) clearTimeout(this.statusTimeout);
    this.statusTimeout = setTimeout(() => {
      this.statusMessage = '';
      this.render();
    }, 5000);
    this.render();
  }

  handleInput(key) {
    // Ctrl+C to quit
    if (key === '\u0003') {
      this.quit();
      return;
    }
    
    // Escape to go back to menu
    if (key === '\u001b' && this.mode !== 'menu') {
      this.mode = 'menu';
      this.inputBuffer = '';
      this.render();
      return;
    }
    
    switch (this.mode) {
      case 'menu':
        this.handleMenuInput(key);
        break;
      case 'repl':
        this.handleReplInput(key);
        break;
      case 'pieces':
        this.handlePiecesInput(key);
        break;
      case 'piece-input':
        this.handlePieceInputMode(key);
        break;
      case 'tests':
        this.handleTestsInput(key);
        break;
      case 'test-params':
        this.handleTestParamsInput(key);
        break;
      case 'logs':
        this.handleLogsInput(key);
        break;
    }
  }

  handleMenuInput(key) {
    // Arrow keys
    if (key === '\u001b[A') { // Up
      this.selectedIndex = Math.max(0, this.selectedIndex - 1);
      this.render();
      return;
    }
    if (key === '\u001b[B') { // Down
      this.selectedIndex = Math.min(this.menuItems.length - 1, this.selectedIndex + 1);
      this.render();
      return;
    }
    
    // Enter to select
    if (key === '\r' || key === '\n') {
      this.menuItems[this.selectedIndex].action();
      return;
    }
    
    // Shortcut keys
    const item = this.menuItems.find(m => m.key === key.toLowerCase());
    if (item) {
      item.action();
    }
  }

  handleReplInput(key) {
    // Enter to execute
    if (key === '\r' || key === '\n') {
      if (this.inputBuffer.trim()) {
        this.executeRepl(this.inputBuffer);
        this.inputBuffer = '';
      }
      this.render();
      return;
    }
    
    // Backspace
    if (key === '\u007f' || key === '\b') {
      this.inputBuffer = this.inputBuffer.slice(0, -1);
      this.render();
      return;
    }
    
    // Regular character
    if (key.length === 1 && key.charCodeAt(0) >= 32) {
      this.inputBuffer += key;
      this.render();
    }
  }

  handlePiecesInput(key) {
    // Arrow keys
    if (key === '\u001b[A') { // Up
      this.selectedIndex = Math.max(0, this.selectedIndex - 1);
      this.render();
      return;
    }
    if (key === '\u001b[B') { // Down
      this.selectedIndex = Math.min(this.filteredPieces.length - 1, this.selectedIndex + 1);
      this.render();
      return;
    }
    
    // Enter to jump
    if (key === '\r' || key === '\n') {
      const piece = this.filteredPieces[this.selectedIndex];
      if (piece) this.jumpToPiece(piece);
      return;
    }
    
    // Tab to autocomplete
    if (key === '\t') {
      const piece = this.filteredPieces[this.selectedIndex];
      if (piece) {
        this.inputBuffer = piece;
        this.render();
      }
      return;
    }
    
    // Backspace
    if (key === '\u007f' || key === '\b') {
      this.inputBuffer = this.inputBuffer.slice(0, -1);
      this.filterPieces();
      this.render();
      return;
    }
    
    // Regular character - start filtering
    if (key.length === 1 && key.charCodeAt(0) >= 32) {
      this.inputBuffer += key;
      this.filterPieces();
      this.render();
    }
  }
  
  handlePieceInputMode(key) {
    // Enter to jump to typed piece
    if (key === '\r' || key === '\n') {
      if (this.inputBuffer.trim()) {
        this.jumpToPiece(this.inputBuffer.trim());
      }
      return;
    }
    
    // Tab to autocomplete from filtered list
    if (key === '\t') {
      if (this.filteredPieces.length > 0) {
        this.inputBuffer = this.filteredPieces[0];
        this.filterPieces();
        this.render();
      }
      return;
    }
    
    // Backspace
    if (key === '\u007f' || key === '\b') {
      this.inputBuffer = this.inputBuffer.slice(0, -1);
      this.filterPieces();
      this.render();
      return;
    }
    
    // Regular character
    if (key.length === 1 && key.charCodeAt(0) >= 32) {
      this.inputBuffer += key;
      this.filterPieces();
      this.render();
    }
  }
  
  filterPieces() {
    const search = this.inputBuffer.toLowerCase();
    if (!search) {
      this.filteredPieces = [...this.allPieces];
    } else {
      this.filteredPieces = this.allPieces.filter(p => p.toLowerCase().includes(search));
    }
    this.selectedIndex = 0;
  }

  handleTestsInput(key) {
    // Arrow keys
    if (key === '\u001b[A') { // Up
      this.selectedIndex = Math.max(0, this.selectedIndex - 1);
      this.render();
      return;
    }
    if (key === '\u001b[B') { // Down
      this.selectedIndex = Math.min((this.testFiles?.length || 1) - 1, this.selectedIndex + 1);
      this.render();
      return;
    }
    
    // Enter to run test (or open params for parametric tests)
    if (key === '\r' || key === '\n') {
      const test = this.testFiles?.[this.selectedIndex];
      if (test) {
        if (test.params) {
          // Show parameter input mode
          this.currentTest = test;
          this.testParams = { ...test.defaults };
          this.paramIndex = 0;
          this.inputBuffer = '';
          this.mode = 'test-params';
          this.render();
        } else {
          this.executeTest(test.file);
        }
      }
      return;
    }
  }
  
  handleTestParamsInput(key) {
    const params = this.currentTest?.params || [];
    const param = params[this.paramIndex];
    
    // Arrow Up - move to previous param
    if (key === '\u001b[A') {
      // Save current value first
      if (param && this.inputBuffer !== undefined) {
        this.testParams[param.key] = this.inputBuffer;
      }
      this.paramIndex = Math.max(0, this.paramIndex - 1);
      if (this.paramIndex < params.length) {
        this.inputBuffer = String(this.testParams[params[this.paramIndex]?.key] || '');
      }
      this.render();
      return;
    }
    
    // Arrow Down - move to next param
    if (key === '\u001b[B') {
      // Save current value first
      if (param && this.inputBuffer !== undefined) {
        this.testParams[param.key] = this.inputBuffer;
      }
      this.paramIndex = Math.min(params.length, this.paramIndex + 1);
      if (this.paramIndex < params.length) {
        this.inputBuffer = String(this.testParams[params[this.paramIndex]?.key] || '');
      } else {
        this.inputBuffer = '';
      }
      this.render();
      return;
    }
    
    // Arrow Left/Right - cycle through options for params that have them
    if (key === '\u001b[D' || key === '\u001b[C') { // Left or Right
      if (param && param.options) {
        const options = param.options;
        const currentVal = this.inputBuffer || this.testParams[param.key] || param.default;
        let idx = options.indexOf(currentVal);
        if (idx === -1) idx = 0;
        
        if (key === '\u001b[C') { // Right - next option
          idx = (idx + 1) % options.length;
        } else { // Left - previous option
          idx = (idx - 1 + options.length) % options.length;
        }
        
        this.inputBuffer = options[idx];
        this.testParams[param.key] = options[idx];
        this.render();
        return;
      } else if (param && param.type === 'number') {
        // For number params, increment/decrement
        let val = parseInt(this.inputBuffer) || parseInt(param.default) || 0;
        const step = param.step || 1;
        const min = param.min ?? 1;
        const max = param.max ?? 100;
        
        if (key === '\u001b[C') { // Right - increment
          val = Math.min(max, val + step);
        } else { // Left - decrement
          val = Math.max(min, val - step);
        }
        
        this.inputBuffer = String(val);
        this.testParams[param.key] = String(val);
        this.render();
        return;
      }
    }
    
    // Enter to confirm param or run test
    if (key === '\r' || key === '\n') {
      if (this.paramIndex < params.length && param) {
        // Save current param and move to next
        this.testParams[param.key] = this.inputBuffer || param.default;
        this.paramIndex++;
        if (this.paramIndex < params.length) {
          this.inputBuffer = String(this.testParams[params[this.paramIndex]?.key] || '');
        } else {
          this.inputBuffer = '';
        }
        this.render();
      } else {
        // Run the test with params
        const args = params.map(p => this.testParams[p.key] || p.default).join(' ');
        this.executeTest(this.currentTest.file, args);
      }
      return;
    }
    
    // Backspace
    if (key === '\u007f' || key === '\b') {
      this.inputBuffer = this.inputBuffer.slice(0, -1);
      this.render();
      return;
    }
    
    // Regular character
    if (key.length === 1 && key.charCodeAt(0) >= 32) {
      this.inputBuffer += key;
      this.render();
    }
  }

  handleLogsInput(key) {
    // Just escape to exit (handled globally)
  }

  async executeRepl(code) {
    if (!this.connected) {
      this.addLog('Not connected to AC', 'error');
      return;
    }
    
    // Handle special commands
    if (code.startsWith('.')) {
      const parts = code.slice(1).split(' ');
      const cmd = parts[0];
      const args = parts.slice(1).join(' ');
      
      switch (cmd) {
        case 'jump':
          await this.jumpToPiece(args);
          return;
        case 'current':
          await this.showCurrent();
          return;
        case 'clear':
          this.logs = [];
          this.render();
          return;
      }
    }
    
    try {
      this.addLog(`> ${code}`, 'input');
      const result = await this.client.eval(code);
      if (result !== undefined) {
        this.addLog(String(result), 'result');
      }
    } catch (e) {
      this.addLog(`Error: ${e.message}`, 'error');
    }
  }

  // Actions
  async openPanel() {
    this.setStatus('Opening AC panel...', 'info');
    this.render();
    
    try {
      // Use the standalone panel opener from Artery which uses the Enter key method
      await Artery.openPanelStandalone();
      
      // Wait a moment for panel to initialize
      await new Promise(r => setTimeout(r, 500));
      
      // Reconnect after panel opens
      await this.connect();
      
      if (this.connected) {
        this.setStatus('Panel opened and connected!', 'success');
      } else {
        this.setStatus('Panel opened - connecting...', 'info');
      }
    } catch (e) {
      this.setStatus(`Failed to open panel: ${e.message}`, 'error');
    }
    
    this.render();
  }

  enterPieceMode() {
    this.mode = 'pieces';
    this.selectedIndex = 0;
    this.inputBuffer = '';
    this.filteredPieces = [...this.allPieces];
    this.render();
  }

  async showCurrent() {
    if (!this.connected) {
      this.setStatus('Not connected', 'error');
      return;
    }
    try {
      this.currentPiece = await this.client.getCurrentPiece();
      this.setStatus(`Current: ${this.currentPiece || '(none)'}`, 'info');
    } catch (e) {
      this.setStatus(`Error: ${e.message}`, 'error');
    }
  }

  async runTests() {
    // Show available tests
    this.mode = 'tests';
    
    // Available melodies from melodies.mjs
    const melodyOptions = [
      'twinkle', 'mary', 'old-macdonald', 'yankee-doodle', 'frere-jacques',
      'london-bridge', 'row-row-row', 'skip-to-my-lou', 'camptown-races',
      'oh-susanna', 'amazing-grace', 'auld-lang-syne', 'shenandoah',
      'home-on-the-range', 'red-river-valley', 'scarborough-fair',
      'greensleeves', 'when-the-saints', 'danny-boy'
    ];
    
    this.testFiles = [
      { name: 'notepat', file: 'test-notepat.mjs', desc: 'Notepat fuzzing test' },
      {
        name: 'melody',
        file: 'test-melody.mjs',
        desc: 'Melody playback [configurable]',
        params: [
          { key: 'melody', label: 'Melody', desc: '←→ to cycle', default: 'twinkle', options: melodyOptions },
        ],
        defaults: { melody: 'twinkle' }
      },
      { name: 'chords', file: 'test-chords.mjs', desc: 'Chord progression test' },
      { name: 'line', file: 'test-line.mjs', desc: 'Line drawing test' },
      { name: 'toss', file: 'test-toss.mjs', desc: 'Comprehensive toss test' },
      { name: 'playlist', file: 'test-playlist.mjs', desc: 'Playlist test' },
      { 
        name: 'waltz', 
        file: 'test-generative-waltz.mjs', 
        desc: 'Generative waltz [configurable]',
        params: [
          { key: 'bars', label: 'Bars', desc: '←→ to adjust', default: '8', type: 'number', min: 4, max: 32, step: 4 },
          { key: 'scale', label: 'Scale', desc: '←→ to cycle', default: 'major', options: ['major', 'minor', 'dorian'] },
          { key: 'seed', label: 'Seed', desc: 'Random seed (type or ←→)', default: String(Date.now()), type: 'number', min: 1, max: 999999, step: 111 },
          { key: 'tempo', label: 'Tempo', desc: '←→ to cycle', default: 'slow', options: ['slow', 'medium', 'viennese'] },
          { key: 'topline', label: 'Top Line', desc: '←→ toggle', default: '', options: ['', 'topline'] },
          { key: 'infinite', label: 'Infinite', desc: '←→ toggle', default: '', options: ['', 'infinite'] },
          { key: 'frolic', label: 'Frolic', desc: '←→ toggle (fast RH)', default: '', options: ['', 'frolic'] },
          { key: 'beat', label: 'Beat', desc: '←→ toggle (drums)', default: '', options: ['', 'beat'] },
        ],
        defaults: { bars: '8', scale: 'major', seed: String(Date.now()), tempo: 'slow', topline: '', infinite: '', frolic: '', beat: '' }
      },
    ];
    this.selectedIndex = 0;
    this.setStatus('Select a test to run (Enter for params)', 'info');
    this.render();
  }
  
  async executeTest(testFile, args = '') {
    this.mode = 'logs';
    this.testRunning = true;
    this.pendingRender = false;
    const argsDisplay = args ? ` (${args})` : '';
    this.setStatus(`Running ${testFile}${argsDisplay}...`, 'info');
    this.render();
    
    try {
      const { spawn } = await import('child_process');
      const testPath = `.vscode/tests/${testFile}`;
      
      // Build command args
      const cmdArgs = [testPath];
      if (args) {
        cmdArgs.push(...args.split(' ').filter(a => a.trim()));
      }
      
      // Run the test in a subprocess
      const proc = spawn('node', cmdArgs, {
        cwd: process.cwd(),
        stdio: ['ignore', 'pipe', 'pipe']
      });
      
      proc.stdout.on('data', (data) => {
        const lines = data.toString().split('\n').filter(l => l.trim());
        lines.forEach(line => this.addLog(line, 'log'));
      });
      
      proc.stderr.on('data', (data) => {
        const lines = data.toString().split('\n').filter(l => l.trim());
        lines.forEach(line => this.addLog(line, 'error'));
      });
      
      proc.on('close', (code) => {
        this.testRunning = false;
        if (code === 0) {
          this.setStatus(`Test ${testFile} completed!`, 'success');
        } else {
          this.setStatus(`Test ${testFile} exited with code ${code}`, 'error');
        }
        // Do the deferred render now that test is complete
        if (this.pendingRender) {
          this.pendingRender = false;
        }
        this.render();
      });
      
    } catch (e) {
      this.testRunning = false;
      this.setStatus(`Test error: ${e.message}`, 'error');
      this.render();
    }
  }

  enterLogsMode() {
    this.mode = 'logs';
    this.unreadLogs = 0; // Clear unread count
    this.render();
  }

  enterReplMode() {
    this.mode = 'repl';
    this.inputBuffer = '';
    this.unreadLogs = 0; // Clear unread since REPL also shows logs
    this.render();
  }

  async activateAudio() {
    if (!this.connected) {
      this.setStatus('Not connected', 'error');
      return;
    }
    try {
      await this.client.click(100, 100);
      this.setStatus('Audio activated!', 'success');
    } catch (e) {
      this.setStatus(`Error: ${e.message}`, 'error');
    }
  }

  async webgpuPerf() {
    this.setStatus('Starting WebGPU perf monitor (5s)...', 'info');
    if (!this.connected) {
      this.setStatus('Not connected', 'error');
      return;
    }
    try {
      const result = await this.client.eval(`
        if (window.api && window.api.webgpu && window.api.webgpu.perf) {
          window.api.webgpu.perf(true);
          'WebGPU perf overlay enabled';
        } else {
          'WebGPU API not available';
        }
      `);
      this.setStatus(result, 'info');
    } catch (e) {
      this.setStatus(`Error: ${e.message}`, 'error');
    }
  }

  async reconnect() {
    if (this.client) {
      try { this.client.close(); } catch (e) {}
    }
    this.client = null;
    this.connected = false;
    await this.connect();
  }

  async jumpToPiece(piece) {
    if (!piece) return;
    if (!this.connected) {
      this.setStatus('Not connected', 'error');
      return;
    }
    try {
      await this.client.jump(piece);
      this.currentPiece = piece;
      this.setStatus(`Jumped to: ${piece}`, 'success');
      this.mode = 'menu';
      this.render();
    } catch (e) {
      this.setStatus(`Jump failed: ${e.message}`, 'error');
    }
  }

  quit() {
    this.write(CURSOR_SHOW);
    this.write(CLEAR_SCREEN);
    this.write(CURSOR_HOME);
    if (this.client) {
      try { this.client.close(); } catch (e) {}
    }
    process.exit(0);
  }

  // Helper to get inner width (accounting for margins)
  get innerWidth() {
    return this.width - (this.marginX * 2);
  }
  
  // Helper to get inner height (accounting for margins)  
  get innerHeight() {
    return this.height - (this.marginY * 2);
  }
  
  // Write a line with margins
  writeLine(content) {
    const margin = ' '.repeat(this.marginX);
    this.write(`${margin}${content}\n`);
  }

  // Rendering
  render() {
    this.write(CURSOR_HOME);
    this.write(CLEAR_SCREEN);
    
    // Top margin
    for (let i = 0; i < this.marginY; i++) {
      this.write('\n');
    }
    
    switch (this.mode) {
      case 'menu':
        this.renderMenu();
        break;
      case 'repl':
        this.renderRepl();
        break;
      case 'pieces':
      case 'piece-input':
        this.renderPieces();
        break;
      case 'tests':
        this.renderTests();
        break;
      case 'test-params':
        this.renderTestParams();
        break;
      case 'logs':
        this.renderLogs();
        break;
    }
  }

  renderHeader() {
    const title = ' 🩸 ARTERY TUI ';
    const status = this.connected 
      ? `${FG_GREEN}● Connected${RESET}` 
      : `${FG_RED}○ Disconnected${RESET}`;
    const piece = this.currentPiece ? ` | ${FG_CYAN}${this.currentPiece}${RESET}` : '';
    
    // Server status indicators
    const localIcon = this.serverStatus.local === true ? `${FG_GREEN}●${RESET}` 
                    : this.serverStatus.local === false ? `${FG_RED}○${RESET}` 
                    : `${DIM}?${RESET}`;
    const prodIcon = this.serverStatus.production === true ? `${FG_GREEN}●${RESET}` 
                   : this.serverStatus.production === false ? `${FG_RED}○${RESET}` 
                   : `${DIM}?${RESET}`;
    const serverInfo = ` | ${DIM}L:${RESET}${localIcon} ${DIM}P:${RESET}${prodIcon}`;
    
    const boxWidth = this.innerWidth;
    
    // Top border
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.topLeft}${BOX.horizontal.repeat(boxWidth - 2)}${BOX.topRight}${RESET}`);
    
    // Title line
    const titleLine = `${BOX.vertical} ${BG_MAGENTA}${FG_WHITE}${BOLD}${title}${RESET} ${status}${piece}${serverInfo}`;
    const padding = boxWidth - this.stripAnsi(titleLine).length - 1;
    this.writeLine(`${FG_BRIGHT_MAGENTA}${titleLine}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    // Separator
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.horizontal.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
  }

  renderFooter() {
    const boxWidth = this.innerWidth;
    
    // Status message
    let statusLine = '';
    if (this.statusMessage) {
      const colors = {
        info: FG_CYAN,
        success: FG_GREEN,
        warn: FG_YELLOW,
        error: FG_RED,
      };
      statusLine = `${colors[this.statusType] || FG_WHITE}${this.statusMessage}${RESET}`;
    }
    
    // Bottom border with status
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.horizontal.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    const footerContent = statusLine || `${DIM}Press [q] to quit | [Esc] for menu${RESET}`;
    const footerPadding = boxWidth - this.stripAnsi(footerContent).length - 4;
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${footerContent}${' '.repeat(Math.max(0, footerPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.bottomLeft}${BOX.horizontal.repeat(boxWidth - 2)}${BOX.bottomRight}${RESET}`);
  }

  renderMenu() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    
    // Menu items - update logs item to show unread count
    this.menuItems.forEach((item, i) => {
      const selected = i === this.selectedIndex;
      const prefix = selected ? `${BG_MAGENTA}${FG_WHITE} ▸ ` : '   ';
      const suffix = selected ? ` ${RESET}` : '';
      const key = `${FG_YELLOW}[${item.key}]${RESET}`;
      let label = selected ? `${BOLD}${item.label}${RESET}` : item.label;
      let desc = `${DIM}${item.desc}${RESET}`;
      
      // Add unread badge to logs item
      if (item.key === 'l' && this.unreadLogs > 0) {
        label = `${label} ${BG_RED}${FG_WHITE} ${this.unreadLogs} ${RESET}`;
      }
      
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${prefix}${key} ${label} ${desc}${suffix}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    });
    
    // Separator before live log preview
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Show recent logs preview at bottom
    const logPreviewTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${DIM}📋 Recent Logs (${this.logs.length} total)${RESET}`;
    const titlePadding = boxWidth - this.stripAnsi(logPreviewTitle).length - 1;
    this.writeLine(`${logPreviewTitle}${' '.repeat(Math.max(0, titlePadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    // Calculate space for log preview
    const usedLines = 3 + this.menuItems.length + 2 + 3 + (this.marginY * 2); // header + items + separator + title + footer + margins
    const logPreviewLines = Math.max(0, this.height - usedLines - 1);
    const recentLogs = this.logs.slice(0, logPreviewLines);
    
    for (let i = 0; i < logPreviewLines; i++) {
      if (i < recentLogs.length) {
        const log = recentLogs[i];
        const colors = {
          log: FG_WHITE,
          info: FG_CYAN,
          warn: FG_YELLOW,
          error: FG_RED,
          warning: FG_YELLOW,
        };
        const color = colors[log.level] || FG_WHITE;
        const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${DIM}${log.timestamp}${RESET} ${color}${log.text}${RESET}`;
        const truncated = this.truncate(line, boxWidth - 2);
        const padding = boxWidth - this.stripAnsi(truncated).length - 1;
        this.writeLine(`${truncated}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
      } else {
        this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
      }
    }
    
    this.renderFooter();
  }

  renderRepl() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    
    // REPL title
    const replTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${BOLD}${FG_CYAN}JavaScript REPL${RESET} ${DIM}(type .help for commands)${RESET}`;
    const replPadding = boxWidth - this.stripAnsi(replTitle).length - 1;
    this.writeLine(`${replTitle}${' '.repeat(Math.max(0, replPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Show recent logs
    const logLines = this.innerHeight - 10;
    const recentLogs = this.logs.slice(0, logLines).reverse();
    
    for (const log of recentLogs) {
      const colors = {
        log: FG_WHITE,
        info: FG_CYAN,
        warn: FG_YELLOW,
        error: FG_RED,
        input: FG_BRIGHT_MAGENTA,
        result: FG_GREEN,
      };
      const color = colors[log.level] || FG_WHITE;
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${DIM}${log.timestamp}${RESET} ${color}${log.text}${RESET}`;
      const truncated = this.truncate(line, boxWidth - 2);
      const padding = boxWidth - this.stripAnsi(truncated).length - 1;
      this.writeLine(`${truncated}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Fill remaining
    for (let i = recentLogs.length; i < logLines; i++) {
      this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Input line
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    const prompt = `${FG_BRIGHT_MAGENTA}🩸${RESET} `;
    const inputLine = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${prompt}${this.inputBuffer}`;
    const inputPadding = boxWidth - this.stripAnsi(inputLine).length - 1;
    this.writeLine(`${inputLine}${' '.repeat(Math.max(0, inputPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    this.renderFooter();
    
    // Show cursor at input position
    this.write(CURSOR_SHOW);
    this.write(moveTo(this.height - 3, this.marginX + 6 + this.inputBuffer.length));
  }

  renderPieces() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    
    // Title with piece count
    const countInfo = `${DIM}(${this.filteredPieces.length}/${this.allPieces.length} pieces)${RESET}`;
    const pieceTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${BOLD}${FG_CYAN}Jump to Piece${RESET} ${countInfo}`;
    const piecePadding = boxWidth - this.stripAnsi(pieceTitle).length - 1;
    this.writeLine(`${pieceTitle}${' '.repeat(Math.max(0, piecePadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    // Search input line
    const searchPrompt = `${FG_YELLOW}🔍${RESET} `;
    const searchDisplay = this.inputBuffer || `${DIM}Type to search...${RESET}`;
    const searchLine = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${searchPrompt}${searchDisplay}`;
    const searchPadding = boxWidth - this.stripAnsi(searchLine).length - 1;
    this.writeLine(`${searchLine}${' '.repeat(Math.max(0, searchPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Pieces list
    const visibleCount = this.innerHeight - 10;
    const startIdx = Math.max(0, this.selectedIndex - Math.floor(visibleCount / 2));
    const endIdx = Math.min(this.filteredPieces.length, startIdx + visibleCount);
    
    for (let i = startIdx; i < endIdx; i++) {
      const piece = this.filteredPieces[i];
      const selected = i === this.selectedIndex;
      const prefix = selected ? `${BG_MAGENTA}${FG_WHITE} ▸ ` : '   ';
      const suffix = selected ? ` ${RESET}` : '';
      
      // Highlight matching part of name
      let label;
      if (this.inputBuffer && piece.toLowerCase().includes(this.inputBuffer.toLowerCase())) {
        const idx = piece.toLowerCase().indexOf(this.inputBuffer.toLowerCase());
        const before = piece.slice(0, idx);
        const match = piece.slice(idx, idx + this.inputBuffer.length);
        const after = piece.slice(idx + this.inputBuffer.length);
        label = selected 
          ? `${BOLD}${before}${FG_YELLOW}${match}${FG_WHITE}${after}${RESET}` 
          : `${before}${FG_YELLOW}${match}${RESET}${after}`;
      } else {
        label = selected ? `${BOLD}${piece}${RESET}` : piece;
      }
      
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${prefix}${label}${suffix}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Fill remaining
    for (let i = endIdx - startIdx; i < visibleCount; i++) {
      this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    this.renderFooter();
    
    // Show cursor at search position
    this.write(CURSOR_SHOW);
    this.write(moveTo(this.marginY + 2, this.marginX + 5 + this.inputBuffer.length));
  }

  renderTests() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    
    // Title
    const testTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${BOLD}${FG_CYAN}🧪 Test Suite${RESET} ${DIM}(↑↓ to select, Enter to run/configure)${RESET}`;
    const testPadding = boxWidth - this.stripAnsi(testTitle).length - 1;
    this.writeLine(`${testTitle}${' '.repeat(Math.max(0, testPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Tests list
    const tests = this.testFiles || [];
    const visibleCount = this.innerHeight - 8;
    
    for (let i = 0; i < Math.min(tests.length, visibleCount); i++) {
      const test = tests[i];
      const selected = i === this.selectedIndex;
      const prefix = selected ? `${BG_MAGENTA}${FG_WHITE} ▸ ` : '   ';
      const suffix = selected ? `${RESET}` : '';
      const configIcon = test.params ? `${FG_YELLOW}⚙${RESET} ` : '';
      const label = selected ? `${BOLD}${test.name}${RESET}` : test.name;
      const desc = `${DIM}${test.desc}${RESET}`;
      
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${prefix}${configIcon}${label}${suffix} ${desc}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Fill remaining
    for (let i = tests.length; i < visibleCount; i++) {
      this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    this.renderFooter();
  }
  
  renderTestParams() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const test = this.currentTest;
    const params = test?.params || [];
    
    // Title
    const testTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${BOLD}${FG_CYAN}⚙ Configure: ${test?.name}${RESET} ${DIM}(↑↓ move, ←→ change value, Enter confirm)${RESET}`;
    const testPadding = boxWidth - this.stripAnsi(testTitle).length - 1;
    this.writeLine(`${testTitle}${' '.repeat(Math.max(0, testPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Parameters list
    for (let i = 0; i < params.length; i++) {
      const param = params[i];
      const selected = i === this.paramIndex;
      const prefix = selected ? `${BG_MAGENTA}${FG_WHITE} ▸ ` : '   ';
      const suffix = selected ? `${RESET}` : '';
      
      const value = selected ? this.inputBuffer : (this.testParams[param.key] || param.default);
      
      // Show value with arrows if it has options or is a number
      let valueDisplay;
      if (selected && (param.options || param.type === 'number')) {
        const leftArrow = `${FG_YELLOW}◀${RESET}`;
        const rightArrow = `${FG_YELLOW}▶${RESET}`;
        valueDisplay = value ? `${leftArrow} ${FG_GREEN}${value}${RESET} ${rightArrow}` : `${leftArrow} ${DIM}(empty)${RESET} ${rightArrow}`;
      } else {
        valueDisplay = value ? `${FG_GREEN}${value}${RESET}` : `${DIM}(empty)${RESET}`;
      }
      
      const label = selected ? `${BOLD}${param.label}${RESET}` : param.label;
      const desc = `${DIM}${param.desc}${RESET}`;
      
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${prefix}${label}:${suffix} ${valueDisplay} ${desc}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Run button
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    const runSelected = this.paramIndex >= params.length;
    const runPrefix = runSelected ? `${BG_GREEN}${FG_WHITE} ▸ ` : '   ';
    const runSuffix = runSelected ? `${RESET}` : '';
    const runLabel = runSelected ? `${BOLD}🚀 RUN TEST${RESET}` : `${FG_GREEN}🚀 RUN TEST${RESET}`;
    const runLine = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${runPrefix}${runLabel}${runSuffix}`;
    const runPadding = boxWidth - this.stripAnsi(runLine).length - 1;
    this.writeLine(`${runLine}${' '.repeat(Math.max(0, runPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    // Preview command
    const args = params.map(p => this.testParams[p.key] || p.default).filter(a => a).join(' ');
    const cmdPreview = `${DIM}Command: node .vscode/tests/${test?.file} ${args}${RESET}`;
    const previewLine = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${cmdPreview}`;
    const previewPadding = boxWidth - this.stripAnsi(previewLine).length - 1;
    this.writeLine(`${previewLine}${' '.repeat(Math.max(0, previewPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    
    // Fill remaining
    const usedLines = params.length + 4;
    const visibleCount = this.innerHeight - 8;
    for (let i = usedLines; i < visibleCount; i++) {
      this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    this.renderFooter();
    
    // Show cursor at input position if editing a param (only for non-option params)
    if (this.paramIndex < params.length) {
      const param = params[this.paramIndex];
      if (!param.options && param.type !== 'number') {
        this.write(CURSOR_SHOW);
        // Approximate cursor position
        const labelLen = param.label.length + 3;
        this.write(moveTo(this.marginY + 3 + this.paramIndex, this.marginX + 4 + labelLen + this.inputBuffer.length));
      }
    }
  }

  renderLogs() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    
    // Title
    const logsTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${BOLD}${FG_CYAN}AC Console Logs${RESET} ${DIM}(${this.logs.length} entries)${RESET}`;
    const logsPadding = boxWidth - this.stripAnsi(logsTitle).length - 1;
    this.writeLine(`${logsTitle}${' '.repeat(Math.max(0, logsPadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Logs
    const logLines = this.innerHeight - 7;
    const recentLogs = this.logs.slice(0, logLines);
    
    for (const log of recentLogs) {
      const colors = {
        log: FG_WHITE,
        info: FG_CYAN,
        warn: FG_YELLOW,
        error: FG_RED,
      };
      const color = colors[log.level] || FG_WHITE;
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${DIM}${log.timestamp}${RESET} ${color}${log.text}${RESET}`;
      const truncated = this.truncate(line, boxWidth - 2);
      const padding = boxWidth - this.stripAnsi(truncated).length - 1;
      this.writeLine(`${truncated}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Fill remaining
    for (let i = recentLogs.length; i < logLines; i++) {
      this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    this.renderFooter();
  }

  // Utilities
  stripAnsi(str) {
    return str.replace(/\x1b\[[0-9;]*m/g, '');
  }

  truncate(str, maxLen) {
    const stripped = this.stripAnsi(str);
    if (stripped.length <= maxLen) return str;
    
    // Simple truncation - not perfect with ANSI codes but good enough
    let visible = 0;
    let result = '';
    let inEscape = false;
    
    for (const char of str) {
      if (char === '\x1b') {
        inEscape = true;
        result += char;
      } else if (inEscape) {
        result += char;
        if (char === 'm') inEscape = false;
      } else {
        if (visible < maxLen - 3) {
          result += char;
          visible++;
        } else if (visible === maxLen - 3) {
          result += '...';
          visible += 3;
        }
      }
    }
    
    return result + RESET;
  }
}

// Main
async function main() {
  const tui = new ArteryTUI();
  await tui.start();
}

main().catch(e => {
  console.error('Fatal error:', e);
  process.exit(1);
});
