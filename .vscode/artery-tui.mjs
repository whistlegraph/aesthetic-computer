#!/usr/bin/env node
/**
 * 🩸 Artery TUI - Interactive Terminal UI for Aesthetic Computer
 * A curses-style interface for controlling AC, running tests, and monitoring.
 */

import Artery from './artery.mjs';
import http from 'http';
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
    this.mode = 'menu'; // 'menu', 'repl', 'pieces', 'tests', 'logs'
    this.logs = [];
    this.maxLogs = 100;
    this.width = process.stdout.columns || 80;
    this.height = process.stdout.rows || 24;
    this.inputBuffer = '';
    this.statusMessage = '';
    this.statusTimeout = null;
    this.unreadLogs = 0;
    this.logRenderTimeout = null;
    
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
    
    // Quick pieces for jump menu
    this.quickPieces = [
      'prompt', 'notepat', 'wand', 'botce', 'painting', 'whistlegraph',
      'bleep', 'melody', 'tracker', 'metronome', 'microphone',
      'plot', 'line', 'rect', 'oval', 'starfield',
      'land', 'world', 'walk', 'stage',
    ];
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
    this.setStatus('Connecting to AC...', 'info');
    
    try {
      // First check if CDP is available
      const targets = await this.getTargets();
      const acTarget = targets.find(t => 
        t.type === 'iframe' && 
        (t.url?.includes('localhost:8888') || t.url?.includes('aesthetic.computer'))
      );
      
      if (!acTarget) {
        this.connected = false;
        this.setStatus('AC not found - press [p] to open panel', 'warn');
        return;
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
      
      this.setStatus('Connected to AC!', 'success');
    } catch (e) {
      this.connected = false;
      this.setStatus(`Connection failed: ${e.message}`, 'error');
    }
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
    if (!this.logRenderTimeout) {
      this.logRenderTimeout = setTimeout(() => {
        this.logRenderTimeout = null;
        this.render();
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
      this.selectedIndex = Math.min(this.quickPieces.length - 1, this.selectedIndex + 1);
      this.render();
      return;
    }
    
    // Enter to jump
    if (key === '\r' || key === '\n') {
      this.jumpToPiece(this.quickPieces[this.selectedIndex]);
      return;
    }
    
    // Type to filter/custom piece
    if (key === '/') {
      this.mode = 'piece-input';
      this.inputBuffer = '';
      this.render();
      return;
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
    try {
      // Use the panel opening logic from artery
      const targets = await this.getTargets();
      const workbenchTarget = targets.find(t => 
        t.type === 'page' && t.url && t.url.includes('workbench.html')
      );
      
      if (!workbenchTarget) {
        this.setStatus('VS Code workbench not found', 'error');
        return;
      }
      
      // Import WebSocket dynamically
      const { default: WebSocket } = await import('ws');
      
      let wsUrl = workbenchTarget.webSocketDebuggerUrl;
      if (CDP_HOST !== 'localhost') {
        wsUrl = wsUrl.replace('localhost', `${CDP_HOST}:9222`).replace(':9222:9222', ':9222');
      }
      
      const ws = new WebSocket(wsUrl);
      
      await new Promise((resolve, reject) => {
        ws.on('open', resolve);
        ws.on('error', reject);
      });
      
      // Click on the AC icon in sidebar or execute command
      ws.send(JSON.stringify({
        id: 1,
        method: 'Runtime.evaluate',
        params: {
          expression: `
            (function() {
              // Try to find and click AC panel button
              const buttons = document.querySelectorAll('.action-item');
              for (const btn of buttons) {
                if (btn.title?.includes('Aesthetic') || btn.getAttribute('aria-label')?.includes('Aesthetic')) {
                  btn.click();
                  return 'clicked';
                }
              }
              // Try command palette approach
              return 'not found';
            })()
          `,
          returnByValue: true
        }
      }));
      
      await new Promise(r => setTimeout(r, 1500));
      ws.close();
      
      // Reconnect after panel opens
      await this.connect();
      this.setStatus('Panel opened!', 'success');
    } catch (e) {
      this.setStatus(`Failed to open panel: ${e.message}`, 'error');
    }
  }

  enterPieceMode() {
    this.mode = 'pieces';
    this.selectedIndex = 0;
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
    this.setStatus('Running tests...', 'info');
    if (!this.connected) {
      this.setStatus('Not connected', 'error');
      return;
    }
    try {
      // Execute test command in AC
      const result = await this.client.eval(`
        if (window.api && window.api.test) {
          window.api.test();
          'Tests started';
        } else {
          'Test API not available';
        }
      `);
      this.setStatus(result, 'info');
    } catch (e) {
      this.setStatus(`Test error: ${e.message}`, 'error');
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
        this.renderPieces();
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
    const boxWidth = this.innerWidth;
    
    // Top border
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.topLeft}${BOX.horizontal.repeat(boxWidth - 2)}${BOX.topRight}${RESET}`);
    
    // Title line
    const titleLine = `${BOX.vertical} ${BG_MAGENTA}${FG_WHITE}${BOLD}${title}${RESET} ${status}${piece}`;
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
    
    // Title
    const pieceTitle = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET} ${BOLD}${FG_CYAN}Jump to Piece${RESET} ${DIM}(↑↓ to select, Enter to jump, / for custom)${RESET}`;
    const piecePadding = boxWidth - this.stripAnsi(pieceTitle).length - 1;
    this.writeLine(`${pieceTitle}${' '.repeat(Math.max(0, piecePadding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.teeRight}${BOX.lightH.repeat(boxWidth - 2)}${BOX.teeLeft}${RESET}`);
    
    // Pieces list
    const visibleCount = this.innerHeight - 8;
    const startIdx = Math.max(0, this.selectedIndex - Math.floor(visibleCount / 2));
    const endIdx = Math.min(this.quickPieces.length, startIdx + visibleCount);
    
    for (let i = startIdx; i < endIdx; i++) {
      const piece = this.quickPieces[i];
      const selected = i === this.selectedIndex;
      const prefix = selected ? `${BG_MAGENTA}${FG_WHITE} ▸ ` : '   ';
      const suffix = selected ? ` ${RESET}` : '';
      const label = selected ? `${BOLD}${piece}${RESET}` : piece;
      
      const line = `${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${prefix}${label}${suffix}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${' '.repeat(Math.max(0, padding))}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    // Fill remaining
    for (let i = endIdx - startIdx; i < visibleCount; i++) {
      this.writeLine(`${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}${' '.repeat(boxWidth - 2)}${FG_BRIGHT_MAGENTA}${BOX.vertical}${RESET}`);
    }
    
    this.renderFooter();
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
