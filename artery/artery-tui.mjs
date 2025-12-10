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
import os from 'os';
import { execSync, spawn } from 'child_process';

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
const BG_YELLOW = `${CSI}43m`;
const BG_BLUE = `${CSI}44m`;
const BG_MAGENTA = `${CSI}45m`;
const BG_CYAN = `${CSI}46m`;
const BG_WHITE = `${CSI}47m`;
const BG_BRIGHT_BLACK = `${CSI}100m`;
const BG_BRIGHT_RED = `${CSI}101m`;
const BG_BRIGHT_GREEN = `${CSI}102m`;
const BG_BRIGHT_YELLOW = `${CSI}103m`;
const BG_BRIGHT_BLUE = `${CSI}104m`;

// DOS-style color combos (defaults - see getThemeColors() for dynamic versions)
const DOS_TITLE = `${BG_BLUE}${FG_WHITE}${BOLD}`;
const DOS_MENU = `${BG_BLUE}${FG_BRIGHT_CYAN}`;
const DOS_HIGHLIGHT = `${BG_CYAN}${FG_BLACK}`;
const DOS_BORDER = `${BG_BLUE}${FG_BRIGHT_CYAN}`;
const DOS_STATUS = `${BG_BLUE}${FG_BRIGHT_YELLOW}`;

// ASCII Art title - AESTHETIC COMPUTER
const ARTERY_ASCII = [
  'AESTHETIC  COMPUTER',
];

// Baby block colors - background colors with white/black text
const BLOCK_COLORS = [
  { bg: '\x1b[41m', fg: '\x1b[97m' },  // red bg, white text
  { bg: '\x1b[43m', fg: '\x1b[30m' },  // yellow bg, black text  
  { bg: '\x1b[42m', fg: '\x1b[97m' },  // green bg, white text
  { bg: '\x1b[46m', fg: '\x1b[30m' },  // cyan bg, black text
  { bg: '\x1b[44m', fg: '\x1b[97m' },  // blue bg, white text
  { bg: '\x1b[45m', fg: '\x1b[97m' },  // magenta bg, white text
  { bg: '\x1b[47m', fg: '\x1b[30m' },  // white bg, black text
  { bg: '\x1b[101m', fg: '\x1b[97m' }, // bright red bg
  { bg: '\x1b[103m', fg: '\x1b[30m' }, // bright yellow bg
  { bg: '\x1b[104m', fg: '\x1b[97m' }, // bright blue bg
];

// Title animation - scattered colors
const BLOOD_FLOW_LENGTH = ARTERY_ASCII[0].length;
const BLOOD_PULSE_WIDTH = 4; // Width of the "blood pulse"

// Cursor control
const CURSOR_HIDE = `${CSI}?25l`;
const CURSOR_SHOW = `${CSI}?25h`;
const CURSOR_HOME = `${CSI}H`;
const CLEAR_SCREEN = `${CSI}2J`;
const CLEAR_LINE = `${CSI}2K`;

// Alternate screen buffer (prevents flicker and preserves scrollback)
const ALT_SCREEN_ON = `${CSI}?1049h`;
const ALT_SCREEN_OFF = `${CSI}?1049l`;

// Mouse support
const MOUSE_ENABLE = `${CSI}?1000h${CSI}?1006h`; // Enable mouse tracking + SGR extended mode
const MOUSE_DISABLE = `${CSI}?1000l${CSI}?1006l`; // Disable mouse tracking

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
  // Not in a container - use localhost
  if (process.env.REMOTE_CONTAINERS !== 'true' && process.env.CODESPACES !== 'true') {
    return 'localhost';
  }
  
  // In a container - need to reach the host
  // First check if HOST_IP is set (common in devcontainer setups)
  if (process.env.HOST_IP) {
    return process.env.HOST_IP;
  }
  
  // Try host.docker.internal (works on Docker Desktop for Mac/Windows)
  return 'host.docker.internal';
}

// Test if CDP is accessible at a given host:port
async function testCDPConnection(host, port) {
  return new Promise((resolve) => {
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
}

// Try to start SSH tunnel for CDP port forwarding
async function tryStartSSHTunnel() {
  // Check if tunnel already exists
  try {
    const existing = execSync('pgrep -f "ssh.*9333.*host.docker.internal" 2>/dev/null', { encoding: 'utf8' });
    if (existing.trim()) return true; // Already running
  } catch (e) {
    // No existing tunnel
  }
  
  // Try to start the tunnel
  return new Promise((resolve) => {
    const tunnel = spawn('ssh', [
      '-f', '-N',
      '-o', 'StrictHostKeyChecking=no',
      '-o', 'BatchMode=yes',
      '-o', 'ConnectTimeout=5',
      '-L', '9333:127.0.0.1:9333',
      'me@host.docker.internal'
    ], {
      detached: true,
      stdio: 'ignore'
    });
    
    tunnel.on('error', () => resolve(false));
    tunnel.unref();
    
    // Give it a moment to establish
    setTimeout(() => resolve(true), 300);
  });
}

// Try multiple hosts to find CDP - returns first working one
// Returns { host, port } object
async function findWorkingCDPHost() {
  const candidates = [];
  
  // Not in a container - only try localhost
  if (process.env.REMOTE_CONTAINERS !== 'true' && process.env.CODESPACES !== 'true') {
    // Try multiple ports in case 9222 is taken
    for (const port of [9333, 9222]) {
      try {
        const works = await testCDPConnection('localhost', port);
        if (works) return { host: 'localhost', port };
      } catch (e) {}
    }
    return { host: 'localhost', port: 9333 };
  }
  
  // In a container - first try localhost:9333 (SSH tunnel)
  // If that fails, try to start the tunnel
  const tunnelWorks = await testCDPConnection('localhost', 9333);
  if (tunnelWorks) {
    return { host: 'localhost', port: 9333 };
  }
  
  // Try to start SSH tunnel
  const tunnelStarted = await tryStartSSHTunnel();
  if (tunnelStarted) {
    // Give it a moment then test again
    await new Promise(r => setTimeout(r, 500));
    const tunnelNowWorks = await testCDPConnection('localhost', 9333);
    if (tunnelNowWorks) {
      return { host: 'localhost', port: 9333 };
    }
  }
  
  // In a container - try multiple host:port combinations
  // Order matters: try most reliable options first
  candidates.push({ host: 'host.docker.internal', port: 9333 }); // Windows with new port
  candidates.push({ host: 'host.docker.internal', port: 9222 }); // Mac/Windows Docker Desktop
  candidates.push({ host: 'localhost', port: 9222 }); // VS Code might forward the port
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
      const works = await testCDPConnection(host, port);
      if (works) {
        return { host, port };
      }
    } catch (e) {
      // Continue to next candidate
    }
  }
  
  // Return the first candidate as fallback (will fail with better error)
  return candidates[0] || { host: 'localhost', port: 9333 };
}

let CDP_HOST = getCDPHost(); // Will be updated dynamically
let CDP_PORT = 9222; // Will be updated dynamically

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
    this.localStatusMessage = ''; // Transient status message for local server
    this.serverPollInterval = null; // Polling interval for server status
    
    // Test running state (defers live updates)
    this.testRunning = false;
    this.pendingRender = false;
    
    // Frame buffer for double-buffering (prevents flicker)
    this.frameBuffer = [];
    this.forceFullRedraw = true; // First render is always full
    this.resizeTimeout = null; // For debouncing resize
    
    // Detect eat terminal (Emacs Application Terminal)
    this.isEatTerminal = process.env.TERM_PROGRAM === 'eat' || 
                         (process.env.INSIDE_EMACS && process.env.INSIDE_EMACS.includes('eat'));
    
    // Screen margins
    this.marginX = 2; // Left/right margin
    this.marginY = 1; // Top/bottom margin
    
    // Menu items
    this.menuItems = [
      { key: 'p', label: 'Toggle Panel', desc: 'Toggle AC sidebar in VS Code', action: () => this.togglePanel() },
      { key: 'k', label: 'KidLisp.com', desc: 'Open KidLisp editor window', action: () => this.openKidLisp() },
      { key: 'e', label: 'Emacs', desc: 'Execute elisp in Emacs', action: () => this.enterEmacsMode() },
      { key: 'j', label: 'Jump to Piece', desc: 'Navigate to a piece', action: () => this.enterPieceMode() },
      { key: 'c', label: 'Current Piece', desc: 'Show current piece', action: () => this.showCurrent() },
      { key: 'r', label: 'REPL Mode', desc: 'Interactive JavaScript console', action: () => this.enterReplMode() },
      { key: 't', label: 'Run Tests', desc: 'Execute test suite', action: () => this.runTests() },
      { key: 'l', label: 'View Logs', desc: 'AC console output', action: () => this.enterLogsMode() },
      { key: 'z', label: 'Keeps (Tezos)', desc: 'Tezos FA2 NFT minting', action: () => this.enterKeepsMode() },
      { key: 'a', label: 'Activate Audio', desc: 'Click to enable audio', action: () => this.activateAudio() },
      { key: 'w', label: 'WebGPU Perf', desc: 'Monitor WebGPU performance', action: () => this.webgpuPerf() },
      { key: 'x', label: 'Reconnect', desc: 'Reconnect to AC', action: () => this.reconnect() },
      { key: 'q', label: 'Quit', desc: 'Exit Artery TUI', action: () => this.quit() },
    ];
    
    // Keeps (Tezos) state
    this.keepsMenu = [
      { key: 'd', label: 'Deploy Contract', desc: 'Deploy FA2 to Ghostnet' },
      { key: 's', label: 'Status', desc: 'Check contract status' },
      { key: 'b', label: 'Balance', desc: 'Check wallet balance' },
      { key: 'u', label: 'Upload to IPFS', desc: 'Upload bundle to Pinata' },
      { key: 'm', label: 'Mint Token', desc: 'Mint a new keep' },
      { key: 't', label: 'List Tokens', desc: 'Show all minted tokens' },
    ];
    this.keepsSelectedIndex = 0;
    this.keepsOutput = '';
    this.keepsRunning = false;
    this.keepsPieceInput = '';
    this.keepsSubMode = 'menu'; // 'menu', 'piece-input', 'running'
    
    // Site monitoring state
    this.siteProcess = null;
    this.siteLogs = [];
    this.maxSiteLogs = 200;
    this.siteStatus = 'stopped'; // 'stopped', 'starting', 'running', 'error'
    this.siteStartTime = null;
    
    // Blood flow animation state
    this.bloodPosition = 0; // Current position of blood pulse (0 to BLOOD_FLOW_LENGTH)
    this.bloodAnimInterval = null;
    this.networkActivity = 0; // Activity level 0-10 (affects speed/intensity)
    this.lastNetworkTime = 0; // Last time we saw network activity
    
    // Load pieces from disk directory
    this.loadPieces();
    
    // CDP tunnel status
    this.cdpStatus = 'unknown'; // 'online', 'offline', 'unknown'
    this.cdpHost = null;
    this.cdpPort = null;
    
    // Detect host environment
    this.hostInfo = this.detectHostInfo();
  }
  
  // Get dynamic theme colors based on connectivity status
  // Returns { bg, border, fg } for current state
  getThemeColors() {
    // Priority: AC connected > local server > production server > nothing
    if (this.connected) {
      // Fully connected - green tint
      return {
        bg: BG_GREEN,
        border: `${BG_GREEN}${FG_WHITE}`,
        fill: BG_GREEN,
        text: FG_WHITE,
        accent: FG_BRIGHT_YELLOW
      };
    } else if (this.serverStatus.local === true) {
      // Local server up but AC not connected - cyan/blue (ready state)
      return {
        bg: BG_CYAN,
        border: `${BG_CYAN}${FG_BLACK}`,
        fill: BG_CYAN,
        text: FG_BLACK,
        accent: FG_BLUE
      };
    } else if (this.serverStatus.production === true) {
      // Only production available - magenta (remote state)
      return {
        bg: BG_MAGENTA,
        border: `${BG_MAGENTA}${FG_WHITE}`,
        fill: BG_MAGENTA,
        text: FG_WHITE,
        accent: FG_BRIGHT_YELLOW
      };
    } else if (this.serverStatus.local === false && this.serverStatus.production === false) {
      // Nothing available - red (error state)
      return {
        bg: BG_RED,
        border: `${BG_RED}${FG_WHITE}`,
        fill: BG_RED,
        text: FG_WHITE,
        accent: FG_BRIGHT_YELLOW
      };
    } else {
      // Unknown/checking - default blue
      return {
        bg: BG_BLUE,
        border: `${BG_BLUE}${FG_BRIGHT_CYAN}`,
        fill: BG_BLUE,
        text: FG_BRIGHT_CYAN,
        accent: FG_BRIGHT_YELLOW
      };
    }
  }
  
  detectHostInfo() {
    const info = {
      inContainer: process.env.REMOTE_CONTAINERS === 'true' || process.env.CODESPACES === 'true',
      containerOS: os.platform(),
      containerDistro: '',
      hostOS: null,
      hostLabel: null,
      hostEmoji: '🖥️',
      hostUser: process.env.USER || 'unknown'
    };
    
    // Get container distro
    try {
      const osRelease = fs.readFileSync('/etc/os-release', 'utf8');
      const nameMatch = osRelease.match(/^PRETTY_NAME="?([^"\n]+)"?/m);
      if (nameMatch) info.containerDistro = nameMatch[1];
    } catch (e) {}
    
    // Try to detect host OS via SSH
    if (info.inContainer) {
      try {
        // Use stdin: 'ignore' to prevent SSH from stealing terminal input if it prompts for password
        const hostOS = execSync('ssh -o ConnectTimeout=2 -o StrictHostKeyChecking=no -o BatchMode=yes me@host.docker.internal "echo %OS% 2>nul || uname -s" 2>/dev/null', { encoding: 'utf8', timeout: 3000, stdio: ['ignore', 'pipe', 'pipe'] }).trim();
        if (hostOS.includes('Windows')) {
          info.hostOS = 'Windows';
          info.hostEmoji = '🪟';
          info.hostLabel = 'Windows Tower';
        } else if (hostOS === 'Darwin') {
          info.hostOS = 'macOS';
          info.hostEmoji = '🍎';
          info.hostLabel = 'MacBook';
        } else if (hostOS === 'Linux') {
          info.hostOS = 'Linux';
          info.hostEmoji = '🐧';
          info.hostLabel = 'Linux Host';
        } else {
          info.hostOS = hostOS || 'Unknown';
        }
      } catch (e) {
        // SSH failed - try alternative detection methods
        // Check for VS Code remote container environment variables
        const remoteContainersDir = process.env.REMOTE_CONTAINERS_IPC;
        
        // Try to detect host from VS Code's remote-containers extension
        // On macOS Docker Desktop, host.docker.internal resolves via special gateway
        try {
          const hostResult = execSync('getent hosts host.docker.internal 2>/dev/null', { encoding: 'utf8', timeout: 1000, stdio: ['ignore', 'pipe', 'pipe'] }).trim();
          if (hostResult) {
            // host.docker.internal exists - this is typically macOS or Windows Docker Desktop
            // Check if it's IPv6 (fdc4:...) which is common on macOS
            if (hostResult.includes('fdc4:') || hostResult.includes('fd00:')) {
              // Likely macOS with Docker Desktop
              info.hostOS = 'macOS';
              info.hostEmoji = '🍎';
              info.hostLabel = 'MacBook';
            } else if (hostResult.includes('192.168.')) {
              // Could be Windows or macOS - default to showing generic
              info.hostLabel = 'Host';
            }
          }
        } catch (e2) {}
        
        // Final fallback: check HOST_IP env
        if (!info.hostOS && process.env.HOST_IP) {
          info.hostLabel = `Host @ ${process.env.HOST_IP}`;
        }
      }
    }
    
    // Try to load cute labels from vault
    try {
      const machinesPath = path.join(process.cwd(), 'aesthetic-computer-vault/machines.json');
      if (fs.existsSync(machinesPath)) {
        const machines = JSON.parse(fs.readFileSync(machinesPath, 'utf8'));
        // Find matching machine based on OS
        if (info.hostOS && machines.detection) {
          const osKey = info.hostOS.toLowerCase();
          if (osKey.includes('windows') && machines.detection.windows) {
            const machineId = machines.detection.windows.defaultMachine;
            if (machines.machines[machineId]) {
              const m = machines.machines[machineId];
              info.hostLabel = m.label;
              info.hostEmoji = m.emoji;
            }
          } else if (osKey.includes('darwin') || osKey.includes('macos')) {
            const machineId = machines.detection.darwin?.defaultMachine;
            if (machineId && machines.machines[machineId]) {
              const m = machines.machines[machineId];
              info.hostLabel = m.label;
              info.hostEmoji = m.emoji;
            }
          }
        }
      }
    } catch (e) {}
    
    return info;
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
    
    // Handle resize with debouncing to prevent jank
    process.stdout.on('resize', () => {
      // Clear any pending resize
      if (this.resizeTimeout) {
        clearTimeout(this.resizeTimeout);
      }
      // Debounce resize events - wait for them to settle
      this.resizeTimeout = setTimeout(() => {
        this.width = process.stdout.columns || 80;
        this.height = process.stdout.rows || 24;
        this.forceFullRedraw = true; // Force complete redraw on resize
        this.lastRenderedBuffer = []; // Clear buffer to force full redraw
        this.render();
      }, 50); // 50ms debounce - fast enough to feel responsive
    });
    
    // Handle input
    process.stdin.on('data', (key) => this.handleInput(key));
    
    // Initial render - use alternate screen buffer to prevent flicker
    this.write(ALT_SCREEN_ON); // Switch to alternate screen (preserves scrollback)
    this.write(CURSOR_HIDE);
    this.write(MOUSE_ENABLE); // Enable mouse tracking
    
    // Initialize UTC clock
    this.utcTime = this.getUTCTimeString();
    
    // Start polling local server status
    this.startServerPolling();
    
    // Start blood flow animation (also updates clock)
    this.startBloodAnimation();
    
    // Try to connect
    await this.connect();
    
    // Main render
    this.render();
  }
  
  startBloodAnimation() {
    // In eat terminal, use slower animation rate to reduce rendering issues
    const animInterval = this.isEatTerminal ? 150 : 80; // ~7fps for eat, ~12fps normal
    
    // Animate blood flow - speed varies with network activity
    this.bloodAnimInterval = setInterval(() => {
      // Skip animation entirely if a resize is pending
      if (this.resizeTimeout) return;
      
      // Decay network activity over time
      const now = Date.now();
      if (now - this.lastNetworkTime > 500) {
        this.networkActivity = Math.max(0, this.networkActivity - 0.5);
      }
      
      // Move blood pulse - faster with more activity
      const speed = 0.5 + (this.networkActivity * 0.3);
      this.bloodPosition = (this.bloodPosition + speed) % (BLOOD_FLOW_LENGTH + BLOOD_PULSE_WIDTH * 2);
      
      // Update UTC clock
      this.utcTime = this.getUTCTimeString();
      
      // Only re-render header area if in menu mode to show animation
      // In eat terminal, do full render instead of partial to avoid artifacts
      if (this.mode === 'menu' && !this.testRunning) {
        if (this.isEatTerminal) {
          this.render(); // Full render in eat terminal
        } else {
          this.renderHeaderOnly();
        }
      }
    }, animInterval);
  }
  
  // Trigger network activity pulse
  pulseNetwork(intensity = 1) {
    this.networkActivity = Math.min(10, this.networkActivity + intensity);
    this.lastNetworkTime = Date.now();
  }
  
  // Render just the header without full screen clear (for animation)
  renderHeaderOnly() {
    // Skip partial render if resize is pending - will do full render after
    if (this.resizeTimeout) return;
    if (this.width < 80) return; // Skip animation in compact mode
    
    const boxWidth = this.innerWidth;
    const artY = this.marginY + 2; // Position of ASCII art lines
    
    for (let lineIdx = 0; lineIdx < ARTERY_ASCII.length; lineIdx++) {
      const artLine = ARTERY_ASCII[lineIdx];
      const artPadding = Math.floor((boxWidth - artLine.length - 4) / 2);
      
      // Build the line with blood animation
      let coloredLine = '';
      for (let i = 0; i < artLine.length; i++) {
        const char = artLine[i];
        const globalPos = i;
        
        // Calculate distance from blood pulse center
        const pulseCenter = this.bloodPosition - BLOOD_PULSE_WIDTH;
        const dist = Math.abs(globalPos - pulseCenter);
        
        // Always show subtle pulse, more intense with network activity
        const baseActivity = 0.2;
        const effectiveActivity = baseActivity + (this.networkActivity * 0.1);
        
        // Color based on distance from pulse
        if (char !== ' ') {
          if (dist < BLOOD_PULSE_WIDTH / 2 && effectiveActivity > 0.3) {
            // Core of pulse - bright red
            coloredLine += `${FG_BRIGHT_RED}${char}`;
          } else if (dist < BLOOD_PULSE_WIDTH && effectiveActivity > 0.2) {
            // Edge of pulse - dim red
            coloredLine += `${FG_RED}${char}`;
          } else if (dist < BLOOD_PULSE_WIDTH * 1.5 && effectiveActivity > 0.1) {
            // Fading edge - magenta tint
            coloredLine += `${FG_MAGENTA}${char}`;
          } else {
            // Normal cyan
            coloredLine += `${FG_BRIGHT_CYAN}${char}`;
          }
        } else {
          coloredLine += char;
        }
      }
      
      // Position cursor and write the line
      const row = artY + lineIdx;
      const col = this.adaptiveMarginX + 2 + artPadding;
      this.write(`${moveTo(row, col)}${BG_BLUE}${coloredLine}${RESET}`);
    }
  }

  startServerPolling() {
    // Poll every 2 seconds for server status
    this.serverPollInterval = setInterval(async () => {
      const wasUp = this.serverStatus.local;
      await this.checkLocalServerWithDetails();
      
      // Pulse on server status change
      if (wasUp !== this.serverStatus.local) {
        this.pulseNetwork(2);
      }
      
      // Only re-render if status changed or we have a new message
      if (wasUp !== this.serverStatus.local || this.localStatusMessage) {
        this.render();
      }
    }, 2000);
  }

  async checkLocalServerWithDetails() {
    return new Promise((resolve) => {
      const timeout = setTimeout(() => {
        this.serverStatus.local = false;
        this.localStatusMessage = 'waiting...';
        resolve(false);
      }, 2000);
      
      const options = {
        hostname: 'localhost',
        port: 8888,
        path: '/',
        method: 'GET',
        rejectUnauthorized: false,
        timeout: 2000
      };
      
      const req = https.request(options, (res) => {
        clearTimeout(timeout);
        if (res.statusCode < 500) {
          this.serverStatus.local = true;
          this.localStatusMessage = ''; // Clear message when ready
          resolve(true);
        } else {
          this.serverStatus.local = false;
          this.localStatusMessage = `HTTP ${res.statusCode}`;
          resolve(false);
        }
      });
      
      req.on('error', (err) => {
        clearTimeout(timeout);
        this.serverStatus.local = false;
        // Parse error for helpful status
        if (err.code === 'ECONNREFUSED') {
          this.localStatusMessage = 'offline';
        } else if (err.code === 'ECONNRESET') {
          this.localStatusMessage = 'interrupted';
        } else if (err.code === 'ETIMEDOUT') {
          this.localStatusMessage = 'timeout';
        } else {
          this.localStatusMessage = err.code || 'error';
        }
        resolve(false);
      });
      
      req.on('timeout', () => {
        clearTimeout(timeout);
        req.destroy();
        this.serverStatus.local = false;
        this.localStatusMessage = 'slow...';
        resolve(false);
      });
      
      req.end();
    });
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
      this.pulseNetwork(5); // Big pulse on connect!
      
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
      this.pulseNetwork(3); // Pulse on connection failure
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
    // Update CDP host/port dynamically
    const cdpInfo = await findWorkingCDPHost();
    CDP_HOST = cdpInfo.host;
    CDP_PORT = cdpInfo.port;
    
    // Store CDP status for display
    this.cdpHost = CDP_HOST;
    this.cdpPort = CDP_PORT;
    
    return new Promise((resolve, reject) => {
      http.get({
        hostname: CDP_HOST,
        port: CDP_PORT,
        path: '/json',
        headers: { 'Host': 'localhost' }
      }, (res) => {
        let data = '';
        res.on('data', (chunk) => data += chunk);
        res.on('end', () => {
          try { 
            this.cdpStatus = 'online';
            resolve(JSON.parse(data)); 
          }
          catch (e) { 
            this.cdpStatus = 'offline';
            reject(new Error('Parse failed')); 
          }
        });
      }).on('error', (e) => {
        this.cdpStatus = 'offline';
        reject(e);
      });
    });
  }

  addLog(text, level = 'log') {
    const timestamp = new Date().toLocaleTimeString('en-US', { hour12: false });
    this.logs.unshift({ timestamp, text, level });
    if (this.logs.length > this.maxLogs) {
      this.logs.pop();
    }
    
    // Pulse blood animation on log activity
    this.pulseNetwork(level === 'error' ? 3 : 1);
    
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
    
    // Parse mouse events (SGR extended mode: \x1b[<button;col;rowM or m)
    const mouseMatch = key.match(/\x1b\[<(\d+);(\d+);(\d+)([Mm])/);
    if (mouseMatch) {
      const button = parseInt(mouseMatch[1]);
      const col = parseInt(mouseMatch[2]);
      const row = parseInt(mouseMatch[3]);
      const release = mouseMatch[4] === 'm';
      
      // Only handle left click release (button 0)
      if (button === 0 && release) {
        this.handleMouseClick(col, row);
      }
      return;
    }
    
    // Escape to go back to menu (but ignore if part of mouse/arrow sequence)
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
      case 'emacs':
        this.handleEmacsInput(key);
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
      case 'site':
        this.handleSiteInput(key);
        break;
      case 'keeps':
        this.handleKeepsInput(key);
        break;
    }
  }

  // Handle mouse click events
  handleMouseClick(col, row) {
    const compact = this.width < 80;
    const headerLines = compact ? 4 : 6; // Header height varies by mode
    const marginTop = this.marginY;
    
    switch (this.mode) {
      case 'menu': {
        // Calculate which menu item was clicked
        // Menu items start after header (marginY + header lines)
        const menuStartRow = marginTop + headerLines;
        const clickedItemIndex = row - menuStartRow;
        
        if (clickedItemIndex >= 0 && clickedItemIndex < this.menuItems.length) {
          this.selectedIndex = clickedItemIndex;
          this.menuItems[clickedItemIndex].action();
        }
        break;
      }
      
      case 'pieces': {
        // Pieces list starts after header + search box
        const listStartRow = marginTop + headerLines + 1;
        const visibleCount = Math.max(1, this.innerHeight - 10);
        const startIdx = Math.max(0, this.selectedIndex - Math.floor(visibleCount / 2));
        const clickedOffset = row - listStartRow;
        
        if (clickedOffset >= 0 && clickedOffset < visibleCount) {
          const clickedItemIndex = startIdx + clickedOffset;
          if (clickedItemIndex < this.filteredPieces.length) {
            this.selectedIndex = clickedItemIndex;
            this.render();
            // Double-click effect: if same item, jump to it
            // (for now, single click just selects)
          }
        }
        break;
      }
      
      case 'tests': {
        // Tests list starts after header
        const listStartRow = marginTop + headerLines;
        const clickedOffset = row - listStartRow;
        const tests = this.testFiles || [];
        
        if (clickedOffset >= 0 && clickedOffset < tests.length) {
          const test = tests[clickedOffset];
          if (test && !test.isSeparator) {
            this.selectedIndex = clickedOffset;
            // Execute the test on click
            if (test.params) {
              this.currentTest = test;
              this.testParams = { ...test.defaults };
              this.paramIndex = 0;
              this.inputBuffer = '';
              this.mode = 'test-params';
            } else {
              this.executeTest(test.file, '', test.isArtery || false);
            }
          }
        }
        break;
      }
      
      case 'test-params': {
        const params = this.currentTest?.params || [];
        const listStartRow = marginTop + headerLines;
        const clickedOffset = row - listStartRow;
        
        if (clickedOffset >= 0 && clickedOffset < params.length) {
          this.paramIndex = clickedOffset;
          this.inputBuffer = String(this.testParams[params[clickedOffset]?.key] || '');
          this.render();
        } else if (clickedOffset === params.length + 1) {
          // Clicked "RUN" button
          let args;
          if (this.currentTest.formatArgs) {
            args = this.currentTest.formatArgs(params, this.testParams);
          } else {
            args = params.map(p => this.testParams[p.key] || p.default).join(' ');
          }
          this.executeTest(this.currentTest.file, args, this.currentTest.isArtery || false);
        }
        break;
      }
      
      default:
        // Other modes: click anywhere to go back to menu
        this.mode = 'menu';
        this.inputBuffer = '';
        this.render();
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

  handleEmacsInput(key) {
    // Escape to go back
    if (key === '\u001b' && this.emacsSubMode === 'menu') {
      this.mode = 'menu';
      this.render();
      return;
    }
    
    if (key === '\u001b' && (this.emacsSubMode === 'eval' || this.emacsSubMode === 'buffers')) {
      this.emacsSubMode = 'menu';
      this.render();
      return;
    }
    
    if (this.emacsSubMode === 'menu') {
      // Menu shortcuts
      if (key === 'e' || key === 'E') {
        this.emacsSubMode = 'eval';
        this.emacsInput = '';
        this.render();
        return;
      }
      if (key === 'b' || key === 'B') {
        this.emacsListBuffers();
        return;
      }
      if (key === 'v' || key === 'V') {
        this.emacsEval('(emacs-version)');
        return;
      }
    }
    
    if (this.emacsSubMode === 'eval') {
      // Enter to execute
      if (key === '\r' || key === '\n') {
        if (this.emacsInput.trim()) {
          this.emacsEval(this.emacsInput);
          this.emacsInput = '';
        }
        return;
      }
      
      // Backspace
      if (key === '\u007f' || key === '\b') {
        this.emacsInput = this.emacsInput.slice(0, -1);
        this.render();
        return;
      }
      
      // Regular character
      if (key.length === 1 && key.charCodeAt(0) >= 32) {
        this.emacsInput += key;
        this.render();
      }
      return;
    }
    
    if (this.emacsSubMode === 'buffers') {
      // Arrow keys to select buffer
      if (key === '\u001b[A') { // Up
        this.emacsSelectedBuffer = Math.max(0, this.emacsSelectedBuffer - 1);
        this.render();
        return;
      }
      if (key === '\u001b[B') { // Down
        this.emacsSelectedBuffer = Math.min(this.emacsBuffers.length - 1, this.emacsSelectedBuffer + 1);
        this.render();
        return;
      }
      
      // Enter to switch to buffer
      if (key === '\r' || key === '\n') {
        const buffer = this.emacsBuffers[this.emacsSelectedBuffer];
        if (buffer) {
          this.emacsSwitchBuffer(buffer);
        }
        return;
      }
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
      let newIndex = this.selectedIndex - 1;
      // Skip separators
      while (newIndex >= 0 && this.testFiles?.[newIndex]?.isSeparator) {
        newIndex--;
      }
      this.selectedIndex = Math.max(0, newIndex);
      this.render();
      return;
    }
    if (key === '\u001b[B') { // Down
      let newIndex = this.selectedIndex + 1;
      // Skip separators
      while (newIndex < (this.testFiles?.length || 0) && this.testFiles?.[newIndex]?.isSeparator) {
        newIndex++;
      }
      this.selectedIndex = Math.min((this.testFiles?.length || 1) - 1, newIndex);
      this.render();
      return;
    }
    
    // Enter to run test (or open params for parametric tests)
    if (key === '\r' || key === '\n') {
      const test = this.testFiles?.[this.selectedIndex];
      if (test && !test.isSeparator) {
        if (test.params) {
          // Show parameter input mode
          this.currentTest = test;
          this.testParams = { ...test.defaults };
          this.paramIndex = 0;
          this.inputBuffer = '';
          this.mode = 'test-params';
          this.render();
        } else {
          this.executeTest(test.file, '', test.isArtery || false);
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
        let args;
        if (this.currentTest.formatArgs) {
          args = this.currentTest.formatArgs(params, this.testParams);
        } else {
          args = params.map(p => this.testParams[p.key] || p.default).join(' ');
        }
        this.executeTest(this.currentTest.file, args, this.currentTest.isArtery || false);
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
  async togglePanel() {
    this.setStatus('Toggling AC panel...', 'info');
    this.render();
    
    try {
      // Use the standalone panel toggler from Artery
      const result = await Artery.togglePanelStandalone();
      
      if (result.toggled) {
        if (result.nowExpanded) {
          // Wait a moment for panel to initialize
          await new Promise(r => setTimeout(r, 500));
          
          // Reconnect after panel opens
          await this.connect();
          
          if (this.connected) {
            this.setStatus('Panel opened and connected!', 'success');
          } else {
            this.setStatus('Panel opened - connecting...', 'info');
          }
        } else {
          this.setStatus('Panel closed', 'info');
        }
      } else {
        this.setStatus('Could not find AC panel', 'warn');
      }
    } catch (e) {
      this.setStatus(`Failed to toggle panel: ${e.message}`, 'error');
    }
    
    this.render();
  }

  // 🌈 Open KidLisp.com editor window
  async openKidLisp() {
    this.setStatus('Opening KidLisp.com editor...', 'info');
    this.render();
    
    try {
      await Artery.openKidLispWindow();
      this.setStatus('KidLisp.com editor opened!', 'success');
    } catch (e) {
      this.setStatus(`Failed to open KidLisp: ${e.message}`, 'error');
    }
    
    this.render();
  }

  // 🧠 Emacs Integration Mode
  async enterEmacsMode() {
    this.mode = 'emacs';
    this.emacsInput = '';
    this.emacsOutput = '';
    this.emacsBuffers = [];
    this.emacsSelectedBuffer = 0;
    this.emacsSubMode = 'menu'; // 'menu', 'eval', 'buffers'
    
    // Check if Emacs is running
    try {
      const version = await Artery.emacsVersion();
      this.emacsOutput = `Connected to ${version}`;
      this.setStatus('Emacs connected!', 'success');
    } catch (e) {
      this.emacsOutput = `Error: ${e.message}`;
      this.setStatus('Emacs not running or emacsclient failed', 'error');
    }
    
    this.render();
  }
  
  async emacsEval(code) {
    try {
      const result = await Artery.execEmacs(code);
      this.emacsOutput = result;
      this.setStatus('Elisp executed', 'success');
    } catch (e) {
      this.emacsOutput = `Error: ${e.message}`;
      this.setStatus('Elisp error', 'error');
    }
    this.render();
  }
  
  async emacsListBuffers() {
    try {
      const result = await Artery.execEmacs('(mapcar #\'buffer-name (buffer-list))');
      // Parse lisp list
      this.emacsBuffers = result.slice(1, -1).split(' ').map(s => s.replace(/^"|"$/g, ''));
      this.emacsSubMode = 'buffers';
      this.emacsSelectedBuffer = 0;
    } catch (e) {
      this.emacsOutput = `Error: ${e.message}`;
    }
    this.render();
  }
  
  async emacsSwitchBuffer(bufferName) {
    try {
      await Artery.emacsSwitchBuffer(bufferName);
      this.setStatus(`Switched to ${bufferName}`, 'success');
      this.emacsSubMode = 'menu';
    } catch (e) {
      this.setStatus(`Error: ${e.message}`, 'error');
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
    
    // Auto-discover test files from artery/ directory
    const arteryTests = await this.discoverArteryTests();
    
    // Available melodies from melodies.mjs
    const melodyOptions = [
      'twinkle', 'mary', 'old-macdonald', 'yankee-doodle', 'frere-jacques',
      'london-bridge', 'row-row-row', 'skip-to-my-lou', 'camptown-races',
      'oh-susanna', 'amazing-grace', 'auld-lang-syne', 'shenandoah',
      'home-on-the-range', 'red-river-valley', 'scarborough-fair',
      'greensleeves', 'when-the-saints', 'danny-boy'
    ];
    
    // Genre + style options for composition test
    const hiphopStyles = ['trap', 'boombap', 'lofi', '808', 'halftime', 'phonk', 'drill', 'gfunk'];
    const waltzStyles = ['classic', 'dark', 'dreamy', 'baroque', 'minimal', 'phonk', 'viennese', 'drill'];
    
    // Special configs for known tests (params, descriptions, etc.)
    const testConfigs = {
      'test-notepat.mjs': {
        name: 'composition',
        desc: '🎹 Full composition [waltz/hiphop]',
        params: [
          { key: 'genre', label: 'Genre', desc: '←→ to cycle', default: 'waltz', options: ['waltz', 'hiphop'] },
          { key: 'style', label: 'Style', desc: '←→ to cycle (depends on genre)', default: 'classic', options: [...waltzStyles, ...hiphopStyles] },
          { key: 'bars', label: 'Bars', desc: '←→ to adjust', default: '24', type: 'number', min: 8, max: 64, step: 4 },
          { key: 'bpm', label: 'BPM', desc: '←→ to adjust', default: '140', type: 'number', min: 80, max: 200, step: 10 },
          { key: 'scale', label: 'Scale', desc: '←→ to cycle', default: 'minor', options: ['minor', 'dorian', 'phrygian', 'harmonic', 'major'] },
          { key: 'room', label: 'Room', desc: '←→ toggle reverb', default: '', options: ['', 'room'] },
          { key: 'waves', label: 'Waves', desc: '←→ toggle wave changes', default: '', options: ['', 'waves'] },
        ],
        defaults: { genre: 'waltz', style: 'classic', bars: '24', bpm: '140', scale: 'minor', room: '', waves: '' },
        formatArgs: (params, values) => {
          const parts = [values.genre, values.style];
          if (values.bars !== '24') parts.push(`bars=${values.bars}`);
          if (values.bpm !== '140') parts.push(`bpm=${values.bpm}`);
          if (values.scale !== 'minor') parts.push(`scale=${values.scale}`);
          if (values.room) parts.push('room');
          if (values.waves) parts.push('waves');
          return parts.join(' ');
        }
      },
      'test-hiphop.mjs': {
        name: 'hiphop',
        desc: '🎤 Hip-hop beat generator',
      },
      'test-trapwaltz.mjs': {
        name: 'trapwaltz', 
        desc: '🩰 Trap waltz (3/4 + trap)',
      },
      'test-1v1-split.mjs': {
        name: '1v1-split',
        desc: '🎮 Split view for 1v1 dueling',
        // No params needed - just runs with default 1v1~1v1
      },
    };
    
    // Build test list from discovered files + legacy .vscode tests
    this.testFiles = [];
    
    // Add discovered artery tests (with configs if available)
    for (const file of arteryTests) {
      const config = testConfigs[file] || {};
      const baseName = file.replace('test-', '').replace('.mjs', '');
      this.testFiles.push({
        name: config.name || baseName,
        file: `artery/${file}`,
        desc: config.desc || `Artery test: ${baseName}`,
        isArtery: true,
        params: config.params,
        defaults: config.defaults,
        formatArgs: config.formatArgs,
      });
    }
    
    // Add separator
    this.testFiles.push({ name: '───────────', file: null, desc: 'Legacy .vscode tests', isSeparator: true });
    
    // Legacy .vscode/tests
    this.testFiles.push(
      { name: 'notepat-fuzz', file: 'test-notepat.mjs', desc: 'Notepat fuzzing test' },
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
    );
    this.selectedIndex = 0;
    this.setStatus('Select a test to run (Enter for params)', 'info');
    this.render();
  }
  
  // Auto-discover test-*.mjs files in artery/ directory
  async discoverArteryTests() {
    try {
      const arteryDir = path.dirname(new URL(import.meta.url).pathname);
      const files = fs.readdirSync(arteryDir);
      return files
        .filter(f => f.startsWith('test-') && f.endsWith('.mjs'))
        .sort();
    } catch (e) {
      return [];
    }
  }
  
  async executeTest(testFile, args = '', isArtery = false) {
    this.mode = 'logs';
    this.testRunning = true;
    this.pendingRender = false;
    const argsDisplay = args ? ` (${args})` : '';
    this.setStatus(`Running ${testFile}${argsDisplay}...`, 'info');
    this.render();
    
    try {
      const { spawn } = await import('child_process');
      // Use artery/ path for artery tests, otherwise .vscode/tests/
      const testPath = isArtery ? testFile : `.vscode/tests/${testFile}`;
      
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

  enterSiteMode() {
    this.mode = 'site';
    this.render();
  }

  handleSiteInput(key) {
    // 's' to start site
    if (key === 's' || key === 'S') {
      this.startSite();
      return;
    }
    // 'k' to kill/stop site
    if (key === 'k' || key === 'K') {
      this.stopSite();
      return;
    }
    // 'r' to restart
    if (key === 'r' || key === 'R') {
      this.restartSite();
      return;
    }
    // 'c' to clear logs
    if (key === 'c' || key === 'C') {
      this.siteLogs = [];
      this.render();
      return;
    }
  }

  async startSite() {
    if (this.siteProcess) {
      this.setStatus('Site already running', 'warn');
      return;
    }
    
    this.siteStatus = 'starting';
    this.siteStartTime = Date.now();
    this.siteLogs = [];
    this.addSiteLog('🚀 Starting ac-site...', 'info');
    this.render();
    
    try {
      const { spawn } = await import('child_process');
      
      // Determine if we're in codespaces
      const isCodespaces = !!process.env.CODESPACES;
      const npmScript = isCodespaces ? 'codespaces-dev' : 'local-dev';
      
      this.addSiteLog(`📦 Using npm run ${npmScript}`, 'info');
      
      // First kill any existing processes
      try {
        const { execSync } = await import('child_process');
        execSync('pkill -f "netlify dev" 2>/dev/null || true', { stdio: 'ignore' });
        execSync('pkill -f "esbuild" 2>/dev/null || true', { stdio: 'ignore' });
        execSync('npx kill-port 8880 8888 8889 8080 8000 8111 3333 3000 3001 2>/dev/null || true', { stdio: 'ignore' });
      } catch (e) {
        // Ignore cleanup errors
      }
      
      // Start netlify dev
      this.siteProcess = spawn('npm', ['run', npmScript], {
        cwd: path.join(process.cwd(), 'system'),
        stdio: ['ignore', 'pipe', 'pipe'],
        env: { ...process.env, DENO_LOG_LEVEL: 'info', DEBUG: '' }
      });
      
      this.siteProcess.stdout.on('data', (data) => {
        const lines = data.toString().split('\n').filter(l => l.trim());
        lines.forEach(line => {
          // Filter out noisy debug lines
          if (!line.includes('DEBUG RS') && !line.includes('[DEBUG]')) {
            this.addSiteLog(line, 'log');
            
            // Check for ready indicators
            if (line.includes('Server now ready') || line.includes('Local:') || line.includes('https://localhost:8888')) {
              this.siteStatus = 'running';
              const elapsed = ((Date.now() - this.siteStartTime) / 1000).toFixed(1);
              this.addSiteLog(`✅ Site ready in ${elapsed}s!`, 'success');
              this.setStatus(`ac-site running (started in ${elapsed}s)`, 'success');
            }
          }
        });
        this.render();
      });
      
      this.siteProcess.stderr.on('data', (data) => {
        const lines = data.toString().split('\n').filter(l => l.trim());
        lines.forEach(line => {
          if (!line.includes('DEBUG RS') && !line.includes('[DEBUG]')) {
            this.addSiteLog(line, 'error');
          }
        });
        this.render();
      });
      
      this.siteProcess.on('close', (code) => {
        this.siteProcess = null;
        if (code === 0) {
          this.siteStatus = 'stopped';
          this.addSiteLog('🛑 Site stopped normally', 'info');
        } else {
          this.siteStatus = 'error';
          this.addSiteLog(`❌ Site crashed with code ${code}`, 'error');
        }
        this.render();
      });
      
      this.siteProcess.on('error', (err) => {
        this.siteProcess = null;
        this.siteStatus = 'error';
        this.addSiteLog(`❌ Failed to start: ${err.message}`, 'error');
        this.render();
      });
      
    } catch (e) {
      this.siteStatus = 'error';
      this.addSiteLog(`❌ Error: ${e.message}`, 'error');
      this.render();
    }
  }

  async stopSite() {
    if (!this.siteProcess) {
      // Try to kill any orphan processes anyway
      try {
        const { execSync } = await import('child_process');
        execSync('pkill -f "netlify dev" 2>/dev/null || true', { stdio: 'ignore' });
        this.addSiteLog('🧹 Cleaned up orphan processes', 'info');
      } catch (e) {}
      this.siteStatus = 'stopped';
      this.setStatus('Site stopped', 'info');
      this.render();
      return;
    }
    
    this.addSiteLog('🛑 Stopping site...', 'info');
    this.siteProcess.kill('SIGTERM');
    
    // Force kill after 3 seconds if still running
    setTimeout(() => {
      if (this.siteProcess) {
        this.siteProcess.kill('SIGKILL');
      }
    }, 3000);
  }

  async restartSite() {
    this.addSiteLog('🔄 Restarting site...', 'info');
    await this.stopSite();
    await new Promise(r => setTimeout(r, 1000));
    await this.startSite();
  }

  addSiteLog(text, level = 'log') {
    const timestamp = new Date().toLocaleTimeString();
    this.siteLogs.push({ text, level, time: timestamp });
    if (this.siteLogs.length > this.maxSiteLogs) {
      this.siteLogs.shift();
    }
  }

  enterReplMode() {
    this.mode = 'repl';
    this.inputBuffer = '';
    this.unreadLogs = 0; // Clear unread since REPL also shows logs
    this.render();
  }

  // 🔮 Keeps (Tezos FA2) Mode
  enterKeepsMode() {
    this.mode = 'keeps';
    this.keepsSelectedIndex = 0;
    this.keepsOutput = 'Welcome to Keeps - Tezos FA2 NFT Minting\n\nSelect an action from the menu.';
    this.keepsRunning = false;
    this.keepsPieceInput = '';
    this.keepsSubMode = 'menu';
    this.render();
  }

  handleKeepsInput(key) {
    if (this.keepsRunning) {
      // While running, only ESC cancels (though we can't actually cancel)
      return;
    }
    
    if (this.keepsSubMode === 'piece-input') {
      // Handle piece name input
      if (key === '\r') {
        // Enter - submit
        const piece = this.keepsPieceInput.trim();
        if (piece) {
          this.keepsSubMode = 'running';
          this.executeKeepsAction(this.keepsMenu[this.keepsSelectedIndex].key, piece);
        }
        return;
      } else if (key === '\u007f' || key === '\b') {
        // Backspace
        this.keepsPieceInput = this.keepsPieceInput.slice(0, -1);
      } else if (key === '\u001b') {
        // Escape - back to menu
        this.keepsSubMode = 'menu';
        this.keepsPieceInput = '';
      } else if (key.length === 1 && key >= ' ') {
        this.keepsPieceInput += key;
      }
      this.render();
      return;
    }
    
    // Menu navigation
    if (key === '\u001b[A' || key === 'k') { // Up
      this.keepsSelectedIndex = Math.max(0, this.keepsSelectedIndex - 1);
    } else if (key === '\u001b[B' || key === 'j') { // Down
      this.keepsSelectedIndex = Math.min(this.keepsMenu.length - 1, this.keepsSelectedIndex + 1);
    } else if (key === '\r') { // Enter
      const action = this.keepsMenu[this.keepsSelectedIndex];
      if (action.key === 'u' || action.key === 'm') {
        // Upload and Mint need piece input
        this.keepsSubMode = 'piece-input';
        this.keepsPieceInput = this.currentPiece || '';
      } else {
        this.keepsSubMode = 'running';
        this.executeKeepsAction(action.key);
      }
    } else {
      // Direct key press
      const action = this.keepsMenu.find(m => m.key === key.toLowerCase());
      if (action) {
        this.keepsSelectedIndex = this.keepsMenu.indexOf(action);
        if (action.key === 'u' || action.key === 'm') {
          this.keepsSubMode = 'piece-input';
          this.keepsPieceInput = this.currentPiece || '';
        } else {
          this.keepsSubMode = 'running';
          this.executeKeepsAction(action.key);
        }
      }
    }
    this.render();
  }

  async executeKeepsAction(actionKey, piece = null) {
    this.keepsRunning = true;
    this.keepsOutput = '⏳ Running...\n';
    this.render();
    
    const keepsScript = path.join(process.cwd(), 'tezos/keeps.mjs');
    
    let cmd;
    switch (actionKey) {
      case 'd':
        cmd = `node "${keepsScript}" deploy`;
        break;
      case 's':
        cmd = `node "${keepsScript}" status`;
        break;
      case 'b':
        cmd = `node "${keepsScript}" balance`;
        break;
      case 'u':
        cmd = `node "${keepsScript}" upload "${piece}"`;
        break;
      case 'm':
        cmd = `node "${keepsScript}" mint "${piece}"`;
        break;
      case 't':
        cmd = `node "${keepsScript}" status`;
        break;
      default:
        this.keepsOutput = '❌ Unknown action';
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.render();
        return;
    }
    
    try {
      const result = execSync(cmd, { 
        encoding: 'utf8', 
        timeout: 120000,
        cwd: process.cwd(),
        maxBuffer: 10 * 1024 * 1024
      });
      this.keepsOutput = result;
      this.setStatus('Keeps operation completed', 'success');
    } catch (error) {
      if (error.stdout) {
        this.keepsOutput = error.stdout + '\n' + (error.stderr || '');
      } else {
        this.keepsOutput = `❌ Error: ${error.message}`;
      }
      this.setStatus('Keeps operation failed', 'error');
    }
    
    this.keepsRunning = false;
    this.keepsSubMode = 'menu';
    this.render();
  }

  renderKeeps() {
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Header
    this.writeLine(`${DOS_TITLE}${'═'.repeat(boxWidth)}${RESET}`);
    this.writeLine(`${DOS_TITLE}  🔮 KEEPS - Tezos FA2 NFT Minting${' '.repeat(Math.max(0, boxWidth - 35))}${RESET}`);
    this.writeLine(`${DOS_TITLE}${'═'.repeat(boxWidth)}${RESET}`);
    this.writeLine('');
    
    // Menu items
    for (let i = 0; i < this.keepsMenu.length; i++) {
      const item = this.keepsMenu[i];
      const selected = i === this.keepsSelectedIndex;
      const prefix = selected ? `${FG_BRIGHT_YELLOW}▶ ` : `${FG_CYAN}  `;
      const keyStyle = selected ? `${BG_CYAN}${FG_BLACK}` : `${FG_BRIGHT_MAGENTA}`;
      const labelStyle = selected ? `${FG_BRIGHT_YELLOW}${BOLD}` : `${FG_WHITE}`;
      const descStyle = `${DIM}${FG_CYAN}`;
      
      const line = `${prefix}${keyStyle}[${item.key}]${RESET}${BG_BLUE} ${labelStyle}${item.label}${RESET}${BG_BLUE} ${descStyle}${item.desc}${RESET}`;
      this.writeLine(line);
    }
    
    this.writeLine('');
    
    // Piece input if in that mode
    if (this.keepsSubMode === 'piece-input') {
      this.writeLine(`${FG_BRIGHT_YELLOW}Enter piece name: ${FG_WHITE}${this.keepsPieceInput}█${RESET}`);
      this.writeLine(`${DIM}(Current piece: ${this.currentPiece || 'none'})${RESET}`);
      this.writeLine('');
    }
    
    // Output box
    this.writeLine(`${FG_CYAN}${'─'.repeat(boxWidth)}${RESET}`);
    this.writeLine(`${FG_BRIGHT_CYAN}Output:${RESET}`);
    this.writeLine('');
    
    // Split output into lines and display
    const outputLines = (this.keepsOutput || '').split('\n');
    const maxOutputLines = Math.max(5, this.innerHeight - 20);
    const startLine = Math.max(0, outputLines.length - maxOutputLines);
    
    for (let i = startLine; i < outputLines.length; i++) {
      let line = outputLines[i];
      // Color code output
      if (line.includes('✅') || line.includes('✓')) {
        line = `${FG_BRIGHT_GREEN}${line}${RESET}`;
      } else if (line.includes('❌') || line.includes('Error')) {
        line = `${FG_BRIGHT_RED}${line}${RESET}`;
      } else if (line.includes('⏳') || line.includes('...')) {
        line = `${FG_BRIGHT_YELLOW}${line}${RESET}`;
      } else if (line.includes('📍') || line.includes('🔗') || line.includes('💰')) {
        line = `${FG_BRIGHT_CYAN}${line}${RESET}`;
      }
      // Truncate long lines
      if (this.stripAnsi(line).length > boxWidth - 2) {
        line = line.slice(0, boxWidth - 5) + '...';
      }
      this.writeLine(`  ${line}`);
    }
    
    // Status bar
    this.writeLine('');
    this.writeLine(`${FG_CYAN}${'─'.repeat(boxWidth)}${RESET}`);
    const statusText = this.keepsRunning ? `${FG_BRIGHT_YELLOW}Running...` : `${FG_GREEN}Ready`;
    this.writeLine(`${statusText}${RESET}${BG_BLUE}  ${DIM}ESC=Back  ↑↓=Navigate  Enter=Select${RESET}`);
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
    // Clean up polling and animation
    if (this.serverPollInterval) {
      clearInterval(this.serverPollInterval);
    }
    if (this.bloodAnimInterval) {
      clearInterval(this.bloodAnimInterval);
    }
    this.write(MOUSE_DISABLE); // Disable mouse tracking
    this.write(CURSOR_SHOW);
    this.write(ALT_SCREEN_OFF); // Switch back to main screen (restores scrollback)
    if (this.client) {
      try { this.client.close(); } catch (e) {}
    }
    process.exit(0);
  }

  // Helper to get inner width (accounting for margins)
  get innerWidth() {
    // Minimum width of 40, use full width minus margins
    return Math.max(40, this.width - (this.marginX * 2));
  }
  
  // Helper to get inner height (accounting for margins)  
  get innerHeight() {
    return Math.max(10, this.height - (this.marginY * 2));
  }
  
  // Get adaptive margin based on terminal width
  get adaptiveMarginX() {
    if (this.width < 60) return 0;
    if (this.width < 80) return 1;
    return this.marginX;
  }
  
  // Write a line with margins and full background fill (to buffer)
  writeLine(content) {
    const margin = ' '.repeat(this.adaptiveMarginX);
    const lineContent = `${BG_BLUE}${margin}${RESET}${content}`;
    // Fill rest of line with background
    const contentLen = this.stripAnsi(lineContent).length;
    const fill = this.width - contentLen;
    const line = `${lineContent}${fill > 0 ? BG_BLUE + ' '.repeat(Math.max(0, fill)) + RESET : ''}`;
    this.frameBuffer.push(line);
  }

  // Rendering - uses double buffering to prevent flicker
  render() {
    // Build frame in buffer first
    this.frameBuffer = [];
    
    // Top margin with background
    for (let i = 0; i < this.marginY; i++) {
      this.frameBuffer.push(`${BG_BLUE}${' '.repeat(Math.max(1, this.width))}${RESET}`);
    }
    
    switch (this.mode) {
      case 'menu':
        this.renderMenu();
        break;
      case 'repl':
        this.renderRepl();
        break;
      case 'emacs':
        this.renderEmacs();
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
      case 'site':
        this.renderSite();
        break;
      case 'keeps':
        this.renderKeeps();
        break;
    }
    
    // Fill remaining lines with blue background
    while (this.frameBuffer.length < this.height) {
      this.frameBuffer.push(`${BG_BLUE}${' '.repeat(Math.max(1, this.width))}${RESET}`);
    }
    
    // Now output the entire frame at once (minimizes flicker)
    // On resize, do a hard clear first
    if (this.forceFullRedraw) {
      this.write(CLEAR_SCREEN);
      this.forceFullRedraw = false;
    }
    
    // Move to home and output all lines
    this.write(CURSOR_HOME);
    this.write(this.frameBuffer.join('\n'));
  }

  // Helper to render a bordered line with proper padding
  renderBoxLine(content, boxWidth, theme) {
    const stripped = this.stripAnsi(content);
    // Count emojis for visual width adjustment
    const emojiCount = (stripped.match(/[\u{1F300}-\u{1F9FF}]|[\u{2600}-\u{26FF}]/gu) || []).length;
    const visualLen = stripped.length + emojiCount;
    const padding = boxWidth - 2 - visualLen;
    this.writeLine(`${theme.border}║${theme.fill}${content}${theme.fill}${' '.repeat(Math.max(0, padding))}${theme.border}║${RESET}`);
  }

  renderHeader() {
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors based on connectivity
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    const themeText = theme.text;
    const themeAccent = theme.accent;
    
    // Top border
    this.writeLine(`${themeBorder}╔${'═'.repeat(boxWidth - 2)}╗${RESET}`);
    
    // === TITLE SECTION ===
    if (!compact) {
      // Empty line for breathing room
      this.renderBoxLine('', boxWidth, theme);
      
      // Animated baby block title
      for (let lineIdx = 0; lineIdx < ARTERY_ASCII.length; lineIdx++) {
        const artLine = ARTERY_ASCII[lineIdx];
        const letterCount = artLine.replace(/ /g, '').length;
        const gapCount = (artLine.match(/  /g) || []).length;
        const blockWidth = letterCount * 3 + gapCount * 3;
        const artPadding = Math.floor((boxWidth - blockWidth - 2) / 2);
        const rightPad = boxWidth - blockWidth - artPadding - 2;
        
        let animatedLine = '';
        let charIdx = 0;
        for (let i = 0; i < artLine.length; i++) {
          const char = artLine[i];
          if (char === ' ') {
            if (i + 1 < artLine.length && artLine[i + 1] === ' ') {
              animatedLine += '   ';
              i++;
            }
          } else {
            const timeOffset = Math.floor(this.bloodPosition / 3);
            const waveOffset = Math.floor(Math.sin((charIdx + timeOffset) * 0.5) * 2);
            const colorIdx = Math.abs((charIdx + timeOffset + waveOffset) % BLOCK_COLORS.length);
            const block = BLOCK_COLORS[colorIdx];
            animatedLine += `${block.bg}${block.fg}${BOLD} ${char} ${RESET}${themeFill}`;
            charIdx++;
          }
        }
        
        this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(Math.max(0, artPadding))}${animatedLine}${' '.repeat(Math.max(0, rightPad))}${themeBorder}║${RESET}`);
      }
      
      // Empty line for breathing room
      this.renderBoxLine('', boxWidth, theme);
    } else {
      // Compact: simple centered title
      const title = `${themeAccent}${BOLD}AESTHETIC COMPUTER${RESET}${themeFill}`;
      const titleLen = 18; // "AESTHETIC COMPUTER"
      const leftPad = Math.floor((boxWidth - 2 - titleLen) / 2);
      const rightPad = boxWidth - 2 - titleLen - leftPad;
      this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(leftPad)}${title}${' '.repeat(rightPad)}${themeBorder}║${RESET}`);
    }
    
    // === STATUS SECTION ===
    // AC status line
    const acStatus = this.connected 
      ? `${FG_BRIGHT_GREEN}● Open` 
      : `${FG_BRIGHT_RED}○ Closed`;
    const piece = this.currentPiece ? ` ${themeText}│ ${themeAccent}${this.currentPiece}` : '';
    
    const localIcon = this.serverStatus.local === true ? `${FG_BRIGHT_GREEN}●` 
                    : this.serverStatus.local === false ? `${FG_BRIGHT_YELLOW}◐` 
                    : `${DIM}?`;
    const prodIcon = this.serverStatus.production === true ? `${FG_BRIGHT_GREEN}●` 
                   : this.serverStatus.production === false ? `${FG_BRIGHT_RED}○` 
                   : `${DIM}?`;
    
    const serverInfo = compact 
      ? ` ${localIcon}${prodIcon}` 
      : ` ${themeText}│ L${localIcon} P${prodIcon}`;
    
    const statusContent = `${acStatus}${piece}${serverInfo}`;
    this.renderBoxLine(statusContent, boxWidth, theme);
    
    // === PLATFORM LINE (non-compact only) ===
    if (this.hostInfo.inContainer && !compact) {
      const containerLabel = this.hostInfo.containerDistro || 'Container';
      const hostLabel = this.hostInfo.hostLabel || this.hostInfo.hostOS || 'Host';
      const cdpIcon = this.cdpStatus === 'online' ? `${FG_BRIGHT_GREEN}●` 
                    : this.cdpStatus === 'offline' ? `${FG_BRIGHT_RED}○` 
                    : `${DIM}?`;
      
      const platformContent = `${FG_BRIGHT_MAGENTA}📦 ${containerLabel} ${FG_BRIGHT_CYAN}→ ${this.hostInfo.hostEmoji} ${FG_WHITE}${BOLD}${hostLabel}${RESET}${themeFill} ${FG_CYAN}CDP${cdpIcon}${RESET}${themeFill}`;
      this.renderBoxLine(platformContent, boxWidth, theme);
    }
    
    // Separator
    this.writeLine(`${themeBorder}╠${'═'.repeat(boxWidth - 2)}╣${RESET}`);
  }

  renderFooter() {
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    const themeText = theme.text;
    
    // Status message (truncate if needed)
    let statusLine = '';
    if (this.statusMessage) {
      const colors = {
        info: FG_BRIGHT_CYAN,
        success: FG_BRIGHT_GREEN,
        warn: FG_BRIGHT_YELLOW,
        error: FG_BRIGHT_RED,
      };
      const maxMsgLen = boxWidth - 20; // Leave room for clock
      const truncMsg = this.statusMessage.length > maxMsgLen 
        ? this.statusMessage.slice(0, maxMsgLen - 3) + '...' 
        : this.statusMessage;
      statusLine = `${colors[this.statusType] || FG_WHITE}${truncMsg}${RESET}`;
    }
    
    const footerHint = compact ? `${themeText}Q${FG_WHITE}uit ${themeText}Esc${RESET}${themeFill}` : `${themeText}[Q]${FG_WHITE}uit ${themeText}[Esc]${FG_WHITE}Menu${RESET}${themeFill}`;
    const footerContent = ` ${statusLine || footerHint}`;
    
    // Footer line with bottom border
    this.writeLine(`${themeBorder}╠${'═'.repeat(boxWidth - 2)}╣${RESET}`);
    this.renderBoxLine(footerContent, boxWidth, theme);
    this.writeLine(`${themeBorder}╚${'═'.repeat(boxWidth - 2)}╝${RESET}`);
  }

  renderMenu() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    const themeText = theme.text;
    
    // Calculate how many menu items we can show
    const headerLines = compact ? 4 : 5;
    const footerLines = 3;
    const logPreviewMinLines = 2;
    const availableLines = this.innerHeight - headerLines - footerLines - logPreviewMinLines - 2;
    const visibleMenuItems = Math.min(this.menuItems.length, Math.max(3, availableLines));
    
    // Menu items - DOS style with highlight
    for (let i = 0; i < visibleMenuItems; i++) {
      const item = this.menuItems[i];
      const selected = i === this.selectedIndex;
      const prefix = selected ? `${DOS_HIGHLIGHT}► ` : `${themeFill}  `;
      const suffix = selected ? `${RESET}` : `${RESET}`;
      const key = selected ? `${FG_BLACK}[${item.key}]` : `${FG_BRIGHT_YELLOW}[${item.key}]${RESET}${themeFill}`;
      let label = selected ? `${FG_BLACK}${BOLD}${item.label}` : `${FG_WHITE}${item.label}`;
      // Only show description if wide enough
      let desc = compact ? '' : (selected ? `${FG_BLACK}${item.desc}` : `${FG_CYAN}${item.desc}`);
      
      // Add unread badge to logs item
      if (item.key === 'l' && this.unreadLogs > 0) {
        label = `${label}${RESET}${selected ? DOS_HIGHLIGHT : themeFill} ${BG_RED}${FG_WHITE}${this.unreadLogs}${RESET}${selected ? DOS_HIGHLIGHT : themeFill}`;
      }
      
      const line = `${themeBorder}║${RESET}${prefix}${key} ${label}${desc ? ' ' + desc : ''}${suffix}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, padding))}${themeBorder}║${RESET}`);
    }
    
    // Show scroll indicator if more items
    if (visibleMenuItems < this.menuItems.length) {
      const moreText = `${FG_CYAN}...${this.menuItems.length - visibleMenuItems} more${RESET}`;
      const moreLine = `${themeBorder}║${RESET}${themeFill}  ${moreText}`;
      const morePad = boxWidth - this.stripAnsi(moreLine).length - 1;
      this.writeLine(`${moreLine}${themeFill}${' '.repeat(Math.max(0, morePad))}${themeBorder}║${RESET}`);
    }
    
    // Separator before live log preview - DOS style
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Show recent logs preview at bottom - DOS style
    const logPreviewTitle = `${themeBorder}║${RESET}${themeFill}${FG_BRIGHT_YELLOW}► Logs${FG_CYAN}(${this.logs.length})${RESET}`;
    const titlePadding = boxWidth - this.stripAnsi(logPreviewTitle).length - 1;
    this.writeLine(`${logPreviewTitle}${themeFill}${' '.repeat(Math.max(0, titlePadding))}${themeBorder}║${RESET}`);
    
    // Calculate space for log preview dynamically
    const menuShown = visibleMenuItems + (visibleMenuItems < this.menuItems.length ? 1 : 0);
    const usedLines = headerLines + menuShown + 2 + 3 + (this.marginY * 2); // header + items + separator + title + footer + margins
    const logPreviewLines = Math.max(1, this.height - usedLines - 1);
    const recentLogs = this.logs.slice(0, logPreviewLines);
    
    for (let i = 0; i < logPreviewLines; i++) {
      if (i < recentLogs.length) {
        const log = recentLogs[i];
        const colors = {
          log: FG_WHITE,
          info: FG_BRIGHT_CYAN,
          warn: FG_BRIGHT_YELLOW,
          error: FG_BRIGHT_RED,
          warning: FG_BRIGHT_YELLOW,
        };
        const color = colors[log.level] || FG_WHITE;
        // Compact: skip timestamp
        const timeStamp = compact ? '' : `${FG_CYAN}${log.timestamp}${RESET}${themeFill} `;
        const line = `${themeBorder}║${RESET}${themeFill}${timeStamp}${color}${log.text}${RESET}`;
        const truncated = this.truncate(line, boxWidth - 1);
        const padding = boxWidth - this.stripAnsi(truncated).length - 1;
        this.writeLine(`${truncated}${themeFill}${' '.repeat(Math.max(0, padding))}${themeBorder}║${RESET}`);
      } else {
        this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(boxWidth - 2)}${themeBorder}║${RESET}`);
      }
    }
    
    this.renderFooter();
  }

  renderRepl() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    
    // REPL title - DOS style
    const helpHint = compact ? '' : ` ${FG_CYAN}(.help)${RESET}`;
    const replTitle = `${themeBorder}║${RESET}${themeFill}${FG_BRIGHT_YELLOW}►${FG_WHITE}REPL${helpHint}`;
    const replPadding = boxWidth - this.stripAnsi(replTitle).length - 1;
    this.writeLine(`${replTitle}${themeFill}${' '.repeat(Math.max(0, replPadding))}${themeBorder}║${RESET}`);
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Show recent logs
    const logLines = Math.max(1, this.innerHeight - 10);
    const recentLogs = this.logs.slice(0, logLines).reverse();
    
    for (const log of recentLogs) {
      const colors = {
        log: FG_WHITE,
        info: FG_BRIGHT_CYAN,
        warn: FG_BRIGHT_YELLOW,
        error: FG_BRIGHT_RED,
        input: FG_BRIGHT_MAGENTA,
        result: FG_BRIGHT_GREEN,
      };
      const color = colors[log.level] || FG_WHITE;
      const timeStamp = compact ? '' : `${FG_CYAN}${log.timestamp}${RESET}${themeFill} `;
      const line = `${themeBorder}║${RESET}${themeFill}${timeStamp}${color}${log.text}${RESET}`;
      const truncated = this.truncate(line, boxWidth - 1);
      const padding = boxWidth - this.stripAnsi(truncated).length - 1;
      this.writeLine(`${truncated}${themeFill}${' '.repeat(Math.max(0, padding))}${themeBorder}║${RESET}`);
    }
    
    // Fill remaining
    for (let i = recentLogs.length; i < logLines; i++) {
      this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(boxWidth - 2)}${themeBorder}║${RESET}`);
    }
    
    // Input line
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    const prompt = `${FG_BRIGHT_MAGENTA}🩸${RESET}${themeFill}`;
    const inputLine = `${themeBorder}║${RESET}${themeFill}${prompt}${FG_WHITE}${this.inputBuffer}${RESET}`;
    const inputPadding = boxWidth - this.stripAnsi(inputLine).length - 1;
    this.writeLine(`${inputLine}${themeFill}${' '.repeat(Math.max(0, inputPadding))}${themeBorder}║${RESET}`);
    
    this.renderFooter();
    
    // Show cursor at input position
    this.write(CURSOR_SHOW);
    this.write(moveTo(this.height - 3, this.adaptiveMarginX + 6 + this.inputBuffer.length));
  }

  renderEmacs() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    
    // Emacs title - DOS style
    const emacsTitle = `${themeBorder}║${RESET}${themeFill}${FG_BRIGHT_GREEN}🧠${FG_WHITE} Emacs${RESET}`;
    const emacsPadding = boxWidth - this.stripAnsi(emacsTitle).length - 1;
    this.writeLine(`${emacsTitle}${themeFill}${' '.repeat(Math.max(0, emacsPadding))}${themeBorder}║${RESET}`);
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    if (this.emacsSubMode === 'menu') {
      // Emacs menu options
      const menuOptions = [
        { key: 'e', label: 'Eval Elisp', desc: 'Execute Emacs Lisp code' },
        { key: 'b', label: 'Buffers', desc: 'List and switch buffers' },
        { key: 'v', label: 'Version', desc: 'Show Emacs version' },
      ];
      
      for (const opt of menuOptions) {
        const line = `${themeBorder}║${RESET}${themeFill}  ${FG_BRIGHT_YELLOW}[${opt.key}]${RESET}${themeFill} ${FG_WHITE}${opt.label}${RESET}${themeFill} ${DIM}${opt.desc}${RESET}`;
        const linePadding = boxWidth - this.stripAnsi(line).length - 1;
        this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, linePadding))}${themeBorder}║${RESET}`);
      }
      
      // Show output
      this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
      const outputTitle = `${themeBorder}║${RESET}${themeFill}${FG_CYAN}Output:${RESET}`;
      const outputPadding = boxWidth - this.stripAnsi(outputTitle).length - 1;
      this.writeLine(`${outputTitle}${themeFill}${' '.repeat(Math.max(0, outputPadding))}${themeBorder}║${RESET}`);
      
      // Wrap output to fit
      const outputLines = (this.emacsOutput || '(no output)').split('\n').slice(0, 5);
      for (const outLine of outputLines) {
        const truncated = this.truncate(outLine, boxWidth - 4);
        const line = `${themeBorder}║${RESET}${themeFill}  ${FG_BRIGHT_GREEN}${truncated}${RESET}`;
        const linePadding = boxWidth - this.stripAnsi(line).length - 1;
        this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, linePadding))}${themeBorder}║${RESET}`);
      }
    } else if (this.emacsSubMode === 'eval') {
      // Elisp input mode
      const helpLine = `${themeBorder}║${RESET}${themeFill}  ${DIM}Type elisp and press Enter. ESC to go back.${RESET}`;
      const helpPadding = boxWidth - this.stripAnsi(helpLine).length - 1;
      this.writeLine(`${helpLine}${themeFill}${' '.repeat(Math.max(0, helpPadding))}${themeBorder}║${RESET}`);
      
      // Input line
      const prompt = `${FG_BRIGHT_GREEN}λ${RESET}${themeFill}`;
      const inputLine = `${themeBorder}║${RESET}${themeFill}${prompt}${FG_WHITE}${this.emacsInput}${RESET}`;
      const inputPadding = boxWidth - this.stripAnsi(inputLine).length - 1;
      this.writeLine(`${inputLine}${themeFill}${' '.repeat(Math.max(0, inputPadding))}${themeBorder}║${RESET}`);
      
      // Show output
      this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
      const outputLines = (this.emacsOutput || '').split('\n').slice(0, 8);
      for (const outLine of outputLines) {
        const truncated = this.truncate(outLine, boxWidth - 4);
        const line = `${themeBorder}║${RESET}${themeFill}  ${FG_BRIGHT_GREEN}${truncated}${RESET}`;
        const linePadding = boxWidth - this.stripAnsi(line).length - 1;
        this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, linePadding))}${themeBorder}║${RESET}`);
      }
    } else if (this.emacsSubMode === 'buffers') {
      // Buffer list
      const helpLine = `${themeBorder}║${RESET}${themeFill}  ${DIM}↑↓ to select, Enter to switch. ESC to go back.${RESET}`;
      const helpPadding = boxWidth - this.stripAnsi(helpLine).length - 1;
      this.writeLine(`${helpLine}${themeFill}${' '.repeat(Math.max(0, helpPadding))}${themeBorder}║${RESET}`);
      this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
      
      const visibleCount = Math.max(1, this.innerHeight - 12);
      const startIdx = Math.max(0, this.emacsSelectedBuffer - Math.floor(visibleCount / 2));
      const endIdx = Math.min(this.emacsBuffers.length, startIdx + visibleCount);
      
      for (let i = startIdx; i < endIdx; i++) {
        const buffer = this.emacsBuffers[i];
        const isSelected = i === this.emacsSelectedBuffer;
        const prefix = isSelected ? `${DOS_HIGHLIGHT}►` : ` ${themeFill}`;
        const color = isSelected ? `${DOS_HIGHLIGHT}` : `${themeFill}${FG_WHITE}`;
        const line = `${themeBorder}║${RESET}${prefix}${color} ${buffer}${RESET}`;
        const linePadding = boxWidth - this.stripAnsi(line).length - 1;
        const bg = isSelected ? DOS_HIGHLIGHT : themeFill;
        this.writeLine(`${line}${bg}${' '.repeat(Math.max(0, linePadding))}${themeBorder}║${RESET}`);
      }
    }
    
    // Fill remaining space
    const usedLines = this.emacsSubMode === 'buffers' 
      ? Math.min(this.emacsBuffers.length, this.innerHeight - 12) + 5
      : 12;
    for (let i = usedLines; i < this.innerHeight - 4; i++) {
      this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(boxWidth - 2)}${themeBorder}║${RESET}`);
    }
    
    this.renderFooter();
    
    // Show cursor in eval mode
    if (this.emacsSubMode === 'eval') {
      this.write(CURSOR_SHOW);
      this.write(moveTo(this.marginY + 6, this.adaptiveMarginX + 4 + this.emacsInput.length));
    }
  }

  renderPieces() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    
    // Title with piece count
    const countInfo = compact ? '' : ` ${DIM}(${this.filteredPieces.length}/${this.allPieces.length})${RESET}`;
    const pieceTitle = `${themeBorder}║${RESET}${themeFill}${FG_BRIGHT_YELLOW}►${FG_WHITE}Pieces${countInfo}`;
    const piecePadding = boxWidth - this.stripAnsi(pieceTitle).length - 1;
    this.writeLine(`${pieceTitle}${themeFill}${' '.repeat(Math.max(0, piecePadding))}${themeBorder}║${RESET}`);
    
    // Search input line
    const searchPrompt = `${FG_YELLOW}🔍${RESET}${themeFill}`;
    const searchDisplay = this.inputBuffer || `${DIM}search...${RESET}`;
    const searchLine = `${themeBorder}║${RESET}${themeFill}${searchPrompt}${searchDisplay}`;
    const searchPadding = boxWidth - this.stripAnsi(searchLine).length - 1;
    this.writeLine(`${searchLine}${themeFill}${' '.repeat(Math.max(0, searchPadding))}${themeBorder}║${RESET}`);
    
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Pieces list
    const visibleCount = Math.max(1, this.innerHeight - 10);
    const startIdx = Math.max(0, this.selectedIndex - Math.floor(visibleCount / 2));
    const endIdx = Math.min(this.filteredPieces.length, startIdx + visibleCount);
    
    for (let i = startIdx; i < endIdx; i++) {
      const piece = this.filteredPieces[i];
      const selected = i === this.selectedIndex;
      const prefix = selected ? `${theme.highlight}▸ ` : `${themeFill}  `;
      const suffix = selected ? `${RESET}` : `${RESET}`;
      
      // Highlight matching part of name
      let label;
      if (this.inputBuffer && piece.toLowerCase().includes(this.inputBuffer.toLowerCase())) {
        const idx = piece.toLowerCase().indexOf(this.inputBuffer.toLowerCase());
        const before = piece.slice(0, idx);
        const match = piece.slice(idx, idx + this.inputBuffer.length);
        const after = piece.slice(idx + this.inputBuffer.length);
        label = selected 
          ? `${FG_BLACK}${BOLD}${before}${FG_YELLOW}${match}${FG_BLACK}${after}${RESET}` 
          : `${FG_WHITE}${before}${FG_YELLOW}${match}${FG_WHITE}${after}${RESET}`;
      } else {
        label = selected ? `${FG_BLACK}${BOLD}${piece}${RESET}` : `${FG_WHITE}${piece}${RESET}`;
      }
      
      const line = `${themeBorder}║${RESET}${prefix}${label}${suffix}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, padding))}${themeBorder}║${RESET}`);
    }
    
    // Fill remaining
    for (let i = endIdx - startIdx; i < visibleCount; i++) {
      this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(boxWidth - 2)}${themeBorder}║${RESET}`);
    }
    
    this.renderFooter();
    
    // Show cursor at search position
    this.write(CURSOR_SHOW);
    this.write(moveTo(this.marginY + 2, this.adaptiveMarginX + 5 + this.inputBuffer.length));
  }

  renderTests() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    
    // Title
    const hint = compact ? '' : ` ${DIM}(↑↓,Enter)${RESET}`;
    const testTitle = `${themeBorder}║${RESET}${themeFill}${FG_BRIGHT_YELLOW}►${FG_WHITE}Tests${hint}`;
    const testPadding = boxWidth - this.stripAnsi(testTitle).length - 1;
    this.writeLine(`${testTitle}${themeFill}${' '.repeat(Math.max(0, testPadding))}${themeBorder}║${RESET}`);
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Tests list
    const tests = this.testFiles || [];
    const visibleCount = Math.max(1, this.innerHeight - 8);
    
    for (let i = 0; i < Math.min(tests.length, visibleCount); i++) {
      const test = tests[i];
      const selected = i === this.selectedIndex;
      
      // Handle separator rows
      if (test.isSeparator) {
        const sepLine = `${themeBorder}║${RESET}${themeFill}${DIM}${FG_CYAN}  ─── ${test.desc} ───${RESET}`;
        const sepPadding = boxWidth - this.stripAnsi(sepLine).length - 1;
        this.writeLine(`${sepLine}${themeFill}${' '.repeat(Math.max(0, sepPadding))}${themeBorder}║${RESET}`);
        continue;
      }
      
      const prefix = selected ? `${theme.highlight}▸ ` : `${themeFill}  `;
      const suffix = selected ? `${RESET}` : `${RESET}`;
      const configIcon = test.params ? `${selected ? FG_BLACK : FG_YELLOW}⚙${RESET}${selected ? theme.highlight : themeFill}` : '';
      const label = selected ? `${FG_BLACK}${BOLD}${test.name}${RESET}` : `${FG_WHITE}${test.name}${RESET}`;
      const desc = compact ? '' : ` ${DIM}${test.desc}${RESET}`;
      
      const line = `${themeBorder}║${RESET}${prefix}${configIcon}${label}${suffix}${desc}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, padding))}${themeBorder}║${RESET}`);
    }
    
    // Fill remaining
    for (let i = tests.length; i < visibleCount; i++) {
      this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(boxWidth - 2)}${themeBorder}║${RESET}`);
    }
    
    this.renderFooter();
  }
  
  renderTestParams() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    const test = this.currentTest;
    const params = test?.params || [];
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    const themeBorder = theme.border;
    const themeFill = theme.fill;
    
    // Title
    const hint = compact ? '' : ` ${DIM}(←→,Enter)${RESET}`;
    const testTitle = `${themeBorder}║${RESET}${themeFill}${FG_BRIGHT_YELLOW}⚙${FG_WHITE}${test?.name}${hint}`;
    const testPadding = boxWidth - this.stripAnsi(testTitle).length - 1;
    this.writeLine(`${testTitle}${themeFill}${' '.repeat(Math.max(0, testPadding))}${themeBorder}║${RESET}`);
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Parameters list
    for (let i = 0; i < params.length; i++) {
      const param = params[i];
      const selected = i === this.paramIndex;
      const prefix = selected ? `${theme.highlight}▸ ` : `${themeFill}  `;
      const suffix = selected ? `${RESET}` : `${RESET}`;
      
      const value = selected ? this.inputBuffer : (this.testParams[param.key] || param.default);
      
      // Show value with arrows if it has options or is a number
      let valueDisplay;
      if (selected && (param.options || param.type === 'number')) {
        const leftArrow = `${FG_BLACK}◀`;
        const rightArrow = `${FG_BLACK}▶`;
        valueDisplay = value ? `${leftArrow} ${FG_YELLOW}${value}${RESET}${theme.highlight} ${rightArrow}` : `${leftArrow} ${DIM}(empty)${RESET}${theme.highlight} ${rightArrow}`;
      } else {
        valueDisplay = value ? `${selected ? FG_YELLOW : FG_GREEN}${value}${RESET}` : `${DIM}(empty)${RESET}`;
      }
      
      const label = selected ? `${FG_BLACK}${BOLD}${param.label}${RESET}` : `${FG_WHITE}${param.label}${RESET}`;
      const desc = compact ? '' : ` ${DIM}${param.desc}${RESET}`;
      
      const line = `${themeBorder}║${RESET}${prefix}${label}:${suffix} ${valueDisplay}${desc}`;
      const padding = boxWidth - this.stripAnsi(line).length - 1;
      this.writeLine(`${line}${themeFill}${' '.repeat(Math.max(0, padding))}${themeBorder}║${RESET}`);
    }
    
    // Run button
    this.writeLine(`${themeBorder}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    const runSelected = this.paramIndex >= params.length;
    const runPrefix = runSelected ? `${BG_GREEN}${FG_WHITE}▸ ` : `${themeFill}  `;
    const runSuffix = runSelected ? `${RESET}` : `${RESET}`;
    const runLabel = runSelected ? `${BOLD}🚀RUN${RESET}` : `${FG_GREEN}🚀RUN${RESET}`;
    const runLine = `${themeBorder}║${RESET}${runPrefix}${runLabel}${runSuffix}`;
    const runPadding = boxWidth - this.stripAnsi(runLine).length - 1;
    this.writeLine(`${runLine}${themeFill}${' '.repeat(Math.max(0, runPadding))}${themeBorder}║${RESET}`);
    
    // Preview command (skip on compact)
    if (!compact) {
      const args = params.map(p => this.testParams[p.key] || p.default).filter(a => a).join(' ');
      const cmdText = `node ${test?.file} ${args}`.slice(0, boxWidth - 10);
      const cmdPreview = `${DIM}>${cmdText}${RESET}`;
      const previewLine = `${themeBorder}║${RESET}${themeFill}${cmdPreview}`;
      const previewPadding = boxWidth - this.stripAnsi(previewLine).length - 1;
      this.writeLine(`${previewLine}${themeFill}${' '.repeat(Math.max(0, previewPadding))}${themeBorder}║${RESET}`);
    }
    
    // Fill remaining
    const usedLines = params.length + (compact ? 3 : 4);
    const visibleCount = Math.max(1, this.innerHeight - 8);
    for (let i = usedLines; i < visibleCount; i++) {
      this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(boxWidth - 2)}${themeBorder}║${RESET}`);
    }
    
    this.renderFooter();
    
    // Show cursor at input position if editing a param (only for non-option params)
    if (this.paramIndex < params.length) {
      const param = params[this.paramIndex];
      if (!param.options && param.type !== 'number') {
        this.write(CURSOR_SHOW);
        const labelLen = param.label.length + 3;
        this.write(moveTo(this.marginY + 3 + this.paramIndex, this.adaptiveMarginX + 4 + labelLen + this.inputBuffer.length));
      }
    }
  }

  renderLogs() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Title
    const countInfo = compact ? '' : `${DIM}(${this.logs.length})${RESET}`;
    const logsTitle = `${DOS_BORDER}║${RESET}${BG_BLUE}${FG_BRIGHT_YELLOW}►${FG_WHITE}Logs${countInfo}`;
    const logsPadding = boxWidth - this.stripAnsi(logsTitle).length - 1;
    this.writeLine(`${logsTitle}${BG_BLUE}${' '.repeat(Math.max(0, logsPadding))}${DOS_BORDER}║${RESET}`);
    this.writeLine(`${DOS_BORDER}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Logs
    const logLines = Math.max(1, this.innerHeight - 7);
    const recentLogs = this.logs.slice(0, logLines);
    
    for (const log of recentLogs) {
      const colors = {
        log: FG_WHITE,
        info: FG_BRIGHT_CYAN,
        warn: FG_BRIGHT_YELLOW,
        error: FG_BRIGHT_RED,
      };
      const color = colors[log.level] || FG_WHITE;
      const timeStamp = compact ? '' : `${FG_CYAN}${log.timestamp}${RESET}${BG_BLUE} `;
      const line = `${DOS_BORDER}║${RESET}${BG_BLUE}${timeStamp}${color}${log.text}${RESET}`;
      const truncated = this.truncate(line, boxWidth - 1);
      const padding = boxWidth - this.stripAnsi(truncated).length - 1;
      this.writeLine(`${truncated}${BG_BLUE}${' '.repeat(Math.max(0, padding))}${DOS_BORDER}║${RESET}`);
    }
    
    // Fill remaining
    for (let i = recentLogs.length; i < logLines; i++) {
      this.writeLine(`${DOS_BORDER}║${BG_BLUE}${' '.repeat(boxWidth - 2)}${DOS_BORDER}║${RESET}`);
    }
    
    this.renderFooter();
  }

  renderSite() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    
    // Status indicator
    const statusColors = {
      stopped: FG_BRIGHT_RED,
      starting: FG_BRIGHT_YELLOW,
      running: FG_BRIGHT_GREEN,
      error: FG_BRIGHT_RED,
    };
    const statusIcons = {
      stopped: '○',
      starting: '◐',
      running: '●',
      error: '✗',
    };
    const statusColor = statusColors[this.siteStatus] || FG_WHITE;
    const statusIcon = statusIcons[this.siteStatus] || '?';
    
    // Calculate uptime if running
    let uptimeStr = '';
    if (!compact && this.siteStatus === 'running' && this.siteStartTime) {
      const elapsed = Math.floor((Date.now() - this.siteStartTime) / 1000);
      const mins = Math.floor(elapsed / 60);
      const secs = elapsed % 60;
      uptimeStr = ` ${DIM}${mins}m${secs}s${RESET}`;
    }
    
    // Title with status
    const siteTitle = `${DOS_BORDER}║${RESET}${BG_BLUE}${FG_BRIGHT_YELLOW}►${FG_WHITE}Site ${statusColor}${statusIcon}${this.siteStatus.toUpperCase()}${RESET}${uptimeStr}`;
    const sitePadding = boxWidth - this.stripAnsi(siteTitle).length - 1;
    this.writeLine(`${siteTitle}${BG_BLUE}${' '.repeat(Math.max(0, sitePadding))}${DOS_BORDER}║${RESET}`);
    
    // Controls bar
    const controls = compact 
      ? `${FG_CYAN}S${FG_WHITE}tart ${FG_CYAN}K${FG_WHITE}ill`
      : `${FG_CYAN}[S]${FG_WHITE}tart ${FG_CYAN}[K]${FG_WHITE}ill ${FG_CYAN}[R]${FG_WHITE}estart ${FG_CYAN}[C]${FG_WHITE}lear${RESET}`;
    const controlsLine = `${DOS_BORDER}║${RESET}${BG_BLUE}${controls}`;
    const controlsPadding = boxWidth - this.stripAnsi(controlsLine).length - 1;
    this.writeLine(`${controlsLine}${BG_BLUE}${' '.repeat(Math.max(0, controlsPadding))}${DOS_BORDER}║${RESET}`);
    
    this.writeLine(`${DOS_BORDER}╟${'─'.repeat(boxWidth - 2)}╢${RESET}`);
    
    // Site logs
    const logLines = Math.max(1, this.innerHeight - 8);
    const recentLogs = this.siteLogs.slice(-logLines);
    
    for (const log of recentLogs) {
      const colors = {
        log: FG_WHITE,
        info: FG_BRIGHT_CYAN,
        warn: FG_BRIGHT_YELLOW,
        error: FG_BRIGHT_RED,
        success: FG_BRIGHT_GREEN,
      };
      const color = colors[log.level] || FG_WHITE;
      const timeStamp = compact ? '' : `${FG_CYAN}${log.time}${RESET}${BG_BLUE} `;
      const line = `${DOS_BORDER}║${RESET}${BG_BLUE}${timeStamp}${color}${log.text}${RESET}`;
      const truncated = this.truncate(line, boxWidth - 1);
      const padding = boxWidth - this.stripAnsi(truncated).length - 1;
      this.writeLine(`${truncated}${BG_BLUE}${' '.repeat(Math.max(0, padding))}${DOS_BORDER}║${RESET}`);
    }
    
    // Fill remaining
    for (let i = recentLogs.length; i < logLines; i++) {
      this.writeLine(`${DOS_BORDER}║${BG_BLUE}${' '.repeat(boxWidth - 2)}${DOS_BORDER}║${RESET}`);
    }
    
    this.renderFooter();
  }

  // Utilities
  getUTCTimeString() {
    const now = new Date();
    const h = String(now.getUTCHours()).padStart(2, '0');
    const m = String(now.getUTCMinutes()).padStart(2, '0');
    const s = String(now.getUTCSeconds()).padStart(2, '0');
    const ms = String(now.getUTCMilliseconds()).padStart(3, '0');
    return `${h}:${m}:${s}.${ms}`;
  }

  // Get analog clock widget - a tiny visual clock
  getAnalogClockWidget() {
    const now = new Date();
    const h = now.getUTCHours() % 12;
    const m = now.getUTCMinutes();
    const s = now.getUTCSeconds();
    const ms = now.getUTCMilliseconds();
    
    // Clock face characters for each hour (Unicode clock faces)
    // 🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚🕛 (on the hour)
    // 🕜🕝🕞🕟🕠🕡🕢🕣🕤🕥🕦🕧 (half past)
    const clocksOnHour = ['🕛','🕐','🕑','🕒','🕓','🕔','🕕','🕖','🕗','🕘','🕙','🕚'];
    const clocksHalfPast = ['🕧','🕜','🕝','🕞','🕟','🕠','🕡','🕢','🕣','🕤','🕥','🕦'];
    
    // Choose clock face based on hour and whether past 30 min
    const clockFace = m >= 30 ? clocksHalfPast[h] : clocksOnHour[h];
    
    // Spinning second hand using braille/box chars that rotate
    const secondChars = ['│','╱','─','╲','│','╱','─','╲'];
    const secIdx = Math.floor((s * 8) / 60);
    const secHand = secondChars[secIdx];
    
    // Millisecond spinner (fast spinning dots)
    const msSpinner = ['⠋','⠙','⠹','⠸','⠼','⠴','⠦','⠧','⠇','⠏'];
    const msIdx = Math.floor((ms * 10) / 1000);
    const spinner = msSpinner[msIdx];
    
    // Digital time compact
    const hh = String(now.getUTCHours()).padStart(2, '0');
    const mm = String(m).padStart(2, '0');
    const ss = String(s).padStart(2, '0');
    
    // Return: clock face + spinner + digital time
    return `${clockFace}${spinner}${hh}:${mm}:${ss}`;
  }

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
  // Parse CLI flags
  const args = process.argv.slice(2);
  const noAnimation = args.includes('--no-animation') || args.includes('-n');
  const simpleMode = args.includes('--simple') || args.includes('-s');
  
  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
🩸 Artery TUI - Terminal UI for Aesthetic Computer

Usage: artery-tui [options]

Options:
  --no-animation, -n   Disable blood flow animation (reduces rendering issues)
  --simple, -s         Simple mode: no animation, basic rendering
  --help, -h           Show this help

Environment:
  TERM_PROGRAM=eat     Auto-detected, uses slower animation
  INSIDE_EMACS=eat     Auto-detected, uses slower animation
`);
    process.exit(0);
  }
  
  const tui = new ArteryTUI();
  
  // Apply CLI options
  if (noAnimation || simpleMode) {
    tui.animationDisabled = true;
  }
  if (simpleMode) {
    tui.simpleMode = true;
  }
  
  await tui.start();
}

main().catch(e => {
  console.error('Fatal error:', e);
  process.exit(1);
});
