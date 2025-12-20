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
import { execSync, spawn, spawnSync } from 'child_process';

// Find the emacsclient's PTY for direct sixel output (bypasses eat terminal)
function findEmacsclientPty() {
  try {
    // Find emacsclient process
    const psResult = spawnSync('pgrep', ['-f', 'emacsclient.*-nw'], { encoding: 'utf-8' });
    if (psResult.status !== 0 || !psResult.stdout.trim()) return null;
    
    const pid = psResult.stdout.trim().split('\n')[0];
    // Get the tty from /proc/<pid>/fd/0
    const fd0 = fs.readlinkSync(`/proc/${pid}/fd/0`);
    if (fd0.startsWith('/dev/pts/')) {
      return fd0;
    }
  } catch (err) {
    // Fall back to null
  }
  return null;
}

// Write sixel directly to the parent terminal (bypassing eat)
function writeSixelDirect(sixelData) {
  const pty = findEmacsclientPty();
  if (pty && sixelData) {
    try {
      fs.writeFileSync(pty, sixelData);
      return true;
    } catch (err) {
      return false;
    }
  }
  return false;
}

// Clear an area of the screen where sixel was rendered (write spaces directly to PTY)
function clearSixelArea(startRow, numRows, width) {
  const pty = findEmacsclientPty();
  if (pty) {
    try {
      const clearLine = ' '.repeat(width);
      let clearSeq = '';
      for (let r = startRow; r < startRow + numRows; r++) {
        clearSeq += `\x1b[${r};1H${clearLine}`;
      }
      fs.writeFileSync(pty, clearSeq);
    } catch {}
  }
}

// Helper: Fetch image and convert to sixel using ImageMagick directly
async function fetchTerminalImage(url, maxWidth = 20, maxHeight = 10) {
  // Handle IPFS URLs - try multiple gateways
  let fetchUrl = url;
  const ipfsGateways = [
    'https://cloudflare-ipfs.com/ipfs/',
    'https://ipfs.io/ipfs/',
    'https://gateway.pinata.cloud/ipfs/',
    'https://w3s.link/ipfs/',
  ];
  
  if (url.startsWith('ipfs://')) {
    const cid = url.slice(7);
    // Try gateways in order
    for (const gateway of ipfsGateways) {
      fetchUrl = `${gateway}${cid}`;
      try {
        const timestamp = Date.now();
        const tmpFile = `/tmp/sixel-thumb-${timestamp}`;
        const curlResult = spawnSync('curl', ['-sL', '--max-time', '8', '-o', `${tmpFile}.img`, fetchUrl], { timeout: 10000 });
        
        if (curlResult.status === 0) {
          const stats = fs.statSync(`${tmpFile}.img`);
          if (stats.size > 500) {
            // Got a real image, convert to sixel
            // Now with direct PTY output, we can use proper sizing
            const pixelSize = 200;
            
            const magickResult = spawnSync('/usr/bin/magick', [
              `${tmpFile}.img[0]`,  // [0] gets first frame of animated images
              '-resize', `${pixelSize}x${pixelSize}>`,
              'sixel:-'
            ], { 
              timeout: 5000,
              maxBuffer: 1024 * 1024 
            });
            
            try { fs.unlinkSync(`${tmpFile}.img`); } catch {}
            
            if (magickResult.status === 0) {
              return magickResult.stdout.toString();
            }
          }
        }
        try { fs.unlinkSync(`${tmpFile}.img`); } catch {}
      } catch (err) {
        // Try next gateway
        continue;
      }
    }
    return null; // All gateways failed
  }
  
  // Non-IPFS URL
  try {
    const timestamp = Date.now();
    const tmpFile = `/tmp/sixel-thumb-${timestamp}`;
    const curlResult = spawnSync('curl', ['-sL', '--max-time', '10', '-o', `${tmpFile}.img`, fetchUrl], { timeout: 12000 });
    if (curlResult.status !== 0) {
      return null;
    }
    
    // Check file size
    try {
      const stats = fs.statSync(`${tmpFile}.img`);
      if (stats.size < 500) {
        fs.unlinkSync(`${tmpFile}.img`);
        return null;
      }
    } catch { return null; }
    
    const pixelSize = 200;
    const magickResult = spawnSync('/usr/bin/magick', [
      `${tmpFile}.img[0]`,
      '-resize', `${pixelSize}x${pixelSize}>`,
      'sixel:-'
    ], { 
      timeout: 5000,
      maxBuffer: 1024 * 1024 
    });
    
    try { fs.unlinkSync(`${tmpFile}.img`); } catch {}
    
    if (magickResult.status !== 0) {
      return null;
    }
    
    return magickResult.stdout.toString();
  } catch (err) {
    return null;
  }
}

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
      { key: 'd', label: 'Deck Control', desc: 'Control KidLisp.com card deck via CDP', action: () => this.enterKidLispCardsMode() },
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
      { key: 'i', label: 'Login', desc: 'AC login/logout', action: () => this.toggleAcLogin() },
      { key: 'q', label: 'Quit', desc: 'Exit Artery TUI', action: () => this.quit() },
    ];
    
    // Keeps (Tezos) state - Enhanced Dashboard
    this.keepsMenu = [
      { key: 's', label: 'Status', desc: 'Refresh contract status' },
      { key: 'b', label: 'Balance', desc: 'Check wallet balance' },
      { key: 't', label: 'Tokens', desc: 'List recent tokens' },
      { key: 'K', label: 'Keep', desc: 'Keep a piece (mint via AC)' },  // New: server-side mint
      { key: 'm', label: 'Mint (Local)', desc: 'Mint using local wallet' },
      { key: 'u', label: 'Upload', desc: 'Upload bundle to IPFS' },
      { key: 'l', label: 'Lock', desc: 'Lock token metadata' },
      { key: 'f', label: 'Fishy', desc: 'Jump to 🐟-fishy terminal' },
      { key: 'r', label: 'Run Tests', desc: 'Run keeps test suite' },
      { key: 'e', label: 'Explorer', desc: 'Open tzkt in browser' },
      { key: 'o', label: 'Objkt', desc: 'Open objkt collection' },
      { key: 'd', label: 'Deploy', desc: 'Deploy FA2 to Ghostnet' },
      { key: 'L', label: 'Login', desc: 'AC login/logout' },  // Dynamic: Login or Logout
    ];
    this.keepsSelectedIndex = 0;
    this.keepsOutput = '';
    this.keepsRunning = false;
    this.keepsPieceInput = '';
    this.keepsSubMode = 'menu'; // 'menu', 'piece-input', 'running'
    this.keepsPendingAction = null; // Track which action needs piece input
    
    // Keeps dashboard live data
    this.keepsContractData = {
      address: 'KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc',
      network: 'ghostnet',
      admin: 'aesthetic',
      tokenCount: 0,
      fee: '0',
      balance: '0',
      recentTokens: [],
      lastUpdated: null,
    };
    this.keepsEngineeringTasks = {
      phase: 'A',
      phaseName: 'Ghostnet Hardening',
      currentFocus: [],
      nextSteps: [],
      successCriteria: '20+ test mints',
      mintsCompleted: 0,
    };
    
    // AC Auth state
    this.acAuth = null; // { user: { handle, email, name }, expires_at }
    this.loadAcAuth(); // Load on startup
    
    // KidLisp Cards state
    this.kidlispCardsMenu = [
      { key: 'n', label: 'Next Card', desc: 'Advance to next card' },
      { key: 'u', label: 'Undo', desc: 'Bring back last discarded card' },
      { key: 'r', label: 'Reset Deck', desc: 'Reset all cards from discard' },
      { key: 's', label: 'Status', desc: 'Show card counts' },
      { key: 'o', label: 'Open Window', desc: 'Open KidLisp.com in VS Code' },
      { key: 't', label: 'Run Tests', desc: 'Run card test suite' },
    ];
    this.kidlispCardsSelectedIndex = 0;
    this.kidlispCardsOutput = '';
    this.kidlispCardsCdpPageId = null;
    
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
    
    // Matrix rain animation state
    this.matrixDrops = []; // Array of { col, row, speed, char, brightness }
    this.matrixChars = 'ARTERY'.split('');
    this.initMatrixRain();
    
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
        accent: FG_BRIGHT_YELLOW,
        highlight: BG_GREEN
      };
    } else if (this.serverStatus.local === true) {
      // Local server up but AC not connected - cyan/blue (ready state)
      return {
        bg: BG_CYAN,
        border: `${BG_CYAN}${FG_BLACK}`,
        fill: BG_CYAN,
        text: FG_BLACK,
        accent: FG_BLUE,
        highlight: BG_CYAN
      };
    } else if (this.serverStatus.production === true) {
      // Only production available - magenta (remote state)
      return {
        bg: BG_MAGENTA,
        border: `${BG_MAGENTA}${FG_WHITE}`,
        fill: BG_MAGENTA,
        text: FG_WHITE,
        accent: FG_BRIGHT_YELLOW,
        highlight: BG_MAGENTA
      };
    } else if (this.serverStatus.local === false && this.serverStatus.production === false) {
      // Nothing available - red (error state)
      return {
        bg: BG_RED,
        border: `${BG_RED}${FG_WHITE}`,
        fill: BG_RED,
        text: FG_WHITE,
        accent: FG_BRIGHT_YELLOW,
        highlight: BG_RED
      };
    } else {
      // Unknown/checking - default blue
      return {
        bg: BG_BLUE,
        border: `${BG_BLUE}${FG_BRIGHT_CYAN}`,
        fill: BG_BLUE,
        text: FG_BRIGHT_CYAN,
        accent: FG_BRIGHT_YELLOW,
        highlight: BG_BLUE
      };
    }
  }
  
  // Get params in visual display order (selects, ranges, toggles)
  getVisualParamOrder(params = []) {
    const selects = params.filter(p => p.type === 'select');
    const ranges = params.filter(p => p.type === 'range');
    const toggles = params.filter(p => p.type === 'toggle');
    return [...selects, ...ranges, ...toggles];
  }
  
  // Initialize Matrix rain drops
  initMatrixRain() {
    this.matrixDrops = [];
    // Create initial drops spread across columns - denser rain
    const numDrops = Math.floor(this.width / 2); // One drop per ~2 columns
    for (let i = 0; i < numDrops; i++) {
      this.matrixDrops.push(this.createMatrixDrop());
    }
  }
  
  createMatrixDrop() {
    return {
      col: Math.floor(Math.random() * this.width),
      row: Math.floor(Math.random() * -10), // Start above screen
      speed: 0.4 + Math.random() * 0.6, // Variable fall speed (faster)
      charIdx: Math.floor(Math.random() * this.matrixChars.length),
      brightness: 0.5 + Math.random() * 0.5, // 0.5-1 for brighter effect
      length: 3 + Math.floor(Math.random() * 5), // Trail length
    };
  }
  
  updateMatrixRain() {
    try {
      if (!this.matrixDrops || !Array.isArray(this.matrixDrops)) return;
      for (let drop of this.matrixDrops) {
        if (!drop) continue;
        drop.row += drop.speed;
        // Occasionally change the character
        if (Math.random() < 0.1) {
          drop.charIdx = Math.floor(Math.random() * this.matrixChars.length);
        }
        // Reset drop when it goes off screen or is out of bounds
        if (drop.row > this.height + drop.length || drop.col >= this.width) {
          drop.col = Math.floor(Math.random() * this.width);
          drop.row = Math.floor(Math.random() * -5);
          drop.speed = 0.4 + Math.random() * 0.6;
          drop.brightness = 0.5 + Math.random() * 0.5;
        }
      }
    } catch (e) {
      // Silently recover from animation errors
    }
  }
  
  // Build matrix rain layer as a 2D array
  getMatrixRainLayer() {
    // Create empty grid
    const grid = [];
    for (let y = 0; y < this.height; y++) {
      grid[y] = new Array(this.width).fill(null);
    }
    
    // Place drops and trails
    for (const drop of this.matrixDrops) {
      const headRow = Math.floor(drop.row);
      for (let i = 0; i < drop.length; i++) {
        const y = headRow - i;
        if (y >= 0 && y < this.height && drop.col >= 0 && drop.col < this.width) {
          // Head is brightest, trail fades
          const fade = 1 - (i / drop.length);
          const char = this.matrixChars[(drop.charIdx + i) % this.matrixChars.length];
          grid[y][drop.col] = { char, fade: fade * drop.brightness };
        }
      }
    }
    return grid;
  }
  
  // Get matrix rain color palette based on connectivity status
  getMatrixColorPalette() {
    if (this.serverStatus.local === true) {
      // 🟢 Local server up - green rain
      return [
        '\x1b[38;5;22m',  // 0: very dark green
        '\x1b[38;5;28m',  // 1: dark green
        '\x1b[38;5;34m',  // 2: medium green
        '\x1b[38;5;40m',  // 3: green
        '\x1b[38;5;46m',  // 4: bright green
        '\x1b[38;5;156m', // 5: very bright green/white (head)
      ];
    } else if (this.serverStatus.local === false) {
      // 🔴 Local server down - red rain
      return [
        '\x1b[38;5;52m',  // 0: very dark red
        '\x1b[38;5;88m',  // 1: dark red
        '\x1b[38;5;124m', // 2: medium red
        '\x1b[38;5;160m', // 3: red
        '\x1b[38;5;196m', // 4: bright red
        '\x1b[38;5;217m', // 5: very bright red/pink (head)
      ];
    } else {
      // 🟡 Unknown/checking - yellow rain
      return [
        '\x1b[38;5;58m',  // 0: very dark yellow/olive
        '\x1b[38;5;100m', // 1: dark yellow
        '\x1b[38;5;142m', // 2: medium yellow
        '\x1b[38;5;184m', // 3: yellow
        '\x1b[38;5;226m', // 4: bright yellow
        '\x1b[38;5;229m', // 5: very bright yellow/white (head)
      ];
    }
  }
  
  // Render a single line of matrix rain
  renderMatrixRainLine(row, rainLayer, bg) {
    if (!rainLayer || !rainLayer[row]) {
      this.writeEmptyLine(bg);
      return;
    }
    
    let line = '';
    const colorPalette = this.getMatrixColorPalette();
    
    for (let col = 0; col < this.width; col++) {
      const cell = rainLayer[row][col];
      if (cell) {
        // Color based on fade value and server status
        // fade 1.0 = bright (head), fade 0.0 = dim (tail)
        const brightness = Math.floor(cell.fade * 5); // 0-5
        const color = colorPalette[Math.min(brightness, 5)];
        line += `${color}${cell.char}${RESET}${bg}`;
      } else {
        line += ' ';
      }
    }
    this.frameBuffer.push(`${bg}${line}${RESET}`);
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
    // Set up signal handlers for graceful cleanup (critical for hot-reload!)
    const cleanup = () => {
      // Clear intervals
      if (this.serverPollInterval) clearInterval(this.serverPollInterval);
      if (this.bloodAnimInterval) clearInterval(this.bloodAnimInterval);
      if (this.resizeTimeout) clearTimeout(this.resizeTimeout);
      
      // Restore terminal state
      this.write(MOUSE_DISABLE);
      this.write(CURSOR_SHOW);
      this.write(ALT_SCREEN_OFF);
      
      // Close client connection
      if (this.client) {
        try { this.client.close(); } catch (e) {}
      }
      
      // Restore stdin
      try {
        process.stdin.setRawMode(false);
        process.stdin.pause();
      } catch (e) {}
    };
    
    // Handle SIGTERM (sent by artery-dev.mjs on hot-reload)
    process.on('SIGTERM', () => {
      cleanup();
      process.exit(0);
    });
    
    // Handle SIGINT (Ctrl+C)
    process.on('SIGINT', () => {
      cleanup();
      process.exit(0);
    });
    
    // Handle uncaught errors to prevent zombie processes
    process.on('uncaughtException', (err) => {
      cleanup();
      console.error('Uncaught exception:', err);
      process.exit(1);
    });
    
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
        this.resizeTimeout = null; // Clear the timeout reference so animation resumes
        this.width = process.stdout.columns || 80;
        this.height = process.stdout.rows || 24;
        this.forceFullRedraw = true; // Force complete redraw on resize
        this.lastRenderedBuffer = []; // Clear buffer to force full redraw
        // Reinitialize matrix rain for new dimensions
        this.initMatrixRain();
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
      try {
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
        
        // Update matrix rain
        this.updateMatrixRain();
        
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
      } catch (e) {
        // Silently recover from animation errors
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
    // Borderless version - just update the title line with animation
    if (this.width < 80) return; // Skip animation in compact mode
    
    const titleBg = BG_BLACK;
    const artLine = 'AESTHETIC  COMPUTER';
    
    // Build animated title
    let animatedLine = '';
    let charIdx = 0;
    for (let i = 0; i < artLine.length; i++) {
      const char = artLine[i];
      if (char === ' ') {
        if (i + 1 < artLine.length && artLine[i + 1] === ' ') {
          animatedLine += '   '; // Gap between words
          i++;
        }
      } else {
        const timeOffset = Math.floor(this.bloodPosition / 3);
        const waveOffset = Math.floor(Math.sin((charIdx + timeOffset) * 0.5) * 2);
        const colorIdx = Math.abs((charIdx + timeOffset + waveOffset) % BLOCK_COLORS.length);
        const block = BLOCK_COLORS[colorIdx];
        animatedLine += `${block.bg}${block.fg}${BOLD} ${char} ${RESET}${titleBg}`;
        charIdx++;
      }
    }
    
    // Calculate centering
    const blockWidth = 17 * 3 + 3; // 17 letters * 3 chars each + 3 for gap
    const leftPad = Math.floor((this.width - blockWidth) / 2);
    const rightPad = this.width - blockWidth - leftPad;
    
    // Position cursor at title line (row 2) and write
    const row = 2;
    this.write(`${moveTo(row, 1)}${titleBg}${' '.repeat(Math.max(0, leftPad))}${animatedLine}${' '.repeat(Math.max(0, rightPad))}${RESET}`);
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
    
    // Also route to testLogs if we're in test-running mode (for CDP console capture)
    if (this.mode === 'test-running' && this.testLogs) {
      this.testLogs.push({ text, time: this.getUTCTimeString(), level });
      if (this.testLogs.length > 100) this.testLogs.shift();
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
    
    // Escape to go back (but ignore if part of mouse/arrow sequence)
    if (key === '\u001b' && this.mode !== 'menu') {
      // Clear sixel if leaving keeps mode
      if (this.mode === 'keeps' && this.keepsSixel) {
        clearSixelArea(18, 15, this.width); // Clear area where sixel was rendered
        this.keepsSixel = null;
      }
      // From test-running or test-params, go back to tests list
      if (this.mode === 'test-running' || this.mode === 'test-params') {
        this.mode = 'tests';
      } else {
        this.mode = 'menu';
      }
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
      case 'test-running':
        this.handleTestRunningInput(key);
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
      case 'kidlisp-cards':
        this.handleKidLispCardsInput(key);
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
              // Initialize testParams from param defaults
              this.testParams = {};
              for (const param of test.params) {
                this.testParams[param.key] = String(param.default || '');
              }
              this.paramIndex = 0;
              this.inputBuffer = String(this.testParams[test.params[0]?.key] || '');
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
      
      case 'test-running':
        // Ignore mouse clicks while test is running - don't exit back to menu
        break;
      
      case 'logs':
        // Ignore mouse clicks in logs view
        break;
      
      default:
        // Other modes: click anywhere to go back to menu
        this.mode = 'menu';
        this.inputBuffer = '';
        this.render();
        break;
    }
  }
  handleMenuInput(key) {
    // Calculate grid dimensions for spatial navigation (must match renderMenuBorderless)
    const itemCount = this.menuItems.length;
    let tilesPerRow;
    if (this.width >= 120) {
      tilesPerRow = Math.min(6, Math.ceil(itemCount / Math.ceil(itemCount / 6)));
    } else if (this.width >= 100) {
      tilesPerRow = Math.min(5, Math.ceil(itemCount / Math.ceil(itemCount / 5)));
    } else if (this.width >= 80) {
      tilesPerRow = Math.min(4, Math.ceil(itemCount / Math.ceil(itemCount / 4)));
    } else {
      tilesPerRow = Math.min(3, Math.ceil(itemCount / Math.ceil(itemCount / 3)));
    }
    
    // Arrow keys - spatial grid navigation
    if (key === '\u001b[A') { // Up - move up one row
      const newIndex = this.selectedIndex - tilesPerRow;
      if (newIndex >= 0) {
        this.selectedIndex = newIndex;
      }
      this.render();
      return;
    }
    if (key === '\u001b[B') { // Down - move down one row
      const newIndex = this.selectedIndex + tilesPerRow;
      if (newIndex < this.menuItems.length) {
        this.selectedIndex = newIndex;
      }
      this.render();
      return;
    }
    if (key === '\u001b[D') { // Left
      if (this.selectedIndex > 0) {
        this.selectedIndex--;
      }
      this.render();
      return;
    }
    if (key === '\u001b[C') { // Right
      if (this.selectedIndex < this.menuItems.length - 1) {
        this.selectedIndex++;
      }
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
    // Filter out separators for navigation
    const tests = (this.testFiles || []).filter(t => !t.isSeparator);
    
    // Get grid dimensions (must match renderTests)
    const testCount = tests.length;
    let tilesPerRow;
    if (this.width >= 120) {
      tilesPerRow = Math.min(6, Math.ceil(testCount / Math.ceil(testCount / 6)));
    } else if (this.width >= 100) {
      tilesPerRow = Math.min(5, Math.ceil(testCount / Math.ceil(testCount / 5)));
    } else if (this.width >= 80) {
      tilesPerRow = Math.min(4, Math.ceil(testCount / Math.ceil(testCount / 4)));
    } else {
      tilesPerRow = Math.min(3, Math.ceil(testCount / Math.ceil(testCount / 3)));
    }
    
    // Find current position in filtered list
    let currentFilteredIdx = 0;
    let counter = 0;
    for (let i = 0; i < (this.testFiles || []).length; i++) {
      if (!this.testFiles[i].isSeparator) {
        if (i === this.selectedIndex) {
          currentFilteredIdx = counter;
          break;
        }
        counter++;
      }
    }
    
    // Map filtered index back to original index
    const filteredToOriginal = (filteredIdx) => {
      let count = 0;
      for (let i = 0; i < (this.testFiles || []).length; i++) {
        if (!this.testFiles[i].isSeparator) {
          if (count === filteredIdx) return i;
          count++;
        }
      }
      return 0;
    };
    
    // Arrow keys - spatial grid navigation
    if (key === '\u001b[A') { // Up - move up one row
      const newFilteredIdx = currentFilteredIdx - tilesPerRow;
      if (newFilteredIdx >= 0) {
        this.selectedIndex = filteredToOriginal(newFilteredIdx);
      }
      this.render();
      return;
    }
    if (key === '\u001b[B') { // Down - move down one row
      const newFilteredIdx = currentFilteredIdx + tilesPerRow;
      if (newFilteredIdx < tests.length) {
        this.selectedIndex = filteredToOriginal(newFilteredIdx);
      }
      this.render();
      return;
    }
    if (key === '\u001b[D') { // Left
      if (currentFilteredIdx > 0) {
        this.selectedIndex = filteredToOriginal(currentFilteredIdx - 1);
      }
      this.render();
      return;
    }
    if (key === '\u001b[C') { // Right
      if (currentFilteredIdx < tests.length - 1) {
        this.selectedIndex = filteredToOriginal(currentFilteredIdx + 1);
      }
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
          // Initialize testParams from param defaults
          this.testParams = {};
          for (const param of test.params) {
            this.testParams[param.key] = String(param.default || '');
          }
          this.paramIndex = 0;
          // Initialize inputBuffer with first param in visual order (selects first, then ranges, then toggles)
          const visualParams = this.getVisualParamOrder(test.params);
          const firstParam = visualParams[0];
          this.inputBuffer = String(this.testParams[firstParam?.key] || '');
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
    const originalParams = this.currentTest?.params || [];
    // Use visual order for navigation (selects, ranges, toggles)
    const params = this.getVisualParamOrder(originalParams);
    const param = params[this.paramIndex];
    
    // Arrow Up - move to previous param
    if (key === '\u001b[A') {
      // Save current value first
      if (param && this.inputBuffer !== undefined) {
        this.testParams[param.key] = this.inputBuffer;
      }
      this.paramIndex = Math.max(0, this.paramIndex - 1);
      if (this.paramIndex < params.length) {
        this.inputBuffer = String(this.testParams[params[this.paramIndex]?.key] || params[this.paramIndex]?.default || '');
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
        this.inputBuffer = String(this.testParams[params[this.paramIndex]?.key] || params[this.paramIndex]?.default || '');
      } else {
        this.inputBuffer = '';
      }
      this.render();
      return;
    }
    
    // Arrow Left/Right - cycle through options or adjust values
    if (key === '\u001b[D' || key === '\u001b[C') { // Left or Right
      if (param && param.type === 'select' && param.options) {
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
      } else if (param && param.type === 'range') {
        // For range params, increment/decrement
        let val = parseInt(this.inputBuffer) || parseInt(param.default) || param.min || 0;
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
      } else if (param && param.type === 'toggle') {
        // Toggle between on/off
        const currentVal = this.testParams[param.key] || param.default || '';
        const newVal = currentVal ? '' : param.key;
        this.inputBuffer = newVal;
        this.testParams[param.key] = newVal;
        this.render();
        return;
      }
    }
    
    // Space bar also toggles for toggle params
    if (key === ' ' && param && param.type === 'toggle') {
      const currentVal = this.testParams[param.key] || param.default || '';
      const newVal = currentVal ? '' : param.key;
      this.inputBuffer = newVal;
      this.testParams[param.key] = newVal;
      this.render();
      return;
    }
    
    // Enter to confirm param or run test
    if (key === '\r' || key === '\n') {
      if (this.paramIndex < params.length && param) {
        // Save current param and move to next
        this.testParams[param.key] = this.inputBuffer || String(param.default || '');
        this.paramIndex++;
        if (this.paramIndex < params.length) {
          this.inputBuffer = String(this.testParams[params[this.paramIndex]?.key] || params[this.paramIndex]?.default || '');
        } else {
          this.inputBuffer = '';
        }
        this.render();
      } else {
        // Run the test with params
        const args = this.buildTestArgs(this.currentTest);
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
    
    // Regular character (only for non-select/toggle params)
    if (key.length === 1 && key.charCodeAt(0) >= 32) {
      if (param && (param.type === 'range' || !param.type)) {
        this.inputBuffer += key;
        this.render();
      }
    }
  }

  handleTestRunningInput(key) {
    // Tab - switch between panels
    if (key === '\t') {
      this.activePanel = this.activePanel === 'left' ? 'right' : 'left';
      this.render();
      return;
    }
    
    // Arrow Up - scroll up in active panel
    if (key === '\u001b[A') {
      const outputLines = this.testOutput || [];
      const logLines = this.testLogs || [];
      const contentHeight = this.height - 6;
      
      if (this.activePanel === 'left') {
        const maxScroll = Math.max(0, outputLines.length - contentHeight);
        if (this.leftScrollOffset > 0) {
          this.leftScrollOffset = Math.max(0, this.leftScrollOffset - 1);
          this.render();
        }
      } else {
        const maxScroll = Math.max(0, logLines.length - contentHeight);
        if (this.rightScrollOffset > 0) {
          this.rightScrollOffset = Math.max(0, this.rightScrollOffset - 1);
          this.render();
        }
      }
      return;
    }
    
    // Arrow Down - scroll down in active panel
    if (key === '\u001b[B') {
      const outputLines = this.testOutput || [];
      const logLines = this.testLogs || [];
      const contentHeight = this.height - 6;
      
      if (this.activePanel === 'left') {
        const maxScroll = Math.max(0, outputLines.length - contentHeight);
        if (this.leftScrollOffset < maxScroll) {
          this.leftScrollOffset = Math.min(maxScroll, this.leftScrollOffset + 1);
          this.render();
        }
      } else {
        const maxScroll = Math.max(0, logLines.length - contentHeight);
        if (this.rightScrollOffset < maxScroll) {
          this.rightScrollOffset = Math.min(maxScroll, this.rightScrollOffset + 1);
          this.render();
        }
      }
      return;
    }
    
    // Page Up - scroll up by page
    if (key === '\u001b[5~') {
      const contentHeight = this.height - 6;
      if (this.activePanel === 'left') {
        this.leftScrollOffset = Math.max(0, this.leftScrollOffset - contentHeight);
      } else {
        this.rightScrollOffset = Math.max(0, this.rightScrollOffset - contentHeight);
      }
      this.render();
      return;
    }
    
    // Page Down - scroll down by page
    if (key === '\u001b[6~') {
      const outputLines = this.testOutput || [];
      const logLines = this.testLogs || [];
      const contentHeight = this.height - 6;
      
      if (this.activePanel === 'left') {
        const maxScroll = Math.max(0, outputLines.length - contentHeight);
        this.leftScrollOffset = Math.min(maxScroll, this.leftScrollOffset + contentHeight);
      } else {
        const maxScroll = Math.max(0, logLines.length - contentHeight);
        this.rightScrollOffset = Math.min(maxScroll, this.rightScrollOffset + contentHeight);
      }
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

  // 🃏 KidLisp Cards Mode - Control card deck via CDP
  async enterKidLispCardsMode() {
    this.mode = 'kidlisp-cards';
    this.kidlispCardsSelectedIndex = 0;
    this.kidlispCardsOutput = 'Connecting to KidLisp.com...';
    this.render();
    
    // Find the kidlisp.com CDP page
    try {
      const pageId = await this.findKidLispCdpPage();
      if (pageId) {
        this.kidlispCardsCdpPageId = pageId;
        const status = await this.getKidLispCardStatus();
        this.kidlispCardsOutput = `✓ Connected to KidLisp.com\n${status}`;
        this.setStatus('KidLisp Cards ready!', 'success');
      } else {
        this.kidlispCardsOutput = '⚠ KidLisp.com window not open\nPress [O] to open it';
        this.setStatus('KidLisp.com not found', 'warn');
      }
    } catch (e) {
      this.kidlispCardsOutput = `✗ CDP error: ${e.message}\nIs VS Code remote debugging enabled?`;
      this.setStatus('CDP connection failed', 'error');
    }
    
    this.render();
  }
  
  async findKidLispCdpPage() {
    const http = await import('http');
    return new Promise((resolve) => {
      const req = http.get({
        hostname: 'host.docker.internal', port: 9222, path: '/json',
        headers: { 'Host': 'localhost' }, timeout: 2000
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => {
          try {
            const targets = JSON.parse(data);
            const kidlisp = targets.find(t => t.url?.includes('kidlisp.com'));
            resolve(kidlisp?.id || null);
          } catch { resolve(null); }
        });
      });
      req.on('error', () => resolve(null));
      req.on('timeout', () => { req.destroy(); resolve(null); });
    });
  }
  
  async evalKidLispJS(code) {
    if (!this.kidlispCardsCdpPageId) return null;
    const WebSocket = (await import('ws')).default;
    const ws = new WebSocket(
      `ws://host.docker.internal:9222/devtools/page/${this.kidlispCardsCdpPageId}`,
      { headers: { Host: 'localhost' } }
    );
    
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => { ws.close(); reject(new Error('Timeout')); }, 3000);
      ws.on('open', () => {
        ws.send(JSON.stringify({ id: 1, method: 'Runtime.evaluate', params: { expression: code } }));
      });
      ws.on('message', (data) => {
        clearTimeout(timeout);
        const msg = JSON.parse(data);
        ws.close();
        resolve(msg.result?.result?.value);
      });
      ws.on('error', (e) => { clearTimeout(timeout); reject(e); });
    });
  }
  
  async getKidLispCardStatus() {
    const mainCount = await this.evalKidLispJS('document.querySelectorAll(".book-frame").length') || 0;
    const discardCount = await this.evalKidLispJS('document.querySelectorAll(".discard-card").length') || 0;
    return `📚 Main Stack: ${mainCount} cards\n🗑️  Discard Pile: ${discardCount} cards`;
  }
  
  async kidlispCardsAction(action) {
    try {
      switch (action) {
        case 'next':
          await this.evalKidLispJS('document.querySelector(".book-stack")?.click()');
          this.kidlispCardsOutput = '▶ Advanced to next card\n' + await this.getKidLispCardStatus();
          break;
        case 'undo':
          await this.evalKidLispJS('document.querySelector(".discard-pile")?.click()');
          this.kidlispCardsOutput = '↩ Brought back card from discard\n' + await this.getKidLispCardStatus();
          break;
        case 'reset':
          // Click discard when main stack is empty, or keep clicking until reset
          const mainCount = await this.evalKidLispJS('document.querySelectorAll(".book-frame").length');
          if (mainCount === 0) {
            await this.evalKidLispJS('document.querySelector(".discard-pile")?.click()');
          }
          this.kidlispCardsOutput = '🔄 Reset deck\n' + await this.getKidLispCardStatus();
          break;
        case 'status':
          this.kidlispCardsOutput = await this.getKidLispCardStatus();
          break;
        case 'open':
          await this.openKidLisp();
          setTimeout(() => this.enterKidLispCardsMode(), 1500);
          return;
        case 'test':
          this.kidlispCardsOutput = '🧪 Running card tests...';
          this.render();
          const { execSync } = await import('child_process');
          try {
            const result = execSync('node artery/test-kidlisp.mjs card-basic', { 
              encoding: 'utf8', cwd: '/workspaces/aesthetic-computer', timeout: 15000 
            });
            this.kidlispCardsOutput = result.slice(-800);
          } catch (e) {
            this.kidlispCardsOutput = `Test error: ${e.message}`;
          }
          break;
      }
      this.setStatus('Action complete', 'success');
    } catch (e) {
      this.kidlispCardsOutput = `Error: ${e.message}`;
      this.setStatus('Action failed', 'error');
    }
    this.render();
  }
  
  handleKidLispCardsInput(key) {
    if (key === 'escape' || key === 'q') {
      this.mode = 'menu';
      this.render();
      return;
    }
    
    if (key === 'up' || key === 'k') {
      this.kidlispCardsSelectedIndex = Math.max(0, this.kidlispCardsSelectedIndex - 1);
    } else if (key === 'down' || key === 'j') {
      this.kidlispCardsSelectedIndex = Math.min(this.kidlispCardsMenu.length - 1, this.kidlispCardsSelectedIndex + 1);
    } else if (key === 'return') {
      const actions = ['next', 'undo', 'reset', 'status', 'open', 'test'];
      this.kidlispCardsAction(actions[this.kidlispCardsSelectedIndex]);
      return;
    } else {
      const item = this.kidlispCardsMenu.find(m => m.key === key.toLowerCase());
      if (item) {
        const actions = { n: 'next', u: 'undo', r: 'reset', s: 'status', o: 'open', t: 'test' };
        this.kidlispCardsAction(actions[key.toLowerCase()]);
        return;
      }
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
    
    // Load test configs from JSON
    const configs = await this.loadTestConfigs();
    
    // Auto-discover test files from artery/ directory
    const arteryTests = await this.discoverArteryTests();
    
    // Build test list from discovered files + legacy .vscode tests
    this.testFiles = [];
    
    // Add discovered artery tests (with configs if available)
    for (const file of arteryTests) {
      const config = configs.tests[file] || {};
      const baseName = file.replace('test-', '').replace('.mjs', '');
      this.testFiles.push({
        name: config.name || baseName,
        icon: config.icon || '▸',
        file: `artery/${file}`,
        desc: config.desc || `Artery test: ${baseName}`,
        isArtery: true,
        params: config.params ? this.expandParams(config.params, configs.presets) : null,
      });
    }
    
    // Add separator
    this.testFiles.push({ name: '───────────', file: null, desc: 'Legacy .vscode tests', isSeparator: true });
    
    // Legacy .vscode/tests
    for (const [file, config] of Object.entries(configs.legacy)) {
      this.testFiles.push({
        name: config.name,
        icon: config.icon || '▸',
        file: file,
        desc: config.desc,
        isArtery: false,
        params: config.params ? this.expandParams(config.params, configs.presets) : null,
      });
    }
    
    this.selectedIndex = 0;
    this.setStatus('Select a test to run (Enter for params)', 'info');
    this.render();
  }
  
  // Load test configs from JSON file
  async loadTestConfigs() {
    try {
      const configPath = path.join(path.dirname(new URL(import.meta.url).pathname), 'test-configs.json');
      const data = fs.readFileSync(configPath, 'utf-8');
      return JSON.parse(data);
    } catch (e) {
      this.addLog(`Failed to load test-configs.json: ${e.message}`, 'error');
      return { presets: {}, tests: {}, legacy: {} };
    }
  }
  
  // Expand params with preset references ($presetName)
  expandParams(params, presets) {
    const expanded = [];
    for (const [key, param] of Object.entries(params)) {
      const p = { key, ...param };
      
      // Expand preset references in options
      if (p.options && typeof p.options === 'string') {
        // Handle $preset+$preset syntax
        const parts = p.options.split('+').map(s => s.trim());
        p.options = parts.flatMap(part => {
          if (part.startsWith('$')) {
            const presetName = part.slice(1);
            return presets[presetName] || [];
          }
          return [part];
        });
      } else if (p.options && Array.isArray(p.options)) {
        // Expand any preset refs in array
        p.options = p.options.flatMap(opt => {
          if (typeof opt === 'string' && opt.startsWith('$')) {
            return presets[opt.slice(1)] || [];
          }
          return [opt];
        });
      }
      
      // Handle special default values
      if (p.default === 'now') {
        p.default = String(Date.now());
      }
      
      // Convert toggle defaults
      if (p.type === 'toggle') {
        p.options = ['', key]; // Toggle between empty and the key name
        p.default = p.default ? key : '';
      }
      
      expanded.push(p);
    }
    return expanded;
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
    // Switch to split-panel test running mode
    this.mode = 'test-running';
    this.testRunning = true;
    this.pendingRender = false;
    this.testOutput = []; // Left panel - stdout (generative output)
    this.testLogs = [];   // Right panel - stderr/console logs
    this.testFile = testFile;
    this.testArgs = args;
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
        lines.forEach(line => {
          this.testOutput.push({ text: line, time: this.getUTCTimeString() });
          // Keep output manageable
          if (this.testOutput.length > 100) this.testOutput.shift();
        });
        this.render();
      });
      
      proc.stderr.on('data', (data) => {
        const lines = data.toString().split('\n').filter(l => l.trim());
        lines.forEach(line => {
          this.testLogs.push({ text: line, time: this.getUTCTimeString(), level: 'error' });
          this.addLog(line, 'error'); // Also add to main logs
          if (this.testLogs.length > 100) this.testLogs.shift();
        });
        this.render();
      });
      
      proc.on('close', (code) => {
        this.testRunning = false;
        if (code === 0) {
          this.setStatus(`Test ${testFile} completed!`, 'success');
          this.testLogs.push({ text: `✓ Completed successfully`, time: this.getUTCTimeString(), level: 'success' });
        } else {
          this.setStatus(`Test ${testFile} exited with code ${code}`, 'error');
          this.testLogs.push({ text: `✗ Exited with code ${code}`, time: this.getUTCTimeString(), level: 'error' });
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

  // 🔮 Keeps (Tezos FA2) Mode - SIMPLIFIED
  async enterKeepsMode() {
    this.mode = 'keeps';
    this.keepsSelectedIndex = 0;
    this.keepsOutput = 'Ready. Press a key to run a command.';
    this.keepsRunning = false;
    this.keepsPieceInput = '';
    this.keepsSubMode = 'menu';
    this.loadAcAuth(); // Refresh auth status
    this.render();
    // Skip loading - just show the menu immediately
  }
  
  // Load AC auth token from ~/.ac-token
  loadAcAuth() {
    try {
      const tokenFile = path.join(os.homedir(), '.ac-token');
      if (fs.existsSync(tokenFile)) {
        const data = JSON.parse(fs.readFileSync(tokenFile, 'utf8'));
        const expired = data.expires_at && Date.now() > data.expires_at;
        this.acAuth = expired ? null : data;
      } else {
        this.acAuth = null;
      }
    } catch (e) {
      this.acAuth = null;
    }
  }
  
  // Login to AC (opens browser)
  async loginAc() {
    if (this.mode === 'keeps') {
      this.keepsOutput = '🌐 Opening browser for AC login...\n';
      this.keepsRunning = true;
    }
    this.render();
    
    const loginScript = path.join(process.cwd(), 'tezos/ac-login.mjs');
    
    return new Promise((resolve) => {
      const child = spawn('node', [loginScript], {
        cwd: process.cwd(),
        stdio: ['ignore', 'pipe', 'pipe'],
        env: { ...process.env, BROWSER: process.env.BROWSER || '' }
      });
      
      child.stdout.on('data', (data) => {
        if (this.mode === 'keeps') {
          this.keepsOutput += data.toString();
        }
        this.render();
      });
      
      child.stderr.on('data', (data) => {
        if (this.mode === 'keeps') {
          this.keepsOutput += data.toString();
        }
        this.render();
      });
      
      child.on('close', (code) => {
        if (this.mode === 'keeps') {
          this.keepsRunning = false;
        }
        this.loadAcAuth(); // Reload auth status
        if (code === 0 && this.acAuth) {
          const name = this.acAuth.user?.handle 
            ? `@${this.acAuth.user.handle}` 
            : this.acAuth.user?.email || 'unknown';
          if (this.mode === 'keeps') {
            this.keepsOutput += `\n✅ Logged in as ${name}\n`;
          }
        }
        this.render();
        resolve(code === 0);
      });
    });
  }
  
  // Logout from AC
  logoutAc() {
    try {
      const tokenFile = path.join(os.homedir(), '.ac-token');
      if (fs.existsSync(tokenFile)) {
        fs.unlinkSync(tokenFile);
      }
      this.acAuth = null;
      if (this.mode === 'keeps') {
        this.keepsOutput = '✅ Logged out from AC\n';
      }
    } catch (e) {
      if (this.mode === 'keeps') {
        this.keepsOutput = `❌ Logout error: ${e.message}\n`;
      }
    }
    this.render();
  }
  
  // Get auth display for header
  getAcAuthDisplay() {
    if (!this.acAuth) return '○ not logged in';
    const name = this.acAuth.user?.handle 
      ? `@${this.acAuth.user.handle}` 
      : this.acAuth.user?.email?.split('@')[0] || '?';
    return `● ${name}`;
  }
  
  // Toggle login/logout from main menu
  async toggleAcLogin() {
    if (this.acAuth) {
      this.logoutAc();
      this.setStatus('Logged out from AC', 'info');
    } else {
      this.setStatus('Opening browser for AC login...', 'info');
      await this.loginAc();
      if (this.acAuth) {
        const name = this.acAuth.user?.handle 
          ? `@${this.acAuth.user.handle}` 
          : this.acAuth.user?.email || 'unknown';
        this.setStatus(`Logged in as ${name}`, 'success');
      }
    }
    this.render();
  }
  
  // Start tezbot daemon if not running
  async ensureTezbot() {
    const tezbotScript = path.join(process.cwd(), 'tezos/ac-tezbot.mjs');
    const pidFile = '/tmp/tezbot.pid';
    
    // Check if already running
    if (fs.existsSync(pidFile)) {
      try {
        const pid = parseInt(fs.readFileSync(pidFile, 'utf8').trim());
        process.kill(pid, 0); // Check if process exists
        return; // Already running
      } catch (e) {
        // Stale PID file, remove it
        fs.unlinkSync(pidFile);
      }
    }
    
    // Start tezbot in background
    try {
      const child = spawn('node', [tezbotScript], {
        cwd: process.cwd(),
        detached: true,
        stdio: ['ignore', 'ignore', 'ignore']
      });
      child.unref();
      this.setStatus('🤖 Tezbot daemon started', 'success');
      // Give it a moment to initialize
      await new Promise(r => setTimeout(r, 300));
    } catch (e) {
      this.setStatus('⚠️ Could not start tezbot', 'warning');
    }
  }
  
  // Load contract data from TzKT API with timeout
  async loadKeepsData() {
    const fetchWithTimeout = (url, timeout = 5000) => {
      return Promise.race([
        fetch(url),
        new Promise((_, reject) => 
          setTimeout(() => reject(new Error('Request timeout')), timeout)
        )
      ]);
    };
    
    try {
      const contractAddress = this.keepsContractData.address;
      const network = this.keepsContractData.network;
      
      // Fetch contract storage from TzKT
      const storageUrl = `https://api.${network}.tzkt.io/v1/contracts/${contractAddress}/storage`;
      const storageRes = await fetchWithTimeout(storageUrl);
      if (storageRes.ok) {
        const storage = await storageRes.json();
        this.keepsContractData.tokenCount = parseInt(storage.next_token_id) || 0;
        this.keepsContractData.admin = storage.administrator?.slice(0, 8) + '...' || 'unknown';
        this.keepsEngineeringTasks.mintsCompleted = this.keepsContractData.tokenCount;
      }
      
      // Fetch recent tokens
      const tokensUrl = `https://api.${network}.tzkt.io/v1/contracts/${contractAddress}/bigmaps/token_metadata/keys?limit=5&sort.desc=id`;
      const tokensRes = await fetchWithTimeout(tokensUrl);
      if (tokensRes.ok) {
        const tokens = await tokensRes.json();
        this.keepsContractData.recentTokens = tokens.map(t => ({
          id: t.key,
          name: t.value?.token_info?.name ? Buffer.from(t.value.token_info.name, 'hex').toString() : `#${t.key}`,
          active: t.active,
        })).reverse();
      }
      
      // Fetch admin balance
      const adminAddr = 'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE'; // aesthetic.tez
      const balanceUrl = `https://api.${network}.tzkt.io/v1/accounts/${adminAddr}/balance`;
      const balanceRes = await fetchWithTimeout(balanceUrl);
      if (balanceRes.ok) {
        const balance = await balanceRes.json();
        this.keepsContractData.balance = (balance / 1_000_000).toFixed(2);
      }
      
      this.keepsContractData.lastUpdated = new Date().toLocaleTimeString();
      this.keepsOutput = 'Dashboard loaded. Ready.';
      
      // Parse implementation plan for tasks
      await this.loadKeepsEngineeringTasks();
      
    } catch (e) {
      this.keepsOutput = `⚠️ Could not load live data: ${e.message}`;
    }
    this.render();
  }
  
  // Parse engineering tasks from implementation plan
  async loadKeepsEngineeringTasks() {
    try {
      const planPath = path.join(process.cwd(), 'tezos/KEEPS-IMPLEMENTATION-PLAN.md');
      if (fs.existsSync(planPath)) {
        const content = fs.readFileSync(planPath, 'utf8');
        
        // Parse current phase
        if (content.includes('Phase A — Ghostnet hardening (IN PROGRESS)')) {
          this.keepsEngineeringTasks.phase = 'A';
          this.keepsEngineeringTasks.phaseName = 'Ghostnet Hardening';
        } else if (content.includes('Phase B') && content.includes('IN PROGRESS')) {
          this.keepsEngineeringTasks.phase = 'B';
          this.keepsEngineeringTasks.phaseName = 'Mainnet Staging';
        } else if (content.includes('Phase C') && content.includes('IN PROGRESS')) {
          this.keepsEngineeringTasks.phase = 'C';
          this.keepsEngineeringTasks.phaseName = 'Mainnet Production';
        }
        
        // Parse next steps (numbered list after "Next steps")
        const nextStepsMatch = content.match(/\*\*Next steps\*\*:\s*\n((?:\s+\d+\..+\n?)+)/);
        if (nextStepsMatch) {
          this.keepsEngineeringTasks.nextSteps = nextStepsMatch[1]
            .split('\n')
            .filter(l => /^\s+\d+\./.test(l))
            .map(l => l.replace(/^\s+\d+\.\s*/, '').trim())
            .slice(0, 5);
        }
        
        // Parse verify items
        const verifyMatch = content.match(/- Verify:\s*\n((?:\s+-.+\n?)+)/);
        if (verifyMatch) {
          this.keepsEngineeringTasks.currentFocus = verifyMatch[1]
            .split('\n')
            .filter(l => /^\s+-/.test(l))
            .map(l => l.replace(/^\s+-\s*/, '').trim())
            .slice(0, 4);
        }
      }
    } catch (e) {
      // Silently fail - not critical
    }
  }

  handleKeepsInput(key) {
    if (this.keepsRunning) {
      return;
    }
    
    if (this.keepsSubMode === 'piece-input') {
      if (key === '\r') {
        const piece = this.keepsPieceInput.trim();
        if (piece) {
          this.keepsSubMode = 'running';
          const action = this.keepsPendingAction || this.keepsMenu[this.keepsSelectedIndex].key;
          if (action === 'keep') {
            this.executeKeep(piece);
          } else {
            this.executeKeepsAction(action, piece);
          }
        }
        return;
      } else if (key === '\u007f' || key === '\b') {
        this.keepsPieceInput = this.keepsPieceInput.slice(0, -1);
      } else if (key === '\u001b') {
        this.keepsSubMode = 'menu';
        this.keepsPieceInput = '';
        this.keepsPendingAction = null;
      } else if (key.length === 1 && key >= ' ') {
        this.keepsPieceInput += key;
      }
      this.render();
      return;
    }
    
    // Confirm keep after preview
    if (this.keepsSubMode === 'confirm-keep') {
      if (key === '\r') {
        // User confirmed - proceed with mint
        const piece = this.keepsPendingPiece;
        this.keepsPendingPiece = null;
        this.keepsSubMode = 'running';
        this.executeKeepMint(piece);
        return;
      } else if (key === '\u001b' || key === 'n' || key === 'N') {
        // User cancelled
        this.keepsOutput = '❌ Keep cancelled\n';
        this.keepsPendingPiece = null;
        this.keepsSubMode = 'menu';
        this.render();
        return;
      }
      // Ignore other keys during confirmation
      return;
    }
    
    // Grid navigation (4 columns for wide, 3 for compact)
    const cols = this.width < 100 ? 3 : 4;
    const rows = Math.ceil(this.keepsMenu.length / cols);
    const currentRow = Math.floor(this.keepsSelectedIndex / cols);
    const currentCol = this.keepsSelectedIndex % cols;
    
    // Arrow key / vim navigation in grid
    if (key === '\u001b[A' || key === 'k') { // Up
      if (currentRow > 0) {
        this.keepsSelectedIndex = (currentRow - 1) * cols + currentCol;
      }
    } else if (key === '\u001b[B' || key === 'j') { // Down
      if (currentRow < rows - 1) {
        const newIdx = (currentRow + 1) * cols + currentCol;
        if (newIdx < this.keepsMenu.length) {
          this.keepsSelectedIndex = newIdx;
        }
      }
    } else if (key === '\u001b[D' || key === 'h') { // Left
      if (this.keepsSelectedIndex > 0) {
        this.keepsSelectedIndex--;
      }
    } else if (key === '\u001b[C' || key === 'l') { // Right
      if (this.keepsSelectedIndex < this.keepsMenu.length - 1) {
        this.keepsSelectedIndex++;
      }
    } else if (key === '\r') { // Enter
      const action = this.keepsMenu[this.keepsSelectedIndex];
      this.handleKeepsAction(action.key);
    } else {
      // Direct key press - check both exact case and lowercase
      const action = this.keepsMenu.find(m => m.key === key || m.key === key.toLowerCase());
      if (action) {
        this.keepsSelectedIndex = this.keepsMenu.indexOf(action);
        this.handleKeepsAction(action.key);
      }
    }
    this.render();
  }
  
  async handleKeepsAction(actionKey) {
    // Clear any existing sixel when taking a new action
    if (this.keepsSixel) {
      clearSixelArea(18, 15, this.width);
      this.keepsSixel = null;
    }
    
    // Special actions that don't run keeps.mjs
    switch (actionKey) {
      case 'f': // Fishy - jump to terminal
        await this.keepsJumpToFishy();
        return;
      case 'e': // Explorer
        this.keepsOpenExplorer();
        return;
      case 'o': // Objkt
        this.keepsOpenObjkt();
        return;
      case 'r': // Run tests
        this.keepsSubMode = 'running';
        await this.keepsRunTests();
        return;
      case 'L': // Login/Logout
        if (this.acAuth) {
          this.logoutAc();
        } else {
          await this.loginAc();
        }
        return;
      case 'K': // Keep via AC server (requires login)
        if (!this.acAuth) {
          this.keepsOutput = '❌ Must be logged in to keep. Press L to login.\n';
          this.render();
          return;
        }
        this.keepsSubMode = 'piece-input';
        this.keepsPieceInput = this.currentPiece || '';
        this.keepsPendingAction = 'keep'; // Track which action needs the piece
        return;
    }
    
    // Actions that need piece input
    if (actionKey === 'u' || actionKey === 'm' || actionKey === 'l') {
      this.keepsSubMode = 'piece-input';
      this.keepsPieceInput = this.currentPiece || '';
      this.keepsPendingAction = actionKey; // Track pending action
      return;
    }
    
    // Standard keeps.mjs actions
    this.keepsSubMode = 'running';
    await this.executeKeepsAction(actionKey);
  }

  async executeKeep(piece) {
    this.keepsRunning = true;
    this.keepsOutput = '� Preview: Opening piece in AC panel...\n';
    this.render();
    
    if (!this.acAuth?.access_token) {
      this.keepsOutput = '❌ Not logged in. Press L to login first.';
      this.keepsRunning = false;
      this.keepsSubMode = 'menu';
      this.render();
      return;
    }
    
    // Step 1: Preview the piece in AC panel
    try {
      if (this.connected && this.client) {
        await this.client.jump(piece);
        this.currentPiece = piece;
        this.keepsOutput += `✅ Previewing $${piece} in AC panel\n`;
        this.keepsOutput += `\n📋 Press ENTER to confirm mint, ESC to cancel\n`;
        this.keepsSubMode = 'confirm-keep';
        this.keepsPendingPiece = piece;
        this.keepsRunning = false;
        this.render();
        return;
      } else {
        this.keepsOutput += '⚠️  AC panel not connected - minting without preview\n';
      }
    } catch (e) {
      this.keepsOutput += `⚠️  Preview failed: ${e.message} - continuing anyway\n`;
    }
    
    // If preview failed or no connection, mint directly
    await this.executeKeepMint(piece);
  }
  
  async executeKeepMint(piece) {
    this.keepsRunning = true;
    this.keepsOutput += '🔒 Keep: Connecting to server...\n';
    this.render();
    
    // Use local dev server when available (supports https with self-signed cert)
    const apiUrl = 'https://localhost:8888/api/keep-mint';
    
    try {
      // Enable fetch for self-signed certs in development
      process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';
      const response = await fetch(apiUrl, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${this.acAuth.access_token}`,
          'Content-Type': 'application/json',
          'Accept': 'text/event-stream',
        },
        // Use mode: "mint" for server-side minting (server wallet pays gas, mints to user)
        body: JSON.stringify({ piece, mode: 'mint' }),
      });
      
      if (!response.ok) {
        const text = await response.text();
        this.keepsOutput = `❌ Server error (${response.status}): ${text}`;
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.render();
        return;
      }
      
      // Parse SSE stream
      const reader = response.body.getReader();
      const decoder = new TextDecoder();
      let buffer = '';
      let tokenId = null;
      let currentEvent = 'progress';
      
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        
        buffer += decoder.decode(value, { stream: true });
        const lines = buffer.split('\n');
        buffer = lines.pop() || '';
        
        for (const line of lines) {
          // Track event type
          if (line.startsWith('event: ')) {
            currentEvent = line.slice(7).trim();
            continue;
          }
          
          if (line.startsWith('data: ')) {
            try {
              const data = JSON.parse(line.slice(6));
              
              // Handle error events
              if (currentEvent === 'error' || data.error) {
                this.keepsOutput += `❌ Error: ${data.error}\n`;
                if (data.tokenId) {
                  this.keepsOutput += `   Already minted as token #${data.tokenId}\n`;
                  this.keepsOutput += `   View: ${data.objktUrl}\n`;
                  if (data.name) this.keepsOutput += `   Name: ${data.name}\n`;
                  if (data.minter) this.keepsOutput += `   Minter: ${data.minter}\n`;
                  
                  // Show sixel thumbnail if available
                  if (data.thumbnailUri) {
                    this.keepsOutput += `   📷 Fetching thumbnail...\n`;
                    this.render();
                    const imageData = await fetchTerminalImage(data.thumbnailUri, 20, 10);
                    if (imageData) {
                      // Store sixel for rendering after TUI completes
                      this.keepsSixel = imageData;
                      this.keepsOutput = this.keepsOutput.replace('📷 Fetching thumbnail...', '✅ Thumbnail below:');
                    } else {
                      this.keepsOutput = this.keepsOutput.replace('📷 Fetching thumbnail...', '❌ Thumbnail unavailable');
                    }
                  }
                }
                this.setStatus('Keep failed: ' + data.error, 'error');
              } else if (data.stage) {
                const icon = data.success === false ? '❌' : 
                            data.done ? '✅' : '⏳';
                this.keepsOutput += `${icon} ${data.stage}`;
                if (data.message) this.keepsOutput += `: ${data.message}`;
                this.keepsOutput += '\n';
                
                if (data.tokenId) tokenId = data.tokenId;
                if (data.opHash) this.keepsOutput += `   → Op: ${data.opHash}\n`;
              }
              
              this.render();
            } catch (e) {
              // Non-JSON data, skip
            }
          }
        }
      }
      
      if (tokenId) {
        this.keepsOutput += `\n🎉 Minted as token #${tokenId}!\n`;
        this.keepsOutput += `   View: https://ghostnet.objkt.com/tokens/keeps-${tokenId}\n`;
        this.setStatus(`Kept "${piece}" as token #${tokenId}`, 'success');
        this.loadKeepsData();
      }
      
    } catch (err) {
      this.keepsOutput = `❌ Error: ${err.message}`;
      this.setStatus('Keep failed', 'error');
    }
    
    this.keepsRunning = false;
    this.keepsSubMode = 'menu';
    this.keepsPendingAction = null;
    this.render();
  }

  async executeKeepsAction(actionKey, piece = null) {
    this.keepsRunning = true;
    this.keepsOutput = '⏳ Running...\n';
    this.render();
    
    // Quick ops go through tezbot daemon (non-blocking)
    const quickOps = ['b', 's', 't'];
    if (quickOps.includes(actionKey)) {
      return this.executeViaTezbot(actionKey);
    }
    
    // Longer ops (deploy, mint, upload, lock) run keeps.mjs directly for live output
    const keepsScript = path.join(process.cwd(), 'tezos/keeps.mjs');
    
    let args;
    switch (actionKey) {
      case 'd':
        args = ['deploy'];
        break;
      case 'u':
        args = ['upload', piece];
        break;
      case 'm':
        args = ['mint', piece];
        break;
      case 'l':
        args = ['lock', piece];
        break;
      default:
        this.keepsOutput = '❌ Unknown action';
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.render();
        return;
    }
    
    const timeoutMs = 120000; // 2 min for longer ops
    
    // Use spawn for non-blocking execution with live output
    return new Promise((resolve) => {
      let child;
      let output = '';
      let resolved = false;
      
      const cleanup = () => {
        if (resolved) return;
        resolved = true;
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.render();
        resolve();
      };
      
      try {
        child = spawn('node', [keepsScript, ...args.filter(Boolean)], {
          cwd: process.cwd(),
          env: process.env,
          stdio: ['ignore', 'pipe', 'pipe'],
        });
      } catch (err) {
        this.keepsOutput = `❌ Failed to spawn: ${err.message}`;
        cleanup();
        return;
      }
      
      child.stdout.on('data', (data) => {
        output += data.toString();
        this.keepsOutput = output;
        this.render();
      });
      
      child.stderr.on('data', (data) => {
        output += data.toString();
        this.keepsOutput = output;
        this.render();
      });
      
      child.on('close', (code) => {
        if (resolved) return;
        if (code === 0) {
          this.setStatus('Keeps operation completed', 'success');
          // Refresh data after successful operation
          if (['m', 'd', 'l'].includes(actionKey)) {
            this.loadKeepsData();
          }
        } else {
          this.setStatus('Keeps operation failed', 'error');
        }
        cleanup();
      });
      
      child.on('error', (err) => {
        if (resolved) return;
        this.keepsOutput = `❌ Error: ${err.message}`;
        this.setStatus('Keeps operation failed', 'error');
        cleanup();
      });
      
      // Timeout - shorter for network ops
      setTimeout(() => {
        if (!resolved) {
          try { child.kill('SIGKILL'); } catch (e) {}
          const timeoutSec = timeoutMs / 1000;
          this.keepsOutput += `\n\n❌ Operation timed out (${timeoutSec}s)`;
          this.setStatus('Keeps operation timed out', 'error');
          cleanup();
        }
      }, timeoutMs);
    });
  }
  
  // Execute quick ops via tezbot daemon (non-blocking)
  // SIMPLIFIED: Run keeps.mjs directly with spawn to avoid IPC complexity
  async executeViaTezbot(actionKey) {
    const actionMap = {
      'b': 'balance',
      's': 'status', 
      't': 'tokens'
    };
    
    const action = actionMap[actionKey];
    if (!action) {
      this.keepsOutput = '❌ Unknown action';
      this.keepsRunning = false;
      this.keepsSubMode = 'menu';
      this.render();
      return;
    }
    
    this.keepsOutput = `⏳ Running ${action}...\n`;
    this.render();
    
    const keepsScript = path.join(process.cwd(), 'tezos/keeps.mjs');
    
    return new Promise((resolve) => {
      let output = '';
      let resolved = false;
      
      const cleanup = () => {
        if (resolved) return;
        resolved = true;
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.render();
        resolve();
      };
      
      let child;
      try {
        child = spawn('node', [keepsScript, action], {
          cwd: process.cwd(),
          env: process.env,
          stdio: ['ignore', 'pipe', 'pipe'],
        });
      } catch (err) {
        this.keepsOutput = `❌ Failed: ${err.message}`;
        cleanup();
        return;
      }
      
      child.stdout.on('data', (data) => {
        output += data.toString();
      });
      
      child.stderr.on('data', (data) => {
        output += data.toString();
      });
      
      child.on('close', (code) => {
        if (resolved) return;
        this.keepsOutput = output.trim() || (code === 0 ? '✅ Done' : '❌ Failed');
        if (code === 0) {
          this.setStatus(`${action} completed`, 'success');
        } else {
          this.setStatus(`${action} failed`, 'error');
        }
        cleanup();
      });
      
      child.on('error', (err) => {
        if (resolved) return;
        this.keepsOutput = `❌ Error: ${err.message}`;
        cleanup();
      });
      
      // 30s timeout for network calls
      setTimeout(() => {
        if (!resolved) {
          try { child.kill('SIGKILL'); } catch (e) {}
          this.keepsOutput = output + '\n\n❌ Timeout (30s)';
          cleanup();
        }
      }, 30000);
    });
  }
  
  async keepsJumpToFishy() {
    // Switch to fishy buffer via emacsclient (with timeout to prevent hangs)
    try {
      execSync('emacsclient -e \'(switch-to-buffer "🐟-fishy")\'', { encoding: 'utf8', timeout: 3000 });
      this.setStatus('Jumped to 🐟-fishy', 'success');
    } catch (e) {
      this.keepsOutput = `⚠️ Could not switch to fishy: ${e.message}\nTry: emacsclient -e '(switch-to-buffer "🐟-fishy")'`;
    }
    this.render();
  }
  
  keepsOpenExplorer() {
    const url = `https://${this.keepsContractData.network}.tzkt.io/${this.keepsContractData.address}`;
    try {
      execSync(`"$BROWSER" "${url}" 2>/dev/null || xdg-open "${url}" 2>/dev/null &`, { shell: true, timeout: 3000 });
      this.keepsOutput = `🔗 Opened: ${url}`;
      this.setStatus('Opened tzkt explorer', 'success');
    } catch (e) {
      this.keepsOutput = `🔗 Explorer: ${url}`;
    }
    this.render();
  }
  
  keepsOpenObjkt() {
    const url = `https://${this.keepsContractData.network}.objkt.com/collection/${this.keepsContractData.address}`;
    try {
      execSync(`"$BROWSER" "${url}" 2>/dev/null || xdg-open "${url}" 2>/dev/null &`, { shell: true, timeout: 3000 });
      this.keepsOutput = `🔗 Opened: ${url}`;
      this.setStatus('Opened objkt collection', 'success');
    } catch (e) {
      this.keepsOutput = `🔗 Objkt: ${url}`;
    }
    this.render();
  }
  
  async keepsRunTests() {
    this.keepsRunning = true;
    this.keepsOutput = '🧪 Running Keeps test suite...\n\n';
    this.render();
    
    // Run a simple status check as a "test" using spawn (non-blocking)
    const keepsScript = path.join(process.cwd(), 'tezos/keeps.mjs');
    
    return new Promise((resolve) => {
      const child = spawn('node', [keepsScript, 'status'], {
        cwd: process.cwd(),
        env: process.env,
      });
      
      let result = '';
      
      child.stdout.on('data', (data) => {
        result += data.toString();
        this.keepsOutput = '🧪 Running Keeps test suite...\n\n' + result;
        this.render();
      });
      
      child.stderr.on('data', (data) => {
        result += data.toString();
        this.keepsOutput = '🧪 Running Keeps test suite...\n\n' + result;
        this.render();
      });
      
      child.on('close', (code) => {
        if (code === 0) {
          this.keepsOutput = '🧪 Test Results:\n\n';
          this.keepsOutput += '✅ Contract accessible\n';
          this.keepsOutput += `✅ Token count: ${this.keepsContractData.tokenCount}\n`;
          this.keepsOutput += `✅ Network: ${this.keepsContractData.network}\n`;
          this.keepsOutput += '\n' + result;
          
          // Check progress toward 20 mints
          const progress = Math.min(100, (this.keepsContractData.tokenCount / 20) * 100);
          this.keepsOutput += `\n📊 Progress: ${this.keepsContractData.tokenCount}/20 test mints (${progress.toFixed(0)}%)`;
          
          this.setStatus('Tests passed', 'success');
        } else {
          this.keepsOutput = `❌ Test failed (exit code ${code})\n\n${result}`;
          this.setStatus('Tests failed', 'error');
        }
        
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.render();
        resolve();
      });
      
      child.on('error', (err) => {
        this.keepsOutput = `❌ Test error: ${err.message}`;
        this.keepsRunning = false;
        this.keepsSubMode = 'menu';
        this.setStatus('Tests failed', 'error');
        this.render();
        resolve();
      });
      
      // Timeout after 30 seconds
      setTimeout(() => {
        if (this.keepsRunning) {
          child.kill();
          this.keepsOutput += '\n\n❌ Test timed out (30s)';
          this.keepsRunning = false;
          this.keepsSubMode = 'menu';
          this.render();
          resolve();
        }
      }, 30000);
    });
  }

  renderKeeps() {
    const boxWidth = this.innerWidth;
    
    // Auth status
    const authDisplay = this.getAcAuthDisplay();
    const authColor = this.acAuth ? FG_GREEN : FG_BRIGHT_RED;
    
    // Simple header with auth
    this.writeLine(`${BG_MAGENTA}${FG_WHITE}${BOLD} 🔮 KEEPS - Tezos FA2 ${'─'.repeat(Math.max(0, boxWidth - 25))}${RESET}`);
    const headerInfo = `Network: ghostnet | AC: ${authDisplay}`;
    this.writeLine(`${BG_MAGENTA}${FG_WHITE} ${headerInfo}${' '.repeat(Math.max(0, boxWidth - headerInfo.length - 2))}${RESET}`);
    this.writeLine(`${BG_BLACK}${' '.repeat(boxWidth)}${RESET}`);
    
    // Actions - simple list (with login/logout)
    this.writeLine(`${BG_BRIGHT_BLACK}${FG_WHITE}${BOLD} ACTIONS ${' '.repeat(Math.max(0, boxWidth - 10))}${RESET}`);
    
    const actions = [
      ['s', 'Status'], ['b', 'Balance'], ['t', 'Tokens'], ['K', 'Keep'],
      ['m', 'Mint (Local)'], ['u', 'Upload'], ['l', 'Lock'], ['d', 'Deploy'],
      ['e', 'Explorer'], ['o', 'Objkt'], ['r', 'Run Tests'], ['f', 'Fishy'],
      this.acAuth ? ['L', 'Logout'] : ['L', 'Login']
    ];
    
    // Render in rows of 4
    for (let i = 0; i < actions.length; i += 4) {
      let row = '';
      for (let j = 0; j < 4 && i + j < actions.length; j++) {
        const [key, label] = actions[i + j];
        const idx = i + j;
        const selected = idx === this.keepsSelectedIndex;
        const actionWidth = Math.floor(boxWidth / 4);
        const text = `[${key}] ${label}`;
        const pad = Math.max(0, actionWidth - text.length - 1);
        
        if (selected) {
          row += `${BG_CYAN}${FG_BLACK}${BOLD}▶${text}${' '.repeat(pad)}${RESET}`;
        } else {
          row += `${BG_BRIGHT_BLACK}${FG_WHITE} ${text}${' '.repeat(pad)}${RESET}`;
        }
      }
      this.writeLine(row);
    }
    
    // Piece input overlay
    if (this.keepsSubMode === 'piece-input') {
      this.writeLine(`${BG_YELLOW}${FG_BLACK}${BOLD} Piece: ${this.keepsPieceInput}█${' '.repeat(Math.max(0, boxWidth - 10 - this.keepsPieceInput.length))}${RESET}`);
    }
    
    // Output
    this.writeLine(`${BG_BLACK}${FG_CYAN}${BOLD} OUTPUT ${' '.repeat(Math.max(0, boxWidth - 9))}${RESET}`);
    
    const outputLines = (this.keepsOutput || '').split('\n').slice(-15); // Last 15 lines
    for (const line of outputLines) {
      const truncated = line.length > boxWidth - 2 ? line.slice(0, boxWidth - 5) + '...' : line;
      this.writeLine(`${BG_BLACK}${FG_WHITE} ${truncated}${' '.repeat(Math.max(0, boxWidth - truncated.length - 2))}${RESET}`);
    }
    
    // Fill remaining
    const used = 4 + Math.ceil(actions.length / 4) + (this.keepsSubMode === 'piece-input' ? 1 : 0) + 1 + outputLines.length;
    for (let i = used; i < this.innerHeight - 2; i++) {
      this.writeLine(`${BG_BLACK}${' '.repeat(boxWidth)}${RESET}`);
    }
    
    // Status
    const status = this.keepsRunning ? '⏳ Running...' : '✓ Ready';
    this.writeLine(`${BG_BRIGHT_BLACK}${FG_WHITE} ${status} | ESC=back ${' '.repeat(Math.max(0, boxWidth - 25))}${RESET}`);
  }
  
  renderKeepsOLD() {
    const boxWidth = this.innerWidth;
    const compact = this.width < 100;
    const colWidth = compact ? boxWidth : Math.floor((boxWidth - 1) / 2);
    
    // Theme colors - magenta/purple for Tezos
    const KEEPS_BG = BG_MAGENTA;
    const KEEPS_FG = FG_WHITE;
    const KEEPS_ACCENT = FG_BRIGHT_YELLOW;
    const KEEPS_DIM = `${BG_MAGENTA}${DIM}${FG_WHITE}`;
    const KEEPS_BORDER = `${BG_MAGENTA}${FG_BRIGHT_CYAN}`;
    
    // Header with full background
    this.writeLine(`${KEEPS_BG}${KEEPS_FG}${'═'.repeat(boxWidth)}${RESET}`);
    const title = '🔮 KEEPS - Tezos FA2 Development Workbench';
    const padding = Math.max(0, boxWidth - title.length - 2);
    this.writeLine(`${KEEPS_BG}${KEEPS_FG}${BOLD} ${title}${' '.repeat(padding)}${RESET}`);
    this.writeLine(`${KEEPS_BG}${KEEPS_FG}${'═'.repeat(boxWidth)}${RESET}`);
    
    const cd = this.keepsContractData;
    const et = this.keepsEngineeringTasks;
    
    if (!compact) {
      // Two-column layout with consistent background
      this.writeLine(`${KEEPS_BG}${'─'.repeat(colWidth)}┬${'─'.repeat(colWidth - 1)}${RESET}`);
      
      // Build left column (CONTRACT)
      const leftLines = [
        `${KEEPS_ACCENT}${BOLD} CONTRACT${RESET}`,
        `${KEEPS_DIM} Network:${RESET}${KEEPS_BG}  ${FG_BRIGHT_GREEN}${cd.network}${RESET}`,
        `${KEEPS_DIM} Address:${RESET}${KEEPS_BG}  ${KEEPS_FG}${cd.address.slice(0, 15)}...${RESET}`,
        `${KEEPS_DIM} Admin:${RESET}${KEEPS_BG}    ${KEEPS_FG}${cd.admin}${RESET}`,
        `${KEEPS_DIM} Tokens:${RESET}${KEEPS_BG}   ${FG_BRIGHT_YELLOW}${BOLD}${cd.tokenCount}${RESET}${KEEPS_BG} minted${RESET}`,
        `${KEEPS_DIM} Balance:${RESET}${KEEPS_BG}  ${FG_BRIGHT_GREEN}~${cd.balance} XTZ${RESET}`,
        `${KEEPS_BG}${RESET}`,
        `${KEEPS_BG}${FG_BRIGHT_CYAN} Recent Tokens:${RESET}`,
      ];
      
      // Add tokens
      if (cd.recentTokens.length > 0) {
        cd.recentTokens.slice(0, 3).forEach(t => {
          const status = t.active ? `${FG_BRIGHT_GREEN}●${RESET}` : `${FG_RED}○${RESET}`;
          leftLines.push(`${KEEPS_BG}  ${status}${KEEPS_BG} ${DIM}[${t.id}]${RESET}${KEEPS_BG} ${KEEPS_FG}${t.name}${RESET}`);
        });
      } else {
        leftLines.push(`${KEEPS_BG}${DIM}  (none)${RESET}`);
      }
      
      // Build right column (ENGINEERING)
      const rightLines = [
        `${KEEPS_BG}${FG_BRIGHT_MAGENTA}${BOLD} PHASE ${et.phase}${RESET}${KEEPS_BG}${KEEPS_FG} ${et.phaseName}${RESET}`,
        `${KEEPS_BG}${RESET}`,
        `${KEEPS_BG}${FG_BRIGHT_CYAN} Progress to Phase B:${RESET}`,
        `${KEEPS_BG} ${this.renderProgressBar(et.mintsCompleted, 20, colWidth - 4)}${RESET}`,
        `${KEEPS_BG}${KEEPS_FG}  ${et.mintsCompleted}/20 test mints${RESET}`,
        `${KEEPS_BG}${RESET}`,
        `${KEEPS_BG}${FG_BRIGHT_CYAN} Next Steps:${RESET}`,
      ];
      
      // Add next steps
      et.nextSteps.slice(0, 3).forEach((step, i) => {
        const truncated = step.length > colWidth - 6 ? step.slice(0, colWidth - 9) + '...' : step;
        rightLines.push(`${KEEPS_BG}${KEEPS_FG}  ${i + 1}. ${truncated}${RESET}`);
      });
      
      // Pad to same length
      const maxLines = Math.max(leftLines.length, rightLines.length);
      while (leftLines.length < maxLines) leftLines.push(`${KEEPS_BG}${RESET}`);
      while (rightLines.length < maxLines) rightLines.push(`${KEEPS_BG}${RESET}`);
      
      // Render two columns
      for (let i = 0; i < maxLines; i++) {
        const leftContent = leftLines[i];
        const rightContent = rightLines[i];
        const leftVisible = this.stripAnsi(leftContent).length;
        const rightVisible = this.stripAnsi(rightContent).length;
        const leftPad = ' '.repeat(Math.max(0, colWidth - leftVisible - 1));
        const rightPad = ' '.repeat(Math.max(0, colWidth - rightVisible - 1));
        this.writeLine(`${leftContent}${KEEPS_BG}${leftPad}│${RESET}${rightContent}${KEEPS_BG}${rightPad}${RESET}`);
      }
      
      this.writeLine(`${KEEPS_BG}${'─'.repeat(colWidth)}┴${'─'.repeat(colWidth - 1)}${RESET}`);
    } else {
      // Compact single-column
      this.writeLine(`${KEEPS_BG}${KEEPS_FG} Contract: ${cd.address.slice(0, 18)}...${' '.repeat(Math.max(0, boxWidth - 32))}${RESET}`);
      this.writeLine(`${KEEPS_BG}${KEEPS_FG} Tokens: ${KEEPS_ACCENT}${cd.tokenCount}${KEEPS_FG}  Balance: ${FG_BRIGHT_GREEN}${cd.balance} XTZ${' '.repeat(Math.max(0, boxWidth - 35))}${RESET}`);
      this.writeLine(`${KEEPS_BG}${'─'.repeat(boxWidth)}${RESET}`);
    }
    
    // Actions section with arrow key navigation
    this.writeLine(`${BG_BRIGHT_BLACK}${FG_WHITE}${BOLD} ACTIONS ${RESET}${BG_BRIGHT_BLACK}${' '.repeat(boxWidth - 10)}${RESET}`);
    
    // Render actions in a grid with visual selection
    const cols = compact ? 3 : 4;
    const actionWidth = Math.floor(boxWidth / cols);
    
    for (let row = 0; row < Math.ceil(this.keepsMenu.length / cols); row++) {
      let rowStr = '';
      for (let col = 0; col < cols; col++) {
        const idx = row * cols + col;
        if (idx >= this.keepsMenu.length) {
          rowStr += `${BG_BRIGHT_BLACK}${' '.repeat(actionWidth)}${RESET}`;
          continue;
        }
        
        const item = this.keepsMenu[idx];
        const selected = idx === this.keepsSelectedIndex;
        
        if (selected) {
          // Highlighted selection with cyan background
          const label = `▶[${item.key}] ${item.label}`;
          const pad = Math.max(0, actionWidth - label.length);
          rowStr += `${BG_CYAN}${FG_BLACK}${BOLD}${label}${' '.repeat(pad)}${RESET}`;
        } else {
          const label = ` [${item.key}] ${item.label}`;
          const pad = Math.max(0, actionWidth - label.length);
          rowStr += `${BG_BRIGHT_BLACK}${FG_WHITE}${label}${' '.repeat(pad)}${RESET}`;
        }
      }
      this.writeLine(rowStr);
    }
    
    // Piece input overlay
    if (this.keepsSubMode === 'piece-input') {
      this.writeLine(`${BG_YELLOW}${FG_BLACK}${BOLD} INPUT: ${this.keepsPieceInput}█${' '.repeat(Math.max(0, boxWidth - 10 - this.keepsPieceInput.length))}${RESET}`);
      this.writeLine(`${BG_YELLOW}${FG_BLACK}${DIM} Current piece: ${this.currentPiece || 'none'}${' '.repeat(Math.max(0, boxWidth - 20 - (this.currentPiece?.length || 4)))}${RESET}`);
    }
    
    // Output panel with scrolling
    const outputTitle = ` OUTPUT ${this.keepsContractData.lastUpdated ? `(${this.keepsContractData.lastUpdated})` : ''}`;
    this.writeLine(`${BG_BLACK}${FG_BRIGHT_CYAN}${BOLD}${outputTitle}${' '.repeat(Math.max(0, boxWidth - outputTitle.length))}${RESET}`);
    
    const outputLines = (this.keepsOutput || '').split('\n');
    const maxOutputLines = Math.max(4, this.innerHeight - (compact ? 18 : 22));
    const startLine = Math.max(0, outputLines.length - maxOutputLines);
    
    // Show scroll indicator if truncated
    if (startLine > 0) {
      this.writeLine(`${BG_BLACK}${DIM}${FG_CYAN} ↑ ${startLine} more lines above...${' '.repeat(Math.max(0, boxWidth - 25))}${RESET}`);
    }
    
    for (let i = startLine; i < outputLines.length; i++) {
      let line = outputLines[i];
      let bg = BG_BLACK;
      let fg = FG_WHITE;
      
      // Color code by content
      if (line.includes('✅') || line.includes('✓') || line.includes('Success')) {
        bg = BG_BLACK; fg = FG_BRIGHT_GREEN;
      } else if (line.includes('❌') || line.includes('Error') || line.includes('failed')) {
        bg = BG_RED; fg = FG_WHITE;
      } else if (line.includes('⏳') || line.includes('Running') || line.includes('...')) {
        bg = BG_YELLOW; fg = FG_BLACK;
      } else if (line.includes('📍') || line.includes('🔗') || line.includes('💰') || line.includes('📊') || line.includes('🧪')) {
        bg = BG_BLACK; fg = FG_BRIGHT_CYAN;
      } else if (line.includes('║') || line.includes('╔') || line.includes('╚')) {
        bg = BG_BLACK; fg = FG_CYAN;
      }
      
      // Truncate and pad line
      const visibleLine = this.stripAnsi(line);
      if (visibleLine.length > boxWidth - 2) {
        line = line.slice(0, boxWidth - 5) + '...';
      }
      const linePad = Math.max(0, boxWidth - visibleLine.length - 1);
      this.writeLine(`${bg}${fg} ${line}${' '.repeat(linePad)}${RESET}`);
    }
    
    // Fill remaining output space
    const usedLines = Math.min(outputLines.length - startLine, maxOutputLines) + (startLine > 0 ? 1 : 0);
    for (let i = usedLines; i < maxOutputLines; i++) {
      this.writeLine(`${BG_BLACK}${' '.repeat(boxWidth)}${RESET}`);
    }
    
    // Status bar
    const statusText = this.keepsRunning 
      ? `${BG_YELLOW}${FG_BLACK}${BOLD} ⏳ RUNNING... ${RESET}` 
      : `${BG_GREEN}${FG_BLACK}${BOLD} ● READY ${RESET}`;
    const helpText = `${DIM}ESC=Back  ←→↑↓=Navigate  Enter=Select  f=🐟${RESET}`;
    const statusPad = Math.max(0, boxWidth - 12 - this.stripAnsi(helpText).length);
    this.writeLine(`${statusText}${BG_BRIGHT_BLACK}${' '.repeat(statusPad)}${helpText}${RESET}`);
  }
  
  // Helper: render a progress bar
  renderProgressBar(current, max, width) {
    const barWidth = Math.max(10, width - 2);
    const filled = Math.round((current / max) * barWidth);
    const empty = Math.max(0, barWidth - filled);
    const pct = Math.min(100, Math.round((current / max) * 100));
    const color = pct >= 100 ? FG_BRIGHT_GREEN : pct >= 50 ? FG_BRIGHT_YELLOW : FG_BRIGHT_RED;
    const filledChar = '█';
    const emptyChar = '░';
    return `${color}[${filledChar.repeat(filled)}${DIM}${emptyChar.repeat(empty)}${RESET}${color}] ${pct}%${RESET}`;
  }
  
  // Helper: pad string to width (strips ANSI for calculation)
  padRight(str, width) {
    const visibleLen = this.stripAnsi(str).length;
    const padding = Math.max(0, width - visibleLen);
    return str + ' '.repeat(padding);
  }

  // 🃏 Render KidLisp Cards Mode
  renderKidLispCards() {
    const boxWidth = this.innerWidth;
    
    // Header
    this.writeLine(`${DOS_TITLE}${'═'.repeat(boxWidth)}${RESET}`);
    this.writeLine(`${DOS_TITLE}  🃏 KidLisp Cards - CDP Remote Control${' '.repeat(Math.max(0, boxWidth - 41))}${RESET}`);
    this.writeLine(`${DOS_TITLE}${'═'.repeat(boxWidth)}${RESET}`);
    this.writeLine('');
    
    // Connection status
    const connStatus = this.kidlispCardsCdpPageId 
      ? `${FG_BRIGHT_GREEN}● Connected${RESET}` 
      : `${FG_BRIGHT_RED}○ Disconnected${RESET}`;
    this.writeLine(`${FG_CYAN}CDP Status: ${connStatus}${BG_BLUE}  ${DIM}Page: ${this.kidlispCardsCdpPageId || 'none'}${RESET}`);
    this.writeLine('');
    
    // Menu items
    for (let i = 0; i < this.kidlispCardsMenu.length; i++) {
      const item = this.kidlispCardsMenu[i];
      const selected = i === this.kidlispCardsSelectedIndex;
      const prefix = selected ? `${FG_BRIGHT_YELLOW}▶ ` : `${FG_CYAN}  `;
      const keyStyle = selected ? `${BG_CYAN}${FG_BLACK}` : `${FG_BRIGHT_MAGENTA}`;
      const labelStyle = selected ? `${FG_BRIGHT_YELLOW}${BOLD}` : `${FG_WHITE}`;
      const descStyle = `${DIM}${FG_CYAN}`;
      
      const line = `${prefix}${keyStyle}[${item.key}]${RESET}${BG_BLUE} ${labelStyle}${item.label}${RESET}${BG_BLUE} ${descStyle}${item.desc}${RESET}`;
      this.writeLine(line);
    }
    
    this.writeLine('');
    
    // Output box
    this.writeLine(`${FG_CYAN}${'─'.repeat(boxWidth)}${RESET}`);
    this.writeLine(`${FG_BRIGHT_CYAN}Output:${RESET}`);
    this.writeLine('');
    
    // Split output into lines and display
    const outputLines = (this.kidlispCardsOutput || '').split('\n');
    const maxOutputLines = Math.max(5, this.innerHeight - 20);
    const startLine = Math.max(0, outputLines.length - maxOutputLines);
    
    for (let i = startLine; i < outputLines.length; i++) {
      let line = outputLines[i];
      // Color code output
      if (line.includes('✓') || line.includes('✅')) {
        line = `${FG_BRIGHT_GREEN}${line}${RESET}`;
      } else if (line.includes('✗') || line.includes('Error')) {
        line = `${FG_BRIGHT_RED}${line}${RESET}`;
      } else if (line.includes('⚠')) {
        line = `${FG_BRIGHT_YELLOW}${line}${RESET}`;
      } else if (line.includes('📚') || line.includes('🗑️')) {
        line = `${FG_BRIGHT_CYAN}${line}${RESET}`;
      } else if (line.includes('▶') || line.includes('↩') || line.includes('🔄')) {
        line = `${FG_BRIGHT_MAGENTA}${line}${RESET}`;
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
    this.writeLine(`${FG_GREEN}Ready${RESET}${BG_BLUE}  ${DIM}ESC/Q=Back  ↑↓/JK=Navigate  Enter=Select${RESET}`);
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
    // Exit code 42 signals intentional quit (don't auto-restart in dev mode)
    process.exit(42);
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
  
  // Calculate visual width of a string accounting for emojis
  getVisualWidth(str) {
    let visualWidth = 0;
    for (const char of str) {
      const code = char.codePointAt(0);
      
      // Skip zero-width characters
      if (code === 0xFE0F ||  // Variation Selector-16 (emoji presentation)
          code === 0xFE0E ||  // Variation Selector-15 (text presentation)
          code === 0x200D ||  // Zero Width Joiner
          code === 0x200B) {  // Zero Width Space
        continue;
      }
      
      // Emoji ranges (generally width 2)
      if ((code >= 0x1F300 && code <= 0x1F9FF) ||  // Misc Symbols and Pictographs, Supplemental Symbols
          (code >= 0x1F5A0 && code <= 0x1F5FF) ||  // Misc Symbols (extended) - includes 🖥
          (code >= 0x2600 && code <= 0x26FF) ||    // Misc Symbols
          (code >= 0x2700 && code <= 0x27BF) ||    // Dingbats (✓ ✗ etc)
          (code >= 0x1F000 && code <= 0x1F02F) ||  // Mahjong
          (code >= 0x1F0A0 && code <= 0x1F0FF) ||  // Playing Cards
          (code >= 0x1F100 && code <= 0x1F64F) ||  // Enclosed chars, Emoticons
          (code >= 0x1F680 && code <= 0x1F6FF) ||  // Transport and Map (🚀 rocket is 0x1F680)
          (code >= 0x2300 && code <= 0x23FF) ||    // Misc Technical (⚙ gear is 0x2699)
          (code >= 0x2190 && code <= 0x21FF)) {    // Arrows (← → ↑ ↓ etc)
        visualWidth += 2;
      }
      // Geometric shapes and box drawing - width 1
      else if ('●○◐◑◀▶▸▲▼→←↑↓│─═╔╗╚╝╠╣╦╩║╟╢┼'.includes(char)) {
        visualWidth += 1;
      }
      // Everything else is width 1
      else {
        visualWidth += 1;
      }
    }
    return visualWidth;
  }
  
  // Write a line with margins and full background fill (to buffer)
  writeLine(content) {
    const margin = ' '.repeat(this.adaptiveMarginX);
    const lineContent = `${BG_BLUE}${margin}${RESET}${content}`;
    // Fill rest of line with background - use visual width for emoji support
    const stripped = this.stripAnsi(lineContent);
    const contentLen = this.getVisualWidth(stripped);
    const fill = this.width - contentLen;
    const line = `${lineContent}${fill > 0 ? BG_BLUE + ' '.repeat(Math.max(0, fill)) + RESET : ''}`;
    this.frameBuffer.push(line);
  }

  // Rendering - uses double buffering to prevent flicker
  render() {
    // Build frame in buffer first
    this.frameBuffer = [];
    
    // For borderless menu mode, skip margins
    const useBorderless = (this.mode === 'menu' || this.mode === 'test-running' || this.mode === 'tests');
    
    if (!useBorderless) {
      // Top margin with background (legacy modes)
      for (let i = 0; i < this.marginY; i++) {
        this.frameBuffer.push(`${BG_BLUE}${' '.repeat(Math.max(1, this.width))}${RESET}`);
      }
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
      case 'test-running':
        this.renderTestRunning();
        break;
      case 'logs':
        this.renderLogs();
        break;
      case 'site':
        this.renderSite();
        break;
      case 'keeps':
        this.renderKeeps();
        // Render sixel thumbnail directly to parent terminal (bypasses eat)
        if (this.keepsSixel) {
          const row = Math.min(this.height - 5, 22);
          // Position cursor in parent terminal and write sixel directly
          const pty = findEmacsclientPty();
          if (pty) {
            try {
              fs.writeFileSync(pty, `\x1b[${row};4H${this.keepsSixel}`);
            } catch {}
          }
        }
        break;
      case 'kidlisp-cards':
        this.renderKidLispCards();
        break;
    }
    
    // Fill remaining lines with background (reserve 1 for footer if pending)
    const fillBg = useBorderless ? BG_BLACK : BG_BLUE;
    const reserveForFooter = (useBorderless && this.pendingFooter) ? 1 : 0;
    const targetHeight = this.height - reserveForFooter;
    
    // Truncate if we have too many lines (content overflow)
    if (this.frameBuffer.length > targetHeight) {
      this.frameBuffer.length = targetHeight;
    }
    
    // Fill remaining space up to target height
    while (this.frameBuffer.length < targetHeight) {
      this.frameBuffer.push(`${fillBg}${' '.repeat(Math.max(1, this.width))}${RESET}`);
    }
    
    // Render footer at very bottom if pending (always exactly at row height)
    if (useBorderless && this.pendingFooter) {
      const { text, bg, fg } = this.pendingFooter;
      const stripped = this.stripAnsi(text);
      const visualWidth = this.getVisualWidth(stripped);
      const padding = this.width - visualWidth;
      this.frameBuffer.push(`${bg}${fg}${text}${bg}${' '.repeat(Math.max(0, padding))}${RESET}`);
      this.pendingFooter = null;
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

  // === BORDERLESS RENDERING HELPERS ===
  
  // Write a full-width line with background color
  writeColorLine(content, bgColor, fgColor = FG_WHITE) {
    const stripped = this.stripAnsi(content);
    const visualWidth = this.getVisualWidth(stripped);
    const padding = this.width - visualWidth;
    // Ensure bgColor is applied after content in case content has RESET
    this.frameBuffer.push(`${bgColor}${fgColor}${content}${bgColor}${' '.repeat(Math.max(0, padding))}${RESET}`);
  }
  
  // Write a centered line
  writeCenteredLine(content, bgColor, fgColor = FG_WHITE) {
    const stripped = this.stripAnsi(content);
    const visualWidth = this.getVisualWidth(stripped);
    const leftPad = Math.floor((this.width - visualWidth) / 2);
    const rightPad = this.width - visualWidth - leftPad;
    this.frameBuffer.push(`${bgColor}${fgColor}${' '.repeat(Math.max(0, leftPad))}${content}${' '.repeat(Math.max(0, rightPad))}${RESET}`);
  }
  
  // Write an empty line with background
  writeEmptyLine(bgColor) {
    this.frameBuffer.push(`${bgColor}${' '.repeat(this.width)}${RESET}`);
  }

  // Helper to render a bordered line with proper padding (LEGACY - keeping for compatibility)
  renderBoxLine(content, boxWidth, theme) {
    const stripped = this.stripAnsi(content);
    const visualWidth = this.getVisualWidth(stripped);
    const padding = boxWidth - 2 - visualWidth;
    this.writeLine(`${theme.border}║${theme.fill}${content}${' '.repeat(Math.max(0, padding))}${theme.border}║${RESET}`);
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
              animatedLine += `${themeFill}   `; // Gap between words
              i++;
            }
          } else {
            const timeOffset = Math.floor(this.bloodPosition / 3);
            const waveOffset = Math.floor(Math.sin((charIdx + timeOffset) * 0.5) * 2);
            const colorIdx = Math.abs((charIdx + timeOffset + waveOffset) % BLOCK_COLORS.length);
            const block = BLOCK_COLORS[colorIdx];
            animatedLine += `${block.bg}${block.fg}${BOLD} ${char} ${RESET}`;
            charIdx++;
          }
        }
        
        this.writeLine(`${themeBorder}║${themeFill}${' '.repeat(Math.max(0, artPadding))}${animatedLine}${themeFill}${' '.repeat(Math.max(0, rightPad))}${themeBorder}║${RESET}`);
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
    
    // CDP status
    const cdpIcon = this.cdpStatus === 'online' ? `${FG_BRIGHT_GREEN}●` 
                  : this.cdpStatus === 'offline' ? `${FG_BRIGHT_RED}○` 
                  : `${DIM}?`;
    
    // AC Auth status
    const authDisplay = this.acAuth 
      ? `${FG_BRIGHT_GREEN}●${this.acAuth.user?.handle ? `@${this.acAuth.user.handle}` : '✓'}` 
      : `${FG_BRIGHT_RED}○`;
    
    const serverInfo = compact 
      ? ` ${localIcon}${prodIcon}` 
      : ` ${themeText}│ Local${localIcon} Prod${prodIcon} ${themeText}│ CDP${cdpIcon}`;
    
    const statusContent = `${acStatus}${piece}${serverInfo}`;
    this.renderBoxLine(statusContent, boxWidth, theme);
    
    // === PLATFORM LINE (non-compact only) ===
    if (this.hostInfo.inContainer && !compact) {
      const containerLabel = this.hostInfo.containerDistro || 'Container';
      const hostLabel = this.hostInfo.hostLabel || this.hostInfo.hostOS || 'Host';
      const cdpIcon = this.cdpStatus === 'online' ? `${FG_BRIGHT_GREEN}●` 
                    : this.cdpStatus === 'offline' ? `${FG_BRIGHT_RED}○` 
                    : `${DIM}?`;
      
      // Note: Use themeFill after RESET to maintain background color
      const platformContent = `${themeFill}${FG_BRIGHT_MAGENTA}📦 ${containerLabel} ${FG_BRIGHT_CYAN}→ ${this.hostInfo.hostEmoji} ${FG_WHITE}${BOLD}${hostLabel}${RESET}${themeFill} ${FG_CYAN}CDP${cdpIcon}${RESET}${themeFill}`;
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

  // === NEW BORDERLESS MENU DESIGN ===
  renderMenuBorderless() {
    const theme = this.getThemeColors();
    const compact = this.width < 80;
    const narrow = this.width < 100;
    
    // Section colors
    const titleBg = BG_BLACK;
    const statusBg = theme.bg;
    const boardBg = BG_BLACK;
    const footerBg = theme.bg;
    
    // Get matrix rain layer for background effect
    const rainLayer = this.getMatrixRainLayer();
    
    // === STATUS BAR (at very top) ===
    const acStatus = this.connected 
      ? `${FG_WHITE}● CONNECTED` 
      : `${FG_WHITE}○ DISCONNECTED`;
    const piece = this.currentPiece ? ` │ ${FG_BRIGHT_YELLOW}${this.currentPiece}${RESET}` : '';
    const localIcon = this.serverStatus.local === true ? `${FG_BRIGHT_GREEN}●` 
                    : this.serverStatus.local === false ? `${FG_BRIGHT_YELLOW}◐` 
                    : `${DIM}?`;
    const prodIcon = this.serverStatus.production === true ? `${FG_BRIGHT_GREEN}●` 
                   : this.serverStatus.production === false ? `${FG_BRIGHT_RED}○` 
                   : `${DIM}?`;
    const cdpIcon = this.cdpStatus === 'online' ? `${FG_BRIGHT_GREEN}●` : `${FG_BRIGHT_RED}○`;
    
    let statusLine = ` ${acStatus}${piece} │ Local${localIcon} Prod${prodIcon}`;
    if (this.hostInfo.inContainer && !compact) {
      statusLine += ` │ CDP${cdpIcon}`;
    }
    statusLine += `  ${FG_CYAN}${this.utcTime}${RESET}`;
    this.writeColorLine(statusLine, statusBg, theme.text);
    
    // === TITLE SECTION - Large ASCII banner with matrix rain ===
    // Render matrix rain row (row 0)
    this.renderMatrixRainLine(0, rainLayer, titleBg);
    
    // Large block letters for AESTHETIC.COMPUTER
    if (!compact) {
      this.renderLargeTitle(titleBg, rainLayer, 1); // start at row 1
    } else {
      // Compact: animated single line
      this.renderAnimatedTitle(titleBg);
    }
    
    // More matrix rain rows after title (rows 4, 5)
    this.renderMatrixRainLine(4, rainLayer, titleBg);

    // === MENU BOARD (grid of colored tiles) ===
    this.writeEmptyLine(boardBg);
    
    // Tile colors for different categories
    const tileColors = [
      { bg: '\x1b[48;5;24m', fg: FG_WHITE },   // Deep blue - navigation
      { bg: '\x1b[48;5;29m', fg: FG_WHITE },   // Teal - tools  
      { bg: '\x1b[48;5;54m', fg: FG_WHITE },   // Purple - dev
      { bg: '\x1b[48;5;94m', fg: FG_WHITE },   // Brown/orange - system
      { bg: '\x1b[48;5;23m', fg: FG_WHITE },   // Dark cyan
      { bg: '\x1b[48;5;52m', fg: FG_WHITE },   // Dark red
    ];
    
    // Blinking selection
    const blink = Math.floor(this.bloodPosition / 4) % 2 === 0;
    const selectFg = blink ? '\x1b[38;5;226m' : '\x1b[38;5;228m'; // Yellow shades
    const selectBg = blink ? '\x1b[48;5;22m' : '\x1b[48;5;28m'; // Green shades for selected
    
    // Responsive tile sizing - expand to fill available width
    const availableHeight = this.height - 8;
    const rowHeight = 2; // Each tile row takes 2 lines (content + gap)
    const maxVisibleRows = Math.floor(availableHeight / rowHeight);
    
    // Determine how many columns based on item count and screen size
    // Prefer fewer wider tiles over more narrow ones
    const itemCount = this.menuItems.length;
    let tilesPerRow;
    if (this.width >= 120) {
      tilesPerRow = Math.min(6, Math.ceil(itemCount / Math.ceil(itemCount / 6)));
    } else if (this.width >= 100) {
      tilesPerRow = Math.min(5, Math.ceil(itemCount / Math.ceil(itemCount / 5)));
    } else if (this.width >= 80) {
      tilesPerRow = Math.min(4, Math.ceil(itemCount / Math.ceil(itemCount / 4)));
    } else {
      tilesPerRow = Math.min(3, Math.ceil(itemCount / Math.ceil(itemCount / 3)));
    }
    
    // Calculate tile width to fill available space (with 1-char gaps)
    const availableWidth = this.width - 2; // margins
    const tileWidth = Math.floor((availableWidth - (tilesPerRow - 1)) / tilesPerRow);
    
    // Calculate how many rows we need
    const totalRows = Math.ceil(this.menuItems.length / tilesPerRow);
    
    // Pagination if needed
    const currentRow = Math.floor(this.selectedIndex / tilesPerRow);
    let startRow = 0;
    if (totalRows > maxVisibleRows) {
      // Keep selected row visible
      startRow = Math.max(0, Math.min(currentRow - Math.floor(maxVisibleRows / 2), totalRows - maxVisibleRows));
    }
    const endRow = Math.min(startRow + maxVisibleRows, totalRows);
    
    const totalTileWidth = tilesPerRow * tileWidth + (tilesPerRow - 1);
    const leftMargin = Math.floor((this.width - totalTileWidth) / 2);
    
    // Render visible rows
    for (let row = startRow; row < endRow; row++) {
      // Single line per tile (compact)
      let tileLine = ' '.repeat(leftMargin);
      for (let col = 0; col < tilesPerRow; col++) {
        const idx = row * tilesPerRow + col;
        if (idx < this.menuItems.length) {
          const item = this.menuItems[idx];
          const selected = idx === this.selectedIndex;
          const tileBg = selected ? selectBg : tileColors[idx % tileColors.length].bg;
          const tileFg = selected ? selectFg : FG_WHITE;
          const keyColor = selected ? FG_WHITE : FG_YELLOW;
          
          // Build tile content: KEY label (key is prominent)
          const keyText = item.key.toUpperCase();
          let label = item.label;
          
          // Dynamic label for Login/Logout
          if (item.key === 'i') {
            label = this.acAuth ? 'Logout' : 'Login';
          }
          
          const maxLabel = tileWidth - 4; // KEY + space + some label
          if (label.length > maxLabel) label = label.slice(0, maxLabel - 2) + '..';
          
          // Log badge
          if (item.key === 'l' && this.unreadLogs > 0) {
            const badge = `(${this.unreadLogs})`;
            const available = maxLabel - badge.length - 1;
            if (label.length > available) label = label.slice(0, available - 2) + '..';
            label = `${label} ${badge}`;
          }
          
          // Content with key highlighted
          const innerContent = `${keyText} ${label}`;
          const pad = tileWidth - innerContent.length - 2;
          
          if (selected) {
            tileLine += `${tileBg}${tileFg}${BOLD}▐${keyColor}${keyText}${RESET}${tileBg}${tileFg} ${label}${RESET}${tileBg}${' '.repeat(Math.max(0, pad))}${tileFg}▌${RESET}`;
          } else {
            tileLine += `${tileBg}${keyColor}${BOLD}${keyText}${RESET}${tileBg}${tileFg} ${label}${' '.repeat(Math.max(0, pad + 1))}${RESET}`;
          }
          if (col < tilesPerRow - 1) tileLine += ' ';
        }
      }
      this.writeColorLine(tileLine, boardBg);
      
      // Gap between rows (skip on last row)
      if (row < endRow - 1) {
        this.writeEmptyLine(boardBg);
      }
    }
    
    // Show pagination indicator if needed
    if (totalRows > maxVisibleRows) {
      const pageInfo = ` ${FG_CYAN}[${startRow + 1}-${endRow}/${totalRows} rows]${RESET}`;
      this.writeColorLine(pageInfo, boardBg);
    } else {
      this.writeEmptyLine(boardBg);
    }
    
    // Store footer for later rendering at very bottom (handled in render())
    this.pendingFooter = {
      text: this.statusMessage 
        ? ` ${this.statusMessage}` 
        : ` ${FG_WHITE}↑↓←→${theme.text} Nav  ${FG_WHITE}Enter${theme.text} Select  ${FG_WHITE}Q${theme.text} Quit  ${FG_CYAN}${this.utcTime}`,
      bg: footerBg,
      fg: theme.text
    };
  }
  
  // Large animated title banner - simple block style with optional matrix rain background
  renderLargeTitle(bg, rainLayer = null, startRow = 0) {
    // Simple 3-row block font
    const font = {
      'A': ['█▀█', '█▀█', '▀ ▀'],
      'E': ['█▀▀', '█▀ ', '▀▀▀'],
      'S': ['█▀▀', '▀▀█', '▀▀▀'],
      'T': ['▀█▀', ' █ ', ' ▀ '],
      'H': ['█ █', '█▀█', '▀ ▀'],
      'I': [' █ ', ' █ ', ' ▀ '],
      'C': ['█▀▀', '█  ', '▀▀▀'],
      'O': ['█▀█', '█ █', '▀▀▀'],
      'M': ['█▄█', '█ █', '▀ ▀'],
      'P': ['█▀█', '█▀▀', '▀  '],
      'U': ['█ █', '█ █', '▀▀▀'],
      'R': ['█▀█', '█▀▄', '▀ ▀'],
      ' ': ['  ', '  ', '  '],
    };
    
    const text = 'AESTHETIC COMPUTER';
    
    // Calculate title width for centering
    let titleWidth = 0;
    for (const char of text) {
      if (char === ' ') {
        titleWidth += 2; // space + dot
      } else {
        titleWidth += 4; // glyph (3) + space (1)
      }
    }
    const leftPad = Math.floor((this.width - titleWidth) / 2);
    
    // Bouncing dot with physics - fast bounce with gravity feel
    // Use sine for smooth acceleration/deceleration at top
    const bounceSpeed = this.bloodPosition * 0.4; // faster
    const bouncePhase = Math.abs(Math.sin(bounceSpeed));
    // Map sine (0-1) to rows: 0=top, 1=mid, 2=bottom
    // Spend more time at bottom (gravity), quick at top
    const dotRow = bouncePhase < 0.3 ? 0 : bouncePhase < 0.6 ? 1 : 2;
    const dotPink = '\x1b[38;5;205m'; // Bright pink
    
    // Filter out black from block colors for better contrast on black bg
    const visibleColors = BLOCK_COLORS.filter(c => 
      !c.bg.includes('40m') && !c.fg.includes('30m') // skip black bg/fg
    );
    
    // Green codes for rain
    const greenCodes = [
      '\x1b[38;5;22m',  // 0: very dark green
      '\x1b[38;5;28m',  // 1: dark green
      '\x1b[38;5;34m',  // 2: medium green
      '\x1b[38;5;40m',  // 3: green
      '\x1b[38;5;46m',  // 4: bright green
      '\x1b[38;5;156m', // 5: very bright green/white (head)
    ];
    
    // Build 3 rows with animated colors per letter
    for (let rowIdx = 0; rowIdx < 3; rowIdx++) {
      // Build rain background for the entire line
      let rainBg = '';
      const rainRow = rainLayer ? rainLayer[startRow + rowIdx] : null;
      if (rainRow) {
        for (let col = 0; col < leftPad; col++) {
          const cell = rainRow[col];
          if (cell) {
            const brightness = Math.floor(cell.fade * 5);
            rainBg += `${greenCodes[Math.min(brightness, 5)]}${cell.char}${RESET}${bg}`;
          } else {
            rainBg += ' ';
          }
        }
      } else {
        rainBg = ' '.repeat(Math.max(0, leftPad));
      }
      
      // Build title text
      let titleLine = '';
      let charIdx = 0;
      for (let i = 0; i < text.length; i++) {
        const char = text[i];
        
        // Handle the space between words - insert bouncing dot
        if (char === ' ') {
          // Bouncing pink dot
          const dotChar = rowIdx === dotRow ? '●' : ' ';
          titleLine += `${dotPink}${dotChar}${RESET}${bg} `;
          continue;
        }
        
        const glyph = font[char] || ['???', '???', '???'];
        const timeOffset = Math.floor(this.bloodPosition / 4);
        const colorIdx = Math.abs((charIdx + timeOffset) % visibleColors.length);
        const block = visibleColors[colorIdx];
        titleLine += `${block.fg}${glyph[rowIdx]} ${RESET}${bg}`;
        charIdx++;
      }
      
      // Build rain background for right side
      let rainRight = '';
      const rightStart = leftPad + titleWidth;
      if (rainRow) {
        for (let col = rightStart; col < this.width; col++) {
          const cell = rainRow[col];
          if (cell) {
            const brightness = Math.floor(cell.fade * 5);
            rainRight += `${greenCodes[Math.min(brightness, 5)]}${cell.char}${RESET}${bg}`;
          } else {
            rainRight += ' ';
          }
        }
      } else {
        rainRight = ' '.repeat(Math.max(0, this.width - rightStart));
      }
      
      this.frameBuffer.push(`${bg}${rainBg}${titleLine}${rainRight}${RESET}`);
    }
  }
  
  // Compact animated single-line title
  renderAnimatedTitle(bg) {
    const artLine = 'AESTHETIC.COMPUTER';
    let animatedLine = '';
    let charIdx = 0;
    for (const char of artLine) {
      if (char === ' ' || char === '.') {
        animatedLine += `${bg}${char}${RESET}`;
      } else {
        const timeOffset = Math.floor(this.bloodPosition / 3);
        const waveOffset = Math.floor(Math.sin((charIdx + timeOffset) * 0.5) * 2);
        const colorIdx = Math.abs((charIdx + timeOffset + waveOffset) % BLOCK_COLORS.length);
        const block = BLOCK_COLORS[colorIdx];
        animatedLine += `${block.bg}${block.fg}${BOLD} ${char} ${RESET}`;
        charIdx++;
      }
    }
    this.writeCenteredLine(animatedLine, bg);
  }

  renderMenu() {
    // Use new borderless design
    this.renderMenuBorderless();
  }

  renderMenuLegacy() {
    // OLD bordered menu - kept for reference
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
    const theme = this.getThemeColors();
    const boardBg = BG_BLACK;
    const footerBg = theme.bg;
    
    // === TITLE SECTION ===
    this.writeEmptyLine(boardBg);
    const testTitle = `${FG_BRIGHT_YELLOW}🧪${RESET}${boardBg} ${FG_WHITE}${BOLD}RUN TESTS${RESET}`;
    this.writeCenteredLine(testTitle, boardBg);
    this.writeEmptyLine(boardBg);
    
    // Tile colors by category
    const arteryColor = { bg: '\x1b[48;5;22m', fg: FG_WHITE };   // Dark green
    const musicColor = { bg: '\x1b[48;5;54m', fg: FG_WHITE };    // Purple
    const graphicsColor = { bg: '\x1b[48;5;24m', fg: FG_WHITE }; // Blue
    const otherColor = { bg: '\x1b[48;5;94m', fg: FG_WHITE };    // Brown
    
    // Blinking selection
    const blink = Math.floor(this.bloodPosition / 4) % 2 === 0;
    const selectFg = blink ? '\x1b[38;5;226m' : '\x1b[38;5;228m';
    const selectBg = blink ? '\x1b[48;5;22m' : '\x1b[48;5;28m';
    
    const getTileColor = (test) => {
      if (test.isArtery) return arteryColor;
      if (test.name.includes('melody') || test.name.includes('chord') || test.name.includes('waltz')) return musicColor;
      if (test.name.includes('line')) return graphicsColor;
      return otherColor;
    };
    
    // Filter out separators
    const tests = (this.testFiles || []).filter(t => !t.isSeparator);
    
    // Find actual selected index in filtered tests
    let actualSelectedIdx = 0;
    let counter = 0;
    for (let i = 0; i < (this.testFiles || []).length; i++) {
      if (!this.testFiles[i].isSeparator) {
        if (i === this.selectedIndex) {
          actualSelectedIdx = counter;
          break;
        }
        counter++;
      }
    }
    
    // Responsive tile sizing - expand to fill width
    const availableHeight = this.height - 7;
    const rowHeight = 2;
    const maxVisibleRows = Math.floor(availableHeight / rowHeight);
    
    // Determine columns based on test count
    const testCount = tests.length;
    let tilesPerRow;
    if (this.width >= 120) {
      tilesPerRow = Math.min(6, Math.ceil(testCount / Math.ceil(testCount / 6)));
    } else if (this.width >= 100) {
      tilesPerRow = Math.min(5, Math.ceil(testCount / Math.ceil(testCount / 5)));
    } else if (this.width >= 80) {
      tilesPerRow = Math.min(4, Math.ceil(testCount / Math.ceil(testCount / 4)));
    } else {
      tilesPerRow = Math.min(3, Math.ceil(testCount / Math.ceil(testCount / 3)));
    }
    
    // Calculate tile width to fill available space
    const availableWidth = this.width - 2;
    const tileWidth = Math.floor((availableWidth - (tilesPerRow - 1)) / tilesPerRow);
    
    const totalRows = Math.ceil(tests.length / tilesPerRow);
    
    // Pagination
    const currentRow = Math.floor(actualSelectedIdx / tilesPerRow);
    let startRow = 0;
    if (totalRows > maxVisibleRows) {
      startRow = Math.max(0, Math.min(currentRow - Math.floor(maxVisibleRows / 2), totalRows - maxVisibleRows));
    }
    const endRow = Math.min(startRow + maxVisibleRows, totalRows);
    
    const totalTileWidth = tilesPerRow * tileWidth + (tilesPerRow - 1);
    const leftMargin = Math.floor((this.width - totalTileWidth) / 2);
    
    // Render visible rows
    for (let row = startRow; row < endRow; row++) {
      let tileLine = ' '.repeat(leftMargin);
      for (let col = 0; col < tilesPerRow; col++) {
        const idx = row * tilesPerRow + col;
        if (idx < tests.length) {
          const test = tests[idx];
          const selected = idx === actualSelectedIdx;
          const tileBg = selected ? selectBg : getTileColor(test).bg;
          const tileFg = selected ? selectFg : FG_WHITE;
          
          const configIcon = test.params ? '⚙' : '';
          let name = test.name;
          const maxName = tileWidth - 3 - configIcon.length;
          if (name.length > maxName) name = name.slice(0, maxName - 2) + '..';
          
          const content = configIcon ? `${configIcon}${name}` : name;
          const pad = tileWidth - content.length - 1;
          
          if (selected) {
            tileLine += `${tileBg}${tileFg}${BOLD}▐${content}${RESET}${tileBg}${' '.repeat(Math.max(0, pad))}${tileFg}▌${RESET}`;
          } else {
            tileLine += `${tileBg}${tileFg} ${content}${' '.repeat(Math.max(0, pad))}${RESET}`;
          }
          if (col < tilesPerRow - 1) tileLine += ' ';
        }
      }
      this.writeColorLine(tileLine, boardBg);
      
      if (row < endRow - 1) {
        this.writeEmptyLine(boardBg);
      }
    }
    
    // Pagination indicator
    if (totalRows > maxVisibleRows) {
      this.writeColorLine(` ${FG_CYAN}[${startRow + 1}-${endRow}/${totalRows}]${RESET}`, boardBg);
    } else {
      this.writeEmptyLine(boardBg);
    }
    
    // Selected test description
    const selectedTest = tests[actualSelectedIdx];
    if (selectedTest) {
      const desc = selectedTest.desc.length > this.width - 4 
        ? selectedTest.desc.slice(0, this.width - 7) + '...'
        : selectedTest.desc;
      this.writeColorLine(` ${FG_CYAN}${desc}${RESET}`, boardBg);
    }
    
    // Footer - use pendingFooter to ensure it's at very bottom
    const footerText = ` ${FG_WHITE}↑↓←→${theme.text} Nav  ${FG_WHITE}Enter${theme.text} Run  ${FG_WHITE}Esc${theme.text} Back  ${FG_CYAN}${this.utcTime}`;
    this.pendingFooter = { text: footerText, bg: footerBg, fg: theme.text };
  }
  
  renderTestParams() {
    this.renderHeader();
    const boxWidth = this.innerWidth;
    const compact = this.width < 80;
    const test = this.currentTest;
    const params = test?.params || [];
    
    // Get dynamic theme colors
    const theme = this.getThemeColors();
    
    // Group params by type for cleaner layout
    const visualParams = this.getVisualParamOrder(params);
    const selects = params.filter(p => p.type === 'select');
    const ranges = params.filter(p => p.type === 'range');
    const toggles = params.filter(p => p.type === 'toggle');
    
    // Title bar with icon
    const icon = test?.icon || '⚙';
    const testTitle = ` ${icon} ${FG_WHITE}${BOLD}${test?.name}${RESET}${theme.fill}  ${DIM}${test?.desc || ''}${RESET}`;
    this.renderBoxLine(testTitle, boxWidth, theme);
    
    // Flat index for navigation
    let flatIndex = 0;
    
    // === SELECTS SECTION ===
    if (selects.length > 0) {
      const sectionLabel = `${DIM}─── Options ${'─'.repeat(Math.max(0, boxWidth - 18))}${RESET}`;
      this.renderBoxLine(sectionLabel, boxWidth, theme);
      for (const param of selects) {
        this.renderParamRowBoxed(param, flatIndex, boxWidth, theme);
        flatIndex++;
      }
    }
    
    // === RANGES SECTION ===
    if (ranges.length > 0) {
      const sectionLabel = `${DIM}─── Values ${'─'.repeat(Math.max(0, boxWidth - 17))}${RESET}`;
      this.renderBoxLine(sectionLabel, boxWidth, theme);
      for (const param of ranges) {
        this.renderParamRowBoxed(param, flatIndex, boxWidth, theme);
        flatIndex++;
      }
    }
    
    // === TOGGLES SECTION ===
    if (toggles.length > 0) {
      const sectionLabel = `${DIM}─── Flags ${'─'.repeat(Math.max(0, boxWidth - 16))}${RESET}`;
      this.renderBoxLine(sectionLabel, boxWidth, theme);
      
      // Render toggles in rows
      const togglesPerRow = compact ? 2 : 4;
      for (let i = 0; i < toggles.length; i += togglesPerRow) {
        const rowToggles = toggles.slice(i, i + togglesPerRow);
        let rowContent = ' ';
        const cellWidth = Math.floor((boxWidth - 4) / togglesPerRow);
        
        for (let j = 0; j < rowToggles.length; j++) {
          const param = rowToggles[j];
          const idx = flatIndex + j;
          const selected = idx === this.paramIndex;
          const value = this.testParams[param.key] || param.default || '';
          const isOn = value !== '';
          
          const indicator = selected ? `${theme.accent}▸` : ` `;
          const toggleIcon = isOn ? `${FG_BRIGHT_GREEN}●` : `${DIM}○`;
          const label = selected ? `${FG_WHITE}${BOLD}${param.key}${RESET}${theme.fill}` : `${theme.text}${param.key}`;
          
          const cell = `${indicator}${toggleIcon} ${label}`;
          const cellLen = this.getVisualWidth(this.stripAnsi(cell));
          const pad = Math.max(0, cellWidth - cellLen);
          rowContent += `${cell}${' '.repeat(pad)}`;
        }
        flatIndex += rowToggles.length;
        this.renderBoxLine(rowContent, boxWidth, theme);
      }
    }
    
    // Empty line
    this.renderBoxLine('', boxWidth, theme);
    
    // Run button
    const runIndex = visualParams.length;
    const runSelected = this.paramIndex >= runIndex;
    const runIndicator = runSelected ? `${FG_WHITE}▸` : ` `;
    const runStyle = runSelected 
      ? `${BG_GREEN}${FG_WHITE}${BOLD} ▶ RUN ${RESET}${theme.fill}` 
      : `${FG_GREEN}${BOLD} ▶ RUN ${RESET}${theme.fill}`;
    this.renderBoxLine(` ${runIndicator}${runStyle}`, boxWidth, theme);
    
    // Command preview
    if (!compact) {
      const args = this.buildTestArgs(test);
      const maxCmdLen = boxWidth - 10;
      let cmdText = `node ${test?.file} ${args}`;
      if (cmdText.length > maxCmdLen) cmdText = cmdText.slice(0, maxCmdLen - 3) + '...';
      this.renderBoxLine(`${DIM}  $ ${cmdText}${RESET}`, boxWidth, theme);
    }
    
    // Fill remaining space
    const usedLines = 1 + // title
                      (selects.length > 0 ? selects.length + 1 : 0) + 
                      (ranges.length > 0 ? ranges.length + 1 : 0) +
                      (toggles.length > 0 ? Math.ceil(toggles.length / (compact ? 2 : 4)) + 1 : 0) +
                      2 + // spacer + run
                      (compact ? 0 : 1); // cmd preview
    const availableLines = this.innerHeight - 8; // header + footer
    for (let i = usedLines; i < availableLines; i++) {
      this.renderBoxLine('', boxWidth, theme);
    }
    
    // Footer hints with color-coded buttons
    const upDown = `${BG_BLUE}${FG_WHITE}${BOLD} ↑↓ ${RESET}`;
    const leftRight = `${BG_BLUE}${FG_WHITE}${BOLD} ←→ ${RESET}`;
    const spaceBtn = `${BG_MAGENTA}${FG_WHITE}${BOLD} Space ${RESET}`;
    const enterBtn = `${BG_GREEN}${FG_WHITE}${BOLD} Enter ${RESET}`;
    const escBtn = `${BG_RED}${FG_WHITE}${BOLD} Esc ${RESET}`;
    
    const hints = `${upDown}${FG_BLUE}Nav  ${leftRight}${FG_BLUE}Edit  ${spaceBtn}${FG_MAGENTA}Toggle  ${enterBtn}${FG_GREEN}Run  ${escBtn}${FG_RED}Back`;
    this.renderBoxLine(hints, boxWidth, theme);
    
    this.renderFooter();
  }
  
  // Render a param row inside the bordered box
  renderParamRowBoxed(param, flatIndex, boxWidth, theme) {
    const selected = flatIndex === this.paramIndex;
    const value = selected ? this.inputBuffer : (this.testParams[param.key] || String(param.default || ''));
    
    const indicator = selected ? `${theme.accent}▸` : ` `;
    const labelStyle = selected ? `${FG_WHITE}${BOLD}` : `${theme.text}`;
    const label = `${labelStyle}${param.key}${RESET}${theme.fill}`;
    
    let valueDisplay;
    if (param.type === 'select') {
      if (selected) {
        valueDisplay = `${DIM}◀${RESET}${theme.fill} ${FG_BRIGHT_YELLOW}${BOLD}${value}${RESET}${theme.fill} ${DIM}▶${RESET}${theme.fill}`;
      } else {
        valueDisplay = `${FG_GREEN}${value}${RESET}${theme.fill}`;
      }
    } else if (param.type === 'range') {
      const numVal = parseInt(value) || param.min || 0;
      const min = param.min ?? 0;
      const max = param.max ?? 100;
      const range = max - min || 1;
      const pct = Math.max(0, Math.min(1, (numVal - min) / range));
      const barWidth = 8;
      const filled = Math.max(0, Math.min(barWidth, Math.round(pct * barWidth)));
      const bar = `${FG_BRIGHT_CYAN}${'━'.repeat(filled)}${DIM}${'─'.repeat(barWidth - filled)}${RESET}${theme.fill}`;
      
      if (selected) {
        valueDisplay = `${DIM}◀${RESET}${theme.fill} ${FG_BRIGHT_YELLOW}${BOLD}${value}${RESET}${theme.fill} ${bar} ${DIM}▶${RESET}${theme.fill}`;
      } else {
        valueDisplay = `${FG_GREEN}${value}${RESET}${theme.fill} ${bar}`;
      }
    }
    
    const content = ` ${indicator} ${label} ${DIM}│${RESET}${theme.fill} ${valueDisplay}`;
    this.renderBoxLine(content, boxWidth, theme);
  }
  
  // Build test args from current params
  buildTestArgs(test) {
    if (!test?.params) return '';
    const parts = [];
    for (const param of test.params) {
      const value = this.testParams[param.key] || String(param.default || '');
      if (value && value !== '') {
        if (param.type === 'toggle') {
          parts.push(value); // Just the key name for toggles
        } else if (param.type === 'range') {
          parts.push(`${param.key}=${value}`);
        } else {
          parts.push(value); // Select values go directly
        }
      }
    }
    return parts.join(' ');
  }

  // Strip emojis and special characters that mess up column alignment
  stripEmojis(str) {
    // Remove emoji ranges and variation selectors
    return str.replace(/[\u{1F300}-\u{1F9FF}]|[\u{2600}-\u{26FF}]|[\u{2700}-\u{27BF}]|[\u{FE00}-\u{FE0F}]|[\u{1F000}-\u{1F02F}]|[\u{1F0A0}-\u{1F0FF}]|[\u{1F100}-\u{1F64F}]|[\u{1F680}-\u{1F6FF}]|[\u{2300}-\u{23FF}]|✨|♪|📦|🖥|🎼|🧪|⚙/gu, '').trim();
  }

  // Split-panel test running view with distinct panel colors
  renderTestRunning() {
    const boxWidth = this.width - 2;
    
    // Panel-specific colors (distinct backgrounds)
    const leftBg = '\x1b[48;5;234m';   // Dark gray for output
    const leftFg = FG_WHITE;
    const rightBg = '\x1b[48;5;17m';   // Dark blue for logs  
    const rightFg = FG_WHITE;
    const borderFg = FG_BRIGHT_CYAN;
    const headerBg = '\x1b[48;5;236m'; // Slightly lighter header
    
    // Calculate panel widths (true 50/50 split)
    const leftWidth = Math.floor((boxWidth - 3) / 2);
    const rightWidth = boxWidth - 3 - leftWidth;
    
    // Available height for content
    const contentHeight = this.height - 6; // top border, header, divider, bottom divider, footer, bottom border
    
    // Initialize scroll positions if needed
    if (this.leftScrollOffset === undefined) this.leftScrollOffset = 0;
    if (this.rightScrollOffset === undefined) this.rightScrollOffset = 0;
    if (this.activePanel === undefined) this.activePanel = 'left';
    
    // === TOP BORDER ===
    this.frameBuffer.push(` ${borderFg}╔${'═'.repeat(leftWidth)}╦${'═'.repeat(rightWidth)}╗${RESET}`);
    
    // === HEADER ROW ===
    const testName = this.testFile ? this.testFile.split('/').pop().replace('.mjs', '') : 'Test';
    const runningIcon = this.testRunning ? '◐' : '●';
    const runningColor = this.testRunning ? FG_BRIGHT_YELLOW : FG_BRIGHT_GREEN;
    const activeLeft = this.activePanel === 'left';
    
    // Left header with test name
    const leftTitle = `${runningColor}${runningIcon}${RESET}${headerBg} ${FG_BRIGHT_GREEN}${BOLD}OUTPUT${RESET}${headerBg} ${FG_WHITE}${testName}`;
    const leftTitleStripped = this.stripAnsi(leftTitle);
    const leftTitlePad = Math.max(0, leftWidth - this.getVisualWidth(leftTitleStripped) - 1);
    
    // Right header
    const rightTitle = `${FG_BRIGHT_MAGENTA}${BOLD}LOGS${RESET}${headerBg} ${FG_CYAN}Console`;
    const rightTitleStripped = this.stripAnsi(rightTitle);
    const rightTitlePad = Math.max(0, rightWidth - this.getVisualWidth(rightTitleStripped) - 1);
    
    this.frameBuffer.push(` ${borderFg}║${headerBg} ${leftTitle}${' '.repeat(leftTitlePad)}${borderFg}║${headerBg} ${rightTitle}${' '.repeat(rightTitlePad)}${borderFg}║${RESET}`);
    
    // === HEADER DIVIDER ===
    this.frameBuffer.push(` ${borderFg}╠${'─'.repeat(leftWidth)}╬${'─'.repeat(rightWidth)}╣${RESET}`);
    
    // === CONTENT ROWS ===
    const outputLines = this.testOutput || [];
    const logLines = this.testLogs || [];
    
    // Auto-scroll to bottom (follow mode)
    const maxLeftScroll = Math.max(0, outputLines.length - contentHeight);
    const maxRightScroll = Math.max(0, logLines.length - contentHeight);
    this.leftScrollOffset = maxLeftScroll; // Auto-follow
    this.rightScrollOffset = maxRightScroll; // Auto-follow
    
    // Get visible portion based on scroll
    const visibleOutput = outputLines.slice(this.leftScrollOffset, this.leftScrollOffset + contentHeight);
    const visibleLogs = logLines.slice(this.rightScrollOffset, this.rightScrollOffset + contentHeight);
    
    for (let row = 0; row < contentHeight; row++) {
      // Left panel - test output (dark gray bg)
      let leftText = '';
      if (row < visibleOutput.length) {
        const line = visibleOutput[row];
        let text = this.stripAnsi(line.text || '');
        text = this.stripEmojis(text); // Remove emojis for alignment
        const maxW = leftWidth - 1;
        if (text.length > maxW) text = text.slice(0, maxW - 2) + '..';
        leftText = text;
      }
      const leftPad = Math.max(0, leftWidth - leftText.length);
      
      // Right panel - console logs (dark blue bg)
      let rightText = '';
      let rightColor = rightFg;
      if (row < visibleLogs.length) {
        const log = visibleLogs[row];
        const levelColors = {
          error: FG_BRIGHT_RED,
          warn: FG_BRIGHT_YELLOW,
          success: FG_BRIGHT_GREEN,
          log: FG_WHITE,
          info: FG_BRIGHT_CYAN,
          input: FG_BRIGHT_MAGENTA,
          result: FG_BRIGHT_GREEN,
        };
        rightColor = levelColors[log.level] || FG_WHITE;
        let text = this.stripAnsi(log.text || '');
        text = this.stripEmojis(text);
        const maxW = rightWidth - 1;
        if (text.length > maxW) text = text.slice(0, maxW - 2) + '..';
        rightText = text;
      }
      const rightPad = Math.max(0, rightWidth - rightText.length);
      
      // Combine with distinct backgrounds
      this.frameBuffer.push(` ${borderFg}║${leftBg}${leftFg}${leftText}${' '.repeat(leftPad)}${RESET}${borderFg}│${rightBg}${rightColor}${rightText}${' '.repeat(rightPad)}${RESET}${borderFg}║${RESET}`);
    }
    
    // === BOTTOM DIVIDER ===
    this.frameBuffer.push(` ${borderFg}╠${'═'.repeat(leftWidth)}╩${'═'.repeat(rightWidth)}╣${RESET}`);
    
    // === FOOTER with color-coded buttons ===
    const statusText = this.testRunning 
      ? `${FG_BRIGHT_YELLOW}${BOLD}● RUNNING${RESET}` 
      : `${FG_BRIGHT_GREEN}${BOLD}✓ COMPLETE${RESET}`;
    
    // Color-coded keyboard hints
    const escBtn = `${BG_RED}${FG_WHITE}${BOLD} Esc ${RESET}`;
    const escLabel = `${FG_RED}Back`;
    const tabBtn = `${BG_BLUE}${FG_WHITE}${BOLD} Tab ${RESET}`;
    const tabLabel = `${FG_BLUE}Switch`;
    const scrollHint = `${DIM}↑↓ Scroll${RESET}`;
    
    const footerContent = ` ${statusText}  ${escBtn}${escLabel}  ${tabBtn}${tabLabel}  ${scrollHint}  ${FG_CYAN}${this.utcTime}`;
    const footerStripped = this.stripAnsi(footerContent);
    const footerPad = Math.max(0, boxWidth - 2 - this.getVisualWidth(footerStripped));
    
    this.frameBuffer.push(` ${borderFg}║${BG_BLACK}${footerContent}${' '.repeat(footerPad)}${borderFg}║${RESET}`);
    this.frameBuffer.push(` ${borderFg}╚${'═'.repeat(boxWidth - 2)}╝${RESET}`);
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
