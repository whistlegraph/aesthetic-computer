#!/usr/bin/env node
/**
 * 🔍 Devcontainer Status - Provides process tree and system info
 * 
 * Usage:
 *   node devcontainer-status.mjs           # JSON output for VS Code extension
 *   node devcontainer-status.mjs --watch   # Continuous updates (SSE style)
 *   node devcontainer-status.mjs --server  # HTTP server on port 7890 with WebSocket
 * 
 * Endpoints:
 *   GET /           - Interactive D3.js dashboard (for Simple Browser debugging)
 *   GET /status     - JSON status snapshot
 *   GET /stream     - SSE live updates
 *   WS  /ws         - WebSocket for real-time updates
 */

import { exec, execSync, spawn } from 'child_process';
import { createServer } from 'http';
import { promisify } from 'util';
import { WebSocketServer } from 'ws';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const execAsync = promisify(exec);

// Key processes we care about for Aesthetic Computer
// IMPORTANT: Order matters! First match wins, so specific patterns must come before broad ones.
const INTERESTING_PROCESSES = [
  // Emacs & terminals
  { pattern: /emacs.*daemon|emacs.*--daemon/, name: 'Emacs Daemon', icon: '🔮', category: 'editor' },
  { pattern: /emacsclient/, name: 'Emacs Client', icon: '📝', category: 'editor' },
  { pattern: /artery-tui|artery\.mjs/, name: 'Artery TUI', icon: '🩸', category: 'tui' },
  { pattern: /emacs-mcp/, name: 'Emacs MCP', icon: '🧠', category: 'bridge' },

  // Eat terminals (emacs tabs)
  { pattern: /eat.*fishy|🐟.*fishy/, name: '🐟 Fishy', icon: '🐟', category: 'shell' },
  { pattern: /eat.*kidlisp|🧪.*kidlisp/, name: '🧪 KidLisp', icon: '🧪', category: 'shell' },
  { pattern: /eat.*tunnel|🚇.*tunnel/, name: '🚇 Tunnel', icon: '🚇', category: 'shell' },
  { pattern: /eat.*url|⚡.*url/, name: '⚡ URL', icon: '⚡', category: 'shell' },
  { pattern: /eat.*bookmarks|🔖.*bookmarks/, name: '🔖 Bookmarks', icon: '🔖', category: 'shell' },

  // AI - Claude Code (BEFORE vscode-server since their paths contain it)
  { pattern: /native-binary\/claude/, name: 'Claude Code', icon: '🧠', category: 'ai' },
  { pattern: /(?:^|\/)claude(?:\s|$)/, name: 'Claude CLI', icon: '🤖', category: 'ai' },
  { pattern: /ollama/, name: 'Ollama', icon: '🤖', category: 'ai' },

  // VS Code main servers (BEFORE LSP - their cmds contain extension names like mongodb-vscode)
  { pattern: /server-main\.js/, name: 'VS Code Server', icon: '💻', category: 'ide' },
  { pattern: /extensionHost/, name: 'VS Code', icon: '💻', category: 'ide' },
  { pattern: /bootstrap-fork.*fileWatcher/, name: 'VS Code Files', icon: '💻', category: 'ide' },
  { pattern: /ptyHost/, name: 'VS Code PTY', icon: '💻', category: 'ide' },

  // LSP servers (child processes of extensionHost with specific server scripts)
  { pattern: /tsserver/, name: 'TypeScript LSP', icon: '📘', category: 'lsp' },
  { pattern: /vscode-pylance|server\.bundle\.js/, name: 'Pylance', icon: '🐍', category: 'lsp' },
  { pattern: /mongodb.*languageServer/, name: 'MongoDB LSP', icon: '🍃', category: 'lsp' },
  { pattern: /eslint.*server|eslintServer/, name: 'ESLint', icon: '📏', category: 'lsp' },
  { pattern: /prettier/, name: 'Prettier', icon: '✨', category: 'lsp' },
  { pattern: /cssServerMain/, name: 'CSS LSP', icon: '🎨', category: 'lsp' },
  { pattern: /jsonServerMain/, name: 'JSON LSP', icon: '📋', category: 'lsp' },
  { pattern: /copilot-agent|github\.copilot/, name: 'Copilot', icon: '✨', category: 'ai' },

  // Node.js processes (specific before generic)
  { pattern: /node.*session\.mjs/, name: 'Session Server', icon: '🔗', category: 'dev' },
  { pattern: /node.*server\.mjs/, name: 'Main Server', icon: '🖥️', category: 'dev' },
  { pattern: /node.*chat\.mjs/, name: 'Chat Service', icon: '💬', category: 'dev' },
  { pattern: /node.*devcontainer-status/, name: 'Status Server', icon: '📊', category: 'dev' },
  { pattern: /node.*stripe/, name: 'Stripe', icon: '💳', category: 'dev' },
  { pattern: /node.*netlify/, name: 'Netlify CLI', icon: '🌐', category: 'dev' },
  { pattern: /netlify-log-filter/, name: 'Log Filter', icon: '📝', category: 'dev' },
  { pattern: /nodemon/, name: 'Nodemon', icon: '👀', category: 'dev' },
  { pattern: /esbuild/, name: 'esbuild', icon: '⚡', category: 'dev' },

  // Infrastructure
  { pattern: /redis-server/, name: 'Redis', icon: '📦', category: 'db' },
  { pattern: /caddy/, name: 'Caddy', icon: '🌐', category: 'proxy' },
  { pattern: /ngrok|cloudflared/, name: 'Tunnel', icon: '🚇', category: 'proxy' },
  { pattern: /deno/, name: 'Deno', icon: '🦕', category: 'dev' },

  // Generic Node.js (last resort for node processes)
  { pattern: /node(?!.*vscode)(?!.*copilot)/, name: 'Node.js', icon: '🟢', category: 'dev' },

  // Shells
  { pattern: /fish(?!.*vscode)/, name: 'Fish Shell', icon: '🐚', category: 'shell' },
  { pattern: /bash/, name: 'Bash', icon: '🐚', category: 'shell' },
];

// Parse ps output into structured data (with PPID for tree structure)
function parseProcessLine(line) {
  const parts = line.trim().split(/\s+/);
  if (parts.length < 12) return null;
  
  const [user, pid, ppid, cpu, mem, vsz, rss, tty, stat, start, time, ...cmdParts] = parts;
  const cmd = cmdParts.join(' ');
  
  return {
    pid: parseInt(pid),
    ppid: parseInt(ppid),
    user,
    cpu: parseFloat(cpu),
    mem: parseFloat(mem),
    rss: parseInt(rss), // Resident memory in KB
    stat,
    start,
    time,
    cmd,
    cmdShort: cmd.slice(0, 80),
  };
}

// Identify interesting processes - extract meaningful names
function categorizeProcess(proc) {
  const cmd = proc.cmd;
  
  for (const { pattern, name, icon, category } of INTERESTING_PROCESSES) {
    if (pattern.test(cmd)) {
      // Try to extract a more specific name for node processes
      let displayName = name;
      
      // For generic Node.js, extract the script name
      if (name === 'Node.js') {
        const scriptMatch = cmd.match(/node\s+(?:--[^\s]+\s+)*([^\s]+\.m?js)/);
        if (scriptMatch) {
          const script = scriptMatch[1].split('/').pop().replace(/\.m?js$/, '');
          displayName = script.charAt(0).toUpperCase() + script.slice(1);
        }
      }
      
      // For chat services, extract which chat
      if (cmd.includes('chat.mjs')) {
        const chatMatch = cmd.match(/chat\.mjs\s+(\S+)/);
        if (chatMatch) {
          displayName = 'Chat: ' + chatMatch[1].replace('chat-', '');
        }
      }
      
      // Make unique by PID for shells
      if (category === 'shell' && !displayName.includes('🐟') && !displayName.includes('🧪')) {
        displayName = displayName + ' #' + proc.pid;
      }
      
      return { ...proc, name: displayName, icon, category, interesting: true };
    }
  }
  return { ...proc, interesting: false };
}

// Get process tree with hierarchy
async function getProcessTree() {
  try {
    // Include PPID in output for tree structure
    const { stdout } = await execAsync('ps -eo user,pid,ppid,%cpu,%mem,vsz,rss,tty,stat,start,time,args --sort=-rss 2>/dev/null || ps aux');
    const lines = stdout.trim().split('\n').slice(1); // Skip header
    
    const allProcesses = lines
      .map(parseProcessLine)
      .filter(Boolean);
    
    const processes = allProcesses.map(p => categorizeProcess(p));
    
    // Build a map of all PIDs for parent lookup
    const pidMap = new Map();
    allProcesses.forEach(p => pidMap.set(p.pid, p));
    
    // Separate interesting processes from others
    const interesting = processes.filter(p => p.interesting);
    
    // For each interesting process, find its parent chain up to another interesting process
    interesting.forEach(p => {
      // Find nearest interesting ancestor
      let parentPid = p.ppid;
      let depth = 0;
      while (parentPid && parentPid > 1 && depth < 20) {
        const parent = pidMap.get(parentPid);
        if (!parent) break;
        
        // Check if parent is also interesting
        const interestingParent = interesting.find(ip => ip.pid === parentPid);
        if (interestingParent) {
          p.parentInteresting = parentPid;
          break;
        }
        parentPid = parent.ppid;
        depth++;
      }
    });
    
    const topMemory = processes
      .filter(p => !p.interesting && p.rss > 10000) // > 10MB
      .slice(0, 10);
    
    return {
      interesting,
      topMemory,
      total: processes.length,
    };
  } catch (err) {
    return { error: err.message, interesting: [], topMemory: [], total: 0 };
  }
}

// Get emacs buffer status via emacsclient
async function getEmacsStatus() {
  try {
    const { stdout } = await execAsync(
      `/usr/sbin/emacsclient --eval '(ac-mcp-format-state)' 2>/dev/null`,
      { timeout: 2000 }
    );
    // Parse the elisp string result
    const state = stdout.trim().replace(/^"|"$/g, '');
    return { online: true, state };
  } catch {
    return { online: false, state: null };
  }
}

// Get system overview
function getSystemInfo() {
  const totalMem = os.totalmem();
  const freeMem = os.freemem();
  const usedMem = totalMem - freeMem;
  const loadAvg = os.loadavg();
  const uptime = os.uptime();
  
  return {
    hostname: os.hostname(),
    platform: os.platform(),
    arch: os.arch(),
    cpus: os.cpus().length,
    memory: {
      total: Math.round(totalMem / 1024 / 1024), // MB
      used: Math.round(usedMem / 1024 / 1024),
      free: Math.round(freeMem / 1024 / 1024),
      percent: Math.round((usedMem / totalMem) * 100),
    },
    load: {
      '1m': loadAvg[0].toFixed(2),
      '5m': loadAvg[1].toFixed(2),
      '15m': loadAvg[2].toFixed(2),
    },
    uptime: {
      seconds: uptime,
      formatted: formatUptime(uptime),
    },
  };
}

function formatUptime(seconds) {
  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const mins = Math.floor((seconds % 3600) / 60);
  
  if (days > 0) return `${days}d ${hours}h`;
  if (hours > 0) return `${hours}h ${mins}m`;
  return `${mins}m`;
}

// Get full status
async function getFullStatus() {
  const [processes, emacs, system] = await Promise.all([
    getProcessTree(),
    getEmacsStatus(),
    Promise.resolve(getSystemInfo()),
  ]);
  
  return {
    timestamp: Date.now(),
    system,
    emacs,
    processes,
  };
}

// Main entry point
async function main() {
  const args = process.argv.slice(2);
  
  if (args.includes('--server')) {
    // HTTP + WebSocket server mode
    const PORT = parseInt(process.env.STATUS_PORT) || 7890;
    
    const server = createServer(async (req, res) => {
      // CORS headers for VS Code webview
      res.setHeader('Access-Control-Allow-Origin', '*');
      res.setHeader('Access-Control-Allow-Methods', 'GET');
      
      // Parse URL to handle query strings
      const url = new URL(req.url, `http://${req.headers.host}`);
      const pathname = url.pathname;
      
      if (pathname === '/status') {
        res.setHeader('Content-Type', 'application/json');
        const status = await getFullStatus();
        res.writeHead(200);
        res.end(JSON.stringify(status, null, 2));
        
      } else if (pathname === '/stream') {
        // SSE stream for live updates
        res.writeHead(200, {
          'Content-Type': 'text/event-stream',
          'Cache-Control': 'no-cache',
          'Connection': 'keep-alive',
        });
        
        const sendUpdate = async () => {
          const status = await getFullStatus();
          res.write(`data: ${JSON.stringify(status)}\n\n`);
        };
        
        await sendUpdate();
        const interval = setInterval(sendUpdate, 2000);
        
        req.on('close', () => {
          clearInterval(interval);
        });
        
      } else if (pathname === '/' || pathname === '/dashboard' || pathname === '/index.html') {
        // Serve the interactive D3.js dashboard HTML
        res.setHeader('Content-Type', 'text/html');
        res.writeHead(200);
        res.end(getDashboardHTML());
        
      } else {
        res.setHeader('Content-Type', 'application/json');
        res.writeHead(404);
        res.end(JSON.stringify({ error: 'Not found' }));
      }
    });
    
    // WebSocket server for real-time updates
    const wss = new WebSocketServer({ server, path: '/ws' });
    
    wss.on('connection', (ws) => {
      console.log('🔌 WebSocket client connected');
      
      // Send initial status immediately
      getFullStatus().then(status => {
        ws.send(JSON.stringify(status));
      });
      
      // Send updates every 1.5 seconds
      const interval = setInterval(async () => {
        if (ws.readyState === ws.OPEN) {
          const status = await getFullStatus();
          ws.send(JSON.stringify(status));
        }
      }, 1500);
      
      ws.on('close', () => {
        console.log('🔌 WebSocket client disconnected');
        clearInterval(interval);
      });
      
      ws.on('error', (err) => {
        console.error('WebSocket error:', err);
        clearInterval(interval);
      });
    });
    
    server.listen(PORT, '0.0.0.0', () => {
      console.log(`🔍 Devcontainer Status Server running on http://localhost:${PORT}`);
      console.log(`   GET /           - Interactive D3.js dashboard`);
      console.log(`   GET /status     - JSON status snapshot`);
      console.log(`   GET /stream     - SSE live updates`);
      console.log(`   WS  /ws         - WebSocket real-time updates`);
    });
    
  } else if (args.includes('--watch')) {
    // Continuous output mode
    const update = async () => {
      const status = await getFullStatus();
      console.clear();
      console.log(JSON.stringify(status, null, 2));
    };
    
    await update();
    setInterval(update, 2000);
    
  } else {
    // Single JSON output (default)
    const status = await getFullStatus();
    console.log(JSON.stringify(status, null, 2));
  }
}

// Generate the interactive dashboard HTML with D3.js
function getDashboardHTML() {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Aesthetic Computer</title>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"></script>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      background: #000;
      color: #fff;
      font-family: monospace;
      overflow: hidden;
      height: 100vh;
    }
    #canvas { position: absolute; top: 0; left: 0; }
    .hud {
      position: absolute;
      z-index: 100;
      pointer-events: none;
    }
    .title {
      top: 16px;
      left: 16px;
      font-size: 14px;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .title .dot { color: #ff69b4; }
    .status-dot {
      width: 6px;
      height: 6px;
      border-radius: 50%;
      background: #ff69b4;
    }
    .status-dot.online { background: #0f0; }
    .stats {
      top: 16px;
      right: 16px;
      text-align: right;
      font-size: 12px;
      color: #555;
    }
    .stats .val { color: #fff; }
    .mem {
      bottom: 16px;
      right: 16px;
      font-size: 12px;
      color: #555;
    }
    .center {
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      text-align: center;
      font-size: 12px;
      color: #555;
    }
    .center .count {
      font-size: 28px;
      color: #fff;
      margin-bottom: 4px;
    }
    .label-container {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      pointer-events: none;
      z-index: 50;
      overflow: hidden;
    }
    .proc-label {
      position: absolute;
      text-align: center;
      transform: translate(-50%, -100%);
      white-space: nowrap;
      text-shadow: 0 0 3px #000, 0 0 6px #000;
      pointer-events: none;
    }
    .proc-label .icon { font-size: 18px; display: block; line-height: 1; }
    .proc-label .name { font-size: 10px; margin-top: 2px; font-weight: bold; letter-spacing: 0.3px; }
    .proc-label .info { font-size: 8px; color: #aaa; margin-top: 1px; }
    .proc-label .bar { 
      width: 80px; 
      height: 5px; 
      background: #333; 
      margin: 5px auto 0; 
      border-radius: 3px;
      overflow: hidden;
    }
    .proc-label .bar-fill { 
      height: 100%; 
      border-radius: 3px;
      transition: width 0.3s ease;
    }
  </style>
</head>
<body>
  <canvas id="canvas"></canvas>
  
  <div class="hud title">
    <div class="status-dot" id="status-dot"></div>
    <span>Aesthetic<span class="dot">.</span>Computer Architecture</span>
  </div>
  
  <div class="hud stats">
    <div><span class="val" id="uptime">—</span></div>
    <div><span class="val" id="cpus">—</span> cpus</div>
  </div>
  
  <div class="hud center">
    <div class="count" id="process-count">0</div>
    <div>processes</div>
  </div>
  
  <div class="hud mem">
    <span id="mem-text">— / —</span> MB
  </div>
  
  <div id="labels" class="label-container"></div>
  
  <script>
    const colors = {
      'editor': 0xb06bff, 'tui': 0xff69b4, 'bridge': 0x6bff9f,
      'db': 0xffeb6b, 'proxy': 0x6b9fff, 'ai': 0xff9f6b,
      'shell': 0x6bffff, 'dev': 0x6bff9f, 'ide': 0x6b9fff, 'lsp': 0x888888
    };
    
    let width = window.innerWidth, height = window.innerHeight;
    let meshes = new Map(), connections = new Map(), ws;
    let graveyard = [];  // Dead processes
    const MAX_GRAVEYARD = 30;  // Keep last 30 dead processes
    const GRAVEYARD_Y = -200;  // Y position for graveyard
    
    // Three.js setup
    const scene = new THREE.Scene();
    const camera = new THREE.PerspectiveCamera(50, width / height, 0.1, 5000);
    camera.position.set(0, 150, 400);
    camera.lookAt(0, 0, 0);
    
    const renderer = new THREE.WebGLRenderer({ canvas: document.getElementById('canvas'), antialias: true });
    renderer.setSize(width, height);
    renderer.setPixelRatio(window.devicePixelRatio);
    
    // OrbitControls for mouse interaction
    const controls = new THREE.OrbitControls(camera, renderer.domElement);
    controls.enableDamping = true;
    controls.dampingFactor = 0.05;
    controls.minDistance = 20;
    controls.maxDistance = 3000;
    controls.enablePan = true;
    controls.autoRotate = true;
    controls.autoRotateSpeed = 0.3;
    controls.target.set(0, 0, 0);
    
    // Focus mode - click on node to orbit around it
    let focusedPid = null;
    let focusTarget = new THREE.Vector3(0, 0, 0);
    let focusDistance = null;  // null = free zoom
    let transitioning = false;
    
    // Raycaster for click detection
    const raycaster = new THREE.Raycaster();
    const mouse = new THREE.Vector2();
    
    renderer.domElement.addEventListener('click', (e) => {
      mouse.x = (e.clientX / width) * 2 - 1;
      mouse.y = -(e.clientY / height) * 2 + 1;
      
      raycaster.setFromCamera(mouse, camera);
      const meshArray = Array.from(meshes.values());
      const intersects = raycaster.intersectObjects(meshArray);
      
      if (intersects.length > 0) {
        const clicked = intersects[0].object;
        const pid = clicked.userData.pid;
        
        if (focusedPid === String(pid)) {
          // Click same node = unfocus
          focusedPid = null;
          focusTarget.set(0, 0, 0);
          focusDistance = null;  // free zoom
        } else {
          // Focus on this node
          focusedPid = String(pid);
          focusTarget.copy(clicked.position);
          focusDistance = 80 + (clicked.userData.size || 6) * 3;
        }
        transitioning = true;
        controls.autoRotate = true;
      } else if (!e.shiftKey) {
        // Click empty space = unfocus (unless shift held)
        focusedPid = null;
        focusTarget.set(0, 0, 0);
        focusDistance = null;  // free zoom
        transitioning = true;
      }
    });
    
    // Double-click to hard reset view
    renderer.domElement.addEventListener('dblclick', () => {
      focusedPid = null;
      focusTarget.set(0, 0, 0);
      focusDistance = null;
      transitioning = true;
      camera.position.set(0, 150, 400);
    });
    
    // Store tree structure
    let processTree = { roots: [], byPid: new Map() };
    
    // Create central kernel/system node - the centroid
    let kernelMesh = null;
    let kernelGlow = null;
    let kernelCore = null;
    function createKernelNode() {
      const group = new THREE.Group();
      
      // Outer wireframe sphere (large)
      const outerGeo = new THREE.SphereGeometry(35, 32, 32);
      const outerMat = new THREE.MeshBasicMaterial({
        color: 0x4488ff,
        transparent: true,
        opacity: 0.15,
        wireframe: true
      });
      const outer = new THREE.Mesh(outerGeo, outerMat);
      group.add(outer);
      
      // Middle ring
      const ringGeo = new THREE.TorusGeometry(25, 1.5, 8, 48);
      const ringMat = new THREE.MeshBasicMaterial({
        color: 0x66aaff,
        transparent: true,
        opacity: 0.4
      });
      const ring = new THREE.Mesh(ringGeo, ringMat);
      ring.rotation.x = Math.PI / 2;
      group.add(ring);
      kernelGlow = ring;
      
      // Inner solid core
      const coreGeo = new THREE.SphereGeometry(12, 24, 24);
      const coreMat = new THREE.MeshBasicMaterial({
        color: 0x88ccff,
        transparent: true,
        opacity: 0.7
      });
      const core = new THREE.Mesh(coreGeo, coreMat);
      group.add(core);
      kernelCore = core;
      
      group.userData = {
        pid: 'kernel',
        name: 'Fedora Linux 43',
        icon: '🐧',
        category: 'kernel',
        cpu: 0,
        rss: 0,
        size: 35,
        targetPos: new THREE.Vector3(0, 0, 0),
        pulsePhase: 0
      };
      group.position.set(0, 0, 0);
      return group;
    }
    
    kernelMesh = createKernelNode();
    scene.add(kernelMesh);
    meshes.set('kernel', kernelMesh);
    
    function createNodeMesh(node, depth = 0, index = 0, siblingCount = 1) {
      const cpu = node.cpu || 0;
      const memMB = (node.rss || 10000) / 1024;
      const baseColor = colors[node.category] || 0x666666;
      
      // Create a small sphere for the node
      const size = Math.max(4, Math.min(12, 3 + memMB * 0.05 + cpu * 0.1));
      const geo = new THREE.SphereGeometry(size, 12, 12);
      const mat = new THREE.MeshBasicMaterial({
        color: baseColor,
        transparent: true,
        opacity: 0.7 + cpu * 0.003
      });
      
      const mesh = new THREE.Mesh(geo, mat);
      mesh.userData = { 
        ...node, 
        size,
        depth,
        index,
        siblingCount,
        baseColor,
        targetPos: new THREE.Vector3(),
        pulsePhase: Math.random() * Math.PI * 2
      };
      
      return mesh;
    }
    
    function createConnectionLine(parentMesh, childMesh) {
      // Use a cylinder for thick lines
      const geo = new THREE.CylinderGeometry(1.5, 1.5, 1, 8);
      const mat = new THREE.MeshBasicMaterial({
        color: 0x444444,
        transparent: true,
        opacity: 0.5
      });
      const mesh = new THREE.Mesh(geo, mat);
      mesh.userData.isCylinder = true;
      return mesh;
    }
    
    function updateConnectionMesh(conn, childPos, parentPos) {
      const mesh = conn.line;
      // Calculate midpoint
      const mid = new THREE.Vector3().addVectors(childPos, parentPos).multiplyScalar(0.5);
      mesh.position.copy(mid);
      
      // Calculate length and direction
      const dir = new THREE.Vector3().subVectors(parentPos, childPos);
      const length = dir.length();
      mesh.scale.set(1, length, 1);
      
      // Orient cylinder to point from child to parent
      mesh.quaternion.setFromUnitVectors(
        new THREE.Vector3(0, 1, 0),
        dir.normalize()
      );
    }
    
    function layoutTree(processes) {
      // Build tree structure
      const byPid = new Map();
      const children = new Map();
      
      processes.forEach(p => {
        byPid.set(String(p.pid), p);
        children.set(String(p.pid), []);
      });
      
      // Find roots (no interesting parent) and build child lists
      const roots = [];
      processes.forEach(p => {
        const parentPid = String(p.parentInteresting || 0);
        if (parentPid && byPid.has(parentPid)) {
          children.get(parentPid).push(p);
        } else {
          roots.push(p);
        }
      });
      
      // Group roots by category for better organization
      const categoryOrder = ['ide', 'editor', 'tui', 'dev', 'db', 'shell', 'ai', 'lsp', 'proxy', 'bridge'];
      roots.sort((a, b) => {
        const ai = categoryOrder.indexOf(a.category);
        const bi = categoryOrder.indexOf(b.category);
        return (ai === -1 ? 99 : ai) - (bi === -1 ? 99 : bi);
      });
      
      // Tighter layout
      const levelHeight = 50;   // Vertical spacing between levels
      const baseRadius = 100;   // Starting radius for roots
      
      // Count total descendants for sizing
      function countDescendants(pid) {
        const nodeChildren = children.get(pid) || [];
        let count = nodeChildren.length;
        nodeChildren.forEach(c => count += countDescendants(String(c.pid)));
        return count;
      }
      
      function positionNode(node, depth, angle, radius, parentX, parentZ) {
        const pid = String(node.pid);
        const nodeChildren = children.get(pid) || [];
        const childCount = nodeChildren.length;
        
        // Position based on angle from parent
        const x = parentX + Math.cos(angle) * radius;
        const z = parentZ + Math.sin(angle) * radius;
        
        node.targetX = x;
        node.targetY = -depth * levelHeight;
        node.targetZ = z;
        
        // Position children in an arc spreading outward
        if (childCount > 0) {
          // Spread children over an arc (not full circle)
          const arcSpread = Math.min(Math.PI * 0.9, Math.PI * 0.3 * childCount);
          const startAngle = angle - arcSpread / 2;
          const childRadius = 35 + childCount * 10;
          
          nodeChildren.forEach((child, i) => {
            const childAngle = childCount === 1 
              ? angle 
              : startAngle + (arcSpread / (childCount - 1)) * i;
            positionNode(child, depth + 1, childAngle, childRadius, x, z);
          });
        }
      }
      
      // Position root nodes in a large circle, spaced by their subtree size
      const totalRoots = roots.length;
      if (totalRoots > 0) {
        // Calculate weights based on descendant count
        const weights = roots.map(r => 1 + countDescendants(String(r.pid)) * 0.5);
        const totalWeight = weights.reduce((a, b) => a + b, 0);
        
        let currentAngle = -Math.PI / 2; // Start at top
        roots.forEach((root, i) => {
          const angleSpan = (weights[i] / totalWeight) * Math.PI * 2;
          const angle = currentAngle + angleSpan / 2;
          currentAngle += angleSpan;
          
          positionNode(root, 0, angle, baseRadius, 0, 0);
        });
      }
      
      return { roots, byPid, children };
    }
    
    function updateLabels() {
      const container = document.getElementById('labels');
      container.innerHTML = '';
      
      scene.updateMatrixWorld();
      
      meshes.forEach((mesh, pid) => {
        const pos = new THREE.Vector3();
        mesh.getWorldPosition(pos);
        
        // Offset label just above the node (closer)
        const labelPos = pos.clone();
        labelPos.y += (mesh.userData.size || 8) + 5;
        
        // Project to screen
        labelPos.project(camera);
        
        const x = (labelPos.x * 0.5 + 0.5) * width;
        const y = (-labelPos.y * 0.5 + 0.5) * height;
        
        // Only show if in front of camera and on screen
        if (labelPos.z < 1 && x > -100 && x < width + 100 && y > -100 && y < height + 100) {
          const d = mesh.userData;
          const color = '#' + (colors[d.category] || 0x666666).toString(16).padStart(6, '0');
          
          // Calculate distance from camera to this node
          const distToCamera = camera.position.distanceTo(pos);
          // Scale up when close (inverse relationship) - closer = bigger
          const proximityScale = Math.max(0.4, Math.min(3, 150 / distToCamera));
          
          // Fade labels that are far away
          const opacity = focusedPid 
            ? (pid === focusedPid ? 1 : (d.parentInteresting === parseInt(focusedPid) ? 0.9 : 0.3))
            : Math.max(0.5, Math.min(1, 300 / distToCamera));
          
          const cpuPct = Math.min(100, d.cpu || 0);
          const memMB = ((d.rss || 0) / 1024).toFixed(0);
          
          const label = document.createElement('div');
          label.className = 'proc-label';
          label.style.left = x + 'px';
          label.style.top = y + 'px';
          label.style.opacity = opacity;
          label.style.transform = \`translate(-50%, -100%) scale(\${proximityScale})\`;
          label.innerHTML = \`
            <div class="icon">\${d.icon || '●'}</div>
            <div class="name" style="color:\${color}">\${d.name || pid}</div>
            <div class="info">\${memMB}MB · \${cpuPct.toFixed(0)}%</div>
          \`;
          container.appendChild(label);
        }
      });
    }
    
    function updateViz(processData) {
      if (!processData?.interesting) return;
      
      const processes = processData.interesting;
      document.getElementById('process-count').textContent = processes.length;
      
      // Layout the tree
      processTree = layoutTree(processes);
      
      // Create a set of current PIDs
      const currentPids = new Set(processes.map(p => String(p.pid)));
      
      // Add/update nodes
      processes.forEach(p => {
        const pid = String(p.pid);
        
        if (!meshes.has(pid)) {
          const mesh = createNodeMesh(p);
          // Initialize at target position
          mesh.position.set(p.targetX || 0, p.targetY || 0, p.targetZ || 0);
          mesh.userData.targetPos.set(p.targetX || 0, p.targetY || 0, p.targetZ || 0);
          scene.add(mesh);
          meshes.set(pid, mesh);
        } else {
          const mesh = meshes.get(pid);
          const d = mesh.userData;
          d.cpu = p.cpu;
          d.mem = p.mem;
          d.rss = p.rss;
          d.name = p.name;
          
          // Update target position from tree layout
          d.targetPos.set(p.targetX || d.targetPos.x, p.targetY || d.targetPos.y, p.targetZ || d.targetPos.z);
          
          // Update size based on activity
          const memMB = (p.rss || 10000) / 1024;
          d.size = Math.max(4, Math.min(12, 3 + memMB * 0.05 + p.cpu * 0.1));
          mesh.scale.setScalar(d.size / 6);
          
          // Update color
          const baseColor = colors[p.category] || 0x666666;
          const brighten = Math.min(1.8, 1 + p.cpu * 0.02);
          const r = ((baseColor >> 16) & 255) * brighten;
          const g = ((baseColor >> 8) & 255) * brighten;
          const b = (baseColor & 255) * brighten;
          mesh.material.color.setRGB(Math.min(255, r) / 255, Math.min(255, g) / 255, Math.min(255, b) / 255);
          mesh.material.opacity = 0.7 + p.cpu * 0.003;
        }
        
        // Create/update connection to parent (or kernel for roots)
        const parentPid = String(p.parentInteresting || 0);
        if (parentPid && meshes.has(parentPid)) {
          const connKey = pid + '->' + parentPid;
          if (!connections.has(connKey)) {
            const line = createConnectionLine();
            scene.add(line);
            connections.set(connKey, { line, childPid: pid, parentPid });
          }
        } else {
          // Root process - connect to kernel
          const connKey = pid + '->kernel';
          if (!connections.has(connKey)) {
            const line = createConnectionLine();
            scene.add(line);
            connections.set(connKey, { line, childPid: pid, parentPid: 'kernel' });
          }
        }
      });
      
      // Move dead processes to graveyard instead of removing
      meshes.forEach((mesh, pid) => {
        if (pid === 'kernel') return;  // Never kill kernel
        if (!currentPids.has(pid) && !mesh.userData.isDead) {
          // Mark as dead and send to graveyard
          mesh.userData.isDead = true;
          mesh.userData.deathTime = Date.now();
          
          // Calculate graveyard position
          const graveyardIndex = graveyard.length;
          const col = graveyardIndex % 10;
          const row = Math.floor(graveyardIndex / 10);
          mesh.userData.targetPos.set(
            (col - 4.5) * 25,
            GRAVEYARD_Y - row * 20,
            0
          );
          
          // Dim the mesh
          mesh.material.opacity = 0.25;
          mesh.material.color.setHex(0x444444);
          
          graveyard.push({ pid, mesh, name: mesh.userData.name, deathTime: Date.now() });
          
          // Remove from active meshes (but keep in scene)
          meshes.delete(pid);
          
          // Limit graveyard size
          while (graveyard.length > MAX_GRAVEYARD) {
            const oldest = graveyard.shift();
            scene.remove(oldest.mesh);
            if (oldest.mesh.geometry) oldest.mesh.geometry.dispose();
            if (oldest.mesh.material) oldest.mesh.material.dispose();
          }
        }
      });
      
      // Remove orphan connections (but not to graveyard)
      const graveyardPids = new Set(graveyard.map(g => g.pid));
      connections.forEach((conn, key) => {
        const childExists = meshes.has(conn.childPid) || graveyardPids.has(conn.childPid);
        const parentExists = meshes.has(conn.parentPid) || graveyardPids.has(conn.parentPid);
        if (!childExists || !parentExists) {
          scene.remove(conn.line);
          conn.line.geometry.dispose();
          conn.line.material.dispose();
          connections.delete(key);
        }
      });
    }
    
    let time = 0;
    function animate() {
      requestAnimationFrame(animate);
      time += 0.016;
      
      // Smooth transition to focus target
      if (focusedPid && meshes.has(focusedPid)) {
        const focusMesh = meshes.get(focusedPid);
        focusTarget.lerp(focusMesh.position, 0.08);
      }
      
      // Transition camera orbit target
      controls.target.lerp(focusTarget, transitioning ? 0.06 : 0.02);
      
      // Adjust zoom only when focused on a node (focusDistance is set)
      if (focusDistance !== null) {
        const currentDist = camera.position.distanceTo(controls.target);
        if (Math.abs(currentDist - focusDistance) > 5) {
          const dir = camera.position.clone().sub(controls.target).normalize();
          const targetPos = controls.target.clone().add(dir.multiplyScalar(focusDistance));
          camera.position.lerp(targetPos, 0.04);
        } else {
          transitioning = false;
        }
      } else {
        transitioning = false;
      }
      
      controls.update();
      
      // Animate kernel centroid
      if (kernelGlow) {
        kernelGlow.rotation.z = time * 0.3;
        kernelGlow.rotation.x = Math.PI / 2 + Math.sin(time * 0.5) * 0.1;
      }
      if (kernelCore) {
        const pulse = 1 + Math.sin(time * 0.8) * 0.1;
        kernelCore.scale.setScalar(pulse);
      }
      if (kernelMesh) {
        kernelMesh.rotation.y = time * 0.1;
      }
      
      // Animate graveyard processes (falling, fading)
      graveyard.forEach((grave, i) => {
        const mesh = grave.mesh;
        if (mesh && mesh.userData) {
          const d = mesh.userData;
          // Slowly drift toward graveyard position
          mesh.position.x += (d.targetPos.x - mesh.position.x) * 0.02;
          mesh.position.y += (d.targetPos.y - mesh.position.y) * 0.015;
          mesh.position.z += (d.targetPos.z - mesh.position.z) * 0.02;
          
          // Gentle sway
          mesh.position.x += Math.sin(time * 0.3 + i) * 0.05;
          
          // Fade older ones more
          const age = (Date.now() - grave.deathTime) / 1000;
          mesh.material.opacity = Math.max(0.1, 0.3 - age * 0.005);
        }
      });
      
      // Animate nodes toward their target positions
      meshes.forEach((mesh, pid) => {
        const d = mesh.userData;
        const cpu = d.cpu || 0;
        const isFocused = focusedPid === pid;
        const isRelated = focusedPid && (d.parentInteresting === parseInt(focusedPid) || String(d.parentInteresting) === focusedPid);
        
        // Smoothly move toward target
        mesh.position.x += (d.targetPos.x - mesh.position.x) * 0.03;
        mesh.position.y += (d.targetPos.y - mesh.position.y) * 0.03;
        mesh.position.z += (d.targetPos.z - mesh.position.z) * 0.03;
        
        // Add gentle floating
        const float = Math.sin(time * 0.5 + d.pulsePhase) * 2;
        mesh.position.y += float * 0.02;
        
        // Pulse based on CPU (stronger when focused)
        const pulseAmp = isFocused ? 0.2 : (0.1 + cpu * 0.005);
        const pulse = 1 + Math.sin(time * (1 + cpu * 0.05) + d.pulsePhase) * pulseAmp;
        const sizeMultiplier = isFocused ? 1.5 : (isRelated ? 1.2 : 1);
        mesh.scale.setScalar((d.size / 6) * pulse * sizeMultiplier);
        
        // Adjust opacity based on focus state
        if (focusedPid) {
          mesh.material.opacity = isFocused ? 1 : (isRelated ? 0.8 : 0.3);
        } else {
          mesh.material.opacity = 0.7 + cpu * 0.003;
        }
      });
      
      // Update connection lines
      connections.forEach(conn => {
        const childMesh = meshes.get(conn.childPid);
        const parentMesh = meshes.get(conn.parentPid);
        if (childMesh && parentMesh) {
          updateConnectionMesh(conn, childMesh.position, parentMesh.position);
          
          // Highlight connections to focused node
          const involvesFocus = focusedPid && (conn.childPid === focusedPid || conn.parentPid === focusedPid);
          conn.line.material.opacity = focusedPid ? (involvesFocus ? 0.9 : 0.15) : 0.5;
          conn.line.material.color.setHex(involvesFocus ? 0xff69b4 : 0x444444);
          // Make focused connections thicker
          const thickness = involvesFocus ? 2.5 : 1.5;
          conn.line.scale.x = thickness / 1.5;
          conn.line.scale.z = thickness / 1.5;
        }
      });
      
      renderer.render(scene, camera);
      updateLabels();
    }
    
    function connectWS() {
      ws = new WebSocket('ws://' + location.host + '/ws');
      ws.onopen = () => document.getElementById('status-dot').classList.add('online');
      ws.onclose = () => {
        document.getElementById('status-dot').classList.remove('online');
        setTimeout(connectWS, 2000);
      };
      ws.onerror = () => ws.close();
      ws.onmessage = (e) => {
        try {
          const data = JSON.parse(e.data);
          if (data.system) {
            document.getElementById('uptime').textContent = data.system.uptime.formatted;
            document.getElementById('cpus').textContent = data.system.cpus;
            const m = data.system.memory;
            document.getElementById('mem-text').textContent = m.used + ' / ' + m.total;
          }
          updateViz(data.processes);
        } catch {}
      };
    }
    
    window.addEventListener('resize', () => {
      width = window.innerWidth;
      height = window.innerHeight;
      camera.aspect = width / height;
      camera.updateProjectionMatrix();
      renderer.setSize(width, height);
    });
    
    animate();
    connectWS();
  </script>
</body>
</html>`;
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
  process.exit(1);
});
