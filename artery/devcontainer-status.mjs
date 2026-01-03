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
const INTERESTING_PROCESSES = [
  { pattern: /emacs/, name: 'Emacs Daemon', icon: '🔮', category: 'editor' },
  { pattern: /artery-tui|artery\.mjs/, name: 'Artery TUI', icon: '🩸', category: 'tui' },
  { pattern: /emacs-mcp/, name: 'Emacs MCP', icon: '🧠', category: 'bridge' },
  { pattern: /redis-server/, name: 'Redis', icon: '📦', category: 'db' },
  { pattern: /caddy/, name: 'Caddy', icon: '🌐', category: 'proxy' },
  { pattern: /ollama/, name: 'Ollama', icon: '🤖', category: 'ai' },
  { pattern: /fish.*-fishy|eat-exec-fish/, name: 'Fish Shell', icon: '🐟', category: 'shell' },
  { pattern: /netlify.*dev|site.*dev/, name: 'Dev Server', icon: '⚡', category: 'dev' },
  { pattern: /node.*session-server/, name: 'Session Server', icon: '🔗', category: 'dev' },
  { pattern: /vscode-server/, name: 'VS Code', icon: '💻', category: 'ide' },
  { pattern: /tsserver/, name: 'TypeScript', icon: '📘', category: 'lsp' },
  { pattern: /mongodb-vscode/, name: 'MongoDB LSP', icon: '🍃', category: 'lsp' },
  { pattern: /copilot/, name: 'Copilot', icon: '✨', category: 'ai' },
];

// Parse ps output into structured data
function parseProcessLine(line) {
  const parts = line.trim().split(/\s+/);
  if (parts.length < 11) return null;
  
  const [user, pid, cpu, mem, vsz, rss, tty, stat, start, time, ...cmdParts] = parts;
  const cmd = cmdParts.join(' ');
  
  return {
    pid: parseInt(pid),
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

// Identify interesting processes
function categorizeProcess(proc) {
  for (const { pattern, name, icon, category } of INTERESTING_PROCESSES) {
    if (pattern.test(proc.cmd)) {
      return { ...proc, name, icon, category, interesting: true };
    }
  }
  return { ...proc, interesting: false };
}

// Get process tree with hierarchy
async function getProcessTree() {
  try {
    const { stdout } = await execAsync('ps aux --sort=-rss 2>/dev/null || ps aux');
    const lines = stdout.trim().split('\n').slice(1); // Skip header
    
    const processes = lines
      .map(parseProcessLine)
      .filter(Boolean)
      .map(categorizeProcess);
    
    // Separate interesting processes from others
    const interesting = processes.filter(p => p.interesting);
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
      
      if (req.url === '/status') {
        res.setHeader('Content-Type', 'application/json');
        const status = await getFullStatus();
        res.writeHead(200);
        res.end(JSON.stringify(status, null, 2));
        
      } else if (req.url === '/stream') {
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
        
      } else if (req.url === '/' || req.url === '/dashboard') {
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
  <title>�️ Devcontainer</title>
  <script src="https://d3js.org/d3.v7.min.js"></script>
  <link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&display=swap" rel="stylesheet">
  <style>
    :root {
      --bg: #0a0a0a;
      --text: #f0f0f0;
      --text-dim: #666;
      --accent: #ff6b9d;
      --blue: #6b9fff;
      --green: #6bff9f;
      --yellow: #ffeb6b;
      --purple: #b06bff;
      --orange: #ff9f6b;
      --cyan: #6bffff;
    }
    
    * { margin: 0; padding: 0; box-sizing: border-box; }
    
    body {
      background: var(--bg);
      color: var(--text);
      font-family: 'IBM Plex Mono', monospace;
      overflow: hidden;
      height: 100vh;
    }
    
    #canvas {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }
    
    /* HUD Overlay */
    .hud {
      position: absolute;
      pointer-events: none;
      z-index: 100;
    }
    
    .hud-top {
      top: 16px;
      left: 16px;
      right: 16px;
      display: flex;
      justify-content: space-between;
      align-items: flex-start;
    }
    
    .hud-title {
      font-size: 0.9rem;
      font-weight: 500;
      display: flex;
      align-items: center;
      gap: 10px;
    }
    
    .hud-title .dot {
      color: var(--accent);
    }
    
    .status-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background: var(--green);
      box-shadow: 0 0 10px var(--green);
      animation: pulse 2s infinite;
    }
    
    .status-dot.offline {
      background: var(--accent);
      box-shadow: 0 0 10px var(--accent);
      animation: none;
    }
    
    @keyframes pulse {
      0%, 100% { opacity: 1; }
      50% { opacity: 0.5; }
    }
    
    .hud-stats {
      text-align: right;
      font-size: 0.7rem;
      color: var(--text-dim);
      line-height: 1.6;
    }
    
    .hud-stats .value {
      color: var(--text);
      font-weight: 500;
    }
    
    .hud-bottom {
      bottom: 16px;
      left: 16px;
      right: 16px;
      display: flex;
      justify-content: space-between;
    }
    
    .legend {
      display: flex;
      gap: 16px;
      font-size: 0.65rem;
      color: var(--text-dim);
    }
    
    .legend-item {
      display: flex;
      align-items: center;
      gap: 6px;
    }
    
    .legend-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
    }
    
    /* Memory bar */
    .memory-bar {
      width: 200px;
      height: 6px;
      background: rgba(255,255,255,0.1);
      border-radius: 3px;
      overflow: hidden;
    }
    
    .memory-fill {
      height: 100%;
      background: linear-gradient(90deg, var(--green), var(--cyan));
      transition: width 0.5s;
    }
    
    .memory-fill.warning { background: linear-gradient(90deg, var(--yellow), var(--orange)); }
    .memory-fill.danger { background: linear-gradient(90deg, var(--accent), var(--orange)); }
    
    /* Tooltip */
    .tooltip {
      position: absolute;
      background: rgba(20, 20, 20, 0.95);
      border: 1px solid var(--accent);
      border-radius: 6px;
      padding: 10px 14px;
      font-size: 0.75rem;
      pointer-events: none;
      opacity: 0;
      transition: opacity 0.15s;
      max-width: 250px;
      box-shadow: 0 0 20px rgba(255, 107, 157, 0.2);
    }
    
    .tooltip.visible { opacity: 1; }
    
    .tooltip h3 {
      font-size: 0.85rem;
      margin-bottom: 6px;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    
    .tooltip-stat {
      display: flex;
      justify-content: space-between;
      color: var(--text-dim);
      margin: 2px 0;
    }
    
    .tooltip-stat .val {
      color: var(--text);
    }
    
    /* Center info */
    .center-info {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      text-align: center;
      pointer-events: none;
    }
    
    .center-icon {
      font-size: 2rem;
      margin-bottom: 8px;
    }
    
    .center-label {
      font-size: 0.7rem;
      color: var(--text-dim);
      text-transform: uppercase;
      letter-spacing: 0.1em;
    }
    
    .center-value {
      font-size: 1.5rem;
      font-weight: 600;
      color: var(--cyan);
    }
  </style>
</head>
<body>
  <svg id="canvas"></svg>
  
  <div class="hud hud-top">
    <div class="hud-title">
      <div class="status-dot" id="status-dot"></div>
      <span>Aesthetic<span class="dot">.</span>Devcontainer</span>
    </div>
    <div class="hud-stats">
      <div><span class="value" id="hostname">—</span></div>
      <div>Uptime: <span class="value" id="uptime">—</span></div>
      <div>CPUs: <span class="value" id="cpus">—</span></div>
      <div id="timestamp"></div>
    </div>
  </div>
  
  <div class="center-info">
    <div class="center-icon">🔮</div>
    <div class="center-label">Emacs</div>
    <div class="center-value" id="emacs-state">—</div>
  </div>
  
  <div class="hud hud-bottom">
    <div class="legend">
      <div class="legend-item"><div class="legend-dot" style="background: var(--purple)"></div> Emacs</div>
      <div class="legend-item"><div class="legend-dot" style="background: var(--green)"></div> Node</div>
      <div class="legend-item"><div class="legend-dot" style="background: var(--blue)"></div> VS Code</div>
      <div class="legend-item"><div class="legend-dot" style="background: var(--orange)"></div> Server</div>
      <div class="legend-item"><div class="legend-dot" style="background: var(--cyan)"></div> System</div>
    </div>
    <div>
      <div style="font-size: 0.65rem; color: var(--text-dim); margin-bottom: 4px;">
        Memory: <span id="mem-text">— / —</span>
      </div>
      <div class="memory-bar">
        <div class="memory-fill" id="mem-bar" style="width: 0%"></div>
      </div>
    </div>
  </div>
  
  <div class="tooltip" id="tooltip"></div>
  
  <script>
    // State
    let processes = [];
    let simulation;
    let nodes = [];
    let ws, sessionWs;
    
    // D3 setup
    const svg = d3.select('#canvas');
    const width = window.innerWidth;
    const height = window.innerHeight;
    const centerX = width / 2;
    const centerY = height / 2;
    
    svg.attr('width', width).attr('height', height);
    
    // Gradient definitions
    const defs = svg.append('defs');
    
    // Glow filter
    const filter = defs.append('filter').attr('id', 'glow');
    filter.append('feGaussianBlur').attr('stdDeviation', '3').attr('result', 'coloredBlur');
    const feMerge = filter.append('feMerge');
    feMerge.append('feMergeNode').attr('in', 'coloredBlur');
    feMerge.append('feMergeNode').attr('in', 'SourceGraphic');
    
    // Background grid
    const gridSize = 40;
    const gridGroup = svg.append('g').attr('class', 'grid');
    
    for (let x = 0; x < width; x += gridSize) {
      gridGroup.append('line')
        .attr('x1', x).attr('y1', 0)
        .attr('x2', x).attr('y2', height)
        .attr('stroke', 'rgba(255,255,255,0.03)');
    }
    for (let y = 0; y < height; y += gridSize) {
      gridGroup.append('line')
        .attr('x1', 0).attr('y1', y)
        .attr('x2', width).attr('y2', y)
        .attr('stroke', 'rgba(255,255,255,0.03)');
    }
    
    // Orbital rings around center
    const orbits = svg.append('g').attr('class', 'orbits');
    [100, 180, 280].forEach((r, i) => {
      orbits.append('circle')
        .attr('cx', centerX)
        .attr('cy', centerY)
        .attr('r', r)
        .attr('fill', 'none')
        .attr('stroke', 'rgba(255,255,255,0.05)')
        .attr('stroke-dasharray', '4,8');
    });
    
    // Connection lines group
    const linksGroup = svg.append('g').attr('class', 'links');
    
    // Nodes group
    const nodesGroup = svg.append('g').attr('class', 'nodes');
    
    // Category colors
    const categoryColors = {
      'emacs': '#b06bff',
      'node': '#6bff9f',
      'vscode': '#6b9fff',
      'server': '#ff9f6b',
      'system': '#6bffff',
      'database': '#ffeb6b',
      'default': '#888'
    };
    
    // Force simulation
    simulation = d3.forceSimulation()
      .force('center', d3.forceCenter(centerX, centerY).strength(0.02))
      .force('charge', d3.forceManyBody().strength(-200))
      .force('collision', d3.forceCollide().radius(d => d.radius + 10))
      .force('radial', d3.forceRadial(d => d.orbit, centerX, centerY).strength(0.3))
      .on('tick', ticked);
    
    function ticked() {
      // Update links
      linksGroup.selectAll('line')
        .attr('x1', centerX)
        .attr('y1', centerY)
        .attr('x2', d => d.x)
        .attr('y2', d => d.y);
      
      // Update nodes
      nodesGroup.selectAll('.node')
        .attr('transform', d => \`translate(\${d.x}, \${d.y})\`);
    }
    
    function updateVisualization(processData) {
      if (!processData || !processData.interesting) return;
      
      // Deduplicate by name
      const byName = new Map();
      processData.interesting.forEach(p => {
        const existing = byName.get(p.name);
        if (!existing || p.cpu > existing.cpu) {
          byName.set(p.name, p);
        }
      });
      
      const procs = Array.from(byName.values());
      
      // Map processes to nodes
      const newNodes = procs.map((p, i) => {
        const existing = nodes.find(n => n.name === p.name);
        const intensity = (p.cpu + p.mem) / 2;
        const radius = Math.max(15, Math.min(50, 10 + intensity * 2));
        
        // Assign orbit based on category
        let orbit = 180;
        if (p.category === 'emacs') orbit = 100;
        else if (p.category === 'vscode') orbit = 140;
        else if (p.category === 'server') orbit = 220;
        else if (p.category === 'system') orbit = 280;
        
        return {
          id: p.name,
          name: p.name,
          icon: p.icon,
          category: p.category,
          cpu: p.cpu,
          mem: p.mem,
          rss: p.rss,
          pid: p.pid,
          radius,
          orbit,
          color: categoryColors[p.category] || categoryColors.default,
          x: existing ? existing.x : centerX + (Math.random() - 0.5) * 200,
          y: existing ? existing.y : centerY + (Math.random() - 0.5) * 200,
        };
      });
      
      nodes = newNodes;
      simulation.nodes(nodes);
      simulation.alpha(0.3).restart();
      
      // Update links (lines from center to each node)
      const links = linksGroup.selectAll('line').data(nodes, d => d.id);
      
      links.enter()
        .append('line')
        .attr('stroke', d => d.color)
        .attr('stroke-opacity', 0.15)
        .attr('stroke-width', 1);
      
      links.exit().remove();
      
      linksGroup.selectAll('line')
        .attr('stroke', d => d.color)
        .attr('stroke-opacity', d => 0.1 + d.cpu / 100);
      
      // Update nodes
      const nodeSelection = nodesGroup.selectAll('.node').data(nodes, d => d.id);
      
      // Enter new nodes
      const nodeEnter = nodeSelection.enter()
        .append('g')
        .attr('class', 'node')
        .style('cursor', 'pointer')
        .on('mouseover', showTooltip)
        .on('mouseout', hideTooltip);
      
      // Outer glow circle
      nodeEnter.append('circle')
        .attr('class', 'glow')
        .attr('r', d => d.radius + 5)
        .attr('fill', d => d.color)
        .attr('opacity', 0.2)
        .attr('filter', 'url(#glow)');
      
      // Main circle
      nodeEnter.append('circle')
        .attr('class', 'main')
        .attr('r', d => d.radius)
        .attr('fill', 'rgba(10,10,10,0.8)')
        .attr('stroke', d => d.color)
        .attr('stroke-width', 2);
      
      // Icon
      nodeEnter.append('text')
        .attr('class', 'icon')
        .attr('text-anchor', 'middle')
        .attr('dominant-baseline', 'central')
        .attr('font-size', d => Math.max(12, d.radius * 0.6))
        .text(d => d.icon);
      
      // Name label
      nodeEnter.append('text')
        .attr('class', 'label')
        .attr('y', d => d.radius + 14)
        .attr('text-anchor', 'middle')
        .attr('fill', 'rgba(255,255,255,0.7)')
        .attr('font-size', '9px')
        .text(d => d.name.length > 12 ? d.name.slice(0, 10) + '…' : d.name);
      
      // Update existing nodes
      nodeSelection.select('.glow')
        .transition().duration(500)
        .attr('r', d => d.radius + 5 + d.cpu / 10)
        .attr('opacity', d => 0.15 + d.cpu / 200);
      
      nodeSelection.select('.main')
        .transition().duration(500)
        .attr('r', d => d.radius)
        .attr('stroke', d => d.color)
        .attr('stroke-width', d => 2 + d.cpu / 20);
      
      nodeSelection.select('.icon')
        .attr('font-size', d => Math.max(12, d.radius * 0.6));
      
      nodeSelection.select('.label')
        .attr('y', d => d.radius + 14);
      
      // Remove old nodes
      nodeSelection.exit().remove();
    }
    
    function showTooltip(event, d) {
      const tooltip = document.getElementById('tooltip');
      tooltip.innerHTML = \`
        <h3>\${d.icon} \${d.name}</h3>
        <div class="tooltip-stat"><span>PID</span><span class="val">\${d.pid}</span></div>
        <div class="tooltip-stat"><span>Category</span><span class="val">\${d.category}</span></div>
        <div class="tooltip-stat"><span>CPU</span><span class="val" style="color: var(--yellow)">\${d.cpu.toFixed(1)}%</span></div>
        <div class="tooltip-stat"><span>Memory</span><span class="val" style="color: var(--purple)">\${(d.rss / 1024).toFixed(0)} MB</span></div>
      \`;
      tooltip.style.left = (event.pageX + 15) + 'px';
      tooltip.style.top = (event.pageY - 10) + 'px';
      tooltip.classList.add('visible');
    }
    
    function hideTooltip() {
      document.getElementById('tooltip').classList.remove('visible');
    }
    
    // WebSocket connections
    function connectStatus() {
      const wsUrl = 'ws://' + window.location.host + '/ws';
      ws = new WebSocket(wsUrl);
      
      ws.onopen = () => {
        document.getElementById('status-dot').classList.remove('offline');
      };
      
      ws.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          updateHUD(data);
          updateVisualization(data.processes);
        } catch (err) {
          console.error('Parse error:', err);
        }
      };
      
      ws.onclose = () => {
        document.getElementById('status-dot').classList.add('offline');
        setTimeout(connectStatus, 2000);
      };
      
      ws.onerror = () => ws.close();
    }
    
    function connectSession() {
      try {
        sessionWs = new WebSocket('wss://localhost:8889');
        sessionWs.onmessage = (e) => {
          try {
            const msg = JSON.parse(e.data);
            if (msg.type === 'reload' && (msg.content?.piece === '*refresh*' || msg.content?.piece?.includes('devcontainer'))) {
              location.reload();
            }
          } catch {}
        };
        sessionWs.onclose = () => setTimeout(connectSession, 3000);
        sessionWs.onerror = () => sessionWs.close();
      } catch {}
    }
    
    function updateHUD(data) {
      document.getElementById('timestamp').textContent = new Date(data.timestamp).toLocaleTimeString();
      
      if (data.system) {
        document.getElementById('hostname').textContent = data.system.hostname;
        document.getElementById('uptime').textContent = data.system.uptime.formatted;
        document.getElementById('cpus').textContent = data.system.cpus;
        
        const mem = data.system.memory;
        document.getElementById('mem-text').textContent = mem.used + ' / ' + mem.total + ' MB';
        const memBar = document.getElementById('mem-bar');
        memBar.style.width = mem.percent + '%';
        memBar.classList.remove('warning', 'danger');
        if (mem.percent > 80) memBar.classList.add('danger');
        else if (mem.percent > 60) memBar.classList.add('warning');
      }
      
      if (data.emacs) {
        document.getElementById('emacs-state').textContent = data.emacs.online ? 'Online' : 'Offline';
        document.getElementById('emacs-state').style.color = data.emacs.online ? 'var(--green)' : 'var(--accent)';
      }
    }
    
    // Handle resize
    window.addEventListener('resize', () => {
      const w = window.innerWidth;
      const h = window.innerHeight;
      svg.attr('width', w).attr('height', h);
      simulation.force('center', d3.forceCenter(w / 2, h / 2));
      simulation.alpha(0.3).restart();
    });
    
    // Start
    connectStatus();
    connectSession();
  </script>
</body>
</html>`;
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
  process.exit(1);
});
