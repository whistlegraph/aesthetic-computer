// 2D Canvas Process Tree Visualization
// Dense, responsive, real-time process tree view

(function() {
  'use strict';

  const isVSCode = typeof acquireVsCodeApi === 'function';

  // Color schemes
  const colorSchemes = {
    dark: {
      bg: '#181318',
      fg: '#fff',
      fgMuted: '#666',
      accent: '#ff69b4',
      online: '#0f0',
      line: 'rgba(255, 255, 255, 0.15)',
      nodeStroke: 'rgba(255, 255, 255, 0.3)',
      categories: {
        editor: '#b05070',
        tui: '#ff5294',
        bridge: '#6c757f',
        db: '#ffb84b',
        proxy: '#6bbd9f',
        ai: '#ff95db',
        shell: '#6c77ff',
        dev: '#6c757f',
        ide: '#6bbd9f',
        lsp: '#889098',
        kernel: '#889fff'
      }
    },
    light: {
      bg: '#fcf7c5',
      fg: '#281e5a',
      fgMuted: '#806060',
      accent: '#006400',
      online: '#006400',
      line: 'rgba(0, 0, 0, 0.15)',
      nodeStroke: 'rgba(0, 0, 0, 0.3)',
      categories: {
        editor: '#804050',
        tui: '#d02070',
        bridge: '#202530',
        db: '#a08020',
        proxy: '#206050',
        ai: '#c05090',
        shell: '#004080',
        dev: '#202530',
        ide: '#206050',
        lsp: '#606068',
        kernel: '#387adf'
      }
    }
  };

  let currentTheme = document.body.dataset.theme || 'dark';
  let scheme = colorSchemes[currentTheme];

  // Canvas setup
  const canvas = document.getElementById('canvas');
  const ctx = canvas.getContext('2d');
  let width, height;
  let dpr = window.devicePixelRatio || 1;

  function resizeCanvas() {
    width = canvas.clientWidth;
    height = canvas.clientHeight;
    canvas.width = width * dpr;
    canvas.height = height * dpr;
    ctx.scale(dpr, dpr);
  }
  resizeCanvas();
  window.addEventListener('resize', resizeCanvas);

  // View transform (pan & zoom)
  let offsetX = 0, offsetY = 0, scale = 1;
  let isDragging = false, dragStartX = 0, dragStartY = 0;

  // Process tree data
  let processTree = [];
  let nodeMap = new Map(); // pid -> node info

  // Layout constants
  const NODE_RADIUS = 6;
  const NODE_SPACING_X = 180;
  const NODE_SPACING_Y = 30;
  const INDENT = 20;

  // Build tree structure from flat process list
  function buildTree(processes) {
    if (!processes || !processes.length) return [];

    const byPid = new Map();
    const children = new Map();

    processes.forEach(p => {
      byPid.set(String(p.pid), p);
      children.set(String(p.pid), []);
    });

    let roots = [];
    processes.forEach(p => {
      const ppid = String(p.ppid);
      if (ppid && byPid.has(ppid) && ppid !== String(p.pid)) {
        children.get(ppid).push(p);
      } else {
        roots.push(p);
      }
    });

    // Layout tree with coordinates
    nodeMap.clear();
    let yOffset = 20;

    function layoutNode(proc, depth, parentY) {
      const pid = String(proc.pid);
      const x = depth * INDENT;
      const y = yOffset;
      yOffset += NODE_SPACING_Y;

      const node = {
        pid,
        name: proc.name || proc.command || `PID ${pid}`,
        cpu: proc.cpu || 0,
        mem: proc.mem || 0,
        category: proc.category || 'shell',
        x,
        y,
        depth,
        parentY,
        children: []
      };

      nodeMap.set(pid, node);

      const kids = children.get(pid) || [];
      kids.forEach(child => {
        const childNode = layoutNode(child, depth + 1, y);
        if (childNode) node.children.push(childNode);
      });

      return node;
    }

    return roots.map(r => layoutNode(r, 0, null)).filter(Boolean);
  }

  // Draw the tree
  function draw() {
    ctx.clearRect(0, 0, width, height);

    ctx.save();
    ctx.translate(offsetX, offsetY);
    ctx.scale(scale, scale);

    // Draw connections first
    nodeMap.forEach(node => {
      if (node.parentY !== null) {
        ctx.strokeStyle = scheme.line;
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.moveTo(node.x, node.y);
        ctx.lineTo(node.x - INDENT, node.parentY);
        ctx.stroke();
      }
    });

    // Draw nodes
    nodeMap.forEach(node => {
      const color = scheme.categories[node.category] || scheme.categories.shell;

      // Node circle
      ctx.fillStyle = color;
      ctx.strokeStyle = scheme.nodeStroke;
      ctx.lineWidth = 1.5;
      ctx.beginPath();
      ctx.arc(node.x, node.y, NODE_RADIUS, 0, Math.PI * 2);
      ctx.fill();
      ctx.stroke();

      // Node label
      ctx.fillStyle = scheme.fg;
      ctx.font = '11px monospace';
      ctx.textAlign = 'left';
      ctx.textBaseline = 'middle';
      ctx.fillText(node.name, node.x + NODE_RADIUS + 6, node.y);

      // CPU/MEM info
      if (node.cpu > 0.1 || node.mem > 0) {
        ctx.fillStyle = scheme.fgMuted;
        ctx.font = '9px monospace';
        const info = `${node.cpu.toFixed(1)}% • ${node.mem.toFixed(0)}MB`;
        ctx.fillText(info, node.x + NODE_RADIUS + 6, node.y + 11);
      }
    });

    ctx.restore();
  }

  // Mouse interaction
  let hoveredNode = null;

  function screenToWorld(screenX, screenY) {
    return {
      x: (screenX - offsetX) / scale,
      y: (screenY - offsetY) / scale
    };
  }

  function findNodeAt(worldX, worldY) {
    for (const [pid, node] of nodeMap) {
      const dx = worldX - node.x;
      const dy = worldY - node.y;
      if (Math.sqrt(dx * dx + dy * dy) <= NODE_RADIUS + 2) {
        return node;
      }
    }
    return null;
  }

  canvas.addEventListener('mousedown', e => {
    isDragging = true;
    dragStartX = e.clientX - offsetX;
    dragStartY = e.clientY - offsetY;
    canvas.style.cursor = 'grabbing';
  });

  canvas.addEventListener('mousemove', e => {
    const rect = canvas.getBoundingClientRect();
    const mouseX = e.clientX - rect.left;
    const mouseY = e.clientY - rect.top;

    if (isDragging) {
      offsetX = e.clientX - dragStartX;
      offsetY = e.clientY - dragStartY;
      draw();
    } else {
      const world = screenToWorld(mouseX, mouseY);
      const node = findNodeAt(world.x, world.y);

      if (node !== hoveredNode) {
        hoveredNode = node;
        if (node) {
          showTooltip(e.clientX, e.clientY, node);
          canvas.style.cursor = 'pointer';
        } else {
          hideTooltip();
          canvas.style.cursor = 'default';
        }
      }
    }
  });

  canvas.addEventListener('mouseup', () => {
    isDragging = false;
    canvas.style.cursor = 'default';
  });

  canvas.addEventListener('mouseleave', () => {
    isDragging = false;
    hideTooltip();
    canvas.style.cursor = 'default';
  });

  canvas.addEventListener('wheel', e => {
    e.preventDefault();
    const delta = e.deltaY > 0 ? 0.9 : 1.1;
    const newScale = Math.max(0.1, Math.min(5, scale * delta));

    const rect = canvas.getBoundingClientRect();
    const mouseX = e.clientX - rect.left;
    const mouseY = e.clientY - rect.top;

    offsetX = mouseX - (mouseX - offsetX) * (newScale / scale);
    offsetY = mouseY - (mouseY - offsetY) * (newScale / scale);
    scale = newScale;

    draw();
  });

  // Tooltip
  const tooltip = document.getElementById('tooltip');

  function showTooltip(x, y, node) {
    tooltip.style.display = 'block';
    tooltip.style.left = (x + 10) + 'px';
    tooltip.style.top = (y + 10) + 'px';
    tooltip.textContent = `${node.name}\nPID: ${node.pid}\nCPU: ${node.cpu.toFixed(1)}%\nMem: ${node.mem.toFixed(0)} MB`;
  }

  function hideTooltip() {
    tooltip.style.display = 'none';
  }

  // Update from WebSocket data
  function updateViz(processData) {
    if (!processData?.interesting) return;

    const processes = processData.interesting;
    document.getElementById('process-count').textContent = `${processes.length} processes`;

    processTree = buildTree(processes);
    draw();
  }

  // WebSocket connection
  let ws;
  function connectWS() {
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const port = location.port || (protocol === 'wss:' ? '443' : '80');
    const wsUrl = `${protocol}//${location.hostname}:${port}/process-tree-ws`;

    try {
      ws = new WebSocket(wsUrl);

      ws.onopen = () => {
        console.log('Connected to process tree');
        document.getElementById('status-dot').classList.add('online');
      };

      ws.onclose = () => {
        document.getElementById('status-dot').classList.remove('online');
        setTimeout(connectWS, 2000);
      };

      ws.onmessage = (e) => {
        try {
          const data = JSON.parse(e.data);
          if (data.system) {
            document.getElementById('uptime').textContent = data.system.uptime?.formatted || '—';
            document.getElementById('cpu-info').textContent = `${data.system.cpus || 0} CPUs`;
            const m = data.system.memory;
            document.getElementById('mem-info').textContent = `${m?.used || 0} / ${m?.total || 0} MB`;
          }
          updateViz(data.processes);
        } catch (err) {
          console.error('Parse error:', err);
        }
      };
    } catch (err) {
      console.error('WebSocket error:', err);
      setTimeout(connectWS, 2000);
    }
  }

  // Theme toggle
  function toggleTheme() {
    currentTheme = currentTheme === 'dark' ? 'light' : 'dark';
    scheme = colorSchemes[currentTheme];
    document.body.dataset.theme = currentTheme;
    draw();
  }

  // Reset view
  function resetView() {
    offsetX = 0;
    offsetY = 0;
    scale = 1;
    draw();
  }

  // Export API
  window.ProcessTree2D = {
    toggleTheme,
    resetView,
    updateViz
  };

  // Start
  connectWS();

  // Animation loop for smooth updates
  function animate() {
    // Could add animation logic here
    requestAnimationFrame(animate);
  }
  animate();

})();
