// 3D Process Tree Visualization
// Shared between VS Code extension and local dev testing

(function() {
  'use strict';
  
  // Check if we're in VS Code webview or standalone
  const isVSCode = typeof acquireVsCodeApi === 'function';
  
  // 🎨 Color Schemes (imported from color-schemes.js or embedded)
  const colorSchemes = window.AestheticColorSchemes?.schemes || {
    dark: {
      background: '#181318',
      backgroundAlt: '#141214',
      foreground: '#ffffffcc',
      foregroundBright: '#ffffff',
      foregroundMuted: '#555555',
      accent: '#a87090',
      accentBright: '#ff69b4',
      statusOnline: '#0f0',
      categories: {
        editor: 0xb06bff, tui: 0xff69b4, bridge: 0x6bff9f,
        db: 0xffeb6b, proxy: 0x6b9fff, ai: 0xff9f6b,
        shell: 0x6bffff, dev: 0x6bff9f, ide: 0x6b9fff, lsp: 0x888888, kernel: 0x88ccff
      },
      three: {
        sceneBackground: 0x000000,
        kernelOuter: 0x4488ff, kernelRing: 0x66aaff, kernelCore: 0x88ccff,
        connectionLine: 0x444444, connectionActive: 0xff69b4, deadProcess: 0x444444
      },
      ui: { shadow: 'rgba(0, 0, 0, 0.6)', overlay: 'rgba(0, 0, 0, 0.85)' }
    },
    light: {
      background: '#fcf7c5',
      backgroundAlt: '#f5f0c0',
      foreground: '#281e5a',
      foregroundBright: '#281e5a',
      foregroundMuted: '#806060',
      accent: '#387adf',
      accentBright: '#006400',
      statusOnline: '#006400',
      categories: {
        editor: 0x8040d0, tui: 0xd04080, bridge: 0x208040,
        db: 0xa08000, proxy: 0x2060c0, ai: 0xc06020,
        shell: 0x008080, dev: 0x208040, ide: 0x2060c0, lsp: 0x606060, kernel: 0x387adf
      },
      three: {
        sceneBackground: 0xfcf7c5,
        kernelOuter: 0x387adf, kernelRing: 0x006400, kernelCore: 0x387adf,
        connectionLine: 0xa8a080, connectionActive: 0x006400, deadProcess: 0xa8a080
      },
      ui: { shadow: 'rgba(0, 0, 0, 0.2)', overlay: 'rgba(252, 247, 197, 0.95)' }
    }
  };
  
  // Detect theme from data attribute, URL param, or VS Code CSS vars
  function detectTheme() {
    // Check data attribute first (set by the HTML)
    const dataTheme = document.body.dataset.theme;
    if (dataTheme === 'light' || dataTheme === 'dark') return dataTheme;
    
    // Check URL param
    const urlParams = new URLSearchParams(window.location.search);
    const urlTheme = urlParams.get('theme');
    if (urlTheme === 'light' || urlTheme === 'dark') return urlTheme;
    
    // Check VS Code CSS variables
    if (typeof getComputedStyle !== 'undefined') {
      const bgColor = getComputedStyle(document.body).getPropertyValue('--vscode-editor-background').trim();
      if (bgColor && bgColor.startsWith('#')) {
        const r = parseInt(bgColor.slice(1, 3), 16);
        const g = parseInt(bgColor.slice(3, 5), 16);
        const b = parseInt(bgColor.slice(5, 7), 16);
        const luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
        return luminance > 0.5 ? 'light' : 'dark';
      }
    }
    
    return 'dark';
  }
  
  let currentTheme = detectTheme();
  let scheme = colorSchemes[currentTheme];
  let colors = scheme.categories;
  
  // Apply initial body styling based on detected theme
  document.body.style.background = scheme.background;
  document.body.style.color = scheme.foreground;
  document.body.dataset.theme = currentTheme;
  
  // Show dev badge if not in VS Code
  if (!isVSCode) {
    const badge = document.createElement('div');
    badge.className = 'dev-badge';
    badge.textContent = 'DEV MODE';
    document.body.appendChild(badge);
  }
  
  let width = window.innerWidth, height = window.innerHeight;
  let meshes = new Map(), connections = new Map(), ws;
  let graveyard = [];
  const MAX_GRAVEYARD = 30;
  const GRAVEYARD_Y = -200;
  
  // Three.js setup
  const scene = new THREE.Scene();
  scene.background = new THREE.Color(scheme.three.sceneBackground);
  
  const camera = new THREE.PerspectiveCamera(50, width / height, 0.1, 5000);
  camera.position.set(0, 150, 400);
  camera.lookAt(0, 0, 0);
  
  const renderer = new THREE.WebGLRenderer({ canvas: document.getElementById('canvas'), antialias: true });
  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio);
  renderer.setClearColor(scheme.three.sceneBackground);
  
  const controls = new THREE.OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.05;
  controls.minDistance = 20;
  controls.maxDistance = 3000;
  controls.enablePan = true;
  controls.autoRotate = true;
  controls.autoRotateSpeed = 0.3;
  controls.target.set(0, 0, 0);
  
  let focusedPid = null;
  let focusTarget = new THREE.Vector3(0, 0, 0);
  let focusDistance = null;
  let transitioning = false;
  
  // Tour mode state
  let tourMode = false;
  let tourIndex = 0;
  let tourProcessList = [];
  let tourAutoPlay = false;
  let tourAutoPlayInterval = null;
  const TOUR_SPEED = 2500; // ms between auto-advances
  
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
        focusedPid = null;
        focusTarget.set(0, 0, 0);
        focusDistance = null;
      } else {
        focusedPid = String(pid);
        focusTarget.copy(clicked.position);
        focusDistance = 80 + (clicked.userData.size || 6) * 3;
      }
      transitioning = true;
      controls.autoRotate = true;
    } else if (!e.shiftKey) {
      focusedPid = null;
      focusTarget.set(0, 0, 0);
      focusDistance = null;
      transitioning = true;
    }
  });
  
  renderer.domElement.addEventListener('dblclick', () => {
    focusedPid = null;
    focusTarget.set(0, 0, 0);
    focusDistance = null;
    transitioning = true;
    camera.position.set(0, 150, 400);
  });
  
  // Tour Mode Functions
  function updateTourUI() {
    let tourUI = document.getElementById('tour-ui');
    if (!tourUI) {
      tourUI = document.createElement('div');
      tourUI.id = 'tour-ui';
      document.body.appendChild(tourUI);
    }
    // Update styling each time based on current theme
    tourUI.style.cssText = `position:fixed;bottom:20px;left:50%;transform:translateX(-50%);background:${scheme.ui.overlay};padding:12px 20px;border-radius:8px;color:${scheme.foregroundBright};font-family:monospace;font-size:12px;z-index:1000;display:none;text-align:center;border:1px solid ${scheme.foregroundMuted}40;`;
    
    if (tourMode && tourProcessList.length > 0) {
      const current = tourProcessList[tourIndex];
      const mesh = meshes.get(current);
      const name = mesh?.userData?.name || current;
      const icon = mesh?.userData?.icon || '●';
      const category = mesh?.userData?.category || '';
      
      tourUI.style.display = 'block';
      tourUI.innerHTML = `
        <div style="margin-bottom:8px;font-size:14px;color:${scheme.accent};">🎬 TOUR MODE</div>
        <div style="font-size:18px;margin-bottom:4px;color:${scheme.foregroundBright};">${icon} ${name}</div>
        <div style="color:${scheme.foregroundMuted};margin-bottom:12px;">${category} • ${tourIndex + 1}/${tourProcessList.length}</div>
        <div style="display:flex;gap:8px;justify-content:center;flex-wrap:wrap;">
          <button onclick="ProcessTreeViz.tourPrev()" style="padding:8px 16px;border-radius:4px;border:1px solid ${scheme.foregroundMuted}40;background:${currentTheme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.1)'};color:${scheme.foregroundBright};cursor:pointer;font-family:monospace;">← Prev</button>
          <button onclick="ProcessTreeViz.toggleAutoPlay()" style="padding:8px 16px;border-radius:4px;border:1px solid ${scheme.foregroundMuted}40;background:${currentTheme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.1)'};color:${scheme.foregroundBright};cursor:pointer;font-family:monospace;">${tourAutoPlay ? '⏸ Stop' : '▶ Auto'}</button>
          <button onclick="ProcessTreeViz.tourNext()" style="padding:8px 16px;border-radius:4px;border:1px solid ${scheme.foregroundMuted}40;background:${currentTheme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.1)'};color:${scheme.foregroundBright};cursor:pointer;font-family:monospace;">Next →</button>
          <button onclick="ProcessTreeViz.exitTour()" style="padding:8px 16px;border-radius:4px;border:1px solid ${scheme.foregroundMuted}40;background:${currentTheme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.1)'};color:${scheme.foregroundBright};cursor:pointer;font-family:monospace;">✕ Exit</button>
        </div>
        ${tourAutoPlay ? `<div style="color:${scheme.accentBright};margin-top:8px;">▶ Auto-playing...</div>` : ''}
      `;
      // Hide the tour button when in tour mode
      const btn = document.getElementById('tour-btn');
      if (btn) btn.style.display = 'none';
    } else {
      tourUI.style.display = 'none';
      // Show the tour button when not in tour mode
      const btn = document.getElementById('tour-btn');
      if (btn) btn.style.display = 'block';
    }
  }
  
  function buildTourList() {
    // Build ordered list: kernel first, then by category, then by tree depth
    const categoryOrder = ['kernel', 'ide', 'editor', 'tui', 'dev', 'db', 'shell', 'ai', 'lsp', 'proxy', 'bridge'];
    const list = Array.from(meshes.keys());
    
    list.sort((a, b) => {
      const meshA = meshes.get(a);
      const meshB = meshes.get(b);
      const catA = meshA?.userData?.category || 'zzz';
      const catB = meshB?.userData?.category || 'zzz';
      const orderA = categoryOrder.indexOf(catA);
      const orderB = categoryOrder.indexOf(catB);
      return (orderA === -1 ? 99 : orderA) - (orderB === -1 ? 99 : orderB);
    });
    
    return list;
  }
  
  function focusOnProcess(pid) {
    const mesh = meshes.get(pid);
    if (!mesh) return;
    
    focusedPid = pid;
    focusTarget.copy(mesh.position);
    focusDistance = 80 + (mesh.userData.size || 6) * 3;
    transitioning = true;
    controls.autoRotate = true;
  }
  
  function startTour() {
    tourMode = true;
    tourProcessList = buildTourList();
    tourIndex = 0;
    if (tourProcessList.length > 0) {
      focusOnProcess(tourProcessList[0]);
    }
    updateTourUI();
  }
  
  function exitTour() {
    tourMode = false;
    tourAutoPlay = false;
    if (tourAutoPlayInterval) {
      clearInterval(tourAutoPlayInterval);
      tourAutoPlayInterval = null;
    }
    focusedPid = null;
    focusTarget.set(0, 0, 0);
    focusDistance = null;
    transitioning = true;
    updateTourUI();
  }
  
  function tourNext() {
    if (!tourMode || tourProcessList.length === 0) return;
    tourIndex = (tourIndex + 1) % tourProcessList.length;
    focusOnProcess(tourProcessList[tourIndex]);
    updateTourUI();
  }
  
  function tourPrev() {
    if (!tourMode || tourProcessList.length === 0) return;
    tourIndex = (tourIndex - 1 + tourProcessList.length) % tourProcessList.length;
    focusOnProcess(tourProcessList[tourIndex]);
    updateTourUI();
  }
  
  function toggleAutoPlay() {
    tourAutoPlay = !tourAutoPlay;
    if (tourAutoPlay) {
      tourAutoPlayInterval = setInterval(tourNext, TOUR_SPEED);
    } else {
      if (tourAutoPlayInterval) {
        clearInterval(tourAutoPlayInterval);
        tourAutoPlayInterval = null;
      }
    }
    updateTourUI();
  }
  
  // Keyboard controls
  document.addEventListener('keydown', (e) => {
    // T to start tour
    if (e.key === 't' || e.key === 'T') {
      if (!tourMode) {
        startTour();
      }
      return;
    }
    
    if (tourMode) {
      switch(e.key) {
        case 'ArrowRight':
        case 'l':
        case 'L':
          tourNext();
          e.preventDefault();
          break;
        case 'ArrowLeft':
        case 'h':
        case 'H':
          tourPrev();
          e.preventDefault();
          break;
        case ' ':
          toggleAutoPlay();
          e.preventDefault();
          break;
        case 'Escape':
        case 'q':
        case 'Q':
          exitTour();
          e.preventDefault();
          break;
      }
    }
  });
  
  let processTree = { roots: [], byPid: new Map() };
  
  let kernelMesh = null, kernelGlow = null, kernelCore = null;
  function createKernelNode() {
    const group = new THREE.Group();
    
    const outerGeo = new THREE.SphereGeometry(35, 32, 32);
    const outerMat = new THREE.MeshBasicMaterial({
      color: scheme.three.kernelOuter, transparent: true, opacity: 0.15, wireframe: true
    });
    group.add(new THREE.Mesh(outerGeo, outerMat));
    
    const ringGeo = new THREE.TorusGeometry(25, 1.5, 8, 48);
    const ringMat = new THREE.MeshBasicMaterial({
      color: scheme.three.kernelRing, transparent: true, opacity: 0.4
    });
    const ring = new THREE.Mesh(ringGeo, ringMat);
    ring.rotation.x = Math.PI / 2;
    group.add(ring);
    kernelGlow = ring;
    
    const coreGeo = new THREE.SphereGeometry(12, 24, 24);
    const coreMat = new THREE.MeshBasicMaterial({
      color: scheme.three.kernelCore, transparent: true, opacity: 0.7
    });
    const core = new THREE.Mesh(coreGeo, coreMat);
    group.add(core);
    kernelCore = core;
    
    group.userData = {
      pid: 'kernel', name: 'Fedora Linux', icon: '🐧', category: 'kernel',
      cpu: 0, rss: 0, size: 35, targetPos: new THREE.Vector3(0, 0, 0), pulsePhase: 0
    };
    return group;
  }
  
  kernelMesh = createKernelNode();
  scene.add(kernelMesh);
  meshes.set('kernel', kernelMesh);
  
  function createNodeMesh(node) {
    const cpu = node.cpu || 0;
    const memMB = (node.rss || 10000) / 1024;
    const baseColor = colors[node.category] || 0x666666;
    const size = Math.max(4, Math.min(12, 3 + memMB * 0.05 + cpu * 0.1));
    
    const geo = new THREE.SphereGeometry(size, 12, 12);
    const mat = new THREE.MeshBasicMaterial({
      color: baseColor, transparent: true, opacity: 0.7 + cpu * 0.003
    });
    
    const mesh = new THREE.Mesh(geo, mat);
    mesh.userData = { 
      ...node, size, baseColor, targetPos: new THREE.Vector3(),
      pulsePhase: Math.random() * Math.PI * 2
    };
    return mesh;
  }
  
  function createConnectionLine() {
    const geo = new THREE.CylinderGeometry(1.5, 1.5, 1, 8);
    const mat = new THREE.MeshBasicMaterial({
      color: scheme.three.connectionLine, transparent: true, opacity: 0.5
    });
    return new THREE.Mesh(geo, mat);
  }
  
  function updateConnectionMesh(conn, childPos, parentPos) {
    const mesh = conn.line;
    const mid = new THREE.Vector3().addVectors(childPos, parentPos).multiplyScalar(0.5);
    mesh.position.copy(mid);
    const dir = new THREE.Vector3().subVectors(parentPos, childPos);
    const length = dir.length();
    mesh.scale.set(1, length, 1);
    mesh.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), dir.normalize());
  }
  
  function layoutTree(processes) {
    const byPid = new Map();
    const children = new Map();
    
    processes.forEach(p => {
      byPid.set(String(p.pid), p);
      children.set(String(p.pid), []);
    });
    
    const roots = [];
    processes.forEach(p => {
      const parentPid = String(p.parentInteresting || 0);
      if (parentPid && byPid.has(parentPid)) {
        children.get(parentPid).push(p);
      } else {
        roots.push(p);
      }
    });
    
    const categoryOrder = ['ide', 'editor', 'tui', 'dev', 'db', 'shell', 'ai', 'lsp', 'proxy', 'bridge'];
    roots.sort((a, b) => {
      const ai = categoryOrder.indexOf(a.category);
      const bi = categoryOrder.indexOf(b.category);
      return (ai === -1 ? 99 : ai) - (bi === -1 ? 99 : bi);
    });
    
    const levelHeight = 50, baseRadius = 100;
    
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
      
      const x = parentX + Math.cos(angle) * radius;
      const z = parentZ + Math.sin(angle) * radius;
      
      node.targetX = x;
      node.targetY = -depth * levelHeight;
      node.targetZ = z;
      
      if (childCount > 0) {
        const arcSpread = Math.min(Math.PI * 0.9, Math.PI * 0.3 * childCount);
        const startAngle = angle - arcSpread / 2;
        const childRadius = 35 + childCount * 10;
        
        nodeChildren.forEach((child, i) => {
          const childAngle = childCount === 1 ? angle : startAngle + (arcSpread / (childCount - 1)) * i;
          positionNode(child, depth + 1, childAngle, childRadius, x, z);
        });
      }
    }
    
    const totalRoots = roots.length;
    if (totalRoots > 0) {
      const weights = roots.map(r => 1 + countDescendants(String(r.pid)) * 0.5);
      const totalWeight = weights.reduce((a, b) => a + b, 0);
      
      let currentAngle = -Math.PI / 2;
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
      const labelPos = pos.clone();
      labelPos.y += (mesh.userData.size || 8) + 5;
      labelPos.project(camera);
      
      const x = (labelPos.x * 0.5 + 0.5) * width;
      const y = (-labelPos.y * 0.5 + 0.5) * height;
      
      if (labelPos.z < 1 && x > -100 && x < width + 100 && y > -100 && y < height + 100) {
        const d = mesh.userData;
        const color = '#' + (colors[d.category] || 0x666666).toString(16).padStart(6, '0');
        const distToCamera = camera.position.distanceTo(pos);
        // Larger base scale, less reduction with distance
        const proximityScale = Math.max(0.7, Math.min(3, 200 / distToCamera));
        // Higher minimum opacity - always readable
        const opacity = focusedPid 
          ? (pid === focusedPid ? 1 : (d.parentInteresting === parseInt(focusedPid) ? 0.95 : 0.7))
          : Math.max(0.85, Math.min(1, 400 / distToCamera));
        
        const cpuPct = Math.min(100, d.cpu || 0);
        const memMB = ((d.rss || 0) / 1024).toFixed(0);
        
        // Extract short command for display (first 40 chars of cmdShort or cmd)
        const cmdDisplay = d.cmdShort || d.cmd || '';
        const cmdShort = cmdDisplay.length > 50 ? cmdDisplay.slice(0, 47) + '...' : cmdDisplay;
        
        const label = document.createElement('div');
        label.className = 'proc-label';
        label.style.left = x + 'px';
        label.style.top = y + 'px';
        label.style.opacity = opacity;
        label.style.transform = 'translate(-50%, -100%) scale(' + proximityScale + ')';
        // Show name, then command on second line, then stats
        label.innerHTML = '<div class="icon">' + (d.icon || '●') + '</div>' +
          '<div class="name" style="color:' + color + '">' + (d.name || pid) + '</div>' +
          (cmdShort ? '<div class="cmd" style="color:' + scheme.foregroundMuted + ';font-size:8px;max-width:150px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">' + cmdShort + '</div>' : '') +
          '<div class="info" style="color:' + scheme.foregroundMuted + ';">' + memMB + 'MB · ' + cpuPct.toFixed(0) + '%</div>';
        container.appendChild(label);
      }
    });
    
    // Add labels for graveyard (dead) processes
    graveyard.forEach((grave) => {
      const mesh = grave.mesh;
      if (!mesh) return;
      
      const pos = new THREE.Vector3();
      mesh.getWorldPosition(pos);
      const labelPos = pos.clone();
      labelPos.y += 8;
      labelPos.project(camera);
      
      const x = (labelPos.x * 0.5 + 0.5) * width;
      const y = (-labelPos.y * 0.5 + 0.5) * height;
      
      if (labelPos.z < 1 && x > -100 && x < width + 100 && y > -100 && y < height + 100) {
        const distToCamera = camera.position.distanceTo(pos);
        const proximityScale = Math.max(0.5, Math.min(2, 150 / distToCamera));
        const age = (Date.now() - grave.deathTime) / 1000;
        const opacity = Math.max(0.3, 0.7 - age * 0.01);
        
        const cmdShort = grave.cmd ? (grave.cmd.length > 30 ? grave.cmd.slice(0, 27) + '...' : grave.cmd) : '';
        const timeAgo = age < 60 ? Math.floor(age) + 's ago' : Math.floor(age / 60) + 'm ago';
        
        const label = document.createElement('div');
        label.className = 'proc-label graveyard';
        label.style.left = x + 'px';
        label.style.top = y + 'px';
        label.style.opacity = opacity;
        label.style.transform = 'translate(-50%, -100%) scale(' + proximityScale + ')';
        label.innerHTML = '<div class="icon">💀</div>' +
          '<div class="name" style="color:' + scheme.foregroundMuted + ';text-decoration:line-through;">' + (grave.name || grave.pid) + '</div>' +
          (cmdShort ? '<div class="cmd" style="color:' + scheme.foregroundMuted + ';font-size:7px;opacity:0.7;max-width:120px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">' + cmdShort + '</div>' : '') +
          '<div class="info" style="color:' + scheme.foregroundMuted + ';opacity:0.6;">' + timeAgo + '</div>';
        container.appendChild(label);
      }
    });
  }
  
  function updateViz(processData) {
    if (!processData?.interesting) return;
    
    const processes = processData.interesting;
    document.getElementById('process-count').textContent = processes.length;
    
    processTree = layoutTree(processes);
    const currentPids = new Set(processes.map(p => String(p.pid)));
    
    processes.forEach(p => {
      const pid = String(p.pid);
      
      if (!meshes.has(pid)) {
        const mesh = createNodeMesh(p);
        mesh.position.set(p.targetX || 0, p.targetY || 0, p.targetZ || 0);
        mesh.userData.targetPos.set(p.targetX || 0, p.targetY || 0, p.targetZ || 0);
        scene.add(mesh);
        meshes.set(pid, mesh);
      } else {
        const mesh = meshes.get(pid);
        const d = mesh.userData;
        d.cpu = p.cpu; d.mem = p.mem; d.rss = p.rss; d.name = p.name;
        d.targetPos.set(p.targetX || d.targetPos.x, p.targetY || d.targetPos.y, p.targetZ || d.targetPos.z);
        
        const memMB = (p.rss || 10000) / 1024;
        d.size = Math.max(4, Math.min(12, 3 + memMB * 0.05 + p.cpu * 0.1));
        mesh.scale.setScalar(d.size / 6);
        
        const baseColor = colors[p.category] || 0x666666;
        const brighten = Math.min(1.8, 1 + p.cpu * 0.02);
        const r = ((baseColor >> 16) & 255) * brighten;
        const g = ((baseColor >> 8) & 255) * brighten;
        const b = (baseColor & 255) * brighten;
        mesh.material.color.setRGB(Math.min(255, r) / 255, Math.min(255, g) / 255, Math.min(255, b) / 255);
        mesh.material.opacity = 0.7 + p.cpu * 0.003;
      }
      
      const parentPid = String(p.parentInteresting || 0);
      if (parentPid && meshes.has(parentPid)) {
        const connKey = pid + '->' + parentPid;
        if (!connections.has(connKey)) {
          const line = createConnectionLine();
          scene.add(line);
          connections.set(connKey, { line, childPid: pid, parentPid });
        }
      } else {
        const connKey = pid + '->kernel';
        if (!connections.has(connKey)) {
          const line = createConnectionLine();
          scene.add(line);
          connections.set(connKey, { line, childPid: pid, parentPid: 'kernel' });
        }
      }
    });
    
    meshes.forEach((mesh, pid) => {
      if (pid === 'kernel') return;
      if (!currentPids.has(pid) && !mesh.userData.isDead) {
        mesh.userData.isDead = true;
        mesh.userData.deathTime = Date.now();
        
        const graveyardIndex = graveyard.length;
        const col = graveyardIndex % 10;
        const row = Math.floor(graveyardIndex / 10);
        mesh.userData.targetPos.set((col - 4.5) * 25, GRAVEYARD_Y - row * 20, 0);
        
        mesh.material.opacity = 0.25;
        mesh.material.color.setHex(scheme.three.deadProcess);
        
        // Store full process info for graveyard labels
        const d = mesh.userData;
        graveyard.push({ 
          pid, 
          mesh, 
          name: d.name,
          icon: d.icon || '💀',
          cmd: d.cmdShort || d.cmd || '',
          category: d.category,
          deathTime: Date.now() 
        });
        meshes.delete(pid);
        
        while (graveyard.length > MAX_GRAVEYARD) {
          const oldest = graveyard.shift();
          scene.remove(oldest.mesh);
          if (oldest.mesh.geometry) oldest.mesh.geometry.dispose();
          if (oldest.mesh.material) oldest.mesh.material.dispose();
        }
      }
    });
    
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
    
    // Refresh tour list if in tour mode (processes may have changed)
    if (tourMode) {
      const oldPid = tourProcessList[tourIndex];
      tourProcessList = buildTourList();
      // Try to stay on the same process if it still exists
      const newIndex = tourProcessList.indexOf(oldPid);
      if (newIndex !== -1) {
        tourIndex = newIndex;
      } else if (tourIndex >= tourProcessList.length) {
        tourIndex = Math.max(0, tourProcessList.length - 1);
      }
      updateTourUI();
    }
  }
  
  let time = 0;
  function animate() {
    requestAnimationFrame(animate);
    time += 0.016;
    
    if (focusedPid && meshes.has(focusedPid)) {
      focusTarget.lerp(meshes.get(focusedPid).position, 0.08);
    }
    
    controls.target.lerp(focusTarget, transitioning ? 0.06 : 0.02);
    
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
    
    // Graveyard animation with null checks
    graveyard.forEach((grave, i) => {
      const mesh = grave.mesh;
      if (mesh && mesh.userData && mesh.material) {
        const d = mesh.userData;
        mesh.position.x += (d.targetPos.x - mesh.position.x) * 0.02;
        mesh.position.y += (d.targetPos.y - mesh.position.y) * 0.015;
        mesh.position.z += (d.targetPos.z - mesh.position.z) * 0.02;
        mesh.position.x += Math.sin(time * 0.3 + i) * 0.05;
        const age = (Date.now() - grave.deathTime) / 1000;
        mesh.material.opacity = Math.max(0.1, 0.3 - age * 0.005);
      }
    });
    
    // Active meshes animation with null checks
    meshes.forEach((mesh, pid) => {
      if (!mesh || !mesh.userData || !mesh.material) return;
      const d = mesh.userData;
      const cpu = d.cpu || 0;
      const isFocused = focusedPid === pid;
      const isRelated = focusedPid && (d.parentInteresting === parseInt(focusedPid) || String(d.parentInteresting) === focusedPid);
      
      mesh.position.x += (d.targetPos.x - mesh.position.x) * 0.03;
      mesh.position.y += (d.targetPos.y - mesh.position.y) * 0.03;
      mesh.position.z += (d.targetPos.z - mesh.position.z) * 0.03;
      
      const float = Math.sin(time * 0.5 + d.pulsePhase) * 2;
      mesh.position.y += float * 0.02;
      
      const pulseAmp = isFocused ? 0.2 : (0.1 + cpu * 0.005);
      const pulse = 1 + Math.sin(time * (1 + cpu * 0.05) + d.pulsePhase) * pulseAmp;
      const sizeMultiplier = isFocused ? 1.5 : (isRelated ? 1.2 : 1);
      mesh.scale.setScalar((d.size / 6) * pulse * sizeMultiplier);
      
      if (focusedPid) {
        mesh.material.opacity = isFocused ? 1 : (isRelated ? 0.8 : 0.3);
      } else {
        mesh.material.opacity = 0.7 + cpu * 0.003;
      }
    });
    
    connections.forEach(conn => {
      const childMesh = meshes.get(conn.childPid);
      const parentMesh = meshes.get(conn.parentPid);
      if (childMesh && parentMesh) {
        updateConnectionMesh(conn, childMesh.position, parentMesh.position);
        const involvesFocus = focusedPid && (conn.childPid === focusedPid || conn.parentPid === focusedPid);
        conn.line.material.opacity = focusedPid ? (involvesFocus ? 0.9 : 0.15) : 0.5;
        conn.line.material.color.setHex(involvesFocus ? scheme.three.connectionActive : scheme.three.connectionLine);
        const thickness = involvesFocus ? 2.5 : 1.5;
        conn.line.scale.x = thickness / 1.5;
        conn.line.scale.z = thickness / 1.5;
      }
    });
    
    // 🌳 AST Tree Animation (if loaded)
    if (window.ASTTreeViz?.animateAST) {
      window.ASTTreeViz.animateAST();
    }
    
    renderer.render(scene, camera);
    updateLabels();
  }
  
  function connectWS() {
    try {
      ws = new WebSocket('ws://127.0.0.1:7890/ws');
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
    } catch {
      setTimeout(connectWS, 2000);
    }
  }
  
  window.addEventListener('resize', () => {
    width = window.innerWidth;
    height = window.innerHeight;
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
    renderer.setSize(width, height);
  });
  
  // 🎨 Theme switching function
  function setTheme(themeName) {
    if (themeName !== 'light' && themeName !== 'dark') return;
    currentTheme = themeName;
    scheme = colorSchemes[currentTheme];
    colors = scheme.categories;
    
    // Update scene background
    scene.background.setHex(scheme.three.sceneBackground);
    renderer.setClearColor(scheme.three.sceneBackground);
    
    // Update body styling
    document.body.dataset.theme = themeName;
    document.body.style.background = scheme.background;
    document.body.style.color = scheme.foreground;
    
    // Update kernel mesh colors
    if (kernelMesh) {
      kernelMesh.children[0].material.color.setHex(scheme.three.kernelOuter);
      if (kernelGlow) kernelGlow.material.color.setHex(scheme.three.kernelRing);
      if (kernelCore) kernelCore.material.color.setHex(scheme.three.kernelCore);
    }
    
    // Update all process node colors
    meshes.forEach((mesh, pid) => {
      if (pid === 'kernel') return;
      const category = mesh.userData.category;
      const newColor = colors[category] || 0x666666;
      mesh.material.color.setHex(newColor);
      mesh.userData.baseColor = newColor;
    });
    
    // Update connections
    connections.forEach(conn => {
      conn.line.material.color.setHex(scheme.three.connectionLine);
    });
    
    // Update graveyard
    graveyard.forEach(grave => {
      if (grave.mesh && grave.mesh.material) {
        grave.mesh.material.color.setHex(scheme.three.deadProcess);
      }
    });
    
    // Update CSS styles
    updateThemeStyles();
  }
  
  function toggleTheme() {
    setTheme(currentTheme === 'dark' ? 'light' : 'dark');
    return currentTheme;
  }
  
  function updateThemeStyles() {
    // Update dynamic CSS based on theme
    let styleEl = document.getElementById('theme-dynamic-styles');
    if (!styleEl) {
      styleEl = document.createElement('style');
      styleEl.id = 'theme-dynamic-styles';
      document.head.appendChild(styleEl);
    }
    styleEl.textContent = `
      .title .dot { color: ${scheme.accentBright}; }
      .status-dot { background: ${scheme.accent}; }
      .status-dot.online { background: ${scheme.statusOnline}; }
      .stats { color: ${scheme.foregroundMuted}; }
      .stats .val { color: ${scheme.foregroundBright}; }
      .mem { color: ${scheme.foregroundMuted}; }
      .center { color: ${scheme.foregroundMuted}; }
      .center .count { color: ${scheme.foregroundBright}; }
      .proc-label { text-shadow: 0 0 3px ${scheme.background}, 0 0 6px ${scheme.background}; }
      .proc-label .info { color: ${scheme.foregroundMuted}; }
      .dev-badge { background: ${currentTheme === 'light' ? scheme.accentBright : scheme.accentBright}; color: ${currentTheme === 'light' ? '#fff' : '#000'}; }
      #tour-ui { background: ${scheme.ui.overlay}; border-color: ${scheme.foregroundMuted}; }
      #tour-hint { background: ${scheme.ui.overlay}; }
    `;
  }
  
  // Apply initial theme styles
  updateThemeStyles();
  
  // Expose for external use (mock data injection, etc.)
  window.ProcessTreeViz = {
    updateViz,
    scene,
    camera,
    meshes,
    connections,
    graveyard,
    // Tour mode
    startTour,
    exitTour,
    tourNext,
    tourPrev,
    toggleAutoPlay,
    isTourMode: () => tourMode,
    // Theme control
    setTheme,
    toggleTheme,
    getTheme: () => currentTheme,
    getScheme: () => scheme,
    colorSchemes
  };
  
  // Add tour button to the UI (touch-friendly, no keyboard shortcuts)
  const tourBtn = document.createElement('button');
  tourBtn.id = 'tour-btn';
  tourBtn.style.cssText = `position:fixed;bottom:20px;right:20px;background:${scheme.ui.overlay};padding:10px 16px;border-radius:6px;color:${scheme.foregroundMuted};font-family:monospace;font-size:12px;z-index:999;border:1px solid ${scheme.foregroundMuted}40;cursor:pointer;`;
  tourBtn.textContent = '🎬 Tour';
  tourBtn.onclick = () => { if (!tourMode) startTour(); else exitTour(); };
  document.body.appendChild(tourBtn);
  
  animate();
  connectWS();
})();