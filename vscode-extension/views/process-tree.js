// 3D Process Tree Visualization
// Shared between VS Code extension and local dev testing

(function() {
  'use strict';
  
  // Check if we're in VS Code webview or standalone
  const isVSCode = typeof acquireVsCodeApi === 'function';
  
  // Show dev badge if not in VS Code
  if (!isVSCode) {
    const badge = document.createElement('div');
    badge.className = 'dev-badge';
    badge.textContent = 'DEV MODE';
    document.body.appendChild(badge);
  }
  
  const colors = {
    'editor': 0xb06bff, 'tui': 0xff69b4, 'bridge': 0x6bff9f,
    'db': 0xffeb6b, 'proxy': 0x6b9fff, 'ai': 0xff9f6b,
    'shell': 0x6bffff, 'dev': 0x6bff9f, 'ide': 0x6b9fff, 'lsp': 0x888888
  };
  
  let width = window.innerWidth, height = window.innerHeight;
  let meshes = new Map(), connections = new Map(), ws;
  let graveyard = [];
  const MAX_GRAVEYARD = 30;
  const GRAVEYARD_Y = -200;
  
  // Three.js setup
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(50, width / height, 0.1, 5000);
  camera.position.set(0, 150, 400);
  camera.lookAt(0, 0, 0);
  
  const renderer = new THREE.WebGLRenderer({ canvas: document.getElementById('canvas'), antialias: true });
  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio);
  
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
      tourUI.style.cssText = 'position:fixed;bottom:20px;left:50%;transform:translateX(-50%);background:rgba(0,0,0,0.85);padding:12px 20px;border-radius:8px;color:#fff;font-family:monospace;font-size:12px;z-index:1000;display:none;text-align:center;border:1px solid #444;';
      document.body.appendChild(tourUI);
    }
    
    if (tourMode && tourProcessList.length > 0) {
      const current = tourProcessList[tourIndex];
      const mesh = meshes.get(current);
      const name = mesh?.userData?.name || current;
      const icon = mesh?.userData?.icon || '●';
      const category = mesh?.userData?.category || '';
      
      tourUI.style.display = 'block';
      tourUI.innerHTML = `
        <div style="margin-bottom:8px;font-size:14px;color:#88ccff;">🎬 TOUR MODE</div>
        <div style="font-size:18px;margin-bottom:4px;">${icon} ${name}</div>
        <div style="color:#888;margin-bottom:8px;">${category} • ${tourIndex + 1}/${tourProcessList.length}</div>
        <div style="color:#666;font-size:10px;">
          ← → Navigate • Space ${tourAutoPlay ? 'Stop' : 'Auto'} • Esc Exit
        </div>
        ${tourAutoPlay ? '<div style="color:#6bff9f;margin-top:6px;">▶ Auto-playing...</div>' : ''}
      `;
      // Hide the hint when in tour mode
      const hint = document.getElementById('tour-hint');
      if (hint) hint.style.display = 'none';
    } else {
      tourUI.style.display = 'none';
      // Show the hint when not in tour mode
      const hint = document.getElementById('tour-hint');
      if (hint) hint.style.display = 'block';
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
      color: 0x4488ff, transparent: true, opacity: 0.15, wireframe: true
    });
    group.add(new THREE.Mesh(outerGeo, outerMat));
    
    const ringGeo = new THREE.TorusGeometry(25, 1.5, 8, 48);
    const ringMat = new THREE.MeshBasicMaterial({
      color: 0x66aaff, transparent: true, opacity: 0.4
    });
    const ring = new THREE.Mesh(ringGeo, ringMat);
    ring.rotation.x = Math.PI / 2;
    group.add(ring);
    kernelGlow = ring;
    
    const coreGeo = new THREE.SphereGeometry(12, 24, 24);
    const coreMat = new THREE.MeshBasicMaterial({
      color: 0x88ccff, transparent: true, opacity: 0.7
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
      color: 0x444444, transparent: true, opacity: 0.5
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
        const proximityScale = Math.max(0.4, Math.min(3, 150 / distToCamera));
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
        label.style.transform = 'translate(-50%, -100%) scale(' + proximityScale + ')';
        label.innerHTML = '<div class="icon">' + (d.icon || '●') + '</div><div class="name" style="color:' + color + '">' + (d.name || pid) + '</div><div class="info">' + memMB + 'MB · ' + cpuPct.toFixed(0) + '%</div>';
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
        mesh.material.color.setHex(0x444444);
        
        graveyard.push({ pid, mesh, name: mesh.userData.name, deathTime: Date.now() });
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
        conn.line.material.color.setHex(involvesFocus ? 0xff69b4 : 0x444444);
        const thickness = involvesFocus ? 2.5 : 1.5;
        conn.line.scale.x = thickness / 1.5;
        conn.line.scale.z = thickness / 1.5;
      }
    });
    
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
    isTourMode: () => tourMode
  };
  
  // Add tour hint to the UI
  const tourHint = document.createElement('div');
  tourHint.id = 'tour-hint';
  tourHint.style.cssText = 'position:fixed;bottom:20px;right:20px;background:rgba(0,0,0,0.7);padding:8px 12px;border-radius:6px;color:#888;font-family:monospace;font-size:11px;z-index:999;';
  tourHint.innerHTML = 'Press <span style="color:#88ccff;font-weight:bold;">T</span> for Tour Mode';
  document.body.appendChild(tourHint);
  
  animate();
  connectWS();
})();
