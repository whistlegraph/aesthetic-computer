// 3D Source Code Visualization
// Renders JavaScript/TypeScript files as interactive 3D trees with click navigation

(function() {
  'use strict';
  
  // Wait for ProcessTreeViz if it's not ready yet
  if (!window.ProcessTreeViz) {
    console.log('⏳ Waiting for ProcessTreeViz...');
    const checkInterval = setInterval(() => {
      if (window.ProcessTreeViz) {
        clearInterval(checkInterval);
        initASTVisualization();
      }
    }, 100);
    return;
  } else {
    initASTVisualization();
  }

  function initASTVisualization() {
    console.log('🚀 Initializing Source Tree Visualization');
    
    const { scene, camera, renderer, colorSchemes } = window.ProcessTreeViz;
    // Update theme reference dynamically as it might change
    let scheme = window.ProcessTreeViz.getScheme();

    // ... rest of initialization ...
    // Tab system state
    let currentTab = 'processes'; // 'processes' or 'sources'
    let sourcesVisible = false;
  
    // Source visualization state
    const sourceFiles = new Map(); // fileName -> { rootMesh, nodes: Map<id, mesh>, connections: [] }
    const sourceConnections = new Map();
    let astFiles = [];
    let focusedSourceNode = null;
    let hoveredNode = null;
  
    // Raycaster for click detection
    const raycaster = new THREE.Raycaster();
    const mouse = new THREE.Vector2();
  
    // Icon mapping for node types
    const nodeIcons = {

    Program: '📄',
    FunctionDeclaration: '🔧',
    FunctionExpression: '🔧',
    ArrowFunctionExpression: '➡️',
    AsyncFunctionDeclaration: '⚡',
    AsyncArrowFunctionExpression: '⚡',
    ClassDeclaration: '🏛️',
    ClassExpression: '🏛️',
    MethodDefinition: '🔩',
    VariableDeclaration: '📦',
    VariableDeclarator: '📦',
    ImportDeclaration: '📥',
    ImportSpecifier: '📥',
    ExportNamedDeclaration: '📤',
    ExportDefaultDeclaration: '📤',
    CallExpression: '📞',
    NewExpression: '🆕',
    MemberExpression: '🔗',
    Identifier: '🏷️',
    Literal: '✨',
    StringLiteral: '💬',
    NumericLiteral: '🔢',
    ObjectExpression: '{}',
    ObjectPattern: '{}',
    ArrayExpression: '[]',
    ArrayPattern: '[]',
    IfStatement: '❓',
    ConditionalExpression: '❓',
    ForStatement: '🔄',
    ForOfStatement: '🔄',
    ForInStatement: '🔄',
    WhileStatement: '🔁',
    DoWhileStatement: '🔁',
    SwitchStatement: '🔀',
    TryStatement: '🛡️',
    CatchClause: '🎣',
    ThrowStatement: '💥',
    ReturnStatement: '↩️',
    AwaitExpression: '⏳',
    YieldExpression: '🌾',
    SpreadElement: '...',
    TemplateLiteral: '📝',
    BlockStatement: '📦',
    Property: '🔑',
    AssignmentExpression: '=',
    BinaryExpression: '➕',
    LogicalExpression: '🧮',
    UnaryExpression: '!',
    UpdateExpression: '++',
    SequenceExpression: ',',
    ExpressionStatement: '💭',
  };
  
  // Color mapping for node types (more vibrant)
  const nodeColors = {
    Program: 0x88ccff,
    FunctionDeclaration: 0xff6b9f,
    FunctionExpression: 0xff6b9f,
    ArrowFunctionExpression: 0xff8faf,
    AsyncFunctionDeclaration: 0xffaf6b,
    ClassDeclaration: 0xb06bff,
    ClassExpression: 0xb06bff,
    MethodDefinition: 0xd080ff,
    VariableDeclaration: 0x6bff9f,
    VariableDeclarator: 0x50d080,
    ImportDeclaration: 0xffeb6b,
    ImportSpecifier: 0xffd040,
    ExportNamedDeclaration: 0xffc040,
    ExportDefaultDeclaration: 0xffa030,
    CallExpression: 0x6bb4ff,
    NewExpression: 0x80c0ff,
    MemberExpression: 0x5090d0,
    Identifier: 0x9999aa,
    Literal: 0x77aa77,
    StringLiteral: 0x88cc88,
    NumericLiteral: 0x88aacc,
    ObjectExpression: 0x6bffff,
    ArrayExpression: 0x50d0d0,
    IfStatement: 0xff9f6b,
    ConditionalExpression: 0xffaf80,
    ForStatement: 0xe08050,
    ForOfStatement: 0xe09060,
    WhileStatement: 0xd07040,
    SwitchStatement: 0xc06030,
    TryStatement: 0x60c0a0,
    CatchClause: 0x50b090,
    ReturnStatement: 0x80ff80,
    AwaitExpression: 0xffd080,
    BlockStatement: 0x555566,
    ExpressionStatement: 0x444455,
    Property: 0x8899aa,
  };
  
  // Get display name for a node (actual code identifier, not generic type)
  function getNodeDisplayName(node) {
    // Priority: actual identifier names
    if (node.name && node.name !== node.type) return node.name;
    
    // For different node types, try to extract meaningful names
    switch (node.type) {
      case 'FunctionDeclaration':
      case 'FunctionExpression':
      case 'ArrowFunctionExpression':
        return node.name || 'λ';
      case 'ClassDeclaration':
      case 'ClassExpression':
        return node.name || 'Class';
      case 'MethodDefinition':
        return node.name || 'method';
      case 'VariableDeclaration':
        return node.kind || 'var'; // const, let, var
      case 'VariableDeclarator':
        return node.name || 'binding';
      case 'ImportDeclaration':
        return node.source || 'import';
      case 'ExportNamedDeclaration':
      case 'ExportDefaultDeclaration':
        return node.name || 'export';
      case 'CallExpression':
        return node.callee || 'call()';
      case 'MemberExpression':
        return node.property || 'member';
      case 'Identifier':
        return node.name || 'id';
      case 'Literal':
        const val = String(node.value || '');
        return val.length > 12 ? val.slice(0, 10) + '…' : val;
      case 'Property':
        return node.name || 'prop';
      case 'Program':
        return '📄 ' + (node.fileName || 'source');
      default:
        return node.name || node.type.replace(/Declaration|Expression|Statement/g, '');
    }
  }
  
  function getNodeIcon(type) {
    return nodeIcons[type] || '●';
  }
  
  function getNodeColor(type) {
    return nodeColors[type] || 0x666688;
  }
  
  function getNodeSize(node) {
    const span = (node.end || 0) - (node.start || 0);
    const baseSize = Math.max(4, Math.min(14, 3 + Math.log(span + 1) * 0.9));
    
    // Boost root and important nodes
    const important = ['Program', 'FunctionDeclaration', 'ClassDeclaration', 'MethodDefinition', 'ExportDefaultDeclaration'];
    if (important.includes(node.type)) return baseSize * 1.4;
    return baseSize;
  }
  
  function createSourceNodeMesh(node, fileInfo) {
    const size = getNodeSize(node);
    const color = getNodeColor(node.type);
    
    // Support legacy string arg or object
    const fileName = (typeof fileInfo === 'string') ? fileInfo : fileInfo.fileName;
    const filePath = (typeof fileInfo === 'object') ? fileInfo.filePath : undefined;
    const fileId = (typeof fileInfo === 'object') ? (fileInfo.id || fileName) : fileName;

    // Use icosahedron for functions/classes, sphere for others
    const isImportant = ['FunctionDeclaration', 'ClassDeclaration', 'MethodDefinition', 'Program'].includes(node.type);
    const geo = isImportant 
      ? new THREE.IcosahedronGeometry(size, 1)
      : new THREE.SphereGeometry(size, 12, 12);
    
    const mat = new THREE.MeshBasicMaterial({
      color: color,
      transparent: true,
      opacity: 0.85,
    });
    
    const mesh = new THREE.Mesh(geo, mat);
    mesh.userData = {
      ...node,
      fileName,
      filePath, // Store full path for navigation
      fileId,   // Store unique ID for management
      size,
      baseColor: color,
      displayName: getNodeDisplayName(node),
      icon: getNodeIcon(node.type),
      targetPos: new THREE.Vector3(),
      pulsePhase: Math.random() * Math.PI * 2,
      isSourceNode: true,
    };
    
    return mesh;
  }
  
  function createSourceConnection(thickness = 1.2) {
    const geo = new THREE.CylinderGeometry(thickness, thickness, 1, 8);
    const mat = new THREE.MeshBasicMaterial({
      color: scheme.three.connectionLine,
      transparent: true,
      opacity: 0.6,
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
  
  // Improved tree layout with more spacing
  function layoutSourceTree(root, fileIndex = 0, totalFiles = 1) {
    if (!root) return;
    
    const levelHeight = 45; // More vertical spacing
    const baseSpacing = 300;
    const fileSpread = totalFiles > 1 ? 360 / totalFiles : 0;
    const fileAngle = (fileIndex / totalFiles) * Math.PI * 2 - Math.PI / 2;
    const baseX = Math.cos(fileAngle) * baseSpacing;
    const baseZ = Math.sin(fileAngle) * baseSpacing;
    
    function countDescendants(node) {
      if (!node.children || node.children.length === 0) return 1;
      return node.children.reduce((sum, child) => sum + countDescendants(child), 0);
    }
    
    function positionNode(node, depth, angle, radius, parentX, parentZ) {
      const childCount = node.children?.length || 0;
      
      const x = parentX + Math.cos(angle) * radius;
      const z = parentZ + Math.sin(angle) * radius;
      const y = -depth * levelHeight + 50; // Start above center
      
      node.targetX = x;
      node.targetY = y;
      node.targetZ = z;
      
      if (childCount > 0) {
        // Calculate arc spread based on descendants for better spacing
        const totalDescendants = node.children.reduce((sum, child) => sum + countDescendants(child), 0);
        const arcSpread = Math.min(Math.PI * 1.5, Math.PI * 0.15 * totalDescendants);
        const startAngle = angle - arcSpread / 2;
        
        let currentAngle = startAngle;
        node.children.forEach((child, i) => {
          const childDescendants = countDescendants(child);
          const childArcPortion = (childDescendants / totalDescendants) * arcSpread;
          const childAngle = currentAngle + childArcPortion / 2;
          currentAngle += childArcPortion;
          
          const childRadius = 30 + Math.sqrt(childDescendants) * 8;
          positionNode(child, depth + 1, childAngle, childRadius, x, z);
        });
      }
    }
    
    positionNode(root, 0, fileAngle, 0, baseX, baseZ);
  }
  
  // Helper to get consistent ID
  const getFileId = (f) => f.id || f.fileName;
  
  // Get icon/color for non-AST buffer files (markdown, lisp)
  function getBufferIcon(fileName) {
    if (fileName.endsWith('.md')) return '📝';
    if (fileName.endsWith('.lisp')) return '🔮';
    return '📄';
  }
  
  function getBufferColor(fileName) {
    if (fileName.endsWith('.md')) return 0x88ccff;     // Markdown - light blue
    if (fileName.endsWith('.lisp')) return 0xff79c6;  // Lisp - pink
    return 0x666688;
  }
  
  // Create a simple buffer node mesh for non-AST files
  function createBufferNodeMesh(file) {
    const size = 12;
    const color = getBufferColor(file.fileName);
    const icon = getBufferIcon(file.fileName);
    
    const geo = new THREE.IcosahedronGeometry(size, 1);
    const mat = new THREE.MeshBasicMaterial({
      color: color,
      transparent: true,
      opacity: 0.85,
    });
    
    const mesh = new THREE.Mesh(geo, mat);
    mesh.userData = {
      type: 'Buffer',
      fileName: file.fileName,
      filePath: file.filePath,
      fileId: getFileId(file),
      size,
      baseColor: color,
      displayName: file.fileName,
      icon: icon,
      targetPos: new THREE.Vector3(),
      pulsePhase: Math.random() * Math.PI * 2,
      isSourceNode: true,
      isBuffer: true,
    };
    
    return mesh;
  }

  function updateSourceVisualization(files) {
    astFiles = files;
    
    if (!sourcesVisible) return;
    
    const currentFileIds = new Set(files.map(f => getFileId(f)));
    
    // Remove old visualizations
    sourceFiles.forEach((fileData, fileId) => {
      if (!currentFileIds.has(fileId)) {
        fileData.nodes.forEach(mesh => {
          scene.remove(mesh);
          mesh.geometry?.dispose();
          mesh.material?.dispose();
        });
        sourceFiles.delete(fileId);
      }
    });
    
    // Remove old connections
    sourceConnections.forEach((conn, key) => {
      // Compatibility: check fileId if available, else fallback to fileName check?
      // Actually we should store fileId in conn
      const idToCheck = conn.fileId || conn.fileName;
      if (!currentFileIds.has(idToCheck)) {
        scene.remove(conn.line);
        conn.line.geometry?.dispose();
        conn.line.material?.dispose();
        sourceConnections.delete(key);
      }
    });
    
    // Create/update visualizations
    const astFiles = files.filter(f => f.ast);
    const bufferFiles = files.filter(f => !f.ast); // Markdown, Lisp, etc.
    const totalAstFiles = astFiles.length;
    const totalBufferFiles = bufferFiles.length;
    let fileIndex = 0;
    
    // Layout AST files (existing behavior)
    astFiles.forEach((file) => {
      const fileId = getFileId(file);

      // Add fileName to root node
      file.ast.fileName = file.fileName;
      
      layoutSourceTree(file.ast, fileIndex, totalAstFiles);
      fileIndex++;
      
      let fileData = sourceFiles.get(fileId);
      if (!fileData) {
        fileData = { nodes: new Map() };
        sourceFiles.set(fileId, fileData);
      }
      
      const currentNodeIds = new Set();
      
      function processNode(node, parentId) {
        if (!node) return;
        
        currentNodeIds.add(node.id);
        
        let mesh = fileData.nodes.get(node.id);
        if (!mesh) {
          mesh = createSourceNodeMesh(node, file); // Pass full file object
          mesh.position.set(node.targetX || 0, node.targetY || 0, node.targetZ || 0);
          scene.add(mesh);
          fileData.nodes.set(node.id, mesh);
        }
        
        // Update mesh data
        mesh.userData.targetPos.set(node.targetX || 0, node.targetY || 0, node.targetZ || 0);
        mesh.userData.displayName = getNodeDisplayName(node);
        mesh.userData.loc = node.loc;
        mesh.visible = sourcesVisible;
        
        // Create connection to parent
        if (parentId) {
          const connKey = `${fileId}:${node.id}->${parentId}`;
          if (!sourceConnections.has(connKey)) {
            const thickness = node.depth < 3 ? 2 : 1.2;
            const line = createSourceConnection(thickness);
            line.visible = sourcesVisible;
            scene.add(line);
            sourceConnections.set(connKey, { 
              line, 
              childId: node.id, 
              parentId, 
              fileId: fileId,   // Store ID
              fileName: file.fileName,
              depth: node.depth 
            });
          } else {
            sourceConnections.get(connKey).line.visible = sourcesVisible;
          }
        }
        
        // Process children
        if (node.children) {
          node.children.forEach(child => processNode(child, node.id));
        }
      }
      
      processNode(file.ast, null);
      
      // Remove deleted nodes
      fileData.nodes.forEach((mesh, nodeId) => {
        if (!currentNodeIds.has(nodeId)) {
          scene.remove(mesh);
          mesh.geometry?.dispose();
          mesh.material?.dispose();
          fileData.nodes.delete(nodeId);
        }
      });
    });
    
    // Layout buffer files (markdown, lisp) - simple nodes in an arc
    bufferFiles.forEach((file, bufferIndex) => {
      const fileId = getFileId(file);
      
      let fileData = sourceFiles.get(fileId);
      if (!fileData) {
        fileData = { nodes: new Map(), isBuffer: true };
        sourceFiles.set(fileId, fileData);
      }
      
      const bufferId = `buffer-${fileId}`;
      let mesh = fileData.nodes.get(bufferId);
      
      // Position buffer files in an arc below AST files
      const bufferRadius = 80;
      const angleSpread = Math.PI * 0.8;
      const startAngle = Math.PI + (Math.PI - angleSpread) / 2;
      const angle = totalBufferFiles === 1 
        ? Math.PI * 1.5 
        : startAngle + (angleSpread / (totalBufferFiles - 1)) * bufferIndex;
      
      const targetX = Math.cos(angle) * bufferRadius;
      const targetY = -60; // Below the main AST view
      const targetZ = Math.sin(angle) * bufferRadius;
      
      if (!mesh) {
        mesh = createBufferNodeMesh(file);
        mesh.position.set(targetX, targetY, targetZ);
        scene.add(mesh);
        fileData.nodes.set(bufferId, mesh);
      }
      
      mesh.userData.targetPos.set(targetX, targetY, targetZ);
      mesh.visible = sourcesVisible;
    });
    
    // Clean orphaned connections
    sourceConnections.forEach((conn, key) => {
      // Use fileId to lookup
      const idToCheck = conn.fileId || conn.fileName;
      const fileData = sourceFiles.get(idToCheck);
      
      if (!fileData || !fileData.nodes.has(conn.childId)) {
        scene.remove(conn.line);
        conn.line.geometry?.dispose();
        conn.line.material?.dispose();
        sourceConnections.delete(key);
      }
    });
  }
  
  // Tab switching
  function setTab(tab) {
    if (tab !== 'processes' && tab !== 'sources') return;
    currentTab = tab;
    
    // Toggle visibility
    sourcesVisible = (tab === 'sources');
    
    // Hide/show process meshes
    window.ProcessTreeViz.meshes?.forEach(mesh => {
      mesh.visible = !sourcesVisible;
    });
    window.ProcessTreeViz.connections?.forEach(conn => {
      if (conn.line) conn.line.visible = !sourcesVisible;
    });
    window.ProcessTreeViz.graveyard?.forEach(grave => {
      if (grave.mesh) grave.mesh.visible = !sourcesVisible;
    });
    
    // Hide/show source meshes
    sourceFiles.forEach(fileData => {
      fileData.nodes.forEach(mesh => {
        mesh.visible = sourcesVisible;
      });
    });
    sourceConnections.forEach(conn => {
      conn.line.visible = sourcesVisible;
    });
    
    // Link Update Labels
    const processLabels = document.getElementById('labels');
    const sourceLabels = document.getElementById('source-labels');
    const hudCenter = document.querySelector('.hud.center');
    
    if (processLabels) processLabels.style.display = sourcesVisible ? 'none' : 'block';
    if (sourceLabels) sourceLabels.style.display = sourcesVisible ? 'block' : 'none';
    if (hudCenter) hudCenter.style.display = sourcesVisible ? 'none' : 'flex';
    
    // Reset camera for sources view
    if (sourcesVisible) {
      // Re-run visualization with current files
      updateSourceVisualization(astFiles);
    }
    
    updateTabUI();
  }
  
  // Tour Mode for Sources
  let tourMode = false;
  let tourList = [];
  let tourIndex = 0;
  let tourAutoPlay = false;
  let tourInterval = null;

  function buildTourList() {
    const list = [];
    sourceFiles.forEach(fileData => {
      // Add file root
      // list.push(fileData.nodes.get(fileData.rootId)); 
      
      // Add interesting nodes
      fileData.nodes.forEach(mesh => {
        const d = mesh.userData;
        const important = ['Program', 'FunctionDeclaration', 'ClassDeclaration', 'MethodDefinition', 'ExportDefaultDeclaration'];
        if (important.includes(d.type)) {
          list.push(mesh);
        }
      });
    });
    
    // Sort by position roughly to make a logical path? 
    // Or just keep them in file/traversal order which map iteration should mostly preserve
    return list;
  }

  function startSourceTour() {
    tourMode = true;
    tourList = buildTourList();
    tourIndex = 0;
    
    if (tourList.length > 0) {
      focusOnNode(tourList[0]);
    }
    
    // Notify ProcessTree to update UI button state if needed
    if (window.ProcessTreeViz) {
      const btn = document.getElementById('tour-btn');
      if (btn) btn.textContent = '⏹ Stop Tour';
    }
  }

  function stopSourceTour() {
    tourMode = false;
    tourAutoPlay = false;
    if (tourInterval) {
      clearInterval(tourInterval);
      tourInterval = null;
    }
    focusedSourceNode = null;
    
    if (window.ProcessTreeViz) {
      const btn = document.getElementById('tour-btn');
      if (btn) btn.textContent = '🎬 Tour';
      window.ProcessTreeViz.controls.autoRotate = false;
    }
  }

  function tourNext() {
    if (!tourMode || tourList.length === 0) return;
    tourIndex = (tourIndex + 1) % tourList.length;
    focusOnNode(tourList[tourIndex]);
  }

  function tourPrev() {
    if (!tourMode || tourList.length === 0) return;
    tourIndex = (tourIndex - 1 + tourList.length) % tourList.length;
    focusOnNode(tourList[tourIndex]);
  }

  function focusOnNode(mesh) {
    if (!mesh) return;
    focusedSourceNode = mesh.userData.id;
    
    const controls = window.ProcessTreeViz.controls;
    const camera = window.ProcessTreeViz.camera;
    
    if (controls) {
      controls.target.copy(mesh.position);
      controls.autoRotate = true;
      controls.autoRotateSpeed = 2.0; // Slow rotation
      
      // Zoom in appropriately
      const dist = 100 + (mesh.userData.size || 10) * 5;
      const currentDist = camera.position.distanceTo(controls.target);
      
      // Smoothly move camera distance in animate loop? 
      // For now, let's just set a target for the animate loop to handle if we add that support
      // Or just jump
      /* 
      const direction = new THREE.Vector3().subVectors(camera.position, controls.target).normalize();
      camera.position.copy(controls.target).add(direction.multiplyScalar(dist));
      */
    }
    updateSourceLabels();
  }

  function toggleTourAutoPlay() {
    tourAutoPlay = !tourAutoPlay;
    if (tourAutoPlay) {
      tourInterval = setInterval(tourNext, 3000); // 3 seconds per node
    } else {
      if (tourInterval) {
        clearInterval(tourInterval);
        tourInterval = null;
      }
    }
  }

  function updateTabUI() {
    // Insert tabs into #header-center if it exists, otherwise create floating tabs
    let headerCenter = document.getElementById('header-center');
    let tabBar = document.getElementById('view-tabs');
    
    if (headerCenter) {
      // Use existing header structure
      if (!tabBar) {
        tabBar = document.createElement('div');
        tabBar.id = 'view-tabs';
        tabBar.style.cssText = 'display: flex; gap: 4px; margin-left: 16px;';
        headerCenter.appendChild(tabBar);
      }
    } else {
      // Fallback: create in header-right area
      const headerRight = document.getElementById('header-right');
      if (headerRight && !tabBar) {
        tabBar = document.createElement('div');
        tabBar.id = 'view-tabs';
        tabBar.style.cssText = 'display: flex; gap: 4px;';
        headerRight.insertBefore(tabBar, headerRight.firstChild);
      }
    }
    
    if (!tabBar) return; // No place to put tabs
    
    const processActive = currentTab === 'processes';
    const sourceActive = currentTab === 'sources';
    const fileCount = astFiles.filter(f => f.ast).length;
    
    tabBar.innerHTML = `
      <button id="tab-processes" class="hdr-btn" style="
        background: ${processActive ? (scheme?.accent || '#ff69b4') : 'rgba(255,255,255,0.08)'};
        color: ${processActive ? '#000' : '#888'};
      ">Proc</button>
      <button id="tab-sources" class="hdr-btn" style="
        background: ${sourceActive ? (scheme?.accent || '#ff69b4') : 'rgba(255,255,255,0.08)'};
        color: ${sourceActive ? '#000' : '#888'};
      ">Src${fileCount > 0 ? ' ' + fileCount : ''}</button>
    `;
    
    document.getElementById('tab-processes').onclick = () => setTab('processes');
    document.getElementById('tab-sources').onclick = () => setTab('sources');
  }
  
  // Source labels with rich info
  function updateSourceLabels() {
    if (!sourcesVisible) return;
    
    let container = document.getElementById('source-labels');
    if (!container) {
      container = document.createElement('div');
      container.id = 'source-labels';
      container.className = 'label-container';
      document.body.appendChild(container);
    }
    container.innerHTML = '';
    container.style.display = sourcesVisible ? 'block' : 'none';
    
    const width = window.innerWidth;
    const height = window.innerHeight;
    const camera = window.ProcessTreeViz.camera;
    
    sourceFiles.forEach((fileData, fileName) => {
      fileData.nodes.forEach((mesh) => {
        if (!mesh.visible) return;
        
        const pos = new THREE.Vector3();
        mesh.getWorldPosition(pos);
        const labelPos = pos.clone();
        labelPos.y += (mesh.userData.size || 6) + 4;
        labelPos.project(camera);
        
        const x = (labelPos.x * 0.5 + 0.5) * width;
        const y = (-labelPos.y * 0.5 + 0.5) * height;
        
        if (labelPos.z < 1 && x > -50 && x < width + 50 && y > -50 && y < height + 50) {
          const d = mesh.userData;
          const distToCamera = camera.position.distanceTo(pos);
          
          // Show more labels when zoomed in
          // Relaxed thresholds for visibility
          const important = ['Program', 'FunctionDeclaration', 'ClassDeclaration', 'MethodDefinition', 
                           'VariableDeclaration', 'ImportDeclaration', 'ExportDefaultDeclaration', 'ExportNamedDeclaration',
                           'Buffer']; // Buffer files (markdown, lisp) are always visible
          const isImportant = important.includes(d.type) || d.isBuffer;
          
          if (!isImportant && distToCamera > 800) return;
          if (distToCamera > 1200) return;
          
          const proximityScale = Math.max(0.5, Math.min(2.5, 300 / distToCamera));
          const opacity = Math.max(0.6, Math.min(1, 400 / distToCamera));
          const color = '#' + (d.baseColor || 0x666666).toString(16).padStart(6, '0');
          const isFocused = focusedSourceNode === d.id;
          const isHovered = hoveredNode === d.id;
          
          const label = document.createElement('div');
          label.className = 'proc-label source-label' + (isFocused ? ' focused' : '') + (isHovered ? ' hovered' : '') + (d.isBuffer ? ' buffer' : '');
          label.style.cssText = `
            left: ${x}px; top: ${y}px;
            opacity: ${isFocused || isHovered ? 1 : opacity};
            transform: translate(-50%, -100%) scale(${isFocused || isHovered ? proximityScale * 1.3 : proximityScale});
            cursor: pointer;
            pointer-events: auto;
            ${isFocused ? 'z-index: 100;' : ''}
            text-align: center;
          `;
          
          label.innerHTML = `
            <div style="
              font-size: ${d.isBuffer ? '10px' : '8px'}; font-weight: bold; color: ${color};
              text-shadow: 0 1px 2px rgba(0,0,0,0.8);
              white-space: nowrap;
              background: rgba(0,0,0,0.4);
              padding: 2px 4px;
              border-radius: 4px;
              display: inline-flex; overflow: visible; align-items: center; gap: 4px;
            ">
              <span style="font-size: ${d.isBuffer ? '14px' : '10px'};">${d.icon}</span>${d.displayName}
            </div>
            ${isHovered || isFocused ? `<div style="font-size: 7px; color: #888; margin-top: 1px;">${d.type}</div>` : ''}
          `;
          
          // Click to focus/navigate
          
          // Click to focus/navigate
          label.onclick = (e) => {
            e.stopPropagation();
            handleNodeClick(mesh);
          };
          
          container.appendChild(label);
        }
      });
    });
  }
  
  // Click handling for source nodes
  function handleNodeClick(mesh) {
    const d = mesh.userData;
    
    if (focusedSourceNode === d.id) {
      // Double-click to navigate to source
      // Prefer filePath if available, fallback to fileName (legacy/simple)
      const targetFile = d.filePath || d.fileName;
      
      if (d.loc && targetFile) {
        // Send message to VS Code to open file at line
        const vscode = window.vscodeApi || (typeof acquireVsCodeApi !== 'undefined' ? acquireVsCodeApi() : null);
        if (vscode) {
          vscode.postMessage({
            command: 'navigateToSource',
            filePath: d.filePath, // Explicitly send both
            fileName: d.fileName,
            line: d.loc.start.line,
            column: d.loc.start.column
          });
        }
        console.log(`📍 Navigate to ${targetFile}:${d.loc.start.line}`);
      }
      focusedSourceNode = null;
    } else {
      // Single click to focus
      focusedSourceNode = d.id;
      
      // Move camera to focus on node
      const controls = window.ProcessTreeViz.controls;
      if (controls) {
        controls.target.copy(mesh.position);
      }
    }
    
    updateSourceLabels();
  }
  
  // Click detection on 3D scene
  function onCanvasClick(event) {
    if (!sourcesVisible) return;
    
    const canvas = renderer.domElement;
    const rect = canvas.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
    
    raycaster.setFromCamera(mouse, camera);
    
    // Get all source meshes
    const meshArray = [];
    sourceFiles.forEach(fileData => {
      fileData.nodes.forEach(mesh => {
        if (mesh.visible) meshArray.push(mesh);
      });
    });
    
    const intersects = raycaster.intersectObjects(meshArray);
    
    if (intersects.length > 0) {
      handleNodeClick(intersects[0].object);
    } else {
      focusedSourceNode = null;
      updateSourceLabels();
    }
  }
  
  // Hover detection
  function onCanvasMove(event) {
    if (!sourcesVisible) return;
    
    const canvas = renderer.domElement;
    const rect = canvas.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
    
    raycaster.setFromCamera(mouse, camera);
    
    const meshArray = [];
    sourceFiles.forEach(fileData => {
      fileData.nodes.forEach(mesh => {
        if (mesh.visible) meshArray.push(mesh);
      });
    });
    
    const intersects = raycaster.intersectObjects(meshArray);
    const newHovered = intersects.length > 0 ? intersects[0].object.userData.id : null;
    
    if (newHovered !== hoveredNode) {
      hoveredNode = newHovered;
      canvas.style.cursor = hoveredNode ? 'pointer' : 'default';
    }
  }
  
  // Animation hook
  let time = 0;
  function animateAST() {
    if (!sourcesVisible) return;
    
    time += 0.016;
    
    // Animate source nodes
    sourceFiles.forEach((fileData) => {
      fileData.nodes.forEach((mesh) => {
        if (!mesh.visible) return;
        const d = mesh.userData;
        if (!d.targetPos) return;
        
        // Smooth movement
        mesh.position.x += (d.targetPos.x - mesh.position.x) * 0.06;
        mesh.position.y += (d.targetPos.y - mesh.position.y) * 0.06;
        mesh.position.z += (d.targetPos.z - mesh.position.z) * 0.06;
        
        // Pulse effect
        const isFocused = focusedSourceNode === d.id;
        const isHovered = hoveredNode === d.id;
        const pulseAmp = isFocused ? 0.25 : (isHovered ? 0.15 : 0.08);
        const pulse = 1 + Math.sin(time * (isFocused ? 2 : 0.8) + d.pulsePhase) * pulseAmp;
        const sizeMult = isFocused ? 1.5 : (isHovered ? 1.2 : 1);
        mesh.scale.setScalar((d.size / 6) * pulse * sizeMult);
        
        // Opacity
        mesh.material.opacity = isFocused ? 1 : (isHovered ? 0.95 : 0.85);
      });
    });
    
    // Update connections
    sourceConnections.forEach(conn => {
      if (!conn.line.visible) return;
      
      const fileData = sourceFiles.get(conn.fileName);
      if (!fileData) return;
      
      const childMesh = fileData.nodes.get(conn.childId);
      const parentMesh = fileData.nodes.get(conn.parentId);
      if (childMesh && parentMesh) {
        updateConnectionMesh(conn, childMesh.position, parentMesh.position);
        
        // Highlight connections to focused node
        const isFocusPath = focusedSourceNode && 
          (conn.childId === focusedSourceNode || conn.parentId === focusedSourceNode);
        conn.line.material.opacity = isFocusPath ? 0.9 : 0.5;
        conn.line.material.color.setHex(isFocusPath ? scheme.three.connectionActive : scheme.three.connectionLine);
      }
    });
    
    updateSourceLabels();
  }
  
  // Initialize
  renderer.domElement.addEventListener('click', onCanvasClick);
  renderer.domElement.addEventListener('mousemove', onCanvasMove);
  
  // Create initial tab UI
  updateTabUI();
  
  // Keyboard shortcut: Tab to switch views
  document.addEventListener('keydown', (e) => {
    if (e.key === 'Tab' && !e.ctrlKey && !e.altKey && !e.shiftKey) {
      e.preventDefault();
      setTab(currentTab === 'processes' ? 'sources' : 'processes');
    }
    // Escape to unfocus
    if (e.key === 'Escape' && sourcesVisible) {
      focusedSourceNode = null;
      updateSourceLabels();
    }
  });
  
  // Expose API
  window.ASTTreeViz = {
    updateASTVisualization: updateSourceVisualization,
    animateAST,
    setTab,
    getTab: () => currentTab,
    sourceFiles,
    sourceConnections,
    focusNode: (id) => { focusedSourceNode = id; },
    // Tour API
    startTour: startSourceTour,
    stopTour: stopSourceTour,
    tourNext,
    tourPrev,
    toggleTourAutoPlay,
    isTourMode: () => tourMode,
  };
  
  console.log('📜 Source Tree Visualization loaded - Press Tab to switch views');
  
  // Request initial data after a short delay to ensure VSCode API is ready
  setTimeout(() => {
    const vscode = window.vscodeApi || (typeof acquireVsCodeApi !== 'undefined' ? acquireVsCodeApi() : null);
    if (vscode) {
      console.log('📡 Requesting initial AST data...');
      vscode.postMessage({ command: 'requestAST' });
    }
  }, 200);

  } // End initASTVisualization
})();
