// 3D AST Tree Visualization
// Renders JavaScript/TypeScript AST as interactive 3D trees

(function() {
  'use strict';
  
  // Only initialize if we have ProcessTreeViz (shared scene)
  if (!window.ProcessTreeViz) {
    console.warn('AST visualization requires ProcessTreeViz to be loaded first');
    return;
  }
  
  const { scene, camera, colorSchemes } = window.ProcessTreeViz;
  let currentTheme = window.ProcessTreeViz.getTheme();
  let scheme = colorSchemes[currentTheme];
  
  // AST visualization state
  const astMeshes = new Map(); // fileName -> { root, nodes: Map<id, mesh> }
  const astConnections = new Map();
  let astFiles = [];
  
  // AST node colors by type
  const nodeColors = {
    Program: 0x88ccff,
    FunctionDeclaration: 0xb06bff,
    FunctionExpression: 0xb06bff,
    ArrowFunctionExpression: 0xa060e0,
    ClassDeclaration: 0xff69b4,
    ClassExpression: 0xff69b4,
    MethodDefinition: 0xe050a0,
    VariableDeclaration: 0x6bff9f,
    VariableDeclarator: 0x50d080,
    ImportDeclaration: 0xffeb6b,
    ExportNamedDeclaration: 0xffc040,
    ExportDefaultDeclaration: 0xffa030,
    CallExpression: 0x6b9fff,
    MemberExpression: 0x5080d0,
    Identifier: 0x888888,
    Literal: 0x666666,
    ObjectExpression: 0x6bffff,
    ArrayExpression: 0x50d0d0,
    IfStatement: 0xff9f6b,
    ForStatement: 0xe08050,
    WhileStatement: 0xd07040,
    ReturnStatement: 0x80ff80,
    BlockStatement: 0x555555,
    ExpressionStatement: 0x444444,
  };
  
  // Position offset for AST trees (to the right of process tree)
  const AST_OFFSET_X = 300;
  const AST_OFFSET_Z = 0;
  
  function getNodeColor(type) {
    return nodeColors[type] || 0x666666;
  }
  
  function getNodeSize(node) {
    // Size based on code span and importance
    const span = node.end - node.start;
    const baseSize = Math.max(3, Math.min(10, 2 + Math.log(span + 1) * 0.8));
    
    // Boost important node types
    const important = ['FunctionDeclaration', 'ClassDeclaration', 'MethodDefinition', 'Program'];
    if (important.includes(node.type)) return baseSize * 1.3;
    return baseSize;
  }
  
  function createASTNodeMesh(node) {
    const size = getNodeSize(node);
    const color = getNodeColor(node.type);
    
    const geo = new THREE.SphereGeometry(size, 10, 10);
    const mat = new THREE.MeshBasicMaterial({
      color: color,
      transparent: true,
      opacity: 0.8,
    });
    
    const mesh = new THREE.Mesh(geo, mat);
    mesh.userData = {
      ...node,
      size,
      baseColor: color,
      targetPos: new THREE.Vector3(),
      pulsePhase: Math.random() * Math.PI * 2,
    };
    
    return mesh;
  }
  
  function createASTConnection() {
    const geo = new THREE.CylinderGeometry(0.8, 0.8, 1, 6);
    const mat = new THREE.MeshBasicMaterial({
      color: scheme.three.connectionLine,
      transparent: true,
      opacity: 0.4,
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
  
  function layoutASTTree(root, fileIndex = 0) {
    if (!root) return;
    
    const levelHeight = 30;
    const baseX = AST_OFFSET_X + fileIndex * 200;
    const baseZ = AST_OFFSET_Z;
    
    function positionNode(node, depth, angle, radius, parentX, parentZ) {
      const childCount = node.children?.length || 0;
      
      const x = parentX + Math.cos(angle) * radius;
      const z = parentZ + Math.sin(angle) * radius;
      const y = -depth * levelHeight;
      
      node.targetX = x;
      node.targetY = y;
      node.targetZ = z;
      
      if (childCount > 0) {
        const arcSpread = Math.min(Math.PI * 1.2, Math.PI * 0.25 * childCount);
        const startAngle = angle - arcSpread / 2;
        const childRadius = 20 + childCount * 5;
        
        node.children.forEach((child, i) => {
          const childAngle = childCount === 1 ? angle : startAngle + (arcSpread / Math.max(1, childCount - 1)) * i;
          positionNode(child, depth + 1, childAngle, childRadius, x, z);
        });
      }
    }
    
    positionNode(root, 0, -Math.PI / 2, 0, baseX, baseZ);
  }
  
  function updateASTVisualization(files) {
    astFiles = files;
    
    // Track which files are still present
    const currentFileNames = new Set(files.map(f => f.fileName));
    
    // Remove old file visualizations
    astMeshes.forEach((fileData, fileName) => {
      if (!currentFileNames.has(fileName)) {
        fileData.nodes.forEach(mesh => {
          scene.remove(mesh);
          mesh.geometry.dispose();
          mesh.material.dispose();
        });
        astMeshes.delete(fileName);
      }
    });
    
    // Remove old connections for removed files
    astConnections.forEach((conn, key) => {
      const fileName = key.split('-')[0];
      if (!currentFileNames.has(fileName)) {
        scene.remove(conn.line);
        conn.line.geometry.dispose();
        conn.line.material.dispose();
        astConnections.delete(key);
      }
    });
    
    // Update/create visualizations for each file
    files.forEach((file, fileIndex) => {
      if (!file.ast) return;
      
      layoutASTTree(file.ast, fileIndex);
      
      // Get or create file data
      let fileData = astMeshes.get(file.fileName);
      if (!fileData) {
        fileData = { nodes: new Map() };
        astMeshes.set(file.fileName, fileData);
      }
      
      const currentNodeIds = new Set();
      
      // Process all nodes recursively
      function processNode(node, parentId) {
        if (!node) return;
        
        currentNodeIds.add(node.id);
        
        // Create or update mesh
        let mesh = fileData.nodes.get(node.id);
        if (!mesh) {
          mesh = createASTNodeMesh(node);
          mesh.position.set(node.targetX || 0, node.targetY || 0, node.targetZ || 0);
          scene.add(mesh);
          fileData.nodes.set(node.id, mesh);
        }
        
        // Update target position
        mesh.userData.targetPos.set(node.targetX || 0, node.targetY || 0, node.targetZ || 0);
        mesh.userData.name = node.name;
        mesh.userData.type = node.type;
        
        // Create connection to parent
        if (parentId) {
          const connKey = `${node.id}->${parentId}`;
          if (!astConnections.has(connKey)) {
            const line = createASTConnection();
            scene.add(line);
            astConnections.set(connKey, { line, childId: node.id, parentId, fileName: file.fileName });
          }
        }
        
        // Process children
        if (node.children) {
          node.children.forEach(child => processNode(child, node.id));
        }
      }
      
      processNode(file.ast, null);
      
      // Remove nodes that no longer exist
      fileData.nodes.forEach((mesh, nodeId) => {
        if (!currentNodeIds.has(nodeId)) {
          scene.remove(mesh);
          mesh.geometry.dispose();
          mesh.material.dispose();
          fileData.nodes.delete(nodeId);
        }
      });
    });
    
    // Clean up orphaned connections
    astConnections.forEach((conn, key) => {
      const [childPart] = key.split('->');
      const fileName = childPart.split('-')[0];
      const fileData = astMeshes.get(fileName);
      
      if (!fileData || !fileData.nodes.has(conn.childId) || !fileData.nodes.has(conn.parentId)) {
        scene.remove(conn.line);
        conn.line.geometry.dispose();
        conn.line.material.dispose();
        astConnections.delete(key);
      }
    });
    
    // Update AST labels
    updateASTLabels();
  }
  
  function updateASTLabels() {
    let container = document.getElementById('ast-labels');
    if (!container) {
      container = document.createElement('div');
      container.id = 'ast-labels';
      container.className = 'label-container';
      document.body.appendChild(container);
    }
    container.innerHTML = '';
    
    const width = window.innerWidth;
    const height = window.innerHeight;
    
    astMeshes.forEach((fileData, fileName) => {
      fileData.nodes.forEach((mesh, nodeId) => {
        const pos = new THREE.Vector3();
        mesh.getWorldPosition(pos);
        const labelPos = pos.clone();
        labelPos.y += (mesh.userData.size || 5) + 3;
        labelPos.project(camera);
        
        const x = (labelPos.x * 0.5 + 0.5) * width;
        const y = (-labelPos.y * 0.5 + 0.5) * height;
        
        if (labelPos.z < 1 && x > -50 && x < width + 50 && y > -50 && y < height + 50) {
          const d = mesh.userData;
          const distToCamera = camera.position.distanceTo(pos);
          const proximityScale = Math.max(0.4, Math.min(2, 120 / distToCamera));
          const opacity = Math.max(0.5, Math.min(1, 250 / distToCamera));
          
          // Only show labels for important nodes or when zoomed in
          const important = ['FunctionDeclaration', 'ClassDeclaration', 'MethodDefinition', 'VariableDeclaration', 'ImportDeclaration', 'ExportNamedDeclaration'];
          if (distToCamera > 150 && !important.includes(d.type)) return;
          
          const color = '#' + (d.baseColor || 0x666666).toString(16).padStart(6, '0');
          
          const label = document.createElement('div');
          label.className = 'proc-label ast-label';
          label.style.left = x + 'px';
          label.style.top = y + 'px';
          label.style.opacity = opacity;
          label.style.transform = 'translate(-50%, -100%) scale(' + proximityScale + ')';
          label.innerHTML = 
            '<div class="name" style="color:' + color + ';font-size:9px;">' + (d.name || d.type) + '</div>' +
            '<div class="info" style="color:' + scheme.foregroundMuted + ';font-size:7px;">' + d.type + '</div>';
          container.appendChild(label);
        }
      });
    });
  }
  
  // Animation hook - call from main animation loop
  let time = 0;
  function animateAST() {
    time += 0.016;
    
    // Animate AST nodes
    astMeshes.forEach((fileData) => {
      fileData.nodes.forEach((mesh) => {
        const d = mesh.userData;
        if (!d.targetPos) return;
        
        // Smooth movement to target
        mesh.position.x += (d.targetPos.x - mesh.position.x) * 0.08;
        mesh.position.y += (d.targetPos.y - mesh.position.y) * 0.08;
        mesh.position.z += (d.targetPos.z - mesh.position.z) * 0.08;
        
        // Gentle pulse
        const pulse = 1 + Math.sin(time * 0.8 + d.pulsePhase) * 0.1;
        mesh.scale.setScalar((d.size / 6) * pulse);
      });
    });
    
    // Update connections
    astConnections.forEach(conn => {
      const fileName = conn.fileName;
      const fileData = astMeshes.get(fileName);
      if (!fileData) return;
      
      const childMesh = fileData.nodes.get(conn.childId);
      const parentMesh = fileData.nodes.get(conn.parentId);
      if (childMesh && parentMesh) {
        updateConnectionMesh(conn, childMesh.position, parentMesh.position);
      }
    });
    
    // Update labels
    updateASTLabels();
  }
  
  // Expose API
  window.ASTTreeViz = {
    updateASTVisualization,
    animateAST,
    astMeshes,
    astConnections,
    getNodeColor,
  };
  
  console.log('🌳 AST Tree Visualization loaded');
})();
