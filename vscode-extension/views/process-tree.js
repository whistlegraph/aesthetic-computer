// 3D Process Tree Visualization
// Shared between VS Code extension and local dev testing

(function() {
  'use strict';
  
  // Check if we're in VS Code webview or standalone
  const isVSCode = typeof acquireVsCodeApi === 'function';
  
  // 🎨 Color Schemes (imported from color-schemes.js or embedded)
  const colorSchemes = window.AestheticColorSchemes?.schemes || {
  "dark": {
    "background": "#181318",
    "backgroundAlt": "#141214",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#a87090",
    "accentBright": "#ff69b4",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1577752,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16738740,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "light": {
    "background": "#fcf7c5",
    "backgroundAlt": "#f5f0c0",
    "foreground": "#281e5a",
    "foregroundBright": "#281e5a",
    "foregroundMuted": "#806060",
    "accent": "#387adf",
    "accentBright": "#006400",
    "statusOnline": "#006400",
    "categories": {
      "editor": 8405200,
      "tui": 13648000,
      "bridge": 2129984,
      "db": 10518528,
      "proxy": 2121920,
      "ai": 12607520,
      "shell": 32896,
      "dev": 2129984,
      "ide": 2121920,
      "lsp": 6316128,
      "kernel": 3701471
    },
    "three": {
      "sceneBackground": 16578501,
      "kernelOuter": 3701471,
      "kernelRing": 25600,
      "kernelCore": 3701471,
      "connectionLine": 11051136,
      "connectionActive": 25600,
      "deadProcess": 11051136
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.2)",
      "overlay": "rgba(252, 247, 197, 0.95)"
    }
  },
  "red": {
    "background": "#181010",
    "backgroundAlt": "#140c0c",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#ff5555",
    "accentBright": "#ff8888",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1576976,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16746632,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "orange": {
    "background": "#181410",
    "backgroundAlt": "#14100c",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#ffb86c",
    "accentBright": "#ffd8a8",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1578000,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16767144,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "yellow": {
    "background": "#181810",
    "backgroundAlt": "#14140c",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#f1fa8c",
    "accentBright": "#ffffa0",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1579024,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16777120,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "green": {
    "background": "#101810",
    "backgroundAlt": "#0c140c",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#50fa7b",
    "accentBright": "#80ffae",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1054736,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 8454062,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "blue": {
    "background": "#101418",
    "backgroundAlt": "#0c1014",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#61afef",
    "accentBright": "#8cd0ff",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1053720,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 9228543,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "indigo": {
    "background": "#121018",
    "backgroundAlt": "#0e0c14",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#6272a4",
    "accentBright": "#8be9fd",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1183768,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 9169405,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "violet": {
    "background": "#161016",
    "backgroundAlt": "#120c12",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#bd93f9",
    "accentBright": "#ff79c6",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1445910,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16742854,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "pink": {
    "background": "#181014",
    "backgroundAlt": "#140c10",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#ff79c6",
    "accentBright": "#ff9ce6",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1576980,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16751846,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  },
  "pencil": {
    "background": "#181818",
    "backgroundAlt": "#141414",
    "foreground": "#ffffffcc",
    "foregroundBright": "#ffffff",
    "foregroundMuted": "#555555",
    "accent": "#e0e0e0",
    "accentBright": "#ffffff",
    "statusOnline": "#0f0",
    "categories": {
      "editor": 11561983,
      "tui": 16738740,
      "bridge": 7077791,
      "db": 16771947,
      "proxy": 7053311,
      "ai": 16752491,
      "shell": 7077887,
      "dev": 7077791,
      "ide": 7053311,
      "lsp": 8947848,
      "kernel": 8965375
    },
    "three": {
      "sceneBackground": 1579032,
      "kernelOuter": 4491519,
      "kernelRing": 6728447,
      "kernelCore": 8965375,
      "connectionLine": 4473924,
      "connectionActive": 16777215,
      "deadProcess": 4473924
    },
    "ui": {
      "shadow": "rgba(0, 0, 0, 0.6)",
      "overlay": "rgba(0, 0, 0, 0.85)"
    }
  }
};
  
  // Detect theme from data attribute, URL param, VS Code CSS vars, or OS preference
  function detectTheme() {
    // Check data attribute first (set by the HTML)
    const dataTheme = document.body.dataset.theme;
    if (colorSchemes[dataTheme]) return dataTheme;
    
    // Check URL param
    const urlParams = new URLSearchParams(window.location.search);
    const urlTheme = urlParams.get('theme');
    if (colorSchemes[urlTheme]) return urlTheme;
    
    // Check VS Code CSS variables
    if (typeof getComputedStyle !== 'undefined') {
      const bgColor = getComputedStyle(document.body).getPropertyValue('--vscode-editor-background').trim();
      
      if (bgColor && bgColor.startsWith('#')) {
        // Exact match against known backgrounds
        const bgLower = bgColor.toLowerCase();
        for (const [key, scheme] of Object.entries(colorSchemes)) {
          if (scheme.background.toLowerCase() === bgLower) {
            return key;
          }
        }

        // Check for Light vs Dark if no exact match
        const r = parseInt(bgColor.slice(1, 3), 16);
        const g = parseInt(bgColor.slice(3, 5), 16);
        const b = parseInt(bgColor.slice(5, 7), 16);
        const luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
        return luminance > 0.5 ? 'light' : 'dark';
      }
    }
    
    // Fall back to OS preference (prefers-color-scheme)
    if (typeof window !== 'undefined' && window.matchMedia) {
      if (window.matchMedia('(prefers-color-scheme: light)').matches) {
        return 'light';
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
  
  // Dev badge is now in the HTML for dev.html - no need to create dynamically
  
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
    // Tour UI positioned above center panel, non-overlapping
    tourUI.style.cssText = `
      position: fixed;
      bottom: 180px;
      left: 50%;
      transform: translateX(-50%);
      background: ${scheme.ui.overlay};
      padding: 16px 24px;
      border-radius: 12px;
      color: ${scheme.foregroundBright};
      font-family: monospace;
      font-size: 12px;
      z-index: 1000;
      display: none;
      text-align: center;
      border: 1px solid ${scheme.foregroundMuted}40;
      backdrop-filter: blur(8px);
      -webkit-backdrop-filter: blur(8px);
      min-width: 280px;
    `;
    
    if (tourMode && tourProcessList.length > 0) {
      const current = tourProcessList[tourIndex];
      const mesh = meshes.get(current);
      const name = mesh?.userData?.name || current;
      const icon = mesh?.userData?.icon || '●';
      const category = mesh?.userData?.category || '';
      
      tourUI.style.display = 'block';
      tourUI.innerHTML = `
        <div style="margin-bottom:10px;font-size:12px;color:${scheme.accent};text-transform:uppercase;letter-spacing:1px;">🎬 Tour Mode</div>
        <div style="font-size:24px;margin-bottom:4px;color:${scheme.foregroundBright};">${icon}</div>
        <div style="font-size:14px;font-weight:bold;color:${scheme.foregroundBright};margin-bottom:2px;">${name}</div>
        <div style="color:${scheme.foregroundMuted};margin-bottom:14px;font-size:11px;">${category} • ${tourIndex + 1}/${tourProcessList.length}</div>
        <div style="display:flex;gap:8px;justify-content:center;">
          <button onclick="ProcessTreeViz.tourPrev()" class="header-btn" style="pointer-events:auto;padding:8px 14px;">← Prev</button>
          <button onclick="ProcessTreeViz.toggleAutoPlay()" class="header-btn" style="pointer-events:auto;padding:8px 14px;">${tourAutoPlay ? '⏸ Stop' : '▶ Auto'}</button>
          <button onclick="ProcessTreeViz.tourNext()" class="header-btn" style="pointer-events:auto;padding:8px 14px;">Next →</button>
          <button onclick="ProcessTreeViz.exitTour()" class="header-btn" style="pointer-events:auto;padding:8px 14px;">✕</button>
        </div>
        ${tourAutoPlay ? `<div style="color:${scheme.accentBright};margin-top:10px;font-size:10px;">▶ Auto-playing...</div>` : ''}
      `;
      // Hide the tour button when in tour mode
      const btn = document.getElementById('tour-btn');
      if (btn) btn.style.display = 'none';
    } else {
      tourUI.style.display = 'none';
      // Show the tour button when not in tour mode
      const btn = document.getElementById('tour-btn');
      if (btn) btn.style.display = '';
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
    if (window.ASTTreeViz?.getTab() === 'sources') {
      window.ASTTreeViz.startTour();
      // Ensure local state reflects we are "busy" or just let AST handle it
      return;
    }

    tourMode = true;
    tourProcessList = buildTourList();
    tourIndex = 0;
    if (tourProcessList.length > 0) {
      focusOnProcess(tourProcessList[0]);
    }
    updateTourUI();
  }
  
  function exitTour() {
    if (window.ASTTreeViz?.getTab() === 'sources') {
      window.ASTTreeViz.stopTour();
      return;
    }

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
    if (window.ASTTreeViz?.getTab() === 'sources') {
      window.ASTTreeViz.tourNext();
      return;
    }

    if (!tourMode || tourProcessList.length === 0) return;
    tourIndex = (tourIndex + 1) % tourProcessList.length;
    focusOnProcess(tourProcessList[tourIndex]);
    updateTourUI();
  }
  
  function tourPrev() {
    if (window.ASTTreeViz?.getTab() === 'sources') {
      window.ASTTreeViz.tourPrev();
      return;
    }

    if (!tourMode || tourProcessList.length === 0) return;
    tourIndex = (tourIndex - 1 + tourProcessList.length) % tourProcessList.length;
    focusOnProcess(tourProcessList[tourIndex]);
    updateTourUI();
  }
  
  function toggleAutoPlay() {
    if (window.ASTTreeViz?.getTab() === 'sources') {
      window.ASTTreeViz.toggleTourAutoPlay();
      return;
    }

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
    const isSourceTour = window.ASTTreeViz?.getTab() === 'sources' && window.ASTTreeViz?.isTourMode();
    const isTourActive = tourMode || isSourceTour;

    // T to start tour
    if (e.key === 't' || e.key === 'T') {
      if (!isTourActive) {
        startTour();
      } else {
        exitTour();
      }
      return;
    }
    
    if (isTourActive) {
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
  
  function createConnectionLine(color) {
    const geo = new THREE.CylinderGeometry(1.5, 1.5, 1, 8);
    const mat = new THREE.MeshBasicMaterial({
      color: color || scheme.three.connectionLine, transparent: true, opacity: 0.5
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
        
        // Calculate rotation based on connection to parent (make label parallel to line)
        let rotation = 0;
        const parentPid = String(d.parentInteresting || 0);
        const parentMesh = meshes.has(parentPid) ? meshes.get(parentPid) : meshes.get('kernel');
        if (parentMesh && pid !== 'kernel') {
          const parentPos = new THREE.Vector3();
          parentMesh.getWorldPosition(parentPos);
          // Project both positions to 2D screen space
          const childScreen = pos.clone().project(camera);
          const parentScreen = parentPos.clone().project(camera);
          // Calculate angle in screen space
          const dx = (parentScreen.x - childScreen.x);
          const dy = (parentScreen.y - childScreen.y);
          rotation = Math.atan2(-dy, dx) * (180 / Math.PI);
          // Clamp rotation to reasonable range (-45 to 45 degrees)
          rotation = Math.max(-45, Math.min(45, rotation));
        }
        
        const label = document.createElement('div');
        label.className = 'proc-label';
        label.style.left = x + 'px';
        label.style.top = y + 'px';
        label.style.opacity = opacity;
        label.style.transform = 'translate(-50%, -100%) scale(' + proximityScale + ') rotate(' + rotation + 'deg)';
        // Show name, then command on second line, then stats (no background)
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
        
        // Respect current visibility
        const isSourcesTab = window.ASTTreeViz?.getTab() === 'sources';
        mesh.visible = !isSourcesTab;
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
      const childColor = colors[p.category] || 0x666666;
      if (parentPid && meshes.has(parentPid)) {
        const connKey = pid + '->' + parentPid;
        if (!connections.has(connKey)) {
          const line = createConnectionLine(childColor);
          scene.add(line);
          connections.set(connKey, { line, childPid: pid, parentPid, childColor });
          
          // Respect current visibility
          const isSourcesTab = window.ASTTreeViz?.getTab() === 'sources';
          line.visible = !isSourcesTab;
        }
      } else {
        const connKey = pid + '->kernel';
        if (!connections.has(connKey)) {
          const line = createConnectionLine(childColor);
          scene.add(line);
          connections.set(connKey, { line, childPid: pid, parentPid: 'kernel', childColor });
          
          // Respect current visibility
          const isSourcesTab = window.ASTTreeViz?.getTab() === 'sources';
          line.visible = !isSourcesTab;
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
        const graveItem = { 
          pid, 
          mesh, 
          name: d.name,
          icon: d.icon || '💀',
          cmd: d.cmdShort || d.cmd || '',
          category: d.category,
          deathTime: Date.now() 
        };
        graveyard.push(graveItem);
        meshes.delete(pid);

        // Respect current visibility
        const isSourcesTab = window.ASTTreeViz?.getTab() === 'sources';
        mesh.visible = !isSourcesTab;
        
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
        conn.line.material.opacity = focusedPid ? (involvesFocus ? 0.9 : 0.15) : 0.6;
        // Use child's category color for the line (color-coded connections)
        const childCategory = childMesh.userData?.category;
        const lineColor = involvesFocus ? scheme.three.connectionActive : (colors[childCategory] || conn.childColor || scheme.three.connectionLine);
        conn.line.material.color.setHex(lineColor);
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
  
  // Connection state tracking
  let connectionState = 'disconnected'; // disconnected, connecting, connected
  let reconnectAttempts = 0;
  let lastConnectTime = 0;
  
  // Connection log messages for the corner indicator
  const connectionLog = [];
  const MAX_LOG_LINES = 6;
  
  function addConnectionLog(msg) {
    const now = new Date();
    const ts = `${String(now.getHours()).padStart(2,'0')}:${String(now.getMinutes()).padStart(2,'0')}:${String(now.getSeconds()).padStart(2,'0')}`;
    connectionLog.push({ ts, msg });
    if (connectionLog.length > MAX_LOG_LINES) connectionLog.shift();
  }
  
  function updateConnectionUI() {
    const dot = document.getElementById('status-dot');
    let indicator = document.getElementById('connection-indicator');
    
    if (!indicator) {
      indicator = document.createElement('div');
      indicator.id = 'connection-indicator';
      indicator.style.cssText = `
        position: fixed; top: 50px; left: 12px;
        max-width: 280px;
        padding: 8px 10px;
        background: ${scheme.ui.shadow};
        border-radius: 6px;
        border: 1px solid ${scheme.foregroundMuted}30;
        backdrop-filter: blur(6px);
        -webkit-backdrop-filter: blur(6px);
        z-index: 500; pointer-events: none;
        transition: opacity 0.5s ease;
        font-family: monospace;
        font-size: 10px;
        line-height: 1.5;
      `;
      document.body.appendChild(indicator);
    }
    
    if (connectionState === 'connected') {
      dot?.classList.add('online');
      addConnectionLog('connected ✓');
      // Show briefly then fade out
      indicator.style.opacity = '1';
      renderConnectionIndicator(indicator);
      setTimeout(() => { indicator.style.opacity = '0'; }, 2000);
      setTimeout(() => { if (connectionState === 'connected') indicator.style.display = 'none'; }, 2500);
    } else {
      dot?.classList.remove('online');
      indicator.style.display = 'block';
      indicator.style.opacity = '1';
      
      if (connectionState === 'connecting') {
        addConnectionLog(`connecting... (attempt ${reconnectAttempts})`);
      } else {
        addConnectionLog(`waiting to reconnect (attempt ${reconnectAttempts})`);
      }
      
      renderConnectionIndicator(indicator);
    }
  }
  
  function renderConnectionIndicator(indicator) {
    const stateColor = connectionState === 'connected' ? (scheme.statusOnline || '#0f0')
                     : connectionState === 'connecting' ? (scheme.accent || '#ff69b4')
                     : (scheme.foregroundMuted || '#555');
    const stateIcon = connectionState === 'connected' ? '●'
                    : connectionState === 'connecting' ? '◌'
                    : '○';
    
    const logHtml = connectionLog.map(l => 
      `<div style="color: ${scheme.foregroundMuted}; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"><span style="color: ${scheme.foregroundMuted}80;">${l.ts}</span> ${l.msg}</div>`
    ).join('');
    
    indicator.innerHTML = `
      <div style="display: flex; align-items: center; gap: 6px; margin-bottom: 4px;">
        <span style="color: ${stateColor}; font-size: 8px;">${stateIcon}</span>
        <span style="color: ${scheme.foreground || '#fff'}; font-size: 10px; font-weight: bold;">process server</span>
      </div>
      ${logHtml}
      ${reconnectAttempts > 5 ? `<button onclick="location.reload()" style="margin-top: 6px; padding: 3px 8px; background: ${scheme.accent}; border: none; border-radius: 3px; color: ${scheme.foregroundBright}; cursor: pointer; pointer-events: auto; font-size: 9px; font-family: monospace;">↻ refresh</button>` : ''}
    `;
  }
  
  function connectWS() {
    connectionState = 'connecting';
    reconnectAttempts++;
    updateConnectionUI();
    
    try {
      ws = new WebSocket('ws://127.0.0.1:7890/ws');
      
      ws.onopen = () => {
        connectionState = 'connected';
        reconnectAttempts = 0;
        lastConnectTime = Date.now();
        updateConnectionUI();
        console.log('🟢 Connected to process server');
      };
      
      ws.onclose = () => {
        connectionState = 'disconnected';
        updateConnectionUI();
        // Exponential backoff: 1s, 2s, 4s, 8s, max 10s
        const delay = Math.min(1000 * Math.pow(2, Math.min(reconnectAttempts - 1, 3)), 10000);
        console.log(`🔴 Disconnected, reconnecting in ${delay}ms (attempt ${reconnectAttempts})`);
        setTimeout(connectWS, delay);
      };
      
      ws.onerror = (err) => {
        console.log('🔴 WebSocket error:', err);
        ws.close();
      };
      
      ws.onmessage = (e) => {
        try {
          const data = JSON.parse(e.data);
          if (data.system) {
            document.getElementById('uptime').textContent = data.system.uptime.formatted;
            document.getElementById('cpus').textContent = data.system.cpus;
            const m = data.system.memory;
            document.getElementById('mem-text').textContent = m.used + ' / ' + m.total;
            
            // Update stats graph with system data
            updateStatsGraph(data.system);
          }
          updateViz(data.processes);
        } catch {}
      };
    } catch (err) {
      console.log('🔴 WebSocket creation error:', err);
      connectionState = 'disconnected';
      updateConnectionUI();
      setTimeout(connectWS, 2000);
    }
  }
  
  // 📊 Stats Graph (CPU/Memory history)
  const GRAPH_POINTS = 60; // 60 data points
  const cpuHistory = new Array(GRAPH_POINTS).fill(0);
  const memHistory = new Array(GRAPH_POINTS).fill(0);
  let statsCanvas = null;
  let statsCtx = null;
  
  function initStatsGraph() {
    statsCanvas = document.getElementById('stats-graph-canvas');
    if (statsCanvas) {
      statsCtx = statsCanvas.getContext('2d');
      // Set actual pixel dimensions for crisp rendering
      const rect = statsCanvas.getBoundingClientRect();
      statsCanvas.width = rect.width * window.devicePixelRatio;
      statsCanvas.height = rect.height * window.devicePixelRatio;
      statsCtx.scale(window.devicePixelRatio, window.devicePixelRatio);
    }
  }
  
  function updateStatsGraph(system) {
    if (!statsCtx) initStatsGraph();
    if (!statsCtx) return;
    
    // Parse memory usage
    const m = system.memory;
    let memPct = 0;
    if (m && m.used && m.total) {
      const usedNum = parseFloat(m.used.replace(/[^\d.]/g, ''));
      const totalNum = parseFloat(m.total.replace(/[^\d.]/g, ''));
      if (totalNum > 0) {
        memPct = (usedNum / totalNum) * 100;
      }
    }
    
    // Calculate total CPU usage from all processes
    let totalCpu = 0;
    meshes.forEach((mesh, pid) => {
      if (pid !== 'kernel' && mesh.userData.cpu) {
        totalCpu += mesh.userData.cpu;
      }
    });
    // Normalize to percentage (divide by number of CPUs)
    const numCpus = parseInt(system.cpus) || 1;
    const cpuPct = Math.min(100, totalCpu / numCpus);
    
    // Shift history and add new values
    cpuHistory.shift();
    cpuHistory.push(cpuPct);
    memHistory.shift();
    memHistory.push(memPct);
    
    // Update text labels
    const cpuEl = document.getElementById('cpu-pct');
    const memEl = document.getElementById('mem-pct');
    if (cpuEl) cpuEl.textContent = cpuPct.toFixed(1);
    if (memEl) memEl.textContent = memPct.toFixed(1);
    
    // Draw graph
    drawStatsGraph();
  }
  
  function drawStatsGraph() {
    if (!statsCtx || !statsCanvas) return;
    
    const rect = statsCanvas.getBoundingClientRect();
    const w = rect.width;
    const h = rect.height;
    
    // Clear canvas
    statsCtx.clearRect(0, 0, w, h);
    
    // Colors based on theme
    const cpuColor = currentTheme === 'light' ? '#006400' : '#50fa7b';
    const memColor = currentTheme === 'light' ? '#c71585' : '#ff79c6';
    const gridColor = currentTheme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.1)';
    
    // Draw grid lines
    statsCtx.strokeStyle = gridColor;
    statsCtx.lineWidth = 0.5;
    for (let i = 0; i <= 4; i++) {
      const y = (h / 4) * i;
      statsCtx.beginPath();
      statsCtx.moveTo(0, y);
      statsCtx.lineTo(w, y);
      statsCtx.stroke();
    }
    
    // Draw CPU line
    statsCtx.strokeStyle = cpuColor;
    statsCtx.lineWidth = 1.5;
    statsCtx.beginPath();
    for (let i = 0; i < GRAPH_POINTS; i++) {
      const x = (w / (GRAPH_POINTS - 1)) * i;
      const y = h - (cpuHistory[i] / 100) * h;
      if (i === 0) statsCtx.moveTo(x, y);
      else statsCtx.lineTo(x, y);
    }
    statsCtx.stroke();
    
    // Fill CPU area (semi-transparent)
    statsCtx.fillStyle = cpuColor.replace(')', ',0.15)').replace('rgb', 'rgba').replace('#', '');
    if (cpuColor.startsWith('#')) {
      const r = parseInt(cpuColor.slice(1, 3), 16);
      const g = parseInt(cpuColor.slice(3, 5), 16);
      const b = parseInt(cpuColor.slice(5, 7), 16);
      statsCtx.fillStyle = `rgba(${r},${g},${b},0.15)`;
    }
    statsCtx.beginPath();
    statsCtx.moveTo(0, h);
    for (let i = 0; i < GRAPH_POINTS; i++) {
      const x = (w / (GRAPH_POINTS - 1)) * i;
      const y = h - (cpuHistory[i] / 100) * h;
      statsCtx.lineTo(x, y);
    }
    statsCtx.lineTo(w, h);
    statsCtx.closePath();
    statsCtx.fill();
    
    // Draw Memory line
    statsCtx.strokeStyle = memColor;
    statsCtx.lineWidth = 1.5;
    statsCtx.beginPath();
    for (let i = 0; i < GRAPH_POINTS; i++) {
      const x = (w / (GRAPH_POINTS - 1)) * i;
      const y = h - (memHistory[i] / 100) * h;
      if (i === 0) statsCtx.moveTo(x, y);
      else statsCtx.lineTo(x, y);
    }
    statsCtx.stroke();
    
    // Fill Memory area (semi-transparent)
    if (memColor.startsWith('#')) {
      const r = parseInt(memColor.slice(1, 3), 16);
      const g = parseInt(memColor.slice(3, 5), 16);
      const b = parseInt(memColor.slice(5, 7), 16);
      statsCtx.fillStyle = `rgba(${r},${g},${b},0.15)`;
    }
    statsCtx.beginPath();
    statsCtx.moveTo(0, h);
    for (let i = 0; i < GRAPH_POINTS; i++) {
      const x = (w / (GRAPH_POINTS - 1)) * i;
      const y = h - (memHistory[i] / 100) * h;
      statsCtx.lineTo(x, y);
    }
    statsCtx.lineTo(w, h);
    statsCtx.closePath();
    statsCtx.fill();
  }
  
  window.addEventListener('resize', () => {
    width = window.innerWidth;
    height = window.innerHeight;
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
    renderer.setSize(width, height);
    
    // Reinitialize stats graph canvas on resize
    statsCanvas = null;
    statsCtx = null;
    initStatsGraph();
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
      /* Header styles */
      .title .dot { color: ${scheme.accentBright}; }
      .status-dot { background: ${scheme.accent}; }
      .status-dot.online { background: ${scheme.statusOnline}; }
      .header-center { color: ${scheme.foregroundMuted}; }
      .header-center .val { color: ${scheme.foregroundBright}; }
      .header-btn { 
        background: ${currentTheme === 'light' ? 'rgba(0,0,0,0.08)' : 'rgba(255,255,255,0.08)'};
        border-color: ${currentTheme === 'light' ? 'rgba(0,0,0,0.15)' : 'rgba(255,255,255,0.15)'};
        color: ${scheme.foregroundBright};
      }
      .header-btn:hover { border-color: ${scheme.accentBright}; }
      
      /* Center panel styles */
      .center-panel {
        background: ${currentTheme === 'light' ? 'rgba(252,247,197,0.8)' : 'rgba(24,19,24,0.7)'};
        border-color: ${currentTheme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.08)'};
      }
      .process-counter .count { color: ${scheme.foregroundBright}; }
      .process-counter .label { color: ${scheme.foregroundMuted}; }
      
      /* Status bar styles */
      .status-bar { color: ${scheme.foregroundMuted}; }
      .dev-badge { 
        background: ${scheme.accentBright}; 
        color: ${currentTheme === 'light' ? '#fff' : '#000'}; 
      }
      
      /* Label styles - no background, stronger text shadow */
      .proc-label { 
        text-shadow: 0 0 6px ${scheme.background}, 0 0 10px ${scheme.background}, 0 0 14px ${scheme.background}; 
        background: transparent;
      }
      .proc-label .info { color: ${scheme.foregroundMuted}; }
      
      /* Tour UI styles */
      #tour-ui { background: ${scheme.ui.overlay}; border-color: ${scheme.foregroundMuted}; }
    `;
  }
  
  // Apply initial theme styles
  updateThemeStyles();
  
  // Expose for external use (mock data injection, etc.)
  window.ProcessTreeViz = {
    updateViz,
    scene,
    camera,
    renderer,
    controls,
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
  
  // Add tour button to #header-right (new structure) or .header-right (old structure)
  const headerRight = document.getElementById('header-right') || document.querySelector('.header-right');
  if (headerRight) {
    const tourBtn = document.createElement('button');
    tourBtn.id = 'tour-btn';
    tourBtn.className = 'hdr-btn';
    tourBtn.textContent = '🎬';
    tourBtn.title = 'Tour Mode';
    tourBtn.onclick = () => { 
      const isSourceTour = window.ASTTreeViz?.getTab() === 'sources' && window.ASTTreeViz?.isTourMode();
      if (!tourMode && !isSourceTour) startTour(); 
      else exitTour(); 
    };
    headerRight.insertBefore(tourBtn, headerRight.firstChild);
  }
  
  animate();
  connectWS();
})();