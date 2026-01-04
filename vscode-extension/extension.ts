// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live coding aesthetic.computer pieces and
// exploring the system documentation.

/* #region TODO 📓 
#endregion */

// Import necessary modules from vscode
import * as vscode from "vscode";

// Dynamically import path and fs to ensure web compatibility.
let path: any, fs: any;
(async () => {
  if (typeof window === "undefined") {
    path = await import("path");
    fs = await import("fs");
  }
})();

import { AestheticAuthenticationProvider } from "./aestheticAuthenticationProviderRemote";
const { keys } = Object;

let local: boolean = false;
let localServerAvailable: boolean = false;
let codeChannel: string | undefined;

// Detect if we're in GitHub Codespaces
const isCodespaces = process?.env.CODESPACES === "true";
const codespaceName = process?.env.CODESPACE_NAME;
const codespacesDomain = process?.env.GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN;

let mergedDocs: any = {};
let docs: any;

let extContext: any;
let webWindow: any;
let kidlispWindow: any;
let welcomePanel: vscode.WebviewPanel | null = null;
let localServerCheckInterval: NodeJS.Timeout | undefined;
let provider: AestheticViewProvider;

// Check if the local server is available
async function checkLocalServer(): Promise<boolean> {
  try {
    // Use https module directly to allow self-signed certificates
    const https = await import("https");
    return new Promise((resolve) => {
      const req = https.request(
        {
          hostname: "localhost",
          port: 8888,
          path: "/",
          method: "HEAD",
          rejectUnauthorized: false, // Allow self-signed certs
          timeout: 2000,
        },
        (res) => {
          resolve(res.statusCode === 200 || res.statusCode === 304);
        }
      );
      req.on("error", () => resolve(false));
      req.on("timeout", () => {
        req.destroy();
        resolve(false);
      });
      req.end();
    });
  } catch (e) {
    return false;
  }
}

// Start polling for local server availability
function startLocalServerCheck() {
  if (localServerCheckInterval) {
    clearInterval(localServerCheckInterval);
  }
  
  // Check immediately
  checkLocalServer().then((available) => {
    const wasAvailable = localServerAvailable;
    localServerAvailable = available;
    if (available && !wasAvailable) {
      console.log("✅ Local server is now available");
      // Refresh webviews when server becomes available
      if (provider) provider.refreshWebview();
      refreshWebWindow();
      refreshKidLispWindow();
    }
  });
  
  // Then check every 3 seconds
  localServerCheckInterval = setInterval(async () => {
    const wasAvailable = localServerAvailable;
    localServerAvailable = await checkLocalServer();
    
    if (localServerAvailable && !wasAvailable) {
      console.log("✅ Local server is now available");
      // Refresh webviews when server becomes available
      if (provider) provider.refreshWebview();
      refreshWebWindow();
      refreshKidLispWindow();
    } else if (!localServerAvailable && wasAvailable) {
      console.log("❌ Local server disconnected");
      // Optionally refresh to show waiting state
      if (provider) provider.refreshWebview();
      refreshWebWindow();
      refreshKidLispWindow();
    }
  }, 3000);
}

// Stop polling for local server
function stopLocalServerCheck() {
  if (localServerCheckInterval) {
    clearInterval(localServerCheckInterval);
    localServerCheckInterval = undefined;
  }
}

async function activate(context: vscode.ExtensionContext): Promise<void> {
  // local = context.globalState.get("aesthetic:local", false); // Retrieve env.

  // Show all environment variables...
  // console.log("🌎 Environment:", process.env);
  // Always retrieve the stored local state first
  local = context.globalState.get("aesthetic:local", false);
  
  const isInDevContainer = process?.env.REMOTE_CONTAINERS === "true";
  if (isInDevContainer) {
    // console.log("✅ 🥡 Running inside a container.");
    // Keep the retrieved local state
  } else {
    // console.log("❌ 🥡 Not in container.");
    // Keep the user's preference even when not in container
  }

  // Start checking for local server if local mode is enabled
  if (local) {
    startLocalServerCheck();
  }

  // console.log("🟢 Aesthetic Computer Extension: Activated");
  extContext = context;

  const savedGoal = context.globalState.get("goalState");

  // Load the docs from the web.
  if (!docs) {
    try {
      // const url = `https://${ // local ? "localhost:8888" : "aesthetic.computer" }/api/docs`;
      const url = `https://aesthetic.computer/docs.json`;
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data: any = await response.json();
      // console.log("📚 Docs loaded:", data);

      keys(data.api).forEach((key) => {
        // Add the category to each doc before smushing them.
        // Such as `structure` so doc pages can be found like `structure:boot`.
        keys(data.api[key]).forEach((k) => {
          data.api[key][k].category = key;
        });
        // 😛 Smush them.
        mergedDocs = {
          ...mergedDocs,
          ...data.api[key],
        };
      });

      docs = data;
    } catch (error) {
      console.error("Failed to fetch documentation:", error);
    }
  }

  // Set up all the autocompletion and doc hints.
  const codeLensProvider = new AestheticCodeLensProvider();
  vscode.languages.registerCodeLensProvider(
    { language: "javascript", pattern: "**/*.mjs" },
    codeLensProvider,
  );

  const completionProvider = vscode.languages.registerCompletionItemProvider(
    { language: "javascript", pattern: "**/*.mjs" },
    {
      provideCompletionItems(
        document: vscode.TextDocument,
        position: vscode.Position,
      ): vscode.ProviderResult<
        vscode.CompletionItem[] | vscode.CompletionList
      > {
        if (document.lineCount > 500 || !docs) return []; // Skip for long files.
        return keys(mergedDocs).map(
          (word: string) => new vscode.CompletionItem(word),
        );
      },
    },
  );

  context.subscriptions.push(completionProvider);

  const hoverProvider = vscode.languages.registerHoverProvider(
    { language: "javascript", pattern: "**/*.mjs" },
    {
      provideHover(document, position) {
        if (document.lineCount > 500 || !docs) return; // Skip for long files.

        const range = document.getWordRangeAtPosition(position);
        const word = document.getText(range);

        if (keys(mergedDocs).indexOf(word) > -1) {
          const contents = new vscode.MarkdownString();
          contents.isTrusted = true; // Enable for custom markdown.
          contents.appendCodeblock(`${mergedDocs[word].sig}`, "javascript");
          contents.appendText("\n\n");
          contents.appendMarkdown(`${mergedDocs[word].desc}`);
          return new vscode.Hover(contents, range);
        }
      },
    },
  );

  context.subscriptions.push(hoverProvider);

  // Code Action Provider for manual documentation opening
  const codeActionProvider = vscode.languages.registerCodeActionsProvider(
    { language: "javascript", pattern: "**/*.mjs" },
    {
      provideCodeActions(
        document: vscode.TextDocument,
        range: vscode.Range | vscode.Selection,
        context: vscode.CodeActionContext,
        token: vscode.CancellationToken
      ): vscode.ProviderResult<(vscode.CodeAction | vscode.Command)[]> {
        if (document.lineCount > 500 || !docs) return []; // Skip for long files.

        const wordRange = document.getWordRangeAtPosition(range.start);
        if (!wordRange) return [];
        
        const word = document.getText(wordRange);
        
        if (keys(mergedDocs).indexOf(word) > -1) {
          const action = new vscode.CodeAction(
            `📚 Open documentation for "${word}"`,
            vscode.CodeActionKind.QuickFix
          );
          action.command = {
            title: `Open docs for ${word}`,
            command: "aestheticComputer.openDoc",
            arguments: [word]
          };
          action.isPreferred = true;
          return [action];
        }
        
        return [];
      }
    }
  );

  context.subscriptions.push(codeActionProvider);
  const definitionProvider = vscode.languages.registerDefinitionProvider(
    { language: "javascript", pattern: "**/*.mjs" },
    {
      provideDefinition(
        document,
        position,
        token,
      ): vscode.ProviderResult<vscode.Definition | vscode.DefinitionLink[]> {
        if (document.lineCount > 500) return; // Skip for large documents.

        const range = document.getWordRangeAtPosition(position);
        const word = document.getText(range);

        if (mergedDocs[word]) {
          // Instead of automatically opening docs, create a virtual definition location
          // that shows in the peek definition view without triggering a popup
          const uri = vscode.Uri.parse(`aesthetic-doc:${word}`);
          return new vscode.Location(uri, new vscode.Position(0, 0));
        }

        return null;
      },
    },
  );

  let docsPanel: any = null; // Only keep one docs panel open.

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "aestheticComputer.openDoc",
      (functionName) => {
        // const uri = vscode.Uri.parse(`${docScheme}:${functionName}`);

        let path = "";
        if (functionName)
          path = "/" + mergedDocs[functionName].category + ":" + functionName;

        const title = path || "docs";

        // Check if the panel already exists. If so, reveal it.
        if (docsPanel) {
          // Update the title
          docsPanel.title =
            "📚 " + title.replace("/", "") + " · Aesthetic Computer";
          docsPanel.reveal(vscode.ViewColumn.Beside);
        } else {
          // Create and show a new webview
          docsPanel = vscode.window.createWebviewPanel(
            "aestheticDoc", // Identifies the type of the webview. Used internally
            "📚 " + title.replace("/", "") + " · Aesthetic Computer", // Title of the panel displayed to the user
            vscode.ViewColumn.Beside, // Editor column to show the new webview panel in.
            { enableScripts: true, enableForms: true }, // Webview options.
          );

          // Reset when the current panel is closed
          docsPanel.onDidDispose(() => {
            docsPanel = null;
          }, null);
        }

        const nonce = getNonce();

        // Build CSP frame-src based on environment
        let cspFrameSrc = "frame-src https://aesthetic.computer https://aesthetic.local:8888 https://localhost:8888";
        let cspChildSrc = "child-src https://aesthetic.computer https://aesthetic.local:8888 https://localhost:8888";
        
        if (isCodespaces && codespacesDomain) {
          const codespaceWildcard = `https://*.${codespacesDomain}`;
          cspFrameSrc += ` ${codespaceWildcard}`;
          cspChildSrc += ` ${codespaceWildcard}`;
        }

        // And set its HTML content
        docsPanel.webview.html = `
        <!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta http-equiv="Permissions-Policy" content="midi=*">
          <meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src 'nonce-${nonce}'; script-src 'nonce-${nonce}'; img-src https: data:;">
          <style nonce="${nonce}">
            body {
              margin: 0;
              padding: 0;
              overflow: hidden;"
            }
            iframe {
              border: none;
              width: 100vw;
              height: 100vh;
            }
          </style>
        </head>
        <body>
          <iframe allow="clipboard-write; clipboard-read" credentialless sandbox="allow-scripts allow-modals allow-popups allow-popups-to-escape-sandbox" src="https://aesthetic.computer/docs${path}">
        </body>
        </html>
      `.trim();
      },
    ),
  );

  // ✨ Welcome Panel (Three.js 3D Architecture)
  function showWelcomePanel() {
    if (welcomePanel) {
      welcomePanel.reveal(vscode.ViewColumn.One);
      return;
    }

    welcomePanel = vscode.window.createWebviewPanel(
      "aestheticWelcome",
      "✦ Aesthetic Computer",
      vscode.ViewColumn.One,
      { 
        enableScripts: true,
        localResourceRoots: [vscode.Uri.joinPath(context.extensionUri, "resources")]
      }
    );

    // Handle messages from the webview
    welcomePanel.webview.onDidReceiveMessage(
      message => {
        console.log("Welcome panel received message:", message);
        switch (message.command) {
          case 'openKidLisp':
            vscode.commands.executeCommand('aestheticComputer.openKidLispWindow');
            return;
          case 'openPane':
            vscode.commands.executeCommand('aestheticComputer.openWindow');
            return;
          case 'newPiece':
            vscode.commands.executeCommand('workbench.action.files.newUntitledFile');
            return;
        }
      },
      undefined,
      context.subscriptions
    );

    welcomePanel.onDidDispose(() => {
      welcomePanel = null;
    });

    const nonce = getNonce();
    
    // Get proper URI for the logo
    const palsUri = welcomePanel.webview.asWebviewUri(
      vscode.Uri.joinPath(context.extensionUri, "resources", "purple-pals.svg")
    );

    welcomePanel.webview.html = `
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; img-src ${welcomePanel.webview.cspSource} https: data:; script-src 'unsafe-inline' https://cdnjs.cloudflare.com https://cdn.jsdelivr.net; connect-src ws://127.0.0.1:7890 wss://localhost:8889;">
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
          .actions {
            pointer-events: auto;
            display: flex;
            gap: 8px;
            margin-top: 16px;
            justify-content: center;
          }
          .action-btn {
            background: rgba(255,255,255,0.08);
            border: 1px solid rgba(255,255,255,0.15);
            border-radius: 6px;
            padding: 8px 14px;
            font-size: 12px;
            color: #fff;
            cursor: pointer;
            transition: all 0.15s ease;
            font-family: inherit;
          }
          .action-btn:hover {
            background: rgba(255,255,255,0.15);
            border-color: #ff69b4;
          }
          .action-btn.primary {
            background: #ff69b4;
            border-color: #ff69b4;
            color: #000;
          }
          .action-btn.primary:hover {
            background: #ff8ab3;
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
          <div class="actions">
            <button class="action-btn primary" id="btn-kidlisp">🎨 KidLisp</button>
            <button class="action-btn" id="btn-pane">💻 AC Pane</button>
          </div>
        </div>
        
        <div class="hud mem">
          <span id="mem-text">— / —</span> MB
        </div>
        
        <div id="labels" class="label-container"></div>
        
        <script>
          const vscode = acquireVsCodeApi();
          
          // Button handlers
          document.getElementById('btn-kidlisp').addEventListener('click', () => {
            vscode.postMessage({command: 'openKidLisp'});
          });
          document.getElementById('btn-pane').addEventListener('click', () => {
            vscode.postMessage({command: 'openPane'});
          });
          
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
          
          animate();
          connectWS();
        </script>
      </body>
      </html>
    `.trim();
  }

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.welcome", showWelcomePanel)
  );

  // Show welcome on startup if no editors are open
  if (vscode.window.visibleTextEditors.length === 0) {
    // Delay slightly to let VS Code finish loading
    setTimeout(() => {
      if (vscode.window.visibleTextEditors.length === 0 && !welcomePanel) {
        showWelcomePanel();
      }
    }, 500);
  }

  // 🚩 Goal
  let statusBarGoal: vscode.StatusBarItem;
  let goalLocation: vscode.Range;
  let goalFilePath: string; // Store the file path for use in the jump command

  function updateStatusBarItem(text: string, filename: string, line: number) {
    if (!statusBarGoal) {
      statusBarGoal = vscode.window.createStatusBarItem(
        vscode.StatusBarAlignment.Left,
        100,
      );
      statusBarGoal.command = "aestheticComputer.visitGoal";
      statusBarGoal.color = new vscode.ThemeColor(
        "statusBarItem.prominentForeground",
      );
      statusBarGoal.show();
      context.subscriptions.push(statusBarGoal);
    }

    statusBarGoal.text = `$(output) Aesthetic ${text} ${filename}:${line}`;
    statusBarGoal.tooltip = text;
  }

  // Save state function (add this inside your setGoal command)
  function saveGoalState(
    text: string,
    filePath: string,
    range: vscode.Range,
    filename: string,
  ) {
    const goalState = {
      text: text,
      filePath: filePath,
      filename: filename,
      lineStart: range.start.line,
      characterStart: range.start.character,
      lineEnd: range.end.line,
      characterEnd: range.end.character,
    };
    context.globalState.update("goalState", goalState);
  }

  function processGoalText(text: string) {
    const pattern = /-\s*\[(.*?)\]\s*/g;
    const match = pattern.exec(text);
    const replacement = match && match[1] ? match[1] + " " : "🚩 ";
    return replacement + text.replace(pattern, "").trim();
  }

  function restoreGoal(savedGoal: any) {
    goalFilePath = savedGoal.filePath;
    goalLocation = new vscode.Range(
      new vscode.Position(savedGoal.lineStart, savedGoal.characterStart),
      new vscode.Position(savedGoal.lineEnd, savedGoal.characterEnd),
    );

    updateStatusBarItem(
      savedGoal.text,
      savedGoal.filename,
      savedGoal.lineStart + 1,
    );
  }

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.setGoal", () => {
      const editor = vscode.window.activeTextEditor;
      if (editor) {
        const cursorPosition = editor.selection.active;
        const line = editor.document.lineAt(cursorPosition.line);
        const text = processGoalText(line.text);
        goalLocation = line.range;
        goalFilePath = editor.document.uri.fsPath;
        const filename = editor.document.fileName.split("/").pop() as string;

        updateStatusBarItem(text, filename, cursorPosition.line + 1);
        saveGoalState(text, goalFilePath, goalLocation, filename);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.visitGoal", () => {
      if (goalFilePath && goalLocation) {
        // Search among all editors, not just visible ones
        let targetEditor = vscode.window.visibleTextEditors.find(
          (editor) => editor.document.uri.fsPath === goalFilePath,
        );

        const openDocument = (document: any) => {
          vscode.window
            .showTextDocument(document, {
              preview: false,
              viewColumn: targetEditor?.viewColumn, // Use existing view column if available
              selection: goalLocation,
            })
            .then((editor) => {
              editor.revealRange(
                goalLocation,
                vscode.TextEditorRevealType.InCenter,
              );
            });
        };

        if (targetEditor) {
          openDocument(targetEditor.document);
        } else {
          vscode.workspace.openTextDocument(goalFilePath).then(openDocument);
        }
      }
    }),
  );

  // Listen for text document changes
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((event) => {
      if (
        goalFilePath &&
        goalLocation &&
        event.document.uri.fsPath === goalFilePath
      ) {
        updateStatusBasedOnLineChange(event);
      }
    }),
  );

  function updateStatusBasedOnLineChange(
    event: vscode.TextDocumentChangeEvent,
  ) {
    if (!goalLocation) return;

    // Check if the change affects the line of interest
    const lineOfInterest = goalLocation.start.line;
    for (const change of event.contentChanges) {
      if (
        change.range.start.line <= lineOfInterest &&
        change.range.end.line >= lineOfInterest
      ) {
        // The line of interest has been changed, update the status bar
        const text = processGoalText(
          event.document.lineAt(lineOfInterest).text,
        );
        const filename = event.document.fileName.split("/").pop() as string;
        updateStatusBarItem(text, filename, lineOfInterest + 1);
        saveGoalState(text, goalFilePath, goalLocation, filename);
        break;
      }
    }
  }

  if (savedGoal) restoreGoal(savedGoal);

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.openWindow", () => {
      const panel = vscode.window.createWebviewPanel(
        "webView", // Identifies the type of the webview. Used internally
        "Aesthetic", // Title of the panel displayed to the user
        vscode.ViewColumn.One, // Editor column to show the new webview panel in.
        {
          enableScripts: true,
          enableForms: true,
          localResourceRoots: [extContext.extensionUri],
        }, // Webview options.
      );

      panel.title = "Aesthetic" + (local ? ": 🧑‍🤝‍🧑" : ""); // Update the title if local.
      panel.webview.html = getWebViewContent(panel.webview, "");
      webWindow = panel;

      panel.onDidDispose(
        () => {
          webWindow = null;
        },
        null,
        context.subscriptions,
      );
    }),
  );

  // 🌈 KidLisp.com Window
  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.openKidLispWindow", () => {
      // If window already exists, reveal it instead of creating a new one
      if (kidlispWindow) {
        kidlispWindow.reveal(vscode.ViewColumn.One);
        return;
      }
      
      const panel = vscode.window.createWebviewPanel(
        "kidlispWebView", // Identifies the type of the webview
        "KidLisp.com", // Title of the panel displayed to the user
        vscode.ViewColumn.One, // Editor column to show the new webview panel in
        {
          enableScripts: true,
          enableForms: true,
          localResourceRoots: [extContext.extensionUri],
        },
      );

      panel.title = "KidLisp.com" + (local ? " 🧑‍🤝‍🧑" : "");
      panel.webview.html = getKidLispWebViewContent(panel.webview);
      kidlispWindow = panel;

      // Handle messages from the KidLisp webview
      panel.webview.onDidReceiveMessage(
        async (message) => {
          switch (message.type) {
            case "vscode-extension:closeAllEditors":
              await vscode.commands.executeCommand("workbench.action.closeAllEditors");
              break;
            case "vscode-extension:reload":
              vscode.commands.executeCommand("workbench.action.reloadWindow");
              break;
          }
        },
        undefined,
        context.subscriptions,
      );

      panel.onDidDispose(
        () => {
          kidlispWindow = null;
        },
        null,
        context.subscriptions,
      );
    }),
  );

  // Add definitionProvider to context.subscriptions if necessary
  context.subscriptions.push(definitionProvider);

  // 🗝️ Authorization
  const ap = new AestheticAuthenticationProvider(context, local, "aesthetic");
  const sp = new AestheticAuthenticationProvider(context, local, "sotce");

  context.subscriptions.push(ap);
  context.subscriptions.push(sp);

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.logIn", async () => {
      const session = await vscode.authentication.getSession(
        "aesthetic",
        ["profile"],
        { createIfNone: true },
      );
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.logOut", async () => {
      const session = await vscode.authentication.getSession(
        "aesthetic",
        ["profile"],
        { silent: true },
      );
      if (session) {
        await ap.removeSession(session.id);
        vscode.window.showInformationMessage("🟪 You have been logged out.");
      } else {
        vscode.window.showInformationMessage("No active session found.");
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "aestheticComputer.sotceLogIn",
      async () => {
        const session = await vscode.authentication.getSession(
          "sotce",
          ["profile"],
          { createIfNone: true },
        );
      },
    ),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "aestheticComputer.sotceLogOut",
      async () => {
        const session = await vscode.authentication.getSession(
          "sotce",
          ["profile"],
          { silent: true },
        );
        if (session) {
          await sp.removeSession(session.id);
          vscode.window.showInformationMessage("🪷 You have been logged out.");
        } else {
          vscode.window.showInformationMessage("No active session found.");
        }
      },
    ),
  );

  const getSession = async (tenant: string) => {
    const session = await vscode.authentication.getSession(
      tenant,
      ["profile"],
      { silent: true },
    );

    if (session) {
      vscode.window.showInformationMessage(
        `👋 Welcome back to ${
          tenant === "aesthetic" ? "Aesthetic Computer" : "Sotce Net"
        }! (${session.account.label})`,
      );
      context.globalState.update(`${tenant}:session`, session);
    } else {
      context.globalState.update(`${tenant}:session`, undefined);
      // console.log("😀 Erased session!");
    }

    return session;
  };

  context.subscriptions.push(
    vscode.authentication.onDidChangeSessions(async (e) => {
      // console.log("🏃 Sessions changed:", e);
      if (e.provider.id === "aesthetic" || e.provider.id === "sotce") {
        await getSession(e.provider.id);
        provider.refreshWebview();
        refreshWebWindow();
      }
    }),
  );

  // GUI

  provider = new AestheticViewProvider();
  
  // Connect to session server for jump commands
  provider.connectToSessionServer();

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      AestheticViewProvider.viewType,
      provider,
    ),
  );

  // 🧩 Piece Running

  // Send piece code through the code channel.
  function upload() {
    let editor = vscode.window.activeTextEditor;
    if (!editor) {
      return;
    }

    if (local) {
      // console.log("😊 Skipping `/run` api endpoint. (In local mode.)");
      return;
    }

    let source = editor.document.getText();
    const piece = editor.document.fileName
      .split(/\/|\\/) // Split on both forward slash and backslash
      .slice(-1)[0]
      .replace(".mjs", "");

    // 📓 The `local` won't work due to VSCode's Proxy, but the option
    // is here just in case it's ever possible again.
    const host = local === false ? "aesthetic.computer" : "localhost:8888";

    let url = `https://${host}/run`;

    vscode.window.showInformationMessage(`🧩 ${piece}`);

    fetch(url, {
      method: "POST",
      body: JSON.stringify({ piece, source, codeChannel }),
      headers: { "Content-Type": "application/json" },
    })
      .then((res) => res.text()) // Convert the response to text
      .then((text) => {
        // Now 'text' is a string that can be used in showInformationMessage
        // vscode.window.showInformationMessage(`🧩 \`${piece}\``);
      })
      .catch((error) => {
        // If you catch an error, make sure to convert it to a string if it isn't already
        console.error(error);
        vscode.window.showInformationMessage("🔴" + "Piece error.");
      });
  }

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.runPiece", () => {
      upload();
    }),
    vscode.commands.registerCommand("aestheticComputer.localServer", () => {
      local = !local;
      context.globalState.update("aesthetic:local", local);
      
      // Start or stop local server checking
      if (local) {
        localServerAvailable = false; // Reset until we confirm
        startLocalServerCheck();
      } else {
        stopLocalServerCheck();
        localServerAvailable = false;
      }
      
      // Refresh the webview with the new local state
      provider.refreshWebview();
      refreshWebWindow();
      refreshKidLispWindow();
      vscode.window.showInformationMessage(
        `💻 Local Development: ${local ? "Enabled" : "Disabled"}`,
      );
    }),
    vscode.commands.registerCommand("aestheticComputer.clearSlug", () => {
      // Clear the stored slug data
      context.globalState.update("panel:slug", "");
      // Refresh the webview to reflect the cleared state
      provider.refreshWebview();
      refreshWebWindow();
      vscode.window.showInformationMessage(
        "🧹 Slug data cleared successfully!",
      );
    }),
    vscode.commands.registerCommand("aestheticComputer.closeAllEditors", async () => {
      await vscode.commands.executeCommand("workbench.action.closeAllEditors");
    }),
  );

  // Automatically re-run the piece when saving any .mjs file.
  vscode.workspace.onDidSaveTextDocument((document) => {
    function mjsOrLisp(path: string) {
      return path.endsWith(".mjs") || path.endsWith(".lisp");
    }

    if (vscode.window.activeTextEditor?.document === document) {
      // console.log("🔩 File path:", document.uri.fsPath);
      const inMonoRepo =
        document.uri.fsPath.indexOf("aesthetic-computer/system") > -1;
      const inDisks =
        document.uri.fsPath.indexOf(
          "aesthetic-computer/system/public/aesthetic.computer/disks",
        ) > -1;

      if (inMonoRepo) {
        if (inDisks && mjsOrLisp(document.uri.fsPath)) {
          // console.log("🟡 Loading piece...", document.uri.fsPath);
          vscode.commands.executeCommand("aestheticComputer.runPiece");
        }
      } else if (mjsOrLisp(document.uri.fsPath)) {
        // console.log("🟡 Loading piece...", document.uri.fsPath);
        vscode.commands.executeCommand("aestheticComputer.runPiece");
      }
    }
  });
}

// 📓 Documentation

// This is just for top-level functions and maybe something at the very top?
class AestheticCodeLensProvider implements vscode.CodeLensProvider {
  provideCodeLenses(
    document: vscode.TextDocument,
    // token: vscode.CancellationToken,
  ): vscode.CodeLens[] {
    let codeLenses: vscode.CodeLens[] = [];

    function escapeRegExp(word: string) {
      return word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    }

    if (document.lineCount > 500 || !docs) return codeLenses; // Don't compute for large documents.

    const escapedWords = keys(docs.api.structure)
      .map((word) => "function " + word)
      .map(escapeRegExp);
    const regex = new RegExp(`\\b(${escapedWords.join("|")})\\b`, "gi");

    for (let i = 0; i < document.lineCount; i++) {
      const line = document.lineAt(i);
      let matches;
      while ((matches = regex.exec(line.text)) !== null) {
        const word = matches[0];
        const range = new vscode.Range(
          i,
          matches.index,
          i,
          matches.index + word.length,
        );
        const docKey = word.toLowerCase().replace("function ", "");

        const command = {
          title: docs.api.structure[docKey].label,
          command: "aestheticComputer.openDoc",
          arguments: [docKey],
        };
        codeLenses.push(new vscode.CodeLens(range, command));
      }
    }

    return codeLenses;
  }
}

// 🪟 Panel Rendering
class AestheticViewProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = "aestheticComputer.sidebarView";
  private _view?: vscode.WebviewView;
  private ws?: any;

  constructor() {}

  // Method to send message to the webview
  public sendMessageToWebview(message: any) {
    if (this._view && this._view.webview) {
      this._view.webview.postMessage(message);
    }
  }

  // Connect to session server as WebSocket client
  public connectToSessionServer() {
    const WebSocket = require('ws');
    const url = local ? "wss://localhost:8889" : "wss://aesthetic.computer";
    
    this.ws = new WebSocket(url, {
      rejectUnauthorized: false
    });
    
    this.ws.on("open", () => {
      console.log("✅ Connected to session server");
      // Identify as VSCode extension
      this.ws.send(JSON.stringify({
        type: "identify",
        content: { type: "vscode" }
      }));
    });
    
    this.ws.on("message", (data: any) => {
      try {
        const msg = JSON.parse(data.toString());
        
        if (msg.type === "vscode:jump" && msg.content?.piece) {
          console.log("🎯 Jump command received:", msg.content.piece);
          this.handleJump(msg.content.piece);
        }
      } catch (err) {
        console.error("Failed to parse message:", err);
      }
    });
    
    this.ws.on("close", () => {
      console.log("❌ Disconnected from session server");
      // Reconnect after 5 seconds
      setTimeout(() => this.connectToSessionServer(), 5000);
    });
    
    this.ws.on("error", (err: any) => {
      console.error("WebSocket error:", err.message);
    });
  }

  // Handle jump command by updating slug and showing panel
  private async handleJump(piece: string) {
    // Update the stored slug BEFORE showing the panel
    // This ensures the panel loads with the correct URL immediately
    await extContext.globalState.update("panel:slug", piece);
    
    // If panel is hidden, show it (it will load with the new slug)
    if (!this._view?.visible) {
      await vscode.commands.executeCommand(
        "aestheticComputer.sidebarView.focus"
      );
      // The panel will initialize with the updated slug from globalState
    } else {
      // Panel is visible, send message to navigate
      this.sendMessageToWebview({ type: "jump", piece });
    }
  }

  public refreshWebview(): void {
    if (this._view) {
      const slug = extContext.globalState.get("panel:slug", "");
      // if (slug) console.log("🪱 Loading slug:", slug);
      this._view.title = slug + (local ? " 🧑‍🤝‍🧑" : "");
      this._view.webview.html = getWebViewContent(this._view.webview, slug);
    }
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext<unknown>,
    _token: vscode.CancellationToken,
  ): void {
    this._view = webviewView;

    const slug = extContext.globalState.get("panel:slug", "");
    // if (slug) console.log("🪱 Loading slug:", slug);

    this._view.title = slug + (local ? " 🧑‍🤝‍🧑" : "");

    // Set retainContextWhenHidden to true
    this._view.webview.options = {
      enableScripts: true,
      enableForms: true,
      localResourceRoots: [extContext.extensionUri],
    };

    webviewView.webview.html = getWebViewContent(this._view.webview, slug);

    webviewView.webview.onDidReceiveMessage(async (data) => {
      switch (data.type) {
        case "url:updated": {
          // console.log("😫 Slug updated...", data.slug);
          extContext.globalState.update("panel:slug", data.slug);
          webviewView.title = data.slug + (local ? " 🧑‍🤝‍🧑" : "");
          break;
        }
        case "clipboard:copy": {
          vscode.env.clipboard.writeText(data.value).then(() => {
            // console.log("📋 Copied text to clipboard!");
            webviewView.webview.postMessage({
              type: "clipboard:copy:confirmation",
            });
          });
          break;
        }
        case "publish":
          if (data.url) vscode.env.openExternal(vscode.Uri.parse(data.url));
          break;
        case "setCode":
          codeChannel = data.value;
          // const currentTitle = webviewView.title;
          // webviewView.title = currentTitle?.split(" · ")[0] + " · " + codeChannel;
          // ^ Disabled because it's always rendered uppercase. 24.01.27.17.26
          break;
        case "vscode-extension:reload": {
          vscode.commands.executeCommand("workbench.action.reloadWindow");
          break;
        }
        case "vscode-extension:closeAllEditors": {
          await vscode.commands.executeCommand("workbench.action.closeAllEditors");
          break;
        }
        case "vscode-extension:defocus": {
          const editor = vscode.window.activeTextEditor;
          if (editor) {
            await vscode.window.showTextDocument(
              editor.document,
              editor.viewColumn,
            );
          }
          break;
          break;
        }
        case "openDocs": {
          // console.log("🏃 Opening docs...");
          vscode.commands.executeCommand("aestheticComputer.openDoc");
          break;
        }
        case "openSource": {
          // console.log("📃 Opening a new source file...", data);
          // const tempUri = document.uri.with({ path: document.uri.path + '.mjs' });
          vscode.workspace
            .openTextDocument({
              content: data.source,
              // language: "javascript",
            })
            .then((document) => {
              vscode.window
                .showTextDocument(document, { preview: false })
                .then(() => {
                  return vscode.window.showInformationMessage(
                    "💾 Save this code with an `.mjs` extension to run it on Aesthetic Computer",
                    { modal: true },
                  );
                })
                .then(() => {
                  if (fs && path) {
                    const defaultUri = vscode.Uri.file(
                      path.join(vscode.workspace.rootPath || "", data.title),
                    );
                    return vscode.window.showSaveDialog({
                      filters: {
                        "JavaScript Module": ["mjs"],
                      },
                      defaultUri: defaultUri,
                      saveLabel: "Save As",
                    });
                  } else {
                    return;
                  }
                })
                .then((fileUri) => {
                  if (fileUri) {
                    // Read the content of the current document
                    const content = document.getText();

                    // Write the content to the new file
                    fs.writeFile(fileUri.fsPath, content, (err: any) => {
                      if (err) {
                        vscode.window.showErrorMessage(
                          "Failed to save file: " + err.message,
                        );
                        // return;
                        return Promise.resolve(null);
                      }

                      // Close the current editor
                      vscode.commands
                        .executeCommand(
                          "workbench.action.revertAndCloseActiveEditor",
                        )
                        .then(() => {
                          // Open the saved file in the editor
                          vscode.workspace
                            .openTextDocument(fileUri)
                            .then((doc) => {
                              vscode.window.showTextDocument(doc, {
                                preview: false,
                              });
                              // vscode.window.showInformationMessage(
                              //   "File saved at: " + fileUri.fsPath,
                              // );
                            });
                        });
                    });
                  }
                });
            });
          break;
        }
        case "runPiece": {
          console.log("🏃 Running piece...");
          vscode.commands.executeCommand("aestheticComputer.runPiece");
          break;
        }
        case "login": {
          console.log("📂 Logging in...");
          const command = data.tenant === "sotce" ? "sotceLogIn" : "logIn";
          vscode.commands.executeCommand(`aestheticComputer.${command}`);
          break;
        }
        case "logout": {
          console.log("🚪 Logging out...");
          const command = data.tenant === "sotce" ? "sotceLogOut" : "logOut";
          vscode.commands.executeCommand(`aestheticComputer.${command}`);
          break;
        }
        case "openExternal": {
          console.log("🌐 Opening external URL:", data.url);
          if (data.url) {
            vscode.env.openExternal(vscode.Uri.parse(data.url));
          }
          break;
        }
      }
    });

    webviewView.onDidChangeVisibility(() => {
      if (!webviewView.visible) {
        // console.log("🔴 Panel hidden.");
        // Perform any cleanup or state update here when the view is hidden
        const slug = extContext.globalState.get("panel:slug", "");
        // if (slug) console.log("🪱 Slug:", slug);
        webviewView.title = slug + (local ? " 🧑‍🤝‍🧑" : "");
        webviewView.webview.html = getWebViewContent(webviewView.webview, slug);
      } else {
        // console.log("🟢 Panel open.");
        // Send focus event to webview so prompt can be activated
        webviewView.webview.postMessage({ type: "aesthetic-parent:focused" });
      }
    });
  }
}

// 📚 Library

function getNonce(): string {
  let text = "";
  const possible =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < 32; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  return text;
}

function refreshWebWindow() {
  if (webWindow) {
    const slug = extContext.globalState.get("panel:slug", "");
    // if (slug) console.log("🪱 Loading slug:", slug);

    webWindow.title = "Aesthetic: " + slug + (local ? " 🧑‍🤝‍🧑" : ""); // Update the title if local.

    webWindow.webview.html = getWebViewContent(webWindow.webview, slug);
  }
}

function refreshKidLispWindow() {
  if (kidlispWindow) {
    kidlispWindow.title = "KidLisp.com" + (local ? " 🧑‍🤝‍🧑" : "");
    kidlispWindow.webview.html = getKidLispWebViewContent(kidlispWindow.webview);
  }
}

function getWebViewContent(webview: any, slug: string) {
  const scriptUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "embedded.js"),
  );

  const nonce = getNonce();
  const styleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "main.css"),
  );

  const resetStyleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "reset.css"),
  );

  const vscodeStyleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "vscode.css"),
  );

  // Get proper URI for background image
  const purplePalsUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "resources", "purple-pals.svg"),
  );

  const sessionAesthetic = extContext.globalState.get(
    "aesthetic:session",
    undefined,
  );

  const sessionSotce = extContext.globalState.get("sotce:session", undefined);
  // console.log("🟪 Aesthetic:", sessionAesthetic, "🪷 Sotce:", sessionSotce);

  // console.log("🪱 Slug:", slug);
  let hashFragment = "";
  let pathPart = slug || "";

  // Encode # as %23 in kidlisp code to prevent URL fragment interpretation
  // This handles cases like "(stamp #wNb 0 0)" in the path
  if (pathPart && !pathPart.startsWith("#")) {
    // Only encode # if it's not already a hash fragment (painting code)
    pathPart = pathPart.replace(/#/g, "%23");
  }

  if (pathPart.startsWith("#")) {
    hashFragment = pathPart;
    pathPart = "";
  }

  const hasQuery = pathPart.includes("?");
  let param = pathPart;

  if (param) {
    param += hasQuery ? "&vscode=true" : "?vscode=true";
  } else {
    param = "?vscode=true";
  }

  [sessionAesthetic, sessionSotce].forEach((session, index) => {
    const paramBase = `&session-${index === 0 ? "aesthetic" : "sotce"}=`;

    if (typeof session === "object") {
      // Logged in.
      if (keys(session)?.length > 0) {
        const base64EncodedSession = btoa(JSON.stringify(session));
        param += paramBase + encodeURIComponent(base64EncodedSession);
      }
    } else {
      // Logged out.
      param += paramBase + "null";
    }
  });

  // param = "?clearSession=true"; Probably never needed.

  // Determine the iframe URL based on environment
  let iframeUrl;
  let iframeProtocol = "https://";
  if (isCodespaces && codespaceName && codespacesDomain) {
    // In Codespaces, always use the forwarded URL (without protocol, added later)
    iframeUrl = `${codespaceName}-8888.${codespacesDomain}`;
  } else {
    // On local laptop, use localhost if local mode is enabled
    iframeUrl = local ? "localhost:8888" : "aesthetic.computer";
  }

  // Build CSP frame-src and child-src based on environment
  let cspFrameSrc = "frame-src https://aesthetic.computer https://hi.aesthetic.computer https://aesthetic.local:8888 https://localhost:8888 https://sotce.net https://hi.sotce.net https://sotce.local:8888";
  let cspChildSrc = "child-src https://aesthetic.computer https://aesthetic.local:8888 https://sotce.net https://sotce.local:8888 https://localhost:8888";
  
  if (isCodespaces && codespacesDomain) {
    // Use wildcard for any codespace in this domain
    const codespaceWildcard = `https://*.${codespacesDomain}`;
    cspFrameSrc += ` ${codespaceWildcard}`;
    cspChildSrc += ` ${codespaceWildcard}`;
  }

  // Show waiting UI if local mode is enabled but server isn't available yet
  if (local && !localServerAvailable && !isCodespaces) {
    return `<!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link href="${styleUri}" rel="stylesheet">
        <link href="${resetStyleUri}" rel="stylesheet">
        <link href="${vscodeStyleUri}" rel="stylesheet">
        <title>aesthetic.computer</title>
        <style>
          body {
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            height: 100vh;
            margin: 0;
            background: #181318;
            color: #ffffffcc;
            font-family: system-ui, -apple-system, sans-serif;
          }
          .waiting {
            text-align: center;
          }
          .plug {
            font-size: 64px;
            margin-bottom: 24px;
            animation: wiggle 2s ease-in-out infinite;
          }
          .title {
            font-size: 20px;
            font-weight: 500;
            color: #a87090;
            margin-bottom: 12px;
            letter-spacing: 0.5px;
          }
          .subtitle {
            font-size: 14px;
            color: #b0a0a8;
            font-family: monospace;
            background: #101010;
            padding: 8px 16px;
            border-radius: 4px;
            border: 1px solid #483848;
          }
          .subtitle code {
            color: #70c070;
            font-weight: 600;
          }
          .dots::after {
            content: '';
            animation: dots 1.5s steps(4, end) infinite;
          }
          @keyframes dots {
            0%, 20% { content: ''; }
            40% { content: '.'; }
            60% { content: '..'; }
            80%, 100% { content: '...'; }
          }
          @keyframes wiggle {
            0%, 100% { transform: rotate(-5deg); }
            50% { transform: rotate(5deg); }
          }
        </style>
      </head>
      <body>
        <div class="waiting">
          <div class="plug">🔌</div>
          <div class="title">Waiting for local server<span class="dots"></span></div>
          <div class="subtitle">Run <code>ac-site</code> to start localhost:8888</div>
        </div>
      </body>
      </html>`;
  }

  return `<!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src ${
          webview.cspSource
        } 'unsafe-inline'; script-src 'nonce-${nonce}'; media-src *; img-src ${webview.cspSource} data:;">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<link href="${styleUri}" rel="stylesheet">
				<link href="${resetStyleUri}" rel="stylesheet">
				<link href="${vscodeStyleUri}" rel="stylesheet">
				<title>aesthetic.computer</title>
			</head>
			<body>
        <iframe id="aesthetic" credentialless sandbox="allow-scripts allow-same-origin allow-modals allow-popups allow-popups-to-escape-sandbox" allow="clipboard-write; clipboard-read" src="${iframeProtocol}${iframeUrl}/${param}${hashFragment}" border="none"></iframe>
       	<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
}

// 🌈 KidLisp.com WebView Content
function getKidLispWebViewContent(webview: any) {
  const nonce = getNonce();
  
  const styleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "main.css"),
  );

  const resetStyleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "reset.css"),
  );

  const vscodeStyleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "vscode.css"),
  );

  // Determine the iframe URL based on environment
  let iframeUrl;
  let iframeProtocol = "https://";
  if (isCodespaces && codespaceName && codespacesDomain) {
    iframeUrl = `${codespaceName}-8888.${codespacesDomain}`;
  } else {
    iframeUrl = local ? "localhost:8888" : "aesthetic.computer";
  }

  // Build CSP for kidlisp.com
  let cspFrameSrc = "frame-src https://aesthetic.computer https://localhost:8888";
  let cspChildSrc = "child-src https://aesthetic.computer https://localhost:8888";
  
  if (isCodespaces && codespacesDomain) {
    const codespaceWildcard = `https://*.${codespacesDomain}`;
    cspFrameSrc += ` ${codespaceWildcard}`;
    cspChildSrc += ` ${codespaceWildcard}`;
  }

  return `<!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src ${webview.cspSource} 'unsafe-inline'; script-src 'nonce-${nonce}'; media-src *; img-src ${webview.cspSource} data:;">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <link href="${styleUri}" rel="stylesheet">
      <link href="${resetStyleUri}" rel="stylesheet">
      <link href="${vscodeStyleUri}" rel="stylesheet">
      <title>KidLisp.com</title>
      <style>
        body {
          margin: 0;
          padding: 0;
          overflow: hidden;
        }
        iframe#kidlisp {
          border: none;
          width: 100vw;
          height: 100vh;
          background: linear-gradient(135deg, #fffacd 0%, #fff9c0 100%);
        }
      </style>
    </head>
    <body>
      <iframe id="kidlisp" class="visible" sandbox="allow-scripts allow-same-origin allow-modals allow-popups allow-popups-to-escape-sandbox" allow="clipboard-write; clipboard-read" src="${iframeProtocol}${iframeUrl}/kidlisp.com" border="none"></iframe>
      <script nonce="${nonce}">
        const vscode = acquireVsCodeApi();
        // Forward messages from kidlisp.com iframe to extension
        window.addEventListener('message', (event) => {
          if (event.data && event.data.type && event.data.type.startsWith('vscode-extension:')) {
            vscode.postMessage(event.data);
          }
        });
      </script>
    </body>
    </html>`;
}

export { activate, AestheticViewProvider };