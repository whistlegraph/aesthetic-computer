// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live coding aesthetic.computer pieces and
// exploring the system documentation.

/* #region TODO 📓 
#endregion */

// Import necessary modules from vscode
import * as vscode from "vscode";

// Import the generated process tree JS (built from views/process-tree.js)
import { PROCESS_TREE_JS } from "./generated-views";

// Dynamically import path and fs to ensure web compatibility.
let path: any, fs: any;
(async () => {
  if (typeof window === "undefined") {
    path = await import("path");
    fs = await import("fs");
  }
})();

import { AestheticAuthenticationProvider } from "./aestheticAuthenticationProviderRemote";
import * as acorn from "acorn";
const { keys } = Object;

// AST parsing and tracking for JS/MJS files
interface ASTNode {
  id: string;
  type: string;
  name?: string;
  start: number;
  end: number;
  children: ASTNode[];
  parent?: string;
  depth: number;
  loc?: { start: { line: number; column: number }; end: { line: number; column: number } };
}

interface TrackedFile {
  uri: string;
  fileName: string;
  ast: ASTNode | null;
  lastUpdate: number;
}

const trackedFiles = new Map<string, TrackedFile>();
let astUpdateCallback: ((files: TrackedFile[]) => void) | null = null;

function parseJSToAST(code: string, fileName: string): ASTNode | null {
  try {
    const ast = acorn.parse(code, {
      ecmaVersion: 'latest',
      sourceType: 'module',
      locations: true,
    });
    
    let nodeId = 0;
    
    function convertNode(node: any, depth: number = 0, parentId?: string): ASTNode {
      const id = `${fileName}-${nodeId++}`;
      const children: ASTNode[] = [];
      
      // Get a friendly name for the node
      let name: string | undefined;
      if (node.id?.name) name = node.id.name;
      else if (node.key?.name) name = node.key.name;
      else if (node.name) name = node.name;
      else if (node.type === 'Literal') name = String(node.value).slice(0, 20);
      else if (node.type === 'Identifier') name = node.name;
      
      const astNode: ASTNode = {
        id,
        type: node.type,
        name,
        start: node.start,
        end: node.end,
        children,
        parent: parentId,
        depth,
        loc: node.loc,
      };
      
      // Recursively process child nodes
      for (const key of Object.keys(node)) {
        if (key === 'type' || key === 'start' || key === 'end' || key === 'loc' || key === 'range') continue;
        const value = node[key];
        if (value && typeof value === 'object') {
          if (Array.isArray(value)) {
            for (const item of value) {
              if (item && typeof item === 'object' && item.type) {
                children.push(convertNode(item, depth + 1, id));
              }
            }
          } else if (value.type) {
            children.push(convertNode(value, depth + 1, id));
          }
        }
      }
      
      return astNode;
    }
    
    return convertNode(ast);
  } catch (e) {
    console.log(`AST parse error for ${fileName}:`, e);
    return null;
  }
}

function updateTrackedFile(document: vscode.TextDocument) {
  const uri = document.uri.toString();
  const fileName = document.fileName.split('/').pop() || document.fileName;
  
  // Only track JS/MJS files
  if (!fileName.endsWith('.js') && !fileName.endsWith('.mjs') && !fileName.endsWith('.ts')) {
    return;
  }
  
  const ast = parseJSToAST(document.getText(), fileName);
  
  trackedFiles.set(uri, {
    uri,
    fileName,
    ast,
    lastUpdate: Date.now(),
  });
  
  if (astUpdateCallback) {
    astUpdateCallback(Array.from(trackedFiles.values()));
  }
}

function removeTrackedFile(uri: string) {
  trackedFiles.delete(uri);
  if (astUpdateCallback) {
    astUpdateCallback(Array.from(trackedFiles.values()));
  }
}

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

  // 🌳 AST Tracking: Watch open JS/MJS documents for live AST visualization
  // Track currently open editors
  vscode.window.visibleTextEditors.forEach(editor => {
    updateTrackedFile(editor.document);
  });
  
  // Track when editors change
  context.subscriptions.push(
    vscode.window.onDidChangeVisibleTextEditors(editors => {
      // Add new files
      editors.forEach(editor => updateTrackedFile(editor.document));
    })
  );
  
  // Track document changes for live updates
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument(event => {
      if (trackedFiles.has(event.document.uri.toString())) {
        updateTrackedFile(event.document);
      }
    })
  );
  
  // Track document closes
  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument(document => {
      removeTrackedFile(document.uri.toString());
    })
  );
  
  // Set up AST update callback to send to welcome panel
  astUpdateCallback = (files) => {
    if (welcomePanel) {
      welcomePanel.webview.postMessage({
        command: 'astUpdate',
        files: files.map(f => ({
          fileName: f.fileName,
          ast: f.ast,
          lastUpdate: f.lastUpdate,
        })),
      });
    }
  };

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

  // 🔧 Dev Mode for Welcome Panel (uses `local` flag - load from local server instead of embedded JS)
  const WELCOME_DEV_URL = 'http://localhost:5555/dev.html';

  // Helper function to generate Welcome Panel HTML from shared process-tree.js
  // Helper function to detect current VS Code theme kind
  function getVSCodeThemeKind(): 'dark' | 'light' {
    const kind = vscode.window.activeColorTheme.kind;
    // ColorThemeKind: 1 = Light, 2 = Dark, 3 = HighContrast, 4 = HighContrastLight
    return (kind === 1 || kind === 4) ? 'light' : 'dark';
  }

  function getWelcomePanelHtml(webview: vscode.Webview, devMode: boolean = false): string {
    const theme = getVSCodeThemeKind();
    
    // Dev mode: load from local server via iframe with live reload support
    if (devMode) {
      return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; frame-src http://localhost:5555; style-src 'unsafe-inline'; script-src 'unsafe-inline';">
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { background: ${theme === 'light' ? '#fcf7c5' : '#000'}; height: 100vh; overflow: hidden; }
    iframe { width: 100%; height: 100%; border: none; }
    .dev-controls { position: fixed; bottom: 12px; left: 12px; z-index: 1000; display: flex; gap: 8px; align-items: center; }
    .dev-btn { background: ${theme === 'light' ? 'rgba(0,0,0,0.1)' : 'rgba(255,255,255,0.1)'}; border: 1px solid ${theme === 'light' ? 'rgba(0,0,0,0.2)' : 'rgba(255,255,255,0.2)'}; color: ${theme === 'light' ? '#281e5a' : '#fff'}; padding: 6px 12px; border-radius: 4px; font-size: 11px; font-family: monospace; cursor: pointer; }
    .dev-btn:hover { background: ${theme === 'light' ? 'rgba(0,0,0,0.2)' : 'rgba(255,255,255,0.2)'}; }
    .dev-status { font-size: 10px; color: ${theme === 'light' ? '#806060' : '#888'}; font-family: monospace; }
  </style>
</head>
<body>
  <iframe id="dev-frame" src="${WELCOME_DEV_URL}?theme=${theme}"></iframe>
  <div class="dev-controls">
    <button class="dev-btn" onclick="reload()">🔄 Reload</button>
    <span class="dev-status" id="status">DEV MODE</span>
  </div>
  <script>
    const vscode = acquireVsCodeApi();
    const frame = document.getElementById('dev-frame');
    const status = document.getElementById('status');
    
    function reload() {
      status.textContent = 'Reloading...';
      frame.src = frame.src.split('?')[0] + '?theme=${theme}&t=' + Date.now();
      setTimeout(() => status.textContent = 'DEV MODE', 500);
    }
    
    // Listen for reload messages from extension
    window.addEventListener('message', (e) => {
      if (e.data?.command === 'reload') {
        reload();
      }
    });
  </script>
</body>
</html>`;
    }
    
    // Production mode: use embedded JS with theme support
    const csp = `default-src 'none'; style-src 'unsafe-inline'; img-src ${webview.cspSource} https: data:; script-src 'unsafe-inline' https://cdnjs.cloudflare.com https://cdn.jsdelivr.net; connect-src ws://127.0.0.1:7890 wss://localhost:8889;`;
    
    // Color schemes from color-schemes.js (embedded for production)
    const darkColors = {
      bg: '#181318', bgAlt: '#141214', fg: '#ffffffcc', fgBright: '#ffffff', fgMuted: '#555555',
      accent: '#a87090', accentBright: '#ff69b4', statusOnline: '#0f0', labelInfo: '#aaa'
    };
    const lightColors = {
      bg: '#fcf7c5', bgAlt: '#f5f0c0', fg: '#281e5a', fgBright: '#281e5a', fgMuted: '#806060',
      accent: '#387adf', accentBright: '#006400', statusOnline: '#006400', labelInfo: '#806060'
    };
    const c = theme === 'light' ? lightColors : darkColors;
    
    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy" content="${csp}">
  <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"></script>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { background: ${c.bg}; color: ${c.fgBright}; font-family: monospace; overflow: hidden; height: 100vh; }
    body[data-theme="dark"] { background: #181318; color: #fff; }
    body[data-theme="light"] { background: #fcf7c5; color: #281e5a; }
    #canvas { position: absolute; top: 0; left: 0; }
    .hud { position: absolute; z-index: 100; pointer-events: none; }
    .title { top: 16px; left: 16px; font-size: 14px; display: flex; align-items: center; gap: 8px; }
    .title .dot { color: ${c.accentBright}; }
    .status-dot { width: 6px; height: 6px; border-radius: 50%; background: ${c.accent}; }
    .status-dot.online { background: ${c.statusOnline}; }
    .stats { top: 16px; right: 16px; text-align: right; font-size: 12px; color: ${c.fgMuted}; }
    .stats .val { color: ${c.fgBright}; }
    .mem { bottom: 16px; right: 16px; font-size: 12px; color: ${c.fgMuted}; }
    .center { top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center; font-size: 12px; color: ${c.fgMuted}; }
    .center .count { font-size: 28px; color: ${c.fgBright}; margin-bottom: 4px; }
    .label-container { position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; z-index: 50; overflow: hidden; }
    .proc-label { position: absolute; text-align: center; transform: translate(-50%, -100%); white-space: nowrap; text-shadow: 0 0 3px ${c.bg}, 0 0 6px ${c.bg}; pointer-events: none; }
    .proc-label .icon { font-size: 18px; display: block; line-height: 1; }
    .proc-label .name { font-size: 10px; margin-top: 2px; font-weight: bold; letter-spacing: 0.3px; }
    .proc-label .info { font-size: 8px; color: ${c.labelInfo}; margin-top: 1px; }
    .theme-toggle { position: fixed; top: 16px; right: 120px; background: rgba(${theme === 'light' ? '0,0,0' : '255,255,255'},0.1); border: 1px solid rgba(${theme === 'light' ? '0,0,0' : '255,255,255'},0.2); color: ${c.fgBright}; padding: 6px 12px; border-radius: 4px; font-size: 11px; font-family: monospace; cursor: pointer; z-index: 200; pointer-events: auto; }
    .theme-toggle:hover { background: rgba(${theme === 'light' ? '0,0,0' : '255,255,255'},0.2); }
  </style>
</head>
<body data-theme="${theme}">
  <canvas id="canvas"></canvas>
  <div class="hud title"><div class="status-dot" id="status-dot"></div><span>Aesthetic<span class="dot">.</span>Computer Architecture</span></div>
  <div class="hud stats"><div><span class="val" id="uptime">—</span></div><div><span class="val" id="cpus">—</span> cpus</div></div>
  <div class="hud center"><div class="count" id="process-count">0</div><div>processes</div></div>
  <div class="hud mem"><span id="mem-text">— / —</span> MB</div>
  <div id="labels" class="label-container"></div>
  <button class="theme-toggle" onclick="window.ProcessTreeViz?.toggleTheme()">🌓 Theme</button>
  <script>${PROCESS_TREE_JS}</script>
</body>
</html>`;
  }

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

    // Generate welcome panel HTML using shared process-tree.js (uses `local` flag for dev mode)
    welcomePanel.webview.html = getWelcomePanelHtml(welcomePanel.webview, local);
  }

  // Refresh welcome panel (called when local mode is toggled or theme changes)
  function refreshWelcomePanel() {
    if (welcomePanel) {
      welcomePanel.webview.html = getWelcomePanelHtml(welcomePanel.webview, local);
    }
  }

  // Live reload: send message to welcome panel to reload iframe (for dev mode)
  function triggerWelcomeReload() {
    if (welcomePanel && local) {
      welcomePanel.webview.postMessage({ command: 'reload' });
    }
  }

  // Watch for file changes in views directory when in local/dev mode
  const viewsWatcher = vscode.workspace.createFileSystemWatcher(
    new vscode.RelativePattern(context.extensionUri, 'views/**/*.{js,html,css}')
  );
  
  viewsWatcher.onDidChange(() => {
    if (local) triggerWelcomeReload();
  });
  viewsWatcher.onDidCreate(() => {
    if (local) triggerWelcomeReload();
  });
  
  context.subscriptions.push(viewsWatcher);

  // Listen for VS Code theme changes and refresh welcome panel
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(() => {
      refreshWelcomePanel();
    })
  );

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
      refreshWelcomePanel(); // Also refresh welcome panel for dev mode
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