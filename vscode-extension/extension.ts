// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live coding aesthetic.computer pieces and
// exploring the system documentation.

/* #region TODO 📓 
#endregion */

// Import necessary modules from vscode
import * as vscode from "vscode";
import { Buffer } from "buffer";

// (generated-views import removed — welcome panel now loads dev.html via iframe only)

// 🌈 KidLisp Syntax Highlighting
// Embedded copy of shared/kidlisp-syntax.mjs for bundling
// This provides Monaco-parity highlighting for .lisp files in VS Code
import * as KidLispSyntax from "./kidlisp-syntax";

// Dynamically import path, fs, child_process, and http to ensure web compatibility.
let path: any, fs: any, cp: any, http: any;
let _modulesReady: Promise<void>;
_modulesReady = (async () => {
  if (typeof window === "undefined") {
    path = await import("path");
    fs = await import("fs");
    cp = await import("child_process");
    http = await import("http");
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
  // Extra metadata for richer display
  kind?: string;      // const, let, var for VariableDeclaration
  source?: string;    // import source path
  callee?: string;    // function being called
  property?: string;  // member access property
  value?: string;     // literal value
}

interface TrackedFile {
  uri: string;
  fileName: string;
  filePath: string;
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
      let kind: string | undefined;
      let source: string | undefined;
      let callee: string | undefined;
      let property: string | undefined;
      let value: string | undefined;
      
      // Extract identifier names
      if (node.id?.name) name = node.id.name;
      else if (node.key?.name) name = node.key.name;
      else if (node.key?.value) name = String(node.key.value);
      else if (node.name) name = node.name;
      else if (node.type === 'Identifier') name = node.name;
      
      // Extract additional metadata based on node type
      switch (node.type) {
        case 'VariableDeclaration':
          kind = node.kind; // const, let, var
          break;
        case 'ImportDeclaration':
          source = node.source?.value;
          break;
        case 'ExportNamedDeclaration':
        case 'ExportDefaultDeclaration':
          if (node.declaration?.id?.name) name = node.declaration.id.name;
          if (node.source?.value) source = node.source.value;
          break;
        case 'CallExpression':
          if (node.callee?.name) callee = node.callee.name;
          else if (node.callee?.property?.name) callee = node.callee.property.name;
          else if (node.callee?.type === 'MemberExpression') {
            const obj = node.callee.object?.name || '';
            const prop = node.callee.property?.name || '';
            callee = obj ? `${obj}.${prop}` : prop;
          }
          break;
        case 'MemberExpression':
          property = node.property?.name || node.property?.value;
          break;
        case 'Literal':
          value = String(node.value).slice(0, 30);
          name = value.length > 15 ? value.slice(0, 12) + '…' : value;
          break;
        case 'TemplateLiteral':
          name = '`template`';
          break;
        case 'Property':
          name = node.key?.name || node.key?.value;
          break;
      }
      
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
        kind,
        source,
        callee,
        property,
        value,
      };
      
      // Recursively process child nodes
      for (const key of Object.keys(node)) {
        if (key === 'type' || key === 'start' || key === 'end' || key === 'loc' || key === 'range') continue;
        const val = node[key];
        if (val && typeof val === 'object') {
          if (Array.isArray(val)) {
            for (const item of val) {
              if (item && typeof item === 'object' && item.type) {
                children.push(convertNode(item, depth + 1, id));
              }
            }
          } else if (val.type) {
            children.push(convertNode(val, depth + 1, id));
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
  const filePath = document.fileName;
  const fileName = filePath.split('/').pop() || filePath;
  
  // Track JS/MJS/TS files (for AST), plus Markdown and Lisp files (as open buffers)
  const isCodeFile = fileName.endsWith('.js') || fileName.endsWith('.mjs') || fileName.endsWith('.ts');
  const isMarkdown = fileName.endsWith('.md');
  const isLisp = fileName.endsWith('.lisp');
  
  if (!isCodeFile && !isMarkdown && !isLisp) {
    return;
  }
  
  // Only parse AST for code files
  const ast = isCodeFile ? parseJSToAST(document.getText(), fileName) : null;
  
  trackedFiles.set(uri, {
    uri,
    fileName,
    filePath,
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
let newsWindow: any;
let atWindow: any;
let welcomePanel: vscode.WebviewPanel | null = null;
let localServerCheckInterval: NodeJS.Timeout | undefined;
let provider: AestheticViewProvider;
let lastWebviewRefreshAt = 0; // Debounce webview refreshes
const WEBVIEW_REFRESH_DEBOUNCE_MS = 10000; // Minimum 10s between auto-refreshes

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
          // HEAD can return 404/redirect even when the server is healthy.
          // We only care that something is listening and responding.
          path: `/?vscode=1&ts=${Date.now()}`,
          method: "GET",
          rejectUnauthorized: false, // Allow self-signed certs
          timeout: 2000,
        },
        (res) => {
          // Any response means the server is reachable.
          resolve(true);
          // Ensure we don't hang waiting for body data.
          res.resume();
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
  
  // Helper to refresh all webviews with debouncing
  function refreshAllWebviews() {
    const now = Date.now();
    if (now - lastWebviewRefreshAt < WEBVIEW_REFRESH_DEBOUNCE_MS) {
      console.log("⏳ Skipping webview refresh (debounced)");
      return;
    }
    lastWebviewRefreshAt = now;
    console.log("✅ Local server is now available");
    // Refresh webviews when server becomes available
    if (provider) provider.refreshWebview();
    refreshWebWindow();
    refreshKidLispWindow();
    refreshNewsWindow();
    refreshAtWindow();
  }

  // Check immediately
  checkLocalServer().then((available) => {
    const wasAvailable = localServerAvailable;
    localServerAvailable = available;
    if (available && !wasAvailable) {
      refreshAllWebviews();
    }
  });
  
  // Then check every 3 seconds
  localServerCheckInterval = setInterval(async () => {
    const wasAvailable = localServerAvailable;
    localServerAvailable = await checkLocalServer();
    
    if (localServerAvailable && !wasAvailable) {
      refreshAllWebviews();
    } else if (!localServerAvailable && wasAvailable) {
      console.log("⏳ Local server disconnected - waiting for reconnect...");
      // Don't immediately show waiting screen - server may come back quickly during hot reload
      // The webview will show the waiting screen on next manual refresh or when we first load
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

// 🌈 KidLisp Syntax Highlighting Setup
// Uses atomic decoration updates to avoid flickering (like Monaco's deltaDecorations)
const kidlispDecorationTypes = new Map<string, vscode.TextEditorDecorationType>();
let kidlispHighlightInterval: NodeJS.Timeout | undefined;
// Track which decoration type keys were used in the last render per editor
const lastUsedDecorationKeys = new Map<string, Set<string>>();

function getOrCreateDecorationType(color: string, options: { bold?: boolean } = {}): vscode.TextEditorDecorationType {
  const key = `${color}-${options.bold ? 'bold' : 'normal'}`;
  if (!kidlispDecorationTypes.has(key)) {
    const hexColor = KidLispSyntax.colorToHex(color);
    kidlispDecorationTypes.set(key, vscode.window.createTextEditorDecorationType({
      color: hexColor,
      fontWeight: options.bold ? 'bold' : 'normal',
    }));
  }
  return kidlispDecorationTypes.get(key)!;
}

function applyKidLispDecorations(editor: vscode.TextEditor, isEditMode: boolean = true) {
  if (!editor || editor.document.languageId !== 'kidlisp') {
    return;
  }

  const document = editor.document;
  const text = document.getText();
  const tokens = KidLispSyntax.tokenizeWithPositions(text);
  const tokenValues = tokens.map(t => t.value);

  // Group decorations by color
  const decorationsByColor = new Map<string, vscode.DecorationOptions[]>();

  // Check if we're in light mode
  const isLightTheme = vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.Light ||
                       vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.HighContrastLight;

  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    let color = KidLispSyntax.getTokenColor(token.value, tokenValues, i, { isEditMode });

    // Handle special color markers
    if (color === 'RAINBOW') {
      // Rainbow: each character gets a different color
      for (let j = 0; j < token.value.length; j++) {
        const charColor = KidLispSyntax.RAINBOW_COLORS[j % KidLispSyntax.RAINBOW_COLORS.length];
        const startPos = document.positionAt(token.pos + j);
        const endPos = document.positionAt(token.pos + j + 1);
        const range = new vscode.Range(startPos, endPos);

        if (!decorationsByColor.has(charColor)) {
          decorationsByColor.set(charColor, []);
        }
        decorationsByColor.get(charColor)!.push({ range });
      }
      continue;
    }

    if (color === 'ZEBRA') {
      // Zebra: alternating black and white
      for (let j = 0; j < token.value.length; j++) {
        const charColor = KidLispSyntax.ZEBRA_COLORS[j % KidLispSyntax.ZEBRA_COLORS.length];
        const startPos = document.positionAt(token.pos + j);
        const endPos = document.positionAt(token.pos + j + 1);
        const range = new vscode.Range(startPos, endPos);

        if (!decorationsByColor.has(charColor)) {
          decorationsByColor.set(charColor, []);
        }
        decorationsByColor.get(charColor)!.push({ range });
      }
      continue;
    }

    if (color.startsWith('COMPOUND:')) {
      // Compound: prefix char in one color, rest in another
      const [, prefixColor, identifierColor] = color.split(':');

      // Prefix character ($ or # or !)
      const prefixStart = document.positionAt(token.pos);
      const prefixEnd = document.positionAt(token.pos + 1);
      const prefixRange = new vscode.Range(prefixStart, prefixEnd);

      const finalPrefixColor = isLightTheme ? KidLispSyntax.getLightModeColor(prefixColor) : prefixColor;
      if (!decorationsByColor.has(finalPrefixColor)) {
        decorationsByColor.set(finalPrefixColor, []);
      }
      decorationsByColor.get(finalPrefixColor)!.push({ range: prefixRange });

      // Identifier part
      const idStart = document.positionAt(token.pos + 1);
      const idEnd = document.positionAt(token.pos + token.value.length);
      const idRange = new vscode.Range(idStart, idEnd);

      const finalIdColor = isLightTheme ? KidLispSyntax.getLightModeColor(identifierColor) : identifierColor;
      if (!decorationsByColor.has(finalIdColor)) {
        decorationsByColor.set(finalIdColor, []);
      }
      decorationsByColor.get(finalIdColor)!.push({ range: idRange });

      continue;
    }

    // Regular color
    const finalColor = isLightTheme ? KidLispSyntax.getLightModeColor(color) : color;
    const startPos = document.positionAt(token.pos);
    const endPos = document.positionAt(token.pos + token.value.length);
    const range = new vscode.Range(startPos, endPos);

    if (!decorationsByColor.has(finalColor)) {
      decorationsByColor.set(finalColor, []);
    }
    decorationsByColor.get(finalColor)!.push({ range });
  }

  // Atomic update: apply new decorations and clear stale ones in one pass
  // setDecorations(type, ranges) replaces all decorations for that type,
  // so we don't need to clear first — just set the new ranges.
  const editorId = editor.document.uri.toString();
  const currentKeys = new Set<string>();

  for (const [color, ranges] of decorationsByColor) {
    const decorationType = getOrCreateDecorationType(color, { bold: true });
    const key = `${color}-bold`;
    currentKeys.add(key);
    editor.setDecorations(decorationType, ranges);
  }

  // Only clear decoration types that were used last time but NOT this time
  const previousKeys = lastUsedDecorationKeys.get(editorId);
  if (previousKeys) {
    for (const key of previousKeys) {
      if (!currentKeys.has(key)) {
        const decorationType = kidlispDecorationTypes.get(key);
        if (decorationType) {
          editor.setDecorations(decorationType, []);
        }
      }
    }
  }

  lastUsedDecorationKeys.set(editorId, currentKeys);
}

// Check if any visible KidLisp editor has timing tokens that need blinking
const timingPattern = /\d*\.?\d+[sf](?:\.\.\.?|!?)/;
function hasTimingTokens(): boolean {
  for (const editor of vscode.window.visibleTextEditors) {
    if (editor.document.languageId === 'kidlisp' && timingPattern.test(editor.document.getText())) {
      return true;
    }
  }
  return false;
}

function setupKidLispHighlighting(context: vscode.ExtensionContext) {
  // Apply highlighting to currently visible KidLisp editors
  function refreshAllKidLispEditors() {
    for (const editor of vscode.window.visibleTextEditors) {
      if (editor.document.languageId === 'kidlisp') {
        applyKidLispDecorations(editor, true);
      }
    }
  }

  // Initial application
  refreshAllKidLispEditors();

  // Refresh when editors change
  context.subscriptions.push(
    vscode.window.onDidChangeVisibleTextEditors(() => {
      refreshAllKidLispEditors();
    })
  );

  // Refresh when document content changes
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument(event => {
      const editor = vscode.window.visibleTextEditors.find(
        e => e.document === event.document
      );
      if (editor && editor.document.languageId === 'kidlisp') {
        applyKidLispDecorations(editor, true);
      }
    })
  );

  // Refresh when theme changes (for light/dark mode support)
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(() => {
      // Clear cached decoration types since colors may need updating
      for (const [, decorationType] of kidlispDecorationTypes) {
        decorationType.dispose();
      }
      kidlispDecorationTypes.clear();
      lastUsedDecorationKeys.clear();
      refreshAllKidLispEditors();
    })
  );

  // Timing blink animation interval — only refreshes when timing tokens exist
  if (kidlispHighlightInterval) {
    clearInterval(kidlispHighlightInterval);
  }
  kidlispHighlightInterval = setInterval(() => {
    if (hasTimingTokens()) {
      refreshAllKidLispEditors();
    }
  }, 250); // ~4fps is enough for blink animation

  // Clean up on deactivation
  context.subscriptions.push({
    dispose: () => {
      if (kidlispHighlightInterval) {
        clearInterval(kidlispHighlightInterval);
        kidlispHighlightInterval = undefined;
      }
      for (const [, decorationType] of kidlispDecorationTypes) {
        decorationType.dispose();
      }
      kidlispDecorationTypes.clear();
      lastUsedDecorationKeys.clear();
    }
  });
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
  
  // 🛡️ Periodic Cleanup: Ensure tracked files match actual open documents
  // (Fixes issues where files might get "stuck" if events are missed)
  setInterval(() => {
    const openUris = new Set(vscode.workspace.textDocuments.map(d => d.uri.toString()));
    let changed = false;
    for (const [uri, file] of trackedFiles) {
      if (!openUris.has(uri)) {
        console.log('🧹 Pruning stuck file:', file.fileName);
        trackedFiles.delete(uri);
        changed = true;
      }
    }
    if (changed && astUpdateCallback) {
      astUpdateCallback(Array.from(trackedFiles.values()));
    }
  }, 2000);
  
  // Set up AST update callback to send to welcome panel
  astUpdateCallback = (files) => {
    if (welcomePanel) {
      welcomePanel.webview.postMessage({
        command: 'astUpdate',
        files: files.map(f => ({
          id: f.uri, // Use URI as unique ID
          fileName: f.fileName,
          filePath: f.filePath, // Send full path
          ast: f.ast,
          lastUpdate: f.lastUpdate,
        })),
      });
    }
  };

  // 🌈 KidLisp Syntax Highlighting with Decorations
  // This provides Monaco-parity highlighting for .lisp files including:
  // - Rainbow/zebra color words, timing blinks, fade expressions
  // - Color codes, CSS colors, RGB channels, nested paren colors
  setupKidLispHighlighting(context);

  // console.log("🟢 Aesthetic Computer Extension: Activated");
  extContext = context;

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
          <iframe allow="clipboard-write; clipboard-read" credentialless sandbox="allow-scripts allow-modals allow-popups allow-popups-to-escape-sandbox allow-presentation" src="https://aesthetic.computer/docs${path}">
        </body>
        </html>
      `.trim();
      },
    ),
  );

  // ✦ Git Status Welcome Panel
  // Runs `git status --porcelain` directly via child_process for maximum speed.
  // Shows changes for both aesthetic-computer and aesthetic-computer-vault.

  interface GitRepoStatus {
    name: string;
    root: string;
    branch: string;
    files: { status: string; file: string }[];
    error?: string;
  }

  // Detect current VS Code theme kind
  function getVSCodeThemeKind(): 'dark' | 'light' {
    const themeName = vscode.window.activeColorTheme.name || '';
    if (themeName.includes('Aesthetic Computer')) {
      if (themeName.includes('Light')) return 'light';
      if (themeName.includes('Dark')) return 'dark';
    }
    const kind = vscode.window.activeColorTheme.kind;
    return (kind === 1 || kind === 4) ? 'light' : 'dark';
  }

  // Run a git command and return stdout (fast, ~5-15ms)
  async function gitExec(args: string, cwd: string): Promise<string> {
    await _modulesReady;
    return new Promise((resolve, reject) => {
      if (!cp) { reject(new Error('child_process not available')); return; }
      cp.exec(`git ${args}`, { cwd, timeout: 5000, maxBuffer: 1024 * 512 }, (err: any, stdout: string) => {
        if (err) reject(err);
        else resolve(stdout);
      });
    });
  }

  // Get git status for a repo directory
  async function getGitStatus(repoPath: string, name: string): Promise<GitRepoStatus> {
    try {
      const [statusOut, branchOut] = await Promise.all([
        gitExec('status --porcelain', repoPath),
        gitExec('rev-parse --abbrev-ref HEAD', repoPath),
      ]);
      const branch = branchOut.trim();
      const files = statusOut.split('\n').filter(Boolean).map(line => ({
        status: line.substring(0, 2).trim(),
        file: line.substring(3),
      }));
      return { name, root: repoPath, branch, files };
    } catch (e: any) {
      return { name, root: repoPath, branch: '?', files: [], error: e.message };
    }
  }

  // Gather status for all repos
  async function getAllGitStatus(): Promise<GitRepoStatus[]> {
    await _modulesReady;
    const repos: { name: string; root: string }[] = [];

    // Find aesthetic-computer workspace root
    const wsFolder = vscode.workspace.workspaceFolders?.[0];
    if (wsFolder && fs && path) {
      const acRoot = wsFolder.uri.fsPath;

      // Verify main repo is a git repo
      try {
        const mainGitDir = path.join(acRoot, '.git');
        if (fs.existsSync(mainGitDir)) {
          repos.push({ name: 'aesthetic-computer', root: acRoot });
        }
      } catch {}

      // Check for vault as a subdirectory
      try {
        const vaultPath = path.join(acRoot, 'aesthetic-computer-vault');
        const stat = fs.statSync(vaultPath);
        if (stat.isDirectory()) {
          const gitDir = path.join(vaultPath, '.git');
          if (fs.existsSync(gitDir)) {
            repos.push({ name: 'vault', root: vaultPath });
          }
        }
      } catch {}
    }

    return Promise.all(repos.map(r => getGitStatus(r.root, r.name)));
  }

  // Send git status update to the welcome panel
  let gitStatusPending = false;
  async function sendGitStatusToPanel() {
    if (!welcomePanel || gitStatusPending) return;
    gitStatusPending = true;
    try {
      const statuses = await getAllGitStatus();
      if (welcomePanel) {
        welcomePanel.webview.postMessage({ command: 'gitStatus', repos: statuses });
      }
    } finally {
      gitStatusPending = false;
    }
  }

  function getWelcomePanelHtml(): string {
    const theme = getVSCodeThemeKind();
    const c = theme === 'light'
      ? { bg: '#fcf7c5', fg: '#281e5a', fgMuted: '#907070', fgDim: '#b0a080', accent: '#387adf',
          added: '#1a7f37', modified: '#9a6700', deleted: '#cf222e', untracked: '#6e7781',
          fileBg: '#f6f0d0', fileBorder: '#e8e0b0', hoverBg: '#efe8c0', repoBg: '#f0e8b8', headerBg: '#e8dfa0' }
      : { bg: '#181318', fg: '#e0d0e0', fgMuted: '#807080', fgDim: '#504050', accent: '#a87090',
          added: '#3fb950', modified: '#d29922', deleted: '#f85149', untracked: '#606870',
          fileBg: '#1e1820', fileBorder: '#2a2030', hoverBg: '#28202e', repoBg: '#201828', headerBg: '#1a1220' };

    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline';">
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { background: ${c.bg}; color: ${c.fg}; font-family: 'SF Mono', 'Cascadia Code', 'Fira Code', 'Consolas', monospace; font-size: 12px; height: 100vh; overflow: auto; padding: 0; }
    .header { padding: 16px 20px 12px; display: flex; align-items: center; gap: 10px; border-bottom: 1px solid ${c.fileBorder}; background: ${c.headerBg}; position: sticky; top: 0; z-index: 1; }
    .header-star { font-size: 18px; color: ${c.accent}; }
    .header-title { font-size: 13px; font-weight: 600; letter-spacing: 0.5px; }
    .header-branch { color: ${c.fgMuted}; font-size: 11px; margin-left: auto; }
    .repo { margin: 0; }
    .repo-header { padding: 10px 20px 6px; font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 1.5px; color: ${c.fgMuted}; background: ${c.repoBg}; border-bottom: 1px solid ${c.fileBorder}; display: flex; align-items: center; gap: 8px; }
    .repo-header .branch { font-weight: 400; text-transform: none; letter-spacing: 0; color: ${c.accent}; }
    .repo-header .count { font-weight: 400; text-transform: none; letter-spacing: 0; color: ${c.fgDim}; margin-left: auto; }
    .file-list { list-style: none; }
    .file-item { display: flex; align-items: center; gap: 8px; padding: 4px 20px; cursor: pointer; border-bottom: 1px solid ${c.fileBorder}; transition: background 0.1s; }
    .file-item:hover { background: ${c.hoverBg}; }
    .status { font-weight: 700; width: 18px; text-align: center; flex-shrink: 0; font-size: 11px; }
    .status.M { color: ${c.modified}; }
    .status.A { color: ${c.added}; }
    .status.D { color: ${c.deleted}; }
    .status.R { color: ${c.modified}; }
    .status.U { color: ${c.untracked}; }
    .status.q { color: ${c.untracked}; }
    .file-path { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .file-dir { color: ${c.fgMuted}; }
    .file-name { color: ${c.fg}; }
    .empty { padding: 20px; text-align: center; color: ${c.fgMuted}; font-style: italic; }
    .error { padding: 12px 20px; color: ${c.deleted}; font-size: 11px; }
    .loading { padding: 20px; text-align: center; color: ${c.fgMuted}; }
    .loading .dot { display: inline-block; animation: pulse 1.5s ease-in-out infinite; color: ${c.accent}; }
    @keyframes pulse { 0%, 100% { opacity: 0.3; } 50% { opacity: 1; } }
  </style>
</head>
<body>
  <div class="header">
    <span class="header-star">✦</span>
    <span class="header-title">Source Changes</span>
  </div>
  <div id="content">
    <div class="loading"><span class="dot">✦</span> scanning...</div>
  </div>
  <script>
    (function() {
      const vscode = acquireVsCodeApi();
      const content = document.getElementById('content');

      function statusLabel(s) {
        if (s === '??' || s === '?') return { letter: '?', cls: 'q', title: 'Untracked' };
        const c = s.charAt(0) === ' ' ? s.charAt(1) : s.charAt(0);
        const map = { M: 'Modified', A: 'Added', D: 'Deleted', R: 'Renamed', C: 'Copied', U: 'Unmerged' };
        return { letter: c, cls: c, title: map[c] || c };
      }

      function esc(s) {
        return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
      }

      function renderRepos(repos) {
        if (!repos || repos.length === 0) {
          content.innerHTML = '<div class="empty">No git repositories detected in workspace.</div>';
          return;
        }

        let html = '';
        for (const repo of repos) {
          html += '<div class="repo">';
          html += '<div class="repo-header">';
          html += '<span>' + esc(repo.name) + '</span>';
          html += '<span class="branch">' + esc(repo.branch) + '</span>';
          html += '<span class="count">' + repo.files.length + ' change' + (repo.files.length !== 1 ? 's' : '') + '</span>';
          html += '</div>';

          if (repo.error) {
            html += '<div class="error">git error: ' + esc(repo.error) + '</div>';
          } else if (repo.files.length === 0) {
            html += '<div class="empty">Working tree clean</div>';
          } else {
            html += '<ul class="file-list">';
            for (const f of repo.files) {
              const s = statusLabel(f.status);
              const lastSlash = f.file.lastIndexOf('/');
              const dir = lastSlash >= 0 ? f.file.substring(0, lastSlash + 1) : '';
              const name = lastSlash >= 0 ? f.file.substring(lastSlash + 1) : f.file;
              html += '<li class="file-item" data-root="' + esc(repo.root) + '" data-file="' + esc(f.file) + '" title="' + esc(s.title + ': ' + f.file) + '">';
              html += '<span class="status ' + s.cls + '">' + s.letter + '</span>';
              html += '<span class="file-path"><span class="file-dir">' + esc(dir) + '</span><span class="file-name">' + esc(name) + '</span></span>';
              html += '</li>';
            }
            html += '</ul>';
          }
          html += '</div>';
        }
        content.innerHTML = html;
      }

      // Handle clicks on file items — open in editor
      content.addEventListener('click', (e) => {
        const item = e.target.closest('.file-item');
        if (!item) return;
        const root = item.getAttribute('data-root');
        const file = item.getAttribute('data-file');
        if (root && file) {
          vscode.postMessage({ command: 'openFile', root, file });
        }
      });

      // Receive git status updates from extension
      window.addEventListener('message', (event) => {
        const msg = event.data;
        if (msg.command === 'gitStatus') {
          renderRepos(msg.repos);
        }
      });

      // Request initial data
      vscode.postMessage({ command: 'requestGitStatus' });
    })();
  </script>
</body>
</html>`;
  }

  // ✨ Welcome Panel — shows git status for aesthetic-computer + vault
  function showWelcomePanel() {
    if (welcomePanel) {
      welcomePanel.reveal(vscode.ViewColumn.One);
      return;
    }

    welcomePanel = vscode.window.createWebviewPanel(
      "aestheticWelcome",
      "✦ Source Changes",
      vscode.ViewColumn.One,
      { enableScripts: true }
    );

    welcomePanel.webview.onDidReceiveMessage(
      message => {
        switch (message.command) {
          case 'requestGitStatus':
            sendGitStatusToPanel();
            return;
          case 'openFile':
            if (message.root && message.file) {
              const filePath = path?.join(message.root, message.file);
              if (filePath) {
                const openPath = vscode.Uri.file(filePath);
                vscode.workspace.openTextDocument(openPath).then(
                  doc => vscode.window.showTextDocument(doc),
                  () => {} // file might be deleted
                );
              }
            }
            return;
        }
      },
      undefined,
      context.subscriptions
    );

    welcomePanel.onDidDispose(() => {
      welcomePanel = null;
    });

    welcomePanel.webview.html = getWelcomePanelHtml();

    // Send initial git status after a brief delay for webview to initialize
    setTimeout(() => sendGitStatusToPanel(), 100);
  }

  // Refresh welcome panel on theme change
  function refreshWelcomePanel() {
    if (welcomePanel) {
      welcomePanel.webview.html = getWelcomePanelHtml();
      setTimeout(() => sendGitStatusToPanel(), 100);
    }
  }

  // Watch for file changes in both repos to auto-refresh git status
  function setupGitWatchers() {
    const wsFolder = vscode.workspace.workspaceFolders?.[0];
    if (!wsFolder) return;

    // Debounce: coalesce rapid file changes into one git status refresh
    let refreshTimer: NodeJS.Timeout | undefined;
    function debouncedRefresh() {
      if (refreshTimer) clearTimeout(refreshTimer);
      refreshTimer = setTimeout(() => sendGitStatusToPanel(), 300);
    }

    // Watch for all file changes in the workspace (covers both repos)
    const watcher = vscode.workspace.createFileSystemWatcher(
      new vscode.RelativePattern(wsFolder, '**/*')
    );
    watcher.onDidChange(debouncedRefresh);
    watcher.onDidCreate(debouncedRefresh);
    watcher.onDidDelete(debouncedRefresh);
    context.subscriptions.push(watcher);

    // Also refresh when documents are saved (catches staging operations)
    context.subscriptions.push(
      vscode.workspace.onDidSaveTextDocument(() => debouncedRefresh())
    );
  }

  setupGitWatchers();

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
    setTimeout(() => {
      if (vscode.window.visibleTextEditors.length === 0 && !welcomePanel) {
        showWelcomePanel();
      }
    }, 500);
  }

  // � User Handle Status Bar
  let statusBarUser: vscode.StatusBarItem;
  
  function updateUserStatusBar() {
    if (!statusBarUser) {
      statusBarUser = vscode.window.createStatusBarItem(
        vscode.StatusBarAlignment.Right,
        50,
      );
      statusBarUser.command = "aestheticComputer.login";
      context.subscriptions.push(statusBarUser);
    }

    // Check for aesthetic session
    const aestheticSession = context.globalState.get<any>("aesthetic:session");
    const sotceSession = context.globalState.get<any>("sotce:session");
    
    if (aestheticSession?.account?.label) {
      const label = aestheticSession.account.label;
      const displayLabel = label.startsWith("@") ? label : `@${label}`;
      statusBarUser.text = `$(account) ${displayLabel}`;
      statusBarUser.tooltip = `Logged in to Aesthetic Computer as ${displayLabel}\nClick to manage account`;
      statusBarUser.backgroundColor = undefined;
      statusBarUser.show();
    } else if (sotceSession?.account?.label) {
      const label = sotceSession.account.label;
      const displayLabel = label.startsWith("@") ? label : `@${label}`;
      statusBarUser.text = `$(account) ${displayLabel}`;
      statusBarUser.tooltip = `Logged in to Sotce Net as ${displayLabel}\nClick to manage account`;
      statusBarUser.backgroundColor = undefined;
      statusBarUser.show();
    } else {
      statusBarUser.text = `$(sign-in) Sign In`;
      statusBarUser.tooltip = `Sign in to Aesthetic Computer`;
      statusBarUser.backgroundColor = new vscode.ThemeColor('statusBarItem.warningBackground');
      statusBarUser.show();
    }
  }
  
  // Initialize user status bar
  updateUserStatusBar();
  
  // 🔥 AC-OS OTA Build Status Bar
  // Polls oven.aesthetic.computer/native-build to show live OTA build status.
  let statusBarOTA: vscode.StatusBarItem;
  let otaPollInterval: NodeJS.Timeout | undefined;

  interface OTABuildState {
    activeJobId: string | null;
    active: {
      id: string;
      ref: string;
      status: string;
      stage: string;
      percent: number;
      elapsedMs: number;
      error?: string;
      buildName?: string;
      commitMsg?: string;
    } | null;
    recent: Array<{
      id: string;
      ref: string;
      status: string;
      stage: string;
      percent: number;
      elapsedMs: number;
      finishedAt?: string;
      error?: string;
      buildName?: string;
      commitMsg?: string;
      flags?: string[];
    }>;
  }

  const OTA_STAGES: Record<string, { icon: string; label: string; step: number }> = {
    'preflight-sync': { icon: '$(repo-sync~spin)', label: 'sync', step: 1 },
    'prune':          { icon: '$(trash~spin)',     label: 'prune', step: 1 },
    'docker-build':   { icon: '$(container~spin)', label: 'docker', step: 2 },
    'binary':         { icon: '$(gear~spin)',      label: 'binary', step: 3 },
    'initramfs':      { icon: '$(package~spin)',   label: 'initramfs', step: 4 },
    'kernel':         { icon: '$(cpu~spin)',       label: 'kernel', step: 5 },
    'smoke-test':     { icon: '$(beaker~spin)',    label: 'test', step: 6 },
    'upload':         { icon: '$(cloud-upload~spin)', label: 'upload', step: 7 },
    'done':           { icon: '$(check)',          label: 'done', step: 8 },
  };
  const OTA_TOTAL_STEPS = 7;

  function getOTAIcon(stage: string, status: string): string {
    if (status === 'running') {
      return OTA_STAGES[stage]?.icon || '$(sync~spin)';
    }
    switch (status) {
      case 'success': return '$(check)';
      case 'failed': return '$(error)';
      case 'cancelled': return '$(close)';
      default: return '$(flame)';
    }
  }

  function getOTAColor(status: string): vscode.ThemeColor | undefined {
    switch (status) {
      case 'running': return new vscode.ThemeColor('statusBarItem.warningBackground');
      case 'failed': return new vscode.ThemeColor('statusBarItem.errorBackground');
      default: return undefined;
    }
  }

  async function fetchOTAStatus(): Promise<OTABuildState | null> {
    try {
      const https = await import("https");
      return new Promise((resolve) => {
        const req = https.request({
          hostname: 'oven.aesthetic.computer',
          path: '/native-build',
          method: 'GET',
          headers: { 'User-Agent': 'aesthetic-computer-vscode' },
          timeout: 5000,
        }, (res: any) => {
          let data = '';
          res.on('data', (chunk: string) => data += chunk);
          res.on('end', () => {
            try { resolve(JSON.parse(data)); }
            catch { resolve(null); }
          });
        });
        req.on('error', () => resolve(null));
        req.on('timeout', () => { req.destroy(); resolve(null); });
        req.end();
      });
    } catch {
      return null;
    }
  }

  async function updateOTAStatusBar() {
    if (!statusBarOTA) {
      statusBarOTA = vscode.window.createStatusBarItem(
        vscode.StatusBarAlignment.Left,
        99,
      );
      statusBarOTA.command = "aestheticComputer.openOSPage";
      context.subscriptions.push(statusBarOTA);
    }

    const state = await fetchOTAStatus();
    if (!state) {
      statusBarOTA.text = '$(flame) OTA';
      statusBarOTA.tooltip = 'Could not reach oven.aesthetic.computer';
      statusBarOTA.backgroundColor = undefined;
      statusBarOTA.show();
      return;
    }

    if (state.active) {
      const a = state.active;
      const elapsed = Math.round(a.elapsedMs / 1000);
      const mins = Math.floor(elapsed / 60);
      const secs = elapsed % 60;
      const timeStr = mins > 0 ? `${mins}m${secs}s` : `${secs}s`;
      const name = a.buildName || a.ref?.substring(0, 7) || '';
      const variant = a.stage?.startsWith('cl-') ? ' CL' : ' C';
      const rawStage = a.stage?.replace('cl-', '') || '';
      const stageInfo = OTA_STAGES[rawStage];
      const stageLabel = stageInfo?.label || rawStage;
      const stepNum = stageInfo?.step || 0;
      const progress = a.percent > 0 ? ` ${a.percent}%` : '';
      const stepStr = stepNum > 0 ? ` [${stepNum}/${OTA_TOTAL_STEPS}]` : '';
      statusBarOTA.text = `${getOTAIcon(a.stage, 'running')} ${name}${variant} | ${stageLabel}${progress}${stepStr} | ${timeStr}`;
      statusBarOTA.backgroundColor = getOTAColor('running');
      let tip = `AC-OS Building: ${name}\nVariant:${variant}\nStage: ${stageLabel}${progress}${stepStr}\nElapsed: ${timeStr}`;
      if (a.commitMsg) tip += `\n${a.commitMsg}`;
      if (a.ref) tip += `\nCommit: ${a.ref.substring(0, 11)}`;
      statusBarOTA.tooltip = tip;
    } else if (state.recent?.length > 0) {
      const r = state.recent[0];
      const name = r.buildName || r.ref?.substring(0, 7) || '';
      const elapsed = Math.round(r.elapsedMs / 1000);
      const mins = Math.floor(elapsed / 60);
      const timeStr = mins > 0 ? `${mins}m${elapsed % 60}s` : `${elapsed}s`;
      const time = r.finishedAt ? new Date(r.finishedAt).toLocaleTimeString() : '';
      const icon = getOTAIcon(r.stage, r.status);
      statusBarOTA.text = r.status === 'success'
        ? `${icon} ${name} | ${timeStr}`
        : `${icon} ${name} ${r.status}`;
      statusBarOTA.backgroundColor = getOTAColor(r.status);
      let tip = `AC-OS: ${r.status} — ${name}`;
      if (r.commitMsg) tip += `\n${r.commitMsg}`;
      tip += `\nBuild time: ${timeStr}`;
      if (time) tip += ` | Finished: ${time}`;
      if (r.error) tip += `\nError: ${r.error}`;
      statusBarOTA.tooltip = tip;
    }

    statusBarOTA.show();
    adjustOTAPollRate(state);
  }

  let otaPollRate = 30000;
  function adjustOTAPollRate(state: OTABuildState | null) {
    const isActive = !!state?.active;
    const newRate = isActive ? 3000 : 30000; // 3s when building, 30s idle
    if (newRate !== otaPollRate) {
      otaPollRate = newRate;
      startOTAPolling();
    }
  }

  function startOTAPolling() {
    if (otaPollInterval) clearInterval(otaPollInterval);
    otaPollInterval = setInterval(() => updateOTAStatusBar(), otaPollRate);
  }

  updateOTAStatusBar();
  startOTAPolling();

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.showOTADetails", async () => {
      const state = await fetchOTAStatus();
      if (!state) {
        vscode.window.showWarningMessage('Could not reach oven.aesthetic.computer');
        return;
      }

      const items: vscode.QuickPickItem[] = [];

      // Active build at top
      if (state.active) {
        const a = state.active;
        const name = a.buildName || a.ref?.substring(0, 7) || 'building';
        items.push({
          label: `$(sync~spin) ${name}`,
          description: `${a.stage} ${a.percent}%`,
          detail: a.commitMsg || `ref: ${a.ref?.substring(0, 11) || '?'}`,
        });
      }

      // Recent builds
      for (const r of state.recent || []) {
        const name = r.buildName || r.ref?.substring(0, 7) || '?';
        const icon = r.status === 'success' ? '$(check)' : r.status === 'failed' ? '$(error)' : '$(close)';
        const elapsed = Math.round(r.elapsedMs / 1000);
        const time = r.finishedAt ? new Date(r.finishedAt).toLocaleTimeString() : '';
        items.push({
          label: `${icon} ${name}`,
          description: `${r.status} — ${elapsed}s${time ? ' — ' + time : ''}`,
          detail: r.commitMsg || (r.error ? `Error: ${r.error}` : `ref: ${r.ref?.substring(0, 11) || '?'}`),
        });
      }

      // Action items at bottom
      items.push(
        { label: '', kind: vscode.QuickPickItemKind.Separator },
        { label: '$(globe) Open Oven Dashboard', description: 'oven.aesthetic.computer' },
        { label: '$(cloud-download) Pull & Flash USB', description: 'ac-os pull' },
        { label: '$(refresh) Refresh', description: '' },
      );

      const pick = await vscode.window.showQuickPick(items, {
        title: 'AC-OS OTA Builds',
        placeHolder: 'Select a build or action...',
      });

      if (pick?.label.includes('Open Oven')) {
        vscode.env.openExternal(vscode.Uri.parse('https://oven.aesthetic.computer/native-build'));
      } else if (pick?.label.includes('Pull & Flash')) {
        const terminal = vscode.window.createTerminal('AC-OS Flash');
        terminal.show();
        terminal.sendText('fedac/native/ac-os pull');
      } else if (pick?.label.includes('Refresh')) {
        updateOTAStatusBar();
      }
    })
  );


  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.openOSPage", () => {
      vscode.env.openExternal(vscode.Uri.parse('https://prompt.ac/os'));
    })
  );

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

      // Push current session into the KidLisp iframe
      sendKidLispSession(panel.webview);

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
            case "kidlisp:ready": {
              sendKidLispSession(panel.webview);
              break;
            }
            case "kidlisp:login": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { createIfNone: true },
              );
              if (session) {
                extContext.globalState.update("aesthetic:session", session);
                updateUserStatusBar();
                await sendKidLispSession(panel.webview);
              }
              break;
            }
            case "kidlisp:logout": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { silent: true },
              );
              if (session) {
                await ap.removeSession(session.id);
                extContext.globalState.update("aesthetic:session", undefined);
              }
              updateUserStatusBar();
              await sendKidLispSession(panel.webview);
              break;
            }
            // Fallback for plain login/logout messages forwarded from the iframe
            case "login": {
              vscode.commands.executeCommand("aestheticComputer.logIn");
              break;
            }
            case "logout": {
              vscode.commands.executeCommand("aestheticComputer.logOut");
              break;
            }
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

  // 🧭 AT Window
  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.openAtWindow", () => {
      if (atWindow) {
        atWindow.reveal(vscode.ViewColumn.One);
        return;
      }

      const panel = vscode.window.createWebviewPanel(
        "atWebView",
        "AT",
        vscode.ViewColumn.One,
        {
          enableScripts: true,
          enableForms: true,
          localResourceRoots: [extContext.extensionUri],
        },
      );

      panel.title = "AT" + (local ? " 🧑‍🤝‍🧑" : "");
      panel.webview.html = getAtWebViewContent(panel.webview);
      atWindow = panel;

      panel.onDidDispose(
        () => {
          atWindow = null;
        },
        null,
        context.subscriptions,
      );
    }),
  );

  // 📰 News Window
  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.openNewsWindow", () => {
      if (newsWindow) {
        newsWindow.reveal(vscode.ViewColumn.One);
        return;
      }

      const panel = vscode.window.createWebviewPanel(
        "newsWebView",
        "News",
        vscode.ViewColumn.One,
        {
          enableScripts: true,
          enableForms: true,
          localResourceRoots: [extContext.extensionUri],
        },
      );

      panel.title = "News" + (local ? " 🧑‍🤝‍🧑" : "");
      panel.webview.html = getNewsWebViewContent(panel.webview);
      newsWindow = panel;

      sendNewsSession(panel.webview);

      panel.webview.onDidReceiveMessage(
        async (message) => {
          switch (message.type) {
            case "news:ready": {
              sendNewsSession(panel.webview);
              break;
            }
            case "news:login": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { createIfNone: true },
              );
              if (session) {
                extContext.globalState.update("aesthetic:session", session);
                updateUserStatusBar();
                await sendNewsSession(panel.webview);
              }
              break;
            }
            case "news:logout": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { silent: true },
              );
              if (session) {
                await ap.removeSession(session.id);
                extContext.globalState.update("aesthetic:session", undefined);
              }
              updateUserStatusBar();
              await sendNewsSession(panel.webview);
              break;
            }
            case "news:signup": {
              // Signup is just login with a hint - Auth0 handles the rest
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { createIfNone: true },
              );
              if (session) {
                extContext.globalState.update("aesthetic:session", session);
                updateUserStatusBar();
                await sendNewsSession(panel.webview);
              }
              break;
            }
            case "login": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { createIfNone: true },
              );
              if (session) {
                extContext.globalState.update("aesthetic:session", session);
                updateUserStatusBar();
                await sendNewsSession(panel.webview);
              }
              break;
            }
            case "signup": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { createIfNone: true },
              );
              if (session) {
                extContext.globalState.update("aesthetic:session", session);
                updateUserStatusBar();
                await sendNewsSession(panel.webview);
              }
              break;
            }
            case "logout": {
              const session = await vscode.authentication.getSession(
                "aesthetic",
                ["profile"],
                { silent: true },
              );
              if (session) {
                await ap.removeSession(session.id);
                extContext.globalState.update("aesthetic:session", undefined);
              }
              updateUserStatusBar();
              await sendNewsSession(panel.webview);
              break;
            }
          }
        },
        undefined,
        context.subscriptions,
      );

      panel.onDidDispose(
        () => {
          newsWindow = null;
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

  // Alias to match older calls expecting lowercase "login"
  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.login", async () => {
      await vscode.commands.executeCommand("aestheticComputer.logIn");
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

  // Alias to match older calls expecting lowercase "logout"
  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.logout", async () => {
      await vscode.commands.executeCommand("aestheticComputer.logOut");
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
    
    // Update user status bar after session change
    updateUserStatusBar();

    return session;
  };

  context.subscriptions.push(
    vscode.authentication.onDidChangeSessions(async (e) => {
      // console.log("🏃 Sessions changed:", e);
      if (e.provider.id === "aesthetic" || e.provider.id === "sotce") {
        await getSession(e.provider.id);
        provider.refreshWebview();
        refreshWebWindow();
        refreshKidLispWindow();
        refreshNewsWindow();
        sendKidLispSession();
        sendNewsSession();
        updateUserStatusBar();
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
      refreshNewsWindow();
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

  // 🌐 CDP Command Server — local HTTP endpoint for test harnesses and automation
  if (http) {
    const CDP_PORT = 19998;
    const cdpServer = http.createServer(async (req: any, res: any) => {
      res.setHeader("Access-Control-Allow-Origin", "*");
      res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      res.setHeader("Access-Control-Allow-Headers", "Content-Type");
      if (req.method === "OPTIONS") { res.writeHead(204); res.end(); return; }

      if (req.url === "/status") {
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ ok: true, extension: "aesthetic-computer" }));
        return;
      }

      if (req.url === "/command" && req.method === "POST") {
        const body: Buffer[] = [];
        req.on("data", (chunk: Buffer) => body.push(chunk));
        req.on("end", async () => {
          try {
            const { command, args } = JSON.parse(Buffer.concat(body).toString());
            const result = await vscode.commands.executeCommand(command, ...(args || []));
            res.writeHead(200, { "Content-Type": "application/json" });
            res.end(JSON.stringify({ ok: true, result: result ?? null }));
          } catch (e: any) {
            res.writeHead(500, { "Content-Type": "application/json" });
            res.end(JSON.stringify({ ok: false, error: e.message }));
          }
        });
        return;
      }

      res.writeHead(404);
      res.end("Not found");
    });

    cdpServer.listen(CDP_PORT, "127.0.0.1", () => {
      console.log(`🌐 CDP command server listening on http://127.0.0.1:${CDP_PORT}`);
    });
    cdpServer.on("error", (e: any) => {
      if (e.code === "EADDRINUSE") {
        console.log(`🌐 CDP command server port ${CDP_PORT} already in use, skipping`);
      }
    });
    context.subscriptions.push({ dispose: () => cdpServer.close() });
  }
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

function refreshAtWindow() {
  if (atWindow) {
    atWindow.title = "AT" + (local ? " 🧑‍🤝‍🧑" : "");
    atWindow.webview.html = getAtWebViewContent(atWindow.webview);
  }
}

function refreshNewsWindow() {
  if (newsWindow) {
    newsWindow.title = "News" + (local ? " 🧑‍🤝‍🧑" : "");
    newsWindow.webview.html = getNewsWebViewContent(newsWindow.webview);
  }
}

async function sendKidLispSession(target?: vscode.Webview) {
  const webview = target || kidlispWindow?.webview;
  if (!webview) return;

  let session: vscode.AuthenticationSession | null = null;
  try {
    session = await vscode.authentication.getSession(
      "aesthetic",
      ["profile"],
      { silent: true },
    );
  } catch (e) {
    console.log("🔴 Unable to fetch KidLisp session:", e);
  }

  webview.postMessage({ type: "setSession", tenant: "aesthetic", session });
}

async function sendNewsSession(target?: vscode.Webview) {
  const webview = target || newsWindow?.webview;
  if (!webview) return;

  let session: vscode.AuthenticationSession | null = null;
  try {
    session = await vscode.authentication.getSession(
      "aesthetic",
      ["profile"],
      { silent: true },
    );
  } catch (e) {
    console.log("🔴 Unable to fetch News session:", e);
  }

  webview.postMessage({ type: "setSession", tenant: "aesthetic", session });
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
    // Theme-aware colors for waiting screen
    const themeKind = vscode.window.activeColorTheme.kind;
    const isDark = themeKind === 2 || themeKind === 3; // Dark or HighContrast
    const bg = isDark ? '#181318' : '#fcf7c5';
    const textColor = isDark ? '#ffffffcc' : '#3b2a1a';
    const titleColor = isDark ? '#a87090' : '#387adf';
    const subtitleBg = isDark ? '#101010' : '#fff9f0';
    const subtitleBorder = isDark ? '#483848' : '#e2d4c3';
    const subtitleColor = isDark ? '#b0a0a8' : '#6b4a2e';
    const codeColor = isDark ? '#70c070' : '#006400';

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
            background: ${bg};
            color: ${textColor};
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
            color: ${titleColor};
            margin-bottom: 12px;
            letter-spacing: 0.5px;
          }
          .subtitle {
            font-size: 14px;
            color: ${subtitleColor};
            font-family: monospace;
            background: ${subtitleBg};
            padding: 8px 16px;
            border-radius: 4px;
            border: 1px solid ${subtitleBorder};
          }
          .subtitle code {
            color: ${codeColor};
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
        <iframe id="aesthetic" sandbox="allow-scripts allow-same-origin allow-modals allow-popups allow-popups-to-escape-sandbox allow-presentation" allow="clipboard-write; clipboard-read" src="${iframeProtocol}${iframeUrl}/${param}${hashFragment}" border="none"></iframe>
       	<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
}

  // 📰 News WebView Content
  function getNewsWebViewContent(webview: any) {
    const nonce = getNonce();
    
    // Detect VS Code theme for News waiting screen
    const themeKind = vscode.window.activeColorTheme.kind;
    const isDark = themeKind === 2 || themeKind === 3; // Dark or HighContrast

    const styleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(extContext.extensionUri, "main.css"),
    );

    const resetStyleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(extContext.extensionUri, "reset.css"),
    );

    const vscodeStyleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(extContext.extensionUri, "vscode.css"),
    );

    if (local && !localServerAvailable && !isCodespaces) {
      // Theme-aware colors for waiting screen
      const bg = isDark ? '#1a1a1a' : '#f7f1e1';
      const textColor = isDark ? '#d4d4d4' : '#3b2a1a';
      const titleColor = isDark ? '#ff69b4' : '#a85a2a';
      const subtitleBg = isDark ? '#252525' : '#fff9f0';
      const subtitleBorder = isDark ? '#404040' : '#e2d4c3';
      const subtitleColor = isDark ? '#a0a0a0' : '#6b4a2e';
      const codeColor = isDark ? '#ff69b4' : '#a85a2a';
      
      return `<!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <link href="${styleUri}" rel="stylesheet">
          <link href="${resetStyleUri}" rel="stylesheet">
          <link href="${vscodeStyleUri}" rel="stylesheet">
          <title>News</title>
          <style>
            body {
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
              height: 100vh;
              margin: 0;
              background: ${bg};
              color: ${textColor};
              font-family: system-ui, -apple-system, sans-serif;
            }
            .waiting { text-align: center; }
            .plug { font-size: 64px; margin-bottom: 24px; animation: wiggle 2s ease-in-out infinite; }
            .title { font-size: 20px; font-weight: 600; color: ${titleColor}; margin-bottom: 12px; letter-spacing: 0.5px; }
            .subtitle {
              font-size: 14px;
              color: ${subtitleColor};
              font-family: monospace;
              background: ${subtitleBg};
              padding: 8px 16px;
              border-radius: 4px;
              border: 1px solid ${subtitleBorder};
            }
            .subtitle code { color: ${codeColor}; font-weight: 600; }
            .dots::after { content: ''; animation: dots 1.5s steps(4, end) infinite; }
            @keyframes dots { 0%, 20% { content: ''; } 40% { content: '.'; } 60% { content: '..'; } 80%, 100% { content: '...'; } }
            @keyframes wiggle { 0%, 100% { transform: rotate(-5deg); } 50% { transform: rotate(5deg); } }
          </style>
        </head>
        <body>
          <div class="waiting">
            <div class="plug">📰</div>
            <div class="title">Waiting for local server<span class="dots"></span></div>
            <div class="subtitle">Run <code>ac-site</code> to start localhost:8888</div>
          </div>
        </body>
        </html>`;
    }

    let iframeUrl;
    let iframeProtocol = "https://";
    let path = "";
    if (isCodespaces && codespaceName && codespacesDomain) {
      iframeUrl = `${codespaceName}-8888.${codespacesDomain}`;
      path = "/news.aesthetic.computer";
    } else if (local) {
      iframeUrl = "localhost:8888";
      path = "/news.aesthetic.computer";
    } else {
      iframeUrl = "news.aesthetic.computer";
      path = "";
    }

    let cspFrameSrc = "frame-src https://news.aesthetic.computer https://localhost:8888 https://www.youtube.com https://youtube.com https://www.youtube-nocookie.com";
    let cspChildSrc = "child-src https://news.aesthetic.computer https://localhost:8888 https://www.youtube.com https://youtube.com https://www.youtube-nocookie.com";
    if (isCodespaces && codespacesDomain) {
      const codespaceWildcard = `https://*.${codespacesDomain}`;
      cspFrameSrc += ` ${codespaceWildcard}`;
      cspChildSrc += ` ${codespaceWildcard}`;
    }

    const sessionAesthetic = extContext.globalState.get(
      "aesthetic:session",
      undefined,
    );

    let param = "?vscode=true";
    if (sessionAesthetic && typeof sessionAesthetic === "object") {
      try {
        const encoded = Buffer.from(JSON.stringify(sessionAesthetic)).toString(
          "base64",
        );
        param += `&session-aesthetic=${encodeURIComponent(encoded)}`;
      } catch (e) {
        console.log("🔴 Failed to encode session for News webview:", e);
        param += "&session-aesthetic=null";
      }
    } else {
      param += "&session-aesthetic=null";
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
        <title>News</title>
        <style>
          body { margin: 0; padding: 0; overflow: hidden; }
          iframe#news { width: 100vw; height: 100vh; border: none; }
        </style>
      </head>
      <body>
        <iframe id="news" class="visible" sandbox="allow-scripts allow-same-origin allow-modals allow-popups allow-popups-to-escape-sandbox allow-forms allow-presentation" allow="clipboard-write; clipboard-read; autoplay; encrypted-media; fullscreen; accelerometer; gyroscope; picture-in-picture" src="${iframeProtocol}${iframeUrl}${path}${param}"></iframe>
        <script nonce="${nonce}">
          const vscode = acquireVsCodeApi();
          const newsIframe = document.getElementById('news');

          window.addEventListener('message', (event) => {
            if (event.data?.type === 'setSession') {
              newsIframe?.contentWindow?.postMessage(event.data, '*');
            }
            // Forward news: prefixed messages and login/logout/signup to extension
            if (event.data?.type && (
              event.data.type.startsWith('vscode-extension:') || 
              event.data.type.startsWith('news:') ||
              event.data.type === 'login' ||
              event.data.type === 'logout' ||
              event.data.type === 'signup' ||
              event.data.type === 'openExternal'
            )) {
              vscode.postMessage(event.data);
            }
          });

          newsIframe?.addEventListener('load', () => {
            vscode.postMessage({ type: 'news:ready' });
          });
        </script>
      </body>
      </html>`;
  }

// 🧭 AT WebView Content
function getAtWebViewContent(webview: any) {
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

  let iframeUrl;
  let iframeProtocol = "https://";
  if (isCodespaces && codespaceName && codespacesDomain) {
    iframeUrl = `${codespaceName}-4177.${codespacesDomain}`;
  } else if (local) {
    iframeUrl = "localhost:4177";
    iframeProtocol = "http://";
  } else {
    iframeUrl = "at.aesthetic.computer";
  }

  let cspFrameSrc = "frame-src https://at.aesthetic.computer http://localhost:4177";
  let cspChildSrc = "child-src https://at.aesthetic.computer http://localhost:4177";

  if (isCodespaces && codespacesDomain) {
    const codespaceWildcard = `https://*.${codespacesDomain}`;
    cspFrameSrc += ` ${codespaceWildcard}`;
    cspChildSrc += ` ${codespaceWildcard}`;
  }

  const path = local ? "/user.html" : "/";
  const param = local ? "?handle=art.at.aesthetic.computer&vscode=true" : "?vscode=true";

  return `<!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src ${webview.cspSource} 'unsafe-inline'; script-src 'nonce-${nonce}'; media-src *; img-src ${webview.cspSource} data:;">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <link href="${styleUri}" rel="stylesheet">
      <link href="${resetStyleUri}" rel="stylesheet">
      <link href="${vscodeStyleUri}" rel="stylesheet">
      <title>AT</title>
      <style>
        body {
          margin: 0;
          padding: 0;
          overflow: hidden;
        }
        iframe#at {
          width: 100vw;
          height: 100vh;
          border: none;
        }
      </style>
    </head>
    <body>
      <iframe id="at" class="visible" sandbox="allow-scripts allow-same-origin allow-modals allow-popups allow-popups-to-escape-sandbox allow-forms allow-presentation" allow="clipboard-write; clipboard-read" src="${iframeProtocol}${iframeUrl}${path}${param}" border="none"></iframe>
    </body>
    </html>`;
}

// 🌈 KidLisp.com WebView Content
function getKidLispWebViewContent(webview: any) {
  const nonce = getNonce();
  
  // Detect VS Code theme for KidLisp waiting screen
  const themeKind = vscode.window.activeColorTheme.kind;
  const isDark = themeKind === 2 || themeKind === 3; // Dark or HighContrast
  
  const styleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "main.css"),
  );

  const resetStyleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "reset.css"),
  );

  const vscodeStyleUri = webview.asWebviewUri(
    vscode.Uri.joinPath(extContext.extensionUri, "vscode.css"),
  );

  // Show waiting UI if local mode is enabled but server isn't available yet
  if (local && !localServerAvailable && !isCodespaces) {
    // Theme-aware colors
    const bg = isDark ? 'linear-gradient(135deg, #181318 0%, #141214 100%)' : 'linear-gradient(135deg, #fffacd 0%, #fff9c0 100%)';
    const textColor = isDark ? '#ffffffcc' : '#333';
    const accentColor = isDark ? '#ff69b4' : '#9370DB';
    const subtitleBg = isDark ? '#1a1a1a' : '#fff';
    const subtitleBorder = isDark ? '#483848' : '#e0d8a8';
    const subtitleColor = isDark ? '#b0a0a8' : '#666';
    
    return `<!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link href="${styleUri}" rel="stylesheet">
        <link href="${resetStyleUri}" rel="stylesheet">
        <link href="${vscodeStyleUri}" rel="stylesheet">
        <title>KidLisp.com</title>
        <style>
          body {
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            height: 100vh;
            margin: 0;
            background: ${bg};
            color: ${textColor};
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
            color: ${accentColor};
            margin-bottom: 12px;
            letter-spacing: 0.5px;
          }
          .subtitle {
            font-size: 14px;
            color: ${subtitleColor};
            font-family: monospace;
            background: ${subtitleBg};
            padding: 8px 16px;
            border-radius: 4px;
            border: 1px solid ${subtitleBorder};
          }
          .subtitle code {
            color: ${accentColor};
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
          <div class="plug">🌈</div>
          <div class="title">Waiting for local server<span class="dots"></span></div>
          <div class="subtitle">Run <code>ac-site</code> to start localhost:8888</div>
        </div>
      </body>
      </html>`;
  }

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

  // Encode current session for kidlisp.com so it can hydrate without another login
  const sessionAesthetic = extContext.globalState.get(
    "aesthetic:session",
    undefined,
  );

  let param = "?vscode=true";
  if (sessionAesthetic && typeof sessionAesthetic === "object") {
    try {
      const encoded = Buffer.from(JSON.stringify(sessionAesthetic)).toString(
        "base64",
      );
      param += `&session-aesthetic=${encodeURIComponent(encoded)}`;
    } catch (e) {
      console.log("🔴 Failed to encode session for KidLisp webview:", e);
      param += "&session-aesthetic=null";
    }
  } else {
    param += "&session-aesthetic=null";
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
          width: 100vw;
          height: 100vh;
          border: none;
          background: linear-gradient(135deg, #fffacd 0%, #fff9c0 100%);
        }
      </style>
    </head>
    <body>
      <iframe id="kidlisp" class="visible" sandbox="allow-scripts allow-same-origin allow-modals allow-popups allow-popups-to-escape-sandbox allow-presentation" allow="clipboard-write; clipboard-read" src="${iframeProtocol}${iframeUrl}/kidlisp.com${param}" border="none"></iframe>
      <script nonce="${nonce}">
        const vscode = acquireVsCodeApi();
        const kidlispIframe = document.getElementById('kidlisp');

        // Forward messages from extension to the iframe (sessions, etc.)
        window.addEventListener('message', (event) => {
          if (event.data?.type === 'setSession') {
            kidlispIframe?.contentWindow?.postMessage(event.data, '*');
          }
          if (event.data && event.data.type && (event.data.type.startsWith('vscode-extension:') || event.data.type.startsWith('kidlisp:'))) {
            vscode.postMessage(event.data);
          }
        });

        kidlispIframe?.addEventListener('load', () => {
          vscode.postMessage({ type: 'kidlisp:ready' });
        });
      </script>
    </body>
    </html>`;
}

export { activate, AestheticViewProvider };