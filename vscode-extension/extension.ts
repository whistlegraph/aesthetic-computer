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
let codeChannel: string | undefined;

// Detect if we're in GitHub Codespaces
const isCodespaces = process?.env.CODESPACES === "true";
const codespaceName = process?.env.CODESPACE_NAME;
const codespacesDomain = process?.env.GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN;

let mergedDocs: any = {};
let docs: any;

let extContext: any;
let webWindow: any;

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
            { enableScripts: true }, // Webview options.
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
        
        if (isCodespaces && codespaceName && codespacesDomain) {
          const codespaceUrl = `https://${codespaceName}-8888.${codespacesDomain}`;
          cspFrameSrc += ` ${codespaceUrl}`;
          cspChildSrc += ` ${codespaceUrl}`;
        }

        // And set its HTML content
        docsPanel.webview.html = `
        <!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta http-equiv="Permissions-Policy" content="midi=*">
          <meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src 'nonce-${nonce}'; script-src 'nonce-${nonce}'; img-src 'self' vscode-resource:;">
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

  const provider = new AestheticViewProvider();

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
      // Refresh the webview with the new local state
      provider.refreshWebview();
      refreshWebWindow();
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

  constructor() {}

  // Method to send message to the webview
  public sendMessageToWebview(message: any) {
    if (this._view && this._view.webview) {
      this._view.webview.postMessage(message);
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

  const sessionAesthetic = extContext.globalState.get(
    "aesthetic:session",
    undefined,
  );

  const sessionSotce = extContext.globalState.get("sotce:session", undefined);
  // console.log("🟪 Aesthetic:", sessionAesthetic, "🪷 Sotce:", sessionSotce);

  // console.log("🪱 Slug:", slug);
  let hashFragment = "";
  let pathPart = slug || "";

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
  if (isCodespaces && codespaceName && codespacesDomain) {
    // In Codespaces, always use the forwarded URL
    iframeUrl = `https://${codespaceName}-8888.${codespacesDomain}`;
  } else {
    // On local laptop, use localhost if local mode is enabled
    iframeUrl = local ? "localhost:8888" : "aesthetic.computer";
  }

  // Build CSP frame-src and child-src based on environment
  let cspFrameSrc = "frame-src https://aesthetic.computer https://hi.aesthetic.computer https://aesthetic.local:8888 https://localhost:8888 https://sotce.net https://hi.sotce.net https://sotce.local:8888";
  let cspChildSrc = "child-src https://aesthetic.computer https://aesthetic.local:8888 https://sotce.net https://sotce.local:8888 https://localhost:8888";
  
  if (isCodespaces && codespaceName && codespacesDomain) {
    const codespaceUrl = `https://${codespaceName}-8888.${codespacesDomain}`;
    cspFrameSrc += ` ${codespaceUrl}`;
    cspChildSrc += ` ${codespaceUrl}`;
  }

  return `<!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src ${
          webview.cspSource
        }; script-src 'nonce-${nonce}'; media-src *; img-src 'self' vscode-resource:;">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<link href="${styleUri}" rel="stylesheet">
				<link href="${resetStyleUri}" rel="stylesheet">
				<link href="${vscodeStyleUri}" rel="stylesheet">
				<title>aesthetic.computer</title>
			</head>
			<body>
        <iframe id="aesthetic" credentialless sandbox="allow-scripts allow-same-origin allow-pointer-lock allow-modals allow-popups allow-popups-to-escape-sandbox" allow="clipboard-write; clipboard-read; pointer-lock" src="https://${iframeUrl}/${param}${hashFragment}" border="none"></iframe>
       	<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
}

export { activate, AestheticViewProvider };