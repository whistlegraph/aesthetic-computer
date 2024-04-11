// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live coding aesthetic.computer pieces and
// exploring the system documentation.

/* #region TODO 📓 
#endregion */

// Import necessary modules from vscode
import * as vscode from "vscode";

import { AestheticAuthenticationProvider } from "./aestheticAuthenticationProviderRemote";
const { keys } = Object;

let local: boolean = false;
let codeChannel: string | undefined;

let mergedDocs: any = {};
let docsTemplate: any;
let docs: any;

async function activate(context: vscode.ExtensionContext): Promise<void> {
  local = context.globalState.get("aesthetic:local", false); // Retrieve env.

  // Load the docs from the web.
  try {
    // const url = `https://${ // local ? "localhost:8888" : "aesthetic.computer" }/api/docs`;
    const url = `https://aesthetic.computer/docs.json`;
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data: any = await response.json();
    console.log("📚 Docs loaded:", data);

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

    docsTemplate = data.template;
    docs = data;
  } catch (error) {
    console.error("Failed to fetch documentation:", error);
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
          vscode.commands.executeCommand("aestheticComputer.openDoc", [word]);
          return null;
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

        // And set its HTML content
        docsPanel.webview.html = `
        <!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta http-equiv="Content-Security-Policy" content="default-src 'none'; frame-src https://aesthetic.computer https://aesthetic.local:8888 https://localhost:8888; child-src https://aesthetic.computer https://aesthetic.local:8888 https://localhost:8888; style-src 'nonce-${nonce}'; script-src 'nonce-${nonce}';">
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
          <iframe allow="clipboard-write; clipboard-read" credentialless sandbox="allow-scripts" src="https://aesthetic.computer/docs${path}">
        </body>
        </html>
      `.trim();

        // vscode.commands.executeCommand("markdown.showPreviewToSide", uri);
      },
    ),
  );


  // context.subscriptions.push(
  //   vscode.commands.registerCommand(
  //     "aestheticComputer.openWindow",
  //     (functionName) => {
  //       // TODO: Open pop-up window.
  //     },
  //   ),
  // );

  context.subscriptions.push(
  vscode.commands.registerCommand("aestheticComputer.openWindow", () => {
    const panel = vscode.window.createWebviewPanel(
      'webView',                 // Identifies the type of the webview. Used internally
      'My Webview',              // Title of the panel displayed to the user
      vscode.ViewColumn.One,     // Editor column to show the new webview panel in.
      {}                         // Webview options.
    );

    // TODO: Eventually this will land in a VS Code version.  

    panel.webview.html = getWebviewContent(); // Set the content of the webview
  })
);

function getWebviewContent() {
  // Define your HTML content here
  return `<!DOCTYPE html>
          <html lang="en">
          <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>My Webview</title>
          </head>
          <body>
              <h1>Hello from my Webview!</h1>
          </body>
          </html>`;
}


  // Add definitionProvider to context.subscriptions if necessary
  context.subscriptions.push(definitionProvider);

  // 🗝️ Authorization
  const ap = new AestheticAuthenticationProvider(context, local);
  context.subscriptions.push(ap);

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
      vscode.window.showInformationMessage(
        "🟡 To log out, please use the profile icon in the VS Code UI.",
      );
    }),
  );

  const getAestheticSession = async () => {
    const session = await vscode.authentication.getSession(
      "aesthetic",
      ["profile"],
      { createIfNone: false },
    );

    if (session) {
      vscode.window.showInformationMessage(
        `👋 Welcome back! (${session.account.label})`,
      );
      context.globalState.update("aesthetic:session", session);
    } else {
      context.globalState.update("aesthetic:session", undefined);
      console.log("😀 Erased session!");
    }

    return session;
  };

  context.subscriptions.push(
    vscode.authentication.onDidChangeSessions(async (e) => {
      console.log("🏃 Sessions changed:", e);
      if (e.provider.id === "aesthetic") {
        await getAestheticSession();
        provider.refreshWebview();
      }
    }),
  );

  // GUI

  const provider = new AestheticViewProvider(
    context.extensionUri,
    context.globalState,
  );

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      AestheticViewProvider.viewType,
      provider,
    ),
  );

  // 🧩 Piece Running

  // Send piece code through the code channel.
  function upload() {
    if (local) {
      // console.log("😊 Skipping `/run` api endpoint. (In local mode.)");
      return;
    }

    let editor = vscode.window.activeTextEditor;
    if (!editor) {
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
        console.log(error);
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
      vscode.window.showInformationMessage(
        `💻 Local Development: ${local ? "Enabled" : "Disabled"}`,
      );
    }),
  );

  // Automatically re-run the piece when saving any .mjs file.
  vscode.workspace.onDidSaveTextDocument((document) => {
    if (
      vscode.window.activeTextEditor?.document === document &&
      document.uri.fsPath.endsWith(".mjs")
    ) {
      vscode.commands.executeCommand("aestheticComputer.runPiece");
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
  private _extensionUri: vscode.Uri;
  private _view?: vscode.WebviewView;
  private _globalState: vscode.Memento;
  // public sessionData: any = {};

  constructor(extensionUri: vscode.Uri, globalState: vscode.Memento) {
    this._extensionUri = extensionUri;
    this._globalState = globalState;
  }

  // Method to send message to the webview
  public sendMessageToWebview(message: any) {
    if (this._view && this._view.webview) {
      this._view.webview.postMessage(message);
    }
  }

  public refreshWebview(): void {
    if (this._view) {
      this._view.title = local ? "Local" : ""; // Update the title if local.
      this._view.webview.html = this._getHtmlForWebview(this._view.webview);
    }
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext<unknown>,
    _token: vscode.CancellationToken,
  ): void {
    this._view = webviewView;
    this._view.title = local ? "Local" : ""; // Update the title if local.

    // Set retainContextWhenHidden to true
    this._view.webview.options = {
      enableScripts: true,
      localResourceRoots: [this._extensionUri],
    };

    webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);

    webviewView.webview.onDidReceiveMessage((data) => {
      switch (data.type) {
        case "clipboard:copy": {
          vscode.env.clipboard.writeText(data.value).then(() => {
            // console.log("📋 Copied text to clipboard!");
            webviewView.webview.postMessage({
              type: "clipboard:copy:confirmation",
            });
          });
        }
        case "publish": {
          if (data.url) vscode.env.openExternal(vscode.Uri.parse(data.url));
          break;
        }
        case "setCode": {
          codeChannel = data.value;
          // const currentTitle = webviewView.title;
          // webviewView.title = currentTitle?.split(" · ")[0] + " · " + codeChannel;
          // ^ Disabled because it's always rendered uppercase. 24.01.27.17.26
          break;
        }
        case "vscode-extension:reload": {
          vscode.commands.executeCommand("workbench.action.reloadWindow");
          break;
        }
        case "openDocs": {
          console.log("🏃 Opening docs...");
          vscode.commands.executeCommand("aestheticComputer.openDoc");
          break;
        }
        case "openSource": {
          console.log("📃 Opening a new source file...", data);
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
                  vscode.window.showInformationMessage(
                    "🟡 Save this file with a `.mjs` extension to run it.",
                  );
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
          vscode.commands.executeCommand("aestheticComputer.logIn");
          break;
        }
        case "signup": {
          console.log("🔏 Signing up...");
          vscode.commands.executeCommand("aestheticComputer.signUp");
          break;
        }
        case "logout": {
          console.log("🚪 Logging out...");
          vscode.commands.executeCommand("aestheticComputer.logOut");
          break;
        }
      }
    });
  }

  private _getHtmlForWebview(webview: vscode.Webview): string {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "sidebar.js"),
    );

    const nonce = getNonce();
    const styleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "main.css"),
    );

    const resetStyleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "reset.css"),
    );

    const vscodeStyleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "vscode.css"),
    );

    const session = this._globalState.get("aesthetic:session", undefined);

    console.log("Building session html with:", session);

    // Include the session data as a global variable in the webview
    // const sessionData = `<script nonce="${nonce}">window.aestheticSession = ${JSON.stringify(
    //   session,
    // )};</script>`;

    let param = "";
    if (typeof session === "object") {
      if (keys(session)?.length > 0) {
        const base64EncodedSession = btoa(JSON.stringify(session));
        param = "?session=" + encodeURIComponent(base64EncodedSession);
      }
    }

    return `<!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; frame-src https://aesthetic.computer https://hi.aesthetic.computer https://aesthetic.local:8888 https://localhost:8888; child-src https://aesthetic.computer https://aesthetic.local:8888 https://localhost:8888; style-src ${
          webview.cspSource
        }; script-src 'nonce-${nonce}';">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<link href="${styleUri}" rel="stylesheet">
				<link href="${resetStyleUri}" rel="stylesheet">
				<link href="${vscodeStyleUri}" rel="stylesheet">
				<title>aesthetic.computer</title>
			</head>
			<body>
        <iframe id="aesthetic" credentialless sandbox="allow-scripts allow-same-origin" allow="clipboard-write; clipboard-read; camera; microphone; gyroscope" src="https://${
          local ? "localhost:8888" : "aesthetic.computer"
        }${param}" border="none"></iframe>
       	<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
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

export { activate, AestheticViewProvider };
