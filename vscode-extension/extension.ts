// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live reloading aesthetic.computer pieces.

/* #region todo 📓 
  - [-] Replace the SVG.
  - [] Make a web extension.
       (https://code.visualstudio.com/api/extension-guides/web-extensions)
  + Done
  - [x] Write better sidebar docs.
  - [x] Publish should no longer publish everywhere / only publish to
       active user.
       - [x] Add a `publish` command to the main prompt.
       - [x] Remove publish from here
  - [x] Add aesthetic.computer extension launch configuration for debugging.
  - [x] Add flag in the extension for local server. 
        (VSCode yields SSL errors.)
  - [x] Add publish button.
  - [x] Fix the extension manifest.
  - [x] Add instructions to the html view.
  - [x] Publish the first version of the extension.
  - [x] Don't update everyone at the same time.
    - [x] Run an AC command to link to the service.
  - [x] Test with the production url.
  - [x] Run npm outdated in this repo to see if install can be fixed
        or reload of the extension can be automated.
  - [x] Only add autoreload after making the `Run Piece` link once...
       check for a file that was already added.
#endregion */

// Import necessary modules from vscode
import * as vscode from "vscode";
import { AestheticAuthenticationProvider } from "./aestheticAuthenticationProvider";

let local: boolean = false;
let codeChannel: string | undefined;

async function activate(context: vscode.ExtensionContext): Promise<void> {
  local = context.globalState.get("aesthetic:local", false); // Retrieve env.

  // Authorization
  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.signIn", async () => {
      const session = await vscode.authentication.getSession("aesthetic", [], {
        createIfNone: true,
      });
    }),
  );

  context.subscriptions.push(new AestheticAuthenticationProvider(context));

  const getAestheticSession = async () => {
    const session = await vscode.authentication.getSession(
      "aesthetic",
      ["profile"],
      { createIfNone: false },
    );

    if (session) {
      vscode.window.showInformationMessage(
        `Welcome back ${session.account.label}`,
      );
    }

    return session;
  };

  context.subscriptions.push(
    vscode.authentication.onDidChangeSessions(async (e) => {
      console.log("Changed sessions:", e);
      if (e.provider.id === "aesthetic") {
        const session = await getAestheticSession();
        provider.sendMessageToWebview({ type: "setSession", session });
      }
    }),
  );

  const session = await getAestheticSession();
  const provider = new AestheticViewProvider(context.extensionUri, session);

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      AestheticViewProvider.viewType,
      provider,
    ),
  );

  // Send piece code through the code channel.
  function upload() {
    let editor = vscode.window.activeTextEditor;
    if (!editor) {
      return;
    }

    let source = editor.document.getText();
    const piece = editor.document.fileName
      .split(/\/|\\/) // Split on both forward slash and backslash
      .slice(-1)[0]
      .replace(".mjs", "");

    // 📓 The `local` probably won't work due to VSCode's Proxy.
    const host = local === false ? "aesthetic.computer" : "localhost:8888";

    let url = `https://${host}/run`;

    // vscode.window.showInformationMessage("Running via: " + url);
    // vscode.window.showInformationMessage("🟠");

    fetch(url, {
      method: "POST",
      body: JSON.stringify({ piece, source, codeChannel }),
      headers: { "Content-Type": "application/json" },
    })
      .then((res) => res.text()) // Convert the response to text
      .then((text) => {
        // Now 'text' is a string that can be used in showInformationMessage
        // vscode.window.showInformationMessage("🟢");
      })
      .catch((error) => {
        // If you catch an error, make sure to convert it to a string if it isn't already
        vscode.window.showInformationMessage("🔴" + error.toString());
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
        `Local Mode: ${local ? "Enabled" : "Disabled"}`,
      );
    }),
  );

  // Automatically re-run the piece when saving.
  vscode.workspace.onDidSaveTextDocument((document) => {
    if (vscode.window.activeTextEditor?.document === document) {
      console.log("Did save document...");
      vscode.commands.executeCommand("aestheticComputer.runPiece");
    }
  });
}

class AestheticViewProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = "aestheticComputer.sidebarView";
  private _extensionUri: vscode.Uri;
  private _view?: vscode.WebviewView;
  private _sessionData: any;

  constructor(extensionUri: vscode.Uri, sessionData: any) {
    this._extensionUri = extensionUri;
    this._sessionData = sessionData;
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
      this._view.webview.html = this._getHtmlForWebview(
        this._view.webview,
        this._sessionData,
      );
    }
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext<unknown>,
    _token: vscode.CancellationToken,
  ): void {
    this._view = webviewView;
    this._view.title = local ? "Local" : ""; // Update the title if local.

    webviewView.webview.options = {
      enableScripts: true,
      localResourceRoots: [this._extensionUri],
    };

    webviewView.webview.html = this._getHtmlForWebview(
      webviewView.webview,
      this._sessionData,
    );

    webviewView.webview.onDidReceiveMessage((data) => {
      switch (data.type) {
        case "publish": {
          if (data.url) vscode.env.openExternal(vscode.Uri.parse(data.url));
          break;
        }
        case "setCode": {
          codeChannel = data.value;
          // vscode.commands.executeCommand("aestheticComputer.runPiece");
          break;
        }
        case "vscode-extension:reload": {
          vscode.commands.executeCommand("workbench.action.reloadWindow");
          break;
        }
        case "runPiece": {
          console.log("Running piece...");
          vscode.commands.executeCommand("aestheticComputer.runPiece");
          break;
        }
      }
    });
  }

  private _getHtmlForWebview(webview: vscode.Webview, session: any): string {
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

    // Include the session data as a global variable in the webview
    const sessionData = `<script nonce="${nonce}">window.aestheticSession = ${JSON.stringify(
      session,
    )};</script>`;

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
        ${sessionData}
        <iframe id="aesthetic" src="https://${
          local ? "localhost:8888" : "aesthetic.computer"
        }"></iframe>
				<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
  }
}

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
