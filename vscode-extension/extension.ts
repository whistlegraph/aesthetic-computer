// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live reloading aesthetic.computer pieces.

/* #region todo 📓 
#endregion */

// Import necessary modules from vscode
import * as vscode from "vscode";

let local: boolean = false;
let codeChannel: string | undefined;

import { AestheticAuthenticationProvider } from "./aestheticAuthenticationProvider";

async function activate(context: vscode.ExtensionContext): Promise<void> {
  local = context.globalState.get("aesthetic:local", false); // Retrieve env.

  // Authorization
  const ap = new AestheticAuthenticationProvider(context);
  context.subscriptions.push(ap);

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.logIn", async () => {
      const session = await vscode.authentication.getSession(
        "aesthetic",
        ["profile"],
        {
          createIfNone: true,
        },
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
      // vscode.window.showInformationMessage(`👋 Hi ${session.account.label}`);
      vscode.window.showInformationMessage(`👋 Welcome back!`);
    }

    provider.sessionData = session;

    return session;
  };

  context.subscriptions.push(
    vscode.authentication.onDidChangeSessions(async (e) => {
      console.log("Changed sessions:", e);
      if (e.provider.id === "aesthetic") {
        const session = await getAestheticSession();
        //if (session) {
        //  provider.sendMessageToWebview({ type: "setSession", session });
        //} else {
        provider.refreshWebview();
        //}
      }
    }),
  );

  const provider = new AestheticViewProvider(context.extensionUri);
  // await getAestheticSession();

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      AestheticViewProvider.viewType,
      provider,
    ),
  );

  // Send piece code through the code channel.
  function upload() {
    if (local) {
      console.log("😊 Skipping `/run` api endpoint. (In local mode.)");
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

  // Automatically re-run the piece when saving.
  vscode.workspace.onDidSaveTextDocument((document) => {
    if (vscode.window.activeTextEditor?.document === document) {
      vscode.commands.executeCommand("aestheticComputer.runPiece");
    }
  });
}

class AestheticViewProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = "aestheticComputer.sidebarView";
  private _extensionUri: vscode.Uri;
  private _view?: vscode.WebviewView;
  public sessionData: any = {};

  constructor(extensionUri: vscode.Uri) {
    this._extensionUri = extensionUri;
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
        this.sessionData,
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
      this.sessionData,
    );

    webviewView.webview.onDidReceiveMessage((data) => {
      switch (data.type) {
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
        // case "logout": {
        // console.log("🚪 Logging out...");
        // vscode.commands.executeCommand("aestheticComputer.logOut");
        // break;
        // }
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

    console.log("Building session html with:", session);

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