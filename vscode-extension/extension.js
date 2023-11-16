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

const vscode = require("vscode");
const fetch = require("node-fetch");

let local = false;
let activeEditor, codeChannel;

async function activate(context) {
  // const fetch = (...args) =>
  //   import("node-fetch").then(({ default: fetch }) => fetch(...args));

  const provider = new AestheticViewProvider(context.extensionUri);

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      AestheticViewProvider.viewType,
      provider
    )
  );

  function upload() {
    let editor = vscode.window.activeTextEditor;
    let source = editor.document.getText();
    const piece = editor.document.fileName
      .split(/\/|\\/) // Split on both forward slash and backslash
      .slice(-1)[0]
      .replace(".mjs", "");

    // 📓 The `local` probably won't work due to VSCode's Proxy.
    const host = local === false ? "aesthetic.computer" : "localhost:8888";

    let url = `https://${host}/run`;

    vscode.window.showInformationMessage("Running via: " + url);

    fetch(url, {
      method: "POST",
      body: JSON.stringify({ piece, source, codeChannel }),
      headers: { "Content-Type": "application/json" },
    })
      .then((res) => {
        console.log("Success:", res);
        vscode.window.showInformationMessage(res);
      })
      .catch((error) => {
        console.error("Error:", error);
        vscode.window.showInformationMessage(error);
      });

    activeEditor = editor; // Set the active editor for live updates.
  }

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.runPiece", () => {
      upload({ publish: false });
    }),
    vscode.commands.registerCommand("aestheticComputer.localServer", () => {
      local = !local;
      vscode.window.showInformationMessage(
        `Local Mode: ${local ? "Enabled" : "Disabled"}`
      );
    })
  );

  // Automatically re-run the piece when saving.
  vscode.workspace.onDidSaveTextDocument((document) => {
    if (
      activeEditor &&
      vscode.window.activeTextEditor &&
      activeEditor.document === document
    ) {
      vscode.commands.executeCommand("aestheticComputer.runPiece");
    }
  });
}

class AestheticViewProvider {
  static viewType = "aestheticComputer.sidebarView";

  constructor(_extensionUri) {
    this._extensionUri = _extensionUri;
    this._view = undefined;
  }

  resolveWebviewView(webviewView, context, _token) {
    this._view = webviewView;

    webviewView.webview.options = {
      enableScripts: true,
      localResourceRoots: [this._extensionUri],
    };

    webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);

    webviewView.webview.onDidReceiveMessage((data) => {
      switch (data.type) {
        case "setCode": {
          codeChannel = data.value;
          break;
        }
        case "runPiece": {
          vscode.commands.executeCommand("aestheticComputer.runPiece");
          break;
        }
      }
    });
  }

  // runPiece() {
  //   if (this._view) {
  //     this._view.show?.(true);
  //     this._view.webview.postMessage({ type: "runPiece" });
  //   }
  // }

  _getHtmlForWebview(webview) {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "sidebar.js")
    );

    const nonce = getNonce();
    const styleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "main.css")
    );

    const resetStyleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "reset.css")
    );

    const vscodeStyleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this._extensionUri, "vscode.css")
    );

    return `<!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<link href="${styleUri}" rel="stylesheet">
				<link href="${resetStyleUri}" rel="stylesheet">
				<link href="${vscodeStyleUri}" rel="stylesheet">
				<title>aesthetic.computer</title>
			</head>
			<body>
        <h3>⚙️ Setup</h3>
        <p>
        Set a <code>code-channel</code> on the <code>prompt</code> and enter it here.
        </p>
        <input id="code" placeholder="Enter Code Channel" type="text"></input>
        <br>
        <h3>💻 Code</h3>
        <p>Run any piece in your active editor.</p>
				<button id="run">Run Piece</button>
        <br>
        <br>
        <h3>🧩 Publish</h3>
        <p>
        Type <code>publish</code> on the <code>prompt</code> to make it public.
        </p>
				<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
  }
}

function getNonce() {
  let text = "";
  const possible =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < 32; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  return text;
}

module.exports = { activate };
