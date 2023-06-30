// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live reloading aesthetic.computer pieces.

/* #region todo 📓 
  - [-] Replace the SVG.
  - [] Fix the extension manifest.
  - [] Add instructions to the html view.
  - [] Publish the first version of the extension.
  + Done
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

let activeEditor, code;

function activate(context) {
  const provider = new AestheticViewProvider(context.extensionUri);

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      AestheticViewProvider.viewType,
      provider
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("aestheticComputer.runPiece", () => {
      let editor = vscode.window.activeTextEditor;
      let source = editor.document.getText();
      const piece = editor.document.fileName
        .split("/")
        .slice(-1)[0]
        .replace(".mjs", "");

      const host = "aesthetic.computer"; // "localhost:8888";

      fetch(`https://${host}/run`, {
        method: "POST",
        body: JSON.stringify({ piece, source, code }),
        headers: { "Content-Type": "application/json" },
      })
        .then((res) => res.json())
        .then((response) => {
          console.log("Success:", JSON.stringify(response));
        })
        .catch((error) => {
          console.error("Error:", error);
        });

      activeEditor = editor; // Set the active editor for live updates.
      // provider.runPiece(); // Update the contents of the button.
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
        case "runPiece": {
          vscode.commands.executeCommand("aestheticComputer.runPiece");
          break;
        }
        case "setCode": {
          code = data.value;
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
        <input id="code" placeholder="Enter Code Channel" type="text"></input>
				<button id="run">Run Piece</button>
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