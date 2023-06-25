// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live reloading aesthetic.computer pieces.

/* #region todo 📓 
  - [ ] Run npm outdated in this repo to see if install can be fixed
        or reload of the extension can be automated.
  - [-] Only add autoreload after making the `Run Piece` link once...
       check for a file that was already added.
  - [🟡] Get working in production again and enable the production host. 
    - [🔴] This will require JamSocket support to be working again...
  - [] Replace the SVG.
  - [] Publish the first version of the extension.
#endregion */

const vscode = require("vscode");
const fetch = require("node-fetch");

function activate(context) {
  console.log("My aesthetic.computer extension is now active!");

  // *** Command Implementation ***
  const runPieceCommand = vscode.commands.registerCommand(
    "aestheticComputer.runPiece",
    async () => {
      let editor = vscode.window.activeTextEditor;
      let source = editor.document.getText();
      const piece = editor.document.fileName
        .split("/")
        .slice(-1)[0]
        .replace(".mjs", "");

      // const host = "aesthetic.computer";
      const host = "localhost:8888";

      fetch(`https://${host}/run`, {
        method: "POST",
        body: JSON.stringify({ piece, source }),
        headers: { "Content-Type": "application/json" },
      })
        .then((res) => res.json())
        .then((response) => {
          console.log("Success:", JSON.stringify(response));
        })
        .catch((error) => {
          console.error("Error:", error);
        });
    }
  );

  // Automatically reload whenever the file changes.
  vscode.workspace.onDidSaveTextDocument((document) => {
    if (
      vscode.window.activeTextEditor &&
      vscode.window.activeTextEditor.document === document
    ) {
      vscode.commands.executeCommand("aestheticComputer.runPiece");
    }
  });

  context.subscriptions.push(runPieceCommand); // Memory clean-up.
}

module.exports = {
  activate,
};
