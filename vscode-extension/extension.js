// VSCode Extension, 23.06.24.18.58
// A VSCode extension for live reloading aesthetic.computer pieces.

/* #region todo 📓 
  - [🟡] Get working in production again and enable the production host. 
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
      const piece = editor.document.fileName.replace(".mjs", "");

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

  context.subscriptions.push(runPieceCommand); // Memory clean-up.
}

module.exports = {
  activate,
};
