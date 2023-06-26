(function () {
  const vscode = acquireVsCodeApi();
  const savedState = vscode.getState() || { code: "" };
  const pieceButton = document.querySelector("#run");
  const codeInput = document.querySelector("#code");

  codeInput.value = savedState.code || ""; // Remember code input.
  if (codeInput.value.length > 0) {
    vscode.postMessage({ type: "setCode", value: codeInput.value });
  }

  pieceButton.addEventListener("click", () => {
    vscode.postMessage({ type: "runPiece" });
  });

  codeInput.addEventListener("change", () => {
    vscode.setState({ code: codeInput.value });
    vscode.postMessage({ type: "setCode", value: codeInput.value });
  });

  // Handle messages sent from the extension to the webview
  // window.addEventListener("message", (event) => {
  // const message = event.data; // The json data that the extension sent
  // switch (message.type) {
  // case "runPiece": {
  //   runPiece();
  //   break;
  // }
  // }
  // });
})();
